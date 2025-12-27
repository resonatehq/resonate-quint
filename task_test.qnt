// Task Tests
//
// Tests for the task state machine matching the API in task.qnt

module task_test {

  import task.* from "./task"

  // Helper to create a task from Init state
  pure def createTask(msg: Message, workers: Set[str]): Task =
    apply(initTask, Create({ msg: msg, workers: workers })).task

  // ==========================================================================
  // Tests - Create Action
  // ==========================================================================

  // --- Create from Init ---
  pure val testCreateFromInit: bool =
    val result = apply(initTask, Create({ msg: Invoke, workers: Set("w1") }))
    result.task.state == Pending and
    result.task.current == Invoke and
    result.task.workers == Set("w1") and
    result.effects.contains(Send(Set("w1")))

  pure val testCreateFromInitWithResume: bool =
    val result = apply(initTask, Create({ msg: Resume("dep1"), workers: Set("w1", "w2") }))
    result.task.state == Pending and
    result.task.current == Resume("dep1") and
    result.task.workers == Set("w1", "w2")

  // --- Create idempotent (Invoke on existing task) ---
  pure val testCreateIdempotentInvoke: bool =
    val t = createTask(Invoke, Set("w1"))
    val result = apply(t, Create({ msg: Invoke, workers: Set("w2") }))
    result.task.state == Pending and
    result.task == t and  // unchanged
    result.effects == Set()

  // --- Create buffering (Resume on existing task) ---
  pure val testCreateBufferResume: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed }
    val result = apply(t, Create({ msg: Resume("dep1"), workers: Set("w1") }))
    result.task.pending == [Resume("dep1")] and
    result.effects == Set()

  pure val testCreateBufferDuplicate: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed, pending: [Resume("dep1")] }
    val result = apply(t, Create({ msg: Resume("dep1"), workers: Set("w1") }))
    result.task.pending == [Resume("dep1")] and  // not added again
    result.effects == Set()

  pure val testCreateBufferMultiple: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed, pending: [Resume("dep1")] }
    val result = apply(t, Create({ msg: Resume("dep2"), workers: Set("w1") }))
    result.task.pending == [Resume("dep1"), Resume("dep2")]

  pure val testCreateBufferCurrentDuplicate: bool =
    val t = { ...createTask(Resume("dep1"), Set("w1")), state: Claimed }
    val result = apply(t, Create({ msg: Resume("dep1"), workers: Set("w1") }))
    result.task.pending == []  // already current, not buffered

  // ==========================================================================
  // Tests - Claim Action
  // ==========================================================================

  pure val testClaimWithCorrectVersion: bool =
    val t = createTask(Invoke, Set("w1"))
    val result = apply(t, Claim(0))
    result.task.state == Claimed and
    result.task.version == 0 and
    result.effects == Set()

  pure val testClaimWithWrongVersion: bool =
    val t = createTask(Invoke, Set("w1"))
    val result = apply(t, Claim(1))
    result.task.state == Pending and  // unchanged
    result.effects == Set()

  pure val testClaimIdempotent: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed }
    val result = apply(t, Claim(0))
    result.task.state == Claimed and
    result.task == t  // unchanged

  pure val testClaimClearsPending: bool =
    val t = { ...createTask(Invoke, Set("w1")), pending: [Resume("dep1"), Resume("dep2")] }
    val result = apply(t, Claim(0))
    result.task.state == Claimed and
    result.task.pending == []  // cleared on claim

  // ==========================================================================
  // Tests - Complete Action
  // ==========================================================================

  // --- Complete (terminal) ---
  pure val testCompleteNoCallbacks: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed }
    val result = apply(t, Complete(false))
    result.task.state == Completed and
    result.effects == Set()

  // --- Complete (with callbacks -> Waiting) ---
  pure val testCompleteWithCallbacks: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed }
    val result = apply(t, Complete(true))
    result.task.state == Waiting and
    result.task.version == 0

  // --- Complete with pending (stay Claimed, process next) ---
  pure val testCompleteWithPending: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed, pending: [Resume("dep1"), Resume("dep2")] }
    val result = apply(t, Complete(true))  // would go to Waiting, but pending exists
    result.task.state == Claimed and  // stays Claimed (not in spec, but pending processed)
    result.task.current == Resume("dep1") and  // head of pending
    result.task.pending == []  // cleared

  pure val testCompleteFromNonClaimed: bool =
    val t = createTask(Invoke, Set("w1"))  // Pending state
    val result = apply(t, Complete(false))
    result.task.state == Pending and  // unchanged
    result.task == t

  // ==========================================================================
  // Tests - Drop Action
  // ==========================================================================

  pure val testDrop: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed }
    val result = apply(t, Drop)
    result.task.state == Pending and
    result.task.version == 1 and
    result.task.current == Invoke and  // stays same
    result.effects.contains(Send(Set("w1")))

  pure val testDropFromNonClaimed: bool =
    val t = createTask(Invoke, Set("w1"))  // Pending state
    val result = apply(t, Drop)
    result.task == t and  // unchanged
    result.effects == Set()

  // ==========================================================================
  // Tests - ResumeTask Action
  // ==========================================================================

  pure val testResumeTask: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Waiting }
    val result = apply(t, ResumeTask("dep1"))
    result.task.state == Pending and
    result.task.version == 1 and
    result.task.current == Resume("dep1") and
    result.effects.contains(Send(Set("w1")))

  pure val testResumeTaskFromNonWaiting: bool =
    val t = { ...createTask(Invoke, Set("w1")), state: Claimed }
    val result = apply(t, ResumeTask("dep1"))
    result.task == t and  // unchanged
    result.effects == Set()

  // ==========================================================================
  // Tests - Full Cycle
  // ==========================================================================

  pure val testFullCycle: bool =
    // Start with fresh task, create with Invoke
    val t0 = createTask(Invoke, Set("w1"))
    // Claim it
    val r1 = apply(t0, Claim(0))
    val t1 = r1.task
    // Complete with callbacks -> Waiting
    val r2 = apply(t1, Complete(true))
    val t2 = r2.task
    // Resume -> back to Pending with version=1, current=Resume("dep1")
    val r3 = apply(t2, ResumeTask("dep1"))
    val t3 = r3.task
    // Claim again with new version
    val r4 = apply(t3, Claim(1))
    val t4 = r4.task
    // Complete without callbacks -> terminal
    val r5 = apply(t4, Complete(false))
    val t5 = r5.task

    t0.current == Invoke and
    t1.state == Claimed and t1.version == 0 and
    t2.state == Waiting and t2.version == 0 and
    t3.state == Pending and t3.version == 1 and t3.current == Resume("dep1") and
    t4.state == Claimed and t4.version == 1 and
    t5.state == Completed

  // --- Drop and retry cycle ---
  pure val testDropRetry: bool =
    val t0 = createTask(Invoke, Set("w1"))
    val t1 = apply(t0, Claim(0)).task
    // Drop -> back to Pending with version=1
    val t2 = apply(t1, Drop).task
    // Old claim with v=0 should fail
    val r3 = apply(t2, Claim(0))
    // New claim with v=1 should succeed
    val r4 = apply(t2, Claim(1))

    t2.state == Pending and t2.version == 1 and
    r3.task.state == Pending and  // claim failed
    r4.task.state == Claimed  // claim succeeded

  // ==========================================================================
  // Tests - Helper Functions
  // ==========================================================================

  pure val testIsTerminal: bool =
    val pending = createTask(Invoke, Set("w1"))
    val completed = { ...pending, state: Completed }
    not(isTerminal(pending)) and isTerminal(completed)

  pure val testIsClaimable: bool =
    val pending = createTask(Invoke, Set("w1"))
    val claimed = { ...pending, state: Claimed }
    isClaimable(pending) and not(isClaimable(claimed))

  pure val testGetSendWorkers: bool =
    val effects: Set[Effect] = Set(Send(Set("w1", "w2")))
    getSendWorkers(effects) == Set("w1", "w2")

  pure val testGetSendWorkersEmpty: bool =
    val effects: Set[Effect] = Set()
    getSendWorkers(effects) == Set()

  pure val testMessageId: bool =
    messageId(Invoke) == "" and
    messageId(Resume("dep1")) == "dep1"

  pure val testIsDuplicate: bool =
    val t = { ...createTask(Resume("dep1"), Set("w1")), pending: [Resume("dep2")] }
    isDuplicate(t, Resume("dep1")) and  // current
    isDuplicate(t, Resume("dep2")) and  // in pending
    not(isDuplicate(t, Resume("dep3"))) and  // not present
    not(isDuplicate(t, Invoke))  // Invoke never duplicate

}
