// Task Tests
//
// Tests for the task state machine matching the API in task.qnt

module task_test {

  import task.* from "./task"

  // Helper to create a task from Init state
  pure def createTask(workers: Set[str]): Task =
    apply(initTask, Create(workers)).task

  // ==========================================================================
  // Tests - Create Action
  // ==========================================================================

  pure val testCreateFromInit: bool =
    val result = apply(initTask, Create(Set("w1")))
    result.task.state == Pending and
    result.task.current == Invoke and
    result.task.workers == Set("w1") and
    result.effects.contains(Send(Set("w1")))

  pure val testCreateIdempotent: bool =
    val t = createTask(Set("w1"))
    val result = apply(t, Create(Set("w2")))
    result.task == t and  // unchanged
    result.effects == Set()

  // ==========================================================================
  // Tests - Claim Action
  // ==========================================================================

  pure val testClaimWithCorrectVersion: bool =
    val t = createTask(Set("w1"))
    val result = apply(t, Claim(0))
    result.task.state == Claimed and
    result.task.version == 0 and
    result.effects == Set()

  pure val testClaimWithWrongVersion: bool =
    val t = createTask(Set("w1"))
    val result = apply(t, Claim(1))
    result.task.state == Pending and  // unchanged
    result.effects == Set()

  pure val testClaimIdempotent: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Claim(0))
    result.task.state == Claimed and
    result.task == t  // unchanged

  // ==========================================================================
  // Tests - Complete Action
  // ==========================================================================

  pure val testCompleteNoCallbacks: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Complete({ version: 0, createsCallbacks: false }))
    result.task.state == Completed and
    result.effects == Set()

  pure val testCompleteWithCallbacks: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Complete({ version: 0, createsCallbacks: true }))
    result.task.state == Waiting and
    result.task.version == 0

  pure val testCompleteWithWrongVersion: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Complete({ version: 1, createsCallbacks: false }))
    result.task.state == Claimed and  // unchanged
    result.task == t

  pure val testCompleteWithPending: bool =
    val t = { ...createTask(Set("w1")), state: Claimed, pending: [Resume("dep1"), Resume("dep2")] }
    val result = apply(t, Complete({ version: 0, createsCallbacks: true }))
    result.task.current == Resume("dep1") and
    result.task.pending == [Resume("dep2")]

  pure val testCompleteFromNonClaimed: bool =
    val t = createTask(Set("w1"))  // Pending state
    val result = apply(t, Complete({ version: 0, createsCallbacks: false }))
    result.task.state == Pending and
    result.task == t

  // ==========================================================================
  // Tests - Drop Action
  // ==========================================================================

  pure val testDrop: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Drop(0))
    result.task.state == Pending and
    result.task.version == 1 and
    result.task.current == Invoke and
    result.effects.contains(Send(Set("w1")))

  pure val testDropWithWrongVersion: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Drop(1))
    result.task == t and  // unchanged
    result.effects == Set()

  pure val testDropFromNonClaimed: bool =
    val t = createTask(Set("w1"))  // Pending state
    val result = apply(t, Drop(0))
    result.task == t and
    result.effects == Set()

  // ==========================================================================
  // Tests - Enqueue Action
  // ==========================================================================

  pure val testEnqueueFromWaiting: bool =
    val t = { ...createTask(Set("w1")), state: Waiting }
    val result = apply(t, Enqueue("dep1"))
    result.task.state == Pending and
    result.task.version == 1 and
    result.task.current == Resume("dep1") and
    result.effects.contains(Send(Set("w1")))

  pure val testEnqueueFromClaimed: bool =
    val t = { ...createTask(Set("w1")), state: Claimed }
    val result = apply(t, Enqueue("dep1"))
    result.task.pending == [Resume("dep1")] and
    result.effects == Set()

  pure val testEnqueueFromPending: bool =
    val t = createTask(Set("w1"))
    val result = apply(t, Enqueue("dep1"))
    result.task.pending == [Resume("dep1")] and
    result.effects == Set()

  pure val testEnqueueDuplicate: bool =
    val t = { ...createTask(Set("w1")), state: Claimed, pending: [Resume("dep1")] }
    val result = apply(t, Enqueue("dep1"))
    result.task.pending == [Resume("dep1")] and  // not added again
    result.effects == Set()

  pure val testEnqueueDuplicateOfCurrent: bool =
    val t = { ...createTask(Set("w1")), state: Claimed, current: Resume("dep1") }
    val result = apply(t, Enqueue("dep1"))
    result.task.pending == [] and  // not added - already current
    result.effects == Set()

  pure val testEnqueueFromInit: bool =
    val result = apply(initTask, Enqueue("dep1"))
    result.task == initTask and  // unchanged
    result.effects == Set()

  pure val testEnqueueFromCompleted: bool =
    val t = { ...createTask(Set("w1")), state: Completed }
    val result = apply(t, Enqueue("dep1"))
    result.task == t and  // unchanged
    result.effects == Set()

  // ==========================================================================
  // Tests - Full Cycle
  // ==========================================================================

  pure val testFullCycle: bool =
    // Create task
    val t0 = createTask(Set("w1"))
    // Claim it
    val t1 = apply(t0, Claim(0)).task
    // Complete with callbacks -> Waiting
    val t2 = apply(t1, Complete({ version: 0, createsCallbacks: true })).task
    // Enqueue -> back to Pending with version=1
    val t3 = apply(t2, Enqueue("dep1")).task
    // Claim again with new version
    val t4 = apply(t3, Claim(1)).task
    // Complete without callbacks -> terminal
    val t5 = apply(t4, Complete({ version: 1, createsCallbacks: false })).task

    t0.current == Invoke and
    t1.state == Claimed and t1.version == 0 and
    t2.state == Waiting and t2.version == 0 and
    t3.state == Pending and t3.version == 1 and t3.current == Resume("dep1") and
    t4.state == Claimed and t4.version == 1 and
    t5.state == Completed

  pure val testDropRetry: bool =
    val t0 = createTask(Set("w1"))
    val t1 = apply(t0, Claim(0)).task
    // Drop -> back to Pending with version=1
    val t2 = apply(t1, Drop(0)).task
    // Old claim with v=0 should fail
    val r3 = apply(t2, Claim(0))
    // New claim with v=1 should succeed
    val r4 = apply(t2, Claim(1))

    t2.state == Pending and t2.version == 1 and
    r3.task.state == Pending and
    r4.task.state == Claimed

  // ==========================================================================
  // Tests - Helper Functions
  // ==========================================================================

  pure val testIsTerminal: bool =
    val pending = createTask(Set("w1"))
    val completed = { ...pending, state: Completed }
    not(isTerminal(pending)) and isTerminal(completed)

  pure val testIsClaimable: bool =
    val pending = createTask(Set("w1"))
    val claimed = { ...pending, state: Claimed }
    isClaimable(pending) and not(isClaimable(claimed))

  pure val testIsDuplicate: bool =
    val t = { ...createTask(Set("w1")), current: Resume("dep1"), pending: [Resume("dep2")] }
    isDuplicate(t, "dep1") and      // current
    isDuplicate(t, "dep2") and      // in pending
    not(isDuplicate(t, "dep3"))     // not present

  pure val testIsDuplicateWithInvoke: bool =
    val t = createTask(Set("w1"))  // current is Invoke
    not(isDuplicate(t, "dep1"))    // never matches Invoke

}
