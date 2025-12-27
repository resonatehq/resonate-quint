// Task Tests

module task_test {

  import task.* from "./task"

  // ==========================================================================
  // Tests - Task Level
  // ==========================================================================

  // --- Enqueue ---
  pure val testEnqueue: bool =
    val result = apply(initTask, Enqueue(Invoke))
    result.status == Created and
    result.tsk.state == Pending and
    result.tsk.current == Invoke

  pure val testEnqueueIdempotent: bool =
    val t = { state: Pending, version: 0, current: Invoke, pending: [] }
    val result = apply(t, Enqueue(Invoke))
    result.status == OK and result.tsk.state == Pending

  // --- Enqueue buffering ---
  pure val testEnqueueBuffer: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [] }
    val result = apply(t, Enqueue(Resume("dep1")))
    result.status == Created and
    result.tsk.pending == [Resume("dep1")]

  pure val testEnqueueBufferDuplicate: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [Resume("dep1")] }
    val result = apply(t, Enqueue(Resume("dep1")))
    result.status == OK and
    result.tsk.pending == [Resume("dep1")]  // not added again

  pure val testEnqueueBufferMultiple: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [Resume("dep1")] }
    val result = apply(t, Enqueue(Resume("dep2")))
    result.status == Created and
    result.tsk.pending == [Resume("dep1"), Resume("dep2")]

  pure val testEnqueueBufferCurrentDuplicate: bool =
    val t = { state: Claimed, version: 0, current: Resume("dep1"), pending: [] }
    val result = apply(t, Enqueue(Resume("dep1")))
    result.status == OK and
    result.tsk.pending == []  // already current

  // --- Claim ---
  pure val testClaimWithCorrectVersion: bool =
    val t = apply(initTask, Enqueue(Invoke)).tsk
    val result = apply(t, ClaimTask(0))
    result.status == Created and
    result.tsk.state == Claimed and
    result.tsk.version == 0

  pure val testClaimWithWrongVersion: bool =
    val t = apply(initTask, Enqueue(Invoke)).tsk
    val result = apply(t, ClaimTask(1))
    result.status == Forbidden and
    result.tsk.state == Pending

  pure val testClaimIdempotent: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [] }
    val result = apply(t, ClaimTask(0))
    result.status == OK and result.tsk.state == Claimed

  pure val testClaimClearsPending: bool =
    val t = { state: Pending, version: 0, current: Invoke, pending: [Resume("dep1"), Resume("dep2")] }
    val result = apply(t, ClaimTask(0))
    result.status == Created and
    result.tsk.state == Claimed and
    result.tsk.pending == []  // cleared on claim

  // --- Complete (terminal) ---
  pure val testCompletePromise: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [] }
    val result = apply(t, CompleteTask(false))
    result.status == Created and
    result.tsk.state == Completed

  // --- Complete (with callbacks -> Waiting) ---
  pure val testCompleteWithCallbacks: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [] }
    val result = apply(t, CompleteTask(true))
    result.status == Created and
    result.tsk.state == Waiting and
    result.tsk.version == 0

  // --- Complete with pending (stay Claimed, process next) ---
  pure val testCompleteWithPending: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [Resume("dep1"), Resume("dep2")] }
    val result = apply(t, CompleteTask(true))  // would go to Waiting, but pending exists
    result.status == Created and
    result.tsk.state == Claimed and  // stays Claimed
    result.tsk.current == Resume("dep1") and  // head of pending
    result.tsk.pending == []  // cleared

  // --- Resume (Waiting -> Pending) ---
  pure val testResume: bool =
    val t = { state: Waiting, version: 0, current: Invoke, pending: [] }
    val result = apply(t, ResumeTask("dep1"))  // dep1 is the promise that resumed us
    result.status == Created and
    result.tsk.state == Pending and
    result.tsk.version == 1 and
    result.tsk.current == Resume("dep1")

  // --- Resume preserves pending ---
  pure val testResumePreservesPending: bool =
    val t = { state: Waiting, version: 0, current: Invoke, pending: [Resume("dep2")] }
    val result = apply(t, ResumeTask("dep1"))
    result.status == Created and
    result.tsk.current == Resume("dep1") and
    result.tsk.pending == [Resume("dep2")]  // preserved

  // --- Drop ---
  pure val testDrop: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [] }
    val result = apply(t, DropTask)
    result.status == Created and
    result.tsk.state == Pending and
    result.tsk.version == 1 and
    result.tsk.current == Invoke  // stays same

  // --- Halt ---
  pure val testHalt: bool =
    val t = { state: Claimed, version: 0, current: Invoke, pending: [] }
    val result = apply(t, HaltTask)
    result.status == Created and
    result.tsk.state == Halted and
    result.tsk.version == 0

  // --- Continue ---
  pure val testContinue: bool =
    val t = { state: Halted, version: 0, current: Invoke, pending: [] }
    val result = apply(t, ContinueTask)
    result.status == Created and
    result.tsk.state == Pending and
    result.tsk.version == 1 and
    result.tsk.current == Invoke

  // --- Full cycle test ---
  pure val testFullCycle: bool =
    // Start with fresh task, enqueue with Invoke
    val t0 = apply(initTask, Enqueue(Invoke)).tsk
    // Claim it
    val r1 = apply(t0, ClaimTask(0))
    val t1 = r1.tsk
    // Complete with callbacks -> Waiting
    val r2 = apply(t1, CompleteTask(true))
    val t2 = r2.tsk
    // Resume -> back to Pending with version=1, current=Resume("dep1")
    val r3 = apply(t2, ResumeTask("dep1"))
    val t3 = r3.tsk
    // Claim again with new version
    val r4 = apply(t3, ClaimTask(1))
    val t4 = r4.tsk
    // Complete promise -> terminal
    val r5 = apply(t4, CompleteTask(false))
    val t5 = r5.tsk

    t0.current == Invoke and
    t1.state == Claimed and t1.version == 0 and
    t2.state == Waiting and t2.version == 0 and
    t3.state == Pending and t3.version == 1 and t3.current == Resume("dep1") and
    t4.state == Claimed and t4.version == 1 and
    t5.state == Completed

  // --- Drop and retry cycle ---
  pure val testDropRetry: bool =
    val t0 = apply(initTask, Enqueue(Invoke)).tsk
    val t1 = apply(t0, ClaimTask(0)).tsk
    // Drop -> back to Pending with version=1
    val t2 = apply(t1, DropTask).tsk
    // Old claim with v=0 should fail
    val r3 = apply(t2, ClaimTask(0))
    // New claim with v=1 should succeed
    val r4 = apply(t2, ClaimTask(1))

    t2.state == Pending and t2.version == 1 and
    r3.status == Forbidden and
    r4.status == Created and r4.tsk.state == Claimed

  // --- Halt and continue cycle ---
  pure val testHaltContinue: bool =
    val t0 = apply(initTask, Enqueue(Invoke)).tsk
    val t1 = apply(t0, ClaimTask(0)).tsk
    val t2 = apply(t1, HaltTask).tsk
    val t3 = apply(t2, ContinueTask).tsk
    // Old claim with v=0 should fail
    val r4 = apply(t3, ClaimTask(0))
    // New claim with v=1 should succeed
    val r5 = apply(t3, ClaimTask(1))

    t2.state == Halted and t2.version == 0 and
    t3.state == Pending and t3.version == 1 and
    r4.status == Forbidden and
    r5.status == Created

  // ==========================================================================
  // Tests - Store Level
  // ==========================================================================

  pure val testStoreEnqueue: bool =
    val result = applyStore(emptyStore, StoreEnqueue({ id: "t1", msg: Invoke }))
    result.status == Created and
    result.tsk.state == Pending and
    result.store.get("t1").state == Pending

  pure val testStoreClaimAndComplete: bool =
    val s1 = applyStore(emptyStore, StoreEnqueue({ id: "t1", msg: Invoke }))
    val s2 = applyStore(s1.store, StoreClaim({ id: "t1", version: 0 }))
    val s3 = applyStore(s2.store, StoreComplete({ id: "t1", createsCallbacks: false }))
    s2.tsk.state == Claimed and
    s3.tsk.state == Completed

  pure val testStoreMultipleTasks: bool =
    val store = execute(emptyStore, [
      StoreEnqueue({ id: "t1", msg: Invoke }),
      StoreEnqueue({ id: "t2", msg: Invoke }),
      StoreClaim({ id: "t1", version: 0 }),
      StoreClaim({ id: "t2", version: 0 }),
      StoreComplete({ id: "t1", createsCallbacks: false }),
      StoreComplete({ id: "t2", createsCallbacks: true })
    ])
    store.get("t1").state == Completed and
    store.get("t2").state == Waiting

  // ==========================================================================
  // All Tests
  // ==========================================================================

  pure val allTestsPass: bool =
    // Task level
    testEnqueue and
    testEnqueueIdempotent and
    testEnqueueBuffer and
    testEnqueueBufferDuplicate and
    testEnqueueBufferMultiple and
    testEnqueueBufferCurrentDuplicate and
    testClaimWithCorrectVersion and
    testClaimWithWrongVersion and
    testClaimIdempotent and
    testClaimClearsPending and
    testCompletePromise and
    testCompleteWithCallbacks and
    testCompleteWithPending and
    testResume and
    testResumePreservesPending and
    testDrop and
    testHalt and
    testContinue and
    testFullCycle and
    testDropRetry and
    testHaltContinue and
    // Store level
    testStoreEnqueue and
    testStoreClaimAndComplete and
    testStoreMultipleTasks

}
