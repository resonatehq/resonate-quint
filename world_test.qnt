// Tests for protocol model
//
// Tests server operations and world-level interactions.

module world_test {
  import task from "./task"
  import promise from "./promise"
  import server from "./server"
  import world.* from "./world"

  // ==========================================================================
  // Test Helpers
  // ==========================================================================

  // Test workers
  pure val testWorkers: Set[str] = Set("w1", "w2")

  // Create a server with a promise and pending task
  pure def serverWithPendingTask(id: str): server::Server =
    server::createPromise(server::init, id, testWorkers)  // workers creates task

  // Create a server with a claimed task
  pure def serverWithClaimedTask(id: str): server::Server =
    val s1 = serverWithPendingTask(id)
    val t = server::getTask(s1, id)
    server::claimTask(s1, id, t.version)

  // Create a protocol with a pending task
  pure def protocolWithPendingTask(id: str): Protocol =
    { server: serverWithPendingTask(id), messages: Set() }

  // Create a protocol with a claimed task
  pure def protocolWithClaimedTask(id: str): Protocol =
    { server: serverWithClaimedTask(id), messages: Set() }

  // Pure advertise function
  pure def advertise(p: Protocol, taskId: str): Protocol = {
    val t = server::getTask(p.server, taskId)
    if (t.state == task::Pending) {
      { ...p, messages: p.messages.union(Set({ taskId: taskId, version: t.version })) }
    } else {
      p
    }
  }

  // ==========================================================================
  // Server Tests - Create
  // ==========================================================================

  pure val testServerCreate: bool = {
    val s = server::createPromise(server::init, "p1", Set())
    server::getPromiseState(s, "p1") == promise::Pending and
    server::getTaskState(s, "p1") == task::Init  // no task without workers
  }

  pure val testServerCreateWithWorkers: bool = {
    val s = server::createPromise(server::init, "p1", testWorkers)
    server::getPromiseState(s, "p1") == promise::Pending and
    server::getTaskState(s, "p1") == task::Pending and  // task created
    server::getTask(s, "p1").workers == testWorkers     // workers stored
  }

  pure val testServerCreateIdempotent: bool = {
    val s1 = server::createPromise(server::init, "p1", Set())
    val s2 = server::createPromise(s1, "p1", Set())
    server::getPromiseState(s2, "p1") == promise::Pending
  }

  // ==========================================================================
  // Server Tests - Settle
  // ==========================================================================

  pure val testServerSettle: bool = {
    val s1 = server::createPromise(server::init, "p1", Set())
    val s2 = server::settlePromise(s1, "p1")
    server::getPromiseState(s2, "p1") == promise::Settled
  }

  pure val testServerSettleIdempotent: bool = {
    val s1 = server::createPromise(server::init, "p1", Set())
    val s2 = server::settlePromise(s1, "p1")
    val s3 = server::settlePromise(s2, "p1")
    server::getPromiseState(s3, "p1") == promise::Settled
  }

  // ==========================================================================
  // Server Tests - Callbacks
  // ==========================================================================

  pure val testServerCallback: bool = {
    val s1 = server::createPromise(server::init, "p1", Set())
    val s2 = server::registerCallback(s1, "p1", "task1")
    server::getPromise(s2, "p1").callbacks.contains("task1")
  }

  // ==========================================================================
  // Advertise Tests
  // ==========================================================================

  pure val testAdvertisePendingTask: bool = {
    val p = protocolWithPendingTask("task1")
    val p2 = advertise(p, "task1")
    p2.messages.size() == 1 and
    { taskId: "task1", version: 0 }.in(p2.messages)
  }

  pure val testAdvertiseIdempotent: bool = {
    val p = protocolWithPendingTask("task1")
    val p2 = advertise(p, "task1")
    val p3 = advertise(p2, "task1")
    p3.messages.size() == 1  // Set semantics: no duplicates
  }

  pure val testAdvertiseClaimedTaskNoOp: bool = {
    val p = protocolWithClaimedTask("task1")
    val p2 = advertise(p, "task1")
    p2.messages.size() == 0  // No advertisement added
  }

  // ==========================================================================
  // Claim Tests
  // ==========================================================================

  pure val testClaimSuccess: bool = {
    val s = serverWithPendingTask("task1")
    val t = server::getTask(s, "task1")
    val s2 = server::claimTask(s, "task1", t.version)
    server::getTaskState(s2, "task1") == task::Claimed
  }

  pure val testClaimWrongVersion: bool = {
    val s = serverWithPendingTask("task1")
    val s2 = server::claimTask(s, "task1", 999)  // wrong version
    server::getTaskState(s2, "task1") == task::Pending  // unchanged
  }

  // ==========================================================================
  // Complete Tests
  // ==========================================================================

  pure val testCompleteNoCallbacks: bool = {
    val s = serverWithClaimedTask("task1")
    val t = server::getTask(s, "task1")
    val s2 = server::completeTask(s, "task1", t.version, false)
    server::getTaskState(s2, "task1") == task::Completed
  }

  pure val testCompleteWithCallbacks: bool = {
    val s = serverWithClaimedTask("task1")
    val t = server::getTask(s, "task1")
    val s2 = server::completeTask(s, "task1", t.version, true)
    server::getTaskState(s2, "task1") == task::Waiting
  }

  // ==========================================================================
  // Drop and Reclaim Tests
  // ==========================================================================

  pure val testDropIncreasesVersion: bool = {
    val s = serverWithClaimedTask("task1")
    val t = server::getTask(s, "task1")
    val s2 = server::dropTask(s, "task1", t.version)
    val t2 = server::getTask(s2, "task1")
    t2.state == task::Pending and t2.version == 1
  }

  pure val testReclaimAfterDrop: bool = {
    val s = serverWithClaimedTask("task1")
    val t = server::getTask(s, "task1")
    val s2 = server::dropTask(s, "task1", t.version)
    val s3old = server::claimTask(s2, "task1", 0)  // old version
    val s3new = server::claimTask(s2, "task1", 1)  // new version
    server::getTaskState(s3old, "task1") == task::Pending and  // old version fails
    server::getTaskState(s3new, "task1") == task::Claimed      // new version works
  }

  // ==========================================================================
  // Compound Operations Tests
  // ==========================================================================

  pure val testCompleteAndSettle: bool = {
    val s = serverWithClaimedTask("task1")
    val t = server::getTask(s, "task1")
    val s2 = server::completeAndSettle(s, "task1", t.version)
    server::getTaskState(s2, "task1") == task::Completed and
    server::getPromiseState(s2, "task1") == promise::Settled
  }

  pure val testCompleteWithChild: bool = {
    val s = serverWithClaimedTask("parent")
    val t = server::getTask(s, "parent")
    val s2 = server::completeWithChild(s, "parent", t.version, "child", testWorkers)
    // Parent goes to Waiting
    server::getTaskState(s2, "parent") == task::Waiting and
    // Child promise and task created
    server::getPromiseState(s2, "child") == promise::Pending and
    server::getTaskState(s2, "child") == task::Pending and
    // Child has workers
    server::getTask(s2, "child").workers == testWorkers and
    // Callback registered on child
    server::getPromise(s2, "child").callbacks.contains("parent")
  }

  pure val testChildSettleResumesParent: bool = {
    val s = serverWithClaimedTask("parent")
    val t = server::getTask(s, "parent")
    val s2 = server::completeWithChild(s, "parent", t.version, "child", testWorkers)
    val s3 = server::settlePromise(s2, "child")
    // Parent should be resumed (back to Pending with Resume message)
    val parentTask = server::getTask(s3, "parent")
    parentTask.state == task::Pending and
    parentTask.version == 1 and
    parentTask.current == task::Resume("child")
  }

  // ==========================================================================
  // All Tests
  // ==========================================================================

  pure val allTestsPass: bool = and {
    // Server create
    testServerCreate,
    testServerCreateWithWorkers,
    testServerCreateIdempotent,
    // Server settle
    testServerSettle,
    testServerSettleIdempotent,
    // Server callback
    testServerCallback,
    // Advertise
    testAdvertisePendingTask,
    testAdvertiseIdempotent,
    testAdvertiseClaimedTaskNoOp,
    // Claim
    testClaimSuccess,
    testClaimWrongVersion,
    // Complete
    testCompleteNoCallbacks,
    testCompleteWithCallbacks,
    // Drop and reclaim
    testDropIncreasesVersion,
    testReclaimAfterDrop,
    // Compound operations
    testCompleteAndSettle,
    testCompleteWithChild,
    testChildSettleResumesParent
  }
}
