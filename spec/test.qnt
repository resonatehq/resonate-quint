module test {
  import util.* from "./util"
  import promise from "./promise"
  import task from "./task"
  import server from "./server"

  // ==========================================================================
  // Action Recording
  // ==========================================================================

  type ActionKind =
    | CreatePromise
    | SettlePromise
    | RegisterCallback
    | AcquireTask
    | ReleaseTask
    | FulfillTask
    | SuspendTask
    | FenceTask
    | Tick

  type ActionData = {
    promiseId: str,
    target: Option,
    timeout: int,
    awaitedId: str,
    awaiterId: str,
    id: str,
    version: int,
    callbacks: List[str],
    settlePromiseId: str,
    newTime: int
  }

  type RecordedAction = {
    kind: ActionKind,
    data: ActionData
  }

  pure def emptyAction: RecordedAction = {
    kind: CreatePromise,
    data: {
      promiseId: "",
      target: None,
      timeout: 0,
      awaitedId: "",
      awaiterId: "",
      id: "",
      version: 0,
      callbacks: [],
      settlePromiseId: "",
      newTime: 0
    }
  }

  // ==========================================================================
  // State
  // ==========================================================================

  var state: server::Server
  var event: RecordedAction

  // IDs for simulation
  pure val promiseIds = Set("p-0", "p-1", "p-2")
  pure val targets = Set(None, Some("default"))
  pure val timeouts = Set(10, 50, 100)
  pure val times = Set(0, 25, 50, 75, 100)
  pure val versions = Set(0, 1, 2)

  // ==========================================================================
  // Coordination Actions
  // ==========================================================================

  action init = all {
    state' = server::init,
    event' = emptyAction
  }

  action doCreatePromise(promiseId: str, target: Option, timeout: int): bool =
    val s1 = server::apply(state, server::C(server::CreatePromise({ id: promiseId, target: target, timeout: timeout })))
    all {
      state' = s1,
      event' = {
        kind: CreatePromise,
        data: { ...emptyAction.data, promiseId: promiseId, target: target, timeout: timeout }
      }
    }

  action doSettlePromise(promiseId: str): bool =
    val s1 = server::apply(state, server::C(server::SettlePromise(promiseId)))
    all {
      state' = s1,
      event' = {
        kind: SettlePromise,
        data: { ...emptyAction.data, promiseId: promiseId }
      }
    }

  action doRegisterCallback(awaitedId: str, awaiterId: str): bool =
    val s1 = server::apply(state, server::C(server::RegisterCallback({ awaited: awaitedId, awaiter: awaiterId })))
    all {
      state' = s1,
      event' = {
        kind: RegisterCallback,
        data: { ...emptyAction.data, awaitedId: awaitedId, awaiterId: awaiterId }
      }
    }

  // ==========================================================================
  // Distribution Actions
  // ==========================================================================

  action doAcquireTask(id: str, version: int): bool =
    val s1 = server::apply(state, server::D(server::AcquireTask({ id: id, version: version })))
    all {
      state' = s1,
      event' = {
        kind: AcquireTask,
        data: { ...emptyAction.data, id: id, version: version }
      }
    }

  action doReleaseTask(id: str, version: int): bool =
    val s1 = server::apply(state, server::D(server::ReleaseTask({ id: id, version: version })))
    all {
      state' = s1,
      event' = {
        kind: ReleaseTask,
        data: { ...emptyAction.data, id: id, version: version }
      }
    }

  action doFulfillTask(id: str, version: int, settlePromiseId: str): bool =
    val s1 = server::apply(state, server::D(server::FulfillTask({
      id: id,
      version: version,
      inner: server::SettlePromise(settlePromiseId)
    })))
    all {
      state' = s1,
      event' = {
        kind: FulfillTask,
        data: { ...emptyAction.data, id: id, version: version, settlePromiseId: settlePromiseId }
      }
    }

  action doSuspendTask(id: str, version: int, callbacks: List[str]): bool =
    val inner = callbacks.foldl([], (acc, awaitedId) => acc.append(server::RegisterCallback({ awaited: awaitedId, awaiter: id })))
    val s1 = server::apply(state, server::D(server::SuspendTask({ id: id, version: version, inner: inner })))
    all {
      state' = s1,
      event' = {
        kind: SuspendTask,
        data: { ...emptyAction.data, id: id, version: version, callbacks: callbacks }
      }
    }

  action doFenceTask(id: str, version: int, promiseId: str, target: Option, timeout: int): bool =
    val s1 = server::apply(state, server::D(server::FenceTask({
      id: id,
      version: version,
      inner: server::CreatePromise({ id: promiseId, target: target, timeout: timeout })
    })))
    all {
      state' = s1,
      event' = {
        kind: FenceTask,
        data: { ...emptyAction.data, promiseId: promiseId, target: target, timeout: timeout, id: id, version: version }
      }
    }

  // ==========================================================================
  // Time Actions
  // ==========================================================================

  action doTick(newTime: int): bool =
    val s1 = server::apply(state, server::Tick(newTime))
    all {
      state' = s1,
      event' = {
        kind: Tick,
        data: { ...emptyAction.data, newTime: newTime }
      }
    }

  // ==========================================================================
  // Simulation Step
  // ==========================================================================

  action step = {
    nondet promiseId = promiseIds.oneOf()
    nondet target = targets.oneOf()
    nondet timeout = timeouts.oneOf()
    nondet awaitedId = promiseIds.oneOf()
    nondet awaiterId = promiseIds.oneOf()
    nondet id = promiseIds.oneOf()
    nondet version = versions.oneOf()
    nondet newTime = times.oneOf()

    any {
      // Coordination actions
      doCreatePromise(promiseId, target, timeout),
      doSettlePromise(promiseId),
      doRegisterCallback(awaitedId, awaiterId),
      // Distribution actions
      doAcquireTask(id, version),
      doReleaseTask(id, version),
      doFulfillTask(id, version, promiseId),
      doSuspendTask(id, version, [awaitedId]),
      doFenceTask(id, version, promiseId, target, timeout),
      // Time actions
      doTick(newTime)
    }
  }

  // ==========================================================================
  // Invariants
  // ==========================================================================

  // Tasks only exist for promises with targets
  val tasksHavePromises = state.tasks.keys().forall(id =>
    state.promises.keys().contains(id)
  )

  // Task versions are non-negative
  val versionsNonNegative = state.tasks.keys().forall(id =>
    server::getWithDefault(state.tasks, id, task::init(id)).version >= 0
  )

  // Timers only exist for pending promises
  val timersForPendingOnly = state.timers.forall(id =>
    state.promises.keys().contains(id) and
    server::getWithDefault(state.promises, id, promise::init(id)).state == promise::Pending
  )

  // Settled promises have no awaiters
  val settledNoAwaiters = state.promises.keys().forall(id =>
    val p = server::getWithDefault(state.promises, id, promise::init(id))
    p.state == promise::Settled implies p.awaiters.size() == 0
  )

  // Waiting tasks are registered as awaiters on at least one promise
  val waitingTasksAreAwaiters = state.tasks.keys().forall(id =>
    val t = server::getWithDefault(state.tasks, id, task::init(id))
    t.state == task::Waiting implies
      state.promises.keys().exists(pid =>
        val p = server::getWithDefault(state.promises, pid, promise::init(pid))
        p.awaiters.contains(id)
      )
  )

  // ==========================================================================
  // Trace Invariants (for "can never happen" analysis)
  // ==========================================================================

  // Helper to check if a trace contains a specific branch
  pure def traceContainsBranch(traces: List[Inspect], branch: str): bool =
    traces.foldl(false, (found, t) => found or t.branch == branch)

  // Helper to check if a trace contains a specific branch for a specific id
  pure def traceContains(traces: List[Inspect], id: str, branch: str): bool =
    traces.foldl(false, (found, t) => found or (t.id == id and t.branch == branch))

  // Example: assert that suspend rejection can never happen
  // Set this to false to find counterexamples showing when it CAN happen
  val suspendRejectNeverHappens = not(traceContainsBranch(state.traces, "task.suspend::claimed.match.reject"))

  // Settled promises with tasks have completed tasks
  // (only applies when a task was actually created, i.e., task state != Init)
  val settledPromisesHaveCompletedTasks = state.promises.keys().forall(id =>
    val p = server::getWithDefault(state.promises, id, promise::init(id))
    val t = server::getWithDefault(state.tasks, id, task::init(id))
    (p.state == promise::Settled and t.state != task::Init) implies t.state == task::Completed
  )

}
