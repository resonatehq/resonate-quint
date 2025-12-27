// World Specification
//
// Protocol model: server + message soup for task advertisements.
// The action layer that uses server.qnt pure functions.

module world {
  import promise from "./promise"
  import task from "./task"
  import server from "./server"

  // ==========================================================================
  // Types
  // ==========================================================================

  /// Advertisement: a message advertising a claimable task
  type Advertisement = {
    taskId: str,
    version: int
  }

  /// Protocol state: server + message soup
  type Protocol = {
    server: server::Server,
    messages: Set[Advertisement]
  }

  /// Initial empty protocol
  pure val init: Protocol = {
    server: server::init,
    messages: Set()
  }

  // ==========================================================================
  // State Machine
  // ==========================================================================

  var state: Protocol

  action doInit = {
    state' = init
  }

  /// Create a promise (and task if workers is non-empty)
  /// Precondition: promise does not exist
  action createPromise(id: str, workers: Set[str]): bool = {
    all {
      server::getPromiseState(state.server, id) == promise::Init,  // precondition
      val newServer = server::createPromise(state.server, id, workers)
      state' = { ...state, server: newServer }
    }
  }

  /// Any pending task can advertise itself
  action doAdvertise(taskId: str): bool = {
    val t = server::getTask(state.server, taskId)
    all {
      t.state == task::Pending,
      state' = { ...state, messages: state.messages.union(Set({ taskId: taskId, version: t.version })) }
    }
  }

  /// Worker picks an advertisement and tries to claim
  action doWorkerClaim(ad: Advertisement): bool = {
    val t = server::getTask(state.server, ad.taskId)
    val newServer = server::claimTask(state.server, ad.taskId, ad.version)
    all {
      ad.in(state.messages),
      t.state == task::Pending,
      t.version == ad.version,
      state' = { ...state, server: newServer }
    }
  }

  /// Worker completes task (no children) - settles promise
  action doWorkerComplete(taskId: str): bool = {
    val t = server::getTask(state.server, taskId)
    val newServer = server::completeAndSettle(state.server, taskId)
    all {
      t.state == task::Claimed,
      state' = { ...state, server: newServer }
    }
  }

  /// Worker completes task with child promise (creates callback)
  action doWorkerCompleteWithChild(parentId: str, childId: str, childWorkers: Set[str]): bool = {
    val t = server::getTask(state.server, parentId)
    val newServer = server::completeWithChild(state.server, parentId, childId, childWorkers)
    all {
      t.state == task::Claimed,
      state' = { ...state, server: newServer }
    }
  }

  /// Non-deterministic step
  action step = any {
    nondet taskId = state.server.tasks.keys().oneOf()
    doAdvertise(taskId),

    nondet ad = state.messages.oneOf()
    doWorkerClaim(ad),

    nondet taskId = state.server.tasks.keys().oneOf()
    doWorkerComplete(taskId),

    nondet parentId = state.server.tasks.keys().oneOf()
    nondet childId = Set("child1", "child2", "child3").oneOf()
    nondet workers = Set(Set("w1"), Set("w2"), Set("w1", "w2")).oneOf()
    doWorkerCompleteWithChild(parentId, childId, workers)
  }

  // ==========================================================================
  // Properties
  // ==========================================================================

  pure def allPromisesTerminal(p: Protocol): bool =
    server::allPromisesTerminal(p.server)

  temporal eventuallyAllTerminal = eventually(allPromisesTerminal(state))
}
