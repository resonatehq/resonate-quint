// Server Specification
//
// Manages collections of promises and tasks.
// Calls pure apply functions from promise.qnt and task.qnt.
// Translates effects from promise layer to task layer.

module server {
  import promise from "./promise"
  import task from "./task"

  // ==========================================================================
  // Types
  // ==========================================================================

  /// Server state: maps of promises and tasks
  type Server = {
    promises: str -> promise::Promise,
    tasks: str -> task::Task
  }

  /// Empty server
  pure val init: Server = {
    promises: Map(),
    tasks: Map()
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  /// Get a promise by ID (returns initPromise if not found)
  pure def getPromise(s: Server, id: str): promise::Promise =
    if (s.promises.keys().contains(id)) s.promises.get(id)
    else promise::initPromise

  /// Get a task by ID (returns initTask if not found)
  pure def getTask(s: Server, id: str): task::Task =
    if (s.tasks.keys().contains(id)) s.tasks.get(id)
    else task::initTask

  /// Get promise state (convenience)
  pure def getPromiseState(s: Server, id: str): promise::State =
    getPromise(s, id).state

  /// Get task state (convenience)
  pure def getTaskState(s: Server, id: str): task::State =
    getTask(s, id).state

  // ==========================================================================
  // Effect Translation
  // ==========================================================================

  /// Translate a single promise effect to task operations
  pure def applyPromiseEffect(s: Server, promiseId: str, effect: promise::Effect): Server =
    match effect {
      | CreateTask(workers) =>
          val t = getTask(s, promiseId)
          val result = task::apply({ ...t, id: promiseId }, task::Create(workers))
          { ...s, tasks: s.tasks.put(promiseId, result.task) }
      | QueueTask(taskId) =>
          val t = getTask(s, taskId)
          val result = task::apply(t, task::Enqueue(promiseId))
          { ...s, tasks: s.tasks.put(taskId, result.task) }
    }

  /// Translate promise effects to task operations
  pure def applyPromiseEffects(s: Server, promiseId: str, effects: Set[promise::Effect]): Server =
    effects.fold(s, (server, effect) => applyPromiseEffect(server, promiseId, effect))

  // ==========================================================================
  // Promise Operations
  // ==========================================================================

  /// Create a promise (and task if workers is non-empty via CreateTask effect)
  /// Precondition: promise does not exist (checked at action layer)
  pure def createPromise(s: Server, id: str, workers: Set[str]): Server =
    val p = { ...promise::initPromise, id: id }
    val result = promise::apply(p, promise::Create(workers))
    val promises = s.promises.put(id, result.promise)
    val server = { ...s, promises: promises }
    applyPromiseEffects(server, id, result.effects)

  /// Settle a promise (and resume waiting tasks via QueueTask effects)
  pure def settlePromise(s: Server, id: str): Server =
    val p = getPromise(s, id)
    val result = promise::apply(p, promise::Settle)
    val promises = s.promises.put(id, result.promise)
    val server = { ...s, promises: promises }
    applyPromiseEffects(server, id, result.effects)

  /// Register a callback on a promise
  pure def registerCallback(s: Server, promiseId: str, taskId: str): Server =
    val p = getPromise(s, promiseId)
    val result = promise::apply(p, promise::RegisterCallback(taskId))
    { ...s, promises: s.promises.put(promiseId, result.promise) }

  // ==========================================================================
  // Task Operations
  // ==========================================================================

  /// Claim a task
  pure def claimTask(s: Server, taskId: str, version: int): Server =
    val t = getTask(s, taskId)
    val result = task::apply(t, task::Claim(version))
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  /// Complete a task
  pure def completeTask(s: Server, taskId: str, version: int, createsCallbacks: bool): Server =
    val t = getTask(s, taskId)
    val result = task::apply(t, task::Complete({ version: version, createsCallbacks: createsCallbacks }))
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  /// Drop a task (release claim)
  pure def dropTask(s: Server, taskId: str, version: int): Server =
    val t = getTask(s, taskId)
    val result = task::apply(t, task::Drop(version))
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  // ==========================================================================
  // Compound Operations
  // ==========================================================================

  /// Complete a task and settle its promise (no children)
  pure def completeAndSettle(s: Server, taskId: str, version: int): Server =
    val s1 = completeTask(s, taskId, version, false)
    settlePromise(s1, taskId)

  /// Complete a task with child, create child promise, register callback
  pure def completeWithChild(s: Server, parentTaskId: str, version: int, childPromiseId: str, childWorkers: Set[str]): Server =
    // Complete parent task (goes to Waiting because createsCallbacks=true)
    val s1 = completeTask(s, parentTaskId, version, true)
    // Create child promise with workers (creates child task)
    val s2 = createPromise(s1, childPromiseId, childWorkers)
    // Register callback: when child settles, resume parent
    registerCallback(s2, childPromiseId, parentTaskId)

  // ==========================================================================
  // Properties
  // ==========================================================================

  pure def allPromisesTerminal(s: Server): bool =
    s.promises.keys().forall(id =>
      promise::isTerminal(getPromise(s, id).state)
    )

  pure def allTasksTerminal(s: Server): bool =
    s.tasks.keys().forall(id =>
      task::isTerminal(getTask(s, id))
    )

}
