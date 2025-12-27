// Server Specification
//
// Manages collections of promises and tasks.
// Calls pure apply functions from promise.qnt and task.qnt.
// Processes effects to coordinate between promises and tasks.

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
  // Promise Operations
  // ==========================================================================

  /// Create a promise (and task if workers is non-empty via CreateTask effect)
  pure def createPromise(s: Server, id: str, workers: Set[str]): Server =
    val current = if (s.promises.keys().contains(id))
      s.promises.get(id)
    else
      { id: id, state: promise::Init, callbacks: Set() }
    val result = promise::apply(current, promise::Create(workers))
    val newPromises = s.promises.put(id, result.promise)
    // Handle CreateTask effect - extract workers and create task
    val taskWorkers = promise::getCreateTaskWorkers(result.effects)
    val newTasks = if (taskWorkers.size() > 0) {
      val taskResult = task::apply({ ...task::initTask, id: id }, task::Create({ msg: task::Invoke, workers: taskWorkers }))
      s.tasks.put(id, taskResult.task)
    } else {
      s.tasks
    }
    { promises: newPromises, tasks: newTasks }

  /// Settle a promise (and resume waiting tasks via QueueTask effects)
  pure def settlePromise(s: Server, id: str): Server =
    val current = getPromise(s, id)
    val result = promise::apply(current, promise::Settle)
    val newPromises = s.promises.put(id, result.promise)
    // Handle QueueTask effects - resume waiting tasks
    val taskIds = promise::getQueuedTaskIds(result.effects)
    val newTasks = taskIds.fold(s.tasks, (tasks, taskId) =>
      val t = if (tasks.keys().contains(taskId)) tasks.get(taskId) else task::initTask
      val taskResult = task::apply(t, task::ResumeTask(id))
      tasks.put(taskId, taskResult.task)
    )
    { promises: newPromises, tasks: newTasks }

  /// Register a callback on a promise
  pure def registerCallback(s: Server, promiseId: str, taskId: str): Server =
    val current = getPromise(s, promiseId)
    val result = promise::apply(current, promise::RegisterCallback(taskId))
    { ...s, promises: s.promises.put(promiseId, result.promise) }

  // ==========================================================================
  // Task Operations
  // ==========================================================================

  /// Claim a task
  pure def claimTask(s: Server, taskId: str, version: int): Server =
    val current = getTask(s, taskId)
    val result = task::apply(current, task::Claim(version))
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  /// Complete a task
  pure def completeTask(s: Server, taskId: str, createsCallbacks: bool): Server =
    val current = getTask(s, taskId)
    val result = task::apply(current, task::Complete(createsCallbacks))
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  /// Drop a task (release claim)
  pure def dropTask(s: Server, taskId: str): Server =
    val current = getTask(s, taskId)
    val result = task::apply(current, task::Drop)
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  /// Create a task
  pure def createTask(s: Server, taskId: str, msg: task::Message, workers: Set[str]): Server =
    val current = if (s.tasks.keys().contains(taskId))
      s.tasks.get(taskId)
    else
      { ...task::initTask, id: taskId }
    val result = task::apply(current, task::Create({ msg: msg, workers: workers }))
    { ...s, tasks: s.tasks.put(taskId, result.task) }

  // ==========================================================================
  // Compound Operations
  // ==========================================================================

  /// Complete a task and settle its promise (no children)
  pure def completeAndSettle(s: Server, taskId: str): Server =
    val s1 = completeTask(s, taskId, false)
    settlePromise(s1, taskId)

  /// Complete a task with child, create child promise, register callback
  pure def completeWithChild(s: Server, parentTaskId: str, childPromiseId: str, childWorkers: Set[str]): Server =
    // Complete parent task (goes to Waiting because createsCallbacks=true)
    val s1 = completeTask(s, parentTaskId, true)
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
