// Task State Machine Specification
//
// Pure state machine for individual task transitions.
// apply(task, action) → Result

module task {

  // ==========================================================================
  // Types
  // ==========================================================================

  /// Task states
  type State =
    | Init       // Not yet created
    | Pending
    | Claimed
    | Waiting
    | Completed  // Terminal state

  /// Message to deliver to worker
  type Message =
    | Invoke        // Initial invocation (task ID is implicit)
    | Resume(str)   // Resume after callback: id is the promise that resumed this one

  /// Task record
  type Task = {
    id: str,
    state: State,
    version: int,
    workers: Set[str],      // Workers that can claim this task
    current: Message,
    pending: List[Message]  // Buffer for future work
  }

  /// Initial task state (before creation)
  pure val initTask: Task = {
    id: "",
    state: Init,
    version: 0,
    workers: Set(),
    current: Invoke,
    pending: []
  }

  // ==========================================================================
  // Actions and Effects
  // ==========================================================================

  /// Create parameters
  type CreateParams = {
    msg: Message,
    workers: Set[str]
  }

  /// Actions that can be applied to a task
  type Action =
    | Create(CreateParams)          // create task with message and target workers
    | Claim(int)                    // version (fencing token)
    | Complete(bool)                // true = creates callbacks (-> Waiting), false = done (-> Completed)
    | Drop
    | ResumeTask(str)               // triggered when deps complete

  /// Effects produced by task state transitions
  type Effect =
    | Send(Set[str])    // Task is ready to be sent to these workers

  /// Result of applying an action to a task
  type Result = {
    task: Task,
    effects: Set[Effect]
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  /// Get the ID from a message
  pure def messageId(msg: Message): str =
    match msg {
      | Invoke => ""
      | Resume(id) => id
    }

  /// Check if a message is already current or in pending
  pure def isDuplicate(t: Task, msg: Message): bool =
    val id = messageId(msg)
    if (id == "") false
    else messageId(t.current) == id or t.pending.select(m => messageId(m) == id).length() > 0

  // ==========================================================================
  // Apply
  // ==========================================================================

  /// Apply an action to a task (idempotent - returns unchanged if not applicable)
  pure def apply(t: Task, a: Action): Result =
    match a {
      | Create(params) =>
          match t.state {
            | Init => {
                task: { ...t, state: Pending, version: 0, workers: params.workers, current: params.msg, pending: [] },
                effects: Set(Send(params.workers))
              }
            | _ =>
                match params.msg {
                  | Invoke => { task: t, effects: Set() }
                  | Resume(_) =>
                      if (isDuplicate(t, params.msg)) { task: t, effects: Set() }
                      else { task: { ...t, pending: t.pending.append(params.msg) }, effects: Set() }
                }
          }

      | Claim(version) =>
          match t.state {
            | Pending =>
                if (version == t.version)
                  { task: { ...t, state: Claimed, pending: [] }, effects: Set() }
                else { task: t, effects: Set() }  // wrong version
            | Claimed =>
                { task: t, effects: Set() }  // already claimed, idempotent
            | _ => { task: t, effects: Set() }
          }

      | Complete(createsCallbacks) =>
          match t.state {
            | Claimed =>
                if (t.pending.length() > 0)
                  { task: { ...t, current: t.pending.head(), pending: [] }, effects: Set() }
                else if (createsCallbacks)
                  { task: { ...t, state: Waiting }, effects: Set() }
                else
                  { task: { ...t, state: Completed }, effects: Set() }
            | _ => { task: t, effects: Set() }
          }

      | Drop =>
          match t.state {
            | Claimed => {
                task: { ...t, state: Pending, version: t.version + 1 },
                effects: Set(Send(t.workers))
              }
            | _ => { task: t, effects: Set() }
          }

      | ResumeTask(resumingPromiseId) =>
          match t.state {
            | Waiting => {
                task: { ...t, state: Pending, version: t.version + 1, current: Resume(resumingPromiseId) },
                effects: Set(Send(t.workers))
              }
            | _ => { task: t, effects: Set() }
          }
    }

  // ==========================================================================
  // Effect Helpers
  // ==========================================================================

  /// Extract workers from Send effect (empty if not present)
  pure def getSendWorkers(effects: Set[Effect]): Set[str] =
    effects.fold(Set(), (acc, effect) =>
      match effect {
        | Send(workers) => acc.union(workers)
      }
    )

  // ==========================================================================
  // Properties
  // ==========================================================================

  pure def isTerminal(t: Task): bool =
    t.state == Completed

  pure def isClaimable(t: Task): bool =
    t.state == Pending

}
