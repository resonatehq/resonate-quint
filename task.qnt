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
    | Resume(str)   // Resume after callback: id is the promise that triggered callback

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

  /// Complete action parameters
  type CompleteParams = {
    version: int,
    createsCallbacks: bool
  }

  /// Actions that can be applied to a task
  type Action =
    | Create(Set[str])          // workers eligible to work on this task
    | Claim(int)                // version (fencing token)
    | Complete(CompleteParams)  // version + createsCallbacks flag
    | Drop(int)                 // version
    | Enqueue(str)              // promiseId that completed (triggers callback)

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

  /// Check if a promiseId is already current or in pending
  pure def isDuplicate(t: Task, promiseId: str): bool =
    val inCurrent = match t.current {
      | Invoke => false
      | Resume(id) => id == promiseId
    }
    val inPending = t.pending.select(m => match m {
      | Invoke => false
      | Resume(id) => id == promiseId
    }).length() > 0
    inCurrent or inPending

  // ==========================================================================
  // Apply
  // ==========================================================================

  /// Apply an action to a task (idempotent - returns unchanged if not applicable)
  pure def apply(t: Task, a: Action): Result =
    match a {
      | Create(workers) =>
          match t.state {
            | Init => {
                task: { ...t, state: Pending, version: 0, workers: workers, current: Invoke, pending: [] },
                effects: Set(Send(workers))
              }
            | _ => { task: t, effects: Set() }  // already exists
          }

      | Claim(version) =>
          match t.state {
            | Pending =>
                if (version == t.version)
                  { task: { ...t, state: Claimed }, effects: Set() }
                else { task: t, effects: Set() }  // wrong version
            | _ => { task: t, effects: Set() }
          }

      | Complete(params) =>
          match t.state {
            | Claimed =>
                if (params.version != t.version) { task: t, effects: Set() }  // wrong version
                else if (t.pending.length() > 0)
                  { task: { ...t, current: t.pending.head(), pending: t.pending.tail() }, effects: Set() }
                else if (params.createsCallbacks)
                  { task: { ...t, state: Waiting }, effects: Set() }
                else
                  { task: { ...t, state: Completed }, effects: Set() }
            | _ => { task: t, effects: Set() }
          }

      | Drop(version) =>
          match t.state {
            | Claimed =>
                if (version != t.version) { task: t, effects: Set() }  // wrong version
                else {
                  task: { ...t, state: Pending, version: t.version + 1 },
                  effects: Set(Send(t.workers))
                }
            | _ => { task: t, effects: Set() }
          }

      | Enqueue(promiseId) =>
          match t.state {
            | Init => { task: t, effects: Set() }  // task doesn't exist
            | Completed => { task: t, effects: Set() }  // task is done
            | Waiting => {
                // Resume immediately
                task: { ...t, state: Pending, version: t.version + 1, current: Resume(promiseId) },
                effects: Set(Send(t.workers))
              }
            | _ =>
                // Pending or Claimed: buffer the message
                if (isDuplicate(t, promiseId)) { task: t, effects: Set() }
                else { task: { ...t, pending: t.pending.append(Resume(promiseId)) }, effects: Set() }
          }
    }

  // ==========================================================================
  // Properties
  // ==========================================================================

  pure def isTerminal(t: Task): bool =
    t.state == Completed

  pure def isClaimable(t: Task): bool =
    t.state == Pending

}
