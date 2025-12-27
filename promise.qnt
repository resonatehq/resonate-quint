// Promise Specification
//
// Pure state machine for individual promise transitions.
// apply(promise, action) → Result

module promise {

  // ==========================================================================
  // Types
  // ==========================================================================

  type State =
    | Init        // Promise does not exist
    | Pending     // Created, awaiting completion
    | Settled     // Completed (success or failure - we don't distinguish)

  // A promise has an id, state, and callbacks
  // When the promise settles, each callback (task ID) is fired as a Resume
  type Promise = {
    id: str,
    state: State,
    callbacks: Set[str]  // task IDs to Resume when this promise settles
  }

  // Default promise (not created)
  pure val initPromise: Promise = { id: "", state: Init, callbacks: Set() }

  /// Actions that can be applied to a promise
  type Action =
    | Create(Set[str])      // workers: if non-empty, emit CreateTask effect
    | Settle
    | RegisterCallback(str)

  /// Effects produced by promise state transitions
  type Effect =
    | CreateTask(Set[str])  // Create a task for this promise, targeting these workers
    | QueueTask(str)        // Queue a Resume on the specified task

  // ==========================================================================
  // Apply
  // ==========================================================================

  /// Result of applying an action to a promise
  type Result = {
    promise: Promise,
    effects: Set[Effect]
  }

  /// Apply an action to a promise (idempotent - returns unchanged if not applicable)
  pure def apply(p: Promise, a: Action): Result =
    match a {
      | Create(workers) =>
          match p.state {
            | Init => {
                promise: { ...p, state: Pending },
                effects: if (workers.size() > 0) Set(CreateTask(workers)) else Set()
              }
            | _ => { promise: p, effects: Set() }
          }

      | Settle =>
          match p.state {
            | Pending => {
                promise: { ...p, state: Settled, callbacks: Set() },
                effects: p.callbacks.map(id => QueueTask(id))
              }
            | _ => { promise: p, effects: Set() }
          }

      | RegisterCallback(resumeId) =>
          match p.state {
            | Pending => { promise: { ...p, callbacks: p.callbacks.union(Set(resumeId)) }, effects: Set() }
            | _ => { promise: p, effects: Set() }
          }
    }

  // ==========================================================================
  // Effect Helpers
  // ==========================================================================

  /// Extract workers from CreateTask effect (empty if not present)
  pure def getCreateTaskWorkers(effects: Set[Effect]): Set[str] =
    effects.fold(Set(), (acc, effect) =>
      match effect {
        | CreateTask(workers) => acc.union(workers)
        | _ => acc
      }
    )

  /// Extract task IDs from QueueTask effects
  pure def getQueuedTaskIds(effects: Set[Effect]): Set[str] =
    effects.fold(Set(), (acc, effect) =>
      match effect {
        | QueueTask(taskId) => acc.union(Set(taskId))
        | _ => acc
      }
    )

  // ==========================================================================
  // Properties
  // ==========================================================================

  pure def isTerminal(s: State): bool =
    s == Settled

  pure def isPromiseTerminal(p: Promise): bool =
    isTerminal(p.state)

  /// Terminal states are immutable
  pure def terminalImmutable(p: Promise): bool =
    isTerminal(p.state) implies (
      apply(p, Create(Set())).promise.state == p.state and
      apply(p, Create(Set("w1"))).promise.state == p.state and
      apply(p, Settle).promise.state == p.state
    )

}
