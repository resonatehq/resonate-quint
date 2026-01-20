module promise {

  // ==========================================================================
  // Types
  // ==========================================================================

  type State =
    | Init
    | Pending
    | Settled

  type Promise = {
    id: str,
    state: State,
    addresses: Set[str],
    awaiters: Set[str]
  }

  pure def init(id: str): Promise = { id: id, state: Init, addresses: Set(), awaiters: Set() }

  // ==========================================================================
  // Actions and Effects
  // ==========================================================================

  type Action =
    | Create(Set[str])
    | Settle
    | Register(str) // awaiter

  type Effect =
    | Invoke
    | Resume(str) // awaiter

  // ==========================================================================
  // Apply
  // ==========================================================================

  type Result = {
    updated: Promise,
    effects: Set[Effect]
  }

  pure def apply(p: Promise, a: Action): Result =
    match a {
      | Create(addresses) =>
          match p.state {
            // branch: promise.create::init
            | Init => {
                updated: { ...p, state: Pending, addresses: addresses },
                effects: if (addresses.size() > 0) Set(Invoke) else Set()
              }
            // branch: promise.create::noop
            | _ => { updated: p, effects: Set() }
          }

      | Settle =>
          match p.state {
            // branch: promise.settle::pending
            | Pending => {
                updated: { ...p, state: Settled, awaiters: Set() },
                effects: p.awaiters.map(awaiter => Resume(awaiter))
              }
            // branch: promise.settle::noop
            | _ => { updated: p, effects: Set() }
          }

      | Register(awaiter) =>
          match p.state {
            // branch: promise.register::pending
            | Pending => { updated: { ...p, awaiters: p.awaiters.union(Set(awaiter)) }, effects: Set() }
            // branch: promise.register::settled
            | Settled => { updated: p, effects: Set(Resume(awaiter)) }
            // branch: promise.register::noop
            | _ => { updated: p, effects: Set() }
          }
    }

}
