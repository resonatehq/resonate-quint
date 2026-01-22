module promise {
  import util.* from "./util"

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
    target: Option,
    timeout: int,
    awaiters: Set[str]
  }

  pure def init(id: str): Promise = { id: id, state: Init, timeout: 0, target: None, awaiters: Set() }

  // ==========================================================================
  // Actions and Effects
  // ==========================================================================

  type Action =
    | Create({ target: Option, timeout: int })
    | Settle
    | Register(str) // awaiter

  type Effect =
    | Invoke
    | Resume(str) // awaiter
    | SetTimer

  // ==========================================================================
  // Apply
  // ==========================================================================

  type Result = {
    updated: Promise,
    effects: Set[Effect]
  }

  pure def apply(p: Promise, a: Action, now: int): Result =
    match a {
      | Create(params) =>
          match p.state {
            // branch: promise.create::init
            | Init =>
                // option: promise.create::init.current
                if (now < params.timeout)
                  {
                    updated: { ...p, state: Pending, target: params.target, timeout: params.timeout },
                    effects: match params.target {
                      // option: promise.create::init.current.target
                      | Some(_) => Set(Invoke, SetTimer)
                      // option: promise.create::init.current.no-target
                      | None => Set(SetTimer)
                    }
                  }
                // option: promise.create::init.expired
                else
                  { updated: { ...p, state: Settled, target: params.target, timeout: params.timeout }, effects: Set() }
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
            | Pending =>
                // option: promise.register::pending.current
                if (now < p.timeout)
                  { updated: { ...p, awaiters: p.awaiters.union(Set(awaiter)) }, effects: Set() }
                // option: promise.register::pending.expired
                else
                  {
                    updated: { ...p, state: Settled, awaiters: Set() },
                    effects: p.awaiters.union(Set(awaiter)).map(a => Resume(a))
                  }
            // branch: promise.register::settled
            | Settled => { updated: p, effects: Set(Resume(awaiter)) }
            // branch: promise.register::noop
            | _ => { updated: p, effects: Set() }
          }
    }

}