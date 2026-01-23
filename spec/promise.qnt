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
    | SetTimer
    | DelTimer

  type Change =
    | DidCreate
    | DidSettle
    | DidTrigger(str) // awaiter

  // ==========================================================================
  // Apply
  // ==========================================================================

  type Result = {
    updated: Promise,
    effects: Set[Effect],
    changes: Set[Change],
    inspect: Inspect
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
                    effects: Set(SetTimer),
                    changes: Set(DidCreate),
                    inspect: { id: p.id, branch: "promise.create::init.current" }
                  }
                // option: promise.create::init.expired
                else
                  {
                    updated: { ...p, state: Settled, target: params.target, timeout: params.timeout },
                    effects: Set(),
                    changes: Set(DidCreate, DidSettle),
                    inspect: { id: p.id, branch: "promise.create::init.expired" }
                  }
            // branch: promise.create::noop
            | _ => { updated: p, effects: Set(), changes: Set(), inspect: { id: p.id, branch: "promise.create::noop" } }
          }

      | Settle =>
          match p.state {
            // branch: promise.settle::pending
            | Pending =>
                {
                  updated: { ...p, state: Settled, awaiters: Set() },
                  effects: Set(DelTimer),
                  changes: p.awaiters.map(awaiter => DidTrigger(awaiter)).union(Set(DidSettle)),
                  inspect: { id: p.id, branch: "promise.settle::pending" }
                }
            // branch: promise.settle::noop
            | _ => { updated: p, effects: Set(), changes: Set(), inspect: { id: p.id, branch: "promise.settle::noop" } }
          }

      | Register(awaiter) =>
          match p.state {
            // branch: promise.register::pending
            | Pending =>
                // option: promise.register::pending.current
                if (now < p.timeout)
                  {
                    updated: { ...p, awaiters: p.awaiters.union(Set(awaiter)) },
                    effects: Set(),
                    changes: Set(),
                    inspect: { id: p.id, branch: "promise.register::pending.current" }
                  }
                // option: promise.register::pending.expired
                else
                  {
                    updated: { ...p, state: Settled, awaiters: Set() },
                    effects: Set(DelTimer),
                    changes: p.awaiters.union(Set(awaiter)).map(a => DidTrigger(a)).union(Set(DidSettle)),
                    inspect: { id: p.id, branch: "promise.register::pending.expired" }
                  }
            // branch: promise.register::settled
            | Settled => { updated: p, effects: Set(), changes: Set(DidTrigger(awaiter)), inspect: { id: p.id, branch: "promise.register::settled" } }
            // branch: promise.register::noop
            | _ => { updated: p, effects: Set(), changes: Set(), inspect: { id: p.id, branch: "promise.register::noop" } }
          }
    }

}