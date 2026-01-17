module server {
  import promise from "./promise"
  import task from "./task"

  // ==========================================================================
  // Types
  // ==========================================================================

  /// Server state
  type Server = {
    messages: Set[task::Message],
    promises: str -> promise::Promise,
    tasks: str -> task::Task
  }

  /// Empty server
  pure val init: Server = {
    messages: Set(),
    promises: Map(),
    tasks: Map()
  }

  // ==========================================================================
  // Actions
  // ==========================================================================

  /// Coordination actions
  type CAction =
    | CreatePromise({ id: str, addresses: Set[str] })
    | SettlePromise(str)
    | RegisterCallback({ awaited: str, awaiter: str })

  /// Distribution actions
  type DAction =
    | AcquireTask({ id: str, version: int })
    | ReleaseTask({ id: str, version: int })
    | FulfillTask({ id: str, version: int, inner: CAction })
    | SuspendTask({ id: str, version: int, inner: List[CAction] })
    | FenceTask({ id: str, version: int, inner: CAction })

  /// Server actions
  type Action =
    | C(CAction)
    | D(DAction)

  // ==========================================================================
  // Helpers
  // ==========================================================================

  /// Get a value from a map, returning a default if not found
  pure def getWithDefault(m: str -> a, key: str, default: a): a =
    if (m.keys().contains(key)) m.get(key)
    else default

  /// Check if task is owned with given version
  pure def taskOwnedWith(s: Server, id: str, version: int): bool =
    val t = getWithDefault(s.tasks, id, task::init(id))
    t.state == task::Claimed and t.version == version

  // ==========================================================================
  // Apply CAction
  // ==========================================================================

  pure def applyCAction(s0: Server, a: CAction): Server =
    match a {
      | CreatePromise(params) =>
          val p0 = getWithDefault(s0.promises, params.id, promise::init(params.id))
          val r0 = promise::apply(p0, promise::Create(params.addresses))
          val s1 = { ...s0, promises: s0.promises.put(params.id, r0.updated) }
          r0.effects.fold(s1, (acc, e) =>
            match e {
              | Invoke =>
                  val t0 = getWithDefault(acc.tasks, params.id, task::init(params.id))
                  val r1 = task::apply(t0, task::EnqueueInvoke)
                  val m0 = r1.messages
                  { ...acc, messages: acc.messages.union(m0), tasks: acc.tasks.put(params.id, r1.updated) }
              | Resume(_) => acc // unreachable: Create never produces Resume
            }
          )

      | SettlePromise(id) =>
          val p0 = getWithDefault(s0.promises, id, promise::init(id))
          val r0 = promise::apply(p0, promise::Settle)
          val s1 = { ...s0, promises: s0.promises.put(id, r0.updated) }
          r0.effects.fold(s1, (acc, e) =>
            match e {
              | Invoke => acc // unreachable: Settle never produces Invoke
              | Resume(awaiter) =>
                  val t0 = getWithDefault(acc.tasks, awaiter, task::init(awaiter))
                  val r1 = task::apply(t0, task::EnqueueResume(id))
                  val m0 = r1.messages
                  { ...acc, messages: acc.messages.union(m0), tasks: acc.tasks.put(awaiter, r1.updated) }
            }
          )

      | RegisterCallback(params) =>
          val p0 = getWithDefault(s0.promises, params.awaited, promise::init(params.awaited))
          val r0 = promise::apply(p0, promise::Register(params.awaiter))
          { ...s0, promises: s0.promises.put(params.awaited, r0.updated) }
    }

  // ==========================================================================
  // Apply DAction
  // ==========================================================================

  pure def applyDAction(s0: Server, a: DAction): Server =
    match a {
      | AcquireTask(params) =>
          val t0 = getWithDefault(s0.tasks, params.id, task::init(params.id))
          val r0 = task::apply(t0, task::Acquire({ version: params.version }))
          { ...s0, tasks: s0.tasks.put(params.id, r0.updated) }

      | ReleaseTask(params) =>
          val t0 = getWithDefault(s0.tasks, params.id, task::init(params.id))
          val r0 = task::apply(t0, task::Release({ version: params.version }))
          val m0 = r0.messages
          { ...s0, messages: s0.messages.union(m0), tasks: s0.tasks.put(params.id, r0.updated) }

      | FulfillTask(params) =>
          // precondition: params.inner must be SettlePromise
          if (not(taskOwnedWith(s0, params.id, params.version)))
            s0
          else
            // Apply the settle action
            val s1 = applyCAction(s0, params.inner)
            // Then fulfill the task
            val t0 = getWithDefault(s1.tasks, params.id, task::init(params.id))
            val r0 = task::apply(t0, task::Fulfill({ version: params.version }))
            { ...s1, tasks: s1.tasks.put(params.id, r0.updated) }

      | SuspendTask(params) =>
          // precondition: all elements of params.inner must be RegisterCallback
          if (not(taskOwnedWith(s0, params.id, params.version)))
            s0
          else
            // Apply all register callbacks and collect immediate resumes
            val r0 = params.inner.foldl({ server: s0, resumes: [] }, (acc, act) =>
              match act {
                | RegisterCallback(reg) =>
                    val p0 = getWithDefault(acc.server.promises, reg.awaited, promise::init(reg.awaited))
                    val r1 = promise::apply(p0, promise::Register(reg.awaiter))
                    val s1 = { ...acc.server, promises: acc.server.promises.put(reg.awaited, r1.updated) }
                    // Check if this returned a Resume effect (promise was settled)
                    val resumes0 = r1.effects.fold(acc.resumes, (rs, e) =>
                      match e {
                        | Resume(_) => rs.append(reg.awaited)
                        | _ => rs
                      }
                    )
                    { server: s1, resumes: resumes0 }
                | _ => acc
              }
            )
            // Suspend the task with the collected resumes
            val t0 = getWithDefault(r0.server.tasks, params.id, task::init(params.id))
            val r1 = task::apply(t0, task::Suspend({ version: params.version, resumes: r0.resumes }))
            { ...r0.server, tasks: r0.server.tasks.put(params.id, r1.updated) }

      | FenceTask(params) =>
          // precondition: params.inner must be CreatePromise or SettlePromise
          if (taskOwnedWith(s0, params.id, params.version))
            applyCAction(s0, params.inner)
          else
            s0
    }

  // ==========================================================================
  // Apply
  // ==========================================================================

  /// Apply an action to the server
  pure def apply(s0: Server, a: Action): Server =
    match a {
      | C(cAction) => applyCAction(s0, cAction)
      | D(dAction) => applyDAction(s0, dAction)
    }

}
