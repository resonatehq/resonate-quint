module server {
  import util.* from "./util"
  import promise from "./promise"
  import task from "./task"

  // ==========================================================================
  // Types
  // ==========================================================================

  /// Server state
  type Server = {
    now: int,
    timers: Set[str],
    messages: Set[task::Message],
    promises: str -> promise::Promise,
    tasks: str -> task::Task,
    traces: List[Inspect]
  }

  /// Empty server
  pure val init: Server = {
    now: 0,
    timers: Set(),
    messages: Set(),
    promises: Map(),
    tasks: Map(),
    traces: []
  }

  // ==========================================================================
  // Actions
  // ==========================================================================

  /// Coordination actions
  type CAction =
    | CreatePromise({ id: str, target: Option, timeout: int })
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
    | Tick(int) // new time

  // ==========================================================================
  // Helpers
  // ==========================================================================

  /// Get a value from a map, returning a default if not found
  pure def getWithDefault(m: str -> a, key: str, default: a): a =
    if (m.keys().contains(key)) m.get(key)
    else default

  /// Put a value in a map only if condition is true
  pure def setWithCondition(m: str -> a, key: str, value: a, condition: bool): str -> a =
    if (condition) m.put(key, value)
    else m

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
          val r0 = promise::apply(p0, promise::Create({ target: params.target, timeout: params.timeout }), s0.now)
          val s1 = { ...s0, promises: s0.promises.put(params.id, r0.updated), traces: s0.traces.append(r0.inspect) }
          // Process effects (timer management)
          val s2 = r0.effects.fold(s1, (acc, e) =>
            match e {
              | SetTimer => { ...acc, timers: acc.timers.union(Set(params.id)) }
              | DelTimer => acc // unreachable: Create never produces DelTimer
            }
          )
          // Process changes (task management)
          r0.changes.fold(s2, (acc, c) =>
            match c {
              | DidCreate =>
                  match params.target {
                    | Some(_) =>
                        val t0 = getWithDefault(acc.tasks, params.id, task::init(params.id))
                        val r1 = task::apply(t0, task::EnqueueInvoke, acc.now)
                        { ...acc, messages: acc.messages.union(r1.messages), tasks: acc.tasks.put(params.id, r1.updated), traces: acc.traces.append(r1.inspect) }
                    | None => acc
                  }
              | DidSettle =>
                  match params.target {
                    | Some(_) =>
                        val t0 = getWithDefault(acc.tasks, params.id, task::init(params.id))
                        val r1 = task::apply(t0, task::Complete, acc.now)
                        { ...acc, tasks: setWithCondition(acc.tasks, params.id, r1.updated, r1.updated.state != task::Init), traces: acc.traces.append(r1.inspect) }
                    | None => acc
                  }
              | DidTrigger(_) => acc // unreachable: Create never produces Trigger
            }
          )

      | SettlePromise(id) =>
          val p0 = getWithDefault(s0.promises, id, promise::init(id))
          val r0 = promise::apply(p0, promise::Settle, s0.now)
          val s1 = { ...s0, promises: setWithCondition(s0.promises, id, r0.updated, r0.updated.state != promise::Init), traces: s0.traces.append(r0.inspect) }
          // Process effects (timer management)
          val s2 = r0.effects.fold(s1, (acc, e) =>
            match e {
              | SetTimer => acc // unreachable: Settle never produces SetTimer
              | DelTimer => { ...acc, timers: acc.timers.exclude(Set(id)) }
            }
          )
          // Process changes (task management)
          r0.changes.fold(s2, (acc, c) =>
            match c {
              | DidCreate => acc // unreachable: Settle never produces Created
              | DidSettle =>
                  match p0.target {
                    | Some(_) =>
                        val t0 = getWithDefault(acc.tasks, id, task::init(id))
                        val r1 = task::apply(t0, task::Complete, acc.now)
                        { ...acc, tasks: setWithCondition(acc.tasks, id, r1.updated, r1.updated.state != task::Init), traces: acc.traces.append(r1.inspect) }
                    | None => acc
                  }
              | DidTrigger(awaiter) =>
                  val t0 = getWithDefault(acc.tasks, awaiter, task::init(awaiter))
                  val r1 = task::apply(t0, task::EnqueueResume(id), acc.now)
                  { ...acc, messages: acc.messages.union(r1.messages), tasks: setWithCondition(acc.tasks, awaiter, r1.updated, r1.updated.state != task::Init), traces: acc.traces.append(r1.inspect) }
            }
          )

      | RegisterCallback(params) =>
          val p0 = getWithDefault(s0.promises, params.awaited, promise::init(params.awaited))
          val r0 = promise::apply(p0, promise::Register(params.awaiter), s0.now)
          val s1 = { ...s0, promises: setWithCondition(s0.promises, params.awaited, r0.updated, r0.updated.state != promise::Init), traces: s0.traces.append(r0.inspect) }
          // Process effects (timer management)
          val s2 = r0.effects.fold(s1, (acc, e) =>
            match e {
              | SetTimer => acc // unreachable: Register never produces SetTimer
              | DelTimer => { ...acc, timers: acc.timers.exclude(Set(params.awaited)) }
            }
          )
          // Process changes (task management)
          r0.changes.fold(s2, (acc, c) =>
            match c {
              | DidCreate => acc // unreachable: Register never produces Created
              | DidSettle =>
                  match p0.target {
                    | Some(_) =>
                        val t0 = getWithDefault(acc.tasks, params.awaited, task::init(params.awaited))
                        val r1 = task::apply(t0, task::Complete, acc.now)
                        { ...acc, tasks: setWithCondition(acc.tasks, params.awaited, r1.updated, r1.updated.state != task::Init), traces: acc.traces.append(r1.inspect) }
                    | None => acc
                  }
              | DidTrigger(awaiter) =>
                  val t0 = getWithDefault(acc.tasks, awaiter, task::init(awaiter))
                  val r1 = task::apply(t0, task::EnqueueResume(params.awaited), acc.now)
                  { ...acc, messages: acc.messages.union(r1.messages), tasks: setWithCondition(acc.tasks, awaiter, r1.updated, r1.updated.state != task::Init), traces: acc.traces.append(r1.inspect) }
            }
          )
    }

  // ==========================================================================
  // Apply DAction
  // ==========================================================================

  pure def applyDAction(s0: Server, a: DAction): Server =
    match a {
      | AcquireTask(params) =>
          val t0 = getWithDefault(s0.tasks, params.id, task::init(params.id))
          val r0 = task::apply(t0, task::Acquire({ version: params.version }), s0.now)
          { ...s0, tasks: setWithCondition(s0.tasks, params.id, r0.updated, r0.updated.state != task::Init), traces: s0.traces.append(r0.inspect) }

      | ReleaseTask(params) =>
          val t0 = getWithDefault(s0.tasks, params.id, task::init(params.id))
          val r0 = task::apply(t0, task::Release({ version: params.version }), s0.now)
          val m0 = r0.messages
          { ...s0, messages: s0.messages.union(m0), tasks: setWithCondition(s0.tasks, params.id, r0.updated, r0.updated.state != task::Init), traces: s0.traces.append(r0.inspect) }

      | FulfillTask(params) =>
          // precondition: params.inner must be SettlePromise
          if (not(taskOwnedWith(s0, params.id, params.version)))
            s0
          else
            // Apply the settle action
            val s1 = applyCAction(s0, params.inner)
            // Then fulfill the task
            val t0 = getWithDefault(s1.tasks, params.id, task::init(params.id))
            val r0 = task::apply(t0, task::Fulfill({ version: params.version }), s1.now)
            { ...s1, tasks: s1.tasks.put(params.id, r0.updated), traces: s1.traces.append(r0.inspect) }

      | SuspendTask(params) =>
          // precondition: task must be owned, all awaited promises must exist
          val allAwaitedExist = params.inner.foldl(true, (ok, act) =>
            ok and match act {
              | RegisterCallback(reg) =>
                  val p = getWithDefault(s0.promises, reg.awaited, promise::init(reg.awaited))
                  p.state != promise::Init
              | _ => true
            }
          )
          if (not(taskOwnedWith(s0, params.id, params.version)) or not(allAwaitedExist))
            s0
          else
            // Apply all register callbacks and collect immediate triggers
            val r0 = params.inner.foldl({ server: s0, triggers: [], traces: [] }, (acc, act) =>
              match act {
                | RegisterCallback(reg) =>
                    val p0 = getWithDefault(acc.server.promises, reg.awaited, promise::init(reg.awaited))
                    val r1 = promise::apply(p0, promise::Register(reg.awaiter), acc.server.now)
                    val s1 = { ...acc.server, promises: setWithCondition(acc.server.promises, reg.awaited, r1.updated, r1.updated.state != promise::Init) }
                    // Process effects (timer management)
                    val s2 = r1.effects.fold(s1, (inner, e) =>
                      match e {
                        | SetTimer => inner // unreachable
                        | DelTimer => { ...inner, timers: inner.timers.exclude(Set(reg.awaited)) }
                      }
                    )
                    // Process changes: collect triggers, handle Settled and CompleteTask
                    val result = r1.changes.fold({ srv: s2, tgs: acc.triggers, ts: acc.traces.append(r1.inspect) }, (inner, c) =>
                      match c {
                        | DidCreate => inner // unreachable
                        | DidSettle =>
                            match p0.target {
                              | Some(_) =>
                                  val t0 = getWithDefault(inner.srv.tasks, reg.awaited, task::init(reg.awaited))
                                  val r2 = task::apply(t0, task::Complete, inner.srv.now)
                                  { ...inner, srv: { ...inner.srv, tasks: setWithCondition(inner.srv.tasks, reg.awaited, r2.updated, r2.updated.state != task::Init) }, ts: inner.ts.append(r2.inspect) }
                              | None => inner
                            }
                        | DidTrigger(_) => { ...inner, tgs: inner.tgs.append(reg.awaited) }
                      }
                    )
                    { server: result.srv, triggers: result.tgs, traces: result.ts }
                | _ => acc
              }
            )
            // Suspend the task with the collected triggers as resumes
            val t0 = getWithDefault(r0.server.tasks, params.id, task::init(params.id))
            val r1 = task::apply(t0, task::Suspend({ version: params.version, resumes: r0.triggers }), r0.server.now)
            { ...r0.server, tasks: r0.server.tasks.put(params.id, r1.updated), traces: r0.server.traces.concat(r0.traces).append(r1.inspect) }

      | FenceTask(params) =>
          // precondition: params.inner must be CreatePromise or SettlePromise
          if (taskOwnedWith(s0, params.id, params.version))
            applyCAction(s0, params.inner)
          else
            s0
    }

  // ==========================================================================
  // Apply Tick
  // ==========================================================================

  pure def applyTick(s0: Server, newTime: int): Server =
    val s1 = { ...s0, now: newTime }
    // Find expired timers and settle them
    s1.timers.fold(s1, (acc, id) =>
      val p = getWithDefault(acc.promises, id, promise::init(id))
      if (p.state == promise::Pending and acc.now >= p.timeout)
        applyCAction(acc, SettlePromise(id))
      else
        acc
    )

  // ==========================================================================
  // Apply
  // ==========================================================================

  /// Apply an action to the server
  pure def apply(s0: Server, a: Action): Server =
    match a {
      | C(cAction) => applyCAction(s0, cAction)
      | D(dAction) => applyDAction(s0, dAction)
      | Tick(newTime) => applyTick(s0, newTime)
    }

}
