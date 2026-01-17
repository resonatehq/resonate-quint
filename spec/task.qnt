module task {

  // ==========================================================================
  // Types
  // ==========================================================================

  type State =
    | Init
    | Pending
    | Claimed
    | Waiting
    | Completed

  type Command =
    | Invoke
    | Resume(str) // awaited

  type Task = {
    id: str,
    state: State,
    version: int,
    current: Command,
    pending: List[Command]
  }

  pure def init(id: str): Task = {
    id: id,
    state: Init,
    version: 0,
    current: Invoke,
    pending: []
  }

  // ==========================================================================
  // Actions and Effects
  // ==========================================================================

  type Action =
    | EnqueueInvoke
    | EnqueueResume(str) // awaited
    | Acquire({ version: int })
    | Fulfill({ version: int })
    | Suspend({ version: int, resumes: List[str] }) // resumes = list of awaited
    | Release({ version: int })

  type Message = { id: str, version: int }

  // ==========================================================================
  // Apply
  // ==========================================================================

  type Result = {
    updated: Task,
    messages: Set[Message]
  }

  pure def apply(t: Task, a: Action): Result =
    match a {

      | EnqueueInvoke =>
          match t.state {
            // branch: task.enqueue_invoke.init
            | Init => {
                updated: { ...t, state: Pending, version: 0, current: Invoke, pending: [] },
                messages: Set({ id: t.id, version: 0 })
              }
            // branch: task.enqueue_invoke.noop
            // UNREACHABLE: EnqueueInvoke only triggered by CreatePromise effect when state is Init
            | _ => { updated: t, messages: Set() }
          }

      | EnqueueResume(awaited) =>
          match t.state {
            // branch: task.enqueue_resume.init
            | Init =>
                { updated: t, messages: Set() }
            // branch: task.enqueue_resume.pending
            | Pending =>
                { updated: { ...t, pending: t.pending.append(Resume(awaited)) }, messages: Set() }
            // branch: task.enqueue_resume.claimed
            | Claimed =>
                { updated: { ...t, pending: t.pending.append(Resume(awaited)) }, messages: Set() }
            // branch: task.enqueue_resume.waiting
            | Waiting => {
                updated: { ...t, state: Pending, version: t.version + 1, current: Resume(awaited) },
                messages: Set({ id: t.id, version: t.version + 1 })
              }
            // branch: task.enqueue_resume.completed
            | Completed =>
                { updated: t, messages: Set() }
          }

      | Acquire(params) =>
          match t.state {
            | Pending =>
                if (params.version == t.version)
                  // branch: task.acquire.pending.match
                  { updated: { ...t, state: Claimed, pending: [] }, messages: Set() }
                else
                  // branch: task.acquire.pending.mismatch
                  { updated: t, messages: Set() }
            // branch: task.acquire.noop
            | _ => { updated: t, messages: Set() }
          }

      | Release(params) =>
          match t.state {
            | Claimed =>
                if (params.version != t.version)
                  // branch: task.release.claimed.mismatch
                  { updated: t, messages: Set() }
                else
                  // branch: task.release.claimed.match
                  {
                    updated: { ...t, state: Pending, version: t.version + 1 },
                    messages: Set({ id: t.id, version: t.version + 1 })
                  }
            // branch: task.release.noop
            | _ => { updated: t, messages: Set() }
          }

      | Suspend(params) =>
          match t.state {
            | Claimed =>
                if (params.version != t.version)
                  // branch: task.suspend.claimed.mismatch
                  // UNREACHABLE: Server guard checks version match before calling
                  { updated: t, messages: Set() }
                else
                  // First add all resumes to pending
                  val nowPending = t.pending.concat(params.resumes.foldl([], (acc, id) => acc.append(Resume(id))))
                  if (nowPending.length() > 0)
                    // branch: task.suspend.claimed.match.resume
                    { updated: { ...t, current: nowPending.head(), pending: nowPending.tail() }, messages: Set() }
                  else
                    // branch: task.suspend.claimed.match.wait
                    { updated: { ...t, state: Waiting }, messages: Set() }
            // branch: task.suspend.noop
            // UNREACHABLE: Server guard checks task is owned before calling
            | _ => { updated: t, messages: Set() }
          }

      | Fulfill(params) =>
          match t.state {
            | Claimed =>
                if (params.version != t.version)
                  // branch: task.fulfill.claimed.mismatch
                  // UNREACHABLE: Server guard checks version match before calling
                  { updated: t, messages: Set() }
                else
                  // branch: task.fulfill.claimed.match
                  { updated: { ...t, state: Completed }, messages: Set() }
            // branch: task.fulfill.noop
            // UNREACHABLE: Server guard checks task is owned before calling
            | _ => { updated: t, messages: Set() }
          }

    }

}
