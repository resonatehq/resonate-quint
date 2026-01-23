module task {
  import util.* from "./util"

  // ==========================================================================
  // Types
  // ==========================================================================

  type State =
    | Init
    | Pending
    | Claimed
    | Waiting
    | Completed

  type Task = {
    id: str,
    state: State,
    version: int,
    pending: List[str] // awaited ids
  }

  pure def init(id: str): Task = {
    id: id,
    state: Init,
    version: 0,
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
    | Complete

  type Message = { id: str, version: int }

  // ==========================================================================
  // Apply
  // ==========================================================================

  type Result = {
    updated: Task,
    messages: Set[Message],
    inspect: Inspect
  }

  pure def apply(t: Task, a: Action, now: int): Result =
    match a {

      | EnqueueInvoke =>
          match t.state {
            // branch: task.enqueue-invoke::init
            | Init => {
                updated: { ...t, state: Pending, version: 0, pending: [] },
                messages: Set({ id: t.id, version: 0 }),
                inspect: { id: t.id, branch: "task.enqueue-invoke::init" }
              }
            // branch: task.enqueue-invoke::noop
            // UNREACHABLE: EnqueueInvoke only triggered by CreatePromise effect when state is Init
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.enqueue-invoke::noop" } }
          }

      | EnqueueResume(awaited) =>
          match t.state {
            // branch: task.enqueue-resume::pending
            | Pending =>
                { updated: { ...t, pending: t.pending.append(awaited) }, messages: Set(), inspect: { id: t.id, branch: "task.enqueue-resume::pending" } }
            // branch: task.enqueue-resume::claimed
            | Claimed =>
                { updated: { ...t, pending: t.pending.append(awaited) }, messages: Set(), inspect: { id: t.id, branch: "task.enqueue-resume::claimed" } }
            // branch: task.enqueue-resume::waiting
            | Waiting => {
                updated: { ...t, state: Pending, version: t.version + 1 },
                messages: Set({ id: t.id, version: t.version + 1 }),
                inspect: { id: t.id, branch: "task.enqueue-resume::waiting" }
              }
            // branch: task.enqueue-resume::noop
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.enqueue-resume::noop" } }
          }

      | Acquire(params) =>
          match t.state {
            // branch: task.acquire::pending
            | Pending =>
                // option: task.acquire::pending.match
                if (params.version == t.version)
                  { updated: { ...t, state: Claimed, pending: [] }, messages: Set(), inspect: { id: t.id, branch: "task.acquire::pending.match" } }
                // option: task.acquire::pending.mismatch
                else
                  { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.acquire::pending.mismatch" } }
            // branch: task.acquire::noop
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.acquire::noop" } }
          }

      | Release(params) =>
          match t.state {
            // branch: task.release::claimed
            | Claimed =>
                // option: task.release::claimed.mismatch
                if (params.version != t.version)
                  { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.release::claimed.mismatch" } }
                // option: task.release::claimed.match
                else
                  {
                    updated: { ...t, state: Pending, version: t.version + 1 },
                    messages: Set({ id: t.id, version: t.version + 1 }),
                    inspect: { id: t.id, branch: "task.release::claimed.match" }
                  }
            // branch: task.release::noop
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.release::noop" } }
          }

      | Suspend(params) =>
          match t.state {
            // branch: task.suspend::claimed
            | Claimed =>
                // option: task.suspend::claimed.mismatch
                // UNREACHABLE: Server guard checks version match before calling
                if (params.version != t.version)
                  { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.suspend::claimed.mismatch" } }
                else
                  // Combine pending with new resumes
                  val nowPending = t.pending.concat(params.resumes)
                  // option: task.suspend::claimed.match.reject
                  if (nowPending.length() > 0)
                    { updated: { ...t, pending: [] }, messages: Set(), inspect: { id: t.id, branch: "task.suspend::claimed.match.reject" } }
                  // option: task.suspend::claimed.match.accept
                  else
                    { updated: { ...t, state: Waiting }, messages: Set(), inspect: { id: t.id, branch: "task.suspend::claimed.match.accept" } }
            // branch: task.suspend::noop
            // UNREACHABLE: Server guard checks task is owned before calling
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.suspend::noop" } }
          }

      | Fulfill(params) =>
          match t.state {
            // branch: task.fulfill::claimed
            | Claimed =>
                // option: task.fulfill::claimed.mismatch
                // UNREACHABLE: Server guard checks version match before calling
                if (params.version != t.version)
                  { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.fulfill::claimed.mismatch" } }
                // option: task.fulfill::claimed.match
                else
                  { updated: { ...t, state: Completed }, messages: Set(), inspect: { id: t.id, branch: "task.fulfill::claimed.match" } }
            // branch: task.fulfill::noop
            // UNREACHABLE: Server guard checks task is owned before calling
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.fulfill::noop" } }
          }

      | Complete =>
          match t.state {
            // branch: task.complete::pending
            | Pending =>
                { updated: { ...t, state: Completed }, messages: Set(), inspect: { id: t.id, branch: "task.complete::pending" } }
            // branch: task.complete::claimed
            | Claimed =>
                { updated: { ...t, state: Completed }, messages: Set(), inspect: { id: t.id, branch: "task.complete::claimed" } }
            // branch: task.complete::waiting
            | Waiting =>
                { updated: { ...t, state: Completed }, messages: Set(), inspect: { id: t.id, branch: "task.complete::waiting" } }
            // branch: task.complete::noop
            | _ => { updated: t, messages: Set(), inspect: { id: t.id, branch: "task.complete::noop" } }
          }

    }

}
