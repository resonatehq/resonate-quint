# Quint Specification

A formal specification of the Resonate durable promise and task system using [Quint](https://quint-lang.org/).

## System Model

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                          World                               │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                       Server                         │    │
│  │  ┌─────────────────┐    ┌─────────────────┐         │    │
│  │  │    Promises     │    │     Tasks       │         │    │
│  │  │  Map[id, Promise]    │  Map[id, Task]  │         │    │
│  │  └─────────────────┘    └─────────────────┘         │    │
│  └─────────────────────────────────────────────────────┘    │
│                              │                               │
│                              │ Send(workers)                 │
│                              ▼                               │
│  ┌─────────────────────────────────────────────────────┐    │
│  │              Message Soup (Advertisements)           │    │
│  │                 Set[{taskId, version}]               │    │
│  └─────────────────────────────────────────────────────┘    │
│                              │                               │
│            ┌─────────────────┼─────────────────┐            │
│            ▼                 ▼                 ▼            │
│      ┌──────────┐      ┌──────────┐      ┌──────────┐      │
│      │ Worker 1 │      │ Worker 2 │      │ Worker N │      │
│      └──────────┘      └──────────┘      └──────────┘      │
└─────────────────────────────────────────────────────────────┘
```

### Components

**Server** - Single server that owns all promises and tasks. Responsible for:
- Creating and settling promises
- Creating tasks when promises need execution
- Managing task lifecycle (claim, complete, drop)
- Processing callbacks when promises settle

**Workers** - Multiple workers identified by string IDs. Workers:
- Receive task advertisements via message soup
- Claim tasks using version-based fencing
- Execute tasks and report completion
- Can create child promises during execution

**Message Soup** - Add-only set of advertisements. When a task becomes pending:
- Server emits `Send(workers)` effect with target worker IDs
- Advertisement `{taskId, version}` added to message soup
- Workers pick advertisements and attempt to claim

## Specification Design

### Three-Layer Architecture

```
┌────────────────────────────────────────────────────────────┐
│  world.qnt - Action Layer                                   │
│  - State variables (var state: Protocol)                    │
│  - Actions that modify state                                │
│  - Non-deterministic step for model checking                │
└────────────────────────────────────────────────────────────┘
                              │
                              │ calls
                              ▼
┌────────────────────────────────────────────────────────────┐
│  server.qnt - Collection Layer (pure)                       │
│  - Server type with promise/task maps                       │
│  - Pure functions: createPromise, settlePromise, etc.       │
│  - Effect processing (CreateTask → task creation)           │
└────────────────────────────────────────────────────────────┘
                              │
                              │ calls
                              ▼
┌────────────────────────────────────────────────────────────┐
│  promise.qnt / task.qnt - Entity Layer (pure)               │
│  - State machines for individual entities                   │
│  - apply(entity, action) → Result                           │
│  - Effects returned, not executed                           │
└────────────────────────────────────────────────────────────┘
```

### Entity Layer: Pure State Machines

**promise.qnt** - Individual promise transitions:
```
State: Init → Pending → Settled

Actions:
  Create(workers)      - Create promise, emit CreateTask if workers non-empty
  Settle               - Complete promise, emit QueueTask for each callback
  RegisterCallback(id) - Register task to resume when this promise settles

Effects:
  CreateTask(workers)  - Create a task targeting these workers
  QueueTask(taskId)    - Resume the specified task
```

**task.qnt** - Individual task transitions:
```
State: Init → Pending ⇄ Claimed → Completed
                ↑          ↓
                └── Waiting ←┘

Actions:
  Create({msg, workers}) - Create task with message and target workers
  Claim(version)         - Claim task with fencing token
  Complete(hasChildren)  - Complete execution (→ Waiting if has children)
  Drop                   - Release claim (version incremented)
  ResumeTask(promiseId)  - Resume from Waiting when dependency settles

Effects:
  Send(workers) - Task ready to be sent to these workers
```

### Collection Layer: Server

**server.qnt** - Manages maps of promises and tasks:
- `createPromise(server, id, workers)` - Creates promise, handles CreateTask effect
- `settlePromise(server, id)` - Settles promise, handles QueueTask effects
- `claimTask(server, taskId, version)` - Worker claims task
- `completeTask(server, taskId, hasChildren)` - Worker completes task
- `completeWithChild(server, parentId, childId, workers)` - Compound operation

All functions are pure - they take a Server and return a new Server.

### Action Layer: World

**world.qnt** - Stateful protocol model:
- `var state: Protocol` - Server + message soup
- Actions call server functions and update state
- Non-deterministic `step` action for model checking

## Key Design Decisions

### Effects Pattern

State transitions return effects rather than executing side effects directly:

```quint
pure def apply(p: Promise, a: Action): Result =
  match a {
    | Create(workers) =>
        match p.state {
          | Init => {
              promise: { ...p, state: Pending },
              effects: if (workers.size() > 0) Set(CreateTask(workers)) else Set()
            }
          ...
        }
  }
```

Benefits:
- Pure functions are easy to test
- Effects are explicit and traceable
- Server layer decides how to process effects

### Worker Targeting

Workers are identified by string IDs. Task creation specifies which workers can handle it:

```
promise::Create(Set("worker1", "worker2"))
  → CreateTask(Set("worker1", "worker2"))
    → task::Create({msg: Invoke, workers: Set("worker1", "worker2")})
      → Send(Set("worker1", "worker2"))
```

### Version-Based Fencing

Tasks have a version number that increments when released:
- `Claim(version)` only succeeds if versions match
- `Drop` increments version, making old claims invalid
- Prevents stale workers from claiming already-reassigned tasks

### Add-Only Message Soup

Advertisements are never removed from the message soup:
- Simplifies the model (no garbage collection)
- Stale advertisements fail at claim time (version mismatch)
- Set semantics prevent duplicates

## Files

| File | Purpose |
|------|---------|
| `promise.qnt` | Promise state machine (pure) |
| `task.qnt` | Task state machine (pure) |
| `server.qnt` | Collection management (pure) |
| `world.qnt` | Protocol model with state |
| `promise_test.qnt` | Promise unit tests |
| `world_test.qnt` | Server and protocol tests |

## Running

```bash
# Typecheck
quint typecheck spec/world.qnt

# Run tests
quint test spec/promise_test.qnt
quint test spec/world_test.qnt

# Model check (if temporal properties defined)
quint run spec/world.qnt --init=doInit --step=step
```
