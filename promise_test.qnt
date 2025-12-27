// Promise Tests
//
// Tests for the pure apply function.

module promise_test {

  import promise.* from "./promise"

  // Test helper: create a promise with a given state
  pure def promiseWithState(s: State): Promise = { id: "test", state: s, callbacks: Set() }

  // Test workers
  pure val testWorkers: Set[str] = Set("w1", "w2")

  // ==========================================================================
  // Tests - State Transitions
  // ==========================================================================

  pure val testCreateOnInit: bool =
    val result = apply(initPromise, Create(Set()))
    result.promise.state == Pending

  pure val testCreateOnPending: bool =
    val result = apply(promiseWithState(Pending), Create(Set()))
    result.promise.state == Pending  // idempotent

  pure val testCreateOnSettled: bool =
    val result = apply(promiseWithState(Settled), Create(Set()))
    result.promise.state == Settled  // idempotent

  pure val testSettleOnPending: bool =
    val result = apply(promiseWithState(Pending), Settle)
    result.promise.state == Settled

  pure val testSettleOnSettled: bool =
    val result = apply(promiseWithState(Settled), Settle)
    result.promise.state == Settled  // idempotent

  pure val testSettleOnInit: bool =
    val result = apply(initPromise, Settle)
    result.promise.state == Init  // no change

  // ==========================================================================
  // Tests - Callbacks
  // ==========================================================================

  pure val testRegisterCallback: bool =
    val p = promiseWithState(Pending)
    val result = apply(p, RegisterCallback("task1"))
    result.promise.callbacks.contains("task1")

  pure val testRegisterCallbackIdempotent: bool =
    val p = promiseWithState(Pending)
    val r1 = apply(p, RegisterCallback("task1"))
    val r2 = apply(r1.promise, RegisterCallback("task1"))
    r2.promise.callbacks.size() == 1

  pure val testRegisterMultipleCallbacks: bool =
    val p = promiseWithState(Pending)
    val r1 = apply(p, RegisterCallback("task1"))
    val r2 = apply(r1.promise, RegisterCallback("task2"))
    r2.promise.callbacks.size() == 2 and
    r2.promise.callbacks.contains("task1") and
    r2.promise.callbacks.contains("task2")

  pure val testRegisterCallbackOnInit: bool =
    val result = apply(initPromise, RegisterCallback("task1"))
    result.promise.callbacks.size() == 0  // no change on Init

  pure val testRegisterCallbackOnSettled: bool =
    val result = apply(promiseWithState(Settled), RegisterCallback("task1"))
    result.promise.callbacks.size() == 0  // no change on Settled

  // ==========================================================================
  // Tests - Effects
  // ==========================================================================

  pure val testCreateWithWorkersEmitsEffect: bool =
    val result = apply(initPromise, Create(testWorkers))
    result.promise.state == Pending and
    result.effects == Set(CreateTask(testWorkers))

  pure val testCreateWithoutWorkersNoEffect: bool =
    val result = apply(initPromise, Create(Set()))
    result.promise.state == Pending and
    result.effects == Set()

  pure val testSettleFiresCallbacks: bool =
    val p: Promise = { id: "test", state: Pending, callbacks: Set("task1", "task2") }
    val result = apply(p, Settle)
    result.effects == Set(QueueTask("task1"), QueueTask("task2")) and
    result.promise.callbacks == Set()  // cleared

  pure val testSettleNoCallbacksNoEffects: bool =
    val result = apply(promiseWithState(Pending), Settle)
    result.effects == Set()

  // ==========================================================================
  // All Tests
  // ==========================================================================

  pure val allTestsPass: bool =
    // State transitions
    testCreateOnInit and
    testCreateOnPending and
    testCreateOnSettled and
    testSettleOnPending and
    testSettleOnSettled and
    testSettleOnInit and
    // Callbacks
    testRegisterCallback and
    testRegisterCallbackIdempotent and
    testRegisterMultipleCallbacks and
    testRegisterCallbackOnInit and
    testRegisterCallbackOnSettled and
    // Effects
    testCreateWithWorkersEmitsEffect and
    testCreateWithoutWorkersNoEffect and
    testSettleFiresCallbacks and
    testSettleNoCallbacksNoEffects

}
