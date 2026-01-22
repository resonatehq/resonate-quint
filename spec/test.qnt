module test {
  import util.* from "./util"
  import promise from "./promise"
  import task from "./task"
  import server from "./server"

  // Test 1: Create promise with future timeout - should go to Pending with timer
  run createPromiseCurrentTest = {
    val s0 = server::init
    val s1 = server::apply(s0, server::C(server::CreatePromise({ id: "p1", target: Some("addr1"), timeout: 100 })))
    all {
      assert(s1.promises.get("p1").state == promise::Pending),
      assert(s1.timers.contains("p1")),
      assert(s1.promises.get("p1").timeout == 100)
    }
  }

  // Test 2: Create promise with expired timeout - should go to Settled immediately, no timer
  run createPromiseExpiredTest = {
    val s0 = { ...server::init, now: 100 }
    val s1 = server::apply(s0, server::C(server::CreatePromise({ id: "p1", target: Some("addr1"), timeout: 50 })))
    all {
      assert(s1.promises.get("p1").state == promise::Settled),
      assert(not(s1.timers.contains("p1")))
    }
  }

  // Test 3: Settle promise - should remove timer
  run settleRemovesTimerTest = {
    val s0 = server::init
    val s1 = server::apply(s0, server::C(server::CreatePromise({ id: "p1", target: Some("addr1"), timeout: 100 })))
    val s2 = server::apply(s1, server::C(server::SettlePromise("p1")))
    all {
      assert(s2.promises.get("p1").state == promise::Settled),
      assert(not(s2.timers.contains("p1")))
    }
  }

  // Test 4: Tick expires promise
  run tickExpiresPromiseTest = {
    val s0 = server::init
    val s1 = server::apply(s0, server::C(server::CreatePromise({ id: "p1", target: Some("addr1"), timeout: 100 })))
    val s2 = server::apply(s1, server::Tick(100))
    all {
      assert(s2.promises.get("p1").state == promise::Settled),
      assert(not(s2.timers.contains("p1"))),
      assert(s2.now == 100)
    }
  }

  // Test 5: Register on expired pending promise settles it and removes timer
  run registerOnExpiredTest = {
    val s0 = server::init
    val s1 = server::apply(s0, server::C(server::CreatePromise({ id: "p1", target: Some("addr1"), timeout: 100 })))
    // Also create a task for the awaiter
    val s2 = server::apply(s1, server::C(server::CreatePromise({ id: "p2", target: Some("addr2"), timeout: 200 })))
    // Advance time past p1's timeout
    val s3 = { ...s2, now: 150 }
    // Register p2 as awaiter on p1 (which is now expired)
    val s4 = server::apply(s3, server::C(server::RegisterCallback({ awaited: "p1", awaiter: "p2" })))
    all {
      assert(s4.promises.get("p1").state == promise::Settled),
      assert(not(s4.timers.contains("p1")))
    }
  }
}
