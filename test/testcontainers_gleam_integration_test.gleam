import envoy
import gleam/string
import gleeunit/should
import testcontainers_gleam
import testcontainers_gleam/cassandra
import testcontainers_gleam/ceph
import testcontainers_gleam/container
import testcontainers_gleam/kafka
import testcontainers_gleam/mysql
import testcontainers_gleam/postgres
import testcontainers_gleam/redis
import testcontainers_gleam/wait_strategy

fn integration_test(test_fn: fn() -> Nil) {
  case envoy.get("TESTCONTAINERS_INTEGRATION_TESTS") {
    Ok(_) -> test_fn()
    Error(_) -> Nil
  }
}

// --- start / stop lifecycle ---

pub fn start_and_stop_container_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()

    // container_id returns a non-empty string
    let id = container.container_id(running)
    id |> string.is_empty() |> should.be_false()

    // mapped_port returns a valid host port
    let host_port = container.mapped_port(running, 6379) |> should.be_ok()
    { host_port > 0 } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- start_link idempotency ---

pub fn start_link_is_idempotent_test() {
  integration_test(fn() {
    // First start_container call ensures the GenServer is already running.
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    // Calling start_link when already running returns Ok(Nil),
    // exercising the already_started branch.
    testcontainers_gleam.start_link() |> should.be_ok()
    testcontainers_gleam.start_link() |> should.be_ok()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- mapped_port edge case ---

pub fn mapped_port_returns_error_for_unexposed_port_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    container.mapped_port(running, 9999) |> should.be_error()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- builder options ---

pub fn start_container_with_environment_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_environment("REDIS_PASSWORD", "test_secret")

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

pub fn start_container_with_labels_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_label("project", "testcontainers_gleam")
      |> container.with_label("env", "test")

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

pub fn start_container_with_cmd_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_cmd(["redis-server", "--appendonly", "yes"])

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

pub fn start_container_with_multiple_ports_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_ports([6379, 6380])

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    let port_a = container.mapped_port(running, 6379) |> should.be_ok()
    let port_b = container.mapped_port(running, 6380) |> should.be_ok()
    { port_a != port_b } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

pub fn start_container_with_auto_remove_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_auto_remove(True)

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- wait strategies ---

pub fn start_container_with_port_wait_strategy_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_waiting_strategy(wait_strategy.port(
        "0.0.0.0",
        6379,
        30_000,
        1000,
      ))

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

pub fn start_container_with_log_wait_strategy_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_waiting_strategy(wait_strategy.log(
        "Ready to accept connections",
        30_000,
        1000,
      ))

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

pub fn start_container_with_command_wait_strategy_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_waiting_strategy(wait_strategy.command(
        ["redis-cli", "ping"],
        30_000,
        1000,
      ))

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- failed wait strategy ---

pub fn start_container_with_failing_log_wait_strategy_returns_error_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_waiting_strategy(wait_strategy.log(
        "this log line will never appear",
        3000,
        500,
      ))

    testcontainers_gleam.start_container(c)
    |> should.be_error()
    |> should.equal(testcontainers_gleam.WaitStrategyFailed)
  })
}

pub fn start_container_with_failing_command_wait_strategy_returns_error_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_waiting_strategy(wait_strategy.command(
        ["false"],
        3000,
        500,
      ))

    testcontainers_gleam.start_container(c)
    |> should.be_error()
    |> should.equal(testcontainers_gleam.WaitStrategyFailed)
  })
}

// --- fixed port ---

pub fn start_container_with_fixed_port_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_fixed_port(6379, 46_379)

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    let host_port = container.mapped_port(running, 6379) |> should.be_ok()
    host_port |> should.equal(46_379)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- error path ---

pub fn start_container_with_nonexistent_image_never_pull_returns_http_error_test() {
  integration_test(fn() {
    let c =
      container.new("nonexistent_image_99999:latest")
      |> container.with_exposed_port(1234)
      |> container.with_pull_policy(container.NeverPull)

    let error =
      testcontainers_gleam.start_container(c)
      |> should.be_error()

    // Docker returns an error when creating a container with a missing image
    error |> should.equal(testcontainers_gleam.FailedToCreateContainer)
  })
}

pub fn start_container_with_nonexistent_image_always_pull_returns_error_test() {
  integration_test(fn() {
    let c =
      container.new("nonexistent_image_99999:latest")
      |> container.with_exposed_port(1234)
      |> container.with_pull_policy(container.AlwaysPull)

    testcontainers_gleam.start_container(c)
    |> should.be_error()
    |> should.equal(testcontainers_gleam.FailedToPullImage)
  })
}

// --- full builder chain integration ---

pub fn start_container_with_full_builder_chain_test() {
  integration_test(fn() {
    let c =
      container.new("redis:7.4-alpine")
      |> container.with_exposed_port(6379)
      |> container.with_environment("REDIS_PASSWORD", "secret")
      |> container.with_cmd(["redis-server", "--appendonly", "yes"])
      |> container.with_label("project", "testcontainers_gleam")
      |> container.with_auto_remove(True)
      |> container.with_waiting_strategy(wait_strategy.log(
        "Ready to accept connections",
        30_000,
        1000,
      ))

    let running = testcontainers_gleam.start_container(c) |> should.be_ok()
    let id = container.container_id(running)

    id |> string.is_empty() |> should.be_false()
    container.mapped_port(running, 6379) |> should.be_ok()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- redis module ---

pub fn redis_module_start_and_stop_test() {
  integration_test(fn() {
    let config = redis.new()
    let built = redis.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = redis.port(running)
    { p > 0 } |> should.be_true()

    let url = redis.connection_url(running)
    url |> string.contains("redis://") |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- postgres module ---

pub fn postgres_module_start_and_stop_test() {
  integration_test(fn() {
    let config = postgres.new()
    let built = postgres.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = postgres.port(running)
    { p > 0 } |> should.be_true()

    let _params = postgres.connection_parameters(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- mysql module ---

pub fn mysql_module_start_and_stop_test() {
  integration_test(fn() {
    let config = mysql.new()
    let built = mysql.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = mysql.port(running)
    { p > 0 } |> should.be_true()

    let _params = mysql.connection_parameters(running)

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- cassandra module ---

pub fn cassandra_module_start_and_stop_test() {
  integration_test(fn() {
    let config = cassandra.new()
    let built = cassandra.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = cassandra.port(running)
    { p > 0 } |> should.be_true()

    let uri = cassandra.connection_uri(running)
    uri |> string.is_empty() |> should.be_false()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- ceph module ---

pub fn ceph_module_start_and_stop_test() {
  integration_test(fn() {
    let config = ceph.new()
    let built = ceph.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = ceph.port(running)
    { p > 0 } |> should.be_true()

    let url = ceph.connection_url(running)
    url |> string.is_empty() |> should.be_false()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}

// --- kafka module ---

pub fn kafka_module_start_and_stop_test() {
  integration_test(fn() {
    let config = kafka.new()
    let built = kafka.build(config)
    let running = testcontainers_gleam.start_container(built) |> should.be_ok()

    // Must call after_start for Kafka
    kafka.after_start(config, running) |> should.be_ok()

    let id = container.container_id(running)
    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
