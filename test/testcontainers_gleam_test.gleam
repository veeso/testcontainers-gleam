import gleeunit
import testcontainers_gleam/container
import testcontainers_gleam/redis
import testcontainers_gleam/wait_strategy

pub fn main() {
  gleeunit.main()
}

// --- container.new ---

pub fn container_new_test() {
  let _container = container.new("redis:7.4-alpine")
  Nil
}

// --- container builder functions ---

pub fn with_exposed_port_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_exposed_port(6379)
  Nil
}

pub fn with_exposed_ports_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_exposed_ports([6379, 6380])
  Nil
}

pub fn with_fixed_port_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_fixed_port(6379, 16_379)
  Nil
}

pub fn with_environment_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_environment("REDIS_PASSWORD", "secret")
  Nil
}

pub fn with_cmd_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_cmd(["redis-server", "--appendonly", "yes"])
  Nil
}

pub fn with_bind_mount_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_bind_mount("/tmp/data", "/data", "rw")
  Nil
}

pub fn with_bind_volume_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_bind_volume("my-vol", "/data", False)
  Nil
}

pub fn with_label_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_label("project", "test")
  Nil
}

pub fn with_waiting_strategy_port_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_waiting_strategy(wait_strategy.port(
      "0.0.0.0",
      6379,
      5000,
      500,
    ))
  Nil
}

pub fn with_waiting_strategy_log_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_waiting_strategy(wait_strategy.log(
      "Ready to accept connections",
      10_000,
      1000,
    ))
  Nil
}

pub fn with_waiting_strategy_command_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_waiting_strategy(wait_strategy.command(
      ["redis-cli", "ping"],
      10_000,
      1000,
    ))
  Nil
}

pub fn with_auto_remove_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_auto_remove(True)
  Nil
}

pub fn with_reuse_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_reuse(True)
  Nil
}

pub fn with_network_mode_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_network_mode("bridge")
  Nil
}

pub fn with_auth_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_auth("user", "password")
  Nil
}

pub fn with_check_image_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_check_image("redis:.*")
  Nil
}

// --- pull policy ---

pub fn with_pull_policy_always_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_pull_policy(container.AlwaysPull)
  Nil
}

pub fn with_pull_policy_never_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_pull_policy(container.NeverPull)
  Nil
}

// --- wait strategy constructors ---

pub fn port_wait_strategy_test() {
  let _strategy = wait_strategy.port("0.0.0.0", 5432, 5000, 500)
  Nil
}

pub fn log_wait_strategy_test() {
  let _strategy = wait_strategy.log("pattern.*match", 10_000, 1000)
  Nil
}

pub fn command_wait_strategy_test() {
  let _strategy = wait_strategy.command(["pg_isready"], 10_000, 1000)
  Nil
}

// --- full builder chain ---

pub fn full_builder_chain_test() {
  let _container =
    container.new("redis:7.4-alpine")
    |> container.with_exposed_port(6379)
    |> container.with_exposed_ports([6380])
    |> container.with_environment("REDIS_PASSWORD", "secret")
    |> container.with_cmd(["redis-server", "--appendonly", "yes"])
    |> container.with_label("project", "test")
    |> container.with_waiting_strategy(wait_strategy.log(
      "Ready to accept connections",
      30_000,
      1000,
    ))
    |> container.with_auto_remove(True)
    |> container.with_network_mode("bridge")
    |> container.with_pull_policy(container.AlwaysPull)
  Nil
}

// --- redis ---

pub fn redis_new_test() {
  let _config = redis.new()
  Nil
}

pub fn redis_with_image_test() {
  let _config = redis.new() |> redis.with_image("redis:6-alpine")
  Nil
}

pub fn redis_with_port_test() {
  let _config = redis.new() |> redis.with_port(6380)
  Nil
}

pub fn redis_with_wait_timeout_test() {
  let _config = redis.new() |> redis.with_wait_timeout(30_000)
  Nil
}

pub fn redis_with_check_image_test() {
  let _config = redis.new() |> redis.with_check_image("redis")
  Nil
}

pub fn redis_with_reuse_test() {
  let _config = redis.new() |> redis.with_reuse(True)
  Nil
}

pub fn redis_build_test() {
  let _container = redis.new() |> redis.build()
  Nil
}

pub fn redis_default_image_test() {
  let _image = redis.default_image()
  Nil
}
