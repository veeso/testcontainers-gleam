import gleeunit
import testcontainers_gleam/cassandra
import testcontainers_gleam/ceph
import testcontainers_gleam/container
import testcontainers_gleam/kafka
import testcontainers_gleam/mysql
import testcontainers_gleam/postgres
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

// --- postgres ---

pub fn postgres_new_test() {
  let _config = postgres.new()
  Nil
}

pub fn postgres_builders_test() {
  let _config =
    postgres.new()
    |> postgres.with_image("postgres:16-alpine")
    |> postgres.with_user("myuser")
    |> postgres.with_password("mypass")
    |> postgres.with_database("mydb")
    |> postgres.with_port(5433)
    |> postgres.with_persistent_volume("/tmp/pgdata")
    |> postgres.with_wait_timeout(120_000)
    |> postgres.with_check_image("postgres")
    |> postgres.with_reuse(True)
  Nil
}

pub fn postgres_build_test() {
  let _container = postgres.new() |> postgres.build()
  Nil
}

pub fn postgres_defaults_test() {
  let _image = postgres.default_image()
  let _port = postgres.default_port()
  Nil
}

// --- mysql ---

pub fn mysql_new_test() {
  let _config = mysql.new()
  Nil
}

pub fn mysql_builders_test() {
  let _config =
    mysql.new()
    |> mysql.with_image("mysql:8.0")
    |> mysql.with_user("myuser")
    |> mysql.with_password("mypass")
    |> mysql.with_database("mydb")
    |> mysql.with_port(3307)
    |> mysql.with_persistent_volume("/tmp/mysqldata")
    |> mysql.with_wait_timeout(240_000)
    |> mysql.with_check_image("mysql")
    |> mysql.with_reuse(True)
  Nil
}

pub fn mysql_build_test() {
  let _container = mysql.new() |> mysql.build()
  Nil
}

pub fn mysql_defaults_test() {
  let _image = mysql.default_image()
  let _port = mysql.default_port()
  Nil
}

// --- cassandra ---

pub fn cassandra_new_test() {
  let _config = cassandra.new()
  Nil
}

pub fn cassandra_builders_test() {
  let _config =
    cassandra.new()
    |> cassandra.with_image("cassandra:4.0")
    |> cassandra.with_check_image("cassandra")
    |> cassandra.with_reuse(True)
  Nil
}

pub fn cassandra_build_test() {
  let _container = cassandra.new() |> cassandra.build()
  Nil
}

pub fn cassandra_defaults_test() {
  let _image = cassandra.default_image()
  let _port = cassandra.default_port()
  let _username = cassandra.get_username()
  let _password = cassandra.get_password()
  Nil
}

// --- ceph ---

pub fn ceph_new_test() {
  let _config = ceph.new()
  Nil
}

pub fn ceph_builders_test() {
  let _config =
    ceph.new()
    |> ceph.with_image("quay.io/ceph/demo:latest")
    |> ceph.with_access_key("myaccesskey")
    |> ceph.with_secret_key("mysecretkey")
    |> ceph.with_bucket("mybucket")
    |> ceph.with_port(8081)
    |> ceph.with_wait_timeout(600_000)
    |> ceph.with_check_image("ceph")
    |> ceph.with_reuse(True)
  Nil
}

pub fn ceph_build_test() {
  let _container = ceph.new() |> ceph.build()
  Nil
}

pub fn ceph_defaults_test() {
  let _image = ceph.default_image()
  Nil
}

// --- kafka ---

pub fn kafka_new_test() {
  let _config = kafka.new()
  Nil
}

pub fn kafka_builders_test() {
  let _config =
    kafka.new()
    |> kafka.with_image("confluentinc/cp-kafka:7.4.3")
    |> kafka.with_kafka_port(9093)
    |> kafka.with_broker_port(29_093)
    |> kafka.with_broker_id(2)
    |> kafka.with_wait_timeout(120_000)
    |> kafka.with_topic_partitions(3)
    |> kafka.with_reuse(True)
  Nil
}

pub fn kafka_build_test() {
  let _container = kafka.new() |> kafka.build()
  Nil
}
