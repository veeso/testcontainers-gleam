# Pre-Built Image Modules Design

## Summary

Add 9 Gleam modules wrapping the Elixir testcontainers pre-built container modules (v1.13.3). Each module provides typed configuration builders, a `build()` function that produces a `container.Container`, and accessors for running containers. The existing `start_container`/`stop_container` lifecycle is unchanged.

## Scope

Wrap all Elixir testcontainers image modules available in v1.13.3, **except** SeleniumContainer (marked WIP/unstable). ToxiproxyContainer is excluded because it only exists in the unreleased 2.0.0-rc.

| Module | Gleam file | Elixir module |
|--------|-----------|---------------|
| Redis | `redis.gleam` | `Testcontainers.RedisContainer` |
| PostgreSQL | `postgres.gleam` | `Testcontainers.PostgresContainer` |
| MySQL | `mysql.gleam` | `Testcontainers.MySqlContainer` |
| Cassandra | `cassandra.gleam` | `Testcontainers.CassandraContainer` |
| Ceph | `ceph.gleam` | `Testcontainers.CephContainer` |
| Kafka | `kafka.gleam` | `Testcontainers.KafkaContainer` |
| EMQX | `emqx.gleam` | `Testcontainers.EmqxContainer` |
| MinIO | `minio.gleam` | `Testcontainers.MinioContainer` |
| RabbitMQ | `rabbitmq.gleam` | `Testcontainers.RabbitMQContainer` |

## Architecture

### Approach

Direct FFI per module (Approach A). Each Gleam module directly calls its Elixir counterpart via `@external` annotations. No shared builder abstraction.

### Pattern

Every module follows the same structure:

1. **Opaque type** for the Elixir config struct (e.g. `pub type RedisConfig`)
2. **`new()`** constructor via FFI to `Elixir.Testcontainers.XxxContainer.new/0`
3. **`with_*()`** builder functions via FFI
4. **`build()`** converts config to `container.Container` via `Testcontainers.ContainerBuilder.build/1`
5. **Accessors** like `port()`, `connection_url()` via FFI on running containers

### User Flow

```gleam
import testcontainers_gleam
import testcontainers_gleam/redis

// Configure
let config = redis.new() |> redis.with_password("secret")

// Build to Container
let container = redis.build(config)

// Start (same lifecycle as always)
let assert Ok(running) = testcontainers_gleam.start_container(container)

// Use accessors
let url = redis.connection_url(running)

// Stop
let id = container.container_id(running)
let assert Ok(Nil) = testcontainers_gleam.stop_container(id)
```

### FFI Layer

Extend `testcontainers_gleam_ffi.erl` with:

```erlang
build_container(Builder) ->
    'Elixir.Testcontainers.ContainerBuilder':build(Builder).

after_start(Builder, Container) ->
    #{conn := Conn} = sys:get_state('Elixir.Testcontainers'),
    case 'Elixir.Testcontainers.ContainerBuilder':after_start(Builder, Container, Conn) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
```

`build_container/1` dispatches through the Elixir protocol for any builder type. `after_start/2` extracts the Docker HTTP connection from the GenServer state and calls the protocol's `after_start/3`.

**Key discovery:** `Testcontainers.start_container/1` already accepts `ContainerBuilder` structs directly (not just `Container`). It internally calls `build()`, starts the container, then calls `after_start()`. However, our Gleam `start_container` is typed to accept `Container`, so the builder protocol mirroring approach uses explicit `build()` + `start_container()` + manual `after_start()` when needed.

### after_start

Only modules that need post-start hooks expose `after_start(config, container)`. Currently only Kafka needs it (uploads startup script with advertised listener configuration). For Kafka, `after_start` is **mandatory** — without it, the container hangs. Other modules' `after_start` implementations are no-ops.

## Per-Module API

### redis.gleam

Note: The v1.13.3 `RedisContainer` does NOT have a `with_password` function. The struct has no password field.

```
pub type RedisConfig
pub fn new() -> RedisConfig
pub fn with_image(config, String) -> RedisConfig
pub fn with_port(config, Int) -> RedisConfig
pub fn with_wait_timeout(config, Int) -> RedisConfig
pub fn with_check_image(config, String) -> RedisConfig
pub fn with_reuse(config, Bool) -> RedisConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn port(container) -> Int
pub fn connection_url(container) -> String
```

### postgres.gleam

```
pub type PostgresConfig
pub fn new() -> PostgresConfig
pub fn with_image(config, String) -> PostgresConfig
pub fn with_user(config, String) -> PostgresConfig
pub fn with_password(config, String) -> PostgresConfig
pub fn with_database(config, String) -> PostgresConfig
pub fn with_port(config, Int) -> PostgresConfig
pub fn with_persistent_volume(config, String) -> PostgresConfig
pub fn with_wait_timeout(config, Int) -> PostgresConfig
pub fn with_check_image(config, String) -> PostgresConfig
pub fn with_reuse(config, Bool) -> PostgresConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn default_port() -> Int
pub fn port(container) -> Int
pub fn connection_parameters(container) -> List(#(String, String))
```

### mysql.gleam

Same shape as postgres: user/password/database/port/persistent_volume + build + accessors.

```
pub type MysqlConfig
pub fn new() -> MysqlConfig
pub fn with_image(config, String) -> MysqlConfig
pub fn with_user(config, String) -> MysqlConfig
pub fn with_password(config, String) -> MysqlConfig
pub fn with_database(config, String) -> MysqlConfig
pub fn with_port(config, Int) -> MysqlConfig
pub fn with_persistent_volume(config, String) -> MysqlConfig
pub fn with_wait_timeout(config, Int) -> MysqlConfig
pub fn with_check_image(config, String) -> MysqlConfig
pub fn with_reuse(config, Bool) -> MysqlConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn default_port() -> Int
pub fn port(container) -> Int
pub fn connection_parameters(container) -> List(#(String, String))
```

### cassandra.gleam

```
pub type CassandraConfig
pub fn new() -> CassandraConfig
pub fn with_image(config, String) -> CassandraConfig
pub fn with_check_image(config, String) -> CassandraConfig
pub fn with_reuse(config, Bool) -> CassandraConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn default_port() -> Int
pub fn get_username() -> String
pub fn get_password() -> String
pub fn port(container) -> Int
pub fn connection_uri(container) -> String
```

### ceph.gleam

```
pub type CephConfig
pub fn new() -> CephConfig
pub fn with_image(config, String) -> CephConfig
pub fn with_access_key(config, String) -> CephConfig
pub fn with_secret_key(config, String) -> CephConfig
pub fn with_bucket(config, String) -> CephConfig
pub fn with_port(config, Int) -> CephConfig
pub fn with_wait_timeout(config, Int) -> CephConfig
pub fn with_check_image(config, String) -> CephConfig
pub fn with_reuse(config, Bool) -> CephConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn port(container) -> Int
pub fn connection_url(container) -> String
```

### kafka.gleam

Note: Based on installed v1.13.3 API. Uses `with_topic_partitions` (integer) instead of `with_topics` (list of strings, 2.0-only). Kafka's `after_start` is **mandatory** — it uploads the startup script that configures advertised listeners with the correct host-mapped port. Without it, the Kafka container will hang waiting for the script.

```
pub type KafkaConfig
pub fn new() -> KafkaConfig
pub fn with_image(config, String) -> KafkaConfig
pub fn with_kafka_port(config, Int) -> KafkaConfig
pub fn with_broker_port(config, Int) -> KafkaConfig
pub fn with_broker_id(config, Int) -> KafkaConfig
pub fn with_consensus_strategy(config, ConsensusStrategy) -> KafkaConfig
pub fn with_wait_timeout(config, Int) -> KafkaConfig
pub fn with_topic_partitions(config, Int) -> KafkaConfig
pub fn with_reuse(config, Bool) -> KafkaConfig
pub fn build(config) -> Container
pub fn after_start(config, container) -> Result(Nil, ContainerError)
pub fn port(container) -> Int
```

### emqx.gleam

```
pub type EmqxConfig
pub fn new() -> EmqxConfig
pub fn with_image(config, String) -> EmqxConfig
pub fn with_ports(config, Int, Int, Int, Int, Int) -> EmqxConfig
pub fn with_check_image(config, String) -> EmqxConfig
pub fn with_reuse(config, Bool) -> EmqxConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn mqtt_port(container) -> Int
```

### minio.gleam

```
pub type MinioConfig
pub fn new() -> MinioConfig
pub fn with_reuse(config, Bool) -> MinioConfig
pub fn build(config) -> Container
pub fn get_username() -> String
pub fn get_password() -> String
pub fn port(container) -> Int
pub fn connection_url(container) -> String
```

### rabbitmq.gleam

```
pub type RabbitMQConfig
pub fn new() -> RabbitMQConfig
pub fn with_image(config, String) -> RabbitMQConfig
pub fn with_port(config, Int) -> RabbitMQConfig
pub fn with_username(config, String) -> RabbitMQConfig
pub fn with_password(config, String) -> RabbitMQConfig
pub fn with_virtual_host(config, String) -> RabbitMQConfig
pub fn with_cmd(config, List(String)) -> RabbitMQConfig
pub fn with_wait_timeout(config, Int) -> RabbitMQConfig
pub fn with_check_image(config, String) -> RabbitMQConfig
pub fn with_reuse(config, Bool) -> RabbitMQConfig
pub fn build(config) -> Container
pub fn default_image() -> String
pub fn default_port() -> Int
pub fn port(container) -> Int
pub fn connection_url(container) -> String
```

## Testing

### Unit Tests

For each module, test that `new()` + all `with_*()` builders + `build()` can be called without Docker. These verify the Elixir structs are created and converted correctly.

### Integration Tests

For each module, test the full lifecycle gated behind `TESTCONTAINERS_INTEGRATION_TESTS`:

1. `new()` + builders → `build()` → `start_container()` → verify accessors → `stop_container()`
2. For Kafka: additionally test `after_start()` with topic creation

Expected containers and approximate startup times:
- Redis: ~2s
- PostgreSQL: ~5s
- MySQL: ~30s (3min timeout)
- Cassandra: ~15s
- Ceph: ~60s (5min timeout)
- Kafka: ~10s
- EMQX: ~10s
- MinIO: ~5s
- RabbitMQ: ~10s

All integration tests follow the existing pattern in `testcontainers_gleam_integration_test.gleam`.
