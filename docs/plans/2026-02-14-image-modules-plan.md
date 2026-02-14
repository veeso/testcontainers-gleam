# Pre-Built Image Modules Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add 9 Gleam wrapper modules for the Elixir testcontainers pre-built container types (Redis, Postgres, MySQL, Cassandra, Ceph, Kafka, EMQX, MinIO, RabbitMQ).

**Architecture:** Each module is a thin FFI wrapper using `@external` annotations to call the corresponding Elixir module. A shared FFI function (`build_container/1`) dispatches through the Elixir `ContainerBuilder` protocol. Each module exposes an opaque config type, `new()` + `with_*()` builders, `build()` → `Container`, and accessors for running containers.

**Tech Stack:** Gleam, Erlang FFI (`@external`), Elixir testcontainers v1.13.3, gleeunit

**Important references:**
- Design doc: `docs/plans/2026-02-14-image-modules-design.md`
- Existing container module: `src/testcontainers_gleam/container.gleam`
- Existing FFI: `src/internal/testcontainers_ffi.gleam` and `src/internal/testcontainers_gleam_ffi.erl`
- Existing tests: `test/testcontainers_gleam_test.gleam` and `test/testcontainers_gleam_integration_test.gleam`
- Elixir source: `build/packages/testcontainers/lib/container/` (for reference)

**Conventions:**
- Use `gleam-conventions` skill before writing any `.gleam` file
- All public functions get `////` module docs and `///` function docs
- Opaque types use `pub type XxxConfig` (not `pub opaque type`)
- Accessors that extract host-mapped ports return `Int` (they panic on missing ports, consistent with Elixir)
- Unit tests: verify struct creation and builder chaining (no Docker needed)
- Integration tests: gated behind `TESTCONTAINERS_INTEGRATION_TESTS` env var

---

### Task 1: Extend Erlang FFI with build_container and after_start

**Files:**
- Modify: `src/internal/testcontainers_gleam_ffi.erl`
- Modify: `src/internal/testcontainers_ffi.gleam`

**Step 1: Add `build_container/1` and `after_start/2` to the Erlang FFI**

In `src/internal/testcontainers_gleam_ffi.erl`, add two new exported functions:

```erlang
-module(testcontainers_gleam_ffi).
-export([start_container/1, build_container/1, after_start/2]).

%% Existing function (unchanged)
start_container(Container) ->
    case 'Elixir.Testcontainers':start_container(Container) of
        {ok, Running} -> {ok, Running};
        {error, Reason} -> {error, Reason};
        {error, Reason, _WaitStrategy} -> {error, Reason}
    end.

%% Dispatch ContainerBuilder.build/1 protocol to convert a builder config
%% (e.g. %RedisContainer{}) into a low-level %Container{} struct.
build_container(Builder) ->
    'Elixir.Testcontainers.ContainerBuilder':build(Builder).

%% Call ContainerBuilder.after_start/3 for builders that need post-start hooks
%% (currently only Kafka). Extracts the Docker HTTP connection from the
%% Testcontainers GenServer state.
after_start(Builder, Container) ->
    #{conn := Conn} = sys:get_state('Elixir.Testcontainers'),
    case 'Elixir.Testcontainers.ContainerBuilder':after_start(Builder, Container, Conn) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
```

**Step 2: Add Gleam FFI declarations**

In `src/internal/testcontainers_ffi.gleam`, add:

```gleam
import gleam/dynamic.{type Dynamic}
import testcontainers_gleam/container.{type Container}

/// Build a Container from any ContainerBuilder config struct.
@external(erlang, "testcontainers_gleam_ffi", "build_container")
pub fn build_container(builder: anything) -> Container

/// Call after_start for builders that need post-start hooks (e.g. Kafka).
@external(erlang, "testcontainers_gleam_ffi", "after_start")
pub fn after_start(builder: anything, container: Container) -> Result(Nil, Dynamic)
```

**Step 3: Run `gleam build` to verify compilation**

Run: `gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add src/internal/testcontainers_gleam_ffi.erl src/internal/testcontainers_ffi.gleam
git commit -m "feat: add build_container and after_start FFI functions"
```

---

### Task 2: Redis module

**Files:**
- Create: `src/testcontainers_gleam/redis.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1: Write unit tests**

Add to `test/testcontainers_gleam_test.gleam`:

```gleam
import testcontainers_gleam/redis

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
```

**Step 2: Run tests to verify they fail**

Run: `gleam test`
Expected: FAIL — module `testcontainers_gleam/redis` not found

**Step 3: Implement `src/testcontainers_gleam/redis.gleam`**

```gleam
//// Redis container configuration.
////
//// Wraps `Testcontainers.RedisContainer` to provide typed builders
//// for a Redis container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/redis
////
//// let config = redis.new()
//// let container = redis.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let url = redis.connection_url(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a Redis container.
pub type RedisConfig

/// Create a new Redis container configuration with defaults.
///
/// Default image: `redis:7.2-alpine`, port: 6379, timeout: 60s.
@external(erlang, "Elixir.Testcontainers.RedisContainer", "new")
pub fn new() -> RedisConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.RedisContainer", "with_image")
pub fn with_image(config: RedisConfig, image: String) -> RedisConfig

/// Override the exposed port (default 6379).
@external(erlang, "Elixir.Testcontainers.RedisContainer", "with_port")
pub fn with_port(config: RedisConfig, port: Int) -> RedisConfig

/// Override the wait timeout in milliseconds (default 60000).
@external(erlang, "Elixir.Testcontainers.RedisContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: RedisConfig, timeout: Int) -> RedisConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.RedisContainer", "with_check_image")
pub fn with_check_image(config: RedisConfig, pattern: String) -> RedisConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.RedisContainer", "with_reuse")
pub fn with_reuse(config: RedisConfig, reuse: Bool) -> RedisConfig

/// Build a `Container` from this Redis configuration.
pub fn build(config: RedisConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.RedisContainer", "default_image")
pub fn default_image() -> String

/// Get the host-mapped port for the Redis container.
@external(erlang, "Elixir.Testcontainers.RedisContainer", "port")
pub fn port(container: Container) -> Int

/// Get the Redis connection URL (e.g. `"redis://localhost:32768/"`).
@external(erlang, "Elixir.Testcontainers.RedisContainer", "connection_url")
pub fn connection_url(container: Container) -> String
```

**Step 4: Run tests to verify they pass**

Run: `gleam test`
Expected: All new redis tests PASS

**Step 5: Write integration test**

Add to `test/testcontainers_gleam_integration_test.gleam`:

```gleam
import testcontainers_gleam/redis

// --- redis module ---

pub fn redis_module_start_and_stop_test() {
  integration_test(fn() {
    let config = redis.new()
    let container = redis.build(config)

    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = redis.port(running)
    { p > 0 } |> should.be_true()

    let url = redis.connection_url(running)
    url |> string.contains("redis://") |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/redis.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add redis container module"
```

---

### Task 3: PostgreSQL module

**Files:**
- Create: `src/testcontainers_gleam/postgres.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1: Write unit tests**

Add to `test/testcontainers_gleam_test.gleam`:

```gleam
import testcontainers_gleam/postgres

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
    |> postgres.with_wait_timeout(30_000)
    |> postgres.with_check_image("postgres")
    |> postgres.with_reuse(True)
  Nil
}

pub fn postgres_build_test() {
  let _container = postgres.new() |> postgres.build()
  Nil
}

pub fn postgres_default_image_test() {
  let _image = postgres.default_image()
  Nil
}

pub fn postgres_default_port_test() {
  let _port = postgres.default_port()
  Nil
}
```

**Step 2: Run tests — expect FAIL**

**Step 3: Implement `src/testcontainers_gleam/postgres.gleam`**

```gleam
//// PostgreSQL container configuration.
////
//// Wraps `Testcontainers.PostgresContainer` to provide typed builders
//// for a PostgreSQL container.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/postgres
////
//// let config =
////   postgres.new()
////   |> postgres.with_user("app")
////   |> postgres.with_password("secret")
////   |> postgres.with_database("app_test")
////
//// let container = postgres.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a PostgreSQL container.
pub type PostgresConfig

/// Create a new PostgreSQL configuration with defaults.
///
/// Default image: `postgres:15-alpine`, user/password/database: `"test"`,
/// port: 5432, timeout: 60s.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "new")
pub fn new() -> PostgresConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_image")
pub fn with_image(config: PostgresConfig, image: String) -> PostgresConfig

/// Override the database user.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_user")
pub fn with_user(config: PostgresConfig, user: String) -> PostgresConfig

/// Override the database password.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_password")
pub fn with_password(config: PostgresConfig, password: String) -> PostgresConfig

/// Override the database name.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_database")
pub fn with_database(config: PostgresConfig, database: String) -> PostgresConfig

/// Override the exposed port (default 5432).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_port")
pub fn with_port(config: PostgresConfig, port: Int) -> PostgresConfig

/// Set a named Docker volume for persistent data.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_persistent_volume")
pub fn with_persistent_volume(
  config: PostgresConfig,
  volume: String,
) -> PostgresConfig

/// Override the wait timeout in milliseconds (default 60000).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: PostgresConfig, timeout: Int) -> PostgresConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_check_image")
pub fn with_check_image(config: PostgresConfig, pattern: String) -> PostgresConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_reuse")
pub fn with_reuse(config: PostgresConfig, reuse: Bool) -> PostgresConfig

/// Build a `Container` from this PostgreSQL configuration.
pub fn build(config: PostgresConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "default_image")
pub fn default_image() -> String

/// Get the default exposed port.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "default_port")
pub fn default_port() -> Int

/// Get the host-mapped port for the PostgreSQL container.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "port")
pub fn port(container: Container) -> Int

/// Get connection parameters as an Elixir keyword list.
///
/// Returns a keyword list with keys: `hostname`, `port`, `username`,
/// `password`, `database`. This is an opaque Erlang term suitable for
/// passing to Ecto or other Elixir database adapters via FFI.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "connection_parameters")
pub fn connection_parameters(container: Container) -> ConnectionParameters

/// Opaque type representing Elixir connection parameters (keyword list).
pub type ConnectionParameters
```

**Step 4: Run tests — expect PASS**

**Step 5: Write integration test**

Add to `test/testcontainers_gleam_integration_test.gleam`:

```gleam
import testcontainers_gleam/postgres

// --- postgres module ---

pub fn postgres_module_start_and_stop_test() {
  integration_test(fn() {
    let config =
      postgres.new()
      |> postgres.with_user("gleam")
      |> postgres.with_password("gleam")
      |> postgres.with_database("gleam_test")

    let container = postgres.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = postgres.port(running)
    { p > 0 } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/postgres.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add postgres container module"
```

---

### Task 4: MySQL module

**Files:**
- Create: `src/testcontainers_gleam/mysql.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1: Write unit tests**

Same pattern as postgres: `mysql_new_test`, `mysql_builders_test`, `mysql_build_test`, `mysql_default_image_test`, `mysql_default_port_test`.

**Step 2: Run tests — expect FAIL**

**Step 3: Implement `src/testcontainers_gleam/mysql.gleam`**

Identical pattern to postgres module but using `Elixir.Testcontainers.MySqlContainer`. Same builder functions: `with_image`, `with_user`, `with_password`, `with_database`, `with_port`, `with_persistent_volume`, `with_wait_timeout`, `with_check_image`, `with_reuse`. Same accessors: `default_image`, `default_port`, `port`, `connection_parameters`.

Type name: `MysqlConfig`. Note: 3-minute default wait timeout.

```gleam
//// MySQL container configuration.
////
//// Default image: `mysql:8`, user/password/database: `"test"`,
//// port: 3306, timeout: 180s (3 minutes).

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

pub type MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "new")
pub fn new() -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_image")
pub fn with_image(config: MysqlConfig, image: String) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_user")
pub fn with_user(config: MysqlConfig, user: String) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_password")
pub fn with_password(config: MysqlConfig, password: String) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_database")
pub fn with_database(config: MysqlConfig, database: String) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_port")
pub fn with_port(config: MysqlConfig, port: Int) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_persistent_volume")
pub fn with_persistent_volume(config: MysqlConfig, volume: String) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: MysqlConfig, timeout: Int) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_check_image")
pub fn with_check_image(config: MysqlConfig, pattern: String) -> MysqlConfig

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_reuse")
pub fn with_reuse(config: MysqlConfig, reuse: Bool) -> MysqlConfig

pub fn build(config: MysqlConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "default_image")
pub fn default_image() -> String

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "default_port")
pub fn default_port() -> Int

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "port")
pub fn port(container: Container) -> Int

@external(erlang, "Elixir.Testcontainers.MySqlContainer", "connection_parameters")
pub fn connection_parameters(container: Container) -> ConnectionParameters

pub type ConnectionParameters
```

**Step 4: Run tests — expect PASS**

**Step 5: Write integration test**

Same pattern as postgres but with MySQL. Note MySQL has a 3-minute timeout.

```gleam
import testcontainers_gleam/mysql

pub fn mysql_module_start_and_stop_test() {
  integration_test(fn() {
    let config = mysql.new()
    let container = mysql.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = mysql.port(running)
    { p > 0 } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/mysql.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add mysql container module"
```

---

### Task 5: Cassandra module

**Files:**
- Create: `src/testcontainers_gleam/cassandra.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1-2: Write unit tests and verify failure**

Tests: `cassandra_new_test`, `cassandra_builders_test` (with_image, with_check_image, with_reuse), `cassandra_build_test`, `cassandra_constants_test` (default_image, default_port, get_username, get_password).

**Step 3: Implement `src/testcontainers_gleam/cassandra.gleam`**

```gleam
//// Cassandra container configuration.
////
//// Default image: `cassandra:3.11.2`, port: 9042, timeout: 60s.
//// Default credentials: `cassandra` / `cassandra`.

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

pub type CassandraConfig

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "new")
pub fn new() -> CassandraConfig

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "with_image")
pub fn with_image(config: CassandraConfig, image: String) -> CassandraConfig

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "with_check_image")
pub fn with_check_image(config: CassandraConfig, pattern: String) -> CassandraConfig

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "with_reuse")
pub fn with_reuse(config: CassandraConfig, reuse: Bool) -> CassandraConfig

pub fn build(config: CassandraConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "default_image")
pub fn default_image() -> String

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "default_port")
pub fn default_port() -> Int

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "get_username")
pub fn get_username() -> String

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "get_password")
pub fn get_password() -> String

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "port")
pub fn port(container: Container) -> Int

@external(erlang, "Elixir.Testcontainers.CassandraContainer", "connection_uri")
pub fn connection_uri(container: Container) -> String
```

**Step 4-5: Run tests, write integration test, verify**

```gleam
import testcontainers_gleam/cassandra

pub fn cassandra_module_start_and_stop_test() {
  integration_test(fn() {
    let config = cassandra.new()
    let container = cassandra.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = cassandra.port(running)
    { p > 0 } |> should.be_true()

    let uri = cassandra.connection_uri(running)
    uri |> string.is_empty() |> should.be_false()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/cassandra.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add cassandra container module"
```

---

### Task 6: Ceph module

**Files:**
- Create: `src/testcontainers_gleam/ceph.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1-2: Write unit tests and verify failure**

Tests: `ceph_new_test`, `ceph_builders_test` (with_image, with_access_key, with_secret_key, with_bucket, with_port, with_wait_timeout, with_check_image, with_reuse), `ceph_build_test`.

**Step 3: Implement `src/testcontainers_gleam/ceph.gleam`**

```gleam
//// Ceph (S3-compatible) container configuration.
////
//// Default image: `quay.io/ceph/demo:latest-quincy`, port: 8080,
//// timeout: 300s (5 minutes). Default access key: `"test"`.

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

pub type CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "new")
pub fn new() -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_image")
pub fn with_image(config: CephConfig, image: String) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_access_key")
pub fn with_access_key(config: CephConfig, access_key: String) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_secret_key")
pub fn with_secret_key(config: CephConfig, secret_key: String) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_bucket")
pub fn with_bucket(config: CephConfig, bucket: String) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_port")
pub fn with_port(config: CephConfig, port: Int) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: CephConfig, timeout: Int) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_check_image")
pub fn with_check_image(config: CephConfig, pattern: String) -> CephConfig

@external(erlang, "Elixir.Testcontainers.CephContainer", "with_reuse")
pub fn with_reuse(config: CephConfig, reuse: Bool) -> CephConfig

pub fn build(config: CephConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

@external(erlang, "Elixir.Testcontainers.CephContainer", "default_image")
pub fn default_image() -> String

@external(erlang, "Elixir.Testcontainers.CephContainer", "port")
pub fn port(container: Container) -> Int

@external(erlang, "Elixir.Testcontainers.CephContainer", "connection_url")
pub fn connection_url(container: Container) -> String
```

**Step 4-5: Run tests, write integration test**

```gleam
import testcontainers_gleam/ceph

pub fn ceph_module_start_and_stop_test() {
  integration_test(fn() {
    let config = ceph.new()
    let container = ceph.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = ceph.port(running)
    { p > 0 } |> should.be_true()

    let url = ceph.connection_url(running)
    url |> string.contains("http://") |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/ceph.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add ceph container module"
```

---

### Task 7: Kafka module

**Files:**
- Create: `src/testcontainers_gleam/kafka.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

This is the most complex module because:
- Kafka uses `after_start` to upload a startup script (mandatory)
- Has a `ConsensusStrategy` enum type
- v1.13.3 uses `with_topic_partitions` (Int), not `with_topics` (List)

**Step 1-2: Write unit tests and verify failure**

```gleam
import testcontainers_gleam/kafka

pub fn kafka_new_test() {
  let _config = kafka.new()
  Nil
}

pub fn kafka_builders_test() {
  let _config =
    kafka.new()
    |> kafka.with_image("confluentinc/cp-kafka:7.4.3")
    |> kafka.with_kafka_port(9093)
    |> kafka.with_broker_port(29093)
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
```

**Step 3: Implement `src/testcontainers_gleam/kafka.gleam`**

```gleam
//// Kafka container configuration.
////
//// Default image: `confluentinc/cp-kafka:7.4.3`, port: 9092, timeout: 60s.
//// Uses embedded Zookeeper by default.
////
//// **Important:** After starting the container, you MUST call `after_start`
//// to upload the startup script that configures advertised listeners.
//// Without this, the Kafka broker will not start.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/kafka
//// import testcontainers_gleam/container
////
//// let config = kafka.new()
//// let built = kafka.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(built)
//// let assert Ok(Nil) = kafka.after_start(config, running)
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/result
import internal/testcontainers_ffi
import testcontainers_gleam.{type ContainerError}
import testcontainers_gleam/container.{type Container}

/// Configuration for a Kafka container.
pub type KafkaConfig

/// Create a new Kafka configuration with defaults.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "new")
pub fn new() -> KafkaConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_image")
pub fn with_image(config: KafkaConfig, image: String) -> KafkaConfig

/// Override the Kafka listener port (default 9092).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_kafka_port")
pub fn with_kafka_port(config: KafkaConfig, port: Int) -> KafkaConfig

/// Override the internal broker port (default 29092).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_broker_port")
pub fn with_broker_port(config: KafkaConfig, port: Int) -> KafkaConfig

/// Override the broker ID (default 1).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_broker_id")
pub fn with_broker_id(config: KafkaConfig, id: Int) -> KafkaConfig

/// Override the wait timeout in milliseconds (default 60000).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: KafkaConfig, timeout: Int) -> KafkaConfig

/// Override the default number of topic partitions (default 1).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_topic_partitions")
pub fn with_topic_partitions(config: KafkaConfig, partitions: Int) -> KafkaConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_reuse")
pub fn with_reuse(config: KafkaConfig, reuse: Bool) -> KafkaConfig

/// Build a `Container` from this Kafka configuration.
pub fn build(config: KafkaConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Run the post-start hook that uploads the startup script.
///
/// **This MUST be called after `start_container`.** The startup script
/// configures Kafka's advertised listeners with the correct host-mapped
/// port. Without it, the Kafka broker will not start.
pub fn after_start(
  config: KafkaConfig,
  running: Container,
) -> Result(Nil, ContainerError) {
  testcontainers_ffi.after_start(config, running)
  |> result.map_error(fn(_) { testcontainers_gleam.Unknown })
}

/// Get the host-mapped Kafka port.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "port")
pub fn port(container: Container) -> Int
```

Note: The v1.13.3 KafkaContainer does NOT have `bootstrap_servers/1` or `connection_url/1` functions. Only `port/1` is available as an accessor. Also, `with_consensus_strategy`, `with_zookeeper_port`, `with_zookeeper_host`, and `with_cluster_id` have guard clauses that depend on the current strategy state, which makes them tricky to expose via FFI (they may crash if called in the wrong order). We'll expose only the safe builders.

**Step 4-5: Run tests, write integration test**

```gleam
import testcontainers_gleam/kafka

pub fn kafka_module_start_and_stop_test() {
  integration_test(fn() {
    let config = kafka.new()
    let built = kafka.build(config)
    let running = testcontainers_gleam.start_container(built) |> should.be_ok()

    // Must call after_start for Kafka
    kafka.after_start(config, running) |> should.be_ok()

    let id = container.container_id(running)
    let p = kafka.port(running)
    { p > 0 } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/kafka.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add kafka container module with after_start hook"
```

---

### Task 8: EMQX module

**Files:**
- Create: `src/testcontainers_gleam/emqx.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1-2: Write unit tests and verify failure**

**Step 3: Implement `src/testcontainers_gleam/emqx.gleam`**

```gleam
//// EMQX (MQTT broker) container configuration.
////
//// Default image: `emqx:5.6.0`, MQTT port: 1883, timeout: 60s.

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

pub type EmqxConfig

@external(erlang, "Elixir.Testcontainers.EmqxContainer", "new")
pub fn new() -> EmqxConfig

@external(erlang, "Elixir.Testcontainers.EmqxContainer", "with_image")
pub fn with_image(config: EmqxConfig, image: String) -> EmqxConfig

@external(erlang, "Elixir.Testcontainers.EmqxContainer", "with_check_image")
pub fn with_check_image(config: EmqxConfig, pattern: String) -> EmqxConfig

@external(erlang, "Elixir.Testcontainers.EmqxContainer", "with_reuse")
pub fn with_reuse(config: EmqxConfig, reuse: Bool) -> EmqxConfig

pub fn build(config: EmqxConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

@external(erlang, "Elixir.Testcontainers.EmqxContainer", "default_image")
pub fn default_image() -> String

@external(erlang, "Elixir.Testcontainers.EmqxContainer", "mqtt_port")
pub fn mqtt_port(container: Container) -> Int
```

Note: `with_ports/6` is omitted because Elixir generates multiple arities (1-6) from default arguments, and calling the 6-arity version from Gleam requires matching all params without defaults. If needed, users can configure ports via the low-level `container` module after `build()`. EMQX's `host/0` is also omitted (it just returns `Testcontainers.get_host()` which is always `"localhost"`).

**Step 4-5: Tests and integration test**

```gleam
import testcontainers_gleam/emqx

pub fn emqx_module_start_and_stop_test() {
  integration_test(fn() {
    let config = emqx.new()
    let container = emqx.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = emqx.mqtt_port(running)
    { p > 0 } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/emqx.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add emqx container module"
```

---

### Task 9: MinIO module

**Files:**
- Create: `src/testcontainers_gleam/minio.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1-2: Write unit tests**

**Step 3: Implement `src/testcontainers_gleam/minio.gleam`**

```gleam
//// MinIO (S3-compatible) container configuration.
////
//// Default image: `minio/minio:RELEASE.2023-11-11T08-14-41Z`,
//// S3 port: 9000, UI port: 9001, timeout: 60s.
//// Default credentials: `minioadmin` / `minioadmin`.

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

pub type MinioConfig

@external(erlang, "Elixir.Testcontainers.MinioContainer", "new")
pub fn new() -> MinioConfig

@external(erlang, "Elixir.Testcontainers.MinioContainer", "with_reuse")
pub fn with_reuse(config: MinioConfig, reuse: Bool) -> MinioConfig

pub fn build(config: MinioConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

@external(erlang, "Elixir.Testcontainers.MinioContainer", "get_username")
pub fn get_username() -> String

@external(erlang, "Elixir.Testcontainers.MinioContainer", "get_password")
pub fn get_password() -> String

@external(erlang, "Elixir.Testcontainers.MinioContainer", "default_s3_port")
pub fn default_s3_port() -> Int

@external(erlang, "Elixir.Testcontainers.MinioContainer", "default_ui_port")
pub fn default_ui_port() -> Int

@external(erlang, "Elixir.Testcontainers.MinioContainer", "port")
pub fn port(container: Container) -> Int

@external(erlang, "Elixir.Testcontainers.MinioContainer", "connection_url")
pub fn connection_url(container: Container) -> String
```

**Step 4-5: Tests and integration test**

```gleam
import testcontainers_gleam/minio

pub fn minio_module_start_and_stop_test() {
  integration_test(fn() {
    let config = minio.new()
    let container = minio.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = minio.port(running)
    { p > 0 } |> should.be_true()

    let url = minio.connection_url(running)
    url |> string.contains("http://") |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/minio.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add minio container module"
```

---

### Task 10: RabbitMQ module

**Files:**
- Create: `src/testcontainers_gleam/rabbitmq.gleam`
- Modify: `test/testcontainers_gleam_test.gleam`
- Modify: `test/testcontainers_gleam_integration_test.gleam`

**Step 1-2: Write unit tests**

**Step 3: Implement `src/testcontainers_gleam/rabbitmq.gleam`**

```gleam
//// RabbitMQ container configuration.
////
//// Default image: `rabbitmq:3-alpine`, port: 5672, timeout: 60s.
//// Default credentials: `guest` / `guest`, virtual host: `"/"`.

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

pub type RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "new")
pub fn new() -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_image")
pub fn with_image(config: RabbitmqConfig, image: String) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_port")
pub fn with_port(config: RabbitmqConfig, port: Int) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_username")
pub fn with_username(config: RabbitmqConfig, username: String) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_password")
pub fn with_password(config: RabbitmqConfig, password: String) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_virtual_host")
pub fn with_virtual_host(config: RabbitmqConfig, vhost: String) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_cmd")
pub fn with_cmd(config: RabbitmqConfig, cmd: List(String)) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: RabbitmqConfig, timeout: Int) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_check_image")
pub fn with_check_image(config: RabbitmqConfig, pattern: String) -> RabbitmqConfig

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_reuse")
pub fn with_reuse(config: RabbitmqConfig, reuse: Bool) -> RabbitmqConfig

pub fn build(config: RabbitmqConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "default_image")
pub fn default_image() -> String

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "default_port")
pub fn default_port() -> Int

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "port")
pub fn port(container: Container) -> Int

@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "connection_url")
pub fn connection_url(container: Container) -> String
```

**Step 4-5: Tests and integration test**

```gleam
import testcontainers_gleam/rabbitmq

pub fn rabbitmq_module_start_and_stop_test() {
  integration_test(fn() {
    let config = rabbitmq.new()
    let container = rabbitmq.build(config)
    let running = testcontainers_gleam.start_container(container) |> should.be_ok()
    let id = container.container_id(running)

    let p = rabbitmq.port(running)
    { p > 0 } |> should.be_true()

    let url = rabbitmq.connection_url(running)
    url |> string.contains("amqp://") |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
```

**Step 6: Commit**

```bash
git add src/testcontainers_gleam/rabbitmq.gleam test/testcontainers_gleam_test.gleam test/testcontainers_gleam_integration_test.gleam
git commit -m "feat: add rabbitmq container module"
```

---

### Task 11: Format check and final verification

**Step 1: Run formatter**

Run: `gleam format src test`

**Step 2: Run format check**

Run: `gleam format --check src test`
Expected: No changes needed

**Step 3: Run all unit tests**

Run: `gleam test`
Expected: All unit tests pass

**Step 4: Run integration tests (if Docker available)**

Run: `TESTCONTAINERS_INTEGRATION_TESTS=1 gleam test`
Expected: All integration tests pass (may take several minutes due to Ceph/MySQL)

**Step 5: Final commit if needed**

```bash
git add -A
git commit -m "chore: format and finalize image modules"
```
