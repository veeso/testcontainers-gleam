//// PostgreSQL container configuration.
////
//// Wraps `Testcontainers.PostgresContainer` to provide typed builders
//// for a PostgreSQL container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/postgres
////
//// let config = postgres.new()
//// let container = postgres.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let params = postgres.connection_parameters(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a PostgreSQL container.
pub type PostgresConfig

/// Opaque type wrapping the Elixir keyword list returned by
/// `PostgresContainer.connection_parameters/1`.
pub type ConnectionParameters

/// Create a new PostgreSQL container configuration with defaults.
///
/// Default image: `postgres:15-alpine`, user/password/database: `"test"`,
/// port: 5432, timeout: 60s.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "new")
pub fn new() -> PostgresConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_image")
pub fn with_image(config: PostgresConfig, image: String) -> PostgresConfig

/// Override the database user (default `"test"`).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_user")
pub fn with_user(config: PostgresConfig, user: String) -> PostgresConfig

/// Override the database password (default `"test"`).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_password")
pub fn with_password(config: PostgresConfig, password: String) -> PostgresConfig

/// Override the database name (default `"test"`).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_database")
pub fn with_database(config: PostgresConfig, database: String) -> PostgresConfig

/// Override the exposed port (default 5432).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_port")
pub fn with_port(config: PostgresConfig, port: Int) -> PostgresConfig

/// Attach a persistent volume at the given path.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_persistent_volume")
pub fn with_persistent_volume(
  config: PostgresConfig,
  path: String,
) -> PostgresConfig

/// Override the wait timeout in milliseconds (default 60000).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: PostgresConfig, timeout: Int) -> PostgresConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "with_check_image")
pub fn with_check_image(
  config: PostgresConfig,
  pattern: String,
) -> PostgresConfig

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

/// Get the default exposed port (5432).
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "default_port")
pub fn default_port() -> Int

/// Get the host-mapped port for the PostgreSQL container.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "port")
pub fn port(container: Container) -> Int

/// Get the connection parameters as an Elixir keyword list.
@external(erlang, "Elixir.Testcontainers.PostgresContainer", "connection_parameters")
pub fn connection_parameters(container: Container) -> ConnectionParameters
