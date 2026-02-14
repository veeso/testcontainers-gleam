//// MySQL container configuration.
////
//// Wraps `Testcontainers.MySqlContainer` to provide typed builders
//// for a MySQL container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/mysql
////
//// let config = mysql.new()
//// let container = mysql.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let params = mysql.connection_parameters(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a MySQL container.
pub type MysqlConfig

/// Opaque type wrapping the Elixir keyword list returned by
/// `MySqlContainer.connection_parameters/1`.
pub type ConnectionParameters

/// Create a new MySQL container configuration with defaults.
///
/// Default image: `mysql:8`, user/password/database: `"test"`,
/// port: 3306, timeout: 180s.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "new")
pub fn new() -> MysqlConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_image")
pub fn with_image(config: MysqlConfig, image: String) -> MysqlConfig

/// Override the database user (default `"test"`).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_user")
pub fn with_user(config: MysqlConfig, user: String) -> MysqlConfig

/// Override the database password (default `"test"`).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_password")
pub fn with_password(config: MysqlConfig, password: String) -> MysqlConfig

/// Override the database name (default `"test"`).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_database")
pub fn with_database(config: MysqlConfig, database: String) -> MysqlConfig

/// Override the exposed port (default 3306).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_port")
pub fn with_port(config: MysqlConfig, port: Int) -> MysqlConfig

/// Attach a persistent volume at the given path.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_persistent_volume")
pub fn with_persistent_volume(config: MysqlConfig, path: String) -> MysqlConfig

/// Override the wait timeout in milliseconds (default 180000).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: MysqlConfig, timeout: Int) -> MysqlConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_check_image")
pub fn with_check_image(config: MysqlConfig, pattern: String) -> MysqlConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "with_reuse")
pub fn with_reuse(config: MysqlConfig, reuse: Bool) -> MysqlConfig

/// Build a `Container` from this MySQL configuration.
pub fn build(config: MysqlConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "default_image")
pub fn default_image() -> String

/// Get the default exposed port (3306).
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "default_port")
pub fn default_port() -> Int

/// Get the host-mapped port for the MySQL container.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "port")
pub fn port(container: Container) -> Int

/// Get the connection parameters as an Elixir keyword list.
@external(erlang, "Elixir.Testcontainers.MySqlContainer", "connection_parameters")
pub fn connection_parameters(container: Container) -> ConnectionParameters
