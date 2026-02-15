//// Cassandra container configuration.
////
//// Wraps `Testcontainers.CassandraContainer` to provide typed builders
//// for a Cassandra container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/cassandra
////
//// let config = cassandra.new()
//// let container = cassandra.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let uri = cassandra.connection_uri(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a Cassandra container.
pub type CassandraConfig

/// Create a new Cassandra container configuration with defaults.
///
/// Default image: `cassandra:3.11.2`, port: 9042, timeout: 60s,
/// credentials: cassandra/cassandra.
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "new")
pub fn new() -> CassandraConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "with_image")
pub fn with_image(config: CassandraConfig, image: String) -> CassandraConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "with_check_image")
pub fn with_check_image(
  config: CassandraConfig,
  pattern: String,
) -> CassandraConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "with_reuse")
pub fn with_reuse(config: CassandraConfig, reuse: Bool) -> CassandraConfig

/// Build a `Container` from this Cassandra configuration.
pub fn build(config: CassandraConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "default_image")
pub fn default_image() -> String

/// Get the default exposed port (9042).
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "default_port")
pub fn default_port() -> Int

/// Get the default username (`"cassandra"`).
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "get_username")
pub fn get_username() -> String

/// Get the default password (`"cassandra"`).
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "get_password")
pub fn get_password() -> String

/// Get the host-mapped port for the Cassandra container.
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "port")
pub fn port(container: Container) -> Int

/// Get the Cassandra connection URI (e.g. `"cassandra://localhost:32768/"`).
@external(erlang, "Elixir.Testcontainers.CassandraContainer", "connection_uri")
pub fn connection_uri(container: Container) -> String
