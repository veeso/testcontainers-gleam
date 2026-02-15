//// RabbitMQ container configuration.
////
//// Wraps `Testcontainers.RabbitMQContainer` to provide typed builders
//// for a RabbitMQ container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// Default image: `rabbitmq:3-alpine`, port: 5672, timeout: 60s,
//// credentials: guest/guest, vhost: "/".
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/rabbitmq
////
//// let config = rabbitmq.new()
//// let container = rabbitmq.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let url = rabbitmq.connection_url(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a RabbitMQ container.
pub type RabbitmqConfig

/// Create a new RabbitMQ container configuration with defaults.
///
/// Default image: `rabbitmq:3-alpine`, port: 5672, timeout: 60s,
/// credentials: guest/guest, vhost: "/".
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "new")
pub fn new() -> RabbitmqConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_image")
pub fn with_image(config: RabbitmqConfig, image: String) -> RabbitmqConfig

/// Override the exposed port (default 5672).
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_port")
pub fn with_port(config: RabbitmqConfig, port: Int) -> RabbitmqConfig

/// Override the username (default "guest").
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_username")
pub fn with_username(config: RabbitmqConfig, username: String) -> RabbitmqConfig

/// Override the password (default "guest").
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_password")
pub fn with_password(config: RabbitmqConfig, password: String) -> RabbitmqConfig

/// Override the virtual host (default "/").
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_virtual_host")
pub fn with_virtual_host(
  config: RabbitmqConfig,
  vhost: String,
) -> RabbitmqConfig

/// Set the command to run in the container.
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_cmd")
pub fn with_cmd(config: RabbitmqConfig, cmd: List(String)) -> RabbitmqConfig

/// Override the wait timeout in milliseconds (default 60000).
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: RabbitmqConfig, timeout: Int) -> RabbitmqConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_check_image")
pub fn with_check_image(
  config: RabbitmqConfig,
  pattern: String,
) -> RabbitmqConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "with_reuse")
pub fn with_reuse(config: RabbitmqConfig, reuse: Bool) -> RabbitmqConfig

/// Build a `Container` from this RabbitMQ configuration.
pub fn build(config: RabbitmqConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "default_image")
pub fn default_image() -> String

/// Get the default port (5672).
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "default_port")
pub fn default_port() -> Int

/// Get the host-mapped port for the RabbitMQ container.
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "port")
pub fn port(container: Container) -> Int

/// Get the RabbitMQ connection URL (e.g. `"amqp://guest:guest@localhost:32768/"`).
@external(erlang, "Elixir.Testcontainers.RabbitMQContainer", "connection_url")
pub fn connection_url(container: Container) -> String
