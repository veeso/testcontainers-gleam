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
