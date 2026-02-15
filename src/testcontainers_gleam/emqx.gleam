//// EMQX container configuration.
////
//// Wraps `Testcontainers.EmqxContainer` to provide typed builders
//// for an EMQX container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// Default image: `emqx:5.6.0`, MQTT port: 1883, timeout: 60s.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/emqx
////
//// let config = emqx.new()
//// let container = emqx.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let port = emqx.mqtt_port(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for an EMQX container.
pub type EmqxConfig

/// Create a new EMQX container configuration with defaults.
///
/// Default image: `emqx:5.6.0`, MQTT port: 1883, timeout: 60s.
@external(erlang, "Elixir.Testcontainers.EmqxContainer", "new")
pub fn new() -> EmqxConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.EmqxContainer", "with_image")
pub fn with_image(config: EmqxConfig, image: String) -> EmqxConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.EmqxContainer", "with_check_image")
pub fn with_check_image(config: EmqxConfig, pattern: String) -> EmqxConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.EmqxContainer", "with_reuse")
pub fn with_reuse(config: EmqxConfig, reuse: Bool) -> EmqxConfig

/// Build a `Container` from this EMQX configuration.
pub fn build(config: EmqxConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.EmqxContainer", "default_image")
pub fn default_image() -> String

/// Get the host-mapped MQTT port for the EMQX container.
@external(erlang, "Elixir.Testcontainers.EmqxContainer", "mqtt_port")
pub fn mqtt_port(container: Container) -> Int
