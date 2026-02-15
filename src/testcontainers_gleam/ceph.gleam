//// Ceph container configuration.
////
//// Wraps `Testcontainers.CephContainer` to provide typed builders
//// for a Ceph container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/ceph
////
//// let config = ceph.new()
//// let container = ceph.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let url = ceph.connection_url(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a Ceph container.
pub type CephConfig

/// Create a new Ceph container configuration with defaults.
///
/// Default image: `quay.io/ceph/demo:latest-quincy`, port: 8080,
/// timeout: 300s, access_key: `"test"`.
@external(erlang, "Elixir.Testcontainers.CephContainer", "new")
pub fn new() -> CephConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_image")
pub fn with_image(config: CephConfig, image: String) -> CephConfig

/// Override the access key (default `"test"`).
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_access_key")
pub fn with_access_key(config: CephConfig, access_key: String) -> CephConfig

/// Override the secret key.
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_secret_key")
pub fn with_secret_key(config: CephConfig, secret_key: String) -> CephConfig

/// Set the bucket name.
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_bucket")
pub fn with_bucket(config: CephConfig, bucket: String) -> CephConfig

/// Override the exposed port (default 8080).
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_port")
pub fn with_port(config: CephConfig, port: Int) -> CephConfig

/// Override the wait timeout in milliseconds (default 300000).
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: CephConfig, timeout: Int) -> CephConfig

/// Set the regex pattern to validate the image name.
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_check_image")
pub fn with_check_image(config: CephConfig, pattern: String) -> CephConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.CephContainer", "with_reuse")
pub fn with_reuse(config: CephConfig, reuse: Bool) -> CephConfig

/// Build a `Container` from this Ceph configuration.
pub fn build(config: CephConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default Docker image name (without tag).
@external(erlang, "Elixir.Testcontainers.CephContainer", "default_image")
pub fn default_image() -> String

/// Get the host-mapped port for the Ceph container.
@external(erlang, "Elixir.Testcontainers.CephContainer", "port")
pub fn port(container: Container) -> Int

/// Get the Ceph connection URL (e.g. `"http://localhost:32768/"`).
@external(erlang, "Elixir.Testcontainers.CephContainer", "connection_url")
pub fn connection_url(container: Container) -> String
