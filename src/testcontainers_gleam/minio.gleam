//// MinIO container configuration.
////
//// Wraps `Testcontainers.MinioContainer` to provide typed builders
//// for a MinIO container. Use `new` to create a default configuration,
//// customize with `with_*` functions, then `build` to get a `Container`.
////
//// Default image: `minio/minio:RELEASE.2023-11-11T08-14-41Z`,
//// S3 port: 9000, UI port: 9001, timeout: 60s,
//// credentials: minioadmin/minioadmin.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/minio
////
//// let config = minio.new()
//// let container = minio.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(container)
//// let url = minio.connection_url(running)
//// ```

import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Configuration for a MinIO container.
pub type MinioConfig

/// Create a new MinIO container configuration with defaults.
///
/// Default image: `minio/minio:RELEASE.2023-11-11T08-14-41Z`,
/// S3 port: 9000, UI port: 9001, timeout: 60s,
/// credentials: minioadmin/minioadmin.
@external(erlang, "Elixir.Testcontainers.MinioContainer", "new")
pub fn new() -> MinioConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.MinioContainer", "with_reuse")
pub fn with_reuse(config: MinioConfig, reuse: Bool) -> MinioConfig

/// Build a `Container` from this MinIO configuration.
pub fn build(config: MinioConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Get the default username (minioadmin).
@external(erlang, "Elixir.Testcontainers.MinioContainer", "get_username")
pub fn get_username() -> String

/// Get the default password (minioadmin).
@external(erlang, "Elixir.Testcontainers.MinioContainer", "get_password")
pub fn get_password() -> String

/// Get the default S3 port (9000).
@external(erlang, "Elixir.Testcontainers.MinioContainer", "default_s3_port")
pub fn default_s3_port() -> Int

/// Get the default UI port (9001).
@external(erlang, "Elixir.Testcontainers.MinioContainer", "default_ui_port")
pub fn default_ui_port() -> Int

/// Get the host-mapped port for the MinIO container.
@external(erlang, "Elixir.Testcontainers.MinioContainer", "port")
pub fn port(container: Container) -> Int

/// Get the MinIO connection URL (e.g. `"http://localhost:32768"`).
@external(erlang, "Elixir.Testcontainers.MinioContainer", "connection_url")
pub fn connection_url(container: Container) -> String
