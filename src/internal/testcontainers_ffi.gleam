//// FFI to Elixir.Testcontainers top-level module functions.

import gleam/dynamic.{type Dynamic}
import testcontainers_gleam/container.{type Container}

/// Start the Testcontainers GenServer.
///
/// Calls `Testcontainers.start_link/0` which returns
/// `{:ok, pid}` or `{:error, reason}`.
@external(erlang, "Elixir.Testcontainers", "start_link")
pub fn start_link() -> Result(Dynamic, Dynamic)

/// Start a container and wait until it is ready.
///
/// Uses an Erlang wrapper that normalizes 3-element error tuples
/// from wait strategies into standard 2-element `{:error, reason}`.
@external(erlang, "testcontainers_gleam_ffi", "start_container")
pub fn start(container: Container) -> Result(Container, Dynamic)

/// Stop a running container by ID.
///
/// Calls `Testcontainers.stop_container/1` which returns
/// `:ok` or `{:error, reason}`. Typed as Dynamic because `:ok`
/// is a bare atom, not a `{:ok, value}` tuple.
@external(erlang, "Elixir.Testcontainers", "stop_container")
pub fn stop_container(container_id: String) -> Dynamic
