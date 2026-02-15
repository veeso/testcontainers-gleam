//// Docker container configuration and builder API.
////
//// Use `new` to create a container definition, then chain `with_*` functions
//// to configure it before passing it to `testcontainers_gleam.start_container`.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/container
////
//// let c =
////   container.new("redis:7.4-alpine")
////   |> container.with_exposed_port(6379)
////   |> container.with_environment("REDIS_PASSWORD", "secret")
////
//// let assert Ok(running) = testcontainers_gleam.start_container(c)
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/result
import testcontainers_gleam/wait_strategy.{type WaitStrategy}

/// External type wrapping the Elixir `%Testcontainers.Container{}` struct.
///
/// Built with `new` and configured with `with_*` functions.
/// After starting, use `container_id` and `mapped_port` to query the
/// running container.
pub type Container

/// Image pull policy for a container.
pub type PullPolicy {
  /// Pull the image every time, even if it exists locally.
  AlwaysPull
  /// Never pull the image; fail if it is not available locally.
  NeverPull
}

/// Create a new container definition for the given Docker image.
///
/// The returned container has default configuration; use the `with_*`
/// functions to customize it before starting.
///
/// ## Example
///
/// ```gleam
/// container.new("postgres:16-alpine")
/// ```
@external(erlang, "Elixir.Testcontainers.Container", "new")
pub fn new(image: String) -> Container

/// Expose a single port from the container.
///
/// The port will be mapped to a random host port. Use `mapped_port`
/// after starting to get the assigned host port.
@external(erlang, "Elixir.Testcontainers.Container", "with_exposed_port")
pub fn with_exposed_port(container: Container, port: Int) -> Container

/// Expose multiple ports from the container.
@external(erlang, "Elixir.Testcontainers.Container", "with_exposed_ports")
pub fn with_exposed_ports(container: Container, ports: List(Int)) -> Container

/// Expose a container `port` and bind it to a fixed `host_port`.
@external(erlang, "Elixir.Testcontainers.Container", "with_fixed_port")
pub fn with_fixed_port(
  container: Container,
  port: Int,
  host_port: Int,
) -> Container

/// Set an environment variable on the container.
@external(erlang, "Elixir.Testcontainers.Container", "with_environment")
pub fn with_environment(
  container: Container,
  key: String,
  value: String,
) -> Container

/// Set the command to run inside the container.
@external(erlang, "Elixir.Testcontainers.Container", "with_cmd")
pub fn with_cmd(container: Container, cmd: List(String)) -> Container

/// Mount a host path into the container.
///
/// The `options` string controls mount flags (e.g. `"ro"` or `"rw"`).
@external(erlang, "Elixir.Testcontainers.Container", "with_bind_mount")
pub fn with_bind_mount(
  container: Container,
  host_src: String,
  dest: String,
  options: String,
) -> Container

/// Mount a named Docker volume into the container.
@external(erlang, "Elixir.Testcontainers.Container", "with_bind_volume")
pub fn with_bind_volume(
  container: Container,
  volume: String,
  dest: String,
  read_only: Bool,
) -> Container

/// Add a label to the container.
@external(erlang, "Elixir.Testcontainers.Container", "with_label")
pub fn with_label(container: Container, key: String, value: String) -> Container

/// Add a wait strategy to determine when the container is ready.
///
/// See `testcontainers_gleam/wait_strategy` for available strategies.
@external(erlang, "Elixir.Testcontainers.Container", "with_waiting_strategy")
pub fn with_waiting_strategy(
  container: Container,
  strategy: WaitStrategy,
) -> Container

/// Set whether Docker should automatically remove the container when it exits.
@external(erlang, "Elixir.Testcontainers.Container", "with_auto_remove")
pub fn with_auto_remove(container: Container, auto_remove: Bool) -> Container

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.Container", "with_reuse")
pub fn with_reuse(container: Container, reuse: Bool) -> Container

/// Set the Docker network mode (e.g. `"bridge"`, `"host"`, `"none"`).
@external(erlang, "Elixir.Testcontainers.Container", "with_network_mode")
pub fn with_network_mode(container: Container, mode: String) -> Container

/// Set registry authentication credentials for pulling the image.
@external(erlang, "Elixir.Testcontainers.Container", "with_auth")
pub fn with_auth(
  container: Container,
  username: String,
  password: String,
) -> Container

/// Set a check image pattern to verify the image name.
@external(erlang, "Elixir.Testcontainers.Container", "with_check_image")
pub fn with_check_image(container: Container, pattern: String) -> Container

/// Set the image pull policy. Defaults to `AlwaysPull`.
pub fn with_pull_policy(container: Container, policy: PullPolicy) -> Container {
  let elixir_policy = case policy {
    AlwaysPull -> do_pull_always()
    NeverPull -> do_pull_never()
  }
  do_with_pull_policy(container, elixir_policy)
}

/// Get the container ID of a running container.
///
/// Only meaningful after the container has been started.
///
/// ## Panics
///
/// Panics if called on a container that has not been started.
pub fn container_id(container: Container) -> String {
  let assert Ok(id) =
    decode.run(
      do_get_field(atom.create("container_id"), container),
      decode.string,
    )
  id
}

/// Get the host port mapped to an exposed container port.
///
/// Returns `Ok(port)` if the mapping exists, or `Error(Nil)` if the
/// port was not exposed or has not been mapped yet.
pub fn mapped_port(container: Container, port: Int) -> Result(Int, Nil) {
  do_mapped_port(container, port)
  |> decode.run(decode.int)
  |> result.replace_error(Nil)
}

@external(erlang, "Elixir.Testcontainers.Container", "with_pull_policy")
fn do_with_pull_policy(
  container: Container,
  policy: ElixirPullPolicy,
) -> Container

@external(erlang, "Elixir.Testcontainers.PullPolicy", "always_pull")
fn do_pull_always() -> ElixirPullPolicy

@external(erlang, "Elixir.Testcontainers.PullPolicy", "never_pull")
fn do_pull_never() -> ElixirPullPolicy

@external(erlang, "maps", "get")
fn do_get_field(key: atom.Atom, container: Container) -> Dynamic

@external(erlang, "Elixir.Testcontainers.Container", "mapped_port")
fn do_mapped_port(container: Container, port: Int) -> Dynamic

type ElixirPullPolicy
