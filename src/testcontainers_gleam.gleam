//// Gleam wrapper for the Elixir
//// [testcontainers](https://hex.pm/packages/testcontainers) library.
////
//// Provides functions to start and stop Docker containers in tests.
//// Use `testcontainers_gleam/container` to build a container definition,
//// then pass it to `start_container` to run it.
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
////
//// let assert Ok(running) = testcontainers_gleam.start_container(c)
//// let id = container.container_id(running)
//// let assert Ok(port) = container.mapped_port(running, 6379)
////
//// let assert Ok(Nil) = testcontainers_gleam.stop_container(id)
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/result
import internal/testcontainers_ffi
import testcontainers_gleam/container.{type Container}

/// Result alias for testcontainers operations.
pub type ContainerResult(a) =
  Result(a, ContainerError)

/// Error returned by testcontainers operations.
pub type ContainerError {
  /// HTTP error from the Docker API with the status code.
  HttpError(status: Int)
  /// Failed to pull the container image.
  FailedToPullImage
  /// Failed to create the container.
  FailedToCreateContainer
  /// Failed to start the container.
  FailedToStartContainer
  /// Failed to inspect a running container.
  FailedToGetContainer
  /// A wait strategy timed out or failed.
  WaitStrategyFailed
  /// An unexpected error from the Elixir library.
  Unknown
}

/// Start the Testcontainers GenServer.
///
/// This is called automatically by `start_container`, so you only
/// need to call it directly if you want to manage the lifecycle yourself.
pub fn start_link() -> ContainerResult(Nil) {
  testcontainers_ffi.start_link()
  |> result.map_error(decode_error)
  |> result.replace(Nil)
}

/// Start a container and wait until it is ready.
///
/// Automatically starts the Testcontainers GenServer if it is not
/// already running. Returns the started container on success, which
/// can be queried with `container.container_id` and `container.mapped_port`.
///
/// ## Example
///
/// ```gleam
/// let c =
///   container.new("redis:7.4-alpine")
///   |> container.with_exposed_port(6379)
///
/// let assert Ok(running) = testcontainers_gleam.start_container(c)
/// ```
pub fn start_container(container: Container) -> ContainerResult(Container) {
  // Ensure the GenServer is running; ignore already-started errors.
  let _ = testcontainers_ffi.start_link()
  container
  |> testcontainers_ffi.start()
  |> result.map_error(decode_error)
}

/// Stop a running container by its container ID.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(Nil) = testcontainers_gleam.stop_container(id)
/// ```
pub fn stop_container(container_id: String) -> ContainerResult(Nil) {
  let raw = testcontainers_ffi.stop_container(container_id)
  let decoded =
    raw
    |> decode.run(atom.decoder())
    |> result.try(fn(tag) {
      case atom.to_string(tag) {
        "ok" -> Ok(Nil)
        _ -> Error([])
      }
    })
  case decoded {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(decode_error(raw))
  }
}

/// Decode an error tuple from the Elixir library into a `ContainerError`.
fn decode_error(raw: Dynamic) -> ContainerError {
  let tag_decoder = decode.at([0], atom.decoder())
  case decode.run(raw, tag_decoder) {
    Ok(tag) ->
      case atom.to_string(tag) {
        "http_error" -> {
          let status_decoder = decode.at([1], decode.int)
          let status = case decode.run(raw, status_decoder) {
            Ok(s) -> s
            Error(_) -> 0
          }
          HttpError(status)
        }
        "failed_to_pull_image" -> FailedToPullImage
        "failed_to_create_container" -> FailedToCreateContainer
        "failed_to_start_container" -> FailedToStartContainer
        "failed_to_get_container" -> FailedToGetContainer
        _ -> Unknown
      }
    Error(_) -> Unknown
  }
}
