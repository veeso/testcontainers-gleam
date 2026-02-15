//// Wait strategies to determine when a container is ready.
////
//// A wait strategy is passed to `container.with_waiting_strategy` to tell
//// testcontainers how to detect that a container has finished starting.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam/container
//// import testcontainers_gleam/wait_strategy
////
//// container.new("postgres:16-alpine")
//// |> container.with_exposed_port(5432)
//// |> container.with_waiting_strategy(
////   wait_strategy.port("0.0.0.0", 5432, 5000, 500),
//// )
//// ```

/// Opaque type wrapping an Elixir wait strategy struct.
///
/// Construct with `port`, `log`, or `command`.
pub type WaitStrategy

/// Wait until the given `port` on `ip` accepts TCP connections.
///
/// Retries every `retry_delay` ms, giving up after `timeout` ms.
@external(erlang, "Elixir.Testcontainers.PortWaitStrategy", "new")
pub fn port(
  ip: String,
  port: Int,
  timeout: Int,
  retry_delay: Int,
) -> WaitStrategy

/// Wait until a container log line matches `pattern`.
///
/// The pattern is compiled to an Elixir Regex before being passed to
/// the underlying `LogWaitStrategy`. Retries every `retry_delay` ms,
/// giving up after `timeout` ms.
pub fn log(pattern: String, timeout: Int, retry_delay: Int) -> WaitStrategy {
  let regex = do_compile_regex(pattern)
  do_log(regex, timeout, retry_delay)
}

/// Wait until `command` exits with status 0 inside the container.
///
/// Retries every `retry_delay` ms, giving up after `timeout` ms.
@external(erlang, "Elixir.Testcontainers.CommandWaitStrategy", "new")
pub fn command(
  cmd: List(String),
  timeout: Int,
  retry_delay: Int,
) -> WaitStrategy

// --- Private FFI ---

type Regex

@external(erlang, "Elixir.Regex", "compile!")
fn do_compile_regex(pattern: String) -> Regex

@external(erlang, "Elixir.Testcontainers.LogWaitStrategy", "new")
fn do_log(regex: Regex, timeout: Int, retry_delay: Int) -> WaitStrategy
