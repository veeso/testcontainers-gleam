//// Utilities for running testcontainers integration tests.
////
//// Provides a test runner with per-test timeouts suitable for container
//// startup, and a guard function to skip tests when Docker is not available.
////
//// ## Usage
////
//// Replace `gleeunit.main()` with `integration.main()` in your test entry
//// point, and use `integration.guard()` to gate individual tests behind the
//// `TESTCONTAINERS_INTEGRATION_TESTS` environment variable:
////
//// ```gleam
//// import testcontainers_gleam/integration
////
//// pub fn main() {
////   integration.main()
//// }
////
//// pub fn redis_test() {
////   use <- integration.guard()
////   // ... start container, run assertions ...
//// }
//// ```

import envoy

/// Run all tests with a 600-second per-test timeout.
///
/// Automatically sets `TESTCONTAINERS_RYUK_DISABLED=1` so the Ryuk sidecar
/// container is not started. This is a drop-in replacement for
/// `gleeunit.main()`.
///
/// ## Examples
///
/// ```gleam
/// import testcontainers_gleam/integration
///
/// pub fn main() {
///   integration.main()
/// }
/// ```
pub fn main() -> Nil {
  main_with_timeout(600)
}

/// Run all tests with a custom per-test timeout in seconds.
///
/// Automatically sets `TESTCONTAINERS_RYUK_DISABLED=1` so the Ryuk sidecar
/// container is not started.
///
/// ## Examples
///
/// ```gleam
/// import testcontainers_gleam/integration
///
/// pub fn main() {
///   integration.main_with_timeout(120)
/// }
/// ```
pub fn main_with_timeout(timeout_seconds timeout_seconds: Int) -> Nil {
  envoy.set("TESTCONTAINERS_RYUK_DISABLED", "1")
  let code = case run_tests(timeout_seconds) {
    Ok(_) -> 0
    Error(_) -> 1
  }
  halt(code)
}

/// Gate a test behind the `TESTCONTAINERS_INTEGRATION_TESTS` environment
/// variable. When the variable is not set, the test body is skipped and
/// `Nil` is returned.
///
/// ## Examples
///
/// ```gleam
/// pub fn redis_test() {
///   use <- integration.guard()
///   // ... start container, run assertions ...
/// }
/// ```
pub fn guard(test_fn: fn() -> Nil) -> Nil {
  case envoy.get("TESTCONTAINERS_INTEGRATION_TESTS") {
    Ok(_) -> test_fn()
    Error(_) -> Nil
  }
}

@external(erlang, "integration_ffi", "run")
fn run_tests(timeout_seconds: Int) -> Result(Nil, a)

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil
