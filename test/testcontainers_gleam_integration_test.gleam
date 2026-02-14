import envoy
import gleam/dynamic
import gleam/erlang/atom
import gleeunit/should
import testcontainers_gleam.{Config}

fn require_integration() -> Bool {
  case envoy.get("TESTCONTAINERS_INTEGRATION_TESTS") {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn integration_test(test_fn: fn() -> Nil) {
  case require_integration() {
    True -> test_fn()
    False -> Nil
  }
}

pub fn should_start_and_stop_container_test() {
  integration_test(fn() {
    let start_info =
      testcontainers_gleam.start_container(Config(
        "redis:7.4-rc1-alpine3.20",
        6379,
      ))

    start_info.container_id
    |> dynamic.string()
    |> dynamic.classify
    |> should.equal("String")

    start_info.port
    |> dynamic.int()
    |> dynamic.classify
    |> should.equal("Int")

    let stop_info = testcontainers_gleam.stop_container(start_info.container_id)
    stop_info |> should.equal(atom.create("ok"))
  })
}
