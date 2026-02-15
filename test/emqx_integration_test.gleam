import envoy
import gleeunit/should
import testcontainers_gleam
import testcontainers_gleam/container
import testcontainers_gleam/emqx

fn integration_test(test_fn: fn() -> Nil) {
  case envoy.get("TESTCONTAINERS_INTEGRATION_TESTS") {
    Ok(_) -> test_fn()
    Error(_) -> Nil
  }
}

pub fn emqx_module_start_and_stop_test() {
  integration_test(fn() {
    let config = emqx.new()
    let built = emqx.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = emqx.mqtt_port(running)
    { p > 0 } |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
