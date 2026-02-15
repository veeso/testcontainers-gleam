import envoy
import gleam/string
import gleeunit/should
import testcontainers_gleam
import testcontainers_gleam/container
import testcontainers_gleam/rabbitmq

fn integration_test(test_fn: fn() -> Nil) {
  case envoy.get("TESTCONTAINERS_INTEGRATION_TESTS") {
    Ok(_) -> test_fn()
    Error(_) -> Nil
  }
}

pub fn rabbitmq_module_start_and_stop_test() {
  integration_test(fn() {
    let config = rabbitmq.new()
    let built = rabbitmq.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = rabbitmq.port(running)
    { p > 0 } |> should.be_true()

    let url = rabbitmq.connection_url(running)
    url |> string.contains("amqp://") |> should.be_true()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
