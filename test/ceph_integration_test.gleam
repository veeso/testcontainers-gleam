import envoy
import gleam/string
import gleeunit/should
import testcontainers_gleam
import testcontainers_gleam/ceph
import testcontainers_gleam/container

fn integration_test(test_fn: fn() -> Nil) {
  case envoy.get("TESTCONTAINERS_INTEGRATION_TESTS") {
    Ok(_) -> test_fn()
    Error(_) -> Nil
  }
}

pub fn ceph_module_start_and_stop_test() {
  integration_test(fn() {
    let config = ceph.new()
    let built = ceph.build(config)

    let running = testcontainers_gleam.start_container(built) |> should.be_ok()
    let id = container.container_id(running)

    let p = ceph.port(running)
    { p > 0 } |> should.be_true()

    let url = ceph.connection_url(running)
    url |> string.is_empty() |> should.be_false()

    testcontainers_gleam.stop_container(id) |> should.be_ok()
  })
}
