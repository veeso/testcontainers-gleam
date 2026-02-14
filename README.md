# testcontainers_gleam

[![Package Version](https://img.shields.io/hexpm/v/testcontainers_gleam)](https://hex.pm/packages/testcontainers_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/testcontainers_gleam/)

Gleam wrapper around [Elixir TestContainers](https://github.com/testcontainers/testcontainers-elixir) for managing Docker containers in tests.

## Installation

```sh
gleam add --dev testcontainers_gleam
```

## Quick start

```gleam
import testcontainers_gleam
import testcontainers_gleam/container

pub fn demo_test() {
  // Build a container definition
  let c =
    container.new("redis:7.4-alpine")
    |> container.with_exposed_port(6379)
    |> container.with_environment("REDIS_PASSWORD", "secret")

  // Start the container (GenServer is started automatically)
  let assert Ok(running) = testcontainers_gleam.start_container(c)

  // Query the running container
  let id = container.container_id(running)
  let assert Ok(host_port) = container.mapped_port(running, 6379)

  // ... use host_port to connect to Redis ...

  // Stop the container
  let assert Ok(Nil) = testcontainers_gleam.stop_container(id)
}
```

## Container configuration

The `container` module exposes a builder API for configuring containers:

```gleam
import testcontainers_gleam/container
import testcontainers_gleam/wait_strategy

container.new("postgres:16-alpine")
|> container.with_exposed_port(5432)
|> container.with_environment("POSTGRES_PASSWORD", "test")
|> container.with_cmd(["postgres", "-c", "log_statement=all"])
|> container.with_label("project", "my_app")
|> container.with_waiting_strategy(
  wait_strategy.log("database system is ready to accept connections", 30_000, 1000),
)
|> container.with_auto_remove(True)
```

### Available builder functions

| Function | Description |
| --- | --- |
| `with_exposed_port` | Expose a single port (mapped to a random host port) |
| `with_exposed_ports` | Expose multiple ports at once |
| `with_fixed_port` | Bind a container port to a specific host port |
| `with_environment` | Set an environment variable |
| `with_cmd` | Set the container command |
| `with_bind_mount` | Mount a host path into the container |
| `with_bind_volume` | Mount a named Docker volume |
| `with_label` | Add a container label |
| `with_waiting_strategy` | Add a readiness wait strategy |
| `with_auto_remove` | Auto-remove the container on exit |
| `with_reuse` | Reuse containers across test runs |
| `with_network_mode` | Set the network mode (`"bridge"`, `"host"`, etc.) |
| `with_auth` | Set registry credentials for private images |
| `with_check_image` | Set an image name validation pattern |
| `with_pull_policy` | Set the pull policy (`AlwaysPull` or `NeverPull`) |

## Wait strategies

Wait strategies control how testcontainers detects that a container is ready:

```gleam
import testcontainers_gleam/wait_strategy

// Wait for a TCP port to accept connections
wait_strategy.port("0.0.0.0", 5432, 5000, 500)

// Wait for a log line matching a regex pattern
wait_strategy.log("Ready to accept connections", 10_000, 1000)

// Wait for a command to exit with status 0
wait_strategy.command(["pg_isready"], 10_000, 1000)
```

All strategies take `timeout` (max wait in ms) and `retry_delay` (polling interval in ms) parameters.

## Requirements

- Docker must be running
- Erlang/OTP and Elixir (this library wraps an Elixir dependency)

Further documentation can be found at <https://hexdocs.pm/testcontainers_gleam>.

## Development

```sh
gleam deps download                                                            # Download dependencies
TESTCONTAINERS_RYUK_DISABLED=1 TESTCONTAINERS_INTEGRATION_TESTS=1 gleam test   # Run the tests (requires Docker)
gleam format src test                                                          # Format code
```

## License

testcontainers-gleam is licensed under the MIT license. See full license [HERE](./LICENSE)
