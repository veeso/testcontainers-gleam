//// Kafka container configuration.
////
//// Default image: `confluentinc/cp-kafka:7.4.3`, port: 9092, timeout: 60s.
//// Uses embedded Zookeeper by default.
////
//// **Important:** After starting the container, you MUST call `after_start`
//// to upload the startup script that configures advertised listeners.
//// Without this, the Kafka broker will not start.
////
//// ## Example
////
//// ```gleam
//// import testcontainers_gleam
//// import testcontainers_gleam/kafka
//// import testcontainers_gleam/container
////
//// let config = kafka.new()
//// let built = kafka.build(config)
//// let assert Ok(running) = testcontainers_gleam.start_container(built)
//// let assert Ok(Nil) = kafka.after_start(config, running)
//// ```

import gleam/result
import internal/testcontainers_ffi
import testcontainers_gleam.{type ContainerError}
import testcontainers_gleam/container.{type Container}

/// Configuration for a Kafka container.
pub type KafkaConfig

/// Create a new Kafka configuration with defaults.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "new")
pub fn new() -> KafkaConfig

/// Override the Docker image.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_image")
pub fn with_image(config: KafkaConfig, image: String) -> KafkaConfig

/// Override the Kafka listener port (default 9092).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_kafka_port")
pub fn with_kafka_port(config: KafkaConfig, port: Int) -> KafkaConfig

/// Override the internal broker port (default 29092).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_broker_port")
pub fn with_broker_port(config: KafkaConfig, port: Int) -> KafkaConfig

/// Override the broker ID (default 1).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_broker_id")
pub fn with_broker_id(config: KafkaConfig, id: Int) -> KafkaConfig

/// Override the wait timeout in milliseconds (default 60000).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_wait_timeout")
pub fn with_wait_timeout(config: KafkaConfig, timeout: Int) -> KafkaConfig

/// Override the default number of topic partitions (default 1).
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_topic_partitions")
pub fn with_topic_partitions(
  config: KafkaConfig,
  partitions: Int,
) -> KafkaConfig

/// Enable or disable container reuse across test runs.
@external(erlang, "Elixir.Testcontainers.KafkaContainer", "with_reuse")
pub fn with_reuse(config: KafkaConfig, reuse: Bool) -> KafkaConfig

/// Build a `Container` from this Kafka configuration.
pub fn build(config: KafkaConfig) -> Container {
  testcontainers_ffi.build_container(config)
}

/// Run the post-start hook that uploads the startup script.
///
/// **This MUST be called after `start_container`.** The startup script
/// configures Kafka's advertised listeners with the correct host-mapped
/// port. Without it, the Kafka broker will not start.
pub fn after_start(
  config: KafkaConfig,
  running: Container,
) -> Result(Nil, ContainerError) {
  testcontainers_ffi.after_start(config, running)
  |> result.map_error(fn(_) { testcontainers_gleam.Unknown })
}
