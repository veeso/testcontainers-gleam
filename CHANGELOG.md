# Changelog

## 2.0.0

### Breaking changes

- Redesigned the entire API around a modular architecture. The previous single-module approach (`testcontainers_gleam`) has been replaced with dedicated modules under `testcontainers_gleam/`.
- Introduced `testcontainers_gleam/container` as the core container builder with a full builder API (`new`, `with_exposed_port`, `with_environment`, `with_cmd`, `with_bind_mount`, `with_bind_volume`, `with_label`, `with_auto_remove`, `with_reuse`, `with_network_mode`, `with_auth`, `with_fixed_port`, `with_check_image`, `with_pull_policy`).
- Added `testcontainers_gleam/wait_strategy` module with three wait strategies: `port`, `log`, and `command`.

### New features

- Added pre-configured container modules with idiomatic builder APIs:
  - `testcontainers_gleam/redis` — Redis container
  - `testcontainers_gleam/postgres` — PostgreSQL container with user, password, database, and persistent volume support
  - `testcontainers_gleam/mysql` — MySQL container with user, password, database, and persistent volume support
  - `testcontainers_gleam/cassandra` — Apache Cassandra container
  - `testcontainers_gleam/ceph` — Ceph object storage container with S3-compatible API
  - `testcontainers_gleam/kafka` — Apache Kafka (KRaft mode) container with `after_start` hook for broker configuration
  - `testcontainers_gleam/emqx` — EMQX MQTT broker container
  - `testcontainers_gleam/minio` — MinIO S3-compatible object storage container
  - `testcontainers_gleam/rabbitmq` — RabbitMQ message broker container
- Added `build_container` and `after_start` FFI functions to support containers that need post-start configuration (e.g. Kafka).
- Added pull policy support (`AlwaysPull`, `NeverPull`).
