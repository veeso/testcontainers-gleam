-module(testcontainers_gleam_ffi).
-export([start_container/1, build_container/1, after_start/2]).

%% Wraps Testcontainers.start_container/1 to normalize 3-element error tuples
%% returned by wait strategies into standard 2-element {error, Reason} tuples.
start_container(Container) ->
    case 'Elixir.Testcontainers':start_container(Container) of
        {ok, Running} -> {ok, Running};
        {error, Reason} -> {error, Reason};
        {error, Reason, _WaitStrategy} -> {error, Reason}
    end.

%% Dispatches through Elixir's ContainerBuilder protocol to convert a
%% builder config struct (e.g. %RedisContainer{}) into a %Container{}.
build_container(Builder) ->
    'Elixir.Testcontainers.ContainerBuilder':build(Builder).

%% Calls the ContainerBuilder after_start/3 hook for builders that need
%% post-start initialization (e.g. Kafka uploading a startup script).
after_start(Builder, Container) ->
    #{conn := Conn} = sys:get_state('Elixir.Testcontainers'),
    case 'Elixir.Testcontainers.ContainerBuilder':after_start(Builder, Container, Conn) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
