-module(testcontainers_gleam_ffi).
-export([start_container/1]).

%% Wraps Testcontainers.start_container/1 to normalize 3-element error tuples
%% returned by wait strategies into standard 2-element {error, Reason} tuples.
start_container(Container) ->
    case 'Elixir.Testcontainers':start_container(Container) of
        {ok, Running} -> {ok, Running};
        {error, Reason} -> {error, Reason};
        {error, Reason, _WaitStrategy} -> {error, Reason}
    end.
