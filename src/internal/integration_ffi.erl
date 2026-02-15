-module(integration_ffi).
-export([run/1]).

%% Custom test runner that wraps each test function with an individual timeout.
%%
%% eunit's default per-test timeout is 5 seconds.  gleeunit's
%% `{timeout, 60, Tests}` only overrides the GROUP timeout, not the
%% per-test timeout.  By discovering every _test/0 function and wrapping
%% each one in `{timeout, T, {M, F}}` we ensure slow containers like
%% Cassandra or Ceph get enough time.
run(Timeout) ->
    Files   = filelib:wildcard("**/*.{erl,gleam}", "test"),
    Modules = lists:map(fun(F) ->
        Name = gleam_module_name(list_to_binary(F)),
        binary_to_atom(Name, utf8)
    end, Files),
    Tests   = lists:flatmap(fun(M) -> test_funs(M, Timeout) end, Modules),
    Options = [verbose, no_tty,
               {report, {gleeunit_progress, [{colored, true}]}}],
    case eunit:test(Tests, Options) of
        ok            -> {ok, nil};
        error         -> {error, nil};
        {error, Term} -> {error, Term}
    end.

%% Return a list of `{timeout, T, {M, F}}` for every exported _test/0
%% function in Module.
test_funs(Module, Timeout) ->
    try Module:module_info(exports) of
        Exports ->
            Funs = [F || {F, 0} <- Exports, is_test_fun(F)],
            [{timeout, Timeout, {Module, F}} || F <- Funs]
    catch
        _:_ -> []
    end.

is_test_fun(Name) ->
    Str = atom_to_list(Name),
    lists:suffix("_test", Str) orelse lists:suffix("_test_", Str).

gleam_module_name(Path) ->
    case binary:match(Path, <<".gleam">>) of
        nomatch ->
            %% Plain .erl file: take the basename without extension
            Parts = binary:split(Path, <<"/">>, [global]),
            Last  = lists:last(Parts),
            binary:part(Last, 0, byte_size(Last) - 4);
        _ ->
            %% Gleam file: strip .gleam, replace / with @
            WithoutExt = binary:part(Path, 0, byte_size(Path) - 6),
            binary:replace(WithoutExt, <<"/">>, <<"@">>, [global])
    end.
