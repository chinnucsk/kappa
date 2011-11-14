-module(kappa_tests).

-export([function1/2,
         function2/2,
         function2/3,
         function3/2]).

-import(kappa, [start/0, stop/0, add/5, call/2, call/3]).

-include_lib("eunit/include/eunit.hrl").

function1(Value, X) ->
  {stop, Value + X}.

function2(Value, X) ->
  {stop, Value - X}.

function2(Value, X, Y) ->
  {stop, Value, X, Y}.

function3(Value, X) ->
  {next, Value + X}.

add_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"add",
      ?_assertEqual(ok,
                    add(id, 10, ?MODULE, function1, 2))},

    {"error undefine function",
      ?_assertError({undef_function, ?MODULE, function3, 3},
                    add(id, 10, ?MODULE, function3, 3))},

    {"error undefine module",
      ?_assertError({undef_module, dummy},
                    add(id, 10, dummy, function1, 2))},

    {"error duplicate function",
      ?_assertError({duplicate_function, id, 10, ?MODULE, function1, 2},
                    add(id, 10, ?MODULE, function1, 2))},

    {"error duplicate priority",
      ?_assertError({duplicate_priority, id, 10, ?MODULE, function2, 2},
                    add(id, 10, ?MODULE, function2, 2))},

    {"error invalid_arity",
      ?_assertError({invalid_arity, id, 30, ?MODULE, function2, 3},
                    add(id, 30, ?MODULE, function2, 3))},

    {"add",
      ?_assertEqual(ok,
                    add(id, 20, ?MODULE, function2, 2))},

    {"stop",
      ?_assertEqual(ok,
                    stop())}
  ].

call_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"no add",
      ?_assertEqual({ok, 0},
                    call(id, 0, [10]))},
    {"add function1/2",
      ?_assertEqual(ok,
                    add(id, 20, ?MODULE, function1, 2))},
    {"",
      ?_assertEqual({ok, 10},
                    call(id, 0, [10]))},
    {"add function3/2",
      ?_assertEqual(ok,
                    add(id, 10, ?MODULE, function3, 2))},
    {"",
      ?_assertEqual({ok, 20},
                    call(id, 0, [10]))},
    {"stop",
      ?_assertEqual(ok,
                    stop())}
  ].
