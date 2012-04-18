-module(kappa_tests).

-export([function1/2,
         function2/2,
         function2/3,
         function3/2,
         function4/1,
         function5/1,
         function6/1,
         function7/2]).

-import(kappa, [start/0, stop/0, add/5, delete/5, call/2, call/3]).

-include_lib("eunit/include/eunit.hrl").

function1(Value, X) ->
  {stop, Value + X}.

function2(Value, X) ->
  {stop, Value - X}.

function2(Value, X, Y) ->
  {stop, Value, X, Y}.

function3(Value, X) ->
  {next, Value + X}.

function4(_X) ->
  next.

function5(X) ->
  {stop, X + 10}.

function6(_X) ->
  ok.

function7(_Value, _X) ->
  ok.

add_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"start",
      ?_assertEqual({error, {already_started, kappa}},
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

delete_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"add",
      ?_assertEqual(ok,
                    add(id, 10, ?MODULE, function1, 2))},

    {"add",
      ?_assertEqual(ok,
                    add(id, 20, ?MODULE, function2, 2))},
    {"delete",
      ?_assertEqual(ok,
                    delete(id, 10, ?MODULE, function1, 2))},
    {"delete",
      ?_assertEqual(ok,
                    delete(id, 20, ?MODULE, function2, 2))},

    {"stop",
      ?_assertEqual(ok,
                    stop())}
  ].

call3_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"no add",
      ?_assertEqual(ok,
                    call(id, 0, [10]))},
    {"add function3/2",
      ?_assertEqual(ok,
                    add(id, 10, ?MODULE, function3, 2))},
    {"add + 0",
      ?_assertEqual(ok,
                    call(id, 0, [0]))},
    {"add + 10",
      ?_assertEqual({ok, 10},
                    call(id, 0, [10]))},
    {"add function1/2",
      ?_assertEqual(ok,
                    add(id, 20, ?MODULE, function1, 2))},
    {"",
      ?_assertEqual({ok, 20},
                    call(id, 0, [10]))},

    {"add function6/1",
      ?_assertEqual(ok,
                    add(error, 10, ?MODULE, function7, 2))},

    {"error invalid_apply",
      ?_assertError({invalid_apply, ?MODULE, function7, 2, 0, [10], {case_clause, ok}},
                    call(error, 0, [10]))},

    {"error invalid_apply",
      ?_assertError({invalid_apply, ?MODULE, function7, 2, 0, [10, 20], undef},
                    call(error, 0, [10, 20]))},
    {"stop",
      ?_assertEqual(ok,
                    stop())}
  ].

call2_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"no add",
      ?_assertEqual(ok,
                    call(id, [10]))},
    {"add function4/1",
      ?_assertEqual(ok,
                    add(id, 20, ?MODULE, function4, 1))},
    {"",
      ?_assertEqual(ok,
                    call(id, [10]))},
    {"add function5/1",
      ?_assertEqual(ok,
                    add(id, 10, ?MODULE, function5, 1))},
    {"",
      ?_assertEqual({ok, 20},
                    call(id, [10]))},

    {"add function6/1",
      ?_assertEqual(ok,
                    add(error, 10, ?MODULE, function6, 1))},

    {"error invalid_apply",
      ?_assertError({invalid_apply, ?MODULE, function6, 1, [10], {case_clause, ok}},
                    call(error, [10]))},

    {"error invalid_apply",
      ?_assertError({invalid_apply, ?MODULE, function6, 1, [10, 20], undef},
                    call(error, [10, 20]))},

    {"stop",
      ?_assertEqual(ok,
                    stop())}
  ].

