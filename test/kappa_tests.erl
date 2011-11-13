-module(kappa_tests).

-export([function1/2,
         function2/2,
         function2/3]).

-import(kappa, [start/0, stop/0, add/5]).

-include_lib("eunit/include/eunit.hrl").

function1(X, Y) ->
  {stop, X + Y}.

function2(X, Y) ->
  {stop, X - Y}.

function2(X, Y, Z) ->
  {stop, X, Y, Z}.

add_test_() ->
  [
    {"start",
      ?_assertEqual(ok,
                    start())},
    {"new add",
      ?_assertEqual(ok,
                    add(id, 10, ?MODULE, function1, 2))},

    {"error undefine function",
      ?_assertEqual({error, {undef_function, ?MODULE, function3, 2}},
                    add(id, 10, ?MODULE, function3, 2))},

    {"error undefine module",
      ?_assertEqual({error, {undef_module, dummy}},
                    add(id, 10, dummy, function1, 2))},

    {"error duplicate function",
      ?_assertEqual({error, {duplicate_function, id, 10, ?MODULE, function1, 2}},
                    add(id, 10, ?MODULE, function1, 2))},

    {"error duplicate priority",
      ?_assertEqual({error, {duplicate_priority, id, 10, ?MODULE, function2, 2}},
                    add(id, 10, ?MODULE, function2, 2))},

    {"error invalid_arity",
      ?_assertEqual({error, {invalid_arity, id, 30, ?MODULE, function2, 3}},
                    add(id, 30, ?MODULE, function2, 3))},

    {"exist add",
      ?_assertEqual(ok,
                    add(id, 20, ?MODULE, function2, 2))},

    {"stop",
      ?_assertEqual(ok,
                    stop())}
  ].

