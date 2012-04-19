-module(kappa_tests).

-import(kappa, [start/0, stop/0, add/5, delete/5, all/2, only/2, every/3]).

-include_lib("eunit/include/eunit.hrl").

application_test_() ->
    [
        {"start",
            fun() ->
                ?assertEqual(ok,
                             application:start(kappa)),
                ?assertEqual(ok,
                             application:stop(kappa))
            end}
    ].


add_test_() ->
    {setup,
        fun() ->
            meck:new(dummy),
            meck:expect(dummy, function1, fun(X) -> X + 10 end),
            meck:expect(dummy, function2, fun(X) -> X + 10 end),
            meck:expect(dummy, function3, fun(X, Y) -> X + Y + 10 end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            {"start",
             ?_assertEqual(ok,
                           start())},
            {"start",
             ?_assertEqual({error, {already_started, kappa}},
                           start())},
            {"add",
             ?_assertEqual(ok,
                           add(id, 10, dummy, function1, 1))},

            {"error undefine function",
             ?_assertError({undef_function, dummy, function4, 1},
                           add(id, 20, dummy, function4, 1))},

            {"error undefine module",
                ?_assertError({undef_module, dummy1},
                           add(id, 10, dummy1, function1, 1))},

            {"error duplicate function",
                ?_assertError({duplicate_function, id, 10, dummy, function1, 1},
                           add(id, 10, dummy, function1, 1))},

            {"error duplicate priority",
                ?_assertError({duplicate_priority, id, 10, dummy, function2, 1},
                           add(id, 10, dummy, function2, 1))},

            {"error invalid_arity",
                ?_assertError({invalid_arity, id, 30, dummy, function3, 2},
                           add(id, 30, dummy, function3, 2))},

            {"add",
             ?_assertEqual(ok,
                           add(id, 20, dummy, function2, 1))},

            {"stop",
             ?_assertEqual(ok,
                           stop())}
        ]
    }.

delete_test_() ->
    {setup,
        fun() ->
            meck:new(dummy),
            meck:expect(dummy, function1, fun(X) -> X + 10 end),
            meck:expect(dummy, function2, fun(X) -> X + 10 end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
          {"start",
            ?_assertEqual(ok,
                          start())},
          {"missing id",
            ?_assertEqual({error, missing_id},
                          delete(id, 10, dummy, function1, 1))},
          {"add",
            ?_assertEqual(ok,
                          add(id, 10, dummy, function1, 1))},

          {"add",
            ?_assertEqual(ok,
                          add(id, 20, dummy, function2, 1))},
          {"delete",
            ?_assertEqual({error, missing_hook},
                          delete(id, 10, dummy, function3, 1))},
          {"delete",
            ?_assertEqual(ok,
                          delete(id, 10, dummy, function1, 1))},
          {"delete",
            ?_assertEqual(ok,
                          delete(id, 20, dummy, function2, 1))},

          {"stop",
            ?_assertEqual(ok,
                          stop())}
        ]
    }.
 

only_test_() ->
    {setup,
        fun() ->
            meck:new(dummy),
            meck:expect(dummy, function1, fun(X) -> X + 10 end),
            meck:expect(dummy, function2, fun(X) -> X + 20 end),
            meck:expect(dummy, function3, fun(X, Y) -> X + Y + 10 end),
            meck:expect(dummy, function4, fun(_X) -> next end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            {"start",
                ?_assertEqual(ok,
                              start())},
            {"no add",
                ?_assertEqual(not_found,
                              only(id, [10]))},
            {"add function4/1",
                ?_assertEqual(ok,
                              add(id, 10, dummy, function4, 1))},
            {"no catch function",
                ?_assertEqual(not_found,
                              only(id, [10]))},
            {"add function1/1",
                ?_assertEqual(ok,
                              add(id, 20, dummy, function1, 1))},
            {"add + 0",
                ?_assertEqual(10,
                              only(id, [0]))},
            {"add + 10",
                ?_assertEqual(20,
                              only(id, [10]))},
            {"add function1/2",
                ?_assertEqual(ok,
                              add(id, 30, dummy, function2, 1))},
            {"",
                ?_assertEqual(20,
                              only(id, [10]))},
            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              only(id, [a]))},

            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              only(id, [10, 20]))},
            {"stop",
                ?_assertEqual(ok,
                              stop())}
        ]
    }.

all_test_() ->
    {setup,
        fun() ->
            meck:new(dummy),
            meck:expect(dummy, function1, fun(X) -> X + 10 end),
            meck:expect(dummy, function2, fun(X) -> X + 20 end),
            meck:expect(dummy, function3, fun(X, Y) -> X + Y + 10 end),
            meck:expect(dummy, function4, fun(X) -> X + 30 end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            {"start",
                ?_assertEqual(ok,
                              start())},
            {"no add",
                ?_assertEqual(not_found,
                              all(id, [10]))},
            {"add function4/1",
                ?_assertEqual(ok,
                              add(id, 10, dummy, function4, 1))},
            {"no catch function",
                ?_assertEqual([40],
                              all(id, [10]))},
            {"add function1/1",
                ?_assertEqual(ok,
                              add(id, 20, dummy, function1, 1))},
            {"add + 0",
                ?_assertEqual([30, 10],
                              all(id, [0]))},
            {"add + 10",
                ?_assertEqual([40, 20],
                              all(id, [10]))},
            {"add function1/2",
                ?_assertEqual(ok,
                              add(id, 30, dummy, function2, 1))},
            {"",
                ?_assertEqual([40,20,30],
                              all(id, [10]))},
            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              all(id, [a]))},

            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              all(id, [10, 20]))},
            {"stop",
                ?_assertEqual(ok,
                              stop())}
        ]
    }.


every_test_() ->
    {setup,
        fun() ->
            meck:new(dummy),
            meck:expect(dummy, function1, fun(Acc, X) -> Acc + X + 10 end),
            meck:expect(dummy, function2, fun(Acc, X) -> Acc + X + 20 end),
            meck:expect(dummy, function3, fun(Acc, X) -> Acc + X + 30 end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            {"start",
                ?_assertEqual(ok,
                              start())},
            {"no add",
                ?_assertEqual(not_found,
                              every(id, 0, [10]))},
            {"add function4/1",
                ?_assertEqual(ok,
                              add(id, 10, dummy, function1, 2))},
            {"10 + 10 + 10",
                ?_assertEqual(30,
                              every(id, 10, [10]))},
            {"add function1/1",
                ?_assertEqual(ok,
                              add(id, 20, dummy, function2, 2))},
            {"add + 0",
                ?_assertEqual(50,
                              every(id, 20, [0]))},
            {"add + 10",
                ?_assertEqual(80,
                              every(id, 30, [10]))},
            {"add function1/2",
                ?_assertEqual(ok,
                              add(id, 30, dummy, function3, 2))},
            {"",
                ?_assertEqual(90,
                              every(id, 0, [10]))},
            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function1, 2}},
                              every(id, 0, [a]))},

            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function1, 2}},
                              every(id, 0, [10, 20]))},
            {"stop",
                ?_assertEqual(ok,
                              stop())}
        ]
    }.

