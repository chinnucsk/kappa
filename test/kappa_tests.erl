-module(kappa_tests).

-import(kappa, [start/0, stop/0, add/4, delete/4, declare/3, all/2, only/2, every/3]).

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

declare_test_() ->
    [
        {"start",
         ?_assertEqual(ok,
                       start())},
        {"start",
         ?_assertEqual({error, {already_started, kappa}},
                       start())},
        {"register: spam only 1",
         ?_assertEqual(ok,
                       declare(spam, only, 1))},
        {"register: spam only 1",
         ?_assertError({duplicate_name, spam},
                       declare(spam, only, 1))},
        {"register: spam every 2",
         ?_assertError({duplicate_name, spam},
                       declare(spam, every, 2))},
        {"stop",
         ?_assertEqual(ok,
                       stop())}
    ].

add_test_() ->
    {setup,
        fun() ->
            meck:new(dummy),
            meck:expect(dummy, function1, fun(X) -> X + 10 end),
            meck:expect(dummy, function2, fun(X) -> X + 20 end),
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
            {"error not declare spam only 1",
             ?_assertError({missing_declare, spam},
                           add(spam, only, 10, {dummy, function1, 1}))},
            {"register: spam only 1",
             ?_assertEqual(ok,
                           declare(spam, only, 1))},
            {"add",
             ?_assertEqual(ok,
                           add(spam, only, 10, {dummy, function1, 1}))},
            {"add",
             ?_assertEqual(ok,
                 catch add(spam, only, 20, {dummy, function2, 1}))},

            {"error undefine function",
             ?_assertError({undef_function, dummy, function4, 1},
                           add(spam, only, 20, {dummy, function4, 1}))},

            {"error undefine module",
                ?_assertError({undef_module, dummy1},
                           add(spam, only, 10, {dummy1, function1, 1}))},

            {"error invalid type",
                ?_assertError({invalid_type, spam, all, {30, {dummy, function2, 1}}},
                           add(spam, all, 30, {dummy, function2, 1}))},

            {"error invalid arity",
                ?_assertError({invalid_arity, spam, only, {30, {dummy, function3, 2}}},
                           add(spam, only, 30, {dummy, function3, 2}))},

            {"error duplicate mfa",
                ?_assertError({duplicate_mfa, spam, only, {30, {dummy, function1, 1}}},
                           add(spam, only, 30, {dummy, function1, 1}))},

            {"error duplicate priority",
                ?_assertError({duplicate_priority, spam, only, {10, {dummy, function2, 1}}},
                           add(spam, only, 10, {dummy, function2, 1}))},

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
            {"missing spam",
                ?_assertError({missing_declare, spam},
                              delete(spam, only, 10, {dummy, function1, 1}))},

            {"register: spam only 1",
                ?_assertEqual(ok,
                              declare(spam, only, 1))},

            {"add",
                ?_assertEqual(ok,
                              add(spam, only, 10, {dummy, function1, 1}))},

            {"add",
                ?_assertEqual(ok,
                              add(spam, only, 20, {dummy, function2, 1}))},
            {"delete",
                ?_assertError({missing_hook, {10, {dummy, function3, 1}}},
                              delete(spam, only, 10, {dummy, function3, 1}))},
            {"delete",
                ?_assertEqual(ok,
                              delete(spam, only, 10, {dummy, function1, 1}))},
            {"delete",
                ?_assertEqual(ok,
                              delete(spam, only, 20, {dummy, function2, 1}))},
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
            {"missing declare",
                ?_assertError({missing_declare, spam},
                              only(spam, [10]))},
            {"declare: spam only 1",
                ?_assertEqual(ok,
                              declare(spam, only, 1))},
            {"no add",
                ?_assertEqual(not_found,
                              only(spam, [10]))},
            {"add function4/1",
                ?_assertEqual(ok,
                              add(spam, only, 10, {dummy, function4, 1}))},
            {"no catch function",
                ?_assertEqual(not_found,
                              only(spam, [10]))},
            {"add function1/1",
                ?_assertEqual(ok,
                              add(spam, only, 20, {dummy, function1, 1}))},
            {"add + 0",
                ?_assertEqual(10,
                              only(spam, [0]))},
            {"add + 10",
                ?_assertEqual(20,
                              only(spam, [10]))},
            {"add function1/2",
                ?_assertEqual(ok,
                              add(spam, only, 30, {dummy, function2, 1}))},
            {"",
                ?_assertEqual(20,
                              only(spam, [10]))},
            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              only(spam, [a]))},

            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              only(spam, [10, 20]))},
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
            {"missing declare",
                ?_assertError({missing_declare, spam},
                              all(spam, [10]))},
            {"declare: spam all 1",
                ?_assertEqual(ok,
                              declare(spam, all, 1))},
            {"no add",
                ?_assertEqual([],
                              all(spam, [10]))},
            {"add function4/1",
                ?_assertEqual(ok,
                              add(spam, all, 10, {dummy, function4, 1}))},
            {"no catch function",
                ?_assertEqual([40],
                              all(spam, [10]))},
            {"add function1/1",
                ?_assertEqual(ok,
                              add(spam, all, 20, {dummy, function1, 1}))},
            {"add + 0",
                ?_assertEqual([30, 10],
                              all(spam, [0]))},
            {"add + 10",
                ?_assertEqual([40, 20],
                              all(spam, [10]))},
            {"add function1/2",
                ?_assertEqual(ok,
                              add(spam, all, 30, {dummy, function2, 1}))},
            {"",
                ?_assertEqual([40,20,30],
                              all(spam, [10]))},
            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              all(spam, [a]))},

            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function4, 1}},
                              all(spam, [10, 20]))},
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
            {"missing declare",
                ?_assertError({missing_declare, spam},
                              every(spam, 0, [10]))},
            {"declare: spam every 2",
                ?_assertEqual(ok,
                              declare(spam, every, 2))},
            {"no add",
                ?_assertEqual(0,
                              every(spam, 0, [10]))},
            {"add function4/1",
                ?_assertEqual(ok,
                              add(spam, every, 10, {dummy, function1, 2}))},
            {"10 + 10 + 10",
                ?_assertEqual(30,
                              every(spam, 10, [10]))},
            {"add function1/1",
                ?_assertEqual(ok,
                              add(spam, every, 20, {dummy, function2, 2}))},
            {"add + 0",
                ?_assertEqual(50,
                              every(spam, 20, [0]))},
            {"add + 10",
                ?_assertEqual(80,
                              every(spam, 30, [10]))},
            {"add function1/2",
                ?_assertEqual(ok,
                              add(spam, every, 30, {dummy, function3, 2}))},
            {"",
                ?_assertEqual(90,
                              every(spam, 0, [10]))},
            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function1, 2}},
                              every(spam, 0, [a]))},

            {"error invalid_apply",
                ?_assertError({invalid_apply, {dummy, function1, 2}},
                              every(spam, 0, [10, 20]))},
            {"stop",
                ?_assertEqual(ok,
                              stop())}
        ]
    }.

