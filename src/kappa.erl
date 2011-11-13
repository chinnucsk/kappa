-module(kappa).

-author('twisted.mind@voluntas.net').

-export([start/0,
         stop/0]).

-export([add/5,
         delete/5]).

-export([call/2,
         call/3]).

-export([format_error/1]).

-define(TABLE, kappa_table).

-type id() :: atom().
-type priority() :: non_neg_integer().
-type args() :: [any()].
-type value() :: any().

%% TODO(nakai): call0 の名前を変更する

%% フックを new するもありか引数を指定できる感じで kappa:new(atom(), arity()).

%% 登録/削除は register/unregister ってのもありか
%% 呼び出しは call? apply? fold?
%% {next, any()} と next; {stop, any()};

%% サンプル
%% main() ->
%%   ok = kappa:add(spam, 5, spam, ham, 4),
%%   ok = kappa:add(spam, 10, spam, bacon, 4),
%%   case kappa:call(spam, 0, [1,2,3]) of
%%     {ok, Sum} ->
%%       %% どこかで止められた
%%       %% もどりがエラーだとしてもなんだとしても ok で返ってくる
%%       %% {ok, Respond}, {ok, 404}, {ok, {error, spam}} ... 気持ち悪いけどそんなイメージ
%%     {error, term()} ->
%%       %% エラー
%%       ...
%%   end.
%% 
%% -spec ham(integer(), integer(), integer()) -> {next, integer()}.
%% ham(Sum, A, B, C) ->
%%   {next, Sum + A + B + C}.
%% 
%% -spec bacon(integer(), integer(), integer()) -> {stop, integer()}.
%% bacon(Sum, A, B, C) ->
%%   {ok, Sum + A}.

-spec start() -> ok.
start() ->
  %% TODO(nakai): write/read concurrency を適用するか検討する
  _Tid = ets:new(?TABLE, [set, public, named_table]),
  ok.

-spec stop() -> ok.
stop() ->
  %% テーブルの削除
  true = ets:delete(?TABLE),
  ok.

-spec add(id(), priority(), module(), function(), arity()) -> ok | {error, term()}.
add(Id, Priority, Module, Function, Arity) ->
  %% まずは存在するかどうかチェック
  try
    ListOfTuple = apply(Module, module_info, [exports]),
    case lists:member({Function, Arity}, ListOfTuple) of
      true ->
        %% 登録へ
        Hook = {Priority, Module, Function, Arity},
        case lists:lookup(?TABLE, Id) of
          [] ->
            true = ets:insert(?TABLE, {Id, [Hook]}),
            ok;
          %% Hook, {Priority, Module, Funciton, Arity}
          [{Id, ListOfHook}] ->
            %% TODO(nakai): 同じ数値があったらエラーにする同じフック Id に同じ優先度は存在してはいけない
            %% TODO(nakai): まるっきり同じフックがあってはいけない
            %% TODO(nakai): Arity が違ってはいけない
            NewListOfHook = lists:merge(ListOfHook, [Hook]),
            true = ets:insert(?TABLE, {Id, NewListOfHook}),
            ok
        end;
      false ->
        %% 指定した関数が export されていない
        {error, {undef_function, Module, Function, Arity}}
    end
  catch
    error:undef ->
      %% 指定したモジュールが定義されていない
      {error, {undef_module, Module}}
  end.

-spec delete(id(), priority(), module(), function(), arity()) -> ok | {error, term()}.
delete(Id, Priority, Module, Function, Arity) ->
  case ets:lookup(?TABLE, Id) of
    [] ->
      {error, missing_id};
    [{Id, ListOfHook}] ->
      Hook = {Priority, Module, Function, Arity},
      case lists:member(Hook, ListOfHook) of
        true ->
          NewListOfHook = lists:delete(Hook, ListOfHook),
          true = ets:insert(?TABLE, {Id, NewListOfHook}),
          ok;
        false ->
          {error, missing_hook}
      end
  end.

-spec call(id(), value(), args()) -> {ok, value()} | {error, term()}.
call(Id, Value, Args) ->
  case ets:lookup(?TABLE, Id) of
    [] ->
      %% フックが存在しない
      {error, invalid_call};
    [{Id, ListOfHook}] ->
      call0(ListOfHook, Value, Args)
  end.

-spec call0([{priority(), module(), fun(), arity()}], value(), args()) -> {ok, value()} | {error, term()}.
call0([], Value, _Args) ->
  {ok, Value};
call0([{_, Module, Function, Arity}|Rest], Value, Args) ->
  try
    case apply(Module, Function, [Value|Args]) of
      {next, Value} ->
        call0(Rest, Value, Args);
      {stop, Value} ->
        {ok, Value};
      _ ->
        {error, invalid_args}
    end
  catch
    _:_ ->
      %% apply に失敗
      {error, {invalid, Module, Function, Arity, Value, Args}}
  end.
  
-spec call(id(), args()) -> ok | {ok, term()} | {error, term()}.
call(Id, Args) when is_list(Args) ->
  case ets:lookup(?TABLE, Id) of
    [] ->
      %% フック が存在しない
      {eror, missing_id};
    [{Id, ListOfHook}] ->
      call0(ListOfHook, Args)
  end.

-spec call0([{priority(), module(), fun(), arity()}], args()) -> ok | {ok, value()} | {error, term()}.
call0([], _Args) ->
  ok;
call0([{_Priority, Module, Function, Arity}|Rest], Args) ->
  try
    case apply(Module, Function, Args) of
      next ->
        call0(Rest, Args);
      {stop, Value} ->
        {ok, Value};
      _ ->
        {error, invalid_args}
    end
  catch
    _:_ ->
      %% apply に失敗
      {error, {invalid, Module, Function, Arity, Args}}
  end.

-spec format_error(term()) -> iolist().
format_error(_Reason) ->
  "Not implemented".

-ifdef(TEST).

add_test_() ->
  {setup,
    fun() ->
      meck:new(module),
      meck:expect(module, function, 2, {stop, 10})
    end,
    fun(_) ->
      meck:unload()
    end,
    [
      {"start",
        ?_assertEqual(ok, start())},
      {"add",
        ?_assertEqual(ok, add(id, 10, module, function, 2))},
      {"error duplicate function",
        ?_assertEqual({error, {duplicate_function, }, add())},
      {"error duplicate priority",
        ?_assertEqual({error, {duplicate_priority, }, add(id, module, function, ))},
      {"error invalid_arity",
        ?_assertEqual({error, {invalid_arity, }}, add())},
      {"stop",
        ?_assertEqual(ok, stop())}
    ]
  }.

-endif.
