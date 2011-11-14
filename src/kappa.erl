-module(kappa).

-author('twisted.mind@voluntas.net').

-export([start/0,
         stop/0]).

-export([add/5,
         delete/5]).

-export([call/2,
         call/3]).

-export([format_error/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TABLE, kappa_table).

-type id() :: atom().
-type priority() :: non_neg_integer().
-type args() :: [any()].
-type value() :: any().

%% TODO(nakai): call0 の名前を変更する

%% 2.0 では -kappa(spam, 2). みたいに登録できるようにする

%% 登録/削除は register/unregister ってのもありか
%% 呼び出しは call? apply? fold?
%% {next, any()} と next; {stop, any()};

-spec start() -> ok | {error, {already_started, kappa}}.
start() ->
  case lists:member(?TABLE, ets:all()) of
    true ->
      {error, {already_started, kappa}};
    false ->
      _Tid = ets:new(?TABLE,
                     [set, public, named_table, {read_concurrency, true}]),
      ok
  end.

-spec stop() -> ok.
stop() ->
  %% テーブルの削除
  true = ets:delete(?TABLE),
  ok.

%% -spec info(id()) -> [{priority(), module(), function(), arity()}].
%% info(Id) ->
%%   ets:lookup(?TABLE, Id).

-spec add(id(), priority(), module(), function(), arity()) -> ok | no_return().
add(Id, Priority, Module, Function, Arity) ->
  try
    %% まずは存在するかどうかチェック
    ListOfTuple = apply(Module, module_info, [exports]),
    case lists:member({Function, Arity}, ListOfTuple) of
      true ->
        %% 登録へ
        Hook = {Priority, Module, Function, Arity},
        case ets:lookup(?TABLE, Id) of
          [] ->
            %% 登録がなかったらすぐ登録
            true = ets:insert(?TABLE, {Id, [Hook]}),
            ok;
          [{Id, ListOfHook}] ->
            %% まるっきり同じフックがあってはいけない
            case lists:member(Hook, ListOfHook) of
              false ->
                %% 同じフック Id に同じ優先度は存在してはいけない
                case lists:keymember(Priority, 1, ListOfHook) of
                  false ->
                    %% Arity が違ってはいけない
                    case lists:keymember(Arity, 4, ListOfHook) of
                      true ->
                        NewListOfHook = lists:merge(ListOfHook, [Hook]),
                        true = ets:insert(?TABLE, {Id, NewListOfHook}),
                        ok;
                      false ->
                        error({invalid_arity, Id, Priority, Module, Function, Arity})
                    end;
                  true ->
                    error({duplicate_priority, Id, Priority, Module, Function, Arity})
                end;
              true ->
                error({duplicate_function, Id, Priority, Module, Function, Arity})
            end
        end;
      false ->
        %% 指定した関数が export されていない
        error({undef_function, Module, Function, Arity})
    end
  catch
    error:undef ->
      %% 指定したモジュールが定義されていない
      error({undef_module, Module})
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
          case lists:delete(Hook, ListOfHook) of
            [] ->
              true = ets:delete(?TABLE, Id),
              ok;  
            NewListOfHook ->
              true = ets:insert(?TABLE, {Id, NewListOfHook}),
              ok
          end;
        false ->
          {error, missing_hook}
      end
  end.

-spec call(id(), value(), args()) -> {ok, value()} | no_return().
call(Id, Value, Args) ->
  case ets:lookup(?TABLE, Id) of
    [] ->
      %% フックが存在しない場合はそのまま返す
      {ok, Value};
    [{Id, ListOfHook}] ->
      call0(ListOfHook, Value, Args)
  end.

-spec call0([{priority(), module(), function(), arity()}], value(), args()) -> {ok, value()} | no_return().
call0([], Value, _Args) ->
  {ok, Value};
call0([{_, Module, Function, Arity}|Rest], Value, Args) ->
  try
    case apply(Module, Function, [Value|Args]) of
      {next, NewValue} ->
        call0(Rest, NewValue, Args);
      {stop, NewValue} ->
        {ok, NewValue}
    end
  catch
    _Class:Reason ->
      %% apply に失敗
      error({invalid_apply, Module, Function, Arity, Value, Args, Reason})
  end.
  
-spec call(id(), args()) -> ok | {ok, term()} | no_return().
call(Id, Args) when is_list(Args) ->
  case ets:lookup(?TABLE, Id) of
    [] ->
      %% フック が存在しない
      ok;
    [{Id, ListOfHook}] ->
      call0(ListOfHook, Args)
  end.

-spec call0([{priority(), module(), function(), arity()}], args()) -> ok | {ok, value()} | no_return(). 
call0([], _Args) ->
  ok;
call0([{_Priority, Module, Function, Arity}|Rest], Args) ->
  try
    case apply(Module, Function, Args) of
      next ->
        call0(Rest, Args);
      {stop, NewValue} ->
        {ok, NewValue}
    end
  catch
    _Class:Reason ->
      %% apply に失敗
      error({invalid_apply, Module, Function, Arity, Args, Reason})
  end.

-spec format_error(term()) -> iolist().
format_error(_Reason) ->
  "Not implemented".
