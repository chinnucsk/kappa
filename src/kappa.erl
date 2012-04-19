-module(kappa).

-author('twisted.mind@voluntas.net').

-export([start/0,
         stop/0]).

-export([add/5,
         delete/5]).

-export([all/2,
         only/2,
         every/3]).

-define(TABLE, kappa_table).

-include_lib("eunit/include/eunit.hrl").

-type id() :: atom().
-type priority() :: non_neg_integer().
-type args() :: [any()].
-type value() :: any().

%% TODO(nakai): call0 の名前を変更する

%% 2.0 では -kappa(spam, 2). みたいに登録できるようにする

%% 登録/削除は register/unregister ってのもありか
%% 呼び出しは call? apply? fold?
%% {next, any()} と next; {stop, any()};

%% all, only, every
%% all はフックポイントに登録してある全ての関数から価を貰って最後に lists:append() して値を戻す
%% only はフックポイントに登録してある関数を優先順位順に見ていって、途中で止められた関数の値を返す
%% every は全ての値で共通の値を処理された値を返す、途中で止めることは出来ない


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
                        verify_add(Id, Priority, Module, Function, Arity, Hook, ListOfHook)
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

verify_add(Id, Priority, Module, Function, Arity, Hook, ListOfHook) ->
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
    end.

%% TODO: エラーハンドリングをしっかりする
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

-spec all(atom(), args()) -> [any()].
all(HookName, Args) ->
    case ets:lookup(?TABLE, HookName) of
        [] ->
            not_found;
        [{HookName, ListOfHook}] ->
            F = fun({_, Module, Function, Arity}) ->
                    try
                        ?debugVal(apply(Module, Function, Args)),
                        apply(Module, Function, Args)
                    catch
                      _Class:_Reason ->
                          %% apply に失敗
                          error({invalid_apply, {Module, Function, Arity}})
                    end
                end,
            ?debugVal(lists:map(F, ListOfHook)),
            lists:map(F, ListOfHook)
    end.

-spec only(atom(), args()) -> not_found | any().
only(HookName, Args) ->
    case ets:lookup(?TABLE, HookName) of
        [] ->
            not_found;
        [{HookName, ListOfHook}] ->
            only0(ListOfHook, Args)
    end.

only0([], _Args) ->
    not_found;
only0([{_, Module, Function, Arity}|Rest], Args) ->
    try
        case apply(Module, Function, Args) of
            next ->
                only0(Rest, Args);
            Value ->
                Value
        end
    catch
      _Class:_Reason ->
          %% apply に失敗
          error({invalid_apply, {Module, Function, Arity}})
    end.

-spec every(atom(), value(), args()) -> any().
every(HookName, Value, Args) ->
    case ets:lookup(?TABLE, HookName) of
        [] ->
            not_found;
        [{HookName, ListOfHook}] ->
            F = fun({_, Module, Function, Arity}, Acc) ->
                    try
                         apply(Module, Function, [Acc|Args])       
                    catch
                      _Class:_Reason ->
                          %% apply に失敗、これだけ Value が付いてくる
                          error({invalid_apply, {Module, Function, Arity}})
                    end
                end,
            lists:foldl(F, Value, ListOfHook)
    end.
