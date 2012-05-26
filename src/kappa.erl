-module(kappa).

-author('twisted.mind@voluntas.net').

-export([start/0, stop/0]).
-export([declare/3]).
-export([add/4, delete/4]).
-export([all/2, only/2, every/3]).  

-define(TABLE, kappa_table).

-include_lib("eunit/include/eunit.hrl").

-type priority() :: non_neg_integer().
-type args() :: [any()].
-type value() :: any().

-type name() :: atom().
-type type() :: all | only | every.

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

-spec declare(atom(), type(), arity()) -> ok.
declare(Name, Type, Arity) ->
    case ets:lookup(?TABLE, Name) of
        [] ->
            true = ets:insert(?TABLE, {Name, Type, Arity, []}),
            ok;
        _ ->
            error({duplicate_name, Name})
    end.

-spec add(name(), type(), priority(), mfa()) -> ok | no_return().
add(Name, Type, Priority, {Module, Function, Arity}) ->
    try
        %% まずは存在するかどうかチェック
        ListOfTuple = apply(Module, module_info, [exports]),
        case lists:member({Function, Arity}, ListOfTuple) of
            true ->
                Hook = {Priority, {Module, Function, Arity}},
                case ets:lookup(?TABLE, Name) of
                    [] ->
                        %% Hook が宣言されていなかったら例外を上げる
                        error({missing_declare, Name});
                    [{Name, Type, Arity, ListOfHook}] ->
                        verify_add(Name, Type, Arity, ListOfHook, Hook);
                    [{_Name, _Type, Arity, _ListOfHook}] ->
                        %% _Type は正しい値が入ってくる
                        error({invalid_type, Name, Type, Hook});
                    [{_Name, Type, _Arity, _ListOfHook}] ->
                        %% _Arity は正しい値が入ってくる
                        error({invalid_arity, Name, Type, Hook})
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

verify_add(Name, Type, Arity, ListOfHook, {Priority, MFA} = Hook) ->
    %% 同じ優先度は存在してはいけない
    case lists:keymember(Priority, 1, ListOfHook) of
        false ->
            %% 同じ M:F/A があってはいけない (Priority は無視する)
            case lists:keymember(MFA, 2, ListOfHook) of
                false ->
                    NewListOfHook = lists:merge(ListOfHook, [Hook]),
                    true = ets:insert(?TABLE, {Name, Type, Arity, NewListOfHook}),
                    ok;
                true ->
                    error({duplicate_mfa, Name, Type, Hook})
            end;
        true ->
            error({duplicate_priority, Name, Type, Hook})
    end.

-spec delete(name(), type(), priority(), mfa()) -> ok.
delete(Name, Type, Priority, {Module, Function, Arity}) ->
    case ets:lookup(?TABLE, Name) of
        [] ->
            error({missing_declare, Name});
        [{Name, Type, Arity, ListOfHook}] ->
            Hook = {Priority, {Module, Function, Arity}},
            case lists:member(Hook, ListOfHook) of
                true ->
                    case lists:delete(Hook, ListOfHook) of
                        [] ->
                            true = ets:delete(?TABLE, Name),
                            ok;  
                        NewListOfHook ->
                            true = ets:insert(?TABLE, {Name, Type, Arity, NewListOfHook}),
                            ok
                    end;
                false ->
                    error({missing_hook, Hook})
            end
    end.

-spec all(atom(), args()) -> [any()].
all(Name, Args) ->
    case ets:lookup(?TABLE, Name) of
        [] ->
            error({missing_declare, Name});
        [{Name, all, _Arity, ListOfHook}] ->
            F = fun({_, {Module, Function, Arity}}) ->
                    try
                        apply(Module, Function, Args)
                    catch
                      _Class:Reason ->
                          %% apply に失敗
                          error({invalid_apply, {Module, Function, Arity}, Reason})
                    end
                end,
            lists:map(F, ListOfHook)
    end.

-spec only(atom(), args()) -> not_found | any().
only(Name, Args) ->
    case ets:lookup(?TABLE, Name) of
        [] ->
            error({missing_declare, Name});
        [{Name, only, _Arity, ListOfHook}] ->
            only0(ListOfHook, Args)
    end.

only0([], _Args) ->
    not_found;
only0([{_Priority, {Module, Function, Arity}}|Rest], Args) ->
    try
        case apply(Module, Function, Args) of
            next ->
                only0(Rest, Args);
            Value ->
                Value
        end
    catch
      _Class:Reason ->
          %% apply に失敗
          error({invalid_apply, {Module, Function, Arity}, Reason})
    end.

-spec every(atom(), value(), args()) -> any().
every(Name, Value, Args) ->
    case ets:lookup(?TABLE, Name) of
        [] ->
            error({missing_declare, Name});
        [{_Name, every, _Arity, ListOfHook}] ->
            every0(Value, Args, ListOfHook)
    end.

every0(Value, Args, ListOfHook) ->
    F = fun({_Priority, {Module, Function, _Arity} = MFA}, Acc) ->
            try
                 apply(Module, Function, [Acc|Args])       
            catch
              _Class:Reason ->
                  %% apply に失敗
                  error({invalid_apply, MFA, Reason})
            end
        end,
    lists:foldl(F, Value, ListOfHook).
