#########################
Kappa
#########################

Kappa is "Erlang Hook Application".

::

  -module(akutagawa).

  -export([imogayu/2,
           kumo/2]).

  -spec imogayu(integer(), integer(), integer()) -> {next, integer()}.
  imogayu(Value, X, Y) ->
    {next, Value + A + 20}.

  -spec kumo(integer(), integer(), integer()) -> {stop, integer()}.
  kumo(Value, _X, _Y) ->
    {stop, Value}.

  main() ->
    ok = kappa:start(),

    ok = kappa:add(haguruma, 10, ?MODULE, imogayu, 2),
    ok = kappa:add(haguruma, 20, ?MODULE, kumo, 2),

    case kappa:call(haguruma, 0, [10, 20]) of
      {ok, Value} ->
        {ok, Value + 20}
      {error, Reason} ->
        {error, Reason}
    end. 
