#########################
Kappa
#########################

Kappa is "Erlang Hook Application".

::

    -module(akutagawa).

    -export([imogayu/2, kumo/2]).

    -spec imogayu(integer(), integer(), integer()) -> next.
    imogayu(_X, _Y) ->
        next.

    -spec kumo(integer(), integer()) -> {ok, integer()}.
    kumo(X, Y) ->
        {ok, X + Y}.

    main() ->
        ok = kappa:start(),
        
        ok = kappa:declare(haguruma, only, 2),

        ok = kappa:add(haguruma, only, 10, {?MODULE, imogayu, 2}),
        ok = kappa:add(haguruma, only, 20, {?MODULE, kumo, 2}),

        case kappa:only(haguruma, [10, 20]) of
          {ok, Value} ->
            {ok, Value + 10}
          {error, Reason} ->
            {error, Reason}
        end. 
