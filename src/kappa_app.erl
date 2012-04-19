-module(kappa_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = kappa:start(),
    kappa_sup:start_link().

stop(_State) ->
    ok.
