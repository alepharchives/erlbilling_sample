-module(billing_app).
-author("Serge Ziryukin <ftrvxmtrx@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for billing.
start(_Type, _StartArgs) ->
    billing_deps:ensure(),
    billing_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for billing.
stop(_State) ->
    ok.
