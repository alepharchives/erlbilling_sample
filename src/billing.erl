-module(billing).
-author("Serge Ziryukin <ftrvxmtrx@gmail.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the billing server.
start() ->
    billing_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    application:start(billing).

%% @spec stop() -> ok
%% @doc Stop the billing server.
stop() ->
    application:stop(mnesia),
    application:stop(billing).
