-module(billing_db_events).
-author("Serge Ziryukin <ftrvxmtrx@gmail.com>").
-behaviour(gen_event).

-export([start/0,
         get_amounts/0]).

-export([init/1,
         handle_call/2,
         handle_info/2,
         handle_event/2,
         terminate/2,
         code_change/3]).

-include_lib("billing_records.hrl").

get_amounts() ->
    gen_event:call({global, ?MODULE}, ?MODULE, 'get_amounts').

start() ->
    {ok, Pid} = gen_event:start_link({global, ?MODULE}),
    gen_event:add_handler(Pid, ?MODULE, []).

init(_Args) ->
    mnesia:subscribe({table, account, detailed}),
    mnesia:subscribe({table, transaction, detailed}),
    { ok, #amounts{} }.

handle_call('get_amounts', State) ->
    {ok, State, State}.

fold_accounts_amount(Old) ->
    lists:foldl(fun(A, Sum) -> Sum + A#account.amount end, 0, Old).

fold_transactions_amount(Old) ->
    lists:foldl(fun(A, Sum) -> Sum + A#transaction.amount end, 0, Old).

%% new or updated account
handle_info({_, {write, account, #account{amount = Amount}, Old, _}}, State) ->
    WithoutOld = State#amounts.amount_available - fold_accounts_amount(Old),
    NewState = State#amounts{amount_available = WithoutOld + Amount},
    {ok, NewState};

%% removed account(s)
handle_info({_, {delete, account, _, Old, _}}, State) ->
    io:format("removed accounts ~p~n", [Old]),
    WithoutOld = State#amounts.amount_available - fold_accounts_amount(Old),
    NewState = State#amounts{amount_available = WithoutOld},
    {ok, NewState};

%% new transaction (reserved)
handle_info({_, {write, transaction, #transaction{amount = Amount, type = reserved}, _, _}}, State) ->
    Reserved = -Amount,
    io:format("new transaction (reserved ~p)~n", [Reserved]),
    NewState = State#amounts{amount_reserved  = State#amounts.amount_reserved + Reserved},
    {ok, NewState};

%% new or confirmed transaction
handle_info({_, {write, transaction, #transaction{type = charged}, Old, _}}, State) ->
    Reserved = -fold_transactions_amount(Old),
    io:format("new/confirmed transaction (charged=~p)~n", [Reserved]),
    NewState = State#amounts{amount_reserved  = State#amounts.amount_reserved - Reserved},
    {ok, NewState};

%% 'refill' transaction
handle_info({_, {write, transaction, #transaction{type = refilled}, _, _}}, State) ->
    %% total amount will be updated via 'account' table, so do nothing here
    {ok, State};

%% cancelled transaction
handle_info({_, {delete, transaction, _, Old, _}}, State) ->
    Cancelled = -fold_transactions_amount(Old),
    io:format("cancelled transaction (amount=~p)~n", [Cancelled]),
    NewState = State#amounts{amount_reserved  = State#amounts.amount_reserved - Cancelled},
    {ok, NewState}.

handle_event(_Event, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
