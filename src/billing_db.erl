
-module(billing_db).
-author("Serge Ziryukin <ftrvxmtrx@gmail.com>").
-behaviour(gen_server).

-export([get_transactions/1,
         add_account/2,
         delete_account/1,
         reserve_amount/2,
         charge_amount/2,
         confirm_transaction/1,
         cancel_transaction/1,
         refill_amount/2,
         get_amounts/1]).

-export([start/0, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("billing_records.hrl").
-record(state, {}).

-define(NUM_ACCOUNTS, 1000000).
-define(NUM_TRANSACTIONS_FOR_ACCOUNT, 30).
-define(NUM_ACCOUNTS_TO_ADD_TRANSACTIONS, 1000).

%%----------------
%% public
%%----------------

get_transactions(AccountNumber) ->
    gen_server:call(?MODULE, {'get_transactions', AccountNumber}).

add_account(AccountNumber, Amount) ->
    gen_server:call(?MODULE, {'add_account', AccountNumber, Amount}).

delete_account(AccountNumber) ->
    gen_server:call(?MODULE, {'delete_account', AccountNumber}).

reserve_amount(AccountNumber, Amount) ->
    gen_server:call(?MODULE, {'reserve_amount', AccountNumber, Amount}).

charge_amount(AccountNumber, Amount) ->
    gen_server:call(?MODULE, {'charge_amount', AccountNumber, Amount}).

confirm_transaction(TransactionID) ->
    gen_server:call(?MODULE, {'confirm_transaction', TransactionID}).

cancel_transaction(TransactionID) ->
    gen_server:call(?MODULE, {'cancel_transaction', TransactionID}).

refill_amount(AccountNumber, Amount) ->
    gen_server:call(?MODULE, {'refill_amount', AccountNumber, Amount}).

get_amounts(AccountNumber) ->
    gen_server:call(?MODULE, {'get_amounts', AccountNumber}).

add_test_accounts(NumAccounts) ->
    WriteRandom = fun(N) ->
                          mnesia:dirty_write(#account{number = N, amount =  random:uniform(1000)})
                  end,
    io:format("adding test records..."),
    lists:foreach(WriteRandom, lists:seq(0, NumAccounts)),
    io:format("done~n").

add_test_transactions(NumAccounts, NumTransactionsForAccount) ->
    %% random transactions for an account
    GenerateTrans = fun(N) ->
                            R = random:uniform(100),
                            F = case random:uniform(3) of
                                    1 -> charge_op(charged, R, dirty);
                                    2 -> charge_op(reserved, R, dirty);
                                    _ -> fun(A) -> add_transaction(A, R, refilled, dirty) end
                                end,
                            for_account(N, F, dirty)
                    end,
    %% random accounts
    RandomTransactions = fun(N) -> lists:foreach(fun(_) -> GenerateTrans(N) end,
                                                 lists:seq(0, random:uniform(NumTransactionsForAccount)))
                        end,
    io:format("adding test transactions..."),
    lists:foreach(RandomTransactions, lists:seq(0, NumAccounts)),
    io:format("done~n").

%% this one is for debugging
add_test_records(NumAccounts, NumTransactionsForAccount, NumAccountsToAddTransactions) ->
    if NumAccounts > 0 ->
            add_test_accounts(NumAccounts),
            if NumTransactionsForAccount > 0 ->
                    add_test_transactions(NumAccountsToAddTransactions, NumTransactionsForAccount);
               true -> ok
            end;
       true -> ok
    end.

%%----------------
%% private
%%----------------

%% mnesia transaction with atomic->ok and aborted->fail mapping
safe_transaction(Tr) ->
    case mnesia:transaction(Tr) of
        {atomic,  Result} -> {ok,   Result};
        {aborted, Reason} -> {fail, Reason}
    end.

%% convert binary to hex
binary_to_hex(B) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(B)]).

%% generate GUID
guid() ->
    binary_to_hex(crypto:rand_bytes(16)).

%% add a transaction
add_transaction(A, Amount, Type, Dirty) ->
    Write = case Dirty of
                safe  -> write;
                dirty -> dirty_write
            end,
    mnesia:Write(A#account{amount = A#account.amount + Amount}),
    Guid = guid(),
    mnesia:Write(#transaction{guid = Guid,
                              account_number = A#account.number,
                              amount = Amount,
                              type = Type}),
    Guid.

add_transaction(A, Amount, Type) ->
    add_transaction(A, Amount, Type, safe).

%% call a function for specific table record (selected by key) or abort with a specific reason
for_row(Table, Id, Fun, Not_found) ->
    Tr = fun() -> case mnesia:wread({Table, Id}) of
                      [A] -> Fun(A);
                      _   -> mnesia:abort(Not_found)
                  end
         end,
    safe_transaction(Tr).

%% call a function for specific account
for_account(AccountNumber, Fun) ->
    for_row(account, AccountNumber, Fun, invalid_account).

for_account(AccountNumber, Fun, dirty) ->
    case mnesia:dirty_read({account, AccountNumber}) of
        [A] -> Fun(A)
    end.

%% call a function for specific transaction
for_transaction(TransactionID, Fun) ->
    for_row(transaction, TransactionID, Fun, transaction_not_found).

%%----------------
%% server
%%----------------

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    mnesia:create_schema([node()]),
    mnesia:create_table(account, [{attributes, record_info(fields, account)}]),
    mnesia:create_table(transaction, [{attributes, record_info(fields, transaction)}]),
    add_test_records(?NUM_ACCOUNTS,
                     ?NUM_TRANSACTIONS_FOR_ACCOUNT,
                     ?NUM_ACCOUNTS_TO_ADD_TRANSACTIONS),
    {ok, #state{}}.

%% contruct a closure with specific type of transaction and its amount
charge_op(Type, Amount) ->
    fun(A) -> case A of
                  A when A#account.amount < Amount ->
                      mnesia:abort(insufficient_credit);
                  A ->
                      add_transaction(A, -Amount, Type)
              end
    end.

charge_op(Type, Amount, dirty) ->
    fun(A) -> case A of
                  A when A#account.amount >= Amount ->
                      add_transaction(A, -Amount, Type, dirty);
                  _ -> ok
              end
    end.

handle_call({'get_transactions', AccountNumber}, _From, State) ->
    %% match by account number
    Q = qlc:q([R || R <- mnesia:table(transaction), R#transaction.account_number == AccountNumber]),
    Result = safe_transaction(fun() -> qlc:e(Q) end),
    {reply, Result, State};

handle_call({'add_account', AccountNumber, Amount}, _From, State) ->
    Tr = fun() -> case mnesia:wread({account, AccountNumber}) of
                      [_] -> mnesia:abort(exists_already);
                      _   -> mnesia:write(#account{number = AccountNumber, amount = Amount})
                  end
         end,
    Result = safe_transaction(Tr),
    {reply, Result, State};

handle_call({'delete_account', AccountNumber}, _From, State) ->
    Q = qlc:q([R || R <- mnesia:table(transaction),
                    R#transaction.account_number == AccountNumber]),
    Result = safe_transaction(fun() ->
                                      %% remove transactions for this account
                                      lists:foreach(fun(E) -> mnesia:delete_object(E) end,
                                                    qlc:e(Q)),
                                      %% remove account itself
                                      for_account(AccountNumber,
                                                  fun(A) -> mnesia:delete_object(A) end),
                                      ok
                              end),
    {reply, Result, State};

handle_call({'reserve_amount', AccountNumber, Amount}, _From, State) ->
    Result = for_account(AccountNumber, charge_op(reserved, Amount)),
    {reply, Result, State};

handle_call({'charge_amount', AccountNumber, Amount}, _From, State) ->
    Result = for_account(AccountNumber, charge_op(charged, Amount)),
    {reply, Result, State};

handle_call({'confirm_transaction', TransactionID}, _From, State) ->
    Result = for_transaction(TransactionID, fun(T) ->
                                                    case T#transaction.type of
                                                        reserved ->
                                                            mnesia:write(T#transaction{type = charged});
                                                        _ ->
                                                            mnesia:abort(wrong_transaction_status)
                                                    end
                                            end),
    {reply, Result, State};

handle_call({'cancel_transaction', TransactionID}, _From, State) ->
    Cancel = fun(T) ->
                     fun(A) ->
                             mnesia:write(A#account{amount = A#account.amount - T#transaction.amount}),
                             mnesia:delete_object(T)
                     end
             end,
    Result = for_transaction(TransactionID, fun(T) ->
                                                    case T#transaction.type of
                                                        reserved ->
                                                            for_account(T#transaction.account_number,
                                                                        Cancel(T));
                                                        _ ->
                                                            mnesia:abort(wrong_transaction_status)
                                                    end,
                                                    ok
                                            end),
    {reply, Result, State};

handle_call({'refill_amount', AccountNumber, Amount}, _From, State) ->
    Result = for_account(AccountNumber, fun(A) -> add_transaction(A, Amount, refilled) end),
    {reply, Result, State};

handle_call({'get_amounts', AccountNumber}, _From, State) ->
    Q = qlc:q([R#transaction.amount || R <- mnesia:table(transaction),
                                       R#transaction.type == reserved,
                                       R#transaction.account_number == AccountNumber]),
    Tr = fun() ->
                 Result = for_account(AccountNumber, fun(A) -> A#account.amount end),
                 case Result of
                     {ok, Available} -> #amounts{amount_available = Available,
                                                 amount_reserved = -lists:sum(qlc:e(Q))};
                     _ -> mnesia:abort(invalid_account)
                 end
         end,
    Result = safe_transaction(Tr),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
