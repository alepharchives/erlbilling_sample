-module(billing_json).
-author("Serge Ziryukin <ftrvxmtrx@gmail.com>").
-export([confirm/1, cancel/1, amounts/1, search/1, create/1, delete/1]).

-include_lib("billing_records.hrl").

message_of(R) ->
    {_, Result} = R,
    {struct, [{message, Result}]}.

%% @spec transaction_to_table_row(Transaction) -> string()
%% @doc Convert transaction to html table row.
transaction_to_table_row(T) ->
    #transaction{guid   = Guid,
                 amount = Amount,
                 type   = Type} = T,
    Button = case Type of
                 reserved -> "<input type=\"button\""
                                 ++ " onclick=\"confirm_transaction($(this))\""
                                 ++ " value=\"Confirm\"/>"
                                 ++ "<input type=\"button\""
                                 ++ " onclick=\"cancel_transaction($(this))\""
                                 ++ " value=\"Cancel\"/>";
                 _ -> ""
             end,
    lists:concat(["<tr id='" ++ Guid ++ "'>"]
                 ++ lists:foldl(fun(E, Acc) -> Acc ++ ["<td><div>", E, "<div/></td>"] end,
                                [],
                                [Guid, Amount, Type])
                 ++ ["<td width=\"160\"><div>" ++ Button ++ "</div></td>"]
                 ++ ["<tr/>\n"]).

%% @spec confirm(Json) -> {struct, [{message, ok}]} | {struct, [{message, ErrorReason::string()}]}
%% @doc Confirm transaction.
confirm(S) ->
    TransactionID = binary_to_list(proplists:get_value(<<"guid">>, S)),
    message_of(billing_db:confirm_transaction(TransactionID)).

%% @spec cancel(Json) -> {struct, [{message, ok}]} | {struct, [{message, ErrorReason::string()}]}
%% @doc Cancel transaction.
cancel(S) ->
    TransactionID = binary_to_list(proplists:get_value(<<"guid">>, S)),
    message_of(billing_db:cancel_transaction(TransactionID)).

%% @doc Only return amounts for an account.
amounts(S) ->
    AccountNumber = proplists:get_value(<<"account_number">>, S),
    case billing_db:get_amounts(AccountNumber) of
        {ok, Account} ->
            #amounts{amount_available = Available, amount_reserved = Reserved} = Account,
            {struct, [{message, ok},
                      {amount_available, Available},
                      {amount_reserved, Reserved}]};
        Fail ->
            message_of(Fail)
    end.

%% @doc Search for an account.
search(S) ->
    AccountNumber = proplists:get_value(<<"account_number">>, S),
    case billing_db:get_amounts(AccountNumber) of
        {ok, Account} ->
            {ok, Transactions} = billing_db:get_transactions(AccountNumber),
            #amounts{amount_available = Available, amount_reserved = Reserved} = Account,
            Transaction_Struct = fun(T) -> transaction_to_table_row(T) end,
            {struct, [{message, ok},
                      {amount_available, Available},
                      {amount_reserved, Reserved},
                      {transactions,
                       list_to_binary("<table id='transactions'>"
                                      ++ "<tr><td>GUID</td><td>Amount</td><td>Type</td></tr>\n"
                                      ++ lists:concat(lists:map(Transaction_Struct,
                                                                Transactions))
                                      ++ "</table>")
                      }]};
        Fail ->
            message_of(Fail)
    end.

%% @spec create(Json) -> {struct, [{message, ok}]} | {struct, [{message, ErrorReason::string()}]}
%% @doc Create new account.
create(S) ->
    AccountNumber = proplists:get_value(<<"account_number">>, S),
    Amount = proplists:get_value(<<"amount">>, S),
    message_of(billing_db:add_account(AccountNumber, Amount)).

%% @spec delete(Json) -> {struct, [{message, ok}]} | {struct, [{message, ErrorReason::string()}]}
%% @doc Remove account from database.
delete(S) ->
    AccountNumber = proplists:get_value(<<"account_number">>, S),
    message_of(billing_db:delete_account(AccountNumber)).
