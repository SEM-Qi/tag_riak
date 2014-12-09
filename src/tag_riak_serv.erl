-module(tag_riak_serv).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%Where Pid is the pid of the requesting process (hopefully)
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% Starts a link to riak, stores it in state.
init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("picard.skip.chalmers.se", 8087),
  {ok, Pid}.

%% Here is where you can add functionaility by making another handle_call function head.
%% Remember to include the API call in tag_riak for any functionaility you want to access

handle_call(update_taglist, _From, SocketPid) ->
	Taglist = case riakc_pb_socket:get(SocketPid, <<"taglistbucket">>, <<"taglist">>) of
        {ok, CurrentTaglist} -> 
          FinalTaglist = binary_to_term(riakc_obj:get_value(CurrentTaglist)),  
          Taglist1 = jiffy:encode({[{<<"tags">>, FinalTaglist}]}),
          Taglist1;
        {error,_} ->
          Taglist1 = jiffy:encode({[{<<"tags">>, <<"Bad List">>}]}),
          Taglist1
  end,
	{reply, Taglist, SocketPid};

handle_call({testpost, TestInfo}, _From, SocketPid) ->
  {TestInfo1} = jiffy:decode(TestInfo),
  Result = case extract(<<"testid">>, TestInfo1) of
    {found, Val} -> 
      Obj = riakc_obj:new(<<"testpost">>,
        Val,
        term_to_binary(TestInfo1)),
      Result1 = riakc_pb_socket:put(SocketPid, Obj),
      Result1;
    not_found -> bad_request
  end, 
  {reply, Result, SocketPid};

handle_call({gettag, Tag}, _From, SocketPid) ->
  Response = case riakc_pb_socket:get(SocketPid, <<"formattedtweets">>, Tag) of
        {ok, TagAttack} -> 
          FinalTagAttack = binary_to_term(riakc_obj:get_value(TagAttack)),  
          FinalTagAttack;
        {error,_} ->
          {Distribution, Cotags} = {[{[{<<"numtags">>, 0}, {<<"tweets">>, ""}]}],[]}
          Response1 = jiffy:encode({[{<<"tag">>, Tag}, 
            {<<"cotags">>, Cotags},
            {<<"distribution">>, Distribution}]}),
          Response1
  end,
{reply, Response, SocketPid}.

handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.