-module(tag_riak_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2]).


%Where Pid is the pid of the requesting process (hopefully)
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("picard.skip.chalmers.se", 8087),
  {ok, Pid}.

handle_call(update_taglist, _From, SocketPid) ->
	case riakc_pb_socket:get(SocketPid, <<"taglistbucket">>, <<"taglist">>) of
        {ok, CurrentTaglist} -> 
          FinalTaglist = binary_to_term(riakc_obj:get_value(CurrentTaglist)),  
          Taglist = jiffy:encode({[{<<"tags">>, FinalTaglist}]});
        {error,_} ->
          Taglist = jiffy:encode({[{<<"tags">>, <<"Bad List">>}]})
  end,
	{reply, Taglist, SocketPid};

handle_call({gettag, Tag}, _From, SocketPid) ->
  Response = case riakc_pb_socket:get(SocketPid, <<"formattedtweets">>, Tag) of
        {ok, TagAttack} -> 
          FinalTagAttack = binary_to_term(riakc_obj:get_value(TagAttack)),  
          FinalTagAttack;
        {error,_} ->
          Response1 = jiffy:encode({[{<<"tag">>, <<"Bad List">>}]}),
          Response1
  end,
	{reply, Response, SocketPid};
	
	
	


%%{reply, Response, SocketPid}.

handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

loopThrough([], L, Cotags) -> {L, sets:to_list(Cotags)};
loopThrough(Tagset, L, OldCotags) ->
  {NewKeys,OldKeys} = lists:split(2, Tagset),
  [{Num, Cotags, Tweets}, {Num2, Cotags2, Tweets2}] = NewKeys,
  L2 = [{[{<<"numtags">>, Num + Num2}, {<<"tweets">>, sets:to_list(sets:union([Tweets, Tweets2]))}]}|L],
  NewCotags = sets:union([Cotags, Cotags2, OldCotags]),
  loopThrough(OldKeys, L2, NewCotags).

timeStamp() ->
  {Mega, Secs, Micro} = erlang:now(),
  Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.

oldTimeStamp() ->
  {Mega, Secs, Micro} = erlang:now(),
  Mega*1000*1000*1000*1000 + ((Secs - 2400) * 1000 * 1000) + Micro.