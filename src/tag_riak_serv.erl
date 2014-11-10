-module(tag_riak_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2]).


%Where Pid is the pid of the requesting process (hopefully)
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("greedo.skip.chalmers.se", 8087),
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
  case riakc_pb_socket:get_index_range(
            SocketPid,
            <<"tags">>, %% bucket name
            {integer_index, "timestamp"}, %% index name
            oldTimeStamp(), timeStamp() %% origin timestamp should eventually have some logic attached
          ) of
    {ok, {_,Keys,_,_}} ->
      AllKeys = lists:reverse(lists:sort(Keys)),
      if
        length(AllKeys) >= 20 ->
          {NewKeys,_} = lists:split(20, AllKeys),
          Objects = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, <<"tags">>, Key), Obj end, NewKeys),
          Tagset = lists:map(fun(Object) -> Value = binary_to_term(riakc_obj:get_value(Object)), case dict:find(Tag, Value) of {ok, Tagged} -> Tagged; error -> {0, sets:new(),sets:new()} end end, Objects),
          {Distribution, Cotags} = loopThrough(Tagset, [], sets:new());
        (length(AllKeys) >= 2) and (length(AllKeys) rem 2 =:= 0) ->
          Objects = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, <<"tags">>, Key), Obj end, AllKeys),
          Tagset = lists:map(fun(Object) -> Value = binary_to_term(riakc_obj:get_value(Object)), case dict:find(Tag, Value) of {ok, Tagged} -> Tagged; error -> {0, sets:new(),sets:new()} end end, Objects),
          {Distribution, Cotags} = loopThrough(Tagset, [], sets:new());
        length(AllKeys) >= 2 ->
          [_|NewKeys] = AllKeys,
          Objects = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, <<"tags">>, Key), Obj end, NewKeys),
          Tagset = lists:map(fun(Object) -> Value = binary_to_term(riakc_obj:get_value(Object)), case dict:find(Tag, Value) of {ok, Tagged} -> Tagged; error -> {0, sets:new(),sets:new()} end end, Objects),
          {Distribution, Cotags} = loopThrough(Tagset, [], sets:new());
        true ->
          {Distribution, Cotags} = {[{[{<<"numtags">>, 0}, {<<"tweets">>, ""}]}],[]}
      end;
    {error, _} ->
      {Distribution, Cotags} = {[{[{<<"numtags">>, 0}, {<<"tweets">>, ""}]}],[]}
  end,
  Response = jiffy:encode({[{<<"tag">>, Tag},
  {<<"cotags">>, Cotags},
  {<<"distribution">>, 
    Distribution}]}),

{reply, Response, SocketPid}.

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