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
  {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
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

%handle_call({setkey, Data}, _From, SocketPid) ->
%	DataMap = jiffy:decode(Data, [return_maps]), 
	%%ProfileImage
%	UserId = maps:get(<<"user_id">>, DataMap, not_found),
%	AuthKey = maps:get(<<"key">>, DataMap, not_found),
%	if AuthKey == not_found, UserId == not_found
%		-> 
%		{reply, bad_request, SocketPid};
%	true 		->
%		Result = riakc_pb_socket:get(SocketPid, <<"users">>, term_to_binary(UserId)),
%		if Result =:= {error, notfound} 
%				 ->     UserMap = #{}; %% put profile image
						%% in ^ "profileimage" => ProfileImage^^
%			true -> 
%				{ok, Object} = Result,
%				UserMap = binary_to_term(riakc_obj:get_value(Object))
%		end,
%		NewUserMap = maps:put("authkey", binary_to_list(AuthKey), UserMap),
%		RiakObj = riakc_obj:new(<<"users">>, term_to_binary(UserId), NewUserMap),
%		riakc_pb_socket:put(SocketPid, RiakObj),
%		{reply, binary_to_list(AuthKey), SocketPid}
%	end;
	%application:ensure_all_started(tw_data_server).

handle_call({setkey, Data}, _From, SocketPid) ->
	Data1 = jiffy:decode(Data), 
	AuthKey = extract(<<"key">>, Data1),
	ProfileImg = extract(<<"profile_image_url">>, Data1), 
	ScreenName = extract(<<"screen_name">>, Data1),
	UserId = extract(<<"user_id">>, Data1),
  if UserId =:= not_found
    ->
		{reply, bad_request, SocketPid};
	 true 		->
		OldUserData = riakc_pb_socket:get(SocketPid, <<"users">>, term_to_binary(UserId)),
		if OldUserData =:= {error, notfound}
				-> 
					LastTags = [],
					MostUsed = [];
		   true ->
					{ok, Object} = OldUserData,
					Value = riakc_obj:get_value(Object),
					LastTags = lists:keyfind(last_tags, 1, Value),
					MostUsed = lists:keyfind(most_used, 1, Value)
		end,
				
    NewUser = [{auth_key, AuthKey}, {profile_image_url, ProfileImg}, {screen_name, ScreenName}, {last_tags, LastTags}, {most_used, MostUsed}], %[{auth_key, "r12312312312312313"}, {profile_img, "http:aadasd/dsad"}, {screen_name, "durak"}]
		RiakObj = riakc_obj:new(<<"users">>, term_to_binary(UserId), term_to_binary(NewUser)),
		riakc_pb_socket:put(SocketPid, RiakObj),
		{reply, binary_to_list(AuthKey), SocketPid}
	end;
	
	
handle_call({getuserinfo, RawData}, _From, SocketPid) ->
	Data = jiffy:decode(RawData),
	UserId = extract(<<"user_id">>, Data),
  if UserId =:= not_found
    ->
			{reply, bad_request, SocketPid};
		true 		->
			Result = riakc_pb_socket:get(SocketPid, <<"users">>, term_to_binary(UserId)),
      {reply, jiffy:encode(binary_to_list(Result)), SocketPid}
    end;
			%application:ensure_all_started(tw_data_server).

	
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
  {Distribution, Cotags} = case riakc_pb_socket:get_index_range(
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
          {Distribution1, Cotags1} = loopThrough(Tagset, [], sets:new()),
          {Distribution1, Cotags1};
        (length(AllKeys) >= 2) and (length(AllKeys) rem 2 =:= 0) ->
          Objects = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, <<"tags">>, Key), Obj end, AllKeys),
          Tagset = lists:map(fun(Object) -> Value = binary_to_term(riakc_obj:get_value(Object)), case dict:find(Tag, Value) of {ok, Tagged} -> Tagged; error -> {0, sets:new(),sets:new()} end end, Objects),
          {Distribution1, Cotags1} = loopThrough(Tagset, [], sets:new()),
          {Distribution1, Cotags1};
        length(AllKeys) >= 2 ->
          [_|NewKeys] = AllKeys,
          Objects = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, <<"tags">>, Key), Obj end, NewKeys),
          Tagset = lists:map(fun(Object) -> Value = binary_to_term(riakc_obj:get_value(Object)), case dict:find(Tag, Value) of {ok, Tagged} -> Tagged; error -> {0, sets:new(),sets:new()} end end, Objects),
          {Distribution1, Cotags1} = loopThrough(Tagset, [], sets:new()),
          {Distribution1, Cotags1};
        true ->
          {[{[{<<"numtags">>, 0}, {<<"tweets">>, ""}]}],[]}
      end;
    {error, _} ->
      {[{[{<<"numtags">>, 0}, {<<"tweets">>, ""}]}],[]}
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

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

extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.
