-module(tag_riak_serv).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%Where Pid is the pid of the requesting process (hopefully)
start_link([Hostname, Key]) ->
  gen_server:start_link(?MODULE, [Hostname, Key], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% Starts a link to riak, stores it in state.
init([Hostname, Key]) ->
  {ok, Pid} = riakc_pb_socket:start_link(Hostname, 8087),
  {ok, {Pid, Key}}.

%% Here is where you can add functionaility by making another handle_call function head.
%% Remember to include the API call in tag_riak for any functionaility you want to access

handle_call(update_taglist, _From, {SocketPid, Rest}) ->
	Taglist = case riakc_pb_socket:get(SocketPid, <<"taglistbucket">>, <<"taglist">>) of
        {ok, CurrentTaglist} -> 
          FinalTaglist = binary_to_term(riakc_obj:get_value(CurrentTaglist)),  
          Taglist1 = jiffy:encode({[{<<"tags">>, FinalTaglist}]}),
          Taglist1;
        {error,_} ->
          Taglist1 = jiffy:encode({[{<<"tags">>, <<"Bad List">>}]}),
          Taglist1
  end,
	{reply, Taglist, {SocketPid, Rest}};

handle_call({gettag, Tag}, _From, {SocketPid, Rest}) ->
  Response = case riakc_pb_socket:get(SocketPid, <<"formattedtweets">>, Tag) of
        {ok, TagAttack} -> 
          FinalTagAttack = binary_to_term(riakc_obj:get_value(TagAttack)),  
          FinalTagAttack;
        {error,_} ->
          {Distribution, Cotags} = {[{[{<<"numtags">>, 0}, {<<"tweets">>, ""}]}],[]},
          Response1 = jiffy:encode({[{<<"tag">>, Tag}, 
            {<<"cotags">>, Cotags},
            {<<"distribution">>, Distribution}]}),
          Response1
  end,
{reply, Response, {SocketPid, Rest}};

handle_call({authorize, Data}, _From, {SocketPid, Rest}) ->
  {found, AuthKey} = extract(<<"auth_key">>, Data),
  {found, UserId} = extract(<<"user_id">>, Data),
  Result = riakc_pb_socket:get(SocketPid, <<"users">>, UserId),
  {ok, Object} = Result,
  RetrievedValues = riakc_obj:get_value(Object),
  {_, Key} = lists:keyfind(auth_key, 1, binary_to_term(RetrievedValues)),
  if AuthKey =:= Key
    ->
    {reply, "true", {SocketPid, Rest}};
    true ->
      {reply, "false", {SocketPid, Rest}}
  end;

handle_call({getuserinfo, RawData}, _From, {SocketPid, Rest}) ->
  {Data} = jiffy:decode(RawData),
  {found, RawUserId} = extract(<<"user_id">>, Data),
  UserId = list_to_integer(binary_to_list(RawUserId)),
  if UserId =:= not_found
    ->
    {reply, bad_request, {SocketPid, Rest}};
    true    ->
      Result = riakc_pb_socket:get(SocketPid, <<"users">>, integer_to_binary(UserId)),
      if Result =:= {error, notfound}
        ->
        {reply, bad_request, {SocketPid, Rest}};
        true ->
          {ok, Object} = Result,
          RetrievedValues = binary_to_term(riakc_obj:get_value(Object)),
          %%  io:format("~p ~n", [RetrievedValues]),
          {reply, jiffy:encode({RetrievedValues}), {SocketPid, Rest}}
      end
  end;

%% Theres no point in checking if UserID exists because this step is happening after the log in
handle_call({updatekey, Data}, _From, {SocketPid, Rest}) ->
  {found, NewAuthKey} = extract(<<"auth_key">>, Data),
  {found, UserIdInGame} = extract(<<"user_id">>, Data),
  %% Retrieve values from DB
  Result = riakc_pb_socket:get(SocketPid, <<"users">>, UserIdInGame),
  {ok, Object} = Result,
  RetrievedValues = binary_to_term(riakc_obj:get_value(Object)),
  %% Extract everything that is in there and then place new
  {found, ProfileImg} = extract(profile_image_url, RetrievedValues),
  {found, ScreenName} = extract(screen_name,       RetrievedValues),
  {found, LastTags}   = extract(last_tags,         RetrievedValues),
  {found, MostTags}   = extract(most_used,         RetrievedValues),
  NewUser = [{auth_key, NewAuthKey}, {profile_image_url, ProfileImg},
    {screen_name, ScreenName}, {last_tags, LastTags}, {most_used, MostTags}],
  RiakObj = riakc_obj:new(<<"users">>, UserIdInGame, term_to_binary(NewUser)),
  riakc_pb_socket:put(SocketPid, RiakObj),
  {reply, NewAuthKey, {SocketPid, Rest}};


handle_call({setkey, Data}, _From, {SocketPid, Rest}) ->
%%   io:format("~p ~n", [Data]),
  {found, AuthKey} = extract(<<"key">>, Data),
  {found, ProfileImg} = extract(<<"profile_image_url">>, Data),
  {found, ScreenName} = extract(<<"screen_name">>, Data),
  {found, UserId} = extract(<<"user_id">>, Data),
  if UserId =:= not_found
    ->
    {reply, bad_request, {SocketPid, Rest}};
    true    ->
      OldUserData = riakc_pb_socket:get(SocketPid, <<"users">>, UserId),
      {LastTags, MostUsed} = case OldUserData of
      {error, notfound}
        ->
          {[], []};
        _ ->
          {ok, Object} = OldUserData,
          Value = binary_to_term(riakc_obj:get_value(Object)),
          LastTagsTemp = extract(<<"last_tags">>, Value), %% !!!! extract function
          MostUsedTemp = extract(<<"most_used">>, Value),
          {LastTagsTemp, MostUsedTemp}
      end,
      NewUser = [{auth_key, binary_to_list(AuthKey)}, {profile_image_url, binary_to_list(ProfileImg)},
        {screen_name, binary_to_list(ScreenName)}, {last_tags, LastTags}, {most_used, MostUsed}],
      RiakObj = riakc_obj:new(<<"users">>, UserId, term_to_binary(NewUser)),
      riakc_pb_socket:put(SocketPid, RiakObj),
      {reply, binary_to_list(AuthKey), {SocketPid, Rest}}
  end.


handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, {_, no_key}) ->
    ok;

terminate(_Reason, {SocketPid, Key}) ->
    riakc_pb_socket:delete(SocketPid, <<"session">>, Key),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.