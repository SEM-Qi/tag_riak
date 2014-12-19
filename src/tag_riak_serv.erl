%% ------------------------------------------------------------------
%% tag_riak_serv is a dynamically generated OTP gen_server that handles all 
%% riak requests and returns or sets data as applicable. 
%% To keep memory leaks to a minumum and maximise fault tolerance, each server handles 
%% its own riak connection and only handles requests from one client
%% ------------------------------------------------------------------


-module(tag_riak_serv).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% Starts a link to local riak node, stores it in state.

init({Hostname, Key}) ->
  Self = self(),
  spawn_link(fun() -> timer:sleep(3600000), supervisor:terminate_child(tag_riak_sup, Self) end),
  {ok, Pid} = riakc_pb_socket:start_link(Hostname, 8087),
  {ok, {Pid, Key}}.

%% Handles requests for  the available taglist

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

%% Handles requests for tag distribution data (most frequent call)

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

%% Handles Authorization queries, returns true or false depending on key match

handle_call({authorize, Data}, _From, {SocketPid, Rest}) ->
  
  %% Query consists of a string which requires to be parsed
  {found, AuthKey} = extract(<<"auth_key">>, Data),
  {found, UserId} = extract(<<"user_id">>, Data),

  %% Gets current information from DB about the user 
  Result = riakc_pb_socket:get(SocketPid, <<"users">>, UserId),
  {ok, Object} = Result,
  RetrievedValues = riakc_obj:get_value(Object),

  %% Extracts key from the users information from DB
  {_, Key} = lists:keyfind(auth_key, 1, binary_to_term(RetrievedValues)),
  NewAuthKey = binary_to_list(AuthKey),

  %% Compares if a key that was sent in query and the one in DB match
  if NewAuthKey =:= Key
    ->
    {reply, "true", {SocketPid, Rest}};
    true ->
      {reply, "false", {SocketPid, Rest}}
  end;

%% Handles user information queries

handle_call({getuserinfo, RawData}, _From, {SocketPid, Rest}) ->

  %% Query consists of a string which requires to be parsed
  {Data} = jiffy:decode(RawData),
  {found, RawUserId} = extract(<<"user_id">>, Data),
  UserId = list_to_integer(binary_to_list(RawUserId)),

  %% Checks if UserID exists in the sent query
  if UserId =:= not_found
    ->
    {reply, bad_request, {SocketPid, Rest}};
    true    ->

      %% If it exists, checks if UserID already exists in DB
      Result = riakc_pb_socket:get(SocketPid, <<"users">>, integer_to_binary(UserId)),
      if Result =:= {error, notfound}
        ->
        {reply, bad_request, {SocketPid, Rest}};
        true ->

          %% If everything went fine, Retrieves values accordingly belonged to userID
          {ok, Object} = Result,
          RetrievedValues = binary_to_term(riakc_obj:get_value(Object)),
          {reply, jiffy:encode({RetrievedValues}), {SocketPid, Rest}}
      end
  end;

%% Updates the auth key data. 


handle_call({updatekey, Data}, _From, {SocketPid, Rest}) ->

  %% Query consists of a string which requires to be parsed
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

  %% Create a new user object according to the userID sent in query by putting the new key in it and old other information
  NewUser = [{auth_key, NewAuthKey}, {profile_image_url, ProfileImg},
  NewUser = [{auth_key, NewAuthKey}, {profile_image_url, ProfileImg},
    {screen_name, ScreenName}, {last_tags, LastTags}, {most_used, MostTags}],
  RiakObj = riakc_obj:new(<<"users">>, UserIdInGame, term_to_binary(NewUser)),
  riakc_pb_socket:put(SocketPid, RiakObj),
  {reply, NewAuthKey, {SocketPid, Rest}};

%% Sets the Auth key The key is generated on twitter sign in.

handle_call({setkey, Data}, _From, {SocketPid, Rest}) ->

  %% Query consists of a string which requires to be parsed
  {found, AuthKey} = extract(<<"key">>, Data),
  {found, ProfileImg} = extract(<<"profile_image_url">>, Data),
  {found, ScreenName} = extract(<<"screen_name">>, Data),
  {found, UserId} = extract(<<"user_id">>, Data),

  %% If UserID is not found, send bad_request
  if UserId =:= not_found
    ->
    {reply, bad_request, {SocketPid, Rest}};

    %% If it was found, retrieve all information from it
    true    ->
      OldUserData = riakc_pb_socket:get(SocketPid, <<"users">>, UserId),
      {LastTags, MostUsed} = case OldUserData of

      %% Check if User has already played a game
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

      %% Create a new user object by putting all the information needed from above
      NewUser = [{auth_key, binary_to_list(AuthKey)}, {profile_image_url, binary_to_list(ProfileImg)},
        {screen_name, binary_to_list(ScreenName)}, {last_tags, LastTags}, {most_used, MostUsed}],
      RiakObj = riakc_obj:new(<<"users">>, UserId, term_to_binary(NewUser)),
      riakc_pb_socket:put(SocketPid, RiakObj),

      %% Send back an auth key
      {reply, binary_to_list(AuthKey), {SocketPid, Rest}}
  end.

%% Required callbacks

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