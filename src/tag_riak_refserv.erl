

%% ------------------------------------------------------------------
%% tag_riak_refserv is an OTP gen_server that recieves and manages connection requests
%% from tw_data_server and assigns them a dynamically generated gen_server to 
%% interact with the Riak DB
%% ------------------------------------------------------------------

-module(tag_riak_refserv).
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


start_link() ->
    gen_server:start_link({local, tag_riak_refserv}, ?MODULE, [], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% On init the host of the current node is determined in order to get a local riak socket up
%% If there is no riak node locally the tag_riak and tw_data_server node will also 
%% crash and stop responding to the DNS so queries are redirected 
%% appropriately and no down time is experienced. 
%% A riak node failure is considered a whole machine failure and 
%% root level retart procedures and startup scripts should kick in

init([]) ->
    {ok, Hostname} = inet:gethostname(),
    {ok,{hostent,FullHostname,[_],inet,_,[_]}} = inet:gethostbyname(Hostname),
    {ok, RiakPID} = riakc_pb_socket:start_link(FullHostname, 8087),
	{ok,{FullHostname, RiakPID}}.

%% Handles a connection call where the player is undetermined, it is not indexed. 
%% The process will be left to self terminate or terminate on connection close

handle_call({connect, no_player}, _From, {Hostname, RiakPID}) ->
    {ok, Pid} = supervisor:start_child(tag_riak_sup, [{Hostname, no_key}]),
    {reply, Pid, {Hostname, RiakPID}};

%% Handles a connection call where the player is identifiable (applicable in most cases). 
%% The player and pid of the process generated are indexed in riak. 
%% The index is deleted if found to be dead and restarted

handle_call({connect, Player}, _From, {Hostname, RiakPID}) ->
    Key = iolist_to_binary([Player, Hostname]),
    Pid = case riakc_pb_socket:get(RiakPID, <<"session">>, Key) of
        {ok, PlayerServer} -> 
            CheckPid = binary_to_term(riakc_obj:get_value(PlayerServer)),
            case is_process_alive(CheckPid) of
                true ->
                    CheckPid;
                false ->
                    riakc_pb_socket:delete(RiakPID, <<"session">>, Key),
                    start_server(Player, Hostname, RiakPID);
                {badarg,_} -> 
                    riakc_pb_socket:delete(RiakPID, <<"session">>, Key),
                    start_server(Player, Hostname, RiakPID)
            end;
        {error,notfound} ->
            start_server(Player, Hostname, RiakPID)
        end,
    {reply, Pid, {Hostname, RiakPID}}.

%% Required callbacks
 
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

%% Starts server and handles indexing. 
%%The process is fed the host information and its own unique index key

start_server(Player, Hostname, RiakPID) -> 
    Key = iolist_to_binary([Player, Hostname]),
    {ok, NewPid} = supervisor:start_child(tag_riak_sup, [{Hostname, Key}]),
    Object = riakc_obj:new(<<"session">>, Key, term_to_binary(NewPid)),
    riakc_pb_socket:put(RiakPID, Object),
    NewPid.