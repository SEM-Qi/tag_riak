-module(tag_riak_refserv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


start_link() ->
    gen_server:start_link({local, tag_riak_refserv}, ?MODULE, [], []).


init([]) ->
    {ok, Hostname} = inet:gethostname(),
    {ok,{hostent,FullHostname,[_],inet,_,[_]}} = inet:gethostbyname(Hostname),
    {ok, RiakPID} = riakc_pb_socket:start_link(FullHostname, 8087),
	{ok,{FullHostname, RiakPID}}.

handle_call({connect, no_player}, _From, {Hostname, RiakPID}) ->
    {ok, Pid} = supervisor:start_child(tag_riak_sup, [{Hostname, no_key}]),
    {reply, Pid, {Hostname, RiakPID}};

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
 
handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

start_server(Player, Hostname, RiakPID) -> 
    Key = iolist_to_binary([Player, Hostname]),
    {ok, NewPid} = supervisor:start_child(tag_riak_sup, [{Hostname, Key}]),
    Object = riakc_obj:new(<<"session">>, Key, term_to_binary(NewPid)),
    riakc_pb_socket:put(RiakPID, Object),
    NewPid.