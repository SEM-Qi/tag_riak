-module(tag_riak_refserv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, connect/0]).


start_link() ->
    gen_server:start_link({local, tag_riak_refserv}, ?MODULE, [], []).


init(State) ->
	{ok,State}.

connect() ->
	{ok, Pid} = supervisor:start_child(tag_riak_sup, []),
	Pid.

handle_call(_Msg, _From, State) ->
	io:format("ref server started"),
	Reply = "blam",
    {reply, Reply, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.