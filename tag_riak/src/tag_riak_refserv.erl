-module(tag_riak_refserv).
-behaviour(gen_server).
-compile(export_all).
% -export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%          code_change/3, terminate/2]).


start_link() ->
    gen_server:start_link({local, tag_riak_refserv}, ?MODULE, [], []).


init(State) ->
	%initiate some kind of storage for connections and 
	%either make it accessible to top super or send it (maybe start tag_riak_super from here and monitor?)
	{ok,State}.

connect() ->
	{ok, Pid} = supervisor:start_child(tag_riak_sup, []),
	Pid.

update_taglist() ->
	ok.

handle_call(reply, _From, State) ->
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

%Make sure the process spawned is ref'd with a monitor and stored. do same with tag riak super??

% handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
%     case gb_sets:is_element(Ref, Refs) of
%         true ->
%             handle_down_worker(Ref, S);
%         false -> %% Not our responsibility
%             {noreply, S}
%     end;