-module(tag_riak_serv).
-behaviour(gen_server).
-compile(export_all).
% -export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%          code_change/3, terminate/2]).


%Where Pid is the pid of the requesting process (hopefully)
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("greedo.skip.chalmers.se", 8087),
  {ok, Pid}.

%DO Call/Cast Handling here

handle_call(update_taglist, _From, SocketPid) ->
	Taglist = jiffy:encode([<<"theifs">>,<<"thekilling">>,<<"them">>,
     <<"themasterstoryteller">>,<<"themselves">>,
     <<"thenextgreatbaker">>,<<"theprofit">>,<<"therapydisco">>,
     <<"theseStreets">>,<<"theshatteredmind">>,
     <<"theshinsekai">>,<<"thesimplethingstour">>,<<"thesis">>,
     <<"thestrain">>,<<"thesuitelife">>,<<"thewaylifegoes">>,
     <<"thework">>,<<"theythinktheyre21">>,<<"theywack">>,
     <<"thingsToDoBeforeIDie">>,<<"thingstodobeforeidie">>,
     <<"thisisasadmoment">>,<<"thisisntover">>]),
	% {ok, Taglist} = riakc_pb_socket:list_buckets(Pid),
	{reply, Taglist, SocketPid};

handle_call({gettag, Tag}, _From, SocketPid) ->
	{ok, List} = riakc_pb_socket:list_keys(SocketPid, Tag),
	NumTag = length(List),
	Result = jiffy:encode({[{Tag, NumTag}]}),
	% {ok, Taglist} = riakc_pb_socket:list_buckets(Pid),
	{reply, Result, SocketPid}.

handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%Call - Start a server under tag_riak_sup supervision