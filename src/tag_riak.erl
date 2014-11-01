-module(tag_riak).
-behaviour(application).
-compile(export_all).
% -export([start/2, stop/1, start_pool/3,
%          run/2, sync_queue/2, async_queue/2, stop_pool/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(normal, _Args) ->
    tag_riak_top:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Interface callbacks
%% ===================================================================

connect() ->
	tag_riak_refserv:connect().

tag_attack(Pid, Tag) -> 
	gen_server:call(Pid, {gettag, Tag}).

update_taglist(Pid) -> 
	gen_server:call(Pid, update_taglist).

getreply() ->
	gen_server:call(tag_riak_refserv, reply).

% get_player_info(Player) -> 
% 	gen_server:call(,Tag)

% new_player(Player_info_object) -> 
% 	gen_server:call(,Tag)

% update_player_info(Player_info_object) -> 
% 	gen_server:call(,Tag)

close_server(Pid) ->
	supervisor:terminate_child(tag_riak_sup, Pid),
	ok.

