-module(tag_riak).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).
%% Interface callbacks
-export([connect/0, tag_attack/2, update_taglist/1, getreply/0, close_server/1]).
% -export([get_player_info/1, new_player/1, update_player_info/1]).

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

test_post(Pid, Val) ->
	gen_server:call(Pid, {testpost, Val}).

% get_player_info(Player) -> 
% 	gen_server:call(,Tag)

% new_player(Player_info_object) -> 
% 	gen_server:call(,Tag)

% update_player_info(Player_info_object) -> 
% 	gen_server:call(,Tag)

close_server(Pid) ->
	supervisor:terminate_child(tag_riak_sup, Pid),
	ok.

