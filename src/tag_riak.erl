
%% ------------------------------------------------------------------
%% tag_riak is an OTP application that handles all riak based querys for 
%% tw_data_server (or other application or erlang process, it is standalone),
%% it exposes the below APIs when running.
%% ------------------------------------------------------------------


-module(tag_riak).
-behaviour(application).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% Interface Function Exports
%% ------------------------------------------------------------------

-export([connect/0, connect/1, tag_attack/2, update_taglist/1, 
	close_server/1,getuserinfo/2, setkey/2, updatekey/2, authorize/2]).


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

%% Here is where to define externally accessible calls to the internal server. 
%% Any functionality you want accessible must be put here


connect() ->
	gen_server:call(tag_riak_refserv, {connect, no_player}).

connect(Player) ->
	gen_server:call(tag_riak_refserv, {connect, Player}).

tag_attack(Pid, Tag) -> 
	gen_server:call(Pid, {gettag, Tag}).

update_taglist(Pid) -> 
	gen_server:call(Pid, update_taglist).

getuserinfo(Pid, Val) ->
	gen_server:call(Pid, {getuserinfo, Val}).

setkey(Pid, Val) ->
	gen_server:call(Pid, {setkey, Val}).

authorize(Pid, Val) ->
	gen_server:call(Pid, {authorize, Val}).

updatekey(Pid, Val) ->
	gen_server:call(Pid, {updatekey, Val}).

close_server(Pid) ->
	supervisor:terminate_child(tag_riak_sup, Pid),
	ok.


