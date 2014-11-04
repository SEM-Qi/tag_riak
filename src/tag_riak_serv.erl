-module(tag_riak_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2]).


%Where Pid is the pid of the requesting process (hopefully)
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("greedo.skip.chalmers.se", 8087),
  {ok, Pid}.

%DO Call/Cast Handling here

handle_call(update_taglist, _From, SocketPid) ->
	case riakc_pb_socket:get(SocketPid, <<"taglistbucket">>, <<"taglist">>) of
        {ok, CurrentTaglist} -> 
          FinalTaglist = binary_to_term(riakc_obj:get_value(CurrentTaglist)),  
          Taglist = jiffy:encode({[{<<"tags">>, FinalTaglist}]});
        {error,_} ->
          Taglist = jiffy:encode({[{<<"tags">>, <<"Bad List">>}]})
  end,
	% {ok, Taglist} = riakc_pb_socket:list_buckets(Pid),
	{reply, Taglist, SocketPid};

handle_call({gettag, Tag}, _From, SocketPid) ->
  {ok, Keys} = riakc_pb_socket:list_keys(SocketPid, <<"tags">>),
  AllKeys = lists:reverse(lists:sort(Keys)),
  {NewKeys,_} = lists:split(20, AllKeys),
  Objects = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, <<"tags">>, Key), Obj end, NewKeys),
  Tagset = lists:map(fun(Object) -> Value = binary_to_term(riakc_obj:get_value(Object)), {ok, Tag} = dict:find(Value), Tag end, Objects),
  {Distribution, Cotags} = loopThrough(Tagset, [], sets:new()),
  Response = jiffy:encode({[{<<"tag">>, Tag},
  {<<"cotags">>, Cotags},
  {<<"distribution">>, 
    Distribution}]}),
{reply, Response, SocketPid}.

% {[{foo, bar}]}

% {[{<<"tag">>, <<"tag">>},
%   {<<"cotags">>, [<<"cotags">>, <<"cotags">>]},
%   {<<"distribution">>, 
%     [{[{<<"numtags">>, 1}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 2}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 3}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 4}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 5}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 6}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 7}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 8}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 9}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 10}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]}]}]},
% {
%     "tag": "string",
%     "cotags":["string"],
%     "distribution": [
%         {
%             "numtags":"integer",
%             "tweets":["string"]
%         }]
% }

% {ok, List} = riakc_pb_socket:list_keys(SocketPid, Tag),
% NumTag = length(List),
% Result = jiffy:encode({[{Tag, NumTag}]}),
% % {ok, Taglist} = riakc_pb_socket:list_buckets(Pid),
% {reply, Result, SocketPid}.

handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% temporary timestamp function
% testoldTimeStamp(Second) ->
%   1414506936992434 - ((Second * 120) * 1000000).

% %% When miner is live, this is the correct timestamp function

% % oldTimeStamp(Second) ->
% %   {Mega, Secs, Micro} = erlang:now(),
% %   Mega*1000*1000*1000*1000 + ((Secs - (Second *120)) * 1000 * 1000) + Micro.

% getTagBySec(Second, Tag, SocketPid) ->
%   case riakc_pb_socket:get_index_range(
%             SocketPid,
%             Tag, %% bucket name
%             {integer_index, "timestamp"}, %% index name
%             testoldTimeStamp(Second), testoldTimeStamp(Second-1) %% origin timestamp should eventually have some logic attached
%           ) of
%     {ok, {_,[],_,_}} ->
%       {0, [], []};
%     {ok, {_,Keys,_,_}} ->
%       TagObjs = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, Tag, Key), Obj end, Keys),
%       Allofthecotags = lists:foldl(fun(Object, AllCotags) -> Binary = riakc_obj:get_value(Object), 
%                         {Cotags, _} = binary_to_term(Binary), Cotags ++ AllCotags end, [], TagObjs),
%       {length(Keys), Allofthecotags, [<<"dummy tweet text">>]}
%   end.

loopThrough([], L, Cotags) -> {L, sets:to_list(Cotags)};
loopThrough(Tagset, L, OldCotags) ->
  {NewKeys,OldKeys} = lists:split(2, Tagset),
  [{Num, Cotags, Tweets}, {Num2, Cotags2, Tweets2}] = NewKeys,
  L2 = [{[{<<"numtags">>, Num + Num2}, {<<"tweets">>, sets:to_list(sets:union([Tweets, Tweets2]))}]}|L],
  NewCotags = sets:union([Cotags, Cotags2, OldCotags]),
  loopThrough(OldKeys, L2, NewCotags).