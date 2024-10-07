-module(server).
-export([start/1,stop/1]).


-record(serverstate, {
    channels,
    users
}).

initialState() -> 
    #serverstate{
        channels = [],
        users = []
    }.

start(ServerAtom) ->
    genserver:start(ServerAtom, initialState(), fun handle/2).


stop(ServerAtom) ->
    genserver:request(ServerAtom, {stop, ServerAtom}).

%koll om att channeln är med i servers channlista,
% om inte så starta channel och lägga till den
%annars joinar man den
%New channel/new users är temp variabel för att uppdatera 
%i servern
handle(St, {join, Pid, Nick, Channel}) ->

    NewChannels =
        case lists:member(Channel, St#serverstate.channels) of
            false ->
                channel:start(Channel),
                [Channel | St#serverstate.channels];
            true ->
                St#serverstate.channels
        end,
    
    NewUsers =
        case lists:member(Nick, St#serverstate.users) of
            false ->
                [Nick | St#serverstate.users];
            true ->
                St#serverstate.users
        end,
        
    Reply = genserver:request(list_to_atom(Channel), {join, Pid}),
    {reply, Reply, St#serverstate{channels = NewChannels, users = NewUsers}};



handle(St, {stop, ServerAtom}) ->
    lists:foreach(fun(Channel) ->
            channel:stop(Channel)
        end, St#serverstate.channels),
        genserver:stop(ServerAtom),
    {reply, ok, St}.
