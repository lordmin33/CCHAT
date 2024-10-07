-module(client).
-export([handle/2, initial_state/3]).

%
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

%tom lista, intialiala state av alla processer
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

%Tar emot request innan genserver för felhantering.
request(Pid, Data) ->
    try genserver:request(Pid, Data) of
        Reply -> Reply
    catch
        error:_ -> {error, server_not_reached, "Server could not be reached"};
        throw:_ -> {error, server_not_reached, "Server could not be reached"}
    end.


%Skickar request till servern som 
%skickar vidare till channel och uppdaterar channellistan
handle(St, {join, Channel}) ->
    case request(St#client_st.server, {join, self(), St#client_st.nick, Channel}) of
        ok ->
            {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
        Error ->
            {reply, Error, St}
    end;

%Skickar direkt till channel och uppdaterar clientens channellista
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of
        false ->
            {reply, {error, user_not_joined, "User is not in channel"}, St};
        true ->
            Reply = request(list_to_atom(Channel), {leave, self()}),
            NewChannels = lists:delete(Channel, St#client_st.channels),
            {reply, Reply, St#client_st{channels = NewChannels}}
    end;

%Skickar request till channel från guin med medelandet
%Kollar om channel är aktiv genom active, om inte channel finns med 
%i klientens channellista 
%kollas om channeln aktiv, om den är aktiv så har usern
% inte joinat annars är channeln död

handle(St, {message_send, Channel, Msg}) ->
    case lists:member(Channel, St#client_st.channels) of
        true ->
            request(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg}),
            {reply, ok, St};
        false ->
            Reply = request(list_to_atom(Channel), {is_active}),
            case Reply of
                active -> {reply, {error, user_not_joined, "TEST: User is not in channel"}, St};
                _ -> {reply, {error, server_not_reached, "Server could not be reached"}, St}
            end
    end;


%ta bort ur channellistan och uppdaterar klientens channels
% med ny lista
handle(St, {remove_channel, Channel}) ->
    {reply, ok, St#client_st{channels = lists:delete(Channel, St#client_st.channels)}};
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
