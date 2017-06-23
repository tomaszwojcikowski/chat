-module(chat_client).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).

-export([connect/2]).
-export([send/3]).

-record (state, {
                 server = undefined,
                 clients = #{},
                 name = "",
                 outbox = #{}
                 }).

% public api

start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

connect(Pid, Server) ->
    gen_server:call(Pid, {connect, Server}).

-spec send(pid(), string(), term()) -> ok | no_client.
send(Pid, ClientName, Msg) ->
    gen_server:call(Pid, {send, ClientName, Msg}).

% state should be change with State that you will pass
init([Name]) ->
  {ok, #state{name=Name}}.

stop(Pid) ->
    gen_server:cast(Pid, stop).

handle_call({connect, Server}, _From, State=#state{name=Name}) ->
    Server ! {connect, self(), Name},
    {reply, ok, State};
handle_call(_, _From, State=#state{server=undefined}) ->
    {reply, not_connected, State};
handle_call({send, ClientName, Msg}, _From, State=#state{}) ->
   {R, State1} = send_msg(ClientName, Msg, State),
   {reply, R, State1}.

handle_info({connected, Server, Clients}, State) ->
    io:format("Connected to ~p, clients: ~p~n", [Server, Clients]),
    erlang:monitor(process, Server),
    {noreply, State#state{server=Server, clients=Clients}};
handle_info({message, ClientPid, Msg}, State) ->
    display_msg(ClientPid, Msg, State),
    sent_read(ClientPid),
    {noreply, State};
handle_info({read, ClientPid}, State) ->
    {noreply, mark_as_read(ClientPid, State)};
handle_info({status, disconnected, ClientPid, _}, State) ->
    io:format("~p disconnected~n", [ClientPid]),
    {noreply, remove(ClientPid, State)};
handle_info({status, connected, ClientPid, Name}, State) ->
    io:format("~p:~p connected~n", [ClientPid, Name]),
    {noreply, add(ClientPid, Name, State)};
handle_info({'DOWN', _MonitorRef, _Type, Server, _Info}, #state{server=Server}) ->
    {noreply, #state{}}.


add(ClientPid, Name, State = #state{clients=Clients}) ->
    State#state{clients=maps:put(ClientPid, Name, Clients)}.

remove(ClientPid, State = #state{clients=Clients}) ->
    State#state{clients=maps:remove(ClientPid, Clients)}.

display_msg(ClientPid, Msg, State) ->
    io:format("~p: ~p~n", [get_name(ClientPid, State), Msg]),
    State.

send_msg(ClientName, Msg, State=#state{outbox=OutboxMap}) ->
 case find_by_name(ClientName, State) of
        {ok, ClientPid} ->
            ClientPid ! {message, self(), Msg},
            Ob = maps:get(ClientPid, OutboxMap, []),
            OutboxMap1 = maps:put(ClientPid, Ob++[Msg], OutboxMap),
            {ok, State#state{outbox=OutboxMap1}};
        _ -> {no_client, State}
 end.

sent_read(ClientPid) ->
    ClientPid ! {read, self()}.

mark_as_read(ClientPid, State = #state{outbox=OutboxMap}) ->
    case maps:get(ClientPid, OutboxMap, []) of
        [] -> State;
        [_|T] -> State#state{outbox=maps:put(ClientPid, T, OutboxMap)}
    end.

handle_cast(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

get_name(ClientPid, #state{clients=Clients}) ->
    maps:get(ClientPid, Clients).

find_by_name(ClientName, #state{clients=Clients}) ->
    maps:fold(fun
        (Pid, V, none) when V =:= ClientName -> {ok, Pid};
        (_, _, N) -> N
    end, none, Clients).
