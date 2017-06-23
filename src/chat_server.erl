-module(chat_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3, stop/1]).

-record (state, {clients = #{}}).

% public api
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% state should be change with State that you will pass
init([]) ->
    {ok, #state{}}.

stop(_Pid) ->
  stop().

stop() ->
  gen_server:cast(?MODULE, stop).

handle_call(_Message, _From, State) ->
  {reply, error, State}.

handle_cast(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


handle_info({connect, ClientPid, ClientName}, State) ->
    State1 = case is_client_connected(ClientPid, State) of
                true ->  State;
                false ->
                    connect_client(ClientPid, ClientName, State)
            end,
    {noreply, State1};
handle_info({'DOWN', _MonitorRef, _Type, ClientPid, _Info}, State) ->
    {noreply, disconnect_client(ClientPid, State)};
handle_info(_Message, State) ->
    {noreply, State}.

connect_client(ClientPid, ClientName, State=#state{clients=Clients}) ->
    ClientPid ! {connected, self(), Clients},
    erlang:monitor(process, ClientPid),
    Clients1 = maps:put(ClientPid, ClientName, Clients),
    broadcast_status(ClientPid, ClientName, connected, State#state{clients=Clients1}).

disconnect_client(ClientPid, State=#state{clients=Clients}) ->
    case is_client_connected(ClientPid, State) of
        false -> State; % ignore already disconnected user
        true ->
            Clients1 = maps:remove(ClientPid, Clients),
            broadcast_status(ClientPid, <<>>, disconnected, State#state{clients=Clients1})
    end.

broadcast_status(ClientPid, Name, Status, State = #state{clients=Clients}) ->
    maps:fold(fun (Pid, _, _) ->
        case Pid of
            ClientPid -> ok;
            _ ->
                Pid ! {status, Status, ClientPid, Name}
        end
    end, ok, Clients),
    State.

is_client_connected(ClientPid, #state{clients=Clients}) ->
    maps:is_key(ClientPid, Clients).


