-module(eip_endpoint).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% API
-export([]).


-record(state, {
    cb_mod :: module(),
}).

start_link(CBModule) ->
    gen_server:start_link(?MODULE, {CBModule}, []).

command(EndpointPid, Type, Content) ->
    gen_server:call(EndpointPid, {Type, Content}).




%%% =============================================================================
%%% Callbacks for `gen_server`.
%%% =============================================================================

init({CBModule}) ->
    State = #state{
        cb_mod   = CBModule,
    },
    {ok, State}.



    %%
    %%
    %%
    handle_call({send, Content}, _From, State = #state{cb_mod = CBModule}) ->
        CBModule:send()
        {reply, ok, State};

    handle_call(_Msg, _From, State) ->
        {reply, ok, State}.



    %%
    %%
    %%
    handle_cast(_Message, State) ->
        {noreply, State}.


    %%
    %%
    %%
    handle_info(_Message, State) ->
        {noreply, State}.


    %%
    %%
    %%
    terminate(_Reason, _State) ->
        ok.


    %%
    %%  Code upgrades.
    %%
    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

