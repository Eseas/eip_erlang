%/--------------------------------------------------------------------
%| Copyright 2018 Kazimieras Senvaitis
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%% @doc
%%% Responsible for managing subscription configuration given in `sys.config'.
%%%
-module(sim_endpoint_eshop).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-export([
    start_link/0,
    send/1,
    recv/1
]).
-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3,
    code_change/3,
    terminate/2
]).

-define(REPORTER, exometer_graphite_reporter).
-define(DEFAULT_RESUB_DELAY, 60000).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Start a subscription manager.
%%
start_link() ->
    {ok, _Pid} = gen_server:start_link({local, eshop}, ?MODULE, [], []).


%%  @doc
%%  Used to start resubscription manually.
%%
send(Order) ->
    gen_server:call(eshop, {send, Order}).

recv(Info) ->
    eshop ! {recv, Info},
    ok.

%%% ============================================================================
%%% Internal state of the module.
%%% ============================================================================

-record(state, {
    warehouse_channel_pid :: term() | undefined
}).



%%% ============================================================================
%%% Callbacks for `gen_server'.
%%% ============================================================================

%% @doc
%% Sets up subscription configuration loop.
%%
init(_) ->
    {ok, #state{}}.


%% @doc
%% Unused.
%%
handle_call({send, Order},  _From, State) ->
    lager:debug("ESHOP: CALLING WAREHOUSE"),
    sim_warehouse_channel:send(Order),
    {reply, ok, State};

handle_call(_Unknown, _From, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
handle_cast(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Updates subscriptions to metrics and continues message sending loop.
%%
handle_info({recv, {status, Status}}, State) ->
    lager:debug("CONFIRMED"),
    {noreply, State};

handle_info(_Unknown, State) ->
    {noreply, State}.


%% @doc
%% Unused.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc
%% Unused.
%%
terminate(_Reason, _State) ->
    ok.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================


