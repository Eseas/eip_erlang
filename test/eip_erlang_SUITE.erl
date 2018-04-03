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

%%%
%%% Common Tests for `sim' application.
%%%
-module(eip_erlang_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).
-export([
    test_integration/1
]).

-define(APP, eip_erlang).

%%% ============================================================================
%%% Callbacks for `common_test'
%%% ============================================================================

%%  @doc
%%  CT API.
%%
all() -> [
    test_integration
].


%%
%%  CT API, initialization.
%%
init_per_suite(Config) ->
    application:load(lager),
%    application:set_env(lager, handlers, [{lager_console_backend, debug}]),
    {ok, Apps} = application:ensure_all_started(eip_erlang),
    [{eip_erlang_apps, Apps} | Config].


%%
%%  CT API, cleanup.
%%
end_per_suite(Config) ->
    ok = lists:foreach(
        fun (A) -> application:stop(A) end,
        proplists:get_value(eip_erlang_apps, Config)
    ),
    ok.


%%
%%  Log test case name at start
%%
init_per_testcase(TestCase, Config) ->
    lager:debug("---------------------- ~p start", [TestCase]),
    Config.


%%
%%  Log test case name at end. Also, clean subscriptions and metrics.
%%
end_per_testcase(TestCase, _Config) ->
    lager:debug("---------------------- ~p end", [TestCase]),
    ok.



%%% ============================================================================
%%% Test cases.
%%% ============================================================================

%%  @doc
%%
%%
test_integration(_Config) ->
%   start platform with its supervisor
%   {ok, Platform} = eip_erlang_platform:start_link(),

%   {ok, Endpoint1} = eip_erlang_endpoint:start_link(Platform, endpoint1),
%   {ok, Endpoint2} = eip_erlang_endpoint:start_link(Platform, endpoint2),


%   This should be in endpoint
%   {ok, Channel} = eip_erlang_channel:start_link(Platform, channel),




%    {ok, _WarehouseChannelPid} = sim_warehouse_channel:start_link(),
%    {ok, _WarehouseRetChannelPid} = sim_warehouse_ret_channel:start_link(),
%    {ok, _CourierChannelPid}   = sim_courier_channel:start_link(),
%    {ok, _CourierRetChannelPid}   = sim_courier_ret_channel:start_link(),
%    {ok, _EshopRetChannelPid}   = sim_eshop_ret_channel:start_link(),
%
%    ok = sim_eshop_endpoint:send([order1, order2]),
%    timer:sleep(50),

    1 = 0, %% To see lager logs
    ok.

