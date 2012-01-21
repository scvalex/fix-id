-module(fix_id_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fix_id_mnesia:init(),
    fix_id_sup:start_link().

stop(_State) ->
    ok.
