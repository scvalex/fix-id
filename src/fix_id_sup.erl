-module(fix_id_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(MAX_WAIT, 16#ffffffff).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [{email_scraper, {email_scraper, start_link, []},
             permanent, ?MAX_WAIT, worker, [email_scraper]}] }}.
