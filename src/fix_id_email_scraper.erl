-module(fix_id_email_scraper).

-behaviour(gen_smtp_server_session).

-export([start_link/0]).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2,
         handle_MAIL_extension/2, handle_RCPT/2,
         handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1,
         handle_VRFY/2, handle_other/3, handle_AUTH/4, code_change/3,
         terminate/2]).

-define(RELAY, true).

-record(state, { from :: string() | 'undefined',
                 to   :: string() | 'undefined' }).

-type(error_message() :: {'error', string(), #state{}}).

%% @doc Start the email server with default options.
-spec(start_link/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link() ->
    case application:get_env(email_port) of
        undefined  -> gen_smtp_server:start_link(?MODULE);
        {ok, Port} -> gen_smtp_server:start_link(?MODULE, [[{port, Port}]])
    end.

%% @doc Initialize the callback module's state for a new session.
%%
%% The arguments to the function are the SMTP server's hostname (for
%% use in the SMTP manner), The number of current sessions (eg. so you
%% can do session limiting), the IP address of the connecting client,
%% and a freeform list of options for the module. The Options are
%% extracted from the `callbackoptions' parameter passed into the
%% `gen_smtp_server_session' when it was started.
%%
-spec(init/4 ::
        (Hostname :: binary(), SessionCount :: non_neg_integer(),
         Address :: tuple(), Options :: list()) ->
                     {'ok', string(), #state{}} | {'stop', any(), string()}).
init(Hostname, SessionCount, Address, _Options) ->
    lager:info("Connected peer: ~p", [Address]),
    case SessionCount > 20 of
        false ->
            Banner = [Hostname, " Fix-Id Email Scraper"],
            State = #state{from = 'undefined',
                           to   = 'undefined'},
            {ok, Banner, State};
        true ->
            lager:info("Connection limit exceeded"),
            {stop, normal, ["421 ", Hostname,
                            " is too busy to accept mail right now"]}
    end.

%% @doc Handle the HELO verb from the client. Arguments are the
%% Hostname sent by the client as part of the HELO and the callback
%% State.
%%
%% Return values are `{ok, State}' to simply continue with a new
%% state, `{ok, MessageSize, State}' to continue with the SMTP session
%% but to impose a maximum message size (which you can determine , for
%% example, by looking at the IP address passed in to the init
%% function) and the new callback state. You can reject the HELO by
%% returning `{error, Message, State}' and the Message will be sent
%% back to the client. The reject message MUST contain the SMTP status
%% code, eg. 554.
-spec(handle_HELO/2 ::
        (Hostname :: binary(), State :: #state{}) ->
                            {'ok', pos_integer(), #state{}} |
                            {'ok', #state{}} | error_message()).
handle_HELO(<<"trusted_host">>, State) ->
    {ok, State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
    lager:info("HELO from ~s", [Hostname]),
    {ok, 655360, State}. %% 640kb of HELO should be enough for anyone.
                         %% If {ok, State} was returned here, we'd use
                         %% the default 10mb limit

%% @doc Handle the EHLO verb from the client. As with EHLO the
%% hostname is provided as an argument, but in addition to that the
%% list of ESMTP Extensions enabled in the session is passed. This
%% list of extensions can be modified by the callback module to
%% add/remove extensions.
%%
%% The return values are `{ok, Extensions, State}' where Extensions is
%% the new list of extensions to use for this session or `{error,
%% Message, State}' where Message is the reject message as with
%% handle_HELO.
-spec(handle_EHLO/3 ::
        (Hostname :: binary(), Extensions :: list(), State :: #state{}) ->
                            {'ok', list(), #state{}} | error_message()).
handle_EHLO(<<"invalid">>, _Extensions, State) ->
    %% contrived example
    {error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
    lager:info("EHLO from ~s", [Hostname]),
    {ok, Extensions, State}.

%% @doc Handle the MAIL FROM verb. The From argument is the email
%% address specified by the MAIL FROM command. Extensions to the MAIL
%% verb are handled by the `handle_MAIL_extension' function.
%%
%% Return values are either `{ok, State}' or `{error, Message, State}'
%% as before.
-spec(handle_MAIL/2 ::
        (From :: binary(), State :: #state{}) ->
                            {'ok', #state{}} | error_message()).
handle_MAIL(<<"badguy@blacklist.com">>, State) ->
    {error, "552 go away", State};
handle_MAIL(From, State) ->
    lager:info("Mail from ~s", [From]),
    %% you can accept or reject the FROM address here
    {ok, State}.

%% @doc Handle an extension to the MAIL verb. Return either `{ok,
%% State}' or `error' to reject the option.
-spec(handle_MAIL_extension/2 ::
        (Extension :: binary(), State :: #state{}) ->
                                      {'ok', #state{}} | 'error').
handle_MAIL_extension(<<"X-SomeExtension">> = Extension, State) ->
    lager:info("Mail from extension ~s", [Extension]),
    %% any MAIL extensions can be handled here
    {ok, State};
handle_MAIL_extension(Extension, _State) ->
    lager:info("Unknown MAIL FROM extension ~s", [Extension]),
    error.

-spec(handle_RCPT/2 ::
        (To :: binary(), State :: #state{}) ->
                            {'ok', #state{}} | {'error', string(), #state{}}).
handle_RCPT(<<"nobody@example.com">>, State) ->
    {error, "550 No such recipient", State};
handle_RCPT(To, State) ->
    lager:info("Mail to ~s", [To]),
    %% you can accept or reject RCPT TO addesses here, one per call
    {ok, State}.

-spec(handle_RCPT_extension/2 ::
        (Extension :: binary(), State :: #state{}) ->
                                      {'ok', #state{}} | 'error').
handle_RCPT_extension(Extension, _State) ->
    lager:info("Unknown RCPT TO extension ~s", [Extension]),
    error.

-spec(handle_DATA/4 ::
        (From :: binary(), To :: [binary(),...], Data :: binary(),
         State :: #state{}) ->
                            {'ok', string(), #state{}} |
                            {'error', string(), #state{}}).
handle_DATA(_From, _To, <<>>, State) ->
    {error, "552 Message too small", State};
handle_DATA(From, To, Data, State) ->
    %% some kind of unique id
    Reference =
        lists:flatten([io_lib:format("~2.16.0b", [X]) ||
                          <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),
    %% if RELAY is true, then relay email to email address, else send
    %% email data to console
    lager:info("Message from ~s to ~p queued as ~s, body length ~p",
               [From, To, Reference, byte_size(Data)]),
    ok = fix_id_mnesia:add_raw_email(erlang:binary_to_list(From),
                                     [erlang:binary_to_list(Recp) || Recp <- To],
                                     erlang:binary_to_list(Data)),
    try mimemail:decode(Data) of
        Result -> lager:info("Message decoded successfully!~n'~p'", [Result])
    catch
        What:Why ->
            lager:info("Message decode FAILED with ~p:~p", [What, Why]),
            %% optionally dump the failed
            %% email somewhere for analysis
            File = "dump/"++Reference,
            case filelib:ensure_dir(File) of
                ok ->
                    file:write_file(File, Data);
                _ ->
                    ok
            end
    end,
    %% At this point, if we return ok, we've accepted responsibility
    %% for the email
    {ok, Reference, State}.

-spec(handle_RSET/1 :: (State :: #state{}) -> #state{}).
handle_RSET(State) ->
    %% reset any relevant internal state
    State.

-spec(handle_VRFY/2 :: (Address :: binary(), State :: #state{}) ->
                            {'ok', string(), #state{}} |
                            {'error', string(), #state{}}).
handle_VRFY(Address, State) ->
    lager:info("VRFY ~p", [Address]),
    {error, "252 VRFY disabled by policy, just send some mail", State}.

-spec(handle_other/3 ::
        (Verb :: binary(), Args :: binary(), #state{}) ->
                             {string(), #state{}}).
handle_other(Verb, Args, State) ->
    lager:info("Other verb '~p': ~p", [Verb, Args]),
    %% You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.

%% this callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
-spec(handle_AUTH/4 ::
        (Type :: 'login' | 'plain' | 'cram-md5', Username :: binary(),
         Password :: binary() | {binary(), binary()}, #state{}) ->
                            {'ok', #state{}} | 'error').
handle_AUTH(Type, <<"username">>, <<"PaSSw0rd">>, State)
  when Type =:= login; Type =:= plain ->
    {ok, State};
handle_AUTH('cram-md5', <<"username">>, {Digest, Seed}, State) ->
    case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
        Digest -> {ok, State};
        _      -> error
    end;
handle_AUTH(Type, Username, _Password, _State) ->
    lager:info("AUTH '~p': ~p", [Type, Username]),
    error.

-spec(code_change/3 ::
        (OldVsn :: any(), State :: #state{}, Extra :: any()) ->
                            {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec(terminate/2 ::
        (Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}).
terminate(Reason, State) ->
    {ok, Reason, State}.

%%% Internal Functions %%%
