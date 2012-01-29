-module(fix_id_mnesia).

-export([init/0]).

-define(CURRENT_VERSION, 2).
-record(fix_id_version, {application, version}).
-record(fix_id_raw_emails, {sha, from, to, data}).

init() ->
    lager:info("Mnesia directory: ~p~n", [dir()]),
    ok = ensure_schema(),
    ok = mnesia:start(),
    ok = ensure_tables(),
    lager:info("Database started normally~n"),
    ok.

%% Fix-id table definitions.
tables() ->
    [{fix_id_version,
      [{attributes, record_info(fields, fix_id_version)},
       {disc_copies, [node()]}]},
     {fix_id_raw_emails,
      [{attributes, record_info(fields, fix_id_raw_emails)},
       {disc_copies, [node()]}]}].

%% Create the disc schema.  Don't fail if it already exists.
ensure_schema() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok                                      -> ok;
        {error, {Node, {already_exists, Node}}} -> ensure_on_disc();
        Error                                   -> Error
    end.

%% The schema exists; let's make sure the needed tables also exist.
ensure_tables() ->
    try mnesia:table_info(fix_id_version, all)
    of _ -> ok = mnesia:wait_for_tables([fix_id_version], 5000)
    catch exit:{aborted, {no_exists, _, _}} -> ok
    end,

    Version = try mnesia:dirty_read(fix_id_version, fix_id)
              catch exit:{aborted, {no_exists, _}} -> []
              end,
    case Version of
        [] ->
            create_tables(tables());
        [{fix_id_version, fix_id, OtherVersion}]
          when OtherVersion =< ?CURRENT_VERSION ->
            ok = lists:foldl(fun(Version, ok) ->
                                     upgrade_tables(Version)
                             end, ok, lists:seq(OtherVersion + 1,
                                                ?CURRENT_VERSION))
    end.

%% Ensure that the schema is on disc.
ensure_on_disc() ->
    NeedsWrite = case mnesia:table_info(schema, disc_copies) of
                     [] -> true;
                     Ns -> not lists:member(node(), Ns)
                 end,
    case NeedsWrite of
        true  -> {atomic, ok} = mnesia:change_table_copy_type(schema, node(),
                                                              disc_copies),
                 ok;
        false -> ok
    end.

%% Upgrade existing tables to an existing version.
upgrade_tables(2) ->
    create_tables(lists:filter(fun({Name, _}) ->
                                       Name == fix_id_raw_emails
                                  end, tables()));
upgrade_tables(Version) ->
    exit({unknown_table_upgrade, Version}).

%% Create a fresh copy of the tables.
create_tables(Tables) ->
    ok = lists:foldl(fun({Name, TabDef}, ok) ->
                             {atomic, ok} = mnesia:create_table(Name, TabDef),
                             ok
                     end, ok, Tables),
    {atomic, ok} =
        mnesia:transaction(
          fun() -> mnesia:write(
                     #fix_id_version{application = fix_id,
                                     version     = ?CURRENT_VERSION})
          end),
    lager:info("Created fresh tables ~p (v~p)~n", [Tables, ?CURRENT_VERSION]),
    ok.

%% Find the mnesia directory.
dir() -> mnesia:system_info(directory).
