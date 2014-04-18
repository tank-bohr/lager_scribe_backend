-module(lager_scribe_category).

-export ([default/1]).

-callback category(Message) -> string()
    when Message :: lager_msg:lager_msg().

-spec default(lager_msg:lager_msg()) -> string().
default(Message) ->
    "lager." ++ atom_to_list(lager_msg:severity(Message)).
