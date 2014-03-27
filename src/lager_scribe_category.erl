-module(lager_scribe_category).

-export ([default/1]).

-callback category(Message) -> string()
    when Message :: lager_msg:lager_msg().

default(Message) ->
    "lager." ++ atom_to_list(Message:severity()).
