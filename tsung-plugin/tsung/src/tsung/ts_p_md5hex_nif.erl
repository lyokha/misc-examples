-module(ts_p_md5hex_nif).
-author('garuda @ blogspot.com').

-export([request/1, response/1]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("./ts_p_md5hex_nif", 0).

request(_Value) ->
    exit(nif_library_not_loaded).

response(_Content) ->
    exit(nif_library_not_loaded).

