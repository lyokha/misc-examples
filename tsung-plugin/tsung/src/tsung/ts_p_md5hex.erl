-module(ts_p_md5hex).
-author('garuda @ blogspot.com').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_p_md5hex.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/2,
         session_defaults/0,
         dump/2,
         parse/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0,
         md5hex/1]).

%%----------------------------------------------------------------------
%% Function: session_defaults/0
%% Purpose:  default parameters for session (ack_type and persistent)
%% Returns:  {ok, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: decode_buffer/0
%% Purpose:  decode buffer for matching or dyn_variables
%% Returns:  decoded buffer
%%----------------------------------------------------------------------
decode_buffer(Buffer, #p_md5hex{}) ->
    Buffer.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose:  initialize session information
%% Returns:  record or []
%%----------------------------------------------------------------------
new_session() ->
    #p_md5hex{}.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose:  Parse the given data and return a new state
%% Args:     Data (binary), State (record)
%% Returns:  NewState (record)
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true}, [], true};

parse(Data, State) ->
    {State#state_rcv{datasize = size(Data)}, [], false}.

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

dump(A,B) ->
    ts_plugin:dump(A,B).

%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose:  Build a message/request
%% Args:     #p_md5hex
%% Returns:  binary
%%----------------------------------------------------------------------
get_message(#p_md5hex{value=Value}, #state_rcv{session=S}) ->
    Packet=ts_p_md5hex_nif:request(Value),
    {Packet, S}.

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_p_md5hex:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose:  add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_, [], Param, _Host) ->
    Param;

add_dynparams(true, DynData, OldReq, _Host) ->
    subst(OldReq, DynData#dyndata.dynvars);

add_dynparams(_Subst, _DynData, Param, _Host) ->
    Param.

%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose:  Replace on the fly dynamic element of the request.
%%----------------------------------------------------------------------
subst(Req=#p_md5hex{value=Value}, DynData) ->
    Req#p_md5hex{value=ts_search:subst(Value, DynData)}.

init_dynparams() ->  #dyndata{}.

%%----------------------------------------------------------------------
%% Function: md5hex/1
%% Purpose:  Retrieve md5hex message from the request.
%%----------------------------------------------------------------------
md5hex(Data) ->
    element(2, ts_p_md5hex_nif:response(Data)).

