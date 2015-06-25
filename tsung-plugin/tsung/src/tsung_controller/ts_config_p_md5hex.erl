-module(ts_config_p_md5hex).
-vc('$Id$ ').
-author('garuda @ blogspot.com').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_p_md5hex.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse a request defined in the XML config file
%% Args:     Element, Config
%% Returns:  List
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);

parse_config(Element = #xmlElement{name=p_md5hex, attributes=Attrs},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->
    Req = case ts_config:getAttr(string,Attrs, datasize) of
               [] ->
                   Value = ts_config:getAttr(string, Attrs, value),
                   #p_md5hex{value=Value}
          end,
    ts_config:mark_prev_req(Id-1, Tab, CurS),
    Msg=#ts_request{ack     = parse,
                    subst   = SubstFlag,
                    match   = MatchRegExp,
                    param   = Req},
    ets:insert(Tab,{{CurS#session.id, Id},Msg#ts_request{endpage=true,
                                                         dynvar_specs=DynVar}}),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);

%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

