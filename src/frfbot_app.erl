-module(frfbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init(),
    frfbot_sup:start_link().

stop(_State) ->
    ok.

init() ->
    inets:start(),
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(jiffy),
    application:start(erlcron).
