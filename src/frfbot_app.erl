-module(frfbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([get_token/0, post/1]).

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
    ets:new(frfdata, [bag, named_table, public]).

get_token() ->
    {ok, Feed} = application:get_env(frfbot, feed),
    {ok, Password} = application:get_env(frfbot, password),
    {ok, AuthResponse} = httpc:request(
        post,
        {
          "https://freefeed.net/v1/session",
          [],
          "application/x-www-form-urlencoded",
          "username=" ++ Feed ++ "&password=" ++ Password
        },
        [],
        []
    ),
    {_HttpCode, _Headers, JSON} = AuthResponse,
    ResponseMap = jiffy:decode(JSON, [return_maps]),
    #{<<"authToken">> := Token} = ResponseMap,
    true = ets:insert(frfdata, {token, Token}).

post(Post) ->
    {ok, Feed} = application:get_env(frfbot, feed),
    FeedBin = list_to_binary(Feed),
    [{token, Token}] = ets:lookup(frfdata, token),
    Request = #{
        <<"post">> => #{
            <<"body">> => Post
        },
        <<"meta">> => #{
            <<"feeds">> => FeedBin
        }
    },
    RequestJSON = jiffy:encode(Request),
    httpc:request(
        post,
        {
            "https://freefeed.net/v1/posts",
            [{"x-authentication-token", binary_to_list(Token)}],
            "application/json",
            RequestJSON
        },
        [],
        []
     ).
