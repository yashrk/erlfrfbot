-module(frfbot).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_posts/1,
         login/0,
         post/2,
         comment/2,
         is_logged_in/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {logged_in=false, token}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

login() ->
    gen_server:call(?MODULE, {login}).

post(Feed, Text) ->
    gen_server:call(?MODULE, {post, Feed, Text}).

comment(PostID, Text) ->
    gen_server:call(?MODULE, {comment, PostID, Text}).

is_logged_in() ->
    gen_server:call(?MODULE, {is_logged_in}).

get_posts(Feed) ->
    gen_server:call(?MODULE, {get_posts, Feed}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {_Status, State} = get_token(#state{}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({login}, _From, State) ->
    {Status, NewState} = get_token(State),
    {reply, Status, NewState};
handle_call({post, Feed, Text}, _From, State) ->
    Status = post(Feed, Text, State),
    {reply, Status, State};
handle_call({comment, PostID, Text}, _From, State) ->
    Status = comment(PostID, Text, State),
    {reply, Status, State};
handle_call({is_logged_in}, _From, State) ->
    {reply, State#state.logged_in, State};
handle_call({get_posts, Feed}, _From, State) ->
    Posts = get_posts(Feed, State),
    {reply, Posts, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_command, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_token(State) ->
    {ok, User} = application:get_env(frfbot, user),
    {ok, Password} = application:get_env(frfbot, password),
    {ok, AuthResponse} = httpc:request(
        post,
        {
          "https://freefeed.net/v1/session",
          [],
          "application/x-www-form-urlencoded",
          "username=" ++ User ++ "&password=" ++ Password
        },
        [],
        []
    ),
    {_HttpCode, _Headers, JSON} = AuthResponse,
    ResponseMap = jiffy:decode(JSON, [return_maps]),
    #{<<"authToken">> := Token} = ResponseMap,
    {ok, State#state{logged_in=true, token=Token}}.

post(Feed, Post, State) ->
    Token = State#state.token,
    Request = #{
        <<"post">> => #{
            <<"body">> => Post
        },
        <<"meta">> => #{
            <<"feeds">> => Feed
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

get_posts(Feed, State) ->
    Token = State#state.token,
    {ok, {_StatusCode, _Headers, Data}} = httpc:request(
        get,
        {
            "https://freefeed.net/v1/timelines/" ++ Feed,
            [{"x-authentication-token", binary_to_list(Token)}]
        },
        [],
        []
     ),
    Response = jiffy:decode(Data, [return_maps]),
    #{<<"posts">> := Posts} = Response,
    Posts.

comment(PostID, Text, State) ->
    Token = State#state.token,
    Request = #{
        <<"comment">> => #{
            <<"body">> => Text,
            <<"postId">> => PostID
        }
    },
    RequestJSON = jiffy:encode(Request),
    io:format(RequestJSON),
    {ok, {StatusCode, _Headers, Data}} = httpc:request(
        post,
        {
            "https://freefeed.net/v1/comments/",
            [{"x-authentication-token", binary_to_list(Token)}],
            "application/json",
            RequestJSON
        },
        [],
        []
    ),
    {StatusCode, Data}.
