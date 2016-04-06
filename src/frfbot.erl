-module(frfbot).

-behaviour(gen_server).

%% API
-export([start_link/0,
         login/0,
         post/1]).

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

post(Text) ->
    gen_server:call(?MODULE, {post, Text}).

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
    {ok, #state{}}.

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
handle_call({post, Text}, _From, State) ->
    Status = post(Text, State),
    {reply, Status, State};
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
    {ok, State#state{logged_in=true, token=Token}}.

post(Post, State) ->
    {ok, Feed} = application:get_env(frfbot, feed),
    FeedBin = list_to_binary(Feed),
    Token = State#state.token,
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
