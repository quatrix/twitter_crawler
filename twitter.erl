#!/usr/bin/env escript

-module(twitter_crawler).
-import(jiffy, [decode/1]).
-import(ej, [get/2]).

api_url(URL) ->
    "https://api.twitter.com/" ++ URL.

call_twitter(Request) ->
    {Method, Params} = Request,

    R = httpc:request(
        Method,
        Params,
        [{ssl,[{verify,0}]}],
        []
    ),

    {ok, {{"HTTP/1.1", 200, "OK"}, _, Body}} = R,
    jiffy:decode(Body).

call_twitter(get, URL, Key) ->
    call_twitter({get, {api_url(URL), [{"Authorization", Key}]}}).

call_twitter(post, URL, Key, Type, Data) ->
    call_twitter({post, {api_url(URL), [{"Authorization", Key}], Type, Data}}).


get_bearer_token(APIKey, APISecret) ->
    Res = call_twitter(
        post,
        "oauth2/token",
        create_basic_key(APIKey, APISecret),
        "application/x-www-form-urlencoded;charset=UTF-8",
        "grant_type=client_credentials"
    ),

    "Bearer " ++ binary_to_list(ej:get({"access_token"}, Res)).


replace_non_url_chars(String) ->
    replace_non_url_chars(String, []).

replace_non_url_chars([], Result) ->
    Result;


replace_non_url_chars([Head|Tail], Result) ->
    case Head of
        $+ ->
            replace_non_url_chars(Tail, Result ++ ["-"]);
        $/ ->
            replace_non_url_chars(Tail, Result ++ ["_"]);
        _ ->
            replace_non_url_chars(Tail, Result ++ [Head])
    end.

base64url_encode(Input) ->
    replace_non_url_chars(binary_to_list(base64:encode(Input))).

create_basic_key(APIKey, APISecret) ->
    "Basic " ++ base64url_encode(APIKey ++ ":" ++ APISecret).

print_friends([]) ->
    ok;

print_friends([Friend|Friends]) ->
    io:format("~p~n", [get_key("screen_name", Friend)]),
    print_friends(Friends).

get_key(Key, Obj) ->
    binary_to_list(ej:get({Key}, Obj)).

get_secret() ->
    {ok, Secret} = file:read_file("twitter.secret"),
    DecodedSecret = jiffy:decode(Secret),

    {get_key("APIKey", DecodedSecret), get_key("APISecret", DecodedSecret)}.

main(_) ->
    inets:start(),
    ssl:start(),

    {APIKey, APISecret} = get_secret(),

    Res = call_twitter(
        get,
        "1.1/friends/list.json?count=10&screen_name=quatrix",
        get_bearer_token(APIKey, APISecret)
    ),
        
    print_friends(ej:get({"users"}, Res)).
