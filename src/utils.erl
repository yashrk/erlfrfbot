-module(utils).
-export([russian_month/1, post_current_date/0]).

russian_month(1)->
    <<"января"/utf8>>;
russian_month(2)->
    <<"февраля"/utf8>>;
russian_month(3)->
    <<"марта"/utf8>>;
russian_month(4)->
    <<"апреля"/utf8>>;
russian_month(5)->
    <<"мая"/utf8>>;
russian_month(6)->
    <<"июня"/utf8>>;
russian_month(7)->
    <<"июля"/utf8>>;
russian_month(8)->
    <<"августа"/utf8>>;
russian_month(9)->
    <<"сентября"/utf8>>;
russian_month(10)->
    <<"октября"/utf8>>;
russian_month(11)->
    <<"ноября"/utf8>>;
russian_month(12)->
    <<"декабря"/utf8>>.

post_current_date() ->
    {_Year, Month, Day} = date(),
    DayBin = unicode:characters_to_binary(io_lib:format("~tp ", [Day])),
    MonthBin = russian_month(Month),
    RussianDate = <<DayBin/binary, MonthBin/binary>>,
    io:format("~tp~n", [RussianDate]),
    frfbot:post(RussianDate).
