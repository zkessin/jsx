%% The MIT License

%% Copyright (c) 2010-2013 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(jsx_to_term).

-export([to_term/2]).
-export([init/1, handle_event/2]).


-record(config, {
    labels = binary
}).

-type config() :: list().

-type json_value() :: list({binary(), json_value()})
    | list(json_value())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().


-spec to_term(Source::binary(), Config::config()) -> json_value().

to_term(Source, Config) when is_list(Config) ->
    (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source).


parse_config(Config) -> parse_config(Config, #config{}).

parse_config([{labels, Val}|Rest], Config)
        when Val == binary; Val == atom; Val == existing_atom; Val == attempt_atom ->
    parse_config(Rest, Config#config{labels = Val});
parse_config([labels|Rest], Config) ->
    parse_config(Rest, Config#config{labels = binary});
parse_config([{K, _}|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([K|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config)
        ; false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([], Config) ->
    Config.


% internal state of the handler is a record with three fields:
%   stack
%   acc
%   config
%
% state is a list of atoms used by a simple pda to keep track of what
%   state it is currently in
% acc is the current object/list in media res
% config is the config, duh

-record(state, {
    stack = [value],
    acc = [],
    config
}).


init(Config) -> #state{config = parse_config(Config)}.

handle_event(Event, State = #state{stack = []}) ->
    done(Event, State);
handle_event(Event, State = #state{stack = [Next|Stack]}) ->
    case Next of
        key -> key(Event, State#state{stack = Stack});
        value -> value(Event, State#state{stack = Stack});
        maybe_done -> maybe_done(Event, State#state{stack = Stack})
    end.


key(Key, State = #state{stack = Stack, acc = Acc, config = Config}) ->
    State#state{stack = [value] ++ Stack, acc = acc({key, format_key(Key, Config)}, Acc)}.

value(start_object, State = #state{stack = Stack, acc = Acc}) ->
    State#state{stack = [maybe_done, object] ++ Stack, acc = [[]] ++ Acc};
value(start_array, State = #state{stack = Stack, acc = Acc}) ->
    State#state{stack = [maybe_done, list] ++ Stack, acc = [[]] ++ Acc};
value(Event, State = #state{stack = Stack, acc = Acc}) ->
    State#state{stack = [maybe_done] ++ Stack, acc = acc(Event, Acc)}.

maybe_done(end_object, State = #state{stack = [object|Stack], acc = [[]|Acc]}) ->
    State#state{stack = [maybe_done] ++ Stack, acc = acc([{}], Acc)};
maybe_done(end_object, State = #state{stack = [object|Stack], acc = [Object|Acc]}) ->
    State#state{stack = [maybe_done] ++ Stack, acc = acc(lists:reverse(Object), Acc)};
maybe_done(end_array, State = #state{stack = [list|Stack], acc = [List|Acc]}) ->
    State#state{stack = [maybe_done] ++ Stack, acc = acc(lists:reverse(List), Acc)};
maybe_done(Event, State = #state{stack = [object|Stack]}) ->
    handle_event(Event, State#state{stack = [key, object] ++ Stack});
maybe_done(Event, State = #state{stack = [list|Stack]}) ->
    handle_event(Event, State#state{stack = [value, list] ++ Stack});
maybe_done(Event, State) -> done(Event, State).

done(end_json, #state{acc = [Result]}) -> Result.


format_key(Key, Config) ->
    case Config#config.labels of
        binary -> Key
        ; atom -> binary_to_atom(Key, utf8)
        ; existing_atom -> binary_to_existing_atom(Key, utf8)
        ; attempt_atom ->
            try binary_to_existing_atom(Key, utf8) of
                Result -> Result
            catch
                error:badarg -> Key
            end
    end.


acc(Event, []) -> [Event];
acc(Event, [[{key, Key}|Object]|Acc]) -> [[{Key, Event}] ++ Object] ++ Acc;
acc(Event, [List|Acc]) -> [[Event] ++ List] ++ Acc.



%% eunit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"implicit binary labels", ?_assertEqual(#config{}, parse_config([labels]))},
        {"binary labels", ?_assertEqual(#config{}, parse_config([{labels, binary}]))},
        {"atom labels", ?_assertEqual(#config{labels=atom}, parse_config([{labels, atom}]))},
        {"existing atom labels", ?_assertEqual(
            #config{labels=existing_atom},
            parse_config([{labels, existing_atom}])
        )},
        {"sloppy existing atom labels", ?_assertEqual(
            #config{labels=attempt_atom},
            parse_config([{labels, attempt_atom}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


format_key_test_() ->
    [
        {"binary key", ?_assertEqual(<<"key">>, format_key(<<"key">>, #config{labels=binary}))},
        {"atom key", ?_assertEqual(key, format_key(<<"key">>, #config{labels=atom}))},
        {"existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=existing_atom})
        )},
        {"nonexisting atom key", ?_assertError(
            badarg,
            format_key(<<"nonexistentatom">>, #config{labels=existing_atom})
        )},
        {"sloppy existing atom key", ?_assertEqual(
            key,
            format_key(<<"key">>, #config{labels=attempt_atom})
        )},
        {"nonexisting atom key", ?_assertEqual(
            <<"nonexistentatom">>,
            format_key(<<"nonexistentatom">>, #config{labels=attempt_atom})
        )}
    ].


handle_event_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                Term,
                lists:foldl(fun handle_event/2, init([]), Events ++ [end_json])
            )
        } || {Title, _, Term, Events} <- Data
    ].


-endif.
