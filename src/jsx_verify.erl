%% The MIT License

%% Copyright (c) 2010-2013 alisdair sullivan <alisdairsullivan@yahoo.ca>

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


-module(jsx_verify).

-export([is_json/2, is_term/2]).
-export([init/1, handle_event/2]).


-record(config, {
    repeated_keys = true
}).

-type config() :: [].


-spec is_json(Source::binary(), Config::config()) -> true | false.

is_json(Source, Config) when is_list(Config) ->
    try (jsx:decoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source)
    catch error:badarg -> false
    end.


-spec is_term(Source::any(), Config::config()) -> true | false.

is_term(Source, Config) when is_list(Config) ->
    try (jsx:encoder(?MODULE, Config, jsx_config:extract_config(Config)))(Source)
    catch error:badarg -> false
    end.


parse_config(Config) -> parse_config(Config, #config{}).

parse_config([no_repeated_keys|Rest], Config) ->
    parse_config(Rest, Config#config{repeated_keys=false});
%% deprecated, use `no_repeated_keys`
parse_config([{repeated_keys, Val}|Rest], Config) when Val == true; Val == false ->
    parse_config(Rest, Config#config{repeated_keys=Val});
parse_config([repeated_keys|Rest], Config) ->
    parse_config(Rest, Config#config{repeated_keys=true});
parse_config([{K, _}|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config);
        false -> erlang:error(badarg, [Options, Config])
    end;
parse_config([K|Rest] = Options, Config) ->
    case lists:member(K, jsx_config:valid_flags()) of
        true -> parse_config(Rest, Config);
        false -> erlang:error(badarg, [Options, Config])
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
% keys is a stack of keys already used in the current context
% config is the config, duh

-record(state, {
    stack = [],
    keys = [],
    config
}).


init(Config) -> #state{config = parse_config(Config)}.


% reaching `end_json` means everything is good so return `true`
handle_event(end_json, _) -> true;

% if repeated keys aren't being checked for do nothing, basically
handle_event(_, State = #state{config = Config}) when Config#config.repeated_keys == true -> State;

% for each object we push a dict onto a stack to contain seen keys
handle_event(start_object, State = #state{stack = Stack, keys = Keys}) ->
    State#state{stack = [key] ++ Stack, keys = [dict:new()] ++ Keys};
% pop it off the stack when the object ends
handle_event(end_object, State = #state{stack = [key|Stack], keys = [_|Keys]}) ->
    State#state{stack = Stack, keys = Keys};

% no checking for keys/values in arrays
handle_event(start_array, State = #state{stack = Stack}) ->
    State#state{stack = [array] ++ Stack};
% push `key` back on the stack if we're exiting a value context
handle_event(end_array, State = #state{stack = [array, value|Stack]}) ->
    State#state{stack = [key] ++ Stack};
handle_event(end_array, State = #state{stack = [array|Stack]}) ->
    State#state{stack = Stack};
handle_event(_, State = #state{stack = [array|_]}) -> State;

% read a key, check that it's unused then push `value` on the stack so only the last clause
%   matches
handle_event(Key, State = #state{stack = [key|Stack], keys = [CurrentKeys|Keys]}) ->
    case dict:is_key(Key, CurrentKeys) of
        true -> erlang:error(badarg);
        false -> State#state{stack = [value] ++ Stack, keys = [dict:store(Key, blah, CurrentKeys)|Keys]}
    end;

% push `key` on the stack so only `end_object` or an appropriate value matches
handle_event(_, State = #state{stack = [value|Stack]}) ->
    State#state{stack = [key] ++ Stack}.



%% eunit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


config_test_() ->
    [
        {"empty config", ?_assertEqual(#config{}, parse_config([]))},
        {"no repeat keys", ?_assertEqual(#config{repeated_keys=false}, parse_config([no_repeated_keys]))},
        {"bare repeated keys", ?_assertEqual(#config{}, parse_config([repeated_keys]))},
        {"repeated keys true", ?_assertEqual(
            #config{},
            parse_config([{repeated_keys, true}])
        )},
        {"repeated keys false", ?_assertEqual(
            #config{repeated_keys=false},
            parse_config([{repeated_keys, false}])
        )},
        {"invalid opt flag", ?_assertError(badarg, parse_config([error]))},
        {"invalid opt tuple", ?_assertError(badarg, parse_config([{error, true}]))}
    ].


repeated_keys_test_() ->
    RepeatedKey = [
        start_object,
            <<"alpha">>,
            true,
            <<"alpha">>,
            false,
        end_object,
        end_json
    ],
    NestedKey = [
        start_object,
            <<"alpha">>,
            start_object,
                <<"alpha">>,
                start_object,
                    <<"alpha">>,
                    <<"alpha">>,
                end_object,
            end_object,
        end_object,
        end_json
    ],
    [
        {"repeated key", ?_assert(
            lists:foldl(fun handle_event/2, init([]), RepeatedKey)
        )},
        {"no repeated key", ?_assertError(
            badarg,
            lists:foldl(fun handle_event/2, init([no_repeated_keys]), RepeatedKey)
        )},
        {"nested key", ?_assert(
            lists:foldl(fun handle_event/2, init([no_repeated_keys]), NestedKey)
        )}
    ].


handle_event_test_() ->
    Data = jsx:test_cases(),
    [
        {
            Title, ?_assertEqual(
                true,
                lists:foldl(fun handle_event/2, init([]), Events ++ [end_json])
            )
        } || {Title, _, _, Events} <- Data
    ].


-endif.