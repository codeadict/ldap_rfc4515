-module(ldap_rfc4515).

-export([to_eldap/1]).

-ifdef(OTP_VERSION).
-if(?OTP_VERSION >= 26).
-type eldap_filter() :: eldap:filter().
-else.
-type eldap_filter() :: [].
-endif.
-else.
-type eldap_filter() :: [].
-endif.

-spec to_eldap(binary() | list()) -> {ok, eldap_filter()} | {error, any()}.
to_eldap(String) when is_binary(String) ->
  to_eldap(binary_to_list(String));
to_eldap(String) when is_list(String) ->
  case tokens(String) of
    {ok, Tokens} ->
      try ldap_rfc4515_parser:parse(Tokens) of
        {ok, Filter} ->
          {ok, Filter};
        {error, {_Line, Module, Error}} ->
          {error, Module:format_error(Error)}
      catch
        _:_ ->
          {error, "syntax error"}
      end;
    {error, Error} ->
      {error, Error}
  end.


tokens(String) -> tokens(String, []).
tokens("", Res) -> {ok, lists:reverse(Res)};
tokens(String, Res) ->
    case ldap_rfc4515_lexer:token([], String) of
        {done, {ok, Token, Line}, Rest} ->
            case is_equal_sign(Token) of
                true ->
                    {Tokens2, Rest2} = parse_value(Rest, Line),
                    tokens(Rest2, Tokens2 ++ [Token | Res]);
                false ->
                    tokens(Rest, [Token | Res])
            end;
        {done, {eof, _}, _Rest} ->
            {error, "unexpected end of file"};
        {done, {error, {_Line, Module, Error}, _}, _} ->
            {error, Module:format_error(Error)};
        {more, _} ->
            {error, "incomplete"}
    end.


is_equal_sign({'~=', _}) -> true;
is_equal_sign({'>=', _}) -> true;
is_equal_sign({'<=', _}) -> true;
is_equal_sign({'=', _}) -> true;
is_equal_sign({':=', _}) -> true;
is_equal_sign({'=*', _}) -> true;
is_equal_sign(_) -> false.

parse_value(Str, Line) ->
  {Str2, Rest} =
    case string:split(Str, ")") of
      [V, R] ->
        {V, [$) | R]};
      [V] ->
        {V, ""}
    end,
  StrTokens = [{str, Line, T} || T <- string:split(Str2, "*", all)],
  Tokens = [T || T <- lists:join({'*', Line}, StrTokens), T =/= {str, Line, ""}],
  {lists:reverse(Tokens), Rest}.
