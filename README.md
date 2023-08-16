# LDAP RFC4515 parser for Erlang/Elixir

[![CI Tests](https://github.com/codeadict/ldap_rfc4515/actions/workflows/erlang.yml/badge.svg)](https://github.com/codeadict/ldap_rfc4515/actions/workflows/erlang.yml)

## Installation

### Erlang

In your `rebar.config`:

```erlang
{deps, [
  {ldap_rfc4515, "0.1.0"}
]}.
```

If using Erlang.mk:

In your `Makefile`

```makefile
DEPS += ldap_rfc4515
dep_ldap_rfc4515 = ldap_rfc4515 0.1.0
```

### Elixir

In your `mix.exs`:

```elixir
def deps do
  [{:ldap_rfc4515, "~> 0.1.0"}]
end
```

## Development

### Build

```shell
rebar3 compile
```

### Tests

```shell
rebar3 xref
rebar3 eunit
rebar3 as test dialyzer
```

