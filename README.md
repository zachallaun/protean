# Protean

Protean is a library for modeling behaviour and managing side-effects through the use of finite state machines and statecharts. It is heavily inspired by [XState](https://xstate.js.org/docs/), a JavaScript/TypeScript project that I heavily recommend looking into if you work in those languages.

Statecharts are an extension to finite state machines that both allow you to model complex behaviour declaratively and are easily visualized. To learn more about statecharts, I recommend [statecharts.dev](https://statecharts.dev/).

## Goals of Protean

This project's goals are largely focused around me at the moment. Foremost, it allows me to more deeply explore two spaces, Elixir and statecharts, that I am very interested in. XState, this project's primary inspiration, chose the Actor model and message passing to manage running machines and services. Obiously, this should map extremely well to Elixir.

Goals fall into two rough categories:

- Gain a deeper understanding of statechart implementations. In particular, XState and the [SCXML](https://www.w3.org/TR/scxml/) standard.
- Gain a deeper understanding of Elixir, including:
  - Elixir as a language and ecosystem, including macro development, package releases, custom mix tasks, etc.
  - Elixir/Erlang as a platform; how to write abstractions that work well within the existing OTP ecosystem.
  - Idiomatic Elixir library development and existing idioms/techniques that may make this package entirely unnecessary in the first place!

If the result of this project is generally useful, that's great! Usefulness, however, is not a part of my criteria for success.

## Installation (Not currently published)

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `protean` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:protean, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/protean>.

