Caveat emptor: Protean is currently a library for personal learning and exploration, not for doing _Serious Work_. Nothing here, including documentation, should be fully trusted!

![Warning](https://img.shields.io/badge/Warning-Experimental-critical?labelColor=870800&color=d11a0f) [![Hex](https://img.shields.io/badge/protean-v0.0.1-orange)](https://hex.pm/packages/protean) [![Docs](https://img.shields.io/badge/-Docs-informational)](https://hexdocs.pm/protean/)

# Protean

Protean is a library for manage state change and side-effects through the use of finite state machines and statecharts. It is heavily inspired by [XState](https://xstate.js.org/docs/), a JavaScript/TypeScript project that I heavily recommend looking into, and [SCXML](https://www.w3.org/TR/scxml/), a W3C standard developed by smart people over a number of years.

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

## Todo

- [ ] Invoked services
  - [ ] Machines/Interpreters
  - [ ] GenServers (non-Protean)
  - [ ] Autoforward events
  - [x] Tasks
- [ ] Actions
  - [x] Send event to self
  - [x] Send event to service
  - [x] Send event to parent
  - [x] Delayed sends
  - [ ] Spawn in assigns
  - [ ] Raise action (queues event in internal queue)
  - [ ] Respond action
  - [ ] Forward action
  - [ ] Escalate action
  - [ ] Choose action / conditional actions
- [x] Transitions
  - [x] Automatic transitions
  - [x] Delayed transitions (implemented as delayed sends to auto transition states)
  - [x] Correctly cancel non-sent actions when transitioning out
  - [x] Internal vs. external transitions
    - External = exit/entry actions will be re-triggered
    - Internal = exit/entry actions will not be re-triggered, only transition actions
- [x] Parallel states
  - [ ] Correct transition conflict handling
- [ ] History states

## Installation

This package can be found on [hex.pm](https://hex.pm/packages/protean) and can be installed by adding `protean` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:protean, "~> 0.0.1"}
  ]
end
```
