![Warning](https://img.shields.io/badge/Warning-Experimental-critical?labelColor=870800&color=d11a0f) [![GitHub](https://img.shields.io/badge/GitHub-zachallaun%2Fprotean-orange?logo=github)](https://github.com/zachallaun/protean) [![Docs](https://img.shields.io/badge/-Docs-informational)](https://hexdocs.pm/protean/)

<!-- MDOC !-->

_Caveat emptor: Protean is a library for personal learning and exploration, not (yet) for doing Serious Work_.

An experimental Elixir library for managing state and side-effects through the use of statecharts. It is heavily inspired by [XState](https://xstate.js.org/docs/), a robust JavaScript/TypeScript statechart implementation, but strays in some places in order to adhere to Elixir idioms and OTP conventions. Protean also attempts to follow the [SCXML](https://www.w3.org/TR/scxml/) standard, though not completely.

**What are statecharts?** They are an extension to finite state machines that allow you to model complex behavior in a declarative, data-driven manner. They include nested and parallel states, enhanced/augmented state (through context), side-effects (through actions), process management (through invoke), and more. To learn more about statecharts, I recommend [statecharts.dev](https://statecharts.dev/).

## Goals

This project is currently an exploration of statecharts as they fit into the context of Elixir and OTP. XState adopted the actor model in its implementation, so Elixir seemed like a natural fit. However, it may be that Elixir/OTP makes these abstractions unnecessary.

## Usage

Add `Protean` under your application supervisor. This starts a supervisor that is used by Protean internally to manage subprocesses. See `Protean.start_link/1` for more information.

```elixir
children = [
  Protean,
  # ...
]
```

This simple statechart has a single state that defines the behavior of a counter with an optional maximum and minimum.

```elixir
defmodule Counter do
  use Protean
  alias Protean.Action

  defmachine [
    initial: "active",
    context: [
      count: 0,
      min: nil,
      max: nil
    ],
    states: [
      active: [
        on: [
          INC: [
            actions: ["increment"],
            when: [not: "at_max"]
          ],
          DEC: [
            actions: ["decrement"],
            when: [not: "at_min"]
          ],
          SET: [
            actions: ["set_min_or_max"]
          ],
          LOG: [
            actions: ["log"]
          ]
        ]
      ]
    ]
  ]

  @impl Protean
  def action("increment", state, _event), do: Action.assign_in(state, [:count], & &1 + 1)
  def action("decrement", state, _event), do: Action.assign_in(state, [:count], & &1 - 1)

  def action("set_min_or_max", state, {"SET", {key, val}}) do
    Action.assign(state, key, val)
  end

  def action("log", state, {_, attribute}) do
    %{context: context} = state
    IO.puts("#{attribute}: #{context[attribute]}")

    state
  end

  @impl Protean
  def condition("at_max", %{context: %{max: max, count: count}}, _event) do
    max && count >= max
  end

  def condition("at_min", %{context: %{min: min, count: count}}, _event) do
    min && count <= min
  end
end
```

It can be started under a supervisor, but we'll start it directly.

```elixir
{:ok, pid} = Counter.start_link()

Protean.current(pid).context
# %{count: 0, min: nil, max: nil}

Protean.call(pid, "INC")
# %Protean.State{
#   context: %{count: 1, max: nil, min: nil},
#   event: {"INC", nil},
#   private: %{actions: []},
#   value: #MapSet<[["active", "#"]]>
# }

Enum.each(1..4, fn _ -> Protean.call(pid, "INC") end)

Protean.current(pid).context
# %{count: 5, min: nil, max: nil}

Protean.call(pid, {"SET", {:max, 10}})
# %Protean.State{
#   context: %{count: 5, max: 10, min: nil},
#   event: {"SET", {:max, 10}},
#   private: %{actions: []},
#   value: #MapSet<[["active", "#"]]>
# }

Enum.each(1..20, fn _ -> Protean.call(pid, "INC") end)

Protean.call(pid, {"LOG", :count})
# count: 10
```

<!-- MDOC !-->

## Documentation

Documentation can be found [on hexdocs](https://hexdocs.pm/protean/readme.html). Things are changing pretty regularly, however, and some documentation is certainly out-of-sync.

## Todo

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
  - [x] Choose action
- [ ] Parallel states
  - [x] Basic implementation
  - [ ] Correct transition conflict handling
- [ ] History states
- [ ] Fix all "FIXME"
- [ ] Fix all "TODO"
- [ ] Stricter machine config parsing with helpful error messages

## Installation

This package is under active development and is pre-0.1. If you would like to experiment with it, you can install from git directly:

```elixir
def deps do
  [
    {:protean, git: "https://github.com/zachallaun/protean.git"}
  ]
end
```
