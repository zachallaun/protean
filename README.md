![Warning](https://img.shields.io/badge/Warning-Experimental-critical?labelColor=870800&color=d11a0f) [![GitHub](https://img.shields.io/badge/GitHub-zachallaun%2Fprotean-orange?logo=github)](https://github.com/zachallaun/protean) [![Docs](https://img.shields.io/badge/-Docs-informational)](https://hexdocs.pm/protean/)

<!-- MDOC !-->

_Caveat emptor: Protean is a library for personal learning and exploration, not (yet) for doing Serious Work_.

An experimental Elixir library for managing state and side-effects through the use of event-driven statecharts.
It is heavily inspired by [XState](https://xstate.js.org/docs/), a robust JavaScript/TypeScript statechart implementation, but strays to adhere to Elixir idioms and OTP conventions.
Protean also attempts to follow the [SCXML](https://www.w3.org/TR/scxml/) standard, though not completely.

**What are statecharts?**
They are an extension to finite state machines that allow you to model complex behavior in a declarative, data-driven manner.
They include nested and parallel states, enhanced/augmented state (through context), side-effects (through actions), process management (through invoke), and more.
To learn more about statecharts, I recommend [statecharts.dev](https://statecharts.dev/).

## Goals

This project is currently an exploration of statecharts as they fit into the context of Elixir and OTP.
XState adopted the actor model in its implementation, so Elixir seemed like a natural fit.
However, it may be that Elixir/OTP makes these abstractions unnecessary.

## Example

Add `Protean.Supervisor` under your application supervisor.
This starts a supervisor that is used by Protean internally to manage subprocesses.

```elixir
children = [
  Protean.Supervisor,
  # ...
]
```

This simple statechart has a single state that defines the behavior of a counter with an optional maximum and minimum.

```elixir
defmodule Counter do
  use Protean
  alias Protean.Action

  defmachine(
    initial: "active",
    context: [
      count: 0,
      min: nil,
      max: nil
    ],
    states: [
      active: [
        on: [
          {"Inc", actions: :increment, guard: [not: :at_max]},
          {"Dec", actions: :decrement, guard: [not: :at_min]},
          {{"Set", _}, actions: :set_min_or_max},
          {{"Log", _}, actions: :log}
        ]
      ]
    ]
  )

  @impl Protean
  def action(:increment, state, _event), do: Action.assign_in(state, [:count], & &1 + 1)
  def action(:decrement, state, _event), do: Action.assign_in(state, [:count], & &1 - 1)

  def action(:set_min_or_max, state, {"Set", {key, val}}) do
    state
    |> Action.assign(key, val)
  end

  def action(:log, state, {"Log", attribute}) do
    %{context: context} = state
    IO.puts("#{attribute}: #{context[attribute]}")

    state
  end

  @impl Protean
  def guard(:at_max, %{context: %{max: max, count: count}}, _event) do
    max && count >= max
  end

  def guard(:at_min, %{context: %{min: min, count: count}}, _event) do
    min && count <= min
  end
end
```

It can be started under a supervisor, but we'll start it directly.

```elixir
{:ok, pid} = Protean.start_link(Counter)

Protean.current(pid).context
# %{count: 0, min: nil, max: nil}

Protean.call(pid, "Inc")
# %Protean.State{
#   context: %{count: 1, max: nil, min: nil},
#   event: "Inc",
#   value: MapSet.new([["active", "#"]])
# }

Enum.each(1..4, fn _ -> Protean.call(pid, "Inc") end)

Protean.current(pid).context
# %{count: 5, min: nil, max: nil}

Protean.call(pid, {"Set", {:max, 10}})
# %Protean.State{
#   context: %{count: 5, max: 10, min: nil},
#   event: {"Set", {:max, 10}},
#   value: MapSet.new([["active", "#"]])
# }

Enum.each(1..20, fn _ -> Protean.call(pid, "Inc") end)

Protean.call(pid, {"Log", :count})
# count: 10
```

## Protean Supervisor

Protean uses a `DynamicSupervisor` to manage internally spawned processes (often spawned through the use of `:invoke`).
The simplest thing to do is to add `Protean.Supervisor` in your application supervision tree:

```elixir
def start(_type, _args) do
  children = [
    Protean.Supervisor,
    # ...
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

This will start the supervisor under the name `Protean.Supervisor` and no additional configuration will be required.

If you would like to start multiple supervisors, or a different type of supervisor (like a fancy `PartitionSupervisor`), you can pass the new name as an option when starting a machine.
Here's how that might look using the counter example from before.

```elixir
# in your supervision tree
children = [
  {Protean.Supervisor, name: ProteanSupervisor1},
  {Protean.Supervisor, name: ProteanSupervisor2}
]

# starting the counter
Protean.start_link(Counter, supervisor: ProteanSupervisor1)
```

In the above example, any processes that are spawned by the Protean interpreter running `Counter` will use `ProteanSupervisor1`.

## Starting supervised machines

Just like `GenServer`, Protean machines will be most often started under a supervision tree.
Invoking `use Protean` will automatically define a `child_spec/1` function that allows you to start the process directly under a supervisor.

```elixir
children = [
  Counter
]

Supervisor.start_link(children, strategy: :one_for_one)
```

Protean machines also accept the same options as `Protean.start_link/3`.
See those docs for more details.

For instance, here's how you could start the `Counter` with a custom supervisor and name:

```elixir
children = [
  # Start the Protean Supervisor
  {Protean.Supervisor, name: MyProteanSupervisor},

  # Start the Counter machine
  {Counter, name: MyCounter, supervisor: MyProteanSupervisor}
]

Supervisor.start_link(children, strategy: :one_for_one)

Protean.current(MyCounter)
# %Protean.State{
#   context: %{count: 0, max: nil, min: nil},
#   event: "$protean.init",
#   value: MapSet.new([["active", "#"]])
# }
```

<!-- MDOC !-->

## Documentation

Documentation can be found [on hexdocs](https://hexdocs.pm/protean/readme.html).
Things are changing pretty regularly, however, and some documentation is certainly out-of-sync.

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
