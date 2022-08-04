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

Protean.send(pid, "Inc")
# :ok

Enum.each(1..4, fn _ -> Protean.send(pid, "Inc") end)

Protean.current(pid).context
# %{count: 5, min: nil, max: nil}

Protean.call(pid, {"Set", {:max, 10}})
# %Protean.State{
#   context: %{count: 5, max: 10, min: nil},
#   event: {"Set", {:max, 10}},
#   value: MapSet.new([["active", "#"]])
# }

Enum.each(1..20, fn _ -> Protean.send(pid, "Inc") end)

Protean.send(pid, {"Log", :count})
# count: 10
```

## Defining a statechart

Protean machines are event-driven _statecharts_, which means that, unlike ordinary finite-state machines, they can have complex, nested, potentially parallel states.
This is more easily visualized than read, and I highly recommend looking at XState's [introduction to state machines and statecharts](https://xstate.js.org/docs/guides/introduction-to-state-machines-and-statecharts/) for that reason.

### TODO: States
### TODO: Transitions
### TODO: Guards and automatic transitions
### TODO: Actions
### TODO: Invoked processes

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

For instance, here's how you could start the `Counter` with a custom name:

```elixir
children = [
  # Start the Counter machine
  {Counter, name: MyCounter}
]

Supervisor.start_link(children, strategy: :one_for_one)

Protean.current(MyCounter)
# %Protean.State{
#   context: %{count: 0, max: nil, min: nil},
#   event: "$protean.init",
#   value: MapSet.new([["active", "#"]])
# }
```

## Interacting with Protean machines

Under the hood, a Protean machine is a `GenServer`, and `Protean` exposes a similar set of functions for interacting with one.
You can see the individual docs for the functions in this module for details on their behavior, but here are some highlights.

### Familiar functions

* `call/3` - Send an event synchronously to a Protean machine and receive the machine state resulting from any transitions in response.
* `send/2` - Send an event asynchronously to a Protean machine. Always returns `:ok`.
* `send_after/3` - Send an event to a Protean machine after a given delay. Like `Process.send_after/4`, returns a timer reference so that the send can be canceled with `Process.cancel_timer/2`.

### Additional functions specific to Protean machines

* `current/1` - Get the current machine state of a running Protean machine.
* `matches?/2` - Query the currently active state(s) of a machine.
* `ask/3` - Like `call/3`, but potentially returns an "answer" value in addition to the machine state.
* `subscribe/2` (and `unsubscribe/2`) - Subscribes the calling process to receive a message on every state transition.

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

<!-- MDOC !-->

## Documentation

Documentation can be found [on hexdocs](https://hexdocs.pm/protean/).
Things are changing pretty regularly, however, and some documentation is certainly out-of-sync.

## Todo

- [ ] Actions
  - [ ] Spawn in assigns
  - [ ] Raise action (queues event in internal queue)
  - [ ] Respond action
  - [ ] Forward action
  - [ ] Escalate action
- [ ] Parallel states
  - [ ] Correct transition conflict handling
- [ ] History states
- [ ] Fix all "FIXME"
- [ ] Fix all "TODO"
- [ ] Stricter machine config parsing with helpful error messages
- [ ] Differentiate between internal and external events (internal events should not trigger catch-all )

## Installation

This package is under active development and is pre-0.1. If you would like to experiment with it, you can install from git directly:

```elixir
def deps do
  [
    {:protean, git: "https://github.com/zachallaun/protean.git"}
  ]
end
```
