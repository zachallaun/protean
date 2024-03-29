[![Docs](https://img.shields.io/badge/hex.pm-docs-8e7ce6.svg)](https://hexdocs.pm/protean/0.1.0-alpha.3/Protean.html)
[![CI](https://github.com/zachallaun/protean/actions/workflows/ci.yml/badge.svg)](https://github.com/zachallaun/protean/actions/workflows/ci.yml)

<!-- MDOC !-->

_Caveat emptor: Protean is likely to undergo significant changes and should not be considered stable._

A library for managing state and side-effects with event-driven statecharts.

Protean was initially inspired by [XState](https://xstate.js.org/docs/), a robust JavaScript/TypeScript statechart implementation, but strays to adhere to Elixir and OTP conventions.
We also follow much of the [SCXML W3C Standard](https://www.w3.org/TR/scxml/)'s recommendations, but compatibility is not a goal.

**What are statecharts?**
They are an extension to finite state machines that allow you to model complex behavior in a declarative, data-driven manner.
They include nested and parallel states, enhanced/augmented state (through assigns), side-effects (through actions), process management (through spawn), and more.
To learn more about statecharts, I recommend [statecharts.dev](https://statecharts.dev/).

## Goals

This project is currently an exploration of statecharts as they fit into the assigns of Elixir and OTP.
XState adopted the actor model in its implementation, so Elixir seemed like a natural fit.
However, it may be that Elixir/OTP makes these abstractions unnecessary.

## Example

This simple statechart has a single state that defines the behavior of a counter with an optional maximum and minimum.

```elixir
defmodule Counter do
  use Protean
  alias Protean.Action

  @machine [
    initial: :active,
    assigns: [
      count: 0,
      min: nil,
      max: nil
    ],
    states: [
      atomic(:active,
        on: [
          match("Inc", actions: :increment, guard: [not: :at_max]),
          match("Dec", actions: :decrement, guard: [not: :at_min]),
          match({"Set", _}, actions: :set_min_or_max),
          match({"Log", _}, actions: :log)
        ]
      )
    ]
  ]

  @impl true
  def handle_action(:increment, context, _event) do
    context
    |> Action.assign_in([:count], & &1 + 1)
  end

  def handle_action(:decrement, context, _event) do
    context
    |> Action.assign_in([:count], & &1 - 1)
  end

  def handle_action(:set_min_or_max, context, {"Set", {key, val}}) do
    context
    |> Action.assign(key, val)
  end

  def handle_action(:log, context, {"Log", attribute}) do
    %{assigns: assigns} = context
    IO.puts("#{attribute}: #{assigns[attribute]}")

    context
  end

  @impl true
  def guard(:at_max, %{assigns: %{max: max, count: count}}, _event) do
    max && count >= max
  end

  def guard(:at_min, %{assigns: %{min: min, count: count}}, _event) do
    min && count <= min
  end
end
```

It can be started under a supervisor, but we'll start it directly using Protean's built-in `DynamicSupervisor`.

```elixir
{:ok, pid} = Protean.start_machine(Counter)

Protean.current(pid).assigns
# %{count: 0, min: nil, max: nil}

Protean.send(pid, "Inc")
# :ok

Enum.each(1..4, fn _ -> Protean.send(pid, "Inc") end)

Protean.current(pid).assigns
# %{count: 5, min: nil, max: nil}

Protean.call(pid, {"Set", {:max, 10}})
# %Protean.Context{
#   assigns: %{count: 5, max: 10, min: nil},
#   event: {"Set", {:max, 10}},
#   value: MapSet.new([["active", "#"]])
# }

Enum.each(1..20, fn _ -> Protean.send(pid, "Inc") end)

Protean.send(pid, {"Log", :count})
# count: 10
```

## Defining a statechart

Protean machines are event-driven _statecharts_, which means that, unlike ordinary finite-state machines, they can have complex, nested, potentially parallel states.
This is more easily visualized than read, and I highly recommend looking at XContext's [introduction to state machines and statecharts](https://xstate.js.org/docs/guides/introduction-to-state-machines-and-statecharts/) for that reason.

Refer to `Protean.Builder` for documentation on machine definitions.
When `use Protean` is invoked, functions and macros from `Protean.Builder` are imported automatically.

### The `@machine` attribute

By default, Protean assumes that your machine is defined on the `@machine` attribute.

```elixir
defmodule MyMachine do
  use Protean

  @machine [
    initial: :my_initial_state,
    states: [
      atomic(:my_initial_state,
        # ...
      ),
      # ...
    ]
  ]
end
```

## Starting supervised machines

Since state machines typically model structured interactions with a defined beginning and end, they will generally be started under a `DynamicSupervisor`.
Protean starts one (as well as a `Registry`) by default, in order to manage subprocesses that are started during machine execution through the use of `Protean.Builder.proc/2` et al.

Machines can be started under this supervisor using `Protean.start_machine/2`.

```elixir
{:ok, machine} = Protean.start_machine(MyMachine)
```

Similar to `GenServer`, calling `use Protean` will also define a `child_spec/1` that allows you to start a machine in a standard supervision tree, if you wish:

```elixir
children = [
  Counter
]

Supervisor.start_link(children, strategy: :one_for_one)
```

## Interacting with Protean machines

Under the hood, a Protean machine is a `GenServer`, and `Protean` exposes a similar set of functions for interacting with one.
You can see the individual docs for the functions in this module for details on their behavior, but here are some highlights.

### Familiar functions

* `call/3` - Send an event synchronously to a Protean machine and receive the machine context and any replies resulting from transition.
* `send/2` - Send an event asynchronously to a Protean machine. Always returns `:ok`.
* `send_after/3` - Send an event to a Protean machine after a given delay. Like `Process.send_after/4`, returns a timer reference so that the send can be canceled with `Process.cancel_timer/2`.

### Additional functions specific to Protean machines

* `current/1` - Get the current machine context of a running Protean machine.
* `matches?/2` - Query the currently active state(s) of a machine.
* `subscribe/2` (and `unsubscribe/2`) - Subscribes the calling process to receive a message on every state transition.

## Subscriptions

You can subscribe to a Protean machine to receive messages when the machine transitions.
This functionality depends on the optional dependency `:phoenix_pubsub`.
To use it, add the following to `deps` in your `mix.exs`:

```elixir
defp deps do
  [
    :phoenix_pubsub
    # ...
  ]
end
```

If you are already starting a `Phoenix.PubSub` in your application (e.g. a Phoenix application), you need to configure the `:protean` application to use your process instead of starting its own.
This can be done by adding the following to your `config.exs`:

```elixir
config :protean, :pubsub,
  name: MyApp.PubSub,
  start: false
```

For subscription usage, see `subscribe/2`.

<!-- MDOC !-->

## Documentation

Documentation can be found [on hexdocs](https://hexdocs.pm/protean/).
Things are changing pretty regularly, however, and some documentation is certainly out-of-sync.

## Installation

```elixir
def deps do
  [
    {:protean, "~> 0.1.0"}
  ]
end
```
