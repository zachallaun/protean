[![Docs](https://img.shields.io/badge/hexdocs.pm-Docs-informational)](https://hexdocs.pm/protean/) [![CI](https://github.com/zachallaun/protean/actions/workflows/ci.yml/badge.svg)](https://github.com/zachallaun/protean/actions/workflows/ci.yml)

<!-- MDOC !-->

_Caveat emptor: Protean started as a library for personal learning and exploration. It should not yet be relied upon._

A library for managing state and side-effects with event-driven statecharts.

Protean is heavily inspired by [XContext](https://xstate.js.org/docs/), a robust JavaScript/TypeScript statechart implementation, but strays to adhere to Elixir idioms and OTP conventions.
Protean also attempts to follow the [SCXML](https://www.w3.org/TR/scxml/) standard, though not completely.

**What are statecharts?**
They are an extension to finite state machines that allow you to model complex behavior in a declarative, data-driven manner.
They include nested and parallel states, enhanced/augmented state (through assigns), side-effects (through actions), process management (through invoke), and more.
To learn more about statecharts, I recommend [statecharts.dev](https://statecharts.dev/).

## Goals

This project is currently an exploration of statecharts as they fit into the assigns of Elixir and OTP.
XContext adopted the actor model in its implementation, so Elixir seemed like a natural fit.
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

  @machine [
    initial: "active",
    assigns: [
      count: 0,
      min: nil,
      max: nil
    ],
    states: [
      active: [
        on: [
          {"Inc", actions: :increment, guard: [not: :at_max]},
          {"Dec", actions: :decrement, guard: [not: :at_min]},
          {match({"Set", _}), actions: :set_min_or_max},
          {match({"Log", _}), actions: :log}
        ]
      ]
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

It can be started under a supervisor, but we'll start it directly.

```elixir
{:ok, pid} = Protean.start_link(Counter)

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

### The `@machine` attribute

By default, Protean assumes that your machine is defined on the `@machine` attribute of the module that called `use Protean`.

```elixir
defmodule MyMachine do
  use Protean

  @machine [
    initial: "my_initial_state",
    states: [
      my_initial_state: [
        # ...
      ],
      # other states
    ]
  ]
end
```

## States

One of the extensions that statecharts make to typical finite-state machines is a notion of hierarchy.
States can contain child states, and the state type determines how those child states are entered or exited.

Protean currently supports four types of states: `:compound`, `:parallel`, `:atomic`, and `:final`.

> #### Note about state names {: .tip}
>
> In order to take advantage of Elixir's keyword list syntax, state names are usually defined as keywords, but they are converted internally to strings.
> Notice above, for instance, that we use `initial: "my_initial_state"` and then `my_initial_state: [` shortly thereafter.

### The `:compound` state

Compound states have children, of which only one can be active at a given time.
They additional define an `:initial` attribute specifying which child should become active if we transition directly to the compound state.

`@machine` points to the _root_, which itself is almost always a compound state:

```elixir
@machine [
  type: :compound,
  initial: "state_a",
  states: [
    state_a: []
  ]
]
```

Because `:compound` is the only state that has an `:initial`, we do not need to explicitly specify the `:type`.
These two examples are equivalent:

```elixir
[
  parent_state: [
    type: :compound,
    initial: "child_a",
    states: [
      child_a: [],
      child_b: []
    ]
  ]
]
# equivalent to
[
  parent_state: [
    initial: "child_a",
    states: [
      child_a: [],
      child_b: []
    ]
  ]
]
```

### The `:parallel` state

Parallel states also have child states, but when a parallel state is entered, all of its children become active concurrently.

Parallel states must be specified using `type: :parallel`.

```elixir
[
  parent_state: [
    type: :parallel,
    states: [
      child_a: [],
      child_b: []
    ]
  ]
]
```

### The `:atomic` state

Atomic states are simple states that cannot define children, but represent some intermediary state of the machine.

Atomic states can be specified with `type: :atomic`, but they are usually inferred.

```elixir
[
  atomic_state: [
    type: :atomic
  ]
]
# equivalent to
[
  atomic_state: []
]
```

### The `:final` state

Final states are a variation of atomic states that represent some form of completion.
These states are most useful in triggering `:done` transitions.
Note that final states cannot define transitions of their own using `:on`.

Final states must be specified with `type: :final`.

```elixir
[
  final_state: [
    type: :final
  ]
]
```

## Event transitions

The most common way to transition from one state to another is in response to an event sent to the machine.
This is done using the `:on` attribute of a state.
This should point to a list of two-element tuples, where the first element matches the event, and the second is a keyword list specifying the transition.

```elixir
[
  state_a: [
    on: [
      {:foo_event, target: "state_b"},
      {:bar_event, target: "state_c"}
    ]
  ]
]
```

### Pattern matching

You can pattern match on events using the automatically-imported `Protean.match/1` macro.

```elixir
[
  state_a: [
    on: [
      {match({:event_with_payload, _payload}), target: "state_b"},
      {match(%Events.OtherEvent{}), target: "state_c"}
    ]
  ]
]
```

This allows Protean machines to match on arbitrary events regardless of how they are sent to the machine.
You could define a catch-all transition, for instance.

```elixir
[
  state_a: [
    {match({:specific_event, _payload}), target: "state_b"},
    {match(_), target: "unknown_event_received"}
  ]
]
```

### Guards

Guards add run-time checks to transitions and are specified using `:guard`.

```elixir
[
  state_a: [
    {:event, target: "state_b", guard: :custom_condition_met?}
  ]
]
```

See `Protean.Guard` and `c:guard/3` for additional details.

### Actions

Actions -- side effects to be performed when a transition occurs -- can be specified using `:actions` inside a transition.

```elixir
[
  state_a: [
    {match(_), actions: [:log_unexpected_event]}
  ]
]
```

See `Protean.Action` and `c:handle_action/3` for additional details.

## Entry and exit actions

In addition to `:actions` specified on a transition, states themselves can specify actions that should be run when that state is entered and exited.

```elixir
[
  state_a: [
    entry: [:my_entry_action],
    exit: [:my_exit_action]
  ]
]
```

See `Protean.Action` and `c:handle_action/3` for additional details.

## Invoked processes

Invoked processes are subprocesses supervised by `Protean` that are started and terminated when the machine enters/exits the state that defines them.

```elixir
[
  runner_state: [
    invoke: [
      task: :some_long_running_task,
      done: [target: "completed", actions: [:save_result]],
      error: [target: "failed", actions: [:log_error]]
    ]
  ]
]
```

Because the `:invoke` above specified a `:task`, Protean will await the return value of the task and then trigger the `:done` transition associated with the invoke.
If the task crashes, the `:error` transition is taken.

In addition to tasks, Protean can invoke `:stream`, which sents messages from the stream to the machine as events, and `:proc`, arbitrary processes (including other machines) that define a supervisor child spec.

If the machine exits the state before an invoked process ends, the process will be exited with a reason of `:normal`.

See `c:invoke/3` for additional details.

## Automatic transitions

TODO: `:always`

See [integration tests](https://github.com/zachallaun/protean/blob/main/test/integration/automatic_transition_test.exs) for examples for now.

## Delayed transitions

TODO: `:after`

See [integration tests](https://github.com/zachallaun/protean/blob/main/test/integration/delayed_transition_test.exs) for examples for now.

## Starting supervised machines

Just like `GenServer`, Protean machines will be most often started under a supervision tree.
Invoking `use Protean` will automatically define a `child_spec/1` function that allows you to start the process directly under a supervisor.

```elixir
children = [
  Counter
]

Supervisor.start_link(children, strategy: :one_for_one)
```

Protean machines also accept the same options as `Protean.start_link/2`.
See those docs for more details.

For instance, here's how you could start the `Counter` with a custom name:

```elixir
children = [
  # Start the Counter machine
  {Counter, name: MyCounter}
]

Supervisor.start_link(children, strategy: :one_for_one)

Protean.current(MyCounter)
# %Protean.Context{
#   assigns: %{count: 0, max: nil, min: nil},
#   event: "$protean.init",
#   value: MapSet.new([["active", "#"]])
# }
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

- [ ] Add `stream` with similar options as `subscribe` so that you can get a stream of `{context, replies}` or just a stream of replies.
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

This package is under active development and is pre-0.1. If you would like to experiment with it, install directly from the repository:

```elixir
def deps do
  [
    {:protean, github: "zachallaun/protean"}
  ]
end
```
