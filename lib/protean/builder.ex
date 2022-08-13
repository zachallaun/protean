defmodule Protean.Builder do
  @moduledoc """
  API for defining Protean machines.

  This module is imported by default when `use Protean` is invoked.

  ## Defining a machine

  At the outermost level, machines are specified as a keyword list, usually associated with the
  `@machine` module attribute of the defining module.

      @machine [
        states: [
          # ...
        ]
      ]

  For the most part, a machine definition is similar to a compound or parallel state definition,
  except that it allows for an addition `:assigns` option to specify the default assigns for the
  machine context.

      @machine [
        assigns: %{
          # ...
        },
        # ...
      ]

  The top-level machine can be parallel by specifying `type: :parallel`:

      @machine [
        type: :parallel,
        states: [
          # ...
        ]
      ]

  See `compound/2` and `parallel/2` for corresponding options.
  """

  alias Protean.Action
  alias Protean.Guard

  @type machine_options :: [compound_machine_option] | [parallel_machine_option]

  @type compound_machine_option ::
          {:assigns, assigns}
          | compound_state_option

  @type parallel_machine_option ::
          {:type, :parallel}
          | {:assigns, assigns}
          | parallel_machine_option

  @typedoc """
  Additional state stored in the machine context.

  The specified assigns will be converted to a `map/0`.
  """
  @type assigns :: Enumerable.t()

  @type final_state_options :: [final_state_option]
  @type final_state_option ::
          {:entry, actions}
          | {:exit, actions}

  @type atomic_state_options :: [atomic_state_option]
  @type atomic_state_option ::
          {:invoke, invokes}
          | {:entry, actions}
          | {:exit, actions}
          | {:always, transitions}
          | {:after, delayed_transitions}
          | {:on, event_transitions}

  @type compound_state_options :: [compound_state_option]
  @type compound_state_option ::
          atomic_state_option
          | {:initial, state_name}
          | {:states, states}
          | {:done, transitions}

  @type parallel_state_options :: [parallel_state_option]
  @type parallel_state_option ::
          atomic_state_option
          | {:states, states}
          | {:done, transitions}

  @type state_name :: atom() | String.t()
  @type states :: [state]
  @type state :: {state_name, keyword()}

  @type invokes :: keyword() | [keyword()]

  @type invoke_options :: [invoke_option]
  @type invoke_option ::
          {:id, String.t()}
          | {:done, transitions}
          | {:error, transitions}
          | {:autoforward, boolean()}

  @type actions :: action | [action]
  @type action :: Action.t() | term()

  @type transitions :: [transition]
  @type transition :: [transition_option]

  @type delayed_transitions :: [delayed_transition]
  @type delayed_transition :: [delayed_transition_option]
  @type delayed_transition_option ::
          transition_option
          | {:delay, milliseconds :: non_neg_integer()}

  @type event_transitions :: [event_transition]
  @type event_transition :: {matcher :: function() | term(), transition}

  @type transition_options :: [transition_option]
  @type transition_option ::
          {:target, state_name}
          | {:actions, actions}
          | {:guard, Guard.t()}

  @doc """
  Builds an atomic state.

  Atomic states are simple states that cannot define children, but represent some intermediary
  state of the machine.

      states: [
        atomic(:loading,
          # ...
        )
      ]

  ## Options

    * `:invoke` - list of processes to invoke, see `invoked/3`;
    * `:entry` - actions to execute when entering this state;
    * `:exit` - actions to execute when exiting this state;
    * `:always` - transitions to immediately take when their guard is true, see `transition/1`;
    * `:after` - transitions to take after a given delay, see `delay/2`;
    * `:on` - transitions to take in response to an event, see `match/2`.

  """
  @spec atomic(state_name, atomic_state_options) :: state
  def atomic(name, opts \\ []) do
    {name, Keyword.put(opts, :type, :atomic)}
  end

  @doc """
  Builds a final state.

  Final states are a variation of atomic states that represent some form of completion. Final
  states cannot define transitions of their own, but entering a final state can trigger a
  transition in a compound or parallel parent. See `compound/2` and `parallel/2`.

      states: [
        final(:completed)
      ]

  ## Options

    * `:entry` - actions to execute when entering this state;
    * `:exit` - actions to execute when exiting this state (as a result of a parent transition).

  """
  @spec final(state_name, final_state_options) :: state
  def final(name, opts \\ []) do
    {name, Keyword.put(opts, :type, :final)}
  end

  @doc """
  Builds a compound state.

  Compound states have children defined by a `:states` list, of which only one will be active at
  a given time. They additional define an `:initial` attribute specifying which child should
  become active if we transition directly to the compound state.

      states: [
        compound(:parent,
          initial: :child_a,
          states: [
            atomic(:child_a)
          ]
        )
      ]

  Compound states can define a `:done` transition that will be taken if one of its `final`
  children become active. In the example below, the `:parent` state will transition to its
  sibling if the final `:child_b` state is entered.

      states: [
        compound(:parent,
          initial: :child_a,
          done: transition(target: :sibling),
          states: [
            atomic(:child_a,
              # ...
            ),
            final(:child_b)
          ]
        )
      ]

  ## Options

    * `:initial` (required) - child state to enter when entering the compound state;
    * `:states` (required) - one or more child states;
    * `:done` - transition to take if a `final` child state is entered;
    * all options available to `atomic/2`.

  """
  @spec compound(state_name, compound_state_options) :: state
  def compound(name, opts) do
    {name, Keyword.put(opts, :type, :compound)}
  end

  @doc """
  Builds a parallel state.

  Parallel states have child states defined by a `:states` list, all of which will be considered
  active concurrently when the parallel state is active.

      states: [
        parallel(:parent,
          states: [
            atomic(:child_a,
              entry: :child_a_action
            ),
            atomic(:child_b,
              entry: :child_b_action
            )
          ]
        )
      ]

  In the example above, transitioning to `:parent` would enter both child states and cause both
  of their entry actions to execute.

  Parallel states can define a `:done` transition that will be taken when all of its children
  are in a final state. Usually, this means the parallel state's children are compound states
  with active final children.

      states: [
        parallel(:parent,
          done: transition(target: :sibling),
          states: [
            compound(:compound_a,
              states: [
                atomic(:a_child1),
                final(:a_child2)
              ]
            ),
            compound(:compound_b,
              states: [
                atomic(:b_child1),
                final(:b_child2)
              ]
            )
          ]
        )
      ]

  In the example above, the parent parallel state will transition to its sibling once both
  compound states have active final children.

  ## Options

    * `:states` (required) - one or more child states, all of which will be concurrently entered
      when the parallel state becomes active;
    * `:done` - transition to take when all children are in a final state.
    * all options available to `atomic/2`.

  """
  @spec parallel(state_name, parallel_state_options) :: state
  def parallel(name, opts) do
    {name, Keyword.put(opts, :type, :parallel)}
  end

  @doc """
  Builds a transition.

  ## Options

    * `:target` - the target state of the transition;
    * `:actions` - one or more actions that should be executed when the transition occurs;
    * `:guard` - condition that must be true in order for the transition to occur.

  ## Guards

  See `Protean.Guard`
  TODO
  """
  @spec transition(transition_options) :: keyword()
  def transition(opts) do
    opts
  end

  @doc """
  Builds a pattern-matching event transition.

  Accepts the same options as `transition/1`.

  ## Example

      on: [
        match({:event_with_payload, _payload}, action: :save_payload),
        match(%Events.OtherEvent{}, target: :other)
      ]

  """
  defmacro match(pattern, opts) do
    {
      quote(do: fn expr -> match?(unquote(pattern), expr) end),
      opts
    }
  end

  @doc """
  Builds a delayed transition.

  Delayed transitions run automatically after the given delay so long as the machine is still in
  the state that defined it and any given guard allows it.

  Accepts the same options as `transition/1`.
  """
  @spec delay(milliseconds :: non_neg_integer() | term(), transition_options) :: keyword()
  def delay(ms, opts) do
    Keyword.put(opts, :delay, ms)
  end

  @doc """
  Builds an invoked process.

  Invoked processes are subprocesses supervised by Protean that are started when the machine
  enters the state that defines them and exited when the machine exits that state.

  ## Invoke types

  `invoke_type` determines how the process will be started and the kind of interaction it will
  have with the machine. It can be one of three values:

    * `:task` - an asynchronous task that is expected to return a single value, after which any
      specified `:done` transition for the invoke will run. The return value will be available
      as the transition event. The `:error` transition will run if the task crashes.
    * `:proc` - an arbitrary process (including other machines). The `:done` transition is run if
      the process exits normally. The `:error` transition is run if the process crashes or exits
      abnormally (with a reason other than `:normal`, `:shutdown`, or `{:shutdown, term()}`).
    * `:stream` - an event stream that will be consumed with each element of the stream being
      sent to the machine as an event. The `:done` transition is run after the stream is fully
      consumed. The `:error` transition is run if the stream crashes.

  ## Example

      invoke: [
        invoked(:task, :some_long_running_task,
          done: transition(target: :completed, actions: :save_result),
          error: transition(target: :failed, actions: :log_error)
        )
      ]

  ## Options

    * `:id`
    * `:done`
    * `:error`
    * `:autoforward`

  """
  @spec invoked(:task | :spec | :proc, term(), invoke_options) :: keyword()
  def invoked(invoke_type, invoke_spec, opts \\ []) do
    Keyword.merge(opts, [{invoke_type, invoke_spec}])
  end
end
