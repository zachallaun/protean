defmodule Protean.Action do
  @moduledoc """
  Protean manages state, processes, and side-effects through the execution of _actions_, which
  are data descriptors of things that should happen in response to state machine transitions.

  ### Pure and Effect
  TODO explain pure/3 and effect/3 callbacks

  ### Resolution and Execution

  Under the hood, Protean uses two protocols to manage actions:
  `Protean.Action.Protocol.Resolvable` and `Protean.Action.Protocol.Executable`. Resolution is a
  preparation step that allows actions to use runtime information to determine what side-effects
  will take place. Execution can be thought of as a "commit" step, executing the complete list of
  actions in order.

  Resolution acts on the machine state and is a functional transformation of one machine state
  to the next (and some actions). Execution acts on the interpreter state, managing things like
  stateful processes.
  """

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.Interpreter
  alias Protean.State

  @typedoc "The string name of an action used to pattern match in a handler."
  @type name :: String.t()

  @typedoc "Must implement the `Protean.Action.Protocol.Executable` protocol."
  @type resolved :: any

  @typedoc "Must implement the `Protean.Action.Protocol.Resolvable` protocol."
  @type unresolved :: any

  @doc "TODO"
  def pure(action_name) when is_binary(action_name),
    do: %Action.Pure.Unresolved{action_name: action_name}

  def pure(%State{} = state, action_name),
    do: State.put_actions(state, [pure(action_name)])

  @doc "TODO"
  def effect(action_name) when is_binary(action_name),
    do: %Action.Effect.Unresolved{action_name: action_name}

  def effect(%State{} = state, action_name),
    do: State.put_actions(state, [effect(action_name)])

  @doc "TODO"
  def assign(%State{} = state, key, value),
    do: State.put_actions(state, [assign(key, value)])

  def assign(%State{} = state, update_fun) when is_function(update_fun),
    do: State.put_actions(state, [assign(update_fun)])

  def assign(%State{} = state, assigns),
    do: State.put_actions(state, [assign(assigns)])

  def assign(key, value),
    do: %Action.Assign.Unresolved{merge: %{key => value}}

  def assign(update_fun) when is_function(update_fun),
    do: %Action.Assign.Unresolved{function: update_fun}

  def assign(assigns),
    do: %Action.Assign.Unresolved{merge: Enum.into(assigns, %{})}

  @doc "TODO"
  def send_event(event, opts \\ []),
    do: %Action.SendEvent.Unresolved{event: event, to: opts[:to], delay: opts[:delay]}

  def send_event(%State{} = state, event, opts),
    do: State.put_actions(state, [send_event(event, opts)])

  @doc "TODO"
  def choose(actions) when is_list(actions),
    do: %Action.Choose.Unresolved{actions: actions}

  def choose(%State{} = state, actions),
    do: State.put_actions(state, [choose(actions)])

  @doc false
  @spec resolve(unresolved, State.t(), module) :: {[resolved], [unresolved]}
  def resolve(action, state, handler) do
    case Resolvable.resolve(action, state, handler) do
      nil -> {[], []}
      {resolved, unresolved} -> {List.wrap(resolved), List.wrap(unresolved)}
      resolved -> {List.wrap(resolved), []}
    end
  end

  @doc false
  @spec exec(resolved, Interpreter.t()) :: Interpreter.t()
  def exec(action, interpreter) do
    case Executable.exec(action, interpreter) do
      nil -> interpreter
      interpreter -> interpreter
    end
  end

  defimpl Resolvable, for: BitString do
    def resolve(action_name, _state, _handler) do
      {nil, [Action.pure(action_name), Action.effect(action_name)]}
    end
  end
end
