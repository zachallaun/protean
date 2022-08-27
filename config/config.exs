import Config

config :protean,
  supervisor: DynamicSupervisor,
  registry: Registry

config :protean, :pubsub,
  name: Protean.PubSub,
  start: true
