import teashop/command
import teashop/event

pub type App(model, msg, flags)

pub type Dispatch(msg) =
  fn(Action(msg)) -> Nil

pub type Action(msg)

@external(javascript, "./teashop.ffi.mjs", "setup")
pub fn app(
  init: fn(flags) -> #(model, command.Command(msg)),
  // mode, event, emit, done
  update: fn(
    model,
    event.Event(msg),
    fn(msg) -> Nil,
    fn(model, command.Command(msg)) -> Nil,
  ) ->
    model,
  view: fn(model) -> String,
) -> App(model, msg, flags)

@external(javascript, "./teashop.ffi.mjs", "run")
pub fn start(
  app: App(model, msg, flags),
  flags: flags,
) -> fn(Action(msg)) -> Nil
