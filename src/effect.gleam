import gleam/javascript/promise
import gleam/list

pub opaque type Effect(msg) {
  Effect(run: List(fn(Actions(msg)) -> Nil))
}

type Actions(msg) {
  Actions(dispatch: fn(msg) -> Nil)
}

pub fn none() -> Effect(msg) {
  Effect(run: [])
}

pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  Effect(run: [fn(actions: Actions(msg)) { effect(actions.dispatch) }])
}

pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect(
    run: list.map(effect.run, fn(eff) {
      fn(actions: Actions(b)) {
        eff(Actions(dispatch: fn(msg) { actions.dispatch(f(msg)) }))
      }
    }),
  )
}

pub fn try(
  res: Result(value, error),
  f: fn(value) -> Effect(Result(b, error)),
) -> Effect(Result(b, error)) {
  case res {
    Ok(value) -> f(value)
    Error(e) -> from(fn(dispatch) { dispatch(Error(e)) })
  }
}

pub fn try_map_error(
  res: Result(value, error),
  map_error: fn(error) -> new_error,
  f: fn(value) -> Effect(Result(b, new_error)),
) -> Effect(Result(b, new_error)) {
  case res {
    Ok(value) -> f(value)
    Error(e) -> from(fn(dispatch) { dispatch(Error(map_error(e))) })
  }
}

pub fn try_await(
  pres: promise.Promise(Result(value, error)),
  f: fn(value) -> Effect(Result(b, error)),
) -> Effect(Result(b, error)) {
  Effect(run: [
    fn(actions) {
      promise.map(pres, fn(result) {
        case result {
          Ok(value) -> {
            let Effect(run: runs) = f(value)
            list.each(runs, fn(run) { run(actions) })
          }
          Error(e) -> actions.dispatch(Error(e))
        }
        Nil
      })
      Nil
    },
  ])
}

pub fn try_await_map_error(
  pres: promise.Promise(Result(value, error)),
  map_error: fn(error) -> new_error,
  f: fn(value) -> Effect(Result(b, new_error)),
) -> Effect(Result(b, new_error)) {
  Effect(run: [
    fn(actions) {
      promise.map(pres, fn(result) {
        case result {
          Ok(value) -> {
            let Effect(run: runs) = f(value)
            list.each(runs, fn(run) { run(actions) })
          }
          Error(e) -> actions.dispatch(Error(map_error(e)))
        }
        Nil
      })
      Nil
    },
  ])
}

pub fn perform(effect: Effect(msg), dispatch: fn(msg) -> Nil) -> Nil {
  let actions = Actions(dispatch: dispatch)
  list.each(effect.run, fn(run) { run(actions) })
}

pub fn dispatch(value: a) -> Effect(a) {
  from(fn(dispatch) { dispatch(value) })
}
