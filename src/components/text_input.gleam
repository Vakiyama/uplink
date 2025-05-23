import components/cursor
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import teashop/command
import teashop/event
import teashop/key

pub type Model {
  Model(
    char_limit: Option(Int),
    cursor: cursor.Model,
    dimension: Option(Dimension),
    focus: Bool,
    place_holder: Option(String),
    position: Int,
    prompt: String,
    value: String,
  )
}

pub opaque type Dimension {
  Dimension(width: Int, left_offset: Int, right_offset: Int)
}

pub fn new() {
  Model(
    char_limit: None,
    cursor: cursor.initial_model(),
    dimension: None,
    focus: False,
    position: 0,
    place_holder: None,
    prompt: "> ",
    value: "",
  )
}

pub fn blink(model: Model) {
  cursor.start_blink(model.cursor)
}

fn set_cursor(model: Model, position) {
  let position = int.clamp(position, 0, string.length(model.value))
  handle_overflow(Model(..model, position: position))
}

pub fn set_focus(model, focus: Bool) {
  Model(..model, focus: focus)
}

pub fn set_placeholder(model, place_holder: String) {
  Model(..model, place_holder: Some(place_holder))
}

pub fn set_value(model: Model, value: String) {
  let char_limit = case model.char_limit {
    Some(char_limit) -> int.min(string.length(value), char_limit)
    None -> string.length(value)
  }
  Model(..model, value: string.slice(value, 0, char_limit))
}

pub fn set_width(model, width: Int) {
  let dimension = Dimension(width, 0, width)
  Model(..model, dimension: Some(dimension))
}

pub fn update(model: Model, event) {
  let original_position = model.position

  let model = case event {
    event.Key(key) ->
      case key {
        key.Right | key.Ctrl(key.Char("f")) -> character_forward(model)
        key.Left | key.Ctrl(key.Char("b")) -> character_backward(model)
        key.Alt(key.Right) | key.Ctrl(key.Right) | key.Alt(key.Char("f")) ->
          word_forward(model)
        key.Alt(key.Left) | key.Ctrl(key.Left) | key.Alt(key.Char("b")) ->
          word_backward(model)
        key.Alt(key.Backspace) | key.Ctrl(key.Char("w")) ->
          delete_word_backward(model)
        key.Alt(key.Delete) | key.Alt(key.Char("d")) ->
          delete_word_forward(model)
        key.Ctrl(key.Char("k")) -> delete_after_cursor(model)
        key.Ctrl(key.Char("u")) -> delete_before_cursor(model)
        key.Backspace | key.Ctrl(key.Char("h")) ->
          delete_character_backward(model)
        key.Delete | key.Ctrl(key.Char("d")) -> delete_character_forward(model)
        key.Home | key.Ctrl(key.Char("a")) -> line_start(model)
        key.End | key.Ctrl(key.Char("e")) -> line_end(model)
        key.Space -> insert_characters(model, " ")
        key.Char(char) -> insert_characters(model, char)
        _otherwise -> model
      }
    _otherwise -> model
  }

  let commands = []

  // update cursor
  let #(cur, cmd) = cursor.update(model.cursor, event)
  let commands = list.prepend(commands, cmd)

  // reset blink if cursor moved
  let #(cur, cmd) = case original_position != model.position {
    True -> {
      let cur = cursor.blink(cur, False)
      cursor.blink_command(cur)
    }
    False -> #(cur, command.none())
  }
  let commands = list.prepend(commands, cmd)

  let model = Model(..model, cursor: cur)
  let model = handle_overflow(model)

  #(model, command.sequence(commands))
}

fn character_forward(model: Model) {
  case model.position < string.length(model.value) {
    True -> set_cursor(model, model.position + 1)
    False -> model
  }
}

fn character_backward(model: Model) {
  case model.position > 0 {
    True -> set_cursor(model, model.position - 1)
    False -> model
  }
}

fn word_forward(model: Model) {
  let position = model.position + 1

  // merge white space
  let position =
    string.slice(model.value, position, string.length(model.value))
    |> string.to_graphemes
    |> list.fold_until(position, fn(acc, char) {
      case char == " " {
        False -> list.Stop(acc)
        True -> list.Continue(acc + 1)
      }
    })

  // offset to nearest white space
  let position =
    string.slice(model.value, position, string.length(model.value))
    |> string.to_graphemes
    |> list.fold_until(position, fn(acc, char) {
      case char == " " {
        True -> list.Stop(acc)
        False -> list.Continue(acc + 1)
      }
    })

  set_cursor(model, position)
}

fn word_backward(model: Model) {
  let position = model.position - 1

  // merge white space
  let position =
    string.slice(model.value, 0, position)
    |> string.reverse
    |> string.to_graphemes
    |> list.fold_until(position, fn(acc, char) {
      case char == " " {
        False -> list.Stop(acc)
        True -> list.Continue(acc - 1)
      }
    })

  // offset to nearest white space
  let position =
    string.slice(model.value, 0, position)
    |> string.reverse
    |> string.to_graphemes
    |> list.fold_until(position, fn(acc, char) {
      case char == " " {
        True -> list.Stop(acc)
        False -> list.Continue(acc - 1)
      }
    })

  set_cursor(model, position)
}

fn delete_word_backward(model: Model) {
  let position = model.position

  let model = word_backward(model)

  let value =
    string.slice(model.value, 0, model.position)
    <> string.slice(model.value, position, string.length(model.value))

  set_value(model, value)
}

fn delete_word_forward(model: Model) {
  let position = model.position

  let model = word_forward(model)

  let value =
    string.slice(model.value, 0, position)
    <> string.slice(model.value, model.position, string.length(model.value))

  set_value(model, value)
}

fn delete_after_cursor(model: Model) {
  let value = string.slice(model.value, 0, model.position)
  set_value(model, value)
  |> set_cursor(string.length(value))
}

fn delete_before_cursor(model: Model) {
  let value =
    string.slice(model.value, model.position, string.length(model.value))
  set_value(model, value)
  |> set_cursor(0)
}

fn delete_character_forward(model: Model) {
  case
    string.length(model.value) > 0
    && model.position < string.length(model.value)
  {
    True -> {
      let value =
        string.slice(model.value, 0, model.position)
        <> string.slice(
          model.value,
          model.position + 1,
          string.length(model.value),
        )

      Model(..model, value: value)
    }
    False -> model
  }
}

fn delete_character_backward(model: Model) {
  let value = model.value
  let value_length = string.length(value)
  case value_length > 0 {
    True -> {
      let value =
        string.slice(value, 0, int.max(0, model.position - 1))
        <> string.slice(value, model.position, value_length)
      let model = Model(..model, value: value)
      case model.position > 0 {
        True -> set_cursor(model, model.position - 1)
        False -> model
      }
    }
    False -> model
  }
}

fn line_start(model: Model) {
  set_cursor(model, 0)
}

fn line_end(model: Model) {
  set_cursor(model, string.length(model.value))
}

fn insert_characters(model: Model, string) {
  let chars = string.to_graphemes(string)

  let chars = case model.char_limit {
    Some(char_limit) -> {
      let available_space = char_limit - string.length(model.value)
      let chars_length = list.length(chars)
      case available_space {
        available_space if available_space <= 0 -> []
        available_space if available_space < chars_length -> {
          let #(chars, _) = list.split(chars, available_space)
          chars
        }
        _otherwise -> chars
      }
    }
    None -> chars
  }

  let head = string.slice(model.value, 0, model.position)
  let tail =
    string.slice(model.value, model.position, string.length(model.value))
  let value = head <> string.join(chars, "") <> tail
  character_forward(
    Model(
      ..model,
      value: value,
      position: model.position + list.length(chars) - 1,
    ),
  )
}

fn handle_overflow(model: Model) {
  case model.dimension {
    Some(dimension) -> {
      let left_offset = dimension.left_offset
      let right_offset = dimension.right_offset

      let #(left_offset, right_offset) = case model.position {
        position if position < left_offset -> {
          let left_offset = model.position
          let value_length = int.min(right_offset, string.length(model.value))
          let i = int.min(value_length - left_offset, dimension.width)

          #(left_offset, left_offset + i)
        }
        position if position >= right_offset -> {
          let right_offset = model.position
          let value_length = int.min(right_offset, string.length(model.value))
          let i = int.max(value_length - dimension.width - 1, 0)

          #(right_offset - { value_length - 1 - i }, right_offset)
        }
        _otherwise -> {
          let right_offset =
            right_offset
            |> int.min(string.length(model.value))

          #(left_offset, right_offset)
        }
      }

      let dimension =
        Dimension(
          ..dimension,
          left_offset: left_offset,
          right_offset: right_offset,
        )
      Model(..model, dimension: Some(dimension))
    }
    None -> model
  }
}

pub fn view(model: Model) {
  case model.place_holder, string.length(model.value) == 0 {
    Some(_), True -> place_holder_view(model)
    _otherwise, _otherwise -> {
      let #(left_offset, right_offset) = case model.dimension {
        Some(dimension) -> #(dimension.left_offset, dimension.right_offset)
        None -> #(0, string.length(model.value))
      }
      let value = string.slice(model.value, left_offset, right_offset)
      let position = int.max(0, model.position - left_offset)

      let view = string.slice(value, 0, position)

      model.prompt
      <> case position < string.length(value) {
        True -> {
          let char = string.slice(value, position, 1)
          let cursor = cursor.char(model.cursor, char)
          view
          <> cursor.view(cursor)
          <> string.slice(value, position + 1, string.length(value))
        }
        False -> {
          let c = cursor.char(model.cursor, " ")
          view <> cursor.view(c)
        }
      }
    }
  }
}

fn place_holder_view(model: Model) {
  let assert Some(place_holder) = model.place_holder
  let cursor =
    model.cursor
    |> cursor.char(string.slice(place_holder, 0, 1))
  model.prompt
  <> cursor.view(cursor)
  <> case string.length(place_holder) <= 1 {
    True -> ""
    False -> string.slice(place_holder, 1, string.length(place_holder))
  }
}
