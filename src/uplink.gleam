import api/models
import components/text_input
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import messages

import teashop
import teashop/command
import teashop/event
import teashop/key

import gleam_community/ansi

import effect/effect_result as effect

pub fn main() {
  // let eff =
  //   effect_2.pure(10)
  //   |> effect_2.try(fn(x) {
  //     cas > 5 {
  //       True -> effect_2.pure("Hi")
  //       False -> effect_2.error(TooSmall)
  //     }
  //   })

  // effect_2.perform(eff, fn(res) {
  //   case res {
  //     Ok(val) -> io.println("Success " <> val)
  //     Error(_) -> io.println("Error: Too small...")
  //   }
  // })

  use models <- effect.perform(models.get_models())
  case models {
    Ok(models) -> {
      let init = get_init(models)
      teashop.app(init, update, view)
      |> teashop.start(Nil)
      Nil
    }
    Error(err) -> {
      case err {
        models.DecodeError -> io.println("Decode err")
        _ -> io.println("other err")
      }
    }
  }
}

type ModelChoiceModel {
  ModelChoiceModel(
    chosen: option.Option(models.Model),
    choices: List(models.Model),
    cursor: Int,
  )
}

type ChatModel {
  ChatModel(
    text_model: text_input.Model,
    messages: option.Option(messages.Messages),
  )
}

type Model {
  Model(model_choice_model: ModelChoiceModel, chat_model: ChatModel)
}

fn get_init(models: List(models.Model)) {
  fn(_) {
    #(
      Model(
        model_choice_model: ModelChoiceModel(
          chosen: option.None,
          choices: models,
          cursor: 0,
        ),
        chat_model: ChatModel(
          text_model: text_input.new(),
          messages: option.None,
        ),
      ),
      command.set_window_title("uplink"),
    )
  }
}

fn update_model_choice(model: ModelChoiceModel, event) {
  case event {
    event.Key(key.Char("q")) | event.Key(key.Esc) -> #(model, command.quit())

    event.Key(key.Char("k")) | event.Key(key.Up) -> {
      let choices_len = list.length(model.choices)
      let cursor = case model.cursor == 0 {
        True -> choices_len - 1
        False -> model.cursor - 1
      }
      #(ModelChoiceModel(..model, cursor: cursor), command.none())
    }

    event.Key(key.Char("j")) | event.Key(key.Down) -> {
      let choices_len = list.length(model.choices)
      let cursor = case model.cursor == { choices_len - 1 } {
        True -> 0
        False -> model.cursor + 1
      }
      #(ModelChoiceModel(..model, cursor: cursor), command.none())
    }
    event.Key(key.Enter) | event.Key(key.Space) -> {
      let chosen =
        list.index_fold(model.choices, option.None, fn(acc, choice, index) {
          case model.cursor == index {
            True -> option.Some(choice)
            False -> acc
          }
        })

      #(ModelChoiceModel(..model, chosen:), command.none())
    }
    _otherwise -> #(model, command.none())
  }
}

fn update_chat_model(model: ChatModel, event) {
  case event {
    event.Key(key.Esc) -> #(model, command.quit())
    event.Key(key.Enter) -> handle_submit_message(model)
    _otherwise -> {
      let #(text_model, command) = text_input.update(model.text_model, event)
      #(ChatModel(..model, text_model:), command)
    }
  }
}

fn handle_submit_message(model: ChatModel) {
  todo
}

fn update(model: Model, event) {
  case model.model_choice_model.chosen {
    option.Some(_) -> {
      let #(chat_model, command) = update_chat_model(model.chat_model, event)
      #(Model(..model, chat_model:), command)
    }
    option.None -> {
      let #(model_choice_model, command) =
        update_model_choice(model.model_choice_model, event)
      #(Model(..model, model_choice_model:), command)
    }
  }
}

fn view(model: Model) {
  case model.model_choice_model.chosen {
    option.Some(choice) -> render_chat(model.chat_model, choice)
    option.None -> render_model_chooser(model.model_choice_model)
  }
}

fn render_chat(model: ChatModel, choice: models.Model) {
  let footer = "Press Escape to quit."
  // we can loop through messages, render them all as simple strings through content for now,
  // then use the cursor model to render a text field
  let body =
    case model.messages {
      option.Some(messages) -> render_messages(messages)
      option.None -> render_empty_messages(choice.display_name)
    }
    <> text_input.view(model.text_model)

  [body, footer]
  |> string.join("\n\n")
}

fn render_messages(messages: messages.Messages) {
  let messages_to_render = [messages.User(messages.first), ..messages.rest]
  use acc, message <- list.fold(messages_to_render, "")
  acc
  <> case message {
    messages.User(user_message) ->
      "Assitant: " <> user_message.content.text |> ansi.green <> "\n"
    messages.Assistant(assistant_message) ->
      "User: " <> assistant_message.content.text |> ansi.red <> "\n"
  }
}

fn render_empty_messages(choice) {
  "Using model " <> choice <> "\n" <> "How can I help you today?" <> "\n"
}

fn render_model_chooser(model: ModelChoiceModel) {
  let header = "Select a model:"
  let footer = "Press q to quit."
  let body =
    model.choices
    |> list.index_map(fn(element, index) {
      let models.Model(display_name:, id: _) = element
      case model.cursor == index {
        True -> ansi.blue("> " <> display_name)
        False -> "  " <> display_name
      }
    })
    |> string.join("\n")

  [header, body, footer]
  |> string.join("\n\n")
}
// we want to be able to:
//  1. [x] Choose a model (should be given a list through the models list api, save/cache the model to use when first opened) 
//  2. [ ] We can start with a simple chat, extend this by (in order):
//    - [ ] having code highlighting in responses, or at minimum parsing markdown into readable responses
//    - [ ] having projects (for grouping chats)
//    - [ ] choosing between multiple llms instead of just claude, all in the same project?
//    - uploading or using images, files (text files?)
//    - send github links? (or even search github would be sick)
//    - use a browser
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
