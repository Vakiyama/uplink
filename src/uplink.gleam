import api/config
import api/messages as api_messages
import api/models
import components/cursor
import components/text_input
import effect
import gleam/fetch
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import gleam_community/ansi
import messages
import teashop
import teashop/command
import teashop/event
import teashop/key

fn print_request_error(error: models.RequestError) {
  let message = case error {
    models.Decode -> "Decode Error"
    models.InvalidEndpoint -> "Invalid Endpoint"
    models.Fetch(fetch_error) ->
      case fetch_error {
        fetch.NetworkError(msg) -> msg
        fetch.UnableToReadBody -> "Unable to read body"
        fetch.InvalidJsonBody -> "Invalid Json Body"
      }
    models.Config(config_error) ->
      case config_error {
        config.NoEnv(missing_env_key) ->
          string.append("Missing env: ", missing_env_key)
      }
    models.ReadBody -> "Read Body Error"
  }
  io.print_error(message)
}

pub fn main() {
  use models <- effect.perform(models.get_models())
  case models {
    Ok(models) -> {
      let init = get_init(models)
      teashop.app(init, update, view)
      |> teashop.start(Nil)
      Nil
    }
    Error(err) -> print_request_error(err)
  }
}

fn update(model: Model, event, emit, done) {
  case model.llm_choice.chosen {
    option.Some(_) -> {
      let loading_model =
        update_chat_model(
          model.chat,
          model.llm_choice,
          event,
          emit,
          fn(new_chat_model, command) {
            done(Model(..model, chat: new_chat_model), command)
          },
        )
      Model(..model, chat: loading_model)
    }
    option.None -> {
      let #(llm_choice, command) = update_model_choice(model.llm_choice, event)
      done(Model(..model, llm_choice:), command)
      model
    }
  }
}

type LLMChoice {
  LLMChoice(
    chosen: option.Option(models.Model),
    choices: List(models.Model),
    cursor: Int,
  )
}

type Chat {
  Chat(
    text_model: text_input.Model,
    messages: option.Option(List(messages.Message)),
    streaming: Bool,
    loading: option.Option(String),
  )
}

type Model {
  Model(llm_choice: LLMChoice, chat: Chat)
}

fn get_init(models: List(models.Model)) {
  fn(_) {
    #(
      Model(
        llm_choice: LLMChoice(chosen: option.None, choices: models, cursor: 0),
        chat: Chat(
          text_model: text_input.new(),
          messages: option.None,
          loading: option.None,
          streaming: False,
        ),
      ),
      command.set_window_title("uplink"),
    )
  }
}

fn update_model_choice(model: LLMChoice, event) {
  case event {
    event.Key(key.Char("q")) | event.Key(key.Esc) -> #(model, command.quit())
    event.Key(key.Char("k")) | event.Key(key.Up) -> {
      let choices_len = list.length(model.choices)
      let cursor = case model.cursor == 0 {
        True -> choices_len - 1
        False -> model.cursor - 1
      }
      #(LLMChoice(..model, cursor: cursor), command.none())
    }

    event.Key(key.Char("j")) | event.Key(key.Down) -> {
      let choices_len = list.length(model.choices)
      let cursor = case model.cursor == { choices_len - 1 } {
        True -> 0
        False -> model.cursor + 1
      }
      #(LLMChoice(..model, cursor: cursor), command.none())
    }
    event.Key(key.Enter) | event.Key(key.Space) -> {
      let chosen =
        list.index_fold(model.choices, option.None, fn(acc, choice, index) {
          case model.cursor == index {
            True -> option.Some(choice)
            False -> acc
          }
        })

      #(LLMChoice(..model, chosen:), command.none())
    }
    _otherwise -> #(model, command.none())
  }
}

fn update_chat_model(model: Chat, model_choice: LLMChoice, event, emit, done) {
  let assert option.Some(chosen_model) = model_choice.chosen

  case event {
    event.Key(key.Esc) -> {
      done(model, command.quit())
      model
    }
    event.Key(key.Enter) -> {
      {
        use new_model <- handle_submit_message(model, chosen_model, emit)
        done(new_model, command.none())
      }
      Chat(..model, loading: option.Some("Thinking..."))
    }
    event.Custom(TestEvent) -> {
      model
    }

    _otherwise -> {
      let #(text_model, command) = text_input.update(model.text_model, event)
      done(Chat(..model, text_model:), command)
      model
    }
  }
}

fn initialize_messages(messages: option.Option(List(messages.Message))) {
  case messages {
    option.Some(list_of_messages) -> list_of_messages
    option.None -> []
  }
}

type TestEvent {
  TestEvent
}

fn handle_submit_message(model: Chat, model_choice: models.Model, emit, done) {
  emit(event.Custom(TestEvent))

  let user_message = messages.make_user_message(model.text_model.value)

  let messages =
    initialize_messages(model.messages) |> list.append([user_message])

  use response <- effect.perform(
    messages
    |> list.append([user_message])
    |> api_messages.get_messages(model_choice, 1024),
  )

  let new_messages = case response {
    Error(error) -> {
      print_request_error(error)
      option.Some(
        list.append(messages, [
          messages.make_ai_message("An error has occured. Please try again."),
        ]),
      )
    }
    Ok(ai_response) -> {
      option.Some(
        messages
        |> list.append(ai_response.content),
      )
    }
  }

  done(Chat(
    messages: new_messages,
    text_model: text_input.set_value(model.text_model, ""),
    loading: option.None,
    streaming: False,
  ))
}

fn view(model: Model) {
  case model.llm_choice.chosen {
    option.Some(choice) -> render_chat(model.chat, choice)
    option.None -> render_model_chooser(model.llm_choice)
  }
}

fn render_chat(model: Chat, choice: models.Model) {
  let footer = "Press Escape to quit."
  // we can loop through messages, render them all as simple strings through content for now,
  // then use the cursor model to render a text field
  let body =
    case model.messages {
      option.Some(messages) -> render_messages(messages)
      option.None -> render_empty_messages(choice.display_name)
    }
    <> case model.loading {
      option.Some(loading) -> loading
      option.None -> text_input.view(model.text_model)
    }

  [body, footer]
  |> string.join("\n\n")
}

fn render_messages(messages: List(messages.Message)) {
  // list.each(messages, fn(mess) {
  //   api_messages.message_to_json(mess)
  //   |> json.to_string
  //   |> io.print
  // })
  use acc, message <- list.fold(messages, "")
  acc
  <> case message {
    messages.User(user_message) ->
      "User: " <> user_message.content.text |> ansi.green <> "\n"
    messages.Assistant(assistant_message) ->
      "Assistant: " <> assistant_message.content.text |> ansi.cyan <> "\n"
  }
}

fn render_empty_messages(choice) {
  "Using model " <> choice <> "\n" <> "How can I help you today?" <> "\n"
}

fn render_model_chooser(model: LLMChoice) {
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
