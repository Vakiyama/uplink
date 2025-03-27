import api/config
import api/models
import effect
import envoy
import gleam/dynamic
import gleam/dynamic/decode
import gleam/fetch
import gleam/http/request
import gleam/json
import gleam/result
import messages

pub type MessagesRequestError {
  Fetch(fetch.FetchError)
  Config(config.ConfigError)
  InvalidEndpoint
  ReadBody
  Decode
}

pub fn get_messages() {
  use api_key <- effect.from_result_map_error(
    config.get_api_key("ANTHROPIC_API_KEY"),
    Config,
  )
  todo
}

fn parse_stop_reason(stop_reason) {
  case stop_reason {
    "end_turn" -> messages.EndTurn
    "max_tokens" -> messages.MaxTokens
    "stop_sequence" -> messages.StopSequence
    "tool_use" -> messages.ToolUse
    _ -> messages.Unknown
  }
}

fn decode_messages(json_string) {
  let content_decoder = {
    use text <- decode.field("text", decode.string)
    decode.success(messages.AIMessage(content: messages.Text(text)))
  }

  let usage_decoder = {
    use input <- decode.field("input_tokens", decode.int)
    use output <- decode.field("output_tokens", decode.int)
    decode.success(messages.Tokens(input, output))
  }

  let messages_decoder = {
    use content <- decode.field("content", decode.list(content_decoder))
    use id <- decode.field("id", decode.string)
    use model <- decode.field("model", decode.string)
    use stop_reason <- decode.field("stop_reason", decode.string)
    use usage <- decode.field("usage", usage_decoder)
    decode.success(messages.AnthropicResponse(
      id:,
      content:,
      model:,
      stop_reason: parse_stop_reason(stop_reason),
      usage:,
    ))
  }

  use dynamic <- result.try(
    json.parse(json_string, decode.dynamic)
    |> result.replace_error(decode.DecodeError),
  )
  decode.run(dynamic, messages_decoder)
  |> result.replace_error(decode.DecodeError)
}
