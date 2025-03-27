import api/config
import api/models
import effect
import effect/promise

// import envoy
// import gleam/dynamic
import gleam/dynamic/decode
import gleam/fetch
import gleam/http
import gleam/http/request
import gleam/json
import gleam/result
import messages

const anthropic_api_messages = "https://api.anthropic.com/v1/messages"

pub fn get_messages(input) {
  use api_key <- effect.from_result_map_error(
    config.get_api_key("ANTHROPIC_API_KEY"),
    models.Config,
  )

  use req <- effect.from_result(
    request.to(anthropic_api_messages)
    |> result.replace_error(models.InvalidEndpoint),
  )

  let req =
    req
    |> models.set_default_headers(api_key)
    |> request.set_method(http.Post)

  use res <- promise.from_promise_result(fetch.send(req), models.Fetch)
  use res <- promise.from_promise(fetch.read_text_body(res))
  use res <- effect.then(
    res |> result.map_error(models.Fetch) |> effect.wrap_result,
  )

  res.body
  |> decode_messages
  |> effect.wrap_result
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
    |> result.replace_error(models.Decode),
  )
  decode.run(dynamic, messages_decoder)
  |> result.replace_error(models.Decode)
}
