import api/config
import effect
import effect/promise as effect_promise
import gleam/dynamic
import gleam/dynamic/decode
import gleam/fetch
import gleam/http/request
import gleam/json
import gleam/result

const anthropic_api_models = "https://api.anthropic.com/v1/models"

pub fn set_default_headers(req, api_key) {
  req
  |> request.set_header("content-type", "application/json")
  |> request.set_header("anthropic-version", "2023-06-01")
  |> request.set_header("x-api-key", api_key)
}

pub type GetModelError {
  Fetch(fetch.FetchError)
  Config(config.ConfigError)
  InvalidEndpoint
  ReadBody
  Decode
}

pub type Model {
  Model(id: String, display_name: String)
}

pub fn get_models() {
  use api_key <- effect.from_result_map_error(
    config.get_api_key("ANTHROPIC_API_KEY"),
    Config,
  )

  use req <- effect.from_result(
    request.to(anthropic_api_models) |> result.replace_error(InvalidEndpoint),
  )

  let req =
    req
    |> set_default_headers(api_key)
    |> request.set_query([#("limit", "40")])

  use res <- effect_promise.from_promise_result(fetch.send(req), Fetch)
  use res <- effect_promise.from_promise(fetch.read_text_body(res))
  use res <- effect.then(res |> result.map_error(Fetch) |> effect.wrap_result)

  res.body
  |> decode_models
  |> effect.wrap_result
}

fn decode_models(json_string) {
  let model_decoder = {
    use id <- decode.field("id", decode.string)
    use display_name <- decode.field("display_name", decode.string)
    decode.success(Model(id:, display_name:))
  }

  let data_decoder = {
    use data <- decode.field("data", decode.dynamic)
    decode.success(dynamic.from(data))
  }

  use dynamic <- result.try(
    json.parse(json_string, data_decoder) |> result.replace_error(Decode),
  )

  decode.run(dynamic, decode.list(of: model_decoder))
  |> result.replace_error(Decode)
}
