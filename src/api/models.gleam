// import effect
import effect
import envoy
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
  NoEnv
  InvalidEndpoint
  ReadBody
  DecodeError
}

pub type Model {
  Model(id: String, display_name: String)
}

pub fn get_models() {
  use api_key <- effect.try(effect.from_result(
    envoy.get("ANTHROPIC_API_KEY") |> result.replace_error(NoEnv),
  ))

  use req <- effect.try(effect.from_result(
    request.to(anthropic_api_models) |> result.replace_error(InvalidEndpoint),
  ))

  let req =
    req
    |> set_default_headers(api_key)
    |> request.set_query([#("limit", "40")])

  use res <- effect.try_await_map_error(fetch.send(req), Fetch)
  use res <- effect.try_await_map_error(fetch.read_text_body(res), Fetch)

  res.body
  |> decode_models
  |> effect.from_result
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
    json.parse(json_string, data_decoder) |> result.replace_error(DecodeError),
  )

  decode.run(dynamic, decode.list(of: model_decoder))
  |> result.replace_error(DecodeError)
}
