import envoy
import fetch_callbacks
import gleam/http/request
import gleam/http/response
import gleam/result

const anthropic_api_models = "https://api.anthropic.com/v1/models"

pub fn get_models(
  callback: fn(Result(response.Response(String), fetch_callbacks.FetchError)) ->
    any,
) {
  let req = request.to(anthropic_api_models)

  use api_key <- result.try(envoy.get("ANTHROPIC_API_KEY"))
  use req <- result.map(req)
  let req =
    request.set_header(req, "x-api-key", api_key)
    |> request.set_header("content-type", "application/json")

  use res <- fetch_callbacks.send(req)
  use res <- result.map(res)
  use body <- fetch_callbacks.read_text_body(res)
  callback(body)
}
