import envoy
import fetch_callbacks
import gleam/http/request
import gleam/http/response
import gleam/result

const anthropic_api_models = "https://api.anthropic.com/v1/models"

pub fn get_models(
  callback: fn(Result(String, fetch_callbacks.FetchError)) -> Nil,
) {
  let req = request.to(anthropic_api_models)

  use api_key <- result.try(envoy.get("ANTHROPIC_API_KEY"))
  use req <- result.map(req)
  let req =
    request.set_header(req, "x-api-key", api_key)
    |> request.set_header("content-type", "application/json")

  use res <- fetch_callbacks.send(req)
  use res <- result.map(res)
  { todo }
  |> callback
  // {
  //   use response <- result.map(res)
  //   use body <- fetch_callbacks.read_text_body(response)
  //   {
  //     use body <- result.map(body)
  //     body.body
  //   }
  //   |> callback
  // }
  // Nil
  // use res <- callbackify(fetch.send(req))
}

pub fn get_request_body(
  req: response.Response(fetch_callbacks.FetchBody),
  cb: fn(Result(response.Response(String), fetch_callbacks.FetchError)) -> Nil,
) {
  use body <- fetch_callbacks.read_text_body(req)
  cb(body)
}
