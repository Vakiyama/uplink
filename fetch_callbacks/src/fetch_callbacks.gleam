import gleam/dynamic.{type Dynamic}
import gleam/fetch/form_data.{type FormData}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/result

pub type FetchError {
  NetworkError(String)
  UnableToReadBody
  InvalidJsonBody
}

pub type FetchBody

pub type FetchRequest

pub type FetchResponse

@external(javascript, "../gleam_fetch_ffi.mjs", "raw_send")
pub fn raw_send(
  a: FetchRequest,
  cb: fn(Result(FetchResponse, FetchError)) -> any,
) -> Nil

pub fn send(
  request: Request(String),
  cb: fn(Result(Response(FetchBody), FetchError)) -> any,
) -> Nil {
  use res <- raw_send(request |> to_fetch_request)
  result.map(res, from_fetch_response)
  |> cb
}

pub fn send_form_data(
  request: Request(FormData),
  cb: fn(Result(Response(FetchBody), FetchError)) -> any,
) -> Nil {
  use res <- raw_send(
    request
    |> form_data_to_fetch_request,
  )

  result.map(res, from_fetch_response) |> cb
}

pub fn send_bits(
  request: Request(BitArray),
  cb: fn(Result(Response(FetchBody), FetchError)) -> any,
) -> Nil {
  use res <- raw_send(
    request
    |> bitarray_request_to_fetch_request,
  )

  result.map(res, from_fetch_response) |> cb
}

@external(javascript, "../gleam_fetch_ffi.mjs", "to_fetch_request")
pub fn to_fetch_request(a: Request(String)) -> FetchRequest

@external(javascript, "../gleam_fetch_ffi.mjs", "form_data_to_fetch_request")
pub fn form_data_to_fetch_request(a: Request(FormData)) -> FetchRequest

@external(javascript, "../gleam_fetch_ffi.mjs", "bitarray_request_to_fetch_request")
pub fn bitarray_request_to_fetch_request(a: Request(BitArray)) -> FetchRequest

@external(javascript, "../gleam_fetch_ffi.mjs", "from_fetch_response")
pub fn from_fetch_response(a: FetchResponse) -> Response(FetchBody)

@external(javascript, "../gleam_fetch_ffi.mjs", "read_bytes_body")
pub fn read_bytes_body(
  a: Response(FetchBody),
  cb: fn(Result(Response(BitArray), FetchError)) -> any,
) -> Nil

@external(javascript, "../gleam_fetch_ffi.mjs", "read_text_body")
pub fn read_text_body(
  a: Response(FetchBody),
  cb: fn(Result(Response(String), FetchError)) -> any,
) -> Nil

@external(javascript, "../gleam_fetch_ffi.mjs", "read_json_body")
pub fn read_json_body(
  a: Response(FetchBody),
  cb: fn(Result(Response(Dynamic), FetchError)) -> any,
) -> Nil
