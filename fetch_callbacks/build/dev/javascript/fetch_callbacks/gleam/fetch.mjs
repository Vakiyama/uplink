import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../gleam_http/gleam/http/response.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $form_data from "../gleam/fetch/form_data.mjs";
import {
  raw_send,
  to_fetch_request,
  form_data_to_fetch_request,
  bitarray_request_to_fetch_request,
  from_fetch_response,
  read_bytes_body,
  read_text_body,
  read_json_body,
} from "../gleam_fetch_ffi.mjs";

export {
  bitarray_request_to_fetch_request,
  form_data_to_fetch_request,
  from_fetch_response,
  raw_send,
  read_bytes_body,
  read_json_body,
  read_text_body,
  to_fetch_request,
};

export class NetworkError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class UnableToReadBody extends $CustomType {}

export class InvalidJsonBody extends $CustomType {}

export function send(request, cb) {
  return raw_send(
    (() => {
      let _pipe = request;
      return to_fetch_request(_pipe);
    })(),
    (res) => {
      let _pipe = $result.map(res, from_fetch_response);
      return cb(_pipe);
    },
  );
}

export function send_form_data(request, cb) {
  return raw_send(
    (() => {
      let _pipe = request;
      return form_data_to_fetch_request(_pipe);
    })(),
    (res) => {
      let _pipe = $result.map(res, from_fetch_response);
      return cb(_pipe);
    },
  );
}

export function send_bits(request, cb) {
  return raw_send(
    (() => {
      let _pipe = request;
      return bitarray_request_to_fetch_request(_pipe);
    })(),
    (res) => {
      let _pipe = $result.map(res, from_fetch_response);
      return cb(_pipe);
    },
  );
}
