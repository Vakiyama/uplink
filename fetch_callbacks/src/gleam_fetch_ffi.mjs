import { Ok, Error, List, toBitArray } from "./gleam.mjs";
import { to_string as uri_to_string } from "../gleam_stdlib/gleam/uri.mjs";
import { method_to_string } from "../gleam_http/gleam/http.mjs";
import { to_uri } from "../gleam_http/gleam/http/request.mjs";
import { Response } from "../gleam_http/gleam/http/response.mjs";
import {
  NetworkError,
  InvalidJsonBody,
  UnableToReadBody,
} from "../gleam_fetch/gleam/fetch.mjs";

export function raw_send(request, callback) {
  fetch(request).then((res) => callback(new Ok(res))).catch(e => callback(new Error(new NetworkError(e.toString()))))
}

export function from_fetch_response(response) {
  return new Response(
    response.status,
    List.fromArray([...response.headers]),
    response
  );
}

function request_common(request) {
  let url = uri_to_string(to_uri(request));
  let method = method_to_string(request.method).toUpperCase();
  let options = {
    headers: make_headers(request.headers),
    method,
  };
  return [url, options]
}

export function to_fetch_request(request) {
  let [url, options] = request_common(request)
  if (options.method !== "GET" && options.method !== "HEAD") options.body = request.body;
  return new globalThis.Request(url, options);
}

export function form_data_to_fetch_request(request) {
  let [url, options] = request_common(request)
  if (options.method !== "GET" && options.method !== "HEAD") options.body = request.body;
  // Remove `content-type`, because the browser will add the correct header by itself.
  delete options.headers['content-type']
  return new globalThis.Request(url, options);
}

export function bitarray_request_to_fetch_request(request) {
  let [url, options] = request_common(request)
  if (options.method !== "GET" && options.method !== "HEAD") options.body = request.body.buffer;
  return new globalThis.Request(url, options);
}

function make_headers(headersList) {
  let headers = new globalThis.Headers();
  for (let [k, v] of headersList) headers.append(k.toLowerCase(), v);
  return headers;
}

export function read_bytes_body(response, callback) {
  response.body.arrayBuffer().then(body => {
    callback(new Ok(response.withFields({ body: toBitArray(new Uint8Array(body)) })));
  }).catch(() => {
    callback(new Error(new UnableToReadBody()))
  })
}

export function read_text_body(response) {
  response.body.text()
    .then(body => new Ok(response.withFields({ body })))
    .catch(() => callback(new Error(new UnableToReadBody())))
}

export function read_json_body(response, callback) {
  response.body.json().then(body =>
    callback(new Ok(response.withFields({ body })))
  )
    .catch(() => callback(new Error(new InvalidJsonBody())))
}

export function newFormData() {
  return new FormData()
}

function cloneFormData(formData) {
  const f = new FormData()
  for (const [key, value] of formData.entries()) f.append(key, value)
  return f
}

export function appendFormData(formData, key, value) {
  const f = cloneFormData(formData)
  f.append(key, value)
  return f
}

export function setFormData(formData, key, value) {
  const f = cloneFormData(formData)
  f.set(key, value)
  return f
}

export function appendBitsFormData(formData, key, value) {
  const f = cloneFormData(formData)
  f.append(key, new Blob([value.buffer]))
  return f
}

export function setBitsFormData(formData, key, value) {
  const f = cloneFormData(formData)
  f.set(key, new Blob([value.buffer]))
  return f
}
