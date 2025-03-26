import envoy
import gleam/result

pub type ConfigError {
  NoEnv(String)
}

pub fn get_api_key(key: String) {
  envoy.get(key) |> result.replace_error(NoEnv(key))
}
