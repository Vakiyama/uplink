// what we need:
// a messages list; ideally, messages is typesafe (check rules for different apis)
//

// pub type Base64ImageString {
//    
// }

/// Anthropic's Content variants can all be modeled here as needed,
/// currently only supporting text
pub type Content {
  Text(text: String)
}

pub type AIMessage {
  AIMessage(content: Content)
}

pub type UserMessage {
  UserMessage(content: Content)
}

pub type Message {
  Assistant(AIMessage)
  User(UserMessage)
}

pub fn make_ai_message(content: String) {
  Assistant(AIMessage(Text(content)))
}

pub fn make_user_message(content: String) {
  User(UserMessage(Text(content)))
}

pub type LLMRequestOptions {
  AnthropicRequest(
    model: String,
    max_tokens: Int,
    system: String,
    messages: List(Message),
    temperature: Float,
  )
}

pub type StopReason {
  EndTurn
  MaxTokens
  StopSequence
  ToolUse
  Unknown
}

pub type Usage {
  Tokens(input: Int, output: Int)
}

pub type LLMResponse {
  AnthropicResponse(
    id: String,
    content: List(Message),
    model: String,
    stop_reason: StopReason,
    usage: Usage,
  )
}
