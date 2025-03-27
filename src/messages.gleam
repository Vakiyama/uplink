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

pub type Messages {
  Messages(first: UserMessage, rest: List(Message))
}

pub type LLMRequestOptions {
  AnthropicRequest(
    model: String,
    max_tokens: Int,
    system: String,
    messages: Messages,
    temperature: Float,
  )
}

// fetch("https://api.anthropic.com/v1/messages", {
//   method: "POST",
//   headers: {
//     "x-api-key": process.env.ANTHROPIC_API_KEY!,
//     "anthropic-version": "2023-06-01",
//     "content-type": "application/json",
//   },
//   body: stringified,
// }),

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
    content: List(AIMessage),
    model: String,
    stop_reason: StopReason,
    usage: Usage,
  )
}
