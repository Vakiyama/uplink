import gleam/io
import gleam/list
import models

import effect

// import messages.{type Content}

pub fn main() {
  io.println("Hello from uplink!")
  use models <- effect.perform(models.get_models())
  case models {
    Ok(models) -> {
      use model <- list.each(models)
      io.println("Model: " <> model.display_name)
    }
    Error(err) -> {
      case err {
        models.DecodeError -> io.println("Decode err")
        _ -> io.println("other err")
      }
    }
  }
}
// we want to be able to:
//  1. Choose a model (should be given a list through the models list api, save/cache the model to use when first opened)
//  2. We can start with a simple chat, extend this by (in order):
//    - having code highlighting in responses, or at minimum parsing markdown into readable responses
//    - having projects (for grouping chats)
//    - choosing between multiple llms instead of just claude, all in the same project?
//    - uploading or using images, files (text files?)
//    - send github links? (or even search github would be sick)
//    - use a browser
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
