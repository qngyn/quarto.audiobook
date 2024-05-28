#' set API credentials
#' @param api API name (openAI, voiceRSS, play.ht)

set_api_credentials <- function(api, ...) {
  supported_api <- c("openai", "voicerss", "play.ht")
  api <- tolower(api)

  if (api == "openai") {
    set_openAI_credentials (key)
  }

  else if (api == "voicerss") {

  }

  else if (api == "play.ht" ) {

  }
  else {
    stop("Not supported API! Please try again. ")
  }

}
