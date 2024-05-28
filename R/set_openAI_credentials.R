#' set OpenAI API credentials
#' @param key openAI API key

set_openAI_credentials <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your openAI API key")
  }
  Sys.setenv("OPENAI_API_KEY" = key)
}
