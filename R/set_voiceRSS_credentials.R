#' set VoiceRSS API credentials
#' @param key VoiceRSS API key

set_openAI_credentials <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your voiceRSS API key")
  }
  Sys.setenv("VOICERSS_API_KEY" = key)
}
