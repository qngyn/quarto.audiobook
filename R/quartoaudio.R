#'convert a quarto file to audio
#'@param key API key
#'@param api TTS APIs (openAI, voiceRSS,)
#'@export language language for the audio file

source("R/quartoaudio_openAI.R")
source("R/quartoaudio_voiceRSS.R")
source("R/quartoaudio_playht.R")
quartoaudio <- function () {

  message("which API do you want to use? (openAI, voiceRSS, play.ht)")
  api_choice <- tolower(readLines(con=stdin(), n = 1))

  supported_api <- c("openai", "voicerss", "play.ht")
  if (!(api_choice %in% supported_api)) {
    message("Not in supported API. Please try again!")
  }
}
