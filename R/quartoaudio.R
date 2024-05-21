#'convert a quarto file to audio
#'@param key API key
#'@param api TTS APIs (openAI, voiceRSS,)
#'@export language language for the audio file

source("R/quartoaudio_openAI.R")
source("R/quartoaudio_voiceRSS.R")
source("R/quartoaudio_playht.R")
quartoaudio <- function (input) {
  #scanning the files to get quarto files.

  #choosing API
  message("choose an API (openAI, voiceRSS, play.ht)")
  repeat {
    # message("choose an API (openAI, voiceRSS, play.ht)")
    api_choice <- tolower(readline(prompt = "Enter API's name (or type 'exit' to quit): "))
    supported_api <- c("openai", "voicerss", "play.ht")

    if (api_choice == 'exit') {
      return(invisible())
    }

    if (api_choice %in% supported_api) {
      break
    } else {
      message("Not in supported API . Please try again!")
    }
  }

  if (api_choice == "openai") {
    api_key <- readline(prompt = "Enter API's key: ")
    model <- readline(prompt = "Enter model's choice (default: 'tts-1'): ")
    voice <- readline(prompt = "Enter voice's choice (default: 'alloy'): ")

    if (model == "") {
      model <- 'tts-1'
    }

    if (voice == "") {
      voice <- "alloy"
    }

    quartoaudio_openAI(input = input, api_key = api_key, model = model, voice = voice)

  } else if (api_choice == "voicerss") {
    api_key <- readline(prompt = "Enter API's key: ")
    language <- readline(prompt = "Enter language (default: 'en-us'): ")
    voices <- readline(prompt = "Enter voice (default: 'Linda'): ")
    audio_codecs <- readline(prompt = "Enter audio codecs (default: 'MP3'): ")
    audio_format <- readline(prompt = "Enter audio format (default: '44khz_16bit_stereo'): ")

    if (language == "") {
      language <- "en-us"
    }

    if (voices == "") {
      voices <- "Linda"
    }

    if (audio_codecs == "") {
      audio_codecs <- "MP3"
    }

    if (audio_format == "") {
      audio_format <- "44khz_16bit_stereo"
    }

    quartoaudio_voiceRSS(input, api_key = api_key, hl=language, c=audio_codecs, f=audio_format)

  } else if (api_choice == "play.ht") {
    user_id <- readline(prompt = "Enter UserID: ")
    secret_key <- readline(prompt = "Enter secret key: ")
    quartoaudio_playht(input, user_id = user_id, secret_key = secret_key)
  }

}
