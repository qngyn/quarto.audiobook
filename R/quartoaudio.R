#'convert a quarto file to audio
#'@param api TTS APIs (openAI, voiceRSS, play.ht)

# source("R/quartoaudio_openAI.R")
# source("R/quartoaudio_voiceRSS.R")
# source("R/quartoaudio_playht.R")

quartoaudio <- function (api,...) {
  api_choice <- tolower(api)
  args <- list(...)

  if (api == "openai") {
    #temporary hold
    if (!"input" %in% names(args) || is.null(args$input) || !"api_key" %in% names(args) || is.null(args$api_key)) {
      stop("OpenAI API requires 'input' and 'api_key'")
    }

    #default values for required params
    if (!"model"%in% names(args)) args$model <- "tts-1"
    if (!"voice" %in% names(args)) args$voice <- "alloy"
    quartoaudio_openAI(input = args$input, api_key = args$api_key, model = args$model, voice = args$voice)
  }

  else if (api == "voicerss") {
    #temporary hold
    if (!"input" %in% names(args)
        || is.null(args$input)
        || !"api_key" %in% names(args)
        || is.null(args$api_key)) {
      stop("VoiceRSS requires 'input' and 'api_key'")
    }

    #default values for required params
    if (!"hl"%in% names(args)) args$hl <- "en-us"
    if (!"c"%in% names(args)) args$c <- "MP3"
    if (!"f"%in% names(args)) args$f <- "44khz_16bit_stereo"
    quartoaudio_voiceRSS(input = args$input, api_key = args$api_key, hl=args$hl, c=args$c, f=args$f)
  }

  else if (api=="play.ht") {
    #temporary hold
    if (!"input" %in% names(args)
        || is.null(args$input)) {
      stop("VoiceRSS requires 'input' ")
    }

    if (!"user_id" %in% names(args)
        || is.null(args$user_id)) {
      stop("VoiceRSS requires 'user_id' ")
    }

    if (!"api_key" %in% names(args)
        || is.null(args$api_key)) {
      stop("VoiceRSS requires 'api_key' ")
    }

    if (!"voice"%in% names(args)) args$voice <- "s3://voice-cloning-zero-shot/d9ff78ba-d016-47f6-b0ef-dd630f59414e/female-cs/manifest.json"

    quartoaudio_playht(input = args$input, user_id = args$user_id, secret_key = args$api_key, voice = args$voice)
  }

  else {
    stop("API is not supported")
  }
}
