#'convert a quarto file to audio
#'@param api TTS APIs (openAI, voiceRSS, play.ht)
#' @export


# source("R/quartoaudio_openAI.R")
# source("R/quartoaudio_voiceRSS.R")
# source("R/quartoaudio_playht.R")

quartoaudio <- function (api,...) {
  api_choice <- tolower(api)
  args <- list(...)

  if (!"file_names" %in% names(args) || is.null(args$file_names)) {
    file_names <- list.files(path = ".", pattern = "\\.html\\.md$", full.names = TRUE)

    if (file.exists("_quarto.yml")) {
      file_names <- c(file_names, "_quarto.yml")
    }
    if (length(file_names) == 0) {
      stop("No .html.md files found in the current directory")
    } else {
      args$file_names <- file_names
    }
  }


  if (api == "openai") {
    if(!"api_key" %in% names(args) || is.null(args$api_key)) {
      args$api_key <- get_openai_api_key()
    }
    #default values for required params
    if (!"model"%in% names(args)) args$model <- "tts-1"
    if (!"voice" %in% names(args)) args$voice <- "alloy"
    quartoaudio_openAI(input_files = args$file_names, api_key = args$api_key, model = args$model, voice = args$voice)
  } else if (api == "voicerss") {
    if(!"api_key" %in% names(args) || is.null(args$api_key)) {
      args$api_key <- get_voicerss_api_key()
    }
    #default values for required params
    if (!"hl"%in% names(args)) args$hl <- "en-us"
    if (!"c"%in% names(args)) args$c <- "MP3"
    if (!"f"%in% names(args)) args$f <- "44khz_16bit_stereo"
    quartoaudio_voiceRSS(input_files = args$file_names, api_key = args$api_key, hl=args$hl, c=args$c, f=args$f)
  } else if (api == "play.ht") {
    #temporary hold
    # if (!"input" %in% names(args)
    #     || is.null(args$input)) {
    #   stop("play.ht requires 'input' ")
    # }

    if (!"user_id" %in% names(args)
        || is.null(args$user_id)) {
      args$user_id <- get_playht_user_id()
    }

    if (!"api_key" %in% names(args)
        || is.null(args$api_key)) {
      args$api_key <- get_playht_api_key()
    }

    if (!"voice"%in% names(args)) args$voice <- "s3://voice-cloning-zero-shot/d9ff78ba-d016-47f6-b0ef-dd630f59414e/female-cs/manifest.json"

    quartoaudio_playht(input_files = args$input, user_id = args$user_id, api_key = args$api_key, voice = args$voice)
  } else {
    stop("API is not supported")
  }
}

