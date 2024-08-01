#'convert a quarto file to audio using openAI API
#'@param input_files A list or a single filename of `.html.md` files to process.
#'@param api_key API key
#'@param model Available openAI TTS model: `tts-1`, `tts-1-hd`
#'@param voice The voice use when generating the audio. Supported voice are `alloy`, `echo`, `fable`, `onyx`, `nova`, `shimmer`
#'@param reponse_format The format to audio in. Defaults to mp3. Supported format are `mp3`, `opus`, `aac`, `flac`, `wav`, `pcm`
#'@param speed The speed of generated audio. Defaults to 1. Values are from `0.25` to `4.0`
#'@export


quartoaudio_openAI <- function(input_files,
                               api_key = get_openai_api_key(),
                               model = "tts-1",
                               voice = "alloy",
                               response_format = "mp3",
                               speed = 1,
                               ...) {
  # Check if the api_key is valid
  if (is.na(api_key) || is.null(api_key) || api_key == "") {
    stop("Please provide a valid API key.")
  }

  # Check if the model is valid
  if (model != "tts-1" && model != "tts-1-hd") {
    stop("Invalid model input.")
  }

  # Check if the voice is valid
  voice_list <- c("alloy", "echo", "fable", "onyx", "nova", "shimmer")
  if (!(voice %in% voice_list)) {
    stop("Invalid voice.")
  }

  # Ensure response_format is valid
  format_list <- c("mp3", "opus", "aac", "flac", "wav", "pcm")
  if (!(response_format %in% format_list)) {
    stop("Invalid audio format.")
  }

  # Ensure speed is valid
  if (speed < 0.25 || speed > 4.0) {
    stop("Invalid speed value.")
  }

  ENDPOINT <- "https://api.openai.com/v1/audio/speech"
  headers <- list(
    `Authorization` = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  # Ensure input_files is a list
  if (is.list(input_files)) {
    input_files <- unlist(input_files)
  }

  additional_params <- list(...)

  # Process each file
  for (file in input_files) {
    text_chunks <- process_docs(file_name = file)
    file_name <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(file)))
    output_dir <- file.path("audio", file_name)

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Process each chunk
    for (i in seq_along(text_chunks)) {
      payload <- list(
        model = model,
        voice = voice,
        input = text_chunks[[i]]
      )
      payload <- c(payload, additional_params)


      # other_information <- list(...)
      # payload <- c(payload, other_information)

      request <- httr2::request(ENDPOINT) |> httr2::req_headers(!!!headers) |> httr2::req_body_json(payload)
      response <- request |> httr2::req_perform()

      if (httr2::resp_status(response) == 200) {
        output_file <- file.path(output_dir, paste0("chunk_", i, ".mp3"))
        writeBin(httr2::resp_body_raw(response), output_file)
        message("Audio saved successfully to ", output_file)
      } else {
        stop(paste("Error:", httr2::resp_body_string(response)))
      }
    }
  }
}

# Helper function to get the OpenAI API key
get_openai_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(key, "")) {
    stop("No OPENAI_API_KEY is found, please supply with `api_key` arguments or with OPENAI_API_KEY env var")
  }
  key
}
