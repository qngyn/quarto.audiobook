set_playht_credentials <- function(user_id = NULL, key = NULL) {
  if (is.null(user_id)) {
    user_id <- askpass::askpass("Please enter your play.ht user_id")
  }
  Sys.setenv("PLAYHT_USER_ID" = user_id)

  if (is.null(key)) {
    key <- askpass::askpass("Please enter your play.ht secret key")
  }
  Sys.setenv("PLAYHT_API_KEY" = key)
}
