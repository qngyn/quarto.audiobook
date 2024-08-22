process_yaml_file <- function(file_name) {
  book_info <- yaml::read_yaml(file_name)$book
  cleaned_chapters <- gsub("\\.qmd$", "", book_info$chapters)
  book_info_flatten <- paste0(
    "title ", book_info$title,
    ", author ", book_info$author,
    ", date ", book_info$date,
    ", chapters ", paste(cleaned_chapters, collapse = ", ")
  )

  return (book_info_flatten)
}
