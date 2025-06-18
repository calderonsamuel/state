.get_context_for_llm <- function() {
  r_files <- list.files(c("R", "tests/testthat"), full.names = TRUE, pattern = "\\.R$")
  all_contents <- vapply(r_files, .get_file_content, "character")

  paste0(all_contents, collapse = "\n\n") |>
    clipr::write_clip()
}

.get_file_content <- function(file) {
  header <- paste0("# filename: ", file)

  all_lines <- readLines(file) |>
    paste0(collapse = "\n")

  paste(header, all_lines, sep = "\n\n")
}
