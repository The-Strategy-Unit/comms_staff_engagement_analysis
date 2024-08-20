#' Parse the raw survy data
#'
#' Handles column names split over two rows
#' Grabs q2
#' @param file full filepath to read in
parse_raw_survey <- function(file) {
  responses <- read_csv(file, show_col_types = FALSE)

  new_names <- responses[1, ] |> as.character()
  new_names[new_names == "NA"] <- NA
  new_names[which(is.na(new_names))] <- names(responses)[which(is.na(new_names))]

  n_skip <- which(!is.na(responses$UserID))[1]
  responses <- read_csv(file, skip = n_skip, col_names = new_names, show_col_types = FALSE)

  responses |>
    janitor::clean_names() |>
    pivot_longer(cols = starts_with("q2") & where(is.character), names_to = "q2_question", values_to = "q2_response") |>
    select(user_no, starts_with("q2")) |>
    mutate(date = substring(strsplit(file, "/")[[1]][length(strsplit(file, "/")[[1]])], 1, 7))
}
