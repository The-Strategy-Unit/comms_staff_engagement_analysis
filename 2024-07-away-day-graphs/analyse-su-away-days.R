source("functions.R")

config <- config::get(file = "config.yml")
folder <- file.path(config$sharepoint, config$sharepoint_comms, "Away Day", "feedback")
files <- list.files(folder, pattern = ".csv", full.names = TRUE)

# Question 1 for May 2024

responses_2024 <- readr::read_csv(file.path(folder, "2024-05.csv"), skip = 1)

likert_data <-
  responses_2024 |>
  janitor::clean_names() |>
  dplyr::select(tidyselect::starts_with("q1_")) |>
  dplyr::select(-"q1_11_any_comments_on_any_of_the_elements") |>
  dplyr::mutate(across(everything(), ~ factor(.x, levels = 1:10))) |>
  as.data.frame()

nice_names <-
  names(responses_2024)[9:18] |>
  stringr::str_sub(start = 7)

names(likert_data) <- nice_names

question1_plot <- plot(likert::likert(likert_data))
ggsave("question1.png", question1_plot, width = 12, height = 6)

# Question 2 across all away days

all <- purrr::map(files, parse_raw_survey) |>
  purrr::list_rbind()

question2 <-
  all |>
  mutate(q2_question = str_sub(q2_question, 1, 4)) |>
  filter(q2_question != "q2_7") |>
  pivot_wider(names_from = q2_question, values_from = q2_response) |>
  dplyr::mutate(across(starts_with("q2"), ~ factor(.x, levels = c("Not enough", "About right", "Too much")))) |>
  drop_na() |>
  dplyr::select(-user_no) |>
  as.data.frame()


names(question2) <- c(
  "Date", "Information/agenda shared before the day",
  "Opportunities for informal catch ups",
  "Sharing with / learning about others across the Unit",
  "General information concerning the Strategy Unit",
  "Hearing from leadership",
  "General pace of the day"
)

question2_plot <- plot(likert::likert(select(question2, -Date), grouping = question2$Date))
ggplot2::ggsave("question2.png", question2_plot, width = 12, height = 6)
