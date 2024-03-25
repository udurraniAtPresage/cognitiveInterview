# Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)


# Load functions
source("dev/testing/functions.R")


# Sign in
signed_in_user <- sign_in(
  email = "udurrani@test.com",
  password = Sys.getenv("PASS"),
  api_key = Sys.getenv("FIREBASE_API_KEY")
  )

## Get access token
access_token <- signed_in_user$idToken


## Project name
PROJECT_NAME <- Sys.getenv("FIREBASE_PROJECT_ID")


# Get and combine all data in the "cognitive_interview" collection
## Get the names of all documents
all_doc_names_CI <- get_all_name_combos(
  accessToken = access_token,
  collection = "cognitive_interview",
  project_name = PROJECT_NAME
  )

all_days <- purrr::map(1:10, function(x) paste0("Day",x)) |> purrr::list_c()

## Get all data
all_data_CI <- purrr::map(
  all_doc_names_CI,
  ~ get_data_for_all_days_for_a_combo(access_token,
                                      matching_name = .x,
                                      project_name = PROJECT_NAME)
)



data_one_combo <- all_data_CI[[66]]

if (is.null(data_one_combo$Pilot2)){
  data_one_combo <- data_one_combo |>
    dplyr::relocate(
      Day, SME, Instructor, Aircraft,
      Pilot1, Pilot1_title, Pilot1_status,
      Event = name, Event_Start, Event_End,
      everything()
    )

  data_one_combo_pilot <- data_one_combo |>
    dplyr::select(Day, Event,
                  Pilot1, Pilot1_title, Pilot1_status) |>
    tidyr::pivot_longer(cols = c(Pilot1, Pilot2),
                        names_to = "Pilot_variable",
                        values_to = "ID") |>
    tidyr::pivot_longer(cols = c(Pilot1_title, Pilot2_title),
                        names_to = "Pilot_variable2",
                        values_to = "Title") |>
    tidyr::pivot_longer(cols = c(Pilot1_status, Pilot2_status),
                        names_to = "Pilot_variable3",
                        values_to = "Status") |>
    dplyr::mutate(Pilot_variable2 = gsub("_title", "", Pilot_variable2),
                  Pilot_variable3 = gsub("_status", "", Pilot_variable3)) |>
    dplyr::filter(Pilot_variable == Pilot_variable2,
                  Pilot_variable == Pilot_variable3) |>
    dplyr::select(-c(Pilot_variable2, Pilot_variable3)) |>
    dplyr::rename(Pilot = Pilot_variable) |>
    janitor::clean_names()


  data_one_combo_long <- data_one_combo |>
    dplyr::select(-c(
      Pilot1, Pilot1_title, Pilot1_status
    )) |>
    tidyr::pivot_longer(
      cols = c(dplyr::ends_with("p1"), dplyr::ends_with("p2")),
      names_to = "Construct",
      values_to = "Score"
    ) |>
    tidyr::pivot_longer(
      cols = c(dplyr::ends_with("comment")),
      names_to = "Construct2",
      values_to = "Comment"
    ) |>
    dplyr::mutate(Construct2 = gsub("_comment", "", Construct2)) |>
    dplyr::rename(general_comments = Notes) |>
    dplyr::filter(Construct == Construct2) |>
    dplyr::select(-Construct2) |>
    tidyr::extract(Construct, into = c("Construct", "pilot"), regex = "(.*)_(p\\d+)", remove = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      pilot = dplyr::case_match(
        pilot,
        "p1" ~ "Pilot1",
        "p2" ~ "Pilot2"
      )
    ) |>
    dplyr::left_join(
      data_one_combo_pilot, by = c("day", "event", "pilot")
    ) |>
    dplyr::relocate(day, sme, aircraft, instructor, pilot, id, title, status, everything())

} else {
  data_one_combo <- data_one_combo |>
    dplyr::relocate(
      Day, SME, Instructor, Aircraft,
      Pilot1, Pilot1_title, Pilot1_status,
      Pilot2, Pilot2_title, Pilot2_status,
      Event = name, Event_Start, Event_End,
      everything()
    )

  data_one_combo_pilot <- data_one_combo |>
    dplyr::select(Day, Event,
                  Pilot1, Pilot1_title, Pilot1_status,
                  Pilot2, Pilot2_title, Pilot2_status) |>
    tidyr::pivot_longer(cols = c(Pilot1, Pilot2),
                        names_to = "Pilot_variable",
                        values_to = "ID") |>
    tidyr::pivot_longer(cols = c(Pilot1_title, Pilot2_title),
                        names_to = "Pilot_variable2",
                        values_to = "Title") |>
    tidyr::pivot_longer(cols = c(Pilot1_status, Pilot2_status),
                        names_to = "Pilot_variable3",
                        values_to = "Status") |>
    dplyr::mutate(Pilot_variable2 = gsub("_title", "", Pilot_variable2),
                  Pilot_variable3 = gsub("_status", "", Pilot_variable3)) |>
    dplyr::filter(Pilot_variable == Pilot_variable2,
                  Pilot_variable == Pilot_variable3) |>
    dplyr::select(-c(Pilot_variable2, Pilot_variable3)) |>
    dplyr::rename(Pilot = Pilot_variable) |>
    janitor::clean_names()


  data_one_combo_long <- data_one_combo |>
    dplyr::select(-c(
      Pilot1, Pilot1_title, Pilot1_status,
      Pilot2, Pilot2_title, Pilot2_status
    )) |>
    tidyr::pivot_longer(
      cols = c(dplyr::ends_with("p1"), dplyr::ends_with("p2")),
      names_to = "Construct",
      values_to = "Score"
    ) |>
    tidyr::pivot_longer(
      cols = c(dplyr::ends_with("comment")),
      names_to = "Construct2",
      values_to = "Comment"
    ) |>
    dplyr::mutate(Construct2 = gsub("_comment", "", Construct2)) |>
    dplyr::rename(general_comments = Notes) |>
    dplyr::filter(Construct == Construct2) |>
    dplyr::select(-Construct2) |>
    tidyr::extract(Construct, into = c("Construct", "pilot"), regex = "(.*)_(p\\d+)", remove = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      pilot = dplyr::case_match(
        pilot,
        "p1" ~ "Pilot1",
        "p2" ~ "Pilot2"
      )
    ) |>
    dplyr::left_join(
      data_one_combo_pilot, by = c("day", "event", "pilot")
    ) |>
    dplyr::relocate(day, sme, aircraft, instructor, pilot, id, title, status, everything())

}





