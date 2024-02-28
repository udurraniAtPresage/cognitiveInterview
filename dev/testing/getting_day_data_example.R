library(firebase)

sign.in <- function(email, password, api_key) {
  r <- httr::POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
                  httr::add_headers("Content-Type" = "application/json"),
                  body = jsonlite::toJSON(list(email = email, password = password, returnSecureToken = TRUE),auto_unbox=TRUE))
  return(httr::content(r))
}



ure <- sign.in("ag@oqfmaxhjb.ure", Sys.getenv("PASS"), Sys.getenv("FIREBASE_API_KEY"))

accessTokenu <- ure$idToken
emailu <- ure$email



library(dplyr)

PROJECT_NAME = "brpa-dev"
accessToken = accessTokenu
Day = "Day1"
SME = "Martin Smith"
Instructor = "Charles Xavier"
pilot_vec = c("XC34B", "P325X")


element_num <- 1

data_of_one_day <- get_ci_data_for_a_day(PROJECT_NAME,
                      accessToken,
                      Day,
                      SME,
                      Instructor,
                      pilot_vec)





all_events_data <- data_of_one_day |>
  filter(!grepl("isd|eoi", name))


isd_data <- data_of_one_day |>
  filter(grepl("isd", name))

stu_data <- data_of_one_day |>
  filter(grepl("eoi_stu", name))

ins_data <- data_of_one_day |>
  filter(grepl("eoi_inst", name))
