library(httr)
library(jsonlite)
library(tidyverse)

source("R/functions.R")
PROJECT_NAME <- "brpa-dev"

sign_in <- function(email, password, api_key) {
  r <- httr::POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
                  httr::add_headers("Content-Type" = "application/json"),
                  body = jsonlite::toJSON(list(email = email, password = password, returnSecureToken = TRUE),auto_unbox=TRUE))
  return(httr::content(r))
}


ure <- sign_in("udurrani@test.com", Sys.getenv("PASS"), Sys.getenv("FIREBASE_API_KEY"))

accessTokenu <- ure$idToken
emailu <- ure$email


# get all data

all_paths <- c("Jacques_JJ_9232/Day3", "Jacques_JJ_9433/Day3",
               "Michel_GG_9232/Day4", "Michel_GG_9433_9232/Day4", "Michel_Michael_3575/Day3",
               "Michel_Michael_8473/Day3")

all_data <- list()

for (i in seq_along(all_paths)){
one_data <- get_data_from_firestore(db_endpoint = paste0("projects/", PROJECT_NAME,
                                             "/databases/(default)/documents/cognitive_interview/", all_paths[i]),
                        auth_token = accessTokenu)

all_data[[i]] <- one_data

}

select_and_rename_vars <- function(response) {
  foo <- jsonlite::fromJSON(httr::content(
    response,
    "text"
  )) |>
    purrr::pluck("documents") |>
    dplyr::select(-c(createTime, updateTime)) |>
    tidyr::unnest_wider(col = fields) |>
    tidyr::unnest_wider(col = dplyr::everything(), names_sep = "_")

  ## Clean column names
  # names(foo) <- stringr::str_extract(names(foo), "[a-zA-Z0-9]+")
  colnames(foo) <- gsub("_stringValue", "", colnames(foo))
  foo$name_1 <- basename(foo$name_1)
  foo
}

all_data_cleaned <- purrr::map(all_data, select_and_rename_vars)
all_data_cleaned <- set_names(x = all_data_cleaned, all_paths)



for (cd in 1:length(all_data_cleaned)){
  one_element <- all_data_cleaned[cd]
  name_combo <- strsplit(names(one_element), split = "/")[[1]][1]
  Day <- strsplit(names(one_element), split = "/")[[1]][2]

  df <- one_element[[1]]
  endpoint_cognitive_interview <- paste0("projects/", PROJECT_NAME, "/databases/(default)/documents/cognitiveInterview")

  for (j in 1:nrow(df)){
    df_row <- df[j, ]

    event_data_list <- list(
      fields = list(
        Functional_p1             = list("stringValue" = df_row$Functional_p1),
        Hierarchical_p1           = list("stringValue" = df_row$Hierarchical_p1),
        Task_Empirical_p1         = list("stringValue" = df_row$Task_Empirical_p1),
        Relational_p1             = list("stringValue" = df_row$Relational_p1),
        Environmental_p1          = list("stringValue" = df_row$Environmental_p1),
        Anticipatory_p1           = list("stringValue" = df_row$Anticipatory_p1),
        Compensatory_p1           = list("stringValue" = df_row$Compensatory_p1),
        Affective_p1              = list("stringValue" = df_row$Affective_p1),
        Critical_p1               = list("stringValue" = df_row$Critical_p1),

        Functional_p1_comment     = list("stringValue" = df_row$Functional_p1_comment),
        Hierarchical_p1_comment   = list("stringValue" = df_row$Hierarchical_p1_comment),
        Task_Empirical_p1_comment = list("stringValue" = df_row$Task_Empirical_p1_comment),
        Relational_p1_comment     = list("stringValue" = df_row$Relational_p1_comment),
        Environmental_p1_comment  = list("stringValue" = df_row$Environmental_p1_comment),
        Anticipatory_p1_comment   = list("stringValue" = df_row$Anticipatory_p1_comment),
        Compensatory_p1_comment   = list("stringValue" = df_row$Compensatory_p1_comment),
        Affective_p1_comment      = list("stringValue" = df_row$Affective_p1_comment),
        Critical_p1_comment       = list("stringValue" = df_row$Critical_p1_comment),

        Functional_p2             = list("stringValue" = df_row$Functional_p2),
        Hierarchical_p2           = list("stringValue" = df_row$Hierarchical_p2),
        Task_Empirical_p2         = list("stringValue" = df_row$Task_Empirical_p2),
        Relational_p2             = list("stringValue" = df_row$Relational_p2),
        Environmental_p2          = list("stringValue" = df_row$Environmental_p2),
        Anticipatory_p2           = list("stringValue" = df_row$Anticipatory_p2),
        Compensatory_p2           = list("stringValue" = df_row$Compensatory_p2),
        Affective_p2              = list("stringValue" = df_row$Affective_p2),
        Critical_p2               = list("stringValue" = df_row$Critical_p2),

        Functional_p2_comment     = list("stringValue" = df_row$Functional_p2_comment),
        Hierarchical_p2_comment   = list("stringValue" = df_row$Hierarchical_p2_comment),
        Task_Empirical_p2_comment = list("stringValue" = df_row$Task_Empirical_p2_comment),
        Relational_p2_comment     = list("stringValue" = df_row$Relational_p2_comment),
        Environmental_p2_comment  = list("stringValue" = df_row$Environmental_p2_comment),
        Anticipatory_p2_comment   = list("stringValue" = df_row$Anticipatory_p2_comment),
        Compensatory_p2_comment   = list("stringValue" = df_row$Compensatory_p2_comment),
        Affective_p2_comment      = list("stringValue" = df_row$Affective_p2_comment),
        Critical_p2_comment       = list("stringValue" = df_row$Critical_p2_comment),

        SME                       = list("stringValue" = df_row$SME),
        Pilot1                    = list("stringValue" = df_row$Pilot1),
        Pilot2                    = list("stringValue" = df_row$Pilot2),
        Aircraft                  = list("stringValue" = df_row$Aircraft),
        Pilot1_status             = list("stringValue" = df_row$Pilot1_status),
        Pilot2_status             = list("stringValue" = df_row$Pilot2_status),
        Pilot1_title              = list("stringValue" = df_row$Pilot1_title),
        Pilot2_title              = list("stringValue" = df_row$Pilot2_title),
        Notes                     = list("stringValue" = df_row$Notes),
        Instructor                = list("stringValue" = df_row$Instructor),
        Event_Start               = list("stringValue" = df_row$Event_Start),
        Event_End                 = list("stringValue" = df_row$Event_End)
      )
    )

    contains_null <- function(x) {
      any(sapply(x, is.null))
    }

    contains_NA <- function(x) {
      any(sapply(x, is.na))
    }

    event_data_list <- lapply(event_data_list, function(x) Filter(Negate(contains_null), x))
    event_data_list <- lapply(event_data_list, function(x) Filter(Negate(contains_NA), x))

    event_data_list <- toJSON(event_data_list, auto_unbox = TRUE)


    name_combo2 <- paste0(name_combo, "_", Day)

    write_request_ci <- post_data_to_firestore(
      db_endpoint = paste0(endpoint_cognitive_interview, "?documentId=", name_combo2, "_", gsub("\\s", "_", df_row$name_1)),
      data = event_data_list,
      auth_token = accessTokenu
    )

    print(write_request_ci)
  }
}

