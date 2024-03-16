# UI functions ------------------------------------------------------------

# create_day_page <- function(x) {
#   number <- as.numeric(gsub("[^0-9]", "", x))
#   navset_card_pill(
#       id = paste0("pill_", number),
#       nav_panel(title = "All Events", mod_part_A_ui(paste0("part_A_", number))),
#       nav_panel(title = "Pilot Debrief", mod_part_B_ui(paste0("part_B_", number))),
#       nav_panel(title = "Instructor Interview", mod_part_C_ui(paste0("part_C_", number)))
#     )
# }


#' Get all name combos
#'
#' @param accessToken Firebase access token.
#' @param project_name Firebase Project ID.
#'
#' @return A vector of name combos.
#' @export
get_all_name_combos <- function(accessToken, project_name = PROJECT_NAME){
  endpoint_cognitive_interview <- paste0("projects/", project_name, "/databases/(default)/documents/cognitive_interview")
  request <- get_data_from_firestore(
    db_endpoint = endpoint_cognitive_interview,
    accessToken
  )

  all_names <- (jsonlite::fromJSON(httr::content(
    request,
    "text"
  )))$documents |>
    dplyr::mutate(name = basename(name)) |>
    dplyr::select(name)

  all_names |> dplyr::pull(name)
}




#' Sanitize event name
#'
#' @param event_name name of workbook.
#'
#' @return sanitized event name.
#' @export
sanitized_sheet_name <- function(event_name) {
  # Remove special characters using regular expression
  sanitized_name <- gsub("[^A-Za-z0-9]", "", event_name)

  # Truncate sheet name to maximum allowed length
  truncated_name <- substr(sanitized_name, 1, 31)
  truncated_name
}



#' Add worksheet with sanitized name
#'
#' @param workbook name of workbook.
#' @param event_name event name.
#' @param ... params of addWorksheet.
#'
#' @return adds worksheet.
#' @export
addWorksheetWithSanitizedName <- function(workbook, event_name, ...) {
  tryCatch(
    {
      sanitized_event_name <- sanitized_sheet_name(event_name)

      addWorksheet(workbook, sheetName = sanitized_event_name, ...)
    },
    error = function(e) {
      # Print an error message or take other appropriate action
      cat("An error occurred:", e$message, "\n")

    }
  )
}


#' Sign in
#'
#' @param email email of user.
#' @param password password of user.
#' @param api_key firebase api key.
#'
#' @return signed in user.
#' @export
sign_in <- function(email, password, api_key) {
  r <- httr::POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
                  httr::add_headers("Content-Type" = "application/json"),
                  body = jsonlite::toJSON(list(email = email, password = password, returnSecureToken = TRUE),auto_unbox=TRUE))
  return(httr::content(r))
}


#' Create Day UI
#'
#' @param x Day.
#'
#' @return UI elements.
#' @export
create_day_page <- function(x) {
  number <- as.numeric(gsub("[^0-9]", "", x))
  navset_card_pill(
    id = paste0("pill_", number),
    nav_panel(
      title = "All Events",
      mod_all_events_ui(paste0("all_events_", number))
      ),
    nav_panel(
      title = "Instructor-Student Debrief",
      mod_IS_ui(paste0("IS_", number))
     ),
    nav_panel(
      title = "Student Interview",
      mod_stu_interview_ui(paste0("stu_interview_", number))
      ),
    nav_panel(
      title = "Instructor Interview",
      mod_inst_interview_ui(paste0("inst_interview_", number))
      )
  )
}





# REST API functions ------------------------------------------------------
## Thanks to: https://gabrielcp.medium.com/introduction-to-working-with-firestore-in-r-99443489b01b

#' Get data from Firestore
#'
#' @param db_endpoint Character. Path to a document on Firestore.
#' @param auth_token Character. Token to access Firestore.
#'
#' @return Response. Data in JSON format.
#' @export
get_data_from_firestore <- function(db_endpoint, auth_token) {
  r <- httr::GET(
    sprintf("https://firestore.googleapis.com/v1beta1/%s", db_endpoint),
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", auth_token)
    )
  )
  return(r)
}



#' Post user data to Firestore
#'
#' @param db_endpoint Character. Path to a document on Firestore.
#' @param data JSON. Data in json format.
#' @param auth_token Character. Token to access Firestore.
#'
#' @return Response. It tells you the details of the posted data.
#' @export
post_data_to_firestore <- function(db_endpoint, data, auth_token) {
  r <- httr::POST(
    url = sprintf("https://firestore.googleapis.com/v1beta1/%s", db_endpoint),
    config = httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", auth_token)
    ),
    body = data
  )
  return(r)
}


#' Update current data on Firestore with new data
#'
#' @param db_endpoint Character. Path to a collection on Firestore.
#' @param document_id Character. ID of the document of which the data is being updated.
#' @param data JSON. Data in json format.
#' @param auth_token Character. Token to access Firestore.
#'
#' @return Response. It tells you the details of the posted data.
#' @export
update_data_on_firestore <- function(db_endpoint, document_id, data, auth_token) {
  r <- httr::PATCH(
    url = sprintf("https://firestore.googleapis.com/v1beta1/%s/%s", db_endpoint, document_id),
    config = add_headers("Content-Type" = "application/json",
                         "Authorization" = paste("Bearer", auth_token)),
    body = data
  )
  return(r)
}



#' Delete data on Firestore
#'
#' @param db_endpoint Character. Path to a collection on Firestore.
#' @param auth_token Character. Token to access Firestore.
#'
#' @return Response. It tells you whose and when the data was deleted.
#' @export
delete_data_on_firestore<- function(db_endpoint, auth_token) {
  r <- httr::DELETE(sprintf("https://firestore.googleapis.com/v1beta1/%s", db_endpoint),
                    httr::add_headers("Content-Type" = "application/json",
                                      "Authorization" = paste("Bearer", auth_token)))
  return(r)
}






# Send event data to Firestore --------------------------------------------

#' Send event data to Firestore
#'
#' @param PROJECT_NAME Character. Name of Firebase project.
#' @param accessToken Character. Access token.
#' @param Day Character. In the format Day<num> where num is 1, 2, etc.
#' @param SME Character. Name of Subject Matter Expert.
#' @param Instructor Character. Name of Instructor.
#' @param Pilot1 Character. Name of pilot separated by space.
#' @param Pilot2 Character. Name of pilot separated by space.
#' @param aircraft Aircraft Type.
#' @param pilot1_status Character. PF or PM.
#' @param pilot1_title Character. Captain or First Officer.
#' @param pilot2_status Character. PF or PM.
#' @param pilot2_title Character. Captain or First Officer.
#' @param event_name Character. Name of event. Examples: "event1", "event_of_interest_candidate", "event_of_interest_instructor".
#' @param constructs_inputs_p1 Character. Responses to 9 constructs by SME for the event for Pilot1.
#' @param text_inputs_p1 Character. Text responses of SMEs for Pilot1.
#' @param constructs_inputs_p2 Character. Responses to 9 constructs by SME for the event for Pilot2.
#' @param text_inputs_p2 Character. Text responses of SMEs for Pilot2.
#' @param notes Character. SME notes about the event.
#' @param event_start Character. Start of an event.
#' @param event_end Character. End of an event.
#'
#' @return Sends data to Firestore and returns an object of class response.
#' @export
post_event_data_ci <- function(PROJECT_NAME, accessToken, Day,
                               SME, Instructor, Pilot1, Pilot2,
                               aircraft,
                               pilot1_status, pilot1_title,
                               pilot2_status, pilot2_title,
                               event_name,
                               constructs_inputs_p1, text_inputs_p1,
                               constructs_inputs_p2, text_inputs_p2,
                               notes,
                               event_start, event_end){

  contains_null <- function(x) {
    any(sapply(x, is.null))
  }

  # Create the initial document name based on SME, instructor, and pilot names

  if (!is.null(Pilot2)){

  Pilots = paste(strsplit(Pilot1, " ")[[1]][1],
                 strsplit(Pilot2, " ")[[1]][1])

  name_combo <- paste0(
    strsplit(x = SME, split = " ")[[1]][1],
    "_",
    strsplit(x = Instructor, split = " ")[[1]][1],
    "_",
    strsplit(x = Pilots, split = " ")[[1]][1],
    "_",
    strsplit(x = Pilots, split = " ")[[1]][2]
  )

  # Create event data list
  event_data_list <- list(
    fields = list(
      Functional_p1 = list("stringValue" = constructs_inputs_p1[1]),
      Hierarchical_p1 = list("stringValue" = constructs_inputs_p1[2]),
      Task_Empirical_p1 = list("stringValue" = constructs_inputs_p1[3]),
      Relational_p1 = list("stringValue" = constructs_inputs_p1[8]),
      Environmental_p1 = list("stringValue" = constructs_inputs_p1[9]),
      Anticipatory_p1 = list("stringValue" = constructs_inputs_p1[6]),
      Compensatory_p1 = list("stringValue" = constructs_inputs_p1[7]),
      Affective_p1 = list("stringValue" = constructs_inputs_p1[5]),
      Critical_p1 = list("stringValue" = constructs_inputs_p1[4]),

      Functional_p1_comment = list("stringValue" = text_inputs_p1[1]),
      Hierarchical_p1_comment = list("stringValue" = text_inputs_p1[2]),
      Task_Empirical_p1_comment = list("stringValue" = text_inputs_p1[3]),
      Relational_p1_comment = list("stringValue" = text_inputs_p1[8]),
      Environmental_p1_comment = list("stringValue" = text_inputs_p1[9]),
      Anticipatory_p1_comment = list("stringValue" = text_inputs_p1[6]),
      Compensatory_p1_comment = list("stringValue" = text_inputs_p1[7]),
      Affective_p1_comment = list("stringValue" = text_inputs_p1[5]),
      Critical_p1_comment = list("stringValue" = text_inputs_p1[4]),

      Functional_p2 = list("stringValue" = constructs_inputs_p2[1]),
      Hierarchical_p2 = list("stringValue" = constructs_inputs_p2[2]),
      Task_Empirical_p2 = list("stringValue" = constructs_inputs_p2[3]),
      Relational_p2 = list("stringValue" = constructs_inputs_p2[8]),
      Environmental_p2 = list("stringValue" = constructs_inputs_p2[9]),
      Anticipatory_p2 = list("stringValue" = constructs_inputs_p2[6]),
      Compensatory_p2 = list("stringValue" = constructs_inputs_p2[7]),
      Affective_p2 = list("stringValue" = constructs_inputs_p2[5]),
      Critical_p2 = list("stringValue" = constructs_inputs_p2[4]),

      Functional_p2_comment = list("stringValue" = text_inputs_p2[1]),
      Hierarchical_p2_comment = list("stringValue" = text_inputs_p2[2]),
      Task_Empirical_p2_comment = list("stringValue" = text_inputs_p2[3]),
      Relational_p2_comment = list("stringValue" = text_inputs_p2[8]),
      Environmental_p2_comment = list("stringValue" = text_inputs_p2[9]),
      Anticipatory_p2_comment = list("stringValue" = text_inputs_p2[6]),
      Compensatory_p2_comment = list("stringValue" = text_inputs_p2[7]),
      Affective_p2_comment = list("stringValue" = text_inputs_p2[5]),
      Critical_p2_comment = list("stringValue" = text_inputs_p2[4]),

      SME = list("stringValue" = SME),
      Pilot1 = list("stringValue" = Pilot1),
      Pilot2 = list("stringValue" = Pilot2),
      Aircraft = list("stringValue" = aircraft),
      Pilot1_status = list("stringValue" = pilot1_status),
      Pilot2_status = list("stringValue" = pilot2_status),
      Pilot1_title = list("stringValue" = pilot1_title),
      Pilot2_title = list("stringValue" = pilot2_title),
      # Pilots = list("arrayValue" = c(Pilot1, Pilot2)),
      Notes = list("stringValue" = notes),
      Instructor = list("stringValue" = Instructor),
      Event_Start = list("stringValue" = event_start),
      Event_End = list("stringValue" = event_end)
    )
  )

  # Remove elements containing NULL values
  event_data_list <- lapply(event_data_list, function(x) Filter(Negate(contains_null), x))

  event_data_list <- toJSON(event_data_list, auto_unbox = TRUE)


  } else {
    Pilots = paste(strsplit(Pilot1, " ")[[1]][1])

    name_combo <- paste0(
      strsplit(x = SME, split = " ")[[1]][1],
      "_",
      strsplit(x = Instructor, split = " ")[[1]][1],
      "_",
      strsplit(x = Pilots, split = " ")[[1]][1]
    )


    # Create event data list
    event_data_list <- list(
      fields = list(
        Functional_p1 = list("stringValue" = constructs_inputs_p1[1]),
        Hierarchical_p1 = list("stringValue" = constructs_inputs_p1[2]),
        Task_Empirical_p1 = list("stringValue" = constructs_inputs_p1[3]),
        Relational_p1 = list("stringValue" = constructs_inputs_p1[8]),
        Environmental_p1 = list("stringValue" = constructs_inputs_p1[9]),
        Anticipatory_p1 = list("stringValue" = constructs_inputs_p1[6]),
        Compensatory_p1 = list("stringValue" = constructs_inputs_p1[7]),
        Affective_p1 = list("stringValue" = constructs_inputs_p1[5]),
        Critical_p1 = list("stringValue" = constructs_inputs_p1[4]),

        Functional_p1_comment = list("stringValue" = text_inputs_p1[1]),
        Hierarchical_p1_comment = list("stringValue" = text_inputs_p1[2]),
        Task_Empirical_p1_comment = list("stringValue" = text_inputs_p1[3]),
        Relational_p1_comment = list("stringValue" = text_inputs_p1[8]),
        Environmental_p1_comment = list("stringValue" = text_inputs_p1[9]),
        Anticipatory_p1_comment = list("stringValue" = text_inputs_p1[6]),
        Compensatory_p1_comment = list("stringValue" = text_inputs_p1[7]),
        Affective_p1_comment = list("stringValue" = text_inputs_p1[5]),
        Critical_p1_comment = list("stringValue" = text_inputs_p1[4]),


        SME = list("stringValue" = SME),
        Pilot1 = list("stringValue" = Pilot1),
        Aircraft = list("stringValue" = aircraft),
        Pilot1_status = list("stringValue" = pilot1_status),
        Pilot1_title = list("stringValue" = pilot1_title),
        # Pilot2 = list("stringValue" = Pilot2),
        # Pilots = list("arrayValue" = c(Pilot1, Pilot2)),
        Notes = list("stringValue" = notes),
        Instructor = list("stringValue" = Instructor),
        Event_Start = list("stringValue" = event_start),
        Event_End = list("stringValue" = event_end)
      )
    )
    # Remove elements containing NULL values
    event_data_list <- lapply(event_data_list, function(x) Filter(Negate(contains_null), x))

    event_data_list <- toJSON(event_data_list, auto_unbox = TRUE)

}




  # Write data
  endpoint_cognitive_interview <- paste0("projects/", PROJECT_NAME, "/databases/(default)/documents/cognitive_interview")

  write_request_ci1 <- post_data_to_firestore(
    db_endpoint = paste0(endpoint_cognitive_interview, "?documentId=",  name_combo),
    data = NULL,
    auth_token = accessToken
  )
  ## overwriting data if already exists
  if (write_request_ci1$status_code == 409L) {
    update_request_ci1 <- update_data_on_firestore(
      db_endpoint = paste0(endpoint_cognitive_interview),
      document_id = name_combo,
      data = NULL,
      auth_token = accessToken
    )

  }

  write_request_ci2 <- post_data_to_firestore(
    db_endpoint = paste0(endpoint_cognitive_interview, "/", name_combo, "/", Day, "?documentId=", gsub("\\s", "_", event_name)),
    data = event_data_list,
    auth_token = accessToken
  )


  ## overwriting data if already exists
  if (write_request_ci2$status_code == 409L) {
    update_request_ci2 <- update_data_on_firestore(
      db_endpoint = paste0(endpoint_cognitive_interview, "/", name_combo, "/", Day),
      document_id = gsub("\\s", "_", event_name),
      data = event_data_list,
      auth_token = accessToken
    )
    return(update_request_ci2)
  } else{
    return(write_request_ci2)
  }
}



















# Data cleaning functions -------------------------------------------------

#' Get and clean single event data for a day.
#'
#' @param PROJECT_NAME Character. Firebase project name.
#' @param accessToken Character. Firebase access token.
#' @param Day Character. For example, "Day1", "Day2", etc.
#' @param event_name Character. Name of the event for which data is requested.
#' @param SME Character. Name of Subject Matter Expert.
#' @param Instructor Character. Name of Instructor.
#' @param Pilot1 Character. Name of Pilot 1.
#' @param Pilot2 Character. Name of Pilot 2.
#'
#' @return A dataframe with event data.
#' @export
get_ci_event_data_for_a_day <- function(PROJECT_NAME, accessToken, Day, event_name,
                                        SME, Instructor, Pilot1, Pilot2){

  # Get the names in the format that the data is on Firestore
  Pilots <- paste(strsplit(Pilot1, " ")[[1]][1],
                  strsplit(Pilot2, " ")[[1]][1])

  name_combo <- paste0(
    strsplit(x = SME, split = " ")[[1]][1],
    "_",
    strsplit(x = Instructor, split = " ")[[1]][1],
    "_",
    strsplit(x = Pilots, split = " ")[[1]][1],
    "_",
    strsplit(x = Pilots, split = " ")[[1]][2]
  )

  # Endpoint
  endpoint_cognitive_interview <- paste0("projects/", PROJECT_NAME, "/databases/(default)/documents/cognitive_interview")
  combo_endpoint <- paste0(endpoint_cognitive_interview, "/", name_combo, "/", Day, "/", event_name)

  # Get data
  request <- get_data_from_firestore(db_endpoint = combo_endpoint,
                                     auth_token = accessToken)


  ## Return NULL if data does not exist
  if (request$status_code != 200L | !"documents" %in% names(jsonlite::fromJSON(httr::content(
    request,
    "text"
  )))) {
    return(NULL)
  } else {
    ci_results <- jsonlite::fromJSON(httr::content(
      request,
      "text"
    )) |>
      purrr::keep_at(c("fields")) |>
      purrr::list_flatten() |>
      purrr::list_simplify() |>
      tibble::as_tibble()


    column_names <- gsub("^fields_", "\\1", names(ci_results))

    names(ci_results) <- column_names

    ci_results
  }

}





#' Get and clean data for a day (all events).
#'
#' @param PROJECT_NAME Character. Firebase project name.
#' @param accessToken Character. Firebase access token.
#' @param Day Character. For example, "Day1", "Day2", etc.
#' @param SME Character. Name of Subject Matter Expert.
#' @param Instructor Character. Name of Instructor.
#' @param pilot_vec Character. Names of Pilot 1 and Pilot 2.
#'
#' @return A dataframe with event data.
#' @export
get_ci_data_for_a_day <- function(PROJECT_NAME, accessToken, Day,
                                  SME, Instructor, pilot_vec){

  if (length(pilot_vec) == 2){

    # Get the names in the format that the data is on Firestore
    Pilots = paste(strsplit(pilot_vec[1], " ")[[1]][1],
                   strsplit(pilot_vec[2], " ")[[1]][1])

    name_combo <- paste0(
      strsplit(x = SME, split = " ")[[1]][1],
      "_",
      strsplit(x = Instructor, split = " ")[[1]][1],
      "_",
      strsplit(x = Pilots, split = " ")[[1]][1],
      "_",
      strsplit(x = Pilots, split = " ")[[1]][2]
    )
  } else {
    # Get the names in the format that the data is on Firestore
    Pilots = paste(strsplit(pilot_vec[1], " ")[[1]][1])

    name_combo <- paste0(
      strsplit(x = SME, split = " ")[[1]][1],
      "_",
      strsplit(x = Instructor, split = " ")[[1]][1],
      "_",
      strsplit(x = Pilots, split = " ")[[1]][1]
    )
  }

  # Endpoint
  endpoint_cognitive_interview <- paste0("projects/", PROJECT_NAME, "/databases/(default)/documents/cognitive_interview")
  combo_endpoint <- paste0(endpoint_cognitive_interview, "/", name_combo, "/", Day)

  # Get data
  request <- get_data_from_firestore(db_endpoint = combo_endpoint,
                                     auth_token = accessToken)


  ## Return NULL if data does not exist
  if (request$status_code != 200L | !"documents" %in% names(jsonlite::fromJSON(httr::content(
    request,
    "text"
  )))) {
    return(NULL)
  } else {
    ci_results <- jsonlite::fromJSON(httr::content(
      request,
      "text"
    ))$documents |>
      dplyr::select(-c(createTime, updateTime)) |>
      dplyr::mutate(name = basename(name)) |>
      tidyr::unnest_wider(col = fields) |>
      tidyr::unnest_wider(col = everything(), names_sep = "_") |>
      dplyr::rename(name = name_1)


    column_names <- gsub("fields_(\\w+)_.*Value", "\\1", gsub("_stringValue", "", names(ci_results)))

    names(ci_results) <- column_names

    ci_results
  }


}










#' Version 2 of get interview data for a day
#'
#' @param accessToken Firebase token.
#' @param matching_name Combination of SME_Instructor_Pilot1_Pilot2
#' @param Day Day of simulation
#' @param project_name Firebase Project ID
#'
#' @return Dataframe with all events of a day for which data was collected.
#' @export
get_ci_data_for_a_day_v2 <- function(accessToken, matching_name, Day, project_name = PROJECT_NAME){

  # Endpoint
  endpoint_cognitive_interview <- paste0("projects/", project_name, "/databases/(default)/documents/cognitive_interview")
  combo_endpoint <- paste0(endpoint_cognitive_interview, "/", matching_name, "/", Day)

  # Get data
  request <- get_data_from_firestore(db_endpoint = combo_endpoint,
                                     auth_token = accessToken)


  ## Return NULL if data does not exist
  if (request$status_code != 200L | !"documents" %in% names(jsonlite::fromJSON(httr::content(
    request,
    "text"
  )))) {
    return(NULL)
  } else {
    ci_results <- jsonlite::fromJSON(httr::content(
      request,
      "text"
    ))$documents |>
      dplyr::select(-c(createTime, updateTime)) |>
      dplyr::mutate(name = basename(name)) |>
      tidyr::unnest_wider(col = fields) |>
      tidyr::unnest_wider(col = everything(), names_sep = "_") |>
      dplyr::rename(name = name_1)


    column_names <- gsub("fields_(\\w+)_.*Value", "\\1", gsub("_stringValue", "", names(ci_results)))

    names(ci_results) <- column_names

    ci_results
  }

}








#' Get data of all events from all days for a given combination
#'
#' @param accessToken firebase token
#' @param matching_name name of SME_instructor_Pilt1_Pilot2
#' @param project_name firebase project
#'
#' @return dataframe of all days
#' @export
get_data_for_all_days_for_a_combo <- function(accessToken, matching_name, project_name = PROJECT_NAME){
  purrr::map_dfr(
    all_days,
    function(day){
      get_ci_data_for_a_day_v2(accessToken,
                               matching_name,
                               day,
                               project_name)
    },
    .id = "Day"
  )
}






