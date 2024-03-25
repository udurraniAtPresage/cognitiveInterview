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



#' Get all name combos
#'
#' @param accessToken Firebase access token.
#' @param project_name Firebase Project ID.
#'
#' @return A vector of name combos.
#' @export
get_all_name_combos <- function(accessToken, collection, project_name = PROJECT_NAME){
  endpoint <- paste0("projects/", project_name, "/databases/(default)/documents/", collection)
  request <- get_data_from_firestore(
    db_endpoint = endpoint,
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
      res <- get_ci_data_for_a_day_v2(accessToken,
                               matching_name,
                               day,
                               project_name)
      if (!is.null(res)){
        res |>
          dplyr::mutate(Day = day)
      } else {
        res
      }
    }
  )
}
