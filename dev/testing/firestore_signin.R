library(firebase)

sign_in <- function(email, password, api_key) {
  r <- httr::POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
                  httr::add_headers("Content-Type" = "application/json"),
                  body = jsonlite::toJSON(list(email = email, password = password, returnSecureToken = TRUE),auto_unbox=TRUE))
  return(httr::content(r))
}



ure <- sign_in("udurrani@test.com", Sys.getenv("PASS"), Sys.getenv("FIREBASE_API_KEY"))

accessTokenu <- ure$idToken
emailu <- ure$email
