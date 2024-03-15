library(httr)
library(jsonlite)

# POST function
post_data_to_firestore <- function(path, data, auth_token) {
  r <- httr::POST(
    url = sprintf("https://firestore.googleapis.com/v1beta1/%s", path),
    config = httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", auth_token)
    ),
    body = data
  )
  return(r)
}

PROJECT_NAME <- "firebase-project"
COLLECTION <- "Block"
accessTokenu <- "access-token"

endpoint <- paste0("projects/", PROJECT_NAME, "/databases/(default)/documents/", COLLECTION)

data_list <- toJSON(
    list(
    fields = list(
      Name = list("stringValue" = "Ground")
    )
  ), auto_unbox = TRUE
)

post_data_to_firestore(
  path = paste0(endpoint, "/House2/Floor1", "?documentId=", "Ground"),
  data = data_list,
  auth_token = accessTokenu
)





post_data_to_firestore(
  path = paste0(endpoint, "?documentId=", "House2"),
  data = NULL,#data_list,
  auth_token = accessTokenu
)



frstore_get("Block", accessTokenu)


endpoint_cognitive_interview <- paste0("projects/", PROJECT_NAME, "/databases/(default)/documents/", "cognitive_interview")
post_data_to_firestore(
  path = paste0(endpoint_cognitive_interview, "?documentId=", "Michel_GG.Isherwood_9232"),
  data = NULL,#data_list,
  auth_token = accessTokenu
)


all_paths <- c("Jacques_JJ_9232", "Jacques_JJ_9433",
               "Michel_GG_9232", "Michel_GG_9433_9232", "Michel_Michael_3575",
               "Michel_Michael_8473")


for (pathh in 1:length(all_paths)){
  post_data_to_firestore(
    path = paste0(endpoint_cognitive_interview, "?documentId=", all_paths[pathh]),
    data = NULL,
    auth_token = accessTokenu
  )
}


frstore_get("cognitive_interview", accessTokenu)
frstore_get("cognitive_interview/Jacques_JJ_9232/Day3/Emergency_Evacuation", accessTokenu)
