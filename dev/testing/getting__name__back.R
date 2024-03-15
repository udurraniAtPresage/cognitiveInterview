PROJECT_NAME <- "brpa-dev"
get_data_from_firestore(db_endpoint = paste0("projects/", PROJECT_NAME,
                                             "/databases/(default)/documents/cognitiveInterview/Jacques_JJ_9232/Day3"),
                        auth_token = accessTokenu)

get_data_from_firestore(db_endpoint = paste0("projects/", PROJECT_NAME,
                                             "/databases/(default)/documents/test"),
                        auth_token = accessTokenu)

frstore_get(document_path=paste0("cognitive_interview"),
            id_token=accessTokenu, fields = "__name__")


foo <- frstore_get(document_path=paste0("cognitiveInterview"),
            id_token=accessTokenu, fields = "__name__")


foo$documents
