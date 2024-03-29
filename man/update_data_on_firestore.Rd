% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{update_data_on_firestore}
\alias{update_data_on_firestore}
\title{Update current data on Firestore with new data}
\usage{
update_data_on_firestore(db_endpoint, document_id, data, auth_token)
}
\arguments{
\item{db_endpoint}{Character. Path to a collection on Firestore.}

\item{document_id}{Character. ID of the document of which the data is being updated.}

\item{data}{JSON. Data in json format.}

\item{auth_token}{Character. Token to access Firestore.}
}
\value{
Response. It tells you the details of the posted data.
}
\description{
Update current data on Firestore with new data
}
