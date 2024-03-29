% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_data_for_all_days_for_a_combo}
\alias{get_data_for_all_days_for_a_combo}
\title{Get data of all events from all days for a given combination}
\usage{
get_data_for_all_days_for_a_combo(
  accessToken,
  matching_name,
  project_name = PROJECT_NAME
)
}
\arguments{
\item{accessToken}{firebase token}

\item{matching_name}{name of SME_instructor_Pilt1_Pilot2}

\item{project_name}{firebase project}
}
\value{
dataframe of all days
}
\description{
Get data of all events from all days for a given combination
}
