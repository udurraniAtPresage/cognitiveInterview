#' inst_interview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput br req renderUI h4 observe
mod_inst_interview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("selected_event_oi")),
    br(),
    mod_form_ui(ns("inst_interview"))
  )
}


#' inst_interview Server Functions
#'
#' @noRd
mod_inst_interview_server <- function(id, constructs_vec, subtitles,
                                      PROJECT_NAME, accessToken, Day,
                                      SME, Instructor, pilot_vec, selected_eoi, full_workbook, ins_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      output$selected_event_oi <- renderUI({
        req(selected_eoi())
        if (!is.null(selected_eoi())){

          h4(paste0("Selected Event of Interest is: ", selected_eoi()))

        } else {
          h4("Please select the event of interest from the Pilot Debrief page.")
        }
      })


      observe({
        req(selected_eoi())

        if (!is.null(ins_data)){
          fs_data_event <- ins_data |>
            dplyr::mutate(name = sub("^eoi_inst_", "", name)) |>
            dplyr::filter(name == selected_eoi())
        } else{
          fs_data_event <- NULL
        }

        mod_form_server(
          "inst_interview", constructs_vec, subtitles,
          choice_names = c("1", "2", "3", "4"),
          choice_values = c(1, 2, 3, 4),
          event_name = paste0("eoi_inst_", selected_eoi()),
          PROJECT_NAME, accessToken, Day,
          SME, Instructor, pilot_vec, size_of_btn = "sm", se = FALSE, full_workbook, fs_data_event
        )
      })

    }
  )
}
