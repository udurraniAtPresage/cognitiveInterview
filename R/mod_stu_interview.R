#' stu_interview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput br reactive renderUI strong span h4 radioButtons observeEvent
#' @importFrom shinyjs disable
mod_stu_interview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("event_of_interest_ui")),
    br(),
    mod_form_ui(ns("stu_interview"))
  )
}

#' stu_interview Server Functions
#'
#' @noRd
mod_stu_interview_server <- function(id, constructs_vec, subtitles,
                                     PROJECT_NAME, accessToken, Day,
                                     SME, Instructor, pilot_vec,
                                     aircraft,
                                     event_info, full_workbook, stu_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      event_names <- reactive({event_info()$event_names})

      output$event_of_interest_ui <- renderUI({

        if (!is.null( event_names())){

          choices <- event_names() %in% event_info()$highlighted_events
          choices_highlighted <- lapply(seq_along(choices), function(i)
            if (choices[i]) {
              strong(span(event_names()[i], style="color:#0072bc"))
            } else {
              event_names()[i]
            }
          )

          if (!is.null(stu_data)) {

            radioButtons(
              inputId = ns("event_select"),
              label = h4("Select event of interest:"),
              choiceNames = choices_highlighted,
              choiceValues = event_names(),
              selected = if (is.null(stu_data)) {
                character(0)
                } else if (length(unique(stu_data$name)) > 1){
                  character(0)
                } else {
                this_event_name <- sub("^eoi_stu_", "", stu_data$name)
                this_event_name
              },
              width = 500
            )

            # shinyjs::disable("event_of_interest_ui")

          } else {

            radioButtons(
              inputId = ns("event_select"),
              label = h4("Select event of interest:"),
              choiceNames = choices_highlighted,
              choiceValues = event_names(),
              selected = character(0),
              width = 500
            )
          }
        } else {
          h4("Please create at least 1 event on the 'All Events' page.")
        }
      })

      observeEvent(input$event_select, {

        if (!is.null(stu_data)){
          fs_data_event <- stu_data |>
            dplyr::mutate(name = sub("^eoi_stu_", "", name)) |>
            dplyr::filter(name == input$event_select)
        } else{
          fs_data_event <- NULL
        }

        # print(fs_data_event)

        mod_form_server(
          "stu_interview", constructs_vec, subtitles,
          choice_names = c("1", "2", "3", "4"),
          choice_values = c("1", "2", "3", "4"),
          event_name = paste0("eoi_stu_", input$event_select),
          PROJECT_NAME, accessToken, Day,
          SME, Instructor, pilot_vec,
          aircraft,
          size_of_btn = "sm", se = FALSE, full_workbook, fs_data_event
        )
      })

      return(
        reactive(input$event_select)
      )

    }
  )
}
