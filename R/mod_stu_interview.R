#' stu_interview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput br reactive renderUI strong span h4 radioButtons observeEvent
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
                                     SME, Instructor, pilot_vec, event_info, full_workbook) {
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

          radioButtons(
            inputId = ns("event_select"),
            label = h4("Select event of interest:"),
            choiceNames = choices_highlighted,
            choiceValues = event_names(),
            selected = character(0),
            width = 500
          )
        } else {
          h4("Please create at least 1 event on the 'All Events' page.")
        }
      })



      observeEvent(input$event_select, {
        mod_form_server(
          "stu_interview", constructs_vec, subtitles,
          choice_names = c("1", "2", "3", "4"),
          choice_values = c(1, 2, 3, 4),
          event_name = paste0("eoi_stu_", input$event_select),
          PROJECT_NAME, accessToken, Day,
          SME, Instructor, pilot_vec, size_of_btn = "sm", se = FALSE, full_workbook
        )
      })

      return(
        reactive(input$event_select)
      )

    }
  )
}
