#' IS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList br uiOutput reactiveVal renderUI actionButton observeEvent
#' @importFrom bslib accordion accordion_panel_insert accordion_panel_close
mod_IS_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      id = ns("accord_IS")
    ),
    br(),
    uiOutput(ns("collapse_all_events_button"))
  )
}

#' IS Server Functions
#'
#' @noRd
mod_IS_server <- function(id, constructs_vec, subtitles,
                          PROJECT_NAME, accessToken, Day,
                          SME, Instructor, pilot_vec,
                          aircraft,
                          event_info, full_workbook, isd_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      existing_event_names <- reactiveVal(character(0))

      observe({

        new_event_names <- event_info()$event_names

        # Check for new event names
        new_names <- setdiff(new_event_names, existing_event_names())


        if (length(new_names) > 0) {
          ## Add accordion panels for each event
          lapply(new_names, function(event_name) {
            accordion_panel_insert(
              id = "accord_IS",
              panel = accordion_panel(
                title = event_name,
                mod_form_ui(ns(paste0("form_", event_name)))
              )
            )

            if (!is.null(isd_data)){
              fs_data_event <- isd_data |>
                dplyr::mutate(name = sub("^isd_", "", name)) |>
                dplyr::filter(name == event_name)
            } else{
              fs_data_event <- NULL
            }


            mod_form_server(
              id = paste0("form_", event_name),
              constructs_vec,
              subtitles,
              choice_names = c("-", "+"),
              choice_values = c("present but negative", "present and positive"),
              event_name = paste0("isd_", event_name),
              PROJECT_NAME,
              accessToken,
              Day,
              SME,
              Instructor,
              pilot_vec,
              aircraft,
              size_of_btn = "normal", se = FALSE, full_workbook, fs_data_event
            )
          })

          # Update existing event names
          existing_event_names(c(existing_event_names(), new_names))
        }
      })



      output$collapse_all_events_button <- renderUI({

        if (length(existing_event_names) < 1) {
          p("")
        } else {
          actionButton(ns("collapse_all"), "Collapse All Events",
                       class = "collapse-button",
                       icon = icon(name = "minimize"))
        }
      })

      observeEvent(input$collapse_all, {
        accordion_panel_close(id = "accord_IS", values = TRUE)
      })

    }
  )
}
