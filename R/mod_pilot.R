#' pilot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput renderUI div span strong p br actionLink textAreaInput observeEvent showModal modalDialog updateTextAreaInput
#' @importFrom shinyWidgets radioGroupButtons
mod_pilot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("event_form"))
  )
}


#' pilot Server Functions
#'
#' @noRd
mod_pilot_server <- function(id, constructs_vec, subtitles,
                             choice_names, choice_values, event_name,
                             Day, Pilot, element_num, size_of_btn = "sm") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Event name as a function of day
      sim_event <- paste0("event_", gsub("\\s", "", event_name), "_", Day, "_", gsub("\\s", "", Pilot()[element_num]))


      # UI part: Show the form with - and + buttons for scoring constructs,
      ## textboxes for comments on individual constructs, textbox for general
      ## comments, checkbox for highlighting an event for later discussion, and
      ## a submit button
      output$event_form <- renderUI({
        # showModal(
        #   modalDialog(
        tagList(
          lapply(
            constructs_vec,
            function(x) {
              subtitle <- subtitles[match(x, constructs_vec)]

              div(
                class = "btn-group-label",
                span(
                  class = "label-spacing",
                  strong(
                    span(
                      paste0(x, " Awareness"),
                      class = "construct-name"
                    )
                  ),
                  br(),
                  p(subtitle, class = "subtitle")
                ),
                actionLink(
                  inputId = ns(paste0(sim_event, "_reset_", gsub("\\s", "", x))),
                  label = "Deselect",
                  onclick = "resetRadioGroup(this.id)"
                ),
                span(class = "space-between"),
                radioGroupButtons(
                  inputId = ns(paste0(sim_event, "_radio_", gsub("\\s", "", x))),
                  choiceNames = choice_names, #c("-", "+"),
                  choiceValues = choice_values, #c("present but negative", "present and positive"),
                  status = "radiobuttonz",
                  size = size_of_btn,
                  selected = character(0),
                  checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"
                    )
                  )
                ),
                actionLink(
                  inputId = ns(paste0(sim_event, "_handwrite_", gsub("\\s", "", x))),
                  label = "Handwrite",
                  icon = icon(name = "up-right-from-square")
                ),
                textAreaInput(
                  inputId = ns(paste0(sim_event, "_text_", gsub("\\s", "", x))),
                  label = NULL,
                  placeholder = paste0("Enter comments about ", x, " Awareness for ", Pilot()[element_num]),
                  width = "400px", height = "150px"
                )
              )
            }
          )
        )
      })



      lapply(constructs_vec, function(x) {
        run <- reactiveVal(NULL)
        observeEvent(input[[paste0(sim_event, "_handwrite_", gsub("\\s", "", x))]], {
          showModal(
            modalDialog(
              tagList(
                mod_handwriting_ui(ns(paste0(sim_event, "_handwriting_", gsub("\\s", "", x)))),
                actionButton(ns(paste0(sim_event, "_pressing_", gsub("\\s", "", x))),
                             label = "Done", class = "save-button")
              ),
              title = paste0("Handwrite your comments on ", x, " Awareness"),
              size = "l"
            )
          )

          run(TRUE)

          text_output <- mod_handwriting_server(paste0(sim_event, "_handwriting_", gsub("\\s", "", x)), run)

          observeEvent(input[[paste0(sim_event, "_pressing_", gsub("\\s", "", x))]], {
            removeModal()
            updateTextAreaInput(session, paste0(sim_event, "_text_", gsub("\\s", "", x)), value = text_output())
          })
        })
      })




      ## Get the input values of all radio buttons
      radio_values <- reactive({
        radio_values <- lapply(
          constructs_vec,
          function(x) {
            input_id <- paste0(sim_event, "_radio_", gsub("\\s", "", x))
            input_value <- input[[input_id]]
            return(input_value)
          }
        )

        ## If any radio button is left unselected, make it "not present"
        radio_values <- lapply(radio_values, function(x) ifelse(is.null(x), "not present", x))
        radio_values |> purrr::list_simplify()
      })

      ## Get the input values of all text inputs for constructs
      text_values <- reactive({
        text_values <- lapply(
          constructs_vec,
          function(x) {
            input_id_text <- paste0(sim_event, "_text_", gsub("\\s", "", x))
            input_value_text <- input[[input_id_text]]
            return(input_value_text)
          }
        )

        ## If any text input is left unselected, make it empty string i.e., ""
        text_values <- lapply(text_values, function(x) ifelse(is.null(x), "", x))
        text_values |> purrr::list_simplify()
      })


      notes <- reactive(input[[paste0(sim_event, "_comments")]])

      return(
        reactive(list(
          radio_values = radio_values,
          text_values = text_values
        )
        )
      )

    }
  )
}

