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
                             Day, Pilot, element_num, size_of_btn = "sm", fs_data_one_pilot) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Event name as a function of day
      sim_event <- reactive(paste0("event_", gsub("\\s", "", event_name), "_", Day, "_", gsub("\\s", "", Pilot()[element_num])))


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
                  inputId = ns(paste0(sim_event(), "_reset_", gsub("\\s", "", x))),
                  label = "Deselect",
                  onclick = "resetRadioGroup(this.id)"
                ),
                span(class = "space-between"),
                radioGroupButtons(
                  inputId = ns(paste0(sim_event(), "_radio_", gsub("\\s", "", x))),
                  choiceNames = choice_names, #c("-", "+"),
                  choiceValues = choice_values, #c("present but negative", "present and positive"),
                  status = "radiobuttonz",
                  size = size_of_btn,
                  selected = if (is.null(fs_data_one_pilot)) {character(0)} else {
                    if (!fs_data_one_pilot[paste0(gsub("\\s", "_", x), "_p", element_num)] %in% c("present and positive", "present but negative")){
                      character(0)
                      } else {
                        fs_data_one_pilot[paste0(gsub("\\s", "_", x), "_p", element_num)]
                        }
                      },
                  checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"
                    )
                  )
                ),
                actionLink(
                  inputId = ns(paste0(sim_event(), "_handwrite_", gsub("\\s", "", x))),
                  label = "Handwrite",
                  icon = icon(name = "up-right-from-square")
                ),
                textAreaInput(
                  inputId = ns(paste0(sim_event(), "_text_", gsub("\\s", "", x))),
                  label = NULL,
                  placeholder = paste0("Enter comments about ", x, " Awareness for ", Pilot()[element_num]),
                  width = "400px", height = "150px",
                  value = if (is.null(fs_data_one_pilot)) {""} else {fs_data_one_pilot[paste0(gsub("\\s", "_", x), "_p", element_num, "_comment")]}
                )
              )
            }
          )
        )
      })



      lapply(constructs_vec, function(x) {
        run <- reactiveVal(NULL)
        observeEvent(input[[paste0(sim_event(), "_handwrite_", gsub("\\s", "", x))]], {
          showModal(
            modalDialog(
              tagList(
                mod_handwriting_ui(ns(paste0(sim_event(), "_handwriting_", gsub("\\s", "", x)))),
                actionButton(ns(paste0(sim_event(), "_pressing_", gsub("\\s", "", x))),
                             label = "Done", class = "save-button")
              ),
              title = paste0("Handwrite your comments on ", x, " Awareness"),
              size = "l"
            )
          )

          run(TRUE)

          text_output <- mod_handwriting_server(paste0(sim_event(), "_handwriting_", gsub("\\s", "", x)), run)

          observeEvent(input[[paste0(sim_event(), "_pressing_", gsub("\\s", "", x))]], {
            removeModal()
            updateTextAreaInput(session, paste0(sim_event(), "_text_", gsub("\\s", "", x)), value = text_output())
          })
        })
      })




      ## Get the input values of all radio buttons
      radio_values <- reactive({
        radio_values <- lapply(
          constructs_vec,
          function(x) {
            input_id <- paste0(sim_event(), "_radio_", gsub("\\s", "", x))
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
            input_id_text <- paste0(sim_event(), "_text_", gsub("\\s", "", x))
            input_value_text <- input[[input_id_text]]
            return(input_value_text)
          }
        )

        ## If any text input is left unselected, make it empty string i.e., ""
        text_values <- lapply(text_values, function(x) ifelse(is.null(x), "", x))
        text_values |> purrr::list_simplify()
      })


      notes <- reactive(input[[paste0(sim_event(), "_comments")]])

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



# Demo-------------------------------
# mod_pilot_demo <- function() {
#   # Global------------------------------
#   library(shiny)
#   library(shinyWidgets)
#   library(shinyjs)
#   library(bslib)
#   library(firebase)
#   library(jsonlite)
#   library(httr)
#   library(purrr)
#   library(tidyr)
#   library(dplyr)
#
#   # source("R/functions.R")
#   source("R/mod_handwriting.R")
#
#   # constructs_vec <-  c("Fun", "Hie", "Tas",
#   #                      "Cri", "Aff", "Ant",
#   #                      "Com", "Rel", "Env")
#   #
#   # subtitles <- c("Fun"       = "sub",
#   #                "Hie"     = "sub",
#   #                "Tas"       = "sub",
#   #                "Cri"    = "sub",
#   #                "Aff"     = "sub",
#   #                "Ant"     = "sub",
#   #                "Com"        = "sub",
#   #                "Rel"         = "sub",
#   #                "Env"      = "sub")
#
#   constructs_vec <- c(
#     "Functional", "Hierarchical", "Task Empirical",
#     "Critical", "Affective", "Anticipatory",
#     "Compensatory", "Relational", "Environmental"
#   )
#
#   subtitles <- c(
#     Functional = "Instrument and Equipment Knowledge",
#     Hierarchical = "Knowing the Procedures            ",
#     "Task Empirical" = "Knowing the Limits                ",
#     Relational = "Keeping Each Other Safe           ",
#     Environmental = "Company Support for Safety        ",
#     Anticipatory = "Seeing the Threat                 ",
#     Compensatory = "Adjusting to the Threat           ",
#     Affective = "Gut Feel for Threats              ",
#     Critical = "Relying on Experience             "
#   )
#
#
#   # event_name <- "V1 cut"
#   # Day <- "Day1"
#
#   # SME <- "Umair"
#   # Instructor <- "Umair"
#   # Pilot1 <- "Umair"
#   # Pilot2 <- "Umair"
#   PROJECT_NAME = "brpa-dev"
#
#   sign.in <- function(email, password, api_key) {
#     r <- httr::POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
#                     httr::add_headers("Content-Type" = "application/json"),
#                     body = jsonlite::toJSON(list(email = email, password = password, returnSecureToken = TRUE),auto_unbox=TRUE))
#     return(httr::content(r))
#   }
#
#
#
#   ure <- sign.in("ag@oqfmaxhjb.ure", Sys.getenv("PASS"), Sys.getenv("FIREBASE_API_KEY"))
#
#
#
#   # UI----------------------------------
#   ui <- page_fluid(
#     tags$head(
#       tags$script(type = "text/javascript", src = "inst/app/www/scrollPage.js"),
#       tags$button(id = "scroll-to-top-button",
#                   onclick = "topFunction()",
#                   "⇧"),
#       tags$link(rel = "stylesheet", type = "text/css", href = "inst/app/www/styles.css"),
#       tags$script(src = "inst/app/www/reset.js"),
#       tags$script(src = "inst/app/www/handwriting.js"),
#       tags$script(src = "inst/app/www/handwriting.canvas.js"),
#       tags$script(src = "inst/app/www/handwriting_for_shiny3.js"),
#       tags$script(src = "inst/app/www/change_color.js"),
#       useFirebase(), # import dependencies
#       useShinyjs()
#     ),
#     theme = bs_theme(
#       version = 5,
#       bg = "#1a1a1a",
#       fg = "#ffffff",
#       primary = "#0072bc",
#       success = "#39b54a",
#       danger = "#d7df23",
#       warning = "#8dc63f",
#       base_font = font_google("Libre Franklin"),
#       preset = "shiny",
#       heading_font = font_google("Libre Franklin")
#     ),
#     mod_pilot_ui("form1"),
#     verbatimTextOutput("printt")
#   )
#
#
#   # Server-------------------------------------------------------
#   server <- server <- function(input, output, session) {
#
#     accessTokenu <- reactive(ure$idToken)
#
#     SME <- reactive("Martin Smith")
#     Instructor <- reactive("Charles Xavier")
#     Pilot <- reactive(c("XC34B", "P325X"))
#     Day <- reactive("Day1")
#
#     event_name <- reactive("v1_cut")
#
#     fs_data_one_pilot <- reactive({
#       get_ci_data_for_a_day(
#         PROJECT_NAME,
#         accessToken = accessTokenu(),
#         Day = Day(),
#         SME = SME(),
#         Instructor = Instructor(),
#         pilot_vec = Pilot()
#       ) |>
#         dplyr::filter(name == event_name())
#   })
#
#
#     pilot1_vals <- mod_pilot_server(
#       id="form1", constructs_vec, subtitles,
#       choice_names = c("-", "+"), choice_values = c("present but negative", "present and positive"),
#       event_name = event_name(),
#       Day = Day(), Pilot = Pilot, element_num = 1, size_of_btn = "sm", fs_data_one_pilot()
#     )
#
#
#     output$printt <- renderPrint({
#       data.frame(
#         radioz = pilot1_vals()$radio_values(),
#         textz = pilot1_vals()$text_values()
#       )
#     })
#
#   }
#
#   shinyApp(ui, server)
# }
