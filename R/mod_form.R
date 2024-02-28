#' form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput reactive textAreaInput renderUI h4 actionLink br selectInput observeEvent showModal modalDialog actionButton removeModal updateTextAreaInput reactiveTimer showNotification
#' @importFrom bslib layout_columns
#' @importFrom openxlsx removeWorksheet addWorksheet writeDataTable
#' @importFrom shinyjs html
mod_form_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("event_form"))
  )
}

#' form Server Functions
#'
#' @noRd
mod_form_server <- function(id, constructs_vec, subtitles,
                            choice_names, choice_values, event_name,
                            PROJECT_NAME, accessToken, Day,
                            SME, Instructor, pilot_vec, size_of_btn, se, full_workbook, fs_data_event) {


  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns



      # Event name as a function of day
      sim_event <- reactive(paste0("event_", gsub("\\s", "", event_name), "_", Day))

      if (se) {
        stend <- layout_columns(
          textAreaInput(
            inputId = ns(paste0(sim_event(), "_eventStart")),
            label = "Event Start",
            placeholder = "Event started at ...",
            value = if (is.null(fs_data_event)) {""} else {fs_data_event["Event_Start"]}
          ),
          textAreaInput(
            inputId = ns(paste0(sim_event(), "_eventEnd")),
            label = "Event End",
            placeholder = "Event ended at ...",
            value = if (is.null(fs_data_event)) {""} else {fs_data_event["Event_End"]}
          )
        )
      } else{
        stend <- tagList()
      }


      # UI part: Show the form with - and + buttons for scoring constructs,
      ## textboxes for comments on individual constructs, textbox for general
      ## comments, checkbox for highlighting an event for later discussion, and
      ## a submit button
      output$event_form <- renderUI({

        tagList(
          h4(paste0(event_name, " (", Day, ")")),
          # layout_columns(
          #   textAreaInput(
          #     inputId = ns(paste0(sim_event(), "_eventStart")),
          #     label = "Event Start",
          #     placeholder = "Event started at ..."
          #   ),
          #   textAreaInput(
          #     inputId = ns(paste0(sim_event(), "_eventEnd")),
          #     label = "Event End",
          #     placeholder = "Event ended at ...",
          #   )
          # ),
          stend,
          actionLink(
            inputId = ns(paste0(sim_event(), "_handwrite")),
            label = "Handwrite",
            icon = icon(name = "up-right-from-square")
          ),
          textAreaInput(
            inputId = ns(paste0(sim_event(), "_comments")),
            label = strong(
              span(
                "General Comments:",
                class = "construct-name"
              )
            ),
            placeholder = "Add your notes here.",
            value = if (is.null(fs_data_event)) {""} else {fs_data_event["Notes"]},
            width = "1200px", height = "200px"
          ),
          br(),

          layout_columns(
            selectInput(ns("FM1"),
                        label = paste0(pilot_vec()[1], " is:"),
                        choices = c("PM", "PF"),
                        selected = if (is.null(fs_data_event)) {NULL} else {fs_data_event["Pilot1_status"]}
                        ),
            textOutput(ns("FM2"))
          ),
          layout_columns(
            selectInput(ns("CF1"),
                        label = "",
                        choices = c("Captain", "First Officer"),
                        selected = if (is.null(fs_data_event)) {NULL} else {fs_data_event["Pilot1_title"]}
                        ),
            textOutput(ns("CF2"))
          ),
          layout_columns(
            mod_pilot_ui(ns(gsub("\\s", "", paste0(sim_event(), "_pilot1")))), mod_pilot_ui(ns(gsub("\\s", "",paste0(sim_event(), "_pilot2"))))
          ),

          actionButton(ns(paste0(sim_event(), "_submit_event")), "Save", class = "save-button",
                       onclick = "change_color(this)")
        )

      })

      if (length(pilot_vec()) == 2){



        fs_data_one_pilot1 <- reactive({
          if (!is.null(fs_data_event)){
            fs_data_event_filt <- fs_data_event |>
              dplyr::select(name, contains("p1"))
          } else {NULL}
        })

        fs_data_one_pilot2 <- reactive({
          if (!is.null(fs_data_event)){
            fs_data_event |>
              dplyr::select(name, contains("p2"))
          } else {NULL}
        })

        pilot1_vals <- mod_pilot_server(
          id = gsub("\\s", "", paste0(sim_event(), "_pilot1")), constructs_vec, subtitles,
          choice_names, choice_values, event_name,
          Day, Pilot = pilot_vec, element_num = 1, size_of_btn, fs_data_one_pilot1()
        )

        pilot2_vals <- mod_pilot_server(
          id = gsub("\\s", "", paste0(sim_event(), "_pilot2")), constructs_vec, subtitles,
          choice_names, choice_values, event_name,
          Day, Pilot = pilot_vec, element_num = 2, size_of_btn, fs_data_one_pilot2()
        )

      } else {

        fs_data_one_pilot1 <- reactive({
          if (!is.null(fs_data_event)){
            fs_data_event |>
              dplyr::select(name, contains("p1"))
          } else {NULL}
        })

        pilot1_vals <- mod_pilot_server(
          id = gsub("\\s", "", paste0(sim_event(), "_pilot1")), constructs_vec, subtitles,
          choice_names, choice_values, event_name,
          Day, Pilot = pilot_vec, element_num = 1, size_of_btn, fs_data_one_pilot1()
        )
      }
      pilot_2_FM <- reactive({
        if (input$FM1 == "PM"){
          pilot_2_FM <- "PF"
        } else if (input$FM1 == "PF") {
          pilot_2_FM <- "PM"
        } else {
          pilot_2_FM <- character(0)
        }

        pilot_2_FM
      })


      pilot_2_CF <- reactive({
        if (input$CF1 == "Captain"){
          pilot_2_CF <- "First Officer"
        } else if (input$CF1 == "First Officer") {
          pilot_2_CF <- "Captain"
        } else {
          pilot_2_CF <- character(0)
        }

        pilot_2_CF
      })


      if (!is.na(pilot_vec()[2])){
        output$FM2 <- renderText({
          paste0(pilot_vec()[2], " is ", pilot_2_FM())
        })

        output$CF2 <- renderText({
          paste0(pilot_vec()[2], " is ", pilot_2_CF())
        })
      }


      run_comments <- reactiveVal(NULL)
      observeEvent(input[[paste0(sim_event(), "_handwrite")]], {
        showModal(
          modalDialog(
            tagList(
              actionButton(ns(paste0(sim_event(), "_pressing1")),
                           label = "Done", class = "save-button"),
              mod_handwriting_ui(ns(paste0(sim_event(), "_handwriting"))),
              actionButton(ns(paste0(sim_event(), "_pressing2")),
                           label = "Done", class = "save-button")
            ),
            title = paste0("Handwrite your comments"),
            size = "l"
          )
        )

        run_comments(TRUE)

        text_outputt <- mod_handwriting_server(paste0(sim_event(), "_handwriting"), run_comments)

        observeEvent(input[[paste0(sim_event(), "_pressing1")]] | input[[paste0(sim_event(), "_pressing2")]], {
          removeModal()
          updateTextAreaInput(session, paste0(sim_event(), "_comments"), value = text_outputt())
        })
      })


      # this_event_data_store <- reactiveVal()

      # When the Submit button is clicked, event data is sent to Firestore
      observeEvent(input[[paste0(sim_event(), "_submit_event")]], {

        # print(shiny::isTruthy(input[[paste0(sim_event(), "_submit_event")]]))
        # print(shiny::isTruthy(paste0(sim_event(), "_submit_event")))

        ## IDs of inputs
        textt_area_id <- paste0(sim_event(), "_comments")

        if (length(pilot_vec()) == 2) {

          Pilot1 <- pilot_vec()[1]
          Pilot2 <- pilot_vec()[2]


          ## Send the data to Firestore
          ### Request
          request <- post_event_data_ci(PROJECT_NAME, accessToken(), Day,
                                        SME(), Instructor(), Pilot1, Pilot2,
                                        pilot1_status = input$FM1, pilot1_title = input$CF1,
                                        pilot2_status = pilot_2_FM(), pilot2_title = pilot_2_CF(),
                                        event_name = event_name,
                                        constructs_inputs_p1 = pilot1_vals()$radio_values(),
                                        text_inputs_p1       = pilot1_vals()$text_values(),

                                        constructs_inputs_p2 = pilot2_vals()$radio_values(),
                                        text_inputs_p2       = pilot2_vals()$text_values(),
                                        notes = input[[textt_area_id]],
                                        event_start = if(se == TRUE) {input[[paste0(sim_event(), "_eventStart")]]} else {NULL},
                                        event_end = if(se == TRUE) {input[[paste0(sim_event(), "_eventEnd")]]} else {NULL}
          )

          # print(request)

          constructs_inputs_p1 <- pilot1_vals()$radio_values()
          text_inputs_p1       <- pilot1_vals()$text_values()

          constructs_inputs_p2 <- pilot2_vals()$radio_values()
          text_inputs_p2       <- pilot2_vals()$text_values()

          notes <- input[[textt_area_id]]


          this_event_data <- dplyr::tibble(
            event_name = rep(event_name, 18),
            start = if(se == TRUE) {rep(input[[paste0(sim_event(), "_eventStart")]], 18)} else {NULL},
            end   = if(se == TRUE) {rep(input[[paste0(sim_event(), "_eventEnd")]],   18)} else {NULL},
            pilot_id = c(rep(Pilot1, 9), rep(Pilot2, 9)),
            construct = rep(c("Functional", "Hierarchical", "Task Empirical",
                              "Relational", "Environmental", "Anticipatory", "Compensatory",
                              "Affective", "Critical"), 2),
            value =     c(constructs_inputs_p1[1], constructs_inputs_p1[2], constructs_inputs_p1[3],
                          constructs_inputs_p1[8], constructs_inputs_p1[9], constructs_inputs_p1[6],
                          constructs_inputs_p1[7], constructs_inputs_p1[5], constructs_inputs_p1[4],

                          constructs_inputs_p2[1], constructs_inputs_p2[2], constructs_inputs_p2[3],
                          constructs_inputs_p2[8], constructs_inputs_p2[9], constructs_inputs_p2[6],
                          constructs_inputs_p2[7], constructs_inputs_p2[5], constructs_inputs_p2[4]),

            comment =   c(text_inputs_p1[1], text_inputs_p1[2], text_inputs_p1[3],
                          text_inputs_p1[8], text_inputs_p1[9], text_inputs_p1[6],
                          text_inputs_p1[7], text_inputs_p1[5], text_inputs_p1[4],

                          text_inputs_p2[1], text_inputs_p2[2], text_inputs_p2[3],
                          text_inputs_p2[8], text_inputs_p2[9], text_inputs_p2[6],
                          text_inputs_p2[7], text_inputs_p2[5], text_inputs_p2[4]),

            general_comments = rep(notes, 18)

          )

          if (event_name %in% full_workbook()$sheet_names){
            removeWorksheet(full_workbook(), sheet = event_name)
          }

          addWorksheet(full_workbook(), sheetName = event_name, gridLines = TRUE)
          writeDataTable(full_workbook(), sheet = event_name, x = this_event_data,
                         colNames = TRUE)



        } else {
          Pilot1 <- pilot_vec()[1]

          ## Send the data to Firestore
          ### Request
          request <- post_event_data_ci(PROJECT_NAME, accessToken(), Day,
                                        SME(), Instructor(), Pilot1, Pilot2 = NULL,
                                        pilot1_status = input$FM1, pilot1_title = input$CF1,
                                        pilot2_status = NULL, pilot2_title = NULL,
                                        event_name = event_name,
                                        constructs_inputs_p1 = pilot1_vals()$radio_values(),
                                        text_inputs_p1       = pilot1_vals()$text_values(),
                                        constructs_inputs_p2 = NULL,
                                        text_inputs_p2       = NULL,
                                        notes = input[[textt_area_id]],
                                        event_start = if(se == TRUE) {input[[paste0(sim_event(), "_eventStart")]]} else {NULL},
                                        event_end = if(se == TRUE) {input[[paste0(sim_event(), "_eventEnd")]]} else {NULL}
          )

          constructs_inputs_p1 <- pilot1_vals()$radio_values()
          text_inputs_p1       <- pilot1_vals()$text_values()

          notes <- input[[textt_area_id]]

          this_event_data <- dplyr::tibble(
            event_name = rep(event_name, 9),
            start = if(se == TRUE) {rep(input[[paste0(sim_event(), "_eventStart")]], 9)} else {NULL},
            end   = if(se == TRUE) {rep(input[[paste0(sim_event(), "_eventEnd")]],   9)} else {NULL},
            pilot_id = rep(Pilot1, 9),
            construct = c("Functional", "Hierarchical", "Task Empirical",
                          "Relational", "Environmental", "Anticipatory", "Compensatory",
                          "Affective", "Critical"),
            value =     c(constructs_inputs_p1[1], constructs_inputs_p1[2], constructs_inputs_p1[3],
                          constructs_inputs_p1[8], constructs_inputs_p1[9], constructs_inputs_p1[6],
                          constructs_inputs_p1[7], constructs_inputs_p1[5], constructs_inputs_p1[4]),

            comment =   c(text_inputs_p1[1], text_inputs_p1[2], text_inputs_p1[3],
                          text_inputs_p1[8], text_inputs_p1[9], text_inputs_p1[6],
                          text_inputs_p1[7], text_inputs_p1[5], text_inputs_p1[4]),

            general_comments = rep(notes, 9)

          )

          if (event_name %in% full_workbook()$sheet_names){
            removeWorksheet(full_workbook(), sheet = event_name)
          }

          addWorksheet(full_workbook(), sheetName = event_name, gridLines = TRUE)
          writeDataTable(full_workbook(), sheet = event_name, x = this_event_data,
                         colNames = TRUE)




          # full_workbook(wb)


        }

        ### Provide notifications
        if (request$status_code %in% c(200L, 409L)) {
          showNotification(h4(paste0(event_name, " data submitted to database!")), type = "message")
          shinyjs::html(id = paste0(sim_event(), "_submit_event"), html = "Update")
        } else {
          showNotification(h4("Something went wrong! Data NOT submitted"), type = "error")
        }
      })





      autoSaveTimer <- reactiveTimer(intervalMs = 60000)



      observe({

        autoSaveTimer()

        # print("Inside observe block in mod_form_server")
        # print(paste("Event name:", event_name))
        # print(paste("Is event name empty?", is.null(event_name) || event_name == ""))

        ## IDs of inputs
        textt_area_id <- paste0(sim_event(), "_comments")

        if (length(pilot_vec()) == 2) {

          Pilot1 <- pilot_vec()[1]
          Pilot2 <- pilot_vec()[2]


          if (!is.null(input$FM1)){
            ## Send the data to Firestore
            ### Request
            request <- post_event_data_ci(PROJECT_NAME, isolate(accessToken()), Day,
                                          isolate(SME()), isolate(Instructor()), Pilot1, Pilot2,
                                          pilot1_status = isolate(input$FM1), pilot1_title = isolate(input$CF1),
                                          pilot2_status = isolate(pilot_2_FM()), pilot2_title = isolate(pilot_2_CF()),
                                          event_name = isolate(event_name),
                                          constructs_inputs_p1 = isolate(pilot1_vals()$radio_values()),
                                          text_inputs_p1       = isolate(pilot1_vals()$text_values()),

                                          constructs_inputs_p2 = isolate(pilot2_vals()$radio_values()),
                                          text_inputs_p2       = isolate(pilot2_vals()$text_values()),
                                          notes = isolate(input[[textt_area_id]]),
                                          event_start = isolate(input[[paste0(sim_event(), "_eventStart")]]),
                                          event_end = isolate(input[[paste0(sim_event(), "_eventEnd")]])
            )
          }

        } else {
          Pilot1 <- pilot_vec()[1]

          if (!is.null(input$FM1)){
            ## Send the data to Firestore
            ### Request
            request <- post_event_data_ci(PROJECT_NAME, isolate(accessToken()), Day,
                                          isolate(SME()), isolate(Instructor()), Pilot1, Pilot2 = NULL,
                                          pilot1_status = isolate(input$FM1), pilot1_title = isolate(input$CF1),
                                          pilot2_status = NULL, pilot2_title = NULL,
                                          event_name = isolate(event_name),
                                          constructs_inputs_p1 = isolate(pilot1_vals()$radio_values()),
                                          text_inputs_p1       = isolate(pilot1_vals()$text_values()),
                                          constructs_inputs_p2 = NULL,
                                          text_inputs_p2       = NULL,
                                          notes = isolate(input[[textt_area_id]]),
                                          event_start = isolate(input[[paste0(sim_event(), "_eventStart")]]),
                                          event_end = isolate(input[[paste0(sim_event(), "_eventEnd")]])
            )
          }
        }

        ### Provide notifications
        if(!is.null(input$FM1)){
          if (request$status_code %in% c(200L, 409L)) {
            # print(paste0(Sys.time(), "        ", request$status_code))
            # showNotification(h4(paste0(event_name, " data submitted to database!")), type = "message")
            # notify(
            #   paste0(event_name, " data submitted to database!"),
            #   type = "info"
            # )
            shinyjs::html(id = paste0(sim_event(), "_submit_event"), html = "Update")
          } else {
            # print(paste0(Sys.time(), "        ", request$status_code))
            # print(Day)
            # print(Pilot1)
            # print(input$FM1)
            # print(pilot1_vals()$radio_values())
            # showNotification(h4("Something went wrong! Data NOT submitted"), type = "error")
          }
        }

      })


      # return(this_event_data_store)


    }
  )
}







# Demo---------------------------------------------
# mod_form_demo <- function() {
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
#   library(openxlsx)
#
#   source("R/functions.R")
#   source("R/mod_handwriting.R")
#   source("R/mod_pilot.R")
#
#
#   PROJECT_NAME = "brpa-dev"
#
#   sign.in <- function(email, password, api_key) {
#     r <- httr::POST(paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", api_key),
#                     httr::add_headers("Content-Type" = "application/json"),
#                     body = jsonlite::toJSON(list(email = email, password = password, returnSecureToken = TRUE), auto_unbox = TRUE)
#     )
#     return(httr::content(r))
#   }
#
#   ure <- sign.in("ag@oqfmaxhjb.ure", Sys.getenv("PASS"), Sys.getenv("FIREBASE_API_KEY"))
#
#
#   # emailu <- ure$email
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
#   #
#   # SME <- "Umair"
#   # Instructor <- "Umair"
#   # Pilot1 <- "Umair"
#   # Pilot2 <- "John"
#   # pilot_vec <- c(Pilot1, Pilot2)
#   #
#   # PROJECT_NAME <- Sys.getenv("FIREBASE_PROJECT_ID")
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
#
#
#     accordion(
#       accordion_panel(title = "First", mod_form_ui("form1")),
#       accordion_panel(title = "Second", mod_form_ui("form2"))
#     )
#
#
#
#   )
#
#
#   # Server-------------------------------------------------------
#   server <- server <- function(input, output, session) {
#     # bs_themer()
#     # constructs_vec <-  c("Fun", "Hie", "Tas",
#     #                      "Cri", "Aff", "Ant",
#     #                      "Com", "Rel", "Env")
#     #
#     # subtitles <- c("Fun"       = "sub",
#     #                "Hie"     = "sub",
#     #                "Tas"       = "sub",
#     #                "Cri"    = "sub",
#     #                "Aff"     = "sub",
#     #                "Ant"     = "sub",
#     #                "Com"        = "sub",
#     #                "Rel"         = "sub",
#     #                "Env"      = "sub")
#     #
#
#
#
#     accessTokenu <- reactive(ure$idToken)
#
#
#     SME <- reactive("Martin Smith")
#     Instructor <- reactive("Charles Xavier")
#     Pilot <- reactive(c("XC34B", "P325X"))
#     Day <- reactive("Day1")
#     event_name <- reactive("v1_cut")
#
#
#     fs_data_event <- reactive({
#       get_ci_data_for_a_day(
#         PROJECT_NAME,
#         accessToken = accessTokenu(),
#         Day = Day(),
#         SME = SME(),
#         Instructor = Instructor(),
#         pilot_vec = Pilot()
#       ) |>
#         dplyr::filter(name == event_name())
#     })
#
#
#     # wb <- createWorkbook()
#
#     full_workbook <- reactiveVal(value = createWorkbook())
#
#     observe({
#     mod_form_server(
#       "form1", constructs_vec, subtitles,
#       choice_names = c("-", "+"), choice_values = c("present but negative", "present and positive"),
#       event_name = event_name(),
#       PROJECT_NAME, accessTokenu, Day(),
#       SME, Instructor, pilot_vec = Pilot, size_of_btn="sm", se = TRUE, full_workbook, fs_data_event()
#     )
# })
#
#     # mod_form_server(
#     #   "form2", constructs_vec, subtitles,
#     #   choice_names = c("-", "+"), choice_values = c("present but negative", "present and positive"),
#     #   event_name = event_name(),
#     #   PROJECT_NAME, accessTokenu, Day(),
#     #   SME, Instructor, pilot_vec = Pilot, size_of_btn="sm", se = TRUE, full_workbook, fs_data_event()
#     # )
#   }
#
#   shinyApp(ui, server)
# }
