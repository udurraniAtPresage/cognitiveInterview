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
                            SME, Instructor, pilot_vec, size_of_btn, se, full_workbook) {


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
            placeholder = "Event started at ..."
          ),
          textAreaInput(
            inputId = ns(paste0(sim_event(), "_eventEnd")),
            label = "Event End",
            placeholder = "Event ended at ...",
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
            width = "1200px", height = "200px"
          ),
          br(),

          layout_columns(
            selectInput(ns("FM1"), label = paste0(pilot_vec()[1], " is:"), choices = c("PM", "PF")),
            textOutput(ns("FM2"))
          ),
          layout_columns(
            selectInput(ns("CF1"), label = "", choices = c("Captain", "First Officer")),
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


        pilot1_vals <- mod_pilot_server(
          id = gsub("\\s", "", paste0(sim_event(), "_pilot1")), constructs_vec, subtitles,
          choice_names, choice_values, event_name,
          Day, Pilot = pilot_vec, element_num = 1, size_of_btn
        )

        pilot2_vals <- mod_pilot_server(
          id = gsub("\\s", "", paste0(sim_event(), "_pilot2")), constructs_vec, subtitles,
          choice_names, choice_values, event_name,
          Day, Pilot = pilot_vec, element_num = 2, size_of_btn
        )

      } else {
        pilot1_vals <- mod_pilot_server(
          id = gsub("\\s", "", paste0(sim_event(), "_pilot1")), constructs_vec, subtitles,
          choice_names, choice_values, event_name,
          Day, Pilot = pilot_vec, element_num = 1, size_of_btn
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
              mod_handwriting_ui(ns(paste0(sim_event(), "_handwriting"))),
              actionButton(ns(paste0(sim_event(), "_pressing")),
                           label = "Done", class = "save-button")
            ),
            title = paste0("Handwrite your comments"),
            size = "l"
          )
        )

        run_comments(TRUE)

        text_outputt <- mod_handwriting_server(paste0(sim_event(), "_handwriting"), run_comments)

        observeEvent(input[[paste0(sim_event(), "_pressing")]], {
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