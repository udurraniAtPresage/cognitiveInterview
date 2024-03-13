#' all_events UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton uiOutput br reactiveValues observeEvent showModal modalDialog textInput reactiveVal renderUI checkboxGroupInput
#' @importFrom bslib accordion accordion_panel_close accordion_panel_insert
mod_all_events_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Button to launch a new event modal where the user provides the event name.
    actionButton(ns("add"), "New Event",
                 class = "save-button",
                 icon = icon(name = "circle-plus")
    ),
    br(),
    accordion(
      id = ns("accord_all_events")
    ),
    br(),
    uiOutput(ns("collapse_all_events_button")),
    br(),
    br(),
    br(),
    uiOutput(ns("choose_events_for_discussion"))
  )
}

#' all_events Server Functions
#'
#' @noRd
mod_all_events_server <- function(id, constructs_vec, subtitles,
                                  PROJECT_NAME, accessToken, Day,
                                  SME, Instructor, pilot_vec,
                                  aircraft,
                                  full_workbook, all_events_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Initialize a vector of all event names
      all_events <- reactiveValues(namez = NULL#,
                                   # event_data = NULL
      )


      observeEvent(input$collapse_all, {
        accordion_panel_close(id = "accord_all_events", values = TRUE)
      })





      # When the 'New Event' button is clicked, a modal pops up asking for event name.
      observeEvent(input$add, {
        # f$req_sign_in()

        if (SME() == "" | Instructor() == "" | any(pilot_vec() == "")){

          showModal(
            modalDialog(
              h6("Please provide all data in the sidebar before creating a new event"),
              title = "Hold On!"
            )
          )
        } else {

          showModal(
            modalDialog(
              tagList(
                textInput(
                  inputId = ns("event_name"),
                  label = "Event name:",
                  value = ""
                ),
                ## this button will take to the main modal
                actionButton(ns("create_event"), "Submit", class = "save-button")
              ),
              title = "New Event",
              footer = modalButton("Dismiss")
            )
          )
        }
      }, ignoreInit = TRUE)


      # eventz <- reactiveVal()


      observeEvent(input$create_event, {

        req(input$event_name)

        removeModal()

        new_event_name <-  gsub("[^A-Za-z0-9 ]", "", input$event_name) #input$event_name


        form_id <- paste0(gsub("\\s", "", new_event_name), "_form")

        accordion_panel_insert(id = "accord_all_events",
                               panel = accordion_panel(title = new_event_name,
                                                       mod_form_ui(ns(form_id))))

        accordion_panel_close(id = "accord_all_events", values = TRUE)



        mod_form_server(
          form_id, constructs_vec, subtitles,
          choice_names = c("-", "+"), choice_values = c("present but negative", "present and positive"),
          event_name = new_event_name,
          PROJECT_NAME, accessToken, Day,
          SME, Instructor, pilot_vec, aircraft,
          size_of_btn = "normal", se = TRUE, full_workbook, fs_data_event = NULL
        )

        # all_events$namez <- c(all_events$namez, input$event_name)
        all_events$namez <- c(all_events$namez, new_event_name)


      }, ignoreInit = TRUE)


      # onBookmark(function(state) {
      #   state$values$Events <- isolate(eventz())
      # })
      #
      # onRestore(function(state) {
      #   if(!is.null(state$values$Events)) {
      #     # note: the list is converted to a dataframe!
      #     for(i in seq_len(nrow(state$values$Events))) {
      #       event <- state$values$Events[i, , drop = FALSE]
      #       form_id <- event$id
      #       accordion_panel_insert(
      #         id = "accord_all_events",
      #         panel = accordion_panel(
      #           title = event$name,
      #           mod_form_ui(ns(form_id))
      #         ),
      #         session = session
      #       )
      #     }
      #   }
      # })
      #
      #
      # setBookmarkExclude(c("add", "create_event", "event_name", "accord_all_events"))

      # observe({

        if (!is.null(all_events_data)){

          # for (new_event_namefs in unique(all_events_data$name)){
          lapply(unique(all_events_data$name), function(new_event_namefs) {

            form_id <- paste0(gsub("\\s", "", new_event_namefs), "_form")

            print(form_id)

            accordion_panel_insert(id = "accord_all_events",
                                   panel = accordion_panel(title = new_event_namefs,
                                                           mod_form_ui(ns(form_id))))

            accordion_panel_close(id = "accord_all_events", values = TRUE)


            fs_data_event <- all_events_data |>
              dplyr::filter(name == new_event_namefs)

            if (nrow(fs_data_event) < 1){
              fs_data_event <- NULL
            }

            mod_form_server(
              form_id, constructs_vec, subtitles,
              choice_names = c("-", "+"), choice_values = c("present but negative", "present and positive"),
              event_name = new_event_namefs,
              PROJECT_NAME, accessToken, Day,
              SME, Instructor, pilot_vec,
              aircraft,
              size_of_btn = "normal", se = TRUE, full_workbook,
              fs_data_event = fs_data_event
            )

            all_events$namez <- c(all_events$namez, new_event_namefs)
          }
          )
        }

      # })

      output$collapse_all_events_button <- renderUI({

        if (length(all_events$namez) < 1) {
          p("")
        } else {
          actionButton(ns("collapse_all"), "Collapse All Events",
                       class = "collapse-button",
                       icon = icon(name = "minimize"))
        }
      })




      output$choose_events_for_discussion <- renderUI({


        if (length(all_events$namez) < 1) {
          p("")
        } else {

          checkboxGroupInput(
            inputId = ns("checkz"),
            label = h5("Select the events for later discussion with Instructor:"),
            width = "1200px",
            choices = all_events$namez
          )
        }
      })


      return(
        reactive(list(event_names = all_events$namez,
                      # event_data = all_events$event_data,
                      highlighted_events = input$checkz))
      )

    }
  )
}
