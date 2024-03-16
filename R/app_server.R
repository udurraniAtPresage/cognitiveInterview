#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import firebase
#' @import openxlsx
#' @import jsonlite
#' @import httr
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @noRd
app_server <- function(input, output, session) {

  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(updateQueryString)

  session$allowReconnect(TRUE)


  observeEvent(input$start_over, {
    session$reload()
  })

  ## Firebase authentication ---------------------------------------------------
  ## Persistence: 'local' means the session would persist even when window is closed
  ### That means a user will remain signed in even when the app window is closed
  ### Only email and gmail are enabled.
  # f <- FirebaseUI$new(
  #   persistence = "local"
  # )$set_providers(
  #   email = TRUE,
  #   google = TRUE)$
  #   launch()
  #
  #
  # ## Sign in
  # user <- reactive({f$get_signed_in()})
  #
  # ## Get the email and access token of the signed-in user (for access to Cloud Firestore database)
  # ### There is no client library for Cloud Firestore. So, this app uses REST API
  # ### Using the REST API requires access token
  # this_email <- reactive({user()$response$email})
  # user_token <- reactive({user()$response$stsTokenManage$accessToken})


  # ui_secret (defined in user interface) is shown after signing in
  # output$logged_in_ui <- shiny::renderUI({
  #   f$req_sign_in()
  #   ui_secret
  # })



  # Sign out ----------------------------------------------------------------
  ## Button for signing out
  # output$sign_out_button <- shiny::renderUI({
  #   f$req_sign_in()
  #   actionButton("signout", "Sign out", class = "save-button")
  # })
  #
  # ## User signs out when the button with ID "signout" is clicked
  # observeEvent(input$signout, {
  #   f$sign_out()
  #   f$launch()
  # })


  user_token <- reactive({
    umair <- sign_in("udurrani@test.com", Sys.getenv("PASS"), Sys.getenv("FIREBASE_API_KEY"))
    umair$idToken

  })


  # Get all name combinations (SME_Instructor_Pilot1_Pilot2) from cognitive_interview collection
  all_name_combos <- reactive({get_all_name_combos(user_token(), PROJECT_NAME)})



  matching_names <- reactive({
    req(input$smename)
    SME_list <- unlist(strsplit(input$smename, " "))
    all_name_combos()[grepl(paste(SME_list, collapse = "|"), all_name_combos())]
  })

  output$data_select <- renderUI({
    selectInput(
      inputId = "data_select",
      label = "Select existing data:",
      choices = c("New", matching_names()),
      selected = "New"
    )
  })


  # all_events_for_all_days <- reactive({
  #   get_data_for_all_days_for_a_combo(user_token(),
  #                                     matching_name = input$data_select,
  #                                     project_name = PROJECT_NAME)
  # })

  all_events_of_a_day <- reactive({
    req(input$day)
    get_ci_data_for_a_day_v2(user_token(),
                             matching_name = input$data_select,
                             input$day,
                             project_name = PROJECT_NAME)
    # all_events_for_all_days() |>
    #   dplyr::filter(Day == input$day)
  })




  output$num_pilots_selection <- renderUI({

    if (!is.null(all_events_of_a_day())){

      if (is.null(all_events_of_a_day()$Pilot2)){
        number_of_pilots <- 1
      } else {
        number_of_pilots <- 2
      }

      shiny::selectInput(
        inputId = "numpilots",
        label = "Select # of pilots:",
        choices = c(1, 2),
        selected = number_of_pilots
      )
    } else {
      shiny::selectInput(
        inputId = "numpilots",
        label = "Select # of pilots:",
        choices = c(1, 2),
        selected = 1
      )
    }
  })



  pilot1_id <- reactive({
    if (!is.null(all_events_of_a_day())){
      unique(all_events_of_a_day()$Pilot1)
    } else{
      ""
    }
  })

  pilot2_id <- reactive({
    if (!is.null(all_events_of_a_day())){
      unique(all_events_of_a_day()$Pilot2)
    } else{
      ""
    }
  })


  observe({
    req(input$numpilots)
    req(input$day)

    if (!is.null(all_events_of_a_day())){
      if (input$numpilots == "2"){
        # print("2 pilots")
        # print(unique(all_events_of_a_day()$Pilot1))
        # print(unique(all_events_of_a_day()$Pilot2))
        # print(class(unique(all_events_of_a_day()$Pilot1)))
        output$pilotz <- renderUI({
          tagList(
            textInput(
              inputId = "pname1",
              label = HTML("ID of Pilot 1: <span style='color:red'>*</span>"),
              value = pilot1_id() #unique(all_events_of_a_day()$Pilot1)
            ),

            textInput(
              inputId = "pname2",
              label = HTML("ID of Pilot 2: <span style='color:red'>*</span>"),
              value = pilot2_id() #unique(all_events_of_a_day()$Pilot2)
            )
          )
        })



      } else if (input$numpilots == "1"){
        # print("1 pilot")
        # print(unique(all_events_of_a_day()$Pilot1))
        # print(class(unique(all_events_of_a_day()$Pilot1)))
        output$pilotz <- renderUI({
          tagList(
            textInput(
              inputId = "pname1",
              label = HTML("ID of Pilot: <span style='color:red'>*</span>"),
              value = pilot1_id() #unique(all_events_of_a_day()$Pilot1)
            )
          )
        })
      }

    } else {

      if (input$numpilots == "2"){
        output$pilotz <- renderUI({
          tagList(
            textInput(
              inputId = "pname1",
              label = HTML("ID of Pilot 1: <span style='color:red'>*</span>"),
              value = ""
            ),

            textInput(
              inputId = "pname2",
              label = HTML("ID of Pilot 2: <span style='color:red'>*</span>"),
              value = ""
            )
          )
        })



      } else if (input$numpilots == "1"){
        output$pilotz <- renderUI({
          tagList(
            textInput(
              inputId = "pname1",
              label = HTML("ID of Pilot: <span style='color:red'>*</span>"),
              value = ""
            )
          )
        })
      }
    }

  })



  output$instructor_select <- renderUI({

    if (!is.null(all_events_of_a_day())){
      shiny::textInput(
        inputId = "iname",
        label = HTML("Name of Instructor: <span style='color:red'>*</span>"),
        value = unique(all_events_of_a_day()$Instructor)
      )

    } else {
      shiny::textInput(
        inputId = "iname",
        label = HTML("Name of Instructor: <span style='color:red'>*</span>"),
        value = ""
      )
    }
  })



  output$aircraft_select <- renderUI({
    if (!is.null(all_events_of_a_day())){
      # print(unique(all_events_of_a_day()$Aircraft))
      shiny::textInput(
        inputId = "aircraft",
        label = shiny::HTML("Aircraft type: <span style='color:red'>*</span>"),
        value = unique(all_events_of_a_day()$Aircraft)
      )
    } else {
      shiny::textInput(
        inputId = "aircraft",
        label = shiny::HTML("Aircraft type: <span style='color:red'>*</span>"),
        value = ""
      )
    }
  })



  ## Vars made reactive:
  SME <- reactive(input$smename)
  Instructor <- reactive(input$iname)
  Pilot1 <- reactive(input$pname1)
  Pilot2 <- reactive(input$pname2)
  aircraft <- reactive(input$aircraft)




  pilotz_vec <- reactive({
    req(input$numpilots)

    if (input$numpilots == "2"){

      pilotz_vec <- c(Pilot1(), Pilot2())

    } else if (input$numpilots == "1"){

      pilotz_vec <- Pilot1()
    }

    pilotz_vec

  })



  # Create an empty workbook
  # wb <- createWorkbook()

  full_workbook <- reactiveVal(value = createWorkbook())

  ## Observe selected day input and then create UI for that day
  observeEvent(input$generate, {
    ## Check if a day is selected
    if (!is.null(input$day) && input$day != "") {
      ## Render the day page
      output$day_page <- renderUI({
        tagList(
          h4(gsub("Day(\\d+)", "Day \\1", input$day)),
          create_day_page(input$day)
        )
      })


      number <- as.numeric(gsub("[^0-9]", "", input$day))
      Day <- paste0("Day", number)

      ## Data from firestore
      fs_data <- get_ci_data_for_a_day(
          PROJECT_NAME,
          accessToken = user_token(),
          Day = Day,
          SME = SME(),
          Instructor = Instructor(),
          pilot_vec = c(Pilot1(), Pilot2())
        )

      if (!is.null(fs_data)){
      all_events_data <- fs_data |>
        filter(!grepl("isd|eoi", name))
      } else {
        all_events_data <- NULL
      }

      # if (is.null(all_events_data) | nrow(all_events_data) < 1){
      #   all_events_data <- NULL
      # }

      if (!is.null(fs_data)){
      isd_data <- fs_data |>
        filter(grepl("isd", name))
      } else {
        isd_data <- NULL
      }


      # if (is.null(isd_data) | nrow(isd_data) < 1){
      #   isd_data <- NULL
      # }

      if (!is.null(fs_data)){
      stu_data <- fs_data |>
        filter(grepl("eoi_stu", name))
      } else {
        stu_data <- NULL
      }

      # if (is.null(stu_data) | nrow(stu_data) < 1){
      #   stu_data <- NULL
      # }

      if (!is.null(fs_data)){
      ins_data <- fs_data |>
        filter(grepl("eoi_inst", name))
      } else {
        ins_data <- NULL
      }

      # if (is.null(ins_data) | nrow(ins_data) < 1){
      #   ins_data <- NULL
      # }


      ## All events server
      event_info <- mod_all_events_server(id = paste0("all_events_", number),
                                          constructs_vec, subtitles,
                                          PROJECT_NAME, user_token, Day,
                                          SME = SME, Instructor = Instructor,
                                          pilot_vec = pilotz_vec,
                                          aircraft = aircraft,
                                          full_workbook, all_events_data)



      ## Instructor-Student Debrief
      mod_IS_server(paste0("IS_", number), constructs_vec, subtitles,
                    PROJECT_NAME, user_token, Day,
                    SME = SME, Instructor = Instructor,
                    pilot_vec = pilotz_vec,
                    aircraft = aircraft,
                    event_info = event_info, full_workbook, isd_data)


      ## Student Interview
      selected_eoi <- mod_stu_interview_server(id = paste0("stu_interview_", number),
                                               constructs_vec, subtitles,
                                               PROJECT_NAME, user_token, Day,
                                               SME = SME, Instructor = Instructor,
                                               pilot_vec = pilotz_vec,
                                               aircraft = aircraft,
                                               event_info = event_info, full_workbook, stu_data)


      ## Instructor Interview
      mod_inst_interview_server(id = paste0("inst_interview_", number),
                                constructs_vec, subtitles,
                                PROJECT_NAME, user_token, Day,
                                SME = SME, Instructor = Instructor,
                                pilot_vec = pilotz_vec,
                                aircraft = aircraft,
                                selected_eoi = selected_eoi, full_workbook, ins_data)


      # if (class(full_workbook()) == "Workbook"){
      #   saveWorkbook(full_workbook(), "writeDataTableExample.xlsx", overwrite = TRUE)
      # }
    }
  })


  output$save_all <- downloadHandler(
    filename = function() {
      # paste0(SME(), "_", Instructor(), "_", pilotz_vec(), "_", input$day, ".xlsx")
      paste0(SME(), "_", Instructor(), "_", pilotz_vec(), ".xlsx")
    },
    content = function(file) {
      # if ("Sheet1" %in% full_workbook()$sheet_names){
      #   removeWorksheet(full_workbook(), sheet = "Sheet1")
      # }
      # saveWorkbook(full_workbook(), file, overwrite = TRUE)
      if (input$data_select == "New"){

        if (length(pilotz_vec()) == 2){
          Pilots <- paste(pilotz_vec(), collapse = "_")
        } else{
          Pilots <- pilotz_vec()
        }

        name_combo <- paste0(
          strsplit(x = SME(), split = " ")[[1]][1],
          "_",
          strsplit(x = Instructor(), split = " ")[[1]][1],
          "_",
          Pilots
        )
        # print(name_combo)
      } else {
        name_combo <- input$data_select
      }
      all_events_for_all_days <- get_data_for_all_days_for_a_combo(user_token(),
                                                                   matching_name = name_combo,
                                                                   project_name = "brpa-dev")
      if (length(pilotz_vec()) == 2){
        all_events_for_all_days <- all_events_for_all_days |>
          dplyr::relocate(
            Day, SME, Instructor, Aircraft,
            Pilot1, Pilot1_title, Pilot1_status,
            Pilot2, Pilot2_title, Pilot2_status,
            Event = name, Event_Start, Event_End,
            everything()
          )
      } else {
      all_events_for_all_days <- all_events_for_all_days |>
        dplyr::relocate(
          Day, SME, Instructor, Aircraft,
          Pilot1, Pilot1_title, Pilot1_status,
          Event = name, Event_Start, Event_End,
          everything()
        )
}
      full_workbook2 <- createWorkbook()

      addWorksheet(full_workbook2, sheetName = "All_Data", gridLines = TRUE)
      writeDataTable(full_workbook2, sheet = "All_Data",
                     x = all_events_for_all_days,
                     colNames = TRUE)

      saveWorkbook(full_workbook2, file, overwrite = TRUE)
    }
  )
}
