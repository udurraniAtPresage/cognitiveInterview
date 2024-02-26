#' mod_handwriting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton observeEvent observe updateTextAreaInput reactive
#' @importFrom bslib layout_columns
mod_handwriting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      actionButton(ns("clear_canvas"), "Clear Canvas", class = "grey-button"),
      actionButton(ns("undo"), "Undo", class = "grey-button"),
      actionButton(ns("redo"), "Redo", class = "grey-button"),
      actionButton(ns("send"), "Handwriting to text", class = "save-button"),
      col_widths = c(3, 2, 2, 5)
    ),
    textAreaInput(ns("manual_text"), "Rendered Text", value = "", width = "1000px"),
    tags$canvas(id = ns("handwritingCanvas"),
                # width = "2400px", height = "600px",
                class = "dynamic-canvas-size"
    )

  )
}

#' mod_handwriting Server Functions
#'
#' @noRd
mod_handwriting_server <- function(id, run) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      prefix <- ns("") # = "module1-foo-"
      nested_id <- sub("-$", "", prefix) # remove the trailing hyphen

      observeEvent(run(), {
        observe({
          session$sendCustomMessage("initialize", message = nested_id)
        })

        observeEvent(input$clear_canvas, {
          session$sendCustomMessage("clearCanvas", message = nested_id)
        })

        observeEvent(input$send, {
          session$sendCustomMessage("sendCanvas", message = nested_id)
        })

        observeEvent(input$undo, {
          print("Undo button clicked")  # Diagnostic output
          session$sendCustomMessage("undoCanvas", message = nested_id)
        })

        observeEvent(input$redo, {
          session$sendCustomMessage("redoCanvas", message = nested_id)
        })

        observeEvent(input$recognized_text, {
          if (!is.null(input$recognized_text)) {
            updateTextAreaInput(session, "manual_text", value = input$recognized_text)
          }
        })


        # observe({
        #   session$sendCustomMessage("initCanvas", message = NULL)
        # })

      })

      return(reactive(input$manual_text))

    }
  )
}

