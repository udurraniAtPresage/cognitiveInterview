#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyjs
#' @import firebase
#' @import waiter
#' @noRd
#'
# Header elements and Theme
# ui_secure <- page_fluid(
#   firebaseUIContainer(),
#
#   ## Theme
#
#   theme = bs_theme(
#     version = 5,
#     bg = "#1a1a1a",
#     fg = "#ffffff",
#     primary = "#00abc5",
#     success = "#39b54a",
#     danger = "#d7df23",
#     warning = "#8dc63f",
#     base_font = font_google("Libre Franklin"),
#     preset = "shiny",
#     heading_font = font_google("Libre Franklin")
#   ),
#
#
#   ## This will show the ui_secret (defined below) once the user is signed in
#   shiny::uiOutput("logged_in_ui")
# )

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      theme = bs_theme(
        version = 5,
        bg = "#1a1a1a",
        fg = "#ffffff",
        primary = "#00abc5",
        success = "#39b54a",
        danger = "#d7df23",
        warning = "#8dc63f",
        base_font = font_google("Libre Franklin"),
        preset = "shiny",
        heading_font = font_google("Libre Franklin")
      ),

      title = tags$span(
        tags$img(
          src = "www/logo.png",
          width = "46px",
          height = "auto",
          class = "me-3"
        ),
        "Cognitive Interview"
      ),

      sidebar = sidebar(
        # firebase::reqSignin(instructor_pilots_name_inputs),
        instructor_pilots_name_inputs,
        br(), br(), br(),
        width = 300
      ),


      # conditionalPanel(
      #   condition = "input.day !== ''",
      #   shiny::uiOutput("day_page")
      # ),

      nav_panel(
        title = "Data Collection",
        shiny::uiOutput("day_page")
      ),

      # nav_panel(
      #   title = "Saved Data",
      #
      # ),

      nav_spacer(),

      nav_item(
        uiOutput("sign_out_button")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    firebaseUIContainer(),
    # bundle_resources(
    #   path = app_sys("app/www"),
    #   app_title = "cognitiveInterview"
    # ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # tags$script(type = "text/javascript", src = "www/scrollPage.js"),
    tags$script(type = "text/javascript", src = "www/scrollPageNavbar.js"),
    tags$button(id = "scroll-to-top-button",
                onclick = "topFunction()",
                "⇧"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(src = "www/reset.js"),
    tags$script(src = "www/handwriting.js"),
    tags$script(src = "www/handwriting.canvas.js"),
    tags$script(src = "www/handwriting_for_shiny3.js"),
    tags$script(src = "www/change_color.js"),
    firebase::useFirebase(), # import dependencies
    shinyjs::useShinyjs(),
    waiter::autoWaiter()
  )
}
