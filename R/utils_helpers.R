# Global vars -------------------------------------------------------------

PROJECT_NAME <- Sys.getenv("FIREBASE_PROJECT_ID")

constructs_vec <- c(
  "Functional", "Hierarchical", "Task Empirical",
  "Critical", "Affective", "Anticipatory",
  "Compensatory", "Relational", "Environmental"
)

subtitles <- c(
  Functional = "Instrument and Equipment Knowledge",
  Hierarchical = "Knowing the Procedures            ",
  "Task Empirical" = "Knowing the Limits                ",
  Relational = "Keeping Each Other Safe           ",
  Environmental = "Company Support for Safety        ",
  Anticipatory = "Seeing the Threat                 ",
  Compensatory = "Adjusting to the Threat           ",
  Affective = "Gut Feel for Threats              ",
  Critical = "Relying on Experience             "
)




# User Interface ----------------------------------------------------------

## SME, Instructor, Pilot Names and Day

instructor_pilots_name_inputs <- shiny::tagList(

  # textInput(
  #   inputId = "smename",
  #   label = HTML("Name of Subject Matter Expert: <span style='color:red'>*</span>"),
  #   value = ""
  # ),

  shiny::selectizeInput(
    inputId = "smename",
    label = shiny::HTML("Name of Subject Matter Expert: <span style='color:red'>*</span>"),
    choices = c("Bill Curtis", "Daniel Marchesseault", "Jacques Mignault", "Mark Stow", "Martin Smith", "Michel Charette"),
    options = list(
      placeholder = 'Select SME',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),


  shiny::textInput(
    inputId = "iname",
    label = HTML("Name of Instructor: <span style='color:red'>*</span>"),
    value = ""
  ),

  shiny::selectInput(
    inputId = "numpilots",
    label = "Select # of pilots:",
    choices = c(1, 2), selected = 1
  ),

  shiny::uiOutput("pilotz"),

  # textInput(
  #   inputId = "pname1",
  #   label = "Name of Pilot 1:",
  #   value = ""
  # ),
  #
  # textInput(
  #   inputId = "pname2",
  #   label = "Name of Pilot 2:",
  #   value = ""
  # ),

  shiny::textInput(
    inputId = "aircraft",
    label = shiny::HTML("Aircraft type: <span style='color:red'>*</span>"),
    value = ""
  ),

  shiny::selectizeInput(
    inputId = "day",
    label = HTML("Select Day: <span style='color:red'>*</span>"),
    choices = c("Day1", "Day2", "Day3", "Day4", "Day5", "Day6", "Day7",
                "Day8", "Day9", "Day10"),
    options = list(
      placeholder = 'Select day',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),

  actionButton("generate", "Generate", class = "save-button"),

  shiny::br(),
  shiny::br(),


  shiny::downloadButton("save_all", label = "Download all data", class = "download-button")
  # br(),
  # br(),
  # br()
)
