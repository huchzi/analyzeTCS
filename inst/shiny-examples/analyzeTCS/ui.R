ui <- fluidPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "analyzeTCS.css"),
  tags$head(),

  # Application title
  titlePanel("Analyze tCS output files"),

  sidebarLayout(
    sidebarPanel(
      htmlOutput("css"),
      fileInput("raw_csv", "Load file", multiple = FALSE),
      wellPanel(
        class = "pat-info",
        htmlOutput("patid")
      ),
      numericInput("filter", label = "Neutral Density Filter", value = .3, min = 0, max = 5, step = .1),
      radioButtons("field", label = "Test field",
                   choices = c("outer", "inner"),
                   selected = "outer"),
      radioButtons("stimulus_type", label = "Stimulus Type",
                   choices = c("L-cone", "M-cone", "S-cone", "Rod", "Mixed"),
                   selected = "character(0)"),
      radioButtons("use_threshold", label = "Use as threshold...",
                   choices = c("both", "first", "second", "gamut", "none"),
                   selected = "both"),
      actionButton("add_to_table", "Add to table"),
      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Stimulus condition",
                 splitLayout(
                   plotOutput("luminance"),
                   plotOutput("waveform"),
                   plotOutput("photoreceptors")
                 ),
                 dataTableOutput("parsedData")
        ),
        tabPanel("Thresholds",
                 plotOutput("staircases"),
                 splitLayout(
                   plotOutput("gamut"),
                   plotOutput("false_positives")
                 ),
                 verbatimTextOutput("parsedData2"),
        ),
        tabPanel("Raw data", verbatimTextOutput("rawData")),
        tabPanel("Collect data",
                 wellPanel(
                   downloadButton("downloadData", "Download")),
                 br(),
                 wellPanel(
                   dataTableOutput("sensitivity_table")),
                 br(),
                 plotOutput("results")),
        tabPanel("Database",
                 br(),
                 br(),
                 actionButton("attach_database", "Attach database"),
                 br(),
                 br(),
                 actionButton("detach_database", "Detach database"),
                 br(),
                 br(),
                 textInput("password", "Password"),
                 br(),
                 br(),
                 textOutput("message")
        )
      ),
      width = 9
    )
  )
)
