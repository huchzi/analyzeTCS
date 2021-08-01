ui <- fluidPage(
  
  # Application title
  titlePanel("Analyze tCS output files"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("raw_csv", "Load file", multiple = FALSE),
      wellPanel(
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
                   choices = c("both", "first", "second", "gamut"),
                   selected = "both"),
      actionButton("add_to_table", "Add to table"),
      width = 3
    ),
    
    # Show a plot of the generated distribution
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
                 plotOutput("results"))
      ),
      width = 9
    )
  )
)