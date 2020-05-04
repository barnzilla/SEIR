# Load the packages
library("DT")
library("ggplot2")
library("lubridate")
library("plotly")
library("plyr")
library("scales")
library("shiny")
library("shinycssloaders")
library("shinythemes")
library("tidyverse")

# Define UI
ui <- navbarPage(
    windowTitle = HTML("Parameter sweep explorer"),
    title = div("Parameter sweep explorer"),
    theme = shinytheme("united"),
    tabPanel("Data visualization",
             sidebarPanel(
               uiOutput("output_file"),
               uiOutput("x_axis"),
               uiOutput("y_axis"),
               width = 3
             ),
             mainPanel(
               #br(), div("Click the ", tags$strong("Column visibility"), " button to view additional variables in the ", tags$strong("outcomes.summary.df"), " dataframe.", style = "background-color: #ffe3d8; color: #d34615; border: 1px solid #ffc8b2; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(), 
               #DTOutput("get_data") %>% withSpinner(color = "#d34615"), br(), br(),
               width = 9
             )
    ),
    tabPanel("View output file",
         sidebarPanel(
           uiOutput("output_file2"),
           width = 3
         ),
        mainPanel(
            #br(), div("Click the ", tags$strong("Column visibility"), " button to view additional variables in the ", tags$strong("outcomes.summary.df"), " dataframe.", style = "background-color: #ffe3d8; color: #d34615; border: 1px solid #ffc8b2; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(), 
            DTOutput("get_data") %>% withSpinner(color = "#d34615"), br(), br(),
            width = 9
        )
    ),
    tags$head(tags$style(HTML('
      // Custom CSS here
    ')))
)

# Define server logic
server <- function(input, output) {
  # Cache select data structures
  cached <- reactiveValues()
  
  # Get object from vector element
  get_object <- function(x) {
    t <- tryCatch(get(x), error = function(e) "Not found")
    l <- ifelse(t == "Not found", return(), return(object = get(x)))
    return(l)
  }  
  
  # Render data object in a searchable/sortable table
    output$get_data <- renderDT(
      get_object(input$output_file2),
      extensions = c("Buttons", "Scroller"), 
      rownames = FALSE,
      options = list(
          columnDefs = list(list(visible = FALSE, targets = c())),
          pageLength = 25, 
          dom = "Bfrtip", 
          buttons = c("colvis", "copy", "csv", "excel", "pdf"), 
          deferRender = TRUE, 
          searchDelay = 500,
          initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
              "}"
          )
      )
    )
    
    # Build output file menu
    output$output_file <- renderUI({
      # Check if objects exists
      output_files <- unlist(unname(sapply(c("df.sweep", "outcomes.summary.df", "parms.tried.df"), function(x) if(exists(x)) x)))
      
      # Conditional menu based on whether output_files is null
      if(is.null(output_files)) {
        files <- "No output files exist"
      } else {
        files <- c("Please select...", output_files)
      }
      names(files) <- files
      cached$files <- files
      selectInput("output_file", "Output file", choices = cached$files, selected = input$output_file2)
    })
    
    # Build output file menu
    output$output_file2 <- renderUI({
      selectInput("output_file2", "Output file", choices = cached$files, selected = input$output_file)
    })
}

# Run the application
shinyApp(ui = ui, server = server)