# Define packages 
package_names <- c(
  "DT", 
  "ggplot2", 
  "lubridate", 
  "plotly", 
  "plyr", 
  "scales", 
  "shiny", 
  "shinycssloaders", 
  "shinythemes", 
  "tidyverse"
)

# Install packages if they haven't been installed previously
install_packages <- lapply(package_names, FUN = function(x) if(! require(x, character.only = TRUE)) install.packages(x))

# Load packages
load_packages <- lapply(package_names, require, character.only = TRUE)

# Define UI
ui <- navbarPage(
    windowTitle = HTML("Parameter sweep app"),
    title = div("Parameter sweep app"),
    theme = shinytheme("united"),
    tabPanel("Data visualization",
    sidebarPanel(
      uiOutput("output_file"),
      uiOutput("y_axis"),
      uiOutput("x_axis"),
      width = 3
    ),
    mainPanel(
      #br(), div("Click the ", tags$strong("Column visibility"), " button to view additional variables in the ", tags$strong("outcomes.summary.df"), " dataframe.", style = "background-color: #ffe3d8; color: #d34615; border: 1px solid #ffc8b2; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(), 
      #DTOutput("get_data") %>% withSpinner(color = "#d34615"), br(), br(),
      plotlyOutput("get_plot") %>% withSpinner(color = "#d34615"), br(), br(), br(),
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
    if(length(t) < 2) object <- NULL else object <- t
    return(object)
  }
  
  # Build scatter plot
  output$get_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(is.null(cached$object) | is.null(input$x_axis) | is.null(input$y_axis)) {
      return()
    } else {
      point_size <- 0.75
      element_text_size <- 12
      ggplotly(ggplot(cached$object, aes(x = !!rlang::sym(input$x_axis), y = !!rlang::sym(input$y_axis))) +
      geom_point(aes(color = !!rlang::sym(input$x_axis)), size = point_size) +
      #xlab(x_label) +
      #ylab("Cumulative incidence") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = element_text_size),
        axis.title.x = element_text(size = element_text_size),
        axis.title.y = element_text(size = element_text_size),
        legend.text = element_text(size = element_text_size),
        legend.title = element_blank()
      ))
    }
  })
  
  # Render data object in a searchable/sortable table
    output$get_data <- renderDT(
      {
        cached$object <- get_object(input$output_file2)
      },
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
      if(is.null(cached$files)) {
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
      } 
      selectInput("output_file", "Output file", choices = cached$files, selected = input$output_file2)
    })
    
    # Build output file menu
    output$output_file2 <- renderUI({
      selectInput("output_file2", "Output file", choices = cached$files, selected = input$output_file)
    })
    
    # Build x_axis menu
    output$x_axis <- renderUI({
      if(is.null(cached$object)) { 
        return() 
      } else {
        selectInput("x_axis", "X axis", choices = cached$options)
      }
    })
    
    # Build y_axis menu
    output$y_axis <- renderUI({
      cached$object <- get_object(input$output_file)
      if(is.null(cached$object)) {
        return()
      } else {
        options <- sort(names(cached$object))
        names(options) <- options
        options <- options[options != "etiquette"]
        cached$options <- options
        selectInput("y_axis", "Y axis", choices = cached$options)
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)