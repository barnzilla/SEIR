# Define packages that will be used to extend base R
package_names <- c("janitor","readxl","dplyr","deSolve","tidyr","ggplot2", "ggpubr", "tidyverse", "shiny", "shinycssloaders", "DT", "scales", "plotly", "matrixcalc") 

# Install any packages that do not exist
install_packages <- lapply(package_names, FUN = function(x) if(! require(x, character.only = TRUE)) install.packages(x))

# Load the packages
load_packages <- lapply(package_names, require, character.only = TRUE)

# Define UI
ui <- fluidPage(
  # Add CSS here
  tags$head(
    tags$style(HTML("
      // CSS goes here
    "))
  ),
  # Application title
  titlePanel("COVID-19 model app"),
  
  # Layout
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload model parameters (Excel file)"),
      uiOutput("age_group"),
      uiOutput("compartment"),
      uiOutput("rate"),
      uiOutput("start_date"),
      uiOutput("max_time"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", br(), plotlyOutput("compartment_plot") %>% withSpinner(color = "#337ab7")),
        tabPanel("Summary statistics", br(), DTOutput("summary_statistics") %>% withSpinner(color = "#337ab7")),
        tabPanel("Model output", br(), DTOutput("model_output") %>% withSpinner(color = "#337ab7")),
        tabPanel("Initial conditions", br(), DTOutput("initial_conditions") %>% withSpinner(color = "#337ab7")),
        tabPanel("Parameters by age", br(), DTOutput("parameters_by_age") %>% withSpinner(color = "#337ab7")),
        tabPanel("Parameters by age x age", br(), DTOutput("parameters_by_age_x_age") %>% withSpinner(color = "#337ab7"))
      ),
      width = 9
    )
  )
)

# Define server logic 
server <- function(input, output) {
  # Read sheets from uploaded Excel file
  get_inputs <- reactive({
    file_to_read <- input$file
    if(is.null(file_to_read)) {
      return(list(parameters_by_age = data.frame(), parameters_by_age_x_age = data.frame(), initial_conditions = data.frame(), columns = NULL))
    } else {
      time_stuff   <- as.data.frame.from.tbl(readxl::read_excel(file_to_read$datapath, sheet = run_model()$sheet_names$parms.1d))  # other parameters
      time_stuff_m <- as.data.frame.from.tbl(readxl::read_excel(file_to_read$datapath, sheet = run_model()$sheet_names$parms.2d)) # c_, cr, cq, and beta
      input_stuff  <- as.data.frame.from.tbl(readxl::read_excel(file_to_read$datapath, sheet = run_model()$sheet_names$initial.conditions)) # initial values
      return(list(parameters_by_age = time_stuff, parameters_by_age_x_age = time_stuff_m, initial_conditions = input_stuff))
    }
  })
  
  # Function to process user input from the start date field
  get_start_date <- function() {
    start_date <- paste0("|", input$start_date, collapse = "")
    if(nchar(gsub("[|]", "", start_date)) == 10) {
      start_date <- as.Date(gsub("[|]", "", start_date), format = "%Y-%m-%d")
    } else {
      start_date <- NA
    }
    return(start_date)
  }
  
  # Compute summary statistics based on the age group and compartment(s) selected with Time converted to YYYY-mm-dd if the start date field is populated
  get_statistics <- reactive({
    if(nrow(run_model()$big_out) < 1 | (is.null(input$compartment) & is.null(input$rate)) | is.null(input$max_time)) {
      return()
    } else {
      # Function to compute min counts 
      get_min_count <- function(df, variable_of_interest) {
        df <- df %>% filter(Time <= input$max_time) %>% filter(!!rlang::sym(variable_of_interest) == min(!!rlang::sym(variable_of_interest))) %>% select(c(Time, all_of(variable_of_interest)))
        start_date <- get_start_date()
        if(! is.na(start_date)) {
          df$Time <- as.character(start_date + df$Time)
        } 
        df <- tibble(Description = names(df)[2], Min = as.integer(df[,2]), Day = df$Time)
      }
      
      # Function to compute max counts
      get_max_count <- function(df, variable_of_interest) {
        df <- df %>% filter(Time <= input$max_time) %>% filter(!!rlang::sym(variable_of_interest) == max(!!rlang::sym(variable_of_interest))) %>% select(c(Time, all_of(variable_of_interest)))
        start_date <- get_start_date()
        if(! is.na(start_date)) {
          df$Time <- as.character(start_date + df$Time)
        } 
        df <- tibble(Description = names(df)[2], Max = as.integer(df[,2]), Day = df$Time)
      }
      
      # Data frame with min stats
      df1 <- as.data.frame(t(sapply(paste0(c(input$compartment, input$rate), input$age_group), function(x) get_min_count(run_model()$big_out, x))))
      for(i in 1:ncol(df1)) {
        df1[,i] <- unname(unlist(df1[,i]))
      }
      
      # Data frame with max stats
      df2 <- as.data.frame(t(sapply(paste0(c(input$compartment, input$rate), input$age_group), function(x) get_max_count(run_model()$big_out, x))))
      for(i in 1:ncol(df2)) {
        df2[,i] <- unname(unlist(df2[,i]))
      }
      
      # Merge min and max data frames
      df <- cbind(df1, df2 %>% select(-Description))
      
      # Convert variable names to long form
      df$Description <- run_model()$lookup$long[match(gsub('[0-9]+', '', df$Description), run_model()$lookup$short)]
      
      return(list(df = df))
    }
  })
  
  # Build age group menu based on the number of age groups detected in the uploaded Excel file
  output$age_group <- renderUI({
    if(is.null(run_model()$nagegrp)) { 
      return() 
    } else {
      age_groups <- 1:run_model()$nagegrp
      names(age_groups) <- paste0("Age group ", age_groups)
      selectInput("age_group", "Select age group", choices = age_groups)
    }
  })
  
  # Build compartment menu
  output$compartment <- renderUI({
    if(is.null(run_model()$nagegrp)) { 
      return() 
    } else {
      checkboxGroupInput("compartment", label = "Select compartments", choices = list("Susceptible" = "S", "Latent" = "L_tot", "Infected" = "I_tot", "Recovered" = "R", "Dead" = "D"), selected = c("S", "L_tot", "I_tot", "R", "D"))
    }
  })
  
  # Build line plot based on the age group and compartment(s) selected with Time converted to YYYY-mm-dd if the start date field is populated
  output$compartment_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(nrow(run_model()$big_out) < 1 | is.null(input$age_group) | (is.null(input$compartment) & is.null(input$rate)) | is.null(input$max_time)) {
      return()
    } else {
      big_out <- run_model()$big_out
      nagegrp <- run_model()$nagegrp
      if (nagegrp > 1){
        #variables_of_interest <- as.vector(sapply(c("S","L_tot","I_tot","R","D"), function(x) paste0(x, input$age_group)))
        variables_of_interest <- as.vector(sapply(c(input$compartment, input$rate), function(x) paste0(x, input$age_group)))
        timelimit <- input$max_time
      } else {
        #variables_of_interest <- c("S","L_tot1","I_tot1","R","D")
        variables_of_interest <- c(input$compartment, input$rate)
        variables_of_interest <- gsub("L_tot", "L_tot1", variables_of_interest)
        variables_of_interest <- gsub("I_tot", "I_tot1", variables_of_interest)
        timelimit <- input$max_time
      }
      big_out_graphs <- big_out %>%
        select(c(Time, all_of(variables_of_interest))) %>%
        filter(Time <= timelimit) # I set a limit of days for the graphics
      
      # if needs nagegrp and lookup0
      get_plot <- function(data, age_group) {
        # Subset the data frame to include only the vectors of interest
        if (nagegrp > 1) {
          data_subset <- filter(data, meta_key %in% paste0(c("S","L_tot","I_tot","R","D", input$rate), age_group))
        } else {
          data_subset <- filter(data, meta_key %in% c("S","L_tot1","I_tot1","R","D", input$rate))
        }
        
        # Refactor the meta_key vector so that levels no longer represented in the vector are removed
        data_subset$meta_key <- factor(data_subset$meta_key)
        
        # Add labels to the factor, which will also appear in the legend
        data_subset$meta_key <- factor(data_subset$meta_key, levels = levels(data_subset$meta_key), labels = gsub("compartment", "", run_model()$lookup$long[match(gsub('[0-9]+', '', variables_of_interest), run_model()$lookup$short)]))
        
        names(data_subset) <- c("Day", "Compartment", "Individuals")
        data_subset$Individuals <- as.integer(data_subset$Individuals)
        
        start_date <- get_start_date()
        if(! is.na(start_date)) {
          data_subset$Day <- start_date + data_subset$Day
          x_lab_label <- paste0("Time (since ", format(start_date, format = "%B %d, %Y"), ")")
        } else {
          x_lab_label = "Time (days)"
        }
        
        # Output the plot
        p <- ggplot(data_subset, aes(x = Day, y = Individuals)) +
          geom_line(aes(color = Compartment), size = 0.85) +
          ggtitle(paste0("SEIR model, age group ", age_group)) +
          xlab(x_lab_label) +
          ylab("Count (individuals)") +
          scale_y_continuous(labels = comma) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_blank()
          )
        p <- ggplotly(p)
      }
      
      # Reshape SEIR model output from wide to long format
      big_out_long <- gather(big_out_graphs, key = meta_key, value = meta_value, 2:ncol(big_out_graphs), factor_key = TRUE)
      
      # Output the plots in a panel
      get_plot(big_out_long, input$age_group)
    }
  })
  
  # Render uploaded Excel file (the "inputs" sheet) in searchable/sortable table
  output$initial_conditions <- renderDT(
    get_inputs()$initial_conditions,
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 10, 
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
  
  # Build max_time menu based on the number of age groups detected in the uploaded Excel file
  output$max_time <- renderUI({
    if(is.null(run_model()$time_min) | is.null(run_model()$time_max)) { 
      return() 
    } else {
      sliderInput("max_time", "Select maximum time", min = run_model()$time_min + 1, max = run_model()$time_max, value = 365, step = 1)
    }
  })
  
  # Render the SEIR model output in searchable/sortable table
  output$model_output = renderDT(
    run_model()$big_out[,grepl(paste0("Time|", input$age_group, collapse = ""), names(run_model()$big_out))] %>% round(),
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 10, 
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
  
  # Render uploaded Excel file (the "time" sheet) in searchable/sortable table
  output$parameters_by_age <- renderDT(
    get_inputs()$parameters_by_age,
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 10, 
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
  
  # Render uploaded Excel file (the "time2" sheet) in searchable/sortable table
  output$parameters_by_age_x_age <- renderDT(
    get_inputs()$parameters_by_age_x_age,
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 10, 
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
  
  # Build rate menu
  output$rate <- renderUI({
    if(is.null(run_model()$nagegrp)) { 
      return() 
    } else {
      checkboxGroupInput("rate", label = "Select other statistics", choices = list("Incidence" = "IncI", "Hospitalized" = "I_ssh", "Quarantined" = "I_aq", "Isolated" = "Isolat"), selected = c(""))
    }
  })
  
  
  # Build start date field
  output$start_date <- renderUI({
    if(is.null(run_model()$nagegrp)) { 
      return() 
    } else {
      dateInput("start_date", "Select start date (for Time = 0)", value = "", format = "yyyy-mm-dd")
    }
  })
  
  # Render the summary statistics in searchable/sortable table
  output$summary_statistics <- renderDT(
    get_statistics()$df,
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 10, 
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
  
  # Run SEIR model by age groups
  run_model <- reactive({
    # generate bins based on input$bins from ui.R
    file_to_read <- input$file
    if(is.null(file_to_read)) {
      return(list(big_out = data.frame(), nagegrp = NULL, time_min = NULL, time_max = NULL, lookup = NULL, columns = NULL))
    } else {
      source("UtilitiesChunks.R")
      source("SEIR.n.Age.Classes.R")
      sheet_names_v2 = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs v2",auxiliary.vars="Intermediate calculations")
      
      results.sheet.v2.good = SEIR.n.Age.Classes(file_to_read$datapath,sheet_names_v2,seq(0,1499,1) ) # scale.rate.to.size  FALSE by default now      
    
      results = results.sheet.v2.good
      
      # continue on below with listOut as before but should consider using results$solution
      #listOut = results$listOut.to.be.decomissioned  
      listOut = results$solution 
      nagegrp = ncol(results$input.info$initial.conditions) - 1
      
      # Merge the data
      big_out <- bind_rows(listOut, .id = "column_label") %>% distinct(time, .keep_all= TRUE)
      xx <- yy <- df <- df2 <- NULL
      for (p in 1: nagegrp){
        if (nagegrp>1){varsc<-names(big_out)[grepl(p,names(big_out))]}else{varsc<-names(big_out)}
        df <-big_out %>% 
          select(one_of(varsc))
        xx <- df %>%
          select_at(vars(starts_with("L"))) %>% 
          rowSums()
        yy <-df %>% 
          select_at(vars(starts_with("I"))) %>% 
          rowSums()
        df2 <- cbind(xx,yy)
        big_out <- cbind(big_out,df2)
        names(big_out)[c(dim(big_out)[2]-1,dim(big_out)[2])]<-c(paste0(c("L_tot","I_tot"),p))
        varsc<-names(big_out)[grepl(p,names(big_out))]
      }
      
      parameters_by_age <- as.data.frame.from.tbl(readxl::read_excel(file_to_read$datapath, sheet = sheet_names_v2$parms.1d))
      
      # Compute incidence
      for(i in 1:nagegrp) {
        sigma <- as.numeric(parameters_by_age %>% filter(agegrp == i) %>% select(sigma) %>% slice(1))
        v <- c()
        I_tot <- unname(unlist(big_out %>% select(all_of(paste0("I_tot", i)))))
        L_tot <- unname(unlist(big_out %>% select(all_of(paste0("L_tot", i)))))
        
        for(j in 1:nrow(big_out)) {
          if(j == 1) {
            v[j] <- I_tot[1]
          } else {
            v[j] <- L_tot[j - 1] * sigma 
          }
        }
        big_out[[paste0("IncI", i)]] <- assign(paste0("IncI", i), v)
      } 
      
      # Compute Isolated
      for(i in 1:nagegrp) {
        I_smis <- unname(unlist(big_out %>% select(all_of(paste0("I_smis", i)))))
        I_ssis <- unname(unlist(big_out %>% select(all_of(paste0("I_ssis", i)))))
        v <- c()
        for(j in 1:nrow(big_out)) {
          v[j] <- I_smis[j] + I_ssis[j]
        }
        big_out[[paste0("Isolat", i)]] <- assign(paste0("Isolat", i), v)
      } 
      
      # Organize model output vectors alphabetically
      big_out <- big_out %>% select(order(colnames(big_out))) %>% select(time, everything())
      
      # Rename the time vector
      colnames(big_out)[1] <- "Time"
      return(list(big_out = big_out, nagegrp = nagegrp, sheet_names = sheet_names_v2, time_min = min(parameters_by_age$tmin), time_max = max(parameters_by_age$tmax), lookup = tibble(short = c("S", "L_tot", "I_tot", "R", "D", "IncI", "I_ssh", "I_aq", "Isolat"), long = c("Susceptible compartment", "Latent compartment", "Infected compartment", "Recovered compartment", "Dead compartment", "Incidence", "Hospitalized", "Quarantined", "Isolated"))))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)