color_gradient <- function(dt, column_names, gradient_colors = c("#6666FF", "#DDDDDD", "#FF6666")) {
  col_func <- colorRampPalette(gradient_colors)
  
  for (column_name in column_names) {
    # Get min & max values of the column
    col_values <- sort(unique(dt$x$data[[column_name]]), decreasing = FALSE)
    
    # Define breakpoints between min & max
    if (length(col_values) > 1) {
      breaks <- seq(min(col_values), max(col_values), length.out = length(col_values))
    } else {
      breaks <- col_values  # If only one unique value, no breaks needed
    }
    
    dt <- dt %>%
      formatStyle(
        column_name, 
        backgroundColor = styleInterval(
          breaks[-1],  # Use all breakpoints except the first
          col_func(length(breaks))  # Generate matching colors
        )
      )
  }
  
  return(dt)
}


server <- function(session, input, output) {
  data_source <- reactiveVal(NULL)
  
  # Observe file input
  observeEvent(input$boostmut_csv, {
    data_source("file")
  })
  
  # Observe button click for example data
  observeEvent(input$example, {
    data_source("example")
  })
  
  # Reactive dataframe
  df <- reactive({
    req(data_source())  # Ensure something has been selected
    
    # Determine the source
    if (data_source() == "file") {
      req(input$boostmut_csv)
      path <- input$boostmut_csv$datapath
    } else {
      path <- "leh_example2.csv"  # Load example file
    }
    
    print("Loading Data...")
    df <- read_csv(path, col_names = TRUE, show_col_types = FALSE, name_repair = "unique_quiet") %>% as.data.frame()
    
    rownames(df) <- df[,1]
    df <- df[,-1]
    float_cols <- sapply(df, is.numeric)
    df$mean <- apply(df[,float_cols], 1, mean)
    df[, float_cols] <- round(df[, float_cols], 3)
    
    df <- df[order(df$mean, decreasing = TRUE), c('mean', setdiff(colnames(df), 'mean'))]
  })

  
  output$mutation_table <- renderDT({
    df <- df()
    if (input$color_cols == 'All') {
      target_cols <- colnames(df)
    } else {
      target_cols <- 'mean'
    }
    datatable(
      df,
      extensions = 'FixedColumns',
      style = 'bootstrap',
      options = list(scrollX = TRUE, # Enable horizontal scrolling
                     fixedColumns = list(leftColumns = 1),  # Freeze the first column
                     paging = FALSE,
                     info = FALSE,
                     scrollY = '35vh', 
                     fixedHeader = TRUE
      )) %>% 
      formatStyle(0, target = 'row', lineHeight='70%') %>% 
      color_gradient(target_cols)
    
  })
  
  output$corr_plot_or_not <- renderPlot({
    req(df(), input$corr_1)
    df <- df()
    # browser()
    ggplot(df) + 
      geom_point(aes(x = .data[[input$corr_1]], y=.data[[input$corr_2]])) +
      # geom_point(aes(x = foldx, y=hpsasa_s)) +
      geom_abline() +
      coord_fixed() +
      scale_x_continuous(limits = c(min(df[,c(input$corr_1, input$corr_2)]), max(df[,c(input$corr_1, input$corr_2)]))) +
      scale_y_continuous(limits = c(min(df[,c(input$corr_1, input$corr_2)]), max(df[,c(input$corr_1, input$corr_2)]))) +
      theme_bw() +
      theme(text = element_text(size=18))
  })

  observe({
    if (!is.null(df()) && is.data.frame(df()) && nrow(df()) > 0) {
      shinyjs::show('hidden_metric_choice')
      shinyjs::show('hidden_metric_choice2')
      updatePickerInput(session, inputId = 'corr_1', selected = 'mean', choices = colnames(df()))
      updatePickerInput(session, inputId = 'corr_2', selected = 'foldx', choices = colnames(df()))
    } else {
      shinyjs::hide('hidden_metric_choice')
      shinyjs::hide('hidden_metric_choice2')
    }
  })


  output$metric_plot_or_not <- renderUI({
    if (!is.null(input$mutation_table_rows_selected)) { 
      div(
        HTML('<span style="text-align:center;"><h5>Metric details</h5></span>'),
        plotOutput("metric_plot", height='40vh')  # Define the placeholder for the plot
      )
    } else if (!is.null(df())) {
      renderUI({
        HTML('<i><span style="color: grey;">Select (a) row(s) in the mutation table</span></i>')
      })
    }
  })

  
  output$metric_plot <- renderPlot({
    req(input$mutation_table_rows_selected)
    df <- df()
    mut_data <- df[input$mutation_table_rows_selected, ] %>% 
      rownames_to_column(var = 'mutation') %>% 
      pivot_longer(., -1, names_to = 'metric', values_to = 'score') %>% 
      as.data.frame()
    mut_data$metric <- factor(mut_data$metric, levels = rev(colnames(df)))
    mut_data$color <- 'black'
    mut_data[mut_data$metric == 'mean', 'color'] <- 'white'
    # browser()
    ggplot(mut_data, aes(group = mutation)) +
      coord_flip() +
      # geom_vline(xintercept = ncol(df) - 0.5) +
      geom_rect(xmin = ncol(df) +1, xmax = ncol(df) - 0.5, ymin=-20, ymax=20, color='black', fill = '#0000001c') +
      geom_col(
        aes(x = metric, y = score, fill = mutation, color = color),
        linewidth = 0.5,
        position = 'dodge'
      ) +
      scale_color_identity() +
      scale_fill_viridis_d(option = "plasma") +
      scale_y_continuous(limits = (c(min(-2,min(mut_data$score)), c(max(2, max(mut_data$score)))))) +
      scale_x_discrete(expand = c(0,0)) +
      ggtitle(paste(unique(mut_data$mutation), collapse = ', ')) +
      theme_bw() +
      theme(text = element_text(size=18))
  })
}