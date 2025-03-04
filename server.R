color_gradient <- function(dt, column_names, gradient_colors = c("#6666FF", "white", "#FF6666")) {
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
      path <- "leh_example.csv"  # Load example file
    }
    
    df <- read_csv(path, col_names = TRUE, show_col_types = FALSE, name_repair = "unique_quiet") %>% as.data.frame()
    
    rownames(df) <- df[,1]
    df <- df[,-1]
    float_cols <- sapply(df, is.numeric)
    df$mean <- apply(df[,float_cols], 1, mean)
    df[, float_cols] <- round(df[, float_cols], 3)
    
    df <- df[order(df$mean, decreasing = TRUE), c('mean', setdiff(colnames(df), 'mean'))]
  })

  
  output$mutation_table <- renderDT({
    df <- df() %>% rownames_to_column(var = 'mutation')
    if (input$color_cols == 'All') {
      target_cols <- setdiff(colnames(df), 'mutation')
    } else {
      target_cols <- 'mean'
    }
    
    # Extract column names
    cols <- colnames(df)
    
    # Identify unique feature names by removing "_r" and "_s"
    feature_names <- unique(sub("_[rs]$", "", cols[grepl("_[rs]$", cols)]))
    
    # Identify standalone columns (those that don’t end in _r or _s)
    standalone_cols <- setdiff(cols, c(paste0(feature_names, "_r"), paste0(feature_names, "_s")))
    
    # Create top header row dynamically
    top_header <- c(
      lapply(standalone_cols, function(name) tags$th(rowspan = 2, name)),  # Standalone columns
      lapply(feature_names, function(name) tags$th(colspan = 2, name))  # Grouped features
    )
    
    # Create subheader row for "residue" and "surrounding"
    sub_header <- c(
      rep("", length(standalone_cols)),  # Empty placeholders for standalone cols
      rep(list(tags$th("residue"), tags$th("surrounding")), length(feature_names))  # Pairs for grouped cols
    )
    
    # Generate `sketch` dynamically
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(top_header),
        tr(sub_header)
      )
    ))
    
    
    datatable(
      df, container = sketch,
      extensions = 'FixedColumns',
      rownames = FALSE,
      style = 'bootstrap',
      options = list(scrollX = TRUE, # Enable horizontal scrolling
                     fixedColumns = list(leftColumns = 1),  # Freeze the first column
                     paging = FALSE,
                     searching = FALSE,
                     info = FALSE,
                     scrollY = '35vh', 
                     fixedHeader = TRUE
      )) %>% 
      formatStyle(0, target = 'row', lineHeight='60%') %>% 
      color_gradient(target_cols, gradient_colors =  c(
        "#6666FF",
        ifelse(input$dark_mode == 'dark', "black", 'white'),
        "#FF6666"
      ))
      
  })
  
  output$corr_plot_or_not <- renderPlot({
    req(df(), input$corr_1)
    
    df <- df()

    dark <- input$dark_mode == 'dark'
    plot_col <- ifelse(dark, 'white', 'black')
    
    # Perform Deming regression
    deming_model <- mcreg(df[,input$corr_1], df[,input$corr_2], method.reg = "Deming")
    
    # Extract coefficients
    slope <- coef(deming_model)[2]
    intercept <- coef(deming_model)[1]

    p1 <- ggplot(df, aes(x = .data[[input$corr_1]], y=.data[[input$corr_2]])) + 
      geom_point(color = plot_col) +
      geom_abline(color = plot_col) +
      geom_abline(slope = slope, intercept = intercept, color = "blue") +
      # geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE)  +
    
      scale_x_continuous(limits = c(min(df[, c(input$corr_1, input$corr_2)]), 
                                    max(df[, c(input$corr_1, input$corr_2)]))) +
      scale_y_continuous(limits = c(min(df[, c(input$corr_1, input$corr_2)]), 
                                    max(df[, c(input$corr_1, input$corr_2)]))) +
      theme_bw() +
      theme(text = element_text(size=18))
    if (dark) {
      p1 <- p1 + theme(text = element_text(color = 'white'),
                       axis.text = element_text(color = 'white'),
                       panel.background = element_rect(fill = 'black'),
                       plot.background = element_rect(fill = '#1d1f21', size = 0),
                       legend.background = element_rect(fill='#1d1f21'),
                       legend.box.background = element_rect(color='#1d1f21'),
                       panel.grid.major = element_line(color='#94949480'),
                       panel.grid.minor = element_line(color='#94949480'),
                       theme(plot.margin=unit(c(-0.30,0,0,0), "null")))
    }
    p1
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
        plotOutput("metric_plot", height='460px')  # Define the placeholder for the plot
      )
    } else if (!is.null(df())) {
      renderUI({
        HTML('<i><span style="color: grey;">Select (a) row(s) in the mutation table</span></i>')
      })
    }
  })

  
  output$metric_plot <- renderPlot({
    req(input$mutation_table_rows_selected)
    dark <- input$dark_mode == 'dark'
    df <- df()
    mut_data <- df[input$mutation_table_rows_selected, ] %>% 
      rownames_to_column(var = 'mutation') %>% 
      pivot_longer(., -1, names_to = 'metric', values_to = 'score') %>% 
      as.data.frame()
    mut_data$metric <- factor(mut_data$metric, levels = rev(colnames(df)))
    mut_data$color <- 'black'
    mut_data[mut_data$metric == 'mean', 'color'] <- 'white'
    # browser()
    p1 <- ggplot(mut_data, aes(group = mutation)) +
      coord_flip() +
      # geom_vline(xintercept = ncol(df) - 0.5) +
      geom_rect(xmin = ncol(df) +1, xmax = ncol(df) - 0.5, ymin=-20, ymax=20, color=ifelse(dark, 'white', 'black'), fill = ifelse(dark, 'grey91', '#0000001c')) +
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
      theme(text = element_text(size=18),
            )
    if (dark) {
      p1 <- p1 + theme(text = element_text(color = 'white'),
                       axis.text = element_text(color = 'white'),
                       panel.background = element_rect(fill = 'black'),
                       plot.background = element_rect(fill = '#1d1f21', size = 0),
                       legend.background = element_rect(fill='#1d1f21'),
                       legend.box.background = element_rect(color='#1d1f21'),
                       panel.grid.major = element_line(color='#94949480'),
                       panel.grid.minor = element_line(color='#94949480'))
    }
    p1
  })
}