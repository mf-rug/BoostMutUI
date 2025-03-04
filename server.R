server <- function(session, input, output) {
  output$mutation_table <- renderDT({
    # browser()
    # req(input$boostmut_csv)
    # df <- read_csv(input$boostmut_csv$datapath, col_names = TRUE, show_col_types = FALSE) %>% as.data.frame()
    df <- read_csv('leh_example.csv', col_names = TRUE, show_col_types = FALSE) %>% as.data.frame()
    rownames(df) <- df[,1]
    df <- df[,-1]
    # colnames(df) <- 'E_Hbond'
    # browser()
    df$dtm <- as.numeric(df$dtm)
    float_cols <- sapply(df, is.numeric)
    df$total <- apply(df[,setdiff(float_cols, 'dtm')], 1, mean)
    df[, float_cols] <- round(df[, float_cols], 3)
    df <- df[, c('dtm',
                 'stabilizing',
                 'exp_tested',
                 'total',
                 setdiff(colnames(df), c(
                   'dtm', 'stabilizing', 'exp_tested', 'total'
                 )))]
    df <- df[order(df$total, decreasing = TRUE),]
    
    # Define a color function with gradient mapping
    color_fn <- scales::col_numeric(
      palette = c("darkblue", "white", "darkred"),  # Gradient colors
      domain = c(-5, 5)  # Define the range for the mapping
    )
    
    # Apply custom coloring for values outside the range
    get_color <- function(value) {
      if (is.na(value)) return('grey')
      if (value <= -5) return("darkblue")  # Anything ≤ -5 stays dark blue
      if (value >= 5) return("darkred")    # Anything ≥ 5 stays dark red
      return(color_fn(value))  # Otherwise, use the gradient
    }
    color_gradient <- function(dt, column_name, gradient_colors = c("#6666FF", "#DDDDDD", "#FF6666")) {
      col_func <- colorRampPalette(gradient_colors)
      
      # Get min & max values of the column
      col_values <- sort(unique(dt$x$data[[column_name]]), decreasing = FALSE)
      
      # Define breakpoints between min & max
      if (length(col_values) > 1) {
        breaks <- seq(min(col_values), max(col_values), length.out = length(col_values))
      } else {
        breaks <- col_values  # If only one unique value, no breaks needed
      }
      
      dt %>%
        formatStyle(
          column_name, 
          backgroundColor = styleInterval(
            breaks[-1],  # Use all breakpoints except the first
            col_func(length(breaks))  # Generate matching colors
          )
        ) 
    }
    
    
    
    datatable(
      df,
      extensions = 'FixedColumns',
      style = 'bootstrap',
      options = list(scrollX = TRUE, # Enable horizontal scrolling
                     fixedColumns = list(leftColumns = 1),  # Freeze the first column
                     pageLength = 10,
                     buttons = list('colvis')
                     
      )
    ) %>% 
      formatStyle(0, target = 'row', lineHeight='75%') %>% 
      formatStyle('stabilizing', backgroundColor = styleEqual(c(1, 0), c('#b2a919', '#996e80'))) %>% 
      formatStyle('exp_tested', backgroundColor = styleEqual(c(1, 0), c('#4c4c9d', ''))) %>% 
      formatStyle(
        'dtm',
        backgroundColor = styleEqual(df$dtm, sapply(df$dtm, get_color))  # Apply colors dynamically
      ) %>% 
      color_gradient("total")
    
  })
}