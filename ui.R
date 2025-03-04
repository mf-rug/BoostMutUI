library(shiny)
library(bslib)
library(readr)
library(tidyr)
library(tibble)
library(ggplot2)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(mcr)
library(htmltools)

ui <- fluidPage(title = 'BoostMut', page_navbar(
  title = 'BoostMut',
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        th {
          text-align: center !important;
          border-right: solid 5px transparent;
          border-bottom: solid 1px;
        }
        /* Target a specific column (e.g., third column, index starts from 1) */
        table.dataTable tbody tr.even td:nth-child(2) {
          box-shadow: inset 0 0 0 9999px rgba(0, 0, 0, 0) !important;
        }
        .table.dataTable.dataTable tbody tr.active td {
        color: inherit;
        background-color: transparent;
        box-shadow: #00aee3 0px 0px 8px 1px inset !important;
        }
       /* Default button styling */
      .btn-default {
          font-size: 11pt;
          padding-right: 11px;
          padding-left: 11px;
      }
    .dtfc-fixed-left {
        background-color: var(--bs-table-bg) !important; /* Ensures it matches the main table */
        color: inherit !important;
      }
      "))
  )),
  nav_spacer(),
  # push nav items to the right
  nav_item(input_dark_mode(id = "dark_mode")),

  nav_panel(NULL, 
  sidebarLayout(
    sidebarPanel(
      fileInput('boostmut_csv', 'Select BoostMut .csv output', accept = 'csv'),
      actionButton('example', 'Load example'),
      br(),br(),
      radioGroupButtons('color_cols',
                        'Columns to highlight',
                        choices = c('All', 'Mean')),
      width = 2
  ), 
  mainPanel(
    DTOutput('mutation_table', width = '100%'), 
    shinyjs::hidden(div(id = 'hidden_metric_choice',hr())),
    fluidRow(
      column(5, 
             shinyjs::hidden(div(id = 'hidden_metric_choice2',
                                 HTML('<span style="text-align:center;"><h5>Correlation analysis</h5></span>'),
                                 fluidRow(
                                   column(6, pickerInput('corr_1', 'Metric 1', width = 'fit', choices = NULL), align = 'right'),
                                   column(6, pickerInput('corr_2', 'Metric 2', width = 'fit', choices = NULL), align = 'left')
             ))),
             div(style = "background-color: #1d1f21; padding: 0px; margin: 0px;",
             plotOutput('corr_plot_or_not', height = '400px'))),
      column(7, uiOutput('metric_plot_or_not'))
    ),
    width = 10))
  )
))