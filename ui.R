library(shiny)
library(bslib)
library(readr)
library(DT)

ui <- fluidPage(title = 'BoostMut', page_navbar(
  title = 'BoostMut',
  nav_spacer(),
  # push nav items to the right
  nav_item(input_dark_mode(id = "dark_mode")),
  sidebarLayout(sidebarPanel(
    fileInput('boostmut_csv', 'Select BoostMut csv output', accept = 'csv'),
    width = 3
  ), mainPanel(DTOutput('mutation_table', width = '100%'), width = 9))
))