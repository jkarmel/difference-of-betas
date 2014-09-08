
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Chance of A better than B"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput('visitors.A', label = 'Visitors A', 100),
      textInput('visitors.B', label = 'Visitors B', 100),
      textInput('conversions.A', label = 'Conversions A', 50),
      textInput('conversions.B', label = 'Conversions B', 50)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot")
      h3('chance a is bigger than b'),
      textOutput('proportion.A.bigger.than.B'),
      dataTableOutput('table'),
      plotOutput('A.less.B.plot', height = 600)
    )
  )
))