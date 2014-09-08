
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(scales)

shinyServer(function(input, output) {

  samples = 10^6
  difference.of.betas = reactive({
    successes.A = strtoi(input$conversions.A)
    successes.B = strtoi(input$conversions.B)
    n.A = strtoi(input$visitors.A)
    n.B = strtoi(input$visitors.B)
    failures.A = n.A - successes.A
    failures.B = n.B - successes.B
    prior.params.A = c(1,1)
    prior.params.B = c(1,1)
    beta.A = rbeta(samples, successes.A + prior.params.A[1], failures.A + prior.params.A[2])
    beta.B = rbeta(samples, successes.B + prior.params.B[1], failures.B + prior.params.B[2])
    sort(beta.A - beta.B)
  })
  
  output$proportion.A.bigger.than.B = renderText({
    times.A.bigger.than.B = sum(difference.of.betas() > 0)
    percent(times.A.bigger.than.B/samples)
  })
  
  output$table = renderDataTable({
    percentiles = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
    chance.at.point = difference.of.betas()[percentiles * samples]
    
    proportion.B =  strtoi(input$conversions.B) /  strtoi(input$visitors.B)
    improvement =  chance.at.point/ proportion.B
    
    frame = data.frame(percent(percentiles), percent(chance.at.point), percent(improvement))
    colnames(frame) = c('Percentile', 'Expected Gain/Loss', 'Expected Percentage Gain/Loss')
    frame
  })
  
  output$A.less.B.plot = renderPlot({
    plot(difference.of.betas(), seq(1, samples), xlab='n', yaxt = 'n', ylab = '')
    axis(2,at = seq(0,samples, length=5), labels = percent(seq(0, 1, length=5)))
  })

})
