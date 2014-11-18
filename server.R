# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(scales)

shinyServer(function(input, output) {
  diff.of.betas = function(n.A, n.B, successes.A, successes.B, num.simulations, prior.params.A = c(1,1), prior.params.B = c(1,1)) {
    failures.A = n.A - successes.A
    failures.B = n.B - successes.B
    prior.params.A = c(1,1)
    prior.params.B = c(1,1)
    beta.A = rbeta(num.simulations, successes.A + prior.params.A[1], failures.A + prior.params.A[2])
    beta.B = rbeta(num.simulations, successes.B + prior.params.B[1], failures.B + prior.params.B[2])
    sort(beta.A - beta.B)
  }
  
  rel.diff.of.betas = function(n.A, n.B, successes.A, successes.B, num.simulations, prior.params.A = c(1,1), prior.params.B = c(1,1)) {
    failures.A = n.A - successes.A
    failures.B = n.B - successes.B
    prior.params.A = c(1,1)
    prior.params.B = c(1,1)
    beta.A = rbeta(num.simulations, successes.A + prior.params.A[1], failures.A + prior.params.A[2])
    beta.B = rbeta(num.simulations, successes.B + prior.params.B[1], failures.B + prior.params.B[2])
    sort((beta.A - beta.B)/beta.A)
  }
  
  samples = reactive({strtoi(input$samples)})
  difference.of.betas = reactive({
    successes.A = strtoi(input$conversions.A)
    successes.B = strtoi(input$conversions.B)
    n.A = strtoi(input$visitors.A)
    n.B = strtoi(input$visitors.B)
    diff.of.betas(n.A, n.B, successes.A, successes.B, samples())
  })
  
  output$proportion.A.bigger.than.B = renderText({
    times.A.bigger.than.B = sum(difference.of.betas() > 0)
    percent(times.A.bigger.than.B/samples())
  })
  
  percentiles = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)  
  chance.at.point = reactive({  difference.of.betas()[percentiles * samples()]  })
  improvement = reactive({ 
    proportion.B =  strtoi(input$conversions.B) /  strtoi(input$visitors.B)
    improvement =  chance.at.point()/ proportion.B
  })
  output$table = renderDataTable({
    frame = data.frame(percent(percentiles), percent(chance.at.point()), percent(improvement()))
    colnames(frame) = c('Percentile', 'Expected Gain/Loss', 'Expected Percentage Gain/Loss')
    frame
  })
  
  output$A.less.B.hist = renderPlot({
    if (input$showPlot == TRUE && samples() < 10^6) {
      hist(difference.of.betas())
    } else {
      hist(0:5)
    }
  })
  output$A.less.B.plot = renderPlot({
    if (input$showPlot == TRUE && samples() < 10^6) {
      par(mar=c(10,2,0,0))
      plot(difference.of.betas(), seq(1, samples()), xlab='', yaxt = 'n', ylab = '', axes = FALSE)
      numYAxisLines = 8
      lapply(seq(1:numYAxisLines), function(i){abline(h=i * samples()/numYAxisLines)})
      axis(2,at = seq(0,samples(), length=5), labels = percent(seq(0, 1, length=5)))
      print(length(chance.at.point()))
      axis(
        1,
        at= chance.at.point(),
        labels = percent(percentiles),
        col='blue'
      )
      mtext("Percentile",1,line=1,adj=0,col="blue")
      axis(
        1,
        at= chance.at.point(),
        labels = percent(chance.at.point()),
        line=2,
        col='red'
      )
      mtext("Difference",1,line=2,adj=0,col="red")
      axis(
        1,
        at= chance.at.point(),
        labels = percent(improvement()),
        line=4,
        col='green'
      )
      mtext("Improvement",1,line=3,adj=0,col="green")

    } else {
      plot(seq(1:5))
    }
    
  })

})
