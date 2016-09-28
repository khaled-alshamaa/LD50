graphHeight <- reactive({
  600*length(levels(results()$Treat))
})

respondingPlot <- reactive({
  data <- merge(modelInputs(), results(), by.x="groups", by.y="Treat", all.x=TRUE)
  g <- ggplot(data, aes(x=dose, y=responding/subjects))
  g <- g + facet_wrap(~groups, ncol=1)
  g <- g + geom_point(position=position_jitter(width=0, height=0.05))
  g <- g + geom_vline(aes(xintercept=Dose), col='red')
  g <- g + geom_text(aes(x=Dose, label=paste0('LD %', input$p)), y=0.1, angle=90, vjust=1.5, col='red')
  g <- g + xlab(input$doseCol) + ylab(paste(input$respondingCol, '%'))
  g <- g + stat_smooth(method='glm', method.args=list(family=binomial(link=input$link)), level=input$conf/100, fullrange=TRUE, size=1)
})

output$ldGraph <- renderPlot({
  print(respondingPlot())
}, height=graphHeight)


output$downloadGraph <- downloadHandler(
  filename = function() { paste0(input$datafile$name, '.png') },
  content = function(file) {
    # 6 inches wide, and manage have golden ratio for graphs! 
    h <- 6 * length(levels(results()$Treat)) / 1.618
    png(file, res=600, height=h, width=6, units="in")
    print(respondingPlot())
    dev.off()
  },
  contentType = 'image/png'
)  
