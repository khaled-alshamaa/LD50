results <- reactive({
  ldList <- NULL
  
  for(i in levels(modelInputs()$groups)){
    subjects   <- modelInputs()$subjects[modelInputs()$groups == i]
    responding <- modelInputs()$responding[modelInputs()$groups == i]
    dose       <- modelInputs()$dose[modelInputs()$groups == i]
    
    val <- ld(subjects, responding, dose, input$p/100, input$link)
    ldList <- rbind(ldList, data.frame(i, input$p, val$x, val$se, val$a, val$ase, val$b, val$bse))
  }
  
  names(ldList) <- c('Treat', 'LD/LC', 'Dose', 'SE', 'Intercept (a)', 'a SE', 'Coefficient (b)', 'b SE')
  row.names(ldList) <- NULL
  ldList
})

output$ldList <- renderTable({
  results()
}, digits=c(0, 0, 0, 2, 3, 3, 4, 3, 4))

output$downloadResults <- downloadHandler(
  filename = function() { paste0(input$datafile$name, '.csv') },
  content = function(file) {
    write.csv(results(), file)
  }
)

output$summary <- renderPrint({
  summary(data.frame(Dose=modelInputs()$dose, Responding=modelInputs()$responding, Subjects=modelInputs()$subjects, Groups=factor(modelInputs()$groups)))
})

output$modelParam <- renderText({
  l.label <- list('logit'='Logit', 'probit'='Probit', 'cloglog'='Complementary log-log')
  t.label <- list('none'='None', 'log'='Logarithmic Transformation (Base 10)', 'ln'='Natural Logarithm (Base e)')
  print(paste('<u>Link Function:</u>', l.label[input$link], '<br/>\n', '<u>Explanatory Transformation:</u>', input$transform))
})