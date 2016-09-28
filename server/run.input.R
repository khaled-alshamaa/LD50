# Import Data
importedData <- reactive({  
  inFile <- input$datafile
  
  if (is.null(inFile)) return(NULL)
  
  read.xlsx(inFile$datapath, sheetIndex=1, header=TRUE)
})

output$contents <- renderDataTable({
  importedData()
})

# Read the Data
modelInputs <- reactive({
  validate(
    need(input$subjectsCol != '', 'Please select numbers of subjects column'),
    need(input$respondingCol != '', 'Please select numbers of responding column'),
    need(input$doseCol != '', 'Please select explanatory variate column')
  )
  
  validate(
    need(input$transform == 'none' | all(importedData()[,input$doseCol] > 0), 'You can not take logs for 0 or negative explanatory values!')
  )
  
  subjects   <- importedData()[,input$subjectsCol]
  responding <- importedData()[,input$respondingCol]
  
  if(input$transform == 'none') { 
    dose <- importedData()[,input$doseCol] 
  } else if(input$transform == 'log') {
    dose <- log10(importedData()[,input$doseCol])
  } else if(input$transform == 'ln') {
    dose <- log(importedData()[,input$doseCol])
  }
  
  if(input$groupsCol != '') {
    groups <- as.factor(importedData()[,input$groupsCol])
  } else {
    groups <- as.factor(rep.int('All', length(dose)))
  }
  
  data.frame(subjects, responding, dose, groups)
})

# Build user interface
output$subjectsCol <- renderUI({
  selectInput('subjectsCol', 'Numbers of Subjects (e.g. Total):*', as.list(c('', colnames(importedData()))))
})

output$respondingCol <- renderUI({
  selectInput('respondingCol', 'Numbers Responding (e.g. Dead):*', as.list(c('', colnames(importedData()))))
})

output$doseCol <- renderUI({
  selectInput('doseCol', 'Explanatory Variate (e.g. Dose):*', as.list(c('', colnames(importedData()))))
})

output$groupsCol <- renderUI({
  selectInput('groupsCol', 'Groups (e.g. Treatment):', c('None'='', as.list(colnames(importedData()))))
})
