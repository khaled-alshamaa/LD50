library(shiny)
library(ggplot2)
library(MASS)
library(xlsx)

source(file.path('server', 'ld50.R'), local=TRUE)$value

# Header ------------------------------------------------------------

header <- headerPanel('ICARDA BioComputing Online (Phase II): Lethal Dose/Concentration Calculator')

# Sidebar -----------------------------------------------------------

sidebar <- sidebarPanel(
  # Style and Logo
  tags$head(
    tags$style(type="text/css", ".col-sm-4 { max-width: 360px; } "),
    tags$style(type="text/css", ".well { max-width: 350px; }")
  ),
  
  tags$img(src='icarda.png', height=90, width=190),
  
  # Upload Data File
  fileInput('datafile', 'Upload Your Data:', accept=c('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xls', '.xlsx')),
  
  # Static Inputs
  tags$div(title='p hint', sliderInput('p', 'Effective (or Lethal) Dose/Concentration:', min=0, max=100, value=50)),
  tags$div(title='conf hint', sliderInput('conf', 'Level of Confidence Interval:', min=0, max=100, value=80)),
  tags$div(title='Function of the response proportion to link to linear function of dose', selectInput('link', 'Link Function:', c('Logit'='logit', 'Probit'='probit', 'Complementary log-log'='cloglog'))),
  tags$div(title='To transform the Explanatory to a linear scale', selectInput('transform', 'Take logs of Explanatory:', c('None'='none', 'Base 10'='log', 'Base e'='ln'))),
  
  # Dynamic Inputs
  tags$div(title='e.g. Total subjects tested', uiOutput('subjectsCol')),
  tags$div(title='e.g. Dead subjects observed', uiOutput('respondingCol')),
  tags$div(title='e.g. Dose', uiOutput('doseCol')),
  tags$div(title='e.g. Treatment', uiOutput('groupsCol')),
  tags$small(tags$em('Tip: click Backspace to unselect groups.')),
  
  # Bookmark Status
  tags$hr(), 
  bookmarkButton()
)

# Body ---------------------------------------------------------------

body <- mainPanel(
          tabsetPanel(
            source(file.path('ui', 'tab.input.R'), local=TRUE)$value,
            source(file.path('ui', 'tab.analysis.R'), local=TRUE)$value,
            source(file.path('ui', 'tab.graphics.R'), local=TRUE)$value,
            source(file.path('ui', 'tab.help.R'), local=TRUE)$value
          )
)

# Setup Shiny app UI components -------------------------------------------

ui <- shinyUI(pageWithSidebar(header, sidebar, body))

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output, session) { 
  # Need to exclude the dynamic uiOutput inputs
  setBookmarkExclude(c('datafile', 'subjectsCol', 'respondingCol', 'doseCol', 'groupsCol'))
  
  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(stopApp)
  
  source(file.path('server', 'run.input.R'), local=TRUE)$value
  source(file.path('server', 'run.analysis.R'), local=TRUE)$value
  source(file.path('server', 'run.graphics.R'), local=TRUE)$value
  source(file.path('server', 'run.help.R'), local=TRUE)$value
  
  onRestore(function(state) {
    query <- parseQueryString(session$clientData$url_search)
    
    updateSliderInput(session, 'p', value=query[['p']])
    updateSliderInput(session, 'conf', value=query[['conf']])
    updateSelectInput(session, 'link', selected=gsub('"', '', query[['link']]))
    updateSelectInput(session, 'transform', selected=gsub('"', '', query[['transform']]))
  })
}
              
# Render Shiny app --------------------------------------------------------

enableBookmarking('url')
shinyApp(ui, server)