output$citation <- renderText({
  url <- paste0(session$clientData$url_protocol, '//', session$clientData$url_hostname)
  if(session$clientData$url_port != '') url <- paste0(url, ':', session$clientData$url_port)
  url <- paste0(url, session$clientData$url_pathname)
  
  citeDate <- format(Sys.Date(), format='%Y, %B %d')
  paste0('Biometrics and Statistics Section, ICARDA. (', citeDate, '). LD50 (Version 1.0) [Web application]. Retrieved from ', url)
})
