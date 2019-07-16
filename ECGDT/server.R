shinyServer(function(input, output){
  options(shiny.maxRequestSize=100*1024^2)
  output$textoSalida1 <- renderText({
      paste("Ha seleccionado el tipo de formato", input$tipoFichero)
  })
  output$textoSalida2 <- renderText({
    paste("Ha seleccionado la frecuencia de muestreo", input$frecuencia[1], "muestras/segundo")
  })
  
})