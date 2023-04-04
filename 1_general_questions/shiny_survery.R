library(shinysurveys)
df=read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/Questionnaire/shiny_output/survey_shiny_2.csv")

ui<-shiny::fluidPage(
  shinysurveys::surveyOutput(df=df,
                             survey_title = "Test Geoprospective",
                             survey_description = "minimal description")
)

server<-function(input,output,session) {
  shinysurveys::renderSurvey()
  
  observeEvent(input$submit,{
    response_data <- shinysurveys::getSurveyData()
    # write.csv(response_data,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/Questionnaire/shiny_output/response_data.csv",
    #           row.names = T)
    
  })
}

shiny::shinyApp(ui = ui, server = server)