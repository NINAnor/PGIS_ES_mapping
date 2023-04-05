# call module questionnaire
return_quest_UI = function(id) {
  ns <- NS(id)
  
  tagList(
    mainPanel(textInput(ns("userID"),"enter user id"),
              numericInput(ns("age"),"What's your age?",NULL,min=12,max=110,step=1),
              selectInput(ns("gender"),"Which best describes your gender?",c("Female" ="f",
                                                                         "Male" = "m",
                                                                         "Prefer not to say"="no_answ"),multiple = F),
              selectInput(ns("edu"),"What is the highest level of education you have attained?",c("Upper secondary education" ="f",
                                                                                              "Tertiary vocational programme" = "m",
                                                                                              "Bachelor"="bsc",
                                                                                              "Master" = "msc",
                                                                                              "Doctoral degree" = "phd",
                                                                                              "Prefer not to say"="no_answ"),multiple = F),
              selectInput(ns("liv"),"How long have you lived in Trondheim area in total?",c(" I don`t  live in the Trondheim area" ="no_TRD",
                                                                                        "Less than 5 years" = "few",
                                                                                        "5 - 10 years"="more",
                                                                                        "10 - 20 years" = "more2",
                                                                                        "more than 20 years" = "all",
                                                                                        "Prefer not to say"="no_answ"),multiple = F),
              selectInput(ns("fam"),"How familiar are you with the areas in and around Trondheim?",c("poor" ="poor",
                                                                                                 "fair" = "fair",
                                                                                                 "good"="good"),multiple = F),
              
              # selectModUI(ns("map_living")),
              radioMatrixInput(ns("matInput"),
                               rowIDs = c("ES1","ES2","ES3"),
                               rowLLabels =  c("Experience: It enables to experience nature by watching it",
                                               "Physical Use: It enables to use nature by biking, hiking, walking in it",
                                               "Education: It enables to learn about and investigate the environment (education, research)"),
                               choices = c("I don`t know", "not important at all", "not very important","of medium importance","quite important","very important"),
                               choiceNames = c("0", "1", "2","3","4","5")),
              radioMatrixInput(ns("matInput2"),
                               rowIDs = c("NEP1","NEP2","NEP3"),
                               rowLLabels =  c("We are approaching the limit of the number of people the Earth can support",
                                               "Humans have the right to modify the natural environment to suit their needs.",
                                               "When humans interfere with nature it often produces disastrous consequences."),
                               choices = c("strongly agree", "agree", "unsure","disagree","strongly disagree"),
                               choiceNames = c("0", "1", "2","3","4"))
    )
  )
}


return_quest_Server = function(input, output, session) {
  # liv_pol <- callModule(selectMod, "map_living",
  #                       leaflet() %>%
  #                         addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
  #                         addFeatures(st_sf(grd), layerId = ~seq_len(length(grd)))


  # )

  # gs<-liv_pol()
  # gs<-st_sf(grd[as.numeric(gs[which(gs$selected==TRUE),"id"])])
  # cent<-st_centroid(gs)
  
  
  
  quest <- reactive({
    

    ## write user data to file

    list(
      userID=input$userID,
      age=input$age,
      gender = input$gender,
      education = input$edu,
      es1 = input$matInput$ES1[1],
      es2 = input$matInput$ES2[1],
      es3 = input$matInput$ES3[1],
      nep1 = input$matInput2$NEP1[1],
      nep2 = input$matInput2$NEP2[1],
      nep3 = input$matInput2$NEP3[1]
      # user_lng = st_coordinates(cent)[1],
      # user_lat = st_coordinates(cent)[2]
      
    )
   
  })
   return(quest)
  
}