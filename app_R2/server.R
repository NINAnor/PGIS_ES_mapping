function(input, output) { 
  observeEvent(input$sub0, {
    if(input$email %in% user_conf$email){
      output$recOpt <- renderMenu({
        menuItem("Results Round 1",tabName = "res", icon = icon("dashboard"),
                 menuSubItem("dashboard", tabName = "dash", icon = icon("gamepad")),
                 menuSubItem("ecosystem services", tabName = "es", icon = icon("gamepad")))

      })
      
      
    }else{
      output$recOpt <- renderMenu({
        menuItem("no results for you",tabName = "no_res", icon = icon("cross"))
        
        
      })
      
    }
    
  })
  userID<-eventReactive(input$sub0, {
    if(input$email %in% user_conf$email){
      userID<-as.character(user_conf%>%filter(email == input$email)%>%select(UID))

      
    }else{
      userID = "uKuI7a21"
    }
    return(userID)
  })
  
  userES<-eventReactive(input$sub0,{
    userID2<-userID()
    userES<-es_user_data%>%filter(userID == userID2)%>%distinct(es_id)
    userES<-as.vector(userES)
    return(userES)
  })
  
  
  observeEvent(input$sub0, {
    ## radarplot NEP with personal line
    
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) 
    fig <- fig %>%
      add_trace(
        r = c(39, 28, 8, 7, 28, 39),
        theta = c('A','B','C', 'D', 'E', 'A'),
        name = 'individual profile'
      ) 
    fig <- fig %>%
      add_trace(
        r = c(1.5, 10, 39, 31, 15, 1.5),
        theta = c('A','B','C', 'D', 'E', 'A'),
        name = 'mean participants'
      ) 
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,50)
          )
        )
      )
    output$radar <- renderPlotly(fig)
    
    age<-plot_ly(quest, x = ~age, type = 'histogram')
    output$age<-renderPlotly(age)
    
    output$living<-renderLeaflet({leaflet() %>%
                                   addProviderTiles(providers$Stamen.TonerLite,
                                                    options = providerTileOptions(noWrap = TRUE)
                                   ) %>%
                                   addMarkers(data = user_pts)})
    
  })
  
  observeEvent(input$sub1,{
    userID<-userID()
    
    imgpath<-paste0(ee_get_assethome(), '/rgee/individual_R1_',input$es_individual,"/", userID)
    
  })
  
  blog_data<-eventReactive(input$sub1, {
    blog_data<-es_user_data%>%filter(es_id == input$es_individual)%>%select(userID,blog)
    #only keep non empty blog entries
    blog_data<-blog_data%>%filter(blog !="")
    blog_data$username<-rep("USER_",nrow(blog_data))
  })
  

  }