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
  
#blog
  observeEvent(input$es_individual, {
    
    blog_data<-es_user_data%>%filter(es_id == input$es_individual)%>%select(blog)
    #only keep non empty blog entries
    blog_data<-blog_data%>%filter(blog !="")
    blog_data$username<-rep("USER_",nrow(blog_data))
    blog_data$username<-paste0(blog_data$username,as.numeric(rownames(blog_data)), ": ",blog_data$blog)
    blog_data<-blog_data%>%select(username)
    output$blog<-renderDT(blog_data,rownames= FALSE, colnames="Blog entries")

  })
# map  
  map<-eventReactive(input$es_individual,{
    userID<-userID()
    imgpath<-paste0(ee_get_assethome(), '/rgee/individual_R1_',input$es_individual,"/", userID)
    colpath<-paste0(ee_get_assethome(), '/rgee/individual_R1_',input$es_individual)
    img_ind<-ee$Image(imgpath)
    
    mean_col<-ee$ImageCollection(colpath)
    mean_col <- mean_col$reduce(ee$Reducer$mean())
    diff <- img_ind$subtract(mean_col)
    #ind_poly<-sf::st_read(paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/output/train_polys_R1/",userID,"_",input$es_individual,".shp"))
    
    # gee_poly<-rgee::sf_as_ee(ind_poly, via = "getInfo")
    
    Map$setCenter(10.38649, 63.40271,10)
    m1<-Map$addLayer(
      eeObject = img_ind,
      vis_qc,
      opacity = 0.4
    ) +Map$addLegend(vis_qc,name = "your predicted ES", color_mapping = "character")
    m2<-Map$addLayer(
      eeObject = mean_col,
      vis_qc,
      opacity = 0.4
    ) +Map$addLayer(
      eeObject = diff,
      vis_diff,
      opacity = 0.4
    ) +Map$addLegend(vis_diff,name = "diff", color_mapping = "character")
    m1  | m2
  },
  ignoreNULL = FALSE
  )
# render map  
  output$es_maps <- renderLeaflet({
    map()
  })

# importance
  bar_plot<-eventReactive(input$es_individual,{
    userID2 <- userID()
    es_sel<-es_user_data%>%filter(es_id == input$es_individual)
    ind_imp<-es_user_data%>%filter(es_id == input$es_individual & userID == userID2)%>%select(imp_own,imp_other)
    
    bar_plot<-plot_ly(es_sel, y =~mean(imp_own), type = "bar", name = "mean own importance for people",color = "red")%>%
      add_bars(es_sel, y =~mean(imp_other), type = "bar", name = "mean imp society",color = "blue")%>%
      add_segments(x = -0.5, xend = 0, y = ind_imp$imp_own, yend = ind_imp$imp_own, color="black",name = "your value") %>%
      add_segments(x = 0, xend = 0.5, y = ind_imp$imp_other, yend = ind_imp$imp_other, color="black",name = "your value")

  })
  
  output$importance<-renderPlotly(bar_plot())

  

  }