function(input, output, session) { 

  ## start and hide map tab
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  # hideTab(inputId = "tabset", target = "p4")

  #show some plots
  output$age<-renderPlotly(
    plot_ly(user_all, x = ~age, type = 'histogram'))

  output$edu<-renderPlotly(
    plot_ly(user_all, x = ~edu, type = 'histogram'))

  output$fam<-renderPlotly(
    plot_ly(user_all, x = ~fam, type = 'histogram'))


  output$gen<-renderPlotly(
    plot_ly(user_all, x = ~gen, type = 'histogram'))
  
  output$map_liv<-renderLeaflet({leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = tileOptions(minZoom = 10, maxZoom = 14))%>%
      addMarkers(data = user_pts)})

  userID_sel<-eventReactive(input$sub0, {
    if(input$email %in% user_conf$userMAIL){
      userID_sel<-as.character(user_conf%>%filter(userMAIL == input$email)%>%select(userID))

    }else{
      userID_sel = "KcdePm2lep"
    }

  })
  # userES<-eventReactive(input$sub0,{
  #   userID_sel<-userID_sel()
  #   userES <- tbl(con, "es_mappingR1")
  #   userES <- select(userES, userID, esID, mapping, siteID, blog) %>% filter(siteID == studyID, userID == userID_sel)%>%
  #     collect()
  # })

  # userID_sel<-"vnt2jZiP8U"
  #mod es 1
  observeEvent(input$sub0, {
    if(input$email %in% user_conf$userMAIL){
      #update shiny content
      updateTabsetPanel(session, "inTabset",
                        selected = "p1")
      hideTab(inputId = "inTabset", target = "p0")
      showTab(inputId = "inTabset", target = "p1")
      userID_sel<-userID_sel()
      userES<-userES%>%filter(userID == userID_sel)
      userES_sel<-userES[1,]
      remapServer("remap1", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,1)

    }else{
      output$login_res<-renderText("Mail adress NOT found")
      
    }
    })
  # u<-eventReactive(input$sub0,{
    
    # userID_sel<-userID_sel()
    # userES<-userES()
    # userES_sel<-userES[1,]
    # u<-remapServer("remap1", isolate(userID_sel()), es_descr, userES, studyID, geometry, sf_bound, vis_qc,1)
  # })
  
  #mod es 2
  observeEvent(input$sub1, {

      #update shiny content
      updateTabsetPanel(session, "inTabset",
                        selected = "p2")
      hideTab(inputId = "inTabset", target = "p1")
      showTab(inputId = "inTabset", target = "p2")
      userID_sel<-userID_sel()
      userES<-userES%>%filter(userID == userID_sel)
      userES_sel<-userES[2,]
      remapServer("remap2", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,2)

  })
  # v<-eventReactive(u(),{
  #   
  #   userID_sel<-userID_sel()
  #   userES<-userES()
  #   userES_sel<-userES[2,]
  #   remapServer("remap2", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,2)
  # })

  
  #mod es 3
  observeEvent(input$sub2, {
    
    #update shiny content
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
    hideTab(inputId = "inTabset", target = "p2")
    showTab(inputId = "inTabset", target = "p3")
      userID_sel<-userID_sel()
      userES<-userES%>%filter(userID == userID_sel)
      userES_sel<-userES[3,]
      remapServer("remap3", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,3)

  })
  # w<-eventReactive(v(),{
  #   
  #   userID_sel<-userID_sel()
  #   userES<-userES()
  #   userES_sel<-userES[3,]
  #   remapServer("remap3", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,3)
  # })

  #stop app
  observeEvent(input$sub2, {
      stopApp(returnValue = invisible())
  })

  
  }