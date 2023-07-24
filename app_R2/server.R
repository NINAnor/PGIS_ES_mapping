function(input, output, session) { 

  ## start and hide map tab
  hideTab(inputId = "tabset", target = "p1")
  hideTab(inputId = "tabset", target = "p2")
  hideTab(inputId = "tabset", target = "p3")
  hideTab(inputId = "tabset", target = "p4")

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
    return(userID_sel)
  })
 
  userES<-eventReactive(input$sub0,{
    userID_sel<-userID_sel()
    userES <- tbl(con, "es_mappingR1")
    #dplyr sql
    userES <- select(userES, userID, esID, mapping, siteID) %>% filter(siteID == siteID_sel, userID == userID_sel)%>%
      collect()

  })
  
  #mod es 1
  observeEvent(input$sub0, {
    if(input$email %in% user_conf$userMAIL){
      #update shiny content
      output$login_res<-renderText("Mail adress found")
      updateTabsetPanel(session, "tabset",
                        selected = "p1")
      hideTab(inputId = "tabset", target = "p0")
      showTab(inputId = "tabset", target = "p1")
      userES<-userES()
      userID_sel<-userID_sel()
      remapServer("remap1", userID_sel, blog_data, es_descr, userES, siteID_sel, geometry, sf_bound, vis_qc,1)

    }else{
      output$login_res<-renderText("Mail adress NOT found")
      
    }
    })
  
  #mod es 2
  observeEvent(input$sub1, {

      #update shiny content
      updateTabsetPanel(session, "tabset",
                        selected = "p2")
      hideTab(inputId = "tabset", target = "p1")
      showTab(inputId = "tabset", target = "p2")
      userES<-userES()
      userID_sel<-userID_sel()
      remapServer("remap2", userID_sel, blog_data, es_descr, userES, siteID_sel, geometry, sf_bound, vis_qc,2)


  })
  
  #mod es 3
  observeEvent(input$sub2, {
    
    #update shiny content
    updateTabsetPanel(session, "tabset",
                      selected = "p3")
    hideTab(inputId = "tabset", target = "p2")
    showTab(inputId = "tabset", target = "p3")
    userES<-userES()
    userID_sel<-userID_sel()
    remapServer("remap3", userID_sel, blog_data, es_descr, userES, siteID_sel, geometry, sf_bound, vis_qc,3)
  })
  
  #mod es 4
  observeEvent(input$sub3, {
    
    #update shiny content
    updateTabsetPanel(session, "tabset",
                      selected = "p4")
    hideTab(inputId = "tabset", target = "p3")
    showTab(inputId = "tabset", target = "p4")
    userES<-userES()
    userID_sel<-userID_sel()
    remapServer("remap4", userID_sel, blog_data, es_descr, userES, siteID_sel, geometry, sf_bound, vis_qc,3)
    
  })
    
  #stop app
  observeEvent(input$sub4, {
      stopApp(returnValue = invisible())
  })

  
  }