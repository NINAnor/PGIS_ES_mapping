function(input, output, session) {
  
  #track_usage(storage_mode = store_rds(path = "logs/"))
  
  ## hiding all tabs but not start
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  hideTab(inputId = "inTabset", target = "p5")
  
  
  ####### generate user ID
  userID<-eventReactive(input$sub0, {
    # create random large string
    UID_part<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
    if(is.null(input$email)){
      email<-"not_provided"
    } else {
      email = input$email}
    
    user_conf<-data.frame(email = email,UID = UID_part)
    ### save user conf
    user_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
    ###here it might be wise to double check if the UID is unique
    ####
    
    user_all<-rbind(user_all,user_conf)
    saveRDS(user_all,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
    rm(user_conf,user_all)
    userID<-as.character(UID_part)
    return(userID)
  })
  
  
  ## submit mail, switch and remove tab one
  observeEvent(input$sub0, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p1")
  })
  observeEvent(input$sub0, {
    hideTab(inputId = "inTabset", target = "p0")
  })
  observeEvent(input$sub0, {
    showTab(inputId = "inTabset", target = "p1")
   
  })
  

  liv_pol <- callModule(module=selectMod, 
                        leafmap=map_liv,
                        id="map_living")
  
  
  ## before switching to expl, we should validate if all values are filled out
  output$cond_b1<-renderUI({
    validate(
      need(input$age, 'Provide your age'),
      need(input$gender != '', 'Select a gender'),
      need(input$edu != '', 'Select an education'),
      need(input$work != '', 'Select a working industry'),
      need(input$liv != '', 'provide the years you live in the study area')
    )
    actionButton('sub1', 'submit answers')
  })
  
  
  output$cond_b2<-renderUI({
    validate(
      need(input$check, 'Please confirm')

    )
    actionButton('sub3', 'go to task')
  })
  
  
  ## submit questionnaire and switch to expl
  observeEvent(input$sub1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
  })
  observeEvent(input$sub1, {
    hideTab(inputId = "inTabset", target = "p1")
    
    # userID<-userID()
    # gs<-liv_pol()
    # gs<-st_sf(plz[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    # cent<-st_centroid(gs)
    # user_lat <- st_coordinates(cent)[2]
    # user_lng <- st_coordinates(cent)[1]
    
    
  })
  observeEvent(input$sub1, {
    showTab(inputId = "inTabset", target = "p2")
    userID<-userID()
    liv_pol<-liv_pol()
    
    quest_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
    liv_pol<-st_sf(plz[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
    cent<-st_centroid(liv_pol)
    user_lat <- st_coordinates(cent)[2]
    user_lng <- st_coordinates(cent)[1]
    print(user_lng)
    quest <-  data.frame(
      userID=userID,
      age=input$age,
      gender = input$gender,
      education = input$edu,
      work = input$work,
      living_time=input$liv,
      familiarity = input$fam,
      nep1 = input$matInput2$NEP1[1],
      nep2 = input$matInput2$NEP2[1],
      nep3 = input$matInput2$NEP3[1],
      user_lat = user_lat,
      user_lng = user_lng
    )
    quest<-rbind(quest_all,quest)
    saveRDS(quest,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
    
  })
  
  ## confirm expl switch to tab mapping I
  observeEvent(input$sub3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
  })
  observeEvent(input$sub3, {
    hideTab(inputId = "inTabset",
            target = "p2")
  })
  observeEvent(input$sub3, {
    showTab(inputId= "inTabset",
            target = "p3")
    rand_es_sel<-rand_es_sel()
    userID<-userID()
    # output$es_title<-renderText(rand_es_sel$es_name_long)
    # output$es_descr<-renderText(rand_es_sel$description)
    
    v = mapselectServer("mapping1", sf_bound, comb, rand_es_sel, userID, geometry, vis_qc)
    
    # output$cond_b3<-renderUI({
    #   if(v() == 1){
    #     actionButton('sub2', 'next ES')
    #   }
    # })
    
  })
  
  ## switch to tab mapping II
  observeEvent(input$sub2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
  })
  observeEvent(input$sub2, {
    hideTab(inputId = "inTabset",
            target = "p3")
  })
  observeEvent(input$sub2, {
    showTab(inputId= "inTabset",
            target = "p4")
    rand_es_sel2<-rand_es_sel2()
    userID<-userID()
    w=mapselectServer("mapping2", sf_bound, comb, rand_es_sel2, userID, geometry, vis_qc)
    # output$cond_b4<-renderUI({
    #   if(w() == 1){
    #     actionButton("sub4","next ES")
    #   }
    # })
  })
  
  ## confirm mapping switch to tab mapping III
  observeEvent(input$sub4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p5")
  })
  observeEvent(input$sub4, {
    hideTab(inputId = "inTabset",
            target = "p4")
  })
  observeEvent(input$sub4, {
    showTab(inputId= "inTabset",
            target = "p5")
    rand_es_sel3<-rand_es_sel3()
    userID<-userID()
    
    u=mapselectServer("mapping3", sf_bound, comb, rand_es_sel3, userID, geometry, vis_qc)
    # output$cond_b5<-renderUI({
    #   if(u() == 1){
    #     actionButton("sub5","end")
    #   }
    # })
  })
  
  
  ##first random ES
  rand_es_sel<-eventReactive(input$sub3,{
    rand_ind<-sample(es_descr$es_ind,1)
    rand_es_sel<-es_descr%>%filter(es_ind %in% rand_ind)
    
    return(rand_es_sel)
  })
  

  ## 2nd random ES not old one
  
  rand_es_sel2<-eventReactive(input$sub2,{
    rand_es_sel <- rand_es_sel()
    rand_es_sel2<-es_descr
    #remove the old value
    rand_es_sel2<-rand_es_sel2%>%filter(es_id!=rand_es_sel$es_id)
    #sampel another out of the rest
    
    rand_ind<-sample(rand_es_sel2$es_ind,1)
    rand_es_sel2<-rand_es_sel2%>%filter(es_ind %in% rand_ind)
    
    return(rand_es_sel2)
  })
  
### third random ES not other ones
  rand_es_sel3<-eventReactive(input$sub4,{
    rand_es_sel <- rand_es_sel()
    rand_es_sel2 <- rand_es_sel2()
    rand_es_sel3<-es_descr
    #remove the old value
    rand_es_sel3<-rand_es_sel3%>%filter(es_id!=rand_es_sel$es_id & es_id!=rand_es_sel2$es_id )
    #sampel another out of the rest
    
    rand_ind<-sample(rand_es_sel3$es_ind,1)
    rand_es_sel3<-rand_es_sel3%>%filter(es_ind %in% rand_ind)
    
    return(rand_es_sel3)
  })
  
## terminate app
  observeEvent(input$sub5,{
    stopApp(returnValue = invisible())
  })
  
  
  

}