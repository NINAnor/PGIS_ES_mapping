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
    ###here it might be wise to double check if the UID is unique but with DB solution no problem
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
  
  ## as soon as ID generated, random es order, save order in vector per part. sample 4 out of 8 ES (shuffeled order)
  rand_es_sel<-eventReactive(input$sub0,{
    rand_es_sel<-es_all%>%slice_sample(n=4, replace = F)
    return(rand_es_sel)
  })
  
  rand_es_nonSel<-eventReactive(input$sub0,{
    rand_es_sel<-rand_es_sel()
    rand_es_nonSel<-es_all%>%anti_join(rand_es_sel)
    return(rand_es_nonSel)
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
      need(input$liv != '', 'provide the years you live in the study area'),
      need(input$land != '', 'provide a landscape type')
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
    hideTab(inputId = "inTabset",
            target = "p1")
  })
  observeEvent(input$sub1, {
    showTab(inputId = "inTabset", target = "p2")
    userID<-userID()
    liv_pol<-liv_pol()
    rand_es_sel<-rand_es_sel()
    es_order<-paste0(rand_es_sel$es_ind[1],"_",rand_es_sel$es_ind[2],"_",rand_es_sel$es_ind[3],"_",rand_es_sel$es_ind[4])
    
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
      NEP1 = input$matInput2$NEP1[1],
      NEP2 = input$matInput2$NEP2[1],
      NEP3 = input$matInput2$NEP3[1],
      NEP4 = input$matInput2$NEP3[1],
      land = input$land,
      user_lat = user_lat,
      user_lng = user_lng,
      es_order = es_order
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
    rand_es_nonSel<-rand_es_nonSel()
    userID<-userID()

    
    v = mapselectServer("mapping1", sf_bound, comb, rand_es_sel, rand_es_nonSel, 1, userID, geometry, vis_qc)
    
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
   
    #2nd col
    rand_es_sel<-rand_es_sel()
    rand_es_nonSel<-rand_es_nonSel()
    userID<-userID()
    w=mapselectServer("mapping2", sf_bound, comb, rand_es_sel, rand_es_nonSel, 2, userID, geometry, vis_qc)
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
    #2nd col
    rand_es_sel<-rand_es_sel()
    rand_es_nonSel<-rand_es_nonSel()
    userID<-userID()
    w=mapselectServer("mapping3", sf_bound, comb, rand_es_sel, rand_es_nonSel, 3, userID, geometry, vis_qc)
    # output$cond_b5<-renderUI({
    #   if(u() == 1){
    #     actionButton("sub5","end")
    #   }
    # })
  })
  
  
## terminate app
  observeEvent(input$sub5,{
    stopApp(returnValue = invisible())
  })
  
  
  

}