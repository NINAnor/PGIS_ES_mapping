function(input, output, session) {
  
  #track_usage(storage_mode = store_rds(path = "logs/"))
  
  ## hiding all tabs but not start
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  hideTab(inputId = "inTabset", target = "p5")
  hideTab(inputId = "inTabset", target = "p6")
  hideTab(inputId = "inTabset", target = "p7")
  
  
  ####### generate user ID
  userID<-eventReactive(input$sub0, {
    # create random large string
    UID_part<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
    if(is.null(input$email)){
      email<-"not_provided"
    } else {
      email = input$email}
    # generate log time
    tlog<-Sys.time()
    
    user_conf<-data.frame(userMAIL = email,userID = UID_part,userTLOG = tlog)
    ### save user conf
    insert_upload_job("rgee-381312", "data_base", "user_conf", user_conf)
    #user_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
    ###here it might be wise to double check if the UID is unique but with DB solution no problem
    ####
    
    #user_all<-rbind(user_all,user_conf)
    #saveRDS(user_all,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
    rm(user_conf)
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
  
  ## as soon as ID generated, random es order, save order in vector per part. sample 4 out of 8 ES (shuffled order)
  rand_es_sel<-eventReactive(input$sub0,{
    rand_es_sel<-es_all%>%slice_sample(n=num_es, replace = F)
    return(rand_es_sel)
  })
  
  # rand_es_nonSel<-eventReactive(input$sub0,{
  #   rand_es_sel<-rand_es_sel()
  #   rand_es_nonSel<-es_all%>%anti_join(rand_es_sel)
  #   return(rand_es_nonSel)
  # })
  


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
    es_order<-paste0(rand_es_sel$esNUM[1],"_",rand_es_sel$esNUM[2],"_",rand_es_sel$esNUM[3],"_",rand_es_sel$esNUM[4])
    
    #quest_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
    liv_pol<-st_sf(plz[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
    cent<-st_centroid(liv_pol)
    user_lat <- st_coordinates(cent)[2]
    user_lng <- st_coordinates(cent)[1]
    print(user_lng)
    quest <-  data.frame(
      userID = userID,
      siteID = siteID,
      edu = input$edu,
      fam = input$fam,
      liv = input$liv,
      gen = input$gender,
      age = as.integer(input$age),
      work = input$work,
      # nep_1 = input$matInput2$NEP1[1],
      # nep_2 = input$matInput2$NEP2[1],
      # nep_3 = input$matInput2$NEP3[1],
      # nep_4 = input$matInput2$NEP3[1],
      nep_1 = NA,
      nep_2 = NA,
      nep_3 = NA,
      nep_4 = NA,
      
      nep_5 = NA,
      nep_6 = NA,
      nep_7 = NA,
      nep_8 = NA,
      nep_9 = NA,
      nep_10 = NA,
      nep_11 = NA,
      nep_12 = NA,
      nep_13 = NA,
      nep_14 = NA,
      nep_15 = NA,
      nep_16 = NA,
      nep_17 = NA,
      nep_18 = NA,
      nep_19 = NA,
      nep_20 = NA,
      userLAT = user_lat,
      userLNG = user_lng,
      land = input$land,
      es_order = es_order,
      questUID = paste0(userID,"_",siteID,"_",es_order)
    )
    
    insert_upload_job("rgee-381312", "data_base", "user_all", quest)
    #quest<-rbind(quest_all,quest)
    #saveRDS(quest,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
    
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
    # rand_es_nonSel<-rand_es_nonSel()
    userID<-userID()
    
    
    #v = mapselectServer("mapping1", sf_bound, comb, rand_es_sel,  1, userID, siteID, geometry, vis_qc)
    u = mapselectServer("mapping1",sf_bound, comb, bands, rand_es_sel, 1, userID, siteID, geometry, maxentviz)
    
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
    # rand_es_nonSel<-rand_es_nonSel()
    userID<-userID()
    # w=mapselectServer("mapping2", sf_bound, comb, rand_es_sel,  2, userID, siteID, geometry, vis_qc)
    v = mapselectServer("mapping2",sf_bound, comb, bands, rand_es_sel, 2, userID, siteID, geometry, maxentviz)
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
    # rand_es_nonSel<-rand_es_nonSel()
    userID<-userID()
    # w=mapselectServer("mapping3", sf_bound, comb, rand_es_sel, 3, userID, siteID, geometry, vis_qc)
    w = mapselectServer("mapping3",sf_bound, comb, bands, rand_es_sel, 3, userID, siteID, geometry, maxentviz)
    # output$cond_b5<-renderUI({
    #   if(u() == 1){
    #     actionButton("sub5","end")
    #   }
    # })
  })
  
  ## AHP section
  observeEvent(input$sub5, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p6")
  })
  observeEvent(input$sub5, {
    hideTab(inputId = "inTabset",
            target = "p5")
  })
  observeEvent(input$sub5, {
    showTab(inputId= "inTabset",
            target = "p6")

    userID<-userID()
    ahp_secServer("ahp_section",   userID, siteID, es_all)

  })
  
  ## AHP detail
  observeEvent(input$sub6, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p7")
  })
  observeEvent(input$sub6, {
    hideTab(inputId = "inTabset",
            target = "p6")
  })
  observeEvent(input$sub6, {
    showTab(inputId= "inTabset",
            target = "p7")
    
    userID<-userID()
    
    ahpServer("ahp", userID, siteID, es_all)
    
  })
  
  
## terminate app
  observeEvent(input$sub7,{
    stopApp(returnValue = invisible())
  })
  
  
  

}