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
  hideTab(inputId = "inTabset", target = "p8")
  
  #before userID is generated check if provided mail is not already present --> mess in R2!
  ## before switching to expl, we should validate if all values are filled out
  observeEvent(input$sub0,{
    if(input$email %in% conf$userMAIL & input$email != ""){
      output$cond_b0<-renderUI({
        renderText("email adress already present! please use another one or leafe it empty")
      })
    }else{
      updateTabsetPanel(session, "inTabset",
                        selected = "p1")
      hideTab(inputId = "inTabset", target = "p0")
      showTab(inputId = "inTabset", target = "p1")
      
    }
    
  })
  
  
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
    
    user_conf<-data.frame(userMAIL = email, userID = UID_part, userTLOG = tlog)
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
  
  
  ## as soon as ID generated, random es order, save order in vector per part. sample 4 out of 8 ES (shuffled order)
  # rand_es_sel<-eventReactive(input$sub0,{
    rand_es_sel<-es_all%>%slice_sample(n=num_es, replace = F)
  #   return(rand_es_sel)
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
  
  
  
  ## submit questionnaire and switch to expl
  # observeEvent(input$sub1, {
  #   updateTabsetPanel(session, "inTabset",
  #                     selected = "p2")
  # })
  # observeEvent(input$sub1, {
  #   hideTab(inputId = "inTabset",
  #           target = "p1")
  # })
  observeEvent(input$sub1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p2")
    
    
    userID<-userID()
    liv_pol<-liv_pol()
    # rand_es_sel<-rand_es_sel()
    es_order<-paste0(rand_es_sel$esNUM[1],"_",rand_es_sel$esNUM[2],"_",rand_es_sel$esNUM[3],"_",rand_es_sel$esNUM[4])
    
    liv_pol<-st_sf(plz[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
    # only take first poly if user selected multiple
    liv_pol<-liv_pol[1,]
    cent<-st_centroid(liv_pol)
    user_lat <- st_coordinates(cent)[2]
    user_lng <- st_coordinates(cent)[1]
    
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
    
  })
  
  output$cond_b2<-renderUI({
    validate(
      need(input$check, 'Please confirm')
      
    )
    actionButton('sub3', 'Go to mapping', class='btn-primary')
  })
  ## confirm expl switch to tab mapping I
  observeEvent(input$sub3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
    hideTab(inputId = "inTabset",
            target = "p2")
    showTab(inputId= "inTabset",
            target = "p3")
  })
  # observeEvent(input$sub3, {
  #   # hideTab(inputId = "inTabset",
  #   #         target = "p2")
  # })
  
  
  m1<- mapselectServer("mapping1",sf_bound, comb, bands, rand_es_sel, 1, isolate(userID()), siteID, geometry, maxentviz)
  
  
  ## switch to tab mapping II
  observeEvent(m1(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
    hideTab(inputId = "inTabset",
            target = "p3")
    showTab(inputId= "inTabset",
            target = "p4")
  })
  
  
  m2 = mapselectServer("mapping2",sf_bound, comb, bands, rand_es_sel, 2, isolate(userID()), siteID, geometry, maxentviz)
  
  
  ## confirm mapping switch to tab mapping III
  observeEvent(m2(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p5")
    hideTab(inputId = "inTabset",
            target = "p4")
    showTab(inputId= "inTabset",
            target = "p5")
    
  })
  ## AHP section
  m3 = mapselectServer("mapping3",sf_bound, comb, bands, rand_es_sel, 3, isolate(userID()), siteID, geometry, maxentviz)
  
  observeEvent(m3(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p6")
    hideTab(inputId = "inTabset",
            target = "p5")
    showTab(inputId= "inTabset",
            target = "p6")
    
  })
  
  m4<-ahp_secServer("ahp_section", isolate(userID()), siteID, es_all)
  
  ## AHP detail
  observeEvent(m4(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p7")
    hideTab(inputId = "inTabset",
            target = "p6")
    showTab(inputId= "inTabset",
            target = "p7")
  })
  
  m5<-ahpServer("ahp", isolate(userID()), siteID, es_all)
  
  
  ## effect distance
  observeEvent(m5(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p8")
    hideTab(inputId = "inTabset",
            target = "p7")
    showTab(inputId= "inTabset",
            target = "p8")
  })
  
  
  ## save and terminate app
  
  
  observeEvent(input$sub8,{
    userID<-userID()
    ## remove km
    dist_cult<-gsub("km", "", input$dist_cult)
    dist_aest<-gsub("km", "", input$dist_aest)
    dist_recr<-gsub("km", "", input$dist_recr)
    dist_wild_prod<-gsub("km", "", input$dist_wild_prod)
    
    dist_table <-  data.frame(
      userID = userID,
      siteID = siteID,
      dist_cult = as.integer(dist_cult),
      dist_aes = as.integer(dist_aest),
      dist_recr = as.integer(dist_recr),
      dist_wild_prod = as.integer(dist_wild_prod))
    
    insert_upload_job("rgee-381312", "data_base", "impact_dist", dist_table)
    
    stopApp(returnValue = invisible())
  })
}