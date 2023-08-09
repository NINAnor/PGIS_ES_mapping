function(input, output, session) {
  
  #track_usage(storage_mode = store_rds(path = "logs/"))
  
  ## hiding all tabs but not start tab
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  hideTab(inputId = "inTabset", target = "p5")
  hideTab(inputId = "inTabset", target = "p6")
  hideTab(inputId = "inTabset", target = "p7")
  hideTab(inputId = "inTabset", target = "p8")
  
  
  url <- a(site$siteNAME, href=site$siteLINK)
  output$study_area<-renderUI(
    tagList(
      h6(site$siteNAME),
      fluidRow(
        column(7,
         renderLeaflet(map_area)      
               ),
        column(5,
               fluidRow(paste0("Area [km2]: ",site$siteAREA)),
               fluidRow(paste0("Population: ",site$sitePOP)),
               tagList("URL link:", url)

      )),
      br(),
      fluidRow(
        column(3,
               tags$figure(
                 class = "centerFigure",
                 tags$img(
                   src = paste0("/study_areas/",studyID,"_1",".jpg"),
                   width = 600,
                   alt = "Picture of an astragalus (bone die)"
                 ),
                 tags$figcaption("Image of Astragalus by Yaan, 2007")
               )),
        column(1),
        column(3,
               tags$figure(
                 class = "centerFigure",
                 tags$img(
                   src = paste0("/study_areas/",studyID,"_2",".jpg"),
                   width = 600,
                   alt = "Picture of an astragalus (bone die)"
                 ),
                 tags$figcaption("Image of Astragalus by Yaan, 2007")
               )),
        column(1),
        column(3,
               tags$figure(
                 class = "centerFigure",
                 tags$img(
                   src = paste0("/study_areas/",studyID,"_3",".jpg"),
                   width = 600,
                   alt = "Picture of an astragalus (bone die)"
                 ),
                 tags$figcaption("Image of Astragalus by Yaan, 2007")
               ))
      )
      
    )
  )
  
  #before userID is generated validate if provided mail is not already present --> mess in R2!
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

  ## generate user ID
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
    rm(user_conf)
    userID<-as.character(UID_part)
    return(userID)
  })
  
  
  ## sample n out of all ES from the es table
    rand_es_sel<-es_all%>%slice_sample(n=num_es, replace = F)
  
  # call the module for the selection of living place  
  liv_pol <- callModule(module=selectMod, 
                        leafmap=map_liv,
                        id="map_living")
  
  
  ## before switching to expl with sub1, we should validate if all values are filled out
  output$cond_b1<-renderUI({
    validate(
      need(input$age, 'Provide your age'),
      need(input$gender != '', 'Select a gender'),
      need(input$edu != '', 'Select an education'),
      need(input$work != '', 'Select a working industry'),
      need(input$liv != '', 'provide the years you live in the study area'),
      need(input$land != '', 'provide a landscape type')
    )
    actionButton('sub1', 'submit answers', class='btn-primary')
  })
  
  ## display expl. tab with initial UI and save user questionnaire to bq
  observeEvent(input$sub1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p2")
    
    output$task_1<-renderUI({
      tagList(
      h6("Mapping ecosystem services"),
      br(),
      "Read carefully the following instructions",
      br(),
      actionButton("proc1", "proceed")
      )
    })
    
    userID<-userID()
    liv_pol<-liv_pol()
    # rand_es_sel<-rand_es_sel()
    es_order<-paste0(rand_es_sel$esNUM[1],"_",rand_es_sel$esNUM[2],"_",rand_es_sel$esNUM[3],"_",rand_es_sel$esNUM[4])
    
    liv_pol<-st_sf(grd[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
    # only take first poly if user selected multiple
    liv_pol<-liv_pol[1,]
    cent<-st_centroid(liv_pol)
    user_lat <- st_coordinates(cent)[2]
    user_lng <- st_coordinates(cent)[1]
    
    quest <-  data.frame(
      userID = userID,
      siteID = studyID,
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
      questUID = paste0(userID,"_",studyID,"_",es_order)
    )
    
    insert_upload_job("rgee-381312", "data_base", "user_all", quest)
    
  })
  
  ## description of task
  observeEvent(input$proc1,{
    output$task_2<-renderUI({
      tagList(
        h6("1. Description of ecosystem service"),
        "Ecosystem services are complex, thus it is important that you first read the description of the ecosystem service.",
        br(),
        h6("2. Indicating the importance"),
        br(),
        "Subsequently you are asked to indicate how important this ecosystem service is for you personally and in general for the society. You can ask yourselfe How much you might benefit from this particular service. Please make sure that you refer your rating only to the study area indicated in the introduction of the study.",
        br(),
        actionButton("ok1","got it")
      )
    })

    removeUI(selector = "#task_1")

    
  })
  
  # description 1
  observeEvent(input$ok1,{
    output$task_3<-renderUI({
      tagList(
        h6("3. Can you map it?"),
        "You are now going to be asked if you feel comfortable to indicate on a map of the study area areas where you think you or other can benefit from this eoscystem service",
        br(),
        "3.1. If not",
        br(),
        "No problem, you are now just beeing asked if you would trust a map of this ecosystem service that has been developed by experts or national authorities.",
        "A next ecosystem service will appear which you might be able to map.",
        br(),
        actionButton("ok2","What if I can map...")
      )
    })

    removeUI(selector = "#task_2")
    
  })
  
  # description 2
  observeEvent(input$ok2,{
    output$task_4<-renderUI({
      tagList(
        h6("4. You can map"),
        br(),
        "An interactive map with orange borders indicating the study area will appear. You can pan and zoom the map.",
        "As shown below you can create and modify one or several rectangles.",
        "The rectangle(s) should meet the following aspects:",
        "- deliniate as precise as possible areas of high ecosystem service benefit",
        "- not too small areas: The squares should have an edge length of min 300m",
        "Press save polygons once you are done!",
        br(),
        img(src="tutorial_selecting.gif", align = "left",height='620px',width='836px'),
        br(),
        actionButton("ok3","proceed")
      )
    })
    removeUI(selector = "#task_3")
    
  })  
  
  # description 3
  observeEvent(input$ok3,{
    output$task_5<-renderUI({
      tagList(
        h6("5. Rate your polygons"),
        br(),
        "As soon as you have saved the polygons they will appear on the map with a red number. Below you find for each polygon a slider with the same number.",
        "Please set now the slider value for each polygon. Higher values indicate that the area serves very high quality to benefit from the ecosystem service",
        "Finally you need to write a few keywords why you choosed these areas",
        "Press submit",
        br(),
        img(src="tutorial_rating.gif", align = "left",height='620px',width='836px'),

        actionButton("ok4","proceed")
      )
    })
    removeUI(selector = "#task_4")
    
  })  
  
  # description 4
  observeEvent(input$ok4,{
    output$task_5<-renderUI({
      tagList(
        h6("6. Your Result"),
        br(),
        "Your polygons and ratings are stored and extrapolated to the whole study region - this can take up to 30 seconds. Please wait until the new map appears",
        "The final map shows you the probability to benefit from the respective ecosystem service, based on your inputs.",
        "Press next and this procedure will repeat four times in total",
        br(),
        actionButton('sub3', 'Go to first mapping task', class='btn-primary')
      )
    })

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

  # call the mapping maxent module
  m1<- mapselectServer("mapping1",sf_bound, comb, bands, rand_es_sel, 1, isolate(userID()), studyID, geometry, maxentviz)
  
  ## switch to tab mapping II
  observeEvent(m1(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
    hideTab(inputId = "inTabset",
            target = "p3")
    showTab(inputId= "inTabset",
            target = "p4")
  })
  
  # call the mapping maxent module
  m2 = mapselectServer("mapping2",sf_bound, comb, bands, rand_es_sel, 2, isolate(userID()), studyID, geometry, maxentviz)
  
  ## confirm mapping switch to tab mapping III
  observeEvent(m2(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p5")
    hideTab(inputId = "inTabset",
            target = "p4")
    showTab(inputId= "inTabset",
            target = "p5")
    
  })

  # call the mapping maxent module
  m3 = mapselectServer("mapping3",sf_bound, comb, bands, rand_es_sel, 3, isolate(userID()), studyID, geometry, maxentviz)
  
  ## confirm and switch to AHP section
  observeEvent(m3(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p6")
    hideTab(inputId = "inTabset",
            target = "p5")
    showTab(inputId= "inTabset",
            target = "p6")
    
  })
  
  m4<-ahp_secServer("ahp_section", isolate(userID()), studyID, es_all)
  
  ## confirm and switch to AHP detail
  observeEvent(m4(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "p7")
    hideTab(inputId = "inTabset",
            target = "p6")
    showTab(inputId= "inTabset",
            target = "p7")
  })
  
  m5<-ahpServer("ahp", isolate(userID()), studyID, es_all)
  
  ##confirm and switch to effect distance of WT
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
      siteID = studyID,
      dist_cult = as.integer(dist_cult),
      dist_aes = as.integer(dist_aest),
      dist_recr = as.integer(dist_recr),
      dist_wild_prod = as.integer(dist_wild_prod))
    
    insert_upload_job("rgee-381312", "data_base", "impact_dist", dist_table)
    
    stopApp(returnValue = invisible())
  })
}