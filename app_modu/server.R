function(input, output, session) {
  
  #track_usage(storage_mode = store_rds(path = "logs/"))
  
  ## hiding all tabs but not start
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  hideTab(inputId = "inTabset", target = "p5")
  
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

  ## submit questionnaire and switch to expl
  observeEvent(input$sub1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
  })
  observeEvent(input$sub1, {
    hideTab(inputId = "inTabset", target = "p1")
  })
  observeEvent(input$sub1, {
    showTab(inputId = "inTabset", target = "p2")
    
    
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
    # quest_data$t2<-Sys.time()
  })
  
  ## confirm expl switch to tab mapping II
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
    # quest_data$t2<-Sys.time()
  })

  ## confirm mapping switch to view map
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
  })

  
  ########## 1. Questionnaire and living map
 
  liv_pol <- callModule(selectMod, "map_living",
                        leaflet() %>%
                          addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
                          addFeatures(st_sf(plz), layerId = ~seq_len(length(plz)))


  )
  #1. call questionnaire module
  observeEvent(input$sub1, { 
    ## extract centroid
    userID<-userID()
    gs<-liv_pol()
    gs<-st_sf(plz[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    cent<-st_centroid(gs)
    user_lat <- st_coordinates(cent)[2]
    user_lng <- st_coordinates(cent)[1]

    callModule(return_quest_Server, "return_quest", userID, user_lat, user_lng)
    })
  
  
  ####### 2. select a first random ES based on the global es list
  rand_es_sel<-eventReactive(input$sub3,{
    rand_ind<-sample(es_descr$es_ind,1)
    rand_es_sel<-es_descr%>%filter(es_ind %in% rand_ind)

    return(rand_es_sel)
  })
  
  ## 2.1 call the module to map the ES (needs user ID and first selected ES)
  observeEvent(input$sub3,{
    rand_es_sel<-rand_es_sel()
    userID<-userID()
    callModule(mapping_server,"es_train1", rand_es_sel, userID, sf_bound, comb, geometry)
  })
  

  ###########
  ##backup here
  ###########

  ####### 3. select a second random ES based on the global es list and exclude the first one...
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
    
  #### 3.1 the second random selected ES
    observeEvent(input$sub2, {
      rand_es_sel2<-rand_es_sel2()
      userID<-userID()
      callModule(mapping_server,"es_train2", rand_es_sel2, userID, sf_bound, comb, geometry)
    })
  
    #### can be extended but not for POC, first draw....

  
  
  ####### 4. Show maps of the random selected ES
  
  # map_ind <- eventReactive(input$sub4,{
  #   rand_es_sel<-rand_es_sel()
  #   rand_es_sel2<-rand_es_sel2()
  #   userID<-userID()
  #   path1<-paste0('users/SPRETO/rgee/individual_R1_',rand_es_sel$es_id)
  #   
  #   #prediction<-ee$Image(prediction)
  #   # gee_poly <- gee_poly()
  # 
  #   col_es_1 <- ee$ImageCollection(path1)
  #   user_img_es_1<-ee$Image(paste0(path1,"/",userID))
  # 
  #   mean <- col_es_1$reduce(ee$Reducer$mean())
  #   diff <- user_img_es_1$subtract(mean)
  #   
  #   
  #   ## vis params in global
  #   
  #   Map$setCenter(10.38649, 63.40271,10)
  #   m1<-Map$addLayer(
  #     eeObject = user_img_es_1,
  #     vis_qc,
  #     opacity = 0.4
  #   ) +Map$addLegend(vis_qc,name = "your map", color_mapping = "character") 
  #   # +
  #   #   Map$addLayer(gee_poly, list(color = "blue"), "colored")
  #   m2<-Map$addLayer(
  #     eeObject = mean,
  #     vis_qc,
  #     opacity = 0.4
  #   ) +Map$addLayer(
  #     eeObject = diff,
  #     vis_diff,
  #     opacity = 0.4
  #   ) +Map$addLegend(vis_diff,name = "difference", color_mapping = "character")
  #   m1  | m2
  # },
  # ignoreNULL = FALSE
  # )
  # 
  # 
  # 
  # output$gee_map <- renderLeaflet({
  #   map_ind()
  # })
  # 
  # output$n_img <-renderText({
  #   user_es <- rand_es_sel()
  #   col <- ee$ImageCollection(paste0('users/SPRETO/rgee/individual_R1_',user_es$es_id))
  #   n_img<-length(col$getInfo()$features)
  #   paste0("The crowd map is absed on ", n_img," other participants")
  #   
  # })
}
