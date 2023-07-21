function(input, output, session) { 

  ## start and hide map tab
  hideTab(inputId = "tabset", target = "p1")
  # hideTab(inputId = "tabset", target = "p2")
  # hideTab(inputId = "tabset", target = "p3")
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
    return(userID_sel)
  })
 
  userES<-eventReactive(input$sub0,{
    userID_sel<-userID_sel()
    userES <- tbl(con, "es_mappingR1")
    #dplyr sql
    userES <- select(userES, userID, esID, mapping, siteID) %>% filter(siteID == siteID_sel, userID == userID_sel)%>%
      collect()

  })
  
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
      remapServer("remap1", userID_sel, blog_data, es_descr, userES, siteID_sel, geometry, sf_bound, vis_qc,3)

    }else{
      output$login_res<-renderText("Mail adress NOT found")
      
    }
    })
    


    

  
# #blog
#   observeEvent(input$es_individual, {
#     
#     blog_data<-es_user%>%filter(es_id == input$es_individual)%>%select(blog)
#     #only keep non empty blog entries
#     blog_data<-blog_data%>%filter(blog !="")
#     blog_data$username<-rep("USER_",nrow(blog_data))
#     blog_data$username<-paste0(blog_data$username,as.numeric(rownames(blog_data)), ": ",blog_data$blog)
#     blog_data<-blog_data%>%select(username)
#     output$blog<-renderDT(blog_data,rownames= FALSE, colnames="Blog entries")
# 
#   })
# # map  
#   observeEvent(input$sub1, {
#     userID<-userID()
#     mapeditServer("edit1","food_prov", userID, vis_qc)
#   })
    
#     imgpath<-paste0(ee_get_assethome(), '/rgee/individual_R1_',input$es_individual,"/", userID)
# 
#     colpath<-paste0(ee_get_assethome(), '/rgee/individual_R1_',input$es_individual)
#     img_ind<-ee$Image(imgpath)
#     
#     mean_col<-ee$ImageCollection(colpath)
#     mean_col <- mean_col$reduce(ee$Reducer$mean())
#     diff <- img_ind$subtract(mean_col)
#     #ind_poly<-sf::st_read(paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/output/train_polys_R1/",userID,"_",input$es_individual,".shp"))
#     
#     # gee_poly<-rgee::sf_as_ee(ind_poly, via = "getInfo")
#     
#     Map$setCenter(10.38649, 63.40271,10)
#     m1<-Map$addLayer(
#       eeObject = img_ind,
#       vis_qc,
#       opacity = 0.4
#     ) +Map$addLegend(vis_qc,name = "your predicted ES", color_mapping = "character")
#     m2<-Map$addLayer(
#       eeObject = mean_col,
#       vis_qc,
#       opacity = 0.4
#     ) +Map$addLayer(
#       eeObject = diff,
#       vis_diff,
#       opacity = 0.4
#     ) +Map$addLegend(vis_diff,name = "diff", color_mapping = "character")
#     m1  | m2
#   },
#   ignoreNULL = FALSE
#   )
# # render map  
#   output$es_maps <- renderLeaflet({
# 
#     map()
#   })

# importance
  # bar_plot<-eventReactive(input$es_individual,{
  #   userID <- userID()
  #   es_sel<-es_user%>%filter(es_id == input$es_individual)
  #   ind_imp<-es_user%>%filter(es_id == input$es_individual & userID == userID)%>%select(imp_own,imp_other)
  #   
  #   bar_plot<-plot_ly(es_sel, y =~mean(imp_own), type = "bar", name = "mean own importance for people",color = "red")%>%
  #     add_bars(es_sel, y =~mean(imp_other), type = "bar", name = "mean imp society",color = "blue")%>%
  #     add_segments(x = -0.5, xend = 0, y = ind_imp$imp_own, yend = ind_imp$imp_own, color="black",name = "your value") %>%
  #     add_segments(x = 0, xend = 0.5, y = ind_imp$imp_other, yend = ind_imp$imp_other, color="black",name = "your value")
  # 
  # })
  # 
  # output$importance<-renderPlotly(bar_plot())

  }