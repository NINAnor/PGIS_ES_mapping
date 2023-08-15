function(input, output, session) { 

  
  ## define reactives
  rv<-reactiveValues(
    u = reactive({}),
    v = reactive({}),
    w = reactive({})
  )
  
  ## start and hide map tab
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")

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
      
      output$task_1<-renderUI({
        tagList(
          h6("Remapping ecosystem services"),
          br(),
          "Please read the following instructions carefully",
          br(),
          actionButton("ok0", "proceed")
        )
      })


    }else{
      output$login_res<-renderText("Mail address NOT found")
      
    }
    })

  observeEvent(input$ok0,{
    output$task_2<-renderUI({
      tagList(
        h3("Thank you very much for your participation in the first round"),
        br(),
        "You will now see your mapped ecosystem services again.",
        br(),
        h3("If you couldn't map the ecosystem service"),
        br(),
        "You are then just able to see the areas of high es benefits based on other peoples responses. In addition you can see, why they choosed these sites. However, in this case you can not remap anything and are asked to proceed with the next ecosystem service.",
        br(),
        actionButton("ok1","What if I have mapped?")
      )
    })
    
    removeUI(selector = "#task_1")
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 10
    )

  })
  
  observeEvent(input$ok1,{
    output$task_3<-renderUI({
      tagList(
        h3("Adjust, delete or add areas"),
        br(),
        "If you were able to map the areas with high ES benefits in round 1, you can adjust, delete or redraw areas where you think you or other benefit from the shown ecosystem service",
        br(),
        "You should do that by considering the general map of all other participants and their explanation why they have mapped these areas. You might agree or disagree and adjust your polygons accordingly.",
        "If you think the general map represents your areas good enough just indicate that you do not want to adjust anything",
        br(),
        actionButton("ok2","How to adjust?")
      )
    })
    
    removeUI(selector = "#task_2")
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 13
    )
    
  })
  
  observeEvent(input$ok2,{
    output$task_4<-renderUI({
      tagList(
        h3("General rules"),
        fluidRow(strong("- deliniate as precise as possible areas of high ecosystem service benefit")),
        fluidRow(strong("- not too small areas: The squares should have an edge length of min 300m")),
        "Press save polygons once you are done!",
        h4("Delete"),
        br(),
        "Press save, after delete!",
        # fluidRow(img(src="delete.gif", align = "central",height='310px',width='418px')),
        br(),
        h4("Add"),
        # fluidRow(img(src="add.gif", align = "central",height='310px',width='418px')),
        br(),
        h4("Adjust"),
        "Press save, after adjust!",
        # fluidRow(img(src="adjust.gif", align = "central",height='310px',width='418px')),
        br(),
        fluidRow(actionButton("ok3","Go to task"))
        
      )
    })
    removeUI(selector = "#task_3")
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 15
    )
    
  })  
  
  observeEvent(input$ok3,{
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
    hideTab(inputId = "inTabset", target = "p1")
    showTab(inputId = "inTabset", target = "p2")
    userID_sel<-userID_sel()
    rv$u<-remapServer("remap1", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,1)
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 17
    )
    
  })
  
  #mod es 2
  observeEvent(rv$u(), {

      #update shiny content
      updateTabsetPanel(session, "inTabset",
                        selected = "p3")
      hideTab(inputId = "inTabset", target = "p2")
      showTab(inputId = "inTabset", target = "p3")
      userID_sel<-userID_sel()
      # userES<-userES%>%filter(userID == userID_sel)
      # userES_sel<-userES[2,]
      rv$v<-remapServer("remap2", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,2)
      updateProgressBar(
        session = session,
        id = "pb1",
        value = 50
      )

  })
  # v<-eventReactive(u(),{
  #   
  #   userID_sel<-userID_sel()
  #   userES<-userES()
  #   userES_sel<-userES[2,]
  #   remapServer("remap2", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,2)
  # })

  
  #mod es 3
  observeEvent(rv$v(), {
    
    #update shiny content
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
    hideTab(inputId = "inTabset", target = "p3")
    showTab(inputId = "inTabset", target = "p4")
      userID_sel<-userID_sel()
      # userES<-userES%>%filter(userID == userID_sel)
      # userES_sel<-userES[3,]
    rv$w<-remapServer("remap3", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,3)
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 75
    )

  })
  # w<-eventReactive(v(),{
  #   
  #   userID_sel<-userID_sel()
  #   userES<-userES()
  #   userES_sel<-userES[3,]
  #   remapServer("remap3", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc,3)
  # })

  #stop app
  observeEvent(rv$w(), {
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 100
    )
      stopApp(returnValue = invisible())
  })

  
  }