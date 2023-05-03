# call module questionnaire
plz<-sf::st_read("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/postnummeromrade_wgs.shp")
plz<-st_as_sfc(plz)

questionUI = function(id, label = "quest") {
  ns <- NS(id)
  tagList(
              numericInput(ns("age"),"How old are you?",NULL,min=12,max=110,step=1),
              selectInput(ns("gender"),"What is your gender?",c("please select" = NA,
                                                                          "Female" ="f",
                                                                         "Male" = "m",
                                                                          "Neutral" = "n",
                                                                         "Prefer not to say"="no_answ"),multiple = F,selected = NULL),
              selectInput(ns("edu"),"What is the highest level of education you have attained?",c("please select" = NA,
                                                                                                  "Upper secondary education" ="upper_s",
                                                                                              "Tertiary vocational programme" = "tertiary",
                                                                                              "Bachelor"="bsc",
                                                                                              "Master" = "msc",
                                                                                              "Doctoral degree" = "phd",
                                                                                              "Prefer not to say"="no_answ"),multiple = F,selected = NULL),
              selectInput(ns("work"),"In which economic sector do you currently work?",
                          c("please select" = NA,"Mining and quarrying" = "mining",
                            "Manufacturing"= "man",
                            "Electricity, gas, steam and air conditioning supply"= "energy",
                            "Water supply; sewerage, waste management and remediation activities" =  "water",
                            "Construction" = "cons",
                            "Distributive trade sector" = "trade",
                            "Transportation and storage services" = "transport",
                            "Accommodation and food services" = "accomodation",
                            "Information and communication services" = "inform",
                            "Real estate activities" = "real_estate",
                            "Professional, scientific and technical activities" = "science",
                            "Administrative and support service activities" = "admin",
                            "Repair of computers and personal and household goods" = "repair"),multiple = F,selected = NULL),
              br(),
              mapedit::selectModUI(ns("map_living")),
              
              
              selectInput(ns("liv"),"How long have you lived in Trondheim area in total?",c("please select" = NA,
                                                                                            " I don`t  live in the Trondheim area" ="no_TRD",
                                                                                        "Less than 5 years" = "few",
                                                                                        "5 - 10 years"="more",
                                                                                        "10 - 20 years" = "more2",
                                                                                        "more than 20 years" = "all",
                                                                                        "Prefer not to say"="no_answ"),multiple = F,selected = NULL),
              
              selectInput(ns("fam"),"How familiar are you with the areas in and around Trondheim?",c("please select" = NA,
                                                                                                     "poor" ="poor",
                                                                                                 "fair" = "fair",
                                                                                                 "good"="good"),multiple = F,selected = NULL),
              br(),
              
              # new environmental paradigm (NEP)

              radioMatrixInput(ns("matInput2"),
                               rowIDs = c("NEP1","NEP2","NEP3"),
                               rowLLabels =  c("We are approaching the limit of the number of people the Earth can support",
                                               "Humans have the right to modify the natural environment to suit their needs.",
                                               "When humans interfere with nature it often produces disastrous consequences."),
                               choices = c("strongly agree", "agree", "unsure","disagree","strongly disagree"),
                               choiceNames = c("0", "1", "2","3","4"))
  
  )
}

questionServer<-function(id,  plz) {
  moduleServer(
    id,
    function(input,output,session){
      # quest_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
      
      # userID = userID
      plz = plz
      
      map_liv<- leaflet() %>%
        addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
        addFeatures(st_sf(plz), layerId = ~seq_len(length(plz)))
      
      liv_pol <- callModule(module=selectMod, 
                            leafmap=map_liv,
                            id="map_living")
      
      liv_pol<-st_sf(plz[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
      cent<-st_centroid(liv_pol)
      user_lat <- st_coordinates(cent)[2]
      user_lng <- st_coordinates(cent)[1]
      

      quest <- reactive({


        list(

          userID=userID,
          age=input$age,
          gender = input$gender,
          education = input$edu,
          work = input$work,
          living_time<-input$liv,
          familiarity = input$fam,
          nep1 = input$matInput2$NEP1[1],
          nep2 = input$matInput2$NEP2[1],
          nep3 = input$matInput2$NEP3[1],
          user_lat = user_lat,
          user_lng = user_lng

        )

      })
      quest<-as.data.frame(({ quest() }))
      # quest_all<-rbind(quest_all,quest)
      # saveRDS(quest_all,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
      
    }
  )
}
### main app
ui <- fluidPage(
  questionUI("mapping1", "Counter #1")
)

server <- function(input, output, session) {
  questionServer("mapping1", plz)
}

shinyApp(ui, server)
