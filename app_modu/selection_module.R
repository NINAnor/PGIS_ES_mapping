
return_select_UI = function(id) {
  ns <- NS(id)
  
  tagList(
  mapedit::editModUI("map")
)
} 
  
return_select_Server = function(input, output, session) {
  

    edits  <- callModule(selectMod, "map",
                         leaflet() %>%
                           addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10))

} 

 