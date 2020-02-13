library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(sf)




ui<- dashboardPage(skin = "black",
  dashboardHeader(title = "Carbon for Crops"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regenratives Globally", tabName = "map"),
      menuItem("Soil Organic Carbon & GWP", tabName = "overview"),
      menuItem("Practices", tabName = "sensativity"),
      menuItem("Sources of GHG's", tabName = "ghg")
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fillPage(
          tmapOutput(outputId = "map_1"))
        )
      )
    )
  )






server<- function(input, output){
  
  map_data<- tribble(
    ~id, ~lon, ~lat,
    "Cotton", -95.300003, 32.349998,
    "Cotton", 87.5396, 42.5246,
    "Cotton", 78.6569, 22.9734,
    "Cotton", -80.6549, -5.1783,
    "Bison Grazing", -103.2310, 44.0805,
    "Bison Grazing", -56.9211, -12.6819,
    "Mango", 72.8777, 19.0760,
    "Mango", -85.2072, 12.8654,
    "Kernza", -94.6859, 46.7296,
    "Kernza", -98.4842, 39.0119
  )

  map_data_sf<- st_as_sf(map_data, coords = c("lon", "lat"))
  
  
  output$map_1<- renderTmap(tm_basemap("CartoDB.DarkMatter")+
                              tm_shape(map_data_sf)+
                              tm_dots(label = "id", col = "id", size = 0.1))
  
}
  



shinyApp(ui = ui, server = server)




