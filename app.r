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
        fillPage(tmapOutput(outputId = "map_1"))
        )
      )
    )
  )






server<- function(input, output){
  
  output$map_1<- renderTmap(tm_basemap("CartoDB.DarkMatter"))
  
}
  



shinyApp(ui = ui, server = server)




