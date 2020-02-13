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
    tabItem(
      tabItem(
        tabName = "map",
        fluidPage(
          box(
            title = "Potential for Regenratives Within Supply Chain",
            tmapOutput(outputId = "map")
          )
        )
      )
    )
  )
)





server<- function(input, output){
  
  output$map<- renderTmap({
    tm_basemap("CartoDB.DarkMatter")
    tmap_mode("view")
    })
  
}








shinyApp(ui = ui, server = server)




