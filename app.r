library(tidyverse)
library(kableExtra)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(sf)
library(leaflet)


crops<- read_csv("Total_Crops.csv")

####################################################
# map markers
####################################################

###### Cotton
# TEXAS
cotton_regen_texas_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "Texas") %>%
  filter(Practice == "Regenerativeerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_organic_texas_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "Texas") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_texas<- bind_rows(cotton_regen_texas_table, cotton_organic_texas_table)
colnames(cotton_texas)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(cotton_texas)<- c("Regenerative ", "Organic")

cotton_texas_table<- kable(cotton_texas, caption = "Cotton: Texas") %>% 
  kable_styling(bootstrap_options = "striped")
cotton_texas_table

#INDIA
cotton_regen_india_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "India") %>%
  filter(Practice == "Regenerativeerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_organic_india_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "India") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_india<- bind_rows(cotton_regen_india_table, cotton_organic_india_table)
colnames(cotton_india)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(cotton_india)<- c("Regenerative ", "Organic")

cotton_india_table<- kable(cotton_india, caption = "Cotton: India") %>% 
  kable_styling(bootstrap_options = "striped")
cotton_india_table

# CHINA
cotton_regen_china_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "China") %>%
  filter(Practice == "Regenerativeerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_organic_china_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "China") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_china<- bind_rows(cotton_regen_china_table, cotton_organic_china_table)
colnames(cotton_china)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(cotton_china)<- c("Regenerative ", "Organic")

cotton_china_table<- kable(cotton_china, caption = "Cotton: China") %>% 
  kable_styling(bootstrap_options = "striped")
cotton_china_table

#PERU
cotton_regen_peru_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "Peru") %>%
  filter(Practice == "Regenerativeerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_organic_peru_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "Peru") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

cotton_peru<- bind_rows(cotton_regen_peru_table, cotton_organic_peru_table)
colnames(cotton_peru)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(cotton_peru)<- c("Regenerative ", "Organic")

cotton_peru_table<- kable(cotton_peru, caption = "Cotton: Peru") %>% 
  kable_styling(bootstrap_options = "striped")
cotton_peru_table



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
          leafletOutput(outputId = "map_1", height = 1000))
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
  
  
  output$map_1<- renderLeaflet(leaflet(map_data) %>% 
                                 addProviderTiles("CartoDB.DarkMatter") %>% 
                                 addCircleMarkers(lng = -95.300003, 
                                                  lat = 32.349998, 
                                                  popup = cotton_texas_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "red", 
                                                  fillOpacity = 0.3) %>% 
                                 addCircleMarkers(lng = 78.6569, 
                                                  lat = 22.9734, 
                                                  popup = cotton_india_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "red", 
                                                  fillOpacity = 0.3) %>% 
                                 addCircleMarkers(lng = 87.5396, 
                                                  lat = 42.5246, 
                                                  popup = cotton_china_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "red", 
                                                  fillOpacity = 0.3) %>% 
                                 addCircleMarkers(lng = -80.6549, 
                                                  lat = -5.1783, 
                                                  popup = cotton_peru_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "red", 
                                                  fillOpacity = 0.3) %>% 
                                 setView(40, 6, 2))  

  
}
  



shinyApp(ui = ui, server = server)




