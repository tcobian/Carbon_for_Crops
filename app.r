library(tidyverse)
library(kableExtra)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(sf)
library(leaflet)


#######
# Working Space
#######
crops<- read_csv("Total_Crops.csv")




cotton_regen_texas <- crops %>% 
  filter(Country == "Texas",
         Practice == "Regenerative",
         Crop == "Cotton") %>% 
  group_by(Practice)

cotton_organic_texas <- crops%>% 
  filter(Country == "Texas",
         Practice == "Organic",
         Crop == "Cotton") %>% 
  group_by(Practice)


cotton_regen_india <- crops %>% 
  filter(Country == "India",
         Practice == "Regenerative",
         Crop == "Cotton") %>% 
  group_by(Practice)

cotton_organic_india <- crops %>% 
  filter(Country == "India",
         Practice == "Organic",
         Crop == "Cotton") %>% 
  group_by(Practice)


cotton_regen_peru <- crops %>% 
  filter(Country == "Peru",
         Practice == "Regenerative",
         Crop == "Cotton") %>% 
  group_by(Practice)

cotton_organic_peru <- crops %>% 
  filter(Country == "Peru",
         Practice == "Organic",
         Crop == "Cotton") %>% 
  group_by(Practice)

cotton_regen_china <- crops%>% 
  filter(Country == "China",
         Practice == "Regenerative",
         Crop == "Cotton") %>% 
  group_by(Practice)

cotton_organic_china <- crops %>% 
  filter(Country == "China",
         Practice == "Organic",
         Crop == "Cotton") %>% 
  group_by(Practice)

kernza_regen_minnesota <- crops %>% 
  filter(Country == "Minnesota",
         Practice == "Regenerative",
         Crop == "Kernza") %>% 
  group_by(Practice)

kernza_organic_minnesota <- crops %>% 
  filter(Country == "Minnesota",
         Practice == "Organic",
         Crop == "Kernza") %>% 
  group_by(Practice)


kernza_regen_scotland <- crops %>% 
  filter(Country == "Scotland",
         Practice == "Regenerative",
         Crop == "Kernza") %>% 
  group_by(Practice)

kernza_organic_scotland <- crops %>% 
  filter(Country == "Scotland",
         Practice == "Organic",
         Crop == "Kernza") %>% 
  group_by(Practice)

kernza_regen_kansas <- crops %>% 
  filter(Country == "Kansas",
         Practice == "Regenerative",
         Crop == "Kernza") %>% 
  group_by(Practice)

kernza_organic_kansas <- crops %>% 
  filter(Country == "Kansas",
         Practice == "Organic",
         Crop == "Kernza") %>% 
  group_by(Practice)

mango_regen_nicaragua <- crops %>% 
  filter(Country == "Nicaragua",
         Practice == "Regenerative",
         Crop == "Mango") 

mango_organic_nicaragua <- crops %>% 
  filter(Country == "Nicaragua",
         Practice == "Organic",
         Crop == "Mango") 

mango_regen_india <- crops %>% 
  filter(Country == "India",
         Practice == "Regenerative",
         Crop == "Mango") 

mango_organic_india <- crops %>% 
  filter(Country == "India",
         Practice == "Organic",
         Crop == "Mango")

bison_regen_sd <- crops %>% 
  filter(Country == "South Dakota",
         Practice == "Regenerative",
         Crop == "Bison") %>% 
  group_by(Practice)

bison_organic_sd <- crops%>% 
  filter(Country == "South Dakota",
         Practice == "Organic",
         Crop == "Bison") %>% 
  group_by(Practice)

bison_regen_bz <- crops %>% 
  filter(Country == "Brazil",
         Practice == "Regenerative",
         Crop == "Bison") %>% 
  group_by(Practice)

bison_organic_bz <- crops %>% 
  filter(Country == "Brazil",
         Practice == "Organic",
         Crop == "Bison") %>% 
  group_by(Practice)
####################################################
# map markers
####################################################

###### Cotton
# TEXAS
cotton_regen_texas_table<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Country == "Texas") %>%
  filter(Practice == "Regenerative") %>% 
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
  filter(Practice == "Regenerative") %>% 
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
  filter(Practice == "Regenerative") %>% 
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
  filter(Practice == "Regenerative") %>% 
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


########
# grazing
grazing_regen_sd_table<- crops %>% 
  filter(Crop == "Bison") %>% 
  filter(Country == "South Dakota") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

grazing_organic_sd_table<- crops %>% 
  filter(Crop == "Bison") %>% 
  filter(Country == "South Dakota") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

grazing_sd<- bind_rows(grazing_regen_sd_table, grazing_organic_sd_table)
colnames(grazing_sd)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(grazing_sd)<- c("Regenerative ", "Organic")

grazing_sd_table<- kable(grazing_sd, caption = "Grazing: South Dakota") %>% 
  kable_styling(bootstrap_options = "striped")
grazing_sd_table


grazing_regen_bz_table<- crops %>% 
  filter(Crop == "Bison") %>% 
  filter(Country == "Brazil") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

grazing_organic_bz_table<- crops %>% 
  filter(Crop == "Bison") %>% 
  filter(Country == "Brazil") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

grazing_bz<- bind_rows(grazing_regen_bz_table, grazing_organic_bz_table)
colnames(grazing_bz)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(grazing_bz)<- c("Regenerative ", "Organic")

grazing_bz_table<- kable(grazing_bz, caption = "Grazing: Brazil") %>% 
  kable_styling(bootstrap_options = "striped")
grazing_bz_table

##############
# Mangos
mango_regen_nic_table<- crops %>% 
  filter(Crop == "Mango") %>% 
  filter(Country == "Nicaragua") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

mango_organic_nic_table<- crops %>% 
  filter(Crop == "Mango") %>% 
  filter(Country == "Nicaragua") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

mango_nic<- bind_rows(mango_regen_nic_table, mango_organic_nic_table)
colnames(mango_nic)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(mango_nic)<- c("Regenerative ", "Organic")

mango_nic_table<- kable(mango_nic, caption = "Mangos: Nicaragua") %>% 
  kable_styling(bootstrap_options = "striped")
mango_nic_table


mango_regen_india_table<- crops %>% 
  filter(Crop == "Mango") %>% 
  filter(Country == "India") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

mango_organic_india_table<- crops %>% 
  filter(Crop == "Mango") %>% 
  filter(Country == "India") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

mango_india<- bind_rows(mango_regen_india_table, mango_organic_india_table)
colnames(mango_india)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(mango_india)<- c("Regenerative ", "Organic")

mango_india_table<- kable(mango_india, caption = "Mangos: India") %>% 
  kable_styling(bootstrap_options = "striped")
mango_india_table


######
#Kernza
######
kernza_regen_min_table<- crops %>% 
  filter(Crop == "Kernza") %>% 
  filter(Country == "Minnesota") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

kernza_organic_min_table<- crops %>% 
  filter(Crop == "Kernza") %>% 
  filter(Country == "Minnesota") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

kernza_min<- bind_rows(kernza_regen_min_table, kernza_organic_min_table)
colnames(kernza_min)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(kernza_min)<- c("Regenerative ", "Organic")

kernza_min_table<- kable(kernza_min, caption = "Kernza: Minnesota") %>% 
  kable_styling(bootstrap_options = "striped")
kernza_min_table


kernza_regen_ks_table<- crops %>% 
  filter(Crop == "Kernza") %>% 
  filter(Country == "Kansas") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

kernza_organic_ks_table<- crops %>% 
  filter(Crop == "Kernza") %>% 
  filter(Country == "Kansas") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

kernza_ks<- bind_rows(kernza_regen_ks_table, kernza_organic_ks_table)
colnames(kernza_ks)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(kernza_ks)<- c("Regenerative ", "Organic")

kernza_ks_table<- kable(kernza_min, caption = "Kernza: Kansas") %>% 
  kable_styling(bootstrap_options = "striped")
kernza_ks_table


kernza_regen_scotland_table<- crops %>% 
  filter(Crop == "Kernza") %>% 
  filter(Country == "Scotland") %>%
  filter(Practice == "Regenerative") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

kernza_organic_scotland_table<- crops %>% 
  filter(Crop == "Kernza") %>% 
  filter(Country == "Scotland") %>%
  filter(Practice == "Organic") %>% 
  summarise(mean_regen_soc = mean(dSOC),
            mean_regen_gwp = mean(GWP))

kernza_scotland<- bind_rows(kernza_regen_scotland_table, kernza_organic_scotland_table)
colnames(kernza_scotland)<- c("Yearly Change kgSOC", "Average Net GHG kgCO2e")
rownames(kernza_scotland)<- c("Regenerative ", "Organic")

kernza_scotland_table<- kable(kernza_scotland, caption = "Kernza: Scotland") %>% 
  kable_styling(bootstrap_options = "striped")
kernza_scotland_table

############################################################################
kernza_regen_n2o<- sum(kernza_regen_minnesota$N2O_CO2e)
kernza_organic_n2o<- sum(kernza_organic_minnesota$N2O_CO2e)

kernza_regen_ch4<- sum(kernza_regen_minnesota$CH4_CO2e)
kernza_organic_ch4<- sum(kernza_organic_minnesota$CH4_CO2e)

kernza_regen_co2<- sum(kernza_regen_minnesota$CO2e)
kernza_organic_co2<- sum(kernza_organic_minnesota$CO2e)

########################################
mango_regen_n2o<- sum(mango_regen_nicaragua$N2O_CO2e)
mango_organic_n2o<- sum(mango_organic_nicaragua$N2O_CO2e)

mango_regen_ch4<- sum(mango_regen_nicaragua$CH4_CO2e)
mango_organic_ch4<- sum(mango_organic_nicaragua$CH4_CO2e)

mango_regen_co2<- sum(mango_regen_nicaragua$CO2e)
mango_organic_co2<- sum(mango_organic_nicaragua$CO2e)

######################################################
cotton_regen_n2o<- sum(cotton_regen_texas$N2O_CO2e)
cotton_organic_n2o<- sum(cotton_organic_texas$N2O_CO2e)

cotton_regen_ch4<- sum(cotton_regen_texas$CH4_CO2e)
cotton_oragnic_ch4<- sum(cotton_organic_texas$CH4_CO2e)

cotton_regen_co2<- sum(cotton_regen_texas$CO2e)
cotton_organic_co2<- sum(cotton_organic_texas$CO2e)


####################################################
bison_regen_n2o<- sum(bison_regen_sd$N2O_CO2e)
bison_organic_n2o<- sum(bison_organic_sd$N2O_CO2e)

bison_regen_ch4<- sum(bison_regen_sd$CH4_CO2e)
bison_organic_ch4<- sum(bison_organic_sd$CH4_CO2e)

bison_regen_co2<- sum(bison_regen_sd$CO2e)
bison_organic_co2<- sum(bison_organic_sd$CO2e)
##################################################



# make a data set of these averages

n2o_ch4_co2<- data.frame(Crop = c("Kernza", "Kernza", "Kernza", "Kernza","Kernza", "Kernza", "Mango", "Mango","Mango", "Mango","Mango", "Mango","Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton","Bison", "Bison", "Bison", "Bison", "Bison", "Bison"),
                         Practice = c("Regenerative", "Organic", "Regenerative", "Organic", "Regenerative", "Organic", "Regenerative", "Organic"),
                         Gas = c("N2O", "N2O", "CH4", "CH4","CO2","CO2", "N2O", "N2O", "CH4", "CH4","CO2","CO2","N2O", "N2O", "CH4", "CH4","CO2","CO2", "N2O", "N2O", "CH4", "CH4", "CO2","CO2"),
                         kg_co2e = c(kernza_regen_n2o, kernza_organic_n2o, kernza_regen_ch4, kernza_organic_ch4, kernza_regen_co2, kernza_organic_co2, mango_regen_n2o, mango_organic_n2o, mango_regen_ch4, mango_organic_ch4, mango_regen_co2, mango_organic_co2, cotton_regen_n2o, cotton_organic_n2o, cotton_regen_ch4, cotton_oragnic_ch4, cotton_regen_co2, cotton_organic_co2, bison_regen_n2o, bison_organic_n2o, bison_regen_ch4, bison_organic_ch4, bison_regen_co2, bison_organic_co2))

####################################
# Breakdown of GHG tab work
####################################
kernza<- crops %>% 
  filter(Crop == "Kernza")

cotton<- crops %>% 
  filter(Crop == "Cotton") %>% 
  filter(Practice == "Regenerativeerative" | 
           Practice == "Organic")

mango<- crops %>% 
  filter(Crop == "Mango")

grazing<- crops %>% 
  filter(Crop == "Bison")

crops_filter<- bind_rows(kernza, cotton, mango, grazing)

#####################################



ui<- dashboardPage(skin = "black",
  dashboardHeader(title = "Carbon for Crops"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regeneratives Globally", tabName = "map"),
      menuItem("Soil Organic Carbon & GWP", tabName = "overview"),
      menuItem("Practices", tabName = "sensativity"),
      menuItem("Sources of GHG's", tabName = "ghg")
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
            fluidRow(leafletOutput(outputId = "map_1", height = 1000))),
      tabItem(
        tabName = "overview",
            fluidRow(
              box(),
              box())),
      tabItem(tabName = "sensativity",
              fluidRow(
                box(title = "Sources of GHGs"),
                box())),
      tabItem(
        tabName = "ghg",
        fluidRow(
          box(title = "Breakdown of GHG Emissions",
              selectInput("ghg_crops",
                          "Choose a Crop",
                          choices = c(unique(crops$Crop))),
              selectInput("ghg_practice",
                          "Choose a Practice",
                          choices = c("Regenerative", "Organic")),
              selectInput("ghg_location",
                          "Choose a Location",
                          choices = c(unique(crops$Country)))),
          box())))))
  
    
  

  

  
  

server<- function(input, output){
  #####################################################################
  # Widget #1: Map
  #####################################################################
  
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
    "Kernza", -98.4842, 39.0119,
    "Kernza", -4.5919, 55.6765
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
                                 addCircleMarkers(lng = -103.2310, 
                                                  lat = 44.0805, 
                                                  popup = grazing_sd_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "blue", 
                                                  fillOpacity = 0.3) %>%
                                 addCircleMarkers(lng = -56.9211, 
                                                  lat = -12.6819, 
                                                  popup = grazing_bz_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "blue", 
                                                  fillOpacity = 0.3) %>%
                                 addCircleMarkers(lng = -85.2072, 
                                                  lat = 12.8654, 
                                                  popup = mango_nic_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "green", 
                                                  fillOpacity = 0.3) %>%
                                 addCircleMarkers(lng = 72.8777, 
                                                  lat = 19.0760, 
                                                  popup = mango_india_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "green", 
                                                  fillOpacity = 0.3) %>%
                                 addCircleMarkers(lng = -94.6859, 
                                                  lat = 46.7296, 
                                                  popup = kernza_min_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "orange", 
                                                  fillOpacity = 0.3) %>%
                                 addCircleMarkers(lng = -98.4842, 
                                                  lat = 39.0119, 
                                                  popup = kernza_ks_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "orange", 
                                                  fillOpacity = 0.3) %>%
                                 addCircleMarkers(lng = -4.5919, 
                                                  lat = 55.6765, 
                                                  popup = kernza_scotland_table, 
                                                  stroke = FALSE, 
                                                  fillColor = "orange", 
                                                  fillOpacity = 0.3) %>%
                                 addLegend(position = "topright", colors = c("red", "blue", "green", "orange"), labels = c("Cotton", "Grazing", "Mango", "Kernza")) %>% 
                                 setView(40, 6, 2))  
  #####################################################################
  #####################################################################
  
  #########################
  # Widget 2
  #########################
  
  
  
  
  
  #########################
  # Input for widget #4 GHG breakdown
  ##########################
  
 
  }
  


shinyApp(ui = ui, server = server)


