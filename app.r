library(tidyverse)
library(kableExtra)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(sf)
library(leaflet)
library(shinyWidgets)


#######
# Working Space
#######
crops<- read_csv("Total_Crops.csv")




cotton_regen_texas <- crops %>% 
  filter(Country == "Texas",
         Practice == "Regenerative",
         Crop == "Cotton") 

cotton_organic_texas <- crops%>% 
  filter(Country == "Texas",
         Practice == "Organic",
         Crop == "Cotton") 


cotton_regen_india <- crops %>% 
  filter(Country == "India",
         Practice == "Regenerative",
         Crop == "Cotton") 

cotton_organic_india <- crops %>% 
  filter(Country == "India",
         Practice == "Organic",
         Crop == "Cotton") 


cotton_regen_peru <- crops %>% 
  filter(Country == "Peru",
         Practice == "Regenerative",
         Crop == "Cotton") 

cotton_organic_peru <- crops %>% 
  filter(Country == "Peru",
         Practice == "Organic",
         Crop == "Cotton") 

cotton_regen_china <- crops%>% 
  filter(Country == "China",
         Practice == "Regenerative",
         Crop == "Cotton") 

cotton_organic_china <- crops %>% 
  filter(Country == "China",
         Practice == "Organic",
         Crop == "Cotton") 

kernza_regen_minnesota <- crops %>% 
  filter(Country == "Minnesota",
         Practice == "Regenerative",
         Crop == "Kernza") 

kernza_organic_minnesota <- crops %>% 
  filter(Country == "Minnesota",
         Practice == "Organic",
         Crop == "Kernza") 


kernza_regen_scotland <- crops %>% 
  filter(Country == "Scotland",
         Practice == "Regenerative",
         Crop == "Kernza") 

kernza_organic_scotland <- crops %>% 
  filter(Country == "Scotland",
         Practice == "Organic",
         Crop == "Kernza") 

kernza_regen_kansas <- crops %>% 
  filter(Country == "Kansas",
         Practice == "Regenerative",
         Crop == "Kernza") 

kernza_organic_kansas <- crops %>% 
  filter(Country == "Kansas",
         Practice == "Organic",
         Crop == "Kernza") 

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
         Crop == "Bison") 

bison_organic_sd <- crops%>% 
  filter(Country == "South Dakota",
         Practice == "Organic",
         Crop == "Bison") 

bison_regen_bz <- crops %>% 
  filter(Country == "Brazil",
         Practice == "Regenerative",
         Crop == "Bison") 

bison_organic_bz <- crops %>% 
  filter(Country == "Brazil",
         Practice == "Organic",
         Crop == "Bison") 
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

kernza_ks_table<- kable(kernza_ks, caption = "Kernza: Kansas") %>% 
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

###############################
# Widget 3

practices_yearly <- crops %>% 
  filter(Practice == "Monocrop" |
           Practice == "Twocrops" | 
           Practice == "Threecrops" |
           Practice == "Fourcrops") %>% 
  mutate(Practice = case_when(
    Practice == "Monocrop" ~ 1,
    Practice == "Twocrops" ~ 2,
    Practice == "Threecrops" ~ 3,
    Practice == "Fourcrops" ~ 4)) %>% 
  select(Year, Crop, Country, Practice, dSOC, GWP) %>% 
  arrange(Practice)


practices_sum <- practices_yearly %>%  
  group_by(Crop, Country, Practice) %>% 
  summarise(sum_dSOC = sum(dSOC),
            sum_GWP = sum(GWP))
  



###############################

ghg_break_down<- crops %>% 
  group_by(Crop, Country, Practice) %>% 
  summarise(CO2 = mean(CO2e),
            CH4 = mean(CH4_CO2e),
            N2O = mean(N2O_CO2e),
            GWP = mean(GWP)) %>% 
  filter(Practice == "Organic" |
           Practice == "Regenerative") %>% 
  gather("Gas", "kgCO2e", 4:6)

ghg_total<- crops %>%
  group_by(Crop, Practice, Country) %>% 
  summarise(GHG = mean(GWP)) %>% 
  filter(Practice == "Organic" |
           Practice == "Regenerative")
ghg_total

crop_breakdown = crops %>% 
  group_by(Crop, Country, Practice) %>% 
  summarise(mean_soc = mean(dSOC),
            mean_ghg = mean(GWP)) %>% 
  filter(Practice == "Organic" |
           Practice == "Regenerative")

overview_total<- crops %>%
  group_by(Crop, Practice, Country) %>% 
  summarise(SOC = mean(dSOC),
            GHG = mean(GWP)) %>% 
  filter(Practice == "Organic" |
           Practice == "Regenerative")

##################################

ui<- dashboardPage(skin = "black",
  dashboardHeader(title = "Carbon for Crops"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Regeneratives Globally", tabName = "map"),
      menuItem("Soil Organic Carbon & GWP", tabName = "overview"),
      menuItem("Practices", tabName = "sensativity"),
      menuItem("Sources of GHG's", tabName = "ghg")
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
          fluidPage(
            setBackgroundImage(src = "land.jpeg", shinydashboard = TRUE),
            titlePanel(strong("Assessing the Soil & Climate Impacts of Regenerative Agriculture")),
            br(),
            box(h3(em("A study conducted by graduate students of the Bren School in collaboration with Patagonia, Inc.", align = "center")), height = 70, width = 100),
            br(),
            img(src = "bren.png", height = 58, width = 171),
            img(src = "patagonia.png", height = 58, width = 140)
            
          )
      ),
      tabItem(
        tabName = "map",
            fluidRow(leafletOutput(outputId = "map_1", height = 1000))),
      tabItem(
        tabName = "overview",
            fluidRow(
              box(title = "Summary of Crop SOC and GHG Emissions",
                  selectInput("overview_crops",
                              "Choose a Crop",
                              choices = c(unique(crops$Crop))),
                  selectInput("overview_location",
                              "Choose a Location",
                              choices = c(unique(crops$Country)))),
              box(plotOutput(outputId = "overview_plot1")),
              box(tableOutput(outputId = "overview_table")),
              box(plotOutput(outputId = "overview_plot2")))),
      tabItem( #####
        tabName = "sensativity",
        fluidRow(
          box(title = "Impact of cropping methods on GHG emissions",
              selectInput("practices_crops",
                          "Choose a target crop",
                          choices = c(unique(practices_yearly$Crop))),
              selectInput("practices_location",
                          "Choose a Location",
                          choices = c(unique(practices_yearly$Country))),
              sliderTextInput("practices_number", 
                          label = "Choose the number of crops grown on a single plot:", 
                          choices = c(unique(practices_yearly$Practice)),
                          grid = TRUE,
                          hide_min_max = TRUE)),
          box(plotOutput(outputId = "practice_sum_dSOC")),
          box(plotOutput(outputId = "practice_sum_GWP")),
          box(plotOutput(outputId = "practice_dSOC")),
          box(plotOutput(outputId = "practice_GWP")))),
      
      tabItem(
        tabName = "ghg",
        fluidRow(
          box(title = "Sources of GHG Emissions",
              selectInput("ghg_crops",
                          "Choose a Crop",
                          choices = c(unique(crops$Crop))),
              radioButtons("ghg_practice",
                          "Choose a Practice",
                          choices = c("Regenerative", "Organic")),
              selectInput("ghg_location",
                          "Choose a Location",
                          choices = c(crops$Country))),
          box(plotOutput(outputId = "ghg_plot")),
          box(tableOutput(outputId = "ghg_table")))))))
  
    
  

  

  
  

server<- function(input, output, session){
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
  
  crop_overview = reactive({
    crop_breakdown %>% 
      filter(Crop == input$overview_crops) %>% 
      filter(Country == input$overview_location) 
  })
  
  output$overview_plot1 = renderPlot({
    ggplot(data = crop_overview(), aes(x = Practice, y = mean_soc))+
      geom_col(aes(fill = Practice), width = 0.5)+
      geom_hline(yintercept = 0)+
      scale_fill_manual(values = c("deepskyblue4", "deepskyblue"))+
      labs(x = "Practice", y = "Average SOC change (kgSOC/ha)", title = "Comparison of SOC change per hectare between Regenerative and Organic")+
      theme_classic()
  })
  
  output$overview_table<- function(){
    req(input$overview_crops)
    req(input$overview_location)
    
    overview_total %>% 
      filter(Crop == input$overview_crops) %>% 
      filter(Country == input$overview_location) %>%
      filter(Practice == "Regenerative" |
               Practice == "Organic") %>% 
      kable("html", col.names = c("Crop", "Practice", "Location", "Average SOC Change (kgSOC/ha)", "Average GHG Emissions (kg CO2e)")) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
  }
  
  output$overview_plot2 = renderPlot({
    ggplot(data = crop_overview(), aes(x = Practice, y = mean_ghg))+
      geom_col(aes(fill = Practice), width = 0.5)+
      geom_hline(yintercept = 0)+
      scale_fill_manual(values = c("darkorange4", "darkorange"))+
      labs(x = "Practice", y = "Average GHG Emissions (kgCO2e/ha)", title = "Comparison of Greenhouse Gas Emissions between Regenerative and Organic Practices")+
      theme_classic()
  })
  
observe({
  updateSelectInput(session,
                    "overview_location",
                    choices = crops %>% 
                      filter(Crop == input$overview_crops) %>% 
                      select(Country) %>%
                      unique() 
    )
  })  
  
  #########################
  # Widget 3
  #########################

  practice_df <- reactive({
    practices_yearly %>% 
      filter(Crop == input$practices_crops) %>% 
      filter(Country == input$practices_location) %>% 
      filter(Practice == input$practices_number)
  })

cols <- reactive({
  cols <- c("1" = "grey", "2" =  "grey", "3" =  "grey",
            "4" =  "grey")
  cols[input$practices_number] <- "deepskyblue4"
  return(cols)
})

cols2 <- reactive({
  cols2 <- c("1" = "grey", "2" =  "grey", "3" =  "grey",
            "4" =  "grey")
  cols2[input$practices_number] <- "darkolivegreen3"
  return(cols2)
})

output$practice_sum_dSOC <- renderPlot({
  ggplot(data = practices_sum, aes(x = Practice, y = sum_dSOC, color = factor(Practice), fill = factor(Practice))) +
    geom_col(stat = "identity", position = "dodge", show.legend = "False", width = 0.5)+
    scale_colour_manual(values = cols(), aesthetics = c("colour", "fill"))+
    labs(title = "Total dSOC by number of crops", x = "Number of crops", y = "Total dSOC")+
    theme_minimal()
})

output$practice_sum_GWP <- renderPlot({
  ggplot(data = practices_sum, aes(x = Practice, y = sum_GWP, color = factor(Practice), fill = factor(Practice))) +
    geom_col(stat = "identity", position = "dodge", show.legend = "False", width = 0.5)+
    scale_colour_manual(values = cols2(), aesthetics = c("colour", "fill"))+
    labs(title = "Total GWP by number of crops", x = "Number of crops", y = "Total GWP")+
    theme_minimal()
})


  output$practice_dSOC <- renderPlot({
    ggplot(data = practice_df(), aes(x = Year, y = dSOC)) +
      geom_col(stat = "identity", position = "dodge", show.legend = "False", width = 0.5, fill =
                 "deepskyblue4")+
      labs(title = "dSOC per year", x = "Year", y = "Total dSOC")+
      theme_minimal()
  })
  
  output$practice_GWP <- renderPlot({
    ggplot(data = practice_df(), aes(x = Year, y = GWP)) +
      geom_col(stat = "identity", position = "dodge", show.legend = "False", width = 0.5, fill =
                 "darkolivegreen3")+
      labs(title = "GWP per year", x = "Year", y = "Total GWP")+
      theme_minimal()
  })



  #########################
  # Input for widget #4 GHG breakdown
  ##########################
  
  ghg_df<- reactive({
    ghg_break_down %>% 
      filter(Crop == input$ghg_crops) %>% 
      filter(Practice == input$ghg_practice) %>% 
      filter(Country == input$ghg_location)
  })
  
output$ghg_plot<- renderPlot({
  ggplot(data = ghg_df(), aes(x = Gas, y = kgCO2e, fill = Gas))+
    geom_bar(stat = "identity", position = "dodge", show.legend = "False", width = 0.5)+
    scale_fill_manual(values = c("darkolivegreen", "darkolivegreen3", "darkolivegreen1"))+
    labs(title = "Average Yearly GHG Emissions", x = "Emissions from Each Gas")+
    theme_minimal()
})


output$ghg_table<- function(){
  req(input$ghg_crops)
  req(input$ghg_practice)
  req(input$ghg_location)
  
  ghg_total %>% 
    filter(Crop == input$ghg_crops) %>% 
    filter(Country == input$ghg_location) %>% 
    kable("html", col.names = c("Crop", "Practice", "Location", "Yearly GHG Emissions (kgCO2e)")) %>% 
    kable_styling(bootstrap_options = c("striped", "hover"))
}
  



observe({
  updateSelectInput(session,
                    "ghg_location",
                    choices = crops %>% 
                      filter(Crop == input$ghg_crops) %>% 
                      select(Country) %>%
                      unique() 
                      )
})


  }
  


shinyApp(ui = ui, server = server)


