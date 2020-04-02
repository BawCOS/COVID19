#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 
#import
# pacman::p_load(dplyr,
#                ggplot2,
#                tidyverse,
#                tidyr,
#                shinydashboard,
#                shiny,
#                geosphere,
#                scales,
#                googleVis,
#                reshape2,
#                usmap,
#                data.table,
#                plyr,
#                choroplethr,
#                choroplethrMaps,
#                DT
# )

library(dplyr)
library(ggplot2)
#library(tidyverse)
library(tidyr)
library(shinydashboard)
library(shiny)
library(geosphere)
library(scales)
library(googleVis)
#library(reshape2)
library(usmap)
library(data.table)
library(plyr)
#library(choroplethr)
#library(choroplethrMaps)
library(DT)



#Define Variables and load in data up front if necessary
CovidConfirmedCases <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
AFBaseLocations <- read.csv("https://gitlab.com/messer06/covid/-/raw/master/AFB_Locs.csv?inline=false")
CountyInfo <- read.csv("https://gitlab.com/messer06/covid/-/raw/master/County_Info.csv?inline=false")
HospitalInfo <- read.csv("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
CovidDeaths<-read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
colnames(CovidDeaths)[1]<-"CountyFIPS"
#Minus<-ncol(CovidDeaths)
#CovidDeaths<-CovidDeaths[,-(Minus)]
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)

#Read in IHME data
temp <- tempfile()
download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", temp, mode="wb")
filename = paste(format(as.Date(Sys.Date()-1), "%Y"), "_",
                 format(as.Date(Sys.Date()-1), "%m"), "_",
                 format(as.Date(Sys.Date()-1), "%d"), ".1",
                 "/Hospitalization_all_locs.csv",
                 sep = "")
unzip(temp, files = filename)
IHME_Model <- read.csv(filename)
unlink(temp)

IHME_Model$date <- as.Date(IHME_Model$date, format = "%Y-%m-%d")
StateList <- data.frame(state.name, state.abb)
IHME_Model <- merge(IHME_Model, StateList, by.x = "location", by.y = names(StateList)[1])
names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"


# #Create list of hospitals, bases, and counties.
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County

#Create National Data table on summary page
NationalDataTable<-CovidConfirmedCases
NationalDataTable$State<-as.factor(NationalDataTable$State)
NationalDataTable<-NationalDataTable[,-c(1,2,4)]
NationalDataTable<-aggregate(.~State, NationalDataTable, sum)
RateofCovidChange<-rev(NationalDataTable)[c(1:7)]
RateofCovidChange<-ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable<-CovidDeaths
NationalDeathTable$State<-as.factor(NationalDeathTable$State)
NationalDeathTable<-NationalDeathTable[,-c(1,2,4)]
NationalDeathTable<-aggregate(.~State, NationalDeathTable, sum)
RateofDeathChange<-rev(NationalDeathTable)[c(1:7)]
RateofDeathChange<-ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)

NationalDataTable<-data.frame(NationalDataTable$State, NationalDataTable[,length(NationalDataTable)],RateofCovidChange, NationalDeathTable[,length(NationalDeathTable)], RateofDeathChange)
colnames(NationalDataTable)<-c("State","Total Cases","Average New Cases Per Day", "Total Deaths","Average New Deaths Per Day")
NationalDataTable$`Cases Per 1000 People`<-c(731449,4822023,2949131,6553255,38041430,5187582,3590347,633427,917092,19317568,9919945,1392313,3074186,1595728,12875255,6537334,2885905,4380415,4601893,6646144,5884563,1329192,9883360,5379139,6021988,2984926,1005141,9752073,699628,1855525,1320718,8864590,2085538,2758931,19570261,11544225,3814820,3899353,12763536,1050292,4723723,833354,6456243,26059203,2855287,8185867,626011,6897012,5726398,1855413,576412)
NationalDataTable$`Cases Per 1000 People`<-round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 1000 People`/1000))


#Build UI
ui <- tagList(
  dashboardPage(skin = "black",
    dashboardHeader(title = "COVID-19 Risk Dashboard",
                    titleWidth = 300,
                    dropdownMenu(
                      headerText = "Want to know more?",
                      icon = icon("info-circle"),
                      tags$li(actionLink("inputInfo", label = "User Inputs", icon = icon("sliders-h")),
                              class = "dropdown"),
                      tags$li(actionLink("calcInfo", label = "Calculations", icon = icon("calculator")),
                              class = "dropdown"),
                      tags$li(actionLink("sourceInfo", label = "Sources", icon = icon("user-secret")),
                              class = "dropdown")
                    )
    ),
    dashboardSidebar(width = 300, 
                     sidebarMenu(
                       selectInput(
                         "Base",
                         "Choose your base:", 
                         list(`Installation` = sort(BaseList) ), 
                         selectize = FALSE),
                       sliderInput("Radius",
                                   "Choose your local radius (miles):",
                                   min = 10,
                                   max = 100,
                                   value = 25),
                       br(),
                       menuItem(
                         "Extra Inputs",
                         tabName = "dashboard",
                         icon = icon("sliders-h"),
                         div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                         sliderInput("proj_days",
                                     "Projection days:",
                                     min = 7,
                                     max = 90,
                                     value = 14),
                         sliderInput("social_dist",
                                     "% Social distancing in your area:",
                                     min = 0,
                                     max = 100,
                                     value = 60)
                       ),
                       br(),
                       actionButton("refresh", "Refresh", width = "90%"),
                       hr(),
                       fluidRow(
                         valueBox("LOW RISK", subtitle ="Mission Risk",color= "green",width = 12)
                       ),
                       fluidRow(
                         valueBox("MEDIUM RISK", subtitle ="Installation Health Risk",color= "yellow", width = 12)
                       ),
                       fluidRow(
                         valueBox("HIGH RISK", subtitle ="Local Health Risk",color= "red",width = 12)
                       )
                     )
                     
    ),
    
    dashboardBody(
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
        }
        '))),
      tags$script(HTML('
                       $(document).ready(function() {
                       $("header").find("nav").append(\'<span class="myClass"> Tailored Risk Assesments </span>\');
                       })
                       ')),
      
      tabsetPanel(id = "tabs",
                  ####### START SUMMARY TAB #######
                  tabPanel(
                    title = "Summary",
                    fluidRow( 
                      box(title = "National Impact Map",solidHeader = T, align = "center", htmlOutput("SummaryPlot"))
                    ),
                    fluidRow( 
                      box(title = "National Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("NationalDataTable1"), style = "height:240px;overflow-y: scroll;overflow-x:scroll"))
                    )
                  ),
                  ####### END SUMMARY TAB #######
                  
                  ####### START CURRENT LOCAL HEALTH  TAB #######
                  tabPanel(
                    title = "Current Local Health",
                    fluidRow(
                      # A static valueBox
                      valueBoxOutput("CovidCases"),
                      valueBoxOutput("LocalCovidDeaths"),
                      valueBoxOutput("HospitalUtilization")
                    ),
                    fluidRow(
                      valueBoxOutput("CaseChangeLocal", width = 4),
                      valueBoxOutput("DeathChangeLocal", width = 4),
                      valueBoxOutput("HospUtlzChange", width = 4)
                    ),
                    fluidRow( 
                      box(title = "Daily Reports",plotOutput("LocalHealthPlot1",height = 300)),
                      box(title = "Total Reports",plotOutput("LocalHealthPlot2",height = 300))
                    ),
                    fluidRow(
                      box(title = "Local Impact Map", plotOutput("CountySummary", height = 250),height = 300),
                      box(title = "Local County Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("CountyDataTable1"), style = "height:240px;overflow-y: scroll"), height = 300)
                    )
                  ),
                  ####### END CURRENT LOCAL HEALTH TAB #######
                  
                  ####### START PROJECTIONS TAB #######
                  tabPanel(
                    title = "Local Health Projections",
                    fluidRow(
                      valueBoxOutput("TotalPopulation")
                    ),
                    fluidRow(
                      box(plotOutput("IHME_State_Hosp",height = 400)),
                      box("insert CHIME projections here",height = 400))
                  ),
                  ####### END PROJECTION TAB #######
      
                  ####### START INSTALLATION HEALTH RISK TAB #######
                  tabPanel(
                    title = "Installation Health", 
                    fluidRow(
                      valueBox(2, subtitle ="Installation Specific Deaths", color= "red",icon = icon("skull")),
                      valueBox("85%", subtitle = "Installation Medical Utilization", color = "teal", icon = icon("hospital"))
                    ),
                    box(status = "primary", width = 13, solidHeader = T, "Current Risk Level: LOW ", align = "center"),
                    fluidRow( 
                      box(title = "Chart 1 Here", "Box content"),
                      box(title = "Chart 2 Here", "Box content")
                    )
                  )
                  ####### END INSTALLATION HEALTH RISK TAB #######
          )
        )
      ),

tags$footer("created by the 4 AFITeers", align = "center", style = "
            position:absolute;
            bottom:50;
            width:100%;
            height:25px;   /* Height of the footer */
            color: grey;
            padding: 0px;
            background-color: transparent;
            z-index: 1000;")
)
#Close UI  
###############################


CalculateCounties<-function(ChosenBase, Radius, IncludedCounties){
  #Finds which counties in given radius. Also Give county statistics
  TotalPopulation <-  sum(IncludedCounties$Population)
  TotalPopulation
}

# Finds Covid Cases and statistics on covid per county
CalculateCovid<-function(ChosenBase, Radius, IncludedCounties){
  #Finds which counties in given radius. Also Give county statistics
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  sum(CovidCounties[,ncol(CovidCounties)])
}

CalculateDeaths<-function(ChosenBase, Radius, IncludedCounties){
  #Finds which counties in given radius. Also Give county statistics
  CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  sum(CovidCountiesDeath[,ncol(CovidCountiesDeath)])
}

HospitalIncreases<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
  #Finds number of hospitals in radius
  TotalBeds<-sum(IncludedHospitals$BEDS)
  #Finds which counties in given radius. Also Give county statistics
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  
  x <- length(CovidCounties)
  changeC <- sum(CovidCounties[x] - CovidCounties[x-1])
  
  n<-ncol(CovidCounties)-6
  TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
  NotHospital<-sum(CovidCounties[,n])
  StillHospital<-ceiling((TotalHospital-NotHospital))
  Upper<- round(((StillHospital+changeC*.1)/TotalBeds+.6)*100,1)
  #Lower<- round(((StillHospital+changeC*.207)/TotalBeds+.55)*100,1)
  paste(Upper," %", sep = "") 
}

#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesPerDayChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
  #Finds number of hospitals in radius
  TotalBeds<-sum(IncludedHospitals$BEDS)
  
  #Finds which counties in given radius. Also Give county statistics
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  VectDailyCovid<-colSums(CovidCounties[29:length(CovidCounties)])
  DailyNewCases<-VectDailyCovid[2:length(VectDailyCovid)]-VectDailyCovid[1:(length(VectDailyCovid)-1)]
  DailyNewCases
  DailyNewHospitalizations<-ceiling(DailyNewCases*.26)
  DailyNewHospitalizations
  
  
  #Find New Deaths
  CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  VectDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
  DailyNewDeaths<-VectDailyDeaths[2:length(VectDailyDeaths)]-VectDailyDeaths[1:(length(VectDailyDeaths)-1)]
  
  
  ForecastDate<- seq(as.Date("2020-02-15"), length=(length(DailyNewDeaths)), by="1 day")
  Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCases,DailyNewHospitalizations,DailyNewDeaths)
  colnames(Chart1Data)<-c("ForecastDate","New Cases","New Hospitalizations","New Fatalities")
  Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
  
  #Plot the forecasts from above but include the actual values from the test data to compare accuracy.
  #plot for local area daily
  ggplot(Chart1DataSub) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
    scale_colour_manual(values=c("Blue", "Orange", "Red"))+
    xlab('Date') +
    ylab('Number of People') +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5))+
    labs(color='')+
    theme_bw()+
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) +
    theme(axis.line = element_line(color = "black"))+
    theme(legend.position = "top")
}

#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
  #Finds number of hospitals in radius
  TotalBeds<-sum(IncludedHospitals$BEDS)
  
  #Finds which counties in given radius. Also Give county statistics
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  CumDailyCovid<-colSums(CovidCounties[29:length(CovidCounties)])
  CumHospitalizations<-ceiling(CumDailyCovid*.26)
  
  
  
  #Find New Deaths
  CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  CumDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
  
  
  
  ForecastDate<- seq(as.Date("2020-02-15"), length=(length(CumDailyDeaths)), by="1 day")
  Chart2Data<-cbind.data.frame(ForecastDate,CumDailyCovid,CumHospitalizations,CumDailyDeaths)
  colnames(Chart2Data)<-c("ForecastDate","Total Cases","Total Hospitalizations","Total Fatalities")
  Chart2DataSub <- melt(data.table(Chart2Data), id=c("ForecastDate"))
  
  #Plot the forecasts from above but include the actual values from the test data to compare accuracy.
  #plot for local area cumulative
  ggplot(Chart2DataSub,height = 250) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
    scale_colour_manual(values=c("Blue", "Orange", "Red"))+
    xlab('Date') +
    ylab('Number of People') +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5))+
    labs(color='')+
    theme_bw()+
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
    ) +
    theme(axis.line = element_line(color = "black"))+
    theme(legend.position = "top")
}


#Create Data Table for local statistics
GetLocalDataTable<-function(IncludedCounties){
  CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
  DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
  CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1])
  CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,2], rev(CountyDataTable)[,1])
  colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities" )
  CountyDataTable
}

# CovidCountyChoropleth<-function(ChosenBase, Radius){
# 
#     #Finds which counties in given radius. Also Give county statistics
#     BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#     for (i in 1:3143) {
#         CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
#     }
#     IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
#     CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
#     
#     
# 
#     
# 
#     
#     
#     
#     
# }



##########################################
# Define server logic, this is where all plots are generated. 
server <- function(input, output) {
  
  
  GetCounties<-reactive({
    BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
    for (i in 1:3143) {
      CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= input$Radius)
    IncludedCounties
  })
  
  GetHospitals<-reactive({
    #Finds number of hospitals in radius
    BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
    for (i in 1:7581) {
      HospitalInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(HospitalInfo$LONGITUDE[i], HospitalInfo$LATITUDE[i]), fun = distHaversine)/1609.34)
    }
    IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= input$Radius))
    IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
    IncludedHospitals
  })
  
  #Finds which counties in given radius. Also Give county statistics
  output$TotalPopulation <- renderValueBox({
    MyCounties<-GetCounties()
    valueBox(subtitle = "Total Regional Population",
             comma(CalculateCounties(input$Base,input$Radius, MyCounties)),
             icon = icon("list-ol")
    )
    
  })
  
  # Finds Covid Cases and statistics on covid per county
  output$CovidCases <- renderValueBox({
    MyCounties<-GetCounties()
    valueBox(subtitle = "Local Cases",
             comma(CalculateCovid(input$Base,input$Radius,MyCounties)),
             icon = icon("list-ol"),
             color = "light-blue"
    )
    
  })
  
  # Finds Covid Cases and statistics on covid per county
  output$LocalCovidDeaths <- renderValueBox({
    MyCounties<-GetCounties()
    valueBox(subtitle = "Local Fatalities",
             comma(CalculateDeaths(input$Base, input$Radius, MyCounties)),
             icon = icon("skull"),
             color = "blue"
    )
  })
  
  #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
  output$HospitalUtilization <- renderValueBox({
    MyCounties<-GetCounties()
    MyHospitals<-GetHospitals()
    valueBox(subtitle = "Local Hospital Utilization",
             HospitalIncreases(input$Base,input$Radius, MyCounties, MyHospitals),
             icon = icon("hospital"),
             color = "navy")
  })
  
  #Create first plot of local health population 
  output$LocalHealthPlot1<-renderPlot({
    MyCounties<-GetCounties()
    MyHospitals<-GetHospitals()
    CovidCasesPerDayChart(input$Base, input$Radius, MyCounties,MyHospitals)
  })
  
  #Create second plot of local health population 
  output$LocalHealthPlot2<-renderPlot({
    MyCounties<-GetCounties()
    MyHospitals<-GetHospitals()
    CovidCasesCumChart(input$Base, input$Radius, MyCounties,MyHospitals)
  })
  
  #Create Country Plot on Summary page
  output$SummaryPlot<-renderGvis({
    DF<-cbind.data.frame(CovidConfirmedCases$State, CovidConfirmedCases[,length(CovidConfirmedCases)])
    colnames(DF)<-c("state","Value")
    ChlorData<-plyr::ddply(DF, "state", numcolwise(sum))
    
    ChlorData<-ChlorData %>% 
      mutate(state_name = state.name[match(state, state.abb)])
    ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
    states <- data.frame(ChlorData$state_name, ChlorData$Value)
    colnames(states)<-c("state_name","COVID-19 Cases")
    
    gvisGeoChart(states, "state_name", "COVID-19 Cases", 
                 options=list(region="US",
                              #colorAxis="{colors:'grey', 'red']}",
                              displayMode="regions", 
                              resolution="provinces",
                              width=600,
                              height = 400))
    
  })
  
  #Create County Plot on Summary page
  output$CountySummary<-renderPlot({
    CovidConfirmedCases <- CovidConfirmedCases[!duplicated(CovidConfirmedCases[1]),]
    countyMap_choro <- merge(CountyInfo,CovidConfirmedCases,by.x = "FIPS",by.y = names(CovidConfirmedCases)[1])
    countyMap_choro$Total <- rowSums(countyMap_choro[,18:length(countyMap_choro)])
    countyTotals <- countyMap_choro[,c("FIPS","Total")]
    names(countyTotals)[1] <- "region"
    names(countyTotals)[2] <- "value"
    countyTotals[2413,1] <- 46113
    MyCounties<-GetCounties()
    nearby_counties <- MyCounties$FIPS
    county_choropleth(countyTotals, num_colors = 1, county_zoom = nearby_counties)
  })
  
  #Create IHME plot by State projected hospitalization 
  output$IHME_State_Hosp<-renderPlot({
    BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
    
    IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
    
    
    ggplot(data=IHME_State, aes(x=date, y=allbed_mean, ymin=allbed_lower, ymax=allbed_upper)) +
      geom_line(linetype = "dashed", size = 1) +
      geom_ribbon(alpha=0.3, fill = "tan3") + 
      labs(title = paste("IHME Hospitalization Projections for ",toString(BaseState$State[1]), " Region", sep = ""), 
           x = "Date", 
           y = "Projected Daily Hospitalizations") +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 18, family = "sans"),
            axis.title = element_text(face = "bold",size = 11,family = "sans"),
            axis.text.x = element_text(angle = 60, hjust = 1), 
            axis.line = element_line(color = "black"),
            legend.position = "top",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      scale_x_date(date_breaks = "2 week")
  })
  
  
  output$CaseChangeLocal <- renderValueBox({
    MyCounties<-GetCounties()
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
    x <- length(CovidCounties)
    changeC <- sum(CovidCounties[x] - CovidCounties[x-1])
    
    valueBox(paste("+",toString(changeC)),
             subtitle = "New Confirmed Cases", 
             color = "light-blue")
  })
  
  output$DeathChangeLocal <- renderValueBox({
    MyCounties<-GetCounties()
    CovidCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
    x <- length(CovidCounties)
    changeC <- sum(CovidCounties[x] - CovidCounties[x-1])
    
    valueBox(paste("+",toString(changeC)),
             subtitle = "New Confirmed Fatalities", 
             color = "blue")
  })
  
  output$HospUtlzChange <- renderValueBox({
    MyCounties<-GetCounties()
    MyHospitals<-GetHospitals()
    TotalBeds<-sum(MyHospitals$BEDS)
    
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
    n <- ncol(CovidCounties)-6
    
    x <- length(CovidCounties)
    changeC <- sum(CovidCounties[x] - CovidCounties[x-1])
    changey <- sum(CovidCounties[x-1] - CovidCounties[x-2])
    
    # Today
    TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
    NotHospital<-sum(CovidCounties[,n])
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Upper<-(signif(((StillHospital+changeC*.1)/TotalBeds+.6)*100,3))
    #Lower<-(signif(((StillHospital+changeC*.207)/TotalBeds+.6)*100,3))
    
    # Yesterday
    TotalHospitaly<-sum(CovidCounties[,ncol(CovidCounties)-1])
    NotHospitaly<-sum(CovidCounties[,n-1])
    StillHospitaly<-ceiling((TotalHospitaly-NotHospitaly))
    Uppery<-(signif(((StillHospitaly+changey*.1)/TotalBeds+.6)*100,3))
    #Lowery<-(signif(((StillHospitaly+changey*.207)/TotalBeds+.6)*100,3))
    
    chng <- round((Upper-Uppery)/2, 1)
    
    if (chng < 0) {
      sign <- ""
    } else {
      sign <- "+"
    }
    
    valueBox(paste(sign,toString(chng),"%"),
             subtitle = "Hospital Utilization Change", 
             color = "navy")
  })
  
  
  #Render National Data Table on summary page
  output$NationalDataTable1<-DT::renderDataTable({
    NationalDataTable <- DT::datatable(data.frame(NationalDataTable),rownames = FALSE, options = list(dom = 't',ordering = F))
    NationalDataTable
    
  })
  
  output$CountyDataTable1<-DT::renderDataTable({
    MyCounties<-GetCounties()
    dt<-GetLocalDataTable(MyCounties)
    dt<-DT::datatable(dt, rownames = FALSE, options = list(dom = 't',ordering = F))
    dt
  })
  
  
  # 
  # 
  # 
  # 
  # 
  #
  
  
  observeEvent(input$inputInfo, {
    showModal(
      modalDialog(
        size = "l",fade = TRUE, easyClose = TRUE, title = "USER INPUTS",
        p("Some information"))
    )
  })
  observeEvent(input$calcInfo, {
    showModal(
      modalDialog(
        size = "l",fade = TRUE, easyClose = TRUE, title = "CALCULATIONS",
        p("Some information"))
    )
  })
  observeEvent(input$sourceInfo, {
    showModal(
      modalDialog(
        size = "l",fade = TRUE, easyClose = TRUE, title = "SOURCES",
        p("Some information"))
    )
  })  
  
  
}


##########################################
# Run the application 
shinyApp(ui, server)
