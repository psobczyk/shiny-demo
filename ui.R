
library(shiny)
library(scales)
library(ggplot2)
library(dplyr)

source('global.R')
source('helpers.R')

ui <- fluidPage(
  
  titlePanel("Zgony i hospitalizacje według zaszczepienia"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age",
                  "Wiek",
                  min = 0,
                  max = 100,
                  value = c(30, 80)),
      selectInput(inputId = "gender", label = "Płeć",
                  choices = c("Kobieta", "Mężczyzna", "Nieznana"),
                  selected = c("Kobieta", "Mężczyzna", "Nieznana"),
                  multiple = TRUE, selectize = TRUE),
      selectInput(inputId = "woj", label = "Województwo",
                  choices = sort(unique(zgony$woj)),
                  selected = sort(unique(zgony$woj))[1],
                  multiple = FALSE, selectize = TRUE),
      dateRangeInput("dateRange",
                     "Date range",
                     start = min(zgony$data_rap_zgonu), 
                     min = min(zgony$data_rap_zgonu),
                     max = max(zgony$data_rap_zgonu),
                     end = max(zgony$data_rap_zgonu))
    ),
    
    mainPanel(
      plotOutput("riskPlot"),
      dataTableOutput("deathRisk"),
      plotOutput("ageRiskPlot"),
      
      h4("https://szczepienia.pzh.gov.pl/analiza-ryzyka-zgonu-wsrod-zaszczepionych-i-niezaszczepionych/")
    )
  )
)

