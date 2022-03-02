#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(ggplot2)
library(dplyr)

source('global.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Zgony i hospitalizacje według zaszczepienia"),

    # Sidebar with a slider input for number of bins 
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
                        selected = sort(unique(zgony$woj)),
                        multiple = TRUE, selectize = TRUE),
            dateRangeInput("dateRange",
                           "Date range",
                           start = min(zgony$data_rap_zgonu), 
                           min = min(zgony$data_rap_zgonu),
                           max = max(zgony$data_rap_zgonu),
                           end = max(zgony$data_rap_zgonu))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("riskPlot"),
           dataTableOutput("deathRisk"),
           plotOutput("ageRiskPlot"),
           
           h4("https://szczepienia.pzh.gov.pl/analiza-ryzyka-zgonu-wsrod-zaszczepionych-i-niezaszczepionych/")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- reactive({
        zgony %>%
            filter(wiek >= input$age[1], wiek <= input$age[2],
                   data_rap_zgonu >= input$dateRange[1], data_rap_zgonu <= input$dateRange[2],
                   plec %in% input$gender,
                   woj %in% input$woj) %>%
            group_by(szczepienie = dawka_ost) %>%
            summarise(zgony = sum(zgony)) %>%
            ungroup() %>%
            mutate(zgony_procentowo = scales::percent(zgony / sum(zgony)))
    })
    
    output$deathRisk <- renderDataTable({
        df()
    })
    
    output$riskPlot <- renderPlot({
        df() %>%
            ggplot(aes(x=szczepienie, y = zgony)) +
            geom_bar(stat="identity")
    })
    
    output$ageRiskPlot <- renderPlot({
        zgony %>%
            filter(wiek >= input$age[1], wiek <= input$age[2],
                   data_rap_zgonu >= input$dateRange[1], data_rap_zgonu <= input$dateRange[2],
                   plec %in% input$gender,
                   woj %in% input$woj) %>%
            mutate(szczepienie = if_else(dawka_ost %in% c("brak szczepienia"), "niezaszczepione", "zaszczepione"),
                   wiek = cut(wiek, c(0, 20, 30, 40, 50, 60, 70, 80, 100))) %>%
            group_by(szczepienie, wiek) %>%
            summarise(zgony = sum(zgony)) %>%
            mutate(zgony_procentowo = zgony / sum(zgony)) %>%
            ggplot(aes(x=wiek, y = zgony_procentowo, fill=wiek)) +
            geom_bar(stat="identity") +
                facet_grid(szczepienie~.) +
            scale_y_continuous(labels = scales::percent)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
