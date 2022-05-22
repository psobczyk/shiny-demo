
library(shiny)
library(scales)
library(ggplot2)
library(dplyr)

source("global.R")
source("helpers.R")

shinyServer(function(input, output) {
  df <- reactive({
    zgony %>%
      filter(
        wiek >= input$age[1], wiek <= input$age[2],
        data_rap_zgonu >= input$dateRange[1], data_rap_zgonu <= input$dateRange[2],
        plec %in% input$gender,
        woj %in% input$woj
      ) %>%
      group_by(szczepienie = dawka_ost) %>%
      summarise(zgony = sum(zgony)) %>%
      ungroup() %>%
      mutate(zgony_procentowo = scales::percent(zgony / sum(zgony)))
  })

  df2 <- reactive({
    df() %>%
      mutate(costam = "costam")
  })

  output$deathRisk <- renderDataTable({
    df2()
  })

  output$riskPlot <- renderPlot({
    df() %>%
      ggplot(aes(x = szczepienie, y = zgony)) +
      geom_bar(stat = "identity")
  })

  output$ageRiskPlot <- renderPlot({
    zgony %>%
      filter(
        wiek >= input$age[1], wiek <= input$age[2],
        data_rap_zgonu >= input$dateRange[1], data_rap_zgonu <= input$dateRange[2],
        plec %in% input$gender,
        woj %in% input$woj
      ) %>%
      mutate(
        szczepienie = if_else(dawka_ost %in% c("brak szczepienia"), "niezaszczepione", "zaszczepione"),
        wiek = cut(wiek, c(0, 20, 30, 40, 50, 60, 70, 80, 100))
      ) %>%
      group_by(szczepienie, wiek) %>%
      summarise(zgony = sum(zgony)) %>%
      mutate(zgony_procentowo = zgony / sum(zgony)) %>%
      ggplot(aes(x = wiek, y = zgony_procentowo, fill = wiek)) +
      geom_bar(stat = "identity") +
      facet_grid(szczepienie ~ .) +
      scale_y_continuous(labels = scales::percent)
  })
})
