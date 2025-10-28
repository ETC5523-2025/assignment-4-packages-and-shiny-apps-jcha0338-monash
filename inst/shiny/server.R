library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)

function(input, output, session) {

  # Load dataset
  data("bhai_pps_sample_distribution", package = "healthburdenr", envir = environment())
  df <- bhai_pps_sample_distribution

  # Value boxes
  output$value_box_total_patients <- renderValueBox({
    total <- df |>
      group_by(country) |>
      summarise(total = first(num_survey_patients)) |>
      summarise(sum(total)) |>
      pull()

    valueBox(
      formatC(total, big.mark = ","),
      subtitle = "Total Survey Patients",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$value_box_total_hai <- renderValueBox({
    total_hai <- df |>
      summarise(total = sum(num_hai_patients)) |>
      pull()

    valueBox(
      formatC(total_hai, big.mark = ","),
      subtitle = "Total Patients with HAI",
      icon = icon("hospital"),
      color = "red"
    )
  })

  output$value_box_infections <- renderValueBox({
    num_types <- df |>
      distinct(infection) |>
      nrow()

    valueBox(
      num_types,
      subtitle = "Unique Infection Types Tracked",
      icon = icon("virus"),
      color = "olive"
    )
  })

  # Prepare treemap data
  treemap_data <- reactive({

    # Level 0: Country roots
    country_totals <- df |>
      group_by(country) |>
      summarise(total_survey = first(num_survey_patients),
                total_hai = sum(num_hai_patients)) |>
      ungroup() |>
      mutate(total_no_hai = total_survey - total_hai)

    level0 <- country_totals |>
      mutate(
        id = country,
        label = country,
        parent = "",
        value = total_survey,
        hover = paste0("Country: ", country, "<br>Total patients: ", total_survey)
      ) |>
      select(id, label, parent, value, hover)

    # Level 1: HAI / Unaffected by HAIs
    level1 <- country_totals |>
      pivot_longer(cols = c(total_hai, total_no_hai),
                   names_to = "type", values_to = "value") |>
      mutate(
        label = ifelse(type == "total_hai", "HAI", "Unaffected by HAIs"),
        parent = country,
        id = paste(country, label, sep = ":"),
        hover = paste0("Country: ", country, "<br>", label, " patients: ", value)
      ) |>
      select(id, label, parent, value, hover)

    # Level 2: infections under HAI
    level2 <- df |>
      mutate(
        id = paste(country, "HAI", infection, sep = ":"),
        label = infection,
        parent = paste(country, "HAI", sep = ":"),
        value = num_hai_patients,
        hover = paste0("Country: ", country, "<br>HAI Type: ", infection,
                       "<br>Patients: ", num_hai_patients)
      ) |>
      select(id, label, parent, value, hover)

    bind_rows(level0, level1, level2)
  })

  # Render treemap
  output$treemap_plot <- renderPlotly({
    df_tm <- treemap_data()

    plot_ly(
      type = "treemap",
      labels = df_tm$label,
      ids = df_tm$id,
      parents = df_tm$parent,
      values = df_tm$value,
      textinfo = "label+value",
      hoverinfo = "text",
      hovertext = df_tm$hover,
      branchvalues = "total",
      marker = list(
        colorscale = "Set2",
        line = list(width = 2, color = "white")
      )
    ) |>
      layout(margin = list(t = 30, l = 0, r = 0, b = 0))
  })

}
