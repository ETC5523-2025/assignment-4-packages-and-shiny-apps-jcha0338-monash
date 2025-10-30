library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)

function(input, output, session) {

  # Load dataset from package ----
  data("bhai_pps_sample_distribution", package = "healthburdenr", envir = environment())

  # Define color palette for infections ----
  infection_colors <- c(
    "HAP" = "#4B418E",
    "SSI" = "#F56A77",
    "BSI" = "#4062BB",
    "UTI" = "#EEC7CB",
    "CDI" = "#59C3C3"
  )

  # Value Box 1: Total Survey Patients ----
  output$value_box_total_patients <- renderValueBox({
    total_patients <- bhai_pps_sample_distribution |>
      group_by(country) |>
      summarise(country_total = first(num_survey_patients), .groups = "drop") |>
      summarise(grand_total = sum(country_total)) |>
      pull(grand_total)

    valueBox(
      value = formatC(total_patients, format = "d", big.mark = ","),
      subtitle = "Total Survey Patients",
      icon = icon("users"),
      color = "blue"
    )
  })

  # Value Box 2: Total HAI Patients ----
  output$value_box_total_hai <- renderValueBox({
    total_hai <- bhai_pps_sample_distribution |>
      summarise(total = sum(num_hai_patients)) |>
      pull(total)

    valueBox(
      value = formatC(total_hai, format = "d", big.mark = ","),
      subtitle = "Total HAI Patients",
      icon = icon("hospital"),
      color = "red"
    )
  })

  # Value Box 3: Number of Infection Types ----
  output$value_box_infections <- renderValueBox({
    num_infection_types <- bhai_pps_sample_distribution |>
      distinct(infection) |>
      nrow()

    valueBox(
      value = num_infection_types,
      subtitle = "Infection Types Tracked",
      icon = icon("virus"),
      color = "olive"
    )
  })

  # Prepare hierarchical treemap data ----
  treemap_data <- reactive({
    df <- bhai_pps_sample_distribution

    # Level 0: Country roots (top level)
    country_summary <- df |>
      group_by(country) |>
      summarise(
        total_survey = first(num_survey_patients),
        total_hai = sum(num_hai_patients),
        .groups = "drop"
      ) |>
      mutate(
        total_unaffected = total_survey - total_hai,
        prevalence_pct = round((total_hai / total_survey) * 100, 2)
      )

    level0 <- country_summary |>
      mutate(
        id = country,
        label = country,
        parent = "",
        value = total_survey,
        hover = paste0(
          "<b>", country, "</b><br>",
          "Total surveyed: ", formatC(total_survey, format = "d", big.mark = ","), "<br>",
          "HAI patients: ", formatC(total_hai, format = "d", big.mark = ","), "<br>",
          "Prevalence: ", prevalence_pct, "%"
        )
      ) |>
      select(id, label, parent, value, hover)

    # Level 1: HAI status (HAI vs Unaffected)
    level1 <- country_summary |>
      pivot_longer(
        cols = c(total_hai, total_unaffected),
        names_to = "status",
        values_to = "value"
      ) |>
      mutate(
        status_label = ifelse(status == "total_hai", "HAI", "Unaffected by HAIs"),
        id = paste(country, status_label, sep = ":"),
        parent = country,
        pct_of_country = round((value / total_survey) * 100, 2),
        hover = paste0(
          "<b>", status_label, "</b><br>",
          "Country: ", country, "<br>",
          "Patients: ", formatC(value, format = "d", big.mark = ","), "<br>",
          "Percentage: ", pct_of_country, "% of surveyed"
        )
      ) |>
      select(id, label = status_label, parent, value, hover)

    # Level 2: Specific infection types (under HAI only)
    level2 <- df |>
      mutate(
        id = paste(country, "HAI", infection, sep = ":"),
        label = infection,
        parent = paste(country, "HAI", sep = ":"),
        value = num_hai_patients
      ) |>
      # Calculate percentage within HAI patients
      left_join(
        country_summary |> select(country, total_hai),
        by = "country"
      ) |>
      mutate(
        pct_of_hai = round((value / total_hai) * 100, 2),
        hover = paste0(
          "<b>", infection, "</b><br>",
          "Country: ", country, "<br>",
          "HAI patients: ", formatC(num_hai_patients, format = "d", big.mark = ","), "<br>",
          "% of all HAI: ", pct_of_hai, "%<br>",
          "Survey patients: ", formatC(num_survey_patients, format = "d", big.mark = ",")
        )
      ) |>
      select(id, label, parent, value, hover)

    # Combine all levels
    bind_rows(level0, level1, level2)
  })

  # Render interactive treemap ----
  output$treemap_plot <- renderPlotly({
    tm_data <- treemap_data() |>
      mutate(
        color = case_when(
        label == "Unaffected by HAIs" ~ "#4D93BF",
        label == "HAI" ~ "#F45B69",
        label %in% c("HAP","SSI","BSI","UTI","CDI") ~ infection_colors[label],
        TRUE ~ "#EBEBEB"
      ))

    plot_ly(
      type = "treemap",
      ids = tm_data$id,
      labels = tm_data$label,
      parents = tm_data$parent,
      values = tm_data$value,
      text = tm_data$hover,
      textinfo = "label+value+percent parent",
      hovertemplate = "%{text}<extra></extra>",
      branchvalues = "total",
      marker = list(
        colors = tm_data$color,
        line = list(width = 2, color = "white"),
        pad = list(t = 30, l = 5, r = 5, b = 5)
      ),
      pathbar = list(
        visible = TRUE,
        thickness = 30
      )
    ) |>
      layout(
        margin = list(t = 40, l = 5, r = 5, b = 5),
        font = list(size = 12, family = "Arial, sans-serif")
      ) |>
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list("lasso2d", "select2d"),
        displaylogo = FALSE
      )
  })

}
