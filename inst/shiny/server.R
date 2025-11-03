library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(scales)
library(ggplot2)

function(input, output, session) {

  # Load dataset from package ----
  data("bhai_pps_sample_distribution", package = "healthburdenr", envir = environment())
  data("bhai_pop_est", package = "healthburdenr", envir = environment())
  data("bhai_strata_summary", package = "healthburdenr", envir = environment())

  # Define color palette for infections ----
  infection_colors <- c(
    "HAP" = "#f0f921",
    "SSI" = "#fdb42f",
    "BSI" = "#5c01a6",
    "UTI" = "#9c179e",
    "CDI" = "#ed7953"
  )

  # ===== TAB 1: OVERVIEW =====

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
        label == "Unaffected by HAIs" ~ "#0d0887",
        label == "HAI" ~ "#cc4778",
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

  # ===== TAB 2: POPULATION ESTIMATES =====

  # Helper functions ----

  format_ci_text <- function(lower, upper) {
    paste0("(", comma(lower), " - ", comma(upper), ")")
  }

  format_with_ci <- function(point, lower, upper) {
    paste0( formatC(point, format = "d", big.mark = ","), " (",
            formatC(lower, format = "d", big.mark = ","), " - ",
            formatC(upper, format = "d", big.mark = ","), ")" ) }

  calculate_cfr <- function(deaths, cases) {
    round(ifelse(cases == 0, 0, (deaths / cases) * 100), 1)
  }

  # Reactive: Population summary ----
  output$population_estimates_table <- DT::renderDataTable({

    estimation_summary <- bhai_pop_est |>
      filter(country == input$country_estimate) |>
      mutate(
        CFR = calculate_cfr(deaths_point_estimate, cases_point_estimate),

        # Numeric columns
        Cases = cases_point_estimate,
        Deaths = deaths_point_estimate,
        DALYs = daly_point_estimate,
        YLL = yll_point_estimate,
        YLD = yld_point_estimate,

        # CI text columns
        `Cases CI`  = format_ci_text(cases_lower_ci, cases_upper_ci),
        `Deaths CI` = format_ci_text(deaths_lower_ci, deaths_upper_ci),
        `DALYs CI`  = format_ci_text(daly_lower_ci, daly_upper_ci),
        `YLL CI`    = format_ci_text(yll_lower_ci, yll_upper_ci),
        `YLD CI`    = format_ci_text(yld_lower_ci, yld_upper_ci),

        # Style colors
        CFR_color = case_when(
          CFR < 3 ~ "#d4edda",
          CFR >= 3 & CFR <= 10 ~ "#fff3cd",
          CFR > 10 ~ "#f8d7da"
        ),
        row_color = ifelse(infection == "ALL", "#f2f2f2", "white")
      ) |>
      select(
        HAI = infection,
        Cases, `Cases CI`,
        Deaths, `Deaths CI`,
        CFR,
        DALYs, `DALYs CI`,
        YLL, `YLL CI`,
        YLD, `YLD CI`,
        CFR_color, row_color
      )

    # Population estimates table ----
    DT::datatable(
      estimation_summary |> select(-CFR_color, -row_color),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy','csv'),
        pageLength = 10
      ),
      rownames = FALSE
    ) |>
      DT::formatCurrency(
        c("Cases","Deaths","DALYs","YLL","YLD"),
        currency = "",
        digits = 0,
        mark = ","
      ) |>
      DT::formatStyle(
        'CFR',
        backgroundColor = DT::styleEqual(estimation_summary$CFR, estimation_summary$CFR_color)
      ) |>
      DT::formatStyle(
        'HAI',
        target = 'row',
        backgroundColor = DT::styleEqual(estimation_summary$HAI, estimation_summary$row_color)
      )
  })

  # Reactive filtered data for value boxes (ALL row only)
  population_summary <- reactive({
    bhai_pop_est |>
      filter(country == input$country_estimate, infection == "ALL") |>
      mutate(
        CFR = calculate_cfr(deaths_point_estimate, cases_point_estimate)
      )
  })

  # Value boxes (ALL row data) ----
  output$vbox_total_cases <- renderValueBox({
    row <- population_summary()
    valueBox(
      formatC(row$cases_point_estimate, format="d", big.mark=","),
      subtitle = paste0("Total Cases (95% CI: ", format_with_ci(row$cases_point_estimate, row$cases_lower_ci, row$cases_upper_ci), ")"),
      icon = icon("procedures"),
      color = "blue"
    )
  })

  output$vbox_total_deaths <- renderValueBox({
    row <- population_summary()
    valueBox(
      formatC(row$deaths_point_estimate, format="d", big.mark=","),
      subtitle = paste0("Total Deaths (95% CI: ", format_with_ci(row$deaths_point_estimate, row$deaths_lower_ci, row$deaths_upper_ci), ")"),
      icon = icon("skull-crossbones"),
      color = "red"
    )
  })

  output$vbox_total_dalys <- renderValueBox({
    row <- population_summary()
    valueBox(
      formatC(row$daly_point_estimate, format="d", big.mark=","),
      subtitle = paste0("Total DALYs (95% CI: ", format_with_ci(row$daly_point_estimate, row$daly_lower_ci, row$daly_upper_ci), ")"),
      icon = icon("heartbeat"),
      color = "purple"
    )
  })

  output$vbox_overall_cfr <- renderValueBox({
    row <- population_summary()
    valueBox(
      paste0(row$CFR, "%"),
      subtitle = "Overall Case-Fatality Rate",
      icon = icon("percent"),
      color = "orange"
    )
  })

  # Bubble plot ----
  output$bubble_plot <- renderPlotly({
    bubble_chart_pop <- bhai_pop_est |>
      filter(
        country == input$country_estimate,
        infection != "ALL"
      ) |>
      mutate(
        CFR = calculate_cfr(deaths_point_estimate, cases_point_estimate),
        infection = factor(infection)
      )

    bubble_plot <- ggplot(
      bubble_chart_pop,
      aes(
        x = cases_point_estimate,
        y = deaths_point_estimate,
        size = daly_point_estimate * 10,
        color = infection,
        text = paste0(
          "<b>", infection, "</b><br>",
          "Cases: ", comma(cases_point_estimate), "<br>",
          "Deaths: ", comma(deaths_point_estimate), "<br>",
          "DALYs: ", comma(daly_point_estimate), "<br>",
          "CFR: ", CFR, "%"
        )
      )
    ) +
      geom_point(alpha = 0.8) +
      scale_size_continuous(range = c(5, 10)) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(
        x = "Cases",
        y = "Deaths",
        size = "DALYs",
        color = "HAI"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank()
      )

    # Convert to plotly
    ggplotly(bubble_plot, tooltip = "text") |>
      layout(legend = list(title = list(text = "Infection Type")))
  })

  # Safe filter helper function
  safe_filter <- function(data, column, values) {
    if (length(values) > 0) {
      dplyr::filter(data, !!rlang::sym(column) %in% values)
    } else {
      data
    }
  }

  filtered_stratified_data <- reactive({
    bhai_strata_summary |>
      filter(country == input$country_stratified) |>
      safe_filter("infection", input$infections_stratified) |>
      safe_filter("age_group", input$age_group_filter_strat) |>
      mutate(
        # Choose metric
        metric_estimate = case_when(
          input$stratified_metric == "ncases" ~ ncases_estimate,
          input$stratified_metric == "ndeath"  ~ ndeath_estimate,
          input$stratified_metric == "ndaly"   ~ daly_estimate
        ),
        metric_lower = case_when(
          input$stratified_metric == "ncases" ~ ncases_lower_ci,
          input$stratified_metric == "ndeath" ~ ndeath_lower_ci,
          input$stratified_metric == "ndaly"  ~ daly_lower_ci
        ),
        metric_upper = case_when(
          input$stratified_metric == "ncases" ~ ncases_upper_ci,
          input$stratified_metric == "ndeath" ~ ndeath_upper_ci,
          input$stratified_metric == "ndaly"  ~ daly_upper_ci
        ),
        # Mirror Male values to negative for diverging bars
        metric_value = if_else(sex == "F", -metric_estimate, metric_estimate),
        error_min = if_else(sex == "F", -metric_lower, metric_lower),
        error_max = if_else(sex == "F", -metric_upper, metric_upper)
      )
  })

  # Diverging Bar Plot
  output$stratified_diverging_plot <- renderPlotly({

    plot_data <- filtered_stratified_data() |>
      mutate(
        tooltip_text = paste0(
          "Age: ", age_group, "<br>",
          "Sex: ", sex, "<br>",
          "Infection: ", infection, "<br>",
          "Value: ", scales::comma(metric_estimate), "<br>",
          "95% CI: ", scales::comma(metric_lower), " - ", scales::comma(metric_upper)
        )
      )

    gg <- ggplot(plot_data, aes(
      y = age_group,
      x = metric_value,
      fill = infection,
      text = tooltip_text
    )) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_errorbar(
        aes(xmin = error_min, xmax = error_max),
        width = 0.3,
        color = "black",
        position = position_dodge(width = 0.8)
      ) +
      scale_x_continuous(labels = abs) +
      scale_fill_manual(values = infection_colors) +
      labs(x = "Burden", y = "Age Group", fill = "Infection") +
      facet_wrap(~sex, ncol = 2, scales = "free_x") +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 13)
      )

    ggplotly(gg, tooltip = "text")
  })

}
