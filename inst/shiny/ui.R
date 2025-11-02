library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "healthburdenr",
    titleWidth = 450
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Population Estimates", tabName = "estimates", icon = icon("project-diagram")),
      menuItem("Stratified Analysis", tabName = "stratified", icon = icon("chart-bar"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    tabItems(
      # TAB 1: Overview ----
      tabItem(
        tabName = "overview",

        # Row 1: Value boxes ----
        fluidRow(
          valueBoxOutput("value_box_total_patients", width = 4),
          valueBoxOutput("value_box_total_hai", width = 4),
          valueBoxOutput("value_box_infections", width = 4)
        ),

        # Row 2: Information boxes ----
        fluidRow(
          box(
            title = "About BHAI & Healthcare-Associated Infections",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 6,
            HTML(
              "<div style='font-size: 14px; line-height: 1.6;'>
                <p><strong>Healthcare-Associated Infections (HAIs)</strong> are infections that patients acquire during healthcare delivery in hospitals or other healthcare facilities.
                These infections were not present or incubating at the time of admission and pose significant challenges to patient safety and healthcare quality.</p>

                <p><strong>BHAI (Burden of Healthcare-Associated Infections)</strong> is a systematic methodology developed to estimate the population-level burden of HAIs.
                It combines surveillance data with epidemiological modeling to quantify:</p>
                <ul style='margin-left: 20px;'>
                  <li>Number of infection cases</li>
                  <li>Attributable deaths</li>
                  <li>Overall population health impact</li>
                </ul>

                <p><strong>Point Prevalence Surveys (PPS)</strong> are one-day cross-sectional surveys that capture the proportion of hospitalized patients with active HAIs.
                These surveys provide essential data for understanding HAI epidemiology and informing prevention strategies.</p>

                <p><strong>Data Sources:</strong> This dashboard presents results from the German PPS 2011 and the European Union PPS,
                processed through BHAI methodology to estimate annual burden.</p>

                <hr style='margin: 15px 0; border: 0; border-top: 1px solid #e0e0e0;'>

                <p><strong>Burden Metrics:</strong></p>
                <ul style='margin-left: 20px;'>
                  <li><strong>Cases:</strong> Estimated annual number of HAI episodes</li>
                  <li><strong>Deaths:</strong> Mortality directly attributable to HAIs</li>
                  <li><strong>DALYs:</strong> Disability-Adjusted Life Years (combines YLL + YLD)</li>
                  <li><strong>YLL:</strong> Years of Life Lost due to premature mortality</li>
                  <li><strong>YLD:</strong> Years Lived with Disability due to infection morbidity</li>
                </ul>
              </div>"
            )
          ),

          box(
            title = "Understanding the Data Fields",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 6,
            HTML(
              "<div style='font-size: 14px; line-height: 1.6;'>
                <dl style='margin-left: 10px;'>
                  <dt style='font-weight: bold; margin-top: 10px;'>HAI (Healthcare-Associated Infection)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Infections acquired during the course of receiving healthcare treatment</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>PPS (Point Prevalence Survey)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>One-day survey measuring the proportion of patients with active infections</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>Survey Patients</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Total number of patients included in the prevalence survey</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>HAI Patients</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Number of surveyed patients diagnosed with at least one healthcare-associated infection</dd>
                </dl>

                <hr style='margin: 15px 0; border: 0; border-top: 1px solid #e0e0e0;'>

                <p style='font-weight: bold; margin-top: 15px;'>Infection Types Tracked:</p>
                <ul style='margin-left: 20px;'>
                  <li><strong>HAP:</strong> Hospital-Acquired Pneumonia - Lower respiratory tract infection</li>
                  <li><strong>SSI:</strong> Surgical Site Infection - Post-operative wound infection</li>
                  <li><strong>BSI:</strong> Bloodstream Infection - Bacteremia or septicemia</li>
                  <li><strong>UTI:</strong> Urinary Tract Infection - Often catheter-associated</li>
                  <li><strong>CDI:</strong> <em>Clostridioides difficile</em> Infection - Antibiotic-associated colitis</li>
                </ul>

                <div style='background-color: #f0f8ff; padding: 10px; border-radius: 5px; margin-top: 15px;'>
                  <p style='margin: 0; font-size: 13px;'><strong>Note:</strong> These five infection types represent the most common and clinically significant HAIs monitored in European healthcare settings.</p>
                </div>
              </div>"
            )
          )
        ),

        # Row 3: Treemap visualization ----
        fluidRow(
          box(
            title = "Survey Sample Distribution: HAI Prevalence by Country and Infection Type",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("<p style='margin-bottom: 15px; color: #666;'>
                  This hierarchical visualization shows the distribution of surveyed patients by country,
                  HAI status, and specific infection types. Larger rectangles represent higher patient counts.
                  </p>"),
            plotlyOutput("treemap_plot", height = "600px")
          )
        ),
      ),

      # TAB 2: Population Estimates ----
      tabItem(
        tabName = "estimates",

        # Header with better description
        fluidRow(
          column(12,
                 h2("Population Burden Estimates", style = "margin-top: 0;"),
                 p(style = "font-size: 15px; color: #666; margin-bottom: 20px;",
                   "Annual burden estimates derived from BHAI modeling, showing cases, deaths, and disability-adjusted life years (DALYs) by infection type. ",
                   "The 'ALL' row represents the aggregate burden across all five HAI types."
                 )
          )
        ),

        # Summary Value Boxes (using ALL row)
        fluidRow(
          valueBoxOutput("vbox_total_cases", width = 3),
          valueBoxOutput("vbox_total_deaths", width = 3),
          valueBoxOutput("vbox_overall_cfr", width = 3),
          valueBoxOutput("vbox_total_dalys", width = 3)
        ),

        # Main content: Filter + Tabbed outputs
        fluidRow(
          # Filter Box
          column(
            width = 3,
            box(
              title = "Filters",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,

              selectInput(
                "country_estimate",
                "Select Country/Region:",
                choices = c("Germany", "European Union"),
                selected = "Germany"
              ),

              hr(style = "margin: 15px 0; border-top: 1px solid #ddd;"),

              HTML("<p style='font-size: 13px; color: #666; margin-bottom: 5px;'><strong>Data Notes:</strong></p>"),
              HTML("<ul style='font-size: 12px; color: #666; margin-left: 15px; line-height: 1.5;'>
                <li>Values are point estimates with 95% confidence intervals</li>
                <li>'ALL' row shows total burden across all HAI types</li>
                <li>CFR = Case-Fatality Rate (%)</li>
              </ul>")
            ),

            box(
              title = "Understanding Burden Metrics",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = NULL,
              HTML("
              <div style='font-size: 14px; line-height: 1.6;'>
                <dl style='margin-left: 10px;'>
                  <dt style='font-weight: bold; margin-top: 10px;'>Point Estimate</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>The most likely value from BHAI simulation (median of 500 simulations)</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>95% Confidence Interval (CI)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Range within which the true value likely falls with 95% probability. Wider intervals indicate greater uncertainty.</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>Cases</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Estimated annual number of HAI episodes in the population</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>Deaths</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Annual mortality directly attributable to HAIs (excess deaths beyond background mortality)</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>Case-Fatality Rate (CFR)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Percentage of HAI cases that result in death (Deaths ÷ Cases × 100). Higher CFR indicates more lethal infections.</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>DALYs (Disability-Adjusted Life Years)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Combined measure of population health loss from both mortality and morbidity. One DALY = one lost year of healthy life. Calculated as YLL + YLD.</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>YLL (Years of Life Lost)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Years lost due to premature death from HAI, calculated using standard life expectancy</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>YLD (Years Lived with Disability)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Years lived with reduced quality of life due to HAI morbidity and long-term health consequences</dd>

                  <dt style='font-weight: bold; margin-top: 10px;'>ALL (Aggregate Row)</dt>
                  <dd style='margin-left: 20px; margin-bottom: 10px;'>Sum of all five HAI types (BSI, HAP, SSI, UTI, CDI) - represents total HAI burden</dd>
                </dl>
              </div>
            ")
            )
          ),

          # Tabbed Outputs (right main panel)
          column(
            width = 9,
            box(
              title = NULL,
              status = "primary",
              solidHeader = FALSE,
              width = NULL,

              tabsetPanel(
                id = "estimates_tabs",
                type = "tabs",

                # Tab 1: Data Table
                tabPanel(
                  title = "Data Table",
                  icon = icon("table"),
                  value = "table",

                  br(),
                  HTML("<p style='margin-bottom: 15px; color: #666;'>
                    Detailed estimates for all infections. The 'ALL' row is highlighted in gray.
                    CFR (Case-Fatality Rate) is color-coded: <span style='background: #d4edda; padding: 2px 6px;'>&lt;3%</span>,
                    <span style='background: #fff3cd; padding: 2px 6px;'>3-10%</span>,
                    <span style='background: #f8d7da; padding: 2px 6px;'>&gt;10%</span>
                  </p>"),
                  DT::dataTableOutput("population_estimates_table")
                ),

                # Tab 2: Bubble Plot
                tabPanel(
                  title = "Bubble Chart",
                  icon = icon("dot-circle"),
                  value = "bubble",

                  br(),

                  # Center the plot with better sizing
                  div(
                    style = "display: flex; justify-content: center; align-items: center;",
                    plotlyOutput("bubble_plot", height = "550px", width = "100%")
                  ),

                  # Interpretation guide below plot
                  hr(style = "margin: 20px 0;"),
                  HTML("
                    <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-size: 13px;'>
                      <strong>Interpretation Tips:</strong>
                      <ul style='margin: 10px 0 5px 20px; line-height: 1.6;'>
                        <li><strong>Position matters:</strong> Infections higher on the Y-axis relative to X-axis have higher case-fatality rates</li>
                        <li><strong>Bubble size indicates total burden:</strong> Large bubbles represent infections with substantial DALY impact</li>
                        <li><strong>Prevention priorities:</strong> Focus on infections that are both high-incidence (far right) and high-mortality (high up)</li>
                        <li><strong>Hover for details:</strong> Move your cursor over bubbles to see exact numbers and calculated CFR</li>
                      </ul>
                    </div>
                  ")
                )
              )
            )
          )
        )
        ),

      tabItem(
        tabName = "simulation",
        h2("Simulation")
      )
    )
  )
)
