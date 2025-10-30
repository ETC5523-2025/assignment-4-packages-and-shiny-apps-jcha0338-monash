library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "BHAI: Healthcare-Associated Infections Dashboard",
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

      tabItem(
        tabName = "estimates",

        # Header
        h2("Population Burden Estimates & Stratified Analysis"),
        p("Annual burden estimates by country with demographic stratification"),

        # --- Summary Value Boxes ---
        fluidRow(
          valueBoxOutput("vbox_total_cases", width = 3),
          valueBoxOutput("vbox_total_deaths", width = 3),
          valueBoxOutput("vbox_overall_cfr", width = 3),
          valueBoxOutput("vbox_total_dalys", width = 3)
        ),

        # --- Filters ---
        fluidRow(
          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            column(
              width = 4,
              selectInput(
                "country_estimate", "Select Country/Region:",
                choices = c("Germany", "European Union"), selected = "Germany"
              )
            ),
            column(
              width = 4,
              checkboxInput(
                "show_detailed_ci", "Show separate CI columns in table", FALSE
              )
            )
          )
        ),

        # --- Data Table ---
        fluidRow(
          box(
            title = "Detailed Burden Estimates by Infection Type",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("population_estimates_table")
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
