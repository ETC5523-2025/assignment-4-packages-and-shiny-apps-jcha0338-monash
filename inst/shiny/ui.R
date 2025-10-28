library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "BHAI: Healthcare-Associated Infections Dashboard"
  ),

  dashboardSidebar(
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
      tabItem(
        tabName = "overview",

        # Value boxes row
        fluidRow(
          valueBoxOutput("value_box_total_patients", width = 4),
          valueBoxOutput("value_box_total_hai", width = 4),
          valueBoxOutput("value_box_infections", width = 4)
        ),

        # Info boxes row
        fluidRow(
          box(
            title = "About BHAI & Healthcare-Associated Infections",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            HTML(
              "<p><strong>Healthcare-Associated Infections (HAIs)</strong> are infections acquired in healthcare settings, not present or incubating at admission.
               They increase patient morbidity, mortality, and healthcare costs.</p>
               <p><strong>BHAI</strong> (Burden of Healthcare-Associated Infections) is a systematic methodology to estimate HAI burden in populations, using surveillance data to quantify cases, deaths, and overall population health loss.</p>
               <p><strong>Point Prevalence Surveys (PPS)</strong> are cross-sectional surveys capturing the frequency and types of HAIs among a defined group of patients at a single point in time.</p>
               <p>Data shown here is from the <em>German PPS 2011</em> and the <em>European Union PPS</em>.</p>
               <p>Burden is measured with the following metrics: <strong>Cases</strong>, <strong>Deaths</strong>, <strong>Disability-Adjusted Life Years (DALYs)</strong>,
               <strong>Years of Life Lost (YLL)</strong>, and <strong>Years Lived with Disability (YLD)</strong>.</p>"
            )
          ),
          box(
            title = "Understanding the Data Fields",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            HTML(
              "<ul>
                <li><strong>HAI:</strong> Healthcare-Associated Infection</li>
                <li><strong>PPS:</strong> Point Prevalence Survey</li>
                <li><strong>Survey Patients:</strong> Total number of patients surveyed</li>
                <li><strong>HAI Patients:</strong> Number of surveyed patients diagnosed with at least one HAI</li>
                <li><strong>Infection Types:</strong> The five tracked HAIs in this dataset:
                  <ul>
                    <li><strong>HAP:</strong> Hospital-Acquired Pneumonia</li>
                    <li><strong>SSI:</strong> Surgical Site Infection</li>
                    <li><strong>BSI:</strong> Bloodstream Infection</li>
                    <li><strong>UTI:</strong> Urinary Tract Infection</li>
                    <li><strong>CDI:</strong> Clostridioides difficile Infection</li>
                  </ul>
                </li>
            </ul>"
            )
          )
        ),

        # Treemap box
        fluidRow(
          box(
            title = "Survey Sample Distribution by HAI Status",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("treemap_plot", height = "600px")
          )
        )
      )
    )
  )
)
