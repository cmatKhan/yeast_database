library(shinydashboard)
library(plotly)
library(DT)


JSFILE = here("www/utils.js") # not currently doing anything
COMPARATIVES = c(">", ">=", "<", "<=")
THRESHOLD_CHOICE_NAMES = c("Custom", "Lower Fence", "Upper Fence")
BATCH_CHOICES = c("strandedness") # must be column in sample table

ui = dashboardPage(
  dashboardHeader(title = "QC Explorer"),
  ## sidebar -------------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("globe")),
      menuItem("Vocab and Definitions", tabName = "vocab_and_defn",
               icon = icon("scroll")),
      menuItem("Distrubtions/Set Thresholds", tabName = "all_metrics",
               icon= icon("chart-bar")),
      menuItem("Correlation Matrix", tabName = "correlation_matrix",
               icon = icon("chart-line")),
      menuItem("Scatter Plots", tabName = 'scatter_plots',
               icon = icon("database"))
    ),
    fluidRow(style = "padding-top:20px",
      column(12, align="center",
        actionButton(inputId = "reset_sample_audit",
                     label = "Reset sample audit",
                     icon = icon("window-restore")))
    ),
    fluidRow(style = "padding-top:20px",
      column(12, align="center",
        downloadButton("download_audited_samplesheet", "Audited Sample Sheet"))
    )
  ),
  dashboardBody(tags$head(tags$script(src = JSFILE)),
    tabItems(
      ## summary ---------------------------------------------------------------
      tabItem(tabName = "summary",
        fluidRow(
          box(title = "STILL UNDER DEVELOPMENT", solidHeader = TRUE)
        )
      ),
      ## vocab and defn --------------------------------------------------------
      tabItem(tabName = "vocab_and_defn",
        fluidRow(
          box(title = "STILL UNDER DEVELOPMENT", solidHeader = TRUE)
        )
      ),
      ## metric distribution and thresholds ------------------------------------
      tabItem(tabName = "all_metrics",
        fluidRow(
          column(width = 4, offset = 0,
            fluidRow(
              box(width = NULL,
                selectInput('qc_table',
                            'Choose Table',
                            NULL)
              )
            ),
            fluidRow(
              box(width = NULL,
                selectInput("qc_metric",
                            "Choose Metric",
                            NULL)
              )
            )
          ),
          column(width = 4, offset = 0,
            box(width = NULL,
              sliderInput('num_bins', 'Set Number of Bins', 0, 100, 30, 1)
            ),
            box(width = NULL,
              numericInput('histogram_n_breaks', 'Set Number of Breaks',
                           10, 0, 1000, 1)
            ),
            box(width = NULL,
              selectInput(inputId = "batch_variable",
                          label = "Boxplot Batch Variable",
                          choices = BATCH_CHOICES,
                          selected = BATCH_CHOICES[[1]])
            )
          ),
          column(width = 4, offset = 0,
            box(title = "Add/Remove Metric Threshold", width = NULL,
              tabsetPanel(
                id = "add_remove_threshold",
                type = "hidden",
                tabPanelBody("current_threshold_view",
                  fluidRow(column(width = 12, align="center",
                                  valueBoxOutput("current_threshold",
                                                 width = "100%")
                  )),
                  fluidRow(column(width = 12, align = "center",
                                  valueBoxOutput("current_comparative",
                                                 width = "100%")
                  )),
                  fluidRow(column(width = 12, align = "center",
                                  actionButton(
                                    inputId = "remove_threshold",
                                    label = "Remove Threshold",
                                    width = "100%")
                  ))
              ),
              tabPanelBody("set_threshold_view",
                  box(width = NULL, solidHeader = FALSE,
                    fluidRow(
                      sliderInput(inputId = "threshold_slider",
                                  label = "Set Threshold (slider)",
                                  0,10,1, step = .01, round = 3),
                      numericInput(inputId = "threshold_numeric",
                                   label = "Set Threshold (numeric)",
                                   0, 100, 50, step = .01),
                      radioButtons(inputId = 'threshold_options',
                                   label = 'Threshold Options',
                                   choiceNames = THRESHOLD_CHOICE_NAMES,
                                   choiceValues = c(1,2,3))
                    ),
                    fluidRow(
                      selectInput("threshold_comparative",
                                  "Choose Threshold Comparative",
                                  choices = COMPARATIVES,
                                  selected = COMPARATIVES[1])
                    ),
                    fluidRow(
                      actionButton(inputId = "set_threshold",
                                   label = "Set Threshold",
                                   width = "100%")
                    )
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            id = "distribution_plot_tab",
            selected = "histogram",
            type = "tabs",
            tabPanel(title = "Histogram", value = "histogram",
                     plotlyOutput("qc_metric_histogram", height = "1000px")
            ),
            tabPanel(title = "Boxplot", value = "boxplot",
                     plotlyOutput("qc_metric_boxplot", height = "1000px")
            )
          )
        ),
        fluidRow(style = "padding-top:10px",
          box(width = NULL,
              checkboxGroupInput(inputId = "histogram_table_column_selector",
                                 label = "Select Metrics To Display",
                                 choices = NULL)
          )
        ),
        fluidRow(style = "padding-top:10px",
          box(width = NULL,
              downloadButton("download_distribution_table", "Download Table")
          )
        ),
        fluidRow(style = 'padding-top:10px',
          box(title = "DataTable", solidHeader = TRUE, width = NULL,
              dataTableOutput("histogram_sample_table"))
        )
    ),
    ## correlation_matrix -----------------------------------------------------
    tabItem(tabName = "correlation_matrix",
      fluidRow(
        box(title = "STILL UNDER DEVELOPMENT", solidHeader = TRUE)
      ),
      fluidRow(style = "padding-top:10px",
        box(width = NULL,
          checkboxGroupInput(inputId = "correlation_table_column_selector",
                             label = "Select Metrics To Display",
                             choices = NULL)
          )
      ),
      fluidRow(style = 'padding-top:10px',
               box(title = "DataTable", solidHeader = TRUE, width = NULL,
                   dataTableOutput("correlation_sample_table"))
      ),
      fluidRow(style = "padding-top:10px",
        box(width = NULL,
          downloadButton("download_correlations_table", "Download Table")
        )
      ),
    ),
    ## scatterplot ------------------------------------------------------------
    tabItem(tabName = "scatter_plots",
      fluidRow(
        column(width = 6,
          box(width = NULL, title = "Choose x axis metric table",
            selectInput('qc_table_x','QC Tables', NULL)),
          box(width = NULL, title = "Choose x axis metric",
            selectInput("scatter_x", "Choose Metric", NULL))
        ),
        column(width = 6,
          box(width = NULL, title = "Choose a table",
            selectInput('qc_table_y', 'QC Tables', NULL)),
           box(width = NULL, title = "Choose an y-axis metric",
            selectInput("scatter_y", "Choose Metric", NULL))
        )
      ),
      fluidRow(
        plotlyOutput("scatter_plot", width = "100%", height = "1000px")
      ),
      fluidRow(style = "padding-top:10px",
        box(width = NULL,
          checkboxGroupInput(inputId = "scatterplot_table_column_selector",
                             label = "Select Metrics To Display",
                             choices = NULL)
        )
      ),
      fluidRow(style = "padding-top:10px",
        box(width = NULL,
          downloadButton("download_scatterplot_table", "Download Table")
        )
      ),
      fluidRow(style = 'padding-top:10px',
        box(title = "DataTable", solidHeader = TRUE, width = NULL,
            dataTableOutput("scatterplot_sample_table"))
      )
    )
  )
)
)
