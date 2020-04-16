# Loading require libraries -----------------------------------------------
library(htmltools)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(Cairo)
library(showtext)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(ggiraph)
library(lubridate)
library(viridis)
library(plyr)
library(dplyr)
library(tidyr)

# Adding Oswald Font --------------------------------------------
font_add(family = "oswald", regular = "www/Oswald-Regular.ttf", bold = "www/Oswald-Bold.ttf")
showtext::showtext_auto()

# Input periods for which data is available -------------------------------
periods <- c("AUG'17" = 20171408, "SEP'17" = 20171409, "AUG'18" = 20181408, "SEP'18" = 20181409)
zone_levs <- c("North", "East", "West", "South")
popstrata_levs <- c("Metro", "TC1", "ROU", "Rural")
channel_levs <- c("Grocer/GS", "Food", "Paan Plus", "Others", "MT")
channel_labs <- c("Grocer/GS", "Food", "Paan Plus", "Others", "MT")
topdealers_levs <- c("Top 50", "Next 30", "Bottom 20")
topdealers_labs <- c("Top 50", "Next 30", "Bottom 20")

# Option Settings -------------------------
options(shiny.usecairo = T)

# Loading required files --------------------------------------------------
## Loading summary file, will be used for the 'Summary' module
SUMMARY_SummaryFile <- readRDS("data/SUMMARY_SummaryFile.rds")
DISTRIBUTION_DistributionFile <- readRDS("data/DISTRIBUTION_DistributionFile.rds")
DISTRIBUTION_TopDealersFile <- readRDS("data/DISTRIBUTION_TopDealersFile.rds")
ASSORTMENT_AssortmentFile <- readRDS("data/ASSORTMENT_AssortmentFile.rds")
ASSORTMENT_TransitionFile <- readRDS("data/ASSORTMENT_TransitionFile.rds")
ASSORTMENT_SKUFile <- readRDS("data/ASSORTMENT_SKUFile.rds")
BRANDINTERACTIONS_GainLossFile <- readRDS("data/BRANDINTERACTIONS_GainLossFile.rds")
BRANDINTERACTIONS_ShiftFile <- readRDS("data/BRANDINTERACTIONS_ShiftFile.rds")
BRANDINTERACTIONS_ManufacturerFile <- readRDS("data/BRANDINTERACTIONS_ManufacturerFile.rds")
BRANDINTERACTIONS_CannibalizationFile <- readRDS("data/BRANDINTERACTIONS_CannibalizationFile.rds")
FORECAST_ForecastsFile <- read.csv("data/FORECAST_ForecastsFile.csv", header = T, as.is = T, check.names = F, na.strings = c("", "NA"))
FORECAST_ForecastsFile$Date <- lubridate::dmy(FORECAST_ForecastsFile$Date)

headerpanel <- shiny::column(
  width = 12,
  shiny::column(
    width = 6,
    offset = 0,
    htmltools::h1(htmltools::strong(toupper("Brand Performance Drivers")), style = "display: inline-block; margin: 10px 0px 0px 0px")
  ),
  shiny::column(
    width = 5,
    offset = 1,
    HTML('<div class="icon-bar">
    <a href=""><i class="fa fa-user"></i></a>
    <a href=""><i class="fa fa-home"></i></a>
    </div>'
    )
  ),
  class = "navbar"
)

# Selection Panel of the Dashboard ----------------------------------------
selection_panel <- shiny::fluidRow(
  lapply(1:45, function(x) htmltools::div(htmltools::br(), style = "display: inline-block;")),
  htmltools::div(shiny::selectInput("base_period", "BASE PERIOD:", choices = periods[-length(periods)], width = "250px", selected = 20171409), style = "display: inline-block;"),
  lapply(1:20, function(x) htmltools::div(htmltools::br(), style = "display: inline-block;")),
  htmltools::div(shiny::selectInput("comparison_period", "COMPARISON PERIOD:", choices = periods[-1], width = "250px", selected = 20181409), style = "display: inline-block;"),
  lapply(1:20, function(x) htmltools::div(htmltools::br(), style = "display: inline-block;")),
  htmltools::div(shiny::selectInput("brand", "BRAND:", choices = c("HEALTHY BITES" = "I T C_BINGO TEDHE MEDHE"), width = "250px"), style = "display: inline-block;"),
  lapply(1:20, function(x) htmltools::div(htmltools::br(), style = "display: inline-block;")),
  shiny::actionButton(inputId = "go", label = "ANALYZE", icon = shiny::icon("line-chart"), class = "analyze_button"),
  class = "input_container"
)

# Summary Tab UI -----------------------------------------------
tabpanel_summary <- shiny::tabPanel(
  htmltools::strong("SUMMARY"),
  htmltools::br(),
  shiny::column(
    width = 5,
    offset = 1,
    htmltools::h4(htmltools::strong("VALUE SALES (IN MILL. RS.)"), style = "margin: 5px 0px 0px -10px;"),
    htmltools::br(),
    shiny::column(
      width = 6,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "value_sales", width = "100%", height = "200px"), type = 8)
    ),
    shiny::column(
      width = 6,
      htmltools::h3(htmltools::strong("VALUE CHANGE"), style = "text-align: center;"),
      htmltools::h5("Base vs. Comparison Period", style = "text-align: center;"),
      shiny::uiOutput(outputId = "value_change")
    ),
    style = "background-color: white;"
  ),
  shiny::column(
    width = 5,
    htmltools::h4(htmltools::strong("VOLUME SALES (IN TONNES)"), style = "margin: 5px 0px 0px -10px;"),
    htmltools::br(),
    shiny::column(
      width = 6,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "volume_sales", width = "100%", height = "200px"), type = 8)
    ),
    shiny::column(
      width = 6,
      htmltools::h3(htmltools::strong("VOLUME CHANGE"), style = "text-align: center;"),
      htmltools::h5("Base vs. Comparison Period", style = "text-align: center;"),
      shiny::uiOutput(outputId = "volume_change")
    ),
    style = "background-color: white; border-left: 3px solid #F0F0F0"
  ),
  shiny::column(
    htmltools::h4(htmltools::strong("SALES & DISTRIBUTION FACTS"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-top: 3px solid #F0F0F0; padding-bottom: 25px",
    width = 10,
    offset = 1,
    shiny::column(
      width = 4,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "numeric_distribution_pie", width = "auto", height = "250px"), type = 8),
      shiny::column(
        width = 6,
        offset = 3,
        shiny::uiOutput(outputId = "numeric_distribution_change")
      )
    ),
    shiny::column(
      width = 4,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "weighted_distribution_pie", width = "100%", height = "250px"), type = 8),
      shiny::column(
        width = 6,
        offset = 3,
        shiny::uiOutput(outputId = "weighted_distribution_change")
      )
    ),
    shiny::column(
      width = 4,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "sah_pie", width = "100%", height = "250px"), type = 8),
      shiny::column(
        width = 6,
        offset = 3,
        shiny::uiOutput(outputId = "sah_change")
      )
    )
  )
)

# Distribution Tab UI -----------------------------------------------------
tabpanel_distribution <- shiny::tabPanel(
  htmltools::strong("DISTRIBUTION"),
  htmltools::br(),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("DISTRIBUTION CHURN ANALYSIS")),
    style = "text-align: center; background-color: #5F6B6D; color: white;"
  ),
  shiny::column(
    width = 5,
    offset = 1,
    htmltools::h4(htmltools::strong("NO. OF STORES (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    htmltools::br(),
    shiny::column(
      width = 6,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "stores", width = "100%", height = "200px"), type = 8)
    ),
    shiny::column(
      width = 6,
      htmltools::h3(htmltools::strong("CHANGE IN STORES"), style = "text-align: center;"),
      htmltools::h5("Base vs. Comparison Period", style = "text-align: center;"),
      shiny::uiOutput(outputId = "stores_change")
    ),
    style = "background-color: white; border-top: 3px solid #F0F0F0"
  ),
  shiny::column(
    width = 5,
    offset = 0,
    htmltools::h4(htmltools::strong("STORE CHURN (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    htmltools::br(),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "store_churn", width = "100%", height = "200px"), type = 8),
    style = "background-color: white; border-top: 3px solid #F0F0F0; border-left: 3px solid #F0F0F0"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("SALES DELTA BY STORE TYPE"), style = "margin: 5px 0px 0px -10px; display: inline-block;"),
    htmltools::br(),
    shiny::column(
      width = 4,
      htmltools::div(shinyWidgets::radioGroupButtons(inputId = "fact", label = "", status = "primary", choices = c("SALES VOL. (IN TONNES)" = "SALVOL", "SALES VAL. (IN MILL.)" = "SALVAL"), direction = "vertical"), style = "margin: auto; width: 50%;"),
      style = "padding: 40px 0px 0px 0px;"
    ),
    shiny::column(
      width = 8,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "sales_churn", width = "100%", height = "200px"), type = 8)
    ),
    style = "background-color: white; border-top: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("DISTRIBUTION CHURN FOR ALL INDIA (U + R)"), style = "margin: 5px 0px 0px -10px; display: inline-block;"),
    htmltools::br(),
    shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "distribution_allIndia"), type = 8),
    style = "background-color: white; border-top: 3px solid #F0F0F0; padding-bottom: 10px;"
  ),
  htmltools::br(),
  shiny::column(
    width = 4,
    offset = 4,
    htmltools::div(shiny::selectInput(inputId = "distribution_store_type", label = "SELECT STORE TYPE:", choices = c("ENTRY STORES", "CONTINUOUS STORES", "EXIT STORES"), width = "250px"), style = "margin: auto; width: 50%;"),
    style = "padding-top: 10px; padding-bottom: 0px;"
  ),
  htmltools::br(),
  shiny::column(
    width = 5,
    offset = 1,
    htmltools::h4(htmltools::strong("STORES BY ZONE (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white;",
    shiny::column(
      width = 6,
      htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "stores_by_zones", width = "100%", height = "200px"), type = 8), style = "margin: 25px 0px 0px 0px;")
    ),
    shiny::column(
      width = 6,
      div(DT::dataTableOutput(outputId = "wd_by_zones"), style = "margin: 0px 0px 0px 0px;")
    )
  ),
  shiny::column(
    width = 5,
    offset = 0,
    htmltools::h4(htmltools::strong("STORES BY POPSTRATA (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-left: 3px solid #F0F0F0;",
    shiny::column(
      width = 6,
      htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "stores_by_popstrata", width = "100%", height = "200px"), type = 8), style = "margin: 25px 0px 0px 0px;")
    ),
    shiny::column(
      width = 6,
      div(DT::dataTableOutput(outputId = "wd_by_popstrata"), style = "margin: 0px 0px 0px 0px;")
    )
  ),
  shiny::column(
    width = 5,
    offset = 1,
    htmltools::h4(htmltools::strong("STORES BY CHANNEL (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-top: 3px solid #F0F0F0",
    shiny::column(
      width = 6,
      htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "stores_by_channel", width = "100%", height = "200px"), type = 8), style = "margin: 25px 0px 0px 0px;")
    ),
    shiny::column(
      width = 6,
      div(DT::dataTableOutput(outputId = "wd_by_channel"), style = "margin: -15px 0px 0px 0px;")
    )
  ),
  shiny::column(
    width = 5,
    offset = 0,
    htmltools::h4(htmltools::strong("STORES BY TOP DEALERS (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-top: 3px solid #F0F0F0; border-left: 3px solid #F0F0F0;",
    shiny::column(
      width = 6,
      htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "stores_by_topdealers", width = "100%", height = "200px"), type = 8), style = "margin: 25px 0px 0px 0px;")
    ),
    shiny::column(
      width = 6,
      div(DT::dataTableOutput(outputId = "wd_by_topdealers"), style = "margin: -10px 0px 0px 0px;")
    )
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("STORES BY STATES (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "stores_by_states", height = "500px"), type = 8), style = "margin: 0px 0px 0px 0px;")
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("TOP DEALER ANALYSIS")),
    style = "text-align: center; background-color: #5F6B6D; border-top: 3px solid #F0F0F0; color: white;"
  ),
  shiny::column(
    width = 2,
    offset = 1,
    shiny::fluidRow(
      class = "top_dealers_state",
      htmltools::div(shiny::actionButton("top50", "TOP 50", width = "200px"), style = "margin: 20px 0px 5px -10px;"),
      htmltools::div(shiny::actionButton("next30", "NEXT 30", width = "150px"), style = "padding: 0px 30px 0px 30px; margin: -5px 0px 5px -10px;"),
      htmltools::div(shiny::actionButton("bottom20", "BOTTOM 20", width = "100px"), style = "padding: 0px 55px 0px 55px; margin: -5px 0px 24px -10px;")
    ),
    htmltools::div(shinydashboard::infoBoxOutput(outputId = "topdealers_nd", width = NULL), style = "width: 116%; margin: 3px 0px 0px -15px;"),
    htmltools::div(shinydashboard::infoBoxOutput(outputId = "topdealers_wd", width = NULL), style = "width: 116%; margin: -12px 0px 0px -15px;"),
    htmltools::div(shinydashboard::infoBoxOutput(outputId = "topdealers_valchange", width = NULL), style = "width: 116%; margin: -12px 0px 0px -15px;"),
    htmltools::div(shinydashboard::infoBoxOutput(outputId = "topdealers_volchange", width = NULL), style = "width: 116%; margin: -12px 0px 0px -15px;")
  ),
  shiny::column(
    width = 8,
    htmltools::h4(htmltools::strong("STATE-WISE GROWTH IN STATES (IN '000)"), style = "margin: 5px 0px 0px -10px;"),
    lapply(1, function(x) htmltools::br()),
    style = "background-color: white; height: 568px; border-top: 3px solid #F0F0F0; border-left: 3px solid #F0F0F0",
    shinycssloaders::withSpinner(ggiraph::ggiraphOutput(outputId = "topdealers_state"), type = 8)
  )
)
# Assortment Tab UI -------------------------------------------------------
tabpanel_assortment <- shiny::tabPanel(
  title = htmltools::strong("ASSORTMENT"),
  htmltools::br(),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("AVERAGE SKU COUNT PER STORE")),
    style = "text-align: center; background-color: #5F6B6D; color: white;"
  ),
  shiny::column(
    width = 5,
    offset = 1,
    htmltools::h4(htmltools::strong("CATEGORY"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    shiny::column(
      width = 6,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cat_sku", width = "100%", height = "200px"), type = 8)
    ),
    shiny::column(
      width = 6,
      shiny::uiOutput(outputId = "icon_cat_sku"),
      shiny::uiOutput(outputId = "change_cat_sku"),
      style = "padding: 0px;"
    )
  ),
  shiny::column(
    width = 5,
    offset = 0,
    htmltools::h4(htmltools::strong("BRAND"), style = "margin: 5px 0px 0px -10px;"),
    style = "background-color: white; border-left: 3px solid #F0F0F0; border-top: 3px solid #F0F0F0;",
    shiny::column(
      width = 6,
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = "brd_sku", width = "100%", height = "200px"), type = 8)
    ),
    shiny::column(
      width = 6,
      shiny::uiOutput(outputId = "icon_brd_sku"),
      shiny::uiOutput(outputId = "change_brd_sku")
    )
  ),
  htmltools::br(),
  shiny::column(
    width = 4,
    offset = 4,
    htmltools::div(shiny::selectInput(inputId = "asst_cut", label = "SELECT CUT:", choices = c("Zone", "Channel", "Popstrata", "Top Dealers"), width = "250px"), style = "margin: auto; width: 50%;"),
    style = "padding-top: 10px; padding-bottom: 0px;"
  ),
  htmltools::br(),
  shiny::column(
    width = 5,
    offset = 1,
    htmltools::h4(htmltools::strong("CATEGORY"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cat_sku_cut", width = "100%", height = "200px"), type = 8),
    style = "background-color: white;"
  ),
  shiny::column(
    width = 5,
    offset = 0,
    htmltools::h4(htmltools::strong("BRAND"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "brd_sku_cut", width = "100%", height = "200px"), type = 8),
    style = "background-color: white; border-left: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("AVERAGE SKU COUNT PER STORE BY STATES - CATEGORY"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cat_sku_state"), type = 8),
    style = "background-color: white; border-top: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("AVERAGE SKU COUNT PER STORE BY STATES - BRAND"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "brd_sku_state"), type = 8),
    style = "background-color: white; border-top: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("STOCK TRANSITION ANALYSIS")),
    style = "text-align: center; background-color: #5F6B6D; color: white; border-top: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("BRAND"), style = "margin: 5px 0px 0px -10px;"),
    shiny::fluidRow(shinyWidgets::radioGroupButtons(inputId = "store_type", width = "150%", label = "", choices = c("All India", "Top 50", "Next 30", "Bottom 20"), status = "primary"), style = "margin: auto; width: 25%;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "transition_ai"), type = 8),
    style = "background-color: white; border-top: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    htmltools::h4(htmltools::strong("TOP SKUs")),
    style = "text-align: center; background-color: #5F6B6D; color: white; border-top: 3px solid #F0F0F0;"
  ),
  shiny::column(
    width = 10,
    offset = 1,
    shiny::fluidRow(shinyWidgets::radioGroupButtons(inputId = "store_type2", width = "150%", label = "", choices = c("All India", "Top 50", "Next 30", "Bottom 20"), status = "primary"), style = "margin: auto; width: 25%;"),
    shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "top_skus"), type = 8),
    style = "background-color: white; border-top: 3px solid #F0F0F0; padding-bottom: 10px;"
  )
)

# Gain Loss Tab UI --------------------------------------------------------
tabpanel_gainloss <- shiny::tabPanel(
  title = htmltools::strong("BRAND INTERACTIONS"),
  htmltools::br(),
  shiny::column(
    width = 6,
    offset = 3,
    htmltools::div(shiny::selectInput(inputId = "gl_type", label = "SELECT FACT:", choices = c("SALES VALUE (IN MILL. RS.)" = "SALVAL", "SALES VOLUME (IN TONNES)" = "SALVOL"), selected = "SALVOL", width = "250px"), style = "margin: auto; width: 50%;"),
    style = "padding-top: 10px; padding-bottom: 0px;"
  ),
  htmltools::br(),
  shiny::column(
    width = 3,
    offset = 1,
    htmltools::h3(strong(HTML("DELTA DUE TO<br>BRAND<br>INTERACTIONS")), style = "margin: -10px 0px 0px 0px;"),
    shiny::uiOutput(outputId = "delta_int"),
    style = "text-align: center; background-color: white; padding: 30px 0px 24px 0px;",
    id = "delta"
  ),
  shiny::column(
    width = 7,
    offset = 0,
    style = "background-color: white; border-left: 3px solid #F0F0F0;",
    id = "split_delta",
    htmltools::h4(htmltools::strong("SPLIT OF DELTA DUE TO BRAND INTERACTIONS"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(plotOutput(outputId = "shift_split", width = "90%", height = "200px"), type = 8)
  ),
  shiny::column(
    width = 5,
    offset = 1,
    style = "background-color:white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("SPLIT BY ZONES"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "split_by_zones", width = "100%", height = "250px"), type = 8)
  ),
  shiny::column(
    width = 5,
    offset = 0,
    style = "background-color:white; border-top: 3px solid #F0F0F0; border-left: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("SPLIT BY POPSTRATA"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "split_by_popstrata", width = "100%", height = "250px"), type = 8)
  ),
  shiny::column(
    width = 5,
    offset = 1,
    style = "background-color:white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("SPLIT BY CHANNELS"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "split_by_channels", width = "100%", height = "250px"), type = 8)
  ),
  shiny::column(
    width = 5,
    offset = 0,
    style = "background-color:white; border-top: 3px solid #F0F0F0; border-left: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("SPLIT BY TOP DEALERS"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "split_by_topdealers", width = "100%", height = "250px"), type = 8)
  ),
  shiny::column(
    width = 10,
    offset = 1,
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("SPLIT BY STATES"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "split_by_states", height = "500px"), type = 8)
  ),
  shiny::column(
    width = 10,
    offset = 1,
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("TOP BRANDS GAINED FROM"), style = "margin: 5px 0px 0px -10px;"),
    shiny::column(
      width = 6,
      htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "top_gains", height = "250px"), type = 8), style = "margin: 40px 0px 0px 0px;")
    ),
    shiny::column(
      width = 6,
      htmltools::div(shinyWidgets::radioGroupButtons(inputId = "cut1", label = "", choices = c("Zone", "Channel", "Popstrata", "Top Dealers"), selected = "Zone", status = "primary"), style = "margin: auto; width: 70%;"),
      htmltools::div(shinycssloaders::withSpinner(ggiraph::ggiraphOutput(outputId = "gains_by_cuts", height = "250px"), type = 8), style = "margin: 0px 0px 0px 0px;"),
      style = "margin: -25px 0px 0px 0px;"
    )
  ),
  shiny::column(
    width = 10,
    offset = 1,
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("TOP BRANDS LOST TO"), style = "margin: 5px 0px 0px -10px;"),
    shiny::column(
      width = 6,
      htmltools::div(shinycssloaders::withSpinner(shiny::plotOutput(outputId = "top_losses", height = "250px"), type = 8), style = "margin: 40px 0px 0px 0px;")
    ),
    shiny::column(
      width = 6,
      htmltools::div(shinyWidgets::radioGroupButtons(inputId = "cut2", label = "", choices = c("Zone", "Channel", "Popstrata", "Top Dealers"), selected = "Zone", status = "primary"), style = "margin: auto; width: 70%;"),
      htmltools::div(shinycssloaders::withSpinner(ggiraph::ggiraphOutput(outputId = "losses_by_cuts", height = "250px"), type = 8), style = "margin: 0px 0px 0px 0px;"),
      style = "margin: -25px 0px 0px 0px;"
    )
  ),
  shiny::column(
    width = 10,
    offset = 1,
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("TOP MANUFACTURER INTERACTIONS"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "company_gl", height = "300px"), type = 8)
  ),
  shiny::column(
    width = 3,
    offset = 1,
    style = "background-color: white; border-top: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("CANNIBALIZATION SPLIT"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cannibalization", height = "300px"), type = 8)
  ),
  shiny::column(
    width = 7,
    style = "background-color: white; border-top: 3px solid #F0F0F0; border-left: 3px solid #F0F0F0;",
    htmltools::h4(htmltools::strong("TOP GAINS & LOSSES FROM CANNIBALIZATION"), style = "margin: 5px 0px 0px -10px;"),
    shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cannibalization_gl", height = "300px"), type = 8)
  )
)

# Forecast Tab UI ---------------------------------------------------------
tabpanel_forecast <- shiny::tabPanel(
  title = htmltools::strong("FORECASTING"),
  htmltools::br(),
  shiny::column(
    width = 8,
    offset = 2,
    htmltools::h4(htmltools::strong("FORECASTS FOR SALES VOLUME (IN TONNES)"), style = "margin: 5px 0px 0px -10px; display: inline-block;"),
    htmltools::br(),
    shiny::column(
      width = 2,
      shinyWidgets::prettyRadioButtons(inputId = "geo_opts", label = "", choices = "All India", status = "primary")
    ),
    shiny::column(
      width = 10,
      shiny::fluidRow(shinyWidgets::radioGroupButtons(inputId = "geography", width = "150%", label = "", choices = c("All India", "Zone", "State"), status = "primary"), style = "margin: auto; width: 25%;"),
      shinycssloaders::withSpinner(ggiraph::ggiraphOutput(outputId = "forecast_graph"), type = 8)
    ),
    style = "background-color: white; height: 550px;"
  ),
  shiny::fluidRow(
    shiny::column(
      width = 4,
      offset = 2,
      div(shinydashboard::infoBoxOutput(outputId = "change_pm", width = 12), class = "ib1")
    ),
    shiny::column(
      width = 4,
      offset = 0,
      div(shinydashboard::infoBoxOutput(outputId = "change_ly", width = 12), class = "ib2")
    )
  )
)

# UI Part of the App ------------------------------------------------------
ui <- shiny::fluidPage(

  tags$script(src = "myscript.js"),

  ## Linking the style sheet to the shiny app
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "ABD.css"),
  ),
  title = "Brand Performance Drivers",
  headerpanel,
  selection_panel,
  htmltools::br(),
  shiny::conditionalPanel(
    condition =  "input.go > 0",
    shiny::tabsetPanel(
      tabpanel_summary,
      tabpanel_distribution,
      tabpanel_assortment,
      tabpanel_gainloss,
      tabpanel_forecast
    )
  )
)

# Server Part of the App --------------------------------------------------
server <- function(input, output, session) {
  shiny::observeEvent(input$base_period, {
    new_periods <- periods[which(periods > input$base_period)]
    shiny::updateSelectInput(session, inputId = "comparison_period", choices = new_periods, selected = new_periods[length(new_periods)])
  })

  base_period <- shiny::reactiveVal()
  comparison_period <- shiny::reactiveVal()
  brand <- shiny::reactiveVal()
  shiny::observe({
    input$go
    base_period(as.numeric(isolate(input$base_period)))
    comparison_period(as.numeric(isolate(input$comparison_period)))
    brand(isolate(input$brand))
  })

  pre_summary_file <- shiny::eventReactive(c(base_period(), brand()), {
    SUMMARY_SummaryFile[which(SUMMARY_SummaryFile$PERIODCODE == base_period() & SUMMARY_SummaryFile$PrdLevel == brand()), ]
  })

  post_summary_file <- shiny::eventReactive(c(comparison_period(), brand()), {
    SUMMARY_SummaryFile[which(SUMMARY_SummaryFile$PERIODCODE == comparison_period() & SUMMARY_SummaryFile$PrdLevel == brand()), ]
  })

  pre_val <- shiny::eventReactive(pre_summary_file(), {
    pre_summary_file()[, "SALVAL"]
  })

  post_val <- shiny::eventReactive(post_summary_file(), {
    post_summary_file()[, "SALVAL"]
  })

  pre_vol <- shiny::eventReactive(pre_summary_file(), {
    pre_summary_file()[, "SALVOL"]
  })

  post_vol <- shiny::eventReactive(post_summary_file(), {
    post_summary_file()[, "SALVOL"]
  })

  pre_nd <- shiny::eventReactive(pre_summary_file(), {
    pre_summary_file()[, "ND"]
  })

  post_nd <- shiny::eventReactive(post_summary_file(), {
    post_summary_file()[, "ND"]
  })

  pre_wd <- shiny::eventReactive(pre_summary_file(), {
    pre_summary_file()[, "WD"]
  })

  post_wd <- shiny::eventReactive(post_summary_file(), {
    post_summary_file()[, "WD"]
  })

  pre_sah <- shiny::eventReactive(pre_summary_file(), {
    pre_summary_file()[, "SAH"]
  })

  post_sah <- shiny::eventReactive(post_summary_file(), {
    post_summary_file()[, "SAH"]
  })

  output$value_sales <- shiny::renderPlot({
    pre <- lubridate::ydm(base_period())
    pre <- paste0(toupper(lubridate::month(pre, label = T, abbr = T)), "'", substr(lubridate::year(pre), 3, 4))
    post <- lubridate::ydm(comparison_period())
    post <- paste0(toupper(lubridate::month(post, label = T, abbr = T)), "'", substr(lubridate::year(post), 3, 4))
    df <- data.frame(x = c(pre, post), y = c(pre_val(), post_val()), stringsAsFactors = F)
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
      ggplot2::geom_bar(stat = "identity", fill = "#B93333", width = 0.5) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept =  0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(mapping = ggplot2::aes(x, y, label = formatC(y, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", vjust = 1.25, size = 5)
  })

  output$volume_sales <- shiny::renderPlot({
    pre <- lubridate::ydm(base_period())
    pre <- paste0(toupper(lubridate::month(pre, label = T, abbr = T)), "'", substr(lubridate::year(pre), 3, 4))
    post <- lubridate::ydm(comparison_period())
    post <- paste0(toupper(lubridate::month(post, label = T, abbr = T)), "'", substr(lubridate::year(post), 3, 4))
    df <- data.frame(x = c(pre, post), y = c(pre_vol(), post_vol()), stringsAsFactors = F)
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
      ggplot2::geom_bar(stat = "identity", fill = "#5B9BD5", width = 0.5) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept =  0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(mapping = ggplot2::aes(x, y, label = formatC(y, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", vjust = 1.25, size = 5)
  })

  output$value_change <- shiny::renderUI({
    change <- (post_val()/pre_val()) - 1
    if (change < 0) {
      txt_change <- paste0(round(change * 100, 1), "%")
      htmltools::h1(txt_change, style = "color: white; text-align: center; background-color: #D83B01; padding: 10px 5px 10px 5px; border-radius: 10px;")
    } else {
      txt_change <- paste0("+", round(change * 100, 1), "%")
      htmltools::h1(txt_change, style = "color: white; text-align: center; background-color: #409640; padding: 10px 5px 10px 5px; border-radius: 10px;")
    }
  })

  output$volume_change <- shiny::renderUI({
    change <- (post_vol()/pre_vol()) - 1
    if (change < 0) {
      txt_change <- paste0(round(change * 100, 1), "%")
      htmltools::h1(txt_change, style = "color: white; text-align: center; background-color: #D83B01; padding: 10px 5px 10px 5px; border-radius: 10px;")
    } else {
      txt_change <- paste0("+", round(change * 100, 1), "%")
      htmltools::h1(txt_change, style = "color: white; text-align: center; background-color: #409640; padding: 10px 5px 10px 5px; border-radius: 10px;")
    }
  })

  output$numeric_distribution_pie <- shiny::renderPlot({
    df <- data.frame(x = factor(c(1, 0)), y = c(post_nd(), 1 - post_nd()))
    ggplot2::ggplot(df, ggplot2::aes(1, y, fill = x)) +
      ggplot2::geom_bar(stat = "identity", width = 0.4, show.legend = F) +
      ggplot2::scale_fill_manual(values = c("#E6E6E6", "#01B8AA")) +
      ggplot2::geom_text(x = 0, y = 1, label = paste0("ND\n", round(post_nd()*100, 1), "%"), family = "oswald", size = 10) +
      ggplot2::coord_polar(theta = "y", start = 0) +
      ggplot2::xlim(0, 1.2) +
      theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )
  })

  output$weighted_distribution_pie <- shiny::renderPlot({
    df <- data.frame(x = factor(c(1, 0)), y = c(post_wd(), 1 - post_wd()))
    ggplot2::ggplot(df, ggplot2::aes(1, y, fill = x)) +
      ggplot2::geom_bar(stat = "identity", width = 0.4, show.legend = F) +
      ggplot2::scale_fill_manual(values = c("#E6E6E6", "#01B8AA")) +
      ggplot2::geom_text(x = 0, y = 1, label = paste0("WD\n", round(post_wd() * 100, 1), "%"), family = "oswald", size = 10) +
      ggplot2::coord_polar(theta = "y", start = 0) +
      ggplot2::xlim(0, 1.2) +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )
  })

  output$sah_pie <- shiny::renderPlot({
    df <- data.frame(x = factor(c(1, 0)), y = c(post_sah(), 1 - post_sah()))
    ggplot2::ggplot(df, ggplot2::aes(1, y, fill = x)) +
      ggplot2::geom_bar(stat = "identity", width = 0.4, show.legend = F) +
      ggplot2::scale_fill_manual(values = c("#E6E6E6", "#01B8AA")) +
      ggplot2::geom_text(x = 0, y = 1, label = paste0("SAH\n", round(post_sah() * 100, 1), "%"), family = "oswald", size = 10) +
      ggplot2::coord_polar(theta = "y", start = 0) +
      ggplot2::xlim(0, 1.2) +
      theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )
  })

  output$numeric_distribution_change <- shiny::renderUI({
    y <- post_nd() - pre_nd()
    if (y > 0) {
      y <- paste0("+", round(y * 100, 1), "%")
    } else {
      y <- paste0(round(y * 100, 1), "%")
    }
    htmltools::h3(y, style = "color: white; text-align: center; background-color: rgb(0, 20, 84); padding: 10px 0px 10px 0px; border-radius: 10px; margin: -20px 0px 0px 0px;")
  })

  output$weighted_distribution_change <- shiny::renderUI({
    y <- post_wd() - pre_wd()
    if (y > 0) {
      y <- paste0("+", round(y * 100, 1), "%")
    } else {
      y <- paste0(round(y * 100, 1), "%")
    }
    htmltools::h3(y, style = "color: white; text-align: center; background-color: rgb(0, 20, 84); padding: 10px 0px 10px 0px; border-radius: 10px; margin: -20px 0px 0px 0px;")
  })

  output$sah_change <- shiny::renderUI({
    y <- post_sah() - pre_sah()
    if (y > 0) {
      y <- paste0("+", round(y * 100, 1), "%")
    } else {
      y <- paste0(round(y * 100, 1), "%")
    }
    htmltools::h3(y, style = "color: white; text-align: center; background-color: rgb(0, 20, 84); padding: 10px 0px 10px 0px; border-radius: 10px; margin: -20px 0px 0px 0px;")
  })

  output$stores <- shiny::renderPlot({
    pre <- lubridate::ydm(base_period())
    pre <- paste0(toupper(lubridate::month(pre, label = T, abbr = T)), "'", substr(lubridate::year(pre), 3, 4))
    post <- lubridate::ydm(comparison_period())
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "All India"), ]
    pre_stores <- dat[which(dat$RANGE_COL == "CONTINUOUS"), "POST_PROJN"] + dat[which(dat$RANGE_COL == "EXIT"), "PRE_PROJN"]
    post_stores <- dat[which(dat$RANGE_COL == "CONTINUOUS"), "POST_PROJN"] + dat[which(dat$RANGE_COL == "ENTRY"), "POST_PROJN"]
    post <- paste0(toupper(lubridate::month(post, label = T, abbr = T)), "'", substr(lubridate::year(post), 3, 4))
    df <- data.frame(x = c(pre, post), y = c(pre_stores, post_stores), stringsAsFactors = F)
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
      ggplot2::geom_bar(stat = "identity", fill = "#B93333", width = 0.5) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept =  0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(mapping = ggplot2::aes(x, y, label = formatC(y, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", vjust = 1.25, size = 5)
  })

  output$stores_change <- shiny::renderUI({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "All India"), ]
    pre_stores <- dat[which(dat$RANGE_COL == "CONTINUOUS"), "POST_PROJN"] + dat[which(dat$RANGE_COL == "EXIT"), "PRE_PROJN"]
    post_stores <- dat[which(dat$RANGE_COL == "CONTINUOUS"), "POST_PROJN"] + dat[which(dat$RANGE_COL == "ENTRY"), "POST_PROJN"]
    change <- post_stores - pre_stores
    if (change < 0) {
      txt_change <- paste0(round(change, 0), "K")
      htmltools::h1(txt_change, style = "color: white; text-align: center; background-color: #D83B01; padding: 10px 5px 10px 5px; border-radius: 10px;")
    } else {
      txt_change <- paste0("+", round(change, 0), "K")
      htmltools::h1(txt_change, style = "color: white; text-align: center; background-color: #409640; padding: 10px 5px 10px 5px; border-radius: 10px;")
    }
  })

  output$store_churn <- shiny::renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "All India"), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", -dat$PRE_PROJN, dat$POST_PROJN)
    dat <- dat[which(dat$RANGE_COL != "CONTINUOUS"), ]
    dat <- dplyr::select(dat, RANGE_COL, PROJN)
    new_vec <- colSums(dat[, c("PROJN"), drop = F])
    dat <- rbind(dat, data.frame(PROJN = new_vec["PROJN"], RANGE_COL = "TOTAL"))
    dat$RANGE_COL <- factor(dat$RANGE_COL, levels = c("ENTRY", "EXIT", "TOTAL"), labels = c("ENTRY STORES", "EXIT STORES", "NET"), ordered = T)
    dat <- dat[order(dat$RANGE_COL), ]
    dat$id <- seq(from = 1, by = 0.05, length.out = length(dat$RANGE_COL))
    dat$type <- ifelse(dat$PROJN > 0, "in", "out")
    dat[which(dat$RANGE_COL %in% c("NET")), "type"] <- "net"
    dat$end <- cumsum(dat$PROJN)
    dat$end <- c(head(dat$end, -1), 0)
    dat$start <- c(0, head(dat$end, -1))
    ggplot2::ggplot(dat, ggplot2::aes(fill = type)) +
      ggplot2::geom_hline(yintercept = 0, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin = id - 0.025, xmax = id + 0.025, ymin = end, ymax = start), show.legend = F, color = "white") +
      ggplot2::scale_fill_manual(values = c("net" = "#00183C", "in" = "#409640", "out" = "#D83B01")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", vjust = 1, size = 12),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_continuous(breaks = dat$id, labels = c("ENTRY\nSTORES", "EXIT\nSTORES", "NET"), expand = c(0.2, 0)) +
      ggplot2::geom_text(ggplot2::aes(x = id, y = (start + end) / 2, label = formatC(PROJN, format = "f", digits = 0, big.mark = ",")), family = "oswald", size = 5, colour = "white") +
      ggplot2::labs(x = "", y = "")
  })

  output$sales_churn <- shiny::renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "All India"), ]
    if (input$fact == "SALVAL") {
      dat$Fact <- dat$POST_SALVAL - dat$PRE_SALVAL
    } else {
      dat$Fact <- dat$POST_SALVOL - dat$PRE_SALVOL
    }
    dat <- dat[, c("RANGE_COL", "Fact")]
    new_vec <- sum(dat$Fact)
    new_dat <- data.frame(Fact = new_vec, RANGE_COL = "NET", stringsAsFactors = F)
    dat <- rbind(dat, new_dat)
    dat$STORE_TYPE <- factor(dat$RANGE_COL, levels = c("CONTINUOUS", "ENTRY", "EXIT", "NET"), labels = c("CONTINUOUS STORES", "ENTRY STORES", "EXIT STORES", "NET"), ordered = T)
    dat <- dat[order(dat$RANGE_COL), ]
    dat$id <- seq(from = 1, by = 0.2, length = length(dat$RANGE_COL))
    dat$type <- ifelse(dat[, "Fact"] > 0, "in", "out")
    dat[which(dat$STORE_TYPE %in% c("NET")), "type"] <- "net"
    dat$end <- cumsum(dat[, "Fact"])
    dat$end <- c(head(dat$end, -1), 0)
    dat$start <- c(0, head(dat$end, -1))
    dat$labs <- formatC(dat[, "Fact"], format = "f", digits = 0, big.mark = ",")
    ggplot2::ggplot(dat, ggplot2::aes(fill = type)) +
      ggplot2::geom_hline(yintercept = 0, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin = id - 0.1, xmax = id + 0.1, ymin = end, ymax = start), show.legend = F, color = "white") +
      ggplot2::scale_fill_manual(values = c("net" = "#00183C", "in" = "#409640", "out" = "#D83B01")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        axis.ticks = ggplot2::element_blank(),
        panel.border = element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", vjust = 1, size = 12),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(family = "oswald", face = "bold")
      ) +
      ggplot2::scale_x_continuous(breaks = dat$id, labels = c("CONTINUOUS\nSTORES", "ENTRY\nSTORES", "EXIT\nSTORES", "TOTAL"), expand = c(0.2, 0)) +
      ggplot2::geom_text(ggplot2::aes(x = id, y = (start + end) / 2, label = labs), family = "oswald", size = 5, colour = "white") +
      ggplot2::labs(x = "", y = "")
  })

  output$distribution_allIndia <- DT::renderDataTable({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "All India"), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN, dat$POST_PROJN)
    dat$SALVAL <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_SALVAL, dat$POST_SALVAL)
    dat$SALVOL <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_SALVOL, dat$POST_SALVOL)
    dat$WDVAL <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_WDVAL, dat$POST_WDVAL)
    dat$CAT_WDVAL <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_CAT_WDVAL, dat$POST_CAT_WDVAL)
    dat <- dat[, c("RANGE_COL", "PROJN", "SALVAL", "SALVOL", "WDVAL", "CAT_WDVAL", "PRE_SALVAL", "POST_SALVAL", "PRE_SALVOL", "POST_SALVOL")]
    temp_dat <- dat[which(dat$RANGE_COL != "EXIT"), ]
    temp_dat <- data.frame(RANGE_COL = "TOTAL", PROJN = sum(temp_dat$PROJN, na.rm = T), SALVAL = sum(temp_dat$SALVAL, na.rm = T), SALVOL = sum(temp_dat$SALVOL, na.rm = T), WDVAL = sum(temp_dat$WDVAL, na.rm = T), CAT_WDVAL = mean(temp_dat$CAT_WDVAL, na.rm = T), PRE_SALVAL = sum(dat$PRE_SALVAL, na.rm = T), POST_SALVAL = sum(dat$POST_SALVAL, na.rm = T), PRE_SALVOL = sum(dat$PRE_SALVOL, na.rm = T), POST_SALVOL = sum(dat$POST_SALVOL, na.rm = T), stringsAsFactors = F)
    dat <- rbind(temp_dat, dat)
    dat$RANGE_COL <- factor(dat$RANGE_COL, levels = c("TOTAL", "ENTRY", "CONTINUOUS", "EXIT"), labels = c("Total", "Entry Stores", "Continuous Stores", "Exit Stores"), ordered = T)
    dat <- dat[order(dat$RANGE_COL), ]
    dat$WD <- dat$WDVAL/dat$CAT_WDVAL
    dat$PDO <- (dat$SALVAL * 10^6)/(dat$PROJN * 10^3)
    dat$VAL_GROWTH <- ifelse(dat$RANGE_COL %in% c("Entry Stores", "Exit Stores"), NA, (dat$POST_SALVAL/dat$PRE_SALVAL) - 1)
    dat$VOL_GROWTH <- ifelse(dat$RANGE_COL %in% c("Entry Stores", "Exit Stores"), NA, (dat$POST_SALVOL/dat$PRE_SALVOL) - 1)
    dat <- dat[, c("RANGE_COL", "PROJN", "WD", "PDO", "VAL_GROWTH", "VOL_GROWTH")]
    rownames(dat) <- dat$RANGE_COL
    dat <- dat[, setdiff(names(dat), "RANGE_COL")]
    colnames(dat) <- c("STORES (IN '000)", "WD%", "PDO (IN RS.)", "VAL. GROWTH", "VOL. GROWTH")
    dat$TEMP <- c(1, 0, 1, 0)
    DT::datatable(
      dat,
      options = list(
        ordering = F,
        searching = F,
        info = F,
        paging = F,
        columnDefs = list(
          list(className = "dt-center", targets = 1:5),
          list(visible = F, targets = 6)
        )
      )
    ) %>%
      DT::formatPercentage(c(2, 4:5), digits = 1) %>%
      DT::formatRound(c(1, 3), digits = 0) %>%
      DT::formatStyle("TEMP", target = "row", backgroundColor = DT::styleEqual(c(0, 1), c('white', "#01B88A")), color = DT::styleEqual(c(0, 1), c("black", "white")))
  })

  output$stores_by_zones <- shiny::renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Zone"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = rev(zone_levs), ordered = T)
    dat <- dat[order(dat$Geography), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN, dat$POST_PROJN)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, PROJN)) +
      ggplot2::geom_bar(stat = "identity", fill = "#002050", width = 0.65)  +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::geom_text(ggplot2::aes(y = PROJN, label = formatC(PROJN, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", hjust = 1.25, size = 5) +
      ggplot2::coord_flip()
  })

  output$wd_by_zones <- DT::renderDataTable({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Zone"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = zone_levs, ordered = T)
    dat$WD <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_WDVAL/dat$PRE_CAT_WDVAL, dat$POST_WDVAL/dat$POST_CAT_WDVAL)
    dat$ND <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN/dat$PRE_CAT_PROJN, dat$POST_PROJN/dat$POST_CAT_PROJN)
    dat <- dat[order(dat$Geography), c("Geography", "WD", "ND")]
    rownames(dat) <- dat$Geography
    dat$Geography <- NULL
    names(dat) <- c("WD%", "ND%")
    i <- 1
    TEMP <- c()
    while (i <= nrow(dat)) {
      if (i == 1) {
        TEMP <- c(TEMP, TRUE)
      } else {
        TEMP <- c(TEMP, !TEMP[i - 1])
      }
      i <- i + 1
    }
    dat$TEMP <- as.numeric(TEMP)
    DT::datatable(
      dat,
      options = list(
        searching = F,
        ordering = F,
        info = F,
        paging = F,
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2)),
          list(visible = F, targets = 3)
        )
      )
    ) %>%
      DT::formatPercentage(c(1, 2), digits = 1) %>%
      DT::formatStyle(3, target = "row", backgroundColor = DT::styleEqual(c(0, 1), c("white", "#01B88A")), color = DT::styleEqual(c(0, 1), c("black", "white")))
  })

  output$stores_by_popstrata <- shiny::renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Popstrata"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = rev(popstrata_levs), ordered = T)
    dat <- dat[order(dat$Geography), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN, dat$POST_PROJN)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, PROJN)) +
      ggplot2::geom_bar(stat = "identity", fill = "#002050", width = 0.65)  +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::geom_text(ggplot2::aes(y = PROJN, label = formatC(PROJN, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", hjust = 1.25, size = 5) +
      ggplot2::coord_flip()
  })

  output$wd_by_popstrata <- DT::renderDataTable({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Popstrata"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = popstrata_levs, ordered = T)
    dat$WD <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_WDVAL/dat$PRE_CAT_WDVAL, dat$POST_WDVAL/dat$POST_CAT_WDVAL)
    dat$ND <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN/dat$PRE_CAT_PROJN, dat$POST_PROJN/dat$POST_CAT_PROJN)
    dat <- dat[order(dat$Geography), c("Geography", "WD", "ND")]
    rownames(dat) <- dat$Geography
    dat$Geography <- NULL
    names(dat) <- c("WD%", "ND%")
    i <- 1
    TEMP <- c()
    while (i <= nrow(dat)) {
      if (i == 1) {
        TEMP <- c(TEMP, TRUE)
      } else {
        TEMP <- c(TEMP, !TEMP[i - 1])
      }
      i <- i + 1
    }
    dat$TEMP <- as.numeric(TEMP)
    DT::datatable(
      dat,
      options = list(
        searching = F,
        ordering = F,
        info = F,
        paging = F,
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2)),
          list(visible = F, targets = 3)
        )
      )
    ) %>%
      DT::formatPercentage(c(1, 2), digits = 1) %>%
      DT::formatStyle(3, target = "row", backgroundColor = DT::styleEqual(c(0, 1), c("white", "#01B88A")), color = DT::styleEqual(c(0, 1), c("black", "white")))
  })

  output$stores_by_channel <- shiny::renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Channel"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = rev(channel_levs), ordered = T)
    dat <- dat[order(dat$Geography), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN, dat$POST_PROJN)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, PROJN)) +
      ggplot2::geom_bar(stat = "identity", fill = "#002050", width = 0.65)  +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::geom_text(ggplot2::aes(y = PROJN, label = formatC(PROJN, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", hjust = 1.25, size = 5) +
      ggplot2::coord_flip()
  })

  output$wd_by_channel <- DT::renderDataTable({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Channel"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = channel_levs, ordered = T)
    dat$WD <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_WDVAL/dat$PRE_CAT_WDVAL, dat$POST_WDVAL/dat$POST_CAT_WDVAL)
    dat$ND <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN/dat$PRE_CAT_PROJN, dat$POST_PROJN/dat$POST_CAT_PROJN)
    dat <- dat[order(dat$Geography), c("Geography", "WD", "ND")]
    rownames(dat) <- dat$Geography
    dat$Geography <- NULL
    names(dat) <- c("WD%", "ND%")
    i <- 1
    TEMP <- c()
    while (i <= nrow(dat)) {
      if (i == 1) {
        TEMP <- c(TEMP, TRUE)
      } else {
        TEMP <- c(TEMP, !TEMP[i - 1])
      }
      i <- i + 1
    }
    dat$TEMP <- as.numeric(TEMP)
    DT::datatable(
      dat,
      options = list(
        searching = F,
        ordering = F,
        info = F,
        paging = F,
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2)),
          list(visible = F, targets = 3)
        )
      )
    ) %>%
      DT::formatPercentage(c(1, 2), digits = 1) %>%
      DT::formatStyle(3, target = "row", backgroundColor = DT::styleEqual(c(0, 1), c("white", "#01B88A")), color = DT::styleEqual(c(0, 1), c("black", "white")))
  })

  output$stores_by_topdealers <- shiny::renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Top Dealers"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = rev(topdealers_levs), ordered = T)
    dat <- dat[order(dat$Geography), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN, dat$POST_PROJN)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, PROJN)) +
      ggplot2::geom_bar(stat = "identity", fill = "#002050", width = 0.65)  +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::geom_text(ggplot2::aes(y = PROJN, label = formatC(PROJN, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", hjust = 1.25, size = 5) +
      ggplot2::coord_flip()
  })

  output$wd_by_topdealers <- DT::renderDataTable({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "Top Dealers"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$Geography <- factor(dat$Geography, levels = topdealers_levs, ordered = T)
    dat$WD <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_WDVAL/dat$PRE_CAT_WDVAL, dat$POST_WDVAL/dat$POST_CAT_WDVAL)
    dat$ND <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN/dat$PRE_CAT_PROJN, dat$POST_PROJN/dat$POST_CAT_PROJN)
    dat <- dat[order(dat$Geography), c("Geography", "WD", "ND")]
    rownames(dat) <- dat$Geography
    dat$Geography <- NULL
    names(dat) <- c("WD%", "ND%")
    i <- 1
    TEMP <- c()
    while (i <= nrow(dat)) {
      if (i == 1) {
        TEMP <- c(TEMP, TRUE)
      } else {
        TEMP <- c(TEMP, !TEMP[i - 1])
      }
      i <- i + 1
    }
    dat$TEMP <- as.numeric(TEMP)
    DT::datatable(
      dat,
      options = list(
        searching = F,
        ordering = F,
        info = F,
        paging = F,
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2)),
          list(visible = F, targets = 3)
        )
      )
    ) %>%
      DT::formatPercentage(c(1, 2), digits = 1) %>%
      DT::formatStyle(3, target = "row", backgroundColor = DT::styleEqual(c(0, 1), c("white", "#01B88A")), color = DT::styleEqual(c(0, 1), c("black", "white")))
  })

  output$stores_by_states <- renderPlot({
    dat <- DISTRIBUTION_DistributionFile[which(DISTRIBUTION_DistributionFile$PrdLevel == brand() & DISTRIBUTION_DistributionFile$PRE == base_period() & DISTRIBUTION_DistributionFile$POST == comparison_period() & DISTRIBUTION_DistributionFile$Cut == "State"), ]
    dat <- dat[which(dat$RANGE_COL == strsplit(input$distribution_store_type, split = " ")[[1]][1]), ]
    dat$PROJN <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN, dat$POST_PROJN)
    dat$WD <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_WDVAL/dat$PRE_CAT_WDVAL, dat$POST_WDVAL/dat$POST_CAT_WDVAL)
    dat$ND <- ifelse(dat$RANGE_COL == "EXIT", dat$PRE_PROJN/dat$PRE_CAT_PROJN, dat$POST_PROJN/dat$POST_CAT_PROJN)
    dat$wd_nd <- paste0("ND: ", round(dat$ND * 100, 1), "%; WD: ", round(dat$WD * 100, 1), "%")
    dat <- dat[order(-dat$PROJN), ]
    dat$horizontal_adjust <- c(1.25, rep(-0.2, nrow(dat) - 1))
    dat$Short <- factor(dat$Short, levels = rev(dat$Short), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(x = Short, y = 0)) +
      ggplot2::geom_segment(mapping = ggplot2::aes(xend = Short, yend = PROJN), size = 1, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = Short, y = PROJN), size = 10, color = "#28738A") +
      ggplot2::geom_text(mapping = ggplot2::aes(x = Short, y = PROJN, label = formatC(PROJN, format = "f", digits = 0, big.mark = ",")), color = "white", family = "oswald", size = 5) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = Short, y = PROJN, label = wd_nd, hjust =  horizontal_adjust), family = "oswald", label.size = 0, label.r = unit(1, "mm"), color = rgb(160, 160, 160, max = 255)) +
      ggplot2::geom_hline(yintercept = 0, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = Short, y = 0, label = Short), hjust = 1.3, size = 4, family = "oswald", fontface = "bold") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white"),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      ) +
      ggplot2::coord_flip()
  })

  DISTRIBUTION_TopDealersFile_Subset <- shiny::reactiveVal(
    value = DISTRIBUTION_TopDealersFile[which(DISTRIBUTION_TopDealersFile$Pareto_Store_Flag == "Top 50"), ]
  )

  shiny::observeEvent(c(input$next30), {
    DISTRIBUTION_TopDealersFile_Subset(DISTRIBUTION_TopDealersFile[which(DISTRIBUTION_TopDealersFile$Pareto_Store_Flag == "Next 30"), ])
  })

  shiny::observeEvent(c(input$bottom20), {
    DISTRIBUTION_TopDealersFile_Subset(DISTRIBUTION_TopDealersFile[which(DISTRIBUTION_TopDealersFile$Pareto_Store_Flag == "Bottom 20"), ])
  })

  shiny::observeEvent(c(input$top50), {
    DISTRIBUTION_TopDealersFile_Subset(DISTRIBUTION_TopDealersFile[which(DISTRIBUTION_TopDealersFile$Pareto_Store_Flag == "Top 50"), ])
  })

  output$topdealers_nd <- shinydashboard::renderInfoBox({
    dat <- DISTRIBUTION_TopDealersFile_Subset()[which(DISTRIBUTION_TopDealersFile_Subset()$PRE == base_period() & DISTRIBUTION_TopDealersFile_Subset()$POST == comparison_period() & DISTRIBUTION_TopDealersFile_Subset()$PrdLevel == brand() & DISTRIBUTION_TopDealersFile_Subset()$Cut == "All India"), ]
    nd <- dat$POST_PROJN/dat$POST_CAT_PROJN
    nd <- paste0(round(nd * 100, 1), "%")
    nd_change <- (dat$POST_PROJN/dat$POST_CAT_PROJN) - (dat$PRE_PROJN/dat$PRE_CAT_PROJN)
    clr <- NULL
    icn <- NULL
    if (nd_change < 0) {
      icn <- shiny::icon("chevron-down")
      clr <- "maroon"
      nd_change <- h3(paste0(round(nd_change * 100, 1), "%"), style = "font-size: 98%; margin: 0px 0px -10px 0px; color: red;")
    } else {
      icn <- shiny::icon("chevron-up")
      clr <- "green"
      nd_change <- h3(paste0("+", round(nd_change * 100, 1), "%"), style = "font-size: 98%; margin: 0px 0px -10px 0px; color: green;")
    }
    shinydashboard::infoBox(title = h4("ND", style = "margin: -1px 0px 0px 0px;"), value = h2(nd, style = "margin: 5px 0px 0px 0px"), subtitle = nd_change, icon = icn, color = clr)
  })

  output$topdealers_wd <- shinydashboard::renderInfoBox({
    dat <- DISTRIBUTION_TopDealersFile_Subset()[which(DISTRIBUTION_TopDealersFile_Subset()$PRE == base_period() & DISTRIBUTION_TopDealersFile_Subset()$POST == comparison_period() & DISTRIBUTION_TopDealersFile_Subset()$PrdLevel == brand() & DISTRIBUTION_TopDealersFile_Subset()$Cut == "All India"), ]
    wd <- dat$POST_WDVAL/dat$POST_CAT_WDVAL
    wd <- paste0(round(wd * 100, 1), "%")
    wd_change <- (dat$POST_WDVAL/dat$POST_CAT_WDVAL) - (dat$PRE_WDVAL/dat$PRE_CAT_WDVAL)
    clr <- NULL
    icn <- NULL
    if (wd_change < 0) {
      icn <- shiny::icon("chevron-down")
      clr <- "maroon"
      wd_change <- h3(paste0(round(wd_change * 100, 1), "%"), style = "font-size: 98%; margin: 0px 0px -10px 0px; color: red;")
    } else {
      icn <- shiny::icon("chevron-up")
      clr <- "green"
      wd_change <- h3(paste0("+", round(wd_change * 100, 1), "%"), style = "font-size: 98%; margin: 0px 0px -10px 0px; color: green;")
    }
    shinydashboard::infoBox(title = h4("WD", style = "margin: -1px 0px 0px 0px;"), value = h2(wd, style = "margin: 5px 0px 0px 0px"), subtitle = wd_change, icon = icn, color = clr)
  })

  output$topdealers_valchange <- shinydashboard::renderInfoBox({
    dat <- DISTRIBUTION_TopDealersFile_Subset()[which(DISTRIBUTION_TopDealersFile_Subset()$PRE == base_period() & DISTRIBUTION_TopDealersFile_Subset()$POST == comparison_period() & DISTRIBUTION_TopDealersFile_Subset()$PrdLevel == brand() & DISTRIBUTION_TopDealersFile_Subset()$Cut == "All India"), ]
    val_change <- (dat$POST_SALVAL/dat$PRE_SALVAL) - 1
    clr <- NULL
    icn <- NULL
    if (val_change < 0) {
      icn <- shiny::icon("chevron-down")
      clr <- "maroon"
      val_change <- h2(paste0(round(val_change * 100, 1), "%"), style = "margin: 5px 0px 0px 0px; color: red;")
    } else {
      icn <- shiny::icon("chevron-up")
      clr <- "green"
      val_change <- h2(paste0("+", round(val_change * 100, 1), "%"), style = "margin: 5px 0px 0px 0px; color: green;")
    }
    shinydashboard::infoBox(title = h4("VAL. GROWTH", style = "margin: -1px 0px 0px 0px;"), value = val_change, icon = icn, color = clr)
  })

  output$topdealers_volchange <- shinydashboard::renderInfoBox({
    dat <- DISTRIBUTION_TopDealersFile_Subset()[which(DISTRIBUTION_TopDealersFile_Subset()$PRE == base_period() & DISTRIBUTION_TopDealersFile_Subset()$POST == comparison_period() & DISTRIBUTION_TopDealersFile_Subset()$PrdLevel == brand() & DISTRIBUTION_TopDealersFile_Subset()$Cut == "All India"), ]
    vol_change <- (dat$POST_SALVOL/dat$PRE_SALVOL) - 1
    clr <- NULL
    icn <- NULL
    if (vol_change < 0) {
      icn <- shiny::icon("chevron-down")
      clr <- "maroon"
      vol_change <- h2(paste0(round(vol_change * 100, 1), "%"), style = "margin: 5px 0px 0px 0px; color: red;")
    } else {
      icn <- shiny::icon("chevron-up")
      clr <- "green"
      vol_change <- h2(paste0("+", round(vol_change * 100, 1), "%"), style = "margin: 5px 0px 0px 0px; color: green;")
    }
    shinydashboard::infoBox(title = h4("VOL. GROWTH", style = "margin: -1px 0px 0px 0px;"), value = vol_change, icon = icn, color = clr)
  })

  output$topdealers_state <- ggiraph::renderggiraph({
    dat <- DISTRIBUTION_TopDealersFile_Subset()[which(DISTRIBUTION_TopDealersFile_Subset()$PRE == base_period() & DISTRIBUTION_TopDealersFile_Subset()$POST == comparison_period() & DISTRIBUTION_TopDealersFile_Subset()$PrdLevel == brand() & DISTRIBUTION_TopDealersFile_Subset()$Cut == "State"), ]
    dat$GROWTH <- (dat$POST_PROJN/dat$PRE_PROJN) - 1
    dat <- dat[order(-dat$GROWTH), ]
    dat$Short <- factor(dat$Short, levels = dat$Short, ordered = T)
    dat <- dat[, c("Short", "GROWTH", "POST_PROJN")]
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = Short, y = GROWTH, size = POST_PROJN)) +
      ggplot2::geom_segment(ggplot2::aes(x = Short, xend = Short, y = 0, yend = GROWTH), size = 1, colour = alpha(rgb(166, 166, 166, max = 255), 1)) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(tooltip = paste0(Short, "\nGrowth: ", round(GROWTH * 100, 1), "%\nStores: ", formatC(POST_PROJN, format = "f", digits = 0, big.mark = ","), "K")), show.legend = F, alpha = 0.7, colour = "#B63679FF") +
      ggplot2::scale_size_continuous(range = c(1, 20)) +
      ggplot2::labs(
        y = "Growth in Number of Stores",
        x = "",
        caption = "Size by Number of Stores in Comparison Period"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        axis.text.y = ggplot2::element_text(family = "oswald"),
        axis.text.x = ggplot2::element_text(family = "oswald"),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(family = "oswald", face = "bold"),
        axis.title.x = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(family = "oswald")
      )
    ggiraph::ggiraph(ggobj = p, width = 1)
  })

  output$cat_sku <- shiny::renderPlot({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "All India"), ]
    pre <- lubridate::ydm(base_period())
    pre <- paste0(toupper(lubridate::month(pre, label = T, abbr = T)), "'", substr(lubridate::year(pre), 3, 4))
    post <- lubridate::ydm(comparison_period())
    post <- paste0(toupper(lubridate::month(post, label = T, abbr = T)), "'", substr(lubridate::year(post), 3, 4))
    pre_sku <- dat$PRE_CAT_AVG_SKU
    post_sku <- dat$POST_CAT_AVG_SKU
    df <- data.frame(x = c(pre, post), y = c(pre_sku, post_sku), stringsAsFactors = F)
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
      ggplot2::geom_bar(stat = "identity", fill = alpha("#ECC846", 0.9), width = 0.5) +
      ggplot2::labs(x = "", y = "Number of SKUs") +
      ggplot2::geom_hline(yintercept =  0, color = rgb(166, 166, 166, max = 255)) +
      theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.title.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12)
      ) +
      ggplot2::geom_text(mapping = ggplot2::aes(x, y, label = formatC(y, format = "f", digits = 1, big.mark = ",")), family = "oswald", colour = "white", vjust = 1.25, size = 5)
  })

  output$brd_sku <- shiny::renderPlot({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "All India"), ]
    pre <- lubridate::ydm(base_period())
    pre <- paste0(toupper(lubridate::month(pre, label = T, abbr = T)), "'", substr(lubridate::year(pre), 3, 4))
    post <- lubridate::ydm(comparison_period())
    post <- paste0(toupper(lubridate::month(post, label = T, abbr = T)), "'", substr(lubridate::year(post), 3, 4))
    pre_sku <- dat$PRE_AVG_SKU
    post_sku <- dat$POST_AVG_SKU
    df <- data.frame(x = c(pre, post), y = c(pre_sku, post_sku), stringsAsFactors = F)
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
      ggplot2::geom_bar(stat = "identity", fill = alpha("#4A588A", 0.9), width = 0.5) +
      ggplot2::labs(x = "", y = "Number of SKUs") +
      ggplot2::geom_hline(yintercept =  0, colour = rgb(166, 166, 166, max = 255)) +
      theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.title.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12)
      ) +
      ggplot2::geom_text(mapping = ggplot2::aes(x, y, label = formatC(y, format = "f", digits = 1, big.mark = ",")), family = "oswald", colour = "white", vjust = 1.25, size = 5)
  })

  output$icon_cat_sku <- shiny::renderUI({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "All India"), ]
    change <- dat$POST_CAT_AVG_SKU - dat$PRE_CAT_AVG_SKU
    if (change < 0) {
      htmltools::h2(shiny::icon("chevron-down", class = "fa-5x"), style = "text-align: center; margin: -10px 0px 0px 0px;")
    } else {
      htmltools::h2(shiny::icon("chevron-up", class = "fa-5x"), style = "text-align: center; margin: -10px 0px 0px 0px;")
    }
  })

  output$icon_brd_sku <- shiny::renderUI({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "All India"), ]
    change <- dat$POST_AVG_SKU - dat$PRE_AVG_SKU
    if (change < 0) {
      htmltools::h2(shiny::icon("chevron-down", class = "fa-5x"), style = "text-align: center; margin: -10px 0px 0px 0px;")
    } else {
      htmltools::h2(shiny::icon("chevron-up", class = "fa-5x"), style = "text-align: center; margin: -10px 0px 0px 0px;")
    }
  })

  output$change_cat_sku <- shiny::renderUI({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "All India"), ]
    change <- dat$POST_CAT_AVG_SKU - dat$PRE_CAT_AVG_SKU
    if (change < 0) {
      txt_change <- paste0(round(change, 1), " SKU")
      htmltools::h2(txt_change, style = "color: black; text-align: center; background-color: white; margin: -30px 0px 0px 0px;")
    } else {
      txt_change <- paste0("+", round(change, 1), " SKU")
      htmltools::h2(txt_change, style = "color: black; text-align: center; background-color: white; margin: -30px 0px 0px 0px;")
    }
  })

  output$change_brd_sku <- shiny::renderUI({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "All India"), ]
    change <- dat$POST_AVG_SKU - dat$PRE_AVG_SKU
    if (change < 0) {
      txt_change <- paste0(round(change, 1), " SKU")
      htmltools::h2(txt_change, style = "color: black; text-align: center; background-color: white; margin: -30px 0px 0px 0px;")
    } else {
      txt_change <- paste0("+", round(change, 1), " SKU")
      htmltools::h2(txt_change, style = "color: black; text-align: center; background-color: white; margin: -30px 0px 0px 0px;")
    }
  })

  output$cat_sku_cut <- renderPlot({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == input$asst_cut), ]
    if (input$asst_cut == "Zone") {
      dat$Geography <- factor(dat$Geography, levels = rev(zone_levs), ordered = T)
    } else if (input$asst_cut == "Popstrata") {
      dat$Geography <- factor(dat$Geography, levels = rev(popstrata_levs), ordered = T)
    } else if (input$asst_cut == "Top Dealers") {
      dat$Geography <- factor(dat$Geography, levels = rev(topdealers_levs), ordered = T)
    } else {
      dat$Geography <- factor(dat$Geography, levels = rev(channel_levs), ordered = T)
    }
    dat <- dat %>% mutate(Flag = ifelse(POST_CAT_AVG_SKU > PRE_CAT_AVG_SKU, "UP", "DOWN"))
    dat$labs <- ifelse(dat$POST_CAT_AVG_SKU > dat$PRE_CAT_AVG_SKU, paste0("+", round(dat$POST_CAT_AVG_SKU - dat$PRE_CAT_AVG_SKU, 1)), round(dat$POST_CAT_AVG_SKU - dat$PRE_CAT_AVG_SKU, 1))
    ggplot(dat, aes(x = POST_CAT_AVG_SKU, y = Geography)) +
      geom_segment(aes(x = PRE_CAT_AVG_SKU, xend = POST_CAT_AVG_SKU, y = Geography, yend = Geography, colour = Flag), size = 1) +
      geom_point(aes(colour = "post"), size = 3) +
      geom_point(aes(x = PRE_CAT_AVG_SKU, y = Geography, colour = "pre"), inherit.aes = F, size = 3) +
      geom_text(aes(y = Geography, x = (PRE_CAT_AVG_SKU + POST_CAT_AVG_SKU)/2, label = paste(round(POST_CAT_AVG_SKU - PRE_CAT_AVG_SKU, 1))), vjust = -0.5, family = "oswald", size = 5) +
      theme_bw() +
      labs(
        x = "Average SKU Count",
        y = "",
        colour = "Average SKU Count in:"
      ) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(family = "oswald", size = 12, face = "bold"),
        legend.text = element_text(family = "oswald", size = 12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "oswald", size = 12),
        axis.text.y = element_text(family = "oswald", size = 12, face = "bold"),
        axis.title.x = element_text(family = "oswald", face = "bold", size = 12)
      ) +
      scale_colour_manual(breaks = c("pre", "post"), labels = c("Base Period", "Comparison Period"), values = c("pre" = "#ECC846", "post" = "#4A588A", "DOWN" = "#D83B01", "UP" = "#409640"))
  })

  output$brd_sku_cut <- renderPlot({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == input$asst_cut), ]
    if (input$asst_cut == "Zone") {
      dat$Geography <- factor(dat$Geography, levels = rev(zone_levs), ordered = T)
    } else if (input$asst_cut == "Popstrata") {
      dat$Geography <- factor(dat$Geography, levels = rev(popstrata_levs), ordered = T)
    } else if (input$asst_cut == "Top Dealers") {
      dat$Geography <- factor(dat$Geography, levels = rev(topdealers_levs), ordered = T)
    } else {
      dat$Geography <- factor(dat$Geography, levels = rev(channel_levs), ordered = T)
    }
    dat <- dat %>% mutate(Flag = ifelse(POST_AVG_SKU > PRE_AVG_SKU, "UP", "DOWN"))
    dat$labs <- ifelse(dat$POST_AVG_SKU > dat$PRE_AVG_SKU, paste0("+", round(dat$POST_AVG_SKU - dat$PRE_AVG_SKU, 1)), round(dat$POST_AVG_SKU - dat$PRE_AVG_SKU, 1))
    ggplot(dat, aes(x = POST_AVG_SKU, y = Geography)) +
      geom_segment(aes(x = PRE_AVG_SKU, xend = POST_AVG_SKU, y = Geography, yend = Geography, colour = Flag), size = 1) +
      geom_point(aes(colour = "post"), size = 3) +
      geom_point(aes(x = PRE_AVG_SKU, y = Geography, colour = "pre"), inherit.aes = F, size = 3) +
      geom_text(aes(y = Geography, x = (PRE_AVG_SKU + POST_AVG_SKU)/2, label = labs), vjust = -0.5, family = "oswald", size = 5) +
      theme_bw() +
      labs(
        x = "Average SKU Count",
        y = "",
        colour = "Average SKU Count in:"
      ) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(family = "oswald", size = 12, face = "bold"),
        legend.text = element_text(family = "oswald", size = 12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(family = "oswald", size = 12),
        axis.text.y = element_text(family = "oswald", size = 12, face = "bold"),
        axis.title.x = element_text(family = "oswald", face = "bold", size = 12)
      ) +
      scale_colour_manual(breaks = c("pre", "post"), labels = c("Base Period", "Comparison Period"), values = c("pre" = "#ECC846", "post" = "#4A588A", "DOWN" = "#D83B01", "UP" = "#409640"))
  })

  output$cat_sku_state <- renderPlot({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "State"), ]
    dat <- dat %>% mutate(Flag = ifelse(POST_CAT_AVG_SKU > PRE_CAT_AVG_SKU, "UP", "DOWN"))
    dat$Geography <- factor(dat$Geography, levels = rev(dat$Geography), ordered = T)
    dat$labs <- ifelse(dat$POST_CAT_AVG_SKU > dat$PRE_CAT_AVG_SKU, paste0("+", round(dat$POST_CAT_AVG_SKU - dat$PRE_CAT_AVG_SKU, 1)), round(dat$POST_CAT_AVG_SKU - dat$PRE_CAT_AVG_SKU, 1))
    ggplot(dat, aes(x = POST_CAT_AVG_SKU, y = Geography)) +
      geom_segment(aes(x = PRE_CAT_AVG_SKU, xend = POST_CAT_AVG_SKU, y = Geography, yend = Geography, colour = Flag), size = 1) +
      geom_point(aes(colour = "post"), size = 3) +
      geom_point(aes(x = PRE_CAT_AVG_SKU, y = Geography, colour = "pre"), inherit.aes = F, size = 3) +
      geom_text(aes(y = Geography, x = ifelse(PRE_CAT_AVG_SKU > POST_CAT_AVG_SKU, PRE_CAT_AVG_SKU, POST_CAT_AVG_SKU), label = labs), hjust = -0.5, family = "oswald", size = 4) +
      theme_bw() +
      labs(
        x = "Average SKU Count",
        y = "",
        colour = "Average SKU Count in:"
      ) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(family = "oswald", size = 12, face = "bold"),
        legend.text = element_text(family = "oswald", size = 12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(family = "oswald", size = 12),
        axis.text.y = element_text(family = "oswald", size = 12, face = "bold"),
        axis.title.x = element_text(family = "oswald", face = "bold", size = 12)
      ) +
      scale_colour_manual(breaks = c("pre", "post"), labels = c("Base Period", "Comparison Period"), values = c("pre" = "#ECC846", "post" = "#4A588A", "DOWN" = "#D83B01", "UP" = "#409640"))
  })

  output$brd_sku_state <- renderPlot({
    dat <- ASSORTMENT_AssortmentFile[which(ASSORTMENT_AssortmentFile$PrdLevel == brand() & ASSORTMENT_AssortmentFile$PRE == base_period() & ASSORTMENT_AssortmentFile$POST == comparison_period() & ASSORTMENT_AssortmentFile$Cut == "State"), ]
    dat <- dat %>% mutate(Flag = ifelse(POST_AVG_SKU > PRE_AVG_SKU, "UP", "DOWN"))
    dat$Geography <- factor(dat$Geography, levels = rev(dat$Geography), ordered = T)
    dat$labs <- ifelse(dat$POST_AVG_SKU > dat$PRE_AVG_SKU, paste0("+", round(dat$POST_AVG_SKU - dat$PRE_AVG_SKU, 1)), round(dat$POST_AVG_SKU - dat$PRE_AVG_SKU, 1))
    ggplot(dat, aes(x = POST_AVG_SKU, y = Geography)) +
      geom_segment(aes(x = PRE_AVG_SKU, xend = POST_AVG_SKU, y = Geography, yend = Geography, colour = Flag), size = 1) +
      geom_point(aes(colour = "post"), size = 3) +
      geom_point(aes(x = PRE_AVG_SKU, y = Geography, colour = "pre"), inherit.aes = F, size = 3) +
      geom_text(aes(y = Geography, x = ifelse(PRE_AVG_SKU > POST_AVG_SKU, PRE_AVG_SKU, POST_AVG_SKU), label = labs), hjust = -0.5, family = "oswald", size = 4) +
      theme_bw() +
      labs(
        x = "Average SKU Count",
        y = "",
        colour = "Average SKU Count in:"
      ) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(family = "oswald", size = 12, face = "bold"),
        legend.text = element_text(family = "oswald", size = 12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "oswald", size = 12),
        axis.text.y = element_text(family = "oswald", size = 12, face = "bold"),
        axis.title.x = element_text(family = "oswald", face = "bold", size = 12)
      ) +
      scale_colour_manual(breaks = c("pre", "post"), labels = c("Base Period", "Comparison Period"), values = c("pre" = "#ECC846", "post" = "#4A588A", "DOWN" = "#D83B01", "UP" = "#409640"))
  })

  output$transition_ai <- shiny::renderPlot({
    dat <- ASSORTMENT_TransitionFile[which(ASSORTMENT_TransitionFile$PRE == base_period() & ASSORTMENT_TransitionFile$POST == comparison_period() & ASSORTMENT_TransitionFile$PrdLevel == brand() & ASSORTMENT_TransitionFile$Cut == input$store_type), ]
    dat <- dat %>% select(PRE_CNT_SKU, POST_CNT_SKU, PROJN) %>% spread(POST_CNT_SKU, PROJN, fill = 0)
    x <- rowSums(dat[, -1])
    for (col in 2:ncol(dat)) {
      dat[, col] <- dat[, col]*100/x
    }
    dat$segpct <- x*100/sum(x)
    dat$xmax <- cumsum(dat$segpct)
    dat$xmin <- dat$xmax - dat$segpct
    dat$segpct <- NULL
    output2 <- dat %>% gather(key = "variable", value = "value", -xmax, -xmin, -PRE_CNT_SKU) %>% filter(value > 0)
    output2 <- plyr::ddply(output2, "PRE_CNT_SKU", transform, ymax = cumsum(value))
    output2 <- plyr::ddply(output2, "PRE_CNT_SKU", transform, ymin = ymax - value)
    output2$xtext <- output2$xmin + ((output2$xmax - output2$xmin)/2)
    output2$ytext <- output2$ymin + ((output2$ymax - output2$ymin)/2)
    ggplot(output2, aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable)) +
      geom_rect(colour = "white") +
      geom_text(aes(x = xtext, y = ytext, label = paste0(round(value, 0), "%")), size = 5, colour = "white", family = "oswald") +
      geom_text(aes(x = xtext, y = 0, label = PRE_CNT_SKU), size = 5, vjust = 1.25, family = "oswald") +
      scale_fill_viridis_d(alpha = 0.8) +
      labs(
        x = "Number of SKUs in Base Period",
        fill = "Number of SKUs\nin Comparison Period",
        y = "",
        caption = "Values: Proportion of stores transitioning from a particular stocking pattern in base period to other in comparison period
        Size of bars: Number of Stores with a particular stocking pattern in base period"
      ) +
      theme(
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.title.align = 0,
        axis.title = element_text(family = "oswald", size = 12),
        legend.title = element_text(family = "oswald", face = "bold", size = 12),
        legend.text = element_text(family = "oswald", size = 12),
        plot.caption = element_text(family = "oswald", size = 11)
      )
  })

  output$top_skus <- DT::renderDataTable({
    dat <- ASSORTMENT_SKUFile[which(ASSORTMENT_SKUFile$PRE == base_period() & ASSORTMENT_SKUFile$POST == comparison_period() & ASSORTMENT_SKUFile$PrdLevel == brand() & ASSORTMENT_SKUFile$Cut == input$store_type2), ]
    dat$POST_ND <- dat$POST_PROJN/dat$POST_CAT_PROJN
    dat$CHANGE_ND <- (dat$POST_PROJN/dat$POST_CAT_PROJN) - (dat$PRE_PROJN/dat$PRE_CAT_PROJN)
    dat$POST_WD <- dat$POST_WDVAL/dat$POST_CAT_WDVAL
    dat$CHANGE_WD <- (dat$POST_WDVAL/dat$POST_CAT_WDVAL) - (dat$PRE_WDVAL/dat$PRE_CAT_WDVAL)
    dat$SALVAL <- dat$POST_SALVAL
    dat$VAL_GROWTH <- (dat$POST_SALVAL/dat$PRE_SALVAL) - 1
    dat$SALVOL <- dat$POST_SALVOL
    dat$VOL_GROWTH <- (dat$POST_SALVOL/dat$PRE_SALVOL) - 1
    dat <- dat[, c("POST_ND", "CHANGE_ND", "POST_WD", "CHANGE_WD", "SALVAL", "VAL_GROWTH", "SALVOL", "VOL_GROWTH", "SKU")]
    rownames(dat) <- dat$SKU
    dat$SKU <- NULL
    i <- 1
    TEMP <- c()
    while (i <= nrow(dat)) {
      if (i == 1) {
        TEMP <- c(TEMP, TRUE)
      } else {
        TEMP <- c(TEMP, !TEMP[i - 1])
      }
      i <- i + 1
    }
    dat$TEMP <- as.numeric(TEMP)
    colnames(dat) <- c("ND%", "Change in ND%", "WD%", "Change in WD%", "Sales Value (in Million Rs.)", "Value Growth", "Sales Volume (in Tonnes)", "Volume Growth", "TEMP")
    DT::datatable(
      dat,
      options = list(
        searching = F,
        ordering = F,
        info = F,
        paging = F,
        columnDefs = list(
          list(className = "dt-center", targets = 1:9),
          list(visible = F, targets = 9)
        )
      )
    ) %>%
      DT::formatPercentage(c(1, 2, 3, 4, 6, 8), digits = 1) %>%
      DT::formatRound(c(5, 7), digits = 0, interval = 3, mark = ",") %>%
      DT::formatStyle(9, target = "row", backgroundColor = DT::styleEqual(c(0, 1), c("white", "#01B88A")), color = DT::styleEqual(c(0, 1), c("black", "white")))
  })

  output$delta_int <- shiny::renderUI({
    unit_of_change <- ifelse(input$gl_type == "SALVAL", "MILLION RS.", "TONNES")
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "All India" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Flag", "shift")]
    shift <- sum(dat$shift)
    if (shift < 0) {
      txt <- HTML(paste0(formatC(shift, format = "f", digits = 0, big.mark = ","), "<br>", unit_of_change))
      htmltools::h3(txt, style = "background-color: #D83B01; color: white; padding: 10px 0px 10px 0px; border-radius: 0px;")
    } else {
      txt <- HTML("+", paste0(formatC(shift, format = "f", digits = 0, big.mark = ","), "<br>", unit_of_change))
      htmltools::h3((txt), style = "background-color: #409640; color: white; padding: 10px 0px 10px 0px; border-radius: 0px;")
    }
  })

  output$shift_split <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "All India" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Flag", "shift")]
    temp_dat <- data.frame(Flag = "Net", shift = sum(dat$shift))
    dat <- rbind(dat, temp_dat)
    dat$Flag <- factor(dat$Flag, levels = c("Gain", "Loss", "Net"), ordered = T)
    dat <- dat[order(dat$Flag), ]
    dat$id <- seq(from = 1, by = 0.05, length.out = length(dat$Flag))
    dat$type <- ifelse(dat$shift < 0, "out", "in")
    dat[which(dat$Flag == "Net"), "type"] <- "net"
    dat$end <- cumsum(dat$shift)
    dat$end <- c(head(dat$end, -1), 0)
    dat$start <- c(0, head(dat$end, -1))
    ggplot2::ggplot(dat, ggplot2::aes(fill = type)) +
      ggplot2::geom_hline(yintercept = 0, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin = id - 0.025, xmax = id + 0.025, ymin = end, ymax = start), show.legend = F, color = "white") +
      ggplot2::scale_fill_manual(values = c("net" = "#00183C", "in" = "#409640", "out" = "#D83B01")) +
      ggplot2::labs(
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10),
        axis.ticks = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", vjust = 1, size = 12),
        axis.text.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_continuous(breaks = dat$id, labels = c("Gains", "Losses", "Net"), expand = c(0.7, 0)) +
      ggplot2::geom_text(ggplot2::aes(x = id, y = (start + end) / 2, label = formatC(shift, format = "f", digits = 0, big.mark = ",")), family = "oswald", size = 5, colour = "white") +
      ggplot2::labs(x = "", y = "")
  })

  output$split_by_zones <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "Zone" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Geography", "Flag", "shift")]
    dat <- dat %>% group_by(Geography) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% mutate(horizontal_adjust = ifelse(shift < 0, -0.25, 1.25))
    dat$Geography <- factor(dat$Geography, levels = rev(zone_levs), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, shift)) +
      ggplot2::geom_bar(stat = "identity", fill = "#5C2D91", width = 0.65) +
      ggplot2::labs(
        x = "",
        y = "",
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        # panel.grid = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white"),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10)
      ) +
      ggplot2::geom_text(ggplot2::aes(y = shift, label = formatC(shift, format = "f", digits = 0, big.mark = ","), hjust = horizontal_adjust), family = "oswald", colour = "white", size = 5) +
      ggplot2::coord_flip()
  })

  output$split_by_popstrata <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "Popstrata" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Geography", "Flag", "shift")]
    dat <- dat %>% group_by(Geography) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% mutate(horizontal_adjust = ifelse(shift < 0, -0.25, 1.25))
    dat$Geography <- factor(dat$Geography, levels = rev(popstrata_levs), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, shift)) +
      ggplot2::geom_bar(stat = "identity", fill = "#5C2D91", width = 0.65) +
      ggplot2::labs(
        x = "",
        y = "",
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        # panel.grid = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white"),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10)
      ) +
      ggplot2::geom_text(ggplot2::aes(y = shift, label = formatC(shift, format = "f", digits = 0, big.mark = ","), hjust = horizontal_adjust), family = "oswald", colour = "white", size = 5) +
      ggplot2::coord_flip()
  })

  output$split_by_channels <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "Channel" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Geography", "Flag", "shift")]
    dat <- dat %>% group_by(Geography) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% mutate(horizontal_adjust = ifelse(shift < 0, -0.25, 1.25))
    dat$Geography <- factor(dat$Geography, levels = rev(channel_levs), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, shift)) +
      ggplot2::geom_bar(stat = "identity", fill = "#5C2D91", width = 0.65) +
      ggplot2::labs(
        x = "",
        y = "",
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        # panel.grid = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white"),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10)
      ) +
      ggplot2::geom_text(ggplot2::aes(y = shift, label = formatC(shift, format = "f", digits = 0, big.mark = ","), hjust = horizontal_adjust), family = "oswald", colour = "white", size = 5) +
      ggplot2::coord_flip()
  })

  output$split_by_topdealers <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "Top Dealers" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Geography", "Flag", "shift")]
    dat <- dat %>% group_by(Geography) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% mutate(horizontal_adjust = ifelse(shift < 0, -0.25, 1.25))
    dat$Geography <- factor(dat$Geography, levels = rev(topdealers_levs), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(Geography, shift)) +
      ggplot2::geom_bar(stat = "identity", fill = "#5C2D91", width = 0.65) +
      ggplot2::labs(
        x = "",
        y = "",
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10)
      ) +
      ggplot2::geom_text(ggplot2::aes(y = shift, label = formatC(shift, format = "f", digits = 0, big.mark = ","), hjust = horizontal_adjust), family = "oswald", colour = "white", size = 5) +
      ggplot2::coord_flip()
  })

  output$split_by_states <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_GainLossFile[which(BRANDINTERACTIONS_GainLossFile$PRE == base_period() & BRANDINTERACTIONS_GainLossFile$POST == comparison_period() & BRANDINTERACTIONS_GainLossFile$PrdLevel == brand() & BRANDINTERACTIONS_GainLossFile$Cut == "State" & BRANDINTERACTIONS_GainLossFile$Fact == input$gl_type), c("Short", "Flag", "shift")]
    dat <- dat %>% group_by(Short) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% arrange(shift)
    dat$fill <- ifelse(dat$shift < 0, "#F64F5C", "#73B761")
    dat$Short <- factor(dat$Short, levels = rev(dat$Short), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(x = Short, y = 0)) +
      ggplot2::geom_segment(mapping = ggplot2::aes(xend = Short, yend = shift), size = 1, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = Short, y = shift, colour = fill), size = 10, show.legend = F, alpha = 1) +
      ggplot2::geom_text(mapping = ggplot2::aes(x = Short, y = shift, label = formatC(shift, format = "f", digits = 0, big.mark = ",")), color = "white", family = "oswald", size = 5) +
      ggplot2::geom_hline(yintercept = 0, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_manual(values = c("#F64F5C" = "#D83B01", "#73B761" = "#409640")) +
      ggplot2::labs(caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10),
        plot.background = ggplot2::element_rect(fill = "white"),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      ) +
      ggplot2::coord_flip()
  })

  output$top_gains <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_ShiftFile[which(BRANDINTERACTIONS_ShiftFile$PrdLevel == brand() & BRANDINTERACTIONS_ShiftFile$PRE == base_period() & BRANDINTERACTIONS_ShiftFile$POST == comparison_period() & BRANDINTERACTIONS_ShiftFile$Cut == "All India" & BRANDINTERACTIONS_ShiftFile$Fact == input$gl_type & BRANDINTERACTIONS_ShiftFile$Flag == "Gain"), ]
    dat <- dat %>% arrange(desc(abs(shift)))
    dat$Product <- factor(dat$Product, levels = rev(dat$Product), ordered = T)
    ggplot2::ggplot(dat, aes(x = Product, y = abs(shift))) +
      geom_bar(stat = "identity", show.legend = F, fill = "#409640", width = 0.65) +
      coord_flip() +
      scale_x_discrete(expand = c(0.1, 0)) +
      labs(caption = ifelse(input$gl_type == "SALVAL", "*Figures in Million Rs.", "*Figures in Tonnes")) +
      geom_text(aes(x = Product, y = abs(shift), label = formatC(shift, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", hjust = 1.1, size = 5) +
      geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "oswald", face = "bold", hjust = 1, size = 12),
        axis.title = element_blank(),
        plot.caption = element_text(family = "oswald")
      )
  })

  output$gains_by_cuts <- ggiraph::renderggiraph({
    dat <- BRANDINTERACTIONS_ShiftFile[which(BRANDINTERACTIONS_ShiftFile$PrdLevel == brand() & BRANDINTERACTIONS_ShiftFile$PRE == base_period() & BRANDINTERACTIONS_ShiftFile$POST == comparison_period() & BRANDINTERACTIONS_ShiftFile$Cut == input$cut1 & BRANDINTERACTIONS_ShiftFile$Fact == input$gl_type & BRANDINTERACTIONS_ShiftFile$Flag == "Gain"), ]
    if (input$cut1 == "Zone") {
      dat$Geography <- factor(dat$Geography, levels = zone_levs, ordered = T)
    }  else if (input$cut1 == "Popstrata") {
      dat$Geography <- factor(dat$Geography, levels = popstrata_levs, ordered = T)
    } else if (input$cut1 == "Channel") {
      dat$Geography <- factor(dat$Geography, levels = channel_levs, labels = channel_labs, ordered = T)
    } else if (input$cut1 == "Top Dealers") {
      dat$Geography <- factor(dat$Geography, levels = topdealers_levs, labels = topdealers_labs, ordered = T)
    }
    temp <- dat %>% group_by(Product) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% arrange(desc(abs(shift))) %>% `$`(Product)
    dat$Product <- factor(dat$Product, levels = rev(temp), ordered = T)
    dat$binary <- dat$shift < 0
    g <- ggplot(dat, ggplot2::aes(Geography, Product, size = abs(shift), colour = binary)) +
      geom_point_interactive(aes(tooltip = formatC(shift, format = "f", digits = 0, big.mark = ",")), show.legend = F) +
      scale_size_continuous(range = c(1, 15)) +
      scale_y_discrete(expand = c(0.05, 0)) +
      labs(caption = ifelse(input$gl_type == "SALVAL", "*Size by Value Shift in Lac Rs.\nColour by Loss (Red) or Gain (Green)", "*Size by Volume Shift in Tonnes\nColour by Loss (Red) or Gain (Green)")) +
      scale_colour_manual(values = c("TRUE" = "#D83B01", "FALSE" = "#409640")) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "oswald", face = "bold", size = 15),
        panel.border = element_blank(),
        plot.caption = element_text(family = "oswald", size = 10)
      )
    ggiraph(ggobj = g)
  })

  output$top_losses <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_ShiftFile[which(BRANDINTERACTIONS_ShiftFile$PrdLevel == brand() & BRANDINTERACTIONS_ShiftFile$PRE == base_period() & BRANDINTERACTIONS_ShiftFile$POST == comparison_period() & BRANDINTERACTIONS_ShiftFile$Cut == "All India" & BRANDINTERACTIONS_ShiftFile$Fact == input$gl_type & BRANDINTERACTIONS_ShiftFile$Flag == "Loss"), ]
    dat <- dat %>% arrange(desc(abs(shift)))
    dat$Product <- factor(dat$Product, levels = rev(dat$Product), ordered = T)
    ggplot2::ggplot(dat, aes(x = Product, y = abs(shift))) +
      geom_bar(stat = "identity", show.legend = F, fill = "#D83B01", width = 0.65) +
      coord_flip() +
      scale_x_discrete(expand = c(0.1, 0)) +
      labs(caption = ifelse(input$gl_type == "SALVAL", "*Figures in Lac Rs.", "*Figures in Tonnes")) +
      geom_text(aes(x = Product, y = abs(shift), label = formatC(shift, format = "f", digits = 0, big.mark = ",")), family = "oswald", colour = "white", hjust = 1.1, size = 5) +
      geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "oswald", face = "bold", hjust = 1, size = 12),
        axis.title = element_blank(),
        plot.caption = element_text(family = "oswald")
      )
  })

  output$losses_by_cuts <- ggiraph::renderggiraph({
    dat <- BRANDINTERACTIONS_ShiftFile[which(BRANDINTERACTIONS_ShiftFile$PrdLevel == brand() & BRANDINTERACTIONS_ShiftFile$PRE == base_period() & BRANDINTERACTIONS_ShiftFile$POST == comparison_period() & BRANDINTERACTIONS_ShiftFile$Cut == input$cut2 & BRANDINTERACTIONS_ShiftFile$Fact == input$gl_type & BRANDINTERACTIONS_ShiftFile$Flag == "Loss"), ]
    if (input$cut2 == "Zone") {
      dat$Geography <- factor(dat$Geography, levels = zone_levs, ordered = T)
    }  else if (input$cut2 == "Popstrata") {
      dat$Geography <- factor(dat$Geography, levels = popstrata_levs, ordered = T)
    } else if (input$cut2 == "Channel") {
      dat$Geography <- factor(dat$Geography, levels = channel_levs, labels = channel_labs, ordered = T)
    } else if (input$cut2 == "Top Dealers") {
      dat$Geography <- factor(dat$Geography, levels = topdealers_levs, labels = topdealers_labs, ordered = T)
    }
    temp <- dat %>% group_by(Product) %>% summarise(shift = sum(shift, na.rm = T)) %>% data.frame() %>% arrange(desc(abs(shift))) %>% `$`(Product)
    dat$Product <- factor(dat$Product, levels = rev(temp), ordered = T)
    dat$binary <- dat$shift < 0
    g <- ggplot(dat, ggplot2::aes(Geography, Product, size = abs(shift), colour = binary)) +
      geom_point_interactive(aes(tooltip = formatC(shift, format = "f", digits = 0, big.mark = ",")), show.legend = F) +
      scale_size_continuous(range = c(1, 15)) +
      labs(caption = ifelse(input$gl_type == "SALVAL", "*Size by Value Shift in Lac Rs.\nColour by Loss (Red) or Gain (Green)", "*Size by Volume Shift in Tonnes\nColour by Loss (Red) or Gain (Green)")) +
      scale_colour_manual(values = c("TRUE" = "#D83B01", "FALSE" = "#409640")) +
      scale_y_discrete(expand = c(0.05, 0)) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "oswald", face = "bold", size = 15),
        panel.border = element_blank(),
        plot.caption = element_text(family = "oswald", size = 10)
      )
    ggiraph(ggobj = g)
  })

  output$company_gl <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_ManufacturerFile[which(BRANDINTERACTIONS_ManufacturerFile$PrdLevel == brand() & BRANDINTERACTIONS_ManufacturerFile$PRE == base_period() & BRANDINTERACTIONS_ManufacturerFile$POST == comparison_period() & BRANDINTERACTIONS_ManufacturerFile$Fact == input$gl_type), c("Company", "shift", "Brands")]
    dat$Flag <- ifelse(dat$shift < 0, "neg", "pos")
    dat$abs_shift <- abs(dat$shift)
    dat <- dat[order(-dat$abs_shift), ]
    dat$Company <- factor(dat$Company, levels = rev(dat$Company), ordered = T)
    ggplot2::ggplot(dat, ggplot2::aes(x = Company, y = abs_shift)) +
      ggplot2::geom_segment(mapping = ggplot2::aes(x = Company, y = 0, xend = Company, yend = abs_shift), inherit.aes = F, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_point(mapping = ggplot2::aes(colour = Flag, size = Brands), show.legend = F, alpha = 0.7) +
      ggplot2::scale_size_continuous(range = c(5, 15)) +
      ggplot2::scale_colour_manual(values = c("neg" = "#D83B01", "pos" = "#409640")) +
      ggplot2::theme_bw() +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255)) +
      ggplot2::labs(
        caption = paste0(ifelse(input$gl_type == "SALVAL", "Shift Figures in Lac Rs.\n", "Shift Figures in Tonnes\n"), "Size by Number of Brands Interacated\nColour by Net Loss (Red) or Net Gain (Green)")
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", size = 10),
        axis.text.y = ggplot2::element_text(hjust = 1, family = "oswald", face = "bold", size = 12),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10)
      )
  })

  output$cannibalization <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_CannibalizationFile[which(BRANDINTERACTIONS_CannibalizationFile$Fact == input$gl_type & BRANDINTERACTIONS_CannibalizationFile$PrdLevel == brand() & BRANDINTERACTIONS_CannibalizationFile$PRE == base_period() & BRANDINTERACTIONS_CannibalizationFile$POST == comparison_period()), c("Cannibalization_Flag", "shift")]
    dat <- dat %>%
      mutate(Flag = ifelse(shift < 0, "Loss", "Gain")) %>%
      dplyr::group_by(Cannibalization_Flag, Flag) %>%
      summarise(shift = sum(shift, na.rm = T)) %>%
      data.frame()
    temp <- data.frame(Cannibalization_Flag = "Net", Flag = "Net", shift = sum(dat$shift, na.rm = T), stringsAsFactors = F)
    dat <- rbind(dat, temp)
    dat$Flag <- factor(dat$Flag, levels = c("Gain", "Loss", "Net"), ordered = T)
    dat <- dat[order(dat$Flag), ]
    dat$id <- as.numeric(dat$Flag)
    dat$end <- cumsum(dat$shift)
    dat$end <- c(head(dat$end, -1), 0)
    dat$start <- c(0, head(dat$end, -1))
    ggplot(dat, aes(fill = Cannibalization_Flag)) +
      ggplot2::geom_hline(yintercept = 0, color = rgb(166, 166, 166, max = 255)) +
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin = id - 0.5, xmax = id + 0.5, ymin = end, ymax = start), show.legend = T, color = "white") +
      ggplot2::scale_fill_manual(values = c("Net" = "#00183C", "Cannibalization" = "#6E79A1", Competition = "#F2C80FE6")) +
      ggplot2::labs(
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Lac Rs.", "*Figures in Tonnes"),
        fill = ""
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.text = ggplot2::element_text(family = "oswald", size = 10),
        legend.direction = "horizontal",
        axis.text.x = ggplot2::element_text(family = "oswald", face = "bold", vjust = 1, size = 12),
        axis.text.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_continuous(breaks = unique(dat$id), labels = c("Gain", "Loss", "Net"), expand = c(0.5, 0)) +
      ggplot2::geom_text(ggplot2::aes(x = id, y = (start + end) / 2, label = formatC(shift, format = "d", big.mark = ",")), family = "oswald", size = 5, colour = "white") +
      ggplot2::labs(x = "", y = "") +
      ggplot2::coord_cartesian(xlim = c(1, 3))
  })

  output$cannibalization_gl <- shiny::renderPlot({
    dat <- BRANDINTERACTIONS_CannibalizationFile[which(BRANDINTERACTIONS_CannibalizationFile$PrdLevel == brand() & BRANDINTERACTIONS_CannibalizationFile$POST == comparison_period() & BRANDINTERACTIONS_CannibalizationFile$PRE == base_period() & BRANDINTERACTIONS_CannibalizationFile$Fact == input$gl_type), ]
    dat <- dat %>%
      dplyr::filter(Cannibalization_Flag == "Cannibalization") %>%
      dplyr::group_by(Brand) %>%
      dplyr::summarise(shift = sum(shift, na.rm = T)) %>%
      data.frame() %>%
      arrange(desc(abs(shift))) %>%
      head(10)
    dat$Brand <- factor(dat$Brand, levels = rev(dat$Brand), ordered = T)
    dat$Flag <- dat$shift < 0
    ggplot2::ggplot(dat, ggplot2::aes(Brand, abs(shift), fill = Flag)) +
      ggplot2::geom_bar(stat = "identity", show.legend = F) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#D83B01", "FALSE" = "#409640")) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        caption = ifelse(input$gl_type == "SALVAL", "*Figures in Lac Rs.\nColour by Loss (red) or Gain (green)", "*Figures in Tonnes\nColour by Loss (red) or Gain (green)")
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(family = "oswald", size = 10),
        axis.text.y = ggplot2::element_text(family = "oswald", face = "bold", size = 12),
        axis.ticks = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(family = "oswald", size = 10)
      ) +
      geom_hline(yintercept = 0, colour = rgb(166, 166, 166, max = 255))
  })

  shiny::observeEvent(input$geography, {
    shinyWidgets::updatePrettyRadioButtons(session, inputId = "geo_opts", label = "", choices = unique(FORECAST_ForecastsFile[which(FORECAST_ForecastsFile$Cut == input$geography), "Geography"]), prettyOptions = list(shape = "round", inline = T, animation = "jelly"))
  })

  output$forecast_graph <- ggiraph::renderggiraph({
    dat <- FORECAST_ForecastsFile[which(FORECAST_ForecastsFile$Cut == isolate(input$geography) & FORECAST_ForecastsFile$PrdLevel == brand() & FORECAST_ForecastsFile$Geography == input$geo_opts), ]
    dat <- dat[order(dat$Date), ]
    forecast_horizon <- max(dat[which(dat$Flag == "Training"), "Date"])
    next_period <- min(dat[which(dat$Flag == "Forecasted"), "Date"])
    one_year_ago <- next_period - years(1)
    dat$Shape_Param <- as.factor(ifelse(dat$Date %in% c(next_period, one_year_ago), 21, 19))
    g <- ggplot(dat, aes(x = Date, y = Value, shape = Shape_Param, colour = Shape_Param, fill = Shape_Param)) +
      coord_cartesian(ylim = c(min(dat$Value), max(dat$Value))) +
      geom_point(size = 8, stroke = 2, show.legend = F) +
      geom_point_interactive(size = 4, colour = alpha("#002050", 0.7), aes(x = Date, y = Value, tooltip = paste0(lubridate::month(Date, label = T), " ", lubridate::year(Date), "\n", formatC(Value, format = "f", digits = 0, big.mark = ","))), show.legend = F, inherit.aes = F) +
      geom_path(group = 1, colour = "#002050", size = 1, alpha = 0.2) +
      scale_shape_manual(values = c("21" = 21, "19" = 19)) +
      scale_fill_manual(values = c("21" = "white", "19"= "white")) +
      scale_colour_manual(values = c("21" = "red", "19" = "white")) +
      geom_line(data = dat %>% filter(Flag != "Forecasted"), colour = "#002050", mapping = aes(x = Date, y = Value), inherit.aes = F, size = 1) +
      geom_ribbon(data = dat %>% filter(Date >= "2018-09-01"), fill = alpha("#002050", 0.4), inherit.aes = F, mapping = aes(x = Date, ymin = 0, ymax = Value)) +
      geom_vline(xintercept = forecast_horizon, linetype = "dashed") +
      theme_bw() +
      labs(
        x = "",
        y = "Sales Volume (in Tonnes)"
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b'%y") +
      theme(
        panel.border = element_blank(),
        axis.line.x = element_line(colour = rgb(166, 166, 166, max = 255)),
        axis.title = element_text(family = "oswald", face = "bold", size = 15),
        axis.text.x = element_text(family = "oswald", angle = 90, vjust = 0.5, size = 15),
        axis.text.y = element_text(family = "oswald", size = 15),
        axis.ticks = element_blank()
      )

    ggiraph(ggobj = g, width = 1, width_svg = 11, height_svg = 7)
  })

  output$change_ly <- shinydashboard::renderInfoBox({
    dat <- FORECAST_ForecastsFile[which(FORECAST_ForecastsFile$Cut == isolate(input$geography) & FORECAST_ForecastsFile$PrdLevel == brand() & FORECAST_ForecastsFile$Geography == input$geo_opts), ]
    next_period <- min(dat[which(dat$Flag == "Forecasted"), "Date"])
    one_year_ago <- next_period - years(1)
    clr <- NULL
    icn <- NULL
    change <- (dat[which(dat$Date == next_period), "Value"]/dat[which(dat$Date == one_year_ago), "Value"]) - 1
    if (change < 0) {
      icn <- shiny::icon("chevron-down")
      clr <- "maroon"
    } else {
      icn <- shiny::icon("chevron-up")
      clr <- "green"
    }
    shinydashboard::infoBox(title = htmltools::h4(paste0("Expected Growth: ", month(next_period, abbr = T, label = T), "'", substr(year(next_period), 3, 4), " vs ", month(one_year_ago, abbr = T, label = T), "'", substr(year(one_year_ago), 3, 4)), style = "margin: -1px 0px 10px 0px; font-size: 99%"), value = htmltools::h1(paste0(round(change * 100, 1), "%"), style = "margin: 5px 0px 0px 0px; font-size: 190%"), color = clr, icon = icn)
  })

  output$change_pm <- shinydashboard::renderInfoBox({
    dat <- FORECAST_ForecastsFile[which(FORECAST_ForecastsFile$Cut == isolate(input$geography) & FORECAST_ForecastsFile$PrdLevel == brand() & FORECAST_ForecastsFile$Geography == input$geo_opts), ]
    forecast_horizon <- max(dat[which(dat$Flag == "Training"), "Date"])
    next_period <- min(dat[which(dat$Flag == "Forecasted"), "Date"])
    icn <- NULL
    clr <- NULL
    change <- (dat[which(dat$Date == next_period), "Value"]/dat[which(dat$Date == forecast_horizon), "Value"]) - 1
    if (change < 0) {
      icn <- shiny::icon("chevron-down")
      clr <- "maroon"
    } else {
      icn <- shiny::icon("chevron-up")
      clr <- "green"
    }
    shinydashboard::infoBox(title = htmltools::h4(paste0("Expected Growth: ", month(next_period, abbr = T, label = T), "'", substr(year(next_period), 3, 4), " vs ", month(forecast_horizon, abbr = T, label = T), "'", substr(year(forecast_horizon), 3, 4)), style = "margin: -1px 0px 10px 0px; font-size: 99%"), value = htmltools::h1(paste0(round(change * 100, 1), "%"), style = "margin: 5px 0px 0px 0px; font-size: 190%"), color = clr, icon = icn)
  })

}

shinyApp(ui, server, options = list(port = 1234, host = "0.0.0.0", launch.browser = T, display.mode = "normal"))