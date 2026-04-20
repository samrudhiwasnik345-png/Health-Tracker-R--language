# ============================================================
#  HEALTH TRACKER — R Shiny Application
#  Tracks: Weight, Exercise, Diet, Vitals, Sleep, Mood
#  Visualizes trends over time using ggplot2
# ============================================================
#
#  REQUIRED PACKAGES — install once before running:
#  install.packages(c("shiny","shinydashboard","ggplot2","dplyr",
#                     "lubridate","DT","plotly","scales","tidyr",
#                     "shinycssloaders","shinyWidgets","fresh"))
#
#  HOW TO RUN:
#  1. Place this file (app.R) and health_data.csv in the same folder.
#  2. Open app.R in RStudio and click "Run App", OR run:
#     shiny::runApp("path/to/folder")
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)
library(scales)
library(tidyr)
library(shinyWidgets)

# ── Colour palette ─────────────────────────────────────────
CLR <- list(
  bg        = "#0f1117",
  card      = "#1a1d27",
  border    = "#2a2d3e",
  accent1   = "#6c63ff",
  accent2   = "#ff6584",
  accent3   = "#43e97b",
  accent4   = "#f7b731",
  accent5   = "#45aaf2",
  text      = "#e8eaf6",
  subtext   = "#9e9fb5",
  success   = "#43e97b",
  warning   = "#f7b731",
  danger    = "#ff6584"
)

# ── ggplot2 dark theme ──────────────────────────────────────
theme_health <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background    = element_rect(fill = CLR$card, colour = NA),
      panel.background   = element_rect(fill = CLR$card, colour = NA),
      panel.grid.major   = element_line(colour = "#2a2d3e", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = CLR$subtext, size = 11),
      axis.title         = element_text(colour = CLR$text, size = 12, face = "bold"),
      plot.title         = element_text(colour = CLR$text, size = 14, face = "bold", margin = margin(b = 10)),
      plot.subtitle      = element_text(colour = CLR$subtext, size = 11, margin = margin(b = 12)),
      legend.background  = element_rect(fill = CLR$card, colour = NA),
      legend.text        = element_text(colour = CLR$subtext),
      legend.title       = element_text(colour = CLR$text),
      strip.text         = element_text(colour = CLR$text, face = "bold"),
      strip.background   = element_rect(fill = "#252836"),
      plot.margin        = margin(16, 16, 16, 16)
    )
}

# ── Custom CSS ──────────────────────────────────────────────
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap');

  * { box-sizing: border-box; }

  body, .content-wrapper, .main-header, .main-sidebar {
    background-color: #0f1117 !important;
    font-family: 'Space Grotesk', sans-serif !important;
    color: #e8eaf6 !important;
  }

  .skin-blue .main-header .logo { background: #0f1117 !important; border-bottom: 1px solid #2a2d3e; }
  .skin-blue .main-header .navbar { background: #0f1117 !important; border-bottom: 1px solid #2a2d3e; }
  .skin-blue .main-sidebar { background: #1a1d27 !important; border-right: 1px solid #2a2d3e; }
  .skin-blue .sidebar-menu > li > a { color: #9e9fb5 !important; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a { color: #6c63ff !important; background: rgba(108,99,255,0.1) !important; }
  .skin-blue .sidebar-menu > li > a .fa { color: inherit; }

  .content-wrapper { background: #0f1117 !important; }
  .box { background: #1a1d27 !important; border: 1px solid #2a2d3e !important; border-radius: 12px !important; box-shadow: none !important; }
  .box-header { border-bottom: 1px solid #2a2d3e !important; color: #e8eaf6 !important; }
  .box-title { color: #e8eaf6 !important; font-weight: 600 !important; font-size: 14px !important; }

  .small-box { border-radius: 12px !important; border: none !important; transition: transform 0.2s; }
  .small-box:hover { transform: translateY(-3px); }
  .small-box h3 { font-family: 'DM Mono', monospace !important; font-size: 2.2rem !important; }
  .small-box p { font-size: 13px !important; }

  .form-control {
    background: #252836 !important;
    border: 1px solid #2a2d3e !important;
    color: #e8eaf6 !important;
    border-radius: 8px !important;
    font-family: 'Space Grotesk', sans-serif !important;
  }
  .form-control:focus { border-color: #6c63ff !important; box-shadow: 0 0 0 3px rgba(108,99,255,0.15) !important; outline: none !important; }

  .btn-primary { background: linear-gradient(135deg,#6c63ff,#8b5cf6) !important; border: none !important; border-radius: 8px !important; font-weight: 600 !important; }
  .btn-success { background: linear-gradient(135deg,#43e97b,#38f9d7) !important; border: none !important; border-radius: 8px !important; font-weight: 600 !important; color: #0f1117 !important; }
  .btn-danger  { background: linear-gradient(135deg,#ff6584,#ff4b6e) !important; border: none !important; border-radius: 8px !important; font-weight: 600 !important; }
  .btn-warning { background: linear-gradient(135deg,#f7b731,#f5a623) !important; border: none !important; border-radius: 8px !important; font-weight: 600 !important; color: #0f1117 !important; }

  label { color: #9e9fb5 !important; font-size: 12px !important; font-weight: 500 !important; letter-spacing: 0.05em !important; text-transform: uppercase !important; }
  h4 { color: #e8eaf6 !important; font-weight: 600 !important; }

  .dataTables_wrapper { color: #9e9fb5 !important; }
  table.dataTable { background: #1a1d27 !important; border: none !important; }
  table.dataTable thead th { background: #252836 !important; color: #6c63ff !important; border-bottom: 1px solid #2a2d3e !important; font-size: 12px !important; text-transform: uppercase; letter-spacing: 0.05em; }
  table.dataTable tbody tr { background: #1a1d27 !important; color: #e8eaf6 !important; }
  table.dataTable tbody tr:hover { background: #252836 !important; }
  table.dataTable tbody td { border-top: 1px solid #2a2d3e !important; }
  .dataTables_filter input, .dataTables_length select { background: #252836 !important; border: 1px solid #2a2d3e !important; color: #e8eaf6 !important; }

  .selectize-input { background: #252836 !important; border: 1px solid #2a2d3e !important; color: #e8eaf6 !important; border-radius: 8px !important; }
  .selectize-dropdown { background: #252836 !important; border: 1px solid #2a2d3e !important; color: #e8eaf6 !important; }
  .selectize-dropdown .option:hover { background: #6c63ff !important; }

  .daterangepicker { background: #1a1d27; border-color: #2a2d3e; }
  .daterangepicker td.active { background: #6c63ff !important; }

  .main-header .logo span.logo-lg { font-family: 'Space Grotesk', sans-serif !important; font-weight: 700; font-size: 18px; color: #6c63ff !important; letter-spacing: -0.5px; }
  .main-header .logo span.logo-mini { font-weight: 700; color: #6c63ff !important; }

  .sidebar-toggle { color: #9e9fb5 !important; }

  /* Metric cards coloring */
  .bg-purple  { background: linear-gradient(135deg, #6c63ff, #8b5cf6) !important; }
  .bg-pink    { background: linear-gradient(135deg, #ff6584, #ff4b6e) !important; }
  .bg-green   { background: linear-gradient(135deg, #43e97b, #38f9d7) !important; }
  .bg-yellow  { background: linear-gradient(135deg, #f7b731, #f5a623) !important; }
  .bg-blue    { background: linear-gradient(135deg, #45aaf2, #2980b9) !important; }
  .bg-purple .small-box-footer, .bg-pink .small-box-footer, .bg-green .small-box-footer,
  .bg-yellow .small-box-footer, .bg-blue .small-box-footer { color: rgba(255,255,255,0.75) !important; }

  hr { border-color: #2a2d3e !important; }
  .nav-tabs { border-color: #2a2d3e !important; }
  .nav-tabs > li > a { color: #9e9fb5 !important; }
  .nav-tabs > li.active > a { background: #252836 !important; border-color: #2a2d3e !important; color: #6c63ff !important; }

  /* Plotly dark override */
  .js-plotly-plot .plotly { background: #1a1d27 !important; }
"

# ─────────────────────────────────────────────────────────────
#  UI
# ─────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$span(class = "logo-lg", "⚡ HealthTrack"),
      tags$span(class = "logo-mini", "HT")
    )
  ),

  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Dashboard",    tabName = "dashboard",  icon = icon("chart-line")),
      menuItem("⚖️  Weight",       tabName = "weight",     icon = icon("weight-scale")),
      menuItem("🏃 Exercise",     tabName = "exercise",   icon = icon("dumbbell")),
      menuItem("🥗 Nutrition",    tabName = "nutrition",  icon = icon("utensils")),
      menuItem("❤️  Vitals",       tabName = "vitals",     icon = icon("heartbeat")),
      menuItem("😴 Sleep & Mood", tabName = "sleep",      icon = icon("moon")),
      menuItem("➕ Log Entry",    tabName = "log",        icon = icon("plus-circle")),
      menuItem("📋 Data Table",   tabName = "table",      icon = icon("table"))
    )
  ),

  dashboardBody(
    tabItems(

      # ── DASHBOARD ──────────────────────────────────────────
      tabItem("dashboard",
        fluidRow(
          column(12,
            tags$div(
              style = "padding: 8px 0 20px; color: #9e9fb5; font-size: 13px;",
              "📅 Showing data from ", textOutput("date_range_label", inline = TRUE)
            )
          )
        ),
        fluidRow(
          valueBoxOutput("vbox_weight",   width = 3),
          valueBoxOutput("vbox_calories", width = 3),
          valueBoxOutput("vbox_steps",    width = 3),
          valueBoxOutput("vbox_sleep",    width = 3)
        ),
        fluidRow(
          box(title = "⚖️ Weight Trend", width = 6, plotlyOutput("dash_weight", height = 280)),
          box(title = "🔥 Calories In vs Burned", width = 6, plotlyOutput("dash_calories", height = 280))
        ),
        fluidRow(
          box(title = "🏃 Exercise Distribution", width = 4, plotlyOutput("dash_exercise_pie", height = 280)),
          box(title = "👣 Daily Steps", width = 4, plotlyOutput("dash_steps", height = 280)),
          box(title = "😴 Sleep Hours", width = 4, plotlyOutput("dash_sleep", height = 280))
        )
      ),

      # ── WEIGHT ─────────────────────────────────────────────
      tabItem("weight",
        fluidRow(
          box(title = "📈 Weight Over Time", width = 12,
              plotlyOutput("weight_trend", height = 380))
        ),
        fluidRow(
          box(title = "📦 Weight Distribution", width = 6,
              plotlyOutput("weight_hist", height = 300)),
          box(title = "📊 Weekly Average Weight", width = 6,
              plotlyOutput("weight_weekly", height = 300))
        )
      ),

      # ── EXERCISE ───────────────────────────────────────────
      tabItem("exercise",
        fluidRow(
          box(title = "⏱️ Exercise Duration Over Time", width = 12,
              plotlyOutput("exercise_duration", height = 380))
        ),
        fluidRow(
          box(title = "🏅 Calories Burned by Exercise Type", width = 6,
              plotlyOutput("exercise_calories_type", height = 300)),
          box(title = "🍩 Exercise Type Breakdown", width = 6,
              plotlyOutput("exercise_donut", height = 300))
        )
      ),

      # ── NUTRITION ──────────────────────────────────────────
      tabItem("nutrition",
        fluidRow(
          box(title = "🥗 Calorie Intake vs Goal (2000 kcal)", width = 12,
              plotlyOutput("nutrition_intake", height = 380))
        ),
        fluidRow(
          box(title = "💧 Daily Water Intake (ml)", width = 6,
              plotlyOutput("water_trend", height = 300)),
          box(title = "⚖️ Net Calories (Intake – Burned)", width = 6,
              plotlyOutput("net_calories", height = 300))
        )
      ),

      # ── VITALS ─────────────────────────────────────────────
      tabItem("vitals",
        fluidRow(
          box(title = "🩺 Blood Pressure Over Time", width = 12,
              plotlyOutput("bp_trend", height = 380))
        ),
        fluidRow(
          box(title = "💓 Resting Heart Rate", width = 6,
              plotlyOutput("hr_trend", height = 300)),
          box(title = "📊 BP Zone Distribution", width = 6,
              plotlyOutput("bp_zone", height = 300))
        )
      ),

      # ── SLEEP & MOOD ───────────────────────────────────────
      tabItem("sleep",
        fluidRow(
          box(title = "😴 Sleep Duration Over Time", width = 12,
              plotlyOutput("sleep_trend", height = 380))
        ),
        fluidRow(
          box(title = "😊 Mood Distribution", width = 6,
              plotlyOutput("mood_bar", height = 300)),
          box(title = "💤 Sleep vs Mood", width = 6,
              plotlyOutput("sleep_mood", height = 300))
        )
      ),

      # ── LOG ENTRY ──────────────────────────────────────────
      tabItem("log",
        fluidRow(
          box(title = "➕ Log a New Health Entry", width = 12,
            fluidRow(
              column(3, dateInput("log_date", "Date", value = Sys.Date())),
              column(3, numericInput("log_weight", "Weight (kg)", value = 70, min = 30, max = 300, step = 0.1)),
              column(3, numericInput("log_calories_in", "Calories Intake", value = 2000, min = 0, max = 6000)),
              column(3, numericInput("log_calories_burned", "Calories Burned", value = 300, min = 0, max = 3000))
            ),
            fluidRow(
              column(3, numericInput("log_steps", "Steps", value = 7000, min = 0, max = 50000)),
              column(3, numericInput("log_water", "Water (ml)", value = 2000, min = 0, max = 6000)),
              column(3, numericInput("log_sleep", "Sleep (hours)", value = 7.5, min = 0, max = 24, step = 0.5)),
              column(3, selectInput("log_mood", "Mood", choices = c("Great", "Good", "Okay", "Bad", "Terrible")))
            ),
            fluidRow(
              column(3, selectInput("log_exercise_type", "Exercise Type",
                                    choices = c("Running","Walking","Cycling","Swimming","Yoga","HIIT","Strength","Rest"))),
              column(3, numericInput("log_exercise_min", "Exercise Duration (min)", value = 30, min = 0, max = 300)),
              column(3, numericInput("log_systolic", "Systolic BP (mmHg)", value = 120, min = 80, max = 200)),
              column(3, numericInput("log_diastolic", "Diastolic BP (mmHg)", value = 80, min = 50, max = 130))
            ),
            fluidRow(
              column(3, numericInput("log_hr", "Heart Rate (bpm)", value = 72, min = 40, max = 200)),
              column(9)
            ),
            br(),
            fluidRow(
              column(12,
                actionButton("btn_add", "💾 Save Entry", class = "btn btn-success btn-lg"),
                tags$span(style = "margin-left: 12px;", textOutput("save_msg", inline = TRUE))
              )
            )
          )
        )
      ),

      # ── DATA TABLE ─────────────────────────────────────────
      tabItem("table",
        fluidRow(
          box(title = "📋 All Health Records", width = 12,
            fluidRow(
              column(3, dateRangeInput("tbl_daterange", "Filter by Date",
                                       start = "2024-01-01", end = Sys.Date())),
              column(3, downloadButton("download_csv", "⬇️ Download CSV", class = "btn btn-primary"))
            ),
            br(),
            DTOutput("health_table")
          )
        )
      )

    )  # end tabItems
  )
)

# ─────────────────────────────────────────────────────────────
#  SERVER
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Load CSV & reactive store ─────────────────────────────
  csv_path <- "health_data.csv"
  init_df <- if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    df
  } else {
    data.frame(
      date = Sys.Date(), weight_kg = 70, calories_intake = 2000,
      calories_burned = 300, steps = 7000, water_ml = 2000,
      sleep_hours = 7.5, exercise_type = "Rest", exercise_duration_min = 0,
      systolic_bp = 120, diastolic_bp = 80, heart_rate = 72, mood = "Good",
      stringsAsFactors = FALSE
    )
  }

  rv <- reactiveValues(df = init_df)

  # ── Helper: plotly layout defaults ───────────────────────
  ply_layout <- function(p, title = "", ylab = "", xlab = "Date") {
    p %>% plotly::layout(
      title       = list(text = title, font = list(color = CLR$text, size = 14, family = "Space Grotesk")),
      paper_bgcolor = CLR$card,
      plot_bgcolor  = CLR$card,
      font          = list(color = CLR$subtext, family = "Space Grotesk"),
      xaxis = list(title = xlab, gridcolor = CLR$border, tickfont = list(color = CLR$subtext), titlefont = list(color = CLR$text)),
      yaxis = list(title = ylab, gridcolor = CLR$border, tickfont = list(color = CLR$subtext), titlefont = list(color = CLR$text)),
      legend = list(font = list(color = CLR$subtext)),
      margin = list(t = 40, r = 20, b = 50, l = 60)
    )
  }

  # ── Date range label ──────────────────────────────────────
  output$date_range_label <- renderText({
    df <- rv$df
    paste(format(min(df$date), "%b %d, %Y"), "to", format(max(df$date), "%b %d, %Y"))
  })

  # ── VALUE BOXES ───────────────────────────────────────────
  output$vbox_weight <- renderValueBox({
    latest <- tail(rv$df, 1)
    valueBox(paste0(latest$weight_kg, " kg"), "Latest Weight",
             icon = icon("weight-scale"), color = "purple")
  })

  output$vbox_calories <- renderValueBox({
    avg <- round(mean(rv$df$calories_intake, na.rm = TRUE))
    valueBox(paste0(avg, " kcal"), "Avg Daily Intake",
             icon = icon("fire"), color = "red")
  })

  output$vbox_steps <- renderValueBox({
    avg <- round(mean(rv$df$steps, na.rm = TRUE))
    valueBox(format(avg, big.mark = ","), "Avg Daily Steps",
             icon = icon("person-walking"), color = "green")
  })

  output$vbox_sleep <- renderValueBox({
    avg <- round(mean(rv$df$sleep_hours, na.rm = TRUE), 1)
    valueBox(paste0(avg, " hrs"), "Avg Sleep",
             icon = icon("moon"), color = "blue")
  })

  # ── DASHBOARD CHARTS ──────────────────────────────────────
  output$dash_weight <- renderPlotly({
    df <- rv$df
    p <- plot_ly(df, x = ~date, y = ~weight_kg, type = "scatter", mode = "lines+markers",
                 line = list(color = CLR$accent1, width = 2.5),
                 marker = list(color = CLR$accent1, size = 5),
                 hovertemplate = "<b>%{x}</b><br>Weight: %{y} kg<extra></extra>")
    ply_layout(p, ylab = "Weight (kg)")
  })

  output$dash_calories <- renderPlotly({
    df <- rv$df
    plot_ly(df, x = ~date) %>%
      add_trace(y = ~calories_intake,  name = "Intake",  type = "bar", marker = list(color = CLR$accent2, opacity = 0.85)) %>%
      add_trace(y = ~calories_burned,  name = "Burned",  type = "bar", marker = list(color = CLR$accent3, opacity = 0.85)) %>%
      plotly::layout(
        barmode = "group", paper_bgcolor = CLR$card, plot_bgcolor = CLR$card,
        font = list(color = CLR$subtext, family = "Space Grotesk"),
        xaxis = list(title = "Date", gridcolor = CLR$border),
        yaxis = list(title = "Calories (kcal)", gridcolor = CLR$border),
        legend = list(font = list(color = CLR$subtext)),
        margin = list(t = 20, r = 20, b = 50, l = 60)
      )
  })

  output$dash_exercise_pie <- renderPlotly({
    df <- rv$df %>% filter(exercise_type != "Rest") %>%
      count(exercise_type)
    plot_ly(df, labels = ~exercise_type, values = ~n, type = "pie",
            marker = list(colors = c(CLR$accent1, CLR$accent2, CLR$accent3,
                                     CLR$accent4, CLR$accent5, "#a29bfe", "#fd79a8")),
            textfont = list(color = "#fff"),
            hovertemplate = "<b>%{label}</b><br>Sessions: %{value}<br>%{percent}<extra></extra>") %>%
      plotly::layout(paper_bgcolor = CLR$card, font = list(color = CLR$subtext, family = "Space Grotesk"),
                     legend = list(font = list(color = CLR$subtext)), margin = list(t = 10))
  })

  output$dash_steps <- renderPlotly({
    df <- rv$df
    p <- plot_ly(df, x = ~date, y = ~steps, type = "bar",
                 marker = list(color = CLR$accent4, opacity = 0.85),
                 hovertemplate = "<b>%{x}</b><br>Steps: %{y:,}<extra></extra>") %>%
      add_trace(x = ~date, y = ~rep(10000, nrow(df)), type = "scatter", mode = "lines",
                line = list(color = CLR$accent2, dash = "dash", width = 1.5),
                name = "10k Goal", hoverinfo = "none")
    ply_layout(p, ylab = "Steps")
  })

  output$dash_sleep <- renderPlotly({
    df <- rv$df
    p <- plot_ly(df, x = ~date, y = ~sleep_hours, type = "scatter", mode = "lines+markers",
                 fill = "tozeroy",
                 line = list(color = CLR$accent5, width = 2),
                 fillcolor = "rgba(69,170,242,0.15)",
                 marker = list(color = CLR$accent5, size = 4),
                 hovertemplate = "<b>%{x}</b><br>Sleep: %{y} hrs<extra></extra>") %>%
      add_trace(x = ~date, y = ~rep(8, nrow(df)), type = "scatter", mode = "lines",
                line = list(color = CLR$success, dash = "dash", width = 1.5),
                name = "8hr Goal", hoverinfo = "none")
    ply_layout(p, ylab = "Hours")
  })

  # ── WEIGHT TAB ────────────────────────────────────────────
  output$weight_trend <- renderPlotly({
    df <- rv$df
    fit <- lm(weight_kg ~ as.numeric(date), data = df)
    df$trend <- predict(fit)
    plot_ly(df, x = ~date) %>%
      add_trace(y = ~weight_kg, type = "scatter", mode = "lines+markers",
                name = "Weight", line = list(color = CLR$accent1, width = 2.5),
                marker = list(color = CLR$accent1, size = 6)) %>%
      add_trace(y = ~trend, type = "scatter", mode = "lines", name = "Trend",
                line = list(color = CLR$accent2, dash = "dot", width = 2)) %>%
      ply_layout(ylab = "Weight (kg)")
  })

  output$weight_hist <- renderPlotly({
    df <- rv$df
    plot_ly(df, x = ~weight_kg, type = "histogram", nbinsx = 15,
            marker = list(color = CLR$accent1, opacity = 0.8,
                          line = list(color = CLR$bg, width = 1))) %>%
      ply_layout(ylab = "Count", xlab = "Weight (kg)")
  })

  output$weight_weekly <- renderPlotly({
    df <- rv$df %>%
      mutate(week = floor_date(date, "week")) %>%
      group_by(week) %>%
      summarise(avg_w = mean(weight_kg, na.rm = TRUE), .groups = "drop")
    p <- plot_ly(df, x = ~week, y = ~avg_w, type = "bar",
                 marker = list(color = CLR$accent1, opacity = 0.85))
    ply_layout(p, ylab = "Avg Weight (kg)", xlab = "Week")
  })

  # ── EXERCISE TAB ──────────────────────────────────────────
  output$exercise_duration <- renderPlotly({
    df <- rv$df
    pal <- c(Running = CLR$accent2, Walking = CLR$accent3, Cycling = CLR$accent4,
             Swimming = CLR$accent5, Yoga = CLR$accent1, HIIT = "#fd79a8",
             Strength = "#a29bfe", Rest = CLR$border)
    plot_ly(df, x = ~date, y = ~exercise_duration_min, color = ~exercise_type,
            colors = pal, type = "bar",
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y} min<extra></extra>") %>%
      ply_layout(ylab = "Duration (min)")
  })

  output$exercise_calories_type <- renderPlotly({
    df <- rv$df %>% filter(exercise_type != "Rest") %>%
      group_by(exercise_type) %>%
      summarise(avg_burned = mean(calories_burned, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_burned))
    plot_ly(df, x = ~avg_burned, y = ~reorder(exercise_type, avg_burned),
            type = "bar", orientation = "h",
            marker = list(color = CLR$accent3, opacity = 0.85)) %>%
      ply_layout(ylab = "", xlab = "Avg Calories Burned")
  })

  output$exercise_donut <- renderPlotly({
    df <- rv$df %>% filter(exercise_type != "Rest") %>%
      group_by(exercise_type) %>%
      summarise(total_min = sum(exercise_duration_min, na.rm = TRUE), .groups = "drop")
    plot_ly(df, labels = ~exercise_type, values = ~total_min, type = "pie",
            hole = 0.45,
            marker = list(colors = c(CLR$accent1, CLR$accent2, CLR$accent3,
                                     CLR$accent4, CLR$accent5, "#a29bfe", "#fd79a8")),
            textfont = list(color = "#fff"),
            hovertemplate = "<b>%{label}</b><br>Total: %{value} min<br>%{percent}<extra></extra>") %>%
      plotly::layout(paper_bgcolor = CLR$card, font = list(color = CLR$subtext, family = "Space Grotesk"),
                     legend = list(font = list(color = CLR$subtext)), margin = list(t = 10))
  })

  # ── NUTRITION TAB ─────────────────────────────────────────
  output$nutrition_intake <- renderPlotly({
    df <- rv$df
    plot_ly(df, x = ~date) %>%
      add_trace(y = ~calories_intake, type = "scatter", mode = "lines+markers",
                name = "Intake", fill = "tozeroy",
                line = list(color = CLR$accent2, width = 2),
                fillcolor = "rgba(255,101,132,0.1)",
                marker = list(color = CLR$accent2, size = 5)) %>%
      add_trace(y = ~rep(2000, nrow(df)), type = "scatter", mode = "lines",
                name = "Goal (2000)", line = list(color = CLR$accent3, dash = "dash", width = 1.5),
                hoverinfo = "none") %>%
      ply_layout(ylab = "Calories (kcal)")
  })

  output$water_trend <- renderPlotly({
    df <- rv$df
    p <- plot_ly(df, x = ~date, y = ~water_ml, type = "bar",
                 marker = list(color = CLR$accent5, opacity = 0.8)) %>%
      add_trace(x = ~date, y = ~rep(2500, nrow(df)), type = "scatter", mode = "lines",
                line = list(color = CLR$accent3, dash = "dash", width = 1.5),
                name = "2.5L Goal", hoverinfo = "none")
    ply_layout(p, ylab = "Water (ml)")
  })

  output$net_calories <- renderPlotly({
    df <- rv$df %>% mutate(net = calories_intake - calories_burned,
                           colour = ifelse(net > 0, CLR$accent2, CLR$accent3))
    plot_ly(df, x = ~date, y = ~net, type = "bar",
            marker = list(color = ~colour, opacity = 0.85),
            hovertemplate = "<b>%{x}</b><br>Net: %{y} kcal<extra></extra>") %>%
      add_trace(x = ~date, y = ~rep(0, nrow(df)), type = "scatter", mode = "lines",
                line = list(color = CLR$subtext, width = 1), hoverinfo = "none", showlegend = FALSE) %>%
      ply_layout(ylab = "Net Calories (kcal)")
  })

  # ── VITALS TAB ────────────────────────────────────────────
  output$bp_trend <- renderPlotly({
    df <- rv$df
    plot_ly(df, x = ~date) %>%
      add_trace(y = ~systolic_bp,  name = "Systolic",  type = "scatter", mode = "lines+markers",
                line = list(color = CLR$accent2, width = 2), marker = list(color = CLR$accent2, size = 5)) %>%
      add_trace(y = ~diastolic_bp, name = "Diastolic", type = "scatter", mode = "lines+markers",
                line = list(color = CLR$accent5, width = 2), marker = list(color = CLR$accent5, size = 5)) %>%
      add_trace(y = ~rep(120, nrow(df)), name = "Systolic Goal",  type = "scatter", mode = "lines",
                line = list(color = CLR$accent2, dash = "dot", width = 1), hoverinfo = "none") %>%
      add_trace(y = ~rep(80, nrow(df)),  name = "Diastolic Goal", type = "scatter", mode = "lines",
                line = list(color = CLR$accent5, dash = "dot", width = 1), hoverinfo = "none") %>%
      ply_layout(ylab = "mmHg")
  })

  output$hr_trend <- renderPlotly({
    df <- rv$df
    p <- plot_ly(df, x = ~date, y = ~heart_rate, type = "scatter", mode = "lines+markers",
                 fill = "tozeroy", fillcolor = "rgba(255,101,132,0.1)",
                 line = list(color = CLR$accent2, width = 2),
                 marker = list(color = CLR$accent2, size = 5),
                 hovertemplate = "<b>%{x}</b><br>HR: %{y} bpm<extra></extra>")
    ply_layout(p, ylab = "Heart Rate (bpm)")
  })

  output$bp_zone <- renderPlotly({
    df <- rv$df %>% mutate(
      zone = case_when(
        systolic_bp < 120 & diastolic_bp < 80   ~ "Normal",
        systolic_bp < 130 & diastolic_bp < 80   ~ "Elevated",
        systolic_bp < 140 | diastolic_bp < 90   ~ "High Stage 1",
        TRUE                                     ~ "High Stage 2"
      )
    ) %>% count(zone)
    zone_colours <- c(Normal = CLR$accent3, Elevated = CLR$accent4,
                      `High Stage 1` = CLR$accent2, `High Stage 2` = CLR$danger)
    plot_ly(df, labels = ~zone, values = ~n, type = "pie",
            marker = list(colors = zone_colours[df$zone]),
            textfont = list(color = "#fff"),
            hovertemplate = "<b>%{label}</b><br>Days: %{value} (%{percent})<extra></extra>") %>%
      plotly::layout(paper_bgcolor = CLR$card, font = list(color = CLR$subtext, family = "Space Grotesk"),
                     legend = list(font = list(color = CLR$subtext)))
  })

  # ── SLEEP & MOOD TAB ──────────────────────────────────────
  output$sleep_trend <- renderPlotly({
    df <- rv$df
    p <- plot_ly(df, x = ~date, y = ~sleep_hours, type = "scatter", mode = "lines+markers",
                 fill = "tozeroy", fillcolor = "rgba(69,170,242,0.12)",
                 line = list(color = CLR$accent5, width = 2),
                 marker = list(color = ~sleep_hours,
                               colorscale = list(c(0,"#ff6584"), c(0.5,"#f7b731"), c(1,"#43e97b")),
                               showscale = TRUE, size = 8, cmin = 4, cmax = 9),
                 hovertemplate = "<b>%{x}</b><br>Sleep: %{y} hrs<extra></extra>") %>%
      add_trace(x = ~date, y = ~rep(7, nrow(df)), type = "scatter", mode = "lines",
                line = list(color = CLR$accent4, dash = "dash", width = 1.5),
                name = "7hr Min", hoverinfo = "none") %>%
      add_trace(x = ~date, y = ~rep(8, nrow(df)), type = "scatter", mode = "lines",
                line = list(color = CLR$accent3, dash = "dash", width = 1.5),
                name = "8hr Ideal", hoverinfo = "none")
    ply_layout(p, ylab = "Hours")
  })

  output$mood_bar <- renderPlotly({
    df <- rv$df %>%
      mutate(mood = factor(mood, levels = c("Great","Good","Okay","Bad","Terrible"))) %>%
      count(mood, .drop = FALSE)
    mood_cols <- c(Great = CLR$accent3, Good = CLR$accent5, Okay = CLR$accent4,
                   Bad = CLR$accent2, Terrible = CLR$danger)
    plot_ly(df, x = ~mood, y = ~n, type = "bar",
            marker = list(color = mood_cols[levels(df$mood)], opacity = 0.85)) %>%
      ply_layout(ylab = "Days", xlab = "Mood")
  })

  output$sleep_mood <- renderPlotly({
    df <- rv$df
    mood_cols <- c(Great = CLR$accent3, Good = CLR$accent5, Okay = CLR$accent4,
                   Bad = CLR$accent2, Terrible = CLR$danger)
    plot_ly(df, x = ~sleep_hours, y = ~weight_kg, color = ~mood,
            colors = mood_cols, type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.8),
            text = ~paste("Date:", date, "<br>Mood:", mood),
            hovertemplate = "%{text}<br>Sleep: %{x} hrs<br>Weight: %{y} kg<extra></extra>") %>%
      ply_layout(xlab = "Sleep Hours", ylab = "Weight (kg)")
  })

  # ── LOG ENTRY ─────────────────────────────────────────────
  output$save_msg <- renderText({ "" })

  observeEvent(input$btn_add, {
    new_row <- data.frame(
      date                  = as.Date(input$log_date),
      weight_kg             = input$log_weight,
      calories_intake       = input$log_calories_in,
      calories_burned       = input$log_calories_burned,
      steps                 = input$log_steps,
      water_ml              = input$log_water,
      sleep_hours           = input$log_sleep,
      exercise_type         = input$log_exercise_type,
      exercise_duration_min = input$log_exercise_min,
      systolic_bp           = input$log_systolic,
      diastolic_bp          = input$log_diastolic,
      heart_rate            = input$log_hr,
      mood                  = input$log_mood,
      stringsAsFactors = FALSE
    )
    rv$df <- rbind(rv$df, new_row) %>% arrange(date)
    write.csv(rv$df, csv_path, row.names = FALSE)
    showNotification("✅ Entry saved successfully!", type = "message", duration = 3)
  })

  # ── DATA TABLE ────────────────────────────────────────────
  tbl_data <- reactive({
    rv$df %>%
      filter(date >= input$tbl_daterange[1], date <= input$tbl_daterange[2]) %>%
      arrange(desc(date))
  })

  output$health_table <- renderDT({
    datatable(
      tbl_data(),
      options = list(
        pageLength = 15, scrollX = TRUE,
        dom = "frtip",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe"
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("health_data_export_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tbl_data(), file, row.names = FALSE)
  )
}

# ─────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
