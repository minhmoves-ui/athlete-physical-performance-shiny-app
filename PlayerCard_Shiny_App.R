# app.R
# KU-style Advanced Player Card + Radar App

library(shiny)
library(tidyverse)
library(fmsb)
library(scales)

# ---------- Load & Prepare Data ----------
raw <- read_csv("PSU_PlayerCard_Tscores.csv")

players <- raw %>%
  rename(
    PlayerName         = `Player Name`,
    Position           = Position,
    PhotoURL           = PhotoURL,
    PeakStrength       = `Peak Strength`,
    ExplosiveStrength  = `Explosive Strength`,
    Sprinting          = Sprinting,
    Acceleration       = Acceleration,
    Bounciness         = Bounciness,
    EnduranceRaw       = Endurance,
    StrengthScore      = `Strength Score`,
    SpeedScore         = `Speed Score`,
    EnduranceScore     = `Endurance Score`,
    OverallAthleticism = `Overall Athleticism`
  )

radar_vars <- c("StrengthScore", "SpeedScore",
                "EnduranceScore", "OverallAthleticism")

# KU colors for server-side use
ku_blue    <- "#0051BA"
ku_crimson <- "#E8000D"
ku_grey    <- "#F4F4F4"

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F4F4F4;
        font-family: 'Helvetica', 'Arial', sans-serif;
      }
      .player-card {
        background: linear-gradient(135deg, #E8000D 0%, #E8000D 40%, white 40%, white 100%);
        border-radius: 18px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.2);
        padding: 18px;
        color: #111111;
        min-height: 420px;
      }
      .player-photo {
        border-radius: 12px;
        border: 4px solid white;
        box-shadow: 0 3px 8px rgba(0,0,0,0.4);
        max-width: 100%;
      }
      .badge-position {
        background-color: #E8000D;
        color: white;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 0.9em;
      }
      .metric-tile {
        background-color: #FFFFFF;
        border-radius: 10px;
        padding: 10px 12px;
        margin-bottom: 8px;
        text-align: center;
        border: 1px solid #DDDDDD;
      }
      .metric-label {
        font-size: 0.8em;
        text-transform: uppercase;
        color: #555555;
        letter-spacing: 0.05em;
      }
      .metric-value {
        font-size: 1.35em;
        font-weight: bold;
      }

      /* --- PLAYER NAME --- */
      .player-name-title {
        color: white !important;
        font-size: 2.2em;
        font-weight: 800;
        font-style: italic;
        letter-spacing: 1px;
        text-shadow: 1px 1px 4px rgba(0,0,0,0.4);
        margin-bottom: 8px;
      }

      /* --- OVERALL ATHLETICISM LABEL --- */
      .overall-label {
        color: white !important;
        font-size: 1.2em;
        font-weight: bold;
        font-style: italic;
        text-transform: uppercase;
        text-shadow: 1px 1px 4px rgba(0,0,0,0.35);
        padding: 4px 10px;
        background-color: rgba(0,0,0,0.35);
        border-radius: 6px;
        display: inline-block;
        margin-bottom: 6px;
      }

      /* --- OVERALL SCORE NUMBER --- */
      .overall-score {
        color: white !important;
        font-size: 2.5em;
        font-weight: 900;
        font-style: italic;
        text-shadow: 2px 2px 6px rgba(0,0,0,0.45);
        margin-top: 5px;
        margin-bottom: 12px;
      }
    "))
  ),
  
  titlePanel("Soccer Athlete Profiling Card"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Player"),
      selectInput(
        "player", "Player:",
        choices = players$PlayerName,
        selected = players$PlayerName[1]
      ),
      hr(),
      h4("Select Position (Radar)"),
      selectInput(
        "pos", "Position:",
        choices = unique(players$Position),
        selected = unique(players$Position)[1]
      ),
      h4("Players to compare (radar)"),
      selectizeInput(
        "players_for_radar",
        "Players in this position (max 3):",
        choices = NULL,
        multiple = TRUE,
        options = list(maxItems = 3)
      ),
      br(),
      helpText("Cards use standardized T-scores or scaled scores.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Player Card",
                 br(),
                 uiOutput("player_card_ui")),
        tabPanel("Position Radar",
                 br(),
                 plotOutput("radar_plot", height = "520px"))
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  color_for_score <- function(x){
    if (is.na(x)) return("black")
    if (x >= 65) ku_crimson
    else if (x >= 55) "#1A7F1A"
    else if (x >= 45) "#333333"
    else "#888888"
  }
  
  # update radar player choices when position changes
  observeEvent(input$pos, {
    pos_players <- players %>%
      filter(Position == input$pos) %>%
      pull(PlayerName)
    
    updateSelectizeInput(
      session,
      "players_for_radar",
      choices = pos_players,
      selected = head(pos_players, 1)
    )
  })
  
  # ---- Player card UI ----
  output$player_card_ui <- renderUI({
    p <- players %>% filter(PlayerName == input$player) %>% slice(1)
    
    fluidRow(
      column(
        width = 4,
        div(class="player-card",
            img(src = p$PhotoURL, class = "player-photo"),
            br(), br(),
            span(class="badge-position", p$Position)
        )
      ),
      column(
        width = 8,
        div(class="player-card",
            h2(class = "player-name-title", p$PlayerName),
            
            div(class = "overall-label", "Overall Athleticism"),
            
            div(class = "overall-score",
                sprintf("%.1f", p$OverallAthleticism)),
            
            br(), br(),
            
            fluidRow(
              column(
                4,
                div(class="metric-tile",
                    div(class="metric-label","Strength Score"),
                    div(class="metric-value",
                        style=sprintf("color:%s;",
                                      color_for_score(p$StrengthScore)),
                        sprintf("%.1f", p$StrengthScore))
                )
              ),
              column(
                4,
                div(class="metric-tile",
                    div(class="metric-label","Speed Score"),
                    div(class="metric-value",
                        style=sprintf("color:%s;",
                                      color_for_score(p$SpeedScore)),
                        sprintf("%.1f", p$SpeedScore))
                )
              ),
              column(
                4,
                div(class="metric-tile",
                    div(class="metric-label","Endurance Score"),
                    div(class="metric-value",
                        style=sprintf("color:%s;",
                                      color_for_score(p$EnduranceScore)),
                        sprintf("%.1f", p$EnduranceScore))
                )
              )
            ),
            br(),
            
            fluidRow(
              column(
                6,
                div(class="metric-tile",
                    div(class="metric-label","Peak Strength"),
                    div(class="metric-value",
                        sprintf("%.1f", p$PeakStrength))
                ),
                div(class="metric-tile",
                    div(class="metric-label","Explosive Strength"),
                    div(class="metric-value",
                        sprintf("%.1f", p$ExplosiveStrength))
                ),
                div(class="metric-tile",
                    div(class="metric-label","Endurance"),
                    div(class="metric-value",
                        sprintf("%.1f", p$EnduranceRaw))
                )
              ),
              column(
                6,
                div(class="metric-tile",
                    div(class="metric-label","Sprinting"),
                    div(class="metric-value",
                        sprintf("%.1f", p$Sprinting))
                ),
                div(class="metric-tile",
                    div(class="metric-label","Acceleration"),
                    div(class="metric-value",
                        sprintf("%.1f", p$Acceleration))
                ),
                div(class="metric-tile",
                    div(class="metric-label","Bounciness"),
                    div(class="metric-value",
                        sprintf("%.1f", p$Bounciness))
                )
              )
            )
        )
      )
    )
  })
  
  # ---- Radar plot: Position Avg + selected players (max 3) ----
  output$radar_plot <- renderPlot({
    # All players in the selected position
    df_pos <- players %>%
      filter(Position == input$pos)
    
    req(nrow(df_pos) > 0)
    
    # Selected players for radar (max 3)
    sel_names <- input$players_for_radar
    if (is.null(sel_names) || length(sel_names) == 0) {
      sel_names <- head(df_pos$PlayerName, 1)
    }
    sel_names <- intersect(sel_names, df_pos$PlayerName)
    req(length(sel_names) > 0)
    
    # Position average row
    pos_mean <- df_pos %>%
      summarise(across(all_of(radar_vars), ~ mean(.x, na.rm = TRUE)))
    pos_mean$PlayerName <- "Position Avg"
    
    # Selected players rows
    df_sel <- df_pos %>%
      filter(PlayerName %in% sel_names) %>%
      select(PlayerName, all_of(radar_vars))
    
    # Combined data (Position Avg first, then players)
    df_plot <- bind_rows(pos_mean, df_sel) %>%
      relocate(PlayerName)
    
    # Fixed scale for radar (T-score-ish)
    max_val <- 80
    min_val <- 40
    
    # fmsb radarchart: first row = max, second = min
    max_row <- rep(max_val, length(radar_vars))
    min_row <- rep(min_val, length(radar_vars))
    
    radar_df <- rbind(max_row, min_row, df_plot[, radar_vars, drop = FALSE])
    colnames(radar_df) <- radar_vars
    
    # Safe, generic row names (avoid NA issues)
    rownames(radar_df) <- c("Max", "Min",
                            paste0("S", seq_len(nrow(df_plot))))
    
    # Colors: Position Avg grey, others colored
    n_series    <- nrow(df_plot)   # 1 (mean) + k players
    cols_border <- c("grey40", hue_pal()(n_series - 1))
    cols_fill   <- alpha(cols_border, 0.25)
    
    radarchart(
      radar_df,
      pcol        = cols_border,
      pfcol       = cols_fill,
      plwd        = 2,
      plty        = 1,
      cglcol      = "grey70",
      cglty       = 1,
      cglwd       = 0.8,
      axislabcol  = "grey40",
      vlcex       = 1.1,
      title       = paste("Position:", input$pos,
                          "- Position Avg + Selected Player(s)")
    )
    
    legend("topright",
           legend = df_plot$PlayerName,
           col    = cols_border,
           lty    = 1,
           lwd    = 2,
           bty    = "n",
           cex    = 0.85)
  })
}

# ---------- Run ----------
shinyApp(ui = ui, server = server)
