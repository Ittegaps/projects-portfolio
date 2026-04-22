# app.R

# biblioteki
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(stringr)
library(tidyr)
library(writexl)
library(scales)
library(grid)
library(forecast)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Ładowanie danych

find_data_dir <- function() {
  cand <- c(
    file.path(getwd(), "data"),                 # 1) ./data obok app.R (domyślne)
    getwd()                                     # 2) bieżący katalog (awaryjnie)
  )
  cand <- unique(na.omit(cand))
  for (d in cand) if (dir.exists(d)) return(normalizePath(d, winslash = "/", mustWork = FALSE))
  stop("Nie znaleziono katalogu z danymi. Utwórz folder 'data' obok app.R")
}

data_dir <- find_data_dir()
message("Using data dir: ", data_dir)

# Lista wymaganych plików
required_files <- c(
  "data_sample.csv"
)

# Sprawdzanie brakujących plików
missing <- required_files[!file.exists(file.path(data_dir, required_files))]
if (length(missing)) {
  stop("Brakuje plików w '", data_dir, "':\n- ", paste(missing, collapse = "\n- "),
       "\nUpewnij się, że wszystkie CSV są w folderze 'data'")
}

# Wczytanie CSV
raw   <- read.csv(file.path(data_dir, "final_data.csv"), stringsAsFactors = FALSE, sep = ',')

# Zmiana nazw
raw <- raw %>%
  rename(
    total_score = total_segment_score,
    TES = total_element_score,
    PCS = total_program_component_score_x,
    season = year,
    category = class,
    total_deductions = deductions_total,
    competition = subclass,
    kategoria_wiekowa = age_class,
    Composition_score = program_components_composition,
    Presentation_score = program_components_presentation,
    Skating_Skills_score = program_components_skating_skills,
    Transitions_score = program_components_transitions,
    Performance_score = program_components_performance,
    Interpretation_score = program_components_interpretation_of_the_music
  ) %>%
  mutate(
    season = as.numeric(as.character(season)),
    element_level = str_trim(element_level)
  )

raw$date <- as.Date(paste0(raw$season, "-01-01"))

numeric_cols <- c("total_score", "TES", "PCS", "total_deductions",
                  "falls_deduction", "falls_count", "time_violation_deduction",
                  "costume_prop_violation_deduction", "interruption_deduction",
                  "extra_element_deduction", "late_start_deduction",
                  "Composition_score", "Presentation_score", "Skating_Skills_score",
                  "Transitions_score", "Performance_score", "Interpretation_score")
existing_numeric_cols <- numeric_cols[numeric_cols %in% names(raw)]
df <- raw %>%
  mutate(across(all_of(existing_numeric_cols), as.numeric))

df <- df %>%
  mutate(
    combo_pos = suppressWarnings(as.integer(combo_pos)),
    in_combo = !is.na(combo_pos) & combo_pos != 0
  )

df <- df %>%
  mutate(
    element_type = trimws(as.character(element_type)),
    element_type = ifelse(
      grepl("^not\\s*performed$", element_type, ignore.case = TRUE),
      NA_character_,
      element_type
    )
  )

if ("name" %in% names(df)) {
  df <- dplyr::select(df, -name)
}


# --- User Interface (UI) ---
ui <- dashboardPage(

  dashboardHeader(title = NULL),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Podsumowanie", tabName = "dashboard", icon = icon("tachometer")),
      menuItem("Analiza Komponentów", tabName = "elements", icon = icon("chart-bar")),
      menuItem("Analiza Kombinacji", tabName = "combos", icon = icon("link")),
      menuItem("Rozkład Elementów", tabName = "distribution", icon = icon("arrows-alt-h")),
      menuItem("Analiza konkurencji", tabName = "competitor_analysis", icon = icon("users")),
      menuItem("Analiza odjęć punktowych", tabName = "errors", icon = icon("exclamation-triangle")),
      menuItem("Analiza Elementów", tabName = "elements_stats", icon = icon("sliders")),
      menuItem("Prognozy", tabName = "forecast", icon = icon("area-chart")),
      menuItem("Strategia", tabName = "strategy", icon = icon("lightbulb")),
      menuItem("Filtry Globalne", tabName = "filters", icon = icon("filter"))
    )
  ),
  
  dashboardBody(
    
    # ===== CSS =====
    tags$head(
      tags$style(HTML('
/* ================== ZMIENNE ================== */
:root{
  --header-h: 50px;

  /* Sidebar: szerokości */
  --sb-wide:   272px;
  --sb-narrow: 90px;

  /* Kolory */
  --sb-bg1:#2f3658; --sb-bg2:#3b4468;       /* tło sidebaru (gradient) */
  --item-bg1:#445084; --item-bg2:#3d487a;   /* piguły nieaktywne */
  --item-hover1:#5b66a8; --item-hover2:#6a74b8; /* piguły hover/active */
  --item-border:rgba(80,90,140,.25);
  --item-shadow:rgba(20,25,60,.25);
  --text:#edf1ff;
}


/* ================== TŁO APLIKACJI / BOXY ================== */
.content-wrapper, .right-side, .main-footer{
  background:
    radial-gradient(1200px 600px at 10% -10%, rgba(210,200,255,0.28), transparent 55%),
    radial-gradient(1000px 500px at 110% 0%, rgba(190,175,255,0.22), transparent 55%),
    linear-gradient(180deg, #F5F2FF 0%, #E7E0FF 100%);
  padding-top: calc(var(--header-h) + 10px) !important; /* treść pod headerem */
}
.box{
  border-radius:12px!important; border:1px solid rgba(70,70,120,.12);
  box-shadow:0 8px 18px rgba(40,40,80,.08); background:#FAF9FF;
}
.box .box-header{
  background:rgba(185,175,255,.22)!important;
  border-bottom:1px solid rgba(70,70,120,.12);
  border-radius:12px 12px 0 0!important;
}

/* ================== HEADER ================== */
.skin-blue .main-header,
.skin-blue .main-header .navbar,
.skin-blue .main-header .logo {
  background: linear-gradient(180deg, var(--sb-bg1), var(--sb-bg2)) !important;
  border: 0 !important;
  box-shadow: none !important;
}

.skin-blue .main-header .navbar{
  position: fixed; top:0; left:0; right:0;
  height: var(--header-h); line-height: var(--header-h);
  z-index:1030;
}

/* ukrycie domyślnych elementów */
.main-header .sidebar-toggle{ display:none!important; }
.main-header .logo{ display:none!important; }

/* tytuł na środku*/
#appTopTitle{
  position:absolute; top:0; left:50%; transform:translateX(-50%);
  height:var(--header-h); line-height:var(--header-h); text-align:center;
  font-size:20px; font-weight:600; letter-spacing:.2px;
  color: var(--text);           /* było #2E2B5E */
  pointer-events:none; white-space:nowrap; z-index:1001;
}
@media (max-width:900px){ #appTopTitle{ font-size:18px; } }

.skin-blue .main-header .navbar{
  margin-left: 0 !important;     /* BYŁO: ~230px */
}
.skin-blue .main-header .logo{
  width: 0 !important;
}

@media (min-width:768px){
  .sidebar-mini .main-header .navbar,
  .sidebar-mini.sidebar-collapse .main-header .navbar{
    margin-left: 0 !important;
  }
}


/* ================== SIDEBAR ================== */
.main-sidebar, .left-side{
  position: fixed; top: var(--header-h); bottom: 0; left: 0;
  width: var(--sb-wide);
  background: linear-gradient(180deg,var(--sb-bg1),var(--sb-bg2)) !important;
  padding-top: 0 !important;
  border-right: 0 !important; box-shadow: none !important;
}

.sidebar-mini .content-wrapper,
.sidebar-mini .right-side,
.sidebar-mini .main-footer{ margin-left: var(--sb-wide) !important; }
.sidebar-mini.sidebar-collapse .content-wrapper,
.sidebar-mini.sidebar-collapse .right-side,
.sidebar-mini.sidebar-collapse .main-footer{ margin-left: var(--sb-narrow) !important; }

/* Płynne przejścia tylko dla sidebaru/treści */
.main-sidebar, .content-wrapper, .right-side, .main-footer{
  transition: width .2s ease, margin-left .2s ease;
}

/* ================== MENU – ROZWINIĘTE ================== */
.sidebar-menu{ margin-top: 10px !important; }
.sidebar-menu > li { margin: 0 12px 12px 12px; }
.sidebar-menu > li > a{
  display:flex; align-items:center; gap:12px;
  padding:12px 14px; border-radius:16px;
  color:var(--text)!important; font-weight:600; letter-spacing:.2px;
  background:linear-gradient(180deg,var(--item-bg1),var(--item-bg2));
  border:1px solid var(--item-border);
  box-shadow:0 8px 18px var(--item-shadow);
  transition: background .18s ease, transform .18s ease;
}
.sidebar-menu > li > a:hover,
.sidebar-menu > li.active > a{
  background:linear-gradient(180deg,var(--item-hover1),var(--item-hover2))!important;
  transform: translateY(-1px);
}
.skin-blue .main-sidebar .sidebar-menu > li > a > i,
.skin-blue .main-sidebar .sidebar-menu > li > a > .fa,
.skin-blue .main-sidebar .sidebar-menu > li > a > .glyphicon,
.skin-blue .main-sidebar .sidebar-menu > li > a > .ion{
  width:28px; min-width:28px; text-align:center; font-size:18px;
}

/* ================== MENU – ZWINIĘTE ================== */
.sidebar-mini.sidebar-collapse .sidebar-menu > li{
  position: relative; margin: 8px 8px;
}
.sidebar-mini.sidebar-collapse .sidebar-menu > li::before{
  content:""; position:absolute; z-index:0;
  left: 8px; right: 8px; top: 4px; bottom: 4px;   /* SZEROKO I WYSOKO */
  border-radius: 22px;
  background: linear-gradient(180deg,var(--item-bg1),var(--item-bg2));
  border: 1px solid var(--item-border);
  box-shadow: 0 12px 24px var(--item-shadow);
  transition: background .18s ease, transform .18s ease;
}
.sidebar-mini.sidebar-collapse .sidebar-menu > li:hover::before,
.sidebar-mini.sidebar-collapse .sidebar-menu > li.active::before{
  background: linear-gradient(180deg,var(--item-hover1),var(--item-hover2));
  transform: translateY(-1px);
}

.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > span{ display:none !important; }
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > i,
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .fa,
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .glyphicon,
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .ion{
  font-size: 28px !important; width: 36px !important; min-width: 36px !important; text-align:center;
}

/* ================== TEKST / FONT ================== */
html, body{
  font-family: "Inter",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans","Liberation Sans",sans-serif !important;
  -webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale;
  text-rendering: optimizeLegibility;
}

:root{
  --sb-narrow: 84px;       /* szerokość zwiniętego sidebaru */
  --item-bg1:#445084; --item-bg2:#3d487a;
  --item-hover1:#5b66a8; --item-hover2:#6a74b8;
  --item-border:rgba(80,90,140,.25);
  --item-shadow:rgba(20,25,60,.25);
}

.sidebar-mini.sidebar-collapse .main-sidebar{ width: var(--sb-narrow) !important; }
.sidebar-mini.sidebar-collapse .main-sidebar .sidebar{ width: var(--sb-narrow) !important; }
.sidebar-mini.sidebar-collapse .sidebar-menu{ width: var(--sb-narrow) !important; }

.sidebar-mini.sidebar-collapse .sidebar-menu > li{
  position: relative;
  width: 100% !important;
  margin: 8px 0 !important;      /* równe odstępy w pionie */
  padding: 0 !important;
}

  .box .box-header,
  .box .box-header .box-title,
  .box.box-solid > .box-header,
  .box.box-solid > .box-header .box-title,
  .box .box-header h3 {
    color: #1f2937 !important;    /* ciemny szary zamiast bieli */
  }

.sidebar-mini.sidebar-collapse .sidebar-menu > li::before{
  content:"";
  position:absolute; z-index:0;
  left: 8px;                     /* zostaw po 8px luzu po bokach */
  right: 8px;
  top: 4px; bottom: 4px;         /* wysokość ok. 52–60px zależnie od fontu */
  border-radius: 22px;
  background: linear-gradient(180deg,var(--item-bg1),var(--item-bg2));
  border: 1px solid var(--item-border);
  box-shadow: 0 12px 24px var(--item-shadow);
  transition: background .18s ease, transform .18s ease;
}
.sidebar-mini.sidebar-collapse .sidebar-menu > li:hover::before,
.sidebar-mini.sidebar-collapse .sidebar-menu > li.active::before{
  background: linear-gradient(180deg,var(--item-hover1),var(--item-hover2));
  transform: translateY(-1px);
}

.sidebar-mini.sidebar-collapse .sidebar-menu > li > a{
  position: relative; z-index:1;
  display:flex; align-items:center; justify-content:center;
  height: 58px !important;
  padding: 0 !important;
  background: transparent !important;
  border: 0 !important;
  box-shadow: none !important;
}
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > span{ display:none !important; }
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > i,
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .fa,
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .glyphicon,
.sidebar-mini.sidebar-collapse .sidebar-menu > li > a > .ion{
  font-size: 28px !important;
  width: 36px !important; min-width: 36px !important; text-align:center;
}

:root{ --header-h: 50px; --sb-bg1:#2f3658; --sb-bg2:#3b4468; }
.main-sidebar{ position: fixed; top: var(--header-h); bottom: 0; left: 0; }

.main-sidebar, .content-wrapper, .right-side, .main-footer{
  transition: width .7s ease, margin-left .7s ease; /* było .2s */
}


')),
      
    # ===== JS =====
    tags$script(HTML("
      $(function(){
        // Wstrzyknij tytuł do navbara (środek)
        if(!$('#appTopTitle').length){
          $('<div id=\"appTopTitle\">Analiza danych łyżwiarstwa figurowego</div>')
            .appendTo('.main-header .navbar');
        }else{
          $('#appTopTitle').appendTo('.main-header .navbar');
        }

        // Hover-collapse sidebar
        var $b = $('body'), $sb = $('.main-sidebar');
        $b.addClass('sidebar-mini sidebar-collapse');
        function isDesktop(){ return $(window).width() > 768; }
        $sb.on('mouseenter', function(){ if(isDesktop()) $b.removeClass('sidebar-collapse'); });
        $sb.on('mouseleave', function(){ if(isDesktop()) $b.addClass('sidebar-collapse'); });
      });
      
      // Hover-collapse sidebar (z 0.5 s opóźnieniem zwijania)
      var $b = $('body'), $sb = $('.main-sidebar');
      $b.addClass('sidebar-mini sidebar-collapse');
      
      function isDesktop(){ return $(window).width() > 768; }
      
      var collapseTimer = null;
      
      $sb.on('mouseenter', function(){
        if(!isDesktop()) return;
        if(collapseTimer){ clearTimeout(collapseTimer); collapseTimer = null; }
        $b.removeClass('sidebar-collapse');               // pokaż od razu
      });
      
      $sb.on('mouseleave', function(){
        if(!isDesktop()) return;
        if(collapseTimer){ clearTimeout(collapseTimer); }
        collapseTimer = setTimeout(function(){
          $b.addClass('sidebar-collapse');                // schowaj po 500 ms
        }, 500);
});

    "))
),
    tabItems(
      # ZAKŁADKA --FILTRY GLOBALNE--
      tabItem(tabName = "filters",
              h2("Opcje globalnego filtrowania"),
              fluidRow(
                box(
                  title = "Dane do filtrowania", status = "primary", solidHeader = TRUE, width = 12,
                  column(4,
                         selectInput("filter_skater", "Wybierz zawodnika(ów):",
                                     choices = c("All", sort(unique(df$skater_id))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_category", "Wybierz kategorię(e):",
                                     choices = c("All", sort(unique(df$category))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_age_category", "Wybierz kategorię(e) wiekową(e):",
                                     choices = c("All", sort(unique(df$kategoria_wiekowa))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_season", "Wybierz sezon(y):",
                                     choices = c("All", sort(unique(df$season))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_competition", "Wybierz rodzaj(e):",
                                     choices = c("All", sort(unique(df$competition))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_segment", "Wybierz segment(y):",
                                     choices = c("All", sort(unique(df$segment))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_nation", "Wybierz region(y) zawodnika:",
                                     choices = c("All", sort(unique(df$nation))),
                                     selected = "All", multiple = TRUE)
                  ),
                  column(4,
                         selectInput("filter_club", "Wybierz klub(y) zawodnika:",
                                     choices = c("All", sort(unique(df$club))),
                                     selected = if ("Klub XYZ" %in% df$club) "Klub XYZ" else "All", multiple = TRUE)
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Przegląad danych po filtrowaniu", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("filtered_data_preview")
                )
              )
      ),
      
      # ZAKŁADKA --DASHBOARD--
      tabItem(tabName = "dashboard",
              h2("Ogólne podsumowanie"),
              fluidRow(
                valueBoxOutput("total_skaters_box"),
                valueBoxOutput("avg_total_score_sp_box"),
                valueBoxOutput("avg_total_score_fs_box")
              ),
              fluidRow(
                valueBoxOutput("personal_best_sp_box"),
                valueBoxOutput("personal_best_fs_box"),
                valueBoxOutput("personal_best_combined_box")
              ),
              fluidRow(
                box(
                  title = "Rozkład łącznej liczby punktów", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("total_score_dist_plot")
                ),
                box(
                  title = "Część punktowa techniczna [TES] vs część komponentów artystycznych [PCS]", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("tes_pcs_scatter_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Wykres porównawczy", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6, selectInput("compare_skater1", "Zawodnik 1:", choices = unique(df$skater_id))),
                    column(6, selectInput("compare_skater2", "Zawodnik 2:", choices = unique(df$skater_id)))
                  ),
                  plotlyOutput("skater_comparison_plot")
                )
              )
      ),
      
      # ZAKŁADKA --ANALIZA KOMPONENTÓW--
      tabItem(tabName = "elements",
              h2("Analiza punktów za komponenty"),
              fluidRow(
                box(
                  title = "Oceny komponentów (PCS) według zawodników", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("component_scores_plot")
                )
              )
      ),
      
      # ZAKŁĄDKA --Analiza kombinacji--
      tabItem(tabName = "combos", #bc why not
              h2("Analiza kombinacji"),
              fluidRow(
                box(title = "Filtry", status="primary", solidHeader=TRUE, width=12,
                    column(6, uiOutput("comb_element_symbol_ui")),
                    column(6, uiOutput("comb_element_level_ui"))
                )
              ),
              fluidRow(
                box(title="Najczęściej wykonywane kombinacje skokowe", status="primary",
                    solidHeader=TRUE, width=12, plotlyOutput("combo_frequency_plot"))
              ),
              fluidRow(
                box(title="Efektywność kombinacji (średnie GOE)", status="primary",
                    solidHeader=TRUE, width=6, plotlyOutput("combo_goe_plot")),
                box(title="Opłacalność wykonawcza kombinacji", status="primary",
                    solidHeader=TRUE, width=6, plotlyOutput("combo_profitability_plot"))
              )
      ),
      
      # ZAKŁADKA --ROZKŁAD ELEMENTÓW--
      tabItem(tabName = "distribution",
              h2("Rozkład elementów w programie"),
              fluidRow(
                box(title = "Efektywność elementów w połowach programu", status="primary",
                    solidHeader=TRUE, width=12,
                    fluidRow(
                      column(4, uiOutput("dist_type_ui")),    # (1)
                      column(4, uiOutput("dist_symbol_ui")),  # (2)
                      column(4, uiOutput("dist_level_ui"))    # (3)
                    ),
                    plotlyOutput("goe_by_program_half_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Średnie GOE według zawodników", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("goe_by_program_half_table")
                )
              ),
              fluidRow(
                box(title = "Top bigramy (kolejne 2 elementy)", status = "primary",
                    solidHeader = TRUE, width = 6, DTOutput("seq_bigrams_tbl")),
                box(title = "Top trigramy (kolejne 3 elementy)", status = "primary",
                    solidHeader = TRUE, width = 6, DTOutput("seq_trigrams_tbl"))
              ),
              fluidRow(
                box(title = "Najbardziej opłacalne bigramy (BV + GOE)", status = "info",
                    solidHeader = TRUE, width = 6, plotlyOutput("seq_bigrams_profit_plot")),
                box(title = "Przejścia typów elementów (P(next | current))", status = "info",
                    solidHeader = TRUE, width = 6, plotlyOutput("seq_type_transitions_heat"))
              )
      ),
      
      # ZAKŁADKA --ANALIZA KONKURENCFJI--
      tabItem(tabName = "competitor_analysis",
              h2("Analiza konkurencji zawodników oraz klubów"),
              # Filtry (typ > element > level)
              fluidRow(
                box(title = "Filtry", status="primary", solidHeader=TRUE, width=12,
                    column(4, uiOutput("comp_element_type_ui")),
                    column(4, uiOutput("comp_element_symbol_ui")),
                    column(4, uiOutput("comp_element_level_ui"))
                )
              ),
              # Ranking
              fluidRow(
                box(title = "Ranking najlepszych wykonawców elementu", status = "primary", solidHeader = TRUE, width = 12, # brzmi ni w 5 ni w 10, może zmienić
                    plotlyOutput("best_element_ranking_plot")
                )
              ),
              # Club vs Other
              fluidRow(
                box(title = "Średnie GOE według klubu", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8, uiOutput("comp_club_ui")),
                      column(4, radioButtons(
                        "club_compare_mode", "Porównanie:",
                        choices = c("Wybrany klub vs reszta" = "vs_rest", "Tylko wybrane kluby" = "selected_only"),
                        inline = TRUE, selected = "vs_rest"
                      ))
                    ),
                    plotlyOutput("club_vs_other_goe_plot")
                )
              ),
              fluidRow(
                box(title = "Grupowanie zawodników (k-means)", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3, sliderInput("cl_k", "Liczba klastrów:", min = 2, max = 8, value = 3, step = 1)),
                      column(3, actionButton("cl_run", "Aktualizuj", icon = icon("rotate-right")))
                    ),
                    br(),
                    plotlyOutput("cl_plot", height = "420px"),
                    br(),
                    DTOutput("cl_table")
                )
              )
      ),
      # ZAKŁADKA --ANALIZA ODJĘĆ--
      tabItem(tabName = "errors",
              h2("Analiza odjęć punktowych"),
              fluidRow(
                box(
                  title = "Odjęcia według typu", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("deduction_type_plot")
                ),
                box(
                  title = "Upadki na przestrzeni czasu", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("falls_over_time_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Elementy generujące najwięcej upadków", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("fall_element_plot")
                )
              )
      ),
      
      # ZAKŁADKA --ANALIZA ELEMENTÓW--
      tabItem(tabName = "elements_stats",
              h2("Analiza elementów"),
              fluidRow(
                box(title = "Filtry", status = "primary",
                    solidHeader = TRUE, width = 12,
                    column(4, uiOutput("stats_type_ui")),
                    column(4, uiOutput("stats_symbol_ui")),
                    column(4, uiOutput("stats_level_ui"))
                )
              ),
              fluidRow(
                box(
                  title = "Częstotliwość występowania wybranego elementu", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("element_usage_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Efektywność elementu", status = "primary", solidHeader = TRUE, width = 6,
                  column(6,
                         h4("Średnie GOE i stabilność"),
                         plotlyOutput("element_effectiveness_plot")),
                  column(6,
                         h4("Dodatnie vs ujemne GOE"),
                         plotlyOutput("goe_distribution_plot"))
                )
              ),
              fluidRow(
                box(
                  title = "Punktacja bazowa vs rzeczywisty wynik", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Opłacalność wykonawcza elementu"),
                  plotlyOutput("element_profitability_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Korelacje programowe (TES/PCS/komponenty/odjęcia)", status = "primary",
                  solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("corr_segment", "Segment:",
                                          choices = c("SP", "FS", "Oba"), selected = "Oba")),
                    column(4, selectInput("corr_method", "Metoda korelacji:",
                                          choices = c("Pearson", "Spearman"), selected = "Pearson")),
                    column(4, selectInput("corr_x", "Wykres punktowy – oś X:", choices = NULL)),
                    column(4, selectInput("corr_y", "Wykres punktowy – oś Y:", choices = NULL))
                  ),
                  plotlyOutput("corr_heatmap", height = "420px"),
                  br(),
                  plotlyOutput("corr_scatter", height = "380px")
                )
              )
      ),
      
      # ZAKŁADKA --PROGNOZY--
      tabItem(tabName = "forecast",
              h2("Prognoza wyników"),
              fluidRow(
                box(title="Ustawienia", status="primary", solidHeader=TRUE, width=12,
                    column(4, selectInput("fc_skater", "Zawodnik:", choices = sort(unique(df$skater_id)))),
                    column(4, radioButtons("fc_segment", "Segment:", choices=c("Wszystko"="all","SP"="sp","FS"="fs"), inline=TRUE)),
                    column(4, numericInput("fc_h", "Horyzont (sezony):", value=3, min=1, max=8)),
                    actionButton("fc_run", "Oblicz prognozę", icon=icon("play"))
                )
              ),
              fluidRow(
                box(title="Prognoza całkowitego wyniku", status="primary", solidHeader=TRUE, width=12,
                    plotlyOutput("fc_plot")
                )
              ),
              fluidRow(
                box(title="Wartości prognozy", status="info", solidHeader=TRUE, width=6, DTOutput("fc_table"))
              )
      ),
      
      # ZAKŁADKA --STRATEGIA--
      
      tabItem(tabName = "strategy",
              h2("Karta strategii zawodnika"),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Ustawienia",
                    fluidRow(
                      column(4, uiOutput("strat_skater_ui")),
                      column(4, selectInput("strat_segment", "Segment",
                                            c("SP","FS","SP+FS (łączny)"),
                                            selected = "SP+FS (łączny)")),
                      column(4, br(), actionButton("strat_refresh", "Odśwież", icon = icon("rotate")))
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("strat_kpi_risk"),
                valueBoxOutput("strat_kpi_stability"),
                valueBoxOutput("strat_kpi_pcs_gap")
              ),
              fluidRow(
                box(width = 6, status = "info", solidHeader = TRUE, title = "Strategia treningowa",
                    uiOutput("training_tips")
                ),
                box(width = 6, status = "warning", solidHeader = TRUE, title = "Strategia startowa",
                    uiOutput("competition_tips")
                )
              )
      )
   
    )
)
)
# --- Server Logic ---
server <- function(input, output, session) {
  
  #Filtry
  filtered_data <- reactive({
    data <- df
    filter_condition <- rep(TRUE, nrow(data))
    
    if (!is.null(input$filter_skater) && !("All" %in% input$filter_skater)) {
      filter_condition <- filter_condition & (data$skater_id %in% input$filter_skater)
    }
    if (!is.null(input$filter_category) && !("All" %in% input$filter_category)) {
      filter_condition <- filter_condition & (data$category %in% input$filter_category)
    }
    if (!is.null(input$filter_age_category) && !("All" %in% input$filter_age_category)) {
      filter_condition <- filter_condition & (data$kategoria_wiekowa %in% input$filter_age_category)
    }
    if (!is.null(input$filter_season) && !("All" %in% input$filter_season)) {
      filter_condition <- filter_condition & (data$season %in% as.numeric(input$filter_season))
    }
    if (!is.null(input$filter_competition) && !("All" %in% input$filter_competition)) {
      filter_condition <- filter_condition & (data$competition %in% input$filter_competition)
    }
    if (!is.null(input$filter_segment) && !("All" %in% input$filter_segment)) {
      filter_condition <- filter_condition & (data$segment %in% input$filter_segment)
    }
    if (!is.null(input$filter_nation) && !("All" %in% input$filter_nation)) {
      filter_condition <- filter_condition & (data$nation %in% input$filter_nation)
    }
    if (!is.null(input$filter_club) && !("All" %in% input$filter_club)) {
      filter_condition <- filter_condition & (data$club %in% input$filter_club)
    }
    
    filtered_df <- data[filter_condition, ]
    if (nrow(filtered_df) == 0) return(data[0, ])
    filtered_df
  })
  
  # Agregowanie do performance_id
  program_data <- reactive({
    req(nrow(filtered_data()) > 0)
    filtered_data() %>%
      group_by(performance_id, skater_id, season, category, competition, kategoria_wiekowa, segment, nation) %>%
      summarise(
        total_score = first(total_score),
        TES = first(TES),
        PCS = first(PCS),
        total_deductions = first(total_deductions),
        falls_count = first(falls_count),
        falls_deduction = first(falls_deduction),
        time_violation_deduction = first(time_violation_deduction),
        costume_prop_violation_deduction = first(costume_prop_violation_deduction),
        interruption_deduction = first(interruption_deduction),
        extra_element_deduction = first(extra_element_deduction),
        late_start_deduction = first(late_start_deduction),
        Composition_score = first(Composition_score),
        Presentation_score = first(Presentation_score),
        Skating_Skills_score = first(Skating_Skills_score),
        Transitions_score = first(Transitions_score),
        Performance_score = first(Performance_score),
        Interpretation_score = first(Interpretation_score),
        .groups = 'drop'
      ) %>%
      ungroup()
  })
  

  # ZAKŁADKA --PODSUMOWANIE--
  
  is_short <- function(x) grepl("\\b(short|sp)\\b", x, ignore.case = TRUE)
  is_free  <- function(x) grepl("\\b(free|fs|long)\\b", x, ignore.case = TRUE)
  
  output$avg_total_score_sp_box <- renderValueBox({
    data <- program_data()
    sp <- if (nrow(data) > 0) dplyr::filter(data, is_short(segment)) else data
    avg_sp <- if (nrow(sp) > 0) round(mean(sp$total_score, na.rm = TRUE), 2) else 0
    valueBox(avg_sp, "Średnie punkty – Short program", icon = icon("gauge-high"), color = "teal")
  })
  
  output$avg_total_score_fs_box <- renderValueBox({
    data <- program_data()
    fs <- if (nrow(data) > 0) dplyr::filter(data, is_free(segment)) else data
    avg_fs <- if (nrow(fs) > 0) round(mean(fs$total_score, na.rm = TRUE), 2) else 0
    valueBox(avg_fs, "Średnie punkty – Program dowolny", icon = icon("gauge"), color = "teal")
  })
  
  output$personal_best_sp_box <- renderValueBox({
    data <- program_data()
    sp <- if (nrow(data) > 0) dplyr::filter(data, is_short(segment)) else data
    if (nrow(sp) == 0) {
      return(valueBox(0, "Najlepszy wynik – Short program",
                      subtitle = "—", icon = icon("star-half-stroke"), color = "light-blue"))
    }
    best <- sp %>%
      dplyr::arrange(dplyr::desc(total_score)) %>%
      dplyr::slice(1)
    
    subtitle <- htmltools::HTML(paste0("Najlepszy wynik – Short program:<br>", best$skater_id, " – ", best$competition, " (", best$season, ")"))
    valueBox(round(best$total_score, 2),
             "Najlepszy wynik – Short program",
             subtitle = subtitle,
             icon = icon("star-half-stroke"), color = "light-blue")
  })
  
  output$personal_best_fs_box <- renderValueBox({
    data <- program_data()
    fs <- if (nrow(data) > 0) dplyr::filter(data, is_free(segment)) else data
    if (nrow(fs) == 0) {
      return(valueBox(0, "Najlepszy wynik – Program dowolny",
                      subtitle = "—", icon = icon("star"), color = "yellow"))
    }
    best <- fs %>%
      dplyr::arrange(dplyr::desc(total_score)) %>%
      dplyr::slice(1)
    
    subtitle <- htmltools::HTML(
      paste0("Najlepszy wynik – Program dowolny:<br>", best$skater_id, " – ", best$competition, " (", best$season, ")"))
    valueBox(round(best$total_score, 2),
             subtitle = subtitle,
             icon = icon("star"), color = "yellow")
  })
  
  strip_segment_suffix <- function(pid) {
    gsub("([_-])(sp|fs)$", "", pid, ignore.case = TRUE)
  }
  
  output$personal_best_combined_box <- renderValueBox({
    fd <- filtered_data()
    if (nrow(fd) == 0) {
      return(valueBox(0, "Najlepszy wynik łączny (SP+FS, ten sam event)",
                      icon = icon("trophy"), color = "maroon"))
    }
    
    seg_scores <- fd %>%
      dplyr::mutate(
        seg_from_id = dplyr::case_when(
          grepl("([_-])SP$", performance_id, ignore.case = TRUE) ~ "SP",
          grepl("([_-])FS$", performance_id, ignore.case = TRUE) ~ "FS",
          TRUE ~ NA_character_
        ),
        seg_from_col = dplyr::case_when(
          is_short(segment) ~ "SP",
          is_free(segment)  ~ "FS",
          TRUE ~ NA_character_
        ),
        seg_class = dplyr::coalesce(seg_from_col, seg_from_id),
        event_id  = strip_segment_suffix(performance_id)
      ) %>%
      dplyr::filter(!is.na(seg_class)) %>%
      dplyr::distinct(skater_id, event_id, seg_class, .keep_all = TRUE) %>%
      dplyr::group_by(skater_id, event_id, seg_class) %>%
      dplyr::summarise(
        seg_total   = suppressWarnings(as.numeric(dplyr::first(total_score))),
        competition = dplyr::first(competition),
        season      = dplyr::first(season),
        performance_id = dplyr::first(performance_id),
        .groups = "drop"
      )
    
    if (nrow(seg_scores) == 0) {
      return(valueBox(0, "Najlepszy wynik łączny (SP+FS, ten sam event)",
                      icon = icon("trophy"), color = "maroon"))
    }
    
    event_totals <- seg_scores %>%
      dplyr::group_by(skater_id, event_id) %>%
      dplyr::summarise(
        n_segments  = dplyr::n_distinct(seg_class),
        event_total = sum(seg_total, na.rm = TRUE),
        competition = dplyr::first(competition),
        season      = dplyr::first(season),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_segments == 2)
    
    if (nrow(event_totals) == 0) {
      return(valueBox(0, "Najlepszy wynik łączny (SP+FS, ten sam event)",
                      icon = icon("trophy"), color = "maroon"))
    }
    
    best_row <- event_totals %>%
      dplyr::arrange(dplyr::desc(event_total)) %>%
      dplyr::slice(1)
    
    subtitle <- htmltools::HTML(paste0("Najlepszy wynik łączny:<br>", best_row$skater_id, " – ", best_row$competition, " (", best_row$season, ")"))
    valueBox(round(best_row$event_total, 2),
             "Najlepszy wynik łączny (SP+FS, ten sam event)",
             subtitle = subtitle,
             icon = icon("trophy"), color = "maroon")
  })
  
  
  
  # ZAKŁADKA --ANALIZA ELEMENTÓW--
  filtered_elements_data <- reactive({
    data <- filtered_data() %>%
      dplyr::distinct(performance_id, element_order, .keep_all = TRUE) %>%
      dplyr::filter(!grepl("^not\\s*performed$", trimws(executed_elements_element_raw), ignore.case = TRUE)) %>%
      dplyr::mutate(
        actual_score = executed_elements_base_value + executed_elements_goe,
        goe_category = dplyr::case_when(
          executed_elements_goe > 0 ~ "Dodatnie",
          executed_elements_goe < 0 ~ "Ujemne",
          TRUE ~ "Zero"
        )
      )
    # (1) typ
    if (!is.null(input$stats_element_type_select) && input$stats_element_type_select != "All")
      data <- dplyr::filter(data, element_type == input$stats_element_type_select)
    
    # (2) element (goły symbol)
    if (!is.null(input$stats_element_symbol) && input$stats_element_symbol != "All")
      data <- dplyr::filter(data, element == input$stats_element_symbol)
    
    # (3) level
    if (!is.null(input$stats_element_level) && input$stats_element_level != "All")
      data <- dplyr::filter(data, toupper(element_level) == toupper(input$stats_element_level))
    
    # pojedyncze skoki: wyklucza kombinacje
    if (!is.null(input$stats_element_type_select) && input$stats_element_type_select == "Jump")
      data <- dplyr::filter(data, is.na(combo_pos) | combo_pos == 0)
    
    data
  })
  
  # --- KORELACJE ---
  corr_vars_available <- reactive({
    pd <- program_data()
    candidates <- c(
      "TES","PCS","total_score","total_deductions","falls_count",
      "Composition_score","Presentation_score","Skating_Skills_score",
      "Transitions_score","Performance_score","Interpretation_score"
    )
    intersect(candidates, names(pd))
  })
  
  observe({
    ch <- corr_vars_available()
    if (length(ch) == 0) ch <- character(0)
    updateSelectInput(session, "corr_x", choices = ch, selected = if (length(ch)>=1) ch[1] else NULL)
    updateSelectInput(session, "corr_y", choices = ch, selected = if (length(ch)>=2) ch[2] else NULL)
  })
  
  corr_df <- reactive({
    pd <- program_data()
    req(nrow(pd) > 0)
    seg <- switch(input$corr_segment %||% "Oba",
                  "SP"  = grepl("\\b(short|sp)\\b", pd$segment, ignore.case = TRUE),
                  "FS"  = grepl("\\b(free|fs|long)\\b", pd$segment, ignore.case = TRUE),
                  "Oba" = rep(TRUE, nrow(pd)))
    pd[seg, , drop = FALSE]
  })
  
  output$corr_heatmap <- renderPlotly({
    pd <- corr_df(); vars <- corr_vars_available()
    validate(need(length(vars) >= 2, "Za mało wspólnych zmiennych do korelacji."))
    
    M <- stats::cor(pd[vars], use = "pairwise.complete.obs",
                    method = tolower(input$corr_method %||% "pearson"))
    df_m <- as.data.frame(as.table(M))
    names(df_m) <- c("Var1","Var2","value")
    
    p <- ggplot(df_m, aes(Var1, Var2, fill = value, text = paste0(Var1, " vs ", Var2, ": ", round(value,3)))) +
      geom_tile() +
      scale_fill_gradient2(limits = c(-1,1), midpoint = 0) +
      coord_fixed() +
      labs(title = paste0("Macierz korelacji (", input$corr_method, ", segment: ", input$corr_segment, ")"),
           x = NULL, y = NULL, fill = "r") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$corr_scatter <- renderPlotly({
    pd <- corr_df(); vars <- corr_vars_available()
    req(input$corr_x %in% vars, input$corr_y %in% vars, input$corr_x != input$corr_y)
    
    d <- pd[, c("season", input$corr_x, input$corr_y), drop = FALSE]
    names(d) <- c("season","x","y")
    
    r <- suppressWarnings(stats::cor(d$x, d$y, use = "complete.obs",
                                     method = tolower(input$corr_method %||% "pearson")))
    ttl <- paste0("Zależność: ", input$corr_x, " vs ", input$corr_y,
                  "  |  r=", ifelse(is.finite(r), round(r, 3), "NA"),
                  " (", input$corr_method, ")")
    
    p <- ggplot(d, aes(x = x, y = y, text = paste0("Sezon: ", season,
                                                   "<br>X: ", round(x,2),
                                                   "<br>Y: ", round(y,2)))) +
      geom_point(alpha = .7) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
      labs(title = ttl, x = input$corr_x, y = input$corr_y) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ZAKŁADKA --ANALIZA KOMBINACJI--
  combinations_data <- reactive({
    data <- filtered_data() %>%
      dplyr::distinct(performance_id, element_order, .keep_all = TRUE) %>%
      dplyr::filter(element_type == "Jump", in_combo, executed_elements_base_value > 0)
    
    if (!is.null(input$comb_element_symbol) && input$comb_element_symbol != "All")
      data <- dplyr::filter(data, element == input$comb_element_symbol)
    
    if (!is.null(input$comb_element_level) && input$comb_element_level != "All")
      data <- dplyr::filter(data, toupper(element_level) == toupper(input$comb_element_level))
    
    dplyr::mutate(data,
                  total_actual_score = executed_elements_base_value + executed_elements_goe,
                  profitability      = executed_elements_goe
    )
  })
  
  output$comb_element_symbol_ui <- renderUI({
    df <- cascade_base_df()
    # tylko skoki w kombinacji
    ch <- sort(unique(na.omit(df$element[df$element_type == "Jump" & df$in_combo])))
    selectInput("comb_element_symbol", "Skok(i):", choices = c("All", ch), selected = "All")
  })
  
  # Combos: level
  output$comb_element_level_ui <- renderUI({
    df <- cascade_base_df()
    el <- input$comb_element_symbol %||% "All"
    
    if (el == "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == "Jump" & df$in_combo])))
    } else {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == "Jump" & df$element == el & df$in_combo])))
    }
    selectInput("comb_element_level", "Poziom:", choices = c("All", lv), selected = "All")
  })
  
  # Rozkład elementów w programie
  distribution_data <- reactive({
    data <- cascade_base_df() %>%
      dplyr::group_by(performance_id) %>%
      dplyr::mutate(
        max_order   = max(element_order, na.rm = TRUE),
        half_cut    = max_order / 2,
        połowa_programu = ifelse(element_order <= half_cut, "Pierwsza połowa", "Druga połowa")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        actual_score = executed_elements_score_of_panel
      )
    
    if (!is.null(input$dist_element_type_select) && input$dist_element_type_select != "All")
      data <- dplyr::filter(data, element_type == input$dist_element_type_select)
    
    if (!is.null(input$dist_element_symbol) && input$dist_element_symbol != "All")
      data <- dplyr::filter(data, element == input$dist_element_symbol)
    
    if (!is.null(input$dist_element_level) && input$dist_element_level != "All")
      data <- dplyr::filter(data, toupper(element_level) == toupper(input$dist_element_level))
    
    # Skoki pojedyncze: bez kombinacji
    if (!is.null(input$dist_element_type_select) && input$dist_element_type_select == "Jump")
      data <- dplyr::filter(data, !in_combo)
    
    data
  })
  
  # --- TO Z KLUBAMI ---
  output$comp_element_type_ui <- renderUI({
    df <- cascade_base_df()
    ch <- sort(unique(na.omit(df$element_type)))
    selectInput(
      "comp_element_type_select", "Typ elementu:",
      choices  = c("All", ch),
      selected = isolate(if (is.null(input$comp_element_type_select)) "All" else input$comp_element_type_select)
    )
  })
  
  cluster_features <- reactive({
    pd <- program_data()
    el <- cascade_base_df()
    
    validate(need(nrow(pd) > 0, "Brak danych po filtrach."))
    
    pcs_cols_all <- c("Composition_score","Presentation_score","Skating_Skills_score",
                      "Transitions_score","Performance_score","Interpretation_score")
    pcs_cols <- pcs_cols_all[pcs_cols_all %in% names(pd)]
    
    base_df <- pd %>%
      dplyr::group_by(skater_id) %>%
      dplyr::summarise(
        avg_total = mean(total_score, na.rm = TRUE),
        avg_TES   = mean(TES, na.rm = TRUE),
        avg_PCS   = mean(PCS, na.rm = TRUE),
        across(dplyr::all_of(pcs_cols), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
        .groups = "drop"
      )
    
    el2 <- elements_one_per_slot()
    if (!is.null(el2) && nrow(el2) > 0) {
      el_agg <- el2 %>%
        dplyr::mutate(
          is_jump = element_type == "Jump",
          is_combo = combo_pos %||% 0L
        ) %>%
        dplyr::group_by(skater_id) %>%
        dplyr::summarise(
          avg_goe_all   = mean(executed_elements_goe, na.rm = TRUE),
          avg_goe_jump  = mean(executed_elements_goe[is_jump & (is_combo == 0)], na.rm = TRUE),
          falls_rate    = sum(as.integer(element_fall), na.rm = TRUE) /
            dplyr::n(),
          .groups = "drop"
        )
      base_df <- dplyr::left_join(base_df, el_agg, by = "skater_id")
    }
    
    num_cols <- names(base_df)[sapply(base_df, is.numeric)]
    feat <- base_df[, c("skater_id", num_cols), drop = FALSE]
    keep <- vapply(feat[num_cols], function(v) stats::var(v, na.rm = TRUE) > 0, logical(1))
    num_cols <- num_cols[keep]
    feat <- feat[, c("skater_id", num_cols), drop = FALSE]
    
    validate(need(length(num_cols) >= 2, "Za mało sensownych cech do klastrowania."))
    
    feat
  })
  

  cluster_model <- eventReactive(input$cl_run, {
    feat <- cluster_features()
    X <- as.matrix(feat[ , -1, drop = FALSE])
    X[!is.finite(X)] <- NA
    for (j in seq_len(ncol(X))) {
      col <- X[, j]
      if (anyNA(col)) {
        med <- stats::median(col, na.rm = TRUE)
        col[is.na(col)] <- med
        X[, j] <- col
      }
    }
    # standaryzacja
    Xs <- scale(X)
    
    k <- input$cl_k %||% 3
    set.seed(42)
    km <- stats::kmeans(Xs, centers = k, nstart = 25)
    
    pca <- stats::prcomp(Xs, center = TRUE, scale. = FALSE)
    pcs <- as.data.frame(pca$x[, 1:2, drop = FALSE])
    pcs$skater_id <- feat$skater_id
    pcs$cluster <- factor(km$cluster)
    
    list(km = km, pcs = pcs, feat = feat, imp = summary(pca)$importance)
  })
  
  output$cl_plot <- renderPlotly({
    m <- cluster_model(); req(m)
    pcs <- m$pcs
    var_exp <- round(100 * m$imp[2, 1:2], 1)  # % wariancji na PC1/PC2
    
    plot_ly(
      pcs, x = ~PC1, y = ~PC2, color = ~cluster, type = "scatter", mode = "markers",
      text = ~paste0("<b>", skater_id, "</b><br>Klastr: ", cluster),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "PCA (2D) – podgląd klastrów",
        xaxis = list(title = paste0("PC1 (", var_exp[1], "%)")),
        yaxis = list(title = paste0("PC2 (", var_exp[2], "%)"))
      )
  })
  
  output$cl_table <- renderDT({
    m <- cluster_model(); req(m)
    feat <- m$feat
    cl   <- factor(m$km$cluster)
    
    # średnie cech w klastrach + liczności
    agg <- cbind(cluster = cl, feat[ , -1, drop = FALSE]) %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.x, na.rm = TRUE), 2)),
                       .groups = "drop") %>%
      dplyr::arrange(cluster)
    
    sizes <- as.data.frame(table(cluster = cl))
    out <- dplyr::left_join(sizes, agg, by = "cluster")
    datatable(out, rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  
  # --- ROZKŁAD ELEMENTÓW ---
  output$dist_type_ui <- renderUI({
    df <- cascade_base_df()
    ch <- sort(unique(na.omit(df$element_type)))
    selectInput(
      "dist_element_type_select", "Typ elementu:",
      choices  = c("All", ch),
      selected = isolate(if (is.null(input$dist_element_type_select)) "All" else input$dist_element_type_select)
    )
  })
  
  output$dist_symbol_ui <- renderUI({
    df <- cascade_base_df()
    et <- if (is.null(input$dist_element_type_select)) "All" else input$dist_element_type_select
    
    if (et == "Jump") {
      ch <- sort(unique(na.omit(df$element[df$element_type == "Jump" & !df$in_combo])))
    } else if (et != "All") {
      ch <- sort(unique(na.omit(df$element[df$element_type == et])))
    } else {
      ch <- sort(unique(na.omit(df$element)))
    }
    
    selectInput(
      "dist_element_symbol", "Element:",
      choices  = c("All", ch),
      selected = isolate(if (is.null(input$dist_element_symbol)) "All" else input$dist_element_symbol)
    )
  })
  
  output$dist_level_ui <- renderUI({
    df <- cascade_base_df()
    et <- if (is.null(input$dist_element_type_select)) "All" else input$dist_element_type_select
    el <- if (is.null(input$dist_element_symbol))      "All" else input$dist_element_symbol
    
    if (et == "Jump" && el != "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == "Jump" & df$element == el & !df$in_combo])))
    } else if (et != "Jump" && et != "All" && el != "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == et & df$element == el])))
    } else if (et != "All" && el == "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == et])))
    } else {
      lv <- sort(unique(na.omit(df$element_level)))
    }
    
    selectInput(
      "dist_element_level", "Poziom:",
      choices  = c("All", lv),
      selected = isolate(if (is.null(input$dist_element_level)) "All" else input$dist_element_level)
    )
  })
  
  # UX: reset kaskady w Rozkładzie
  observeEvent(input$dist_element_type_select, {
    updateSelectInput(session, "dist_element_symbol", selected = "All")
    updateSelectInput(session, "dist_element_level",  selected = "All")
  }, ignoreInit = TRUE)
  
  observeEvent(input$dist_element_symbol, {
    updateSelectInput(session, "dist_element_level", selected = "All")
  }, ignoreInit = TRUE)
  
  cascade_base_df <- reactive({
    filtered_data() %>%
      dplyr::distinct(performance_id, element_order, .keep_all = TRUE) %>%
      dplyr::filter(!grepl("^not\\s*performed$", trimws(executed_elements_element_raw), ignore.case = TRUE)) %>%
      dplyr::mutate(
        element_type  = trimws(as.character(element_type)),
        element       = trimws(as.character(element)),
        element_level = toupper(trimws(as.character(element_level))),
        in_combo      = ifelse(is.na(combo_pos), FALSE, combo_pos != 0),
        combo_pos     = as.integer(dplyr::coalesce(combo_pos, 0L)),
        element_fall  = as.integer(dplyr::coalesce(element_fall, 0L))
      )
  })
  
  elements_one_per_slot <- reactive({
    cascade_base_df() %>%
      dplyr::mutate(
        combo_pos    = as.integer(dplyr::coalesce(combo_pos, 0L)),
        element_fall = as.integer(dplyr::coalesce(element_fall, 0L))
      ) %>%
      dplyr::group_by(performance_id, element_order) %>%
      dplyr::slice_max(combo_pos, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
  })
  
  
  seq_slots <- reactive({

    clean_token <- function(x){
      x <- toupper(x %||% "")
      x <- gsub("[*+><!?qre\\-_: ]", "", x)
      x
    }

    is_full_jump <- function(tok){
      grepl("^[1-4](A|LZ|LO|F|S|T)$", tok, ignore.case = TRUE)
    }
    
    cascade_base_df() %>%
      arrange(performance_id, element_order, combo_pos) %>%
      group_by(performance_id, season, element_order) %>%
      summarise(
        # wektor surowych nazw w slocie
        raw = clean_token(executed_elements_element_raw),
        et  = element_type,
        cp  = combo_pos,

        token = {

          has_jump_combo <- any(in_combo & et == "Jump", na.rm = TRUE)
          if (has_jump_combo) {
            jumps <- raw[et == "Jump" & in_combo]
            cps   <- cp[et == "Jump" & in_combo]
            jumps <- jumps[order(cps)]
            jumps <- jumps[is_full_jump(jumps)]
            if (length(jumps)) paste(jumps, collapse = "+") else NA_character_
          } else {
            first_tok <- dplyr::first(raw)
            first_typ <- dplyr::first(et)
            if (identical(first_typ, "Jump")) {
              if (is_full_jump(first_tok)) first_tok else NA_character_
            } else {
              first_tok
            }
          }
        },
        type = {
          if (any(et == "Jump", na.rm = TRUE)) "Jump" else dplyr::first(et)
        },
        profit = sum(executed_elements_base_value + executed_elements_goe, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(token) & token != "") %>%
      arrange(performance_id, element_order) %>%
      group_by(performance_id) %>%
      mutate(
        next_token  = lead(token),
        next_type   = lead(type),
        next_profit = lead(profit)
      ) %>%
      ungroup()
  })
  
  
  
  # Bigramy (częstość + "opłacalność" dwóch kolejnych slotów)
  seq_bigrams <- reactive({
    seq_slots() %>%
      filter(!is.na(next_token)) %>%
      transmute(
        pair       = paste(token, "→", next_token),
        pair_profit= profit + coalesce(next_profit, 0)
      ) %>%
      group_by(pair) %>%
      summarise(
        n          = n(),
        avg_profit = round(mean(pair_profit, na.rm = TRUE),2),
        .groups = "drop"
      ) %>%
      arrange(desc(n))
  })
  
  # Trigramy (tylko do częstości – na tokenach)
  seq_trigrams <- reactive({
    seq_slots() %>%
      group_by(performance_id) %>%
      mutate(next2_token = lead(token, 2)) %>%
      ungroup() %>%
      filter(!is.na(next_token), !is.na(next2_token)) %>%
      transmute(triple = paste(token, "→", next_token, "→", next2_token)) %>%
      count(triple, name = "n") %>%
      arrange(desc(n))
  })
  
  # Macierz przejść typów (Jump/Spin/Step)
  seq_type_trans <- reactive({
    seq_slots() %>%
      filter(!is.na(type), !is.na(next_type)) %>%
      count(type, next_type, name = "n") %>%
      group_by(type) %>%
      mutate(p = n / sum(n)) %>%
      ungroup()
  })
  
  
  # ===== OUTPUTS =====
  output$seq_bigrams_tbl <- DT::renderDT({
    d <- seq_bigrams() %>% dplyr::slice_head(n = 15)
    DT::datatable(d, options = list(pageLength = 15, dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$seq_trigrams_tbl <- DT::renderDT({
    d <- seq_trigrams() %>% dplyr::slice_head(n = 15)
    DT::datatable(d, options = list(pageLength = 15, dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$seq_bigrams_profit_plot <- plotly::renderPlotly({
    d <- seq_bigrams() %>% dplyr::arrange(dplyr::desc(avg_profit)) %>% dplyr::slice_head(n = 10)
    req(nrow(d) > 0)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = reorder(pair, avg_profit), y = avg_profit,
                                         text = paste0(pair,
                                                       "<br>Śr. zysk: ", round(avg_profit, 2),
                                                       "<br>Wystąpienia: ", n))) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::labs(x = "Bigram", y = "Średni zysk (BV+GOE)", title = "Top bigramy wg opłacalności") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p, tooltip = "text")
  })
  
  output$seq_type_transitions_heat <- plotly::renderPlotly({
    d <- seq_type_trans(); req(nrow(d) > 0)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = type, y = next_type, fill = p,
                                         text = paste0("P(", next_type, " | ", type, ") = ",
                                                       scales::percent(p, 0.1), "<br>n = ", n))) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "#f0f0ff", high = "#3d487a") +
      ggplot2::labs(x = "Aktualny typ", y = "Następny typ", fill = "P") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p, tooltip = "text")
  })
  
  
  
  # ZAKŁADKA PROGNOZY
  
  is_short <- function(x) grepl("\\b(short|sp)\\b", x, ignore.case = TRUE)
  is_free  <- function(x) grepl("\\b(free|fs|long)\\b", x, ignore.case = TRUE)
  strip_segment_suffix <- function(pid) gsub("([_-])(sp|fs)$", "", pid, ignore.case = TRUE)
  
  
  # --- SERIA DO PROGNOZY: SP, FS albo SP+FS ---
  build_fc_series <- function(data, skater, seg_choice = c("all","SP","FS")) {
    seg_choice <- match.arg(seg_choice)
    d0 <- data %>% dplyr::filter(skater_id == skater)
    
    if (seg_choice == "all") {
      seg_scores <- d0 %>%
        dplyr::mutate(
          seg_from_id = dplyr::case_when(
            grepl("([_-])SP$", performance_id, ignore.case = TRUE) ~ "SP",
            grepl("([_-])FS$", performance_id, ignore.case = TRUE) ~ "FS",
            TRUE ~ NA_character_
          ),
          seg_from_col = dplyr::case_when(
            is_short(segment) ~ "SP",
            is_free(segment)  ~ "FS",
            TRUE ~ NA_character_
          ),
          seg_class = dplyr::coalesce(seg_from_col, seg_from_id),
          event_id  = strip_segment_suffix(performance_id)
        ) %>%
        dplyr::filter(!is.na(seg_class)) %>%
        dplyr::distinct(skater_id, event_id, seg_class, .keep_all = TRUE) %>%
        dplyr::group_by(skater_id, event_id, seg_class, season) %>%
        dplyr::summarise(seg_total = dplyr::first(total_score), .groups = "drop")
      
      # 2) suma SP+FS tylko dla eventów z obiema częściami
      event_totals <- seg_scores %>%
        dplyr::group_by(skater_id, event_id, season) %>%
        dplyr::summarise(
          n_segments  = dplyr::n_distinct(seg_class),
          event_total = sum(seg_total, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_segments == 2)
      
      # 3) sezonowa seria (jeśli w sezonie jest kilka eventów – bierzemy średnią łączną)
      series <- event_totals %>%
        dplyr::group_by(season) %>%
        dplyr::summarise(y = mean(event_total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(season)
      
      label <- "SP+FS (łączny wynik eventu)"
    } else {
      use_short <- (seg_choice == "SP")
      d1 <- if (use_short) dplyr::filter(d0, is_short(segment)) else dplyr::filter(d0, is_free(segment))
      series <- d1 %>%
        dplyr::group_by(season) %>%
        dplyr::summarise(y = mean(total_score, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(season)
      label <- if (use_short) "SP" else "FS"
    }
    
    list(series = series, label = label)
  }
  
  
  # --- DANE DO PROGNOZY: SP, FS albo SP+FS (łączny wynik eventu) ---
  fc_data <- eventReactive(input$fc_run, {
    pd <- program_data()
    req(nrow(pd) > 0, input$fc_skater)
    
    seg_choice <- switch(tolower(input$fc_segment),
                         "sp"="SP", "fs"="FS", "all"="all", "wszystko"="all", "all")
    
    fc <- build_fc_series(
      data = pd,
      skater = input$fc_skater,
      seg_choice = seg_choice
    )
    
    validate(need(nrow(fc$series) >= 2,
                  "Za mało obserwacji do prognozy (min. 2 sezony)."))
    fc
  })
  
  # --- MODEL ---
  fc_model <- eventReactive(input$fc_run, {
    fc <- fc_data(); req(fc$series)
    ts_y <- ts(fc$series$y, start = min(fc$series$season), frequency = 1)
    fit <- tryCatch(forecast::auto.arima(ts_y, stepwise=TRUE, approximation=TRUE),
                    error = function(e) NULL)
    if (is.null(fit)) fit <- forecast::ets(ts_y)
    list(fit = fit, seasons_hist = fc$series$season, label = fc$label)
  })
  
  # --- PROGNOZA ---
  fc_result <- eventReactive(input$fc_run, {
    m <- fc_model(); req(m$fit)
    h <- as.integer(ifelse(is.na(input$fc_h), 2, input$fc_h))
    forecast::forecast(m$fit, h = max(1, h))
  })
  
  observe({
    pd <- program_data()
    sks <- sort(unique(pd$skater_id))
    
    # zachowaj poprzedni wybór jeśli nadal jest dostępny
    prev <- isolate(input$fc_skater)
    sel  <- if (!is.null(prev) && prev %in% sks) prev else dplyr::first(sks)
    
    updateSelectInput(session, "fc_skater",
                      choices = sks,
                      selected = sel)
  })
  
  # ZAKŁADKA STRATEGIA
  
  # UI select dla zawodnika (po filtrach globalnych)
  output$strat_skater_ui <- renderUI({
    skaters <- sort(unique(program_data()$skater_id))
    selectInput("strat_skater", "Zawodnik", choices = skaters, selected = head(skaters, 1))
  })
  
  is_short <- function(x) grepl("\\b(short|sp)\\b", x, ignore.case = TRUE)
  is_free  <- function(x) grepl("\\b(free|fs|long)\\b", x, ignore.case = TRUE)
  
  strat_metrics <- eventReactive(input$strat_refresh, {
    req(input$strat_skater)
    
    # --- dane programowe (po performance_id) ---
    pd <- program_data() %>%
      dplyr::filter(skater_id == input$strat_skater)
    
    pd <- switch(input$strat_segment,
                 "SP"             = dplyr::filter(pd, is_short(segment)),
                 "FS"             = dplyr::filter(pd, is_free(segment)),
                 "SP+FS (łączny)" = pd
    )
    
    # --- dane elementowe ---
    es <- cascade_base_df() %>%
      dplyr::filter(skater_id == input$strat_skater) %>%
      { if (input$strat_segment=="SP") dplyr::filter(., is_short(segment))
        else if (input$strat_segment=="FS") dplyr::filter(., is_free(segment)) else . } %>%
      dplyr::mutate(element_fall = as.integer(dplyr::coalesce(element_fall, 0L)))
    
    # ===== 1. Stabilność i ryzyko =====
    goe_sd    <- suppressWarnings(sd(es$executed_elements_goe, na.rm = TRUE))
    fall_rate <- mean(es$element_fall, na.rm = TRUE)   # odsetek elementów z upadkiem
    
    # najbardziej ryzykowny element (warunek: min 3 próby)
    riskiest_element <- NA
    riskiest_rate    <- NA_real_
    if (nrow(es) > 0) {
      tmp_risk <- es %>%
        dplyr::group_by(executed_elements_element_raw) %>%
        dplyr::summarise(
          attempts = dplyr::n(),
          falls    = sum(element_fall, na.rm = TRUE),
          rate     = ifelse(attempts > 0, falls / attempts, NA_real_),
          .groups  = "drop"
        ) %>%
        dplyr::filter(attempts >= 3, !is.na(rate)) %>%
        dplyr::arrange(dplyr::desc(rate), dplyr::desc(attempts)) %>%
        dplyr::slice(1)
      if (nrow(tmp_risk) == 1) {
        riskiest_element <- tmp_risk$executed_elements_element_raw
        riskiest_rate    <- tmp_risk$rate
      }
    }
    
    # ===== 2. PCS – najsłabszy i różnice =====
    pcs_cols <- c("Composition_score","Presentation_score","Skating_Skills_score",
                  "Transitions_score","Performance_score","Interpretation_score")
    pcs_cols <- pcs_cols[pcs_cols %in% names(pd)]
    
    pcs_gap  <- NA_real_
    pcs_weak <- NA
    pcs_strong <- NA
    if (length(pcs_cols) > 0 && nrow(pd) > 0) {
      pcs_avg   <- colMeans(pd[, pcs_cols], na.rm = TRUE)
      ord       <- sort(pcs_avg, decreasing = TRUE)
      pcs_weak  <- names(ord)[length(ord)]
      pcs_strong<- names(ord)[1]
      pcs_gap   <- max(pcs_avg, na.rm = TRUE) - min(pcs_avg, na.rm = TRUE)
    }
    
    # ===== 3. Rozkład GOE w połowach programu =====
    diff_half <- NA_real_
    if (nrow(es) > 0) {
      dist <- cascade_base_df() %>%
        dplyr::filter(skater_id == input$strat_skater) %>%
        { if (input$strat_segment=="SP") dplyr::filter(., is_short(segment))
          else if (input$strat_segment=="FS") dplyr::filter(., is_free(segment)) else . } %>%
        dplyr::group_by(performance_id) %>%
        dplyr::mutate(
          max_order = max(element_order, na.rm = TRUE),
          half_cut  = max_order / 2,
          half      = ifelse(element_order <= half_cut, "H1", "H2")
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(half) %>%
        dplyr::summarise(avg_goe = mean(executed_elements_goe, na.rm = TRUE),
                         .groups = "drop")
      if (nrow(dist) == 2) {
        diff_half <- dist$avg_goe[dist$half == "H2"] - dist$avg_goe[dist$half == "H1"]
      }
    }
    
    # ===== 4. Kombinacje skokowe =====
    comb <- combinations_data() %>% dplyr::filter(skater_id == input$strat_skater)
    best_combo <- NA
    if (nrow(comb) > 0) {
      tmp <- comb %>%
        dplyr::group_by(element_combination) %>%
        dplyr::summarise(
          avg_profit = mean(executed_elements_goe + executed_elements_base_value, na.rm = TRUE),
          n          = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(avg_profit), dplyr::desc(n)) %>%
        dplyr::slice(1)
      if (nrow(tmp) > 0) {
        best_combo <- paste0(tmp$element_combination,
                             " (średnio ", round(tmp$avg_profit, 2), " pkt; próby: ", tmp$n, ")")
      }
    }
    
    # ===== 5. Poziom wyników i trend sezonowy =====
    avg_total   <- NA_real_
    best_total  <- NA_real_
    trend_total <- NA_real_
    tes_share   <- NA_real_
    
    if (nrow(pd) > 0) {
      avg_total  <- mean(pd$total_score, na.rm = TRUE)
      best_total <- max(pd$total_score,  na.rm = TRUE)
      
      # średni udział TES vs PCS
      if (all(c("TES","total_score") %in% names(pd))) {
        tes_share <- mean(pd$TES / ifelse(pd$total_score > 0, pd$total_score, NA_real_), na.rm = TRUE)
      }
      
      # trend po sezonach – jeśli min. 2 sezony
      by_season <- pd %>%
        dplyr::group_by(season) %>%
        dplyr::summarise(mean_total = mean(total_score, na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::arrange(season)
      if (nrow(by_season) >= 2) {
        fit <- tryCatch(lm(mean_total ~ season, data = by_season),
                        error = function(e) NULL)
        if (!is.null(fit)) {
          trend_total <- coef(fit)[["season"]]
        }
      }
    }
    
    list(
      goe_sd           = goe_sd,
      fall_rate        = fall_rate,
      pcs_weak         = pcs_weak,
      pcs_strong       = pcs_strong,
      pcs_gap          = pcs_gap,
      diff_half        = diff_half,
      best_combo       = best_combo,
      riskiest_element = riskiest_element,
      riskiest_rate    = riskiest_rate,
      avg_total        = avg_total,
      best_total       = best_total,
      trend_total      = trend_total,
      tes_share        = tes_share
    )
  })
  
  # --- Strategia treningowa ---
  output$training_tips <- renderUI({
    m <- strat_metrics(); req(m)
    tips <- c()
    
    # 1) praca nad PCS
    if (!is.na(m$pcs_weak)) {
      tips <- c(
        tips,
        paste0("Dodaj 1–2 jednostki tygodniowo ukierunkowane na komponent ",
               gsub("_score","", m$pcs_weak),
               " (ćwiczenia choreograficzne, praca na krawędziach, interpretacja muzyki).")
      )
    }
    if (!is.na(m$pcs_gap) && m$pcs_gap > 0.8) {
      tips <- c(
        tips,
        "Różnice między komponentami są wyraźne – rozważ indywidualne zajęcia artystyczne (taniec, balet, praca nad ekspresją) w celu domknięcia najsłabszego PCS."
      )
    }
    
    # 2) stabilność GOE
    if (!is.na(m$goe_sd)) {
      if (m$goe_sd >= 1.1) {
        tips <- c(
          tips,
          "Duża zmienność GOE – w treningu wprowadź serie powtórzeń kluczowych skoków/ piruetów z kontrolą jakości (nagranie wideo + omawianie wykonania po każdym powtórzeniu)."
        )
      } else {
        tips <- c(
          tips,
          "GOE jest relatywnie stabilne – utrzymuj aktualny wolumen powtórzeń, skup się na stopniowym podnoszeniu trudności (wyższe poziomy piruetów, kombinacje z trudnym wejściem)."
        )
      }
    }
    
    # 3) praca nad elementem najbardziej ryzykownym
    if (!is.na(m$riskiest_element) && !is.na(m$riskiest_rate)) {
      tips <- c(
        tips,
        paste0("Element o najwyższym ryzyku upadku: ", m$riskiest_element,
               " (ok. ", round(100 * m$riskiest_rate, 1), "% upadków przy ≥3 próbach). ",
               "Wprowadź blok techniczny 2–3× w tygodniu poświęcony tylko temu elementowi – najpierw w prostym wejściu, potem w warunkach zbliżonych do programu.")
      )
    }
    
    # 4) proporcje TES / PCS
    if (!is.na(m$tes_share)) {
      if (m$tes_share > 0.6) {
        tips <- c(
          tips,
          "Profil punktowy jest mocno techniczny (TES > 60% wyniku) – w cyklu mezocyklu dodaj akcent na pracę nad komponentami (układy kroków, choreografia, prezentacja), żeby zwiększyć bezpieczeństwo punktowe przy słabszym dniu technicznym."
        )
      } else if (m$tes_share < 0.5) {
        tips <- c(
          tips,
          "Profil punktowy jest relatywnie artystyczny (TES < 50% wyniku) – zaplanuj w mikrocyklu więcej pracy nad powtarzalnością skoków z bonusem oraz stabilnością kombinacji."
        )
      }
    }
    
    # 5) trend sezonowy
    if (!is.na(m$trend_total)) {
      if (m$trend_total > 0.5) {
        tips <- c(
          tips,
          "Trend sezonowy jest rosnący – możesz stopniowo wprowadzać trudniejsze konfiguracje elementów w treningu, pamiętając o zachowaniu rezerwy bezpieczeństwa przed głównymi imprezami."
        )
      } else if (m$trend_total < -0.5) {
        tips <- c(
          tips,
          "Trend sezonowy jest spadkowy – warto wrócić na chwilę do prostszych zadań technicznych (większy nacisk na jakość skoków bazowych) oraz przeanalizować obciążenia treningowe/pauzy zdrowotne."
        )
      }
    }
    
    if (length(tips) == 0)
      tips <- "Brak szczególnych wskazówek treningowych – aktualny plan jest spójny z profilem zawodnika."
    
    tags$ul(lapply(tips, tags$li))
  })
  
  # --- Strategia startowa ---
  output$competition_tips <- renderUI({
    m <- strat_metrics(); req(m)
    tips <- c()
    
    # 1) zarządzanie ryzykiem upadków
    if (!is.na(m$fall_rate)) {
      if (m$fall_rate >= 0.08) {
        tips <- c(
          tips,
          paste0("Średnio ok. ", round(100*m$fall_rate, 1),
                 "% elementów kończy się upadkiem – w programie startowym rozważ uproszczenie jednego z najbardziej ryzykownych skoków lub zmianę wejścia na bezpieczniejsze.")
        )
      } else if (m$fall_rate <= 0.04) {
        tips <- c(
          tips,
          "Niskie ryzyko upadku – w głównych startach można rozważyć wprowadzenie jednego elementu o wyższej bazie (np. trudniejsza kombinacja) przy zachowaniu obecnej struktury rozgrzewki."
        )
      }
    }
    
    # 2) rozkład jakości między połowami programu
    if (!is.na(m$diff_half)) {
      if (m$diff_half < -0.15) {
        tips <- c(
          tips,
          "GOE w drugiej połowie programu jest wyraźnie słabsze – na startach ustaw najtrudniejsze skoki bliżej początku programu, a w drugiej połowie zostaw elementy pewniejsze / krótsze kombinacje."
        )
      } else if (m$diff_half > 0.15) {
        tips <- c(
          tips,
          "Druga połowa programu jest punktowo mocniejsza – możesz utrzymać tam kluczowe skoki z bonusem, pamiętając o odpowiedniej gospodarce siłami (mocna rozgrzewka, kontrola tempa przejazdu)."
        )
      } else {
        tips <- c(
          tips,
          "Różnice między pierwszą i drugą połową programu są niewielkie – struktura programu jest zbalansowana, można skupić się na dopracowaniu detali wykonawczych."
        )
      }
    }
    
    # 3) wykorzystanie najlepszej kombinacji
    if (!is.na(m$best_combo)) {
      tips <- c(
        tips,
        paste0("Najbardziej opłacalna kombinacja w historii przejazdów: ", m$best_combo,
               ". Powinna pozostać stałym punktem programu – nie eksperymentuj nią tuż przed głównymi zawodami.")
      )
    }
    
    # 4) mocny vs słaby PCS na starcie
    if (!is.na(m$pcs_strong)) {
      tips <- c(
        tips,
        paste0("Najsilniejszy komponent: ", gsub("_score","", m$pcs_strong),
               " – na startach podkreśl ten atut (np. wyróżnij fragment choreografii, pracę rąk, kontakt z muzyką w miejscu, gdzie zawodnik czuje się najpewniej).")
      )
    }
    if (!is.na(m$pcs_weak)) {
      tips <- c(
        tips,
        paste0("Najsłabszy komponent: ", gsub("_score","", m$pcs_weak),
               " – w trakcie startu wysyłaj zawodnikowi jedno proste hasło techniczne związane z tym obszarem (np. „głowa w górze”, „dokończ ruch”, „jedź na krawędzi”), zamiast przeciążać go wieloma uwagami.")
      )
    }
    
    # 5) poziom wyników
    if (!is.na(m$avg_total) && !is.na(m$best_total)) {
      tips <- c(
        tips,
        paste0("Średni wynik w wybranym segmencie: ~", round(m$avg_total, 1),
               " pkt, rekord życiowy (wg danych): ", round(m$best_total, 1),
               " pkt – cele startowe możesz ustawiać w przedziale ",
               round(m$avg_total - 2, 1), "–", round(m$best_total, 1),
               " pkt, w zależności od rangi zawodów.")
      )
    }
    
    if (length(tips) == 0)
      tips <- "Brak szczególnych zaleceń startowych – program jest zbalansowany, a profil ryzyka adekwatny."
    
    tags$ul(lapply(tips, tags$li))
  })
  
  
  # KPI (valueBoxy)
  output$strat_kpi_risk <- renderValueBox({
    m <- strat_metrics(); req(m)
    col <- if      (m$fall_rate >= 0.08) "red" 
    else if (m$fall_rate >= 0.04) "yellow" 
    else                          "green"
    valueBox(
      paste0(round(100*m$fall_rate,1), "%"),
      "Ryzyko upadku na element",
      icon = icon("triangle-exclamation"), color = col
    )
  })
  
  output$strat_kpi_stability <- renderValueBox({
    m <- strat_metrics(); req(m)
    col <- if (m$goe_sd >= 1.1) "yellow" else "teal"
    valueBox(round(m$goe_sd,2), "Stabilność GOE (SD)", icon = icon("wave-square"), color = col)
  })
  
  output$strat_kpi_pcs_gap <- renderValueBox({
    m <- strat_metrics(); req(m)
    lbl <- if (is.na(m$pcs_weak)) "PCS – brak danych" else paste("Najsłabszy PCS:", gsub("_score","",m$pcs_weak))
    valueBox(ifelse(is.na(m$pcs_gap), "-", round(m$pcs_gap,2)), lbl, icon = icon("arrows-left-right-to-line"), color = "purple")
  })
  
  
  # Wskazówki tekstowe (zwięzłe reguły)
  output$coach_tips <- renderUI({
    m <- strat_metrics(); req(m)
    tips <- c()
    
    if (!is.na(m$pcs_weak))
      tips <- c(tips, paste0("Wzmocnij ", gsub("_score","",m$pcs_weak), ": 1–2 sesje/tydzień (skup na choreo/edge quality)."))
    
    if (!is.na(m$diff_half)) {
      if (m$diff_half < -0.15) tips <- c(tips, "Lepsze GOE w 1. połowie – rozważ przeniesienie trudnego skoku do H1.")
      if (m$diff_half >  0.15) tips <- c(tips, "Lepsze GOE w 2. połowie – opłaca się trzymać bonus/kluczowe skoki w H2.")
    }
    
    if (!is.na(m$best_combo))
      tips <- c(tips, paste0("Najbardziej opłacalna kombinacja: ", m$best_combo, ". Utrzymaj ją jako „go-to”."))
    
    if (!is.na(m$fall_rate) && m$fall_rate >= 0.08)
      tips <- c(tips, "Wysokie ryzyko upadku – ogranicz próby elementu granicznego na starcie lub uprość wariant.")
    
    if (length(tips) == 0) tips <- "Brak szczególnych zaleceń – utrzymuj bieżący plan."
    
    tags$ul(lapply(tips, function(t) tags$li(t)))
  })
  
  
  # ZAKŁADKA Z KLUBAMI - analiza konkurencji i guess ---
  
  falls_over_time_data <- reactive({
    pd <- program_data()
    
    if ("falls_count" %in% names(pd) && any(pd$falls_count > 0, na.rm = TRUE)) {
      pd %>%
        dplyr::group_by(skater_id, season) %>%
        dplyr::summarise(
          total_falls = sum(falls_count, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      elements_one_per_slot() %>%
        dplyr::group_by(skater_id, season) %>%
        dplyr::summarise(
          total_falls = sum(element_fall, na.rm = TRUE),
          .groups = "drop"
        )
    }
  })
  
  
  output$falls_over_time_plot <- renderPlotly({
    plot_data <- falls_over_time_data()
    if (nrow(plot_data) == 0) {
      return(ggplotly(ggplot() +
                        annotate("text", x=.5, y=.5, label="Brak rekordów upadków dla wybranych filtrów.", size=5) +
                        theme_void()))
    }
    p <- ggplot(plot_data, aes(x = season, y = total_falls, color = skater_id, group = skater_id,
                               text = paste("Zawodnik:", skater_id, "<br>Sezon:", season, "<br>Upadki:", total_falls))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Całkowita liczba upadków na sezon", x = "Sezon", y = "Liczba upadków") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$comp_club_ui <- renderUI({
    df <- competitor_analysis_data()
    ch <- sort(unique(na.omit(df$club)))
    selectizeInput("comp_club_select", "Klub(y):", choices = ch, multiple = TRUE,
                   options = list(placeholder = "Wybierz jeden lub więcej klubów"))
  })
  
  
  output$comp_element_symbol_ui <- renderUI({
    df <- cascade_base_df()
    et <- input$comp_element_type_select %||% "All"
    if (et == "Jump") {
      ch <- sort(unique(na.omit(df$element[df$element_type == "Jump" & !df$in_combo])))
    } else if (et != "All") {
      ch <- sort(unique(na.omit(df$element[df$element_type == et])))
    } else {
      ch <- sort(unique(na.omit(df$element)))
    }
    selectInput("comp_element_symbol", "Element:", choices = c("All", ch), selected = "All")
  })
  
  output$comp_element_level_ui <- renderUI({
    df <- cascade_base_df()
    et <- input$comp_element_type_select %||% "All"
    el <- input$comp_element_symbol      %||% "All"
    if (et == "Jump" && el != "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == "Jump" & df$element == el & !df$in_combo])))
    } else if (et != "Jump" && et != "All" && el != "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == et & df$element == el])))
    } else if (et != "All" && el == "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == et])))
    } else {
      lv <- sort(unique(na.omit(df$element_level)))
    }
    selectInput("comp_element_level", "Poziom:", choices = c("All", lv), selected = "All")
  })
  
  # Reset kaskady dla UX
  observeEvent(input$comp_element_type_select, {
    updateSelectInput(session, "comp_element_symbol", selected = "All")
    updateSelectInput(session, "comp_element_level",  selected = "All")
  }, ignoreInit = TRUE)
  observeEvent(input$comp_element_symbol, {
    updateSelectInput(session, "comp_element_level",  selected = "All")
  }, ignoreInit = TRUE)
  
  
  # --- STATYSTYKA ELEMENTÓW ---
  output$stats_type_ui <- renderUI({
    df <- cascade_base_df()
    ch <- sort(unique(na.omit(df$element_type)))
    selectInput(
      "stats_element_type_select", "Typ elementu:",
      choices  = c("All", ch),
      selected = isolate(if (is.null(input$stats_element_type_select)) "All" else input$stats_element_type_select)
    )
  })
  
  output$stats_symbol_ui <- renderUI({
    df <- cascade_base_df()
    et <- if (is.null(input$stats_element_type_select)) "All" else input$stats_element_type_select
    
    if (et == "Jump") {
      ch <- sort(unique(na.omit(df$element[df$element_type == "Jump" & !df$in_combo])))
    } else if (et != "All") {
      ch <- sort(unique(na.omit(df$element[df$element_type == et])))
    } else {
      ch <- sort(unique(na.omit(df$element)))
    }
    
    selectInput(
      "stats_element_symbol", "Element:",
      choices  = c("All", ch),
      selected = isolate(if (is.null(input$stats_element_symbol)) "All" else input$stats_element_symbol)
    )
  })
  
  output$stats_level_ui <- renderUI({
    df <- cascade_base_df()
    et <- if (is.null(input$stats_element_type_select)) "All" else input$stats_element_type_select
    el <- if (is.null(input$stats_element_symbol))      "All" else input$stats_element_symbol
    
    if (et == "Jump" && el != "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == "Jump" & df$element == el & !df$in_combo])))
    } else if (et != "Jump" && et != "All" && el != "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == et & df$element == el])))
    } else if (et != "All" && el == "All") {
      lv <- sort(unique(na.omit(df$element_level[df$element_type == et])))
    } else {
      lv <- sort(unique(na.omit(df$element_level)))
    }
    
    selectInput(
      "stats_element_level", "Poziom:",
      choices  = c("All", lv),
      selected = isolate(if (is.null(input$stats_element_level)) "All" else input$stats_element_level)
    )
  })
  

  observeEvent(input$stats_element_type_select, {
    updateSelectInput(session, "stats_element_symbol", selected = "All")
    updateSelectInput(session, "stats_element_level",  selected = "All")
  }, ignoreInit = TRUE)
  
  observeEvent(input$stats_element_symbol, {
    updateSelectInput(session, "stats_element_level", selected = "All")
  }, ignoreInit = TRUE)
  
  
  fall_elements_data <- reactive({
    elements_one_per_slot() %>%
      dplyr::filter(element_fall == 1) %>%
      dplyr::count(executed_elements_element_raw, name = "fall_count") %>%
      dplyr::mutate(fall_percentage = fall_count / sum(fall_count) * 100) %>%
      dplyr::arrange(dplyr::desc(fall_count))
  })
  
  output$fall_element_plot <- renderPlotly({
    data <- fall_elements_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(ggplotly(
        ggplot() +
          annotate("text", x = .5, y = .5,
                   label = "Brak danych o upadkach dla wybranych filtrów.",
                   size = 5) +
          theme_void()
      ))
    }
    
    plot_data <- data %>%
      dplyr::arrange(dplyr::desc(fall_count)) %>%
      dplyr::slice_head(n = 15)
    
    p <- ggplot(
      plot_data,
      aes(x = reorder(executed_elements_element_raw, fall_count),
          y = fall_count,
          fill = fall_count,
          text = paste0(
            "Element: ", executed_elements_element_raw,
            "<br>Upadki: ", fall_count,
            "<br>Udział: ", round(fall_percentage, 1), "%"
          ))
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Elementy generujące najwięcej upadków",
        x = "Element",
        y = "Liczba upadków"
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#FFD6D6", high = "#D90429")  # jasny -> ciemny czerwony
    
    ggplotly(p, tooltip = "text")
  })
  
  competitor_analysis_data <- reactive({
    data <- cascade_base_df()
    
    # (1) typ
    if (!is.null(input$comp_element_type_select) && input$comp_element_type_select != "All") {
      data <- dplyr::filter(data, element_type == input$comp_element_type_select)
    }
    # (2) element (goły symbol)
    if (!is.null(input$comp_element_symbol) && input$comp_element_symbol != "All") {
      data <- dplyr::filter(data, element == input$comp_element_symbol)
    }
    # (3) level
    if (!is.null(input$comp_element_level) && input$comp_element_level != "All") {
      data <- dplyr::filter(data, toupper(element_level) == toupper(input$comp_element_level))
    }
    # Skoki: zawsze bez combo - pamiętać
    if (!is.null(input$comp_element_type_select) && input$comp_element_type_select == "Jump") {
      data <- dplyr::filter(data, !in_combo)
    }
    
    data
  })
  
  # --- Observe Events ---
  observeEvent(input$filter_skater, {
    if ("All" %in% input$filter_skater && length(input$filter_skater) > 1) {
      updateSelectInput(session, "filter_skater", selected = input$filter_skater[input$filter_skater != "All"])
    }
    if (is.null(input$filter_skater) || length(input$filter_skater) == 0) {
      updateSelectInput(session, "filter_skater", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_category, {
    if ("All" %in% input$filter_category && length(input$filter_category) > 1) {
      updateSelectInput(session, "filter_category", selected = input$filter_category[input$filter_category != "All"])
    }
    if (is.null(input$filter_category) || length(input$filter_category) == 0) {
      updateSelectInput(session, "filter_category", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_age_category, {
    if ("All" %in% input$filter_age_category && length(input$filter_age_category) > 1) {
      updateSelectInput(session, "filter_age_category", selected = input$filter_age_category[input$filter_age_category != "All"])
    }
    if (is.null(input$filter_age_category) || length(input$filter_age_category) == 0) {
      updateSelectInput(session, "filter_age_category", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_season, {
    if ("All" %in% input$filter_season && length(input$filter_season) > 1) {
      updateSelectInput(session, "filter_season", selected = input$filter_season[input$filter_season != "All"])
    }
    if (is.null(input$filter_season) || length(input$filter_season) == 0) {
      updateSelectInput(session, "filter_season", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_competition, {
    if ("All" %in% input$filter_competition && length(input$filter_competition) > 1) {
      updateSelectInput(session, "filter_competition", selected = input$filter_competition[input$filter_competition != "All"])
    }
    if (is.null(input$filter_competition) || length(input$filter_competition) == 0) {
      updateSelectInput(session, "filter_competition", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_segment, {
    if ("All" %in% input$filter_segment && length(input$filter_segment) > 1) {
      updateSelectInput(session, "filter_segment", selected = "All")
    }
    if (is.null(input$filter_segment) || length(input$filter_segment) == 0) {
      updateSelectInput(session, "filter_segment", selected = "All")  # <-- dodaj "filter_segment"
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_nation, {
    if ("All" %in% input$filter_nation && length(input$filter_nation) > 1) {
      updateSelectInput(session, "filter_nation", selected = input$filter_nation[input$filter_nation != "All"])
    }
    if (is.null(input$filter_nation) || length(input$filter_nation) == 0) {
      updateSelectInput(session, "filter_nation", selected = "All")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_club, {
    sel <- input$filter_club
    
    if (is.null(sel) || length(sel) == 0) {
      updateSelectInput(session, "filter_club", selected = "All")
      return()
    }
    
    if ("All" %in% sel && length(sel) > 1) {
      updateSelectInput(session, "filter_club", selected = setdiff(sel, "All"))
      return()
    }
    
  }, ignoreInit = TRUE)
  

  observe({
    skater_choices <- unique(filtered_data()$skater_id)
    updateSelectInput(session, "compare_skater1", choices = skater_choices, selected = input$compare_skater1)
    updateSelectInput(session, "compare_skater2", choices = skater_choices, selected = input$compare_skater2)
  })
  
  
  
  # --- Global Filters ---
  output$filtered_data_preview <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- Dashboard Summary ---
  output$total_skaters_box <- renderValueBox({
    data <- program_data()
    num_skaters <- if (nrow(data) > 0) { data %>% distinct(skater_id) %>% nrow() } else { 0 }
    valueBox(num_skaters, "Ilość zawodników (filtr)", icon = icon("users"), color = "purple")
  })

  output$total_score_dist_plot <- renderPlotly({
    data <- program_data()
    p <- if (nrow(data) > 0) {
      ggplot(data, aes(x = total_score, fill = segment)) +
        geom_histogram(binwidth = 5, position = "dodge", color = "white") +
        labs(title = "Rozkład punktów całkowitych", x = "Całkowity wynik", y = "Liczba zawodów") +
        theme_minimal()
    } else {
      ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów", size=5) + theme_void()
    }
    ggplotly(p)
  })
  output$tes_pcs_scatter_plot <- renderPlotly({
    data <- program_data()
    p <- if (nrow(data) > 0) {
      ggplot(data, aes(x = TES, y = PCS, color = skater_id, text = paste("Zawodnik:", skater_id, "<br>Zawody:", competition, "<br>Wynik:", total_score))) +
        geom_point(alpha = 0.7) +
        labs(title = "Punkty TES vs PCS", x = "Punkty techniczne (TES)", y = "Komponenty (PCS)") +
        theme_minimal()
    } else {
      ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów", size=5) + theme_void()
    }
    ggplotly(p, tooltip = "text")
  })
  output$skater_comparison_plot <- renderPlotly({
    data <- program_data()
    req(input$compare_skater1, input$compare_skater2)
    comp_data <- data %>%
      filter(skater_id %in% c(input$compare_skater1, input$compare_skater2)) %>%
      group_by(skater_id, season) %>%
      summarise(avg_total_score = mean(total_score, na.rm = TRUE), .groups = 'drop') %>%
      arrange(season)
    if (nrow(comp_data) == 0) {
      return(ggplotly(ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów", size=5) + theme_void()))
    }
    p <- ggplot(comp_data, aes(x = season, y = avg_total_score, color = skater_id, group = skater_id, text = paste("Zawodnik:", skater_id, "<br>Sezon:", season, "<br>Średnia punktów:", round(avg_total_score, 2)))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = paste("Porównanie średniej całkowitej liczby punktów:", input$compare_skater1, "vs", input$compare_skater2),
           x = "Sezon", y = "Średnia liczba punktów") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # ZAKŁADKA --ANALIZA KOMPONENTÓW--
  output$component_scores_plot <- renderPlotly({
    data <- program_data()
    all_expected_component_score_cols <- c("Composition_score", "Presentation_score", "Skating_Skills_score", "Transitions_score", "Performance_score", "Interpretation_score")
    component_cols <- names(data)[names(data) %in% all_expected_component_score_cols]
    if (length(component_cols) == 0 || nrow(data) == 0) {
      return(ggplotly(ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów.", size=5) + theme_void()))
    }
    component_data_long <- data %>%
      select(skater_id, all_of(component_cols)) %>%
      pivot_longer(cols = all_of(component_cols), names_to = "component_type", values_to = "score") %>%
      mutate(component_type = gsub("_score", "", component_type), component_type = gsub("_", " ", component_type))
    p <- ggplot(component_data_long, aes(x = component_type, y = score, fill = skater_id)) +
      geom_boxplot() +
      labs(title = "Wynik punktowy komponentów według zawodnika", x = "Komponenty programu", y = "Wynik punktowy") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # ZAKŁADKA --ANALIZA KOMBINACJI--
  
  output$combo_frequency_plot <- renderPlotly({
    data <- combinations_data()
    req(nrow(data) > 0)
    
    plot_data <- data %>%
      count(element_combination) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    p <- ggplot(plot_data, aes(x = reorder(element_combination, n), y = n, fill = element_combination, text = paste("Kombinacja", element_combination, "<br>Ilość:", n))) +
      geom_bar(stat = "identity") +
      labs(title = "Najczęściej wykonywane kombinacje", x = "Kombinacja", y = "Ilość") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$combo_goe_plot <- renderPlotly({
    data <- combinations_data()
    req(nrow(data) > 0)
    
    plot_data <- data %>%
      group_by(element_combination) %>%
      summarise(
        avg_goe = mean(executed_elements_goe, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_goe)) %>%
      slice_head(n = 10)
    
    p <- ggplot(plot_data, aes(x = reorder(element_combination, avg_goe), y = avg_goe, fill = avg_goe, text = paste("Kombinacja:", element_combination, "<br>Średnie GOE:", round(avg_goe, 2)))) +
      geom_bar(stat = "identity") +
      labs(title = "Średnie GOE najlepszych kombinacji", x = "Kombinacja", y = "Średnie GOE") +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$combo_profitability_plot <- renderPlotly({
    data <- combinations_data()
    req(nrow(data) > 0)
    
    plot_data <- data %>%
      group_by(element_combination) %>%
      summarise(
        avg_actual_score = mean(total_actual_score, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_actual_score)) %>%
      slice_head(n = 10)
    
    p <- ggplot(plot_data, aes(x = reorder(element_combination, avg_actual_score), y = avg_actual_score, fill = avg_actual_score,
                               text = paste("Kombinacja:", element_combination, "<br>Średni wynik całkowity:", round(avg_actual_score, 2)))) +
      geom_bar(stat = "identity") +
      labs(title = "Średni wynik całkowity dla najlepszych kombinacji", x = "Kombinacja", y = "Średni wynik całkowity") +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ZAKŁADKA --ROZKŁAD ELEMENTÓW--
  output$goe_by_program_half_plot <- renderPlotly({
    data <- distribution_data()
    req(nrow(data) > 0)
    
    p <- ggplot(data, aes(x = element_order, y = executed_elements_goe, color = połowa_programu, text = paste("Element:", executed_elements_element_raw, "<br>GOE:", executed_elements_goe, "<br>Kolejność:", element_order))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "GOE vs. kolejność wykonania elementu", x = "Kolejność wykonania", y = "GOE") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$goe_by_program_half_table <- renderDT({
    data <- distribution_data()
    req(nrow(data) > 0)
    
    table_data <- data %>%
      group_by(skater_id, połowa_programu) %>%
      summarise(avg_goe = mean(executed_elements_goe, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = połowa_programu, values_from = avg_goe, names_prefix = "Średnie GOE ") %>%
      ungroup()
    
    datatable(table_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- ZAKŁADKA ANALIZA KONKURENCJI ---
  
  output$best_element_ranking_plot <- renderPlotly({
    data <- competitor_analysis_data()
    req(!is.null(input$comp_element_symbol), input$comp_element_symbol != "All", nrow(data) > 0)
    
    # Top10 najlepszy wynik CAŁKOWITY (goe + baza)
    ranking_data <- data %>%
      dplyr::group_by(skater_id) %>%
      dplyr::summarise(best_score = max(executed_elements_base_value + executed_elements_goe, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(best_score)) %>%
      dplyr::slice_head(n = 10)
    
    req(nrow(ranking_data) > 0)
    
    # referencje dla WYBRANEGO elementu po filtrach z zakładki
    ref_avg  <- mean(data$executed_elements_base_value + data$executed_elements_goe, na.rm = TRUE)
    ref_base <- stats::median(data$executed_elements_base_value, na.rm = TRUE)
    xmax     <- max(c(ranking_data$best_score, ref_avg, ref_base), na.rm = TRUE)
    
    sel_lbl <- paste0(
      if (!is.null(input$comp_element_type_select) && input$comp_element_type_select != "All")
        paste0(input$comp_element_type_select, " – ") else "",
      input$comp_element_symbol,
      if (!is.null(input$comp_element_level) && input$comp_element_level != "All")
        paste0(" (", input$comp_element_level, ")") else ""
    )
    
    p <- ggplot(
      ranking_data,
      aes(x = best_score,
          y = reorder(skater_id, best_score),
          fill = best_score,
          text = paste0("Zawodnik: ", skater_id, "<br>Najlepszy wynik: ", round(best_score, 2)))
    ) +
      geom_col(width = 0.72) +
      scale_fill_gradient(low = "#FFE169", high = "#A2E638") +
      geom_vline(xintercept = ref_avg,  linewidth = 1.5, colour = "#1E90FF") +
      geom_vline(xintercept = ref_base, linewidth = 1.5, linetype = "dotted", colour = "#DC143C") +
      guides(fill = guide_colorbar(title = "Najlepszy wynik", barheight = unit(120, "pt"))) +
      labs(title = paste0("Najlepszy wynik dla ", sel_lbl),
           x = "Najlepszy wynik", y = "Zawodnik") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")
    
    # adnotacje nad panelem
    y_ann <- 1.06
    x_rng <- diff(range(ranking_data$best_score, na.rm = TRUE))
    close_lines <- is.finite(ref_avg) && is.finite(ref_base) && (abs(ref_avg - ref_base) < 0.05 * x_rng)
    base_xshift <- if (close_lines) 70 else 6
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis  = list(range = c(0, xmax * 1.05)),
        margin = list(t = 60),
        annotations = list(
          list(x = ref_avg,  y = y_ann, xref = "x", yref = "paper",
               text = paste0("Średni wynik ogólnie: ", round(ref_avg, 2)),
               showarrow = FALSE, xanchor = "left", xshift = 6,
               bgcolor = "rgba(255,255,255,0.95)", bordercolor = "#1E90FF",
               font = list(size = 13, color = "#1E90FF")),
          list(x = ref_base, y = y_ann, xref = "x", yref = "paper",
               text = paste0("Baza za element: ", round(ref_base, 2)),
               showarrow = FALSE, xanchor = "left", xshift = base_xshift,
               bgcolor = "rgba(255,255,255,0.95)", bordercolor = "#DC143C",
               font = list(size = 13, color = "#DC143C"))
        )
      )
  })
  
  output$club_vs_other_goe_plot <- renderPlotly({
    data <- competitor_analysis_data()
    req(nrow(data) > 0)
    
    # grupowanie: „wybrane vs reszta” lub tylko wybrane
    if (!is.null(input$comp_club_select) && length(input$comp_club_select) > 0) {
      if (identical(input$club_compare_mode, "vs_rest")) {
        data <- data %>%
          dplyr::mutate(group = ifelse(nation %in% input$comp_club_select, as.character(nation), "Inne"))
      } else {
        data <- data %>%
          dplyr::filter(nation %in% input$comp_club_select) %>%
          dplyr::mutate(group = as.character(nation))
      }
    } else {
      data <- data %>% dplyr::mutate(group = as.character(nation))
    }
    
    plot_data <- data %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        avg_goe = mean(executed_elements_goe, na.rm = TRUE),
        n = dplyr::n(), .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(n))
    
    if (is.null(input$comp_club_select) || length(input$comp_club_select) == 0) {
      plot_data <- dplyr::slice_head(plot_data, n = 12)
    }
    
    rng <- range(plot_data$avg_goe, na.rm = TRUE)
    m <- max(abs(rng[1]), abs(rng[2]))
    if (!is.finite(m) || m == 0) m <- 1
    
    p <- ggplot(plot_data,
                aes(x = reorder(group, avg_goe),
                    y = avg_goe,
                    fill = avg_goe,
                    text = paste(
                      "Region:", group,
                      "<br>Średnia GOE:", round(avg_goe, 2),
                      "<br>liczba obserwacji:", n
                    ))) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 0, color = "grey60") +
      labs(title = "Średnie GOE dla regionów",
           x = "Region", y = "Średnie GOE") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient2(
        low = "#DC143C",
        mid = "#D7E5DC",
        high = "#A2E638",
        midpoint = 0,
        limits = c(-m, m),
        oob = scales::squish
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # ZAKŁADKA --ANALIZA ODJĘĆ--
  output$deduction_type_plot <- renderPlotly({
    pd <- program_data()
    if (nrow(pd) == 0) {
      return(ggplotly(
        ggplot() +
          annotate("text", x = .5, y = .5,
                   label = "Brak danych dla wybranych filtrów.", size = 5) +
          theme_void()
      ))
    }
    
    # 1) Pozostałe odjęcia (czas, kostium itd.)
    nonfall_cols <- c(
      "time_violation_deduction",
      "costume_prop_violation_deduction",
      "interruption_deduction",
      "extra_element_deduction",
      "late_start_deduction"
    )
    existing_nonfall <- nonfall_cols[nonfall_cols %in% names(pd)]
    
    nonfall_df <- if (length(existing_nonfall) > 0) {
      pd %>%
        dplyr::summarise(across(all_of(existing_nonfall),
                                ~ sum(abs(.), na.rm = TRUE))) %>%
        tidyr::pivot_longer(
          dplyr::everything(),
          names_to  = "deduction_type",   # <--- KLUCZOWA ZMIANA
          values_to = "points"            # <--- KLUCZOWA ZMIANA
        )
    } else {
      tibble::tibble(deduction_type = character(), points = numeric())
    }
    
    # 2) Upadki – liczone osobno z poziomu elementów
    es <- elements_one_per_slot()
    total_falls <- es %>%
      dplyr::summarise(total_falls = sum(as.integer(element_fall), na.rm = TRUE)) %>%
      dplyr::pull(total_falls)
    
    # kara za 1 upadek (wyciągnięta z danych programowych)
    per_fall <- pd %>%
      dplyr::filter(!is.na(falls_deduction), !is.na(falls_count), falls_count > 0) %>%
      dplyr::mutate(per_fall = abs(falls_deduction) / falls_count) %>%
      dplyr::summarise(
        per_fall = ifelse(all(is.na(per_fall)),
                          1,
                          stats::median(per_fall, na.rm = TRUE))
      ) %>%
      dplyr::pull(per_fall)
    if (length(per_fall) == 0 || is.na(per_fall)) per_fall <- 1
    
    falls_df <- tibble::tibble(
      deduction_type = "falls",
      points = total_falls * per_fall
    )
    
    # 3) Połączenie wszystkiego i przygotowanie do wykresu
    plot_data <- dplyr::bind_rows(nonfall_df, falls_df) %>%
      dplyr::mutate(
        deduction_type = gsub("_deduction", "", deduction_type),
        deduction_type = gsub("_", " ", deduction_type)
      ) %>%
      dplyr::arrange(dplyr::desc(points)) %>%
      dplyr::filter(points > 0)
    
    if (nrow(plot_data) == 0 || all(plot_data$points == 0 | is.na(plot_data$points))) {
      return(ggplotly(
        ggplot() +
          annotate("text", x = .5, y = .5,
                   label = "Brak danych do pokazania", size = 5) +
          theme_void()
      ))
    }
    
    p <- ggplot(plot_data,
                aes(x = reorder(deduction_type, points),
                    y = points,
                    fill = deduction_type,
                    text = paste0(
                      "Typ: ", deduction_type,
                      "<br>Całkowita ilość punktów: ", round(points, 2)
                    ))) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Całkowite odjęcia według typu odjęcia",
        x = "Typ odjęcia",
        y = "Całościowa liczba odjęć punktowych"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  # ZAKŁADKA --ANALIZA ELEMENTÓW--
  output$element_usage_plot <- renderPlotly({
    data <- filtered_elements_data()
    req(nrow(data) > 0)
    
    plot_data <- data %>%
      count(competition, name = "count") %>%
      arrange(desc(count))
    
    if(nrow(plot_data) == 0) {
      return(ggplotly(ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów.", size=5) + theme_void()))
    }
    
    p <- ggplot(plot_data, aes(x = reorder(competition, count), y = count, fill = competition, text = paste("Zawody:", competition, "<br>Ilość:", count))) +
      geom_bar(stat = "identity") +
      labs(title = "Częstość wykonania danego elementu", x = "Zawody", y = "Ilość") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$element_effectiveness_plot <- renderPlotly({
    data <- filtered_elements_data()
    req(nrow(data) > 0)
    
    plot_data <- data %>%
      dplyr::summarise(
        avg_goe = mean(executed_elements_goe, na.rm = TRUE),
        goe_sd  = sd(executed_elements_goe,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "metric", values_to = "value") %>%
      dplyr::mutate(metric = factor(metric, levels = c("avg_goe","goe_sd")))
    
    p <- ggplot(plot_data,
                aes(x = metric, y = value, fill = metric, text = paste0(ifelse(metric=="avg_goe","Średnie GOE","GOE SD"), ": ", round(value, 3)))) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_x_discrete(labels = c("Średnie GOE", "GOE SD\n(stabilność: niżej = pewniej)")) +
      labs(title = "Skuteczność wykonanego elementu", x = NULL, y = "Wartość") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(size = 11, margin = margin(t = 11)),
        plot.title  = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis  = list(automargin = TRUE),
        margin = list(b = 95, l = 60, r = 20, t = 50)
      )
  })
  
  
  output$goe_distribution_plot <- renderPlotly({
    data <- filtered_elements_data()
    req(nrow(data) > 0)
    
    plot_data <- data %>%
      count(goe_category) %>%
      mutate(percentage = n / sum(n) * 100)
    
    if(nrow(plot_data) == 0) {
      return(ggplotly(ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów.", size=5) + theme_void()))
    }
    
    p <- plot_ly(plot_data, labels = ~goe_category, values = ~percentage, type = 'pie', textinfo = 'percent', hoverinfo = 'text', text = ~paste(goe_category, ":", round(percentage, 2), "%")) %>%
      layout(title = "Rozkład GOE", showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p
  })
  
  output$element_profitability_plot <- renderPlotly({
    data <- filtered_elements_data()
    
    plot_data <- data %>%
      group_by(executed_elements_element_raw) %>%
      summarise(
        avg_base_value = mean(executed_elements_base_value, na.rm = TRUE),
        avg_actual_score = mean(executed_elements_base_value + executed_elements_goe, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      ungroup() %>%
      mutate(profitability = avg_actual_score - avg_base_value) %>%
      arrange(desc(profitability))
    
    if(nrow(plot_data) == 0) {
      return(ggplotly(ggplot() + annotate("text", x=0.5, y=0.5, label="Brak danych dla wybranych filtrów.", size=5) + theme_void()))
    }
    
    p <- ggplot(plot_data, aes(x = reorder(executed_elements_element_raw, profitability), y = profitability, fill = profitability > 0, text = paste("Element:", executed_elements_element_raw,
                                            "<br>Średnia wartość bazowa:", round(avg_base_value, 2),
                                            "<br>Średnia ogólna wartość:", round(avg_actual_score, 2),
                                            "<br>Opłacalność:", round(profitability, 2)))) +
      geom_bar(stat = "identity") +
      labs(title = "Opłacalność wykonawcza elementu",
           x = "Element", y = "Średni zysk/strata punktów") +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none")
    
    ggplotly(p, tooltip = "text")
  })

  
  # Zakładka --PROGNOZY--
  
  output$fc_plot <- renderPlotly({
    fc_ser <- fc_data();
    fc     <- fc_result();
    req(fc_ser$series, fc)
    
    d <- fc_ser$series
    f_years <- seq(max(d$season) + 1, by = 1, length.out = length(fc$mean))
    
    df_hist <- data.frame(season = d$season, value = d$y, type = "History")
    df_fc   <- data.frame(
      season = f_years,
      value  = as.numeric(fc$mean),
      lo80   = as.numeric(fc$lower[, "80%"]),
      hi80   = as.numeric(fc$upper[, "80%"]),
      lo95   = as.numeric(fc$lower[, "95%"]),
      hi95   = as.numeric(fc$upper[, "95%"]),
      type   = "Forecast"
    )
    
    p <- ggplot() +
      geom_line(data = df_hist, aes(season, value), linewidth = 1) +
      geom_point(data = df_hist, aes(season, value)) +
      geom_ribbon(data = df_fc, aes(season, ymin = lo95, ymax = hi95), alpha = .15) +
      geom_ribbon(data = df_fc, aes(season, ymin = lo80, ymax = hi80), alpha = .25) +
      geom_line(data = df_fc, aes(season, value), linewidth = 1, linetype = "dashed") +
      labs(
        title = paste("Prognoza –", input$fc_skater, "(", fc_ser$label, ")"),
        x = "Sezon",
        y = if (grepl("^SP\\+FS", fc_ser$label)) "Łączny wynik (SP+FS)" else "Średni wynik"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "y+x") %>% toWebGL()
  })
  
  
  #tabela
  output$fc_table <- renderDT({
    fc_ser <- fc_data(); fc <- fc_result()
    req(fc_ser$series, fc)
    
    years <- seq(max(fc_ser$series$season) + 1, by = 1, length.out = length(fc$mean))
    out <- data.frame(
      Sezon   = years,
      Prognoza = round(as.numeric(fc$mean), 2),
      PI80_L   = round(as.numeric(fc$lower[, "80%"]), 2),
      PI80_U   = round(as.numeric(fc$upper[, "80%"]), 2),
      PI95_L   = round(as.numeric(fc$lower[, "95%"]), 2),
      PI95_U   = round(as.numeric(fc$upper[, "95%"]), 2)
    )
    datatable(out, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
  })
  
}

# Standardowe uruchamianie
shinyApp(ui, server)