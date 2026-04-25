# ============================================================
# ACTIVIDAD 3 — APLICACIÓN SHINY
# Modelación del Rendimiento Académico en Educación Secundaria
# Portuguesa mediante Regresión Lineal Simple
#
# Autor : Oriana Giraldo Arcia
# Curso : Métodos y Simulación Estadística — Maestría en Ciencia de Datos
# Fuente: Cortez & Silva (2008) — UCI Repository
# ============================================================
#
# INSTALACIÓN DE PAQUETES (primera vez):
#
# install.packages(c("shiny", "bslib", "bsicons", "ggplot2", "DT",
#                    "dplyr", "corrplot", "ggcorrplot",
#                    "lmtest", "nortest", "scales",
#                    "shinyWidgets", "gridExtra"))
#
# ============================================================


# ------------------------------------------------------------
# 1. LIBRERÍAS
# ------------------------------------------------------------
library(shiny)
library(bslib)          # Bootstrap 5 theming + cards + value_box
library(bsicons)        # Bootstrap icons
library(ggplot2)
library(DT)
library(dplyr)
library(corrplot)
library(lmtest)         # bptest, dwtest
library(nortest)        # lillie.test
library(shinyWidgets)   # awesomeCheckbox, pickerInput, sliderInput personalizado
library(gridExtra)      # grid.arrange (bug corregido: ahora cargado)
library(scales)


# ------------------------------------------------------------
# 2. CARGA Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------
# Los CSV usan ";" como separador (Cortez & Silva, 2008)

.leer_csv <- function(path) {
  read.table(path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
}

mat_raw <- .leer_csv("data/studentmat.csv")
por_raw <- .leer_csv("data/studentpor.csv")

# Forzar conversión numérica de G1 y G2 (pueden leerse como chr)
for (col in c("G1", "G2")) {
  mat_raw[[col]] <- as.numeric(as.character(mat_raw[[col]]))
  por_raw[[col]] <- as.numeric(as.character(por_raw[[col]]))
}

# Excluir G3 = 0 (abandono tardío — ver Sección 1.3 del informe)
mat_base <- mat_raw[mat_raw$G3 > 0, ]
por_base <- por_raw[por_raw$G3 > 0, ]

# Datasets disponibles
lista_datos <- list(
  "Matemáticas (MAT)" = mat_base,
  "Portugués (POR)"   = por_base
)

# Variables cuantitativas candidatas
vars_cuant <- c("age", "Medu", "Fedu", "traveltime", "studytime",
                "failures", "famrel", "freetime", "goout",
                "Dalc", "Walc", "health", "absences", "G1", "G2", "G3")

predictores_disponibles <- c(
  "G2       (r ≈ 0.90 con G3)"  = "G2",
  "failures (r ≈ -0.36 con G3)" = "failures"
)


# ------------------------------------------------------------
# 3. PALETA Y FUNCIONES AUXILIARES
# ------------------------------------------------------------
PAL <- list(
  primary   = "#2C3E50",   # azul oscuro  (header)
  accent    = "#2980B9",   # azul medio   (resaltes)
  success   = "#27AE60",   # verde        (POR / rechazos H0)
  warning   = "#F39C12",   # naranja      (atípicos)
  danger    = "#E74C3C",   # rojo         (influyentes)
  neutral   = "#7F8C8D",   # gris         (secundario)
  bg_light  = "#F8F9FA",   # gris muy claro
  text_dark = "#1A252F"
)

# Formatear p-valor con asteriscos
fmt_p <- function(p) {
  if (is.na(p)) return("—")
  sig <- dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
  paste0(formatC(p, format = "f", digits = 4), " ", sig)
}

# Umbrales de influencia
umbral_cook <- function(n) 4 / n
umbral_lev  <- function(n) 4 / n              # = 2(p+1)/n con p=1
umbral_dff  <- function(n) 2 * sqrt(2 / n)    # = 2√((p+1)/n)

# Tema ggplot consistente con la paleta
tema_app <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5,
                                       color = PAL$text_dark),
      plot.subtitle    = element_text(hjust = 0.5, color = PAL$neutral, size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ECEFF1", linewidth = 0.3),
      axis.title       = element_text(color = PAL$text_dark),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}


# ------------------------------------------------------------
# 4. TEMA VISUAL (bslib)
# ------------------------------------------------------------
tema_bs <- bs_theme(
  version        = 5,
  bootswatch     = "flatly",
  primary        = PAL$primary,
  secondary      = PAL$neutral,
  success        = PAL$success,
  info           = PAL$accent,
  warning        = PAL$warning,
  danger         = PAL$danger,
  base_font      = font_google("Inter"),
  heading_font   = font_google("Inter"),
  code_font      = font_google("JetBrains Mono"),
  "card-border-radius"    = "12px",
  "card-cap-bg"           = paste0(PAL$primary, "10"),
  "navbar-padding-y"      = "0.75rem"
)


# CSS custom para detalles finos
css_custom <- "
  .navbar-brand { font-weight: 600; letter-spacing: -0.01em; }
  .bslib-value-box { transition: transform 0.15s ease; }
  .bslib-value-box:hover { transform: translateY(-2px); }
  .card-header { font-weight: 600; font-size: 0.95rem; }
  
  /* --- CORRECCIÓN: Estilos para la barra de navegación (navbar) --- */
  .navbar-nav .nav-link.active, 
  .navbar-nav .nav-link.active i { /* Aplica al texto y al ícono */
    color: #FFFFFF !important;
    font-weight: 600;
  }
  .navbar-nav .nav-link:hover,
  .navbar-nav .nav-link:hover i {
    color: #E8F4F8 !important; 
  }
  /* ---------------------------------------------------------------- */

  .nav-tabs .nav-link { font-weight: 500; }
  .nav-tabs .nav-link.active { border-bottom: 3px solid #2980B9; }
  .tooltip-inner { max-width: 280px; text-align: left; font-size: 0.85rem; }
  .info-badge {
    display: inline-block; padding: 2px 8px;
    background: #E8F4F8; color: #2980B9;
    border-radius: 10px; font-size: 0.75rem;
    font-weight: 500; margin-left: 4px;
  }
  .callout {
    border-left: 4px solid #2980B9;
    background: #F0F7FC;
    padding: 12px 16px; border-radius: 6px;
    margin: 8px 0;
  }
  .callout-warn {
    border-left-color: #F39C12;
    background: #FEF9F0;
  }
  .callout-success {
    border-left-color: #27AE60;
    background: #F0F9F4;
  }
  .footer-app {
    text-align: center; color: #7F8C8D;
    font-size: 0.85rem; padding: 20px;
    border-top: 1px solid #ECEFF1; margin-top: 30px;
  }
  code { color: #2C3E50; background: #F4F6F7; padding: 2px 6px; border-radius: 4px; }
"


# ------------------------------------------------------------
# 5. HELPER — tooltip de ayuda estadística
# ------------------------------------------------------------
info_tip <- function(txt) {
  tags$span(
    bsicons::bs_icon("info-circle"),
    class = "text-info",
    style = "cursor: help; margin-left: 4px;",
    title = txt,
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = "right"
  )
}


# ------------------------------------------------------------
# 6. INTERFAZ DE USUARIO
# ------------------------------------------------------------
ui <- page_navbar(
  title = div(
    bsicons::bs_icon("bar-chart-line-fill"),
    span("Regresión Lineal Simple", style = "font-weight: 600;"),
    span("—", style = "margin: 0 6px; color: #95A5A6;"),
    span("Rendimiento Académico", style = "color: #BDC3C7; font-weight: 400;")
  ),
  theme = tema_bs,
  navbar_options = navbar_options(bg = PAL$primary, theme = "dark"),
  fillable = FALSE,
  header = tags$head(
    tags$style(HTML(css_custom)),
    tags$script(HTML("
      // Inicializar tooltips de Bootstrap
      document.addEventListener('DOMContentLoaded', function() {
        var ttList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        ttList.map(function (t) { return new bootstrap.Tooltip(t); });
      });
      // Re-inicializar cuando cambia de pestaña
      document.addEventListener('shown.bs.tab', function () {
        var ttList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        ttList.map(function (t) { return new bootstrap.Tooltip(t); });
      });
    "))
  ),

  # ── Sidebar global ────────────────────────────────────────────────
  sidebar = sidebar(
    width = 320,
    bg = "#FFFFFF",
    open = "always",

    h6(bsicons::bs_icon("sliders"),
       "Configuración del análisis",
       style = paste0("color:", PAL$primary, "; font-weight:600; margin-top: 4px;")),

    shinyWidgets::pickerInput(
      inputId  = "dataset",
      label    = tagList("Asignatura (G3):",
                         info_tip("Selecciona el conjunto de datos. MAT = Matemáticas (n=357), POR = Portugués (n=634). Ambos tras excluir G3 = 0.")),
      choices  = names(lista_datos),
      selected = "Matemáticas (MAT)",
      options  = list(style = "btn-outline-primary btn-sm")
    ),

    shinyWidgets::pickerInput(
      inputId  = "predictor",
      label    = tagList("Predictor (X):",
                         info_tip("G2 = calificación del segundo período (r≈0.90 con G3). failures = número de reprobaciones previas (r≈-0.36).")),
      choices  = predictores_disponibles,
      selected = "G2",
      options  = list(style = "btn-outline-primary btn-sm")
    ),

    hr(style = "margin: 12px 0;"),

    sliderInput(
      inputId = "nivel_conf",
      label   = tagList("Nivel de confianza:",
                        info_tip("1 - α. Afecta al ancho de los intervalos de confianza y predicción. Mayor nivel → bandas más amplias.")),
      min = 0.80, max = 0.99, value = 0.95, step = 0.01,
      ticks = FALSE
    ),

    hr(style = "margin: 12px 0;"),

    tags$label(
      tagList(bsicons::bs_icon("x-circle"),
              " Excluir observaciones:",
              info_tip("Selecciona índices para simular el efecto de eliminar ciertas observaciones sobre el ajuste en tiempo real.")),
      style = "font-weight: 600; font-size: 0.9rem;"
    ),
    uiOutput("ui_excluir"),

    hr(style = "margin: 12px 0;"),

    # Panel de modelo actual (en card mini)
    card(
      class = "bg-light",
      card_header(
        bsicons::bs_icon("clipboard-data"),
        " Modelo actual",
        style = paste0("background:", PAL$primary, "; color:white; font-size:0.85rem; padding: 6px 12px;")
      ),
      card_body(
        padding = "10px",
        verbatimTextOutput("modelo_texto")
      )
    ),

    br(),
    div(
      style = paste0("font-size: 0.75rem; color:", PAL$neutral, "; text-align: center;"),
      bsicons::bs_icon("mortarboard"),
      " Cortez & Silva (2008)", br(),
      tags$small("Significancia: *** p<0.001, ** p<0.01, * p<0.05")
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 0 — INICIO (contexto y guía)
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("house-door-fill"), " Inicio"),

    # Hero / KPIs generales
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box(
        title    = "Dataset activo",
        value    = textOutput("kpi_dataset", inline = TRUE),
        showcase = bsicons::bs_icon("book-half"),
        theme    = "primary",
        p(textOutput("kpi_subtitle", inline = TRUE))
      ),
      value_box(
        title    = "Observaciones (n)",
        value    = textOutput("kpi_n", inline = TRUE),
        showcase = bsicons::bs_icon("people-fill"),
        theme    = "info",
        p("Tras excluir G3 = 0 y filtrados")
      ),
      value_box(
        title    = "Coeficiente R²",
        value    = textOutput("kpi_r2", inline = TRUE),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme    = "success",
        p(textOutput("kpi_r2_pct", inline = TRUE))
      ),
      value_box(
        title    = "Error residual σ̂",
        value    = textOutput("kpi_sigma", inline = TRUE),
        showcase = bsicons::bs_icon("rulers"),
        theme    = "warning",
        p("Puntos de calificación (0–20)")
      )
    ),

    br(),

    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(bsicons::bs_icon("info-square"), " Contexto del estudio"),
        card_body(
          tags$p(
            "Esta aplicación complementa el informe formal de la Actividad 3, permitiendo la ",
            tags$b("exploración interactiva"),
            " de los modelos de regresión lineal simple ajustados al conjunto de datos de rendimiento académico en educación secundaria portuguesa (Cortez & Silva, 2008)."
          ),
          tags$p(
            "Se modelan las calificaciones finales G3 como función de dos predictores seleccionables:"
          ),
          tags$ul(
            tags$li(tags$b("G2"), ": calificación del segundo período (predictor principal con r ≈ 0.90)."),
            tags$li(tags$b("failures"), ": número de reprobaciones previas (r ≈ -0.36).")
          ),
          div(
            class = "callout",
            tags$b("Cómo usar esta app"), br(),
            "1. Selecciona asignatura y predictor en el panel lateral.", br(),
            "2. Ajusta el nivel de confianza para ver IC/IP.", br(),
            "3. Usa ", tags$code("Excluir observaciones"), " para análisis de sensibilidad.", br(),
            "4. Navega por las pestañas para análisis detallado."
          )
        )
      ),
      card(
        card_header(bsicons::bs_icon("list-check"), " Estructura de análisis"),
        card_body(
          tags$ol(
            tags$li(bsicons::bs_icon("table"), tags$b(" Datos"),
                    " — Vista del conjunto activo"),
            tags$li(bsicons::bs_icon("diagram-3"), tags$b(" Correlaciones"),
                    " — Asociaciones lineales entre variables"),
            tags$li(bsicons::bs_icon("graph-up"), tags$b(" Modelo"),
                    " — Estimación, ANOVA, IC de parámetros"),
            tags$li(bsicons::bs_icon("search"), tags$b(" Diagnóstico"),
                    " — Verificación de supuestos (S1–S4)"),
            tags$li(bsicons::bs_icon("exclamation-diamond"), tags$b(" Atípicos e influyentes"),
                    " — Cook, leverage, DFFITS"),
            tags$li(bsicons::bs_icon("bullseye"), tags$b(" Predicción"),
                    " — IC para media e IP individual")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 1 — DATOS
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("table"), " Datos"),
    card(
      card_header(
        bsicons::bs_icon("table"),
        " Vista del conjunto de datos (observaciones activas)",
        info_tip("Las observaciones excluidas en el sidebar se remueven de esta vista y de todos los análisis.")
      ),
      card_body(
        DTOutput("tabla_datos")
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 2 — CORRELACIONES
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("diagram-3-fill"), " Correlaciones"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        full_screen = TRUE,
        card_header(
          bsicons::bs_icon("list-ol"),
          " Correlaciones con G3",
          info_tip("Pearson mide asociación lineal; Spearman es robusta a valores atípicos y monotónica (no exige linealidad).")
        ),
        card_body(
          DTOutput("tabla_correlaciones")
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          bsicons::bs_icon("grid-3x3-gap"),
          " Mapa de calor — variables cuantitativas"
        ),
        card_body(
          plotOutput("plot_corrplot", height = "480px")
        )
      )
    ),
    br(),
    card(
      full_screen = TRUE,
      card_header(
        bsicons::bs_icon("bounding-box-circles"),
        " Diagrama de dispersión: G3 ~ predictor seleccionado"
      ),
      card_body(
        plotOutput("plot_scatter", height = "400px")
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 3 — MODELO
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("graph-up"), " Modelo"),

    # KPIs del modelo actual
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box(
        title    = "β̂₁ (pendiente)",
        value    = textOutput("kpi_b1", inline = TRUE),
        showcase = bsicons::bs_icon("slash-lg"),
        theme    = "info"
      ),
      value_box(
        title    = "p-valor (H₀: β₁=0)",
        value    = textOutput("kpi_pval", inline = TRUE),
        showcase = bsicons::bs_icon("asterisk"),
        theme    = "success"
      ),
      value_box(
        title    = "Estadístico F",
        value    = textOutput("kpi_fstat", inline = TRUE),
        showcase = bsicons::bs_icon("bar-chart-fill"),
        theme    = "primary"
      ),
      value_box(
        title    = "R² ajustado",
        value    = textOutput("kpi_r2adj", inline = TRUE),
        showcase = bsicons::bs_icon("bullseye"),
        theme    = "warning"
      )
    ),

    br(),

    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,

      # Columna izquierda
      div(
        card(
          card_header(bsicons::bs_icon("calculator"), " Ecuación ajustada"),
          card_body(verbatimTextOutput("ecuacion_modelo"))
        ),
        br(),
        card(
          card_header(
            bsicons::bs_icon("list-columns"),
            " Coeficientes — prueba t",
            info_tip("H₀: βⱼ = 0 vs H₁: βⱼ ≠ 0. Si p < α, hay evidencia de que el parámetro es distinto de cero.")
          ),
          card_body(DTOutput("tabla_coeficientes"))
        ),
        br(),
        card(
          card_header(
            bsicons::bs_icon("table"),
            " Tabla ANOVA — prueba F",
            info_tip("Descompone la variabilidad total (SST) en explicada por el modelo (SSR) y residual (SSE). F = MSR/MSE.")
          ),
          card_body(DTOutput("tabla_anova"))
        )
      ),

      # Columna derecha
      div(
        card(
          card_header(bsicons::bs_icon("trophy"), " Bondad de ajuste"),
          card_body(DTOutput("tabla_r2"))
        ),
        br(),
        card(
          card_header(
            bsicons::bs_icon("arrows-collapse-vertical"),
            " Intervalos de confianza para parámetros",
            info_tip("IC = estimación ± t(α/2, n-2) · SE. Si el IC de β₁ no contiene 0, β₁ es significativo al nivel α.")
          ),
          card_body(DTOutput("tabla_ic_params"))
        ),
        br(),
        card(
          full_screen = FALSE,
          card_header(bsicons::bs_icon("activity"), " Recta de regresión ajustada"),
          card_body(plotOutput("plot_regresion", height = "330px"))
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 4 — DIAGNÓSTICO
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("search"), " Diagnóstico"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = FALSE,
        card_header(
          bsicons::bs_icon("bar-chart-line"),
          " Gráficos de diagnóstico (4 paneles)",
          info_tip("S1: linealidad y media cero. S2: normalidad de residuales. S3: homocedasticidad. S4: independencia (vs índice).")
        ),
        card_body(plotOutput("plot_diagnostico", height = "520px"))
      ),
      div(
        card(
          card_header(bsicons::bs_icon("check2-circle"), " Pruebas formales de supuestos"),
          card_body(DTOutput("tabla_supuestos"))
        ),
        br(),
        div(
          class = "callout callout-warn",
          tags$b(bsicons::bs_icon("exclamation-triangle"), " Nota metodológica"),
          tags$p(
            "Para n > 300, Shapiro-Wilk y Lilliefors tienen ",
            tags$b("alta potencia"),
            " y pueden rechazar H₀ ante desviaciones triviales. El TCL garantiza validez asintótica de la inferencia sobre β₁ (Casella & Berger, 2002, Teo. 10.1.6)."
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 5 — ATÍPICOS E INFLUYENTES
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("exclamation-diamond-fill"), " Influyentes"),

    card(
      card_header(bsicons::bs_icon("rulers"), " Umbrales de detección"),
      card_body(uiOutput("ui_umbrales"))
    ),

    br(),

    layout_columns(
      col_widths = c(7, 5),
      div(
        card(
          full_screen = TRUE,
          card_header(
            bsicons::bs_icon("bar-chart-line"),
            " Distancia de Cook",
            info_tip("Dᵢ mide el cambio en todos los valores ajustados si se elimina la obs i. Umbral: 4/n.")
          ),
          card_body(plotOutput("plot_cook", height = "300px"))
        ),
        br(),
        card(
          full_screen = TRUE,
          card_header(
            bsicons::bs_icon("circle"),
            " Leverage vs Residual estandarizado",
            info_tip("El tamaño del círculo = Dᵢ. Alto leverage + alto |rᵢ| = observación influyente.")
          ),
          card_body(plotOutput("plot_bubble", height = "350px"))
        )
      ),
      div(
        card(
          full_screen = FALSE,
          card_header(bsicons::bs_icon("list-check"), " Observaciones influyentes"),
          card_body(DTOutput("tabla_influyentes"))
        ),
        br(),
        card(
          card_header(bsicons::bs_icon("clipboard-check"), " Resumen por criterio"),
          card_body(DTOutput("tabla_flags"))
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════
  # PESTAÑA 6 — PREDICCIÓN
  # ══════════════════════════════════════════════════════════════════
  nav_panel(
    title = tagList(bsicons::bs_icon("bullseye"), " Predicción"),
    layout_columns(
      col_widths = c(4, 8),

      # Columna izquierda: input + tabla puntual
      div(
        card(
          card_header(bsicons::bs_icon("bullseye"), " Predicción puntual"),
          card_body(
            numericInput(
              inputId = "x_nuevo",
              label   = tagList("Valor del predictor (x₀):",
                                info_tip("Introduce un valor de X para obtener la predicción puntual, el IC para la media condicional y el IP para un estudiante específico.")),
              value   = 12, min = 0, max = 20, step = 1
            ),
            br(),
            DTOutput("tabla_prediccion_puntual"),
            br(),
            div(
              class = "callout callout-success",
              tags$b(bsicons::bs_icon("chat-quote"), " Interpretación"),
              uiOutput("texto_prediccion")
            ),
            br(),
            div(
              class = "callout",
              tags$b("Distinción IC vs IP:"), br(),
              tags$small(
                tags$b("IC"), " responde: ¿cuál es la calificación ",
                tags$b("promedio"), " de todos los estudiantes con X = x₀?", br(),
                tags$b("IP"), " responde: ¿cuál será la calificación de ",
                tags$b("este estudiante específico"), " con X = x₀?"
              )
            )
          )
        )
      ),

      # Columna derecha: gráfico + tabla de rango
      div(
        card(
          full_screen = TRUE,
          card_header(bsicons::bs_icon("graph-up-arrow"),
                      " Recta ajustada con bandas IC y IP"),
          card_body(plotOutput("plot_prediccion", height = "420px"))
        ),
        br(),
        card(
          full_screen = TRUE,
          card_header(bsicons::bs_icon("grid-3x3"),
                      " Predicciones a lo largo del rango del predictor"),
          card_body(DTOutput("tabla_pred_rango"))
        )
      )
    )
  ),

  # Footer
  footer = div(
    class = "footer-app",
    bsicons::bs_icon("github"),
    " Oriana Giraldo Arcia · Maestría en Ciencia de Datos · Métodos y Simulación Estadística",
    br(),
    tags$small(
      "Fuente: Cortez, P. & Silva, A. (2008). Using Data Mining to Predict Secondary School Student Performance. UCI Repository."
    )
  )
)


# ============================================================
# 7. SERVIDOR
# ============================================================
server <- function(input, output, session) {

  # ── 7.1 DATOS REACTIVOS ──────────────────────────────────────────
  datos_base <- reactive({
    lista_datos[[input$dataset]]
  })

  # UI de exclusión: se regenera al cambiar el dataset
  output$ui_excluir <- renderUI({
    n <- nrow(datos_base())
    shinyWidgets::pickerInput(
      inputId  = "excluir",
      label    = NULL,
      choices  = seq_len(n),
      selected = NULL,
      multiple = TRUE,
      options  = list(
        `actions-box`          = TRUE,
        `live-search`          = TRUE,
        `selected-text-format` = "count > 3",
        size                   = 8,
        style                  = "btn-outline-danger btn-sm",
        `none-selected-text`   = "Ninguna excluida"
      )
    )
  })

  # Datos filtrados (sin las observaciones excluidas)
  datos <- reactive({
    df   <- datos_base()
    excl <- as.integer(input$excluir)
    if (length(excl) > 0) df <- df[-excl, , drop = FALSE]
    df
  })

  # ── 7.2 MODELO REACTIVO ──────────────────────────────────────────
  modelo <- reactive({
    req(input$predictor)
    df  <- datos()
    frm <- as.formula(paste("G3 ~", input$predictor))
    lm(frm, data = df)
  })

  # ── 7.3 KPIs DE INICIO ───────────────────────────────────────────
  output$kpi_dataset <- renderText({ input$dataset })
  output$kpi_subtitle <- renderText({
    paste("Predictor:", input$predictor)
  })
  output$kpi_n <- renderText({
    formatC(nrow(datos()), format = "d", big.mark = ",")
  })
  output$kpi_r2 <- renderText({
    formatC(summary(modelo())$r.squared, format = "f", digits = 4)
  })
  output$kpi_r2_pct <- renderText({
    paste0(round(summary(modelo())$r.squared * 100, 2),
           "% de la variabilidad explicada")
  })
  output$kpi_sigma <- renderText({
    formatC(summary(modelo())$sigma, format = "f", digits = 3)
  })

  # KPIs del modelo
  output$kpi_b1 <- renderText({
    formatC(coef(modelo())[2], format = "f", digits = 4)
  })
  output$kpi_pval <- renderText({
    p <- coef(summary(modelo()))[2, "Pr(>|t|)"]
    if (p < 0.001) "< 0.001" else formatC(p, format = "f", digits = 4)
  })
  output$kpi_fstat <- renderText({
    formatC(anova(modelo())[1, "F value"], format = "f", digits = 2)
  })
  output$kpi_r2adj <- renderText({
    formatC(summary(modelo())$adj.r.squared, format = "f", digits = 4)
  })

  # ── 7.4 TEXTO RESUMEN DEL MODELO (SIDEBAR) ───────────────────────
  output$modelo_texto <- renderPrint({
    m   <- modelo()
    b0  <- round(coef(m)[1], 3)
    b1  <- round(coef(m)[2], 3)
    r2  <- round(summary(m)$r.squared, 4)
    sig <- round(summary(m)$sigma, 3)
    cat(paste0(
      "Ĝ3 = ", b0,
      ifelse(b1 >= 0, " + ", " - "),
      abs(b1), " × ", input$predictor, "\n",
      "R² = ", r2, "    σ̂ = ", sig, "\n",
      "n  = ", nrow(datos())
    ))
  })

  # ── 7.5 PESTAÑA DATOS ────────────────────────────────────────────
  output$tabla_datos <- renderDT({
    datatable(
      datos(),
      options = list(
        pageLength = 10, scrollX = TRUE,
        lengthMenu = c(10, 25, 50, 100),
        language   = list(
          search        = "Buscar:",
          lengthMenu    = "Mostrar _MENU_ filas",
          info          = "Mostrando _START_ a _END_ de _TOTAL_ obs.",
          infoEmpty     = "Sin registros",
          infoFiltered  = "(filtrado de _MAX_ totales)",
          paginate      = list(previous = "Anterior", `next` = "Siguiente")
        )
      ),
      rownames = TRUE,
      class    = "stripe hover compact",
      filter   = "top"
    )
  })

  # ── 7.6 PESTAÑA CORRELACIONES ────────────────────────────────────
  output$tabla_correlaciones <- renderDT({
    df <- datos()
    vars_disp <- intersect(vars_cuant, names(df))
    vars_pred <- setdiff(vars_disp, "G3")

    tab <- data.frame(
      Variable     = vars_pred,
      `r Pearson`  = sapply(vars_pred, function(v)
        round(cor(df[[v]], df$G3, use = "complete.obs", method = "pearson"),  4)),
      `ρ Spearman` = sapply(vars_pred, function(v)
        round(cor(df[[v]], df$G3, use = "complete.obs", method = "spearman"), 4)),
      check.names = FALSE
    )
    tab <- tab[order(abs(tab$`r Pearson`), decreasing = TRUE), ]

    datatable(
      tab,
      rownames = FALSE,
      options  = list(pageLength = 16, dom = "t"),
      class    = "stripe hover compact"
    ) |>
      formatStyle(
        "r Pearson",
        background = styleInterval(
          c(-0.5, -0.3, 0.3, 0.5),
          c("#f8d7da", "#ffeeba", "white", "#d4edda", "#c3e6cb")
        ),
        fontWeight = "bold"
      ) |>
      formatStyle(
        "ρ Spearman",
        background = styleInterval(
          c(-0.5, -0.3, 0.3, 0.5),
          c("#f8d7da", "#ffeeba", "white", "#d4edda", "#c3e6cb")
        )
      )
  })

  output$plot_corrplot <- renderPlot({
    df      <- datos()
    vars_ok <- intersect(vars_cuant, names(df))
    mat_cor <- cor(df[, vars_ok], use = "complete.obs")

    corrplot::corrplot(
      mat_cor,
      method      = "color",
      type        = "lower",
      tl.col      = PAL$text_dark,
      tl.srt      = 45,
      tl.cex      = 0.75,
      addCoef.col = "#333333",
      number.cex  = 0.55,
      col         = colorRampPalette(c(PAL$danger, "white", PAL$accent))(200),
      title       = paste("Correlaciones —", input$dataset),
      mar         = c(0, 0, 2, 0)
    )
  })

  output$plot_scatter <- renderPlot({
    df  <- datos()
    xv  <- input$predictor
    m   <- modelo()
    b0  <- round(coef(m)[1], 3)
    b1  <- round(coef(m)[2], 3)
    r2  <- round(summary(m)$r.squared, 4)

    ggplot(df, aes(x = .data[[xv]], y = G3)) +
      geom_jitter(width = 0.15, height = 0.15,
                  alpha = 0.45, size = 1.8, color = PAL$accent) +
      geom_smooth(method = "lm", formula = y ~ x,
                  se = TRUE, color = PAL$danger,
                  fill = "#F5B7B1", alpha = 0.30, linewidth = 1.15) +
      annotate("label",
               x = min(df[[xv]], na.rm = TRUE),
               y = max(df$G3, na.rm = TRUE),
               label = paste0("Ĝ3 = ", b0,
                              ifelse(b1 >= 0, " + ", " - "),
                              abs(b1), " · ", xv,
                              "\nR² = ", r2),
               hjust = 0, vjust = 1, size = 4,
               color = "#922B21", fontface = "bold",
               fill = "white", label.size = 0.3) +
      labs(title    = paste(input$dataset, "· G3 ~", xv),
           subtitle = paste0("n = ", nrow(df),
                             " · pendiente β̂₁ = ", b1),
           x = xv, y = "Calificación final (G3)") +
      tema_app(13)
  })

  # ── 7.7 PESTAÑA MODELO ───────────────────────────────────────────
  output$ecuacion_modelo <- renderPrint({
    m  <- modelo()
    b0 <- round(coef(m)[1], 4)
    b1 <- round(coef(m)[2], 4)
    cat(paste0(
      "Ĝ3 = ", b0,
      ifelse(b1 >= 0, " + ", " - "), abs(b1),
      " × ", input$predictor, "\n\n",
      "Interpretación de β̂₁:\n",
      "  Por cada punto adicional en ", input$predictor, ",\n",
      "  G3 cambia en promedio ", b1, " puntos, manteniendo\n",
      "  las demás variables constantes."
    ))
  })

  output$tabla_coeficientes <- renderDT({
    m   <- modelo()
    sm  <- summary(m)
    co  <- coef(sm)

    tab <- data.frame(
      Parámetro     = rownames(co),
      Estimación    = round(co[, "Estimate"],   4),
      `Error Est.`  = round(co[, "Std. Error"], 4),
      `t`           = round(co[, "t value"],    4),
      `p-valor`     = sapply(co[, "Pr(>|t|)"], fmt_p),
      check.names   = FALSE
    )

    # BUG CORREGIDO: formatStyle con styleEqual robusto ante ausencia de ***
    estrellas <- grep("\\*\\*\\*", tab$`p-valor`, value = TRUE)
    dt <- datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    )
    if (length(estrellas) > 0) {
      dt <- dt |> formatStyle(
        "p-valor",
        color      = styleEqual(estrellas, rep("#27AE60", length(estrellas))),
        fontWeight = styleEqual(estrellas, rep("bold",    length(estrellas)))
      )
    }
    dt
  })

  output$tabla_anova <- renderDT({
    m   <- modelo()
    av  <- anova(m)
    # BUG CORREGIDO: variable clara (antes "dados <- datos()")
    n   <- nrow(datos())
    SSR <- round(av[1, "Sum Sq"],  4)
    SSE <- round(av[2, "Sum Sq"],  4)
    SST <- round(SSR + SSE,        4)
    MSR <- round(av[1, "Mean Sq"], 4)
    MSE <- round(av[2, "Mean Sq"], 4)
    Fv  <- round(av[1, "F value"], 4)
    pF  <- fmt_p(av[1, "Pr(>F)"])

    tab <- data.frame(
      Fuente       = c("Regresión (predictor)", "Error residual", "Total"),
      SC           = c(SSR, SSE, SST),
      gl           = c(1, n - 2, n - 1),
      CM           = c(MSR, MSE, NA),
      F            = c(Fv,  NA,  NA),
      `p-valor`    = c(pF,  "—", "—"),
      check.names  = FALSE
    )
    datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    ) |>
      formatStyle(0, target = "row",
                  backgroundColor = styleEqual(
                    c(1, 3), c("#D4EDDA", "#F8F9FA")
                  ))
  })

  output$tabla_r2 <- renderDT({
    sm <- summary(modelo())
    tab <- data.frame(
      Métrica = c("R²", "R² ajustado", "σ̂ (Error est. residual)", "n"),
      Valor   = c(round(sm$r.squared,     4),
                  round(sm$adj.r.squared, 4),
                  round(sm$sigma,         4),
                  nrow(datos())),
      Interpretación = c(
        paste0(round(sm$r.squared * 100, 2), "% de G3 explicada"),
        "Ajustado por gl",
        "Puntos de calif. (0-20)",
        "Obs. activas"
      )
    )
    datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    ) |>
      formatStyle("Valor", fontWeight = "bold",
                  color = PAL$primary)
  })

  output$tabla_ic_params <- renderDT({
    m   <- modelo()
    nc  <- input$nivel_conf
    ic  <- confint(m, level = nc)
    co  <- coef(m)

    tab <- data.frame(
      Parámetro   = rownames(ic),
      Estimación  = round(co, 4),
      `LC inf.`   = round(ic[, 1], 4),
      `LC sup.`   = round(ic[, 2], 4),
      Decisión    = ifelse(sign(ic[, 1]) == sign(ic[, 2]),
                            "IC no contiene 0", "IC contiene 0"),
      check.names = FALSE
    )
    nivel_pct <- round(nc * 100, 1)
    names(tab)[3:4] <- paste0(c("LC inf.", "LC sup."),
                               " (", nivel_pct, "%)")

    datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    ) |>
      formatStyle("Decisión",
                  color = styleEqual(
                    c("IC no contiene 0", "IC contiene 0"),
                    c("#27AE60", "#E74C3C")
                  ),
                  fontWeight = "bold")
  })

  output$plot_regresion <- renderPlot({
    df  <- datos()
    m   <- modelo()
    xv  <- input$predictor
    nc  <- input$nivel_conf
    xr  <- range(df[[xv]], na.rm = TRUE)
    xs  <- data.frame(x = seq(xr[1], xr[2], length.out = 200))
    names(xs) <- xv
    ic  <- as.data.frame(predict(m, newdata = xs,
                                 interval = "confidence", level = nc))
    bnd <- cbind(xs, ic)

    ggplot() +
      geom_ribbon(data = bnd,
                  aes(x = .data[[xv]], ymin = lwr, ymax = upr),
                  fill = PAL$accent, alpha = 0.22) +
      geom_jitter(data = df,
                  aes(x = .data[[xv]], y = G3),
                  width = 0.12, height = 0.12,
                  alpha = 0.40, size = 1.5, color = "#1A5276") +
      geom_line(data = bnd,
                aes(x = .data[[xv]], y = fit),
                color = PAL$accent, linewidth = 1.3) +
      labs(title    = paste0("G3 ~ ", xv),
           subtitle = paste0("IC ", round(nc * 100), "% para la media condicional"),
           x = xv, y = "G3") +
      tema_app(12)
  })

  # ── 7.8 PESTAÑA DIAGNÓSTICO ──────────────────────────────────────
  output$plot_diagnostico <- renderPlot({
    m   <- modelo()
    res <- residuals(m)
    fit <- fitted(m)
    rst <- rstandard(m)

    df_d <- data.frame(
      idx        = seq_along(res),
      resid      = res,
      fitted     = fit,
      rst        = rst,
      sqrt_abs_r = sqrt(abs(rst))
    )

    panel_tema <- tema_app(11)

    p1 <- ggplot(df_d, aes(x = fitted, y = resid)) +
      geom_point(alpha = 0.45, size = 1.6, color = PAL$accent) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = PAL$danger, linewidth = 0.8) +
      geom_smooth(method = "loess", formula = y ~ x,
                  color = PAL$danger, se = FALSE, linewidth = 0.9) +
      labs(title = "(S1) Residuales vs Ajustados",
           x = "Valores ajustados", y = "Residuales") + panel_tema

    p2 <- ggplot(df_d, aes(sample = rst)) +
      stat_qq(alpha = 0.45, size = 1.6, color = PAL$accent) +
      stat_qq_line(color = PAL$danger, linewidth = 0.9, linetype = "dashed") +
      labs(title = "(S2) Q-Q Normal",
           x = "Cuantiles teóricos", y = "Residuales estand.") + panel_tema

    p3 <- ggplot(df_d, aes(x = fitted, y = sqrt_abs_r)) +
      geom_point(alpha = 0.45, size = 1.6, color = PAL$accent) +
      geom_smooth(method = "loess", formula = y ~ x,
                  color = PAL$danger, se = FALSE, linewidth = 0.9) +
      labs(title = "(S3) Scale-Location",
           x = "Valores ajustados",
           y = expression(sqrt("|Res. estand.|"))) + panel_tema

    p4 <- ggplot(df_d, aes(x = idx, y = resid)) +
      geom_line(alpha = 0.50, color = PAL$accent, linewidth = 0.6) +
      geom_point(alpha = 0.35, size = 1.2, color = PAL$accent) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = PAL$danger, linewidth = 0.8) +
      labs(title = "(S4) Residuales vs Índice",
           x = "Índice de observación", y = "Residuales") + panel_tema

    # BUG CORREGIDO: gridExtra ya cargado al inicio
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
  })

  output$tabla_supuestos <- renderDT({
    m   <- modelo()
    res <- residuals(m)

    sw <- tryCatch(shapiro.test(res),         error = function(e) NULL)
    lf <- tryCatch(nortest::lillie.test(res), error = function(e) NULL)
    bp <- tryCatch(lmtest::bptest(m),         error = function(e) NULL)
    dw <- tryCatch(lmtest::dwtest(m, alternative = "two.sided"),
                    error = function(e) NULL)

    get_stat <- function(x) if (!is.null(x)) round(x$statistic, 4) else NA
    get_p    <- function(x) if (!is.null(x)) fmt_p(x$p.value)     else "—"
    get_dec  <- function(x) if (is.null(x)) "—" else
                 ifelse(x$p.value < 0.05, "Rechaza H₀", "No rechaza H₀")

    tab <- data.frame(
      Supuesto    = c("(S2) Normalidad", "(S2) Normalidad",
                      "(S3) Homocedasticidad", "(S4) Independencia"),
      Prueba      = c("Shapiro-Wilk", "Lilliefors",
                      "Breusch-Pagan", "Durbin-Watson"),
      Estadístico = c(get_stat(sw), get_stat(lf), get_stat(bp), get_stat(dw)),
      `p-valor`   = c(get_p(sw),    get_p(lf),    get_p(bp),    get_p(dw)),
      Decisión    = c(get_dec(sw),  get_dec(lf),  get_dec(bp),  get_dec(dw)),
      check.names = FALSE
    )

    datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    ) |>
      formatStyle("Decisión",
                  color = styleEqual(
                    c("Rechaza H₀", "No rechaza H₀"),
                    c("#E74C3C", "#27AE60")
                  ),
                  fontWeight = "bold")
  })

  # ── 7.9 PESTAÑA INFLUYENTES ──────────────────────────────────────
  output$ui_umbrales <- renderUI({
    n <- nrow(datos())
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Cook (Dᵢ)",
        value = round(umbral_cook(n), 4),
        showcase = bsicons::bs_icon("triangle-fill"),
        theme = "danger",
        p("Dᵢ > 4/n")
      ),
      value_box(
        title = "Leverage (hᵢᵢ)",
        value = round(umbral_lev(n), 4),
        showcase = bsicons::bs_icon("arrow-up-right-square"),
        theme = "warning",
        p("hᵢᵢ > 4/n")
      ),
      value_box(
        title = "|rᵢ| (Res. est.)",
        value = "2.0000",
        showcase = bsicons::bs_icon("dash-square"),
        theme = "info",
        p("Umbral convencional")
      ),
      value_box(
        title = "|DFFITSᵢ|",
        value = round(umbral_dff(n), 4),
        showcase = bsicons::bs_icon("caret-up-fill"),
        theme = "secondary",
        p("2·√(2/n)")
      )
    )
  })

  infl_data <- reactive({
    m     <- modelo()
    df    <- datos()
    n     <- nrow(df)

    cook  <- cooks.distance(m)
    lev   <- hatvalues(m)
    rstd  <- rstandard(m)
    dffit <- dffits(m)

    flag_c <- cook     > umbral_cook(n)
    flag_l <- lev      > umbral_lev(n)
    flag_r <- abs(rstd)  > 2
    flag_d <- abs(dffit) > umbral_dff(n)
    flag_a <- flag_c | flag_l | flag_r | flag_d

    list(cook = cook, lev = lev, rstd = rstd, dffit = dffit,
         flag_c = flag_c, flag_l = flag_l,
         flag_r = flag_r, flag_d = flag_d, flag_a = flag_a, n = n)
  })

  output$plot_cook <- renderPlot({
    id <- infl_data()
    n  <- id$n
    df_c <- data.frame(
      idx  = seq_along(id$cook),
      cook = id$cook,
      flag = id$flag_a
    )
    ggplot(df_c, aes(x = idx, y = cook, fill = flag)) +
      geom_col(width = 0.7, alpha = 0.85) +
      geom_hline(yintercept = umbral_cook(n),
                 color = PAL$danger, linetype = "dashed", linewidth = 0.9) +
      geom_text(data = df_c[df_c$flag, ],
                aes(label = idx), vjust = -0.4,
                size = 2.8, color = PAL$danger) +
      scale_fill_manual(values = c("FALSE" = "#85C1E9", "TRUE" = PAL$danger),
                        guide = "none") +
      labs(title = "Distancia de Cook por observación",
           subtitle = paste0("Umbral (línea roja): 4/n = ",
                             round(umbral_cook(n), 4)),
           x = "Índice de observación", y = expression(D[i])) +
      tema_app(11)
  })

  output$plot_bubble <- renderPlot({
    id <- infl_data()
    n  <- id$n
    df_b <- data.frame(
      lev  = id$lev,
      rstd = id$rstd,
      cook = id$cook,
      flag = id$flag_a,
      idx  = seq_along(id$lev)
    )
    ggplot(df_b, aes(x = lev, y = rstd,
                     size = cook, color = flag)) +
      geom_point(alpha = 0.65) +
      geom_hline(yintercept = c(-2, 2),
                 color = PAL$warning, linetype = "dashed") +
      geom_vline(xintercept = umbral_lev(n),
                 color = "#8E44AD", linetype = "dashed") +
      geom_text(data = df_b[df_b$flag, ],
                aes(label = idx), size = 2.8,
                vjust = -0.8, color = PAL$danger,
                show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = "#5DADE2", "TRUE" = PAL$danger),
                         guide  = "none") +
      scale_size_continuous(name = expression(D[i]), range = c(1, 8)) +
      labs(title = "Leverage vs Residual estandarizado",
           subtitle = "Tamaño del círculo proporcional a Dᵢ",
           x = expression(h[ii]~" (leverage)"),
           y = expression(r[i]~" (residual estandarizado)")) +
      tema_app(11)
  })

  output$tabla_influyentes <- renderDT({
    id  <- infl_data()
    df  <- datos()
    xv  <- input$predictor
    idx <- which(id$flag_a)

    if (length(idx) == 0) {
      return(datatable(
        data.frame(Resultado = "✓ No se detectaron observaciones influyentes."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }

    tab <- data.frame(
      Obs         = idx,
      X           = df[[xv]][idx],
      G3          = df$G3[idx],
      `rᵢ`        = round(id$rstd[idx],  3),
      `hᵢᵢ`       = round(id$lev[idx],   4),
      `Dᵢ`        = round(id$cook[idx],  5),
      `DFFITS`    = round(id$dffit[idx], 3),
      `|r|>2`     = ifelse(id$flag_r[idx], "●", ""),
      Lev         = ifelse(id$flag_l[idx], "●", ""),
      Cook        = ifelse(id$flag_c[idx], "●", ""),
      DFF         = ifelse(id$flag_d[idx], "●", ""),
      check.names = FALSE
    )
    names(tab)[2] <- xv

    datatable(
      tab, rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip"),
      class   = "stripe compact"
    ) |>
      formatStyle(c("|r|>2", "Lev", "Cook", "DFF"),
                  color = PAL$danger, fontWeight = "bold",
                  textAlign = "center")
  })

  output$tabla_flags <- renderDT({
    id <- infl_data()
    n  <- id$n
    tab <- data.frame(
      Criterio    = c("|rᵢ| > 2", "Leverage hᵢᵢ", "Cook Dᵢ",
                      "DFFITS", "≥ 1 criterio"),
      `N° obs.`   = c(sum(id$flag_r), sum(id$flag_l),
                       sum(id$flag_c), sum(id$flag_d), sum(id$flag_a)),
      `% total`   = round(c(sum(id$flag_r), sum(id$flag_l),
                             sum(id$flag_c), sum(id$flag_d),
                             sum(id$flag_a)) / n * 100, 2),
      check.names = FALSE
    )
    datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    ) |>
      formatStyle(0, target = "row",
                  backgroundColor = styleEqual(5, "#FFF3CD"),
                  fontWeight      = styleEqual(5, "bold"))
  })

  # ── 7.10 PESTAÑA PREDICCIÓN ──────────────────────────────────────
  output$tabla_prediccion_puntual <- renderDT({
    m   <- modelo()
    xv  <- input$predictor
    x0  <- input$x_nuevo
    nc  <- input$nivel_conf
    nd  <- setNames(data.frame(x0), xv)

    ic <- predict(m, newdata = nd, interval = "confidence", level = nc)
    ip <- predict(m, newdata = nd, interval = "prediction", level = nc)

    nv_pct <- round(nc * 100)

    tab <- data.frame(
      Tipo       = c("Ĝ3 (puntual)",
                     paste0("IC media (",      nv_pct, "%)"),
                     paste0("IP individual (", nv_pct, "%)")),
      Inferior   = c("—", round(ic[,"lwr"], 3), round(ip[,"lwr"], 3)),
      Estimación = rep(round(ic[,"fit"], 3), 3),
      Superior   = c("—", round(ic[,"upr"], 3), round(ip[,"upr"], 3))
    )
    datatable(
      tab, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      class   = "stripe compact"
    ) |>
      formatStyle("Estimación",
                  fontWeight = "bold",
                  color = PAL$primary)
  })

  output$texto_prediccion <- renderUI({
    m   <- modelo()
    xv  <- input$predictor
    x0  <- input$x_nuevo
    nc  <- input$nivel_conf
    nd  <- setNames(data.frame(x0), xv)
    ic  <- predict(m, newdata = nd, interval = "confidence", level = nc)
    ip  <- predict(m, newdata = nd, interval = "prediction", level = nc)

    tags$p(
      style = "margin: 0; font-size: 0.9rem;",
      "Para un estudiante con ", tags$code(paste0(xv, " = ", x0)),
      ", la calificación final predicha es ",
      tags$b(round(ic[,"fit"], 2), " puntos"),
      ". Con ", round(nc * 100),
      "% de confianza, la media poblacional se ubica en [",
      round(ic[,"lwr"], 2), ", ",
      round(ic[,"upr"], 2), "] y la calificación individual en [",
      round(ip[,"lwr"], 2), ", ",
      round(ip[,"upr"], 2), "]."
    )
  })

  output$plot_prediccion <- renderPlot({
    m   <- modelo()
    df  <- datos()
    xv  <- input$predictor
    x0  <- input$x_nuevo
    nc  <- input$nivel_conf
    xr  <- range(df[[xv]], na.rm = TRUE)
    xs  <- setNames(data.frame(seq(xr[1], xr[2], length.out = 300)), xv)

    ic <- as.data.frame(predict(m, newdata = xs,
                                interval = "confidence", level = nc))
    ip <- as.data.frame(predict(m, newdata = xs,
                                interval = "prediction", level = nc))
    bnd <- cbind(xs, fit = ic$fit,
                 ic_lwr = ic$lwr, ic_upr = ic$upr,
                 ip_lwr = ip$lwr, ip_upr = ip$upr)

    nd0   <- setNames(data.frame(x0), xv)
    pred0 <- predict(m, newdata = nd0)

    ggplot() +
      geom_ribbon(data = bnd,
                  aes(x = .data[[xv]], ymin = ip_lwr, ymax = ip_upr),
                  fill = "#AED6F1", alpha = 0.30) +
      geom_ribbon(data = bnd,
                  aes(x = .data[[xv]], ymin = ic_lwr, ymax = ic_upr),
                  fill = PAL$accent, alpha = 0.35) +
      geom_jitter(data = df,
                  aes(x = .data[[xv]], y = G3),
                  width = 0.12, height = 0.12,
                  alpha = 0.35, size = 1.4, color = "#1A5276") +
      geom_line(data = bnd,
                aes(x = .data[[xv]], y = fit),
                color = PAL$accent, linewidth = 1.3) +
      geom_vline(xintercept = x0,
                 color = PAL$danger, linetype = "dashed", linewidth = 0.9) +
      geom_point(aes(x = x0, y = pred0),
                 color = PAL$danger, size = 4.5, shape = 18) +
      annotate("label", x = x0, y = pred0,
               label = paste0("Ĝ3 = ", round(pred0, 2)),
               color = "#922B21", size = 4, fontface = "bold",
               hjust = -0.15, fill = "white", label.size = 0.4) +
      labs(
        title    = paste0(input$dataset, " — Recta con IC y IP al ",
                          round(nc * 100), "%"),
        subtitle = "Banda oscura: IC para E(G3|X=x₀) · Banda clara: IP para obs. individual",
        x = xv, y = "Calificación final (G3)"
      ) +
      tema_app(12)
  })

  output$tabla_pred_rango <- renderDT({
    m   <- modelo()
    xv  <- input$predictor
    df  <- datos()
    nc  <- input$nivel_conf
    xr  <- range(df[[xv]], na.rm = TRUE)
    x0s <- setNames(data.frame(seq(ceiling(xr[1]), floor(xr[2]), by = 1)), xv)

    ic <- as.data.frame(predict(m, newdata = x0s,
                                interval = "confidence", level = nc))
    ip <- as.data.frame(predict(m, newdata = x0s,
                                interval = "prediction", level = nc))

    tab <- data.frame(
      X0          = x0s[[1]],
      `Ĝ3`        = round(ic$fit, 3),
      `IC inf.`   = round(ic$lwr, 3),
      `IC sup.`   = round(ic$upr, 3),
      `IP inf.`   = round(ip$lwr, 3),
      `IP sup.`   = round(ip$upr, 3),
      `Amp. IP`   = round(ip$upr - ip$lwr, 3),
      check.names = FALSE
    )
    names(tab)[1] <- xv

    datatable(
      tab, rownames = FALSE,
      options = list(pageLength = 10, dom = "tip"),
      class   = "stripe hover compact"
    ) |>
      formatStyle("Ĝ3", fontWeight = "bold", color = PAL$primary) |>
      formatStyle(c("IC inf.", "IC sup."), background = "#E8F4F8") |>
      formatStyle(c("IP inf.", "IP sup."), background = "#FEF5E7")
  })

} # fin server


# ------------------------------------------------------------
# 8. EJECUTAR LA APLICACIÓN
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
