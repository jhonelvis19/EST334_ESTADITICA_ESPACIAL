# ============================================================
#  SAUSALITO · Dashboard UNALM  —  app.R
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(scales)

# ── Paleta de colores ─────────────────────────────────────
C_TEAL      <- "#0d9488"   # Verde azulado principal
C_TEAL_D    <- "#0f766e"   # Teal oscuro
C_TEAL_L    <- "#ccfbf1"   # Teal muy claro (fondo suave)
C_AMBER     <- "#d97706"   # Ámbar / warning
C_AMBER_L   <- "#fef3c7"
C_INDIGO    <- "#4f46e5"   # Azul índigo / info
C_INDIGO_L  <- "#e0e7ff"
C_ROSE      <- "#e11d48"   # Rosa / alerta
C_ROSE_L    <- "#ffe4e6"
C_SLATE     <- "#475569"   # Gris pizarra
C_SLATE_L   <- "#f1f5f9"   # Fondo general
C_WHITE     <- "#ffffff"
C_BORDER    <- "#e2e8f0"

PALETA_GRAF <- c("#0d9488","#d97706","#4f46e5","#e11d48","#475569","#7c3aed","#0284c7")

# ── Carga de datos ────────────────────────────────────────
load_data <- function() {
  base <- "."
  safe_read <- function(f) {
    p <- file.path(base, f)
    if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
    else data.frame()
  }
  list(
    prod        = safe_read("produccion_leche.csv"),
    gestantes   = safe_read("vacas_gestantes.csv"),
    para_secar  = safe_read("vacas_para_secar.csv"),
    prox_parto  = safe_read("vacas_proximas_parto.csv"),
    inseminadas = safe_read("vacas_inseminadas.csv"),
    secas       = safe_read("vacas_secas.csv"),
    problema    = safe_read("vacas_problema.csv"),
    mastitis    = safe_read("mastitis.csv"),
    test_prenez = safe_read("test_prenez.csv"),
    vaquillas   = safe_read("vaquillas.csv"),
    vaq_parto   = safe_read("vaquillas_proximas_parto.csv")
  )
}

datos <- load_data()

# ── KPIs ──────────────────────────────────────────────────
prod_prom      <- if (nrow(datos$prod) > 0) round(mean(datos$prod$produccion_promedio, na.rm=TRUE), 1) else 0
vacas_prod     <- nrow(datos$prod)
vacas_gest     <- nrow(datos$gestantes)
vacas_secar    <- nrow(datos$para_secar)
vacas_secas    <- nrow(datos$secas)
vacas_insem    <- nrow(datos$inseminadas)
vacas_problema <- nrow(datos$problema)
vacas_mastitis <- if (nrow(datos$mastitis) > 0) nrow(unique(datos$mastitis["vaca"])) else 0
iep_prom       <- if (nrow(datos$test_prenez) > 0) round(mean(datos$test_prenez$dias_insem, na.rm=TRUE)) else 0
pct_gest       <- if (vacas_prod > 0) round(vacas_gest / vacas_prod * 100, 1) else 0
tasa_concep    <- if ((vacas_gest + vacas_insem) > 0) round(vacas_gest / (vacas_gest + vacas_insem) * 100) else 0

hato_completo <- data.frame(
  categoria = c("En Producción", "Secas / Preparto", "Vaquillas Reemplazo"),
  n = c(vacas_prod, vacas_secas, nrow(datos$vaquillas))
) %>% mutate(pct = round(n / sum(n) * 100, 1))

tend_prod <- datos$prod %>%
  mutate(dim_bin = floor(dias_lactacion / 30) * 30) %>%
  group_by(dim_bin) %>%
  summarise(prom_hato = mean(produccion_actual_24h, na.rm=TRUE),
            prom_prod = mean(produccion_promedio, na.rm=TRUE), .groups="drop") %>%
  arrange(dim_bin) %>% mutate(meta = 40)

prod_grupo <- datos$prod %>%
  group_by(grupo) %>%
  summarise(prod_prom_g = round(mean(produccion_promedio, na.rm=TRUE), 1),
            n_vacas = n(), .groups="drop") %>%
  arrange(desc(prod_prom_g)) %>%
  mutate(grupo_label = paste0("Grupo ", grupo, " (n=", n_vacas, ")"))

alertas <- datos$para_secar %>%
  select(vaca, status, dias_lactacion, dias_al_secado, prod_actual,
         fecha_secado_recom, fecha_estimada_parto) %>%
  rename("Vaca N°"="vaca","Estado"="status","Días Lact."="dias_lactacion",
         "Días a Secar"="dias_al_secado","Prod. Prom.(kg)"="prod_actual",
         "Fecha Secado"="fecha_secado_recom","Parto Estimado"="fecha_estimada_parto") %>%
  arrange(`Días a Secar`)

# ── Helper: KPI card ──────────────────────────────────────
kpi <- function(valor, etiqueta, icono, color_fondo, color_icono, color_texto="#1e293b") {
  div(
    style = paste0(
      "background:", color_fondo, ";",
      "border-radius:16px; padding:20px 22px; margin-bottom:16px;",
      "box-shadow:0 1px 4px rgba(0,0,0,.06);",
      "display:flex; align-items:center; gap:16px;",
      "border:1px solid ", C_BORDER, ";"
    ),
    div(
      style = paste0(
        "width:52px; height:52px; border-radius:12px;",
        "background:", color_icono, ";",
        "display:flex; align-items:center; justify-content:center;",
        "font-size:22px; color:#fff; flex-shrink:0;"
      ),
      icon(icono)
    ),
    div(
      div(style=paste0("font-size:26px; font-weight:700; color:", color_texto, "; line-height:1;"), valor),
      div(style="font-size:11px; color:#64748b; margin-top:5px; text-transform:uppercase; letter-spacing:.7px; font-weight:500;", etiqueta)
    )
  )
}

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span(
      tags$img(src="", style="display:none;"),
      tags$span(style="font-weight:800; font-size:15px; letter-spacing:.3px; color:#fff;",
                "🐄 SAUSALITO"),
      tags$span(style="font-size:11px; color:#94a3b8; margin-left:6px; font-weight:400;",
                "· Dashboard UNALM")
    ),
    titleWidth = 290
  ),
  
  dashboardSidebar(
    width = 250,
    tags$div(
      style = "padding:20px 16px 8px; color:#94a3b8; font-size:10px; font-weight:600; letter-spacing:1.2px; text-transform:uppercase;",
      "Módulos"
    ),
    sidebarMenu(
      menuItem("Dashboard Principal",  tabName = "dashboard",    icon = icon("gauge-high")),
      menuItem("Inventario del Hato",  tabName = "inventario",   icon = icon("cow")),
      menuItem("Producción Diaria",    tabName = "produccion",   icon = icon("droplet")),
      menuItem("Control Reproductivo", tabName = "reproduccion", icon = icon("calendar-check")),
      menuItem("Mastitis",             tabName = "mastitis",     icon = icon("bacteria")),
      menuItem("Vacas Problema",       tabName = "problema",     icon = icon("triangle-exclamation"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML(paste0("

      /* ── Reset general ── */
      body, .content-wrapper, .right-side {
        background: ", C_SLATE_L, " !important;
        font-family: 'Segoe UI', system-ui, -apple-system, sans-serif !important;
      }

      /* ── Sidebar ── */
      .main-sidebar {
        background: #0f172a !important;
        border-right: none !important;
      }
      .sidebar-menu > li > a {
        color: #94a3b8 !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        padding: 11px 20px !important;
        border-radius: 8px !important;
        margin: 2px 10px !important;
        transition: all .2s !important;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li > a:hover {
        background: #1e293b !important;
        color: #f1f5f9 !important;
      }
      .sidebar-menu > li.active > a {
        border-left: 3px solid ", C_TEAL, " !important;
        color: ", C_TEAL, " !important;
      }
      .sidebar-menu > li > a .fa,
      .sidebar-menu > li > a .fas,
      .sidebar-menu > li > a .far {
        color: #64748b !important;
        width: 20px !important;
      }
      .sidebar-menu > li.active > a .fa,
      .sidebar-menu > li.active > a .fas {
        color: ", C_TEAL, " !important;
      }

      /* ── Header ── */
      .skin-black .main-header .navbar,
      .skin-black .main-header .logo {
        background: #0f172a !important;
        border-bottom: 1px solid #1e293b !important;
      }
      .skin-black .main-header .navbar .sidebar-toggle {
        color: #94a3b8 !important;
      }

      /* ── Content padding ── */
      .content { padding: 24px !important; }
      .tab-content { padding-top: 4px; }

      /* ── Section header strip ── */
      .sec-header {
        background: linear-gradient(135deg, ", C_TEAL_D, ", ", C_TEAL, ");
        color: #fff;
        border-radius: 10px;
        padding: 10px 18px;
        font-size: 13px;
        font-weight: 600;
        margin-bottom: 14px;
        letter-spacing: .3px;
      }

      /* ── Chart / table card ── */
      .chart-box {
        background: ", C_WHITE, ";
        border-radius: 16px;
        padding: 20px 22px;
        box-shadow: 0 1px 4px rgba(0,0,0,.06);
        margin-bottom: 16px;
        border: 1px solid ", C_BORDER, ";
      }
      .chart-title {
        font-size: 13px;
        color: #475569;
        font-weight: 600;
        margin-bottom: 12px;
        padding-bottom: 10px;
        border-bottom: 1px solid ", C_BORDER, ";
      }

      /* ── DataTables ── */
      table.dataTable thead th {
        background: #0f172a !important;
        color: #f1f5f9 !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        border: none !important;
        padding: 10px 14px !important;
      }
      table.dataTable tbody tr { font-size: 12.5px; }
      table.dataTable tbody tr:hover { background: ", C_TEAL_L, " !important; }
      table.dataTable tbody td { padding: 8px 14px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background: ", C_TEAL, " !important;
        color: #fff !important;
        border-radius: 6px;
        border: none !important;
      }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid ", C_BORDER, ";
        border-radius: 8px;
        padding: 4px 10px;
        font-size: 12px;
      }

      /* ── Row gap ── */
      .row { margin-bottom: 4px; }

      /* ── Scrollbar ── */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: #f1f5f9; }
      ::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 3px; }
    ")))),
    
    tabItems(
      
      # ═══════════════════════════════════════════════════
      # TAB 1 — DASHBOARD PRINCIPAL
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "dashboard",
              
              fluidRow(
                column(2, kpi(paste0(prod_prom," kg"), "Prod. Prom./Vaca/Día",
                              "weight-hanging", C_TEAL_L, C_TEAL)),
                column(2, kpi(vacas_prod, "Vacas en Producción",
                              "cow", "#fef3c7", C_AMBER)),
                column(2, kpi(paste0(vacas_gest," (",pct_gest,"%)"), "Vacas Gestantes",
                              "baby", C_INDIGO_L, C_INDIGO)),
                column(2, kpi(paste0(iep_prom," d"), "Días Insem. Prom.",
                              "calendar-days", C_SLATE_L, C_SLATE)),
                column(2, kpi(vacas_mastitis, "Con Mastitis",
                              "bacteria", C_ROSE_L, C_ROSE)),
                column(2, kpi(paste0(tasa_concep,"%"), "Tasa Concepción",
                              "percent", "#f0fdf4", "#16a34a"))
              ),
              
              fluidRow(
                column(8, div(class="chart-box",
                              div(class="chart-title", "📈 Tendencia de Producción por Período de Lactancia (DIM)"),
                              plotlyOutput("plot_tendencia", height="300px")
                )),
                column(4, div(class="chart-box",
                              div(class="chart-title", "🐄 Composición del Hato"),
                              plotlyOutput("plot_estado_hato", height="300px")
                ))
              ),
              
              fluidRow(
                column(7, div(class="chart-box",
                              div(class="sec-header", "⚠️  Alertas Reproductivas — Vacas a Secar"),
                              DTOutput("tabla_alertas")
                )),
                column(5, div(class="chart-box",
                              div(class="chart-title", "🥛 Producción Promedio por Grupo (kg/día)"),
                              plotlyOutput("plot_grupos", height="280px")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB 2 — INVENTARIO
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "inventario",
              
              fluidRow(
                column(3, kpi(vacas_prod, "Vacas en Producción", "cow", C_TEAL_L, C_TEAL)),
                column(3, kpi(vacas_secas, "Vacas Secas", "moon", "#fef3c7", C_AMBER)),
                column(3, kpi(nrow(datos$vaquillas), "Vaquillas Reemplazo", "seedling", C_INDIGO_L, C_INDIGO)),
                column(3, kpi(vacas_prod + vacas_secas + nrow(datos$vaquillas), "Total Animales", "list", C_SLATE_L, C_SLATE))
              ),
              
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución completa del hato"),
                              plotlyOutput("plot_hato_completo", height="350px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Estado de las vacas en producción"),
                              plotlyOutput("plot_status_prod", height="350px")
                ))
              ),
              
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="sec-header", "📋  Listado Producción"),
                               DTOutput("tabla_prod_completa")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB 3 — PRODUCCIÓN DIARIA
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "produccion",
              
              fluidRow(
                column(3, kpi(paste0(prod_prom," kg"), "Prom. Producción", "chart-line", C_TEAL_L, C_TEAL)),
                column(3, kpi(paste0(round(max(datos$prod$produccion_promedio, na.rm=TRUE),1)," kg"),
                              "Máxima Actual", "arrow-trend-up", "#fef3c7", C_AMBER)),
                column(3, kpi(paste0(round(min(datos$prod$produccion_promedio, na.rm=TRUE),1)," kg"),
                              "Mínima Actual", "arrow-trend-down", C_INDIGO_L, C_INDIGO)),
                column(3, kpi(paste0(round(sum(datos$prod$produccion_promedio, na.rm=TRUE)/1000,1)," t/día"),
                              "Producción Total Est.", "droplet", C_SLATE_L, C_SLATE))
              ),
              
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="chart-title", "Distribución de producción promedio por vaca (kg/día)"),
                               plotlyOutput("hist_produccion", height="300px")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="sec-header", "📋  Detalle Producción por Vaca"),
                               DTOutput("tabla_produccion_detalle")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB 4 — REPRODUCCIÓN
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "reproduccion",
              
              fluidRow(
                column(3, kpi(vacas_insem, "Vacas Inseminadas", "syringe", C_TEAL_L, C_TEAL)),
                column(3, kpi(vacas_gest, "Vacas Gestantes", "baby", "#fef3c7", C_AMBER)),
                column(3, kpi(iep_prom, "Días Insem. Prom.", "calendar-days", C_INDIGO_L, C_INDIGO)),
                column(3, kpi(vacas_problema, "Vacas Problema (≥3)", "triangle-exclamation", C_ROSE_L, C_ROSE))
              ),
              
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución de días abiertos"),
                              plotlyOutput("hist_dias_abiertos", height="280px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución de inseminaciones por vaca"),
                              plotlyOutput("plot_cant_insem", height="280px")
                ))
              ),
              
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="sec-header", "📋  Próximas al Parto (siguientes 21 días)"),
                               DTOutput("tabla_prox_parto")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="sec-header", "⚠️  Vacas Problema (≥3 inseminaciones)"),
                               DTOutput("tabla_problema")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB 5 — MASTITIS
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "mastitis",
              
              fluidRow(
                column(4, kpi(vacas_mastitis, "Vacas con Historial de Mastitis", "bacteria", C_ROSE_L, C_ROSE)),
                column(4, kpi(nrow(datos$mastitis), "Total Eventos de Mastitis", "file-medical", "#fef3c7", C_AMBER)),
                column(4, kpi(paste0(if(vacas_prod>0) round(vacas_mastitis/vacas_prod*100,1) else 0, "%"),
                              "% Hato con Mastitis", "percent", C_INDIGO_L, C_INDIGO))
              ),
              
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Eventos por número de lactación"),
                              plotlyOutput("plot_mastitis_lac", height="300px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Tipo de diagnóstico"),
                              plotlyOutput("plot_mastitis_tipo", height="300px")
                ))
              ),
              
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="sec-header", "🦠  Listado Completo — Animales con Mastitis"),
                               DTOutput("tabla_mastitis_full")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB 6 — VACAS PROBLEMA
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "problema",
              
              fluidRow(
                column(4, kpi(vacas_problema, "Vacas con 3 o más inseminaciones",
                              "triangle-exclamation", C_ROSE_L, C_ROSE))
              ),
              
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución DIM en vacas problema"),
                              plotlyOutput("plot_problema_dim", height="280px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Estado de vacas problema"),
                              plotlyOutput("plot_problema_status", height="280px")
                ))
              ),
              
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="sec-header", "⚠️  Listado completo — Vacas Problema"),
                               DTOutput("tabla_problema_full")
                ))
              )
      )
      
    ) # fin tabItems
  ) # fin dashboardBody
)

# ============================================================
#  SERVER  (lógica idéntica, colores actualizados)
# ============================================================
server <- function(input, output, session) {
  
  TEAL  <- "#0d9488"
  AMBER <- "#d97706"
  INDIGO<- "#4f46e5"
  ROSE  <- "#e11d48"
  SLATE <- "#475569"
  PAL   <- c("#0d9488","#d97706","#4f46e5","#e11d48","#475569","#7c3aed","#0284c7")
  
  lay <- function(fig, xtitle="", ytitle="")
    layout(fig,
           xaxis = list(title=xtitle, gridcolor="#f1f5f9", zeroline=FALSE,
                        titlefont=list(size=12, color="#64748b")),
           yaxis = list(title=ytitle, gridcolor="#f1f5f9", zeroline=FALSE,
                        titlefont=list(size=12, color="#64748b")),
           plot_bgcolor  = "#ffffff",
           paper_bgcolor = "#ffffff",
           font  = list(family="Segoe UI, system-ui, sans-serif", color="#334155"),
           margin = list(l=46, r=16, t=14, b=46))
  
  # Tendencia producción
  output$plot_tendencia <- renderPlotly({
    df <- tend_prod
    plot_ly(df, x=~dim_bin) %>%
      add_lines(y=~prom_hato, name="Prod. Actual 24h",
                line=list(color=TEAL, width=3)) %>%
      add_lines(y=~prom_prod, name="Prod. Promedio",
                line=list(color=AMBER, width=2, dash="dot")) %>%
      add_lines(y=~meta, name="Meta 40 kg",
                line=list(color=ROSE, width=1.5, dash="dash")) %>%
      lay("Días en Leche (DIM)", "kg / vaca / día") %>%
      layout(legend=list(orientation="h", y=-0.28, font=list(size=11)),
             hovermode="x unified")
  })
  
  output$plot_estado_hato <- renderPlotly({
    plot_ly(hato_completo, labels=~categoria, values=~n, type="pie",
            marker=list(colors=c(TEAL, AMBER, INDIGO),
                        line=list(color="#fff", width=2)),
            textinfo="label+percent", hoverinfo="label+value+percent",
            hole=0.4) %>%
      layout(paper_bgcolor="#fff", showlegend=TRUE,
             legend=list(font=list(size=11)),
             margin=list(l=5,r=5,t=10,b=5))
  })
  
  output$tabla_alertas <- renderDT({
    datatable(alertas, rownames=FALSE,
              options=list(pageLength=8, scrollX=TRUE, dom="ftp",
                           language=list(search="Buscar:"))) %>%
      formatStyle("Días a Secar",
                  backgroundColor=styleInterval(c(-90,-30), c("#ffe4e6","#fef3c7","#d1fae5")),
                  fontWeight="bold")
  })
  
  output$plot_grupos <- renderPlotly({
    df <- prod_grupo %>% head(15)
    plot_ly(df, y=~grupo_label, x=~prod_prom_g,
            type="bar", orientation="h",
            marker=list(color=TEAL, line=list(color=C_TEAL_D, width=1),
                        opacity=.88)) %>%
      lay("kg/vaca/día promedio", "") %>%
      layout(yaxis=list(autorange="reversed"))
  })
  
  output$plot_hato_completo <- renderPlotly({
    plot_ly(hato_completo, labels=~categoria, values=~n, type="pie",
            marker=list(colors=c(TEAL, AMBER, INDIGO),
                        line=list(color="#fff", width=2)),
            textinfo="label+value+percent", hole=0.35) %>%
      layout(paper_bgcolor="#fff", margin=list(l=5,r=5,t=10,b=5))
  })
  
  output$plot_status_prod <- renderPlotly({
    df <- datos$prod %>% count(status) %>% arrange(desc(n))
    plot_ly(df, x=~status, y=~n, type="bar",
            marker=list(color=PAL[seq_len(nrow(df))], opacity=.88)) %>%
      lay("Estado", "N° vacas")
  })
  
  output$tabla_prod_completa <- renderDT({
    datos$prod %>%
      select(vaca, grupo, status, dias_lactacion,
             produccion_actual_24h, produccion_promedio,
             produccion_maxima, total_lactacion) %>%
      rename("Vaca"="vaca","Grupo"="grupo","Estado"="status","DIM"="dias_lactacion",
             "Prod.24h(kg)"="produccion_actual_24h","Prom.(kg)"="produccion_promedio",
             "Máx.(kg)"="produccion_maxima","Total Lact."="total_lactacion") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
  
  output$hist_produccion <- renderPlotly({
    plot_ly(datos$prod, x=~produccion_promedio, type="histogram",
            nbinsx=20, marker=list(color=TEAL, line=list(color="#0f766e", width=1),
                                   opacity=.85)) %>%
      lay("Producción promedio (kg/día)", "N° vacas")
  })
  
  output$tabla_produccion_detalle <- renderDT({
    datos$prod %>%
      select(vaca, grupo, status, dias_lactacion, kg_ultimo_ordeno,
             produccion_actual_24h, produccion_promedio,
             variacion_diaria, produccion_maxima) %>%
      rename("Vaca"="vaca","Gp"="grupo","Estado"="status","DIM"="dias_lactacion",
             "Últ.Ordeño(kg)"="kg_ultimo_ordeno","Prod.24h(kg)"="produccion_actual_24h",
             "Prom.(kg)"="produccion_promedio","Variación"="variacion_diaria",
             "Máx.(kg)"="produccion_maxima") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE)) %>%
      formatStyle("Variación",
                  color=styleInterval(0, c("#e11d48","#16a34a")), fontWeight="bold")
  })
  
  output$hist_dias_abiertos <- renderPlotly({
    plot_ly(datos$inseminadas, x=~dias_abiertos, type="histogram",
            nbinsx=20, marker=list(color=AMBER, opacity=.85)) %>%
      lay("Días abiertos", "N° vacas")
  })
  
  output$plot_cant_insem <- renderPlotly({
    df <- datos$inseminadas %>% count(cant_inseminaciones) %>%
      mutate(cant_inseminaciones=factor(cant_inseminaciones))
    plot_ly(df, x=~cant_inseminaciones, y=~n, type="bar",
            marker=list(color=INDIGO, opacity=.88)) %>%
      lay("N° inseminaciones", "N° vacas")
  })
  
  output$tabla_prox_parto <- renderDT({
    datos$prox_parto %>%
      select(vaca, grupo, nombre, lactacion, dim,
             toro, fecha_parto_est, dias_faltantes, status) %>%
      rename("Vaca"="vaca","Grupo"="grupo","Nombre"="nombre","Lact."="lactacion",
             "DIM"="dim","Toro"="toro","Parto Est."="fecha_parto_est",
             "Días Faltan"="dias_faltantes","Estado"="status") %>%
      arrange(`Días Faltan`) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             language=list(search="Buscar:"))) %>%
      formatStyle("Días Faltan",
                  backgroundColor=styleInterval(c(-7,7),c("#ffe4e6","#fef9c3","#d1fae5")),
                  fontWeight="bold")
  })
  
  output$tabla_problema <- renderDT({
    datos$problema %>%
      rename("Vaca"="vaca","Estado"="status","Grupo"="grupo","DIM"="dim") %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
  
  output$plot_mastitis_lac <- renderPlotly({
    df <- datos$mastitis %>% count(lactacion) %>%
      mutate(lactacion=factor(lactacion))
    plot_ly(df, x=~lactacion, y=~n, type="bar",
            marker=list(color=ROSE, opacity=.88)) %>%
      lay("Número de Lactación", "N° eventos")
  })
  
  output$plot_mastitis_tipo <- renderPlotly({
    df <- datos$mastitis %>%
      filter(tipo != "" & !is.na(tipo)) %>%
      count(tipo) %>% arrange(desc(n)) %>% head(10)
    plot_ly(df, y=~reorder(tipo, n), x=~n, type="bar", orientation="h",
            marker=list(color=AMBER, opacity=.88)) %>%
      lay("N° eventos", "Tipo diagnóstico") %>%
      layout(yaxis=list(autorange="reversed"))
  })
  
  output$tabla_mastitis_full <- renderDT({
    datos$mastitis %>%
      rename("Vaca"="vaca","Edad"="edad","Lactación"="lactacion",
             "DIM"="dias_lact","Leche 24h(kg)"="leche_24h",
             "Días Relativo"="dias_relativo","Fecha"="fecha_evento",
             "Tipo"="tipo","Tratamiento"="tratamiento") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=20, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
  
  output$plot_problema_dim <- renderPlotly({
    plot_ly(datos$problema, x=~dim, type="histogram",
            nbinsx=10, marker=list(color=ROSE, opacity=.88)) %>%
      lay("Días en Leche (DIM)", "N° vacas")
  })
  
  output$plot_problema_status <- renderPlotly({
    df <- datos$problema %>% count(status) %>% arrange(desc(n))
    plot_ly(df, x=~status, y=~n, type="bar",
            marker=list(color=PAL[seq_len(nrow(df))], opacity=.88)) %>%
      lay("Estado", "N° vacas")
  })
  
  output$tabla_problema_full <- renderDT({
    datos$problema %>%
      rename("Vaca"="vaca","Estado"="status","Grupo"="grupo","DIM"="dim") %>%
      datatable(rownames=FALSE,
                options=list(pageLength=20, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
}

shinyApp(ui, server)