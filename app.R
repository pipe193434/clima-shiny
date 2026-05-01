library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(scales)

# ── DATOS ──────────────────────────────────────────────────────────────────
df_raw <- read.csv("data/global_climate_energy_2020_2024.csv",
                   stringsAsFactors = FALSE)

df <- df_raw %>%
  mutate(
    date               = as.Date(date),
    avg_temperature    = as.numeric(avg_temperature),
    co2_emission       = as.numeric(co2_emission),
    energy_consumption = as.numeric(energy_consumption),
    renewable_share    = as.numeric(renewable_share),
    country            = trimws(country),
    year               = year(date),
    month              = floor_date(date, "month")
  ) %>%
  filter(
    !is.na(date), !is.na(country),
    !is.na(avg_temperature), !is.na(co2_emission),
    !is.na(energy_consumption), !is.na(renewable_share)
  )

all_countries <- sort(unique(df$country))
all_years     <- sort(unique(df$year))

# ── COLORES ────────────────────────────────────────────────────────────────
BLUE_MAIN  <- "#1f3b73"
BLUE_LIGHT <- "#4e79a7"
GREEN_MAIN <- "#2a9d8f"
GREEN_DARK <- "#1b7f6b"
GREY_GRID  <- "#ebebeb"

# ── Función: añadir título+subtítulo+pregunta dentro del chart (igual D3) ──
add_chart_titles <- function(p, title, subtitle, question,
                              margin_top = 120) {
  p %>% layout(
    margin = list(t = margin_top, l = 65, r = 30, b = 60),
    annotations = list(
      list(
        text = title,
        xref = "paper", yref = "paper",
        x = 0, y = 1,
        xanchor = "left", yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 15, color = "#111", family = "Segoe UI, sans-serif"),
        yshift = 72
      ),
      list(
        text = subtitle,
        xref = "paper", yref = "paper",
        x = 0, y = 1,
        xanchor = "left", yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 11, color = "#999", family = "Segoe UI, sans-serif"),
        yshift = 52
      ),
      list(
        text = question,
        xref = "paper", yref = "paper",
        x = 0, y = 1,
        xanchor = "left", yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 11, color = "#555", family = "Segoe UI, sans-serif"),
        yshift = 32
      )
    )
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Dashboard Climático",
  tags$head(tags$style(HTML("

    body {
      font-family: 'Segoe UI', system-ui, sans-serif;
      background: #f4f4f0;
      color: #1a1a1a;
      margin: 0;
      padding: 40px 40px 80px;
    }
    .container-fluid {
      max-width: 1000px;
      margin: 0 auto;
      padding: 0;
    }

    /* Título principal */
    h1.page-title {
      font-size: 24px;
      font-weight: 600;
      color: #111;
      letter-spacing: -0.3px;
      margin-bottom: 4px;
    }
    hr.page-divider {
      border: none;
      border-top: 1.5px solid #dddbd3;
      margin-bottom: 36px;
    }

    /* Filtros */
    .filtros-bar {
      background: #fff;
      border: 1px solid #e2dfd8;
      border-radius: 10px;
      padding: 14px 22px;
      margin-bottom: 28px;
      display: flex;
      flex-wrap: wrap;
      gap: 20px;
      align-items: flex-end;
    }
    .filtros-bar .form-group { margin-bottom: 0; }
    .filtros-label {
      font-size: 10px;
      font-weight: 600;
      letter-spacing: 0.09em;
      text-transform: uppercase;
      color: #bbb;
      display: block;
      margin-bottom: 5px;
    }
    .selectize-input {
      font-size: 12px !important;
      min-height: 34px !important;
    }

    /* Card */
    .chart-card {
      background: #ffffff;
      border: 1px solid #e2dfd8;
      border-radius: 10px;
      padding: 22px 28px 22px;
      margin-bottom: 30px;
      transition: box-shadow 0.3s;
    }
    .chart-card:hover {
      box-shadow: 0 8px 24px rgba(0,0,0,.08);
    }
    .chart-card h2 {
      font-size: 16px;
      font-weight: 600;
      color: #111;
      letter-spacing: -0.2px;
      margin: 0 0 14px 0;
    }

    /* Análisis */
    .insight-wrap { margin-top: 4px; }
    .insight-wrap .insight-title {
      font-size: 15px;
      font-weight: 700;
      color: #111;
      margin: 0 0 4px 0;
    }
    .insight-wrap .insight-text {
      font-size: 12px;
      color: #555;
      line-height: 1.65;
      margin: 0;
    }

    @media (max-width: 680px) {
      body { padding: 20px 16px 60px; }
      .chart-card { padding: 20px 16px; }
      h1.page-title { font-size: 20px; }
    }
  "))),

  tags$h1("Clima", class = "page-title"),
  tags$hr(class = "page-divider"),

  # Inputs ocultos (mantienen la lógica reactiva sin mostrarse)
  tags$div(style = "display:none;",
    selectInput("sel_countries", label = NULL,
                choices = all_countries, selected = all_countries,
                multiple = TRUE),
    selectInput("sel_years", label = NULL,
                choices = all_years, selected = all_years,
                multiple = TRUE)
  ),

  # ── Chart 1 ─────────────────────────────────────────────────────────────
  tags$div(class = "chart-card",
    tags$h2("Temperatura vs Tiempo"),
    plotlyOutput("plot_temp", height = "400px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p(paste(
        "Esta gráfica muestra cómo cambia la temperatura promedio a lo largo del tiempo.",
        "Se puede observar un patrón muy claro y repetitivo, donde la temperatura sube y baja",
        "de forma constante cada año.Los picos representan los momentos de mayor temperatura,",
        "mientras que los puntos más bajos corresponden a las épocas más frías.",
        "Este comportamiento se repite de manera similar en todos los años, lo que indica",
        "que hay una tendencia estacional bastante marcada"
      ), class = "insight-text")
    )
  ),

  # ── Chart 2 ─────────────────────────────────────────────────────────────
  tags$div(class = "chart-card",
    tags$h2("CO2 por país"),
    plotlyOutput("plot_co2", height = "400px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p(paste(
        "Las emisiones de CO₂ entre los países no son muy diferentes entre sí, ya que la mayoría",
        "se mantiene en un rango bastante parecido. Australia es el país con el valor más alto,",
        "mientras que Turquía tiene el más bajo. Aunque hay un orden de mayor a menor,la diferencia",
        "entre los países no es muy grande,lo que indica que todos tienen niveles de emisiones",
        "bastante similares dentro del conjunto de datos."
      ), class = "insight-text")
    )
  ),

  # ── Chart 3 ─────────────────────────────────────────────────────────────
  tags$div(class = "chart-card",
    tags$h2("Consumo vs Precio"),
    plotlyOutput("plot_box", height = "400px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p(paste(
        "Esta gráfica muestra cómo se distribuye el consumo energético en diferentes países.",
        "Se puede ver que la mayoría tienen valores bastante parecidos, ya que las medianas",
        "están muy cercanas entre sí, alrededor de los 7k. Pero no todos se comportan igual.",
        "Algunos países tienen mayor variación en sus datos, lo que indica que su consumo",
        "cambia más con el tiempo, mientras que otros son más estables. Tambien algunos valores",
        "más altos o más bajos de lo normal, lo que puede deberse a cambios en la actividad",
        "industrial o en el uso de energía."
      ), class = "insight-text")
    )
  ),

  # ── Chart 4 ─────────────────────────────────────────────────────────────
  tags$div(class = "chart-card",
    tags$h2("Energía renovable por país"),
    plotlyOutput("plot_renew", height = "400px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p(paste(
        "La gráfica muestra los 10 países con mayor participación de energía renovable.",
        "Se observa que todos los países tienen valores muy cercanos entre sí, entre",
        "aproximadamente el 15.9% y el 16.1%. Mostrando que, aunque estos países lideran el uso",
        "de energías renovables dentro del conjunto de datos, no hay una diferencia muy marcada",
        "entre ellos. ninguno sobresale de forma significativa, sino que todos mantienen niveles",
        "similares de uso. También se puede notar que países como México y Reino Unido aparecen",
        "ligeramente por encima del resto, aunque la diferencia es mínima. Mostrando que el avance",
        "en energías renovables es relativamente equilibrado entre estos países."
      ), class = "insight-text")
    )
  ),

  # ── Chart 5 ─────────────────────────────────────────────────────────────
  tags$div(class = "chart-card",
    tags$h2("Renovable vs CO2"),
    plotlyOutput("plot_renew_co2", height = "400px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p(paste(
        "Esta gráfica muestra como las emisiones se mantienen relativamente estables a lo largo",
        "de los distintos niveles de energía renovable, sin cambios muy bruscos.",
        "no se evidencia una disminución clara de las emisiones a medida que aumenta el uso de",
        "energías renovables. En algunos puntos incluso se presentan ligeros aumentos lo que nos",
        "dice que la relación no es completamente directa. Y en el final de la grafica se observa",
        "una caída más marcada en las emisiones, lo que podría indicar un posible efecto positivo",
        "del aumento en el uso de energías renovables, aunque no es un patrón constante."
      ), class = "insight-text")
    )

  ),

  # Chart 6: Heatmap temperatura
  tags$div(class = "chart-card",
    tags$h2("Temperatura por Pa\u00eds y Mes"),
    plotlyOutput("plot_heatmap", height = "440px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p("El mapa de calor revela patrones estacionales claros. Pa\u00edses como Canada y Norway muestran temperaturas muy bajas en invierno y picos en verano, mientras que Australia presenta el patr\u00f3n inverso por estar en el hemisferio sur. Indonesia e India mantienen temperaturas altas y estables durante todo el a\u00f1o al ser pa\u00edses tropicales.", class = "insight-text")
    )
  ),

  # Chart 7: Scatter consumo vs CO2
  tags$div(class = "chart-card",
    tags$h2("Consumo vs CO2 por Pa\u00eds"),
    plotlyOutput("plot_scatter", height = "420px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p("El gr\u00e1fico muestra la relaci\u00f3n entre consumo energ\u00e9tico promedio y emisiones de CO\u2082 por pa\u00eds. El color refleja el porcentaje de renovables: pa\u00edses con mayor participaci\u00f3n de energ\u00eda limpia tienden a emitir menos CO\u2082 para niveles similares de consumo.", class = "insight-text")
    )
  ),

  # Chart 8: Evolución anual CO2
  tags$div(class = "chart-card",
    tags$h2("Evoluci\u00f3n Anual de CO2"),
    plotlyOutput("plot_co2_trend", height = "380px"),
    tags$div(class = "insight-wrap",
      tags$p("Analisis", class = "insight-title"),
      tags$p("La evoluci\u00f3n anual de emisiones de CO\u2082 muestra las variaciones entre 2020 y 2024. Los cambios a\u00f1o a a\u00f1o reflejan la influencia de la actividad industrial, la adopci\u00f3n de renovables y factores externos. La tendencia permite identificar si los pa\u00edses est\u00e1n avanzando en la reducci\u00f3n de sus emisiones.", class = "insight-text")
    )
  )
)

# ── SERVER ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  filtered <- reactive({
    req(input$sel_countries, input$sel_years)
    df %>% filter(
      country %in% input$sel_countries,
      year    %in% as.integer(input$sel_years)
    )
  })

  # Tema base — minimalista igual al D3
  theme_clima <- function(flip = FALSE) {
    t <- theme_minimal(base_size = 11, base_family = "sans") +
      theme(
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.title  = element_text(size = 10, color = "#aaa"),
        axis.text   = element_text(size = 9,  color = "#888"),
        axis.line   = element_line(color = "#d8d5ce", linewidth = 0.5),
        axis.ticks  = element_line(color = "#d8d5ce"),
        panel.grid.major = element_line(color = GREY_GRID,
                                        linetype = "dashed", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        legend.position  = "none",
        plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        plot.margin   = margin(0, 0, 0, 0)
      )
    if (flip) t <- t + theme(panel.grid.major.y = element_blank())
    t
  }

  hover_style <- list(
    bgcolor     = "#111",
    font        = list(color = "#fff", size = 12, family = "Segoe UI, sans-serif"),
    bordercolor = "transparent"
  )

  # ── CHART 1: Temperatura vs Tiempo ───────────────────────────────────────
  output$plot_temp <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    monthly <- d %>%
      group_by(month) %>%
      summarise(value = mean(avg_temperature, na.rm = TRUE), .groups = "drop") %>%
      arrange(month)

    tip <- paste0(format(monthly$month, "%B %Y"),
                  "<br>Temp: ", round(monthly$value, 1), " \u00b0C")

    plot_ly(monthly, x = ~month) %>%
      # Área rellena (igual D3: fill azul claro alpha 0.12)
      add_trace(
        y          = ~value,
        type       = "scatter", mode = "lines",
        fill       = "tozeroy",
        fillcolor  = "rgba(78,121,167,0.12)",
        line       = list(color = BLUE_MAIN, width = 2.2),
        hoverinfo  = "skip",
        showlegend = FALSE
      ) %>%
      # Puntos encima con tooltip
      add_trace(
        y          = ~value,
        type       = "scatter", mode = "markers",
        marker     = list(color = BLUE_MAIN, size = 6,
                          line = list(color = "#fff", width = 1)),
        text       = tip,
        hoverinfo  = "text",
        showlegend = FALSE
      ) %>%
      add_chart_titles(
        title    = "Variaci\u00f3n de temperatura promedio (2020\u20132024)",
        subtitle = "Tendencia mensual global \u2014 promedio de todos los pa\u00edses",
        question = "\u00bfC\u00f3mo ha cambiado la temperatura a lo largo del tiempo?"
      ) %>%
      layout(
        hovermode  = "x unified",
        hoverlabel = hover_style,
        xaxis = list(
          tickfont     = list(size = 9, color = "#888"),
          tickformat   = "%b %Y",
          showgrid     = TRUE,
          gridcolor    = GREY_GRID,
          gridwidth    = 1,
          griddash     = "dash",
          zeroline     = FALSE,
          showline     = TRUE,
          linecolor    = "#d8d5ce"
        ),
        yaxis = list(
          tickfont   = list(size = 9, color = "#888"),
          ticksuffix = "\u00b0",
          rangemode  = "tozero",
          showgrid   = TRUE,
          gridcolor  = GREY_GRID,
          gridwidth  = 1,
          griddash   = "dash",
          zeroline   = FALSE,
          showline   = TRUE,
          linecolor  = "#d8d5ce",
          title      = list(text = "Temperatura",
                             font = list(size = 10, color = "#aaa"))
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 2: CO₂ por país ────────────────────────────────────────────────
  output$plot_co2 <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    co2 <- d %>%
      group_by(country) %>%
      summarise(val = mean(co2_emission, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(val)) %>%
      mutate(country = factor(country, levels = country))

    n   <- nrow(co2)
    pal <- colorRampPalette(c("#c6dbef", "#084594"))(n)
    co2 <- co2 %>%
      mutate(
        idx      = rank(val, ties.method = "first"),
        fill_col = pal[idx]
      )

    p <- ggplot(co2, aes(x = country, y = val, fill = fill_col,
      text = paste0(country, "<br>CO\u2082 promedio: ", round(val, 1), " ton/d\u00eda"))) +
      geom_col(width = 0.72) +
      geom_text(aes(label = round(val, 0)), vjust = -0.5,
                size = 3.2, color = "#222", fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, .13))) +
      labs(x = NULL, y = "CO\u2082 promedio") +
      theme_clima() +
      theme(
        axis.text.x        = element_text(angle = 22, hjust = 1),
        panel.grid.major.x = element_blank()
      )

    ggplotly(p, tooltip = "text") %>%
      add_chart_titles(
        title    = "Emisiones de CO\u2082 por pa\u00eds",
        subtitle = "Promedio diario de emisiones 2020\u20132024 \u2014 ordenado de mayor a menor",
        question = "Pregunta: \u00bfQu\u00e9 pa\u00edses contaminan m\u00e1s?"
      ) %>%
      layout(hoverlabel = hover_style) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 3: Consumo vs Precio (Boxplot) ─────────────────────────────────
  output$plot_box <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    order_med <- d %>%
      group_by(country) %>%
      summarise(med = median(energy_consumption, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(med))

    d <- d %>% mutate(country = factor(country, levels = order_med$country))

    n   <- nrow(order_med)
    pal <- colorRampPalette(c("#c6dbef", "#084594"))(n)
    fill_map <- setNames(rev(pal), order_med$country)

    p <- ggplot(d, aes(x = country, y = energy_consumption, fill = country)) +
      geom_boxplot(
        outlier.size = 1.0, outlier.alpha = 0.40, outlier.color = "#aaa",
        width = 0.65, color = "#bbb", linewidth = 0.40
      ) +
      scale_fill_manual(values = fill_map) +
      scale_y_continuous(labels = function(x) paste0(round(x / 1000, 1), "k"),
                         limits = c(0, NA)) +
      labs(x = NULL, y = "Consumo energ\u00e9tico") +
      theme_clima() +
      theme(
        axis.text.x        = element_text(angle = 22, hjust = 1),
        panel.grid.major.x = element_blank()
      )

    # Etiquetas de mediana dentro de la caja (como D3)
    p <- p +
      geom_text(data = order_med,
                aes(x = country, y = med,
                    label = paste0(round(med / 1000, 1), "k")),
                size = 2.7, color = "#fff", fontface = "bold",
                vjust = 0.4, inherit.aes = FALSE)

    ggplotly(p) %>%
      add_chart_titles(
        title    = "Distribuci\u00f3n del consumo energ\u00e9tico por pa\u00eds",
        subtitle = "Mediana, rango intercuart\u00edlico y valores extremos \u2014 ordenado por consumo mediano",
        question = "\u00bfC\u00f3mo var\u00eda el consumo energ\u00e9tico entre pa\u00edses?"
      ) %>%
      layout(hoverlabel = hover_style) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 4: Energía renovable por país ──────────────────────────────────
  output$plot_renew <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    renew <- d %>%
      group_by(country) %>%
      summarise(val = mean(renewable_share, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(val)) %>%
      slice_head(n = 10) %>%
      mutate(country = factor(country, levels = rev(country)))

    max_x <- max(renew$val) * 1.15

    p <- ggplot(renew) +
      geom_col(aes(x = max_x, y = country),
               fill = "#f2f2f2", width = 0.50) +
      geom_col(aes(x = val, y = country,
                   text = paste0(country,
                                 "<br>Renovable: ", round(val, 1), "%")),
               fill = GREEN_MAIN, alpha = 0.90, width = 0.50) +
      geom_text(aes(x = max_x * 1.01, y = country,
                    label = paste0(round(val, 1), "%")),
                hjust = 0, size = 2.9,
                color = "#444", fontface = "bold") +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, .18))
      ) +
      labs(x = "Participaci\u00f3n renovable (%)", y = NULL) +
      theme_clima(flip = TRUE) +
      theme(panel.grid.major.x = element_line(color = GREY_GRID,
                                               linetype = "dashed",
                                               linewidth = 0.4))

    ggplotly(p, tooltip = "text") %>%
      add_chart_titles(
        title    = "Participaci\u00f3n de energ\u00edas renovables por pa\u00eds",
        subtitle = "Porcentaje promedio de energ\u00eda renovable sobre el total (2020\u20132024)",
        question = "\u00bfCuales son los 10 pa\u00edses que utilizan m\u00e1s energ\u00eda renovable?"
      ) %>%
      layout(hoverlabel = hover_style) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 5 (= chart6 D3): Renovable vs CO₂ ─────────────────────────────
  output$plot_renew_co2 <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    d2 <- d %>%
      mutate(bin = cut(renewable_share, breaks = 12)) %>%
      group_by(bin) %>%
      summarise(
        x = mean(renewable_share, na.rm = TRUE),
        y = mean(co2_emission,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(x), !is.na(y)) %>%
      arrange(x)

    tip <- paste0("Renovable: ", round(d2$x, 1),
                  "%<br>CO\u2082: ", round(d2$y, 2))

    plot_ly(d2, x = ~x) %>%
      # Área rellena verde (igual D3: #2a9d8f alpha 0.10)
      add_trace(
        y          = ~y,
        type       = "scatter", mode = "lines",
        fill       = "tozeroy",
        fillcolor  = "rgba(42,157,143,0.10)",
        line       = list(color = GREEN_DARK, width = 2.4),
        hoverinfo  = "skip",
        showlegend = FALSE
      ) %>%
      # Puntos encima con tooltip
      add_trace(
        y          = ~y,
        type       = "scatter", mode = "markers",
        marker     = list(color = GREEN_DARK, size = 8, opacity = 0.85),
        text       = tip,
        hoverinfo  = "text",
        showlegend = FALSE
      ) %>%
      add_chart_titles(
        title    = "Relaci\u00f3n entre energ\u00eda renovable y emisiones de CO\u2082",
        subtitle = "Promedios agrupados \u2014 tendencia general",
        question = "Pregunta : \u00bfM\u00e1s energ\u00eda renovable reduce las emisiones?"
      ) %>%
      layout(
        hoverlabel = hover_style,
        xaxis = list(
          tickfont   = list(size = 9, color = "#888"),
          ticksuffix = "%",
          range      = list(5, 35),
          tickvals   = list(5, 10, 15, 20, 25, 30, 35),
          showgrid   = TRUE,
          gridcolor  = GREY_GRID,
          gridwidth  = 1,
          griddash   = "dash",
          zeroline   = FALSE,
          showline   = TRUE,
          linecolor  = "#d8d5ce",
          title      = list(text = "Energ\u00eda renovable (%)",
                             font = list(size = 10, color = "#aaa"))
        ),
        yaxis = list(
          tickfont  = list(size = 9, color = "#888"),
          range     = list(400, 460),
          showgrid  = TRUE,
          gridcolor = GREY_GRID,
          gridwidth = 1,
          griddash  = "dash",
          zeroline  = FALSE,
          showline  = TRUE,
          linecolor = "#d8d5ce",
          title     = list(text = "Emisiones de CO\u2082",
                            font = list(size = 10, color = "#aaa"))
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 6: Heatmap temperatura por país y mes ───────────────────────────
  output$plot_heatmap <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    heat <- d %>%
      mutate(mes = format(date, "%b"), mes_num = month(date)) %>%
      group_by(country, mes, mes_num) %>%
      summarise(temp = mean(avg_temperature, na.rm = TRUE), .groups = "drop") %>%
      arrange(mes_num) %>%
      mutate(mes = factor(mes, levels = unique(mes)))

    plot_ly(heat,
            x = ~mes, y = ~country, z = ~round(temp, 1),
            type = "heatmap",
            colorscale = list(
              list(0,   "#fff5eb"),
              list(0.2, "#fdd0a2"),
              list(0.4, "#fdae6b"),
              list(0.6, "#f16913"),
              list(0.8, "#d94801"),
              list(1,   "#7f2704")
            ),
            text = ~paste0(country, " — ", mes, "<br>Temp: ", round(temp, 1), " °C"),
            hoverinfo = "text",
            showscale = TRUE,
            colorbar = list(
              title = list(text = "°C", font = list(size = 11, color = "#888")),
              tickfont = list(size = 9, color = "#888"),
              thickness = 12, len = 0.6
            )
    ) %>%
      add_chart_titles(
        title    = "Temperatura promedio por pa\u00eds y mes",
        subtitle = "Promedio hist\u00f3rico 2020\u20132024 \u2014 paleta naranja-rojo (mayor temperatura = m\u00e1s oscuro)",
        question = "\u00bfC\u00f3mo var\u00eda la temperatura seg\u00fan el pa\u00eds y la \u00e9poca del a\u00f1o?"
      ) %>%
      layout(
        hoverlabel = hover_style,
        xaxis = list(title = list(text = "Mes", font = list(size = 10, color = "#aaa")),
                     tickfont = list(size = 9, color = "#888")),
        yaxis = list(title = "", tickfont = list(size = 9, color = "#888")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 7: Scatter consumo vs CO2 por país ──────────────────────────────
  output$plot_scatter <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    by_country <- d %>%
      group_by(country) %>%
      summarise(
        consumo  = mean(energy_consumption, na.rm = TRUE),
        co2      = mean(co2_emission,       na.rm = TRUE),
        renew    = mean(renewable_share,    na.rm = TRUE),
        .groups  = "drop"
      )

    plot_ly(by_country,
            x    = ~consumo,
            y    = ~co2,
            text = ~paste0(country,
                           "<br>Consumo: ", round(consumo), " MWh",
                           "<br>CO\u2082: ", round(co2, 1), " ton",
                           "<br>Renovable: ", round(renew, 1), "%"),
            type      = "scatter",
            mode      = "markers+text",
            textposition = "top center",
            textfont  = list(size = 9, color = "#555"),
            marker    = list(
              size    = ~renew * 2.8,
              color   = ~renew,
              colorscale = list(
                list(0,   "#7b2d8b"),
                list(0.3, "#c06ec0"),
                list(0.6, "#f4a8d4"),
                list(1,   "#fde8f5")
              ),
              reversescale = TRUE,
              showscale = TRUE,
              colorbar  = list(
                title = list(text = "% Renov.", font = list(size = 10, color = "#888")),
                tickfont = list(size = 9, color = "#888"),
                thickness = 12, len = 0.6
              ),
              line = list(color = "#fff", width = 1.2)
            ),
            hoverinfo = "text",
            showlegend = FALSE
    ) %>%
      add_chart_titles(
        title    = "Consumo energ\u00e9tico vs Emisiones de CO\u2082 por pa\u00eds",
        subtitle = "Cada punto es un pa\u00eds — tama\u00f1o y color: % de energ\u00eda renovable",
        question = "\u00bfLos pa\u00edses con m\u00e1s renovables contaminan menos?"
      ) %>%
      layout(
        hoverlabel = hover_style,
        xaxis = list(
          title    = list(text = "Consumo energ\u00e9tico promedio (MWh)",
                          font = list(size = 10, color = "#aaa")),
          tickfont = list(size = 9, color = "#888"),
          showgrid = TRUE, gridcolor = GREY_GRID, griddash = "dash",
          zeroline = FALSE, showline = TRUE, linecolor = "#d8d5ce"
        ),
        yaxis = list(
          title    = list(text = "Emisiones CO\u2082 promedio (ton/d\u00eda)",
                          font = list(size = 10, color = "#aaa")),
          tickfont = list(size = 9, color = "#888"),
          showgrid = TRUE, gridcolor = GREY_GRID, griddash = "dash",
          zeroline = FALSE, showline = TRUE, linecolor = "#d8d5ce"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── CHART 8: Evolución anual CO2 ─────────────────────────────────────────
  output$plot_co2_trend <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)

    anual <- d %>%
      group_by(year) %>%
      summarise(co2 = mean(co2_emission, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)

    tip <- paste0("A\u00f1o: ", anual$year, "<br>CO\u2082 promedio: ", round(anual$co2, 1), " ton/d\u00eda")

    plot_ly(anual, x = ~year) %>%
      add_trace(
        y         = ~co2,
        type      = "scatter", mode = "lines",
        fill      = "tozeroy",
        fillcolor = "rgba(99,71,153,0.10)",
        line      = list(color = "#6347a0", width = 2.5),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      add_trace(
        y         = ~co2,
        type      = "scatter", mode = "markers+text",
        marker    = list(color = "#6347a0", size = 10,
                         line = list(color = "#fff", width = 2)),
        text      = ~round(co2, 1),
        textposition = "top center",
        textfont  = list(size = 10, color = "#6347a0", family = "Segoe UI, sans-serif"),
        hovertext = tip,
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      add_chart_titles(
        title    = "Evoluci\u00f3n anual de emisiones de CO\u2082 (2020\u20132024)",
        subtitle = "Promedio global por a\u00f1o \u2014 todos los pa\u00edses seleccionados",
        question = "\u00bfLas emisiones han aumentado o disminuido con los a\u00f1os?"
      ) %>%
      layout(
        hoverlabel = hover_style,
        xaxis = list(
          tickfont  = list(size = 10, color = "#888"),
          tickvals  = as.list(anual$year),
          ticktext  = as.list(as.character(anual$year)),
          showgrid  = TRUE, gridcolor = GREY_GRID, griddash = "dash",
          zeroline  = FALSE, showline = TRUE, linecolor = "#d8d5ce",
          title     = list(text = "A\u00f1o", font = list(size = 10, color = "#aaa"))
        ),
        yaxis = list(
          tickfont  = list(size = 9, color = "#888"),
          rangemode = "tozero",
          showgrid  = TRUE, gridcolor = GREY_GRID, griddash = "dash",
          zeroline  = FALSE, showline = TRUE, linecolor = "#d8d5ce",
          title     = list(text = "CO\u2082 promedio (ton/d\u00eda)",
                            font = list(size = 10, color = "#aaa"))
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
