library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(scales)

df_raw <- read.csv("data/global_climate_energy_2020_2024.csv", stringsAsFactors = FALSE)

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
  filter(!is.na(date), !is.na(country), !is.na(avg_temperature),
         !is.na(co2_emission), !is.na(energy_consumption), !is.na(renewable_share))

all_countries <- sort(unique(df$country))
all_years     <- sort(unique(df$year))

BLUE_MAIN  <- "#1f3b73"
BLUE_LIGHT <- "#4e79a7"
GREEN_MAIN <- "#2a9d8f"
GREEN_DARK <- "#1b7f6b"
GREY_GRID  <- "#ebebeb"

add_chart_titles <- function(p, title, subtitle, question, margin_top = 120) {
  p %>% layout(
    margin = list(t = margin_top, l = 65, r = 30, b = 60),
    annotations = list(
      list(text = title, xref = "paper", yref = "paper", x = 0, y = 1,
           xanchor = "left", yanchor = "bottom", showarrow = FALSE,
           font = list(size = 15, color = "#111", family = "Segoe UI, sans-serif"), yshift = 72),
      list(text = subtitle, xref = "paper", yref = "paper", x = 0, y = 1,
           xanchor = "left", yanchor = "bottom", showarrow = FALSE,
           font = list(size = 11, color = "#999", family = "Segoe UI, sans-serif"), yshift = 52),
      list(text = question, xref = "paper", yref = "paper", x = 0, y = 1,
           xanchor = "left", yanchor = "bottom", showarrow = FALSE,
           font = list(size = 11, color = "#555", family = "Segoe UI, sans-serif"), yshift = 32)
    )
  )
}

ui <- fluidPage(
  title = "Dashboard Climático",
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', system-ui, sans-serif; background: #f4f4f0;
           color: #1a1a1a; margin: 0; padding: 40px 40px 80px; }
    .container-fluid { max-width: 1000px; margin: 0 auto; padding: 0; }
    h1.page-title { font-size: 24px; font-weight: 600; color: #111;
                    letter-spacing: -0.3px; margin-bottom: 4px; }
    hr.page-divider { border: none; border-top: 1.5px solid #dddbd3; margin-bottom: 36px; }
    .chart-card { background: #ffffff; border: 1px solid #e2dfd8; border-radius: 10px;
                  padding: 22px 28px 22px; margin-bottom: 30px; transition: box-shadow 0.3s; }
    .chart-card:hover { box-shadow: 0 8px 24px rgba(0,0,0,.08); }
    .chart-card h2 { font-size: 16px; font-weight: 600; color: #111;
                     letter-spacing: -0.2px; margin: 0 0 14px 0; }
    .insight-wrap { margin-top: 4px; }
    .insight-wrap .insight-title { font-size: 15px; font-weight: 700; color: #111; margin: 0 0 4px 0; }
    .insight-wrap .insight-text { font-size: 12px; color: #555; line-height: 1.65; margin: 0; }
  "))),
  
  tags$h1("Clima", class = "page-title"),
  tags$hr(class = "page-divider"),
  
  tags$div(style = "display:none;",
           selectInput("sel_countries", NULL, choices = all_countries, selected = all_countries, multiple = TRUE),
           selectInput("sel_years", NULL, choices = all_years, selected = all_years, multiple = TRUE)
  ),
  
  tags$div(class = "chart-card",
           tags$h2("Temperatura vs Tiempo"),
           plotlyOutput("plot_temp", height = "400px"),
           tags$div(class = "insight-wrap",
                    tags$p("Analisis", class = "insight-title"),
                    tags$p("Esta gráfica muestra cómo cambia la temperatura promedio a lo largo del tiempo. Se puede observar un patrón muy claro y repetitivo, donde la temperatura sube y baja de forma constante cada año.Los picos representan los momentos de mayor temperatura, mientras que los puntos más bajos corresponden a las épocas más frías. Este comportamiento se repite de manera similar en todos los años, lo que indica que hay una tendencia estacional bastante marcada", class = "insight-text")
           )
  ),
  
  tags$div(class = "chart-card",
           tags$h2("CO2 por país"),
           plotlyOutput("plot_co2", height = "400px"),
           tags$div(class = "insight-wrap",
                    tags$p("Analisis", class = "insight-title"),
                    tags$p("Las emisiones de CO₂ entre los países no son muy diferentes entre sí, ya que la mayoría se mantiene en un rango bastante parecido. Australia es el país con el valor más alto, mientras que Turquía tiene el más bajo. Aunque hay un orden de mayor a menor,la diferencia entre los países no es muy grande,lo que indica que todos tienen niveles de emisiones bastante similares dentro del conjunto de datos.", class = "insight-text")
           )
  ),
  
  tags$div(class = "chart-card",
           tags$h2("Consumo vs Precio"),
           plotlyOutput("plot_box", height = "400px"),
           tags$div(class = "insight-wrap",
                    tags$p("Analisis", class = "insight-title"),
                    tags$p("Esta gráfica muestra cómo se distribuye el consumo energético en diferentes países. Se puede ver que la mayoría tienen valores bastante parecidos, ya que las medianas están muy cercanas entre sí, alrededor de los 7k. Pero no todos se comportan igual. Algunos países tienen mayor variación en sus datos, lo que indica que su consumo cambia más con el tiempo, mientras que otros son más estables. Tambien algunos valores más altos o más bajos de lo normal, lo que puede deberse a cambios en la actividad industrial o en el uso de energía.", class = "insight-text")
           )
  ),
  
  tags$div(class = "chart-card",
           tags$h2("Energía renovable por país"),
           plotlyOutput("plot_renew", height = "400px"),
           tags$div(class = "insight-wrap",
                    tags$p("Analisis", class = "insight-title"),
                    tags$p("La gráfica muestra los 10 países con mayor participación de energía renovable. Se observa que todos los países tienen valores muy cercanos entre sí, entre aproximadamente el 15.9% y el 16.1%. Mostrando que, aunque estos países lideran el uso de energías renovables dentro del conjunto de datos, no hay una diferencia muy marcada entre ellos. ninguno sobresale de forma significativa, sino que todos mantienen niveles similares de uso. También se puede notar que países como México y Reino Unido aparecen ligeramente por encima del resto, aunque la diferencia es mínima. Mostrando que el avance en energías renovables es relativamente equilibrado entre estos países.", class = "insight-text")
           )
  ),
  
  tags$div(class = "chart-card",
           tags$h2("Renovable vs CO2"),
           plotlyOutput("plot_renew_co2", height = "400px"),
           tags$div(class = "insight-wrap",
                    tags$p("Analisis", class = "insight-title"),
                    tags$p("Esta gráfica muestra como las emisiones se mantienen relativamente estables a lo largo de los distintos niveles de energía renovable, sin cambios muy bruscos. no se evidencia una disminución clara de las emisiones a medida que aumenta el uso de energías renovables. En algunos puntos incluso se presentan ligeros aumentos lo que nos dice que la relación no es completamente directa. Y en el final de la grafica se observa una caída más marcada en las emisiones, lo que podría indicar un posible efecto positivo del aumento en el uso de energías renovables, aunque no es un patrón constante.", class = "insight-text")
           )
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    req(input$sel_countries, input$sel_years)
    df %>% filter(country %in% input$sel_countries, year %in% as.integer(input$sel_years))
  })
  
  theme_clima <- function(flip = FALSE) {
    t <- theme_minimal(base_size = 11, base_family = "sans") +
      theme(
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.title  = element_text(size = 10, color = "#aaa"),
        axis.text   = element_text(size = 9, color = "#888"),
        axis.line   = element_line(color = "#d8d5ce", linewidth = 0.5),
        axis.ticks  = element_line(color = "#d8d5ce"),
        panel.grid.major = element_line(color = GREY_GRID, linetype = "dashed", linewidth = 0.4),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        legend.position = "none", plot.title = element_blank(),
        plot.subtitle = element_blank(), plot.margin = margin(0,0,0,0)
      )
    if (flip) t <- t + theme(panel.grid.major.y = element_blank())
    t
  }
  
  hover_style <- list(bgcolor = "#111", font = list(color = "#fff", size = 12,
                                                    family = "Segoe UI, sans-serif"), bordercolor = "transparent")
  
  output$plot_temp <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    monthly <- d %>% group_by(month) %>%
      summarise(value = mean(avg_temperature, na.rm = TRUE), .groups = "drop") %>% arrange(month)
    tip <- paste0(format(monthly$month, "%B %Y"), "<br>Temp: ", round(monthly$value, 1), " °C")
    plot_ly(monthly, x = ~month) %>%
      add_trace(y = ~value, type = "scatter", mode = "lines", fill = "tozeroy",
                fillcolor = "rgba(78,121,167,0.12)", line = list(color = BLUE_MAIN, width = 2.2),
                hoverinfo = "skip", showlegend = FALSE) %>%
      add_trace(y = ~value, type = "scatter", mode = "markers",
                marker = list(color = BLUE_MAIN, size = 6, line = list(color = "#fff", width = 1)),
                text = tip, hoverinfo = "text", showlegend = FALSE) %>%
      add_chart_titles("Variación de temperatura promedio (2020–2024)",
                       "Tendencia mensual global — promedio de todos los países",
                       "¿Cómo ha cambiado la temperatura a lo largo del tiempo?") %>%
      layout(hovermode = "x unified", hoverlabel = hover_style,
             xaxis = list(tickfont = list(size=9,color="#888"), tickformat="%b %Y",
                          showgrid=TRUE, gridcolor=GREY_GRID, griddash="dash", zeroline=FALSE,
                          showline=TRUE, linecolor="#d8d5ce"),
             yaxis = list(tickfont=list(size=9,color="#888"), ticksuffix="°", rangemode="tozero",
                          showgrid=TRUE, gridcolor=GREY_GRID, griddash="dash", zeroline=FALSE,
                          showline=TRUE, linecolor="#d8d5ce",
                          title=list(text="Temperatura", font=list(size=10,color="#aaa"))),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)") %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_co2 <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    co2 <- d %>% group_by(country) %>%
      summarise(val = mean(co2_emission, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(val)) %>% mutate(country = factor(country, levels = country))
    n <- nrow(co2); pal <- colorRampPalette(c("#c6dbef","#084594"))(n)
    co2 <- co2 %>% mutate(idx = rank(val, ties.method="first"), fill_col = pal[idx])
    p <- ggplot(co2, aes(x=country, y=val, fill=fill_col,
                         text=paste0(country,"<br>CO₂ promedio: ",round(val,1)," ton/día"))) +
      geom_col(width=0.72) +
      geom_text(aes(label=round(val,0)), vjust=-0.45, size=2.8, color="#555", fontface="bold") +
      scale_fill_identity() + scale_y_continuous(expand=expansion(mult=c(0,.13))) +
      labs(x=NULL, y="CO₂ promedio") + theme_clima() +
      theme(axis.text.x=element_text(angle=22,hjust=1), panel.grid.major.x=element_blank())
    ggplotly(p, tooltip="text") %>%
      add_chart_titles("Emisiones de CO₂ por país",
                       "Promedio diario de emisiones 2020–2024 — ordenado de mayor a menor",
                       "Pregunta: ¿Qué países contaminan más?") %>%
      layout(hoverlabel=hover_style) %>% config(displayModeBar=FALSE)
  })
  
  output$plot_box <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    order_med <- d %>% group_by(country) %>%
      summarise(med=median(energy_consumption,na.rm=TRUE), .groups="drop") %>% arrange(desc(med))
    d <- d %>% mutate(country=factor(country, levels=order_med$country))
    n <- nrow(order_med); pal <- colorRampPalette(c("#c6dbef","#084594"))(n)
    fill_map <- setNames(rev(pal), order_med$country)
    p <- ggplot(d, aes(x=country, y=energy_consumption, fill=country)) +
      geom_boxplot(outlier.size=1.0, outlier.alpha=0.40, outlier.color="#aaa",
                   width=0.65, color="#bbb", linewidth=0.40) +
      scale_fill_manual(values=fill_map) +
      scale_y_continuous(labels=function(x) paste0(round(x/1000,1),"k"), limits=c(0,NA)) +
      labs(x=NULL, y="Consumo energético") + theme_clima() +
      theme(axis.text.x=element_text(angle=22,hjust=1), panel.grid.major.x=element_blank())
    fig <- ggplotly(p) %>%
      add_chart_titles("Distribución del consumo energético por país",
                       "Mediana, rango intercuartílico y valores extremos — ordenado por consumo mediano",
                       "¿Cómo varía el consumo energético entre países?") %>%
      layout(hoverlabel=hover_style) %>% config(displayModeBar=FALSE)
    med_labels <- order_med %>% mutate(label=paste0(round(med/1000,1),"k"))
    for (i in seq_len(nrow(med_labels))) {
      fig <- fig %>% add_annotations(x=med_labels$country[i], y=med_labels$med[i],
                                     text=med_labels$label[i], xref="x", yref="y", showarrow=FALSE,
                                     font=list(size=9, color="#3a3939", family="Segoe UI, sans-serif"), yshift=8)
    }
    fig
  })
  
  output$plot_renew <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    renew <- d %>% group_by(country) %>%
      summarise(val=mean(renewable_share,na.rm=TRUE), .groups="drop") %>%
      arrange(desc(val)) %>% slice_head(n=10) %>%
      mutate(country=factor(country, levels=rev(country)))
    max_x <- max(renew$val) * 1.15
    p <- ggplot(renew) +
      geom_col(aes(x=max_x, y=country), fill="#f2f2f2", width=0.50) +
      geom_col(aes(x=val, y=country, text=paste0(country,"<br>Renovable: ",round(val,1),"%")),
               fill=GREEN_MAIN, alpha=0.90, width=0.50) +
      geom_text(aes(x=max_x*1.01, y=country, label=paste0(round(val,1),"%")),
                hjust=0, size=2.9, color="#444", fontface="bold") +
      scale_x_continuous(labels=function(x) paste0(x,"%"), expand=expansion(mult=c(0,.18))) +
      labs(x="Participación renovable (%)", y=NULL) + theme_clima(flip=TRUE) +
      theme(panel.grid.major.x=element_line(color=GREY_GRID,linetype="dashed",linewidth=0.4))
    ggplotly(p, tooltip="text") %>%
      add_chart_titles("Participación de energías renovables por país",
                       "Porcentaje promedio de energía renovable sobre el total (2020–2024)",
                       "¿Cuales son los 10 países que utilizan más energía renovable?") %>%
      layout(hoverlabel=hover_style) %>% config(displayModeBar=FALSE)
  })
  
  output$plot_renew_co2 <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    d2 <- d %>% mutate(bin=cut(renewable_share,breaks=12)) %>% group_by(bin) %>%
      summarise(x=mean(renewable_share,na.rm=TRUE), y=mean(co2_emission,na.rm=TRUE), .groups="drop") %>%
      filter(!is.na(x),!is.na(y)) %>% arrange(x)
    tip <- paste0("Renovable: ",round(d2$x,1),"%<br>CO₂: ",round(d2$y,2))
    plot_ly(d2, x=~x) %>%
      add_trace(y=~y, type="scatter", mode="lines", fill="tozeroy",
                fillcolor="rgba(42,157,143,0.10)", line=list(color=GREEN_DARK,width=2.4),
                hoverinfo="skip", showlegend=FALSE) %>%
      add_trace(y=~y, type="scatter", mode="markers",
                marker=list(color=GREEN_DARK,size=8,opacity=0.85),
                text=tip, hoverinfo="text", showlegend=FALSE) %>%
      add_chart_titles("Relación entre energía renovable y emisiones de CO₂",
                       "Promedios agrupados — tendencia general",
                       "Pregunta : ¿Más energía renovable reduce las emisiones?") %>%
      layout(hoverlabel=hover_style,
             xaxis=list(tickfont=list(size=9,color="#888"), ticksuffix="%", range=list(5,35),
                        tickvals=list(5,10,15,20,25,30,35), showgrid=TRUE, gridcolor=GREY_GRID,
                        griddash="dash", zeroline=FALSE, showline=TRUE, linecolor="#d8d5ce",
                        title=list(text="Energía renovable (%)",font=list(size=10,color="#aaa"))),
             yaxis=list(tickfont=list(size=9,color="#888"), range=list(400,460),
                        showgrid=TRUE, gridcolor=GREY_GRID, griddash="dash", zeroline=FALSE,
                        showline=TRUE, linecolor="#d8d5ce",
                        title=list(text="Emisiones de CO₂",font=list(size=10,color="#aaa"))),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)") %>%
      config(displayModeBar=FALSE)
  })
}

shinyApp(ui, server)