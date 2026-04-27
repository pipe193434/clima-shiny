# Clima & Energía Global 2020–2024 — Shiny App (R)

## Descripción

Aplicación web interactiva desarrollada con **Shiny (R)** que visualiza datos climáticos y energéticos globales del período 2020–2024. Hace parte del **Proyecto 2** del curso *Herramientas y Visualización de Datos* (Fundación Universitaria Los Libertadores).

La app replica y extiende las visualizaciones del proyecto D3.js, aprovechando las capacidades estadísticas de R y la interactividad de `plotly`.

---

## Dataset

| Campo | Detalle |
|---|---|
| **Fuente** | Kaggle |
| **URL** | [global_climate_energy_2020_2024.csv](https://www.kaggle.com/datasets) |
| **Descripción** | Registros diarios por país con variables climáticas y energéticas |
| **Filas** | 36 540 |
| **Columnas** | 10 |
| **Período** | 2020-01-01 → 2024-12-31 |
| **Países** | 20 (Australia, Brazil, Canada, China, France, Germany, India, Indonesia, Italy, Japan, Mexico, Netherlands, Norway, Poland, South Africa, Spain, Sweden, Turkey, United Kingdom, United States) |

### Variables del dataset

| Variable | Tipo | Descripción |
|---|---|---|
| `date` | Fecha | Fecha de la observación |
| `country` | Categórica | País |
| `avg_temperature` | Numérica | Temperatura promedio diaria (°C) |
| `co2_emission` | Numérica | Emisiones de CO₂ (ton/día) |
| `energy_consumption` | Numérica | Consumo energético (MWh) |
| `renewable_share` | Numérica | Porcentaje de energía renovable (%) |
| `industrial_activity_index` | Numérica | Índice de actividad industrial |
| `energy_price` | Numérica | Precio de la energía |
| `humidity` | Numérica | Humedad relativa (%) |
| `urban_population` | Numérica | Población urbana |

---

## Hallazgos Principales

1. **Estacionalidad clara de temperatura**: La temperatura global promedio oscila de forma cíclica y predecible cada año, con picos estivales y valles invernales bien marcados durante 2020–2024.

2. **Emisiones CO₂ similares entre países**: La mayoría de los países del dataset presenta promedios de emisión muy cercanos. Australia lidera levemente; Turquía registra el valor más bajo, pero la brecha es pequeña.

3. **Consumo energético estable con variabilidad moderada**: Las medianas de consumo por país rondan los 7 000 MWh. Algunos países muestran colas largas (outliers), relacionadas con picos de actividad industrial.

4. **Participación renovable homogénea (~16 %)**: Los líderes en energías limpias como México y Reino Unido apenas superan a los demás; el avance es equilibrado sin un líder destacado.

5. **Relación no lineal entre renovable y CO₂**: Mayor proporción de renovables no implica directamente menores emisiones en la mayoría del rango, aunque al superar ~28 % se observa una caída más notoria, sugiriendo un umbral de impacto.

6. **Consumo energético ≠ emisiones CO₂**: El scatter plot con regresión lineal confirma que la correlación existe pero es débil; el mix de fuentes energéticas es el factor modulador clave.

---

## Visualizaciones Implementadas

| # | Tipo | Descripción |
|---|---|---|
| 1 | Serie temporal | Evolución mensual de temperatura promedio global |
| 2 | Gráfico de barras | Emisiones promedio de CO₂ por país (paleta secuencial) |
| 3 | Boxplot | Distribución del consumo energético por país |
| 4 | Barras horizontales | Top N países en participación de energía renovable |
| 5 | Línea (bins) | Tendencia de CO₂ según nivel de energía renovable |
| 6 | Scatter + regresión | Relación consumo energético vs emisiones CO₂ por país |

---

## Tecnologías Utilizadas

- **Framework**: Shiny + shinydashboard
- **Lenguaje**: R 4.x
- **Visualización**: ggplot2 + plotly (interactividad)
- **Manipulación de datos**: dplyr, lubridate, tidyr
- **Escala / formatos**: scales

---

## Instalación y Ejecución Local

### Requisitos previos

- R ≥ 4.1
- RStudio (recomendado) o R base

### 1. Clonar el repositorio

```bash
git clone https://github.com/USUARIO/clima-shiny.git
cd clima-shiny
```

### 2. Instalar dependencias

En la consola de R:

```r
install.packages(c(
  "shiny", "shinydashboard", "ggplot2",
  "dplyr", "lubridate", "plotly",
  "scales", "tidyr"
))
```

### 3. Ejecutar la app

```r
shiny::runApp(".")
```

O desde RStudio: abrir `app.R` → botón **Run App**.

---

## Despliegue

**URL en producción**: [https://USUARIO.shinyapps.io/clima-shiny](https://USUARIO.shinyapps.io/clima-shiny)

Para desplegar en [shinyapps.io](https://www.shinyapps.io/) (free tier):

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="USUARIO", token="TOKEN", secret="SECRET")
rsconnect::deployApp(".")
```

---

## Estructura del Proyecto

```
clima-shiny/
├── app.R                              # Aplicación Shiny (UI + Server)
├── data/
│   └── global_climate_energy_2020_2024.csv
├── README.md
└── packages.R                         # Script de instalación de paquetes
```

---

## Autores

- Nombre Apellido 1  
- Nombre Apellido 2
