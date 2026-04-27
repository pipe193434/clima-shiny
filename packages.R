# packages.R
# Instala todas las dependencias necesarias para la Shiny app
# Ejecutar UNA VEZ antes de correr app.R

pkgs <- c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "lubridate",
  "plotly",
  "scales",
  "tidyr"
)

nuevos <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(nuevos)) install.packages(nuevos)

cat("✓ Todos los paquetes instalados.\n")
