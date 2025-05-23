library(tidyverse)

# Crear datos largos con error estándar
datos <- tibble::tibble(
  país   = rep(c("México", "OCDE"), each = 18),
  año    = rep(rep(2018:2023, each = 3), times = 2),
  nivel  = rep(c("Below upper secondary education", "Upper secondary or post-secondary non-tertiary education", "Tertiary education"), times = 6 * 2),
  valor  = c(
    # México
    60.85, 21.17, 17.98, 60.21, 21.53, 18.26, 58.40, 22.16, 19.43,
    57.19, 22.35, 20.46, 56.22, 23.15, 20.64, 55.56, 23.48, 20.96,
    # OCDE
    22.02, 41.51, 36.89, 21.42, 41.07, 37.92, 19.81, 41.40, 39.21,
    19.91, 40.53, 39.99, 19.52, 40.22, 40.68, 19.23, 40.23, 40.95
  ),
  error_std = c(
    # México
    0.11, 0.09, 0.09, 0.11, 0.09, 0.09, 0.12, 0.10, 0.09,
    0.11, 0.09, 0.09, 0.11, 0.09, 0.09, 0.11, 0.09, 0.09,
    # OCDE: sin datos de error estándar
    rep(NA, 18)
  )
)

tema_grafico <- theme_classic(base_size = 13) +
  theme(legend.position = "top")

etiquetas <- geom_text(
  aes(label = sprintf("%.2f", valor)),
  position = position_dodge(width = 0.8),
  vjust = -0.5, size = 3.5
)


datos |> 
  filter(país == "México") |>
  ggplot(aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9),
           color="black", size=1) +
  etiquetas +
  labs(
    title = "Distribución educativa en México con error estándar (2018–2023)",
    x = "Año", y = "Porcentaje", fill = "Nivel educativo"
  ) +
  tema_grafico





