library(tidyverse)
 
# Crear datos largos con error estándar
datos <- tibble::tibble(
  país = rep(c("México", "OCDE"), each = 3 * 19),
  año = rep(rep(2005:2023, each = 3), times = 2),
  nivel = rep(c(
    "Below upper secondary education",
    "Upper secondary or post-secondary non-tertiary education",
    "Tertiary education"
  ), times = 19 * 2),
  valor = c(
    # México
    71.8, 15.4, 12.7,
    71.0, 15.8, 13.3,
    70.4, 16.0, 13.6,
    70.1, 16.2, 13.7,
    68.7, 17.0, 14.3,
    67.9, 17.4, 14.7,
    66.9, 18.1, 15.0,
    66.0, 18.3, 15.7,
    65.2, 18.6, 16.2,
    64.9, 19.1, 16.0,
    64.3, 19.4, 16.3,
    63.4, 19.8, 16.8,
    62.3, 20.2, 17.4,
    60.9, 21.2, 18.0,
    60.2, 21.5, 18.3,
    58.4, 22.2, 19.4,
    57.2, 22.4, 20.5,
    56.2, 23.1, 20.6,
    55.6, 23.5, 21.0,
    # OCDE
    30.0, 44.2, 26.3,
    29.3, 44.7, 26.5,
    28.5, 44.2, 27.6,
    27.7, 44.1, 28.6,
    27.1, 43.9, 29.3,
    27.0, 43.5, 29.9,
    26.8, 43.3, 30.4,
    25.5, 43.2, 31.8,
    25.0, 43.1, 32.3,
    24.3, 42.4, 33.6,
    24.2, 42.2, 34.0,
    23.0, 42.2, 35.3,
    22.8, 41.8, 35.8,
    22.0, 41.5, 36.9,
    21.4, 41.1, 37.9,
    19.8, 41.4, 39.2,
    19.9, 40.5, 40.0,
    19.5, 40.2, 40.7,
    19.2, 40.2, 41.0
  )
)


datos <- datos |>
  mutate(
    nivel = factor(
      nivel,
      levels = c(
        "Tertiary education",
        "Upper secondary or post-secondary non-tertiary education",
        "Below upper secondary education"
      )
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



datos_pos <- datos |>
  group_by(país, año) |>
  arrange(país, año, desc(nivel)) |>
  mutate(
    pos = cumsum(valor) - valor / 2,
    etiqueta = sprintf("%.1f%%", valor)
  )


datos_pos |> 
  filter(país == "México") |>
  ggplot(aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", color="black", size=1) +
  geom_text(aes(y = pos, label = etiqueta), color = "white", size = 2.5) +
  labs(
    title = "Distribución apilada del nivel educativo en México (2018–2023)",
    x = "Año", y = "Porcentaje total", fill = "Nivel educativo"
  ) +
  theme_classic()


datos_pos |> 
  filter(país == "OCDE") |>
  ggplot(aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", , color="black", size=1) +
  geom_text(aes(y = pos, label = etiqueta), color = "white", size = 2.5) +
  labs(
    title = "Distribución apilada del nivel educativo en países OCDE (2018–2023)",
    x = "Año", y = "Porcentaje total", fill = "Nivel educativo"
  ) +
  theme_classic()


ggplot(datos_pos, aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", color="black", size=1) + 
  facet_wrap(~ país) +
  labs(
    title = "Distribución educativa apilada: México vs OCDE (2018–2023)",
    x = "Año", y = "Porcentaje total", fill = "Nivel educativo"
  ) +
  theme_classic() +
  theme(legend.position = "top")
















