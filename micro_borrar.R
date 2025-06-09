# Cargar librería
library(ggplot2)

# Curva de demanda
Q <- seq(0, 100, 0.1)
P <- 100 - Q
df <- data.frame(Q = Q, P = P)

# Equilibrios
# Pre-fusión
Q_pre <- 45
P_pre <- 55

# Post-fusión
Q_post <- 40
P_post <- 60

# Crear gráfico
ggplot(df, aes(x = Q, y = P)) +
  geom_line(color = "lightblue", size = 1.2) +  # Curva de demanda
  geom_hline(yintercept = 40, linetype = "dashed", color = "gray", size = 1) +  # Costo marginal
  geom_point(aes(x = Q_pre, y = P_pre), color = "lightgreen", size = 4) +  # Punto pre-fusión
  geom_point(aes(x = Q_post, y = P_post), color = "orange", size = 4) +   # Punto post-fusión
  annotate("text", x = Q_pre + 2, y = P_pre, label = "Pre-fusión", color = "lightgreen", hjust = 0) +
  annotate("text", x = Q_post + 2, y = P_post, label = "Post-fusión", color = "orange", hjust = 0) +
  annotate("text", x = Q_post + 40, y = P_post - 25, label = "Costo Marginal", color = "gray", hjust = 0) +
  annotate("text", x = Q_post + 40, y = P_post - 55, label = "Demanda inversa", color = "lightblue", hjust = 0) +
  labs(title = "Comparación Pre y Post Fusión",
       x = "Cantidad Total (Q)",
       y = "Precio (p)") +
  theme_classic()



