# Carregar os dados e modelos
all_accidents <- readRDS("output/all_accidents.rds")
models <- readRDS("output/models.rds")

# Gráfico de coeficientes FE
coefficients <- as.data.frame(summary(models$fe_model)$coefficients) %>%
  rownames_to_column(var = "Variable")

coef_plot <- ggplot(coefficients, aes(x = reorder(Variable, Estimate), y = Estimate, fill = Estimate > 0)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = round(Estimate, 2)), hjust = ifelse(coefficients$Estimate > 0, -0.3, 1.3), size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")) +
  labs(title = "Efeitos Temporais e Categoria de Incidentes", x = "Variáveis", y = "Coeficientes Estimados") +
  theme_minimal()

ggsave("output/coef_plot.pdf", coef_plot, width = 10, height = 8)
