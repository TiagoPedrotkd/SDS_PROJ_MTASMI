# Carregar os modelos
models <- readRDS("output/models.rds")

# Exportar resultados dos modelos
stargazer(models$fe_model, models$re_model, type = "text", title = "Modelos de Dados em Painel", out = "output/models_summary.txt")
