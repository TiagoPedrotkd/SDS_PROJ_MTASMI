# Preparar os dados para painel
panel_data <- pdata.frame(all_accidents, index = c("line", "date"))

# Modelos
fe_model <- plm(count ~ period + incident_category, data = panel_data, model = "within")
re_model <- plm(count ~ period + incident_category, data = panel_data, model = "random")

# Salvar os modelos
saveRDS(list(fe_model = fe_model, re_model = re_model), "output/models.rds")
