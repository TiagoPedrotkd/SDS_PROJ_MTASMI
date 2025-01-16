# Carregar os modelos
models <- readRDS("output/models.rds")

# Teste de Hausman
hausman_test <- phtest(models$fe_model, models$re_model)

# Salvar o teste
writeLines(capture.output(hausman_test), "output/hausman_test.txt")
