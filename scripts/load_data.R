# Criar o diretório 'output' se ele não existir
if (!dir.exists("output")) {
  dir.create("output")
}

# Carregar e transformar os dados
dataset_pre_pandemic <- read_csv("data/MTA_Subway_Major_Incidents__2015-2019_20250116.csv")
dataset_post_pandemic <- read_csv("data/MTA_Subway_Major_Incidents__Beginning_2020_20250116.csv")

# Renomear a coluna `month` para `date`
dataset_pre_pandemic <- dataset_pre_pandemic %>% rename(date = month)
dataset_post_pandemic <- dataset_post_pandemic %>% rename(date = month)

# Transformar os dados
all_accidents <- bind_rows(
  dataset_pre_pandemic %>% mutate(period = "pre_pandemic"),
  dataset_post_pandemic %>% filter(year(as.Date(date, "%Y-%m-%d")) == 2023) %>% mutate(period = "post_pandemic")
) %>%
  mutate(
    date = as.Date(date, "%Y-%m-%d"),
    period_numeric = ifelse(period == "pre_pandemic", 0, 1),
    year = year(date),
    month = month(date),
    incident_category = case_when(
      grepl("Signals", category, ignore.case = TRUE) ~ "Signal Issues",
      grepl("Track", category, ignore.case = TRUE) ~ "Track Issues",
      grepl("Persons", category, ignore.case = TRUE) ~ "Person-Related Issues",
      grepl("Subway Car", category, ignore.case = TRUE) ~ "Subway Car Issues",
      grepl("Stations", category, ignore.case = TRUE) ~ "Station/Structure Issues",
      TRUE ~ "Other"
    )
  )

all_accidents <- all_accidents %>%
  filter(!is.na(line) & !is.na(date)) %>%
  distinct(line, date, .keep_all = TRUE)

# Salvar os dados como arquivo RDS
saveRDS(all_accidents, "output/all_accidents.rds")
