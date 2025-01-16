# Required Libraries -----------------------------------------------------
# Check and install required packages
required_packages <- c(
  "tidyverse", "lubridate", "broom", "fixest", "kableExtra",
  "gridExtra", "patchwork", "ggplot2", "plm", "lmtest", "stargazer"
)
install.packages(setdiff(required_packages, installed.packages()[,"Package"]), repos = "https://cran.r-project.org")

# Load required libraries
library(tidyverse)
library(lubridate)
library(broom)
library(fixest)
library(kableExtra)
library(gridExtra)
library(patchwork)
library(ggplot2)
library(plm)
library(lmtest)
library(stargazer)

# Data Import and Transformation -----------------------------------------
# Load datasets for pre-pandemic and post-pandemic incidents
dataset_pre_pandemic <- read_csv("data/MTA_Subway_Major_Incidents__2015-2019_20250116.csv")
dataset_post_pandemic <- read_csv("data/MTA_Subway_Major_Incidents__Beginning_2020_20250116.csv")

# Rename `month` column to `date` for consistency
dataset_pre_pandemic <- dataset_pre_pandemic %>% rename(date = month)
dataset_post_pandemic <- dataset_post_pandemic %>% rename(date = month)

# Convert `date` to Date format and add a period column
dataset_pre_pandemic <- dataset_pre_pandemic %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"), period = "pre_pandemic")

dataset_post_pandemic <- dataset_post_pandemic %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(year(date) == 2023) %>%
  mutate(period = "post_pandemic")

# Combine datasets and categorize incidents
all_accidents <- bind_rows(dataset_pre_pandemic, dataset_post_pandemic) %>%
  mutate(
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
  ) %>%
  group_by(line, date, period, incident_category, year) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  filter(!is.na(line) & !is.na(date))

# Check data structure
cat("Combined Data Structure:\n")
str(all_accidents)

# Descriptive Statistics -------------------------------------------------
# Generate statistics by period and incident category
summary_stats <- all_accidents %>%
  group_by(period, incident_category) %>%
  summarise(
    total_incidents = sum(count, na.rm = TRUE),
    avg_incidents_per_month = mean(count, na.rm = TRUE),
    .groups = "drop"
  )

cat("Descriptive Statistics:\n")
print(summary_stats)

# Data Visualization -----------------------------------------------------
# Plot incident trends by category over time
ggplot(all_accidents, aes(x = factor(year), y = count, fill = incident_category)) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  labs(
    title = "Incident Trends by Category (Pre- and Post-Pandemic)",
    x = "Year", y = "Number of Incidents"
  ) +
  theme_minimal()

# Econometric Modeling ---------------------------------------------------
# Prepare panel data
panel_data <- pdata.frame(all_accidents, index = c("line", "date"))

# Fixed Effects Model
fe_model <- plm(
  formula = count ~ period + incident_category,
  data = panel_data,
  model = "within"
)

# Random Effects Model
re_model <- plm(
  formula = count ~ period + incident_category,
  data = panel_data,
  model = "random"
)

# Save summaries for Fixed and Random Effects Models
fe_summary <- capture.output(summary(fe_model))
re_summary <- capture.output(summary(re_model))

# Save the summaries as PDFs ---------------------------------------------
# Create tables for FE and RE model summaries
fe_summary_table <- ggplot() +
  theme_void() +
  annotate(
    "text", x = 0.5, y = 0.5, label = paste(fe_summary, collapse = "\n"),
    hjust = 0.5, vjust = 0.5, size = 3, family = "mono", lineheight = 0.9
  ) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))

re_summary_table <- ggplot() +
  theme_void() +
  annotate(
    "text", x = 0.5, y = 0.5, label = paste(re_summary, collapse = "\n"),
    hjust = 0.5, vjust = 0.5, size = 3, family = "mono", lineheight = 0.9
  ) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))

# Save tables as separate pages in a PDF
pdf("Model_Summaries_FE_RE.pdf", width = 10, height = 8)
grid.arrange(fe_summary_table, top = "Fixed Effects Model Summary")
grid.arrange(re_summary_table, top = "Random Effects Model Summary")
dev.off()

# Hausman Test -----------------------------------------------------------
hausman_test <- phtest(fe_model, re_model)
cat("Hausman Test Results:\n")
print(hausman_test)

# Model Results Using Stargazer ------------------------------------------
stargazer_output <- capture.output(
  stargazer(fe_model, re_model, type = "text", title = "Panel Data Models")
)

stargazer_table <- ggplot() +
  theme_void() +
  annotate(
    "text", x = 0.5, y = 0.5, label = paste(stargazer_output, collapse = "\n"),
    hjust = 0.5, vjust = 0.5, size = 3, family = "mono", lineheight = 0.9
  ) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))

ggsave("Model_Results_Stargazer.pdf", stargazer_table, width = 10, height = 8)

# Coefficient Visualization ----------------------------------------------
coefficients <- as.data.frame(summary(fe_model)$coefficients) %>%
  rownames_to_column(var = "Variable")

# Plot estimated coefficients
ggplot(coefficients, aes(x = reorder(Variable, Estimate), y = Estimate, fill = Estimate > 0)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = round(Estimate, 2)),
            hjust = ifelse(coefficients$Estimate > 0, -0.2, 1.2),
            size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")) +
  labs(
    title = "Temporal Effects and Incident Categories",
    subtitle = "Estimated Coefficients from Fixed Effects Model",
    x = "Variables",
    y = "Estimated Coefficients"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 8, face = "bold"),
    axis.title.y = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 8)
  )
