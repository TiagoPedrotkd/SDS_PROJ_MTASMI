summary_stats <- all_accidents %>%
    group_by(period, incident_category) %>%
    summarise(
        total_incidents = sum(count, na.rm = TRUE),
        avg_incidents_per_month = mean(count, na.rm = TRUE),
        .groups = "drop"
    )
write.csv(summary_stats, "output/descriptive_stats.csv")
