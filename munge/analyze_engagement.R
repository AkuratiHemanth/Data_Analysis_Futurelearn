# Load cached data
load("cache/merged_data.RData")

# Analyze Engagement by Archetype
engagement_by_archetype <- merged_data %>%
    group_by(archetype) %>%
    summarize(engaged_count = sum(fully_participated, na.rm = TRUE),
              total_count = n(),
              engagement_rate = engaged_count / total_count) %>%
    arrange(desc(engagement_rate))

# Save engagement analysis to cache
save(engagement_by_archetype, file = "cache/engagement_by_archetype.RData")
