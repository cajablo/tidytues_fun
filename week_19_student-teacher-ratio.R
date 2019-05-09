library(tidyverse)

#load data
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

#get unique countries, how many years of data for each country, and how many unique education level groups
uniq_countries <- student_ratio %>%
  group_by(country_code, country) %>%
  summarize(num_years = length(unique(year)), num_unique_indicators = length(unique(indicator)))

#plot average student ratio (across all indicators) for high, upper/lower middle and low income countries over the years

#World Bank Index country code to filter
WBI_code <- c("Low income countries", "Lower middle income countries", "Upper middle income countries", "High income countries") %>%
  as_factor()

WBI_mean_ratio <- student_ratio %>%
  filter(country %in% WBI_code)%>%
  mutate(country = factor(country, levels = WBI_code)) %>%
  group_by(country, year, indicator) %>%
  summarize(mean_ratio = mean(student_ratio, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(country, year) %>%
  mutate(all_levels_mean = mean(mean_ratio))%>%
  spread(indicator, mean_ratio) %>%
  gather(level, mean_ratio, 3:8)

write.csv(WBI_mean_ratio, file = "week_19_WBI_mean_ratio.csv", row.names = FALSE)

#Test plot
#first filter based on education level
WBI_mean_ratio_filter <- WBI_mean_ratio %>%
  na.omit() %>%
  filter(level == "Pre-Primary Education")

#plot
WBI_student_ratio_plot <- ggplot(WBI_mean_ratio_filter) +
  geom_line(aes(x = year, y = mean_ratio, color = country)) +
  theme_minimal() +
  labs(
    color = "WB Income Level",
    x = "",
    y = "Mean Student-Teacher Ratio"
  ) +
  theme(legend.position = "bottom")

WBI_student_ratio_plot
