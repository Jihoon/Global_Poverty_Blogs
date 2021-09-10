library(povcalnetR)
library(haven)
library(tidyverse)
library(ggthemes)



# constants ----------------------------------------------------------------

country_list <- c("ARG", "THA", "GHA")
year_range <- 1990:2020
metadata_path <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/506801/povcalnet_comparability.csv"

#metadata_path <- "https://raw.githubusercontent.com/worldbank/povcalnet/master/metadata/povcalnet_comparability.csv"

# Load data ---------------------------------------------------------------

metadata <- read_csv(metadata_path)
cov_lkup <- c(3, 2, 1, 4)
names(cov_lkup) <- c("N", "U", "R", "A")

dat_lkup <- c(2,1)
names(dat_lkup) <- c("income","consumption")

# Data prep ---------------------------------------------------------------

df <- pcn %>%
  inner_join(metadata, by = c("countrycode", "year", "coveragetype", "datatype")) %>%
  filter(#countrycode %in% country_list,
         year %in% year_range) %>%
  group_by(countrycode, comparability) %>%
  mutate(
    spell = max(year) - min(year)
  ) %>%
  group_by(countrycode) %>%
  filter(spell == max(spell)) %>%
  pivot_longer(cols = starts_with("decile"), names_to = "decile", values_to = "decile_value") %>%
  mutate(
    decile_value = 10 * decile_value * mean,
    decile = str_replace(decile, "decile", ""),
    decile = as.numeric(decile) * 10,
    min_year = min(year),
    max_year = max(year),
  ) %>%
  group_by(countrycode, decile) %>%
  arrange(year) %>%
  mutate(
    g = ((last(decile_value) / first(decile_value))^(1/spell) - 1) * 100,
    m = ((last(mean) / first(mean))^(1/spell) - 1) * 100,
    legend_keys = paste0(countryname, " (", unique(min_year), "-", unique(max_year), ")")
  ) %>%
  ungroup() %>%
  select(countryname, decile, g, m, legend_keys) %>%
  distinct()

ggplot(df, aes(x = decile, y = g, color = legend_keys)) +
  geom_line(aes(y = m), linetype = "dashed", size = rel(1), alpha = .6) +
  geom_line(size = rel(0.8)) +
  geom_point(size = rel(2)) +
  # scale_y_continuous(limits = c(0, 8.5), breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_color_colorblind() +
  labs(
    x = "Decile group",
    y = "Annual growth in decile average income (%)"
  ) +
  theme_clean() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(linetype = "blank")
  ) +
  annotate("text", x = 80, y = 7.5,
           label = "Horizontal lines stand for\n annualized mean growth")















