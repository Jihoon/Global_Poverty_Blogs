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


pcn <- povcalnet()
pcn$coveragetype <- cov_lkup[pcn$coveragetype]
pcn$datatype <- dat_lkup[pcn$datatype]


# Data prep ---------------------------------------------------------------

df <- pcn %>%
  inner_join(metadata, by = c("countrycode", "year", "coveragetype", "datatype")) %>%
  filter(#countrycode %in% country_list,
         year %in% year_range) %>%
  mutate(
    gini = gini * 100,
    code_break = paste0(countrycode, comparability)
  ) %>%
  group_by(code_break) %>%
  arrange(year) %>%
  mutate(
    min_year = min(year),
    max_year = max(year),
    legend_keys = paste0(countryname, " (", unique(min_year), "-", unique(max_year), ")")
  ) %>%
  ungroup() %>%
  select(countryname, gini, year, code_break, legend_keys) %>%
  distinct()




ggplot(df, aes(x = year, y = gini, color = countryname)) +
  geom_line(aes(linetype = legend_keys), size = rel(.8)) +
  scale_y_continuous(limits = c(35, 55), breaks = c(35, 40, 45, 50, 55)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020)) +
  geom_point(size = rel(2), aes(shape = countryname)) +
  scale_color_colorblind() +
  scale_linetype_discrete() +
  guides(colour = FALSE,
         shape = FALSE,
         linetype = guide_legend(override.aes = list(colour = c("black", "black", "#E69F00", "#56B4E9", "#56B4E9", "#56B4E9")))) +
  labs(
    x = "Year",
    y = "Gini index"
  ) +
  theme_clean() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(linetype = c("blank"))
  )















