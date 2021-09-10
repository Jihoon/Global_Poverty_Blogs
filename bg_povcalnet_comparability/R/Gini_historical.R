pcn.national = pcn %>%
  filter(coveragetype==3)

pcn.national.income = pcn.national %>% filter(datatype==2)
pcn.national.consum = pcn.national %>% filter(datatype==1)

df <- pcn %>%
  inner_join(metadata, by = c("countrycode", "year", "coveragetype", "datatype")) %>%
  filter(#countrycode %in% country_list,
    year %in% year_range) %>%
  mutate(
    gini = gini * 100,
    code_break = paste0(countrycode, comparability)
  ) %>%
  select(-contains("decile")) %>%
  group_by(code_break) 


# df.min.max = df %>%
#   mutate(
#     prev.year = lag(year),
#     prev.gini = lag(gini),
#     spell = year - prev.year,
#     spell.tot = max(year) - min(year)
#   ) %>%
#   filter(spell > 0) %>%
#   mutate(
#     gini.r = ((gini / prev.gini)^(1/spell) - 1) * 100,
#     gini.r.long = ((last(gini) / first(gini))^(1/spell.tot) - 1) * 100,
#   )

spell.max = df.min.max %>% count(spell.tot) 

df.all = data.frame()
# Check 1 to 15 years as WB did
for (lag in 1:15) {
  df.lag = df %>%
    mutate(
      prev.year = lag(year, lag),
      prev.gini = lag(gini, lag),
      spell = year - prev.year
    ) %>%
    filter(spell > 0) %>%
    mutate(
      gini.r = ((gini / prev.gini)^(1/spell) - 1) * 100
    ) %>%
    select(countrycode:year, gini, code_break:gini.r)
  
  df.all = df.all %>% rbind(df.lag)
}

df.all = df.all %>% arrange(code_break, year) %>% ungroup() %>% group_by(spell) 

library(scales)

# Summary stats by spell length
df.sum = df.all %>% 
  summarise(quantile = scales::percent(c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)),
            gini.r = quantile(gini.r, c(0.05, 0.1,0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE))

df.sum %>% filter(spell==5)

# Summary stats for all spells >= 5
df.all %>% ungroup() %>% filter(spell >=5) %>% 
  summarise(quantile = scales::percent(c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)),
            gini.r = quantile(gini.r, c(0.05, 0.1,0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE))

ggplot(df.all %>% filter(spell>4, spell<16), aes(x=gini.r)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(vars(spell))

ggplot(pcn, aes(x=gini*100)) +
  geom_histogram(binwidth = 0.5) 
