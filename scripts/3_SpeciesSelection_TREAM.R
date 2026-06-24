# Prepare TREAM data set (Welti et al. (2024) https://doi.org/10.1038/s41597-024-03445-3)


#----------------------------------------------------------

# ------------------------------------------------------- #
#                     03. Selecting species               #
# ------------------------------------------------------- #


#----------------------------------------------------------

# Selecting the 18 species + Pilot species for the BMIP project
# We select two species per range class (small, medium, large) and trend class (increasing, stable, decreasing) combination (= 18 species)

# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("species_information_TREAM_lm.csv", header = T)

# Tidy NA data
nyrs = 2020 - 1990

df_clean <- df %>%
  tidyr::drop_na() %>% 
  mutate(trend_lower = trend - qnorm(0.975) * std.error,
         trend_upper = trend + qnorm(0.975) * std.error) %>% 
  mutate(perc_trend = 100 * (exp(trend * nyrs) - 1)) %>% 
  # 50CI for percentage trend
  mutate(perc_lower = 100 * (exp(trend_lower * nyrs) - 1),
         perc_upper = 100 * (exp(trend_upper * nyrs) - 1)) %>%
  # signal-to-noise ratio (absolute trend / SE)
  mutate(trend_sn = abs(trend) / std.error)

# classify trend and range classes
df_lab <- df_clean %>%
  mutate(range_class = factor(range_class, levels = c("small", "medium", "large"))) %>% 
  mutate(trend_class = case_when(is.na(p.value) | is.na(perc_trend)            ~ NA_character_,
                                 perc_trend >=  5 ~ "increasing",
                                 perc_trend <= -5 ~ "declining",
                                 abs(perc_trend) < 5 ~ "stable",
                                 TRUE ~ "other")) %>% 
  mutate(overlapping = case_when(perc_lower <= 0 & perc_upper >= 0 ~ "overlapping CI")) %>% 
  select(-range_class, -trend_class, range_class, trend_class)

# Look at the number of species in the single classes
table(df_lab$trend_class, df_lab$range_class)

species_groups <- df_lab %>%
  group_by(trend_class, range_class) %>%
  group_split()

View(species_groups[[1]])

## SELECTED SPECIES

## Pilot species
# Agapetus ochripes

## Small declining

# Leuctra geniculata
# Simulium noelleri

## Medium declining

# Athripsodes aterrimus
# Prodiamesa olivacea

## Large declining

# Baetis vernus
# Hydropsyche angustipennis

## Small increasing

# Simulium angustitarse
# Simulium trifasciatum

## Medium increasing

# Brachyptera risi
# Nigrobaetis niger

## Large increasing

# Baetis rhodani
# Ephemera danica

## Small stable

# Anabolia furcata
# Sialis nigripes

## Medium stable

# Anabolia nervosa
# Kageronia fuscogrisea

## Large stable

# Platambus maculatus
# Polycentropus flavomaculatus


# Plot the species and their specific classes
df_lab_out <- df_lab %>%
  filter(binomial %in% c("Leuctra geniculata", "Simulium noelleri", "Athripsodes aterrimus", "Prodiamesa olivacea", "Baetis vernus", "Hydropsyche angustipennis", "Simulium angustitarse", "Simulium trifasciatum", "Brachyptera risi", "Nigrobaetis niger", "Baetis rhodani", "Ephemera danica", "Anabolia furcata", "Sialis nigripes", "Anabolia nervosa", "Kageronia fuscogrisea", "Platambus maculatus", "Polycentropus flavomaculatus")) %>% 
  mutate(group = paste(range_class, trend_class)) %>% 
  mutate(group = as.factor(group))

out <- ggplot(
  df_lab_out,
  aes(x = MCP2, y = perc_trend, colour = group)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  # --- 95% CI error bars ---
  geom_errorbar(aes(ymin = perc_lower, ymax = perc_upper),
                width = 0,            # no horizontal whiskers (vertical bars only)
                alpha = 0.6,
                linewidth = 0.4) +
  geom_point(size = 3, alpha = 0.9) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "MCP2",
    y = "Trend (% per year)",  # clarify units since we're plotting perc_trend
    colour = "Range and trend group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )