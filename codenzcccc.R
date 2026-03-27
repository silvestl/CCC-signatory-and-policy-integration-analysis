library(ggplot2)
library(readxl)
library(stringr)
library(vegan)
library(forcats)
library(tidyr)
library(tibble)
library(writexl)
library(ggrepel)
library(igraph)
library(scales)
library(janitor)
library(lmtest)
library(car)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidygeocoder)
library(tidygraph)
library(viridis)
library(patchwork)
library(maps)
library(lubridate)
library(fixest)

df <- NetworkstakeholdersCCC
df$CAP_Topic <- substr(df$CAP_Topic, 1, nchar(df$CAP_Topic) - 4)

# Count actors by sector, type, city, and state
df %>% count(State, sort = TRUE)
df %>% count(City, sort = TRUE)
df %>% 
  count(City) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 77) +
  labs(
    x = "Number of co-signatories",
    y = "Number of CCCs")
df %>% 
  count(City) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(aes(y = after_stat(density)), bins = 77, alpha = 0.6) +
  geom_density() +
  labs(
    x = "Number of stakeholders per city",
    y = "Density")
df %>% 
  count(City) %>% 
  summarise(
    Q1  = quantile(n, 0.25),
    Q3  = quantile(n, 0.75),
    IQR = IQR(n))
df %>% count(Type, sort = TRUE)
df %>% count(Sector, sort = TRUE)
df %>% count(CAP_Topic, sort = TRUE)
CAPs <- df %>% count(City, CAP_Topic, sort = TRUE)
Type_totals <- df %>% count(City, Type, name = "n")

table(df$City, df$Type)
table(df$City, df$CAP_Topic)

#visualize
CAP_totals <- df %>% 
  count(CAP_Topic, name = "total_n") %>% 
  arrange(desc(total_n))
CAPs_plot <- CAPs %>% 
  left_join(CAP_totals, by = "CAP_Topic")
ggplot(CAPs_plot, aes(x = reorder(CAP_Topic, total_n), y = n, fill = CAP_Topic)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    #title = "Signatories per CAP Topic",
    x = "CAP Topic",
    y = "Count"
  ) +
  theme_minimal()
ggplot(CAPs, aes(x = City, y = n, fill = CAP_Topic)) +
  geom_col() +
  coord_flip() +
  labs(
    #title = "Stakeholders per CAP Topic by City",
    x = "City",
    y = "Count",
    fill = "CAP Topic"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )
#type
Type_totals_grouped <- Type_totals %>%
  mutate(Type_grouped = case_when(
    Type %in% c("Local public sector", "Public sector") ~ "Public sector / Local public sector",
    TRUE ~ Type
  ))
Grouped_totals <- Type_totals_grouped %>%
  group_by(Type_grouped) %>%
  summarise(total_n = sum(n), .groups = "drop")
Type_plot <- Type_totals_grouped %>%
  left_join(Grouped_totals, by = "Type_grouped")
ggplot(Type_plot, aes(x = fct_reorder(Type_grouped, total_n),  # rank by total
           y = n,
           fill = Type)) +                          # keep original colours
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    #title = "Signatories per actor type",
    x = "Type",
    y = "Count"
  ) +
  theme_minimal()
ggplot(Type_totals, aes(x = City, y = n, fill = Type)) +
  geom_col() +
  coord_flip() +
  labs(
    #title = "Stakeholders per actor type Topic by City",
    x = "City",
    y = "Count",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "right")
## per region according to UN geoscheme
region_lut <- tribble(
  ~State,            ~Region,
  # Western
  "Belgium","Western", "France","Western", "Netherlands","Western",
  "Austria","Western", "Germany","Western",
  # Eastern
  "Poland","Eastern",
  "Czechia","Eastern", "Slovakia","Eastern",
  "Hungary","Eastern", "Slovenia","Eastern",
  "Bulgaria","Eastern", "Romania","Eastern",
  # Northern
  "Latvia","Northern", "Lithuania","Northern",
  "Denmark","Northern", "Finland","Northern", "Iceland","Northern",
  "Norway","Northern", "Sweden","Northern", "UK","Northern", "Ireland","Northern",
  # Southern
  "Cyprus","Southern", "Greece","Southern", "Israel","Southern",
  "Italy","Southern", "Portugal","Southern", "Spain","Southern",
  "Turkey","Southern", "Croatia", "Southern")
df_reg <- df %>%
  left_join(region_lut, by = "State") %>%
  mutate(
    Region = fct_relevel(Region, "Northern","Western","Southern","Eastern"))
region <- df_reg %>% count(Region, Type, CAP_Topic)
#plots
region_totals <- region %>%
  group_by(Region) %>%
  summarise(total_n = sum(n), .groups = "drop")
region_cap_share <- region %>%
  group_by(Region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  left_join(region %>% group_by(Region) %>% summarise(total = sum(n)), by = "Region") %>%
  mutate(Region = fct_reorder(Region, total))
region_type_share <- region %>%
  group_by(Region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  left_join(region %>% group_by(Region) %>% summarise(total = sum(n)), by = "Region") %>%
  mutate(Region = fct_reorder(Region, total))
# Absolute counts plots (horizontal)
p_counts_cap <- region %>%
  left_join(region_totals, by = "Region") %>%
  ggplot(aes(x = n, y = fct_reorder(Region, total_n), fill = CAP_Topic)) +
  geom_col() +
  # add region total as label at end of bar
  geom_text(aes(x = total_n, y = Region, label = total_n),
            hjust = -0.2, size = 3) +
  expand_limits(x = max(region_totals$total_n) * 1.1) +
  labs(
    #title = "Signatories per CAP Topic (Counts)",
    x = "Count", 
    y = "Region"
  ) +
  theme_minimal()
p_counts_type <- region %>%
  left_join(region_totals, by = "Region") %>%
  ggplot(aes(x = n, y = fct_reorder(Region, total_n), fill = Type)) +
  geom_col() +
  geom_text(aes(x = total_n, y = Region, label = total_n),
            hjust = -0.2, size = 3) +
  expand_limits(x = max(region_totals$total_n) * 1.1) +
  labs(
    #title = "Signatories per Type (Counts)",
    x = "Count", 
    y = "Region"
  ) +
  theme_minimal()
# Share plots (horizontal)
p_share_cap <- ggplot(region_cap_share, aes(x = share, y = fct_reorder(Region, share), fill = CAP_Topic)) +
  geom_col() +
  scale_x_continuous(labels = percent_format()) +
  labs(
    #title = "Signatories per CAP Topic (Shares)",
       x = "Share", y = "Region", fill = "CAP Topic") +
  theme_minimal()
p_share_type <- ggplot(region_type_share, aes(x = share, y = fct_reorder(Region, share), fill = Type)) +
  geom_col() +
  scale_x_continuous(labels = percent_format()) +
  labs(
    #title = "Signatories per Type (Shares)",
       x = "Share", y = "Region", fill = "Type") +
  theme_minimal()
# Combine with patchwork: counts on top, shares below
p_counts_cap | p_share_cap
p_counts_type | p_share_type

#Heatmap
cap_order <- region %>%
  group_by(CAP_Topic) %>%
  summarise(total = sum(n), .groups="drop") %>%
  arrange(total) %>%
  pull(CAP_Topic)
ggplot(region %>% mutate(CAP_Topic = factor(CAP_Topic, levels = cap_order)),
       aes(x = Region, y = CAP_Topic, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) +
  labs(
    #title = "CAP Topic Intensity by European Region",
    x = "Region", y = "CAP Topic", fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
type_order <- region %>%
  group_by(Type) %>%
  summarise(total = sum(n), .groups="drop") %>%
  arrange(total) %>%
  pull(Type)
ggplot(region %>% mutate(Type = factor(Type, levels = type_order)),
       aes(x = Region, y = Type, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) +   # brighter = fewer actors
  labs(
    #title = "Actor Type Intensity by European Region",
    x = "Region", y = "Type", fill = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##HHI
hhi_from_counts <- function(n) {
  p <- n / sum(n)
  sum(p^2)
}
city_hhi_type <- df %>%
  count(City, Type) %>%
  group_by(City) %>%
  summarise(
    HHI_Type = hhi_from_counts(n),
    .groups = "drop"
  )
city_hhi_cap <- df %>%
  count(City, CAP_Topic) %>%
  group_by(City) %>%
  summarise(
    HHI_CAP = hhi_from_counts(n),
    .groups = "drop"
  )
#visual
hhi_from_counts <- function(n){ p <- n/sum(n); sum(p^2) }
city_totals <- df %>% count(City, name = "Total")
city_hhi_type <- df %>%
  count(City, Type) %>%
  group_by(City) %>%
  summarise(HHI_Type = hhi_from_counts(n), .groups = "drop")
city_hhi_cap <- df %>%
  count(City, CAP_Topic) %>%
  group_by(City) %>%
  summarise(HHI_CAP = hhi_from_counts(n), .groups = "drop")
city_hhi <- city_hhi_type %>%
  left_join(city_hhi_cap, by = "City") %>%
  left_join(city_totals,   by = "City")
med_x <- median(city_hhi$HHI_Type, na.rm = TRUE)
med_y <- median(city_hhi$HHI_CAP,  na.rm = TRUE)
ggplot(city_hhi, aes(x = HHI_Type, y = HHI_CAP, size = Total, label = City)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(max.overlaps = 25, size = 3) +
  geom_vline(xintercept = med_x, linetype = "dashed") +
  geom_hline(yintercept = med_y, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_size_continuous(name = "Signatories") +
  labs(
    #title = "City HHI: Actor Types vs. CAPs",
       x = "HHI (Types)", y = "HHI (CAPs)") +
  theme_minimal()
#new HHI
ggplot(
  full_dataset,
  aes(
    x = HHI_Type,
    y = HHI_CAP,
    size  = Total.x,
    color = Region,
    shape = EP_group,
    label = City)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(
    max.overlaps = 25,
    size = 3,
    show.legend = FALSE) +
  geom_vline(xintercept = med_x, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = med_y, linetype = "dashed", color = "grey40") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
  scale_size_continuous(name = "Signatories") +
  labs(
    #title = "City HHI: Actor Types vs. CAPs",
    subtitle = "Color = European region, Shape = EP political group",
    x = "HHI (Actor Types)",
    y = "HHI (CAP Topics)",
    color = "European region",
    shape = "EP group") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank())

#graph network in python to compute weighted average degrees for each city

#NetworkMetrics_Cities[78, "City"] <- "Eindhoven"
#inside Data MA combine the rows Eindhoven and Helmond
DataMA$Population <- as.numeric(gsub("\\.", "", DataMA$Population))
DataMA <- DataMA %>%
  mutate(Mayor_EP_group = ifelse(Mayor_EP_group == "NA", "Independent/Other", Mayor_EP_group))
DataMA <- DataMA %>%
  filter(City != "Helmond") %>%
  mutate(City = ifelse(City == "Eindhoven", "Eindhoven & Helmond", City),
         Population = ifelse(City == "Eindhoven & Helmond",
                             Population + DataMA$Population[DataMA$City == "Helmond"],
                             Population),
         Mayor_EP_group = ifelse(City == "Eindhoven & Helmond", NA, Mayor_EP_group))
full_dataset <- NetworkMetrics_Cities %>%
  left_join(city_hhi,   by = "City") %>%        # adds HHI_Type and HHI_CAP
  left_join(city_totals, by = "City") %>%       # adds Total
  left_join(AdditionalData, by = "City") %>%       # adds Total
  left_join(DataMA, by = "City") %>%       # adds population, GDP, etc.
  rename(
    Population = Population,
    GDP_capita_USD = `GDP/capita_USD`,
    Green_share = Green_areas_as_share,
    Internet_speed = Internetspeed_as_deviation_from_nat_avg,
    EP_group = Mayor_EP_group,
    AirPoll = `AirPollutionPM2.5Annmean`,
    CCPI = `CCPI_of_state`,
    NoisePoll = Noisepollutionavoidabledeathsrounded,
    CityNet = `C40+ICLEI`)
full_dataset <- full_dataset %>%  mutate(
    Population = as.numeric(gsub("\\.", "", Population)),
    log_pop = log(Population),
    log_gdp = log(GDP_capita_USD),
    Publicationdate = as.Date(Publicationdate))
full_dataset$Publicationdate <- as.Date(full_dataset$Publicationdate)
full_dataset$pubdaterank <- rank(full_dataset$Publicationdate, ties.method = "first")

hist(full_dataset$WeightedAvgDegree)
hist(log(full_dataset$WeightedAvgDegree))
hist(full_dataset$Population)
hist(log(full_dataset$Population))
hist(full_dataset$GDP_capita_USD)
hist(log(full_dataset$GDP_capita_USD))

#median Network Density
median(full_dataset$WeightedDensity, na.rm = TRUE)

#map representation
df2 <- full_dataset
cities <- df2 %>% distinct(City)
dfcoord <- cities %>%
  geocode(City, method = "osm", lat = latitude, long = longitude)
df2 <- df2 %>%
  left_join(dfcoord, by = "City")
#need to hardcode Kosice, Eindhoven & Helmond
df2[24, "latitude"] <- 48.7164
df2[24, "longitude"] <- 21.2611

#Europe bounding box
x_limits <- c(-22, 37)   # longitude
y_limits <- c(30, 66)    # latitude
#map
ggplot() +
  annotation_borders("world", colour = "grey70", fill = "grey90") +
  coord_fixed(xlim = x_limits, ylim = y_limits) +
  geom_point(
    data = df2,
    aes(
      x = longitude, y = latitude,
      size = WeightedDensity,
      color = WeightedDensity
    ),
    alpha = 0.8
  ) +
  geom_text_repel(
    data = df2,
    aes(x = longitude, y = latitude, label = City),
    size = 3,
    max.overlaps = 15,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey60"
  ) +
  scale_color_viridis_c(
    name = "Weighted Density",
    option = "plasma"
  ) +
  scale_size(range = c(2, 9), name = "Weighted Density") +
  guides(
    size = guide_legend(order = 1),
    color = guide_colorbar(order = 1)
  ) +
  labs(
    #title = "Governance Embeddedness across CCC-Cities measured in Bipartite Network Density",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#e7f7ff", color = NA),
    axis.text = element_blank(),
    legend.position = "right")
#with parties
#remove helmond
df3_no_Helmond <- df2[-78, ]
ggplot() +
  annotation_borders("world", colour = "grey70", fill = "grey90") +
  coord_fixed(xlim = x_limits, ylim = y_limits) +
  geom_point(
    data = df3_no_Helmond,
    aes(
      x = longitude,
      y = latitude,
      size  = WeightedDensity,
      color = EP_group),
    alpha = 0.8) +
  geom_text_repel(
    data = df3_no_Helmond,
    aes(x = longitude, y = latitude, label = City),
    size = 3,
    max.overlaps = 15,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey60") +
  scale_size(
    range = c(2, 9),
    name = "Governance\nEmbeddedness\n(Weighted Density)") +
  scale_color_brewer(
    palette = "Set2",
    name = "EP Party Group",
    na.value = "grey70") +
  guides(
    size  = guide_legend(order = 1),
    color = guide_legend(order = 2)) +
  labs(
    #title = "Governance Embeddedness and EP Party Affiliation across CCC Cities",
    x = NULL,
    y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#e7f7ff", color = NA),
    axis.text = element_blank(),
    legend.position = "right")
df3_no_Limassol <- df2[-59, ]
ggplot() +
  annotation_borders("world", colour = "grey70", fill = "grey90") +
  coord_fixed(xlim = x_limits, ylim = y_limits) +
  geom_point(
    data = df3_no_Limassol,
    aes(
      x = longitude, y = latitude,
      size = Total.x,
      color = WeightedDensity
    ),
    alpha = 0.8
  ) +
  geom_text_repel(
    data = df3_no_Limassol,
    aes(x = longitude, y = latitude, label = City),
    size = 3,
    max.overlaps = 15,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey60"
  ) +
  scale_color_viridis_c(
    name = "Weighted Density",
    option = "plasma"
  ) +
  scale_size(range = c(1, 9), name = "Signatories") +
  guides(
    size = guide_legend(order = 1),
    color = guide_colorbar(order = 1)
  ) +
  labs(
    #title = "Governance Embeddedness across CCCs measured in Bipartite Network Density",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#e7f7ff", color = NA),
    axis.text = element_blank(),
    legend.position = "right")

#weighted density and horizontal embeddedness only from CCC
#DOUBLECHECKLEGENDS
edges2 <- read_excel("HorizontalEdges_Cities_ALT.xlsx")
edges2 <- edges2 %>%
  dplyr::mutate(w_norm = weight / max(weight, na.rm = TRUE))
ggplot() +
  annotation_borders("world", colour = "grey70", fill = "grey90") +
  coord_fixed(xlim = x_limits, ylim = y_limits) +
  #edges
  geom_curve(
    data = edges2,
    aes(
      x = lon_from, y = lat_from,
      xend = lon_to, yend = lat_to,
      linewidth = weight),
    curvature = 0.2,
    alpha = 0.4,
    colour = "grey40")+
  geom_point(
    data = df2,
    aes(
      x = longitude, y = latitude,
      size = Total.x,
      color = WeightedDensity),
    alpha = 0.8
  ) +
  geom_text_repel(
    data = df2,
    aes(x = longitude, y = latitude, label = City),
    size = 3,
    max.overlaps = 15,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey60"
  ) +
  scale_color_viridis_c(
    name = "Governance \nEmbeddedness",
    option = "plasma"
  ) +
  scale_size(range = c(2, 9), name = "Stakeholders") +
  guides(
    size = guide_legend(order = 1),
    color = guide_colorbar(order = 1)) +
  labs(
    #title = "Horizontal and Internal Governance Embeddedness across CCCs",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#e7f7ff", color = NA),
    axis.text = element_blank(),
    legend.position = "right")

#plot with gov emb
ggplot(full_dataset, aes(x = Region, y = WeightedDensity)) +
  geom_violin(alpha = 0.3, fill = "lightblue") +
  geom_point(aes(size = Total.x, color = EP_group), position = position_jitter(width = 0.15, height = 0)) +
  geom_hline(yintercept = med_ix, linetype = "dashed", color = "grey40") +
  labs(
    #title = "Governance Embeddedness by Region",
    x = "European Region",
    y = "Weighted Density",
    color = "EP group",
    size = "Signatories"
  ) +
  theme_minimal()

#regression models: what drives weighted density? h3
rmh3log <- lm(log(WeightedDensity) ~ log_pop + log_gdp + CCPI +
             EP_group + CityNet, data = df_complete_no_Lim)
summary(rmh3log)
augh3log <- broom::augment(rmh3log)
ggplot(augh3log, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
  geom_point(alpha = 0.6) +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()
#tests
bptest(rmh3log)
vif(rmh3log)
crPlots(rmh3log)

## tests
ggplot(df3_no_Limassol, 
       aes(x = Total.x, y = WeightedDensity, color = Region)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Region) +
  theme_minimal() +
  labs(x = "Number of Co-Signatories",
       y = "Weighted Bipartite Network Density")
ggplot(df3_no_Limassol, 
       aes(x = Total.x, y = WeightedDensity, color = EP_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ EP_group) +
  theme_minimal() +
  labs(x = "Number of Co-Signatories",
       y = "Weighted Bipartite Network Density")
#bivariate correlations
numeric_vars <- c("Total.x", "log_gdp", "log_pop", "CCPI", "CityNet", "AirPoll")
cor_results <- sapply(numeric_vars, function(var) {
  x <- df_complete_no_Lim[[var]]
  y <- log(df_complete_no_Lim$WeightedDensity)
  complete_idx <- complete.cases(x, y)
  cor(x[complete_idx], y[complete_idx], method = "pearson")
})
cor_results <- round(cor_results, 3)
cor_results

#scatterplot
cor_test <- cor.test(df3_no_Limassol$Total.x, log(df3_no_Limassol$WeightedDensity))
cor_label <- paste0("r = ", round(cor_test$estimate, 2))
ggplot(df3_no_Limassol, aes(x = Total.x, y = log(WeightedDensity))) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "#3B8BC2") +
  geom_text_repel(aes(label = City), size = 3) +
  annotate("text", x = Inf, y = -Inf, label = cor_label,
           hjust = 1.1, vjust = -0.5, size = 3.5, color = "black") +
  labs(
    x = "Total Signatories",
    y = "log WeightedDensity") +
  theme_minimal()
