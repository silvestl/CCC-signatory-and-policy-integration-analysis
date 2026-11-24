#CCCanalysis
#Run bipartite_networks.py first to produce datasets

library(tidygraph)
library(viridis)
library(maps)
library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
library(tidygeocoder)
library(fixest)
library(lmtest)
library(car)

#inputs
NetworkstakeholdersCCC <- read_excel("NetworkstakeholdersCCC.xlsx")
NetworkMetrics_Cities <- read_excel("NetworkMetrics_Cities.xlsx")
NodeDegrees_Cities <- read_excel("NodeDegrees_Cities.xlsx")
NodeDegrees_Summary <- read_excel("NodeDegrees_Summary.xlsx")
DataMA <- read_excel("Data_Silvestri_2025.xlsx")
AdditionalData <- read_excel("AdditionalData.xlsx")
GlobalCoM <- read_excel("GlobalCoM.xlsx", sheet = 4)

df <- NetworkstakeholdersCCC

#colleague sample test
set.seed(42)
dfcodermaster <- NetworkstakeholdersCCC %>% slice_sample(n = 50)
write_xlsx(dfcodermaster, "coder_reliab_master.xlsx")
dfcodermaster %>% 
  select(-5) %>% 
  mutate(across(c(Type, CAP_Topic), ~ "")) %>% 
  write_xlsx("coder_reliab_blind.xlsx")
master <- read_excel("coder_reliab_master.xlsx")
blind <- read_excel("coder_reliab_blindK.xlsx")
type_agreement <- mean(master$Type == blind$Type) * 100
cap_agreement <- mean(master$CAP_Topic == blind$CAP_Topic) * 100
cat(sprintf("Percentage Agreement for Actor Type: %.1f%%\n", type_agreement))
cat(sprintf("Percentage Agreement for Policy Sector: %.1f%%\n", cap_agreement))

#code
df$CAP_Topic <- substr(df$CAP_Topic, 1, nchar(df$CAP_Topic) - 4)
city_state_lut <- df %>% 
  distinct(City, State)

# Count actors by sector, type, city, and state
df %>%
  count(City) %>%
  summarise(
    Mean = mean(n),
    Q1  = quantile(n, 0.25),
    Q3  = quantile(n, 0.75),
    IQR = IQR(n))
df %>% count(City) %>% filter(n < 5)     # CCCs with fewer than 5 signatories
df %>% count(Type, sort = TRUE)
df %>% count(CAP_Topic, sort = TRUE)
n_distinct(df$CAP_Topic)                 # policy sectors present (K in Eq. 1)
df %>%                                   # public actors (public sector, local public sector, public companies)
  filter(Type %in% c("Public sector", "Local public sector", "Public company")) %>%
  nrow()

#visualize policy sectors
CAPs <- df %>% count(City, CAP_Topic, sort = TRUE)
Type_totals <- df %>% count(City, Type, name = "n")
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
           fill = Type)) +                          
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    #title = "Signatories per actor type",
    x = "Type",
    y = "Count"
  ) +
  theme_minimal()

###HHI (Section 2.2, Figure 3)
hhi_from_counts <- function(n) { p <- n / sum(n); sum(p^2) }
city_hhi <- df %>%
  count(City, Type) %>% group_by(City) %>%
  summarise(HHI_Type = hhi_from_counts(n), .groups = "drop") %>%
  left_join(df %>% count(City, CAP_Topic) %>% group_by(City) %>%
              summarise(HHI_CAP = hhi_from_counts(n), .groups = "drop"), by = "City") %>%
  left_join(df %>% count(City, name = "Total"), by = "City")
med_x <- median(city_hhi$HHI_Type)
med_y <- median(city_hhi$HHI_CAP)
ggplot(city_hhi, aes(HHI_Type, HHI_CAP, label = City)) +
  geom_vline(xintercept = med_x, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_hline(yintercept = med_y, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_point(aes(size = Total), alpha = 0.6, colour = "grey30") +
  geom_text_repel(size = 2.8,                     # ≈ 8 pt
                  max.overlaps = Inf,             # label every city
                  min.segment.length = 0,
                  segment.size = 0.2, segment.colour = "grey60",
                  box.padding = 0.15, point.padding = 0.1,
                  force = 2, max.iter = 1e5, max.time = 5, seed = 42) +
  scale_size_area(max_size = 5, name = "Signatories") +
  labs(x = "HHI actor types", y = "HHI policy sectors") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave("Figure_3_HHI.pdf", width = 16, height = 14, units = "cm")

#visualization fig2 (degree vs weighted degree, from bipartite_networks.py)
H2 <- NodeDegrees_Summary %>%
  mutate(Node = paste0(Node, " (", Signatories, ")"),
         Node = fct_reorder(Node, Share_signatories)) %>%
  pivot_longer(c(Share_signatories, Share_links), names_to = "Measure", values_to = "Share") %>%
  mutate(Measure = recode(Measure, Share_signatories = "Share of signatories",
                          Share_links = "Share of links between node sets"))
ggplot(H2, aes(Share, Node)) +
  geom_line(aes(group = Node), colour = "grey75", linewidth = 0.6) +
  geom_point(aes(colour = Measure, shape = Measure), size = 2.4) +
  facet_grid(Side ~ ., scales = "free_y", space = "free_y") +     # stacked, height ∝ rows
  scale_colour_manual(values = c("Share of signatories" = "#2a78d6",
                                 "Share of links between node sets" = "#eb6834")) +
  scale_shape_manual(values = c("Share of signatories" = 16,
                                "Share of links between node sets" = 17)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Share across all CCCs", y = NULL, colour = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        axis.text.y = element_text(size = 9),                       # category names
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text.y = element_text(size = 10, angle = -90),
        panel.spacing = unit(0.6, "cm"))
ggsave("Figure_2.pdf", width = 17, height = 18, units = "cm")

#degrees & weighted degrees
NodeDegrees_Cities %>%
  filter(Side == "Actor type") %>%
  summarise(median_sectors = median(Degree), mean_sectors = mean(Degree))
#table with all metrics supplementary (Table S3)
tab_base <- df %>%
  group_by(City) %>%
  summarise(Country        = first(State),
            Signatories    = n(),
            Actor_types    = n_distinct(Type),
            Policy_sectors = n_distinct(CAP_Topic),
            .groups = "drop") %>%
  left_join(city_hhi %>% select(City, HHI_Type, HHI_CAP), by = "City") %>%
  left_join(NetworkMetrics_Cities %>% select(City, WeightedDensity), by = "City")
# medians without the outlier Limassol (as in Section 3.2.2)
med <- tab_base %>%
  filter(City != "Limassol") %>%
  summarise(across(c(HHI_Type, HHI_CAP, WeightedDensity), median))
tab_s <- tab_base %>%
  mutate(
    Diversity = case_when(
      HHI_Type <  med$HHI_Type & HHI_CAP <  med$HHI_CAP ~ "Diverse",
      HHI_Type >= med$HHI_Type & HHI_CAP >= med$HHI_CAP ~ "Concentrated",
      TRUE                                              ~ "Mixed"),
    Density = if_else(WeightedDensity > med$WeightedDensity, "high WD", "low WD"),
    Diversityxdensity = paste0(Diversity, ", ", Density),
    Diversityxdensity = if_else(City == "Limassol", paste(Diversityxdensity, "(outlier)"), Diversityxdensity)) %>%
  arrange(desc(WeightedDensity)) %>%
  mutate(Rank = row_number(),
         across(c(HHI_Type, HHI_CAP, WeightedDensity), ~ round(.x, 2))) %>%
  select(Rank, City, Country, Signatories, Actor_types, Policy_sectors,
         HHI_Type, HHI_CAP, WeightedDensity, Diversityxdensity) %>%
  rename(`HHI (actor types)`    = HHI_Type,
         `HHI (policy sectors)` = HHI_CAP,
         `Weighted density`     = WeightedDensity,
         `Actor types`          = Actor_types,
         `Policy sectors`       = Policy_sectors)
write_xlsx(tab_s, "0_TableS3_CitiesData.xlsx")

#weighted density: range and median without Limassol (Section 3.2.2)
tab_base %>%
  filter(City != "Limassol") %>%
  summarise(min = min(WeightedDensity), median = median(WeightedDensity), max = max(WeightedDensity))
#number of CCCs per coalition type (Section 3.2.2)
tab_s %>% filter(City != "Limassol") %>% count(Diversityxdensity)
#inside Data MA combine the rows Eindhoven and Helmond
DataMA <- DataMA %>%
  mutate(Mayor_EP_group = ifelse(Mayor_EP_group == "NA", "Independent/Other", Mayor_EP_group)) %>%
  mutate(
    City = if_else(City == "Eindhoven", "Eindhoven & Helmond", City),
    Mayor_EP_group = if_else(City == "Eindhoven & Helmond", NA_character_, Mayor_EP_group)
  ) %>%
  filter(City != "Helmond")
full_dataset <- NetworkMetrics_Cities %>%
  left_join(city_state_lut, by = "City") %>%
  left_join(city_hhi,       by = "City") %>%      # adds HHI_Type, HHI_CAP, Total
  left_join(AdditionalData, by = "City") %>%
  left_join(DataMA,         by = "City") %>%      # adds GDP per capita, mayor's EP group
  rename(GDP_capita_USD = `GDP/capita_USD`,
         EP_group       = Mayor_EP_group) %>%
  mutate(log_gdp = log(GDP_capita_USD),
         Publicationdate = as.Date(Publicationdate))
range(full_dataset$Publicationdate, na.rm = TRUE)   # timeframe of the CCCs
#map (Figure 4)
df2 <- full_dataset
cities <- df2 %>% distinct(City)
dfcoord <- cities %>%
  geocode(City, method = "osm", lat = latitude, long = longitude)
df2 <- df2 %>%
  left_join(dfcoord, by = "City")
#need to hardcode cities that OSM places wrongly (by name, not by row number)
df2 <- df2 %>%
  mutate(
    latitude  = case_when(City == "Kosice"              ~ 48.7164,
                          City == "Eindhoven & Helmond" ~ 51.44083,
                          City == "Athens"              ~ 37.9838,
                          City == "Zagreb"              ~ 45.8154,
                          TRUE                          ~ latitude),
    longitude = case_when(City == "Kosice"              ~ 21.2611,
                          City == "Eindhoven & Helmond" ~ 5.47778,
                          City == "Athens"              ~ 23.7275,
                          City == "Zagreb"              ~ 15.978,
                          TRUE                          ~ longitude))
#Europe bounding box
x_limits <- c(-22, 37)   # longitude
y_limits <- c(30, 66)    # latitude
df3_no_Limassol <- df2 %>% filter(City != "Limassol")
ggplot() +
  annotation_borders("world", colour = "grey70", fill = "grey90") +
  coord_fixed(xlim = x_limits, ylim = y_limits) +
  geom_point(
    data = df3_no_Limassol,
    aes(
      x = longitude, y = latitude,
      size = Total,
      color = WeightedDensity
    ),
    alpha = 0.8
  ) +
  geom_text_repel(
    data = df3_no_Limassol,
    aes(x = longitude, y = latitude, label = City),
    size = 2.5,                 # ≈ 7 pt at print size
    max.overlaps = Inf,         # keep every city name
    box.padding = 0.15,
    point.padding = 0.1,
    min.segment.length = 0,
    segment.size = 0.2,
    segment.color = "grey60",
    seed = 42                   # same label positions every run
  ) +
  scale_color_viridis_c(
    name = "Weighted Density",
    option = "plasma"
  ) +
  scale_size(range = c(1, 6), name = "Signatories") +
  guides(
    size = guide_legend(order = 1),
    color = guide_colorbar(order = 1)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#e7f7ff", color = NA),
    axis.text = element_blank(),
    legend.position = "bottom")                         # map gets full width
ggsave("Figure_map.pdf", width = 17, height = 13, units = "cm")

#regression models H3
#using gcom data for population and emissions
gcom_t3 <- GlobalCoM
colnames(gcom_t3) <- make.names(colnames(gcom_t3))
pop_col <- grep("population.*2018|population", colnames(gcom_t3), value = TRUE)[1]
gdp_col <- grep("GDP.per.capita", colnames(gcom_t3), value = TRUE)[1]
ghg_col <- grep("GHG.emissions.per.capita", colnames(gcom_t3), value = TRUE)[1]
gcom_t3_cleaned <- gcom_t3 %>%
  mutate(
    signatory.name = trimws(gsub("[[:space:]]+", " ", signatory.name)),
    signatory.name = case_when(
      grepl("Eindhoven|Helmond", signatory.name, ignore.case = TRUE) ~ "Eindhoven & Helmond",
      signatory.name == "Liepāja" ~ "Liepaja",
      signatory.name == "Klagenfurt am Wörthersee" ~ "Klagenfurt",
      signatory.name == "Göteborg" ~ "Gothenburg",
      signatory.name == "Århus" ~ "Aarhus",
      signatory.name == "Cork City Council" ~ "Cork",
      signatory.name == "BORDEAUX METROPOLE" ~ "Bordeaux",
      signatory.name == "Dublin City Council" ~ "Dublin",
      signatory.name == "Eilat City ISRAEL" ~ "Eilat",
      signatory.name == "Milan" ~ "Milano",
      signatory.name == "Wrocław" ~ "Wroclaw",
      signatory.name == "Bucharest 2nd District" ~ "Bucharest",
      signatory.name == "Firenze" ~ "Florence",
      signatory.name == "Izmir Metropolitan Municipality" ~ "İzmir",
      signatory.name == "Municipality of Kozani" ~ "Kozani",
      signatory.name == "Sonderborg" ~ "Sønderborg",
      signatory.name == "Sevilla" ~ "Seville",
      grepl("Vitoria.*Gasteiz", signatory.name, ignore.case = TRUE) ~ "Vitoria-Gasteiz",
      signatory.name == "Angers Loire Métropole" ~ "Angers",
      signatory.name == "München" ~ "Munich",
      TRUE ~ signatory.name
    )
  )
gcom_t3_aggregated <- gcom_t3_cleaned %>%
  group_by(signatory.name) %>%
  summarise(
    across(all_of(pop_col), ~sum(.x, na.rm = TRUE), .names = "gcom_population"),
    across(all_of(c(gdp_col, ghg_col)), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
gcom_t3_final <- gcom_t3_aggregated %>%
  rename(gcom_co2_per_capita = all_of(ghg_col))
merged_df <- df2 %>%
  inner_join(
    gcom_t3_final,
    by = c("City" = "signatory.name")
  )
merged_df <- merged_df %>%
  filter(City != "Limassol") %>%
  mutate(log_wd  = log(WeightedDensity),
         log_pop = log(gcom_population))      # population of the signing unit (GCoM)
#model 1: pooled OLS, standard errors clustered by country (N = 71)
m1 <- feols(
  log_wd ~ log_gdp + log_pop + gcom_co2_per_capita,
  data = merged_df,
  vcov = ~State
)
#model 2: country fixed effects; countries with a single CCC drop out (N = 60)
m2 <- feols(
  log_wd ~ log_gdp + log_pop + gcom_co2_per_capita | State,
  data = merged_df,
  vcov = ~State
)
etable(
  m1, m2,
  headers = c("Baseline OLS", "Country FE"),
  fitstat = c("n", "r2", "ar2", "wr2")
)
#diagnostics model 1
d   <- merged_df %>% drop_na(log_wd, log_gdp, log_pop, gcom_co2_per_capita)   # N = 71
lm1 <- lm(log_wd ~ log_gdp + log_pop + gcom_co2_per_capita, data = d)
lmtest::bptest(lm1)            # heteroskedasticity (R3 #15)
car::vif(lm1)                  # collinearity
plot(lm1, which = 1:2)         # residuals vs fitted and Q-Q plot


#correlation coalition size and weighted density (R1)
with(filter(full_dataset, City != "Limassol"), cor(Total, log(WeightedDensity)))
