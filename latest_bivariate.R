library(tidyverse)
library(janitor)
library(stringr)
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(rgdal)
library(ggthemes)
library(plyr)
library(biscale)
library(sf)
library(plotly)
library(cowplot)
options(scipen = 999)

vaccines <- read.csv("vaccines_18thMay.csv",row.names = NULL,
                     stringsAsFactors = FALSE, na.strings = "-")
total_cases <- read.csv("cases_18may.csv",row.names = NULL,
                        stringsAsFactors = FALSE,  header = TRUE)

states_shape <- readOGR("Admin2.shp")
vaccines <- clean_names(vaccines)
total_cases <- clean_names(total_cases)


names(vaccines)[1] <- paste("id")
names(total_cases)[1] <- paste("id")

replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

vaccines$dose_1_18th_may <- replaceCommas(vaccines$dose_1_18th_may)
vaccines$dose_2_18th_may<- replaceCommas(vaccines$dose_2_18th_may)
vaccines$total_doses_18th_may<- replaceCommas(vaccines$total_doses_18th_may)
vaccines$total_doses_the_dat_before_18th_may<- replaceCommas(vaccines$total_doses_the_dat_before_18th_may)

total_cases$total_cases <- replaceCommas(total_cases$total_cases)
total_cases$per_100_000<- replaceCommas(total_cases$per_100_000)
total_cases$daily_avg_in_last_7_days<- replaceCommas(total_cases$daily_avg_in_last_7_days)
total_cases$per_100_000_1<- replaceCommas(total_cases$per_100_000_1)

vaccines <- merge(vaccines,total_cases)

vaccines$total_doses_in_pipeline_18th_may_3_days[is.na(vaccines$total_doses_in_pipeline_18th_may_3_days)] <- 0

vaccines[-1] <- lapply(vaccines[-1], as.numeric)

vaccines$percapitavaccinated <- ((vaccines$total_doses_18th_may)/(vaccines$population_projection))*100
vaccines$percapitavaccineavail <- ((vaccines$doses_available_18th_may + vaccines$total_doses_in_pipeline_18th_may_3_days)/(vaccines$population_projection))*100
vaccines$percapitacases <- ((vaccines$daily_avg_in_last_7_days)/(vaccines$population_projection))*100

#standardise the state names
vaccines$id <- replace(vaccines$id, vaccines$id =="Jammu and Kashmir", "Jammu & Kashmir")

fortify_shape = fortify(states_shape, region = "ST_NM")

quantiles_vaccineavial<- vaccines %>%
  pull(percapitavaccineavail) %>%
  quantile(probs = seq(0, 1, length.out = 4))

quantiles_cases<- vaccines %>%
  pull(percapitacases) %>%
  quantile(probs = seq(0, 1, length.out = 4))

bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high vaccine avail, high caseload
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low vaccineavail, high caseload
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium vacc, medium caseload
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high vaccineavail, low caseload
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low vaccavail, low caseload
) %>%
  gather("numbers", "fill")

vaccines_maps <- fortify_shape %>% left_join(vaccines)

#vaccines_maps <- fortify_shape %>% 
# left_join(total_cases)

vaccines %<>%
  mutate(
    quantiles_vaccineavial = cut(
      percapitavaccineavail,
      breaks = quantiles_vaccineavial,
      include.lowest = TRUE
    ),
    quantiles_cases = cut(
      percapitacases,
      breaks = quantiles_cases,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    numbers = paste(
      as.numeric(quantiles_vaccineavial), "-",
      as.numeric(quantiles_cases)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the vaccine availability and vaccinations
  left_join(bivariate_color_scale, by = "numbers")

final.plot<- (vaccines_maps[(vaccines_maps$order), ])

map <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = fill), color = "black", size=0.15) +
  coord_map()+scale_fill_identity()

bivariate_color_scale %<>%
  separate(numbers, into = c("vaccineavail", "caseload"), sep = " - ") %>%
  mutate(vaccineavail = as.integer(vaccineavail),
         caseload = as.integer(caseload))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = vaccineavail,
      y = caseload,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Per capita vaccine availability (per 100)",
       y = "Per capita daily case load (per 100") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)

