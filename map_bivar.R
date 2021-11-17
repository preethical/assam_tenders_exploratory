library(sf)
library(tidyverse)
library(ggplot2)
library(plotly)
library(psych)
library(janitor)
library(stringr)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(rgdal)
library(ggthemes)
library(plyr)
library(biscale)
library(cowplot)
library(networkD3)
library(dplyr)
options(scipen = 999)

set.seed(8000)

options(scipen =999)
#setwd("~/Documents/R_things/assam_tenders_exploratory/Assam_floods")
states_shape <- st_read("2011_Dist.shp")

states_shape <- states_shape %>% filter(ST_NM == "Assam")

fortify_shape = fortify(states_shape, region = "DISTRICT")

assam_reg <- read.csv ("assam_maternal.csv",row.names = NULL,
                       stringsAsFactors = FALSE,  header = TRUE)

assam_reg <- clean_names(assam_reg)
names(assam_reg)[3] <- paste("DISTRICT")

fortify_shape = fortify(states_shape, region = "DISTRICT")

maps <- fortify_shape %>% left_join(assam_reg)

ggplot(data = maps) + geom_sf(aes(fill = n)) +
                     scale_fill_viridis_c(trans = "sqrt", alpha = .4)


assam_MCH <- read.csv("maternal_data_final.csv",row.names = NULL, stringsAsFactors = FALSE, header = TRUE)

## Bargraph
assam_MCH_health_org <- assam_MCH %>% group_by(Org.x, publishedyear.x) %>% tally()

ggplot(data = assam_MCH_health_org, aes(x = reorder(Org.x, -n), y=n)) + geom_bar(stat = "identity") + 
  facet_grid(~publishedyear.x, scales = "free") + 
  theme(axis.text.x = element_text (angle=90,hjust=1,vjust=.5,colour='black'))+ 
  scale_x_discrete(name = "") + scale_y_continuous(name="Number of tenders published", limits=c(0, 60))

assam_MCH_health_schemes <- assam_MCH %>% group_by(Scheme, publishedyear.x) %>% tally()

ggplot(data = assam_MCH_health_schemes, aes(x = reorder(Scheme, -n), y=n)) + geom_bar(stat = "identity") + 
  facet_grid(~publishedyear.x, scales = "free") + 
  theme(axis.text.x = element_text (angle=90,hjust=1,vjust=.5,colour='black'))+ 
  scale_x_discrete(name = "") + scale_y_continuous(name="Number of tenders published", limits=c(0, 60))


## Sankey

results <- assam_MCH %>% group_by(publishedyear.x, Scheme) %>% tally()
tender_age <- unique(as.character(results$publishedyear.x))
tender_scheme <- unique(as.character(results$Scheme))
#tender_category <- unique(as.character(results$product_category.x))
nodes <- data.frame(node = c(0:18), name = c(tender_age,tender_scheme))

results <- merge(results, nodes, by.x = "Scheme", by.y = "name")
#results <- merge(results, nodes, by.x = "product_category.x", by.y = "name")
results <- merge(results, nodes, by.x = "publishedyear.x", by.y = "name")
links <- results[ , c("node.x", "node.y", "n")]
colnames(links) <- c("source","target","value")

sankeyNetwork(Links = links, Nodes = nodes, 
              Source = 'source', 
              Target = 'target', 
              Value = 'value', 
              NodeID = 'name',
              units = 'n')

results_1 <- assam_MCH %>% group_by (product_category.x, Scheme) %>% tally () 
tender_cat <- unique(as.character(results_1$product_category.x))
tender_scheme <- unique(as.character(results_1$Scheme))
nodes <- data.frame(node = c(0:24), name = c(tender_cat,tender_scheme))

results_1 <- merge(results_1, nodes, by.x = "Scheme", by.y = "name")
#results <- merge(results, nodes, by.x = "product_category.x", by.y = "name")
results_1 <- merge(results_1, nodes, by.x = "product_category.x", by.y = "name")
links <- results_1[ , c("node.x", "node.y", "n")]
colnames(links) <- c("source","target","value")

sankeyNetwork(Links = links, Nodes = nodes, 
              Source = 'source', 
              Target = 'target', 
              Value = 'value', 
              NodeID = 'name',
              units = 'n')