## next steps include extracting only village and district level information from names, getting a unique set of values? and mapping it to existing dataset 
#First I attempt to get places associated with the tender - currently only a 1000 show up
library(dplyr)
library(stringi)
library(tidyverse)
library(janitor)
library(ggplot2)
library(plotly)
library(splitstackshape)

## read required files and clean it up
assam_published <- read.csv("assam_tenders_published.csv", header = T, stringsAsFactors = F)
assam_oc <- read.csv("assam_aoc.csv", header = T, stringsAsFactors = F)
assam_region <- read.csv("assam_tenders_region_exact_new.csv", header = T, stringsAsFactors = F)
assam_region <- clean_names(assam_region)
assam_region$tender_id <- gsub('\\s+', '', assam_region$tender_id)
assam_merge <- left_join(assam_published,assam_oc, by = "tender_id")
assam_merge <- assam_merge[ -c(35:57) ]
assam_merge <- merge(assam_merge, assam_region, by="tender_id")
assam_merge  <- assam_merge[ -c(44:77)]

write.csv(assam_merge, "assam_merge.csv")

##Extract health related procurement

assam_nhm <- assam_merge %>% filter(Org.x == "NATIONAL HEALTH MISSION")
assam_HFW <- assam_merge %>% filter(Org.x == "HEALTH AND FAMILY WELFARE DEPARTMENT")
assam_PHED <- assam_merge %>% filter(Org.x == "PUBLIC HEALTH ENGINEERING DEPARTMENT")
assam_PWDNH <- assam_merge %>% filter(Org.x == "PUBLIC WORKS BUILDING AND NH DEPARTMENT")

Keywords = c("MEDICAL","HOSPITAL","PHC","LABOUR ROOM","SURGERY","GMC","AYUSH", 
             "AYURVEDIC", "DISPENSARY", "DENTAL","AMCH","HOMEO","MATERNITY","JMCH",
             "HEALTH","ORTHOPEDICS", "HANDICAPPED","SMC","CEMETERY","CREMATION","ANGANWADI")

assam_PWDNH_health<- assam_PWDNH %>%
  filter(grepl(paste(Keywords, collapse="|"), tender_title.x))

assam_social <- assam_merge %>% filter(Org.x == "SOCIAL WELFARE DEPARTMENT")
assam_social <- filter(assam_social, grepl('ICDS', tender_reference_no.x))

assam_emergency <- assam_merge %>% filter(Org.x == "DIRECTOR- FIRE AND EMERGENCY SERVICES-ASSAM")

assam_home <- assam_merge %>% filter(Org.x == "HOME B")
assam_home <- filter(assam_home, grepl('DIETARY|SDMA', tender_title.x))

assam_health_final<- rbind(assam_nhm, assam_HFW, assam_PHED, 
                           assam_PWDNH_health, assam_social, assam_social, 
                           assam_emergency, assam_home)

## Clean the column of regions up 
assam_health_final$region <- paste(assam_health_final$names_tender_title, 
                                   assam_health_final$organisation, 
                                   assam_health_final$reference_no, sep="")

assam_health_final  <- assam_health_final[ -c(44:51)]

assam_health_final$region <- gsub("\\[|\\]", "", assam_health_final$region)
assam_health_final$region <- gsub("\\'", " ", assam_health_final$region)
assam_health_final$region <- gsub("\\,", " ", assam_health_final$region)

assam_health_final$region <- sapply(assam_health_final$region, 
               function(x) paste(unique(unlist(str_split(x," "))), collapse = " "))

assam_health_final <- assam_health_final %>% 
  separate(region, c("region1","region2", "region3","region4"), " ", extra = "merge")


##Load the census information to get list of districts in assam
assam_list1 <- read.csv("assam_list.csv", header = T)
names(assam_list1) <- assam_list1[1,]
assam_list1 <- assam_list1[-1,]
assam_list1$Name <- gsub("\\)|\\(", "", assam_list1$Name)
assam_list1$Name <- gsub("\\b[[:alpha:]]{1,3}\\b","", assam_list1$Name)
#assam_list1$Name <- removeNumbers(assam_list1$Name)
assam_list1$Name <- gsub("^ +| +$|( ) +", "\\1", assam_list1$Name)

## identify districts from the names extracted from the census information
for(i in seq_len(nrow(assam_list1))) {
  want <- grepl(assam_list1[i, "Name"], assam_health_final[, "region2"],ignore.case = T)
  assam_health_final [want, "District1"] <- assam_list1[i, "District"]
}  

for(i in seq_len(nrow(assam_list1))) {
  want <- grepl(assam_list1[i, "Name"], assam_health_final[, "region3"],ignore.case = T)
  assam_health_final [want, "District2"] <- assam_list1[i, "District"]
} 

for(i in seq_len(nrow(assam_list1))) {
  want <- grepl(assam_list1[i, "Name"], assam_health_final[, "region4"],ignore.case = T)
  assam_health_final [want, "District3"] <- assam_list1[i, "District"]
}

for(i in seq_len(nrow(assam_list1))) {
  want <- grepl(assam_list1[i, "District"], assam_health_final[, "region2"],ignore.case = T)
  assam_health_final [want, "District4"] <- assam_list1[i, "District"]
}

for(i in seq_len(nrow(assam_list1))) {
  want <- grepl(assam_list1[i, "District"], assam_health_final[, "region3"],ignore.case = T)
  assam_health_final [want, "District5"] <- assam_list1[i, "District"]
}

for(i in seq_len(nrow(assam_list1))) {
  want <- grepl(assam_list1[i, "District"], assam_health_final[, "region4"],ignore.case = T)
  assam_health_final [want, "District6"] <- assam_list1[i, "District"]
}

## Clean up of the unnecesary columns

assam_health_final  <- assam_health_final[ -c(44:47)]
assam_health_final[is.na(assam_health_final)] <- " "
assam_health_final$region <- paste(assam_health_final$District1, 
                                   assam_health_final$District2, 
                                   assam_health_final$District3,assam_health_final$District4,
                                   assam_health_final$District5, 
                                   assam_health_final$District6, sep="")

assam_health_final$region <- sapply(assam_health_final$region, 
                                    function(x) paste(unique(unlist(str_split(x," "))), collapse = " "))

#assam_health_final$district <- assam_list1$District[match(assam_health_final$region1,assam_list1$Name)]

## First identify which organizations have been spending on health related tenders

assam_health_org<- assam_health_final %>% group_by(Org.x, publishedyear.x) %>% 
  summarise(total = sum(value_of_tender_in_rs.x, na.rm = TRUE))

assam_health_org1<- assam_health_final %>% group_by(Org.x, publishedyear.x) %>% 
  tally()

ggplotly(ggplot(data = assam_health_org1, aes(x = reorder(Org.x, -n), y=n)) + 
           geom_bar(stat = "identity") + facet_grid(~publishedyear.x, scales = "free") + 
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black')))

ggplotly(ggplot(data = assam_health_org, aes(x = reorder(Org.x, -total), y=total)) + 
           geom_bar(stat = "identity") + facet_grid(~publishedyear.x, scales = "free") + 
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black')))

## Identify the status of the health tenders present in the data set
assam_health_status <- assam_health_final %>% 
  group_by(Org.x,tender_stage.x,tender_status.x, publishedyear.x) %>% 
  summarise(total = sum(value_of_tender_in_rs.x, na.rm = TRUE))

ggplotly(ggplot(data = assam_health_status, aes(x = publishedyear.x, y=total, fill = tender_status.x)) + 
           geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black')))  
  
## Identify the average numbers of bids across tenders 

assam_health_bid <- assam_health_final %>%group_by(no_of_bids_received.x,publishedyear.x) %>% tally()
assam_health_bid[] <- cbind(apply(assam_health_bid[,1], 1, function(x) ifelse(x >10, ">10",x)), assam_health_bid[,2:3])

ggplotly(assam_health_bid %>% ggplot(aes(x= factor(publishedyear.x), y = n, fill = no_of_bids_received.x)) +
           geom_bar(stat = "identity")+
           theme(axis.text.x = element_text(size = 6, angle = 90))+
           xlab("Published Year") + ylab("Number of tenders"))
           
           
#write.csv(assam_health_final, "assam_health_final.csv")
## And then manually fill out the districts that did not show up as well as 
##breakdown of data into specific topics (To be automated in the next version)
## reload modified file and clean it

assam_health_final_1 <- read.csv("assam_health_final_1_27Oct_latest.csv", header = T)
assam_health_final_1$District <- trimws(assam_health_final_1$District)

assam_region <- assam_health_final_1 %>% group_by(publishedyear.x, District) %>% tally()

##bargraph to check how many districts show up after manual replacement

ggplotly(assam_region %>% ggplot(aes(x= factor(publishedyear.x), y = n, fill = District)) +
           geom_bar(stat = "identity")+
           theme(axis.text.x = element_text(size = 6, angle = 90))+
           xlab("Published Year") + ylab("Number of tenders"))

## group health tenders by district and published year. 
assam_region_1 <- assam_health_final_1 %>% group_by(District) %>% tally()

assam_topic <- assam_health_final_1 %>% group_by(publishedyear.x, Topic) %>% tally()

## Plot a graph of tenders by topic
ggplotly(assam_topic %>% ggplot(aes(x= factor(publishedyear.x), y = n, fill = Topic)) +
           geom_bar(stat = "identity")+
           theme(axis.text.x = element_text(size = 6, angle = 90))+
           xlab("Published Year") + ylab("Number of tenders"))

## co-erce all district columns into one new colum
assam_health_final_1$Districtnew <- paste(assam_health_final_1$District, assam_health_final_1$District_1, sep=",")

## check for number of observations
allobs <- table(unlist(strsplit(as.character(assam_health_final_1$Districtnew), ',')))

## create a new data frame by tallying total number of district occurences across tenders
assam_region <- assam_health_final_1 %>%
  separate_rows(Districtnew) %>%
  group_by(Districtnew) %>% 
  count(Districtnew)

## write to csv to further use in map
write.csv(assam_region, "assam_region_2.csv")

## create a new dataframe by tally total number of AOC tenders

assam_region_aoc <- assam_health_final_1 %>%
  separate_rows(Districtnew) %>%
  group_by(tender_stage.x,Districtnew) %>% 
  count(Districtnew)

assam_region_aoc <- assam_region_aoc %>% filter(tender_stage.x == "AOC")

write.csv(assam_region_aoc, "assam_region_aoc.csv")

## create a new dataframe of organization and the districts they service most with tenders
assam_Org<- assam_health_final_1 %>%
  separate_rows(Districtnew) %>%
  group_by(Org.x,Districtnew) %>% 
  count(Districtnew)

assam_Org_tender <- assam_health_final_1 %>% 
  separate_rows(Districtnew) %>% 
  group_by (Org.x, tender_status.x, Districtnew) %>% 
  count(Districtnew)

assam_region_org_hfw <- assam_Org %>% filter(Org.x == "HEALTH AND FAMILY WELFARE DEPARTMENT")
write.csv (assam_region_org_hfw, "assam_region_org_hfw.csv")

assam_region_nhm <- assam_Org %>% filter(Org.x == "NATIONAL HEALTH MISSION")
write.csv (assam_region_nhm, "assam_nhm.csv")


## create a new dataframe of districts and the topics of tenders most of seen
assam_topic <- assam_health_final_1 %>% 
  separate_rows(Districtnew) %>% 
  group_by(Topic, Districtnew) %>% 
  count(Districtnew)

assam_region_maternal <- assam_topic %>% filter (Topic == "Maternal and child health")

assam_region_phc <- assam_topic %>% filter (Topic == "PHC")

write.csv (assam_region_maternal, "assam_maternal.csv")

write.csv (assam_region_phc, "assam_phc.csv")

##assam_health_aoc <- assam_health_final_1 %>% filter (tender_stage.x == "AOC")


key <- c("CHC","PHC","District Hospital","Geriatric Unit","Health & WC", "FRU", "Sub Divisional Hospital", "Subcentre")

assam_health <- assam_health_final_1 %>%
  filter(grepl(paste(key, collapse="|"), Topic))

assam_topic_hospital <- assam_health_aoc %>% 
  separate_rows(Districtnew) %>% 
  group_by(Districtnew) %>% 
  count(Districtnew)

write.csv(assam_topic_hospital, "assam_topic_hospital.csv")


assam_medical_education <- assam_topic %>% filter (Topic == "Medical education")
write.csv(assam_medical_education, "assam_education.csv")

key_sanit <- c("Sanitation","Water Purification","Water Supply")
assam_sanit <- assam_health_final_1 %>% filter(grepl(paste(key_sanit, collapse = "|"),Topic))

assam_topic_sanit <- assam_sanit %>% 
  separate_rows(Districtnew) %>% 
  group_by(Districtnew) %>% 
  count(Districtnew)

write.csv(assam_topic_sanit, "assam_topic_sanit.csv")


assam_covid <- assam_health_final_1 %>% filter (X.1 == "Covid")
assam_covid <- assam_covid %>% 
  separate_rows(Districtnew) %>% 
  group_by(Districtnew) %>% 
  count(Districtnew)

write.csv(assam_covid, "assam_covid.csv")

## mapping the information available

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
options(scipen = 999)

set.seed(8000)

options(scipen =999)
#setwd("~/Documents/R_things/assam_tenders_exploratory/Assam_floods")
states_shape <- st_read("2011_Dist.shp")

states_shape <- states_shape %>% filter(ST_NM == "Assam")

fortify_shape = fortify(states_shape, region = "DISTRICT")

#assam_reg <- read.csv("assam_region_1.csv", row.names = NULL,
 #                   stringsAsFactors = FALSE,  header = TRUE)

##assam_reg <- read.csv("assam_region_aoc.csv", row.names = NULL,
  ##                           stringsAsFactors = FALSE,  header = TRUE)


##assam_reg <- read.csv("assam_region_org_hfw.csv", row.names = NULL,
##stringsAsFactors = FALSE,  header = TRUE)

#assam_reg <- read.csv("assam_nhm.csv", row.names = NULL,
#stringsAsFactors = FALSE,  header = TRUE)

assam_nreg_1 <- read.csv("PG_MHC_Data.csv", row.names=NULL,
                         stringsAsFactors = FALSE, header = TRUE)

assam_reg <- read.csv ("assam_maternal.csv",row.names = NULL,
                       stringsAsFactors = FALSE,  header = TRUE)

#assam_reg <- read.csv ("assam_phc.csv",row.names = NULL,
#                     stringsAsFactors = FALSE,  header = TRUE)

#assam_reg <- read.csv ("assam_topic_hospital.csv", row.names = NULL,
 #                       stringsAsFactors = FALSE,  header = TRUE)

##assam_reg <- read.csv ("assam_education.csv",row.names = NULL,
  ##                  stringsAsFactors = FALSE,  header = TRUE)

##assam_reg <- read.csv ("assam_topic_sanit.csv",row.names = NULL,
  ##                stringsAsFactors = FALSE,  header = TRUE)

#assam_reg <- read.csv ("assam_covid.csv",row.names = NULL,
#                stringsAsFactors = FALSE,  header = TRUE)

assam_reg <- clean_names(assam_reg)
assam_nreg_1 <- clean_names(assam_nreg_1)

names(assam_reg)[3] <- paste("DISTRICT") ## change column number accordingly
names(assam_nreg_1)[1] <- paste("DISTRICT")

new_mat <- merge(assam_reg, assam_nreg_1)

quantiles_tenders <- new_mat %>%  
  pull(n) %>%
  quantile(probs = seq(0, 1, length.out = 4))

quantiles_imr <- new_mat %>%
  pull(projected_imr) %>%
  quantile(probs = seq(0, 1, length.out = 4))


bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # large tenders, large imr
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low tenders, high imr
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium tenders, medium imr
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high tenders, low imr
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low tenders, low imr
) %>%
  gather("numbers", "fill")

fortify_shape = fortify(states_shape, region = "DISTRICT")

new_mat %<>%
  mutate(
    quantiles_tenders = cut(
      n,
      breaks = quantiles_tenders,
      include.lowest = TRUE
    ),
    quantiles_imr = cut(
      projected_imr,
      breaks = quantiles_imr,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    numbers = paste(
      as.numeric(quantiles_tenders), "-",
      as.numeric(quantiles_imr)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the vaccine availability and vaccinations
  left_join(bivariate_color_scale, by = "numbers")

maps <- fortify_shape %>% left_join(new_mat)

maps_bivar<- ggplot(data = maps) + 
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = "none") + 
  geom_sf(aes(fill = fill), 
          color = "white", 
          size = 0.1) +
  scale_fill_identity()

bivariate_color_scale %<>%
  separate(numbers, into = c("MCH_Tenders", "Projected_IMR"), sep = " - ") %>%
  mutate(MCH_Tenders  = as.integer(MCH_Tenders),
         Projected_IMR = as.integer(Projected_IMR))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = MCH_Tenders,
      y = Projected_IMR,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Maternal & Child Health Tenders",
       y = "Projected IMR") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()


ggdraw() +
  draw_plot(maps_bivar, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.05, 0.2, 0.2)

maternal_data <- subset(assam_health_final_2, Topic =="Maternal and child health"| X.1 == "Vaccine")
write.csv(maternal_data, "maternal_data.csv")


#IMR

quantiles_imr_sept <- assam_nreg_1 %>%
  pull(projected_imr) %>%
  quantile(probs = seq(0, 1, length.out = 4))

univariate_color_scale <- tibble(
  "3" = "#4885C1", # low tenders, high imr
  "2" = "#BC7C8F",
  "1" = "#CABED0" # low tenders, low imr
) %>%
  gather("numbers", "fill")

assam_nreg_1 %<>%
  mutate(
    quantiles_imr = cut(
      projected_imr,
      breaks = quantiles_imr,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    numbers = paste(
      as.numeric(quantiles_imr)
    )) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the vaccine availability and vaccinations
  left_join(univariate_color_scale, by = "numbers")

maps_imr <- fortify_shape %>% left_join(assam_nreg_1)

maps_univar<- ggplot(data = maps_imr) + 
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = "none") + 
  geom_sf(aes(fill = fill), 
          color = "white", 
          size = 0.1) +
  scale_fill_identity()

#univariate_color_scale %>%
#  mutate(Projected_IMR = as.integer(numbers))

#legend <- ggplot() +
#  geom_tile(
#    data = univariate_color_scale,
#    mapping = aes(fill=fill)+
#  scale_fill_identity())

ggdraw() +
  draw_plot(maps_bivar, 0, 0, 1, 1) 
#+
# draw_plot(legend, 0.05, 0.05, 0.2, 0.2)

##Maternal & child health tenders

quantiles_tenders_sept <- assam_reg %>%
  pull(n) %>%
  quantile(probs = seq(0, 1, length.out = 4))

univariate_color_scale_tend <- tibble(
  "3" = "#AE3A4E", 
  "2" = "#BC7C8F",
  "1" = "#CABED0" 
) %>%
  gather("numbers", "fill")

assam_reg %<>%
  mutate(
    quantiles_tenders_sept = cut(
      n,
      breaks = quantiles_tenders_sept,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    numbers = paste(
      as.numeric(quantiles_tenders_sept)
    )) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the vaccine availability and vaccinations
  left_join(univariate_color_scale_tend, by = "numbers")

maps_tend <- fortify_shape %>% left_join(assam_reg)

maps_univar_tend<- ggplot(data = maps_tend) + 
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = "none") + 
  geom_sf(aes(fill = fill), 
          color = "white", 
          size = 0.1) +
  scale_fill_identity()

#univariate_color_scale %>%
#  mutate(Projected_IMR = as.integer(numbers))

#legend <- ggplot() +
#  geom_tile(
#    data = univariate_color_scale,
#    mapping = aes(fill=fill)+
#  scale_fill_identity())

ggdraw() +
  draw_plot(maps_univar, 0, 0, 1, 1)

## redraw mch_org figure

Org_mch <- MCH_data %>% group_by(Org.x,publishedyear.x) %>% tally()

Org_mch$Org.x <- replace(Org_mch$Org.x, Org_mch$Org.x=="NATIONAL HEALTH MISSION", "NHM")
Org_mch$Org.x <- replace(Org_mch$Org.x, Org_mch$Org.x=="HEALTH AND FAMILY WELFARE DEPARTMENT", "HFW")
Org_mch$Org.x <- replace(Org_mch$Org.x, Org_mch$Org.x=="PUBLIC WORKS BUILDING AND NH DEPARTMENT", "PWD-NH")


ggplotly(ggplot(data = Org_mch, aes(x = reorder(Org.x, -n), y=n)) + 
           geom_bar(stat = "identity") + facet_grid(~publishedyear.x, scales = "free") + 
           xlab("") + ylab("Number of tenders") +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black')))


