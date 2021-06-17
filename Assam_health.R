## next steps include extracting only village and district level information from names, getting a unique set of values? and mapping it to existing dataset 
#First I attempt to get places associated with the tender - currently only a 1000 show up
library(dplyr)
library(stringi)
library(tidyverse)
library(janitor)
assam_published1 <- read.csv("assam_tenders_published.csv", header = T, stringsAsFactors = F)
assam_list <- read.csv("Census Combined (all in one sheet) - All together.csv",  header=T, stringsAsFactors = F)
assam_list1 <- read.csv("assam_list.csv", stringsAsFactors = F)
assam_published <- read.csv("assam_tenders_region_exact_new - assam_tenders_region_exact_new.csv", header = T, stringsAsFactors = F)
assam_published <- clean_names(assam_published)
##Then I Extract the medical tender data

text = c("NATIONAL HEALTH MISSION", "HEALTH AND FAMILY WELFARE DEPARTMENT")
assam_health<- filter(assam_published, grepl('NATIONAL HEALTH MISSION|HEALTH AND FAMILY WELFARE DEPARTMENT', org))

assam_health1 <- filter(assam_published, grepl('NH DEPARTMENT', org))

Keywords <- c(
  "MEDICAL COLLEGE",
  "Dispensary",
  "LABOUR ROOM",
  "PHC",
  "CHC",
  "Pharmacy",
  "GMC",
  "SURGERY",
  "NURSING COLLEGE",
  "Ayush",
  "AYURVEDIC",
  "Morgue",
  "ORTHOPAEDICS",
  "ORTHOPEDICS",
  "Nurse",
  "Homeo",
  "MATERNITY",
  "CLINICAL",
  "JMCH",
  "IVF CENTRE",
  "MEDICAL GAS",
  "ANGANWADI",
  "CREMATION",
  "HOSPITAL",
  "DOCTOR",
  "GNM")

assam_health1 <- assam_health1 %>%
  filter(grepl(paste(Keywords, collapse="|"), tender_title))

assam_health2 <- filter(assam_published, grepl('SOCIAL WELFARE DEPARTMENT', org))
assam_health2 <- filter(assam_published, grepl('ICDS', tender_reference_no))

assam_health3 <- filter(assam_published, grepl('RASHTRIYA MADHYAMIK SIKSHA ABHIJAN', org))
assam_health3 <- filter(assam_published, grepl('Healthcare', tender_title))

assam_health_final<- rbind(assam_health, assam_health1, assam_health2, assam_health3)

write.csv(assam_health_final, "assam_health_final.csv")


assam_flood1 <- filter(assam_published, grepl('DEPARTMENT OF WATER RESOURCES|BODOLAND TERRITORIAL COUNCIL - WRD|SOIL CONSERVATION DEPARTMENT|DIRECTOR -  FIRE AND EMERGENCY SERVICES - ASSAM', org))
write.csv(assam_flood1, "assam_floods.csv")
