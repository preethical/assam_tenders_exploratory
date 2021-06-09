## next steps include extracting only village and district level information from names, getting a unique set of values? and mapping it to existing dataset 
#First I attempt to get places associated with the tender - currently only a 1000 show up
library(dplyr)
library(stringi)
library(tidyverse)
assam_published <- read.csv("assam_tenders_published.csv", header = T, stringsAsFactors = F)
assam_list <- read.csv("Census Combined (all in one sheet) - All together.csv",  header=T, stringsAsFactors = F)
assam_list1 <- read.csv("assam_list.csv", stringsAsFactors = F)

##Then I Extract the medical tender data

text = c("NATIONAL HEALTH MISSION", "HEALTH AND FAMILY WELFARE DEPARTMENT")
assam_health<- filter(assam_published, grepl('NATIONAL HEALTH MISSION|HEALTH AND FAMILY WELFARE DEPARTMENT', Org))

assam_health1 <- filter(assam_published, grepl('NH DEPARTMENT', Org))

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

assam_health2 <- filter(assam_published, grepl('SOCIAL WELFARE DEPARTMENT', Org))
assam_health2 <- filter(assam_published, grepl('ICDS', tender_reference_no))

assam_health3 <- filter(assam_published, grepl('RASHTRIYA MADHYAMIK SIKSHA ABHIJAN', Org))
assam_health3 <- filter(assam_published, grepl('Healthcare', tender_title))

assam_health_final<- rbind(assam_health, assam_health1, assam_health2, assam_health3)

