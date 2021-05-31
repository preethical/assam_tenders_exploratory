library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(stringr)

setwd("~/Documents/R_things/assam_tenders_exploratory")
#import, merge, remove duplicates and clean each sheet
assam_files <- list.files(pattern = "*.xls")

#published
assam_tenders_published <- lapply(assam_files, function(i){
  assam_yearly = read_excel(path = i, sheet = 1, skip = 1, col_names = T)
  assam_yearly$file = i
  assam_yearly
})
assam_tenders_published<- do.call("bind_rows", assam_tenders_published)
assam_tenders_published<- 
  assam_tenders_published[!duplicated(assam_tenders_published[2]),]

assam_tenders_published <- clean_names(assam_tenders_published)
assam_tenders_published$tender_title <- toupper(assam_tenders_published$tender_title)
assam_tenders_published$organisation_chain <- toupper(assam_tenders_published$organisation_chain)
assam_tenders_published$published_date <- as.Date(assam_tenders_published$published_date, 
                                                  format="%d-%b-%Y")
assam_tenders_published$pre_bid_meeting_date <- as.Date(assam_tenders_published$pre_bid_meeting_date, 
                                                        format="%d-%b-%Y")

assam_tenders_published$bid_opening_date<- as.Date(assam_tenders_published$bid_opening_date, 
                                                   format="%d-%b-%Y")

assam_tenders_published$price_bid_opening_date <- as.Date(assam_tenders_published$price_bid_opening_date, 
                                                          format="%d-%b-%Y")

assam_tenders_published$publishedyear <- year(assam_tenders_published$published_date)

#assam_tenders_published <- assam_tenders_published %>% separate(organisation_chain, into = c("Org","role"), sep = "\\|\\|", extra = "merge")


#write.csv(assam_tenders_published, "assam_tenders_published.csv")

#assam_tenders_org <- assam_tenders_published %>% group_by(Org, publishedyear) %>% summarise(total = sum(value_of_tender_in_rs, na.rm = TRUE)) %>% top_n(n = 5, wt = total)

#tenders <- assam_tenders_org %>% top_n(5) %>% ggplot()+ aes(x = publishedyear, y=total)+ geom_bar(stat = "identity", position = "dodge",width = 0.7)

#ggplot(assam_tenders_org, aes(x = Org, y = total)) +
#         geom_col() +
#         facet_grid(.~publishedyear, scales = "free")

#in Progress
assam_tenders_progress <- lapply(assam_files, function(i){
  assam_yearly = read_excel(path = i, sheet = 2, skip = 1, col_names = T)
  assam_yearly$file = i
  assam_yearly
})
assam_tenders_progress<- do.call("bind_rows", assam_tenders_progress)

assam_tenders_progress<- 
  assam_tenders_progress[!duplicated(assam_tenders_progress[2]),]

assam_tenders_progress <- clean_names(assam_tenders_progress)
assam_tenders_progress$tender_title <- toupper(assam_tenders_progress$tender_title)
assam_tenders_progress$organisation_chain <- toupper(assam_tenders_progress$organisation_chain)
assam_tenders_progress$published_date <- as.Date(assam_tenders_progress$published_date, 
                                                  format="%d-%b-%Y")
assam_tenders_progress$pre_bid_meeting_date <- as.Date(assam_tenders_progress$pre_bid_meeting_date, 
                                                        format="%d-%b-%Y")

assam_tenders_progress$bid_opening_date<- as.Date(assam_tenders_progress$bid_opening_date, 
                                                   format="%d-%b-%Y")

assam_tenders_progress$price_bid_opening_date <- as.Date(assam_tenders_progress$price_bid_opening_date, 
                                                          format="%d-%b-%Y")

assam_tenders_progress$publishedyear <- year(assam_tenders_progress$published_date)


#noresponse
assam_tenders_noresp <- lapply(assam_files, function(i){
  assam_yearly = read_excel(path = i, sheet = 3, skip = 1, col_names = T)
  assam_yearly$file = i
  assam_yearly
})
assam_tenders_noresp<- do.call("bind_rows", assam_tenders_noresp)
assam_tenders_noresp<- 
  assam_tenders_noresp[!duplicated(assam_tenders_noresp[2]),]
assam_tenders_noresp <- clean_names(assam_tenders_noresp)

assam_tenders_noresp$tender_title <- toupper(assam_tenders_noresp$tender_title)
assam_tenders_noresp$organisation_chain <- toupper(assam_tenders_noresp$organisation_chain)
assam_tenders_noresp$published_date <- as.Date(assam_tenders_noresp$published_date, 
                                                 format="%d-%b-%Y")
assam_tenders_noresp$pre_bid_meeting_date <- as.Date(assam_tenders_noresp$pre_bid_meeting_date, 
                                                       format="%d-%b-%Y")

assam_tenders_noresp$bid_opening_date<- as.Date(assam_tenders_noresp$bid_opening_date, 
                                                  format="%d-%b-%Y")

assam_tenders_noresp$publishedyear <- year(assam_tenders_noresp$published_date)

#aoc
assam_tenders_aoc <- lapply(assam_files, function(i){
  assam_yearly = read_excel(path = i, sheet = 4, skip = 1, col_names = T)
  assam_yearly$file = i
  assam_yearly
})
assam_tenders_aoc<- do.call("bind_rows", assam_tenders_aoc)
assam_tenders_aoc<- 
  assam_tenders_aoc[!duplicated(assam_tenders_aoc[2]),]

assam_tenders_aoc <- clean_names(assam_tenders_aoc)

assam_tenders_aoc$tender_title <- toupper(assam_tenders_aoc$tender_title)
assam_tenders_aoc$organisation_chain <- toupper(assam_tenders_aoc$organisation_chain)
assam_tenders_aoc$published_date <- as.Date(assam_tenders_aoc$published_date, 
                                               format="%d-%b-%Y")
assam_tenders_aoc$pre_bid_meeting_date <- as.Date(assam_tenders_aoc$pre_bid_meeting_date, 
                                                     format="%d-%b-%Y")

assam_tenders_aoc$bid_opening_date<- as.Date(assam_tenders_aoc$bid_opening_date, 
                                                format="%d-%b-%Y")
assam_tenders_aoc$date_of_award_of_contract <- as.Date(assam_tenders_aoc$date_of_award_of_contract,
                                                       format="%d-%b-%Y" )
assam_tenders_aoc$price_bid_opening_date<- as.Date(assam_tenders_aoc$price_bid_opening_date,
                                                       format="%d-%b-%Y" )

assam_tenders_aoc$publishedyear <- year(assam_tenders_aoc$published_date)

assam_tenders_merge<- merge(assam_tenders_published, 
                                assam_tenders_aoc[c("date_of_award_of_contract","awarded_price_in_rs")], 
                                all.x = T)



assam_tenders_published<- 
  assam_tenders_published[!duplicated(assam_tenders_published[2]),]


assam_tenders_published<- assam_tenders_published %>% separate(organisation_chain, into = c("Org","role"), sep = "\\|\\|", extra = "merge")
assam_tenders_published$aocdate<- assam_tenders_published$date_of_award_of_contract - assam_tenders_published$published_date

#merge all these sheets to produce a master one


#assam_tender_merged <- do.call ("merge", list(assam_tender_merged, assam_tenders_noresp))
#assam_tender_merged <- do.call("merge", list(assam_tender_merged,assam_tenders_aoc))

assam_tenders_published<- 
  assam_tenders_published[!duplicated(assam_tenders_published[2]),] 
  
write.csv(assam_tender_merged, "assam_merged.csv")
write.csv(assam_tenders_aoc, "assam_aoc.csv")
