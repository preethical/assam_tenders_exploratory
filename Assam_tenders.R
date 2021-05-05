library(tidyverse)
library(lubridate)
#readall the excel files
Assamtender_201617_published <- readxl::read_excel("TenderDetailsExcel2016_17.xls", sheet=1, skip = 1,col_names = TRUE)
Assamtender_201617_progress <- readxl::read_excel("TenderDetailsExcel2016_17.xls", sheet=2, skip = 1,col_names = TRUE)
Assamtender_201617_noresp <- readxl::read_excel("TenderDetailsExcel2016_17.xls", sheet=3, skip = 1,col_names = TRUE)
Assamtender_201617_aoc <- readxl::read_excel("TenderDetailsExcel2016_17.xls", sheet=4, skip = 1,col_names = TRUE)

Assamtender_201718_published <- readxl::read_excel("TenderDetailsExcel2017_18.xls", sheet=1, skip = 1,col_names = TRUE)
Assamtender_201718_progress <- readxl::read_excel("TenderDetailsExcel2017_18.xls", sheet=2, skip = 1,col_names = TRUE)
Assamtender_201718_noresp <- readxl::read_excel("TenderDetailsExcel2017_18.xls", sheet=3, skip = 1,col_names = TRUE)
Assamtender_201718_aoc <- readxl::read_excel("TenderDetailsExcel2017_18.xls", sheet=4, skip = 1,col_names = TRUE)

Assamtender_201819_published <- readxl::read_excel("TenderDetailsExcel2018_19.xls", sheet=1, skip = 1,col_names = TRUE)
Assamtender_201819_progress <- readxl::read_excel("TenderDetailsExcel2018_19.xls", sheet=2, skip = 1,col_names = TRUE)
Assamtender_201819_noresp <- readxl::read_excel("TenderDetailsExcel2018_19.xls", sheet=3, skip = 1,col_names = TRUE)
Assamtender_201819_aoc <- readxl::read_excel("TenderDetailsExcel2018_19.xls", sheet=4, skip = 1,col_names = TRUE)

Assamtender_201920_published <- readxl::read_excel("TenderDetailsExcel2019_20.xls", sheet=1, skip = 1,col_names = TRUE)
Assamtender_201920_progress <- readxl::read_excel("TenderDetailsExcel2019_20.xls", sheet=2, skip = 1,col_names = TRUE)
Assamtender_201920_noresp <- readxl::read_excel("TenderDetailsExcel2019_20.xls", sheet=3, skip = 1,col_names = TRUE)
Assamtender_201920_aoc <- readxl::read_excel("TenderDetailsExcel2019_20.xls", sheet=4, skip = 1,col_names = TRUE)

Assamtender_202021_published <- readxl::read_excel("TenderDetailsExcel2020_21.xls", sheet=1, skip = 1,col_names = TRUE)
Assamtender_202021_progress <- readxl::read_excel("TenderDetailsExcel2020_21.xls", sheet=2, skip = 1,col_names = TRUE)
Assamtender_202021_noresp <- readxl::read_excel("TenderDetailsExcel2020_21.xls", sheet=3, skip = 1,col_names = TRUE)
Assamtender_202021_aoc <- readxl::read_excel("TenderDetailsExcel2020_21.xls", sheet=4, skip = 1,col_names = TRUE)

Assamtender_published <- rbind(Assamtender_201617_published,Assamtender_201718_published,Assamtender_201819_published,Assamtender_201920_published,Assamtender_202021_published)
Assamtender_published<- 
  Assamtender_published[!duplicated(Assamtender_published[2]),]

Assamtender_progress <- rbind(Assamtender_201617_progress,Assamtender_201718_progress,Assamtender_201819_progress,Assamtender_201920_progress,Assamtender_202021_progress)
Assamtender_progress<- 
  Assamtender_progress[!duplicated(Assamtender_progress[2]),]

Assamtender_noresp <- rbind(Assamtender_201617_noresp,Assamtender_201718_noresp,Assamtender_201819_noresp,Assamtender_201920_noresp,Assamtender_202021_noresp)
Assamtender_noresp<- 
  Assamtender_noresp[!duplicated(Assamtender_noresp[2]),]

Assamtender_aoc <- rbind(Assamtender_201617_aoc,Assamtender_201718_aoc,Assamtender_201819_aoc,Assamtender_201920_aoc,Assamtender_202021_aoc)
Assamtender_aoc<- 
  Assamtender_aoc[!duplicated(Assamtender_aoc[2]),]

Assamtendermerged <- merge(Assamtender_published, Assamtender_aoc,all = TRUE)
tendermerge1 <- merge(Assamtendermerged, Assamtender_progress, all = TRUE)
Finalmerge <- merge(tendermerge1, Assamtender_noresp, all = TRUE)
Finalmerge<- 
  Finalmerge[!duplicated(Finalmerge[2]),]

Finalmerge$`Published Date` <- as.Date(Finalmerge$`Published Date`, 
                                      format="%d-%b-%Y")
Finalmerge$`PreBid Meeting Date` <- as.Date(Finalmerge$`PreBid Meeting Date`, 
                                    format="%d-%b-%Y")

Finalmerge$`Price Bid Opening Date` <- as.Date(Finalmerge$`Price Bid Opening Date`, 
                                          format="%d-%b-%Y")

Finalmerge$`Stage/Status updated on` <- as.Date(Finalmerge$`Stage/Status updated on`, 
                                            format="%d-%b-%Y")

Finalmerge$`Date of Award Of Contract` <- as.Date(Finalmerge$`Date of Award Of Contract`, 
                                             format="%d-%b-%Y")

Finalmerge$publishedyear <- year(Finalmerge$`Published Date`)

org_plot<- ggplot(Finalmerge, aes(x=Finalmerge$`Organisation Chain`, y= tally()))+ 
  geom_boxplot() + theme(axis.text.x = element_text(size = 6, angle = 90))
