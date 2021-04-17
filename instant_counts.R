#
# Tally up instant counts

library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


instant <- read_excel("Instantcounts_camerastatus_MASTER-copy.xlsx", sheet = "Instantaneous Counts")

active <- data.frame(Lake = unique(instant$Lake), Active = NA)
active$Active <- c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE)

instant.df <- instant %>% 
  full_join(active, by="Lake") %>% 
  mutate(yr = year(Date), mon = month(Date), 
         temp.mon.code = ifelse(mon %in% c(1:3),0,1), FY = paste0("FY",(yr+temp.mon.code)))
unique(instant.df$FY)


tot.counts <- instant.df %>% 
  group_by(Lake, FY, Season) %>% 
  summarize(length(Lake), unique(Active)) %>% 
  arrange(FY, Lake)

tot.counts


# summary instant counts by fiscal year

(FY2020 <- tot.counts %>% 
  filter(FY %in% "FY2020",`unique(Active)` %in% TRUE) %>% 
  summarize(Lake, Season, visits = `length(Lake)`) %>% 
  spread(Lake, visits))


(FY2021 <- tot.counts %>% 
  filter(FY %in% "FY2021", `unique(Active)` %in% TRUE) %>% 
  summarize(Lake, Season, visits = `length(Lake)`) %>% 
  spread(Lake, visits))


(all.FY <- tot.counts %>% 
    filter(`unique(Active)` %in% TRUE) %>% 
    summarize(Lake, Season, visits = `length(Lake)`) %>% 
    spread(Lake, visits))




#calculate when cameras were running- dates are a formatting mess! Fix first

camera1 <- read_excel("16feb2021-database_uploads_template_v8.2_camera7B-copy.xlsx", 
                     sheet = "Effort Counts",n_max=3355, 
                     col_types = c("guess","guess","text","guess","guess","guess",
                                   "guess","guess","guess","guess","guess","guess",
                                   "guess","guess","guess","guess","guess","guess",
                                   "guess","guess","guess","guess","guess","guess",
                                   "guess","guess","guess","guess","guess","guess","guess"))
fixt1 <- camera1 %>% 
  mutate(date = dmy(Date))
fixt1$date


camera2 <- read_excel("16feb2021-database_uploads_template_v8.2_camera7B-copy.xlsx", 
                      sheet = "Effort Counts",
                      col_types = c("guess","guess","guess","guess","guess","guess",
                                    "guess","guess","guess","guess","guess","guess",
                                    "guess","guess","guess","guess","guess","guess",
                                    "guess","guess","guess","guess","guess","guess",
                                    "guess","guess","guess","guess","guess","guess","guess"))

camera2 <- camera2[3356:nrow(camera2),] 
unique(camera2$Date)

fixt2 <- camera2 %>% 
  mutate(date = as.Date(as.numeric(Date), origin = "1899-12-30"))

camera <- rbind(fixt1, fixt2) %>% 
  select(-Date)





#when were cameras running on each lake?

(active.cams <- camera %>% 
  arrange(date) %>% 
  group_by(Camera_Name) %>% 
  summarize(start = first(na.omit(date)), end = last(na.omit(date))))


str(camera)

one.isl2020 <- camera %>% 
  filter(Camera_Name %in% "ONE ISLAND")


str(instant.df)

one.isl_inst <- instant.df %>% 
  filter(Lake %in% "One Island") %>% 
  mutate(dayofweek = wday(Date), weekend = ifelse(dayofweek %in% c(1,7),T,F))%>% 
  dplyr::group_by(Date, weekend) %>% 
  summarize(daily.total = sum(Total_Boats, na.rm=T), )

#need to fix - account for boats/day
ggplot(one.isl_inst)+
  geom_histogram(aes(x=Date, fill=weekend), position = "dodge", bins  = 2)



x <- seq(Sys.Date()-10, Sys.Date(), by = 1)
x[lubridate::wday(x) %in% c(1, 7)]







