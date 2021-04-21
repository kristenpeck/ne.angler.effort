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
         temp.mon.code = ifelse(mon %in% c(1:3),0,1), FY = paste0("FY",(yr+temp.mon.code))) %>% 
  select(-c(temp.mon.code))
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

str(camera)

# summarize camera data so it is comparable to the instant counts
#  First, match instant count categories

unique(camera$Lake_Condition)

camera.df <- camera %>% 
  mutate(Season = ifelse(Lake_Condition %in% "Ice covered", "ice", 
                         ifelse(Lake_Condition %in% "Open", "openwater",
                                ifelse(Lake_Condition %in% "Ice covered - partial", "shoulder",NA)))) %>% 
  mutate(Total_Boats = sum(Boats_1Angler,Boats_2Anglers,Boats_3Anglers,Boats_4Anglers,Boats_5Anglers,
                           Boats_Unknown, Boats_NOT_Fishing)) %>% 
  select(-c(Assessment_ID,Method,Camera_Type,Picture_Name,Sampler,Folder_Name,Page_ID,Time_Arrive,
            Time_Leave))




# when were cameras running on each lake?

(active.cams <- camera.df %>% 
  arrange(date) %>% 
  group_by(Camera_Name) %>% 
  summarize(start = first(na.omit(date)), end = last(na.omit(date))))



one.isl_cam <- camera.df %>% 
  filter(Camera_Name %in% "ONE ISLAND")


str(instant.df)
unique(instant.df$Season)

(one.isl_inst <- instant.df %>% 
  filter(Lake %in% "One Island", Season %in% "openwater") %>% 
  mutate(dayofweek = wday(Date,label=T,abbr=T), weekend = ifelse(dayofweek %in% c("Sat","Sun"),T,F),
         year=year(Date))%>% 
  dplyr::group_by(year, Date, dayofweek, weekend) %>% 
  summarize(daily.total = sum(Total_Boats, na.rm=T), ))



(one.isl_inst.sum <- one.isl_inst %>% 
  group_by(year, weekend, dayofweek) %>% 
  summarize(total.boats = sum(daily.total), num.days = length(daily.total), 
            ave.per.day = total.boats/num.days))

ggplot(data=one.isl_inst[one.isl_inst$year %in% "2020",])+
  geom_point(aes(x=Date,y=daily.total, col=weekend))


#sketch out schedule for creel technicians:
#n = 20 -> 15 on the weekends, 5 on weekdays from May 15th to Sept. 30th


date <- seq(dmy("15-May-2021"),dmy("30-Sep-2021"),1)

creel.sched <- data.frame(date, yday = NA, dayofweek = NA, weekend = NA)

(creel.sched <- creel.sched %>% 
  mutate(yday = yday(date), dayofweek = wday(date, label=T, abbr=T),
         weekend = ifelse(dayofweek %in% c("Sat","Sun"),T,F)))

(weekends <- creel.sched %>% 
  filter(weekend %in% "TRUE") %>% 
  mutate(sample = sample(yday, 20, replace=F))
  )

sample.weekend = sample(weekends$date, 20, replace=F)
sample.weekend[order(sample.weekend)]

