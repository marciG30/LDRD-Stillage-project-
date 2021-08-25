library(tidyverse)  
library(lubridate)
BC <- read.csv(file = "data/BioscreenExperiment_shewy_8_7_21.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
BC2 <- gather(BC,key = "Well","OD", 2:201)
BC2$Time <- hms(BC2$Time )
BC2 <- BC2 %>% mutate(HOURS = hour(BC2$Time) + minute(BC2$Time) / 60 + second(BC2$Time) / 360)
BC2$Well <- as.factor(BC2$Well)

BC2_new <- BC2 %>% mutate(Well2 = str_remove_all(Well, "Well."))
# extract the number from "Well2".
# If the period was still in, the numbers would be parsed as 0.101, etc.
# create a new column with conditions, using "case_when"
# syntax for case_when is: CONDITION1 ~ a, CONDITION2 ~ b, etc.
BC2_new <- BC2_new %>%    mutate(Media_Type = case_when(Well2 >= 101 & Well2 <= 110 ~ "A.niger",
                                                        Well2 >= 111 & Well2 <= 120 ~ "A.niger_Blank",
                                                        Well2 >= 121 & Well2 <= 130 ~ "A.niger_wt",
                                                        Well2 >= 131 & Well2 <= 140 ~ "A.niger_wt_Blank",
                                                        Well2 >= 141 & Well2 <= 150 ~ "Lacto D. O2 limit",
                                                        Well2 >= 151 & Well2 <= 160 ~ "Lacto D. O2 limit_Blank",
                                                        Well2 >= 161 & Well2 <= 170 ~ "Yarrowin Lipo",
                                                        Well2 >= 171 & Well2 <= 180 ~ "Yarrowin Lipo_Blank",
                                                        Well2 >= 181 & Well2 <= 190 ~ "NanoWire",
                                                        Well2 >= 191 & Well2 <= 200 ~ "H20",
                                                        Well2 >= 201 & Well2 <= 210 ~ "lacto D.aerobic",
                                                        Well2 >= 211 & Well2 <= 220 ~ "lacto D.aerobic_Blank",
                                                        Well2 >= 221 & Well2 <= 230 ~ "Fodder yeast",
                                                        Well2 >= 231 & Well2 <= 240 ~ "Fodder yeast_blank",
                                                        Well2 >= 241 & Well2 <= 250 ~ "lacto D. anaerobic",
                                                        Well2 >= 251 & Well2 <= 260 ~ "lacto D. anaerobic_Blank",
                                                        Well2 >= 261 & Well2 <= 270 ~ "Nanowire",
                                                        Well2 >= 271 & Well2 <= 280 ~ "Nanowire_blank",
                                                        Well2 >= 281 & Well2 <= 290~ "H20",
                                                        Well2 >= 291 & Well2 <= 300 ~ "H20",))
rm(BC)
rm(BC2)


ODaverages <- BC2_new %>% group_by(Media_Type,HOURS) %>% summarize(AveOD = mean(OD), STDEV = sd(OD))
ODaverages$Media_Type <- as.factor(ODaverages$Media_Type)

ODaverages %>% ggplot(aes(x=HOURS,y=AveOD))+geom_point()+facet_wrap(facets = ODaverages$Media_Type, scales = "free") +ggtitle("Shewanella W3-18-1 Growth On Various Media using Bioscreen C")+xlab("Time (hours)") +ylab("Optical Density @ 600 nm") #indivualized scales
ODaverages %>% ggplot(aes(x=HOURS,y=AveOD))+geom_point()+facet_wrap(facets = ODaverages$Media_Type) +ggtitle("Shewanella W3-18-1 Growth On Various Media using Bioscreen C") +xlab("Time (hours)") +ylab("Optical Density @ 600 nm") #all same scale
ODaverages %>% ggplot(aes(x=HOURS,y=AveOD,color=Media_Type))+geom_point()+ylim(0,1.5)+ggtitle("Shewanella W3-18-1 Growth On Various Media using Bioscreen C") +xlab("Time (hours)") +ylab("Optical Density @ 600 nm")



BC2_new_kp = 
  BC2_new %>% 
  # calculate average per media
  group_by(Time, HOURS, Media_Type) %>% 
  dplyr::summarise(OD_mean = mean(OD)) %>% 
  mutate(Media_Type = str_replace_all(Media_Type, "Blank", "blank"),
         blank = if_else(grepl("blank", Media_Type), "blank", "sample")) %>% 
  mutate(Media_name = str_remove_all(Media_Type, "_blank")) %>% 
  dplyr::select(-Media_Type) %>% 
  pivot_wider(
              names_from = blank,
              values_from = OD_mean) %>%
  filter(!is.na(Media_name)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(OD_corr = sample - blank,
         OD_corr = if_else(Media_name == "Water", abs(OD_corr), OD_corr)) 
  
BC2_new_kp %>% 
  ggplot(aes(x=HOURS,y=OD_corr))+
  geom_point(size = 0.1)+
  #ylim(0,1.5)+
  ggtitle("Shewanella W3-18-1 Growth On Various Media using Bioscreen C") +
  xlab("Time (hours)") +
  ylab("Optical Density @ 600 nm")+
  facet_wrap(~Media_name)

ggsave("output/shewanella_OD_stillages_21_08_07_combined.tiff")

BC2_new_kp %>% ggplot(aes(x=HOURS, y=OD_corr, color = Media_name))+geom_point()+xlab("Time (Hours)")+ylab("Optical Density @ 600 nm")
ggsave("shewanella_OD_stillages_21_05_10_combined_1graph.tiff")
