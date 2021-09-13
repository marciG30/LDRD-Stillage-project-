library(tidyverse)  
library(lubridate)
BC <- read.csv(file = "data_bioscreen/21_08_27_shewy_variant_BioscreenExperiment.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
BC2 <- gather(BC,key = "Well","OD", 2:201)
BC2$Time <- hms(BC2$Time )
BC2 <- BC2 %>% mutate(HOURS = hour(BC2$Time) + minute(BC2$Time) / 60 + second(BC2$Time) / 360)
BC2$Well <- as.factor(BC2$Well)

BC2_new <- BC2 %>% mutate(Well2 = str_remove_all(Well, "Well."))
# extract the number from "Well2".
# If the period was still in, the numbers would be parsed as 0.101, etc.
# create a new column with conditions, using "case_when"
# syntax for case_when is: CONDITION1 ~ a, CONDITION2 ~ b, etc.
BC2_new <- BC2_new %>%    mutate(Media_Type = case_when(Well2 >= 101 & Well2 <= 110 ~ "sodium sulfate",
                                                        Well2 >= 111 & Well2 <= 120 ~ "sodium sulfate_blank",
                                                        Well2 >= 121 & Well2 <= 130 ~ "amonium nitrate",
                                                        Well2 >= 131 & Well2 <= 140 ~ "amonium nitrate_blank",
                                                        Well2 >= 141 & Well2 <= 150 ~ "dipotasium phosphate",
                                                        Well2 >= 151 & Well2 <= 160 ~ "dipotasium phosphate_Blank",
                                                        Well2 >= 161 & Well2 <= 170 ~ "ferric citrate",
                                                        Well2 >= 171 & Well2 <= 180 ~ "ferric citrate _Blank",
                                                        Well2 >= 191 & Well2 <= 200 ~ "H20",
                                                        Well2 >= 201 & Well2 <= 210 ~ "formate",
                                                        Well2 >= 211 & Well2 <= 220 ~ "formate_Blank",
                                                        Well2 >= 221 & Well2 <= 230 ~ "pyruvate",
                                                        Well2 >= 231 & Well2 <= 240 ~ "pyruvate_Blank",
                                                        Well2 >= 241 & Well2 <= 250 ~ "nothing added",
                                                        Well2 >= 251 & Well2 <= 260 ~ "nothing added _Blank",
                                                        Well2 >= 261 & Well2 <= 270 ~ "everything added",
                                                        Well2 >= 271 & Well2 <= 280 ~ "everything added_blank",
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
         OD_corr = if_else(Media_name == "Water", abs(OD_corr), OD_corr))+
  mutate(OD_corr = if_else(OD_corr < 0, 0, OD_corr)

BC2_new_kp %>% 
  ggplot(aes(x=HOURS,y=OD_corr))+
  geom_point(size = 0.5)+
  ylim(0,1.5)+
  ggtitle("Shewanella W3-18-1 Growth nutriant limited On A. niger stillage") +
  xlab("Time (hours)") +
  ylab("Optical Density @ 600 nm")+
  facet_wrap(~Media_name)

ggsave("output/Shewanella W3-18-1 Growth nutriant limited_21_08_27_combined.tiff")

BC2_new_kp %>% ggplot(aes(x=HOURS, y=OD_corr, color = Media_name))+geom_point()+xlab("Time (Hours)")+ylab("Optical Density @ 600 nm")
ggsave("Shewanella W3-18-1 Growth nutriant limited_21_08_27_combined_1graph.tiff")
