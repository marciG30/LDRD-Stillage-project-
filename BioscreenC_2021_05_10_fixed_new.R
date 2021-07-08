library(tidyverse)  
library(lubridate)
BC <- read.csv(file = "/Users/garc286/Rstudio/bioscreen_2021/for_R_Corrected_Bioscreen_shewy_reformated.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
BC2 <- gather(BC,key = "Well","OD", 2:201)
BC2$Time <- hms(BC2$Time )
BC2 <- BC2 %>% mutate(HOURS = hour(BC2$Time) + minute(BC2$Time) / 60 + second(BC2$Time) / 360)
BC2$Well <- as.factor(BC2$Well)

BC2_new <- BC2 %>% mutate(Well2 = str_remove_all(Well, "Well."))
# extract the number from "Well2".
# If the period was still in, the numbers would be parsed as 0.101, etc.
# create a new column with conditions, using "case_when"
# syntax for case_when is: CONDITION1 ~ a, CONDITION2 ~ b, etc.
BC2_new <- BC2_new %>%    mutate(Media_Type = case_when(Well2 >= 101 & Well2 <= 105 ~ "NanoWire",
                                                        Well2 >= 111 & Well2 <= 114 ~ "Water_Blank",
                                                        Well2 >= 116 & Well2 <= 120 ~ "TSB minus Dextrose",
                                                        Well2 >= 121 & Well2 <= 124 ~ "Avicel 10_01_2020",
                                                        Well2 >= 126 & Well2 <= 130 ~ "Avicel 10_01_2020_Blank",
                                                        Well2 >= 131 & Well2 <= 135 ~ "Avicel 11_9_2020",
                                                        Well2 >= 136 & Well2 <= 140 ~ "Avicel 11_9_2020_blank",
                                                        Well2 >= 141 & Well2 <= 145 ~ "Filtered, O2 Limit, Lactobacillis D.",
                                                        Well2 >= 146 & Well2 <= 150 ~ "Filtered, O2 Limit, Lactobacillis D._blank",
                                                        Well2 >= 151 & Well2 <= 155 ~ "Aerobic Lactobaccillus D.",
                                                        Well2 >= 156 & Well2 <= 160 ~ "Aerobic Lactobaccillus D._blank",
                                                        Well2 >= 161 & Well2 <= 165 ~ "Anarobic Lactobaccillius D.",
                                                        Well2 >= 165 & Well2 <= 170 ~ "Anarobic Lactobaccillius D._blank",
                                                        Well2 >= 171 & Well2 <= 175 ~ "Fodder yeast",
                                                        Well2 >= 176 & Well2 <= 180 ~ "Fodder yeast_blank",
                                                        Well2 >= 181 & Well2 <= 185 ~ "R. pallida",
                                                        Well2 >= 186 & Well2 <= 190 ~ "A. niger_blank",
                                                        Well2 >= 191 & Well2 <= 195 ~ "A. niger",
                                                        Well2 >= 196 & Well2 <= 200 ~ "R. pallida_blank",
                                                        Well2 >= 201 & Well2 <= 205 ~ "NanoWire",
                                                        Well2 >= 211 & Well2 <= 215 ~ "Water_Blank",
                                                        Well2 >= 216 & Well2 <= 220 ~ "TSB minus Dextrose",
                                                        Well2 >= 221 & Well2 <= 225 ~ "Avicel 10_01_2020",
                                                        Well2 >= 226 & Well2 <= 230 ~ "Avicel 10_01_2020_Blank",
                                                        Well2 >= 231 & Well2 <= 235 ~ "Avicel 11_9_2020",
                                                        Well2 >= 236 & Well2 <= 240 ~ "Avicel 11_9_2020_blank",
                                                        Well2 >= 241 & Well2 <= 245 ~ "Filtered, O2 Limit, Lactobacillis D.",
                                                        Well2 >= 246 & Well2 <= 250 ~ "Filtered, O2 Limit, Lactobacillis D._blank",
                                                        Well2 >= 251 & Well2 <= 255 ~ "Aerobic Lactobaccillus D.",
                                                        Well2 >= 256 & Well2 <= 260 ~ "Aerobic Lactobaccillus D._blank",
                                                        Well2 >= 261 & Well2 <= 265 ~ "Anarobic Lactobaccillius D.",
                                                        Well2 >= 265 & Well2 <= 270 ~ "Anarobic Lactobaccillius D._blank",
                                                        Well2 >= 271 & Well2 <= 275 ~ "Fodder yeast",
                                                        Well2 >= 276 & Well2 <= 280 ~ "Fodder yeast_blank",
                                                        Well2 >= 281 & Well2 <= 285 ~ "R. pallida",
                                                        Well2 >= 286 & Well2 <= 290 ~ "A. niger_blank",
                                                        Well2 >= 291 & Well2 <= 295 ~ "R. pallida",
                                                        Well2 >= 296 & Well2 <= 300 ~ "A. niger_blank",)) 

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

ggsave("shewanella_OD_stillages_21_05_10_combined.tiff")

BC2_new_kp %>% ggplot(aes(x=HOURS, y=OD_corr, color = Media_name))+geom_point()+xlab("Time (Hours)")+ylab("Optical Density @ 600 nm")
ggsave("shewanella_OD_stillages_21_05_10_combined_1graph.tiff")
