## R processing script for SquidStat

## YOU ONLY HAVE TO SET THE CHRONO FILE PATHS AND THE OUTPUT FIGURE FILE PATHS (STEP 0)
## DO NOT CHANGE ANYTHING ELSE IN THIS SCRIPT.

## This script "sources" the processing functions in "code/processing_functions.R", and will run them here.
## Just run this script (Step 1, Step 2) as is, and it will process your data, and plot and save the graphs

## 2021-02-05 Kaizad F. Patel, Marci Garcia
## 2021-03-05 update: converted into functions for a more streamlined workflow
## 2021-05-07 update: updating for a semi-automated workflow

###################### #
###################### #

## STEP 0: Set the input file paths and the output figure paths
currentDate = Sys.Date()

## set file path
DATA_PATH = "data"
FIGURE_PATH_CURRENT = paste0("figures/", currentDate, "chrono_current.png") # this will automatically add the current date to the output file name
FIGURE_PATH_CURRENTDENSITY = paste0("figures/", currentDate, "chrono_currentdensity.png")
FIGURE_PATH_H2 = paste0("figures/", currentDate, "chrono_H2.png")

#
# step 1: process the data -----------------------------------------------
source("2-functions.R")

processed_data = process_chrono_files(DATA_PATH)
chrono_processed = processed_data
(chrono_graph_current = plot_chrono_graph_current(processed_data)) 
(chrono_graph_current_density = plot_chrono_graph_currentdensity(processed_data))
(chrono_graph_H2 = plot_chrono_graph_H2(processed_data))

# step 2: save the graphs -------------------------------------------------
ggsave(plot = chrono_graph_current, FIGURE_PATH_CURRENT, height = 5, width = 12)
ggsave(plot = chrono_graph_current_density, FIGURE_PATH_CURRENTDENSITY, height = 5, width = 12)
ggsave(plot = chrono_graph_H2, FIGURE_PATH_H2, height = 5, width = 12)


## new graph format
chrono_processed %>% 
  ggplot(aes(x = elapsed_time_hr, y = H2,
             color = channel, group = instrument))+
  #geom_point()+
  #geom_path()+
  geom_line()+
#  scale_color_manual(values = c(
#    "nafion membrane" = "red", 
#    "no membrane" = "blue", 
#    "nickel foam" = "purple"))+
  labs(x = "Elapsed time (hours)",
       y = "Liters H2/L(reactor)/day")+
  facet_grid(instrument + channel ~ membrane)
  facet_wrap(~instrument + channel)


# step 3: save the processed file -----------------------------------------
crunch::write.csv.gz(processed_data, paste0("processed_data_", currentDate, ".csv.gz"), row.names = F, na="")
  