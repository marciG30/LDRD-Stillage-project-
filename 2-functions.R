## processing FUNCTIONS for SquidStat.
## do not change this code for day-to-day work. 
## make changes only if you want to alter the workflow.

## 2021-05-06 Kaizad F. Patel

# These functions will pull all chrono files together,
# clean and process the data,
# and plot graphs.


# PART 0. load packages --------------------------------------------------------------

library(tidyverse)
theme_set(theme_bw())

#
# PART 1. CHRONO DATA -----------------------------------------------------
process_chrono_files = function(DATA_PATH){
  
  # this function will pull all .csv files from the input folder,
  # and compile them into a single file
  import_files = function(DATA_PATH){
    filePaths <- list.files(path = DATA_PATH, pattern = "*.csv", 
                            recursive = TRUE, full.names = TRUE)
    
    chrono_data <- 
      do.call(bind_rows, lapply(filePaths, function(path) {
        # the files are comma-delimited, but in a weird encoding format, 
        # so read.csv needs an encoding argument 
        df <- read.csv(path, sep = ",", fileEncoding = "latin1", fill = TRUE, stringsAsFactors = FALSE) %>% 
          # mutate(`Elapsed.Time..s.` = as.numeric(`Elapsed.Time..s.`))
          mutate_all(as.character)
        
        # then, add a new column `source` to denote the file name
        df[["source"]] <- rep(path, nrow(df))
        
        df
      }))
  }
  chrono_files = import_files(DATA_PATH)
  
  names(chrono_files)
  
#  4, 5, 6, 7,  9, 10, 11, 13, 
  
  
  
  # this function will clean the file (rename, filter unnecessary data)
  clean_chrono_files_OLD = function(chrono_files){
    ## this is the OLD function, which calculated total elapsed time
    ## by stacking multiple runs and adding
    ## NOT IN USE NOW - 2021-07-07
    temp = 
      chrono_files %>% 
      # rename and subset the columns needed
 #     mutate(source = if_else(grepl("bulk", source), filename, source)) %>% 
      rename(step_number = `Step.number`,
             elapsed_time_s = `Elapsed.Time..s.`,
             current_mA = `Current..mA.`,
             working_electrode_V = `Working.Electrode..V.`,
             working_electrode_NHE = `Working.Electrode.vs..NHE..V.`,
             cumul_charge_mAh = `Cumulative.Charge..mAh.`,
             current_density_mA_cm2 = `Current.Density..mA.cm..`
             ) %>% 
      dplyr::select(step_number, elapsed_time_s, current_mA, 
                    working_electrode_V, working_electrode_NHE,
                    cumul_charge_mAh, current_density_mA_cm2,
                    source) %>% 
      ## make character variables numeric if needed
      mutate(working_electrode_V = as.numeric(working_electrode_V),
             working_electrode_NHE = as.numeric(working_electrode_NHE),
             cumul_charge_mAh = as.numeric(cumul_charge_mAh),
             current_density_mA_cm2 = as.numeric(current_density_mA_cm2)) %>% 
      ## make sure instrument/channel notations are in the correct case
      mutate(source = str_replace_all(source, "Prime", "prime"),
             source = str_replace_all(source, "chan_", "Chan_")) %>% 
      mutate(instrument = str_extract(source, "prime[0-9]{4}"),
             channel = str_extract(source, "Chan_[0-9]"),
             date1 = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
             date2 = str_extract(source, "[0-9]{4}_[0-9]{2}_[0-9]{2}"),
             date = paste0 (date2),
             date = str_remove_all(date, "NA")) %>% 
      mutate(elapsed_time_s = as.numeric(elapsed_time_s),
             current_mA = as.numeric(current_mA),
             date = lubridate::ymd(date)) %>% 
      dplyr::select(-source, -date1, -date2) %>%
      # add a new column for membrane
      mutate(membrane = case_when(
        (instrument == "prime1092" & channel == "Chan_4" | instrument == "prime1092" & channel == "Chan_2") ~ "nickel foam",
        (instrument == "prime1338" & channel == "Chan_1" | instrument == "prime1338" & channel == "Chan_2" | instrument == "prime1338" & channel == "Chan_3") ~ "nafion membrane",
        (instrument == "prime1338" & channel == "Chan_4" | instrument == "prime1092" & channel == "Chan_1" | instrument == "prime1092" & channel == "Chan_3") ~ "no membrane",
      )) %>% 
      
      # if a run spanned multiple days, we need to add the maximum time of the first day to the time of the second day
      # to do this, we assign a group number to each date
      # and then add the max per group to the next group
      group_by(channel, instrument) %>% 
      mutate(newdate = date != c(NA, head(date, -1))) %>% 
      drop_na() %>% 
      group_by(channel, instrument) %>% 
      mutate(date_group = cumsum(newdate)) %>% 
      # add new column, H2 (mL per day)
      mutate(H2 = 9.1806050221 * current_mA/100) %>% 
    #  dplyr::select(-newdate) %>% 
      force()
    
    max_time = 
      temp %>% 
      group_by(channel, instrument, date_group) %>% 
      dplyr::summarise(max = max(elapsed_time_s)) %>% 
      mutate(date_group = date_group+1) %>% 
      rename(max_prev = max) %>% 
      mutate(max_cumul = cumsum(max_prev))
    
  temp %>% 
      left_join(max_time) %>% 
      replace(is.na(.), 0) %>% 
      mutate(elapsed_time_s2 = elapsed_time_s + max_cumul) %>% 
      # convert elapsed_time_s into hour
      # rearrange by date and time
      mutate(elapsed_time_hr = elapsed_time_s2/3600) %>% 
      #dplyr::select(-max_prev) %>% 
      group_by(instrument, channel) %>% 
      arrange(date, elapsed_time_s2)
    
  }
  clean_chrono_files = function(chrono_files){
    temp = 
      chrono_files %>% 
      # rename and subset the columns needed
      #     mutate(source = if_else(grepl("bulk", source), filename, source)) %>% 
      rename(step_number = `Step.number`,
             elapsed_time_s = `Elapsed.Time..s.`,
             current_mA = `Current..mA.`,
             working_electrode_V = `Working.Electrode..V.`,
             working_electrode_NHE = `Working.Electrode.vs..NHE..V.`,
             cumul_charge_mAh = `Cumulative.Charge..mAh.`,
             current_density_mA_cm2 = `Current.Density..mA.cm..`
      ) %>% 
      dplyr::select(step_number, elapsed_time_s, current_mA, 
                    working_electrode_V, working_electrode_NHE,
                    cumul_charge_mAh, current_density_mA_cm2,
                    source) %>% 
      ## make character variables numeric if needed
      mutate(working_electrode_V = as.numeric(working_electrode_V),
             working_electrode_NHE = as.numeric(working_electrode_NHE),
             cumul_charge_mAh = as.numeric(cumul_charge_mAh),
             current_density_mA_cm2 = as.numeric(current_density_mA_cm2)) %>% 
      ## make sure instrument/channel notations are in the correct case
      mutate(source = str_replace_all(source, "Prime", "prime"),
             source = str_replace_all(source, "chan_", "Chan_")) %>% 
      ## extract instrument and channel names
      mutate(instrument = str_extract(source, "prime[0-9]{4}"),
             channel = str_extract(source, "Chan_[0-9]"),
      ## extract start date-time from file name, 
      ## which is in format YYYYMMDD HHMMSS (8 numbers, space, 6 numbers)       
             datetime_start = str_extract(source, "[0-9]{8} [0-9]{6}"),
             datetime_start2 = lubridate::ymd_hms(datetime_start)
             ) %>% 
      ## calculate datetime per datapoint by adding elapsed time to starting date-time
      mutate(elapsed_time_s = as.numeric(elapsed_time_s),
             datetime = datetime_start2 + elapsed_time_s,
             current_mA = as.numeric(current_mA),
             ) %>% 
      dplyr::select(-source, -datetime_start, -datetime_start2) %>%
      # add a new column for membrane
      mutate(membrane = case_when(
        (instrument == "prime1092" & channel == "Chan_4" | instrument == "prime1092" & channel == "Chan_2") ~ "nickel foam",
        (instrument == "prime1338" & channel == "Chan_1" | instrument == "prime1338" & channel == "Chan_2" | instrument == "prime1338" & channel == "Chan_3") ~ "nafion membrane",
        (instrument == "prime1338" & channel == "Chan_4" | instrument == "prime1092" & channel == "Chan_1" | instrument == "prime1092" & channel == "Chan_3") ~ "no membrane",
      )) %>% 
      
      # add new column, H2 (mL per day)
      mutate(H2 = 9.1806050221 * current_mA/100) %>% 
      force()
    
    ## calculate overall elapsed time, in sec and hr
    temp %>%
      dplyr::select(instrument, channel, membrane, datetime, 
                    current_mA, working_electrode_V, working_electrode_NHE, 
                    cumul_charge_mAh, current_density_mA_cm2, H2) %>% 
      group_by(instrument, channel) %>% 
      mutate(elapsed_time_sec = difftime(datetime, min(datetime), units = "secs"),
             elapsed_time_sec = as.numeric(elapsed_time_sec),
             elapsed_time_hr = elapsed_time_sec/3600)
      
  }
  
  chrono_processed = clean_chrono_files(chrono_files)
  chrono_processed
}
plot_chrono_graph_current = function(chrono_processed){    
  chrono_processed %>% 
    ggplot(aes(x = elapsed_time_hr, y = current_mA,
               color = membrane))+
    #geom_point()+
    #geom_path()+
    geom_line()+
    scale_color_manual(values = c(
      "nafion membrane" = "red", 
      "no membrane" = "blue", 
      "nickel foam" = "purple"))+
    labs(x = "Elapsed time (hours)",
         y = "Current (mA)")+
    facet_wrap(~instrument + channel, scales = "free_y")
}

plot_chrono_graph_currentdensity = function(chrono_processed){    
  chrono_processed %>% 
    ggplot(aes(x = elapsed_time_hr, y = current_density_mA_cm2,
               color = membrane))+
    #geom_point()+
    #geom_path()+
    geom_line()+
    scale_color_manual(values = c(
      "nafion membrane" = "red", 
      "no membrane" = "blue", 
      "nickel foam" = "purple"))+
    labs(x = "Elapsed time (hours)",
         y = "Current density (mA/cm2)")+
    facet_wrap(~instrument + channel)
}

plot_chrono_graph_H2 = function(chrono_processed){    
  chrono_processed %>% 
    ggplot(aes(x = elapsed_time_hr, y = H2,
               color = membrane))+
    #geom_point()+
    #geom_path()+
    geom_line()+
    scale_color_manual(values = c(
      "nafion membrane" = "red", 
      "no membrane" = "blue", 
      "nickel foam" = "purple"))+
    labs(x = "Elapsed time (hours)",
         y = "Liters H2/L(reactor)/day")+
    facet_wrap(~instrument + channel, scales = "free_y")
}
