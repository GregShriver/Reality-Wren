## Playing with BirdNET output for the Carolina Wren Ecology Study

## load packages
library(tidyverse)
library(stringr)

#########################################################################################################
## Section one = get output files from BirdNET that were saved on the CANR Server (\\10.2.28.245\Shriver)
##
#########################################################################################################

## This is not the right way to do this and should be changed to a relative path.
setwd('Z:/RealityWren/BikePath/Site03')

## create list of files from BirdNET output folder for the Site identified in the directory above
list_of_files <- list.files(path = "./output",
                            recursive = TRUE,
                            pattern = "*.csv",
                            full.names = TRUE)

## read contents of files into data frame (df)...this takes a few minutes (15 - 20 min)
df <- readr::read_csv(list_of_files, id = "file_name")

## add side ID
df$site <- 03

## subtract year, month, day, and time from 'file_name'
df$year <- substr(df$file_name, 10, 13)
df$month <- substr(df$file_name, 14, 15)
df$day <- substr(df$file_name, 16, 17)
df$time <- substr(df$file_name, 19, 22)

## convert columns to appropriate type
df$year <- as.factor(df$year)
df$month <- as.numeric(df$month) 
df$day <- as.numeric(df$day)
df$time <- as.numeric(df$time)

## add a count column (each row is one wren detection)
df$count <- 1

## save csv file so this can be the new start file for visualization in Section 2 below
write.csv(df, 'Z:/RealityWren/BikePath/results/Site_03_results.csv', row.names=FALSE)


## If necessary, add rows on days no wrens were detected 
# x <- complete(df, day = min(day):max(day), fill = list(count = 0))


## summarize the number of wren detentions by day 
total_wren <- df %>% 
  group_by(month, day) %>%
  summarize(total_wren = sum(count))

## visualize with ggplot
total_wren %>%
  ggplot(aes(x = day, y = total_wren)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "day",
    y = "total wren detections",
    title = paste(
      "Reality Wren Phenology Mar-Oct Site 03"
    )) +
  facet_grid(rows=vars(month))

#########################################################################################################
## Section one = get output files from BirdNET that were saved on the CANR Server (\\10.2.28.245\Shriver)
##
#########################################################################################################
