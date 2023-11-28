## Playing with BirdNET output for the Carolina Wren Ecology Study
## trying to convince Liz that this is a good idea!  


## load packages
library(tidyverse)
library(stringr)
library(ggmap)



# Section I - get BirdNET output ----------------------------------------


## This is not the right way to do this and should be changed to a relative path.
setwd('Z:/RealityWren/BikePath/Site04')

## create list of files from BirdNET output folder for the Site identified in the directory above
list_of_files <- list.files(path = "./output",
                            recursive = TRUE,
                            pattern = "*.csv",
                            full.names = TRUE)

## read contents of files into data frame (df)...this takes a few minutes (15 - 20 min)
df <- readr::read_csv(list_of_files, id = "file_name")

## add side ID
df$site <- 04

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
write.csv(df, 'Z:/RealityWren/BikePath/results/Site_04_results.csv', row.names=FALSE)


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
      "Reality Wren Phenology Mar-Oct Site 04"
    )) +
  facet_grid(rows=vars(month))



# Section II - get results files saved from above ------------------------



## read BirdNET results files for each site into site specific data frame

Site01 <- read.csv('../Reality-Wren/Data/Site_01_results.csv')
Site02 <- read.csv('../Reality-Wren/Data/Site_02_results.csv')
Site03 <- read.csv('../Reality-Wren/Data/Site_03_results.csv')
Site04 <- read.csv('../Reality-Wren/Data/Site_04_results.csv')
Site11 <- read.csv('../Reality-Wren/Data/Site_11_results.csv')
Site12 <- read.csv('../Reality-Wren/Data/Site_12_results.csv')
Site13 <- read.csv('../Reality-Wren/Data/Site_13_results.csv')

## rbind site specific data frames into one data frame (song.dat)
song.dat <- rbind(Site01, Site02, Site03, Site11, Site12)

## save combined file that includes all wren detections for all sites and months

write.csv(song.dat, '../Reality-Wren/Output/wren_detections.csv', row.names=FALSE)



## Create a column for morning (am) and evening (pm)

song.dat <- song.dat %>%
  mutate(time_code = case_when(time < 1200 ~ 'am',
                               time > 1200 ~ 'pm'))


## Create df grouped by time_code (am & pm)

total_wren_day <- song.dat %>% 
  group_by(site, month, day, time_code) %>%
  summarize(total_wren = sum(count))


total_wren_day$site <- as.factor(total_wren_day$site)

sites <- levels(total_wren_day$site)

## create a plot function that will use lapply to iterate through sites making a figure for each site (like above but this time with stacked bars showing songs in am and pm)


## Get file names to change
my_plot_hours <- function(sites){
  total_wren_day %>%
    filter(site == sites) %>%
    ggplot(aes(fill=time_code, y=total_wren, x=day)) + 
    geom_bar(position="stack", stat="identity")+
    theme_classic() +
    labs(
      x = "day",
      y = "total wren detections",
      title = paste("Reality Wren Phenology Mar-Oct Site", sites)) +
    facet_grid(rows=vars(month))
  ggsave(filename=paste0(sites,".png"))
}


## run the vector of sites through the function using lapply
lapply(sites, my_plot_hours)




## older graphing code-----------------
## visualize with ggplot

## try to iterate through with a for loop
for (i in levels(total_wren$site)) {
  p <- total_wren %>%
    filter(site == i) %>%
    ggplot(aes(x = day, y = total_wren)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    labs(
      x = "day",
      y = "total wren detections",
      title = paste("Reality Wren Phenology Mar-Oct Site", i)) +
    facet_grid(rows=vars(month))
    print(p)
    ggsave(p, filename = paste0("site", i, ".png"))
    
}


## now try a Functional Approach which seems better!

## create the plotting function that lapply will iterate through 

my_plot <- function(sites){
  total_wren %>%
    filter(site == sites) %>%
    ggplot(aes(x = day, y = total_wren)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    labs(
      x = "day",
      y = "total wren detections",
      title = paste("Reality Wren Phenology Mar-Oct Site", sites)) +
    facet_grid(rows=vars(month))
  # ggsave(p, filename = paste0("site", i, ".png"))
}

## create the vector of sites 
sites <- levels(total_wren$site)

## run the vector of sites through the function using lapply
lapply(sites, my_plot)










# Section III - make a map of sites -----------------------------------------------------


## Load in site coordinates; this will only work with a key (not provided)

site.coords <- read.csv('../Reality-Wren/Data/2023_RealityWrenAudioMothDeployment.csv')

site.coords$Site_ID <- as.factor(site.coords$Site_ID)



## make ggmap; get map center, set map type, plot points on map, save map as an image
map.center <- summarise(site.coords, long = mean(longitude), lat = mean(latitude))

wren.map <- get_map(location = map.center, source = "google", maptype = "satellite", zoom = 12)

p <- ggmap(wren.map) + geom_point(data = site.coords, aes(x = longitude, y = latitude), color = "red", size = 3) +
  labs(x = 'Longitude', y = 'Latitude') +
  geom_text(data = site.coords,
            aes(x = longitude, y = latitude, label = Site_ID), 
            vjust = 0.3, hjust = -0.8,
            colour = "white")


ggsave(filename="wren.map.png", plot = p, width=6, height=6, units = "in")











## FROM MARKDOWN
my_plot_hours <- function(sites){
  total_wren_day %>%
    filter(site == sites) %>%
    ggplot(aes(fill=time_code, y=total_wren, x=day)) + 
    geom_bar(position="stack", stat="identity")+
    theme_classic() +
    labs(
      x = "day",
      y = "total wren detections",
      title = paste("Reality Wren Phenology Mar-Oct Site", sites)) +
    facet_grid(rows=vars(month))
}

## create the vector of sites 
sites <- levels(total_wren_day$site)

## run the vector of sites through the function using lapply
lapply(sites, my_plot_hours)

my_plot_hours














# Section IV - Making sonograms of wren vocalizations ---------------------

library (av)
library(tuneR)
library(seewave)
library(phonTools)

## from av
plot(read_audio_fft('../Reality-Wren/Data/audio/20230305_060100_0_00447_692.wav'))

plot(read_audio_fft('../Reality-Wren/Data/audio/20230306_121110_0_00450_178.wav'))


# Create new audio file with first 3 sec to then make a video
av_audio_convert('../Reality-Wren/Data/audio/20230305_060100_0_00447_692.wav', 'short.mp3', total_time = 3.0)

#> use the av package to create an mp4 video of the 'short.mp3' file created in step above
av_spectrogram_video('short.mp3', output = 'spectrogram.mp4', width = 1280, height = 720, res = 144)


## from phonTools
sound <- loadsound('../Reality-Wren/Data/audio/20230305_060100_0_00447_692.wav')
spectrogram(sound, maxfreq = sound$fs/2)








