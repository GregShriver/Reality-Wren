## Playing with BirdNET output for the Carolina Wren Ecology Study
## trying to convince Liz that this is a good idea!  


## load packages
library(tidyverse)
library(stringr)
library(ggmap)
library(lubridate)
library(magrittr)

# Section I - get BirdNET output ----------------------------------------


## create list of files from BirdNET output folder for the Site you are processing.  You need to update the site number in 4 places; list_of_files, adding 'site' to the dataframe, write.csv, and ggplot code

list_of_files <- list.files(path = "//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/Site16/output",
                            recursive = TRUE,
                            pattern = "*.csv",
                            full.names = TRUE)



## read contents of files into data frame (df)...this takes a few minutes (15 - 20 min)
df <- readr::read_csv(list_of_files, id = "file_name")

## add side ID
df$site <- 16

## subtract year, month, day, and time from 'file_name'
df$year <- substr(df$file_name, 67, 70)
df$month <- substr(df$file_name, 71, 72)
df$day <- substr(df$file_name, 73, 74)
df$time <- substr(df$file_name, 76, 79)

## convert columns to appropriate type
df$year <- as.factor(df$year)
df$month <- as.numeric(df$month) 
df$day <- as.numeric(df$day)
df$time <- as.numeric(df$time)

## add a count column (each row is one wren detection)
df$count <- 1

## save csv file so this can be the new start file for visualization in Section 2 below
write.csv(df, 'Z:/RealityWren/BikePath/results/Site_16_results.csv', row.names=FALSE)


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
      "Reality Wren Phenology Mar-Oct Site 16"
    )) +
  facet_grid(rows=vars(month))



# Section II - get results files saved from above ------------------------



## read BirdNET results files for each site into site specific data frame

Site01 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_01_results.csv')
Site02 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_02_results.csv')
Site03 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_03_results.csv')
Site04 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_04_results.csv')
Site05 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_05_results.csv')
Site06 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_06_results.csv')
Site07 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_07_results.csv')
Site08 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_08_results.csv')
Site09 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_09_results.csv')
Site10 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_10_results.csv')
Site11 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_11_results.csv')
Site12 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_12_results.csv')
Site13 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_13_results.csv')
Site14 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_14_results.csv')
Site15 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_15_results.csv')
Site16 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_16_results.csv')
Site17 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_17_results.csv')
Site18 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_18_results.csv')
Site19 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_19_results.csv')
Site20 <- read.csv('//10.2.28.245/Shriver/ARU_Data/RealityWren/BikePath/results/Site_20_results.csv')




## rbind site specific data frames into one data frame (song.dat)

song.dat <- rbind(Site01, Site02, Site03, Site04, Site05, Site06, Site07, Site08, Site09, Site10, Site11, Site12, Site13, Site14, Site15, Site16, Site17, Site18, Site19, Site20)

## save combined file that includes all wren detections for all sites and months

write.csv(song.dat, '../Reality-Wren/Output/wren_detections.csv', row.names=FALSE)


###############################################
## NEED TO ADD A READ.CSV TO CREATE song.dat

song.dat <- read.csv('../Reality-Wren/Output/wren_detections.csv')

## Create a column for morning (am) and evening (pm)

song.dat <- song.dat %>%
  mutate(time_code = case_when(time < 1200 ~ 'am',
                               time > 1200 ~ 'pm'))


## Dealing with creating an actual date field from day, month, year
song.dat$day <- as.integer(song.dat$day)

song.dat$day <- sprintf("%02d", song.dat$day)

song.dat$month <- as.integer(song.dat$month)

song.dat$month <- sprintf("%02d", song.dat$month)

song.dat$date <- paste0(song.dat$year, song.dat$month, song.dat$day)

song.dat$date <- as.numeric(song.dat$date)

song.dat$date <- ymd(song.dat$date)


## Create a year day from the newly created date field

song.dat$yday <- yday(song.dat$date)


## Create a week field from date

song.dat$week <- week(song.dat$date)


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
  ggsave(path='../Reality-Wren/Output/', filename=paste0(sites,".png"))
}


## run the vector of sites through the function using lapply
lapply(sites, my_plot_hours)







# Section III - Heatmap  ----------------------------------------------------------------

## Create df grouped by year day

total_wren_yday <- song.dat %>% 
  group_by(site, yday) %>%
  summarize(total_wren = sum(count))


total_wren_yday$site <- as.numeric(total_wren_yday$site)

site <- total_wren_yday$site


## create the heat map

ggplot(total_wren_yday, aes(x = yday, y = site, fill = total_wren)) +
  geom_tile() +
    theme_classic() +
   scale_fill_viridis(direction = -1) + 
   scale_y_continuous(breaks = site)



total_wren_yday %<>% mutate(zscore = (total_wren - mean(total_wren))/sd(total_wren))

#option = "magma"


ggplot(total_wren_yday, aes(x = yday, y = site, fill = zscore)) +
  geom_tile() +
  theme_classic() +
  scale_fill_gradient2(
    low = "grey",
    mid = "white",
    high = "brown", 
    midpoint = 0) + 
  scale_y_continuous(breaks = site)
  
    

ggplot(total_wren_yday, aes(x = yday, y = site, fill = zscore)) +
  geom_tile() +
  theme_classic() +
  scale_fill_viridis_c(direction = -1) + 
  scale_y_continuous(breaks = site)


x <- select(total_wren_yday, yday, site, total_wren)
x$site <- as.numeric(x$site)
x <- as.matrix(x)

heatmap(x, Colv = NA, Rowv = NA, scale = "column")

densityHeatmap(scale(x))



# Section IV - Boxplots by week -------------------------------------------


## Create df grouped by week

total_wren_week <- song.dat %>% 
  group_by(site, day) %>%
  summarize(total_wren = sum(count), across())


total_wren_week$site <- as.factor(total_wren_week$site)
song.dat$site <- as.factor(song.dat$site)
song.dat$week <- as.factor(song.dat$week)

ggplot(total_wren_week, aes(x = reorder(site, -total_wren), y = total_wren)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, size = 8)) +
  theme_classic()


ggplot(total_wren_week, aes(x = week, y = total_wren, fill = site)) + 
  geom_boxplot() +
  facet_wrap(~ site, scales = "free")+
  theme_classic() +
  theme(axis.text.x = element_text(size=5, angle=45)) +
  theme(legend.position = "none")


ggplot(total_wren_week, aes(x = month, y = total_wren, fill = site)) + 
  geom_boxplot() +
  facet_wrap(~ site, scales = "free")+
  theme_classic() +
  theme(axis.text.x = element_text(size=5, angle=45)) +
  theme(legend.position = "none")





total_wren_site <- song.dat %>% 
  group_by(site) %>%
  summarize(total_wren = sum(count))


# Section Appendix - make a map of sites -----------------------------------------------------


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








