#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER
glimpse(ds)
ds$Year <- as.numeric(ds$Year) #One option
ds <- ds %>% mutate(Year = as.numeric(Year)) #Another option
typeof(ds$Year)

# "It is a historical anomaly that R has two names for its floating-point vectors, double and numeric (and formerly had real ). 
# double is the name of the type. numeric is the name of the mode and also of the implicit class. As an S4 formal class, 
# use "numeric" ."

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds <- ds %>% rename_all(tolower) #one option
ds <- ds %>% rename(year = Year, rank = Rank, artist = Artist, song = Song) #less good option

# Some did

ds %>% rename_with(tolower)

# ds %>% rename_with(tolower, .cols = c(Rank,Year))

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER
ds <- ds %>% mutate(decade = floor(year/10)*10)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
ds <- ds %>% arrange(rank)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER

top10 <- ds %>% filter(rank <= 10) %>% select(artist, song)

# Some people did this, which is valid
top10 <- slice_head(ds, n=10)
                    # but you'd want to pipe
top10 <- slice_head(ds, n=10) %>%
  select(artist, song)

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list (no need to save it anywhere)

#ANSWER
summary <- ds %>% summarize(min_yr = min(year, na.rm = T),
                 max_yr = max(year, na.rm = T),
                 mean_yr = mean(year, na.rm = T))

# Someone did
ds_sum <- ds %>% summarize(Year_min=min(Year[!is.na(Year)]),
                           Year_recent=max(Year[!is.na(Year)]),
                           Year_mean=mean(Year[!is.na(Year)])
)

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set. 
# Use one filter command only, and sort the responses by year

#ANSWER
ds %>% filter(year == min(round(summary$min_yr)) | year == min(round(summary$max_yr)) | year == min(round(summary$mean_yr)) ) %>% arrange(year)

# Someone did (hard coded)
ds3 <- ds %>% filter(Year %in% c("1879","2020","1980") ) %>% select(Artist, Song, Year) %>% arrange(Year)

# Another person did
ds %>% 
  filter(Year %in% c(min(Year[!is.na(Year)]), max(Year[!is.na(Year)]), round(mean(Year[!is.na(Year)])))) %>% 
  arrange(Year)

# Base R
ds4<-ds[ds$Year %in% c(1879,2020,1980), c("Artist","Song","Year")]
ds4<-ds4[order(ds4$Year),]

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
ds  <- ds %>% mutate(year = ifelse(song == "Brass in Pocket", 1979, year),
                     decade = floor(year/10)*10) 
summary <- ds %>% summarize(min_yr = min(year, na.rm = T),
                 max_yr = max(year, na.rm = T),
                 mean_yr = mean(year, na.rm = T))
ds %>% filter(year == min(round(summary$min_yr)) | year == min(round(summary$max_yr)) | year == min(round(summary$mean_yr)) ) %>% arrange(year)

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

# There are some NAs

#ds[is.na(ds$Year),]

#ANSWER
ds %>% filter(!is.na(decade)) %>% 
  group_by(decade) %>% 
  summarize(mean_rank = mean(rank), n_songs = n())

# Someone also did
ds %>% drop_na(decade) %>% group_by(decade) %>% summarise(average_rank=mean(Rank), number_of_songs=n())

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% drop_na() %>% count(decade) %>% slice_max(n)

# #what does "Source on Save" box mean on R (next to the save button) and do I need to click it?? 

# My understanding is this feature will just execute your code entirely every time you save

#lots of things in help show data as .ds - should I have included the . in mine? 

# Based on the comments provided by joran, 42, and hadley (who wrote dplyr), it appears that the correct answer is to avoid name 
# collisions with the data() function in the base packages in R.
# It is considered a best practice to avoid name collisions, and thus the dot prefix is just an arbitrary character to disambiguate the 
# .data parameter in dplyr from the data() function in the base R package.
