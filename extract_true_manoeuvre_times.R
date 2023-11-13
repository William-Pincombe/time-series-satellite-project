
### DATA CLEANING : EXTRACTING TRUE MANOEUVRE TIMES ###

# Load packages
pacman::p_load(tidyverse)

## PART 1 : NON-FENGYUN SATELLITES ##

filenames <- c("cs2man.txt","en1man.txt",
               "h2aman.txt","h2cman.txt","h2dman.txt",
               "ja1man.txt","ja2man.txt","ja3man.txt",
               "s3aman.txt","s3bman.txt","s6aman.txt",
               "sp2man.txt","sp4man.txt","sp5man.txt",
               "srlman.txt","topman.txt")

# Read in manoeuvres information as text
manoeuvres <- data.frame(
  raw_data = read_lines(here::here("satellite_data", "manoeuvres",filenames))
)

# Extract information out of the string, using the information in man.readme
manoeuvres <- manoeuvres %>% 
  mutate(
    name = str_sub(raw_data,1,5),
    begin_year = as.numeric(str_sub(raw_data,7,10)),
    begin_day = as.numeric(str_sub(raw_data,12,14)),
    begin_hour = as.numeric(str_sub(raw_data,16,17)),
    begin_minute = as.numeric(str_sub(raw_data,19,20)),
    end_year = as.numeric(str_sub(raw_data,22,25)),
    end_day = as.numeric(str_sub(raw_data,27,29)),
    end_hour = as.numeric(str_sub(raw_data,31,32)),
    end_minute = as.numeric(str_sub(raw_data,34,35))
  )

# Get dates in date format, using year and number of days
# as.Date can find the date a given number of days from a specified origin date
# we specify the origin date as the start of the year
# and the number of days as the day of the year - 1
# (e.g. Jan 1 is day 1 of year but 0 days from start of year)
manoeuvres <- manoeuvres %>% 
  mutate(
    begin_date = as.Date(begin_day-1, origin = paste(begin_year, "-01-01", sep = '')),
    end_date = as.Date(end_day-1, origin = paste(end_year, "-01-01", sep = ''))
  )

# Include exact time in date <- WOULD BE GREAT TO FIGURE OUT HOW TO DO THIS IN TIDYVERSE
hour(manoeuvres$begin_date) <- manoeuvres$begin_hour
minute(manoeuvres$begin_date) <- manoeuvres$begin_minute
hour(manoeuvres$end_date) <- manoeuvres$end_hour
minute(manoeuvres$end_date) <- manoeuvres$end_minute

# Drop all but necessary variables
manoeuvres <- manoeuvres %>% 
  select(name, begin_date, end_date)

## PART 2 : FENGYUN SATELLITES ##

sat_names_fengyun <- c("FY2D","FY2E","FY2F","FY2H","FY4A")

filenames_fengyun <- c("manFY2D.txt.fy","manFY2E.txt.fy",
                       "manFY2F.txt.fy","manFY2H.txt.fy",
                       "manFY4A.txt.fy")

filepaths_fengyun <- here::here("satellite_data","manoeuvres",filenames_fengyun)

# Initialise data frame
fengyun_manoeuvres <- data.frame()

# Add each satellite to the data frame
for (i in 1:length(filenames_fengyun)){
  # Read in the data
  new_data <- read_table(file = filepaths_fengyun[i], col_names = FALSE)
  
  # Add column names
  colnames(new_data) <- c("type","name","begin_date","tz1","end_date","tz2")
  
  # Replace the satellite name with something more legible
  new_data$name <- rep(sat_names_fengyun[i], nrow(new_data))
  
  # Select only necessary columns
  new_data <- select(new_data, type, name, begin_date, end_date)
  
  # Add to data frame
  fengyun_manoeuvres <- rbind(fengyun_manoeuvres, new_data)
}

# fengyun_data = read_table(file = here::here("satellite_data","manoeuvres",filenames_fengyun), 
                          # col_names = FALSE)

# Remove accidental " signs from strings and save dates as datetime format
fengyun_manoeuvres <- fengyun_manoeuvres %>% 
  mutate(
    begin_date = as_datetime(str_remove(begin_date, "\"")),
    end_date = as_datetime(str_remove(end_date, "\""))
  )

# Remove maneouvre type, as we don't have this for the non-fengyun satellites
# IF we import this for the non-fengyun satellites, comment out this step
fengyun_manoeuvres <- fengyun_manoeuvres %>% 
  select(name, begin_date, end_date)



# Merge the Fengyun satellites into the main data frame
manoeuvres <- rbind(manoeuvres, fengyun_manoeuvres)

# Save cleaned data in a csv file
write_csv(manoeuvres, 
          file = paste("cleaned_manoeuvre_times_", as.Date(now()), ".csv", 
                       sep = ""))

