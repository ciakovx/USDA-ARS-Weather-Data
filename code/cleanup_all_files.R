
# load libraries ----------------------------------------------------------

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(chron)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)



# read data and clean column names ----------------------------------------

data_path <- "./data/raw"
files <- dir(data_path)

dat <- files %>%
  map(., function(z) {
    read_xlsx(file.path(data_path, z),
              col_names = FALSE,
              na = c("", "-")) %>%
      clean_names()
    })


# examine and build header names ------------------------------------------

my_headers <- map(dat, function(z) {
  #create header1 vector
  header1_start <- which(str_detect(z$x1, "\\bDate\\b") 
                         & str_detect(z$x2, "\\bTemperature\\b"))
  header1 <- z[header1_start, ] %>%
    make_clean_names() %>%
    str_replace("na", NA_character_)
  
  #create header2 vector
  header2_start <- which(str_detect(z$x2, "\\bmaximum\\b") 
                         & str_detect(z$x3, "\\bminimum\\b"))
  
  header2 <- z[header2_start, ] %>%
    make_clean_names() %>%
    str_replace("na", NA_character_)
  
  # for comparison
  headers2 <- tibble("header1" = header1,
                     "header2" = header2)
  
})


# construct header vector
my_headers <- c(my_headers[[1]]$header1[1],
                paste0("temp_", my_headers[[1]]$header2[2:5]),
                paste0("precip_", my_headers[[1]]$header2[6:9]),
                "precip_snow_depth_tobs",
                "wind_dir_tobs",
                "weather_state_tobs",
                "wind_dir_day",
                "weather_state_day")





# create data subset ------------------------------------------------------

mydf <- map(dat, function(mydat) {
  z <- mydat %>% select(1:14) 
  header2_start <- which(str_detect(z$x2, "\\bmaximum\\b")  
                         & str_detect(z$x3, "\\bminimum\\b")) 
  data_begin <- header2_start + 1
  data_end <- which(str_detect(z$x1, "1\\*")) - 1
  
  dat_subset <- z %>%
    slice(data_begin:data_end)
  names(dat_subset) <- my_headers
  return(dat_subset)
})



# create date/time variable for precip ------------------------------------

datafile_info <- map(dat, function(z) {
  tibble(
    month = z$x2[3],
    year = z$x4[3],
    location = z$x6[3],
    county = z$x8[3],
    state = z$x2[4],
    lat = z$x4[4],
    long = z$x6[4])
  }
  )


# loop through each df within mydf
# within each iteration, pull the month, year, location, etc from datafile_info
# then for non-NA time_conv values, add date and time for parsing
datafile_info_mod <- map2(mydf, 1:6, function(z, y) {
  z %>%
    mutate(month = datafile_info[[y]]$month,
           year = datafile_info[[y]]$year,
           location = rep(datafile_info[[y]]$location, nrow(.)),
           county = rep(datafile_info[[y]]$county, nrow(.)),
           state = rep(datafile_info[[y]]$state, nrow(.)),
           lat = rep(datafile_info[[y]]$lat, nrow(.)),
           long= rep(datafile_info[[y]]$long, nrow(.)),
           date_conv = mdy(paste(month, date, year)))
}
  )

all_dat <- map_dfr(precip_dt_mod, bind_rows)  %>%
  mutate(date = as.integer(date))

all_dat$precip_time_of_beginning_conv[!is.na(all_dat$precip_time_of_beginning_conv)] <- paste(all_dat$date_conv[!is.na(all_dat$precip_time_of_beginning_conv)], all_dat$precip_time_of_beginning_conv[!is.na(all_dat$precip_time_of_beginning_conv)])


all_dat$precip_time_of_beginning_conv <- ymd_hms(all_dat$precip_time_of_beginning_conv, tz = "US/Central")


write_csv(all_dat, "./data/processed/all_dat_20211011.csv")

# plot snowfall depth across month ----------------------------------------

mydf_snow_plot <- all_dat %>%
  drop_na(precip_snowfall_in_inches) %>%
  filter(month == "January")

snowfall_plot <- ggplot(mydf_snow_plot) +
  geom_line(aes(x = date,
                y = precip_snowfall_in_inches,
                group = 1)) +
  facet_grid(rows = vars(year)) +
  theme(legend.position = 'bottom',
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = '#BABABA')) +
  labs(x = "Date",
       y = "Snowfall in inches",
       title = "Snowfall in inches, January 1930, 1935, 1940 Stillwater Station", 
       caption = 'Source: U. S. Department of Agriculture, Weather Bureau')

snowfall_plot

ggsave("./fig/1930-40_January_snowfall.png", snowfall_plot)


