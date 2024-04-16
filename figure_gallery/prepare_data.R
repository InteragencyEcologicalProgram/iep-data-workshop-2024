# prepare_data.R
# This script provides code for downloading data from online sources and manipulating them
# These data are saved in the "data/raw" or "data/processed" folders
# and then read in again in "baydelta-figure-gallery.Rmd"
# Code from Catarina Pien (cpien@usbr.gov),
# Rosie Hartman (Rosemary.Hartman@water.ca.gov),
# and Trinh Nguyen (Trinh.Nguyen@wildlife.ca.gov)


# Call in packages ---------------------------------------------------
library(CDECRetrieve) # Downloading data from cdec
# Can also use cder and sharpshootr.
library(dplyr) # Manipulating data
library(lubridate) # Easy ways to define time variables
library(viridis) # Color-blind friendly palettes
library(readr) # For reading and writing data faster
library(here) # For helping read in data - starts with your project directory filepath
library(tidyverse) # data tidying
library(rvest) # Web scraping
library(xml2) # Web scraping
library(contentid) # Reproducible workflows


# CDEC data (cdec.water.ca.gov) ---------------------------------------------
stations_wtemp <-  c("JER", "VER", "RVB")
sensors_RVB <- c(100, 4, 25, 61, 27)
start = as.Date("2015-01-01")
end = as.Date("2019-01-01")

## Download CDEC data, bind, write --------------------------------------------

# This allows you to download water temperature data from several stations
wtemp <- lapply(stations_wtemp,
              function(x){
                cdec_query(station = x,
                           sensor_num = 25,
                           dur_code = "D", #daily
                           start_date = start,
                           end_date = end)
              })
wtemp_df <- bind_rows(wtemp)

# This allows you to download EC, AirTemp, WaterTemp, Turbidity, DO data from RVB
RVB <- lapply(sensors_RVB,
                function(x){
                  cdec_query(station = "RVB",
                             sensor_num = x,
                             dur_code = "H", #hourly
                             start_date = start,
                             end_date = end)
                })
RVB_df <- bind_rows(RVB) %>%
  # renaming parameters from sensor numbers
  mutate(parameter = case_when(parameter_cd == "100" ~ "EC",
                               parameter_cd == "04" ~ "AirTempF",
                               parameter_cd == "25" ~ "WaterTempC",
                               parameter_cd == "27" ~ "TurbidityNTU",
                               parameter_cd == "-99" ~ "DO")) %>%
  mutate(date = date(datetime),
         year = year(datetime)) %>%
  # just filtering out some bad data
  filter(!(parameter == "WaterTempC" & parameter_value > 40),
         !(parameter == "AirTempF" & parameter_value > 120))

# saveRDS(wtemp_df, here("figure_gallery/data/CDEC_watertemp_JER_VER_RVB_2015_2019.rds"))
# saveRDS(RVB_df, here("figure_gallery/data/CDEC_parameters_RVB_2015_2019.rds"))


## Downloading water temperature from the MAL CDEC gauges ------------------------------
malWaterTemp <- read.csv("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=MAL&SensorNums=25&dur_code=H&Start=2009-01-01&End=2024-04-04") %>%
  # Format data correctly and covnert temp to C
  mutate(date = as.Date(DATE.TIME, format = "%Y%m%d"),
         waterTemperatureC = (as.numeric(VALUE) - 32) * 5/9) %>%
  # Quick and dirty filter to remove outliers
  filter(between(waterTemperatureC, 8, 30)) %>%
  # Need a dummy date variable if we want to plot all years on the same season
  # Year picked only needs to be a leap year but freedom is yours
  group_by(year = year(date),
           month = month(date),
           dummyDate = as.Date(paste0("2024-", format(date, format = "%m-%d"))),
           color = ifelse(year == 2024, "2024", "Others")) %>%
  # Calculate daily water temperature, since this is hourly data
  summarise(dailyTemperature = mean(waterTemperatureC, na.rm = T))

# saveRDS(malWaterTemp, here("figure_gallery/data/processed/CDEC_MAL_2024.rds"))

## Downloading DTO (Delta Outflow) ----------------------------------
DTO = cdec_query(station = "DTO",
                 sensor_num = 23,
                 dur_code = "D",
                 start_date = as.Date("2015-01-01"),
                 end_date = as.Date("2023-12-31"))

# saveRDS(DTO, here("data/raw/CDEC_DTO_2015_2023.rds"))

# Downloading Dayflow ------------------------------------------------

# The other data source for Delta Outflow (as well as a lot of other flow parameters) is DWR's Dayflow model.
# This model is run once per year, usually in January or February of the following water year.
# It's run after all of the input data has been fully QAQC'd, so you have to wait for it, but the data are better.
# It's hosted on the CNRA open data portal and the format is more difficult to work with:
# https://data.cnra.ca.gov/dataset/dayflow

# Download the data

dayflow2023 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/f7c1ba7f-bd64-4762-88e3-6db9b2501b38/download/dayflowcalculations2023.csv")%>%
  mutate(Date = mdy(Date))

Dayflow2022 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/5300e2e6-8ff0-45b7-ad70-9e0c37ab8fcf/download/dayflowcalculations2022.csv")

Dayflow2021 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/83122ce7-e7f5-4ad1-b5e9-6a7032cb1117/download/dayflowcalculations2021.csv")

#the older data uses a different formula for some of the parameters, so they have different names. read the documentation for details on the differences.
Dayflow29_39 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/ab12e85f-82f4-4723-9973-deeed41b2057/download/dayflow-results-1929-1939.csv") %>%
  rename(YOLO = YOLO1, OUT = OUT1, RIO = RIO1, DIVER = DIVER1, EFFDIV = EFFDIV1, WEST = WEST1, Mo = Month) %>%
  mutate(Date = mdy(Date), DIVER = as.numeric(DIVER), EFFDIV = as.numeric(EFFDIV))

Dayflow40_49 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/bf58c67c-63b4-47d4-9a25-2b95e5479a0c/download/dayflow-results-1940-1949.csv")%>%
  rename(YOLO = YOLO2, OUT = OUT2, RIO = RIO2, DIVER = DIVER2, EFFDIV = EFFDIV2, WEST = WEST2, TOT = TOT2)%>%
  mutate(Date = mdy(Date))

Dayflow50_55 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/9225dbe7-54a6-4466-b360-e66f51407683/download/dayflow-results-1950-1955.csv")%>%
  rename(YOLO = YOLO2, OUT = OUT2, RIO = RIO2, DIVER = DIVER2, EFFDIV = EFFDIV2, WEST = WEST2, TOT = TOT2)%>%
  mutate(Date = mdy(Date))

Dayflow56_69 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/3109f3ef-b77b-4288-9ece-3483899d10da/download/dayflow-results-1956-1969.csv")%>%
  mutate(Date = mdy(Date))

Dayflow70_83 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv")%>%
  mutate(Date = mdy(Date))

Dayflow84_96 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv")%>%
  mutate(Date = mdy(Date))

Dayflow97_20 = read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv")%>%
  mutate(Date = mdy(Date))

# Bind data
Dayflow = bind_rows(Dayflow29_39, Dayflow40_49, Dayflow50_55, Dayflow56_69, Dayflow70_83, Dayflow84_96, Dayflow97_20,
                    Dayflow2021, Dayflow2022, dayflow2023)

# write_csv(Dayflow, here("figure_gallery/data/processed/dayflow_1929_2023.csv"))

# Downloading Water Year Type Data -------------------------------------------

#Read in the website
wytype <- read_html("https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST")

#Extract the block of text with the water year type
wytype2 <- wytype %>%
  html_elements("pre") %>%
  html_text2() %>%
  str_split("\r\n") %>%
  unlist() %>%
  str_trim() %>%
  .[-c(1:14)]

#Choose the rows starting with years, just the first table with the Sac/SJ valley index and Water Year Type
wytype2_keep <- wytype2[if_else(cumsum(if_else(wytype2 == "", 1, 0)) < 1, TRUE, FALSE)]

#Set up the names for the table
wytype_names <- c(
  "Year",
  "Sac_OctMar",
  "Sac_AprJul",
  "Sac_WYsum",
  "Sac_Index",
  "Sac_WYtype",
  "SJ_OctMar",
  "SJ_AprJul",
  "SJ_WYsum",
  "SJ_Index",
  "SJ_WYtype"
)

#the first few years only have san joaquin index, so do those seperately
df_wytype_1901_1905 <- read_table(wytype2_keep[1:5], col_names = wytype_names[c(1, 7:11)])

#now parse the rest of the table into coloums
df_wytype_1906_cur <- read_table(wytype2_keep[6:length(wytype2_keep)], col_names = wytype_names)

#Bind the two tables togeter, arrange by water year, and turn the WYtypes into factors with the right orter
df_wytype <- bind_rows(df_wytype_1906_cur, df_wytype_1901_1905) %>% arrange(Year) %>%
  mutate(Sac_WYtype = factor(Sac_WYtype, levels = c("C", "D", "BN", "AN", "W")),
         SJ_WYtype = factor(SJ_WYtype, levels = c("C", "D", "BN", "AN", "W")))

# Write out the table
# write_csv(df_wytype, here("figure_gallery/data/processed/wytype_1906_cur.csv"))

# Downloading USFWS DJFMP (Delta Juvenile Fish Monitoring Program) data -----------------------------------

# Read about contentid here: https://github.com/cboettig/contentid

# This provides a hash code that will link directly to a specific url/doi/version of the data.
# If the data has changed, the hash code will change.
(djfmp_catch_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.12&entityid=781adc2c13fbc39cfbede18146b97a7c"))
(djfmp_site_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.12&entityid=99a038d691f27cd306ff93fdcbc03b77"))

# That hash code was copied in here. If your local copy still matches what this hash code detects, the data will
# read in from there. This saves time for downloading large files.
djfmp_catch_file <- contentid::resolve("hash://sha256/9492da36cd8cb12eef2016b3b23026b369fd5cfa38c4ff453f104d620e539819")
djfmp_site_file <- contentid::resolve("hash://sha256/f0f9e66da7415df90a7c0ea4c01938ecadd71ad412af1ccc940e861e63e96ba7")

# Now you can read in the file like normal. You could also skip the above steps and just throw in the url
# after read_csv.
djfmp_catch0 <- readr::read_csv(djfmp_catch_file)
djfmp_sites0 <- readr::read_csv(djfmp_site_file)

# Remove a few stations
stations_notincl <- c("SP000E","SP000W","SP001W","SP003E",
                      "SP008E","SA001M","SA004W","SA007E","SA008W","SA009E", "SA010W")

djfmp_catch <- djfmp_catch0 %>%
  filter(!(StationCode %in% stations_notincl))

# Create a data frame for just the individual sampling events
djfmp_sample <- djfmp_catch %>%
  mutate(Datetime = paste(SampleDate, SampleTime),
         Datetime = ymd_hms(Datetime)) %>%
  select(Datetime, StationCode) %>%
  distinct() %>%
  mutate(n = 1:nrow(.))

# Select columns to keep, create EventID
djfmp_catch2 <- djfmp_catch %>%
  mutate(Datetime = paste(SampleDate, SampleTime),
         Datetime = ymd_hms(Datetime),
         Month = month(Datetime),
         Jday = yday(Datetime),
         Year = year(Datetime)) %>%
  left_join(djfmp_sample) %>%
  mutate(EventID = paste0("DJFMP_", n)) %>%
  filter(Year>2014 & Year < 2020) %>%
  select(Location, RegionCode, EventID, StationCode, Datetime, SampleDate, Year, Month, Jday, MethodCode, GearConditionCode,
         WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance,
         FlowDebris, SiteDisturbance, AlternateSite,
         Volume, IEPFishCode, ForkLength, Count)

# write out file

# saveRDS(djfmp_catch2, "figure_gallery/data/processed/djfmp_data_2015-2019.rds")
# saveRDS(djfmp_sites0, "figure_gallery/data/processed/djfmp_stations.rds")


# Downloading Yolo Bypass Inundation Data --------------------------------------------

inun <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.840.2&entityid=cf6ebcb764db6d6dfc29502ff783af1f")
inun_2016_2019 <- inun %>%
  mutate(Year = year(Dates),
         Month = month(Dates),
         DOY = yday(Dates)) %>%
  filter(Year > 2015 & Year < 2020,
         Month >=1 & Month < 7)

# Downloading Yolo Water Quality Data --------------------------------------------

yolo_wq <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1391.1&entityid=8c457392256ff1e88ef4b757d8ffa3a6")
chl_2016_2019 <-  yolo_wq %>%
  select(sample_date, station_code, latitude, longitude, chlorophyll) %>%
  mutate(Year = year(sample_date),
         Month = month(sample_date),
         DOY = yday(sample_date)) %>%
  filter(Year > 2015 & Year < 2020,
         Month >=1 & Month < 7)

# write_csv(inun_2016_2019, "figure_gallery/data/processed/yolo_inun_2016_2019.csv")
# write_csv(chl_2016_2019, "figure_gallery/data/processed/yolo_chl_2016_2019.csv")

