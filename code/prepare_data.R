library(CDECRetrieve)
library(dplyr)
library(readr)
library(lubridate)
library(here)


# CDEC data ---------------------------------------------
stations_wtemp <-  c("JER", "VER", "RVB")
sensors_wtemp <- c(100, 4, 25, 61, 27)
start = as.Date("2015-01-01")
end = as.Date("2019-01-01")

## Download data, bind, write --------------------------------------------
wtemp <- lapply(stations_wtemp,
              function(x){
                cdec_query(station = x,
                           sensor_num = 25,
                           dur_code = "D", #daily
                           start_date = start,
                           end_date = end)
              })
wtemp_df <- bind_rows(wtemp)

RVB <- lapply(sensors_wtemp,
                function(x){
                  cdec_query(station = "RVB",
                             sensor_num = x,
                             dur_code = "H", #hourly
                             start_date = start,
                             end_date = end)
                })
RVB_df <- bind_rows(RVB) %>%
  mutate(parameter = case_when(parameter_cd == "100" ~ "EC",
                               parameter_cd == "04" ~ "AirTempF",
                               parameter_cd == "25" ~ "WaterTempC",
                               parameter_cd == "27" ~ "TurbidityNTU",
                               parameter_cd == "-99" ~ "DO")) %>%
  mutate(date = date(datetime),
         year = year(datetime)) %>%
  filter(!(parameter == "WaterTempC" & parameter_value > 40),
         !(parameter == "AirTempF" & parameter_value > 120))

# saveRDS(wtemp_df, here("data/CDEC_watertemp_JER_VER_RVB_2015_2019.rds"))
# saveRDS(RVB_df, here("data/CDEC_parameters_RVB_2015_2019.rds"))

# DJFMP data -----------------------------------

(djfmp_catch_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.12&entityid=781adc2c13fbc39cfbede18146b97a7c"))
(djfmp_site_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.12&entityid=99a038d691f27cd306ff93fdcbc03b77"))

djfmp_catch_file <- contentid::resolve("hash://sha256/9492da36cd8cb12eef2016b3b23026b369fd5cfa38c4ff453f104d620e539819")
djfmp_site_file <- contentid::resolve("hash://sha256/f0f9e66da7415df90a7c0ea4c01938ecadd71ad412af1ccc940e861e63e96ba7")

djfmp_catch0 <- readr::read_csv(djfmp_catch_file)
djfmp_sites0 <- readr::read_csv(djfmp_site_file)

# Filter from USFWS list
stations_notincl <- c("SP000E","SP000W","SP001W","SP003E",
                      "SP008E","SA001M","SA004W","SA007E","SA008W","SA009E", "SA010W")

djfmp_catch <- djfmp_catch0 %>%
  filter(!(StationCode %in% stations_notincl))

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

saveRDS(djfmp_catch2, "data/processed/djfmp_data_2015-2019.rds")
