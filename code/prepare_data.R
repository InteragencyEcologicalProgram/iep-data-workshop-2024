library(CDECRetrieve)
library(dplyr)
library(lubridate)
library(here)
library(CDECRetrieve)


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

# EDSM data -----------------------------------



