library(dplyr)
library(readr)

## 1. Read the necessary files
water_level <- read_csv("C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/WATERLEVEL.csv")
site_info <- read_csv("C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/SITE_INFO.csv")

## 2. Select only the required columns from each dataset
water_level_clean <- water_level %>%
  select(SiteNo, Time, 
         `Depth to Water Below Land Surface in ft.`, 
         `Water level in feet relative to NAVD88`)

site_info_clean <- site_info %>%
  select(SiteNo, SiteName, DecLatVa, DecLongVa, 
         WellDepth, StateNm, AquiferType, WlWellPurposeDesc)

## 3. Merge the cleaned datasets
final_well_data <- water_level_clean %>%
  left_join(site_info_clean, by = "SiteNo") %>%
  # Clean up column names (remove spaces and special characters)
  rename(
    DepthToWater_ft = `Depth to Water Below Land Surface in ft.`,
    WaterLevel_NAVD88_ft = `Water level in feet relative to NAVD88`,
    Longitude = DecLongVa,
    Latitude = DecLatVa,
    WellPurpose = WlWellPurposeDesc
  ) %>%
  # Convert Time to proper datetime format if needed
  mutate(Time = as.POSIXct(Time)) %>%
  # Arrange by SiteNo and Time
  arrange(SiteNo, Time)

## 4. Check the results
glimpse(final_well_data)

## 5. Save the cleaned data
write_csv(final_well_data, 
          "C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/ozark_plateaus_clean.csv")

library(lubridate)

## 1. Prepare Well Data (already cleaned)
well_data <- final_well_data %>%  # From previous step
  mutate(
    Time = as.Date(Time),
    YearMonth = format(Time, "%Y-%m")  # Create year-month for matching
  )

write_csv(well_data, 
          "C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/ozark_plateaus_clean_yearmonth.csv")

## 6. Create spatial version
library(sf)
wells_sf <- well_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # WGS84 coordinates

## 7. Quick summary statistics
summary_stats <- final_well_data %>%
  group_by(SiteNo, SiteName, StateNm, AquiferType, WellPurpose) %>%
  summarise(
    N_Measurements = n(),
    First_Date = min(Time),
    Last_Date = max(Time),
    Mean_Depth_ft = mean(DepthToWater_ft, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)
