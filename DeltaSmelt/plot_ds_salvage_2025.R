library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(viridis)
library(janitor)
library(readxl)

# Salvage plot -------------------------------

# Updated 2026 to pull from SacPAS, which already pulls from salvage database

## Salvage data --------------------
# reading from SacPAS which is connected to the Salvage database 
# Salvage lat/lon
sta_salvage <- data.frame(station = c("CVP", "SWP"),
                          latitude = c(37.815176,37.82524),
                          longitude = c(-121.560709, -121.59523),
                          region = "South")

salvage_ds_data_raw <- read_csv("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=2026&species=26%3Aall&dnaOnly=no&age=no")  %>%
  filter(!is.na(Species)) %>%
  clean_names()

salvage_ds_data <- salvage_ds_data_raw %>%
  mutate(sample_time = ymd_hms(sample_time),
         date = date(sample_time),
         length = as.numeric(length)) %>%
  mutate(source = "salvage") %>%
  mutate(salvage = if_else(!is.na(sample_fraction), nfish/sample_fraction, nfish)) %>%
  select(source, station = facility, date, study_type, catch = nfish, salvage,
         fork_length = length,
         omri = x14_day_omri) %>%
  left_join(sta_salvage)


# read in csv of cumulative wy salvage
salvage <- read_csv(here::here("DeltaSmelt/data/salvage_OMRI_2026.csv")) %>%
  clean_names() %>%
  mutate(date = mdy(date),
         federal_season_salvage_adult = as.numeric(federal_season_salvage_adult),
         federal_season_salvage_juvenile = as.numeric(federal_season_salvage_juvenile))#,
         #OMR = gsub(middle_old_r_net_daily_flow_cfs,pattern = ",", replacement = ""),
         #OMR = replace(OMR, OMR == "ND", NA),
         #OMR = as.numeric(OMR))

# read in operations data (to add flow and pumping)
omr_data <- read_excel(here("Operations/data","CVP Delta OPS_WY26.xlsx"), skip = 1) %>%
  select(date = 1, JPP = 4, CCF = 6, 
         omr_1 = 10) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date),
         date <= as.Date('2026-06-24'), date >= as.Date('2025-10-01')) %>%
  mutate(across(2:4, as.numeric))

# optional combine- graphed separately right now, so not needed
# # combine pumping and salvage data
# salvage_ds <- left_join(salvage, omr_data, by= "date") %>% 
#   mutate(pumping_total= JPP + CCF)

# For older code pulling from CDFW Salvage report, see github for WY 2025 report
# e.g., Copied from Nicole's LFS code. Data adapted from Geir Aasen's (CDFW) salvage report: Copy of SMELT_SALVAGE_TABLES_2025_06232025_END_OF_YEAR_REPORT_


(sal <- ggplot() +
    geom_line(data = salvage, aes(x = date, y = federal_season_salvage_adult), color = "navy", linewidth = 1) +
    geom_line(data = salvage, aes(x = date, y = federal_season_salvage_juvenile), color = "lightblue3", linewidth = 1,
              position = position_dodge(width = 0.2)) +
    scale_x_date(limits = as.Date(c("2025-10-01", "2026-06-30")), date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(limits = c(0, 10), ) +
    annotate(geom = "text", label = "Juvenile Salvage", x = as.Date("2026-01-16"), y = 1, color = "lightblue3") +
    annotate(geom = "text", label = "Adult Salvage", x = as.Date("2026-04-01"), y = 5.5, color = "navy") +
    theme_bw() +
    ylab("DS Cumulative Seasonal Salvage") +
    xlab("Date") +
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

#plot flow
(flow <- ggplot() +
    geom_line(data=omr_data, aes(x=date, y= omr_1), linewidth=1) +
    theme_bw() +
    labs(y = "OMR Index 1-day (cfs)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")+
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_blank()) )

plot_ds_salv <- sal/flow
ggsave("DeltaSmelt/output/2026salvage.png", height=4.8, width=6.4, units="in")
