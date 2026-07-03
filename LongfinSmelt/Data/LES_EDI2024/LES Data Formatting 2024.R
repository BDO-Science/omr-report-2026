# Purpose -----------------------------------------------------------------
# This script compiles data exported from the LES database first and second
# entry tables. It then compares those tables and compiles any discrepancies
# into a HTML file that it saves.


# set working directory ---------------------------------------------------
# modify this to the folder where you've saved this script.
# this will be where the exported csv and html reports are saved. 
setwd("C:\\Users\\MGilbert\\Desktop\\LES EDI")


# set target year ---------------------------------------------------------
# this is used to subset data.
target_year <- c(2022, 2023, 2024)


# library -----------------------------------------------------------------
# central repository for library calls
library(dplyr)
library(tidyr)
library(lubridate)
library(compareDF)
library(htmltools)

# functions ---------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))
# this is my idiosyncratic function to get anything in one table 
# that does not appear in another. Useful for finding what's missing
# between tables.


# fish codes --------------------------------------------------------------
# for easier reference later
# this code assembles the fish codes used by the database and their
# corresponding common names in a table for later joining to the catch table.
FishCodes <- structure(
  list(
    FishCode = structure(
      1:88,
      levels = c(
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "11",
        "12",
        "13",
        "14",
        "15",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23",
        "24",
        "25",
        "26",
        "27",
        "28",
        "29",
        "30",
        "31",
        "32",
        "33",
        "34",
        "35",
        "36",
        "37",
        "38",
        "39",
        "40",
        "41",
        "42",
        "43",
        "44",
        "45",
        "46",
        "47",
        "48",
        "49",
        "50",
        "51",
        "52",
        "53",
        "54",
        "55",
        "56",
        "57",
        "58",
        "59",
        "60",
        "61",
        "62",
        "63",
        "64",
        "65",
        "66",
        "67",
        "68",
        "69",
        "70",
        "71",
        "72",
        "73",
        "74",
        "75",
        "76",
        "77",
        "80",
        "83",
        "84",
        "96",
        "99",
        "2775",
        "2813",
        "2826",
        "2829",
        "3127",
        "7105"
      ),
      class = "factor"
    ),
    CommonName = c(
      "StripedBass",
      "UnidSmelt",
      "LongfinSmelt",
      "DeltaSmelt",
      "UnidHerring",
      "AmericanShad",
      "ThreadfinShad",
      "UnidCatfish",
      "WhiteCatfish",
      "ChannelCatfish",
      "YellowfinGoby",
      "UnidGobies",
      "TidewaterGoby",
      "ArrowGoby",
      "NorthernAnchovy",
      "ChinookSalmon",
      "Warmouth",
      "BlackBullhead",
      "StarryFlounder",
      "ThreespineStickleback",
      "UnidSturgeon",
      "WhiteSturgeon",
      "GreenSturgeon",
      "UnidCyprinids",
      "GoldenShiner",
      "Carp",
      "Goldfish",
      "Splittail",
      "Hardhead",
      "SacramentoPikeminnow",
      "Mosquitofish",
      "UnidSilversides",
      "Topsmelt",
      "Jacksmelt",
      "BayPipefish",
      "UnidCentrarchids",
      "GreenSunfish",
      "BluegillSunfish",
      "LargemouthBass",
      "WhiteCrappie",
      "BlackCrappie",
      "SacramentoPerch",
      "UnidPerches",
      "BigscaleLogperch",
      "UnidSurfperches",
      "TulePerch",
      "ShinerPerch",
      "PacificStaghornSculpin",
      "PricklySculpin",
      "PlainfinMidshipman",
      "WhiteCroaker",
      "PacificHerring",
      "InlandSilverside",
      "BrownBullhead",
      "Steelhead",
      "RedearSunfish",
      "Hitch",
      "ChameleonGoby",
      "BayGoby",
      "UnidLampreys",
      "PacificLamprey",
      "RiverLamprey",
      "UnidSculpins",
      "SacramentoSucker",
      "RainwaterKillifish",
      "ShimofuriGoby",
      "Wakasagi",
      "SmallmouthBass",
      "LongjawMudsucker",
      "CheekspotGoby",
      "SacramentoBlackfish",
      "RedShiner",
      "FatheadMinnow",
      "ShokihazeGoby",
      "TridentigerGobySpecies",
      "CaliforniaTonguefish",
      "SpottedBass",
      "SpeckledSanddab",
      "EnglishSole",
      "UnidFlatfish",
      "BrownRockfish",
      "Unknown",
      "MonkeyfacePrickleback",
      "Pricklebackspp.",
      "PenpointGunnel",
      "SaddlebackGunnel",
      "UnidFlounder",
      "BlueCatfish"
    )
  ),
  row.names = c(NA, -88L),
  class = "data.frame"
)


# read csv files ----------------------------------------------------------
# this code reads in the raw CSV files downloaded from the LES app.
# be sure the file path C:\\temp\\LarvalEntrainment\\csvFiles\\ exists
# then hit "export tables as CSV" from the apps main menu.

waterinfo  <- read.csv("C:\\temp\\LarvalEntrainment\\csvFiles\\WaterInfo.csv")

towinfo    <- read.csv("C:\\temp\\LarvalEntrainment\\csvFiles\\TowInfo.csv")


catch      <- read.csv("C:\\temp\\LarvalEntrainment\\csvFiles\\Catch.csv")

length     <- read.csv("C:\\temp\\LarvalEntrainment\\csvFiles\\Lengths.csv")



# waterinfo formatting ----------------------------------------------------
# this code does some formatting work on the waterinfo table, mostly to 
# remove some funky additions R likes to add during import and formatting dates
# in a readable way.
# for some reason the software appends hh:mm:ss, removing with gsub.
waterinfo$Date <- gsub(" 00:00:00", "", waterinfo$Date)
waterinfo$Date <- as.Date(waterinfo$Date, format = "%m/%d/%Y")
waterinfo$Date <- ymd(waterinfo$Date)

# subset to year ----------------------------------------------------------
# this code subsets the data down to a single year. Set the target year in the
# section "set target year", above.
waterinfo$Year <- year(waterinfo$Date)
waterinfo <- subset(waterinfo, waterinfo$Year %in% target_year)


# now that waterinfo is subset to target year, use WaterInfoID to subset towinfo
towinfo <- subset(towinfo, towinfo$WaterInfoID %in% waterinfo$WaterInfoID)


# now TowInfoID to subset Catch
catch <- subset(catch, catch$TowInfoID %in% towinfo$TowInfoID)


# And CatchID to subset lengths
length <- subset(length, length$CatchID %in% catch$CatchID)


# generate initial ID -----------------------------------------------------
# generate short IDs for WaterInfo. The new column ID_short is the Date, station
# and tow that each row represents. This shorter ID omits the Sample column
# which avoids creating duplicate rows that do not exist within the database.
# ID_short is used to connect the WaterInfo table to the other tables.
id_table_short <- waterinfo %>% select(WaterInfoID, Date, Station)
tow_table_short <- towinfo %>% select(WaterInfoID, Tow)
id_table_short <- left_join(id_table_short, tow_table_short, by="WaterInfoID")
id_table_short <- distinct(id_table_short)
id_table_short$ID_short <- paste(id_table_short$Date, id_table_short$Station, id_table_short$Tow, sep="_")
id_table_short$Date <- NULL
id_table_short$Station <- NULL
id_table_short$Tow <- NULL
id_table_short <- distinct(id_table_short)
id_table_short <- id_table_short[!duplicated(id_table_short[1]),]

waterinfo <- left_join(waterinfo, id_table_short, by="WaterInfoID")


# generate ID's for TowInfo
# this code generates the new column ID_long, which is the date, station, tow
# and sample number. This is used to connect the TowInfo, Catch, and Length 
# tables.
towinfo <- left_join(towinfo, id_table_short, by="WaterInfoID")
tow_id <- towinfo %>% select(TowInfoID, WaterInfoID, Sample, SpecialStudy)
id_table <- left_join(id_table_short, tow_id, by="WaterInfoID")
id_table$ID_long <- paste(id_table$ID_short, "_", id_table$Sample, id_table$SpecialStudy, sep="")
id_table$Sample <- NULL
id_table$WaterInfoID <- NULL
id_table_join <- id_table %>% select(TowInfoID, ID_long)
towinfo <- left_join(towinfo, id_table_join, by="TowInfoID")


# add ID to catch
id_table <- id_table %>% select(TowInfoID, ID_short, ID_long)
id_table <- subset(id_table, id_table$TowInfoID %in% catch$TowInfoID)
id_table <- distinct(id_table)
id_table$TowInfoID <- as.factor(id_table$TowInfoID)
catch$TowInfoID <- as.factor(catch$TowInfoID)

catch<- left_join(catch, id_table, by="TowInfoID")



# add ID to lengths
id_table <- catch %>% select(CatchID, ID_long)
length <- left_join(length, id_table, by="CatchID", relationship = "many-to-many")

# clean up connecting tables and ID stubs to make the environment less awful.
rm(id_table_short, id_table, id_table_join, tow_table_short, tow_id)

# Generating QC indices ---------------------------------------------------
# This codes checks various environmental variables and assigns a condition value to them.
# Condition 1 indicates data with no issues. Condition 3 indicates data that is missing.
#
# GPS Data
# This code formats GPS data for review
waterinfo$StartLat <- as.character(waterinfo$StartLat)
waterinfo$StartLong <- as.character(waterinfo$StartLong)
waterinfo$WaterInfoID <- as.character(waterinfo$WaterInfoID)
# GPS_table isolates GPS variables for review
GPS_table <- waterinfo %>%
  select(WaterInfoID, StartLat, StartLong)
GPS_table <- GPS_table %>% mutate_all(na_if, "")
# GPS_missing identifies samples with missing GPS variables.
GPS_missing <- subset(GPS_table, complete.cases(GPS_table) == FALSE)

# Flowmeter calculations --------------------------------------------------
# Flowmeter Data calculates the total number of flowmeter revolutions.
# Flowmeters reset at one million revolutions - the code below checks for this
# and calculates accordingly.

# water_id  makes a portable stub to connect TowInfo and WaterInfo tables.
water_id <- waterinfo %>%
  select(WaterInfoID, Date, Station)

towinfo$FlowTotal <- ifelse(
  towinfo$NetMeterEnd < towinfo$NetMeterStart,
  ((1000000 + towinfo$NetMeterEnd) - towinfo$NetMeterStart),
  towinfo$NetMeterEnd - towinfo$NetMeterStart)
# constant taken from General Oceanics manual, a pdf is available at
# https://envcoglobal.com/wp-content/uploads/2014/10/2030-flowmete-manual.pdf.
flow_constant <- 26873
# net area in square meters
net_area <- 0.37
# calculating tow volume
towinfo$Distance <- (towinfo$FlowTotal * flow_constant) / 999999
towinfo$Volume <- (3.14 * net_area * towinfo$Distance) / 4
towinfo$Distance <- NULL
# checking for flowmeter totals with unusually high or low values.
tow_meter_errors <- subset(
  towinfo,
  towinfo$FlowTotal < 3000 |
    towinfo$FlowTotal > 50000 | is.na(towinfo$FlowTotal) == TRUE
)
meter_check <- tow_meter_errors %>%
  select(TowInfoID, WaterInfoID, FlowTotal, Volume)
water_id$WaterInfoID <- as.factor(water_id$WaterInfoID)
meter_check$WaterInfoID <- as.factor(meter_check$WaterInfoID)
meter_check <- right_join(water_id, meter_check, by = join_by(WaterInfoID))

# Applying Condition ------------------------------------------------------
# see opening notes for description
# Assigning Condition 3 to missing spatial data
waterinfo$GPSCondition <- ifelse(waterinfo$WaterInfoID %in% GPS_missing$WaterInfoID, 3, 1)

# Assigning Condition 3 to anomalous flow data
towinfo$FlowCondition <- ifelse(towinfo$WaterInfoID %in% meter_check$WaterInfoID, 3, 1)

# Fixing Time
# function for extracting characters from the left hand side;
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

towinfo$Time <- substrRight(towinfo$Time, 8)
towinfo$Time <- substr(towinfo$Time, 1,5)

# final transformations of waterinfo and towinfo before combining them
waterinfo <- waterinfo %>%
  select(
    ID_short,
    WaterInfoID,
    Survey,
    Date,
    Station,
    GearType,
    StartLat,
    StartLong,
    GPSCondition,
    TopEC,
    BottomEC,
    Secchi,
    Turbidity,
    Temp,
    Tide
  )

towinfo <- towinfo %>%
  select(
    ID_long,
    WaterInfoID,
    TowInfoID,
    Tow,
    Sample,
    Time,
    SpecialStudy,
    BottomDepth,
    CableOut,
    Duration,
    NetMeterStart,
    NetMeterEnd,
    FlowTotal,
    FlowCondition,
    Volume
  )


# combining WaterInfo and TowInfo -----------------------------------------
waterinfo$WaterInfoID <- as.factor(waterinfo$WaterInfoID)
towinfo$WaterInfoID <- as.factor(towinfo$WaterInfoID)
combined_environmental <- left_join(waterinfo, towinfo, by = "WaterInfoID")
combined_environmental <- subset(combined_environmental, is.na(combined_environmental$TowInfoID)==FALSE)


# catch -------------------------------------------------------------------

# Combining FishCodes and Catch to incorporate common name.
catch$FishCode <- as.factor(catch$FishCode)
catch <- left_join(catch, FishCodes, by = "FishCode")

# combining with catch
catch <- catch %>%
  select(TowInfoID, ID_long, CommonName, Catch, CatchID)
catch_join <- catch %>%
  select(TowInfoID, CommonName, Catch, CatchID)
catch_join$TowInfoID <- as.factor(catch$TowInfoID)
combined_environmental$TowInfoID <- as.factor(combined_environmental$TowInfoID)
combined_table <- left_join(combined_environmental, catch_join, by = "TowInfoID")
combined_table$CommonName[is.na(combined_table$CommonName)] = "NoCatch"
combined_table$Catch[is.na(combined_table$Catch)] = "0"

# length ------------------------------------------------------------------
catch_stub <- catch %>% 
  select(CatchID, CommonName)
length <- left_join(length, catch_stub, by="CatchID")
length$MeasuredCatch <- 1

length_summary <- length %>% 
  group_by(Length, ID_long, CommonName, CatchID) %>% 
  summarise(MeasuredCatch = sum(MeasuredCatch))

# Creating length frequency table -----------------------------------------
# This code generates a length frequency and calculates adjusted values
# based on the number of fish in a sample that were measured.
# the number of unique lengths will be important later.
unique_lengths <- n_distinct(length_summary$Length)+2

# Pivoting to create a length frequency table.
length_summary$MeasuredCatch <- as.numeric(length_summary$MeasuredCatch)
lfrq <- length_summary %>%
  pivot_wider(names_from = Length, values_from = MeasuredCatch)
lfrq[is.na(lfrq)] <- 0
lfrq$MeasuredCatch <- rowSums(lfrq[,4:65])
catch_lfrq_join <- catch %>% 
  select(CatchID, Catch)
lfrq <- left_join(lfrq, catch_lfrq_join, by="CatchID")
lfrq$UnmeasuredCatch <- lfrq$Catch-lfrq$MeasuredCatch

lfrq_check <- subset(lfrq, lfrq$UnmeasuredCatch<0)
lfrq_check <- lfrq_check %>% 
  select(ID_long, CatchID, UnmeasuredCatch, Catch)
# LFRQ check is a useful tool for detecting errors/discrepancies between the 
# Catch and lengths table, i.e, when too many lengths were entered or an
# inaccurate catch value was entered.

# relative length frequency table -----------------------------------------
#Function to automate adjusted lfrq
adjusted_frequency <- function(x) {
  x + ((x / lfrq$MeasuredCatch) * lfrq$UnmeasuredCatch)
}
lfrq_r <- lfrq
lfrq_r[, 4:65] <- lapply(lfrq_r[, 4:65], adjusted_frequency)
lfrq_r$Catch <- NULL
lfrq_r$MeasuredCatch <- NULL
lfrq_r$UnmeasuredCatch <- NULL
# now we pivot taller to create a catch table with the length frequency and 
# adjusted length frequency.
lfrq_r <- pivot_longer(lfrq_r,
                     cols = 4:65,
                     names_to = "Length",
                     values_to = "AdjustedFrequency")
lfrq_r <- subset(lfrq_r, lfrq_r$AdjustedFrequency != 0)
lfrq_r$ID_long <- NULL
lfrq_r$CommonName <- NULL
combined_table <- left_join(combined_table, lfrq_r, by = "CatchID")
sum(combined_table$AdjustedFrequency, na.rm=TRUE)
sum(catch$Catch, na.rm=TRUE)
combined_table <- combined_table %>%
  select(
    ID_short,
    ID_long,
    WaterInfoID,
    TowInfoID,
    CatchID,
    Date,
    Time,
    Survey,
    Station,
    Tow,
    Sample,
    GearType,
    SpecialStudy,
    StartLat,
    StartLong,
    GPSCondition,
    TopEC,
    BottomEC,
    Secchi,
    Turbidity,
    Temp,
    Tide,
    BottomDepth,
    CableOut,
    Duration,
    NetMeterStart,
    NetMeterEnd,
    FlowTotal,
    FlowCondition,
    Volume,
    CommonName,
    Catch,
    Length,
    AdjustedFrequency
  )
combined_table$AdjustedFrequency <- round(combined_table$AdjustedFrequency, digits=2)
length$MeasuredCatch <- NULL

write.csv(combined_table, "LES_combined_data.csv", row.names = FALSE)
write.csv(catch, "Catch.csv", row.names = FALSE)
write.csv(length, "Length.csv", row.names = FALSE)
write.csv(waterinfo, "WaterInfo.csv", row.names = FALSE)
write.csv(towinfo, "TowInfo.csv", row.names = FALSE)
