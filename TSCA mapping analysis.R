##############################################################
### Data processing code for the Environmental Defense Fund's
### Chemical Exposure Action Map
###
### Final release, 8/8/2023
### Updated by Paige Varner, 5/7/2026
### by Jeremy Proville and Mary Collins, Environmental Defense Fund (edf.org)
### for more visit https://github.com/proville/TSCA-Map
##############################################################

rm(list = ls())
library(readxl)
library(maps)
library(ggplot2)
library(dplyr)
library(sf)
#library(rgdal)
library(openxlsx)
library(progress)
library(tigris)
library(stringr)
library(tidyr)

raw=read.csv("data/DemographicImpactsofTRIv2.csv")
naics=read.xlsx("data/NAICS.xlsx")
weights=read.xlsx("data/TSCA Fenceline Map Chem Hazard Data Extraction_v2.xlsx",sheet=3,startRow = 2)


####################################
#Weights

colnames(weights) <- make.unique(colnames(weights))

weights <- weights %>%
  mutate(across(where(is.character), ~ case_when(
    . == "Carbon Tetrachloride" ~ "Carbon tetrachloride",
    . == "Trichloroethylene (TCE)" ~ "Trichloroethylene",
    . == "Tetrachloroethylene (PCE)" ~ "Tetrachloroethylene",
    . == "Di-ethylhexyl phthalate (DEHP)" ~ "Di(2-ethylhexyl) phthalate",
    . == "1,2-Dibromoethane (ethylene dibromide)" ~ "1,2-Dibromoethane (Ethylene dibromide)",
    . == "1,2-Dichloroethane (ethylene dichloride)" ~ "1,2-Dichloroethane",
    . == "Phthalic anhydride (PA)" ~ "Phthalic anhydride",
    . == "Methylene chloride" ~ "Dichloromethane (Methylene chloride)",
    . == "Cyclic Aliphatic Bromide Cluster (HBCD)" ~ "Hexabromocyclododecane",
    . == "Dibutyl phthalate (DBP)" ~ "Dibutyl phthalate",
    . == "N-Methyl-2-pyrrolidone (NMP)" ~ "N-Methyl-2-pyrrolidone",
    . == "1,1-Dichloroethane" ~ "Ethylidene dichloride (1,1-Dichloroethane)",
    . == "Benzenamine (aniline)" ~ "Aniline",
    . == "4,4′-Methylene bis(2-chloroaniline) (MBOCA)" ~ "4,4'-Methylenebis(2-chloroaniline)",
    TRUE ~ .
  )))

Cancer_weights <- weights[, c(1, 5)]
Cancer_weights <- Cancer_weights[complete.cases(Cancer_weights), ]
Dev_weights <- weights[, c(6, 10)]
Dev_weights <- Dev_weights[complete.cases(Dev_weights), ]
Asthma_weights <- weights[, c(11, 16)]
Asthma_weights <- Asthma_weights[complete.cases(Asthma_weights), ]

####################################
#Adding PFAS compounds at the same facility into one chemical identifier

pfas_facility_summary <- raw %>%
  filter(PFAS == TRUE) %>%                      # keep only PFAS chemicals
  group_by(FacilityID) %>%
  summarise(
    # keep other columns (since they’re identical within facility)
    across(
      .cols = -c(PoundsReleased_2018, PoundsReleased_2019,
                 PoundsReleased_2020, PoundsReleased_2021,
                 PoundsReleased_2022, Chemical, PFAS),
      .fns = first
    ),
    
    # sum yearly release columns
    PoundsReleased_2018 = sum(PoundsReleased_2018, na.rm = TRUE),
    PoundsReleased_2019 = sum(PoundsReleased_2019, na.rm = TRUE),
    PoundsReleased_2020 = sum(PoundsReleased_2020, na.rm = TRUE),
    PoundsReleased_2021 = sum(PoundsReleased_2021, na.rm = TRUE),
    PoundsReleased_2022 = sum(PoundsReleased_2022, na.rm = TRUE),
    
    # collapse chemical names into one column
    PFAS_chemicals = paste(unique(Chemical), collapse = ", "),
    
    # Make this a proper “chemical row” and flag as PFAS facility
    Chemical = "PFAS",
    PFAS = TRUE,
    
    .groups = "drop"
  )

#Remove PFAS rows from raw
raw_no_pfas <- raw %>%
  filter(PFAS == FALSE)

#Add back into raw
raw <- bind_rows(raw_no_pfas, pfas_facility_summary)

####################################
#Calcs for releases across years 
release_mat <- raw[, c(
  "PoundsReleased_2018",
  "PoundsReleased_2019",
  "PoundsReleased_2020",
  "PoundsReleased_2021",
  "PoundsReleased_2022"
)]

release_mat <- as.data.frame(lapply(release_mat, as.numeric))

raw$PoundsReleased_5yr_min <- apply(release_mat, 1, min, na.rm = TRUE)
raw$PoundsReleased_5yr_max <- apply(release_mat, 1, max, na.rm = TRUE)
raw$PoundsReleased_5yr_sum <- apply(release_mat, 1, max, na.rm = TRUE)
####################################
#Health outcome calcs
#CANCER chemicals only
Cancer <- c("1,1,2-Trichloroethane", "1,2-Dibromoethane (Ethylene dibromide)", "1,2-Dichloroethane", "1,2-Dichloropropane", "1,3-Butadiene", "1,4-Dichlorobenzene (p-Dichlorobenzene)", "1,4-Dioxane", "1-Bromopropane", "Asbestos (friable)", "Carbon tetrachloride", "Di(2-ethylhexyl) phthalate", "Formaldehyde", "Dichloromethane (Methylene chloride)", "Tetrachloroethylene", "Trichloroethylene", "Tetrabromobisphenol A", "Acetaldehyde", "Acrylonitrile", "Aniline", "Vinyl chloride", "4,4'-Methylenebis(2-chloroaniline)", "PFAS")

raw$Cancer_PoundsReleased_5yr_min <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Cancer, min(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})
raw$Cancer_PoundsReleased_5yr_max <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Cancer, max(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Cancer_PoundsReleased_5yr_sum <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Cancer, max(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})


raw$Cancer_weighted_5yr_sum <- apply(raw, MARGIN = 1, FUN = function(x) {
  lbs<-as.numeric(x[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")])
  max_value <- max(lbs, na.rm = TRUE)  # Get the maximum value
  chemical <- x[["Chemical"]]  # Get the corresponding 'Chemical' value 
  weight <- Cancer_weights$Weight[Cancer_weights$Chemical.Name == chemical]  # Lookup the weight based on 'Chemical' value
  result=as.numeric(max_value * weight)
  if (length(result) == 0) {
    result <- 0
  }
  return(result)
})


#DEV chemicals only
Dev <- c("1,1-Dichloroethane", "1,2-Dibromoethane (Ethylene dibromide)", "1,2-Dichloropropane", "1,3-Butadiene", "1,4-Dioxane", "1-Bromopropane", "Dibutyl phthalate", "Di(2-ethylhexyl) phthalate", "N-Methyl-2-pyrrolidone", "Tetrachloroethylene", "Trichloroethylene", "Hexabromocyclododecane", "Acrylonitrile", "Vinyl chloride", "PFAS")

raw$Dev_PoundsReleased_5yr_min <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Dev, min(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Dev_PoundsReleased_5yr_max <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Dev, max(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Dev_PoundsReleased_5yr_sum <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Dev, max(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Dev_weighted_5yr_sum <- apply(raw, MARGIN = 1, FUN = function(x) {
  lbs <- as.numeric(x[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")])
  max_value <- max(lbs, na.rm = TRUE)
  chemical <- x[["Chemical"]]
  weight <- Dev_weights$Weight[Dev_weights$Chemical.Name == chemical]
  result <- as.numeric(max_value * weight)
  if (length(result) == 0) {
    result <- 0
  }
  return(result)
})


#ASTHMA chemicals only
Asthma <- c("1,2-Dibromoethane (Ethylene dibromide)", "Formaldehyde", "Phthalic anhydride", "Acetaldehyde")

raw$Asthma_PoundsReleased_5yr_min <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Asthma, min(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Asthma_PoundsReleased_5yr_max <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Asthma, max(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Asthma_PoundsReleased_5yr_sum <- apply(raw, 1, function(row) {
  ifelse(row["Chemical"] %in% Asthma, max(as.numeric(row[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")]), na.rm = TRUE), 0)
})

raw$Asthma_weighted_5yr_sum <- apply(raw, MARGIN = 1, FUN = function(x) {
  lbs <- as.numeric(x[c("PoundsReleased_2018", "PoundsReleased_2019", "PoundsReleased_2020", "PoundsReleased_2021", "PoundsReleased_2022")])
  max_value <- max(lbs, na.rm = TRUE)
  chemical <- x[["Chemical"]]
  weight <- Asthma_weights$Weight[Asthma_weights$Chemical.Name == chemical]
  result <- as.numeric(max_value * weight)
  if (length(result) == 0) {
    result <- 0
  }
  return(result)
})

####################################
# FACILITY-LEVEL AGGREGATION TABLE (NEW STEP)

names(raw)

facility_level <- raw %>%
  group_by(FacilityID) %>%
  summarise(
    
    # keep one row per facility
    across(c(FacilityName, Street, City, County, State, ZIPCode,
             Longitude, Latitude, ModeledNAICS),
           first),
    
    # RELEASE METRICS (aggregate across chemicals)
    PoundsReleased_5yr_min = min(PoundsReleased_5yr_min, na.rm = TRUE),
    PoundsReleased_5yr_sum = sum(PoundsReleased_5yr_sum, na.rm = TRUE),
    PoundsReleased_5yr_max = max(PoundsReleased_5yr_max, na.rm = TRUE),
    
    Cancer_PoundsReleased_5yr_min = min(Cancer_PoundsReleased_5yr_min, na.rm = TRUE),
    Cancer_PoundsReleased_5yr_sum = sum(Cancer_PoundsReleased_5yr_sum, na.rm = TRUE),
    Cancer_PoundsReleased_5yr_max = max(Cancer_PoundsReleased_5yr_max, na.rm = TRUE),
    
    Dev_PoundsReleased_5yr_min = min(Dev_PoundsReleased_5yr_min, na.rm = TRUE),
    Dev_PoundsReleased_5yr_sum = sum(Dev_PoundsReleased_5yr_sum, na.rm = TRUE),
    Dev_PoundsReleased_5yr_max = max(Dev_PoundsReleased_5yr_max, na.rm = TRUE),
    
    Asthma_PoundsReleased_5yr_min = min(Asthma_PoundsReleased_5yr_min, na.rm = TRUE),
    Asthma_PoundsReleased_5yr_sum = sum(Asthma_PoundsReleased_5yr_sum, na.rm = TRUE),
    Asthma_PoundsReleased_5yr_max = max(Asthma_PoundsReleased_5yr_max, na.rm = TRUE),
    
    # WEIGHTED METRICS (sum across chemicals)
    Cancer_weighted_5yr_sum = sum(Cancer_weighted_5yr_sum, na.rm = TRUE),
    Dev_weighted_5yr_sum    = sum(Dev_weighted_5yr_sum, na.rm = TRUE),
    Asthma_weighted_5yr_sum = sum(Asthma_weighted_5yr_sum, na.rm = TRUE),
    
    .groups = "drop"
  )

#join facility metrics back to row-level data
raw <- raw %>%
  left_join(
    facility_level %>%
      select(FacilityID,
             PoundsReleased_5yr_min, PoundsReleased_5yr_sum, PoundsReleased_5yr_max,
             Cancer_PoundsReleased_5yr_min, Cancer_PoundsReleased_5yr_sum, Cancer_PoundsReleased_5yr_max,
             Dev_PoundsReleased_5yr_min, Dev_PoundsReleased_5yr_sum, Dev_PoundsReleased_5yr_max,
             Asthma_PoundsReleased_5yr_min, Asthma_PoundsReleased_5yr_sum, Asthma_PoundsReleased_5yr_max,
             Cancer_weighted_5yr_sum, Dev_weighted_5yr_sum, Asthma_weighted_5yr_sum),
    by = "FacilityID",
    suffix = c(".old", "")
  ) %>%
  
  # remove old chemical-level versions
  select(-ends_with(".old"))

####################################
#District & State Summary Info 

library(sf)
library(dplyr)
library(tigris)

# Load districts
districts <- tigris::congressional_districts(cb = TRUE, year = 2024)
districts <- st_transform(districts, crs = 4326)

# Convert raw to sf
raw_sf <- raw %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Spatial join
raw_districts <- st_join(raw_sf, districts, join = st_within)

# FIX 1: REMOVE .x / .y DUPLICATES IMMEDIATELY
raw_districts <- raw_districts %>%
  mutate(across(ends_with(".x"), ~ .)) %>%
  rename_with(~ gsub("\\.x$", "", .x)) %>%
  select(-ends_with(".y"))

# Optional sanity check
names(raw_districts)

# State-level summary
state_summary <- raw_districts %>%
  st_drop_geometry() %>%
  distinct(State, FacilityID, PoundsReleased_5yr_sum, Chemical) %>%
  group_by(State) %>%
  summarise(
    state_num_facilities = n_distinct(FacilityID),
    state_num_chemicals = n_distinct(Chemical),
    state_sum_releases = sum(PoundsReleased_5yr_sum, na.rm = TRUE),
    .groups = "drop"
  )

# District-level summary
district_summary <- raw_districts %>%
  st_drop_geometry() %>%
  distinct(GEOID, FacilityID, PoundsReleased_5yr_sum, Chemical) %>%
  group_by(GEOID) %>%
  summarise(
    district_num_facilities = n_distinct(FacilityID),
    district_num_chemicals = n_distinct(Chemical),
    district_sum_releases = sum(PoundsReleased_5yr_sum, na.rm = TRUE),
    .groups = "drop"
  )

# Re-attach summaries WITHOUT geometry corruption
raw_districts <- raw_districts %>%
  left_join(state_summary, by = "State") %>%
  left_join(district_summary, by = "GEOID")

# Ensure coordinates are clean
coords <- st_coordinates(raw_districts)
raw_districts$Longitude <- coords[, "X"]
raw_districts$Latitude <- coords[, "Y"]

####################################
#Collapsing data to single facility per row (don't want to do this for the de-aggregated data, but renaming the raw_districts df to raw_collapsed so the code can stay consistent)

raw_collapsed = raw_districts %>%
  left_join(facility_level, by = "FacilityID")

# Remove duplicate .x/.y columns after join
raw_collapsed <- raw_collapsed %>%
  select(-ends_with(".x")) %>%   # remove old pre-aggregation vars
  rename_with(~ gsub("\\.y$", "", .x), ends_with(".y"))

#raw_collapsed <- raw_districts %>%
# mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
#group_by(FacilityID) %>%
#summarise(
# across(PoundsReleased_5yr_min, min), 
#across(PoundsReleased_5yr_sum, sum), 
#across(Cancer_PoundsReleased_5yr_min, min), 
#across(Cancer_PoundsReleased_5yr_sum, sum), 
#across(Dev_PoundsReleased_5yr_min, min), 
#across(Dev_PoundsReleased_5yr_sum, sum), 
#across(Asthma_PoundsReleased_5yr_min, min), 
#across(Asthma_PoundsReleased_5yr_sum, sum),
#across(Cancer_weighted_5yr_sum, sum),
#across(Dev_weighted_5yr_sum, sum),
#across(Asthma_weighted_5yr_sum, sum),
#Longitude = first(Longitude),
#Latitude  = first(Latitude),
#Chemical = toString(unique(Chemical)),
#GEOID = first(GEOID),
#District = first(NAMELSAD),  # or whatever your district column name is
#across(everything(), ~ if (is.numeric(.)) max(.) else first(.)),
#.groups = "drop"
#)


#Renaming some mismatched demographic columns
raw_collapsed <- raw_collapsed %>%
  rename(housing_units_county = housing_unitsE_county, avg_median_income_county = medincomeE_county, avg_median_house_value_county = median_house_valueE_county, housing_units_10km = housing_unitsE_10km, avg_median_income_10km = medincomeE_10km, avg_median_house_value_10km = median_house_valueE_10km, population_10km = populationE_10km, population_county = populationE_county)

####################################
#Spatial buffer calcs

# Convert 'raw_collapsed' data frame to an sf object
raw_collapsed_sf <- st_as_sf(raw_collapsed, coords = c("Longitude", "Latitude"), crs = st_crs(4326),remove=FALSE)

# IMPORTANT: collapse spatial layer to unique facilities ONLY for buffer calculations
buffer_sf <- raw_collapsed_sf %>%
  group_by(FacilityID) %>%
  slice(1) %>%
  ungroup()

buffer_sf$`10km_Pounds_sum`=0
buffer_sf$`10km_Pounds_min`=0
buffer_sf$`10km_Pounds_max`=0
buffer_sf$`10km_Cancer_Pounds_sum`=0
buffer_sf$`10km_Cancer_Pounds_min`=0
buffer_sf$`10km_Cancer_Pounds_max`=0
buffer_sf$`10km_Dev_Pounds_sum`=0
buffer_sf$`10km_Dev_Pounds_min`=0
buffer_sf$`10km_Dev_Pounds_max`=0
buffer_sf$`10km_Asthma_Pounds_sum`=0
buffer_sf$`10km_Asthma_Pounds_min`=0
buffer_sf$`10km_Asthma_Pounds_max`=0

buffer_sf <- buffer_sf %>%
  mutate(`10km_facnum` = lengths(st_within(st_geometry(.), st_buffer(st_geometry(.), dist = 10000))))

#Init progress bar for buffer spatial calcs
n_rows <- nrow(buffer_sf)
progress_bar <- txtProgressBar(min = 0, max = n_rows, style = 3)

points_within_radius=0

#All facility buffer calcs
for (i in seq_len(n_rows)) {
  geom <- st_geometry(buffer_sf)[i]
  buffer <- st_buffer(geom, dist = 10000)
  distances <- st_distance(buffer_sf, buffer)
  points_within_radius <- buffer_sf[as.numeric(distances) <= 10000, ]
  sum_sum <- sum(points_within_radius$PoundsReleased_5yr_sum, na.rm = TRUE)
  sum_min <- sum(points_within_radius$PoundsReleased_5yr_min, na.rm = TRUE)
  sum_max <- sum(points_within_radius$PoundsReleased_5yr_max, na.rm = TRUE)
  buffer_sf$`10km_Pounds_sum`[i] <- sum_sum
  buffer_sf$`10km_Pounds_min`[i] <- sum_min
  buffer_sf$`10km_Pounds_max`[i] <- sum_max
  setTxtProgressBar(progress_bar, i)
}

#Cancer risk buffer calcs
for (i in seq_len(n_rows)) {
  geom <- st_geometry(buffer_sf)[i]
  buffer <- st_buffer(geom, dist = 10000)
  distances <- st_distance(buffer_sf, buffer)
  points_within_radius <- buffer_sf[as.numeric(distances) <= 10000, ]
  sum_sum <- sum(points_within_radius$Cancer_PoundsReleased_5yr_sum, na.rm = TRUE)
  sum_min <- sum(points_within_radius$Cancer_PoundsReleased_5yr_min, na.rm = TRUE)
  sum_max <- sum(points_within_radius$Cancer_PoundsReleased_5yr_max, na.rm = TRUE)
  buffer_sf$`10km_Cancer_Pounds_sum`[i] <- sum_sum
  buffer_sf$`10km_Cancer_Pounds_min`[i] <- sum_min
  buffer_sf$`10km_Cancer_Pounds_max`[i] <- sum_max
  setTxtProgressBar(progress_bar, i)
}

#Dev risk buffer calcs
for (i in seq_len(n_rows)) {
  geom <- st_geometry(buffer_sf)[i]
  buffer <- st_buffer(geom, dist = 10000)
  distances <- st_distance(buffer_sf, buffer)
  points_within_radius <- buffer_sf[as.numeric(distances) <= 10000, ]
  sum_sum <- sum(points_within_radius$Dev_PoundsReleased_5yr_sum, na.rm = TRUE)
  sum_min <- sum(points_within_radius$Dev_PoundsReleased_5yr_min, na.rm = TRUE)
  sum_max <- sum(points_within_radius$Dev_PoundsReleased_5yr_max, na.rm = TRUE)
  buffer_sf$`10km_Dev_Pounds_sum`[i] <- sum_sum
  buffer_sf$`10km_Dev_Pounds_min`[i] <- sum_min
  buffer_sf$`10km_Dev_Pounds_max`[i] <- sum_max
  setTxtProgressBar(progress_bar, i)
}

#Asthma risk buffer calcs
for (i in seq_len(n_rows)) {
  geom <- st_geometry(buffer_sf)[i]
  buffer <- st_buffer(geom, dist = 10000)
  distances <- st_distance(buffer_sf, buffer)
  points_within_radius <- buffer_sf[as.numeric(distances) <= 10000, ]
  sum_sum <- sum(points_within_radius$Asthma_PoundsReleased_5yr_sum, na.rm = TRUE)
  sum_min <- sum(points_within_radius$Asthma_PoundsReleased_5yr_min, na.rm = TRUE)
  sum_max <- sum(points_within_radius$Asthma_PoundsReleased_5yr_max, na.rm = TRUE)
  buffer_sf$`10km_Asthma_Pounds_sum`[i] <- sum_sum
  buffer_sf$`10km_Asthma_Pounds_min`[i] <- sum_min
  buffer_sf$`10km_Asthma_Pounds_max`[i] <- sum_max
  setTxtProgressBar(progress_bar, i)
}

close(progress_bar)

buffer_sf$'10km_facnum' = buffer_sf$'10km_facnum' - 1


####################################
# Add NAICS descriptions

buffer_sf <- buffer_sf %>%
  left_join(naics, by = c("ModeledNAICS" = "NAICS")) %>%
  rename(NAICS = `2022.NAICS.US.Title`)

####################################
# Demographic calculations

demovars <- c(
  "WhtPercent", "NWPercent","HispPercent", "BlkPercent",
  "AsianPercent", "AmerIndPercent", "Under5Percent",
  "ReprodFemPercent", "Over64Percent","EduPercent",
  "housing_units","VacPercent","OwnOccPercent",
  "avg_median_income","avg_median_house_value"
)

for (v in demovars) {
  
  new_col <- paste0(v, "_change")
  
  buffer_sf[[new_col]] <-
    (
      buffer_sf[[paste0(v, "_10km")]] -
        buffer_sf[[paste0(v, "_county")]]
    ) /
    buffer_sf[[paste0(v, "_county")]] * 100
}

####################################
# Percentile rank variables

buffer_sf <- buffer_sf %>%
  mutate(
    Pounds_5yr_perc =
      percent_rank(PoundsReleased_5yr_max) * 100,
    
    Cancer_Pounds_5yr_perc =
      percent_rank(Cancer_PoundsReleased_5yr_max) * 100,
    
    Dev_Pounds_5yr_perc =
      percent_rank(Dev_PoundsReleased_5yr_max) * 100,
    
    Asthma_Pounds_5yr_perc =
      percent_rank(Asthma_PoundsReleased_5yr_max) * 100
  )

####################################
# Health risk count

buffer_sf$Health_Risk_Count <- 0

for (i in 1:nrow(buffer_sf)) {
  
  if (buffer_sf$Cancer_PoundsReleased_5yr_sum[i] > 0)
    buffer_sf$Health_Risk_Count[i] <-
      buffer_sf$Health_Risk_Count[i] + 1
  
  if (buffer_sf$Dev_PoundsReleased_5yr_sum[i] > 0)
    buffer_sf$Health_Risk_Count[i] <-
      buffer_sf$Health_Risk_Count[i] + 1
  
  if (buffer_sf$Asthma_PoundsReleased_5yr_sum[i] > 0)
    buffer_sf$Health_Risk_Count[i] <-
      buffer_sf$Health_Risk_Count[i] + 1
}

####################################
#Cleanup & Export 

# Rename district column in chemical-level dataset
names(raw_districts)[names(raw_districts) == "NAMELSAD"] <- "District"

names(buffer_sf)

#CLEAN BUFFER DATA (FIXED)

buffer_df <- buffer_sf %>%
  sf::st_drop_geometry() %>%
  as.data.frame()

buffer_df <- buffer_df %>%
  select(
    FacilityID,
    
    # buffer exposure metrics
    starts_with("10km_"),
    
    # demographic levels
    population_10km,
    WhtPercent_10km,
    NWPercent_10km,
    HispPercent_10km,
    BlkPercent_10km,
    AsianPercent_10km,
    AmerIndPercent_10km,
    Under5Percent_10km,
    ReprodFemPercent_10km,
    Over64Percent_10km,
    EduPercent_10km,
    housing_units_10km,
    VacPercent_10km,
    OwnOccPercent_10km,
    avg_median_income_10km,
    avg_median_house_value_10km,
    
    population_county,
    WhtPercent_county,
    NWPercent_county,
    HispPercent_county,
    BlkPercent_county,
    AsianPercent_county,
    AmerIndPercent_county,
    Under5Percent_county,
    ReprodFemPercent_county,
    Over64Percent_county,
    EduPercent_county,
    housing_units_county,
    VacPercent_county,
    OwnOccPercent_county,
    avg_median_income_county,
    avg_median_house_value_county,
    
    # derived metrics
    Health_Risk_Count,
    Pounds_5yr_perc,
    Cancer_Pounds_5yr_perc,
    Dev_Pounds_5yr_perc,
    Asthma_Pounds_5yr_perc,
    
    WhtPercent_change,
    NWPercent_change,
    HispPercent_change,
    BlkPercent_change,
    AsianPercent_change,
    AmerIndPercent_change,
    Under5Percent_change,
    ReprodFemPercent_change,
    Over64Percent_change,
    EduPercent_change,
    
    NAICS
  )

#JOIN BACK TO DATA
final <- raw_districts %>%
  left_join(buffer_df, by = "FacilityID", suffix = c("", "_buffer"))

#KEEP ONLY DESIRED COLUMNS
final <- final %>%
  select(
    FacilityID,
    FacilityName,
    Street,
    City,
    County,
    State,
    ZIPCode,
    Longitude,
    Latitude,
    Chemical,
    PFAS,
    PFAS_chemicals,
    PoundsReleased_2018,
    PoundsReleased_2019,
    PoundsReleased_2020,
    PoundsReleased_2021,
    PoundsReleased_2022,
    PoundsReleased_5yr_min,
    PoundsReleased_5yr_sum,
    PoundsReleased_5yr_max,
    Pounds_5yr_perc,
    Cancer_PoundsReleased_5yr_min,
    Cancer_PoundsReleased_5yr_sum,
    Cancer_PoundsReleased_5yr_max,
    Cancer_Pounds_5yr_perc,
    Cancer_weighted_5yr_sum,
    Dev_PoundsReleased_5yr_min,
    Dev_PoundsReleased_5yr_sum,
    Dev_PoundsReleased_5yr_max,
    Dev_Pounds_5yr_perc,
    Dev_weighted_5yr_sum,
    Asthma_PoundsReleased_5yr_min,
    Asthma_PoundsReleased_5yr_sum,
    Asthma_PoundsReleased_5yr_max,
    Asthma_Pounds_5yr_perc,
    Asthma_weighted_5yr_sum,
    state_num_facilities,
    state_num_chemicals,
    state_sum_releases,
    District,
    GEOID,
    district_num_facilities,
    district_num_chemicals,
    district_sum_releases,
    population_10km,
    WhtPercent_10km,
    NWPercent_10km,
    HispPercent_10km,
    BlkPercent_10km,
    AsianPercent_10km,
    AmerIndPercent_10km,
    Under5Percent_10km,
    ReprodFemPercent_10km,
    Over64Percent_10km,
    EduPercent_10km,
    housing_units_10km,
    VacPercent_10km,
    OwnOccPercent_10km,
    avg_median_income_10km,
    avg_median_house_value_10km,
    population_county,
    WhtPercent_county,
    NWPercent_county,
    HispPercent_county,
    BlkPercent_county,
    AsianPercent_county,
    AmerIndPercent_county,
    Under5Percent_county,
    ReprodFemPercent_county,
    Over64Percent_county,
    EduPercent_county,
    housing_units_county,
    VacPercent_county,
    OwnOccPercent_county,
    avg_median_income_county,
    avg_median_house_value_county,
    WhtPercent_change,
    NWPercent_change,
    HispPercent_change,
    BlkPercent_change,
    AsianPercent_change,
    AmerIndPercent_change,
    Under5Percent_change,
    ReprodFemPercent_change,
    Over64Percent_change,
    EduPercent_change,
    NAICS,
    `10km_Pounds_sum`,
    `10km_Pounds_min`,
    `10km_Pounds_max`,
    `10km_Cancer_Pounds_sum`,
    `10km_Cancer_Pounds_min`,
    `10km_Cancer_Pounds_max`,
    `10km_Dev_Pounds_sum`,
    `10km_Dev_Pounds_min`,
    `10km_Dev_Pounds_max`,
    `10km_Asthma_Pounds_sum`,
    `10km_Asthma_Pounds_min`,
    `10km_Asthma_Pounds_max`,
    `10km_facnum`,
    Health_Risk_Count
  )

# fix zip code issue
final$ZIPCode <- as.character(final$ZIPCode)
final$ZIPCode <- str_pad(final$ZIPCode, width = 5, side = "left", pad = "0")

# Export table
write.xlsx(
  st_drop_geometry(final),
  file = "~/TSCA/Fenceline Map/TSCA-Map/data/TSCA_PFASupdate_May2026.xlsx",
  rowNames = FALSE
)

# Export geopackage
gdb_path <- "~/TSCA/Fenceline Map/TSCA-Map/data/TSCA_PFASupdate_May2026.gpkg"

st_write(
  final,
  gdb_path,
  driver = "GPKG",
  layer_options = "OVERWRITE=yes",
  append = FALSE
)

###################################################################
#QC code
####################################
# Create a map of the United States
map_data <- map_data("state")

us_map <- ggplot(map_data, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group), fill="white", color="black")

# Add points from 'raw' data frame to the map
us_map <- us_map + geom_point(data=raw, aes(x=Longitude, y=Latitude))

# Display the map with points
us_map

# Set fixed size and aspect ratio for map
map_size <- theme(
  plot.background = element_rect(fill = "white"),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_text(hjust = 0.5),
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank()
)

# Loop through numeric columns and add points to the map, colored by column
for (col in names(raw)[sapply(raw, is.numeric)]) {
  us_map <- ggplot(map_data, aes(x=long, y=lat)) +
    geom_polygon(aes(group=group), fill="white", color="black") +
    geom_point(data=raw, aes(x=Longitude, y=Latitude, color=.data[[col]])) +
    scale_color_gradient(low = "yellow", high = "red") +
    theme(legend.position = "bottom") +
    labs(title = paste("Map of", col)) +
    map_size
  print(us_map)
}

#filtering/QC
test2 = final %>% 
  filter(FacilityID == "01041HZNPPTHIRD") %>% select(everything())

#testing weighting functions
test = raw %>% select(FacilityID,Chemical,PoundsReleased_5yr_min,PoundsReleased_5yr_min,PoundsReleased_5yr_sum,Cancer_PoundsReleased_5yr_sum,Cancer_weighted_5yr_sum,Dev_weighted_5yr_sum,Asthma_weighted_5yr_sum)


n_distinct(final$state_sum_releases)
n_distinct(final$district_sum_releases)
