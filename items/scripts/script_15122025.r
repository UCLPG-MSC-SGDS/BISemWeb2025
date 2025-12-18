# =====================================================================================================================================
# Title: Data Preparation for the Computer Practical Sessions for BI&Sem'25
#
# Purpose:
#		[1] Data cleaning and preparation for records from the MalariaAtlas.org
#		[2] Code chunks for the 2-day tutorials
#
# Dependencies:
# 	[1] tmap
# 	[2] sf
# 	[3] dplyr
#
# Author: Dr. Anwar Musah
# Date: 2025-12-15
# =====================================================================================================================================

# Clear environment and memory
rm(list = ls())
gc()

# PART 1:
# Set the working directory
setwd("/Users/anwarmusah/Documents/Websites/BISemWeb2025/items/datasets")

# Import and clean CIV_Anopheles_Resistance_Data.csv dataset
CIV_Anoph_data <- read.csv("CIV_Anopheles_Resistance_Data.csv")

# PART 2:
# Create the mosquito count indicators for SURV and DEATH from adjusted mortality rate column "MORTALITY_ADJUSTED"
# Replace values "Not available" with "100" under "MOSQUITO_NUMBER"
CIV_Anoph_data$MOSQUITO_NUMBER[CIV_Anoph_data$MOSQUITO_NUMBER == "Not available"] <- "100"

# Replace values "80-100" with "90" as the middlemost value under "MOSQUITO_NUMBER"
CIV_Anoph_data$MOSQUITO_NUMBER[CIV_Anoph_data$MOSQUITO_NUMBER == "80-100"] <- "90"

# Change to numeric type
CIV_Anoph_data$MOSQUITO_NUMBER <- as.numeric(CIV_Anoph_data$MOSQUITO_NUMBER)

# Calculate the death counts of mosquitoes
CIV_Anoph_data$MOSQUTIO_DEATH_NUMBER <- round(CIV_Anoph_data$MOSQUITO_NUMBER * (CIV_Anoph_data$MORTALITY_ADJUSTED/100), .1)

# Calculate the survival counts of mosquitoes
CIV_Anoph_data$MOSQUTIO_SURV_NUMBER <- CIV_Anoph_data$MOSQUITO_NUMBER - CIV_Anoph_data$MOSQUTIO_DEATH_NUMBER

# Shave the data frame down to the important columns only
names(CIV_Anoph_data)
CIV_Anoph_data <- CIV_Anoph_data[, c("LONGITUDE", "LATITUDE", "MORTALITY_ADJUSTED", "MOSQUITO_NUMBER", "MOSQUTIO_DEATH_NUMBER", "MOSQUTIO_SURV_NUMBER")]

# PART 3:
# Load the shapefiles for CIV
# Initiate libraries
library("sf")
library("tmap")
library("dplyr")

# load shapefile for ADMIN1, 2 and 3
CIV_ADMIN1 <- read_sf("gadm40_CIV_1.shp")
CIV_ADMIN3 <- read_sf("gadm40_CIV_3.shp")

# Convert survey points to point sf object
CIV_Insecticide_locations <- st_as_sf(CIV_Anoph_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Clean down these shapefiles
CIV_ADMIN1 <- CIV_ADMIN1[,c("COUNTRY", "NAME_1")]
CIV_ADMIN3 <- CIV_ADMIN3[,c("COUNTRY", "NAME_1", "NAME_3")]

# Visualize for checks on where the points fall in CIV
tm_shape(CIV_ADMIN3) + 
	tm_polygons(fill_alpha = 0) +
tm_shape(CIV_Insecticide_locations) +
	tm_dots(fill = "red", size = 0.1) +
tm_shape(CIV_ADMIN1) +
	tm_polygons(fill_alpha = 0, col = "red", lwd = 2)

# Spatial join: attach polygon attributes to each point
CIV_Insecticide_ADMIN3 <- st_join(CIV_Insecticide_locations, CIV_ADMIN3, join = st_within)
CIV_Insecticide_ADMIN3 <- st_drop_geometry(CIV_Insecticide_ADMIN3)

# Perform aggregation by "NAME_3"
CIV_Agg_Insect_ADMIN3 <- CIV_Insecticide_ADMIN3 %>% group_by(NAME_3) %>%
	summarise(
		TOTAL_MOSQ = sum(MOSQUITO_NUMBER),
		TOTAL_MOSQ_DEATH = sum(MOSQUTIO_DEATH_NUMBER, na.rm = TRUE),
		TOTAL_MOSQ_SURV = sum(MOSQUTIO_SURV_NUMBER, na.rm = TRUE),
		MORTALITY_ADJ_AVR = mean(MORTALITY_ADJUSTED, na.rm = TRUE)
	)

# Merge the non-spatial table with ADMIN3 data frame
CIV_Agg_Insect_ADMIN3 <- merge(CIV_ADMIN3, CIV_Agg_Insect_ADMIN3, by.x = "NAME_3", by.y = "NAME_3", all.x = TRUE)

# Recalculate the mortality adjusted rate
CIV_Agg_Insect_ADMIN3$MORTALITY_ADJ_AVR <- round(CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ_DEATH/CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ * 100, 2)

# replace the missing values
library("spdep")
library("INLA")
nb <- poly2nb(CIV_Agg_Insect_ADMIN3)
nb2INLA("adjacencyObject.adj", nb)
g <- inla.read.graph(filename = "adjacencyObject.adj")
TOTAL_MOSQ_DEATH <- CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ_DEATH
TOTAL_MOSQ <- CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ

# Identify missing
is_obs <- !is.na(TOTAL_MOSQ_DEATH) & !is.na(TOTAL_MOSQ)
CIV_Agg_Insect_ADMIN3$idx <- 1:nrow(CIV_Agg_Insect_ADMIN3)

# Use INLA to generate missing mortality 
RES_MORTALITY <- inla(
	TOTAL_MOSQ_DEATH ~ 1 + f(idx, model = "bym2", graph = g),
	family = "binomial",
	Ntrials = TOTAL_MOSQ,
	data = CIV_Agg_Insect_ADMIN3,
	control.predictor = list(compute = TRUE),
	control.compute = list(dic = TRUE, waic = TRUE)
)

# Use INLA to generate missing counts for totals 
RES_TOTAL <- inla(
	TOTAL_MOSQ ~ 1 + f(idx, model = "bym2", graph = g),
	family = "poisson",
	data = CIV_Agg_Insect_ADMIN3,
	control.predictor = list(compute = TRUE),
	control.compute = list(dic = TRUE, waic = TRUE)
)

CIV_Agg_Insect_ADMIN3$p_mean <- round(abs(RES_MORTALITY$summary.fitted.values$mean), 2)
CIV_Agg_Insect_ADMIN3$n_mean <- round(RES_TOTAL$summary.fitted.values$mean, .1)

set.seed(123)

idx <- CIV_Agg_Insect_ADMIN3$n_mean == 7
CIV_Agg_Insect_ADMIN3$n_mean[idx] <- CIV_Agg_Insect_ADMIN3$n_mean[idx] + sample(100:900, sum(idx), replace = TRUE)

CIV_Agg_Insect_ADMIN3$death_mean <- round(CIV_Agg_Insect_ADMIN3$n_mean * CIV_Agg_Insect_ADMIN3$p_mean, .1)
CIV_Agg_Insect_ADMIN3$surv_mean  <- round(CIV_Agg_Insect_ADMIN3$n_mean - CIV_Agg_Insect_ADMIN3$death_mean, .1)

CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ <- CIV_Agg_Insect_ADMIN3$n_mean
CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ_DEATH <- CIV_Agg_Insect_ADMIN3$death_mean
CIV_Agg_Insect_ADMIN3$TOTAL_MOSQ_SURV <- CIV_Agg_Insect_ADMIN3$surv_mean
CIV_Agg_Insect_ADMIN3$MORTALITY_ADJ_AVR <- CIV_Agg_Insect_ADMIN3$p_mean

CIV_Agg_Insect_ADMIN3 <- CIV_Agg_Insect_ADMIN3[, c(1, 4:7)]
CIV_Agg_Insect_ADMIN3 <- st_drop_geometry(CIV_Agg_Insect_ADMIN3)

# save outputs
write.csv(CIV_Agg_Insect_ADMIN3, file = "CIV_Sub_Prefecture_Mosquito_Data.csv", row.names = FALSE)
CIV_ADMIN3 <- CIV_ADMIN3[,-2]
write_sf(CIV_ADMIN3, "CIV_Sub_Prefectures.shp")
write_sf(CIV_ADMIN1, "CIV_District.shp")

# https://github.com/UCLPG-MSC-SGDS/BISemWeb2025/raw/main/items/attach_to_gitpage/BISem2025%20Lecture%20Notes%20-%20Introduction.pdf
# https://github.com/UCLPG-MSC-SGDS/BISemWeb2025/raw/main/items/attach_to_gitpage/Dataset.zip