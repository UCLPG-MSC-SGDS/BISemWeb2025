rm(list = ls())
gc()

setwd("~/Desktop/BISemWeb2025/Day 2")

library("sf")
library("tmap")
library("sp")
library("spdep")
library("dplyr")
library("malariaAtlas")

# get malaria records
# CIV_pr_data <- getPR(ISO = "CIV", species = "both", version = "202206")
# CIV_pr_data <- CIV_pr_data[, c("longitude", "latitude", "examined", "positive")]
# CIV_pr_data$type <- "MAL"
# write.csv(CIV_pr_data, file = "Malaria_CIV.csv", row.names = FALSE)

# load data
CIV_Spatial_Data <- read_sf("CIV_WHO_Insecticide_Spatial_Data.shp")
# keep NAME_3 only
CIV_ADMIN3 <- CIV_Spatial_Data[,c("NAME_3")]
CIV_ADMIN1 <- read_sf("CIV_District.shp")
CIV_ADMIN4 <- read_sf("gadm40_CIV_4.shp")
CIV_ADMIN4 <- CIV_ADMIN4[,c(3,7)]

# load all NTD point locations with reported cases
CIV_NTD_sites <- read.csv("NTD_data.csv")

# Convert survey points to point sf object
CIV_NTD_sites_sf <- st_as_sf(CIV_NTD_sites, coords = c("longitude", "latitude"), crs = 4326)

# spatial configuration of points
tm_shape(CIV_ADMIN4) + 
	tm_polygons(fill = "white") +
	tm_shape(CIV_NTD_sites_sf) +
	tm_dots("black")

# Spatial join: attach polygon attributes to each point
CIV_NTDS_ADMIN4 <- st_join(CIV_NTD_sites_sf, CIV_ADMIN4, join = st_within)
CIV_NTDS_ADMIN4 <- st_drop_geometry(CIV_NTDS_ADMIN4)

# Perform aggregation by "NAME_3"
CIV_Agg_NTD_ADMIN4 <- CIV_NTDS_ADMIN4 %>% group_by(NAME_4) %>%
	summarise(
		examined = sum(examined),
		positive = sum(positive),
	)

# Merge the non-spatial table with ADMIN3 data frame
CIV_Agg_NTD_ADMIN4  <- merge(CIV_ADMIN4, CIV_Agg_NTD_ADMIN4 , by.x = "NAME_4", by.y = "NAME_4", all.x = TRUE)

CIV_Agg_NTD_ADMIN4$NTD_prev <- (CIV_Agg_NTD_ADMIN4$positive/CIV_Agg_NTD_ADMIN4$examined)
CIV_Agg_NTD_ADMIN4$examined <- round(CIV_Agg_NTD_ADMIN4$examined, .1)
CIV_Agg_NTD_ADMIN4$positive <- round(CIV_Agg_NTD_ADMIN4$positive, .1)
CIV_Agg_NTD_ADMIN4$NTD_prev <- round(CIV_Agg_NTD_ADMIN4$NTD_prev, 2)
CIV_Agg_NTD_ADMIN4$NTD_prev[CIV_Agg_NTD_ADMIN4$examined == 0 & CIV_Agg_NTD_ADMIN4$positive == 0] <- 0.00

CIV_Agg_NTD_ADMIN4$NTD_status <- NA
CIV_Agg_NTD_ADMIN4$NTD_status[CIV_Agg_NTD_ADMIN4$NTD_prev == 0.00] <- 1
CIV_Agg_NTD_ADMIN4$NTD_status[CIV_Agg_NTD_ADMIN4$NTD_prev >= 0.01 & CIV_Agg_NTD_ADMIN4$NTD_prev < 0.10] <- 2
CIV_Agg_NTD_ADMIN4$NTD_status[CIV_Agg_NTD_ADMIN4$NTD_prev >= 0.10 & CIV_Agg_NTD_ADMIN4$NTD_prev < 0.30] <- 3
CIV_Agg_NTD_ADMIN4$NTD_status[CIV_Agg_NTD_ADMIN4$NTD_prev >= 0.30 & CIV_Agg_NTD_ADMIN4$NTD_prev < 0.50] <- 4
CIV_Agg_NTD_ADMIN4$NTD_status[CIV_Agg_NTD_ADMIN4$NTD_prev >= 0.50 & CIV_Agg_NTD_ADMIN4$NTD_prev < 1.00] <- 5

# re-roll...
CIV_Agg_NTD_ADMIN4 <- read.csv("CIV_NTD_ADMIN4_Data.csv")
CIV_Agg_NTD_ADMIN4  <- merge(CIV_ADMIN4, CIV_Agg_NTD_ADMIN4 , by.x = "NAME_4", by.y = "NAME_4", all.x = TRUE)
colnames(CIV_Agg_NTD_ADMIN4)[2] <- "NAME_1"
CIV_Agg_NTD_ADMIN4 <- CIV_Agg_NTD_ADMIN4[,-3]

write_sf(CIV_Agg_NTD_ADMIN4, "CIV_NTD_Community_ADMIN4.shp")

# Magic starts here
rm(list = ls())
gc()

CIV_ADMIN1 <- read_sf("CIV_District.shp")
CIV_Agg_NTD_ADMIN4 <- read_sf("CIV_NTD_Community_ADMIN4.shp")

tm_shape(CIV_Agg_NTD_ADMIN4) + 
	tm_polygons(
		fill = "NTD_status",
		fill.scale = tm_scale_categorical(values = c("white", "#74add1", "#ffffbf", "#fee08b", "#f46d43"), 
			labels = c("Negligible (<1%)", "Low (1-10%)", "Medium (10-30%)", "High (30-50%)", "Extreme (50%+)")),
		fill.legend = tm_legend(title = "NTD Infection Status", frame = FALSE, position = tm_pos_out()),
		fill_alpha = 1, col_alpha = 0.5, col = "black", lwd = 0.5) +
	tm_shape(CIV_ADMIN1) + tm_text("NAME_1") +
	tm_polygons(fill_alpha = 0, col_alpha = 1, col = "black", lwd = 2) +
	tm_compass(type = "arrow", position = c("right", "top")) +
	tm_scalebar(position = c("right", "bottom"))

# create unique ID
#CIV_Agg_NTD_ADMIN4$PrefID <- 1:nrow(CIV_Agg_NTD_ADMIN4)
#CIV_Agg_NTD_ADMIN4_df <- st_drop_geometry(CIV_Agg_NTD_ADMIN4)
#write.csv(CIV_Agg_NTD_ADMIN4_df, "NTD_ADMIN4_Dataset.csv", row.names = FALSE)

# see spatial configuration of areas
tm_shape(CIV_Agg_NTD_ADMIN4) + 
	tm_polygons(fill = "white") + tm_text("PrefID", size = 0.7)

# create adjacency matrix
adjacency_matrix_community <- poly2nb(CIV_Agg_NTD_ADMIN4)
# sanity checks - ensure all is in alignment
names(adjacency_matrix_community) <- CIV_Agg_NTD_ADMIN4$NAME_4
# check first 10 neighbourhoods, and cross-check with map to examine it the neighbours are right!
head(adjacency_matrix_community, n = 10)


# create the list weights object
community_weights_list <- nb2listw(adjacency_matrix_community, style='W')

# global Moran's I test
moran.test(CIV_Agg_NTD_ADMIN4$positive, community_weights_list)
moran.mc(CIV_Agg_NTD_ADMIN4$positive, community_weights_list, nsim=1000)

# local Moran's I test
local_moran_CIV_NTD <- localmoran(CIV_Agg_NTD_ADMIN4$positive, community_weights_list)
# re-scale
CIV_Agg_NTD_ADMIN4$scale_positive <- scale(CIV_Agg_NTD_ADMIN4$positive)
# create a spatial lag variable 
CIV_Agg_NTD_ADMIN4$scale_lag_positive <- lag.listw(community_weights_list, CIV_Agg_NTD_ADMIN4$scale_positive)

# create indicator for cluster map
CIV_Agg_NTD_ADMIN4$ClusterID <- NA
CIV_Agg_NTD_ADMIN4$ClusterID[CIV_Agg_NTD_ADMIN4$scale_positive > 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive > 0] <- 1
CIV_Agg_NTD_ADMIN4$ClusterID[CIV_Agg_NTD_ADMIN4$scale_positive > 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive < 0] <- 2
CIV_Agg_NTD_ADMIN4$ClusterID[CIV_Agg_NTD_ADMIN4$scale_positive < 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive > 0] <- 3
CIV_Agg_NTD_ADMIN4$ClusterID[CIV_Agg_NTD_ADMIN4$scale_positive < 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive < 0] <- 4

# previously we created the output - full visualisation
tm_shape(CIV_Agg_NTD_ADMIN4) + 
	tm_polygons(
		fill = "ClusterID",
		fill.scale = tm_scale_categorical(values = c("#f46d43", "#fee0d2", "#deebf7", "#3182bd"), 
			labels = c("HH", "HL", "LH", "LL")),
		fill.legend = tm_legend(title = "NTD Cluster Map", frame = FALSE, position = tm_pos_out()),
		fill_alpha = 1, col_alpha = 0.5, col = "black", lwd = 0.5) +
	tm_shape(CIV_ADMIN1) + tm_text("NAME_1") +
	tm_polygons(fill_alpha = 0, col_alpha = 1, col = "black", lwd = 2) +
	tm_compass(type = "arrow", position = c("right", "top")) +
	tm_scalebar(position = c("right", "bottom"))

# create indicator for cluster map with statistical significance
CIV_Agg_NTD_ADMIN4$Sig_ID <- NA
CIV_Agg_NTD_ADMIN4$Sig_ID[CIV_Agg_NTD_ADMIN4$scale_positive > 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive > 0 & local_moran_CIV_NTD[,5] < 0.05] <- 1
CIV_Agg_NTD_ADMIN4$Sig_ID[CIV_Agg_NTD_ADMIN4$scale_positive > 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive < 0 & local_moran_CIV_NTD[,5] < 0.05] <- 2
CIV_Agg_NTD_ADMIN4$Sig_ID[CIV_Agg_NTD_ADMIN4$scale_positive < 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive > 0 & local_moran_CIV_NTD[,5] < 0.05] <- 3
CIV_Agg_NTD_ADMIN4$Sig_ID[CIV_Agg_NTD_ADMIN4$scale_positive < 0 & CIV_Agg_NTD_ADMIN4$scale_lag_positive < 0 & local_moran_CIV_NTD[,5] < 0.05] <- 4
CIV_Agg_NTD_ADMIN4$Sig_ID[local_moran_CIV_NTD[,5] >= 0.05] <- 5

table(CIV_Agg_NTD_ADMIN4$Sig_ID)

tm_shape(CIV_Agg_NTD_ADMIN4) + 
	tm_polygons(
		fill = "Sig_ID",
		fill.scale = tm_scale_categorical(values = c("#f46d43", "#deebf7", "white"), 
			labels = c("HH", "LH", "Not Significant")),
		fill.legend = tm_legend(title = "NTD Cluster Map: Significance", frame = FALSE, position = tm_pos_out()),
		fill_alpha = 1, col_alpha = 0.5, col = "black", lwd = 0.5) +
	tm_shape(CIV_ADMIN1) + tm_text("NAME_1") +
	tm_polygons(fill_alpha = 0, col_alpha = 1, col = "black", lwd = 2) +
	tm_compass(type = "arrow", position = c("right", "top")) +
	tm_scalebar(position = c("right", "bottom"))