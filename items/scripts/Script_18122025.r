rm(list = ls())
gc()

setwd("/Users/anwarmusah/Desktop/BISemWeb2025/Day 1")

CIV_Agg_Insect_ADMIN3 <- read.csv(file = "CIV_Sub_Prefecture_Mosquito_Data.csv", header = TRUE, sep = ",")
View(CIV_Agg_Insect_ADMIN3)

CIV_ADMIN1 <- read_sf("CIV_District.shp")
CIV_ADMIN3 <- read_sf("CIV_Sub_Prefectures.shp")

# Merge the non-spatial table with ADMIN3 data frame
CIV_Spatial_Data <- merge(CIV_ADMIN3, CIV_Agg_Insect_ADMIN3, by.x = "NAME_3", by.y = "NAME_3", all.x = TRUE)
# View the datasets
View(CIV_Spatial_Data)

# Outline only
tm_shape(CIV_Spatial_Data) + tm_polygons()

# Outline with transpancy setting
tm_shape(CIV_Spatial_Data) + tm_polygons(fill_alpha = 0.1, col_alpha = 0.4)

# adding a layer
tm_shape(CIV_Spatial_Data) + 
	tm_polygons(fill_alpha = 0.1, col_alpha = 0.4) +
tm_shape(CIV_ADMIN1) +
	tm_polygons(fill_alpha = 0, col_alpha = 1, col = "black", lwd = 2)

# full visualisation
tm_shape(CIV_Spatial_Data) + 
	tm_polygons(
		fill = "MORTALITY_ADJ_AVR",
		fill.scale = tm_scale_continuous(values = "brewer.rd_yl_bu"),
		fill.legend = tm_legend(title = "Mortality of Mosquitoes after inserticide [%]", frame = FALSE, position = tm_pos_out()),
		fill_alpha = 1, col_alpha = 0.5, col = "black", lwd = 0.5) +
tm_shape(CIV_ADMIN1) + tm_text("NAME_1") +
	tm_polygons(fill_alpha = 0, col_alpha = 1, col = "black", lwd = 2) +
tm_compass(type = "arrow", position = c("right", "top")) +
tm_scalebar(position = c("right", "bottom"))

tmap_mode("view")

cols4all::c4a_palettes()


