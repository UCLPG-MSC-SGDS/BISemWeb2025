# =====================================================================================================================================
# Title: Data Preparation for the Computer Practical Sessions for BI&Sem'25
#
# Purpose:
#		[1] Data cleaning and preparation for records from the MalariaAtlas.org
#		[2] Code chunks for the 2-day tutorials
#
# Dependencies:
# 	[1] tmap
# 	[2] malariaAtlas
# 	[3] sf
#
# Author: Dr. Anwar Musah
# Date: 2025-12-15
# =====================================================================================================================================

# Load libraries
library("sf")
library("tmap")
library("malariaAtlas")

# Get the prevalence data for Cote Ivoire
CIV_malaria_pr_data <- getPR(country = "Côte d'Ivoire", ISO = "CIV" , species = "both")
autoplot(CIV_malaria_pr_data)