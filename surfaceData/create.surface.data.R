##############################################################################################
#' @title Workflow to generate surface date for NCAR-NEON project

#' @author
#' Samantha Weintraub \email{sweintraub@battelleecology.org}

#' @description 
#' Code for collating NEON soil data from API plus adding on tower land cover

# changelog and author contributions / copyrights
# Samantha Weintraub (2021-02-01)
#   original creation
##############################################################################################

# Reset workspace
rm(list = ls())

#############################################################
#Dependencies
#############################################################

# Load packages
library(tidyverse) # joining and wrangling functions
library(neonUtilities) # for NEON data download via the API

# # Load packages - Git 
# library(devtools)
# # geoNEON - for getting spatial data about sampling sites. uncomment and run line below if need this package
# # install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=T) 
# library(geoNEON)


#############################################################
#Workflow parameters and helper files
#############################################################

# Set directory - add for new users as needed
if (file.exists('/Users/sweintraub/')) {
  inputs <- "/Users/sweintraub/Documents/GitHub/NCAR-NEON/surfaceData/"
  outputs <- paste0(inputs, "siteFiles/")
}

# Add info about megapit land cover (from TIS) and dominant plants in the tower (pheno)
cover <- read.csv(paste0(inputs, "tower.site.metadata.csv"), 
                         header = T, stringsAsFactors = F)


##############################################################################
#Megapit soil data download (DP1.00096.001)
##############################################################################
mgp <- loadByProduct(
  site = "all",
  dpID = "DP1.00096.001",
  package = "basic",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
) # can remove 'token' parameter if you don't have one

list2env(mgp,envir=.GlobalEnv)


##############################################################################
#Combine data across tables plus clean up
##############################################################################

# Remove empty rows and audit samples - biogeo table
mgp_perbiogeo.1 <- mgp_perbiogeosample %>%
  filter(!biogeoID == "",!biogeoSampleType == "Audit") %>%
  mutate(coarseFrac2to20 = (coarseFrag2To5 + coarseFrag5To20)*.1) # fraction of coarse frags 2-20
  
# Remove empty rows and audit samples - bulk dens table
mgp_perbulk.1 <- mgp_perbulksample %>%
  filter(!bulkDensID == "",!bulkDensSampleType == "Audit")

# Combine the two
intersect(colnames(mgp_perbiogeo.1), colnames(mgp_perbulk.1))
mgp_all <- mgp_perbiogeo.1 %>%
  left_join(
    mgp_perbulk.1,
    by = c(
      "domainID",
      "siteID",
      "pitNamedLocation",
      "pitID",
      "horizonID",
      "horizonName",
      "setDate",
      "collectDate",
      "laboratoryName",
      "labProjID", 
      "publicationDate", 
      "release"))

# Add pit-level metadata
intersect(colnames(mgp_all), colnames(mgp_permegapit))
mgp_all.1 <- mgp_all %>%
  left_join(
  y = mgp_permegapit,
  by = c(
    "domainID",
    "siteID",
    "pitNamedLocation",
    "pitID",
    "setDate",
    "collectDate",
    "publicationDate",
    "release"))

# Add cover and dominant plants, arrange by pit and horizon, keep only vars needed by CLM
intersect(colnames(mgp_all.1), colnames(cover))
mgp_all.2 <- mgp_all.1 %>%
  left_join(cover, by = "siteID") %>%
  arrange(pitID, biogeoTopDepth) %>%
  select(domainID, 
         siteID, 
         decimalLatitude,
         decimalLongitude, 
         elevation, 
         ecosystemType_Megapit,
         ecoregionWWF_Megapit,
         landCover_Megapit, 
         dominantPlants_Tower,
         horizonName, 
         biogeoTopDepth, 
         biogeoBottomDepth, 
         carbonTot, 
         nitrogenTot, 
         phH2o, 
         coarseFrag2To5, 
         coarseFrag5To20,
         coarseFrac2to20, 
         sandTotal, 
         siltTotal, 
         clayTotal, 
         bulkDensTopDepth, 
         bulkDensBottomDepth, 
         bulkDensExclCoarseFrag)

##############################################################################
#Prepare and export single-site files
##############################################################################

# Split into list, one DF per site
mgp_all.list <- split(mgp_all.2, mgp_all.2$siteID)

# Write them to the GitHub folder
for (i in 1:length(mgp_all.list)) {
  write.csv(mgp_all.list[[i]], file = paste0(outputs, names(mgp_all.list[i]), "_surfaceData.csv"), row.names = FALSE)
}

# Also write the 'variables' file, for units
write.csv(variables_00096, file = paste0(inputs, "variables.megapit.00096.csv"), row.names = FALSE)




