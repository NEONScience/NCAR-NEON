##############################################################################################
#' @title Workflow to generate surface date for NCAR-NEON project

#' @author
#' Samantha Weintraub \email{sweintraub@battelleecology.org}

#' @description 
#' Code for collating NEON soil data from API plus adding on tower land cover

# changelog and author contributions / copyrights
# Samantha Weintraub (2021-02-01)
#   original creation
# David Durden (2021-02-02)
#   Adding variable description, uploading output to S3 option
##############################################################################################

# Reset workspace
rm(list = ls())

#############################################################
#Dependencies
#############################################################

#Call the R HDF5 Library
packReq <- c('tidyverse','neonUtilities','aws.s3')# for tidyverse: joining and wrangling data; neonUtilities: NEON data download via the API

#Install and load all required packages
lapply(packReq, function(x) {
  print(x)
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})


# # Load packages - Git 
# library(devtools)
# # geoNEON - for getting spatial data about sampling sites. uncomment and run line below if need this package
# # install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=T) 
# library(geoNEON)

###############################################################################
#Output to S3 ENV variable definitions
###############################################################################
MethOut <- c("local", "s3")[2]

if(MethOut == "s3"){
#Set ENV variables
base::Sys.setenv("S3PATHUPLD" = "NEON/surf_files")
base::Sys.setenv("NEON_S3_ACCESS_KEY_ID" = "neon-ncar-writer")
#base::Sys.setenv("NEON_S3_SECRET_KEY" = "Access-key-needed")
base::Sys.setenv("NEON_S3_ENDPOINT" = "s3.data.neonscience.org")
base::Sys.setenv("NEON_S3_OUTPUT_BUCKET" = "neon-ncar")

#Grab needed ENV variable
S3PathUpld <- base::Sys.getenv("S3PATHUPLD")
}


#############################################################
#Workflow parameters and helper files
#############################################################


# Set directory - add for new users as needed
# user-customizeable access to input and output data directories
DirUsr <- c(
  dd = "~/eddy/data/CLM/surf",
  sw = "/Users/sweintraub/Documents/GitHub/NCAR-NEON/surfaceData",
  dock = tempdir()
)["dd"]

#Create input output directories
  inputs <- DirUsr
  outputs <- paste0(inputs, "/siteFiles")
  
#if folder doesn't exist create it
if(!dir.exists(outputs)) dir.create(outputs, recursive = TRUE)


# Add info about megapit land cover (from TIS) and dominant plants in the tower (pheno)
cover <- read.csv("https://s3.data.neonscience.org/neon-ncar/NEON/surf_files/inpMeta/tower.site.metadata.csv", 
                  header = T, stringsAsFactors = F)


##############################################################################
#Megapit soil data download (DP1.00096.001)
##############################################################################
mgp <- neonUtilities::loadByProduct(
  site = "all",
  dpID = "DP1.00096.001",
  package = "basic",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
) # can remove 'token' parameter if you don't have one

#Bring list components into global environment
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
      "publicationDate"))
#,"release")) #Remove release until we update neonUtilities

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
    "publicationDate"))
  #,"release"))#Remove release until we update neonUtilities

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
  write.csv(mgp_all.list[[i]], file = paste0(outputs,"/", names(mgp_all.list[i]), "_surfaceData.csv"), row.names = FALSE)
}


##############################################################################
#Prepare and write variable description metadata
##############################################################################


#List of variables output
varList <- c("domainID", "siteID", "decimalLatitude","decimalLongitude","elevation","ecosystemType_Megapit","ecoregionWWF_Megapit","landCover_Megapit", "dominantPlants_Tower","horizonName", "biogeoTopDepth","biogeoBottomDepth", "carbonTot","nitrogenTot","phH2o", "coarseFrag2To5","coarseFrag5To20","coarseFrac2to20", "sandTotal", "siltTotal", "clayTotal", "bulkDensTopDepth", "bulkDensBottomDepth", "bulkDensExclCoarseFrag")

#Remove duplicate variable descriptions
varSub <- variables_00096[!duplicated(variables_00096$fieldName),]

#Filter and subset variable description metadata
varMeta <- varSub %>% filter(fieldName %in% varList) %>% select(fieldName, description, dataType, units)
#Metadata for additional data streams ##TODO:: move to internal data later
varMetaAdd <- data.frame(fieldName = c("ecosystemType_Megapit","ecoregionWWF_Megapit","landCover_Megapit", "dominantPlants_Tower", "coarseFrac2to20"), description = c("The predominant ecosystem type found at the site","The ecoregion at the site","The landcover type found at the site","The most dominant plant species found at the site", "Coarse fragment (2-20 mm) content of the <20 mm size fraction of the biogeochemistry soil sample"), dataType = c("string","string","string","string","real"), units = c(NA,NA,NA,NA,"	gramsPerKilogram"))
#Combine metadata to final dataframe
varMetaSurf <- rbind(varMeta,varMetaAdd)

#Sort dataframe alphabetically
varMetaSurf <- varMetaSurf[order(varMetaSurf$fieldName),]

#Output filename for variable metadata
fileOutMeta <- "varMetaSurf.csv"
# Also write the 'variables' file, for units
write.csv(varMetaSurf, file = paste0(inputs, fileOutMeta), row.names = FALSE)

###############################################################################
#Output to S3
###############################################################################
#Should data be written out to S3
if(MethOut == "s3"){

#Upload PDF to ECS
accs::upload.to.ecs(s3Path = S3PathUpld, localPath = inputs, s3filename = fileOutMeta, filename = fileOutMeta)

#Grab all output files names
fileOut <- base::list.files(outputs)  
 
#Upload to S3
lapply(fileOut, function(x){
  print(x)
  #Function to upload to ECS
  accs::upload.to.ecs(s3Path = S3PathUpld, localPath = outputs, s3filename = x, filename = x) 
})#End lapply for writing data out to S3

} #End if statement to write to S3
