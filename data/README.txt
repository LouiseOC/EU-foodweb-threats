1) Species-specific threats data 

- tetrapod_main_threats.RData contains a dataframe indicating species taxonomy and sensitivity to threats. This is based on the raw data European_Red_List_2017_December.csv 
Sensitivity to threat is a binary information: 1 indicates that the species is known to be sensitive to the threat, else 0. 
In columns, Major threat categories names start with a capital letter; threat subcategories names start with a lowercase. 

- ThreatSpeciesMatrix.Rdata is the threats data in matrix format (species in rows, threat types in columns)

2) Trophic data 
- 2022_obligate_cooccur_BARM_Adult.RData contains the filtered version of the metaweb with only typical (obligate) interactions, on the adult lifestage of the prey, and between cooccurring species only.  
- SBMgroups_species.RData contains a table specifying the membership of each species to each trophic group. 
- species_codes_and_taxonomy is the raw data from Maiorano et al., 2020 detailing species taxonomy and correspondance with their code, used throughout the analysis. 

3) Spatial Data 
the folder RasterOUT contains tif files that represent the spatial vulnerability of food webs across Europe to different threats. 
- EU_[spp-or-links]_thr_by_[threat].tif are the raster files containing the proportion of species (spp), or interactions (links) to each threat type. 
- multiply_[threat].tif are the raster files containing the hotspots of food web vulnerability per threat. Hotspots are defined as areas with both many species and many interactions, that are vulnerable to each threat type. 
