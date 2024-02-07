
# clean data 
library(dplyr)
library(tidyr)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#----------------- 1 Clean raw data --------------#### 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# from eea website 
raw <- read.csv("../Data/European_Red_List_2017/European_Red_List_2017_December.csv")
# keep info on taxonomy & IUCN categories (Europe & EU to compare)
tetrapods <- subset(raw, raw$taxonomicRankClass %in% c("AMPHIBIA", "AVES", "REPTILIA", "MAMMALIA"))

tetrapods$euRegionalRedListCategory <- gsub(pattern = " (PE)", replacement = "", x = tetrapods$euRegionalRedListCategory, fixed = T)
tetrapods$euRegionalRedListCategory <- gsub(pattern = "/PE", replacement = "", x = tetrapods$euRegionalRedListCategory, fixed = T)
tetrapods$euRegionalRedListCategory <- gsub("lc", "LC", tetrapods$euRegionalRedListCategory)
table(tetrapods$euRegionalRedListCategory) 

# concatenate species names into 1 column
Mamm <- subset(tetrapods, tetrapods$taxonomicRankClass == "MAMMALIA")
other <- subset(tetrapods, tetrapods$taxonomicRankClass != "MAMMALIA")

Mamm$sppname <- gsub(" ", "_", Mamm$scientificName)
other$sppname <- paste(other$taxonomicRankGenus, other$taxonomicRankSpecies, sep = "_")

tetrapods <- rbind(other, Mamm)
colnames(tetrapods)
tetrapods <- tetrapods[, c(5:8, 33, 14:15, 17:18, 23:28)]

# 1031 terrestrial species in this dataset 
# write.csv(tetrapods, file = "2021-10-27_EUredlist_w_threats_and_habitat.csv")

tetrapods.threats.bin <- tetrapods

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##                       Taxonomy              ###### 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

##  match species names in IUCN data to our species names

spp.ID <- read.csv("../Data/BioticData/Spp_feeding/Maiorano2020/species_codes_and_taxonomy.csv", sep = ";")
spp.ID$Species <- gsub(x=spp.ID$Species, pattern = " kl. ", replacement = " ")


spp.ID$Species <- gsub(x=spp.ID$Species, pattern = " ", replacement = "_")

spp.ID$Species <- gsub(x=spp.ID$Species, pattern = "_lorenzii", replacement = "") ## P. sindianus lorenzii is the subspecies of P. s. that occurs in Europe - P. s. sindianus occurs in the Himalayas. 

tetrapods.threats.bin$sppID <- spp.ID$Species.ID[match(tetrapods.threats.bin$sppname, spp.ID$Species)]

mismatches <- tetrapods.threats.bin[is.na(tetrapods.threats.bin$sppID), "sppname"]
## used these mismatches to create the "convert taxonomy" R script. 

tetrapods.threats.corrected <- convert2LuigiTaxonomy(df = tetrapods.threats.bin)
tetrapods.threats.corrected$sppID <- spp.ID$Species.ID[match(tetrapods.threats.corrected$sppname, spp.ID$Species)]
mismatches <- tetrapods.threats.corrected[is.na(tetrapods.threats.corrected$sppID), "sppname"] ## only 96 species (non native / wintering and vagrant birds / extinct / marine mammals)

tetrapods.threats <- tetrapods.threats.corrected[!is.na(tetrapods.threats.corrected$sppID), ] # remove these 96 species 

# save(tetrapods.threats, file = "2021-10-26_threats_correctednames.RData")




############################################################## .
##                  Threat data                  #############
############################################################## .

load("2021-10-26_threats_correctednames.RData")

## threats are in the form of sentences 
## build dataframe with threats

tetrapods.threats.bin <- tetrapods.threats %>%
  mutate(noMajorThreat = ifelse(grepl(pattern = "no major threat|no significant threat|no overall major threat|widespread species|adaptable species|adapts well|no known significant threat|no known threat|not believed to be significantly threatened|not thought to be any current|no serious threat", x = tetrapods.threats$threats), 
                                1, 0)) %>%
  ### residential and commercial development 
  mutate(leisure = ifelse(grepl(pattern = " ski |ski resort|skiing|alpinism|mountaineering|tourism|tourist|recreati|Recreation|leisure|golf|yacht|resort|hotel", x = tetrapods.threats$threats),  ## remove disturbance
                          1, 0)) %>% 
  mutate(construction = ifelse(grepl(pattern = "construction|infrastructure development|industrial development|commercial development", x = tetrapods.threats$threats),  ## remove disturbance
                               1, 0)) %>% 
  mutate(housing = ifelse(grepl(pattern = " housing|urban expansion|urbanization|urbanisation|urban develop", x = tetrapods.threats$threats),  ## remove disturbance
                          1, 0)) %>% 
  mutate(humanDisturbance = ifelse(grepl(pattern = "disturbance by human|human disturb|disturbed by human|disturbance by walk|disturbance by peopl", x = tetrapods.threats$threats),  ## remove disturbance
                                   1, 0)) %>% 
  ### pollution
  mutate(Pollution = ifelse(grepl(pattern = "pollut|chemical|contamin|antibiotic|sewage|waste water|water pollution|Water pollution| carbamate|run-off|organochloride|phosphate|nutrient|eutrophic|acidification|hypertrophication|toxic", x = tetrapods.threats$threats),
                            1, 0)) %>%
  ### Agriculture 
  mutate(agriculturexpansion = ifelse(grepl(pattern = "to agricultur|livestock|farming|overgrazing|increased cultivation|abandonment of traditional agr|intensive crop|agricultural intensification|intensive arable agriculture|intensive farming|agricultural development| fertilisation|agricultural activities|intensification of farm|hedgerow", x = tetrapods.threats$threats), 
                                      1, 0)) %>%
  mutate(pesticides = ifelse(grepl(pattern = "pesticid|herbicid|Pesticid|biocide|rodenticide", x = tetrapods.threats$threats), 
                             1, 0)) %>%
  ### Aquaculture & aquatic resources 
  mutate(Fishing = ifelse(grepl(pattern = "aquaculture|Bycatch|bycatch|entanglement|entangled|fishing net|fishing gear|gillnet|driftnet|fishnet|drown|longline fish|trawl fish|intensification of fish|fisheries|overfishing", x = tetrapods.threats$threats), 
                          1, 0)) %>%
  ### forestry 
  mutate(logging = ifelse(grepl(pattern = "logging| sylvi| wood harvesting| forestry| plantation", x = tetrapods.threats$threats), 
                          1, 0)) %>%
  mutate(deforestation = ifelse(grepl(pattern = "clear-cut|deforest|forest loss|loss of forest|removal of wood", x = tetrapods.threats$threats), 
                                1, 0)) %>%
  mutate(reforestation = ifelse(grepl(pattern = "reforest|afforest", x = tetrapods.threats$threats), 
                                1, 0)) %>%
  #### hunting & persecution
  mutate(hunting = ifelse(grepl(pattern = " trapping| hunting|hunted|Hunting|shoot|game|killing|poaching|consumption|harvest| fur| exploitation for food", x = tetrapods.threats$threats), 
                          1, 0)) %>%
  mutate(persecution = ifelse(grepl(pattern = "persecut|Persecut|killed when encounter|destroy egg|destroy nest|destruction of nest|destruction of roost|poison bait|poisoning", x = tetrapods.threats$threats), 
                              1, 0)) %>%
  mutate(collection = ifelse(grepl(pattern = "collect| pet|pet trade", x = tetrapods.threats$threats), 
                             1, 0)) %>%
  ### biotic factors  
  mutate(IAS = ifelse(grepl(pattern = "introduc|invasi|alien|exotic|American mink|Louisiana crayfish", x = tetrapods.threats$threats), 
                      1, 0)) %>%
  mutate(disease = ifelse(grepl(pattern = " fung|diseas|virus|viral|hytridiomyco|yxomatos|pathogen|parasites|malaria|influenza", x= tetrapods.threats$threats), 
                          1, 0))%>%
  ### energy production 
  mutate(renewables = ifelse(grepl(pattern = "tidal energy|wind turbine|wind energy|wind farm|Wind farm|hydroelectr", x = tetrapods.threats$threats), 
                             1, 0)) %>%
  mutate(mining = ifelse(grepl(pattern = "coal min|acid rain|mining|gravel extract|sand extract|peat extract|peat-extract|quarries|quarry| mineral", x = tetrapods.threats$threats), 
                         1, 0)) %>% 
  mutate(oil.gas = ifelse(grepl(pattern = " oil spill|petrol| oil pollution| gas | Oil", x = tetrapods.threats$threats), 
                          1, 0)) %>%
  ### other  
  mutate(coasts.dunes.loss= ifelse(grepl(pattern = "coast|dune", x = tetrapods.threats$threats), 
                                   1, 0)) %>%
  mutate(wetland.loss = ifelse(grepl(pattern = "drainage|wetland|freshwater|water extraction|water abstraction", x = tetrapods.threats$threats), 
                               1, 0)) %>%
  mutate(fires = ifelse(grepl(pattern = " fire| Fire| burn", x = tetrapods.threats$threats), 
                        1, 0)) %>%
  mutate(roads.rail = ifelse(grepl(pattern = "traffic accident| road|highway|collision with cars| cars |vehicles|heavy traffic| train | trains | rail", x = tetrapods.threats$threats), 
                             1, 0)) %>%
  ### climate change
  mutate(climate = ifelse(grepl(pattern = "climate|Climate|climat", x = tetrapods.threats$threats), 
                          1, 0)) %>%
  mutate(harshWinters = ifelse(grepl(pattern = "cold winter|harsh winter|Harsh winter|severe winter", x = tetrapods.threats$threats), 
                               1, 0)) %>%
  mutate(drought = ifelse(grepl(pattern = "desertification|drought|aridity|dessic|desicc|decreased spring rain|shorter rainy season|reduced winter precipit|reduced precipit|reduced rain", x = tetrapods.threats$threats), 
                          1, 0)) %>%
  mutate(extremeEvents = ifelse(grepl(pattern = "storms|extremes|temperature extreme|extreme temperature|extreme weather|extreme climat|Extreme weather|Extreme climat| Floods | floods ", x = tetrapods.threats$threats), 
                                1, 0)) %>%
  mutate(warming = ifelse(grepl(pattern = " hot| Hot| warm| Warm| heat| Heat|temperature rise|increasing temperature|increased spring temperature|increased summer tempera", x = tetrapods.threats$threats), 
                          1, 0)) 



### Finalise threats data ### 
# # create threats x species matrix with sppID
# threats.spp <- tetrapods.threats.bin[, c(16, 18:45)]
# rownames(threats.spp) <- threats.spp$sppID
# threats.spp$sppID <- NULL

#### Create Major threats categories based on subcategories ### 
tetrapods.threats.bin <- tetrapods.threats.bin%>%
  mutate(Climate.Change = ifelse(climate == 1 | drought == 1 |harshWinters == 1|extremeEvents == 1|warming==1, 
                                 1, 0))%>%
  mutate(Forestry = ifelse(logging == 1 | deforestation == 1 | reforestation == 1, 
                           1, 0)) %>%
  mutate(Direct.Exploitation = ifelse(persecution == 1 | hunting==1 | collection == 1, 
                                      1, 0))%>%
  mutate(IAS.Diseases = ifelse(IAS == 1 | disease ==1, 
                               1, 0))%>%
  mutate(Urbanisation = ifelse(construction == 1 |housing  == 1 | leisure  == 1 | humanDisturbance == 1, 
                               1, 0))%>%
  mutate(Agriculture = ifelse(agriculturexpansion == 1 |pesticides == 1, 
                              1, 0))%>%
  mutate(Mining.Energy.Production = ifelse(oil.gas == 1|mining==1|renewables ==1, 
                                           1, 0))

save(tetrapods.threats.bin, file = "data/analysisInput/tetrapod_main_threats.RData") 


## How many species threatened by each threat type ? 
colSums(tetrapods.threats.bin[, -c(1:16)])
