##########################################
# Generates specieslist                  #
# Compatible with new LIST structure     #
# Modified to work w/ SAV component      # 
#                                        #
# Created by: MJM - March 21, 2014       #
# Updated: 07/08/2014                    # 
##########################################

SPECIES29 <- function(){  
  
  # assign each parameter for each species from the global workspace to an object  
  maxrgr <- c(maxSAVrgr,maxrgr01,maxrgr02,maxrgr03,maxrgr04)
  initial_cells <- c(initialSAVcells,initial01cells,initial02cells,initial03cells,initial04cells)
  initial_total_biomass <- c(initialSAVtotmass,initial01totmass,initial02totmass,initial03totmass,initial04totmass)
  overwinter <- c(overwinterSAV,overwinter01,overwinter02,overwinter03,overwinter04) 
  # halfsatB <- c(halfsatBSAV,halfsatB01,halfsatB02,halfsatB03,halfsatB04)
  halfsatN <- c(halfsatNSAV,halfsatN01,halfsatN02,halfsatN03,halfsatN04)
  # halfsatP <- c(halfsatPSAV,halfsatP01,halfsatP02,halfsatP03,halfsatP04)
  uptakeN <- c(uptakeNSAV ,uptakeN01,uptakeN02,uptakeN03,uptakeN04)
  uptakeP <- c(uptakePSAV ,uptakeP01,uptakeP02,uptakeP03,uptakeP04)
  lightlimitation <- c(lightlimitationSAV ,lightlimitation01,lightlimitation02,lightlimitation03,lightlimitation04)
  
  # remove any extra values for FP species that are not included 
  maxrgr <- maxrgr[1:(numbFPspecies+1)]
  initial_cells <- initial_cells[1:(numbFPspecies+1)]
  initial_total_biomass <- initial_total_biomass[1:(numbFPspecies+1)]
  overwinter <- overwinter[1:(numbFPspecies+1)]
  # halfsatB <- halfsatB[1:(numbFPspecies+1)]
  halfsatN <- halfsatN[1:(numbFPspecies+1)]
  # halfsatP <- halfsatP[1:(numbFPspecies+1)]
  uptakeN <- uptakeN[1:(numbFPspecies+1)]
  uptakeP <- uptakeP[1:(numbFPspecies+1)]
  lightlimitation <- lightlimitation[1:(numbFPspecies+1)]
  
  # combine vectors into a list 
  mylist <- list(maxrgr, initial_cells, initial_total_biomass,
                 overwinter, halfsatN, uptakeN, 
                 uptakeP, lightlimitation)
  # mylist <- list(maxrgr, initial_cells, initial_total_biomass, overwinter, halfsatB, halfsatN, halfsatP, uptakeN, uptakeP, lightlimitation)
  
  # give each vector a name 
  names(mylist) <- c("maxrgr", "initial_cells","initial_total_biomass",
                     "overwinter","halfsatN","uptakeN",
                     "uptakeP","lightlimitation")
  #names(mylist) <- c("maxrgr", "initial_cells","initial_total_biomass", "overwinter","halfsatB","halfsatN","halfsatP","uptakeN","uptakeP","lightlimitation")
  
  return(mylist) 
}