WARNING <- function(x){
  ##################################
  # Print a summary of each column #
  ##################################
  print("Summary of all columns(variables):")
  print(summary(parameters))
  
  ###################
  # Columns with NA #
  ###################
  print("Columns with missing values (NA):")
  missing <- which(sapply(parameters, function(x) any(is.na(x)))=="TRUE")
  print(missing)  
    
  ####################################################
  # number of cells in each column that are negative #
  ####################################################
  # Handles things differently if they are numeric or integers 
  # apply(parameters, 2, function(x) length(x[x<0]))
  apply(parameters, 2, function(x) any(x<0))
  
  which(apply(parameters, 2, function(x) any(x<0))=="TRUE")
  # sapply(parameters, function(x) x<0,simplify=TRUE)
  
  ########################
  # Check variable names #
  ########################
  good_names <- c("simulation","height1","width1","height2","width2",
                  "shape","area","size","HtoW","numbFPspecies","days","years",
                  "TOTALN","TOTALP","N_to_P","initial_perc_SAV_cover",
                  "initial_perc_SAV_cells_occupied","SAV_biomass_per_cell",
                  "initial_perc_FP_cover","initial_perc_FP_cells_occupied",
                  "FP_biomass_per_cell","initialSAVcells","initial01cells",
                  "initial02cells","initial03cells","initial04cells",
                  "initialSAVtotmass","initial01totmass","initial02totmass",
                  "initial03totmass","initial04totmass","neigh_thresh_SAV",
                  "focal_thresh_SAV","amnt_colonize_SAV","neigh_thresh_FP",
                  "focal_thresh_FP","amnt_colonize_FP","maxSAVrgr","maxrgr01",
                  "maxrgr02","maxrgr03","maxrgr04","lightlimitationSAV",
                  "lightlimitation01","lightlimitation02","lightlimitation03",
                  "lightlimitation04","halfsatNSAV","halfsatN01","halfsatN02",
                  "halfsatN03","halfsatN04","shadingbyFP","lightattenuation", 
                  "lossSAV","lossFP","uptakeNSAV","uptakeN01","uptakeN02",
                  "uptakeN03","uptakeN04","uptakePSAV","uptakeP01","uptakeP02",
                  "uptakeP03","uptakeP04","overwinterSAV","overwinter01",
                  "overwinter02","overwinter03","overwinter04","wind_shape1",
                  "wind_shape2","wind_avg","wind_stdev","prob_down","prob_up",
                  "prob_left","prob_right","wind_direction","full_thresh_wind")
  
  print("Are all column (variable) names correct?")
  
  if(all(unique(good_names)==unique(colnames(parameters)))) cat("Yes\n")
  
  else cat("No\n")
  
  ###################################
  # check if wind probabilities = 1 #
  ###################################
  # row by row 
  
  ################################
  # check if shapes is allowable #
  ################################
  # row by row 
  
  ####################################
  # check if initial # cells <= area #
  ####################################
  # row by row 
  
}



# tells me if there are any NAs in each column 
# sapply(parameters, function(x)any(is.na(x)),simplify=TRUE)

# tells me which coluns have an NA 
# missing <- which(sapply(parameters, function(x)any(is.na(x)),simplify=TRUE)=="TRUE",arr.ind=TRUE)
# print(missing)