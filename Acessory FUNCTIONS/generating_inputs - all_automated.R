#############
# Waterbody #
#############
height <- 20
width <- 20 

# calculated # 
area <- height*width 
if(height==width) {shape <- "square"} 
else {shape <- "rect"}
HtoW <- height/width 

##############
# Simulating #
##############
days <- 150
years <- 8
numbFPspecies <- 1

######################
# Initial conditions #
######################
initial_perc_SAV_cover <- c(1,20,50,80,99) 
initial_perc_SAV_cells_occupied <- c(1,20,50,80,99)
# this will do all combinations of the values above 
# I want to do select combinations 
# I may have to put the selection combinations into a list 

initial_perc_FP_cover <- c(1,50,99) 
initial_perc_FP_cells_occupied <- c(1,50,99)

# calculated # 
initialSAVcells <- (initial_perc_SAV_cells_occupied/100)*area
initial01cells <- ((initial_perc_FP_cells_occupied/100)*area) / numbFPspecies
initial02cells <- ((initial_perc_FP_cells_occupied/100)*area) / numbFPspecies
initial03cells <- ((initial_perc_FP_cells_occupied/100)*area) / numbFPspecies
initial04cells <- ((initial_perc_FP_cells_occupied/100)*area) / numbFPspecies

initialSAVtotmass <- area * initial_perc_SAV_cover
initial01totmass <- (area * initial_perc_FP_cover) / numbFPspecies
initial02totmass <- (area * initial_perc_FP_cover) / numbFPspecies
initial03totmass <- (area * initial_perc_FP_cover) / numbFPspecies
initial04totmass <- (area * initial_perc_FP_cover) / numbFPspecies

SAV_biomass_per_cell <- initialSAVtotmass / initialSAVcells
FP_biomass_per_cell <- (initial01totmass / initial01cells) * numbFPspecies

#############
# Nutrients #
#############
TOTALN <- c(0.1,0.5,seq(1,10,1))
TOTALP <- TOTALN/4

# calculated # 
N_to_P <- TOTALN/TOTALP

############
# Movement #
############
neigh_thresh_SAV <- 100 
focal_thresh_SAV <- 1
amnt_colonize_SAV <- 1

neigh_thresh_FP <- 100
focal_thresh_FP <- 1
amnt_colonize_FP <- 1

##########
# Growth #
##########
lossSAV <- 0.05 
lossFP <- 0.05

maxSAVrgr <- 0.5 
maxrgr01 <- 0.5 
maxrgr02 <- 0.5 
maxrgr03 <- 0.5 
maxrgr04 <- 0.5 

halfsatNSAV <- 0
halfsatN01 <- 0.2
halfsatN02 <- 0.2
halfsatN03 <- 0.2
halfsatN04 <- 0.2

lightlimitationSAV <- c(0.1,0.2)
lightlimitation01 <- c(0.1,0.2)
lightlimitation02 <- c(0.1,0.2)
lightlimitation03 <- c(0.1,0.2)
lightlimitation04 <- c(0.1,0.2)
# this will do all combinations of the values above 
# I want to do select combinations 
# I may have to put the selection combinations into a list 

shadingbyFP <- c(0.2,0.4,0.8)
lightattenuation <- 0

########## 
# Uptake #
##########
uptakeNSAV <- 0.075
uptakeN01 <- 0.005
uptakeN02 <- 0.005
uptakeN03 <- 0.005
uptakeN04 <- 0.005

uptakePSAV <- uptakeNSAV/4
uptakeP01 <- uptakeN01/4
uptakeP02 <- uptakeN02/4
uptakeP03 <- uptakeN03/4
uptakeP04 <- uptakeN04/4
 
########
# Wind #
######## 
wind_shape1 <- 0.1
wind_shape2 <- 0.4

prob_up <- 0.25
prob_down <- 0.25
prob_left <- 0.25
prob_right <- 0.25

wind_direction
full_thresh_wind

# calculated 
wind_avg <- wind_shape1/(wind_shape1+wind_shape2)
wind_stdev <- sqrt((wind_shape1*wind_shape2) / 
                     ((wind_shape1+wind_shape2)^2 *
                        (wind_shape1+wind_shape2+1)))

#################
# Overwintering #  
#################
overwinterSAV
overwinter01
overwinter02
overwinter03
overwinter04
