rm(list=ls())
library(manip)

###################################
# Deflection Calculation: 
#   Date: 28 mar 2019
#   Author: FY Lin 
#   Function: Visulise the maximum Deflection of a beam in relation to the length of the beam, 
#   of which there are two supports at each edge. That is, a plot of max_Delta and L_beam. 
#   In addition, there are two equal loads applied to the beam.
###################################

# Parameters
  max_Delta_deflection <- matrix(nrow= 46,ncol=46) #The maximum deflection at the middle of the beam, unit in m.
  L_array <- seq(200,650,10)/1000 # The length of the beam, unit in m.
    length_L_array <- length(L_array)
  a_array <- {} # The distance between the support and the nearest load.
  Weight <- 82 # The weight of the load, unit in kg.
  Force <- Weight*9.8/2 # The force of each load, unit in Newton; there are overal two load. 
  Gpa <- 69 # The modulus of elasticity, unit in GPa.
  E <- Gpa*1e9 # Convert the unit from GPa to N/m2.
  I <- 5.12/(100^4) # Area moment of inertia of cross section, unit in m^4

# Formula for deflection: max_Delta_deflection = ((Force*a)/(24*E*I))*(3*(L^2)-4*(a^2))

# Plot the correlation between L and max_Delta_deflection.
# The interested range of L is between 350 mm and 650 mm. 
# Use these values of L as inputs, we are able to estimate the max_Delta_deflection
  
  i<-0
  for (L in L_array ){
    i <- i+1  
    j<-0
    a_array <-rbind(a_array,seq(0, L/2, length.out= length_L_array))
    
    for (a in a_array[i,] ){
      j <- j+1
      max_Delta_deflection[i,j] = ((Force*a)/(24*E*I))*(3*(L^2)-4*(a^2))
    }
  }
  
  deflection_mm <- round(max_Delta_deflection*1e3,2)
  L_mm <- round(L_array*1000,2)
  AllowableDeflection<- round(L_mm/240,2)

  i<-0
  max_a_mm<-{}
  for (L in L_array ){
     i<- i+1
     Mask_safe_deflection <- deflection_mm[i,] <= AllowableDeflection[i]
     max_j <- length(deflection_mm[i,Mask_safe_deflection])
     max_a_mm[i] <- round(a_array[i,max_j],2)*1000
  }
  

  
###########
###########
  Result <- data.frame(L_mm, AllowableDeflection, max_a_mm)
  
###########
###########
  plot(L_mm,max_a_mm,
       xlab = "Length of Beam (mm)", ylab = "Maximum a (mm), with which, the deflection is allowable")
  #lines(L_mm, L_mm/240,col="red")
  
  grid()
  title(main = c("The relation between L and Deflection \n",
                 paste0("Load =", Weight," Kg;"," E =", Gpa," Gpa;"," I =", I*(100^4)," cm^4")
        ))#,
        #sub="The red line indicates the allowable deflection.")
