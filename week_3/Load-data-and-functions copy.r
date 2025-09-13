############################
# Clean up the environment #
############################
rm(list=ls(all=TRUE))
#########################################
# load functions			#
#########################################

Delay1 <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/Delay1.txt")  
ddelay <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/ddelay.txt") 
matrixfill <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/matrixfill.txt") 
psum <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/psum.txt") 
shift <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/shift.txt") 
strip <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/strip.txt") 
shiftvec <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/shiftvec.txt") 

inputdata <- dget("/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Functions/datainput.txt")
#########################################
# load input Data	
#########################################	

# data tables
   #############

# default data tables
 dataFiles <- matrix(c("Data file", "/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/
                       Nowcast_case_files/March_10_2019.txt"),1, 2)

dimnames(dataFiles)[[2]] <- c("Data Type", "File Name") 

   # user modification
   dataFiles <- edit(dataFiles)

   # modified data tables 

 

  Timeline<-inputdata(dataFiles[1, "File Name"])

########################################################################################### 
#  A 2-way crosstabulation of counts are generated as mat0, with structural zero's filled. 
#  This matrix should be an upper-triangle square matrix: week of onset vs week of reporting. 
###########################################################################################   
mat0<-matrixfill(Timeline)

##########################################################################################################################################
#  The following matrix will be used in ddelay function. It is also an upper-triangle square matrix: week of onset vs week of reporting.
##########################################################################################################################################
mat<-as.matrix(shift(mat0))

dimnames(mat)[[2]]<-0:(ncol(mat)-1)


##
incidence<-ddelay(mat,ncol(mat))$incidence


###########################
# make graphics
#########################


dates<-as.Date("1900-01-01") + as.numeric(dimnames(incidence)[[1]])-2
dimnames(incidence)[[1]]<-as.character(dates)
dimnames(incidence)[[2]]<-c("as reported", "delay adjusted", "95% LCL", "95% UCL")
plot(dates, incidence[,4], ylab="", main="Reporting delay adjustment", type="l", col=3)
lines(dates, incidence[,3], type="l", lty=2, col=3)
lines(dates, incidence[,2], type="b", col=2)
lines(dates, incidence[,1], type="b", col=1)
leg.names=c("95% upper CL", "95% lower CL",  "Delay adjusted epicurve by date of onset", "Reported epicuve by date of onset" )
legend("topright", leg.names, pch=c(NA, NA, 1,NA), bty="n", lty=c(1,2,1),col=c(3,3,2,1))



########################
# Save results to file #
########################


write.table(round(incidence), file = "/Users/amnatariq/Library/CloudStorage/Box-Box/Amna Tariq's Files/Documents/Stanford-work/Now-casting paper materials/EpiSurv4/Output/By-onset.txt", sep="\t", quote = FALSE, col.names=FALSE)




