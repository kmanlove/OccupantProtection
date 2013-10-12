#-- Data to read in spatial data and assemble a GIS in R --#
#install.packages("rgdal")
#install.packages("sp")
#install.packages("maptools")
require(rgdal)
require(sp)
require(maptools)

#-- read in various shapefiles --#
filepath <-
	"~/work/Kezia/Research/Grants/MTDOT_OccupantProtection/Tasks/DataAnalysis/Data/GeographicData/"

mtcounties <- readShapePoly(paste (filepath, "MTCounties/tl_2010_30_county10.shp",  
																	 sep = ""), IDvar = "NAME10")
	#-- names(mtcounties) shows element "NAME10", which contains county names --#
mtschdist <- readShapePoly(paste (filepath,
																	"SchoolDistrictBoundaries/tl_2010_30_scsd10.shp",
																	sep = ""))
mtres <- readShapePoly(paste (filepath,
															"TribalTracts2010Census/TribalTract_2010Census_DP1.shp",
															sep = ""))
mtseatsites <- getKMLcoordinates(paste (filepath,
														 "MDTSiteLocations/SeatBeltSurveySites_1.kml", sep
													 = ""), ignoreAltitude =
TRUE)

#-- note that mtseatsites is a list, with n 1x2 vector elements --#
	#-- Build SpatialPoints object for each seatbelt sampling site --#

mtseatvec <- unlist(mtseatsites)
index <- rep(c(0, 1), length(mtseatvec) / 2) 
mtseatx <- mtseatvec[index == 0] 
mtseaty <- mtseatvec[index == 1] 
coord.mat <- cbind(mtseatx, mtseaty)
mtseatpoints <- SpatialPoints(coord.mat)

#-- preliminary plotting --#
mapasp(mtcounties)
plot(mtcounties)
points(mtseaty ~ mtseatx, pch = 16, col = "blue")

dev.new()
plot(mtschdist)
points(mtseaty ~ mtseatx, pch = 16, col = "blue")

dev.new()
plot(mtres)
points(mtseaty ~ mtseatx, pch = 16, col = "blue")

#-- overlay site SpatialPoints object on each polygon object to get what --#
#-- polygon each point falls within --#

SiteCounties <- (mtseatpoints %over% mtcounties)$NAME10
SiteSchDist <- (mtseatpoints %over% mtschdist)$NAME10
SiteTribal <- (mtseatpoints %over% mtres)$NAMELSAD10

#-- Build an output dataframe for each site and its corresponding --#
#-- county, school district, and tribal status --#

site.dat <- as.data.frame(cbind(as.character(SiteCounties),
																						 as.character(SiteSchDist),
																						 as.character(SiteTribal)))

write.csv(site.dat, "SeatSitesShapeIntersection.csv")

#-- preliminary tabulation of school districts/counties --#
site.dat <- read.csv("SeatSitesShapeIntersection.csv", header = T, sep = "\t")
write.csv(table(site.dat$County), "SiteCountyList.csv")
write.csv(table(site.dat$HighSchoolDistrict), "SiteSchoolDistrictList.csv")

#-- pull in response data from each sampling event --#
resppath <-
	"~/work/Kezia/Research/Grants/MTDOT_OccupantProtection/Tasks/DataAnalysis/Data/"

compd.data <- read.csv(paste(resppath, "CompiledData/compileddata_08Sept2013.csv", sep = ""), header = T, sep = "\t")

counties <- read.csv(paste(resppath,
													 "GeographicData/CountySocioecon/CompiledSocioecCountyLevel.csv",
													 sep = ""), header = T)

opi <- read.csv(paste(resppath,
											"OPIData/CompositeSchoolDistrictData_01July2013.csv",
											sep = ""), header = T)

match.list <- read.csv(paste(resppath,
													 "CoSchDistSiteMatching/SeatSitesShapeIntersection.csv", sep =
													 ""), header = T, sep = "\t")

sampling.dat <- read.csv(paste(resppath, "CoSchDistSiteMatching/SiteNames.csv",
															 sep = ""), header = T, sep = "\t")

compd.data$OPIavgcost <- compd.data$OPICompleted <- rep(NA, dim(compd.data)[1]) 
compd.data$OPIEligible <- compd.data$CoPercUnins <- rep(NA, dim(compd.data)[1]) 
compd.data$CoMedInc <- compd.data$Co2010Pop <- rep(NA, dim(compd.data)[1])
compd.data$STEPHours <- compd.data$STEPSites <- rep(NA, dim(compd.data)[1]) 
compd.data$BUMTCoal <- compd.data$County <- rep(NA, dim(compd.data)[1]) 
compd.data$SchDist <- compd.data$Tribe <- rep(NA, dim(compd.data)[1])
compd.data$SiteNum <- compd.data$SampDay <- rep(NA, dim(compd.data)[1])
compd.data$StartTime <- compd.data$Duration <- rep(NA, dim(compd.data)[1])
compd.data$StuEligible <- compd.data$StuCompltd <- rep(NA, dim(compd.data)[1])
compd.data$AvgCostPerStu <- rep(NA, dim(compd.data)[1])

for(i in 2076:dim(compd.data)[1]){
	sitespecs <- subset(sampling.dat, as.character(SiteName) ==
									 as.character(compd.data$Site[i]))
	compd.data$SiteNum[i] <- as.numeric(as.character(sitespecs$SiteNum))
	compd.data$SampDay[i] <- as.character(sitespecs$Day)
	compd.data$StartTime[i] <- as.numeric(as.character(sitespecs$StartTime))
	compd.data$Duration[i] <- as.numeric(as.character(sitespecs$Duration))

	matchspecs <- subset(match.list, SiteNo ==
											 as.numeric(as.character(compd.data$SiteNum[i])))
	compd.data$County[i] <- as.character(matchspecs$County)
	compd.data$SchDist[i] <- as.character(matchspecs$SchDist)
	compd.data$Tribe[i] <- as.character(matchspecs$Tribe)

	cospecs <- subset(counties, as.character(County) ==
										as.character(compd.data$County[i]))
	compd.data$CoMedInc[i] <-
		as.numeric(as.character(cospecs$MedianHouseholdIncome))
	compd.data$Co2010Pop[i] <- as.numeric(as.character(cospecs$X2010Pop))
	compd.data$CoPercUnins[i] <- as.numeric(as.character(cospecs$NotInsured))
	compd.data$BUMTCoal[i] <- as.character(cospecs$BUMTCoalition)
	compd.data$STEPSites[i] <- as.numeric(as.character(cospecs$STEPSites))
	compd.data$STEPHours[i] <- as.numeric(as.character(cospecs$STEPHours))

	distspecs <- subset(opi, as.character(District) ==
											as.character(compd.data$SchDist[i]) &
											as.numeric(as.character(Year)) ==
											as.numeric(as.character(compd.data$Year[i])))
	compd.data$StuElibigle[i] <-
		ifelse(dim(distspecs)[1] == 0, NA,
					 as.numeric(as.character(distspecs$StudentsEligible)))	
	compd.data$StuCompltd[i] <-  
		ifelse(dim(distspecs)[1] == 0, NA,
					 as.numeric(as.character(distspecs$StudentsCompleted)))	
	compd.data$AvgCostPerStu[i] <- 
		ifelse(dim(distspecs)[1] == 0, NA,
					 as.numeric(as.character(distspecs$AvgCostPerStu)))	
}

write.csv(compd.data, paste(resppath,
														"CompiledData/compileddata09Sept2013.csv", sep = ""))
