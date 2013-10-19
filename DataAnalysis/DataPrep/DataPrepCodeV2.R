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

#compd.data <- read.csv(paste(resppath, "CompiledData/compileddata_08Sept2013.csv", sep = ""), header = T, sep = "\t")

compd.data <- read.csv(paste(resppath, "CompiledData/compileddata09Sept2013.csv", sep = ""), header = T, sep = "\t")

#counties <- read.csv(paste(resppath,
#													 "GeographicData/CountySocioecon/CompiledSocioecCountyLevel_V2.csv",
#													 sep = ""), sep = "\t", header = T)

counties <- read.csv(paste(resppath,
													 "GeographicData/CountySocioecon/CompiledSocioecCountyLevel_V3.csv",
													 sep = ""), sep = ",", header = T)

opi <- read.csv(paste(resppath,
											"OPIData/CompositeSchoolDistrictData_01July2013.csv",
											sep = ""), header = T)

match.list <- read.csv(paste(resppath,
													 "CoSchDistSiteMatching/SeatSitesShapeIntersection.csv", sep =
													 ""), header = T, sep = "\t")

sampling.dat <- read.csv(paste(resppath, "CoSchDistSiteMatching/SiteNames.csv",
															 sep = ""), header = T, sep = "\t")

sitecity <- read.csv(paste(resppath,
													 "GeographicData/MTCitiesTowns/SiteCityMatch_25Sept2013.csv",
													 sep = ""), header = T, sep = "\t")

media.dat <- read.csv(paste(resppath,
					"GeographicData/MTCitiesTowns/MTCityMediaData_12Oct2013.csv", sep
														= ""), header = T)

CitySTEPData <- read.csv(paste(resppath,
															 "GeographicData/MTCitiesTowns/CitySpecificSTEPData_08Oct2013.csv",
															 sep = ""), header = T)

DojSTEPData <- read.csv(paste(resppath,
															"GeographicData/DOJStepData_08Oct2013.csv", sep =
															""), header = T, sep = "\t")

BUMTBudget <- read.csv(paste(resppath, "BUMT/CoalitionBudgetSummary.csv", sep =
														 ""), sep = "\t", header = T)

#compd.data$OPIavgcost <- compd.data$OPICompleted <- rep(NA, dim(compd.data)[1]) 
#compd.data$OPIEligible <- compd.data$CoPercUnins <- rep(NA, dim(compd.data)[1]) 
#compd.data$CoMedInc <- compd.data$Co2010Pop <- rep(NA, dim(compd.data)[1])
#compd.data$STEPHours <- compd.data$STEPSites <- rep(NA, dim(compd.data)[1]) 
#compd.data$BUMTCoal <- compd.data$County <- rep(NA, dim(compd.data)[1]) 
#compd.data$SchDist <- compd.data$Tribe <- rep(NA, dim(compd.data)[1])
#compd.data$SiteNum <- compd.data$SampDay <- rep(NA, dim(compd.data)[1])
#compd.data$StartTime <- compd.data$Duration <- rep(NA, dim(compd.data)[1])
#compd.data$StuEligible <- compd.data$StuCompltd <- rep(NA, dim(compd.data)[1])
#compd.data$AvgCostPerStu <- rep(NA, dim(compd.data)[1])
compd.data$CitySTEPHours <- compd.data$CitySTEPSites <- rep(NA,
																														dim(compd.data)[1])
compd.data$CitySTEPDays <- compd.data$DOJSTEPHours <- rep(NA,
																													dim(compd.data)[1])
compd.data$DOJSTEPSites <- compd.data$DOJSTEPDays <- rep(NA,
																												 dim(compd.data)[1])
compd.data$CoSTEPSites <- compd.data$CoSTEPHours <- rep(NA, dim(compd.data)[1])
compd.data$CoSTEPDays <- compd.data$DOJPop <- rep(NA, dim(compd.data)[1])
compd.data$TVPaid <- compd.data$TVEarned <- rep(NA, dim(comdp.data)[1])
compd.data$RadioPaid <- compd.data$RadioEarned <- rep(NA, dim(compd.data)[1])
compd.data$CablePaid <- compd.data$CableEarned <- rep(NA, dim(compd.data)[1])
compd.data$OnlinePaid <- compd.data$OnlineEarned <- rep(NA, dim(compd.data)[1])
compd.data$TVCost <- compd.data$CableCost <- rep(NA, dim(compd.data)[1])
compd.data$RadioCost <- compd.data$OnlineCost <- rep(NA, dim(compd.data)[1])
compd.data$City <- compd.data$CityPop2010 <- rep(NA, dim(compd.data)[1])
compd.data$BUMTExpend <- compd.data$DOJPop2010 <- rep(NA, dim(compd.data)[1])
compd.data$BUMTBudget <- compd.data$DOJDistrict <- rep(NA, dim(compd.data)[1])

#-- errors at 69, 70, 310, 311, 515, 635, 755, 875, 1029, 1030, 1235, 1355
#-- 1475, 1595, 1715, 1835, 1955, 2075
to.remove <- c(69, 70, 309, 310, 311, 515, 635, 755, 875, 1029, 1030, 1235, 1355,
							 1475, 1595, 1715, 1835, 1955, 2075)
to.use <- seq(1:dim(compd.data)[1])[-to.remove]

for(i in to.use){
#	sitespecs <- subset(sampling.dat, as.character(SiteName) ==
#									 as.character(compd.data$Site[i]))
#	compd.data$SiteNum[i] <- as.numeric(as.character(sitespecs$SiteNum))
#	compd.data$SampDay[i] <- as.character(sitespecs$Day)
#	compd.data$StartTime[i] <- as.numeric(as.character(sitespecs$StartTime))
#	compd.data$Duration[i] <- as.numeric(as.character(sitespecs$Duration))
#
	matchspecs <- subset(match.list, SiteNo ==
											 as.numeric(as.character(compd.data$SiteNum[i])))
	compd.data$County[i] <- as.character(matchspecs$County)
#	compd.data$SchDist[i] <- as.character(matchspecs$SchDist)
#	compd.data$Tribe[i] <- as.character(matchspecs$Tribe)
#
#	cospecs <- subset(counties, as.character(County) ==
#										as.character(compd.data$County[i]))
	cospecs <- subset(counties, as.character(County) ==
										as.character(compd.data$County[i]))
#	compd.data$CoMedInc[i] <-
#		as.numeric(as.character(cospecs$MedianHouseholdIncome))
#	compd.data$Co2010Pop[i] <- as.numeric(as.character(cospecs$X2010Pop))
#	compd.data$CoPercUnins[i] <- as.numeric(as.character(cospecs$NotInsured))
#	compd.data$BUMTCoal[i] <- as.character(cospecs$BUMTCoalition)
#	compd.data$STEPSites[i] <- as.numeric(as.character(cospecs$STEPSites))
#	compd.data$STEPHours[i] <- as.numeric(as.character(cospecs$STEPHours))
	compd.data$CoSTEPHours[i] <- as.numeric(as.character(cospecs$STEPHours))
	compd.data$CoSTEPSites[i] <- as.numeric(as.character(cospecs$STEPSites))
	compd.data$CoSTEPDays[i] <- as.numeric(as.character(cospecs$STEPDays))
	compd.data$DOJDistrict[i] <- as.numeric(as.character(cospecs$DOJDistrict))

#	distspecs <- subset(opi, as.character(District) ==
#											as.character(compd.data$SchDist[i]) &
#											as.numeric(as.character(Year)) ==
#											as.numeric(as.character(compd.data$Year[i])))
#	compd.data$StuElibigle[i] <-
#		ifelse(dim(distspecs)[1] == 0, NA,
#					 as.numeric(as.character(distspecs$StudentsEligible)))	
#	compd.data$StuCompltd[i] <-  
#		ifelse(dim(distspecs)[1] == 0, NA,
#					 as.numeric(as.character(distspecs$StudentsCompleted)))	
#	compd.data$AvgCostPerStu[i] <- 
#		ifelse(dim(distspecs)[1] == 0, NA,
#					 as.numeric(as.character(distspecs$AvgCostPerStu)))	

	sitecitymatch <- subset(sitecity, as.character(Site) ==
													as.character(compd.data$Site[i]))
	compd.data$City[i] <- ifelse(dim(sitecitymatch)[1] == 0, NA,
															 as.character(sitecitymatch$City[1]))
	compd.data$CitySTEPSites[i] <- ifelse(dim(sitecitymatch)[1] == 0, NA, 
		as.numeric(as.character(sitecitymatch$STEPSites[1])))
	compd.data$CitySTEPHours[i] <- ifelse(dim(sitecitymatch)[1] == 0, NA, 
		as.numeric(as.character(sitecitymatch$STEPHours[1])))
	compd.data$CitySTEPDays[i] <- ifelse(dim(sitecitymatch)[1] == 0, NA, 
		as.numeric(as.character(sitecitymatch$STEPDays[1])))
	compd.data$CityPop2010[i] <- ifelse(dim(sitecitymatch)[1] == 0, NA, 
		as.numeric(as.character(sitecitymatch$CityPop2010[1])))

	mediamatch <- subset(media.dat, as.character(City) ==
											 as.character(compd.data$City[i]) & as.character(Year) ==
											 as.character(compd.data$Year[i]))
	compd.data$TVPaid[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
															as.numeric(as.character(mediamatch$TVPaid)))
	compd.data$TVEarned[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																as.numeric(as.character(mediamatch$TVBonus)))
	compd.data$RadioPaid[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																 as.numeric(as.character(mediamatch$RadioPaid)))
	compd.data$CablePaid[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																 as.numeric(as.character(mediamatch$CablePaid)))
	compd.data$OnlinePaid[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																	as.numeric(as.character(mediamatch$OnlinePaid)))
	compd.data$RadioEarned[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																 as.numeric(as.character(mediamatch$RadioBonus)))
	compd.data$CableEarned[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																	 as.numeric(as.character(mediamatch$CableBonus)))
	compd.data$OnlineEarned[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																		as.numeric(as.character(mediamatch$OnlineBonus)))
	compd.data$TVCost[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																		as.numeric(as.character(mediamatch$TVCost)))
	compd.data$RadioCost[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																		as.numeric(as.character(mediamatch$RadioCost)))
	compd.data$CableCost[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																		as.numeric(as.character(mediamatch$CableCost)))
	compd.data$OnlineCost[i] <- ifelse(dim(mediamatch)[1] == 0, NA,
																		as.numeric(as.character(mediamatch$OnlineCost)))
 
	DOJDistDat <- subset(DojSTEPData, DOJDistrict == compd.data$DOJDistrict[i])
	compd.data$DOJSTEPSites[i] <- ifelse(dim(DOJDistDat)[1] == 0, na,
																			 DOJDistDat$STEPSites) 
	compd.data$DOJSTEPDays[i] <- ifelse(dim(DOJDistDat)[1] == 0, na,
																			 DOJDistDat$STEPDays) 
	compd.data$DOJSTEPHours[i] <- ifelse(dim(DOJDistDat)[1] == 0, na,
																			 DOJDistDat$STEPHours) 

	BumtExpend <- subset(BUMTBudget, as.character(BUMTCoalition) ==
											 as.character(compd.data$BUMTCoalition[i]) & Year ==
											 compd.data$Year[i])
	compd.data$BUMTBudget[i] <- ifelse(dim(BumtExpend)[1] == 0, NA, Budget)
	compd.data$BUMTExpend[i] <- ifelse(dim(BumtExpend)[1] == 0, NA, Expenditure)
}

#-- errors at 69, 70, 310, 311, 515, 635, 755, 875, 1029, 1030, 1235, 1355
#-- 1475, 1595, 1715, 1835, 1955, 2075

write.csv(compd.data, paste(resppath,
														"CompiledData/compileddata16Oct2013.csv", sep = ""))
