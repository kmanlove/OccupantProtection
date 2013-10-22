#-- specify data path and write path --#
dat.path <-
	"~/work/Kezia/Research/Grants/MTDOT_OccupantProtection/Tasks/DataAnalysis/Data/CompiledData/"

write.path <-
	"~/work/Kezia/Research/Grants/MTDOT_OccupantProtection/Tasks/DataAnalysis/"

#-- load in compiled data --#
compd.data <- read.csv(paste(dat.path, "compileddata19Oct2013.csv", sep = ""), header = T)

#-- call required packages --#
require(lme4)
require(arm)
require(xtable)

#-- subset down to only total responses --#
tot.compd.data <- subset(compd.data, Driver_Passenger == "T")

#-- modify necessary covariates --#
tot.compd.data$OPICompltRate <- tot.compd.data$StuCompltd / tot.compd.data$StuEligible
tot.compd.data$TransformedPrecip <- log(tot.compd.data$Precip + 1)
tot.compd.data$Co2010Pop.standardized <- (tot.compd.data$Co2010Pop -
																					median(tot.compd.data$Co2010Pop)) / (2
																					* sd(tot.compd.data$Co2010Pop))
tot.compd.data$CoMedInc.standardized <- (tot.compd.data$CoMedInc -
																					median(tot.compd.data$CoMedInc)) / (2
																					* sd(tot.compd.data$CoMedInc))
tot.compd.data$Year <- factor(tot.compd.data$Year)
tot.compd.data$CitySTEPDaysRate <- ifelse(is.na(tot.compd.data$CitySTEPDays) ==
																					T, 0, tot.compd.data$CitySTEPDays / tot.compd.data$CityPop2010)
tot.compd.data$CitySTEPHoursRate <- ifelse(is.na(tot.compd.data$CitySTEPHours) ==
																					T, 0, tot.compd.data$CitySTEPHours / tot.compd.data$CityPop2010)
tot.compd.data$CitySTEPSitesRate <- ifelse(is.na(tot.compd.data$CitySTEPDays) ==
																					T, 0, tot.compd.data$CitySTEPDays / tot.compd.data$CityPop2010)
tot.compd.data$CoSTEPDaysRate <- ifelse(is.na(tot.compd.data$CoSTEPDays) ==
																					T, 0, tot.compd.data$CoSTEPDays /
																					tot.compd.data$Co2010Pop /
																					tot.compd.data$CoArea.misq)
tot.compd.data$CoSTEPHoursRate <- ifelse(is.na(tot.compd.data$CoSTEPHours) ==
																					T, 0, tot.compd.data$CoSTEPHours /
																					tot.compd.data$Co2010Pop /
																					tot.compd.data$CoArea.misq)
tot.compd.data$CoSTEPSitesRate <- ifelse(is.na(tot.compd.data$CoSTEPSites) ==
																					T, 0, tot.compd.data$CoSTEPSites /
																					tot.compd.data$Co2010Pop /
																					tot.compd.data$CoArea.misq)
tot.compd.data$DOJSTEPSitesRate <- ifelse(is.na(tot.compd.data$DOJSTEPSites) ==
																					T, 0, tot.compd.data$DOJSTEPSites /
																					tot.compd.data$DOJPop /
																					tot.compd.data$DOJArea.misq)
tot.compd.data$DOJSTEPHoursRate <- ifelse(is.na(tot.compd.data$DOJSTEPHours) ==
																					T, 0, tot.compd.data$DOJSTEPHours /
																					tot.compd.data$DOJPop /
																					tot.compd.data$DOJArea.misq)
tot.compd.data$DOJSTEPDaysRate <- ifelse(is.na(tot.compd.data$DOJSTEPDays) ==
																					T, 0, tot.compd.data$DOJSTEPDays /
																					tot.compd.data$DOJPop /
																					tot.compd.data$DOJArea.misq)
tot.compd.data$AggregateSTEPHoursRate <- tot.compd.data$DOJSTEPHoursRate + tot.compd.data$CoSTEPHoursRat + tot.compd.data$CitySTEPHoursRate
tot.compd.data$TVPaid <- ifelse(is.na(tot.compd.data$TVPaid) == T, 0,
																tot.compd.data$TVPaid)
tot.compd.data$RadioPaid <- ifelse(is.na(tot.compd.data$RadioPaid) == T, 0,
																tot.compd.data$RadioPaid)
tot.compd.data$CablePaid <- ifelse(is.na(tot.compd.data$CablePaid) == T, 0,
																tot.compd.data$CablePaid)
tot.compd.data$OnlinePaid <- ifelse(is.na(tot.compd.data$OnlinePaid) == T, 0,
																tot.compd.data$OnlinePaid)
tot.compd.data$TVCost <- ifelse(is.na(tot.compd.data$TVCost) == T, 0,
																tot.compd.data$TVCost)
tot.compd.data$RadioCost <- ifelse(is.na(tot.compd.data$RadioCost) == T, 0,
																tot.compd.data$RadioCost)
tot.compd.data$CableCost <- ifelse(is.na(tot.compd.data$CableCost) == T, 0,
																tot.compd.data$CableCost)
tot.compd.data$OnlineCost <- ifelse(is.na(tot.compd.data$OnlineCost) == T, 0,
																tot.compd.data$OnlineCost)
tot.compd.data$BUMTCoal <- ifelse(is.na(tot.compd.data$BUMTCoal) == T,
																	"NoCoalition", tot.compd.data$BUMTCoal)
tot.compd.data$TotMediaCost <- tot.compd.data$TVCost + tot.compd.data$RadioCost
+ tot.compd.data$CableCost + tot.compd.data$OnlineCost 

#-- Preliminary plotting --#
	#-- pairs plots of covariate blocks --#
mdt.pairs.covs <- subset(tot.compd.data, select = c("TransformedPrecip",
																										"Meantemp",
																										"Co2010Pop.standardized",
																										"CoMedInc.standardized",
																										"AvgCostPerStu",
																										"OPICompltRate",
																										"TotMediaCost",
																										"CitySTEPHoursRate",
																										"CoSTEPHoursRate",
																										"DOJSTEPHoursRate",
																										"AggregateSTEPHoursRate"))

#-- utility pairs plot functions --#
panel.hist <- function(x, ...){
	usr <- par("usr")
	on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5))
	h <- hist(x, plot = F)
	breaks <- h$breaks
	nB <- length(breaks)
	y <- h$counts
	y <- y / max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
	usr <- par("usr")
	on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r <- abs(cor(x, y))
	txt <- format(c (r, 0.1234567), digits = digits)[1]
	txt <- paste0(prefix, txt)
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex.cor * r)
}


#-- clip down to complete cases --#
complete.case.count <- table(complete.cases(mdt.pairs.covs) == T)["TRUE"]
mdt.pairs.covs.complete <- mdt.pairs.covs[complete.cases(mdt.pairs.covs)==T, ]

svg(paste(write.path, "Figures/FigureDrafts/PairsPlot", sep = ""), height = 10, width = 14)
pairs(mdt.pairs.covs.complete, lower.panel = panel.cor, diag.panel = panel.hist, upper.panel
			= panel.smooth)
dev.off()

pairs(mdt.pairs.covs.complete, lower.panel = panel.cor, diag.panel = panel.hist,
			upper.panel = panel.smooth)

#-- write out incomplete and complete tot.compd.data for comparison --#
	#-- 610 complete cases --#
tot.compd.data.complete <- tot.compd.data[complete.cases(mdt.pairs.covs) == T, ]
write.csv(tot.compd.data.complete, paste(write.path,
																				 "Data/CompiledData/CompleteCasesFinal.csv",
																				 sep = ""))
#-- 110 incomplete --#
tot.compd.data.incomplete <- tot.compd.data[complete.cases(mdt.pairs.covs) == F, ]
write.csv(tot.compd.data.incomplete, paste(write.path,
																				 "Data/CompiledData/IncompleteCasesFinal.csv",
																				 sep = ""))

#-- Try simple model to test --#
#saturated <- glmer(cbind(Belted_Yes, Belted_No) ~ Month + Stratum + TransformedPrecip +
#									  factor(Year) +
#									Meantemp + Co2010Pop.standardized + CoMedInc.standardized +
#									(1|Enum) + (1|County/Site),
#							 family = binomial( link = "logit"), data = tot.compd.data)
#
#saturated.nores <- glm(cbind(Belted_Yes, Belted_No) ~ Month + Stratum + TransformedPrecip +
#									 factor(Year) +
#									Meantemp + Co2010Pop.standardized + CoMedInc.standardized,
#							 family = binomial( link = "logit"), data = tot.compd.data)
#
#svg(paste(write.path, "SaturatedCoefPlot.svg", sep = ""), height = 10, width =
#		5)
#coefplot(saturated.nores)
#dev.off()

#-- MDT model fitting --#
sat.mdt.nointeract <- glmer(cbind(Belted_Yes, Belted_No) ~ Month + Stratum +
								 TransformedPrecip + 
									  factor(Year) +
									Meantemp + Co2010Pop.standardized + CoMedInc.standardized +
									 AvgCostPerStu + OPICompltRate + 
								 TotMediaCost + CoSTEPHoursRate +
								 CitySTEPHoursRate + 
								 DOJSTEPHoursRate +
								factor(BUMTCoal) + (1|County/Site),
							 family = binomial( link = "logit"), data =
							 tot.compd.data.complete)
	#-- converges --#

mdt.interact.fit <- glmer(cbind(Belted_Yes, Belted_No) ~ Stratum +
								 TransformedPrecip + 
									  factor(Year) +
									Meantemp + Co2010Pop.standardized + CoMedInc.standardized +
									Month + (OPICompltRate + 
								 TotMediaCost + 
								 AggregateSTEPHoursRate)^2 +
								factor(BUMTCoal) + (1|County/Site),
							 family = binomial( link = "logit"), data =
							 tot.compd.data.complete)
	#-- converges --#

#-- write model coefficients to tex format using xtable --#
interact.fecoefs <- coef(summary(mdt.interact.fit))
tex.fetab <- xtable(interact.fecoefs, digits = 4)
print(tex.fetab, type = "latex", paste(write.path,
																"Output/MDTInteractFitFECoefs_22Oct2013.txt",
																sep = ""))
