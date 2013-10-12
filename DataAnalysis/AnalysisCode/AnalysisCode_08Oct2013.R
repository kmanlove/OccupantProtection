#-- load in compiled data --#
compd.data <- read.csv("compileddata09Sept2013.csv", header = T)

#-- call required packages --#
require(lme4)
require(arm)

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

#-- Do preliminary plotting --#
#-- pairs plots of covariate blocks --#
pairs.covs <- subset(tot.compd.data, select = c("Meantemp", "TransformedPrecip",
																								"OPICompltRate", "CoPercUnins",
																								"Co2010Pop.standardized",
																								"CoMedInc.standardized",
																								"STEPSites", "STEPHours"))

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

write.path <-
	"~/work/Kezia/Research/Grants/MTDOT_OccupantProtection/Tasks/DataAnalysis/Figures/FigureDrafts/"
svg(paste(write.path, "PairsPlot", sep = ""), height = 10, width = 14)
pairs(pairs.covs, lower.panel = panel.cor, diag.panel = panel.hist, upper.panel
			= panel.smooth)
dev.off()

#-- Try simple model to test --#
saturated <- glmer(cbind(Belted_Yes, Belted_No) ~ Month + Stratum + TransformedPrecip +
									  factor(Year) +
									Meantemp + Co2010Pop.standardized + CoMedInc.standardized +
									(1|Enum) + (1|County/Site),
							 family = binomial( link = "logit"), data = tot.compd.data)

saturated.nores <- glm(cbind(Belted_Yes, Belted_No) ~ Month + Stratum + TransformedPrecip +
									 factor(Year) +
									Meantemp + Co2010Pop.standardized + CoMedInc.standardized,
							 family = binomial( link = "logit"), data = tot.compd.data)

svg(paste(write.path, "SaturatedCoefPlot.svg", sep = ""), height = 10, width =
		5)
coefplot(saturated.nores)
dev.off()
