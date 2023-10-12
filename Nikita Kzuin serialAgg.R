#libraries
library(readr)


# Functions
# Vadim's function
serialAgg=function (x, AggCats, AggTarg = NULL, FUN = function(x) mean(x, na.rm = TRUE)){
  if (is.null(AggTarg)) {
    if (is.numeric(AggCats)) 
      AggTarg = (1:ncol(x))[-AggCats]
    if (is.character(AggCats)) 
      AggTarg = colnames(x)[!colnames(x) %in% AggCats]
  }
  Numbers = prod(apply(t(t(x[, AggCats])), 2, is.numeric))
  ncat = length(AggCats)
  if (ncat == 1) 
    Cats = as.character(x[, AggCats])
  else Cats = codify(x[, AggCats])
  agged = as.matrix(aggregate(x[, AggTarg], by = list(Cats), FUN = FUN))
  if (ncat > 1) 
    agged = cbind(matrix(unlist(strsplit(agged[, 1], 
                                         "_")), ncol = ncat, byrow = TRUE), matrix(agged[, 
                                                                                         -1], ncol = ncol(agged) - 1))
  if (Numbers) 
    agged = t(apply(agged, 1, as.numeric))
  colnames(agged) = colnames(cbind(x[, c(AggCats[1], AggCats)], 
                                   x[, c(AggTarg, AggTarg[1])]))[c(1, 3:(ncol(agged) + 1))]
  agged
}

codify=function (x, cols = 1:ncol(x), sep = "_") 
  as.matrix(cbind(Index = apply(x[, cols], 1, paste0, collapse = sep),  x[, -cols]))


# import data
data <- read.csv("KU/Biology Research/Data/NOAA_EcosystemData_10-6-2023.csv")

# aggregate(EXPCATCHNUM ~ spnm, data = data, FUN = length) # Equivalent
# unique(data$spnm)
# length(unique(data$spnm))

# new df of Bobtail squid only
bobtail_df <- data[which(data$spnm == "BOBTAIL UNCL"),]

# density and biomass agreggated by year
density_y <- serialAgg(bobtail_df, AggCats = "GMT_YEAR", AggTarg = "EXPCATCHNUM")
biomass_y <- serialAgg(bobtail_df, AggCats = "GMT_YEAR", AggTarg = "EXPCATCHWT")

# density over time plot
plot(density_y, type = "l", lty = 1,
     main = "Bobtail's Density over Time",
     xlab = "Year",
     ylab = "Density")

# biomass over time plot
plot(biomass_y, type = "l", lty = 1,
     main = "Bobtail's Biomass over Time",
     xlab = "Year",
     ylab = "Biomass")

#Activity 5
# add a column of the lattitude rounded to a degree
bobtail_df$latr <- round(bobtail_df$DECDEG_BEGLAT)

# density and biomass agreggated by year and lattitude
density_yl <- serialAgg(bobtail_df, AggCats = c("GMT_YEAR", "latr"), AggTarg = "EXPCATCHNUM")
biomass_yl <- serialAgg(bobtail_df, AggCats = c("GMT_YEAR", "latr"), AggTarg = "EXPCATCHWT")

# see unique lattitude
unique(density_yl[,2])

#plot the biomass for each latitude
c = 1
color <- c
name <- sort(unique(density_yl[,2]))
lats <- name[-1]
plot(biomass_yl[biomass_yl[,2] == 35, c(1,3)], type = "l", col = c, 
     ylim = c(min(biomass_yl[,3]), max(biomass_yl[,3])),
     main = "Bobtail's Biomass over Time per Lattitude",
     xlab = "Year",
     ylab = "Biomass")

for (i in lats) {
  c = c + 1
  lines(biomass_yl[biomass_yl[,2] == i, c(1,3)], col = c + 1)
  color <- c(color, c)
}
legend(x = "topleft",
       legend = name,
       lty = 1,
       col = color,
       bg="transparent",
       title = "Lattitude") 

#Activity 6
#Histograms of the following variables:  GMT_YEAR, fall, STRATUM, SVSPP, doy, 
#DECDEG_BEGLAT, DECDEG_BEGLON, EXPCATCHWT

par(mfrow=c(3,3))
hist_columns <- c(1:7,16)
for (i in hist_columns) {
  hist(bobtail_df[,i], main = colnames(data[i]), xlab = "")
}

# Plotting doy, AVGDEPTH, SVVESSEL, and TOWDUR over GMT_YEAR
par(mfrow=c(2,2))
plot(bobtail_df[,c(1,5)])
plot(bobtail_df[,c(1,10)])
plot(bobtail_df[,c(1,9)])
plot(bobtail_df[,c(1,13)])










