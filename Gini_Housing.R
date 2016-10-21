## APM Data Inequality Analysis -- Data Import and Clean 20160923 BakerM

library(plyr)
library(dplyr)
library(ggplot2)
library(ineq)

Dat2012 <- read.csv("~/Documents/NewsLabs/housing/APM/201609 Matt Baker Raw Basic Sydney_Jun12.csv")
Dat2013 <- read.csv("~/Documents/NewsLabs/housing/APM/201609 Matt Baker Raw Basic Sydney_Jun13.csv")
Dat2014 <- read.csv("~/Documents/NewsLabs/housing/APM/201609 Matt Baker Raw Basic Sydney_Jun14.csv")
Dat2015 <- read.csv("~/Documents/NewsLabs/housing/APM/201609 Matt Baker Raw Basic Sydney_Jun15.csv")
Dat2016 <- read.csv("~/Documents/NewsLabs/housing/APM/201609 Matt Baker Raw Basic Sydney_Jun16.csv")

Dat2012t <- Dat2012[,c(1,4,5,7,8,9)]
head(Dat2012t)

# chop into one dataframe
## woudl have been better to do this with list, then use rbind, .... anyway, only 5 sets but thinking forward for scale.
hdf1 <- data.frame(Dat2012)
hdf1$year <- 2012
hdf2 <- data.frame(Dat2013)
hdf2$year <- 2013
hdf3 <- data.frame(Dat2014)
hdf3$year <- 2014
hdf4 <- data.frame(Dat2015)
hdf4$year <- 2015
hdf5 <- data.frame(Dat2016)
hdf5$year <- 2016

hdfall <- rbind(hdf1,hdf2,hdf3,hdf4,hdf5)
hdfall <- hdfall[,c(1,3,4,5,7,8,9,10)] #remove redundant eventID,NSW

#rename lowercase, 
names(hdfall) <- c("cat","houseID","suburb","postcode","date","price","proptype","year")

sort.hdfall <- hdfall[order(hdfall$suburb),]

usdf <- unique(sort.hdfall)

tusdf <- tbl_df(usdf)

tusdf <- tusdf[!duplicated(tusdf$houseID),]  ## remove duplicates based on houseID

## Analysis across all 5 years bundled together (no trend, just overall gini)

# remove zero price sales (why are they in here?)
tusdf <- group_by(tusdf,price) %>%
  filter(price > 50000, proptype!="Land") ## 50,000 is arbitrary, but there are some definite funny ones < 10,000

## remove postcodes outside 40km radius (CAREFUL, CHOPPING A LOT)
# postcodes30km <- c(2000,2006,2007,2008,2009,2010,2011,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036,2037,2038,2039,2040,2041,2042,2043,2044,2045,2046,2047,2048,2049,2050,2055,2060,2061,2062,2063,2064,2065,2066,2067,2068,2069,2070,2071,2072,2073,2074,2075,2076,2077,2079,2080,2081,2082,2084,2085,2086,2087,2088,2089,2090,2092,2093,2094,2095,2096,2097,2099,2100,2101,2102,2103,2104,2105,2106,2107,2108,2110,2111,2112,2113,2114,2115,2116,2117,2118,2119,2120,2121,2122,2125,2126,2127,2128,2130,2131,2132,2133,2134,2135,2136,2137,2138,2140,2141,2142,2143,2144,2145,2146,2147,2148,2150,2151,2152,2153,2154,2155,2156,2158,2159,2160,2161,2162,2163,2164,2165,2166,2168,2170,2172,2173,2176,2177,2190,2191,2192,2193,2194,2195,2196,2197,2198,2199,2200,2203,2204,2205,2206,2207,2208,2209,2210,2211,2212,2213,2214,2216,2217,2218,2219,2220,2221,2222,2223,2224,2225,2226,2227,2228,2229,2230,2231,2232,2233,2234)
# tusdf <- group_by(tusdf,postcode) %>%
#   filter(postcode %in% postcodes30km)


housetypes <- c("House","Cottage","Semi","Terrace","Townhouse","Villa")
unittypes <- c("Duplex","Flat","Unit")

# include only Houses
housedf <- group_by(tusdf,price) %>%
  filter(price > 0, proptype %in% housetypes)

#include only Units
unitdf <- group_by(tusdf,price) %>%
  filter(price > 0, proptype %in% unittypes)

# filter so have at least ten sales per suburb, calculate mean, min/max price and number entries
housesubs <- group_by(housedf, suburb) %>%
  summarise(n_entries = n(), mprice = mean(price), sp = sd(price), minprice = min(price), maxprice = max(price)) %>%
  filter(n_entries > 100)

unitsubs <- group_by(unitdf, suburb) %>%
  summarise(n_entries = n(), mprice = mean(price), sp = sd(price), minprice = min(price), maxprice = max(price)) %>%
  filter(n_entries > 100)

# ordbigsubs <- bigsubs[order(bigsubs$n_entries),]

housesubtus <- housedf[housedf$suburb %in% housesubs$suburb,]
unitsubtus <- unitdf[unitdf$suburb %in% unitsubs$suburb,]

## postcodes below, but just doing suburbs for now
postcodes <- group_by(tusdf,postcode) %>%
  filter(price > 0, proptype %in% housetypes) %>%
  summarise(n_post = n(),mprice = mean(price), sp = sd(price), minprice = min(price), maxprice = max(price)) %>%
  filter(n_post > 100)

posttus <- tusdf[tusdf$postcode %in% postcodes$postcode,]

library(ineq)

#calculate gini across all suburbs in bigsub (> 10 suburbs) ## CLOSE TO 1 IS LESS EQUAL, CLOSE TO ZERO IS MORE EQUAL
unit_gini_sub <- group_by(unitsubtus,suburb) %>%## duplicate or change for houses vs units
  summarise(gini = ineq(price))
unit_gini_sub <- inner_join(unit_gini_sub,unitsubs)

house_gini_sub <- group_by(housesubtus,suburb) %>%## duplicate or change for houses vs units
  summarise(gini = ineq(price))
house_gini_sub <- inner_join(house_gini_sub,housesubs)


h_top20sub <- arrange(house_gini_sub,desc(gini))[1:20,]
h_bot20sub <- arrange(house_gini_sub,gini)[1:20,]

u_top20sub <- arrange(unit_gini_sub,desc(gini))[1:20,]
u_bot20sub <- arrange(unit_gini_sub,gini)[1:20,]

gini_post <- group_by(posttus,postcode) %>%
  summarise(gini = ineq(price))

gini_post <- inner_join(gini_post,postcodes)

top5post <- arrange(gini_post,desc(gini))[1:5,]
bot5post <- arrange(gini_post,gini)[1:5,]

## Adding year to year data to same tbl_dfs, ie, now we want to have gini2012, gini2013 etc, so we can track movements. Perhaps best to have this as a List?

# doing each one, probably a better way not using dplyr, but it works.

h_giniyearlist <- list('vector')
yearindex <- c(2012,2013,2014,2015,2016)
for (i in 1:5) {
  h_giniyearlist[[i]] <- group_by(housesubtus, suburb) %>%
    filter(year==yearindex[i]) %>%
    summarise(ny_entries = n(), mprice = mean(price), sp = sd(price), minprice = min(price), maxprice = max(price), giniyear = ineq(price)) %>%
    filter(ny_entries > 100) ## need the double filter as only filtered previously >100 over all 5 years
  names(h_giniyearlist[[i]])[7] = paste("gini",yearindex[i], sep = "") ## change name for later join
}

u_giniyearlist <- list('vector')
yearindex <- c(2012,2013,2014,2015,2016)
for (i in 1:5) {
  u_giniyearlist[[i]] <- group_by(unitsubtus, suburb) %>%
    filter(year==yearindex[i]) %>%
    summarise(ny_entries = n(), mprice = mean(price), sp = sd(price), minprice = min(price), maxprice = max(price), giniyear = ineq(price)) %>%
    filter(ny_entries > 50) ## need the double filter as only filtered previously >100 over all 5 years
  names(u_giniyearlist[[i]])[7] = paste("gini",yearindex[i], sep = "") ## change name for later join
}

## calculate gini by year using Reduce and merge and discarding other columns
hgby <- Reduce(function(x, y) merge(x, y, by="suburb"),h_giniyearlist)
keepers <- c("suburb","gini2012","gini2013","gini2014","gini2015","gini2016")
hgby<- hgby[,keepers]

ugby <- Reduce(function(x, y) merge(x, y, by="suburb"),u_giniyearlist)
keepers <- c("suburb","gini2012","gini2013","gini2014","gini2015","gini2016")
ugby<- ugby[,keepers]

## biggest changes
# houses first
hgbym <- as.matrix(hgby[,2:6])
ranger <- function(x) max(x) - min(x) ## my function to determine range
range_gini <- apply(hgbym,1,ranger)
hgby[,7] <- range_gini
hgby[,8] <- hgbym[,1]-hgbym[,5]
names(hgby)[7] <- "range_gini"
names(hgby)[8] <- "startfin"
sort.hgby <- hgby[order(hgby$range_gini),]
sort2.hgby <- hgby[order(hgby$startfin),]
# check range and startfin, something up here, or just remove

#calculate linear regression over each suburb in gini_by_year
gradient = vector()
for (i in 1:dim(hgbym)[1]) {
  templm <- lm(hgbym[i,] ~ yearindex)
  gradient[i] <- templm$coefficients[2]
}

hgby[,9] <- gradient
names(hgby)[9] <- "gradient"
sort3.hgby <- hgby[order(hgby$gradient),]

topgradsubs <- sort3.hgby[1:5,]$suburb
botgradsubs <- tail(sort3.hgby,5)$suburb

## repeat for units  ## should have done this as a function

ugbym <- as.matrix(ugby[,2:6])
ranger <- function(x) max(x) - min(x) ## my function to determine range
range_gini <- apply(ugbym,1,ranger)
ugby[,7] <- range_gini
ugby[,8] <- ugbym[,1]-ugbym[,5]
names(ugby)[7] <- "range_gini"
names(ugby)[8] <- "startfin"
sort.ugby <- ugby[order(ugby$range_gini),]
sort2.ugby <- ugby[order(ugby$startfin),]
# check range and startfin, something up here, or just remove

#calculate linear regression over each suburb in gini_by_year
gradient = vector()
for (i in 1:dim(ugbym)[1]) {
  templm <- lm(ugbym[i,] ~ yearindex)
  gradient[i] <- templm$coefficients[2]
}

ugby[,9] <- gradient
names(ugby)[9] <- "gradient"
sort3.ugby <- ugby[order(ugby$gradient),]

topunitgradsubs <- sort3.ugby[1:5,]$suburb
botunitgradsubs <- tail(sort3.ugby,5)$suburb

## same thing with postcode: TBD (as above, group_by postcode)

##### PLOTS AND VISUALISATION

## boxplots

#rearrange top and bot ready to plot
hkeep_top5 <- h_top20sub[1:5,]$suburb
hkeep_bot5 <- h_bot20sub[1:5,]$suburb

ht5df <- housedf[housedf$suburb %in% hkeep_top5,]
hb5df <- housedf[housedf$suburb %in% hkeep_bot5,]

ukeep_top5 <- u_top20sub[1:5,]$suburb
ukeep_bot5 <- u_bot20sub[1:5,]$suburb

ut5df <- unitdf[unitdf$suburb %in% ukeep_top5,]
ub5df <- unitdf[unitdf$suburb %in% ukeep_bot5,]

ht5df <- droplevels(ht5df) # otherwise plots retain all the suburbs; makes it a bit hard to read.
hb5df <- droplevels(hb5df)
ut5df <- droplevels(ut5df)
ub5df <- droplevels(ub5df)

#boxplots
pdf("House_Top5Gini_Boxplot.pdf")
boxplot(price~suburb,data=ht5df, main="House Price per Suburb",xlab="Top 5 Disparate Suburbs",ylab="Price",ylim = c(0,10e6),cex.axis = 0.5)
dev.off()

pdf("House_Bot5Gini_Boxplot.pdf",5,5)
boxplot(price~suburb,data=hb5df, main="House Price per Suburb",xlab="Bot 5 Disparate Suburbs",ylab="Price", ylim = c(0,2e6),cex.axis = 0.5)

dev.off()

pdf("Unit_Top5Gini_Boxplot.pdf")
boxplot(price~suburb,data=ut5df, main="Unit Price per Suburb",xlab="Top 5 Disparate Suburbs",ylab="Price",ylim = c(0,10e6),cex.axis = 0.5)
dev.off()

pdf("Unit_Bot5Gini_Boxplot.pdf",5,5)
boxplot(price~suburb,data=ub5df, main="Unit Price per Suburb",xlab="Bot 5 Disparate Suburbs",ylab="Price", ylim = c(0,2e6),cex.axis = 0.5)
dev.off()

#quintiles per year

require(plyr) ## need this to do a hack to allow for cbind.fill
cbind.fill <- function(...) {                                                                                                                                                       
  trans <- lapply(list(...),t)                                                                                                                                                 
  trans_dataframe <- lapply(trans, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(trans_dataframe))))                                                                                                                          
} 


# function which returns data frame, uncleaned, on top10%, median, and bot 10% per year
quintyear <- function(propertysubtus,keeplist) {
quintyearlist <- list('vector')
keepquint <- list('vector')
for (i in 1:5) {
 quintyearlist[[i]] <- group_by(propertysubtus, suburb) %>%
    filter(year==yearindex[i]) %>%
    summarise(nq_entries = n(), quant10 = quantile(price,c(1,9)/10)[1], medianprice = median(price), quant90 = quantile(price,c(1,9)/10)[2])
keepquint[[i]] <- quintyearlist[[i]][quintyearlist[[i]]$suburb %in% keeplist,]
}
keepquint <- Reduce(cbind.fill,keepquint)
keepquint
}

htopquintyear <- quintyear(housesubtus,hkeep_top5)
hbotquintyear <- quintyear(housesubtus,hkeep_bot5)

utopquintyear <- quintyear(unitsubtus,ukeep_top5)
ubotquintyear <- quintyear(unitsubtus,ukeep_bot5)

low10quint_index <- c(1,3,8,13,18,23)
high10quint_index <- c(1,5,10,15,20,25)
med_index <- c(1,4,9,14,19,24)

hbotquintsumm <- hbotquintyear[,c(low10quint_index,med_index,high10quint_index)]

plotquints <- function(quintyear) {  ## this function not working, I just manually iterate through it at the moment
  
  low10quint_index <- c(1,3,8,13,18,23)
  high10quint_index <- c(1,5,10,15,20,25)
  med_index <- c(1,4,9,14,19,24)
  decilechange <- vector()
for (i in 1:5) {
  low10 <- data.frame(yearindex,as.numeric(t(quintyear[i,low10quint_index][2:6])))
  names(low10) <- c("Year","Low10")
  med <- data.frame(yearindex,as.numeric(t(quintyear[i,med_index][2:6])))
  names(med) <- c("Year","Median")
  high10 <- data.frame(yearindex,as.numeric(t(quintyear[i,high10quint_index][2:6])))
  names(high10) <- c("Year","High10")
  dataforplot <- cbind(low10,med,high10)[,c(1,2,4,6)]
  matdat <- as.matrix(dataforplot)
  decilefilename <- paste(quintyear[i,1],"_DecilePlot.pdf",sep="")
  decilefilename
  pdf(decilefilename,5,5)
  print(ggplot(data = dataforplot, aes(x=Year)) + 
    geom_line(aes(color="gray",y=Low10),linetype=2) +
    geom_line(aes(color="red",y=Median)) +
    geom_line(aes(color="gray",y=High10),linetype=2))
  dev.off()
  barfilename <- paste(quintyear[i,1],"_BasicBar.pdf",sep="")
  pdf(barfilename,5,5)
  print(
    barplot(matdat[c(1,5),c(2,4)], legend = c("2012","2016"), beside=TRUE)
  )
  dev.off()
  csvfilename <- paste(quintyear[i,1],"_RawDat.csv",sep = "")
  write.csv(dataforplot,csvfilename)
  ratiodecilechange <- matdat[,4]/matdat[,2]
  decilechange[i] <- ratiodecilechange[5]/ratiodecilechange[1]
}
decilechange
}

##  Bar Plots as alternate quantile representation:
mdat <- melt(dataforplot)
ggplot(data = mdat, aes(x=Year)) + 
  geom_bar(aes(fill= variable,y=Low10),position="dodge", stat="identity") +
  geom_bar(aes(fill= variable,y=High10),position="dodge", stat="identity")

matdat <- as.matrix(dataforplot)
barplot(matdat[c(1,5),c(2,4)], legend = c("L10","med","H10"), beside=TRUE)



## 
## gini per year of top 5, bot 5, and top 5 movers up and down

housetop5gini <- sort3.hgby[sort3.hgby$suburb %in% topgradsubs,1:6]
housebot5gini <- sort3.hgby[sort3.hgby$suburb %in% botgradsubs,1:6]

unittop5gini <- sort3.ugby[sort3.ugby$suburb %in% topunitgradsubs,1:6]
unitbot5gini <- sort3.ugby[sort3.ugby$suburb %in% botunitgradsubs,1:6]

plotgini <- function(top5gini,filenamestring) {
  tempmelt <- melt(top5gini)[1:25,]
  pdf(paste(filenamestring,"_Gini_vs_Year_Plot.pdf",sep=""),10,10)
  print(
    ggplot(data = tempmelt) + geom_line(aes(x=variable,y=value), group=1) + facet_wrap(~suburb)
         )
  dev.off()
}

## Quantiles for most changed

h_trend_bot5gini <- quintyear(housesubtus,housebot5gini$suburb)
u_trend_bot5gini <- quintyear(unitsubtus,unitbot5gini$suburb)

cumquant <- function(typesubtus,keepsuburb) {
cumquantmat <- data.frame()
ratiochange <- data.frame()
for (suburbcheck in keepsuburb) {  ## loop not complete currently
  subofintA <- group_by(typesubtus,suburb) %>%
    filter(suburb == suburbcheck, year==2012) 
  quantA <- summarise(subofintA,nqu_entries = n(), quant10 = quantile(price,c(1,9)/10)[1], medianprice = median(price), quant90 = quantile(price,c(1,9)/10)[2])
  subofintB <- group_by(typesubtus,suburb) %>%
    filter(suburb == suburbcheck, year==2016) 
  quantB <- summarise(subofintB, nqu_entries = n(), quant10 = quantile(price,c(1,9)/10)[1], medianprice = median(price), quant90 = quantile(price,c(1,9)/10)[2])
  cumsubindex <- as.character(suburbcheck)
  cumquantmat[cumsubindex,1] <- sum(subofintA[which(subofintA$price<quantA$quant10),]$price)/(quantA$nqu_entries/10) ## mean, /10 because decile
  cumquantmat[cumsubindex,2] <- sum(subofintA[which(subofintA$price>quantA$quant90),]$price)/(quantA$nqu_entries/10)
  cumquantmat[cumsubindex,3] <- sum(subofintB[which(subofintB$price<quantB$quant10),]$price)/(quantB$nqu_entries/10)
  cumquantmat[cumsubindex,4] <- sum(subofintB[which(subofintB$price>quantB$quant90),]$price)/(quantB$nqu_entries/10)
  ratiochange[cumsubindex,1] <- (cumquantmat[cumsubindex,4]/cumquantmat[cumsubindex,3]) / (cumquantmat[cumsubindex,2]/cumquantmat[cumsubindex,1])
}
cumquantmat
}

cqm <- cumquant(housesubtus,housetop5gini$suburb)
subsums <- colSums(cqm)
(subsums[4]/subsums[3]) / (subsums[2]/subsums[1])

cqm <- cumquant(unitsubtus,unitbot5gini$suburb)
subsums <- colSums(cqm)
(subsums[4]/subsums[3]) / (subsums[2]/subsums[1])

cqm <- cumquant(housesubtus,housebot5gini$suburb)
subsums <- colSums(cqm)
(subsums[4]/subsums[3]) / (subsums[2]/subsums[1])


cqm <- as.matrix(cumquant(housesubtus,housetop5gini$suburb))
colnames(cqm) <- c("2012bot10","2012top10","2016bot10","2016top10")
barplot(cqm,beside=TRUE,legend=rownames(cqm))

### number of houses above a certain amount (to put counts in yarn)

numberabovebelow <- function(housedf,targetsuburb,targetabove,targetbelow) {
  housesabove2012 <- group_by(housedf,suburb) %>%
    filter(year == 2012, suburb == targetsuburb, price >targetabove) 
  housesbelow2012 <- group_by(housedf,suburb) %>%
    filter(year == 2012, suburb == targetsuburb, price < targetbelow)
  housesabove2016 <- group_by(housedf,suburb) %>%
    filter(year == 2016, suburb == targetsuburb, price >targetabove)
  housesbelow2016 <- group_by(housedf,suburb) %>%
    filter(year == 2016, suburb == targetsuburb, price < targetbelow) 
     numbers <- c(dim(housesabove2012)[1],dim(housesbelow2012)[1],dim(housesabove2016)[1],dim(housesbelow2016)[1])
 numbers
}

numberabovebelowall <- function(housedf,targetsuburb,targetabove,targetbelow,targetyear) {
  housesaboveall <- group_by(housedf,suburb) %>% 
    filter(year %in% targetyear, suburb == targetsuburb, price > targetabove)
  housesbelowall <- group_by(housedf,suburb) %>% 
    filter(year %in% targetyear, suburb == targetsuburb, price < targetbelow)
  numbers <- c(dim(housesaboveall)[1],dim(housesbelowall)[1])
  numbers
}

numberabovetotal <- function(housedf,targetsuburb,targetabove) {
  housesabove <- group_by(housedf,suburb) %>%
    filter(suburb == targetsuburb, price >targetabove)
  housesabovenum <- dim(housesabove)[1]
  housesabovenum
}

meanquant <- function(housedf,targetsuburbs,targetyear,percentile) {
  meanquant <- vector()
  houses <- group_by(housedf,suburb) %>%
    filter(year %in% targetyear, suburb %in% targetsuburbs)
    # summarise(quant = quantile(price,c(percentile)/100)) 
  decile <- quantile(houses$price,c(percentile)/100)
  meanquant[1] <- mean(as.numeric(unlist(houses[which(houses$price<decile),"price"])))
  meanquant[2] <- mean(as.numeric(unlist(houses[which(houses$price>decile),"price"])))
  meanquant
}

## ratio change between top 10% and bottom 10% between 2012 and 2016.

ratiochange <- function(housetypedf, targetsuburbs) {
  earlyyr <- vector()
  lateyr <- vector()
  earlyyr[1] <- meanquant(housetypedf,targetsuburbs,2012,10)[1]
  earlyyr[2] <- meanquant(housetypedf,targetsuburbs,2012,90)[2]
  lateyr[1] <- meanquant(housetypedf,targetsuburbs,2016,10)[1]
  lateyr[2] <- meanquant(housetypedf,targetsuburbs,2016,90)[2]
  ratio <- (lateyr[2]/lateyr[1])/(earlyyr[2]/earlyyr[1])
  ratio
}

## expensive houses only:
expensive_gini <- filter(house_gini_sub, mprice>2e6) %>%
  arrange(gini)

expsubs <- expensive_gini$suburb

expsubmat <- cumquant(housesubtus,expsubs) 

expsubmat[which(expsubmat$V1 < 1e6),] # q10 2012 mean less than a million

names(expsubmat) <- c("q10_2012","q90_2012","q10_2016","q90_2016")

bargexpsubmat <- expsubmat[which(expsubmat$q10_2012 < 1e6),] # q10 2012 mean less than a million
bargexpsubmat[,5] <- rownames(bargexpsubmat) # making tbl_df so I can use left_join
names(bargexpsubmat)[5] <- "suburb"
bargexpall <- left_join(bargexpsubmat,house_gini_sub)
bargexpall <- left_join(bargexpall,sort3.hgby)



### expensive in specific suburb

expensiveinsub <- function(targetsubtus,targetsuburb,targetabove) {
  expsubtus <- group_by(targetsubtus,suburb) %>%
    filter(suburb == targetsuburb, price >targetabove)
  expsubtus
}

LeppExp <- expensiveinsub(housesubtus,"Leppington",5e6)
write.csv(LeppExp,file = "LeppExp.csv")

SchofExp <- expensiveinsub(housesubtus,"Schofields",5e6)
write.csv(SchofExp,file = "SchofExp.csv")

EdmExp <- expensiveinsub(housesubtus,"Edmondson Park",5e6)
write.csv(EdmExp,file="EdmExp.csv")

## Overall write to CSV all postcodes with gini.

write.csv(gini_post,file="AllPostcodes_over100sales_Gini.csv")

# 
# filter(year==yearindex[i]) %>%
#   summarise(ny_entries = n(), mprice = mean(price), sp = sd(price), minprice = min(price), maxprice = max(price), giniyear = ineq(price)) %>%
#   filter(ny_entries > 100) ## need the double filter as only filtered previously >100 over all 5 years
