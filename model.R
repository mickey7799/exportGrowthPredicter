library(readr)
library(readxl)
library(tidyr)
library(dplyr)
#
df <- read_excel("TW_EX.xlsx")[,c(1,7:21)]
names(df) <- c("country","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
df$country <- as.factor(df$country)

#gdp_growth
gdp <- read_delim("weoreptc2.txt",delim ="\t",
                  col_types = 'c____ccccccccccccccc_______', n_max = 382)
for (i in 2:16){
gdp[[i]]<- as.numeric(gsub(",","",gdp[[i]]))
}

gdp1 <- gdp[seq(1,381,2),]
names(gdp1) <- c("country","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")


gdp2 <- gdp[seq(2,382,2),]
names(gdp2) <- c("country","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")


im <- read_excel("2001-2015 import value.xlsx",col_types = c("text","text","numeric"))


df2 <- gather(df,year,export, 2:16)
gdp1_1<- gather(gdp1,year,gdp, 2:16)
gdp2_1<- gather(gdp2,year,gdp_per, 2:16)
im2 <- im
#im2<- gather(im,year,import, 2:6)

f1 <- inner_join(df2,gdp1_1,by=c("country","year"))
f2 <- inner_join(gdp2_1, im2, by=c("country","year"))
f <- inner_join(f1,f2,by=c("country","year"))

f[which(f$export==0),]$export <- 1


f$export <- log(f$export)
f$gdp_per <- log(f$gdp_per)
f$import <- log(f$import)
#f$gdp <- log(f$gdp*1000000000)


f[[1]]<- as.factor(f[[1]])
f[[2]]<- as.factor(f[[2]])

train <- f[f$year=="2001"| f$year=="2002"|f$year=="2003"|f$year=="2004"| f$year=="2005"|f$year=="2006"|f$year=="2007"| f$year=="2008"|f$year=="2009"|f$year=="2010"|f$year=="2011"| f$year=="2012"|f$year=="2013",]
test <- f[f$year=="2014"|f$year=="2015",]
lm <- lm(export~import+gdp+gdp_per,data=train)
