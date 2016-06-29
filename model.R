library(readr)
library(readxl)
library(tidyr)
library(dplyr)

# Read in and clean data
export <- read_excel("TW_EX.xlsx")[ , c(1, 7:21)]
names(export) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                   "2009", "2010", "2011", "2012", "2013", "2014", "2015")
export$country <- as.factor(export$country)

gdp <- read_delim("weoreptc2.txt", delim = "\t",
                  col_types = "c____ccccccccccccccc_______", n_max = 382)
for (i in 2:16) {
  gdp[[i]]<- as.numeric(gsub(",", "", gdp[[i]]))
}
gdp_nation <- gdp[seq(1, 381, 2), ]
names(gdp_nation) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                       "2009", "2010", "2011", "2012", "2013", "2014", "2015")
gdp_capita <- gdp[seq(2, 382, 2), ]
names(gdp_capita) <- c("country", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                       "2009", "2010", "2011", "2012", "2013", "2014", "2015")

import <- read_excel("2001-2015 import value.xlsx", col_types = c("text", "text", "numeric"))

# Gather year columns
export <- gather(export, year, export, 2:16)
gdp_nation <- gather(gdp_nation, year, gdp_nation, 2:16)
gdp_capita <- gather(gdp_capita, year, gdp_capita, 2:16)

# Join all four tables
data <- inner_join(export, gdp_nation, by = c("country", "year")) %>%
  inner_join(inner_join(gdp_capita, import, by = c("country", "year")), by = c("country", "year"))

# Handle zero entries
data[which(data$export == 0), ]$export <- 1

# Take logarithm
data$export <- log(data$export)
data$gdp_capita <- log(data$gdp_capita)
data$import <- log(data$import)

data[[1]] <- as.factor(data[[1]])
data[[2]] <- as.factor(data[[2]])

# Method #1: Split by year
train <- data[grepl(paste(2001:2013, collapse = '|'), data$year), ]
test <- data[grepl(paste(2014:2015, collapse = '|'), data$year), ]
lm_year <- lm(export ~ gdp_nation + gdp_capita + import, data = train)

# Method #2: Split randomly into 10-fold
