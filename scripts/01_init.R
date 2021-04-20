#Init session
reqPkg <- c("tidyverse",
            "colorspace",
            "crosstalk",
            "here",
            "lubridate",
            "plotly",
            "RColorBrewer",
            "readxl",
            "scales",
            "ggthemes",
            "magrittr",
            "rlist",
            "ggcorrplot",
            "reshape2",
            "packrat",
            "rsconnect",
            "quantmod",
            "tidyquant",
            "sos")
lapply(reqPkg, require, character.only = TRUE)

# Import ------------------------------------------------------------------
stkNms <- c("LNVGY",
            "YAMHF",
            "COST",
            "MUSA",
            "JBLU",
            "GD",
            "RF",
            "CSIOY",
            "VLO",
            "CAG")
#NT: Stops at 2021-03-01, is this because 2021-04-01 is not complete?
stkAll <- quantmod::getSymbols(stkNms,from = "2016-03-01",
                                        to = "2021-04-01",
                                        periodicity = "monthly")
stkDates <- seq.Date(from = as.Date("2016-03-01", format = "%Y-%m-%d"), to = as.Date("2021-03-01", format = "%Y-%m-%d"), by = "month")

#Must list each stock without quotes for Ad()
stkNmsNoQt <- list(LNVGY,
                YAMHF,
                COST,
                MUSA,
                JBLU,
                GD,
                RF,
                CSIOY,
                VLO,
                CAG)
adjCloseAll <- purrr::map(stkNmsNoQt, quantmod::Ad) 
adjCloseAll <- do.call(merge, adjCloseAll)
adjCloseAll <- as.data.frame(adjCloseAll)

#Monthly RT
lagRt <- function(x) {
  (x-lag(x, n = 1))/x
}

#Monthly Returns
#NT: Why does [x] call a column with date? Why must it be in c()? Why dont i need to use [,]? I think its because im calling each individual column which is not an m,n object
#Nt:Access xts dates with index
adjCloseAllMthRt <- purrr::map(adjCloseAll[c(1:10)], lagRt)
adjCloseAllMthRt <- as.data.frame(adjCloseAllMthRt)
adjCloseAllMthRt$Date <- stkDates
adjCloseAllMthRt <- adjCloseAllMthRt[c(11, 1:10)]
adjCloseAllMthRt[2:11] <- round(adjCloseAllMthRt[2:11], digits = 2)
adjCloseAllMthRt <- na.omit(adjCloseAllMthRt)

#Format for print 
adjCloseAllMthRtPt <- adjCloseAllMthRt
adjCloseAllMthRtPt[2:11] <- map(adjCloseAllMthRtPt[c(2:11)], label_percent())
#write.csv(adjCloseAllMthRtPt, "adjCloseAllMthRtPt.csv")

#Average monthly return
avgRtAll <- data.frame(Date = as.Date(character()),
                          AveMthRt = as.numeric(character()),
                          AveYrRt = as.numeric(character()),
                          AnnStdDev = as.numeric(character()))
#EDIT STOP HERE
aveMthRt <- purrr::map(adjCloseAllMthRt[2:10], mean)























