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
lapply(reqPkg,
       library,
       character.only = TRUE)

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
stkImport <- quantmod::getSymbols(stkNms,
                                  from = "2016-03-01",
                                  to = "2021-04-01",
                                  periodicity = "monthly")

stkDates <- seq.Date(from = as.Date("2016-03-01",
                                    format = "%Y-%m-%d"),
                     to = as.Date("2021-03-01",
                                  format = "%Y-%m-%d"),
                     by = "month")

#Must list each stock without quotes for Ad()
stkNmsLs <- list(LNVGY,
                YAMHF,
                COST,
                MUSA,
                JBLU,
                GD,
                RF,
                CSIOY,
                VLO,
                CAG)

stkAdjCls <- purrr::map(stkNmsLs,
                        quantmod::Ad) 


# Part 1 ------------------------------------------------------------------
#Monthly RT

#Define round function for sub-lists
##Must define helper function because there is no round method
lagRt <- function(myList){
  dfMe <- as.data.frame(myList)
  lagMe <- (dfMe - lag(dfMe, n = 1))/dfMe
  roundMe <- round(lagMe, digits = 3)
}

stkAdjClsMthRt <- lapply(stkAdjCls, lagRt)
stkAdjClsMthRt <- lapply(stkAdjClsMthRt, na.omit)
#EDIT Print

#Average monthly return
#Mean only works on a column not a df
#had to convert to data frame then apply to each column
stkAdjClsMthRtAvgMean <- lapply(as.data.frame(stkAdjClsMthRt[1:10]), mean)
#EDIT Print

#Annualized Standard Deviation = Mnth Returns * Sqrt(12)
avgRt <- (as.data.frame(stkAdjClsMthRtAvgMean[1:10])*sqrt(12))
#EDIT Print


# Part 2 ------------------------------------------------------------------
#Adjusted Returns correlation + Cov matrix
stkAdjClsCorr <- cor(as.data.frame(stkAdjClsMthRt)) %>%
  round(., digits = 3)
#EDIT Print


#What does the cov between same asset signify? Why are they different values?
#TODO confirm this is correct
stkAdjClsCov <- cov(stkAdjClsCorr) %>%
  round(., digits = 3)
#EDIT Print


# Part 3 ------------------------------------------------------------------
#Expected portfolio return: COST, RF
#50-50 Allocation #Adjusted closing price * 0.5, sum 


#omit first data point, 60/61 months
costRt <- (stkAdjCls[[3]][[61]] - stkAdjCls[[3]][2])/stkAdjCls[[3]][2]
costRt <- as.data.frame(costRt)
costRt <- unlist(costRt, use.names = FALSE)

rfRt <- (stkAdjCls[[7]][[61]] - stkAdjCls[[7]][2])/stkAdjCls[[7]][2]
rfRt <- as.data.frame(rfRt)
rfRt <- unlist(rfRt, use.names = FALSE)

costRF50 <- (costRt+rfRt)*.5
#Print

#Efficient Frontier 
#COST = A
#RF = B
costRFEff <- as.data.frame(x = seq(0,1,0.1))
colnames(costRFEff)[c(1)] <- "wtA"
costRFEff$COSTA <- (costRFEff$wt*costRt)
costRFEff$RFB <- ((1-costRFEff$wt)*rfRt)
costRFEff$portRt <- (costRFEff$wt*costRt) + ((1-costRFEff$wt)*rfRt)
costRFEff$portSd <- (costRFEff$wtA * sd(costRFEff$COSTA))^2 + ((1 - costRFEff$wtA) * sd(costRFEff$RFB))^2 + (2*(costRFEff$wtA)*(1- costRFEff$wtA)*sd(costRFEff$COSTA)*sd(costRFEff$RFB)* cor(costRFEff$COSTA,costRFEff$RFB))

#Plot Eff Front
#https://medium.com/magnimetrics/optimal-portfolios-and-the-efficient-frontier-2e4ef897716d
costRfPlot <- ggplot(costRFEff, aes(x = portSd, y = portRt)) +
  geom_line(data = costRFEff[6:11,], aes(x = portSd, y = portRt),
             colour = "orange") +
  geom_point(data = costRFEff[6:11,], aes(x = portSd, y = portRt),
            colour = "orange",
            size = 3) +
  geom_line(data = costRFEff[1:6,], aes(x = portSd, y = portRt),
            colour = "blue") +
  geom_point(data = costRFEff[1:5,], aes(x = portSd, y = portRt),
             colour = "blue",
             size = 3) 






















