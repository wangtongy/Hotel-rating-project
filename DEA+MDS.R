library(dplyr)
library(readr)
library(rDEA)
library(ggfortify)
library(reshape2)
library(ggplot2)
library(plotly)
library(caret)


#Import Data
WashHotel <- read_csv("~/Documents/Data Analytics/DA401/Data/Washington5-star.csv")
LonHotel <- read_csv("~/Documents/Data Analytics/DA401/Data/London-5star.csv")
BeiHotel <- read_csv("~/Documents/Data Analytics/DA401/Data/Beijing-5star.csv")
ParHotel <- read_csv("~/Documents/Data Analytics/DA401/Data/paris-5star.csv")
DuHotel <- read_csv("~/Documents/Data Analytics/DA401/Data/Dubai-5star.csv")
TokHotel <- read_csv("~/Documents/Data Analytics/DA401/Data/Tokyo-5star.csv")

#Function
fast = function(In, Out, data){
  In <- dea.robust(X=In, Y=Out, model="input", RTS="variable", B=1000)
  In <- (round(In$theta_hat_hat,3))
  In <- data.frame(In)
  rownames(In)<-data$Name
  return (In)
}



#Tokyo
TA <- cbind(TokHotel$Satisfaction)
TB <- cbind(TokHotel$Customervalue)
TAB <- cbind(TokHotel$Satisfaction,TokHotel$Customervalue)
T1 <- cbind(TokHotel$Cleanliness)
T12 <- cbind(TokHotel$Cleanliness, TokHotel$Location)
T13 <- cbind(TokHotel$Cleanliness,TokHotel$Facilities)
T14 <- cbind(TokHotel$Cleanliness,TokHotel$Staff)
T2 <- cbind(TokHotel$Location)
T23 <- cbind(TokHotel$Location,TokHotel$Facilities)
T24 <- cbind(TokHotel$Location,TokHotel$Staff)
T34 <- cbind(TokHotel$Facilities,TokHotel$Staff)
T123 <- cbind(TokHotel$Cleanliness,TokHotel$Location,TokHotel$Facilities)
T124 <- cbind(TokHotel$Cleanliness,TokHotel$Location,TokHotel$Staff)
T234 <- cbind(TokHotel$Location,TokHotel$Facilities,TokHotel$Staff)
T134 <- cbind(TokHotel$Cleanliness,TokHotel$Facilities,TokHotel$Staff)
T1234 <- cbind(TokHotel$Cleanliness,TokHotel$Facilities,TokHotel$Staff,TokHotel$Location)
T3 <- cbind(TokHotel$Facilities)
T4 <- cbind(TokHotel$Staff)

T1A <- fast(T1, TA, TokHotel)
T12A <- fast(T12, TA, TokHotel)
T13A <- fast(T13, TA, TokHotel)
T14A <- fast(T14, TA, TokHotel)
T2A <- fast(T2, TA, TokHotel)
T23A <- fast(T23, TA, TokHotel)
T24A <- fast(T24, TA, TokHotel)
T34A <- fast(T34, TA, TokHotel)
T123A <- fast(T123, TA, TokHotel)
T124A <- fast(T124, TA, TokHotel)
T134A <- fast(T134,TA, TokHotel)
T234A <- fast(T234, TA, TokHotel)
T1234A <- fast(T1234, TA, TokHotel)
T3A <- fast(T3, TA, TokHotel)
T4A <- fast(T4, TA,TokHotel)

T1B <- fast(T1, TB, TokHotel)
T12B <- fast(T12, TB, TokHotel)
T13B <- fast(T13, TB, TokHotel)
T14B <- fast(T14, TB, TokHotel)
T2B<- fast(T2, TB, TokHotel)
T23B <- fast(T23, TB, TokHotel)
T24B <- fast(T24, TB, TokHotel)
T34B <- fast(T34, TB, TokHotel)
T123B <- fast(T123, TB, TokHotel)
T124B <- fast(T124, TB, TokHotel)
T134B <- fast(T134,TB, TokHotel)
T234B <- fast(T234, TB, TokHotel)
T1234B <- fast(T1234, TB, TokHotel)
T3B <- fast(T3, TB, TokHotel)
T4B <- fast(T4, TB,TokHotel)

T1AB <- fast(T1, TAB, TokHotel)
T12AB <- fast(T12, TAB, TokHotel)
T13AB <- fast(T13, TAB, TokHotel)
T14AB <- fast(T14, TAB, TokHotel)
T2AB <- fast(T2, TAB, TokHotel)
T23AB <- fast(T23, TAB, TokHotel)
T24AB <- fast(T24, TAB, TokHotel)
T34AB <- fast(T34, TAB, TokHotel)
T123AB <- fast(T123, TAB, TokHotel)
T124AB <- fast(T124, TAB, TokHotel)
T134AB <- fast(T134,TAB, TokHotel)
T234AB <- fast(T234, TAB, TokHotel)
T1234AB <- fast(T1234, TAB, TokHotel)
T3AB <- fast(T3, TAB, TokHotel)
T4AB <- fast(T4, TAB,TokHotel)

Tokyo_names <- c("T1A","T12A", "T13A","T14A","T2A","T23A","T24A","T3A","T34A","T4A","T123A","T124A","T234A","T134A","T1234A",
                 "T1B","T12B", "T13B","T14B","T2B","T23B","T24B","T3B","T34B","T4B","T123B","T124B","T234B","T134B","T1234B",
                 "T1AB","T12AB", "T13AB","T14AB","T2AB","T23AB","T24AB","T3AB","T34AB","T4AB","T123AB","T124AB","T234AB","T134AB","T1234AB")
Tokyo_DEA <- cbind(T1A,T12A, T13A,T14A,T2A,T23A,T24A,T3A,T34A,T4A,T123A,T124A,T234A,T134A,T1234A,
                   T1B,T12B,T13B,T14B,T2B,T23B,T24B,T3B,T34B,T4B,T123B,T124B,T234B,T134B,T1234B,
                   T1AB,T12AB,T13AB,T14AB,T2AB,T23AB,T24AB,T3AB,T34AB,T4AB,T123AB,T124AB,T234AB,T134AB,T1234AB)
colnames(Tokyo_DEA) = Tokyo_names
rm(T1A,T12A, T13A,T14A,T2A,T23A,T24A,T3A,T34A,T4A,T123A,T124A,T234A,T134A,T1234A,
   T1B,T12B,T13B,T14B,T2B,T23B,T24B,T3B,T34B,T4B,T123B,T124B,T234B,T134B,T1234B,
   T1AB,T12AB,T13AB,T14AB,T2AB,T23AB,T24AB,T3AB,T34AB,T4AB,T123AB,T124AB,T234AB,T134AB,T1234AB)

Tokyo_DEA_AB <- Tokyo_DEA[,c(31:45)]
Tokyo_DEA_AB["sum"] <- NA
for (i in 1:nrow(Tokyo_DEA_AB)){
  Tokyo_DEA_AB[i, "sum"] = sum(Tokyo_DEA_AB[i, 1:15])
}


#PCA:
TokyoPCA = prclibrary(magrittr)
library(ggpubr)omp(Tokyo_DEA_AB,scale. = TRUE)
autoplot(TokyoPCA, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
round(TokyoPCA$rotation[,1:7],2)

pca_var = TokyoPCA$sdev^2
prop_varex <- pca_var/sum(pca_var)
plot(cumsum(prop_varex),type='b',xlab = "Number of components", 
     ylab = "Ratio of variation", main = "Cumulative Variation plot of Tokyo hotels")

library(magrittr)
library(ggpubr)

# Cmpute MDS
mds <- Tokyo_DEA_AB %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(fit, x = "Dim.1", y = "Dim.2", 
          label = rownames(Tokyo_DEA_AB),
          size = 1,
          repel = TRUE)
set.seed(5)
clust <- kmeans(mds, 7)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)

# Plot and color by groups
ggscatter(mds, 
          x = "Dim.1",
          y = "Dim.2", 
          label = rownames(Tokyo_DEA_AB),
          font.label = 7,
          color = "groups",
          palette = "jco",
          size = 2, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

H_TokyoOdaiba <- Tokyo_DEA_AB[10, c(1:15)]
H_TokyoOdaiba <- as.data.frame(t(H_TokyoOdaiba))
Winstin <- Tokyo_DEA_AB[14, c(1:15)]
Winstin <- as.data.frame(t(Winstin))


Win_mds <- Winstin %>%
  dist()%>%
  cmdscale()%>%
  as_tibble()
colnames(Win_mds) <- c("Dim1", "Dim2")
ggscatter(Win_mds, x = "Dim1", y = "Dim2", 
          label = rownames(Winstin),
          size = 1,
          repel = TRUE)

H_mds <- H_TokyoOdaiba %>%
  dist()%>%
  cmdscale()%>%
  as_tibble()
colnames(H_mds) <- c("Dim1", "Dim2")
ggscatter(H_mds, x = "Dim1", y = "Dim2", 
          label = rownames(H_TokyoOdaiba),
          size = 1,
          repel = TRUE)


