setwd("C:\\Users\\mcast_000\\Documents\\projects\\Code for America\\discover_sf")

####### 

crime1 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\ROBBERYRUNAWAYSEXOFFENSESFORCIBLESEXOFFENSESNONFORCIBLESTOLENPROPERTY.csv")
crime2 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\NONCRIMINALOTHEROFFENSESPORNOGRAPHYOBSCENEMATPROSTITUTIONRECOVEREDVEHICLE.csv")
crime3 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\KIDNAPPINGLARCENYTHEFTLIQUORLAWSLOITERINGMISSINGPERSON.csv")
crime4 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\EXTORTIONFAMILYOFFENSESFORGERYCOUNTERFEITINGFRAUDGAMBLING.csv")
crime5 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\DISORDERLYCONDUCTDRIVINGUNDERTHEINFLUENCEDRUGNARCOTICDRUNKENNESSEMBEZZLEMENT.csv")
crime6 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\HEROINMARIJUANAMENTALHEALTHSTOLENAUTOTAXI.csv")
crime7 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\AMPHETAMINEATTEMPTBIKECHILDCOCAINE.csv")
crime8 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\WARRANTSWEAPONLAWS.csv")
crime9 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\ARSONASSAULTBADCHECKSBRIBERYBURGLARY.csv")
crime10 <- read.csv("C:\\Users\\mcast_000\\Documents\\Projects\\Code for America\\discover_sf\\SUICIDESUSPICIOUSOCCTRESPASSVANDALISMVEHICLETHEFT.csv")

if (!require(data.table)) install.packages("data.table")
library(data.table)

crime_list <- list(crime1, crime2, crime3, crime4, crime5, crime6, crime7, crime8, crime9, crime10)

crime_list <- lapply(crime_list, function(c) {
	colnames(c) <- tolower(colnames(c))
	data.table(c, key="tractid")
})

crime <- Reduce(merge,crime_list)
dim(crime)

crime <- data.frame(crime)

pcaCrime <- prcomp(crime[,-1], scale=T)

corCrime <- cor(crime[,-1])

if (!require(cluster)) install.packages("cluster")
library(cluster)

cg <- clusGap(scale(crime[,-1]), kmeans, K.max=18, B=72,	
			  nstart=7, iter.max=27, algorithm="Hart")

cg
				
if (!require(mclust)) install.packages("mclust")
library(mclust)
if (!require(kernlab)) install.packages("kernlab")
library(kernlab)

mc <- Mclust(t(scale(crime[,-1])), G=1:18)
crime_clus <- mc$classification
sc <- specc(t(scale(crime[,-1])), centers=6)
sc$sexoffensesnonforcible
km <- kmeans(t(scale(crime[,-1])), centers=6)
km$cluster
kkm <- kkmeans(t(scale(crime[,-1])), centers=7, kernel="vanilladot")
kkm
pca <- kpca(crime[,-1], features=6)

for (c in 1:7) {
	print(c)
	for (k in 1:length(kkm)) {
		if (c==kkm[[k]]) {
			print(names(kkm)[[k]])
		}
	}
}