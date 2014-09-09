setwd("C:/Users/mcast_000/Documents/Projects/Code for America")
sf_earnings_tract <- read.csv("~/Projects/Code for America/sf_earnings_tract.csv")
sf_crimes_tract <- read.csv("~/Projects/Code for America/sf_crimes_tract.csv")

sf_all_tract <- merge(sf_earnings_tract, sf_crimes_tract, all=T)
sf_all_tract[is.na(sf_all_tract)] <- 0
colnames(sf_all_tract) <- tolower(colnames(sf_all_tract))
attach(sf_all_tract)
rownames(sf_all_tract) <- paste(neighborhood, tract, sep=" - ")
detach(sf_all_tract)
colnames(sf_all_tract)[c(4,6)] <- c("median.earnings", "alcohol.dui")

library(dplyr)
sf_all_tract <- filter(sf_all_tract, total.population>0)
sf_all_tract <- transform(sf_all_tract,
						  arson.per.capita = arson/total.population,
						  dui.per.capita = alcohol.dui/total.population,
						  robbery.per.capita = robbery/total.population)

attach(sf_all_tract)

library(ggplot2)

arson_earn_lm <- lm(arson.per.capita ~ median.earnings, weights=total.population)
summary(arson_earn_lm)
plot(arson.per.capita ~ median.earnings)
abline(arson_earn_lm)

alc_earn_lm <- lm(dui.per.capita ~ median.earnings, weights=total.population)
summary(alc_earn_lm)
plot(dui.per.capita ~ median.earnings)
abline(reg=alc_earn_lm)

rob_earn_lm <- lm(robbery.per.capita ~ median.earnings, weights=total.population)
summary(rob_earn_lm)c
plot(robbery.per.capita ~ median.earnings)
abline(reg=rob_earn_lm)

ggplot(data=sf_all_tract, aes(x=median.earnings, y=dui.per.capita)) +
geom_point(aes(size=total.population, colour=arson.per.capita)) +
scale_colour_continuous(low="#EDA066", high="#2F3BBF") +
geom_abline(aes(intercept=alc_earn_lm$coefficients[1],slope=alc_earn_lm$coefficients[2]))

plot(dui.per.capita, median.earnings, arson.per.capita)