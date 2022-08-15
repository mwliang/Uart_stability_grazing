# load packages
library(nlme)
library(MuMIn)
# load data
Uart_div_sta.data <- read.csv('Uart_div_sta_ln.csv',header=T)
Uart_div_sta.data$grazing <- factor(Uart_div_sta.data$grazing, levels=c("NG","MG","HG"))
# variable.names(Uart_div_sta.data)

#####################################################################################
# the effects of dominant species among biome_types
dd <- Uart_div_sta.data
group <- unique(dd$biome_types); group

all.result <- c()
for (i in 1:length (group)){
  data <- dd[dd$biome_types==group[i], ]
  all.result_1 <- c()
  for (j in 6:17){
    each.depend <- data[ , j]
    each.x <- data[ , 5] # dominants_abun
    grazing <- data[ , 2]
    each.fit <- lme(each.depend ~ each.x, random=list(~1|grazing), data = data)
    each.result <- c(unlist(summary(each.fit)$tTable[1, 1:2]),
                     unlist(summary(each.fit)$tTable[2, 1:5]), 
                     r2 <- r.squaredGLMM(each.fit))
    d <- c(as.character(group[i]), colnames(dd)[j], each.result)
    all.result_1 <- rbind(all.result_1, d)
  }
  all.result<-rbind(all.result,all.result_1)
}
colnames(all.result) <- c("biome_types", "variables", 
                          "intercept", "se_intercept",
                          "slope", "se_slope", "df", "t value", "p",
                          "r2m", "r2c")
#write.csv(all.result, file = "all_d_abun_effects_types.csv")

#####################################################################################
# the effects of alpha diveristy among biome_types
dd <- Uart_div_sta.data
group <- unique(dd$biome_types); group

all.result <- c()
for (i in 1:length (group)){
  data <- dd[dd$biome_types==group[i], ]
  all.result_1 <- c()
  for (j in 13:17){
    each.depend <- data[ , j]
    each.x <- data[ , 10] # alphaD2
    grazing <- data[ , 2]
    each.fit <- lme(each.depend ~ each.x, random=list(~1|grazing), data = data)
    each.result <- c(unlist(summary(each.fit)$tTable[1, 1:2]),
                     unlist(summary(each.fit)$tTable[2, 1:5]), 
                     r2 <- r.squaredGLMM(each.fit))
    d <- c(as.character(group[i]), colnames(dd)[j], each.result)
    all.result_1 <- rbind(all.result_1, d)
  }
  all.result<-rbind(all.result,all.result_1)
}
colnames(all.result) <- c("biome_types", "variables", 
                          "intercept", "se_intercept",
                          "slope", "se_slope", "df", "t value", "p",
                          "r2m", "r2c")
#write.csv(all.result, file = "all_alpha_effects_types.csv")

#####################################################################################
# the effects of beta diveristy among biome_types
dd <- Uart_div_sta.data
group <- unique(dd$biome_types); group

all.result <- c()
for (i in 1:length (group)){
  data <- dd[dd$biome_types==group[i], ]
  all.result_1 <- c()
  for (j in 13:17){
    each.depend <- data[ , j]
    each.x <- data[ , 11] # betaD2
    grazing <- data[ , 2]
    each.fit <- lme(each.depend ~ each.x, random=list(~1|grazing), data = data)
    each.result <- c(unlist(summary(each.fit)$tTable[1, 1:2]),
                     unlist(summary(each.fit)$tTable[2, 1:5]), 
                     r2 <- r.squaredGLMM(each.fit))
    d <- c(as.character(group[i]), colnames(dd)[j], each.result)
    all.result_1 <- rbind(all.result_1, d)
  }
  all.result<-rbind(all.result,all.result_1)
}
colnames(all.result) <- c("biome_types", "variables", 
                          "intercept", "se_intercept",
                          "slope", "se_slope", "df", "t value", "p",
                          "r2m", "r2c")
#write.csv(all.result, file = "all_beta_effects_types.csv")

#####################################################################################
# the effects of gamma diveristy among biome_types
dd <- Uart_div_sta.data
group <- unique(dd$biome_types); group

all.result <- c()
for (i in 1:length (group)){
  data <- dd[dd$biome_types==group[i], ]
  all.result_1 <- c()
  for (j in 13:17){
    each.depend <- data[ , j]
    each.x <- data[ , 12] # gammaD2
    grazing <- data[ , 2]
    each.fit <- lme(each.depend ~ each.x, random=list(~1|grazing), data = data)
    each.result <- c(unlist(summary(each.fit)$tTable[1, 1:2]),
                     unlist(summary(each.fit)$tTable[2, 1:5]), 
                     r2 <- r.squaredGLMM(each.fit))
    d <- c(as.character(group[i]), colnames(dd)[j], each.result)
    all.result_1 <- rbind(all.result_1, d)
  }
  all.result<-rbind(all.result,all.result_1)
}
colnames(all.result) <- c("biome_types", "variables", 
                          "intercept", "se_intercept",
                          "slope", "se_slope", "df", "t value", "p",
                          "r2m", "r2c")
#write.csv(all.result, file = "all_gamma_effects_types.csv")
