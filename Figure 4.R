# load packages
library(piecewiseSEM)

# load data
Uart_sem.data <- read.csv('Uart_div_sta_ln.csv',header=T)
variable.names(Uart_sem.data)

#####################################################################################
# in grasslands
Uart_grassland_sem.data <- Uart_sem.data[grep("Grasslands", Uart_sem.data$biome_types),]
grassland.sem <- psem(
  
  lm(dominants_abun ~ GI, na.action=na.omit, data = Uart_grassland_sem.data),
  
  lm(alphaD2 ~ dominants_abun, na.action=na.omit, data = Uart_grassland_sem.data),
  lm(betaD2 ~ GI + dominants_abun, na.action=na.omit, data = Uart_grassland_sem.data),
  
  lm(spe_sta ~ alphaD2, na.action=na.omit, data = Uart_grassland_sem.data),
  lm(spe_asy ~ GI + dominants_abun + alphaD2, na.action=na.omit, data = Uart_grassland_sem.data),
  lm(spa_asy ~ GI + dominants_abun + betaD2, na.action=na.omit, data = Uart_grassland_sem.data),
  
  lm(alpha_sta ~ spe_sta + spe_asy, na.action=na.omit, data = Uart_grassland_sem.data),
  lm(gamma_sta ~ spa_asy + alpha_sta, na.action=na.omit, data = Uart_grassland_sem.data),
  
  alphaD2 %~~% betaD2,
  
  data = Uart_grassland_sem.data
)
fisherC(grassland.sem)
summary(grassland.sem)
AIC(grassland.sem)
dSep(grassland.sem)

#####################################################################################
# in shrublands
Uart_shrubland_sem.data <- Uart_sem.data[grep("Shrublands", Uart_sem.data$biome_types),]
shrubland.sem <- psem(
  
  lm(dominants_abun ~ GI, na.action=na.omit, data = Uart_shrubland_sem.data),
  
  lm(alphaD2 ~ dominants_abun, na.action=na.omit, data = Uart_shrubland_sem.data),
  lm(betaD2 ~ dominants_abun, na.action=na.omit, data = Uart_shrubland_sem.data),
  
  lm(spe_sta ~ GI + alphaD2, na.action=na.omit, data = Uart_shrubland_sem.data),
  lm(spe_asy ~ alphaD2, na.action=na.omit, data = Uart_shrubland_sem.data),
  lm(spa_asy ~ betaD2 + dominants_abun, na.action=na.omit, data = Uart_shrubland_sem.data),
  
  lm(alpha_sta ~ spe_sta + spe_asy, na.action=na.omit, data = Uart_shrubland_sem.data),
  lm(gamma_sta ~ spa_asy + alpha_sta, na.action=na.omit, data = Uart_shrubland_sem.data),
  
  alphaD2 %~~% betaD2,
  
  data = Uart_shrubland_sem.data
)
fisherC(shrubland.sem)
summary(shrubland.sem)
AIC(shrubland.sem)
dSep(shrubland.sem)