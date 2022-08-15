#####################################################################################
#####################################################################################
# load data
Uart_div.data <- read.csv('Uart_div.csv',header=T)
variable.names(Uart_div.data)

Uart_div.data$biome_types <- factor(Uart_div.data$biome_types, levels=c("Grasslands","Shrublands"))
Uart_div.data$year <- factor(Uart_div.data$year, levels=c("2017","2018","2019","2020","2021"))
Uart_div.data$grazing <- factor(Uart_div.data$grazing, levels=c("NG","MG","HG"))

# Table 1 three-way ANOVA
summary(aov(alphaD2 ~ grazing*biome_types*year, data = Uart_div.data))
summary(aov(betaD2 ~ grazing*biome_types*year, data = Uart_div.data))
summary(aov(gammaD2 ~ grazing*biome_types*year, data = Uart_div.data))

summary(aov(biomass ~ grazing*biome_types*year, data = Uart_div.data))
summary(aov(dominants_biomass ~ grazing*biome_types*year, data = Uart_div.data))
summary(aov(dominants_abundance ~ grazing*biome_types*year, data = Uart_div.data))

# Table S1 three-way ANOVA
summary(aov(alphaD ~ grazing*biome_types*year, data = Uart_div.data))
summary(aov(betaD ~ grazing*biome_types*year, data = Uart_div.data))
summary(aov(gammaD ~ grazing*biome_types*year, data = Uart_div.data))

#####################################################################################
#####################################################################################
# Load data
Uart_div_sta.data <- read.csv('Uart_div_sta.csv',header=T)
variable.names(Uart_div_sta.data)

Uart_div_sta.data$grazing <- factor(Uart_div_sta.data$grazing, levels=c("NG","MG","HG"))
Uart_div_sta.data$biome_types <- factor(Uart_div_sta.data$biome_types, levels=c("Grasslands","Shrublands"))

# Table S2 two-way ANOVA
summary(aov(spe_sta ~ grazing*biome_types, data = Uart_div_sta.data))
summary(aov(spe_asy ~ grazing*biome_types, data = Uart_div_sta.data))
summary(aov(alpha_sta ~ grazing*biome_types, data = Uart_div_sta.data))
summary(aov(spa_asy ~ grazing*biome_types, data = Uart_div_sta.data))
summary(aov(gamma_sta ~ grazing*biome_types, data = Uart_div_sta.data))
summary(aov(dominants_sta ~ grazing*biome_types, data = Uart_div_sta.data))


