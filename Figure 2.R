# load packages
library(ggplot2)
library(ggpubr)
#####################################################################################
#####################################################################################
my_theme <- theme(legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_text(size = 12, family = "Arial", color = "black"),
                  axis.text.y = element_text(size = 16, family = "Arial", color = "black"),
                  axis.title.x = element_blank(),      
                  axis.title.y = element_text(size = 18, family = "Arial", color = "black"),
                  axis.ticks.length = unit(1, "mm"),  
                  axis.line = element_line(color = "black", size = 0.5),
                  plot.tag = element_text(size = 22, family = "Arial", color = "black"))
#####################################################################################
#####################################################################################
scaleFUN <- function(x) sprintf("%.1f", x)
#####################################################################################
#####################################################################################
# load data
Uart_div_sta.data <- read.csv('Uart_div_sta.csv',header=T)
variable.names(Uart_div_sta.data)

Uart_div_sta.data$grazing <- factor(Uart_div_sta.data$grazing, levels=c("NG","MG","HG"))
# data in grasslands
Grass.data <- Uart_div_sta.data[grep("Grasslands", Uart_div_sta.data$biome_types),]

# data in shrublands
Shrub.data <- Uart_div_sta.data[grep("Shrublands", Uart_div_sta.data$biome_types),]
#####################################################################################
#####################################################################################
################################# alpha stability ################################### Figure 2A
summary(aov(alpha_sta ~ grazing*biome_types, data = Uart_div_sta.data))
TukeyHSD(aov(alpha_sta ~ grazing, data = Uart_div_sta.data))

# difference between grasslands and shrublands
summary(aov(alpha_sta ~ biome_types, data = Uart_div_sta.data))

# grazing effects in grasslands
summary(aov(alpha_sta ~ grazing, data = Grass.data))
TukeyHSD(aov(alpha_sta ~ grazing, data = Grass.data))

# grazing effects in grasslands
summary(aov(alpha_sta ~ grazing, data = Shrub.data))
TukeyHSD(aov(alpha_sta ~ grazing, data = Shrub.data))

alpha_sta_box.plot <- ggplot() + 
  geom_boxplot(mapping = aes(x=biome_types, y=alpha_sta, fill=grazing), data = Uart_div_sta.data, 
               width = 0.8, alpha = 0.6) +
  geom_boxplot(coef = 0, outlier.size = 1) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab(bquote(paste(alpha*" stability"))) +
  xlab("Biome types") +
  scale_y_continuous(labels=scaleFUN, limits = c(0.8,5)) +
  labs(tag = "A") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 4.5, label = "**", label.size = 5) +
  theme_bw() +
  my_theme
#####################################################################################
############################### spatial asynchrony ################################## Figure 2B
summary(aov(spa_asy ~ grazing*biome_types, data = Uart_div_sta.data))
TukeyHSD(aov(spa_asy ~ grazing, data = Uart_div_sta.data))

# difference between grasslands and shrublands
summary(aov(spa_asy ~ biome_types, data = Uart_div_sta.data))

# grazing effects in grasslands
summary(aov(spa_asy ~ grazing, data = Grass.data))
TukeyHSD(aov(spa_asy ~ grazing, data = Grass.data))

# grazing effects in grasslands
summary(aov(spa_asy ~ grazing, data = Shrub.data))
TukeyHSD(aov(spa_asy ~ grazing, data = Shrub.data))

spa_asy_box.plot <- ggplot() + 
  geom_boxplot(mapping = aes(x=biome_types, y=spa_asy, fill=grazing), data = Uart_div_sta.data, 
               width = 0.8, alpha = 0.6) +
  geom_boxplot(coef = 0, outlier.size = 1) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab(bquote(atop(paste("Spatial asynchrony"), (NULL[paste(gamma[sta]/alpha[sta])])))) +
  xlab("Biome types") +
  scale_y_continuous(labels=scaleFUN, limits = c(0.8,2.5)) +
  labs(tag = "B") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 2.2, label = "ns", label.size = 4) +
  theme_bw() +
  my_theme

#####################################################################################
################################# gamma stability ################################### Figure 2C
summary(aov(gamma_sta ~ grazing*biome_types, data = Uart_div_sta.data))
TukeyHSD(aov(gamma_sta ~ grazing, data = Uart_div_sta.data))

# difference between grasslands and shrublands
summary(aov(gamma_sta ~ biome_types, data = Uart_div_sta.data))

# grazing effects in grasslands
summary(aov(gamma_sta ~ grazing, data = Grass.data))
TukeyHSD(aov(gamma_sta ~ grazing, data = Grass.data))

# grazing effects in grasslands
summary(aov(gamma_sta ~ grazing, data = Shrub.data))
TukeyHSD(aov(gamma_sta ~ grazing, data = Shrub.data))

gamma_sta_box.plot <- ggplot() + 
  geom_boxplot(mapping = aes(x=biome_types, y=gamma_sta, fill=grazing), data = Uart_div_sta.data, 
               width = 0.8, alpha = 0.6) +
  geom_boxplot(coef = 0, outlier.size = 1) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab(bquote(paste(gamma*" stability"))) +
  xlab("Biome types") +
  scale_y_continuous(labels=scaleFUN, limits = c(1,8)) +
  labs(tag = "C") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 7, label = "*", label.size = 5) +
  theme_bw() +
  my_theme

#####################################################################################
############################## population stability ################################# Figure 2D
summary(aov(spe_sta ~ grazing*biome_types, data = Uart_div_sta.data))
TukeyHSD(aov(spe_sta ~ grazing, data = Uart_div_sta.data))

# difference between grasslands and shrublands
summary(aov(spe_sta ~ biome_types, data = Uart_div_sta.data))

# grazing effects in grasslands
summary(aov(spe_sta ~ grazing, data = Grass.data))
TukeyHSD(aov(spe_sta ~ grazing, data = Grass.data))

# grazing effects in grasslands
summary(aov(spe_sta ~ grazing, data = Shrub.data))
TukeyHSD(aov(spe_sta ~ grazing, data = Shrub.data))

spe_sta_box.plot <- ggplot() + 
  geom_boxplot(mapping = aes(x=biome_types, y=spe_sta, fill=grazing), data = Uart_div_sta.data, 
               width = 0.8, alpha = 0.6) +
  geom_boxplot(coef = 0, outlier.size = 1) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab("Population stability") +
  xlab("Biome types") +
  scale_y_continuous(labels=scaleFUN, limits = c(0.7,1.7)) +
  labs(tag = "D") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 1.6, label = "ns", label.size = 4) +
  theme_bw() +
  my_theme

#####################################################################################
############################### species asynchrony ################################## Figure 2E
summary(aov(spe_asy ~ grazing*biome_types, data = Uart_div_sta.data))
TukeyHSD(aov(spe_asy ~ grazing, data = Uart_div_sta.data))

# difference between grasslands and shrublands
summary(aov(spe_asy ~ biome_types, data = Uart_div_sta.data))

# grazing effects in grasslands
summary(aov(spe_asy ~ grazing, data = Grass.data))
TukeyHSD(aov(spe_asy ~ grazing, data = Grass.data))

# grazing effects in grasslands
summary(aov(spe_asy ~ grazing, data = Shrub.data))
TukeyHSD(aov(spe_asy ~ grazing, data = Shrub.data))

spe_asy_box.plot <- ggplot() + 
  geom_boxplot(mapping = aes(x=biome_types, y=spe_asy, fill=grazing), data = Uart_div_sta.data, 
               width = 0.8, alpha = 0.6) +
  geom_boxplot(coef = 0, outlier.size = 1) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab(bquote(atop(paste("Species asynchrony"), (NULL[paste(alpha[sta]/"Pop"[sta])])))) +
  xlab("Biome types") +
  scale_y_continuous(labels=scaleFUN, limits = c(1,4)) +
  labs(tag = "E") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 3.6, label = "*", label.size = 5) +
  theme_bw() +
  my_theme

#####################################################################################
########################## Dominant species stability ############################### Figure 2F
summary(aov(dominants_sta ~ grazing*biome_types, data = Uart_div_sta.data))
TukeyHSD(aov(dominants_sta ~ grazing, data = Uart_div_sta.data))

# difference between grasslands and shrublands
summary(aov(dominants_sta ~ biome_types, data = Uart_div_sta.data))

# grazing effects in grasslands
summary(aov(dominants_sta ~ grazing, data = Grass.data))
TukeyHSD(aov(dominants_sta ~ grazing, data = Grass.data))

# grazing effects in grasslands
summary(aov(dominants_sta ~ grazing, data = Shrub.data))
TukeyHSD(aov(dominants_sta ~ grazing, data = Shrub.data))

dominants_sta_box.plot <- ggplot() + 
  geom_boxplot(mapping = aes(x=biome_types, y=dominants_sta, fill=grazing), data = Uart_div_sta.data, 
               width = 0.8, alpha = 0.6) +
  geom_boxplot(coef = 0, outlier.size = 1) +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab("Dominant species stability") +
  xlab("Biome types") +
  scale_y_continuous(labels=scaleFUN, limits = c(0.7,5.5)) +
  labs(tag = "F") +
  geom_bracket(xmin = 1, xmax = 2, y.position = 5.3, label = "**", label.size = 5) +
  theme_bw() +
  my_theme
