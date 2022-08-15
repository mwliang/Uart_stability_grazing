# load packages
library(ggplot2)
#####################################################################################
#####################################################################################
my_theme <- theme(legend.position = "none",
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(colour = "black", size = 0.5),
                  panel.border = element_blank(),
                  axis.text = element_text(size = 14, family = "Arial", color = "black"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 16, family = "Arial", color = "black"),
                  axis.ticks.length = unit(1, "mm"),
                  axis.line = element_line(color = "black", size = 0.5),
                  strip.text = element_text(size = 16, family = "Arial", color = "black"),
                  plot.tag = element_text(size = 20, family = "Arial", color = "black"))
#####################################################################################
#####################################################################################
scaleFUN <- function(x) sprintf("%.1f", x)
#####################################################################################
#####################################################################################
# load data
all_effects.data <- read.csv('all_effects_Uart.csv',header=T)
variable.names(all_effects.data)

# the effects of dominant
all_d.data <- all_effects.data[grep("Dominants abundance", all_effects.data$effects),]
all_d.data$variable <- factor(all_d.data$variable, 
                              levels=c("dominants_sta", "spe_asy","spe_sta",
                                       "gamma_sta","spa_asy","alpha_sta",
                                       "gammaD2", "betaD2","alphaD2"))

dis_d.plot <- ggplot(all_d.data , aes(x = variable, y = slope)) +
  geom_hline(yintercept=0, linetype="dashed", size=0.5, colour = "gray") +
  geom_errorbar(aes(ymin = slope - se*1.96, ymax = slope + se*1.96), size = 1, width= 0,alpha=0.3) +
  geom_errorbar(aes(ymin = slope - se, ymax = slope + se), size = 1.5, width= 0,alpha=0.5) +
  geom_point(fill="white", shape=21, size = 3.5) +
  #scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB")) +
  facet_grid(.~biome_types) +
  coord_flip(ylim = c(-2,2)) +
  ylab(bquote(atop(paste("Effect sizes"), (NULL[paste("regression coefficients")])))) +
  xlab("") +
  labs(tag = "A") +
  theme_bw() +
  my_theme

# the effects of diversity
all_b.data <- all_effects.data[!all_effects.data$effects%in%c("Dominants abundance"),]
all_b.data$variable <- factor(all_b.data$variable, 
                              levels=c("gamma_sta","spa_asy","alpha_sta", "spe_asy","spe_sta"))

dis_b.plot <- ggplot(all_b.data , aes(x = variable, y = slope)) +
  geom_hline(yintercept=0, linetype="dashed", size=0.5, colour = "gray") +
  geom_errorbar(aes(ymin = slope - se*1.96, ymax = slope + se*1.96), size = 1, width= 0,alpha=0.3) +
  geom_errorbar(aes(ymin = slope - se, ymax = slope + se), size = 1.5, width= 0,alpha=0.5) +
  geom_point(fill="white", shape=21, size = 3.5) +
  facet_grid(.~biome_types) +
  coord_flip(ylim = c(-2,2)) +
  ylab(bquote(atop(paste("Effect sizes"), (NULL[paste("regression coefficients")])))) +
  xlab("") +
  labs(tag = "B") +
  theme_bw() +
  my_theme
