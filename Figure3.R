install.packages("easypackages")
library("easypackages")
libraries("readxl", "tidyr", "dplyr", "ggplot2")

Supplementary_Data_S5_feedback <- read_excel("Supplementary Data S5.xlsx", 
                                             sheet = "Partial motif count") %>% 
  filter(grepl("D", Motif))
Supplementary_Data_S5_feedforward <- read_excel("Supplementary Data S5.xlsx", 
                                             sheet = "Partial motif count") %>%
  filter(grepl("F", Motif))

Supplementary_Data_S5_feed <- read_excel("Supplementary Data S5.xlsx", 
                                                sheet = "Partial motif count") %>%
  filter(grepl("F|D", Motif))


feedback <- Supplementary_Data_S5_feedback %>% 
  gather(random, count, Random_1:Random_1000, factor_key=TRUE)
feedforward <- Supplementary_Data_S5_feedforward %>% 
  gather(random, count, Random_1:Random_1000, factor_key=TRUE)
feed <- Supplementary_Data_S5_feed %>% 
  gather(random, count, Random_1:Random_1000, factor_key=TRUE)

feedbackorder = c("D001","D003","D002","D004")
feedforwardorder = c("F001","F006","F004","F007", "F003", "F008", "F002", "F005")
feedorder <- c("D001","D003","D002","D004","F001","F006","F004","F007", "F003", "F008", "F002", "F005")

# Box plot of motif count distributions of feedback motifs + overlay of connectome counts
ggplot() +
  geom_boxplot(data=feedback[feedback$Motif %in% feedbackorder,], 
               aes(x=factor(Motif, level=feedbackorder), y=count, fill=Motif), lwd=0.2, alpha=0.7) +
  geom_point(data=feedback[feedback$Motif %in% feedbackorder,], 
             aes(x=factor(Motif, level=feedbackorder), y=Connectome), shape=4, color="red", size = 4, alpha=0.7) +
  theme(legend.position="none", 
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
        ) +
  scale_fill_brewer(palette="Set2")

# Box plot of motif count distributions of feedforward motifs + overlay of connectome counts
ggplot() +
  geom_boxplot(data=feedforward[feedforward$Motif %in% feedforwardorder,],
               aes(x=factor(Motif, level=feedforwardorder), y=count, fill=Motif), lwd=0.2, alpha=0.7) +
  geom_point(data=feedforward[feedforward$Motif %in% feedforwardorder,],
             aes(x=factor(Motif, level=feedforwardorder), y=Connectome), shape=4, color="red", size = 4, alpha=0.7) +
  theme(legend.position="none", 
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  scale_fill_brewer(palette="Set2")

# Box plot of motif count distributions for all feedback and feedforward motifs + overlay of connectome counts
ggplot() +
  geom_boxplot(data=feed[feed$Motif %in% feedorder,], 
               aes(x=factor(Motif, level=feedorder), y=count, fill=Motif), lwd=0.2, alpha=0.7) +
  geom_point(data=feed[feed$Motif %in% feedorder,], 
             aes(x=factor(Motif, level=feedorder), y=Connectome), shape=4, color="red", size = 4, alpha=0.7) +
  geom
  theme(legend.position="none", 
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
  ) +
  scale_fill_brewer(palette="white")

