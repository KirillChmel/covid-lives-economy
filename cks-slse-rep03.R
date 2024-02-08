################################################################################
################################################################################
### 3. Replication File - Figures

library(dplyr)
library(ggpubr)
library(ggplot2)
library(Rmisc, include.only = 'summarySE')


### Use CKS-Study1.Rdata to replicate Study 1
### Use CKS-Study2.Rdata to replicate Study 2

study1 <- readRDS('CKS-Study1.Rdata')
study2 <- readRDS('CKS-Study2.Rdata')


################################################################################
### Figure 1 - Group means with 95% error bars (Study 1)

### Standardization
study1$dv_approval <- scale(study1$dv_approval)
study1$dv_support <- scale(study1$dv_support)
study1$dv_voting <- scale(study1$dv_voting)

### Calculate group means and standard errors
dv_approval_se <- summarySE(study1, measurevar="dv_approval", groupvars=c("treatment"), na.rm = T)
dv_approval_se <- cbind(dv_approval_se, 
                        data.frame(t_lives = c("Yes", "No", "Yes", "No")),
                        t_economy = c("No", "Yes", "Yes", "No"))

dv_support_se <- summarySE(study1, measurevar="dv_support", groupvars=c("treatment"), na.rm = T)
dv_support_se <- cbind(dv_support_se,
                       data.frame(t_lives = c("Yes", "No", "Yes", "No")),
                       t_economy = c("No", "Yes", "Yes", "No"))

dv_voting_se <- summarySE(study1, measurevar="dv_voting", groupvars=c("treatment"), na.rm = T)
dv_voting_se <- cbind(dv_voting_se,
                      data.frame(t_lives = c("Yes", "No", "Yes", "No")),
                      t_economy = c("No", "Yes", "Yes", "No"))

### Generate Figure 1
pd <- position_dodge(0.1)

### Figure 1.a
figure_1_a <- ggplot(dv_approval_se, 
              aes(x = t_economy, y = dv_approval, color = t_lives)) +
  geom_errorbar(aes(ymin = dv_approval - se, ymax = dv_approval + se), 
                width = .1, position = pd) +
  geom_point(position = pd) + 
  theme_bw() +
  xlab("Saving the Economy") +
  ylab("Approval of the Incumbent’s Policy") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray"), 
        legend.background = element_rect(size = 0.2, linetype = "solid", colour = "black")) +
  guides(colour = guide_legend(title = "Saving Lives")) + 
  scale_color_grey() +
  ylim(-0.3, 0.3)


### Figure 1.b
figure_1_b <- ggplot(dv_support_se,
                     aes(x = t_economy, y = dv_support, color = t_lives)) +
  geom_errorbar(aes(ymin = dv_support - se, ymax = dv_support + se), 
                width = .1, position = pd) +
  geom_point(position = pd) + 
  theme_bw() +
  xlab("Saving the Economy") +
  ylab("Support for the Extension of Powers")  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray"), 
        legend.background = element_rect(size = 0.2, linetype = "solid", colour = "black")) +
  guides(colour = guide_legend(title= "Saving Lives")) + 
  scale_color_grey() + 
  ylim(-0.3, 0.3)


### Figure 1.c
figure_1_c <- ggplot(dv_voting_se,
                     aes(x = t_economy, y = dv_voting, color = t_lives)) +
  geom_errorbar(aes(ymin = dv_voting - se, ymax = dv_voting + se),
                width = .1, position = pd) +
  geom_point(position = pd) + theme_bw() +
  xlab("Saving the Economy") +
  ylab("Voting Intention")  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray"), 
        legend.background = element_rect(size = 0.2, linetype = "solid", colour = "black")) +
  guides(colour=guide_legend(title="Saving Lives")) +
  scale_color_grey() +
  ylim(-0.3, 0.3)


### Figure 1
figure_1 <- ggarrange(figure_1_a, figure_1_b, figure_1_c,
                      labels = c("1a", "1b", "1c"),
                      ncol = 3, nrow = 1, common.legend = TRUE, legend="top")



################################################################################
### Figure 2 - Group means with 95% error bars (Study 2)

study2$dv_approval <- scale(study2$dv_approval)
study2$dv_support <- scale(study2$dv_support)
study2$dv_voting <- scale(study2$dv_voting)

### Calculate group means and standard errors
dv_approval_se <- summarySE(study2, measurevar="dv_approval", groupvars=c("treatment"), na.rm = T)
dv_approval_se <- cbind(dv_approval_se, 
                        data.frame(t_lives = c("Yes", "No", "Yes", "No")),
                        t_economy = c("No", "Yes", "Yes", "No"))

dv_support_se <- summarySE(study2, measurevar="dv_support", groupvars=c("treatment"), na.rm = T)
dv_support_se <- cbind(dv_support_se,
                       data.frame(t_lives = c("Yes", "No", "Yes", "No")),
                       t_economy = c("No", "Yes", "Yes", "No"))

dv_voting_se <- summarySE(study2, measurevar="dv_voting", groupvars=c("treatment"), na.rm = T)
dv_voting_se <- cbind(dv_voting_se,
                      data.frame(t_lives = c("Yes", "No", "Yes", "No")),
                      t_economy = c("No", "Yes", "Yes", "No"))

### Generate Figure 2
pd <- position_dodge(0.1)

### Figure 2.a
figure_2_a <- ggplot(dv_approval_se, 
                     aes(x = t_economy, y = dv_approval, color = t_lives)) +
  geom_errorbar(aes(ymin = dv_approval - se, ymax = dv_approval + se), 
                width = .1, position = pd) +
  geom_point(position = pd) + 
  theme_bw() +
  xlab("Saving the Economy") +
  ylab("Approval of the Incumbent’s Policy") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray"), 
        legend.background = element_rect(size = 0.2, linetype = "solid", colour = "black")) +
  guides(colour = guide_legend(title = "Saving Lives")) + 
  scale_color_grey() +
  ylim(-0.5, 0.5)


### Figure 2.b
figure_2_b <- ggplot(dv_support_se,
                     aes(x = t_economy, y = dv_support, color = t_lives)) +
  geom_errorbar(aes(ymin = dv_support - se, ymax = dv_support + se), 
                width = .1, position = pd) +
  geom_point(position = pd) + 
  theme_bw() +
  xlab("Saving the Economy") +
  ylab("Support for the Extension of Powers")  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray"), 
        legend.background = element_rect(size = 0.2, linetype = "solid", colour = "black")) +
  guides(colour = guide_legend(title= "Saving Lives")) + 
  scale_color_grey() + 
  ylim(-0.5, 0.5)


### Figure 2.c
figure_2_c <- ggplot(dv_voting_se,
                     aes(x = t_economy, y = dv_voting, color = t_lives)) +
  geom_errorbar(aes(ymin = dv_voting - se, ymax = dv_voting + se),
                width = .1, position = pd) +
  geom_point(position = pd) + theme_bw() +
  xlab("Saving the Economy") +
  ylab("Voting Intention")  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "gray"), 
        legend.background = element_rect(size = 0.2, linetype = "solid", colour = "black")) +
  guides(colour=guide_legend(title="Saving Lives")) +
  scale_color_grey() +
  ylim(-0.5, 0.5)


### Figure 2
figure_2 <- ggarrange(figure_2_a, figure_2_b, figure_2_c,
                      labels = c("2a", "2b", "2c"),
                      ncol = 3, nrow = 1, common.legend = TRUE, legend="top")
