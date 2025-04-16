DansData <- openxlsx::read.xlsx("C:/Users/zcrowley/OneDrive - Southern Cross University/Uni Stuff/Supervision/PhD's/Daniel Sullivan/Stats/DanSullivan/DanSullivanData.xlsx", sheet = "HMA_DATA_LONG_V", na.strings = c("","NA", "#NULL!"))
str(DansData)
DansData$Group <- factor(DansData$Group, levels = c("Control", "Intervention"))
DansData$Time <- factor(DansData$Time, levels = c("Pre", "Post"))

library(lmerTest)
library(lsmeans)
library(emmeans)
library(dplyr)
library(ggplot2)
library(effectsize)
library(pwr)
library(ggdist)

###(SF12MCS)###
#Fit a linear mixed-effects model 
LMEresultSF12MCS <- lmer(SF12MCS~ Time+Group+Time*Group+(1|ParticipantCode),data=DansData)

#Run ANOVA analysis on the linear mixed-effects model and calculate effect sizes (η²).
avcSF12MCS = anova(LMEresultSF12MCS)
avcSF12MCS <- avcSF12MCS[,c(1:3,5,6)]
colnames(avcSF12MCS)<-c("Sum Sq", "Mean sq","DF", "F-value", "p-value")
avceffect_sizesSF12MCS <- eta_squared(LMEresultSF12MCS, partial = TRUE)
avcSF12MCS$`Partial eta^2` <- round(avceffect_sizesSF12MCS$Eta2_partial, 3)

#Prepare a clean, user-friendly table of the model coefficients along with their confidence intervals and p-values
coefficientsSF12MCS <- summary(LMEresultSF12MCS)$coefficients
coefficientsSF12MCS <- data.frame(coefficientsSF12MCS)

confIntSF12MCS <- confint(LMEresultSF12MCS)
confIntSF12MCS <- data.frame(confIntSF12MCS)

confIntSF12MCS$X2.5.. <- round(confIntSF12MCS$X2.5.., 3)
confIntSF12MCS$X97.5.. <- round(confIntSF12MCS$X97.5.., 3)

coefficientsSF12MCS$Pr...t.. <- ifelse(coefficientsSF12MCS$Pr...t.. <= 0.001, "< 0.001", round(coefficientsSF12MCS$Pr...t.., digits = 3))
coefficientsSF12MCS$Estimate <- round(coefficientsSF12MCS$Estimate, 3)

tableSF12MCS <- cbind(coefficientsSF12MCS$Estimate, confIntSF12MCS[-c(1:2),], coefficientsSF12MCS$Pr...t..)
colnames(tableSF12MCS) <- c("Estimate", "lower95%CI", "Upper95%CI", "p-value")

# Perform post-hoc pairwise comparison for the interaction of Time and Group
posthocSF12MCS <- lsmeans::lsmeans(LMEresultSF12MCS, pairwise ~ Time*Group, adjust = 'bonferroni')$contrasts
posthocconintSF12MCS <- confint(posthocSF12MCS)
posthocSF12MCS <- data.frame(posthocSF12MCS)
posthocconintSF12MCS <- data.frame(posthocconintSF12MCS)
posthocSF12MCS <- cbind(posthocSF12MCS, posthocconintSF12MCS[5:6])
colnames(posthocSF12MCS) <- c("Contrast", "Estimate", "SE", "df", "t-ratio", "p-value", "lower CI", "Upper CI")

# Generate emmeans object for contrasts and extract contrasts and calculate Cohen's d manually
emmeans_objectSF12MCS <- emmeans::emmeans(LMEresultSF12MCS, ~ Time*Group)
posthocSF12MCS$CohenD <- with(posthocSF12MCS, round(Estimate / SE, 3))

#Generate plots for assumptions
plot(LMEresultSF12MCS)
Sys.sleep(1)  # Pause for 1 second
qqnorm(residuals(LMEresultSF12MCS))

# Calculate group-level averages and standard deviations
group_avgSF12MCS <- DansData %>%
  group_by(Group, Time) %>%
  summarise(
    Average = mean(SF12MCS, na.rm = TRUE), 
    SD = sd(SF12MCS, na.rm = TRUE),
    SE = SD / sqrt(n()),
    .groups = 'drop'
  )

# Create the spaghetti plot with individual and group-level change indicators
ggplot() +
  geom_line(data = DansData, aes(x = Time, y = SF12MCS, group = ParticipantCode), alpha = 0.3) +
  geom_point(data = DansData, aes(x = Time, y = SF12MCS, group = ParticipantCode), size = 2, shape = 21, fill = "white") +
  geom_line(data = group_avgSF12MCS, aes(x = Time, y = Average, group = Group, color = Group), size = 1.2) +
  geom_point(data = group_avgSF12MCS, aes(x = Time, y = Average, color = Group), size = 3) +
  facet_grid(. ~ Group) +
  labs(title = "Spaghetti plot of outcome over time by treatment group", 
       y = "SF12MCS", 
       x = "Time") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

#Create box plot 
qplot(interaction(Time, Group), SF12MCS, data = DansData, geom = "boxplot") + aes(fill = Group) +
  theme(axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "top",
        legend.title = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")) + 
  scale_fill_brewer(palette ="Paired") +
  labs(title = "Box plot of SF12MCS over time by treatment group", y = "SF12MCS", x = "Time, Treatment group") + guides(fill=guide_legend(title="Treatment Group"))

# Create a raincloud plot
ggplot(DansData, aes(x = interaction(Time, Group), y = SF12MCS, fill = Group)) +
  ggdist::stat_halfeye(
    adjust = 0.5, 
    justification = -0.2, 
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = 0.2, 
    outlier.shape = NA, 
    alpha = 0.5
  ) +
  geom_jitter(
    aes(color = Group), 
    width = 0.1, 
    alpha = 0.5
  ) +
  labs(
    title = "Raincloud Plot of SF12MCS",
    x = "Time and Treatment Group",
    y = "SF12MCS"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create violin plot
ggplot(DansData, aes(x = interaction(Time, Group), y = SF12MCS, fill = Group)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot showing distributions
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9), outlier.shape = NA, alpha = 0.5) +  # Add a boxplot overlay
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +  # Add individual data points
  labs(
    title = "Violin Plot of SF12MCS by Time and Group",
    x = "Time and Group",
    y = "SF12MCS",
    fill = "Group"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create interaction plot
ggplot(group_avgSF12MCS, aes(x = Time, y = Average, color = Group, group = Group)) +
  geom_line(size = 1.2) +  # Line for group means
  geom_point(size = 3) +  # Points for group means
  geom_errorbar(aes(ymin = Average - SE, ymax = Average + SE), width = 0.2) +  # Error bars
  labs(
    title = "Interaction Plot of SF12MCS by Time and Group",
    x = "Time",
    y = "Mean SF12MCS",
    color = "Group"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create bar chart
ggplot(group_avgSF12MCS, aes(x = interaction(Time, Group), y = Average, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +  # Bars for means
  geom_errorbar(
    aes(ymin = Average - SE, ymax = Average + SE),
    width = 0.2,
    position = position_dodge(0.9)
  ) +  # Error bars for SE
  labs(
    title = "Bar Chart of SF12MCS with Mean and SE",
    x = "Time and Group",
    y = "Mean SF12MCS",
    fill = "Group"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Print tables
print(avcSF12MCS)
print(tableSF12MCS)
print(posthocSF12MCS)
print(group_avgSF12MCS)




###(BREF_PHYS)###
#Fit a linear mixed-effects model 
LMEresultBREF_PHYS <- lmer(BREF_PHYS~ Time+Group+Time*Group+(1|ParticipantCode),data=DansData)

#Run ANOVA analysis on the linear mixed-effects model and calculate effect sizes (η²).
avcBREF_PHYS = anova(LMEresultBREF_PHYS)
avcBREF_PHYS <- avcBREF_PHYS[,c(1:3,5,6)]
colnames(avcBREF_PHYS)<-c("Sum Sq", "Mean sq","DF", "F-value", "p-value")
avceffect_sizesBREF_PHYS <- eta_squared(LMEresultBREF_PHYS, partial = TRUE)
avcBREF_PHYS$`Partial eta^2` <- round(avceffect_sizesBREF_PHYS$Eta2_partial, 3)

#Prepare a clean, user-friendly table of the model coefficients along with their confidence intervals and p-values
coefficientsBREF_PHYS <- summary(LMEresultBREF_PHYS)$coefficients
coefficientsBREF_PHYS <- data.frame(coefficientsBREF_PHYS)

confIntBREF_PHYS <- confint(LMEresultBREF_PHYS)
confIntBREF_PHYS <- data.frame(confIntBREF_PHYS)

confIntBREF_PHYS$X2.5.. <- round(confIntBREF_PHYS$X2.5.., 3)
confIntBREF_PHYS$X97.5.. <- round(confIntBREF_PHYS$X97.5.., 3)

coefficientsBREF_PHYS$Pr...t.. <- ifelse(coefficientsBREF_PHYS$Pr...t.. <= 0.001, "< 0.001", round(coefficientsBREF_PHYS$Pr...t.., digits = 3))
coefficientsBREF_PHYS$Estimate <- round(coefficientsBREF_PHYS$Estimate, 3)

tableBREF_PHYS <- cbind(coefficientsBREF_PHYS$Estimate, confIntBREF_PHYS[-c(1:2),], coefficientsBREF_PHYS$Pr...t..)
colnames(tableBREF_PHYS) <- c("Estimate", "lower95%CI", "Upper95%CI", "p-value")

# Perform post-hoc pairwise comparison for the interaction of Time and Group
posthocBREF_PHYS <- lsmeans::lsmeans(LMEresultBREF_PHYS, pairwise ~ Time*Group, adjust = 'bonferroni')$contrasts
posthocconintBREF_PHYS <- confint(posthocBREF_PHYS)
posthocBREF_PHYS <- data.frame(posthocBREF_PHYS)
posthocconintBREF_PHYS <- data.frame(posthocconintBREF_PHYS)
posthocBREF_PHYS <- cbind(posthocBREF_PHYS, posthocconintBREF_PHYS[5:6])
colnames(posthocBREF_PHYS) <- c("Contrast", "Estimate", "SE", "df", "t-ratio", "p-value", "lower CI", "Upper CI")

# Generate emmeans object for contrasts and extract contrasts and calculate Cohen's d manually
emmeans_objectBREF_PHYS <- emmeans::emmeans(LMEresultBREF_PHYS, ~ Time*Group)
posthocBREF_PHYS$CohenD <- with(posthocBREF_PHYS, round(Estimate / SE, 3))

#Generate plots for assumptions
plot(LMEresultBREF_PHYS)
Sys.sleep(1)  # Pause for 1 second
qqnorm(residuals(LMEresultBREF_PHYS))

# Calculate group-level averages and standard deviations
group_avgBREF_PHYS <- DansData %>%
  group_by(Group, Time) %>%
  summarise(
    Average = mean(BREF_PHYS, na.rm = TRUE), 
    SD = sd(BREF_PHYS, na.rm = TRUE),
    SE = SD / sqrt(n()),
    .groups = 'drop'
  )

# Create the spaghetti plot with individual and group-level change indicators
ggplot() +
  geom_line(data = DansData, aes(x = Time, y = BREF_PHYS, group = ParticipantCode), alpha = 0.3) +
  geom_point(data = DansData, aes(x = Time, y = BREF_PHYS, group = ParticipantCode), size = 2, shape = 21, fill = "white") +
  geom_line(data = group_avgBREF_PHYS, aes(x = Time, y = Average, group = Group, color = Group), size = 1.2) +
  geom_point(data = group_avgBREF_PHYS, aes(x = Time, y = Average, color = Group), size = 3) +
  facet_grid(. ~ Group) +
  labs(title = "Spaghetti plot of outcome over time by treatment group", 
       y = "BREF_PHYS", 
       x = "Time") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

#Create box plot 
qplot(interaction(Time, Group), BREF_PHYS, data = DansData, geom = "boxplot") + aes(fill = Group) +
  theme(axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "top",
        legend.title = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")) + 
  scale_fill_brewer(palette ="Paired") +
  labs(title = "Box plot of BREF_PHYS over time by treatment group", y = "BREF_PHYS", x = "Time, Treatment group") + guides(fill=guide_legend(title="Treatment Group"))

# Create a raincloud plot
ggplot(DansData, aes(x = interaction(Time, Group), y = BREF_PHYS, fill = Group)) +
  ggdist::stat_halfeye(
    adjust = 0.5, 
    justification = -0.2, 
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = 0.2, 
    outlier.shape = NA, 
    alpha = 0.5
  ) +
  geom_jitter(
    aes(color = Group), 
    width = 0.1, 
    alpha = 0.5
  ) +
  labs(
    title = "Raincloud Plot of BREF_PHYS",
    x = "Time and Treatment Group",
    y = "BREF_PHYS"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create violin plot
ggplot(DansData, aes(x = interaction(Time, Group), y = BREF_PHYS, fill = Group)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot showing distributions
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9), outlier.shape = NA, alpha = 0.5) +  # Add a boxplot overlay
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +  # Add individual data points
  labs(
    title = "Violin Plot of BREF_PHYS by Time and Group",
    x = "Time and Group",
    y = "BREF_PHYS",
    fill = "Group"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create interaction plot
ggplot(group_avgBREF_PHYS, aes(x = Time, y = Average, color = Group, group = Group)) +
  geom_line(size = 1.2) +  # Line for group means
  geom_point(size = 3) +  # Points for group means
  geom_errorbar(aes(ymin = Average - SE, ymax = Average + SE), width = 0.2) +  # Error bars
  labs(
    title = "Interaction Plot of BREF_PHYS by Time and Group",
    x = "Time",
    y = "Mean BREF_PHYS",
    color = "Group"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create bar chart
ggplot(group_avgBREF_PHYS, aes(x = interaction(Time, Group), y = Average, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +  # Bars for means
  geom_errorbar(
    aes(ymin = Average - SE, ymax = Average + SE),
    width = 0.2,
    position = position_dodge(0.9)
  ) +  # Error bars for SE
  labs(
    title = "Bar Chart of BREF_PHYS with Mean and SE",
    x = "Time and Group",
    y = "Mean BREF_PHYS",
    fill = "Group"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Print tables
print(avcBREF_PHYS)
print(tableBREF_PHYS)
print(posthocBREF_PHYS)
print(group_avgBREF_PHYS)

