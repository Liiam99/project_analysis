rm(list = ls())

library(car)
library(ggplot2)
library(jtools)
library(stringr)
library(tidyverse)
library(tools)

source("functions.R")

#------------------------------Data Processing--------------------------------#
# Loads the files of every run.
files <- list.files(path="data/runs", pattern="*csv", recursive=TRUE)
my_data <- lapply(paste("data/runs/", files, sep=""), read.csv)
names(my_data) <- basename(gsub("\\.csv$", "", files))

# Loads the file with all the tree locations in it.
trees <- read.csv("data/trees_list.csv")

# Categorises the runs in lists of 5 with the day as the name.
runs = list()
indices <- seq(1, length(my_data), by=5)
for (i in indices) {
  run_name = names(my_data[i])
  run_day = str_extract(run_name, "[^_]+afternoon$|[^_]+morning$")
  runs[run_day] <- list(my_data[i:(i+4)])
}

# Prepares the data frames for the storage of the quotients and variances.
constancy_clustered <- data.frame(a=double(), b=double(),
                                  prop_1=double(), prop_2=double(),
                                  prop_3=double(), prop_4=double())
constancy_uniform <- data.frame(a=double(), b=double(),
                                prop_1=double(), prop_2=double(),
                                prop_3=double(), prop_4=double())
quotients_clustered <- data.frame(Monday=double(20), Tuesday=double(20),
                                  Wednesday=double(20), Thursday=double(20),
                                  Friday=double(20))
quotients_uniform <- data.frame(Monday=double(20), Tuesday=double(20),
                                Wednesday=double(20), Thursday=double(20),
                                Friday=double(20))

# Loops over the runs and calculates the constancy of that run.
for (i in 1:length(runs)){
  real_distances <- calculate_real_distances(runs[i])
  optimal_distances <- calculate_optimal_distances(runs[i])

  quotients <- calculate_quotients(real_distances, optimal_distances)

  # Thursday morning's participant spent 5 minutes on guiding a stranger.
  if (names(runs[i]) == "thursdaymorning"){
    quotients$time[15] <- quotients$time[15] - 300
  }

  # Creates proportions for the strategies of the collected ribbons.
  quotients$strategy <- as.numeric(sapply(
    quotients$strategy, function(x) as.numeric(gsub("\\D", "", x))))
  strategy_props <- prop.table(table(quotients$strategy))

  constancy_a <- var(quotients$a)
  constancy_b <- var(quotients$b)
  prop_1 <- strategy_props[names(strategy_props) == 1]
  prop_2 <- strategy_props[names(strategy_props) == 2]
  prop_3 <- strategy_props[names(strategy_props) == 3]
  prop_4 <- strategy_props[names(strategy_props) == 4]

  # Tuesday morning's run is missing a ribbon.
  if (names(runs[i]) == "tuesdaymorning"){
    quotients[20,] <- c(NA, NA, NA, NA, NA, NA)
  }

  if (i %% 2 == 0){
    constancy_clustered[i / 2,] <- c(constancy_a, constancy_b,
                                     prop_1, prop_2, prop_3, prop_4)
    quotients_clustered[,i / 2] <- round(quotients$a, digits=2)
  } else{
    constancy_uniform[(i + 1)/2,] <- c(constancy_a, constancy_b,
                                       prop_1, prop_2, prop_3, prop_4)
    quotients_uniform[,(i + 1)/2] <- round(quotients$a, digits=2)
  }
}

# Creates the appendix tables for the quotients.
write.table(quotients_uniform, "results/quotients_uniform.txt", sep=",")
write.table(quotients_clustered, "results/quotients_clustered.txt", sep=",")

# Creates a new data frame to store values of both spatial arrangements.
all_constancy_values <- rbind(constancy_uniform, constancy_clustered)
all_constancy_values$day[1:5] <- seq(1, 5)
all_constancy_values$treatment[1:5] <- "Uniform"
all_constancy_values$treatment[6:10] <- "Clustered"
all_constancy_values$treatment <- factor(all_constancy_values$treatment,
                                         levels=c("Uniform", "Clustered"))



#----------------------------------Analysis-----------------------------------#
model_1 <- lm(a ~ day, data=all_constancy_values)
summary(model_1)

model_2 <- lm(a ~ treatment, data=all_constancy_values)
summary(model_2)

model_3 <- lm(a ~ day + treatment, data=all_constancy_values)
summary(model_3)

model_4 <- lm(a ~ day * treatment, data=all_constancy_values)
summary(model_4)



#----------------------Preparations for the Visualisation---------------------#
all_quotients <- rbind(quotients_uniform, quotients_clustered)
all_quotients$arrangement <- factor(c(rep("Uniform", 20), rep("Clustered", 20)),
                                    levels=c("Uniform", "Clustered"))
all_quotients <- gather(all_quotients, day, quotient,
                        Monday, Tuesday, Wednesday, Thursday, Friday)

workweek = unique(all_quotients$day)
all_quotients$day <- factor(all_quotients$day, levels=workweek)

# Creates the proportions of the four different ribbon strategies as percentages
# for the collected ribbons in the uniform runs.
props_uniform <- constancy_uniform
props_uniform[1:2] <- NULL
props_uniform <- gather(props_uniform, key=prop_type, value=prop)
props_uniform$day <- rep(seq(1, 5))
props_uniform$prop <- props_uniform$prop * 100

# Creates the proportions of the four different ribbon strategies as percentages
# for the collected ribbons in the clustered runs.
props_clustered <- constancy_clustered
props_clustered[1:2] <- NULL
props_clustered <- gather(props_clustered, key=prop_type, value=prop)
props_clustered$day <- rep(seq(1, 5))
props_clustered$prop <- props_clustered$prop * 100



#--------------------------------Visualisation--------------------------------#
quotients_plot <- ggplot(all_quotients, aes(day, quotient, color=arrangement)) +
  geom_point(position=position_dodge(0.5)) +
  labs(x="Day", y="Quotient") +
  scale_color_manual(values=c("#00BFC4", "#F8766D")) +
  theme_apa(x.font.size=14, y.font.size=14) +
  theme(axis.text=element_text(size=12))
print(quotients_plot)
# ggsave("results/quotients_variances_displayed.png")

# Creates the plot where the fitted lines of the interaction model are plotted
# with the observed variances for each spatial arrangement on each day.
interaction_plot <- ggplot(data=all_constancy_values,
                           aes(day, model_4$fitted.values,
                           color=treatment)) +
  geom_point(data=all_constancy_values, aes(day, a), size=3.5) +
  geom_line(size=1) +
  labs(x="Day", y=expression(paste("Variance (", sigma^{2}, ")", sep=""))) +
  scale_x_continuous(labels=workweek) +
  scale_color_manual(values=c("#00BFC4", "#F8766D")) +
  theme_apa(x.font.size=14, y.font.size=14) +
  theme(axis.text=element_text(size=12))
print(interaction_plot)
# ggsave("results/linear_regression_interaction.png")

# Creates the plot for the proportions of the four different ribbon strategies
# of the collected ribbons in the uniform runs.
uniform_props_plot <- ggplot(data=props_uniform, aes(fill=prop_type, day, prop)) +
  geom_bar(position="stack", stat="identity") +
  labs(x="Day", y="Proportion (%)", tag="A") +
  scale_fill_discrete(name="", labels=c('1', "2", "3", "4")) +
  scale_x_continuous(breaks=seq(1, 5), labels=workweek) +
  theme_apa(x.font.size=14, y.font.size=14) +
  theme(axis.text=element_text(size=12), plot.tag.position=c(0.960, 0.95),
        text=element_text(size = 20))
print(uniform_props_plot)
# ggsave("results/ribbon_strategy_proportions_uniform.png")

# Creates the plot for the proportions of the four different ribbon strategies
# of the collected ribbons in the clustered runs.
clustered_props_plot <- ggplot(data=props_clustered, aes(fill=prop_type, day, prop)) +
  geom_bar(position="stack", stat="identity") +
  labs(x="Day", y="Proportion (%)", tag="B") +
  scale_fill_discrete(name="", labels=c('1', "2", "3", "4")) +
  scale_x_continuous(breaks = seq(1, 5), labels=workweek) +
  theme_apa(x.font.size=14, y.font.size=14) +
  theme(axis.text=element_text(size=12), plot.tag.position = c(0.960, 0.95),
        text = element_text(size = 20))
print(clustered_props_plot)
# ggsave("results/ribbon_strategy_proportions_clustered.png")



#--------------------------------Miscellaneous--------------------------------#
# Removes redundant environment objects.
rm(constancy_clustered)
rm(constancy_uniform)
rm(my_data)
rm(optimal_distances)
rm(quotients)
rm(real_distances)
