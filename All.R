rm(list = ls())

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(MuMIn)
library("sjPlot")
library(performance)
library(insight)
library(plyr)

#### Load in dataset ####
All_sites <- read.csv("INSERT_FILE_PATH\\All_sites.csv")

##
unique(All_sites$Site)

## No. of plots ##
unique(All_sites$Plot)

## No. of 'Species'
unique(All_sites$Species)

### Remove non-Id species ###
All_sites<- All_sites %>% filter(Species != "long grass" & Species != "oak (seedling)" & Species != "oak (leaf-litter)"
                                 & Species != "Pratensis-like" & Species != "moss (dying?)" & Species != "Opposite-alternates"
                                 & Species != "brain coral" & Species != "fluffy grass" & Species !=  "moss " & Species !=  "leaf litter "
                                 & Species !=  "soil " & Species !=  "wood " & Species !=  "dead grass" & Species !=  "dried grass "
                                 & Species !=  "dry grass "  & Species !=  "mud " & Species !=  "Leaf litter" & Species !=  "Moss"
                                 & Species !=  "Bare Ground" & Species !=  "Dead grass" & Species !=  "Dead twig" & Species !=  "soil"
                                 & Species !=  "organic matter"& Species !=  "rock"& Species !=  "Dead leaves"& Species !=  "Bare ground"
                                 & Species !=  "browsed yellow"& Species !=  "moss"& Species !=  "tiny plant" & Species !=  "poo"& Species !=  "burrow"
                                 & Species !=  "lichen"& Species !=  "sticks"& Species !=  "rocks"& Species !=  "new aster center flwr rosette"
                                 & Species !=  "light purple flower"& Species !=  "grass greens - tall green thin seed"
                                 & Species !=  "un-ided small plant" & Species !=  "moose poop"& Species !=  "onion leaf"& Species !=  "log - fallen tree"
                                 & Species !=  "chickweed" & Species != "tiny plant - small chickweed"& Species != "log"
                                 & Species != "basal_serrated leaf rossette_purple stem"& Species != "grass"& Species != "tongue leaved aster"
                                 & Species != "white flowered brassica" & Species != "broad leafed grass" & Species !=  "new tiny plant"
                                 & Species !=  "woody little spray of leaves" & Species !=  "gray lichen" & Species !=  "thick leaved grass"
                                 & Species !=  "jagged edged rosette"& Species !=  "clover"& Species !=  "dark purple Brassica"& Species !=  "chickweed-5 petaled tiny"
                                 & Species !=  "alternate leaved plant" & Species !=  "little green plant"& Species !=  "poop"& Species !=  "thin grass greens"
                                 & Species !=  "lily w central white stripe" & Species !=  "apiaceae tall white umbel"& Species !=  "little woody sprout"
                                 & Species !=  "very thin bluegreen grass"& Species !=  "very thin leaved Erigeron sp."& Species !=  "green grass"
                                 & Species !=  "light green soldier lichen"& Species !=  "thin leaved grass"& Species !=  "pea"& Species !=  "thin leaved rough plant"
                                 & Species !=  "white flowered lily"& Species !=  "new grass-wheat"& Species !=  "bark"& Species !=  "new heart leaved vine"
                                 & Species !=  "deer poop"& Species !=  "thin tan poop"& Species !=  "small thin leaved grass"& Species !=  "cow poop"
                                 & Species !=  "thinner leaved grass"& Species !=  "white pouf grass flower"& Species !="pale fuzzy many dissected leaf"
                                 & Species !="brassica little purple flowers"& Species !="long thin grass"& Species !="new white flower feather leaf"
                                 & Species !="med leafed grass-wheat?"& Species !="very thin leaved aster"& Species !="thick stalk"
                                 & Species !="very thin green grass"& Species !="bare soil"& Species !="broad-leafed grass"& Species !="rocky mountain daisy aster"
                                 & Species !="long leaved shiny droopy grass"& Species !="rice grass"& Species !="basal rosette of leaves"& Species !="woody sprout"
                                 & Species !="animal droppings"& Species !="dried star plant"& Species !="dried yellow onion leaf?"& Species !="fuzzy little rosette"
                                 & Species !="tiny chickweed"& Species !="deer poo"& Species !="CHECK"& Species !="thin green grass"& Species !="animal hair"
                                 & Species !="teeny tiny plant"& Species !="logs"& Species !=""& Species !="hairy leaves"& Species !="dead sapling"
                                 & Species !="feathers"& Species !="dry grass"& Species !="leaves"& Species !="dead flowers"& Species !="dead leaves"
                                 & Species !="shoot"& Species !="dead stem"& Species !="Mystery plant CHECK"& Species !="roots"& Species !="dead shrub"
                                 & Species !="Poo"& Species !="dead branch "& Species !="Bare ground "& Species !="bare ground "& Species !="dead branches"
                                 & Species !="twigs"& Species !="dead oak branch"& Species !="dead twigs "& Species !="Bird cherry"& Species !="Dead twigs"
                                 & Species !="branches"& Species !="dead brach "& Species !="dead brach "& Species !="bare ground"& Species !="dead branch"
                                 & Species !="dead bark"& Species !="Deadgrass"& Species !="stones"& Species !="twigs"& Species !="dead"& Species !="leaf litter"
                                 & Species !="cow pat"& Species !="tree root"& Species !="feather"& Species !="Sheep poo"& Species !="Twigs"& Species !="Sample 4")

## No. of species
unique(All_sites$Species)

### Function to calculate Simpsons diversity ###
simpsons_diversity <- function(species_counts) {
  n <- sum(species_counts)
  sum_sq <- sum(species_counts * (species_counts - 1))
  D <- sum_sq / (n * (n - 1))
  return(1 - D)  # Simpson's Index = 1 - D
}

#########################
### Simpsons per plot ###
#########################
Metrics <-All_sites %>%
  group_by(Plot, Site ) %>%
  dplyr::summarise(Alpha_Diversity = simpsons_diversity(Cover))


### Gamma Simpsons per site ###
###############################
Gamma_Simpsons <-All_sites %>%
  group_by(Site) %>%
  dplyr::summarise(Gamma_Diversity = simpsons_diversity(Cover))
### Join ### 
Metrics <- Metrics %>%
  left_join(Gamma_Simpsons, by="Site")

### Beta Simpsons ###
Metrics$Beta_Diversity<-Metrics$Gamma_Diversity / Metrics$Alpha_Diversity



#########################
### Richness per plot ###
#########################
Alpha_richness <- All_sites %>%
  group_by(Plot, Site) %>%
  summarise(Alpha_Richness = n_distinct(Species))
### Join ### 
Metrics <- Metrics %>%
  left_join(Alpha_richness, by = c("Site", "Plot"))

### Gamma Richness per site ###
###############################
Gamma_Richness <- All_sites %>%#filter(Species!="leaf litter"|)
  group_by(Site) %>%
  summarise(Gamma_Richness = n_distinct(Species))
### Join ###
Metrics <- Metrics %>%
  left_join(Gamma_Richness, by="Site")

### Beta Richness ###
#####################
Metrics$Beta_Richness<-Metrics$Gamma_Richness / Metrics$Alpha_Richness

###############################################
### Temporal (Ecosystem) stability per plot ###
###############################################

## Remove na ##
filtered_data <- All_sites %>% 
  filter(!is.na(Species) & !is.na(Cover))

# Group by Site, Plot and calculate mean and standard deviation of Cover
grouped_data <- filtered_data %>%
  group_by(Plot, Site) %>%
  dplyr::summarise(
    mean_cover = mean(Cover),
    std_cover = sd(Cover)
  ) %>%
  ungroup()

# Calculate the temporal stability (μ/σ)
Ecosystem_stability <- grouped_data %>%
  mutate(temporal_stability =  mean_cover / std_cover)

### Join ### 
Metrics <- Metrics %>%
  left_join(Ecosystem_stability, by = c("Site", "Plot"))

### Remove sites with inf stability ###
clean_data <- Metrics %>%
  filter(!is.infinite(temporal_stability))

###########################
#### Fit Simpsons LMER ####
###########################
BioDiv_Stab_lmer1<-lmer(temporal_stability ~ Alpha_Diversity + (Alpha_Diversity|Site), data = clean_data, REML = FALSE)
BioDiv_Stab_lmer2<-lmer(temporal_stability ~ Alpha_Diversity + (1|Site), data = clean_data, REML = FALSE)
BioDiv_Stab_lm<-lm(temporal_stability ~ Alpha_Diversity, data = clean_data)

AIC(BioDiv_Stab_lmer1, BioDiv_Stab_lmer2, BioDiv_Stab_lm)

clean_data$fit <- predict(BioDiv_Stab_lmer1, clean_data)
clean_data$fit2<- predict(BioDiv_Stab_lmer1, clean_data, re.form=NA)
summary(BioDiv_Stab_lmer1)

r.squaredGLMM(BioDiv_Stab_lmer1)

library(rstanarm)
bays.mod <- stan_lmer(temporal_stability ~ Alpha_Diversity + (Alpha_Diversity|Site), data = clean_data, REML = FALSE)
summary(bays.mod)

tab_model(BioDiv_Stab_lmer1)

### assess ###
# anova(BioDiv_Stab_lmer, BioDiv_Stab_lm)
# 
# rate<-compare_performance(BioDiv_Stab_lmer1, BioDiv_Stab_lmer2)
# rate
# testrate<-test_performance(BioDiv_Stab_lmer1, BioDiv_Stab_lmer2)
# testrate

### Plot LMER ###
ggplot(clean_data,aes(Alpha_Diversity, temporal_stability, col=Site )) + # 
  geom_line(aes(y=fit), linewidth=0.8, alpha=0.5) +
  geom_line(aes(y=fit2), linewidth=1, color ="Black") +
  geom_point(alpha = 0.2) +
  theme_bw() + ylim(0, 2.25) +ylab("Ecosystem stability (μ/σ)") + xlab("Alpha diversity (1 / Simpsons Index)") +
  annotate("text", x= 0.7, y= 2.185, label= "Marginal R2 < 0.01")+
  annotate("text", x= 0.7, y= 2.085, label= "Conditional R2 = 0.377")



#plot(1:10, main=expression(3 %*% 10^-5))
# plotmath help entry
###########################
#### Fit Richness LMER ####
###########################

Rich_Stab_lmer1<-lmer(temporal_stability ~ Alpha_Richness + (Alpha_Richness|Site), data = clean_data)
Rich_Stab_lmer2<-lmer(temporal_stability ~ Alpha_Richness + (1|Site), data = clean_data)
Rich_Stab_lm<-lm(temporal_stability ~ Alpha_Richness, data = clean_data)

AIC(Rich_Stab_lmer1, Rich_Stab_lmer2, Rich_Stab_lm)

clean_data$fit3 <- predict(Rich_Stab_lmer1, clean_data)
clean_data$fit4 <- predict(Rich_Stab_lmer1, clean_data, re.form=NA)
summary(Rich_Stab_lmer1)

r.squaredGLMM(Rich_Stab_lmer1)

tab_model(Rich_Stab_lmer1)

### Plot LMER ###
ggplot(clean_data,aes(Alpha_Richness, temporal_stability, col=Site )) + # 
  geom_line(aes(y=fit3), linewidth=0.8, alpha=0.5) +
  geom_line(aes(y=fit4), linewidth=1, color ="Black") +
  geom_point(alpha = 0.2) +
  theme_bw() + ylim(0, 2.25) +ylab("Ecosystem stability (μ/σ)") + xlab("Alpha Richness") +
  annotate("text", x= 40, y= 2.185, label= "Marginal R2 = 0.095")+
  annotate("text", x= 40, y= 2.085, label= "Conditional R2 = 0.532")
##
### assess ###

bays.mod.2 <- stan_lmer(temporal_stability ~ Alpha_Richness + (Alpha_Richness|Site), data = clean_data, REML = FALSE)
summary(bays.mod.2)

#########################
### Species Asynchrony ###
#########################
g_cover <- All_sites%>%group_by(Site, Plot)%>%summarise(Total.Cover=sum(Cover, na.rm = T), Count=n())

s_cover <- All_sites%>%select(Site, Plot, Species,Cover)

t_cover <- merge(s_cover, g_cover, by=c("Site", "Plot"))
t_cover$non_cover<- t_cover$Total.Cover-t_cover$Cover

t_cover$site_plot <- paste(t_cover$Site, t_cover$Plot)
results_cor <- data.frame(Site=character(), Plot=character(), asyn=numeric())

for(i in unique(t_cover$site_plot)){
  temp <- subset(t_cover, t_cover$site_plot==i)
  temp_result <- data.frame(Site=temp$Site[[1]], Plot=temp$Plot[[1]], Species_Asyn=cor(temp$Cover, temp$non_cover)*(1/temp$Count)*-1)
  results_cor<- rbind(results_cor, temp_result)
}

results_cor<- results_cor%>%distinct()

Metrics <- Metrics %>%
  inner_join(results_cor, by=c("Site", "Plot") )

Spe_Asy_Data <- Metrics %>%
  filter(!is.infinite(temporal_stability))

SpAsynchlmer1<-lmer(temporal_stability ~ Species_Asyn + (Species_Asyn|Site), data = Spe_Asy_Data, REML = FALSE)
SpAsynchlmer2<-lmer(temporal_stability ~ Species_Asyn + (1|Site), data = Spe_Asy_Data, REML = FALSE)
SpAsynchlmer3<-lm(temporal_stability ~ Species_Asyn, data = Spe_Asy_Data)

AIC(SpAsynchlmer1, SpAsynchlmer2, SpAsynchlmer3)

summary(SpAsynchlmer1)
r.squaredGLMM(SpAsynchlmer1)
tab_model(SpAsynchlmer1)

Spe_Asy_Data$fit <- predict(SpAsynchlmer1, Spe_Asy_Data)
Spe_Asy_Data$fit2<- predict(SpAsynchlmer1, Spe_Asy_Data, re.form=NA)

ggplot(Spe_Asy_Data,aes(Species_Asyn, temporal_stability, col=Site )) + # 
  geom_line(aes(y=fit), linewidth=0.8, alpha=0.5) +
  geom_line(aes(y=fit2), linewidth=1, color ="Black") +
  geom_point(alpha = 0.2) +
  theme_bw() + ylim(0, 2.25) +ylab("Ecosystem stability (μ/σ)") + xlab("Species Asyncrhony") +
  annotate("text", x= 0.04, y= 2.05, label= "Marginal R2 = 0.130")+
  annotate("text", x= 0.04, y= 1.95, label= "Conditional R2 = 0.490")


bays.mod.3 <- stan_lmer(temporal_stability ~ Species_Asyn + (Species_Asyn|Site), data = Spe_Asy_Data, REML = FALSE)
summary(bays.mod.3)

############################
### Temporal variability ###
############################

Metrics$Temporal_variability <- Metrics$std_cover/Metrics$mean_cover# std_cover/mean_cover

###########################
### Spatial Variability ###
###########################


results<- data.frame(site=character(),plot.var=numeric(), year.var=numeric())			
for(i in unique(All_sites$Site)){
  temp<- subset(All_sites, All_sites$Site==i)
  # random-effect linear regression
  var.mod <- lmer(Cover~(1|Year)+ (1|Plot:Year), data=temp) 
  # Extract spatial variance (crossed plot term)
  plot.var<-(attr(VarCorr(var.mod)$'Plot:Year', "stddev"))^2
  # Extract temporal variance (year term)
  year.var <- (attr(VarCorr(var.mod)$'Year',"stddev"))^2 
  var.results<-array(NA,c(1,2))
  colnames(var.results)<-c("plot.var","year.var")
  var.results <- data.frame(site=i, plot.var=plot.var[[1]], year.var=year.var[[1]])
  results <- rbind(results, var.results)
}

Temp_Spat_lm<-lm(year.var ~ plot.var, results) 
confint(lm(year.var ~ plot.var, results) )
results$fit <- predict(Temp_Spat_lm, results)

summary(Temp_Spat_lm)
r.squaredGLMM(Temp_Spat_lm)
tab_model(Temp_Spat_lm)

### Plot LMER ###
ggplot(results,aes(plot.var, year.var)) + # 
  #geom_line(aes(y=fit), linewidth=0.8, alpha=0.5) +
  #geom_point(alpha = 0.2,) +
  theme_bw()  +ylab("Temporal (γ) variability") + xlab("Spatial (β) variability") +
  annotate("text", x= 100, y= 460, label= "p<0.05") +
  annotate("text", x= 100, y= 440, label= "Adjusted R2 = 0.917")+
  geom_text(aes(label = site))



# # Extract 95% CIs using ggplot2# # Extract siTRUE# # Extract 95% CIs using ggplot2# # Extract site95% CIs using ggplot2
# library(ggplot2)
# p <- qplot(plot.var, year.var,data=results)+stat_smooth(method = "lm")
# (p.dat <- ggplot_build(p)$data[[2]])
# 








# # Group by Site, Plot, and Year and calculate mean and standard deviation of 'Cover'
# grouped_data_2 <- filtered_data %>%
#   group_by(Year, Plot, Site) %>%
#   dplyr::summarise(
#     mean_biomass = mean(Cover),
#     std_biomass = sd(Cover)
#   ) %>%
#   ungroup()
# 
# # Calculate the spatial Var (σ/μ)
# Spatial_stability <- grouped_data_2 %>%
#   mutate(spatial_stability = std_biomass/mean_biomass)
# 
# mean_spat_stab <- Spatial_stability %>%
#   group_by(Plot, Site) %>%
#   summarise(
#     mean_spatial_var = mean(spatial_stability)
#   ) %>%
#   ungroup()
# 
# ### Join ### 
# Metrics <- Metrics %>%
#   left_join(mean_spat_stab, by = c("Site", "Plot"))


# # Calculate the average biomass (cover) for each plot
# plot_mean_cover <- All_sites %>%
#   group_by(Site, Plot, Year) %>%
#   summarize(MeanCover = mean(Cover, na.rm = TRUE))
# 
# # Merge the mean cover back to the original data
# data_merged <- All_sites %>%
#   inner_join(plot_mean_cover, by = c("Site", "Plot", "Year"))
# 
# # Calculate the variance within each plot at a fixed time t
# data_merged <- data_merged %>%
#   mutate(Variance = (Cover - MeanCover)^2)
# 
# # Summing up the variances for each plot to get V_S
# spatial_variance <- data_merged %>%
#   group_by(Site, Plot, Year) %>%
#   summarize(SpatialVariance = sum(Variance, na.rm = TRUE))
# 
# # Calculate the spatial variability (CV^2_S) for each plot
# spatial_variance <- spatial_variance %>%
#   inner_join(plot_mean_cover, by = c("Site", "Plot", "Year")) %>%
#   mutate(SpatialVariability = SpatialVariance / (MeanCover^2))
# 
# spatial_variance <- spatial_variance %>%
#   inner_join(Metrics, by = c("Site", "Plot"))
# 
# spatial_variance$Year <- as.factor(spatial_variance$Year)
# 
# Temp_Spat_lmer <- lmer(Temporal_variability ~ SpatialVariability + (1|Year) + (1|Site), data = spatial_variance)
# spatial_variance$fit <- predict(Temp_Spat_lmer, spatial_variance)
# summary(Temp_Spat_lmer)
# 
# r.squaredGLMM(Temp_Spat_lmer)
# 
# tab_model(Temp_Spat_lmer)
# 
# 
# ### Plot ###
# ggplot(spatial_variance, aes(SpatialVariability, Temporal_variability, col=Site, linetype = Year )) + # 
#   geom_line(aes(y=fit), linewidth=0.8, alpha=0.5) +
#  # geom_line(aes(y=fit2), linewidth=1, color ="Black") +
#   geom_point(alpha = 0.2) +
#   theme_bw() +ylab("Temporal (γ) variability") + xlab("Spatial (β) variability") +
#    annotate("text", x= 100, y= 3.1, label= "Marginal R2 = 0.479")+
#    annotate("text", x= 100, y= 2.9, label= "Conditional R2 = 0.572")
# 
# 
# ### portfolio effect ###
# 
# 
# ### OR add to Metrics db ###
# # Metrics <- Metrics %>%
# #    left_join(spatial_variance, by = c("Site", "Plot"))
# # 
# # Metrics$Spatial_variability <- Metrics$SpatialVariance / ((Metrics$mean_cover)^2)
# # 
# # # Calculate the spatial variability (CV^2_S) for each plot
# # spatial_variance <- spatial_variance %>%
# #   inner_join(plot_mean_cover, by = c("Site", "Plot")) %>%
# #   mutate(SpatialVariability = SpatialVariance / (MeanCover^2))
# # 
# # # Display the results
# # print(spatial_variance)











# 
# ##########################
# ### Spatial asynchrony ###
# ##########################
# 
# # Calculate yearly productivity for each plot
# yearly_productivity <- filtered_data %>%
#   group_by(Site, Plot, Year) %>%
#   summarize(Cover = sum(Cover, na.rm = TRUE)) %>%
#   ungroup()
# 
# # Calculate covariance matrix for each plot
# plots <- unique(yearly_productivity$Plot)
# covariance_matrices <- list()
# 
# for (plot in plots) {
#   plot_data <- yearly_productivity %>% filter(Plot == plot)
#   pivot_table <- plot_data %>%
#     pivot_wider(names_from = Site, values_from = Cover) %>%
#     select(-Year)
#   pivot_table[is.na(pivot_table)] <- 0
#   # Ensure that the pivot table contains only numeric data
#   numeric_pivot_table <- as.data.frame(lapply(pivot_table, function(x) as.numeric(as.character(x))))
#   cov_matrix <- cov(numeric_pivot_table, use = "complete.obs")
#   covariance_matrices[[plot]] <- cov_matrix
# }
# 
# # Calculate spatial asynchrony for each plot
# spatial_asynchrony <- data.frame(Plot = character(), Spatial_Asynchrony = numeric(), stringsAsFactors = FALSE)
# 
# for (plot in names(covariance_matrices)) {
#   cov_matrix <- covariance_matrices[[plot]]
#   v_ii_sum <- sum(diag(cov_matrix))
#   v_ij_sum <- sum(cov_matrix)
#   omega <- ifelse(v_ij_sum != 0, (v_ii_sum^2) / v_ij_sum, 0)
#   spatial_asynchrony <- rbind(spatial_asynchrony, data.frame(Plot = plot, Spatial_Asynchrony = omega))
# }
# 
# # Display the results
# print(spatial_asynchrony)
# 
# 



will.spat.cov <- function()