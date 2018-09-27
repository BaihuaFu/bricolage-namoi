library(party)
library(reshape2)
library(ggplot2)

results <- read.csv("runs16384_extremecases.csv")

results$type="scenarios"


### tidy up inputs
results$AWD_surface_choice <- as.factor(results$AWD_surface_choice)
results$AWD_gw_choice <- as.factor(results$AWD_gw_choice)
results$crop_price_choice <- as.factor(results$crop_price_choice)
results$type <- as.factor(results$type)
results$profit_mean <- results$profit_mean/1000000
results$profit_std <- results$profit_std/1000000


### Update column names and state names
colnames(results) = c("Flow_req_weights", "Flood_WUE","Spray_WUE","Spray_adopt_rate","Climate","flood_minsep","flood_minduration","CTF","Flow_allocation","Gw_allocation", "Crop_price","Flood_timing","Flood_duration","Dry_period","Gw_depth_req","Flow_uncert","Gw_uncert","Crop_price_change","Conjunctive_options","Profit_mean","Profit_Std","RRG_flow_index","RRG_gw_index","Gw_depth_mean","Gw_depth_min", "type")

levels(results$Flood_WUE)[match("max",levels(results$Flood_WUE))] <- "max(80%)"
levels(results$Flood_WUE)[match("min",levels(results$Flood_WUE))] <- "min(50%)"

levels(results$Spray_WUE)[match("max",levels(results$Spray_WUE))] <- "max(90%)"
levels(results$Spray_WUE)[match("min",levels(results$Spray_WUE))] <- "min(70%)"

levels(results$Spray_adopt_rate)[match("max",levels(results$Spray_adopt_rate))] <- "min(16.9%)"
levels(results$Spray_adopt_rate)[match("min",levels(results$Spray_adopt_rate))] <- "max(0.01%)"

levels(results$Flow_allocation)[match("2",levels(results$Flow_allocation))] <- "max200%"
levels(results$Flow_allocation)[match("0.5",levels(results$Flow_allocation))] <- "max50%"

levels(results$Gw_allocation)[match("2",levels(results$Gw_allocation))] <- "max200%"
levels(results$Gw_allocation)[match("0.5",levels(results$Gw_allocation))] <- "max50%"

levels(results$Gw_depth_req)[match("F2",levels(results$Gw_depth_req))] <- "deep roots"
levels(results$Gw_depth_req)[match("Index",levels(results$Gw_depth_req))] <- "best guess"

levels(results$Flow_req_weights)[match("Favour duration",levels(results$Flow_req_weights))] <- "favour duration"
levels(results$Flow_req_weights)[match("Default",levels(results$Flow_req_weights))] <- "best guess"


### Figure 2:
resultm <- melt(results, measure.vars = c("Profit_mean","Profit_Std","RRG_flow_index","RRG_gw_index","Gw_depth_mean","Gw_depth_min"), all=TRUE)

levels(resultm$variable) <- c("Profit mean (million$)", "Profit std", "RRG flow annual index", "RRG gw annual index", "Gw depth mean (m)", "Gw depth min (m)")

ggplot(resultm, aes(x=Climate, y=value))+geom_violin(aes(fill=Climate, trim=FALSE), alpha=0.6)+geom_boxplot(width=0.2)+facet_wrap(~variable, scales="free", ncol=6)+theme_bw() + theme(legend.position = "none", text = element_text(size=10),axis.title.y = element_blank())

ggplot(resultm, aes(x=Flow_allocation, y=value))+geom_violin(aes(fill=Flow_allocation, trim=FALSE), alpha=0.6)+geom_boxplot(width=0.2)+facet_wrap(~variable, scales="free", ncol=6)+theme_bw() + theme(legend.position = "none", text = element_text(size=10),axis.title.y = element_blank())


### standardise the results.
normalise <- function(x, na.rm = TRUE) {
    ranx <- range(x, na.rm = na.rm)
    (x - ranx[1]) / diff(ranx)
}

resultscale <- results
resultscale[, 20:25] <- apply(results[, 20:25], 2, normalise) ## to standadised by range


## Figure 4a - four cluster violin
resultsm <- melt(resultscale, measure.vars = c("Profit_mean","Profit_Std","RRG_flow_index","RRG_gw_index","Gw_depth_mean","Gw_depth_min"), all=TRUE)

levels(resultsm$variable) <- gsub("_", "\n", levels(resultsm$variable))

ggplot(resultsm, aes(x=variable, y=value))+geom_violin(aes(fill=variable, trim=FALSE), alpha=0.6)+facet_grid(Crop_price_change~Climate, scales="free", labeller = labeller(.rows = label_both, .cols = label_both))+theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=10))+labs(y="Scaled values")


## Figure 4b - aggregated results based on weights on ecology, gw, profit
weight <- c(1,1,1)
resultscaleag <- resultscale
resultscaleag$Equal_weight <- apply(subset(resultscaleag, select= c(Profit_mean, RRG_flow_index,Gw_depth_mean)), 1, function(d) weighted.mean(d, weight, na.rm=TRUE))

weight <- c(3,1,1)
resultscaleag$Pro_profit <- apply(subset(resultscaleag, select= c(Profit_mean, RRG_flow_index,Gw_depth_mean)), 1, function(d) weighted.mean(d, weight, na.rm=TRUE))

weight <- c(1,3,1)
resultscaleag$Pro_ecology <- apply(subset(resultscaleag, select= c(Profit_mean, RRG_flow_index,Gw_depth_mean)), 1, function(d) weighted.mean(d, weight, na.rm=TRUE))

weight <- c(1,1,3)
resultscaleag$Pro_gw <- apply(subset(resultscaleag, select= c(Profit_mean, RRG_flow_index,Gw_depth_mean)), 1, function(d) weighted.mean(d, weight, na.rm=TRUE))

resultscaleagm <- melt(resultscaleag, measure.var=c("Equal_weight", "Pro_profit", "Pro_ecology", "Pro_gw"), all=TRUE)

names(resultscaleagm)[names(resultscaleagm) == 'variable'] <- 'value_judgement'
names(resultscaleagm)[names(resultscaleagm) == 'value'] <- 'value_judgement_score'


ggplot(resultscaleagm, aes(x=value_judgement_score))+geom_density(fill="grey") +theme_bw()+xlab(NULL)+ylab(NULL)+facet_grid(Crop_price_change~Climate, scales="fixed",labeller = labeller(.rows = label_both, .cols = label_both))+labs(x="Aggregated values")+theme(text = element_text(size=10))


### Figure 5: regression tree
drydata <- subset(resultscale, Climate == "dry")
wetdata <- subset(resultscale, Climate == "wet")

tree<- ctree(RRG_gw_index~Flow_req_weights+ Climate+flood_minsep+flood_minduration+CTF+Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change, data=drydata, controls = ctree_control(maxdepth = 3))
plot(tree)

tree<- ctree(RRG_gw_index~Flow_req_weights+ Climate+flood_minsep+flood_minduration+CTF+Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change, data=wetdata, controls = ctree_control(maxdepth = 3))
plot(tree)


## Figure 6:
plotdata2 = subset(resultscaleag, Climate == "dry" & Crop_price_change == "up")

tree<- ctree(Equal_weight~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+Conjunctive_options, data=plotdata2, control = ctree_control(testtype="Bonferroni", maxdepth = 3,  mincriterion = 0.995))
plot(tree)


## synthesis - analysis to generate data for Table 2. One example is shown here. The actual analysis involves manually changing subset (climate and crop_price_change values), and the y in the ctree function (e.g. Pro_ecology, Pro_gw, Pro_profit, Equal_weight)
plotdata4 = subset(resultscaleag, Climate == "wet" & Crop_price_change == "down")
tree<- ctree(Pro_profit~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+ Crop_price_change+Conjunctive_options, data=plotdata4, control = ctree_control(maxdepth = 3, mincriterion = 0.995))
plot(tree)


##########     END PAPER     ################

