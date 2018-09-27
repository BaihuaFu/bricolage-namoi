### explore namoi scenario data results
### Baihua Fu
### 7/7/15
### v2: 30/5/16
### V3: post iemss, paper v7, 9/16
### v4: paper v9, 1/12/16; paper v10b, 7/12/16

setwd("C:\\UserData\\fub\\work09\\Namoi\\integrated model results")
library(party)
library(reshape2)
library(ggplot2)
library(gplots)
library(viridis)


results <- read.csv("runs16384_extremecases.csv")

results$type="scenarios"


### set up all inputs as 'factor'
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


### Plot data on raw output space, by climate
resultm <- melt(results, measure.vars = c("Profit_mean","Profit_Std","RRG_flow_index","RRG_gw_index","Gw_depth_mean","Gw_depth_min"), all=TRUE)

levels(resultm$variable) <- c("Profit mean (million$)", "Profit std", "RRG flow annual index", "RRG gw annual index", "Gw depth mean (m)", "Gw depth min (m)")

# svg("Namoiscenario fig1.svg",width = 6, height = 3)
p<-ggplot(resultm, aes(x=value))+geom_density(aes(fill=factor(Climate), y = ..scaled..), alpha=0.8, lwd=0.2)+facet_wrap(~variable, scale="free")+theme_bw()+theme(axis.title = element_blank(), text = element_text(size=10),  axis.ticks=element_line(size=0.2))+ scale_fill_grey(name="Climate")
plot(p)
# dev.off()

# ggsave("Namoiscenario fig5.tiff",width = 6, height = 3)
# ggsave("Namoiscenario fig5.pdf",width = 6, height = 3)

# ggplot(resultm, aes(x=Conjunctive_options, y=value))+geom_violin(aes(trim=FALSE), fill="gray70") +geom_boxplot(width=0.2)+facet_wrap(~variable, scales="free", ncol=3)+theme_bw() + theme(legend.position = "none", text = element_text(size=10))

## merged two figures
ggplot(resultm, aes(x=Climate, y=value))+geom_violin(aes(fill=Climate, trim=FALSE), alpha=0.6)+geom_boxplot(width=0.2)+facet_wrap(~variable, scales="free", ncol=6)+theme_bw() + theme(legend.position = "none", text = element_text(size=10),axis.title.y = element_blank())
# + scale_fill_grey()

ggplot(resultm, aes(x=Flow_allocation, y=value))+geom_violin(aes(fill=Flow_allocation, trim=FALSE), alpha=0.6)+geom_boxplot(width=0.2)+facet_wrap(~variable, scales="free", ncol=6)+theme_bw() + theme(legend.position = "none", text = element_text(size=10),axis.title.y = element_blank())
# + scale_fill_grey()



# ggplot(resultm, aes(x=Conjunctive_options, y=value))+geom_violin(aes(fill=Conjunctive_options, trim=FALSE), alpha=0.6)+geom_boxplot(width=0.2)+facet_wrap(~variable, scales="free", ncol=3)+theme_bw() + theme(legend.position = "none", text = element_text(size=10))+ scale_fill_grey()
# ggsave("Namoiscenario fig6 conjoption.tiff",width = 6, height = 3)
# ggsave("Namoiscenario fig6 conjoption.pdf",width = 6, height = 3)


### standadise the results.
## function to normalise the data based on its range.
normalise <- function(x, na.rm = TRUE) {
    ranx <- range(x, na.rm = na.rm)
    (x - ranx[1]) / diff(ranx)
}

## resultscale is all results and the outputs are scaled
resultscale <- results
resultscale[, 20:25] <- apply(results[, 20:25], 2, normalise) ## to standadised by range

## heatmap
results_nocond.imp <- read.csv("results_varimp_15jun16.csv", header=T, row.names=1)
colnames(results_nocond.imp) <- gsub("_", "\n", colnames(results_nocond.imp))

# heatmap.2(as.matrix(results_nocond.imp), col=viridis, cexRow=0.8, cexCol=0.9, scale="column", trace="none", margin = c(5,10), dendrogram=NULL, key.title=NA, key.ylab="Count", key.xlab="Variable importance")#, breaks=col_breaks)

heatmap.2(as.matrix(results_nocond.imp), col=viridis, cexRow=0.8, cexCol=0.9, srtCol=0,adjCol = c(0.5,0.8),trace="none", margin = c(5,10), dendrogram=NULL, key.title=NA, key.ylab="Count", key.xlab="Variable importance")#, breaks=col_breaks)


## four cluster violin
resultsm <- melt(resultscale, measure.vars = c("Profit_mean","Profit_Std","RRG_flow_index","RRG_gw_index","Gw_depth_mean","Gw_depth_min"), all=TRUE)

levels(resultsm$variable) <- gsub("_", "\n", levels(resultsm$variable))

ggplot(resultsm, aes(x=variable, y=value))+geom_violin(aes(fill=variable, trim=FALSE), alpha=0.6)+facet_grid(Crop_price_change~Climate, scales="free", labeller = labeller(.rows = label_both, .cols = label_both))+theme_bw() + theme(legend.position = "none", axis.title = element_blank())
# + scale_fill_grey()
# scale_fill_manual(values = c("gray70","gray70", "gray70", "gray70", "gray70", "gray70"))


## aggregated results
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

# ggplot(resultscaleagm, aes(x=Climate, y=value))+geom_boxplot(fill="orange") +theme_bw()+xlab(NULL)+ylab(NULL) + geom_jitter(shape=16, size=0.5, position=position_jitter(0.3), alpha=(0.1), col="steel blue")+facet_wrap(Crop_price_change~Climate, scales="free", ncol=4)

# resultscaleagmm <- melt(resultscaleagm, measure.vars = c("Profit_mean","Profit_Std","RRG_flow_index","RRG_gw_index","Gw_depth_mean","Gw_depth_min"), all=TRUE)

# ggplot(resultscaleagmm, aes(x=Climate, y=value))+geom_violin(aes(fill="grey", trim=FALSE), alpha=0.6)+facet_grid(.~Crop_price_change, scales="free", labeller = labeller(.rows = label_both, .cols = label_both))+theme_bw() + theme(legend.position = "none", axis.title = element_blank())

ggplot(resultscaleagm, aes(x=value_judgement_score))+geom_density(fill="grey") +theme_bw()+xlab(NULL)+ylab(NULL)+facet_grid(Crop_price_change + Climate ~., scales="fixed")

## merged two figures #2
ggplot(resultsm, aes(x=variable, y=value))+geom_violin(aes(fill=variable, trim=FALSE), alpha=0.6)+facet_grid(.~ Climate+Crop_price_change, scales="free", labeller = labeller(.rows = label_both, .cols = label_both))+theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=10))+labs(y="Scaled values")

ggplot(resultscaleagm, aes(x=value_judgement_score))+geom_density(fill="grey") +theme_bw()+xlab(NULL)+ylab(NULL)+facet_grid(.~Climate +Crop_price_change, scales="fixed")+labs(x="Aggregated values")+theme(strip.text = element_blank() , strip.background = element_blank(),text = element_text(size=10))


## 2 by 2
ggplot(resultsm, aes(x=variable, y=value))+geom_violin(aes(fill=variable, trim=FALSE), alpha=0.6)+facet_grid(Crop_price_change~Climate, scales="free", labeller = labeller(.rows = label_both, .cols = label_both))+theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=10))+labs(y="Scaled values")

ggplot(resultscaleagm, aes(x=value_judgement_score))+geom_density(fill="grey") +theme_bw()+xlab(NULL)+ylab(NULL)+facet_grid(Crop_price_change~Climate, scales="fixed",labeller = labeller(.rows = label_both, .cols = label_both))+labs(x="Aggregated values")+theme(text = element_text(size=10))


### regression tree
## groundwater index
plotdata <- subset(resultscale, Climate == "dry")

# tree<- ctree(RRG_gw_index~Flow_req_weights+ Flood_WUE+Spray_WUE+Spray_adopt_rate+Climate+flood_minsep+flood_minduration+CTF+Flow_allocation+Gw_allocation+ Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change+Conjunctive_options, data=plotdata, controls = ctree_control(maxdepth = 2))
# plot(tree)

## all data for gw index
# tree<- ctree(RRG_gw_index~Flow_req_weights+ Flood_WUE+Spray_WUE+Spray_adopt_rate+Climate+flood_minsep+flood_minduration+CTF+Flow_allocation+Gw_allocation+ Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change+Conjunctive_options, data=resultscale, controls = ctree_control(maxdepth = 3))
# plot(tree)

## system uncertainty only
# tree<- ctree(RRG_gw_index~Flow_req_weights+ Climate+flood_minsep+flood_minduration+CTF+Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change, data=resultscale, controls = ctree_control(maxdepth = 3))
# plot(tree)

tree<- ctree(RRG_gw_index~Flow_req_weights+ Flood_WUE+Spray_WUE+Spray_adopt_rate+Climate+flood_minsep+flood_minduration+CTF+Flow_allocation+Gw_allocation+ Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change+Conjunctive_options, data=subset(resultscale, Climate == "dry"), controls = ctree_control(testtype="Bonferroni", maxdepth = 3,  mincriterion = 0.995))
plot(tree, main="Dry climate")

tree<- ctree(RRG_gw_index~Flow_req_weights+ Flood_WUE+Spray_WUE+Spray_adopt_rate+Climate+flood_minsep+flood_minduration+CTF+Flow_allocation+Gw_allocation+ Crop_price+Flood_timing+Flood_duration+Dry_period+Gw_depth_req+Flow_uncert+Gw_uncert+Crop_price_change+Conjunctive_options, data=subset(resultscale, Climate == "dry" & Gw_uncert == "80%" & Gw_depth_req == "deep roots"), controls = ctree_control(testtype="Bonferroni", maxdepth = 3,  mincriterion = 0.995))
plot(tree, main="Dry climate")


## equal weight
plotdata2 = subset(resultscaleag, Climate == "dry" & Crop_price_change == "up")

tree<- ctree(Equal_weight~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+Conjunctive_options, data=plotdata2, control = ctree_control(testtype="Bonferroni", maxdepth = 3,  mincriterion = 0.995))
plot(tree)

tree<- ctree(Equal_weight~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+Conjunctive_options, data=plotdata2, control = ctree_control(testtype="Bonferroni",  mincriterion = 0.995))
plot(tree)


## all weights
plotdata3 = subset(resultscaleagm, Climate == "dry" & Crop_price_change == "up")

# tree<- ctree(value_judgement_score~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+ Crop_price_change+Conjunctive_options, data=plotdata3, control = ctree_control(maxdepth = 3, mincriterion = 0.995))
# plot(tree)

# tree<- ctree(value_judgement_score~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+ Crop_price_change+Conjunctive_options + value_judgement, data=plotdata3, control = ctree_control(maxdepth = 3, mincriterion = 0.995))
# plot(tree)

## synthesis
plotdata4 = subset(resultscaleag, Climate == "wet" & Crop_price_change == "down")
tree<- ctree(Pro_ecology~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+ Crop_price_change+Conjunctive_options, data=plotdata4, control = ctree_control(maxdepth = 3, mincriterion = 0.995))
plot(tree)


plotdata4 = subset(resultscaleag, Climate == "wet" & Crop_price_change == "down")

tree<- ctree(Pro_gw~Flood_WUE+Spray_WUE+Spray_adopt_rate+Flow_allocation+Gw_allocation+ Conjunctive_options, data=plotdata4, control = ctree_control(mincriterion = 0.995))
plot(tree)

# nodes(tree, unique(where(tree)))

# a <- by(plotdata4$Equal_weight,where(tree),quantile, probs = 0.9)
a <- by(plotdata4$Pro_gw,where(tree),median)
which.max(a)

plot(plotdata4$Pro_profit ~ as.factor(where(tree)))


##########     END PAPER     ################

