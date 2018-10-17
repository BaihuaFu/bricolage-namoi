library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

runs = read.csv("runs16384_extremecases.csv")

scen.vars = c(
  "eco_weights_choice",
  "WUE_flood_choice",
  "WUE_spray_choice",
  "adoption_choice",
  "climate_choice",
  "eco_min_separation_choice",
  "eco_min_duration_choice",
  "eco_ctf_choice",
  "AWD_surface_choice",
  "AWD_gw_choice",
  "crop_price_choice",
  "timing_col",
  "duration_col",
  "dry_col",
  "gwlevel_col",
  "sw_uncertainty_choice",
  "gw_uncertainty_choice",
  "crop_trend",
  "cj_options"
)
out.vars = c(
  "profit_mean",
  "profit_std",
  "surface_index",
  "gw_index",
  "gwlevel_mean",
  "gwlevel_min"
)
scen.vars.labelled<-c(
  "Flow req. weights","Flood WUE","Spray WUE","Spray adopt. rate",
  "Climate","Flood min. sep.","Flood min. duration","Flood CTF",
  "Flow allocation","GW allocation","Crop price",
  "Flood timing","Flood duration","Dry period","GW depth req.",
  "Flow uncertainty","GW uncertainty",
  "Crop price change","Conj. use options"
)
names(scen.vars.labelled)<-scen.vars

################################
# Marginal effect of each variable on profit.mean ----
diff = function(data, focus.var, out.var, scen.vars) {
  vals = data[[focus.var]] %>% unique() %>% as.character
  stopifnot(length(vals) == 2)
  
  data %>%
    select(!!out.var, !!scen.vars) %>%
    spread(!!focus.var, !!out.var) %>%
    rename(a = !!(vals[1]), b = !!(vals[[2]])) %>%
    mutate(
      diff = a - b,
      a = !!(vals[1]),
      b = !!(vals[2]),
      focus.var = !!focus.var
    )
}

#Test, using marginal effect of climate_choice on profit_mean
stopifnot(identical(
  diff(runs, "climate_choice", "profit_mean", scen.vars),
  runs %>% select(profit_mean, !!scen.vars) %>%
    spread(key = climate_choice, value = profit_mean) %>% mutate(diff = dry- wet) %>% 
    mutate(dry=NULL,wet=NULL,a="dry",b="wet",focus.var="climate_choice") %>% 
    select(setdiff(scen.vars,"climate_choice"),a,b,diff,focus.var)
))


#Only select the variables with two levels
scen.vars.varying = scen.vars[sapply(scen.vars, function(n)
  length(unique(runs[[n]]))) == 2]

marginal.all = lapply(scen.vars.varying, function(focus.var)
  diff(runs, focus.var, "profit_mean", scen.vars)) %>% bind_rows

# Deal with cj_options - compare pairs of options
marginal.all <- runs$cj_options %>% unique %>% as.character() %>%
  combn(2) %>%
  apply(2, function(vals)
    diff(
      filter(runs, cj_options %in% vals) ,
      "cj_options",
      "profit_mean",
      scen.vars
    )) %>% bind_rows %>%
  bind_rows(marginal.all)


# Emphasise what the difference is between
marginal.all$focus.var2 <-
  with(marginal.all, sprintf("%s\n%s-%s", focus.var, a, b))


marginal.mean.diff = marginal.all %>% group_by(focus.var2) %>% 
  summarise(mean_diff = abs(mean(diff))) %>% arrange(mean_diff)

marginal.all$focus.var2 %<>% 
  ordered(.,
          levels = marginal.mean.diff$focus.var2,
          labels=stringr::str_replace_all(marginal.mean.diff$focus.var2,scen.vars.labelled)
  )


ggplot(data = marginal.all, aes(x = focus.var2, y = diff/1e6)) +
  geom_boxplot(width = 0.2) + coord_flip() +
  theme_bw() +
  labs(x="",y="Change in mean profit (million$)")

ggsave("marginal_effect_profitmean.eps",width=19,units="cm")

################################
# Marginal effect of each scen.var on each out.var
mean_diff = function(data, out.var, scen.vars) {
  scen.vars.varying = scen.vars[sapply(scen.vars, function(n)
    length(unique(data[[n]]))) == 2]
  marginal.all = lapply(scen.vars.varying, function(focus.var)
    diff(data, focus.var, out.var, scen.vars)) %>% bind_rows
  
  # Deal with cj_options - compare pairs of options
  marginal.all <- data$cj_options %>% unique %>% as.character() %>%
    combn(2) %>%
    apply(2, function(vals)
      diff(
        filter(data, cj_options %in% vals) ,
        "cj_options",
        out.var,
        scen.vars
      )) %>% bind_rows %>%
    bind_rows(marginal.all)
  
  marginal.all$focus.var2 <-
    with(marginal.all, sprintf("%s\n%s-%s", focus.var, a, b))
  marginal.mean.diff = marginal.all %>%
    group_by(focus.var2) %>%
    summarise(mean_diff = mean(diff)) %>%
    arrange(mean_diff) %>%
    mutate(out.var = out.var)
  marginal.mean.diff
}

# Test against data from above, for out.var="profit_mean"
stopifnot(identical(
  mean_diff(runs, "profit_mean", scen.vars),
  marginal.all %>% 
    mutate(focus.var2=sprintf("%s\n%s-%s", focus.var, a, b)) %>% 
    group_by(focus.var2) %>% 
    summarise(mean_diff = mean(diff)) %>% arrange(mean_diff) %>% 
    mutate(out.var="profit_mean",focus.var2=as.character(focus.var2))
))

mean.diff.all <-
  lapply(out.vars, function(out.var)
    mean_diff(runs, out.var, scen.vars)) %>% bind_rows

mean.diff.all.normalised <- mean.diff.all %>% group_by(out.var) %>%
  mutate(
    abs_mean_diff = abs(mean_diff),
    norm_mean_diff = abs_mean_diff / max(abs_mean_diff)
  )
mean.diff.all.normalised$focus.var2 %<>% 
  ordered(., 
          levels = marginal.mean.diff$focus.var2,
          labels=stringr::str_replace_all(marginal.mean.diff$focus.var2,scen.vars.labelled))


mean.diff.all.normalised$out.var %<>% ordered(
  levels = c(
    "profit_std",
    "profit_mean",
    "surface_index",
    "gw_index",
    "gwlevel_min",
    "gwlevel_mean"
  ),
  labels=c(
    "Profit\nStd",
    "Profit\nmean",
    "RRG\nflow\nindex",
    "RRG\ngw\nindex",
    "GW\ndepth\nmin",
    "GW\ndepth\nmean"
  )
)

# As colour
ggplot(data = mean.diff.all.normalised, aes(y = focus.var2, x = out.var)) +
  geom_tile(aes(fill = norm_mean_diff)) +
  scale_fill_viridis(
    labels=scales::percent_format(),
    guide=guide_colorbar(title="Mean marginal effect\n(as % of max for each output variable)"))+
  labs(x="",y="")+
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave("marginal_effects.eps",width=19,units="cm")

