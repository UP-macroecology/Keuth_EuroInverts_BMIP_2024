# test number of occurrences

load("data/in_between_MCP2_results.Rdata")

GBIF_MCP_5k <- species_info[, c("binomial", "MCP2")]

load("data/MCP/tmp.sf_Cordylophora caspia.Rdata")
tmp.sf <- st_convex_hull(st_union(tmp.sf))
print(st_is_valid(tmp.sf, reason = T))
range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
GBIF_MCP_5k[which(GBIF_MCP_5k$binomial == "Cordylophora caspia"), "MCP2"] <- as.numeric(range_coverage_df)

GBIF_MCP_5k$MCP2_perc <- 1

GBIF_MCP_5k$occ <- "5000"                         

# next data set

load("data/in_between_MCP2_results_10k.Rdata")

GBIF_MCP_10k <- species_info[, c("binomial", "MCP2")]

load("data/MCP/tmp.sf_Cordylophora caspia10k.Rdata")
tmp.sf <- st_convex_hull(st_union(tmp.sf))
print(st_is_valid(tmp.sf, reason = T))
range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
GBIF_MCP_10k[which(GBIF_MCP_10k$binomial == "Cordylophora caspia"), "MCP2"] <- as.numeric(range_coverage_df)

GBIF_MCP_10k$MCP2_perc <- GBIF_MCP_10k$MCP2/ GBIF_MCP_5k$MCP2

GBIF_MCP_10k$occ <- "10000"  

# next data set

load("data/in_between_MCP2_results_20k.Rdata")

GBIF_MCP_20k <- species_info[, c("binomial", "MCP2")]

load("data/MCP/tmp.sf_Cordylophora caspia20k.Rdata")
tmp.sf <- st_convex_hull(st_union(tmp.sf))
print(st_is_valid(tmp.sf, reason = T))
range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
GBIF_MCP_20k[which(GBIF_MCP_20k$binomial == "Cordylophora caspia"), "MCP2"] <- as.numeric(range_coverage_df)

load("data/MCP/tmp.sf_Galba truncatula20k.Rdata")
tmp.sf <- st_convex_hull(st_union(tmp.sf))
print(st_is_valid(tmp.sf, reason = T))
range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
GBIF_MCP_20k[which(GBIF_MCP_20k$binomial == "Galba truncatula"), "MCP2"] <- as.numeric(range_coverage_df)

GBIF_MCP_20k$MCP2_perc <- GBIF_MCP_20k$MCP2/ GBIF_MCP_5k$MCP2

GBIF_MCP_20k$occ <- "20000"   

# join all data sets
GBIF_MCP <- rbind(GBIF_MCP_5k, GBIF_MCP_10k, GBIF_MCP_20k)
GBIF_MCP$occ <- as.numeric(GBIF_MCP$occ)

# which have values under 0
which(GBIF_MCP$MCP2_perc> 30)

special <- GBIF_MCP[which(GBIF_MCP$MCP2_perc > 30),]

library(ggplot2)

ggplot(GBIF_MCP, aes(x=occ, y = MCP2_perc))+
  geom_point()+
  geom_smooth(aes(group = binomial), method = "loess", linewidth = 0.8)+
  xlab("Number of GBIF occurrences")+
  ylab("MCP")+
  geom_smooth(col = "red", method = "loess", linewidth = 1.5)

ggplot(GBIF_MCP, aes(x=occ, y = MCP2_perc))+
  geom_point()+
  geom_smooth(aes(group = binomial), method = "loess", linewidth = 0.8)+
  xlab("Number of GBIF occurrences")+
  ylab("MCP")+
  geom_smooth(col = "red", method = "loess", linewidth = 1.5)+
  ylim(c(0.8,8))

library(ggforce)

p <- ggplot(GBIF_MCP, aes(x=occ, y = MCP2))+
  geom_point()+
  geom_line()+
  facet_wrap_paginate(~binomial, ncol = 5, nrow = 5, scales = "free_y")

for(i in 1:n_pages(p)){
  p_save <-  p + 
    facet_wrap_paginate(~ binomial, ncol = 5, nrow = 5, page = i, scales = "free_y")
  ggsave(plot = p_save, filename = paste0('data/plots/page_', i, '.jpg'))
}
