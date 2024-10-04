# test # of occurrences

load("data/in_between_MCP2_results.Rdata")

GBIF_MCP_5k <- species_info[, c("binomial", "MCP2")]

GBIF_MCP_5k$occ <- "5000"                         

# next data set

load("data/in_between_MCP2_results_10k.Rdata")

GBIF_MCP_10k <- species_info[, c("binomial", "MCP2")]

GBIF_MCP_10k$occ <- "10000"  

# next data set

load("data/in_between_MCP2_results_20k.Rdata")

GBIF_MCP_20k <- species_info[, c("binomial", "MCP2")]

GBIF_MCP_20k$occ <- "20000"   

# join all data sets
GBIF_MCP <- rbind(GBIF_MCP_5k, GBIF_MCP_10k, GBIF_MCP_20k)
GBIF_MCP$occ <- as.numeric(GBIF_MCP$occ)

#GBIF_MCP$occ <- factor(GBIF_MCP$occ, levels = c("5k", "10k", "20k"))

library(ggplot2)

ggplot(GBIF_MCP, aes(x=occ, y = MCP2, group = "species"))+
  geom_point()+
  geom_smooth(method = "loess")+
  xlab("Number of GBIF occurrences")+
  ylab("MCP")

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
