library(tidyverse)
library(ggmap)
library(ggthemes)
library(ggraph)
#read data -----
df <- tibble()
for(y in 1958:2016){
  df.y <- read.table(paste0(y,".csv"),header=T)
  df.y$year=y
  df <- bind_rows(df,df.y)
}

# clean data ----
df <- df %>% mutate(points=ifelse(is.na(points),0,points),
                    country=ifelse(country=="United Kingdom","UK",country),
                    country=ifelse(country=="FYR Macedonia","Macedonia",country),
                    country=ifelse(country=="Bosnia & Herzegovina","Bosnia and Herzegovina",country))
 
# max voting graph ----

g <- df %>% group_by(voter,year) %>% 
  dplyr::filter(points==max(points)) %>% 
  ungroup() %>% 
  select(voter,country) %>% 
  as.matrix() %>% 
  graph_from_edgelist()
E(g)$weight <- 1
g <- g %>% igraph::simplify(edge.attr.comb = "sum")
plot(g)
groups<-cluster_optimal(g)
V(g)$cluster=groups$membership

ggraph(g, layout = 'kk') + 
  geom_edge_link(aes(width = weight),edge_colour="gray",alpha=0.7, show.legend = FALSE) + 
  geom_node_point(size=8,col="black")+ 
  theme_graph()

# cumsum points ----
df %>% group_by(country,year) %>% 
  dplyr::summarise(pts=sum(points)) %>% 
  mutate(total=cumsum(pts)) %>% 
  ggplot(aes(x=year,y=total)) +
  geom_line(aes(col=country)) +
  theme(legend.position = "none")

df %>% group_by(country,year) %>% 
  dplyr::summarise(pts=sum(points)) %>% 
  mutate(total=cumsum(pts)) %>% 
  dplyr::filter(year==2016) %>% 
  arrange(-total)

# maps ----
map<-map_data("world")
map <- map %>% semi_join(df,by=c("region"="country"))
map <- map %>% left_join(data.frame(region=groups$names,clust=factor(groups$membership)))
map$lat[map$region=="Australia"] <- (map$lat[map$region=="Australia"]+40)*0.7
map$long[map$region=="Australia"] <- (map$long[map$region=="Australia"]-90)*0.7

ggplot() + 
  geom_polygon(data=map, aes(x=long, y=lat, group=group,fill=clust), colour="white",size=0.2)+
  annotate("rect",xmin=15,xmax=49,ymin=-4,ymax=22,col="gray",fill=NA)+
  coord_fixed(xlim=c(-20,50),ylim=c(-5,70),ratio = 0.80)+
  theme_tufte()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  labs(x="",y="")

