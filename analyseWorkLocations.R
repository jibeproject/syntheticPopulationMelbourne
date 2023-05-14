# libraries and functions -------------------------------------------------
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)


df=globalDistComparisonLong;binwidth=5

aggregateData <- function(df,binwidth,max_dist=Inf) {
  # binwidth=400
  maxDist <- max(df$distance)
  
  df_aggregated <- df %>%
    mutate(distance=findInterval(distance,seq(0,maxDist,binwidth))) %>%
    mutate(distance=distance*binwidth-(binwidth*0.5)) %>%
    mutate(distance=ifelse(type=="expected",distance-binwidth*0.25,distance+binwidth*0.25)) %>%
    group_by(distance,type) %>%
    summarise(proportion=sum(proportion,na.rm=T)) %>%
    dplyr::select(distance,type,proportion) %>%
    filter(distance<max_dist)
}

# import data -------------------------------------------------------------
work_hist_global <- readRDS("work_hist_global.rds") %>%
  mutate(distance=row_number()) %>%
  select(distance,global_dist_pr=pr) %>%
  data.table()

work_hist_sa3 <- readRDS("work_hist_sa3.rds")
work_sa3_movement <- readRDS("work_sa3_movement.rds")
workers <- readRDS("workers_10pc.rds")
workers$sa3_home <- as.integer(substr(workers$SA1_MAINCODE_2016,1,5))
workLocationsSA1 <- read.csv("workLocationsSA1.csv") %>%
  select(sa1_maincode_2016,work_location_pr=sa3_pr)
workLocationsSA1<-data.table(workLocationsSA1)
setkey(workLocationsSA1, sa1_maincode_2016)
sa3DistCounterIndex <<- data.table(sa3=distanceMatrixIndexWork$sa3%>%unique()%>%sort())%>%mutate(index=row_number())

workLocationCounter<-readRDS("workLocationCounter_balanced.rds")
sa3DistCounter<-readRDS("sa3DistCounter_balanced.rds")
globalDistCounter<-readRDS("globalDistCounter_balanced.rds")
workers_sa1<-readRDS("workers_sa1_balanced.rds")

# workLocationCounter<-readRDS("workLocationCounter_local.rds")
# sa3DistCounter<-readRDS("sa3DistCounter_local.rds")
# globalDistCounter<-readRDS("globalDistCounter_local.rds")
# workers_sa1<-readRDS("workers_sa1_local.rds")

workLocationComparison <- workLocationsSA1 %>%
  inner_join(workLocationCounter,by="sa1_maincode_2016") %>%
  rename(pr_expected=work_location_pr) %>%
  mutate(pr_actual=count/sum(count)) %>%
  mutate(diff=abs(pr_expected-pr_actual)) %>%
  mutate(diff2=pr_expected-pr_actual)

sum(workLocationComparison$diff)
# hist(workLocationComparison$diff)

ggplot(workLocationComparison,aes(x=diff2)) +
  geom_histogram(binwidth=0.001) +
  scale_x_continuous(minor_breaks=NULL, n.breaks=4, labels=scales::percent_format(accuracy=1)) +
  theme(
    legend.title=element_blank(),
    legend.position="bottom",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-5,0,5,0),
    panel.spacing.x = unit(1, "lines"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 10),
    axis.ticks = element_line(size=0.2),
    panel.grid.major = element_line(size=0.2),
    panel.grid.minor = element_blank()) +
  labs(x = "Difference in proportion", y="Count")
ggsave(paste0("work-location-histogram_balanced.pdf"),width=6,height=4)



globalDistComparison <- work_hist_global %>%
  inner_join(globalDistCounter,by="distance") %>%
  rename(pr_expected=global_dist_pr) %>%
  mutate(pr_actual=count/sum(count)) %>%
  mutate(diff=abs(pr_expected-pr_actual)) %>%
  mutate(distance=0.5*distance-0.25)
sum(globalDistComparison$diff)

globalDistComparisonLong <- globalDistComparison %>%
  select(distance,expected=pr_expected,actual=pr_actual) %>%
  pivot_longer(cols=c(expected,actual),
               names_to="type",
               values_to="proportion") # %>% mutate(distance=ifelse(type=="expected",distance-0.125,distance+0.125))

globalDistPlot <- aggregateData(globalDistComparisonLong,1)


ggplot(globalDistPlot,aes(x=distance,y=proportion)) +
  geom_col(aes(fill=type,width=.5)) +
  scale_fill_manual(values=c('#009B95','#FF7100')) +
  scale_y_continuous(minor_breaks=NULL, n.breaks=4, labels=scales::percent_format(accuracy=1)) +
  scale_x_continuous(limits=c(0, 100)) +
  theme(
    legend.title=element_blank(),
    legend.position="bottom",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-5,0,5,0),
    panel.spacing.x = unit(1, "lines"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 10),
    axis.ticks = element_line(size=0.2),
    panel.grid.major = element_line(size=0.2),
    panel.grid.minor = element_blank()) +
  labs(x = "Distance traveled (km)", y="Proportion")
ggsave(paste0("distance-histogram_balanced.pdf"),width=6,height=4)

# 
# tmp<-sa3DistCounter[1:3,1:3,]
# tmp[1,1,]/sum(tmp[1,1,])
# 
# tmp3<-apply(tmp, c(1,2,3), function(x) mean(x))
# tmp3<-apply(sa3DistCounter, c(1,2), function(x) x/sum(x))
# tmp3<-apply(sa3DistCounter, c(1,2), function(x) ifelse(sum(x)>0,x/sum(x),x))
# 
# tmp2 <- apply(tmp, c(1,2), function(x) x/sum(x))

getPr <- function(x) {
  if(sum(x)==0) return(x)
  x<-x/sum(x)
  return(x)
}


sa3DistCounterTable <- sa3DistCounter %>%
  as.data.table() %>%
  inner_join(sa3DistCounterIndex%>%rename(sa3_home=sa3),by=c("V1"="index")) %>%
  inner_join(sa3DistCounterIndex%>%rename(sa3_work=sa3),by=c("V2"="index")) %>%
  mutate(range_value=0.5*V3-0.25) %>%
  group_by(sa3_home,sa3_work) %>%
  mutate(pr_actual=getPr(value)) %>%
  ungroup() %>%
  select(sa3_home,sa3_work,range_value,pr_actual) %>%
  inner_join(work_hist_sa3,by=c("sa3_home","sa3_work","range_value")) %>%
  select(sa3_home,sa3_work,range_value,pr_expected=pr,pr_actual) %>%
  mutate(diff=abs(pr_expected-pr_actual))
sum(sa3DistCounterTable$diff)
# hist(sa3DistCounterTable$diff)



plot1<- sa3DistCounterTable %>%
  filter(pr_expected!=0 & pr_actual!=0)
ggplot(plot1, aes(x=pr_expected, y=pr_actual)) + 
  geom_abline(aes(slope = 1, intercept=0),size=0.3) +
  geom_point(aes(fill=range_value), colour = 'NA', size=3, shape=21, alpha=0.01) + 
  labs(x="Expected probability", y="Actual probability", fill="Distance") + 
  guides(colour="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # facet_wrap(vars(facet_name), scales="free", ncol=4) +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) 
ggsave(paste0("sa3_dist_balanced-qq.pdf"), width=10, height=8, units = "in")
