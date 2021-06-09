###################################################################### 

## Code author: Steven Kerr

## Description: 
# GMM clustering on PHOSP data

###################################################################### 

library(tidyverse)
library(mclust)
library(ggplot2)
# library(data.table)

################## 

diffVars <- c('eq5d5l_q1_delta', 'eq5d5l_q2_delta', 'eq5d5l_q3_delta', 'eq5d5l_q4_delta' , 'eq5d5l_q5_delta', 
              'patient_sq_l_t_seeing_delta', 'patient_sq_l_t_hearing_delta', 'patient_sq_l_t_walking_delta',            
              'patient_sq_l_t_remembering_delta', 'patient_sq_l_t_self_care_delta', 'patient_sq_l_t_communicate_delta',
              'psq_scale_blness_delta', 'psq_scale_fatigue_delta', 'psq_scale_cough_delta', 'psq_scale_pain_delta',                  
              'psq_scale_sleep_delta')

###################### FUNCTIONS #######################################

zscoreInverse <- function(df){
  for(col in colnames(df)){
    df[col] <-  df[col] * meanSD['SD', col] + meanSD['Mean', col]
  }
  return(df)
}

plotMeans <- function(clustering){
  clusterMeans <- as.data.frame( t( as.data.frame( summary(clustering, parameters = TRUE)['mean'] )))
  
  #  clustermeans <- zscoreInverse(clusterMeans)
  
  measureVars <- colnames(clusterMeans)
  clusterMeans['Cluster'] <- 1:nrow(clusterMeans)
  
  clusterMeans['variable'] <- rownames(clusterMeans)
  
  clusterMeans <- as.data.table(clusterMeans)
  
  clusterMeans <- melt( clusterMeans, id.vars = 'Cluster', 
                        value.name="value", measure.vars = measureVars  )
  
  clusterMeans$Cluster <- as.factor(clusterMeans$Cluster)
  
  ggplot(clusterMeans, mapping = aes( x = variable, y= value, color = Cluster, group=Cluster) ) + geom_line() + theme_minimal() + coord_flip()
}


########################## CLUSTERING ######################################

complete <- drop_na(phosp_3m[c('study_id', diffVars)])

# clustering <- Mclust(complete[diffVars])
#
# plot(clustering, what = "BIC", ylim = range(clustering$BIC[,-(1:2)], na.rm = TRUE),legendArgs = list(x = 'bottomright'))
# 
# for(ncluster in 2:4){
#   clustering <- Mclust( drop_na( complete[diffVars] ), ncluster)
#   
#   print(summary(clustering))
#   
#   print(plotMeans(clustering))
#   
#   plot(MclustDR(clustering), what = "contour")
# }

clustering3 <- Mclust(complete[diffVars], G = 3)

complete['cluster'] <- predict(clustering3, drop_na( complete[diffVars] ) )$classification

# Join back with phosp_3m ------------------------------------------------
phosp_3m = phosp_3m %>% 
  left_join(complete %>% select(study_id, cluster), by = "study_id") %>% 
  mutate(cluster = factor(cluster) %>% 
           ff_label("Patient cluster"))

rm(complete, clustering3, diffVars, plotMeans, zscoreInverse)
