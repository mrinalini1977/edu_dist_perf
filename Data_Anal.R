#2Overall_Literacy (%)
#3Fem_Literacy (%)	
#4SC_Enrol(%)	
#5ST_Enrol(%)
#6OBC_Enrol (%)	
#7Muslim_Enrol (%)	
#8Girl_ Boy_Ratio	
#9RTE_Score	
#10SC_Tch (%)
#11ST_Tch (%)	
#12Gender_ratio_Tch 	
#13Med_Instr_Non_Eng(%)	
#14Med_Instr_Eng(%)
#15Pvt_School (%)	
#16Not_In_School(%)	
#173to5_Can_read_1text(%)	
#183to5_Can_Subtract (%)

##### Import libraries #####
library(cluster)
library(factoextra)
library(NbClust)
library(sqldf)

##### Read data #####

setwd("C:/Jigsaw/education/cluster_analysis/District/code_0915")

input_data <- read.csv("District_Data.csv")

##### Explore Data #####

n= length(colnames(input_data))

dim(input_data)

str(input_data)

summary(input_data)

colSums(is.na(input_data))

# overall literacy (1 outlier )
boxplot(input_data$Overall_Literacy....)
summary(input_data$Overall_Literacy....)

#female literacy
boxplot(input_data$Fem_Literacy....)
summary(input_data$Fem_Literacy....)

#SC enrolment
boxplot(input_data$SC_Enrol...)
summary(input_data$SC_Enrol...)

#ST enrolment
boxplot(input_data$ST_Enrol...)
summary(input_data$ST_Enrol...)

#OBC enrolment ( two outliers - fixed, data issue )
boxplot(input_data$OBC_Enrol....)
summary(input_data$OBC_Enrol....)

#Muslim enrolment ( three outliers )
boxplot(input_data$Muslim_Enrol....)
summary(input_data$Muslim_Enrol....)

#girl boy ratio ( 2 outliers )
boxplot(input_data$Girl_.Boy_Ratio)
summary(input_data$Girl_.Boy_Ratio)

# RTE Score (one outlier )
boxplot(input_data$RTE_Score)
summary(input_data$RTE_Score)

# Teacher gender ratio ( 2 outlers )
boxplot(input_data$Gender_ratio_Tch)
summary(input_data$Gender_ratio_Tch)

# Medium Instruction Non Englsih
boxplot(input_data$Med_Instr_Non_Eng...)
summary(input_data$Med_Instr_Non_Eng...)

# Medium Instruction Englsih
boxplot(input_data$Med_Instr_Eng...)
summary(input_data$Med_Instr_Eng...)

# Not in School (1 outlier )
boxplot(input_data$Not_In_School...)
summary(input_data$Not_In_School...)

#Private School ( 1 outlier )
boxplot(input_data$Pvt_School...)
summary(input_data$Pvt_School...)

# read ( 1 outlier )
boxplot(input_data$X3to5_Can_read_1text...)
summary(input_data$X3to5_Can_read_1text...)

# subtract
boxplot(input_data$X3to5_Can_Subtract....)
summary(input_data$X3to5_Can_Subtract....)

##### Prepare Data #####

## ASER Data for the year 2014 was missing for Chikkaballapur,
# Ramanagar and Yadgiri. The mssing data replaced with ASER data from 2016

##Subsetting the data ##
#The subset does not include the column for district names#

input_data_sub <- input_data[, -1]

  
## Scaling the data ##
input_data_scaled <- scale(input_data_sub)

## adding weights to reading and maths performance colums ##

input_data_scaled[,16] = 3 * input_data_scaled[,16] #3to5_Can_read_1text(%)

input_data_scaled[,17] = 3 * input_data_scaled[,17] #3to5_Can_Subtract (%)

input_data_scaled[,6] = .2 * input_data_scaled[,6] #3to5_Can_Subtract (%)
##

##### Create Model #####
# Hierarchical clustering is used for creating the model. #

## Finding optimal number of clusters ##

#elbow method
fviz_nbclust(input_data_scaled,  hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Average silhouette method #

fviz_nbclust(input_data_scaled, hcut, method = "silhouette",
             hc_method = "ward.D2")

#gap statistics
# Compute gap statistic
set.seed(123)
gap_stat <- clusGap(input_data_scaled, FUN = hcut, K.max = 5, B = 60)

# Plot gap statistic
fviz_gap_stat(gap_stat)

##Converting distances into a matrix and writing it into a csv file ##
d <- dist(input_data_scaled, method = "euclidean")
write.csv(as.matrix(d), "distmatrix.csv", row.names = F)


## Applying the algorithm ##

clust <-hclust (d, method = "ward.D2")
clust$labels <- input_data$District

## Plotting dendogram ##

plot(as.dendrogram(clust))
rect.hclust(clust, 3)

## Slicing the dendogram to get finite number of clusters ##

k <- cutree(clust, k=3)
head(k)

## attach it to the original dataset and write it to a csv file ##
input_data$cluster <- k
write.csv(input_data, "results_clust.csv")


##### Profiling #####

## population summaries ##
options(scripen = 999)
popMean <- apply(input_data[,2:18],2,mean)
popSD <- apply(input_data[,2:18],2,sd)

pmeans <- data.frame(matrix(0, ncol = 18, nrow = 1))

names(pmeans) = names(c("Summary",input_data_sub) )
pmeans <- rbind(pmeans, z= c("Mean", popMean))
pmeans <- rbind(pmeans, z= c("Std.Dev", popSD))

pmeans <- pmeans[-1,]

write.csv(pmeans,"pmeans.csv",row.names = F)

## Clusterwise summaries ##
cmeans <- aggregate(input_data[,2:18], by = list(input_data$cluster), FUN = mean)

clust_col_list <- names(cmeans)

#value normalization 
# Function to calculate Z values #

for(i in 1:length(clust_col_list))
{
  y <- (cmeans[,i+1] - apply(input_data[,2:18],2,mean)[i])/ (apply(input_data[,2:18],2,sd)[i])
  cmeans <- cbind(cmeans,y)
  names(cmeans)[i+18] <- paste("z", clust_col_list[i+1], sep="_")
  print(clust_col_list[i+1])
}

cmeans <- cmeans[,-(2*18)]
write.csv(cmeans,"cmeans.csv",row.names = F)

## Number of records in each cluster ##
sqldf("select count(*) from input_data group by cluster")

