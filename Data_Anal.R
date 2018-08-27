##### Import libraries #####
library(cluster)
library(factoextra)
library(NbClust)
library(sqldf)
library(rgdal)
library(maptools)
library(ggplot2)
library(tmap)
library(sp)
library(ggmap)
library(raster)

##### Read data #####

setwd("C:/work/Cluster_Analysis/Karnataka/primary_education_cluster_analysis")
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

input_data_scaled <- as.data.frame (scale(input_data[,-1]))

class(input_data_scaled)

row.names(input_data_scaled) <- input_data$District


## adding weights to reading and maths performance colums ##

input_data_scaled[,18] = 3 * input_data_scaled[,18] #3to5_Can_read_1text(%)

input_data_scaled[,19] = 3 * input_data_scaled[,19] #3to5_Can_Subtract (%)

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
d <- get_dist(input_data_scaled, method = "euclidean")
#write.csv(as.matrix(d), "distmatrix.csv", row.names = F)
## For Visualising the distance matrix
fviz_dist(d,lab_size = 8)

## Applying the algorithm ##

clust <-hclust (d, method = "ward.D2")

## Plotting dendogram ##

plot(as.dendrogram(clust))
rect.hclust(clust, 3)

## Slicing the dendogram to get finite number of clusters ##

groups <- cutree(clust, k=3)
values_cat <- c("Tier 2", "Tier 3", "Tier 1")

## attach cluster number and cluster category to the original dataset  ##
input_data$group <- groups

#index <- c(1, 2, 3)
input_data$clust_cat <- values_cat[input_data$group]


## create a data frame with district, cluster number and cluster category and write it to a csv file.
perf_data <- data.frame(input_data$District, input_data$group, input_data$clust_cat)
colnames(perf_data) <-  c("District", "cluster_num", "cluster_cat")

write.csv(perf_data, "clust_info.csv")

## scatter plot to visualise the clusters
hc.cut <- hcut(input_data_scaled, k=3, hc_method = "ward.D2")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE, show_labels = TRUE)

fviz_cluster(list(data = as.matrix(input_data_scaled), cluster = input_data$clust_cat), 
             palette = "Set2", ggtheme = theme_minimal(), labelsize = 8)


##### Profiling #####

## population summaries ##
options(scripen = 999)
popMean <- apply(input_data[,2:20],2,mean)
popSD <- apply(input_data[,2:20],2,sd)

pmeans <- data.frame(matrix(0, ncol = 20, nrow = 1))

names(pmeans) = names(c("Summary",input_data_sub) )
pmeans <- rbind(pmeans, z= c("Mean", popMean))
pmeans <- rbind(pmeans, z= c("Std.Dev", popSD))

pmeans <- pmeans[-1,]

write.csv(pmeans,"pmeans.csv",row.names = F)

## Clusterwise summaries ##
cmeans <- aggregate(input_data[,2:20], by = list(input_data$group), FUN = mean)

clust_col_list <- names(cmeans)

#value normalization 
# Function to calculate Z values #

for(i in 1:length(clust_col_list))
{
  y <- (cmeans[,i+1] - apply(input_data[,2:20],2,mean)[i])/ (apply(input_data[,2:20],2,sd)[i])
  cmeans <- cbind(cmeans,y)
  names(cmeans)[i+20] <- paste("z", clust_col_list[i+1], sep="_")
  print(clust_col_list[i+1])
}

cmeans <- cmeans[,-(2*20)]
write.csv(cmeans,"cmeans.csv",row.names = F)

## Number of records in each cluster ##
sqldf("select clust_cat, count(*) as count from input_data group by clust_cat")




#################### Visualization###############################

karnataka.rg <- readOGR(dsn = "shapeFile/district_Output.shp", "district_Output")

DIST <- karnataka.rg$DIST

file_csv<- read.csv("clust_info.csv")

# merge on common variable, here called 'key'
m <- merge(karnataka.rg, file_csv, by.x='DIST', by.y="District")
m$DIST = as.factor(m$DIST)

shapefile(m, "shapeFile/merged_district_Output.shp", overwrite = TRUE)

karnataka.rg_m <- readOGR(dsn = "shapeFile/merged_district_Output.shp", "merged_district_Output")

tm_shape(karnataka.rg_m) + tm_polygons("clstr_c", aes.palette = "hclust", palette = "-Blues", contrast = .5, title = "Learning Outcome Clusters", legend.show = TRUE ) + 
tm_layout(  legend.title.size = 1.5,
            legend.text.size = 0.5,
            legend.outside = FALSE,
            legend.position = c(0,.82),
            legend.bg.color = "white",
            legend.bg.alpha = .5
            )  +
tm_text("DIST", size = "AREA", scale  = .6)  


