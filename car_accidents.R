# data source: https://github.com/fivethirtyeight/data/tree/master/bad-drivers

packs <- c('tidyverse','GGally')

for(p in packs){
  if(!require(p, character.only = T)){
    install.packages(p)
    library(p, character.only = T)
  }
}

# Check the name of the current folder
current_dir <- getwd() 
print(current_dir)

# List all files in this folder
file_list <- list.files(current_dir)
print(file_list)

# List files inside the datasets folder
file_list_ds <- list.files(path=current_dir)
print(file_list_ds)

# View the first 20 lines of road-accidents.csv in the datasets folder
accidents_head <- readLines(file_list_ds[1],20)
print(accidents_head)

# Read in road-accidents.csv and set the comment argument
car_acc <- read_delim('road-accidents.csv', delim='|', comment='#')

# Save the number of rows columns
rows_and_cols <- dim(car_acc)
print(rows_and_cols)

# Generate an overview of the data frame
str(car_acc)

# Display the last six rows of the data frame. 
tail(car_acc,6)

# Compute summary statistics of all columns in the car_acc data frame
dat_summ <- summary(car_acc)
print(dat_summ)

# Deselect the state column and create a pairwise scatterplot
car_acc %>% select(-state) %>% 
  ggpairs(columns=1:4)

# Using pipes, remove the state column and then compute the correlation coefficient for all column pairs 
corr_col <- car_acc %>% select(-state) %>% cor()

# Print the correlation coefficient for all column pairs
print(corr_col)

# Use lm to fit a multivariate linear regression model 
fit_reg <- lm(drvr_fatl_col_bmiles~perc_fatl_speed+perc_fatl_alcohol+
                perc_fatl_1st_time, data=car_acc)
summary(fit_reg)
# Retrieve the regression coefficients from the model fit
fit_coef <- coef(fit_reg)
print(fit_coef)

# Center and standardise the three feature columns
car_acc_standised <- car_acc %>% 
  mutate(perc_fatl_speed=scale(perc_fatl_speed),
         perc_fatl_alcohol=scale(perc_fatl_alcohol),
         perc_fatl_1st_time=scale(perc_fatl_1st_time) )

# Perform PCA on standardized features
pca_fit <- princomp(car_acc_standised[,3:5])

# Obtain the proportion of variance explained by each principle component
pr_var <- pca_fit$sdev^2
pve <- pr_var / sum(pr_var)

# Plot the proportion of variance explained, draw a point plot connected with lines
data_frame( comp_id=1:length(pve) , pve ) %>%
  ggplot( aes(x=comp_id , y=pve) ) + geom_point() + geom_line() +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Principal Component", 
       y="Proportion of Variance Explained")

# Compute the cumulative proportion of variance and extract the variance
# explained by the first two principal components
cve <- cumsum(pve)
cve_pc2 <- cve[1:2]
print(cve_pc2)

# Get the principle component scores from the PCA fit
pcomp1 <- pca_fit$scores[,1]
pcomp2 <- pca_fit$scores[,2]

# Plot the first 2 principle components in a scatterplot using ggplot
data_frame(pcomp1,pcomp2) %>%
  ggplot(aes(pcomp1,pcomp2)) + geom_point()

# Create a vector of 1 to 10 
k_vec <- c(1:10)

# Initialise vector of inertias
inertias <- rep(NA,times=10)

# Initialise empty list to save K-mean fits 
mykm <- list()

# Set the seed of random number generator 
set.seed(1)
for (k in k_vec) {
  # for each k, fit a K-mean model with k clusters and save it in the mykm list
  mykm[[k]] <- kmeans(car_acc_standised[,c(3,4,5)], centers=k, nstart=50)
  # for each k, get the within-cluster sum-of-squares and save
  inertias[k] <- mykm[[k]]$tot.withinss             
}

# Plot the within-cluster sum-of-squares against the number of clusters used
data_frame(k_vec,inertias) %>%
  ggplot( aes(k_vec,inertias) ) +
  geom_point() + geom_line() +
  labs(x="Number of clusters", y="Intertias")

# Obtain cluster-ids from the kmeans fit with k=3
cluster_id <- as.factor(mykm[[3]]$cluster)

# Color the points of the principle component plot according to their cluster number
data_frame(pcomp1,pcomp2) %>%
  ggplot(aes(x=pcomp1,y=pcomp2)) + geom_point(aes(col=cluster_id)) +
  labs(x="Principle Component 1",
       y="Principle Component 2") 

# Add cluster_id to the original data frame
car_acc$cluster <- cluster_id

# Get the data into long format and plot
car_acc %>%
  select(-drvr_fatl_col_bmiles) %>% 
  gather(key=feature, value=percent, -state, -cluster) %>%
  ggplot(aes(x=feature,y=percent, fill=cluster)) +
  geom_violin() +
  coord_flip()

# Read in the miles-driven.csv file
miles_driven <- read_delim( file="miles-driven.csv", delim = '|' )

# Join miles_driven with car_acc and add num_drvr_fatl_col 
carr_acc_joined <- car_acc  %>% 
  left_join(miles_driven, by='state')  %>% 
  mutate(num_drvr_fatl_col= drvr_fatl_col_bmiles*million_miles_annually/1000)

# Group the new data frame, select relevant variables, and summarise 
carr_acc_joined_summ <- carr_acc_joined %>%
  group_by(cluster) %>%
  select(cluster,num_drvr_fatl_col) %>%
  summarise(count=n(),
            mean=mean(num_drvr_fatl_col),
            sum=sum(num_drvr_fatl_col))
print(carr_acc_joined_summ)

# Compare the total fatal accident sum across clusters using a bar plot
carr_acc_joined_summ %>%
  ggplot(aes(x=cluster, y=sum)) +
  geom_bar(aes(fill = cluster), stat = 'identity', show.legend = F)
