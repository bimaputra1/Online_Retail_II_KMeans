#################################################
## "Online Retail II RFM K-Means Clustering"
#################################################
## HarvardX: PH125.9x - Capstone Project
## by: Bima Putra Pratama


### DATA SET ------------------------

## DATA SET PREPARATION

# Prepare Required Library
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", 
                                      repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", 
                                      repos = "http://cran.us.r-project.org")
# Import Dataset
files <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00502/online_retail_II.xlsx", files)
df <- read_excel(files, sheet = 'Year 2010-2011', col_names = TRUE)
df <- df %>% rename(CustomerID = `Customer ID`) # Rename CustomerID Column Names
glimpse(df)

# Summary Data
summary(df)

## CLEAN UP DATA SET

# Remove Rows that have Negative Values of Quantity and Price
clean_df <- df %>%
  filter(Quantity > 0 & Price > 0) %>% 
  drop_na()

# Recode Dataset
Recode_df <- clean_df %>% 
  mutate(Invoice = as.factor(Invoice), StockCode = as.factor(StockCode),
         InvoiceDate = date(InvoiceDate), CustomerID = as.factor(CustomerID), 
         Country = as.factor(Country))

summary(Recode_df)

# Calculate Total Spend
Recode_df <- Recode_df %>% 
  mutate(TotalSpend = Quantity * Price)

summary(Recode_df)

## RFM Data Preparation
 
# Get Analysis Reference Date (One Day After Last Transaction)
Max_Date <- date(max(Recode_df$InvoiceDate)) + 1

# Calculate RFM Values 
rfm_df <- Recode_df %>% 
  group_by(CustomerID) %>% 
  summarise(recency = as.numeric(Max_Date - max(InvoiceDate)),
            frequency = n_distinct(Invoice), monetary = sum(TotalSpend)) 

head(rfm_df)

# Create RFM distribution Plot
rfm_df %>% 
  gather(type,value,recency:monetary) %>% 
  ggplot(aes(x = value, color = type, fill = type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~type, nrow = 1, scales="free") +
  labs(title = 'RFM Data Distribution')


### Method and Analysis --------------------

# Log transform dataset
log_rfm <- rfm_df %>% 
  mutate(log_recency = log(recency), log_frequency = log(frequency), log_monetary = log(monetary))

# Create Log RFM Data Distribution Plot
log_rfm %>% 
  gather(type,value,log_recency:log_monetary) %>% 
  ggplot(aes(x = value, color = type, fill = type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~type, nrow = 1, scales="free") +
  labs(title = 'Log RFM Data Distribution')

# Scaling Data
scale_df <- log_rfm %>% 
  mutate(scale_log_r = scale(log_recency), scale_log_f = scale(log_frequency), scale_log_m = scale(log_monetary))

# Create Scale Data Distribution Plot
scale_df %>% 
  gather(type,value,scale_log_r:scale_log_m) %>% 
  ggplot(aes(x = value, color = type, fill = type)) +
  geom_density(alpha = 0.6) +
  labs(title = 'Scaled Log RFM Data Distribution')

# Iterate from 1 to 15 to find the most optimum cluster by elbow curve and generate plot
set.seed(100)
used_var = c("scale_log_r","scale_log_f","scale_log_m")
sse <- sapply(1:15, 
              function(k)
              {
                kmeans(x=scale_df[used_var], k, nstart=25)$tot.withinss
              }
)

# Generate Plot
plot(sse, type = "o", xlab = "n - cluster", main = 'Elbow Curves')

# Calculate Cluster group with 4 cluster
segment_4 <- kmeans(x=scale_df[used_var], 4, nstart=25)
cluster <- as.factor(segment_4$cluster)
rfm_clustered4 <- cbind(scale_df,cluster)

rfm_clust4_summary <- rfm_clustered4 %>% 
  group_by(cluster) %>% 
  summarise(total = n_distinct(CustomerID), 
            average_recency = round(mean(recency),2),
            average_frequency = round(mean(frequency),2),
            average_monetary = round(mean(monetary),2)
  )

rfm_clust4_summary %>% knitr::kable()

# Create Barplot Cluster Characteristics
rfm_clust4_summary %>% 
  gather(key = 'measure', value = 'values',c(5,4,3,2)) %>% 
  ggplot(aes(x = cluster , y = values, fill = cluster)) +
  geom_col() +
  facet_wrap(~measure, ncol = 1, scales="free_y")

# Create Snake Plot of 4 Clusters
rfm_clustered4 %>% 
  group_by(cluster) %>% 
  summarise(average_recency = round(mean(scale_log_r),2),
            average_frequency = round(mean(scale_log_f),2),
            average_monetary = round(mean(scale_log_m),2)) %>% 
  ggparcoord(columns = 2:4, groupColumn = 'cluster',
             showPoints = TRUE, 
             alphaLines = 0.3, title = "Cluster Snakeplot")

### Result ---------------------

# Cluster Summary
rfm_clust4_summary %>% knitr::kable()
