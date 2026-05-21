
install.packages("readxl")
library(readxl)
setwd("C:/FINAL PROJECT MULTI")
cancerprs <- read_excel("can-24-1943_table_s6_suppst6.xlsx", skip = 2)

names(cancerprs) <- c("Patient_ID", "Gene_Symbol", "Pre_Tx_CCF", 
                      "Post_Tx_CCF", "CCF_change", "CCF_status")
head(cancerprs)
names(cancerprs)

library(dplyr)
library(ggplot2)
library(psych)


total_patients <- length(unique(cancerprs$Patient_ID))
total_observations <- nrow(cancerprs)

cat("Total patients:", total_patients, "\n")
cat("Total observations:", total_observations, "\n")


cancerprs %>%
  summarise(
    pre_mean = mean(Pre_Tx_CCF, na.rm = TRUE),
    pre_sd = sd(Pre_Tx_CCF, na.rm = TRUE),
    pre_median = median(Pre_Tx_CCF, na.rm = TRUE),
    pre_IQR = IQR(Pre_Tx_CCF, na.rm = TRUE),
    
    post_mean = mean(Post_Tx_CCF, na.rm = TRUE),
    post_sd = sd(Post_Tx_CCF, na.rm = TRUE),
    post_median = median(Post_Tx_CCF, na.rm = TRUE),
    post_IQR = IQR(Post_Tx_CCF, na.rm = TRUE),
    
    change_mean = mean(CCF_change, na.rm = TRUE),
    change_sd = sd(CCF_change, na.rm = TRUE),
    change_median = median(CCF_change, na.rm = TRUE),
    change_IQR = IQR(CCF_change, na.rm = TRUE)
  )

table(cancerprs$CCF_status)


cancerprs_long <- cancerprs %>%
  tidyr::pivot_longer(cols = c(Pre_Tx_CCF, Post_Tx_CCF),
                      names_to = "Timepoint",
                      values_to = "CCF")

ggplot(cancerprs_long, aes(x = Timepoint, y = CCF)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Cancer Cell Fraction (CCF) Before and After Chemotherapy",
       x = "Timepoint",
       y = "Cancer Cell Fraction")



cancerprs_long <- cancerprs %>%
  pivot_longer(cols = c(Pre_Tx_CCF, Post_Tx_CCF),
               names_to = "Timepoint",
               values_to = "CCF") %>%
  mutate(Timepoint = factor(Timepoint, 
                            levels = c("Pre_Tx_CCF", "Post_Tx_CCF"),
                            labels = c("Pre-Treatment", "Post-Treatment")))

# Create trajectory plot
ggplot(cancerprs_long, aes(x = Timepoint, y = CCF, group = Patient_ID)) +
  
  
  # Mean trend line with 95% confidence band
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               color = "steelblue",
               size = 1.2) +
  stat_summary(aes(group = 1),
               fun.data = mean_cl_normal,
               geom = "ribbon",
               fill = "skyblue",
               alpha = 0.3) 

library(ICC)
iccre <- ICCest(x = "Patient_ID", y = "CCF_change", data = cancerprs)
print(iccre)


