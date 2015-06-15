#
#  It uses the ToothGrowth data set which is built into R
# Based on a study to see the effects of vitaminC intake for guinea pigs
# Guinea Pigs, like humans, are dependent on consuming vitamin C
#
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
#
# it is a 60 x 3 data.frame
class(ToothGrowth)
dim(ToothGrowth)
#
# Data variables avaiable in the data set
#
names(ToothGrowth)
# len - tooth length
# supp - kind of supplement used
# dos - dosage (0.5, 1.0, 2.0)
#
# Two different supplements are used vitamin C and orange juice
# Two things to look into:
#
# 1) Effect of dosage on tooth growth
# 2) Effect of supplement on tooth growth
#
# Are they different for tooth growth?
#
# Things to look for:
# does the confidence interval contain 0
#
#
# Take a look at the data
#

ggplot(tooth_combo, aes(factor(dose), len)) + 
  geom_boxplot() +
  facet_grid(.~supp) +
  xlab("Dosage") +
  ylab("Tooth Length") +
  ggtitle("Vitamin C Supplement and Tooth Length in Guinea Pigs")


supp_types <- unique(ToothGrowth$supp)
dosage_type <- unique(ToothGrowth$dose)

#
# Get some summary data
#
summ_data <- data.frame()
data <- numeric()
for (i in supp_types){
    supp_subset <- subset(ToothGrowth, ToothGrowth$supp == i)
    
    for (k in dosage_type){
        dosage_subset <- subset(supp_subset, supp_subset$dose == k)
        
        t_mean <- mean(dosage_subset$len)
        t_std <- sd(dosage_subset$len)
        t_count <- nrow(dosage_subset)
        
        summ_data <- rbind(summ_data, data.frame(supp=i, dose=k, mean=t_mean, std=t_std, count=t_count))
        
        data <- cbind(data, dosage_subset$len)
        colnames(data)[ncol(data)] <- paste(i, k)
    }
}
#
# We use the t-distribution since it's small sample size
#

t_stats<-matrix(nrow=ncol(data), ncol=ncol(data))
colnames(t_stats) <- colnames(data)
rownames(t_stats) <- colnames(data)

#
# do pairwise t.test
categories <- colnames(data)
a<- data.frame()
for (i in categories){
    for (k in categories){
        t_stats[i,k] <- t.test(data[, i], data[,k], conf.level=0.95, paired=FALSE)$p.value
        if (t.test(data[, i], data[,k], conf.level=0.95, paired=FALSE)$p.value < 0.05){
            a <- rbind(a, data.frame(first=i, second=k))
        }
    }
}

format(t_stats, digits=4, nsmall=2, scientific=FALSE, truncate=TRUE)

alpha = 0.05

filter(ToothGrowth, supp = "VC")

tooth_combo<-cbind(ToothGrowth, combo = paste(ToothGrowth$supp, ToothGrowth$dose))

#
# See effect of Supplement and Dosage
boxplot(len ~ combo, tooth_combo, xlab = "Supplement Dosage", ylab="Tooth Length")

# Clearly, in both supplements, the higher the dosage the longer the tooth length.
# Using orange juice seems to show 

boxplot(len ~ dose, ToothGrowth, xlab="Dosage")
boxplot(len ~ supp, ToothGrowth, xlab="Supplement")

t.test(data[,"VC 0.5"], data[,"VC 1"], data=data)$p.value
