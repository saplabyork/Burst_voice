data <- read.csv("/Users/chandannarayan/GitHub/Burst_voice/Data/data_1-26-24.csv", header=TRUE)
data$coda_vc <- as.factor(data$coda_vc) #change class of coda_vc
data$ons_poa <- as.character(data$ons_poa) #change class of ons_poa

oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)
library(lme4)


m1 <- lmer(vot ~ coda_vc + vdur + (1 | sub) + (1 | word), data = data, REML=FALSE)
print(m1, corr = FALSE)
