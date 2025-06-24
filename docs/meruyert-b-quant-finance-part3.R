library(readr)
library(urca)
PEP <- read_csv("Desktop/AD 685/PEP.csv")
View(PEP)

Return_PEP <- PEP$Return_PEP
Return_KO <- PEP$Return_KO

model_p3 <- lm (Return_PEP ~ Return_KO, data = PEP)
summary(model_p3)

residuals <- residuals(model_p3)
eg_adf_test <- ur.df(residuals, type = "none", selectlags = "AIC")
summary(eg_adf_test)