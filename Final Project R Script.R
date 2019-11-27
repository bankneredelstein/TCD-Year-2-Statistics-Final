## Final Statistics and Computation Project ##
# Beaven Ankner-Edelstein 18302579 #

##Setup of working directory##
setwd("C:/Users/bankn/Desktop/R/Projects/Trinity Year 2/Final Stats Project")
getwd()

##Import data set##
data <- read.csv("C:/Users/bankn/Desktop/R/Projects/Trinity Year 2/Final Stats Project/Sorted DATA.csv")


################################################
##Linear Models of Data set##
##Do linear models explain the data and is there a correlation between number of prey and time to catch them all##

##Linear Model test of the control data vs. number of prey##
controllm <- lm( Time.to.Catch..sec.[Prey.Behavior == "Control"] ~ Number.of.Prey[Prey.Behavior == "Control"], data = data )
summary(controllm)
plot( Time.to.Catch..sec.[Prey.Behavior == "Control"] ~ Number.of.Prey[Prey.Behavior == "Control"], data = data )
abline(controllm)

##Comparison of control and avoidance behavior vs. number of prey##
avoidancelm <- lm( Time.to.Catch..sec.[Prey.Behavior == "Avoidance"] ~ Number.of.Prey[Prey.Behavior == "Avoidance"], data = data )
summary(avoidancelm)
plot( Time.to.Catch..sec.[Prey.Behavior == "Avoidance"] ~ Number.of.Prey[Prey.Behavior == "Avoidance"], data = data )
abline(avoidancelm)

##Comparison of control and attraction behavior vs. number of prey##
attractionlm <- lm( Time.to.Catch..sec.[Prey.Behavior == "Attraction"] ~ Number.of.Prey[Prey.Behavior == "Attraction"], data = data )
summary(attractionlm)
plot( Time.to.Catch..sec.[Prey.Behavior == "Attraction"] ~ Number.of.Prey[Prey.Behavior == "Attraction"], data = data )
abline(attractionlm)

##Comparison of control and freeze behavior vs. number of prey##
freezelm <- lm( Time.to.Catch..sec.[Prey.Behavior == "Freeze"] ~ Number.of.Prey[Prey.Behavior == "Freeze"], data = data )
summary(freezelm)
plot( Time.to.Catch..sec.[Prey.Behavior == "Freeze"] ~ Number.of.Prey[Prey.Behavior == "Freeze"], data = data )
abline(freezelm)


#################################################
##ANCOVA comparison of all linear models##
##Are there statistically significant differences in the linear models? Does behavior type affect the relationship of starting prey and time to catch them all?##

##Model with categorical and predictor variable interaction##
interaction1 <- aov( Time.to.Catch..sec. ~ Number.of.Prey*Prey.Behavior, data = data )
summary(interaction1)

##Model without categorical and predictor variable interaction##
interaction2 <- aov( Time.to.Catch..sec. ~ Number.of.Prey+Prey.Behavior, data = data )
summary(interaction2)

##Comparing the two models with ANOVA##
anova(interaction1, interaction2)


################################################
##ANOVA comparison of each set of linear models##
##Are there statistically significant differences between linear models? Tests here assume no interaction between categorical and predictor variables (prey behavior and number of starting prey), this is correct and was controlled for to ensure to relationship##

##ANOVA of Control/Avoidance##
controloravoidance <- (data$Prey.Behavior == "Control" | data$Prey.Behavior == "Avoidance")
controltoavoidance <- aov( Time.to.Catch..sec.[controloravoidance] ~ Number.of.Prey[controloravoidance]+Prey.Behavior[controloravoidance], data = data )
summary(controltoavoidance)

##ANOVA of Control/Attraction##
controlorattraction <- (data$Prey.Behavior == "Control" | data$Prey.Behavior == "Attraction")
controltoattraction <- aov( Time.to.Catch..sec.[controlorattraction] ~ Number.of.Prey[controlorattraction]+Prey.Behavior[controlorattraction], data = data )
summary(controltoattraction)

##ANOVA of Control/Freeze##
controlorfreeze <- (data$Prey.Behavior == "Control" | data$Prey.Behavior == "Freeze")
controltofreeze <- aov( Time.to.Catch..sec.[controlorfreeze] ~ Number.of.Prey[controlorfreeze]+Prey.Behavior[controlorfreeze], data = data )
summary(controltofreeze)

##ANOVA of Avoidance/Attraction##
avoidanceorattraction <- (data$Prey.Behavior == "Avoidance" | data$Prey.Behavior == "Attraction")
avoidancetoattraction <- aov( Time.to.Catch..sec.[avoidanceorattraction] ~ Number.of.Prey[avoidanceorattraction]+Prey.Behavior[avoidanceorattraction], data = data )
summary(avoidancetoattraction)

##ANOVA of Avoidance/Freeze##
avoidanceorfreeze <- (data$Prey.Behavior == "Avoidance" | data$Prey.Behavior == "Freeze")
avoidancetofreeze <- aov( Time.to.Catch..sec.[avoidanceorfreeze] ~ Number.of.Prey[avoidanceorfreeze]+Prey.Behavior[avoidanceorfreeze], data = data )
summary(avoidancetofreeze)

##ANOVA of Attraction/Freeze##
attractionorfreeze <- (data$Prey.Behavior == "Attraction" | data$Prey.Behavior == "Freeze")
attractiontofreeze <- aov( Time.to.Catch..sec.[attractionorfreeze] ~ Number.of.Prey[attractionorfreeze]+Prey.Behavior[attractionorfreeze], data = data )
summary(attractiontofreeze)


##########################################################
##Calculate average distance between regression lines##

##Create average distance finding function (finds average difference as a fraction of behavior over control times)##
avgdiff <- function(slope, testedlm) {

sequence <- seq(from=5, to=100, by=5)
ycontrol <- c(sequence*0.13466+coef(controllm)["(Intercept)"])
ybehavior <- c(sequence*slope+coef(testedlm)["(Intercept)"])

sum(ybehavior)/sum(ycontrol)
}

##Control and Avoidance##

slope <- 0.17603
testedlm <- avoidancelm
ans1 <- avgdiff(slope, testedlm)
ans1


##Control and Attraction##

slope <- 0.06342
testedlm <- attractionlm
ans2 <- avgdiff(slope, testedlm)
ans2


##Control and Freeze##

slope <- 0.10001
testedlm <- freezelm
ans3 <- avgdiff(slope, testedlm)
ans3

