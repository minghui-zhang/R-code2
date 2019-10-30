pitch = c(233, 204, 242, 130, 112, 142)
sex = c(rep("female", 3), rep("male", 3))

my.df = data.frame(sex, pitch)

xmdl = lm(pitch ~ sex, my.df)
summary(xmdl)

plot(fitted(xmdl), residuals(xmdl))

# distinguish between the significance of the overall model (p value at bottom of the output)
# which considers all effects together from the p-value of individual coefficients

# the intercept is the mean of one of the fixed effect groups, female
# the estimate for sexmale is the adjustment needed for the mean of the male group

# if you subtract mean from all the predictors and then do lm, the intercept will be
# the predicted y at average x, instead of predicted y at x = 0 when it's not de-meaned

# assumptions of the model
# 1. linearity: if not linear, residual plot will have a curve or other pattern 
#    residual plot = fitted values (predicted values) vs residual
#    plot(fitted(xmdl), residuals(xmdl))
# 2. not collinear: two predictors aren't correlated with each other
# 3. homoscedasticity: variance of data is appropximately equal across the range
#    of predicted values
#    residuals of the model need to have similar amount of deviation from predicted values
#    check by looking at residual plot (it shouldn't be a fan shape)
# 4. normality of residuals.
#    hist(residuals(xmdl)) or qqnorm(residuals(xmdl))
# 5. no influential data points
#    check with dfbeta: dfbeta(xmdl). for each coefficient of your model, gives 
#    DFbeta values. these are the values with which the coefficients have to be
#    adjusted if a particular data point is excluded
#    any point that changes the slope is definitely influential
# 6. independence: each data point comes from a different subject
#    violating independence will inflate chance of finding a spurious result
#    responses from the same subject aren't independent of each other

# mixed effects model:
# add random effect for each subject to retain independence
# assume a different 'baseline' for each subject
# model differences by assuming different random intercepts for each subject
# R notation for random effect: (1|subject), where 1 means intercept and 'subject'
# means 'assume an intercept that's different for each subject"

# mixed models in R
library(lme4) # lmer() is mixed model equivalent of lm()

# boxplot that interacts two variables
boxplot(frequency ~ attitude*gender,
        col = c("white", "lightgray"), politeness)

politeness.model <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data = politeness) 
# note will get error if don't include random effect (1|subject), etc

summary(politeness.model)
# look at random effects section. the StdDev column is a measure of how much
# variability in the dependent measure there is due to scenarios and 
# subjects (our two random effects)
# look at fixed effects section. this output mirrors the table we get from lm

# statistical significance of mixed models
# Likelihood = probability of seeing hte data you collected given your model
# likelihood ratio test: compares the likelihood of two models with each other
# first construct the null model:
politeness.null = lmer(frequency ~ gender +
                         (1|subject) + (1|scenario), data=politeness,
                       REML=FALSE) # REML is for comparing models for likelihood ratio test
# then do full model
politeness.model = lmer(frequency ~ attitude +
                          gender + (1|subject) + (1|scenario),
                        data=politeness, REML=FALSE)
# do likelihood ratio test with anova:
anova(politeness.null, politeness.model)
# get a p value from comparing the two models

# can also compare interactions:
# frequency ~ attitude*gender vs
# frequency ~ attitude + gender
# if compare these two using anova, get a p value that gives the significance 
# of the interaction

# above, it was random intercept model, where the slopes are the same
# we can have random slope model, where can have different slopes AND different intercepts

# random slope model:
politeness.model = lmer(frequency ~ attitude +
                          gender + (1+attitude|subject) +
                          (1+attitude|scenario),
                        data=politeness,
                        REML=FALSE)
# (1 + attitude | subject) means the model will expect differing baseline-levels
# of frequency (the intercept, represented by 1) as well as differing responses to the main
# factor in question, which is 'attitude' in this case

coef(politeness.model)

# compare random slope model to null model, which has same random effects structure
# if your full model is a random slope model, your null model also needs to be
# a random slope model
politeness.null = lmer(frequency ~ gender +
                         (1+attitude|subject) + (1+attitude|scenario),
                       data=politeness, REML=FALSE)

# likelihood ratio test
anova(politeness.null,politeness.model)

# in general, include all random slopes that are justified by your experimental design

# mixed models can also violate independence if you're missing important fixed
# or random effects

# for mixed models, dfbeta() for lm WONT'T work to check influential points
# for mixed models, use package influence.ME, or code yourself:
all.res=numeric(nrow(mydataframe))
for(i in 1:nrow(mydataframe)){
  myfullmodel=lmer(response~predictor+
                     (1+predictor|randomeffect),POP[-i,])
  all.res[i]=fixef(myfullmodel)[some number]
}
