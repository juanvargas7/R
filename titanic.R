library(epiR)
library(epitools)
library(tidyverse)
library(lme4)
library(ggthemes)
library(lmtest)
library(ROCit)
library(car)

data("TitanicSurvival")

data = TitanicSurvival
data = na.omit(data) # remove all NAs

specificity <- function(z,a=0,denom,sens ){ 
  if (a == 1){
    return(print(paste0('TN = ',sens*denom)))
  }else{ return(z[1,1]/(z[1,1]+z[2,1]))
  }
}

sensitivity <- function(z,a=0,num,spec){  
  if (a == 1){
    return(print(paste0('TP = ',spec*num))) }else{
      return(z[2,2]/(z[1,2]+z[2,2]))
    }
}

#### EDA #####

c = xtabs(~survived + passengerClass, data = data)
c
chisq.test(c)



data %>%
  ggplot(aes(x = survived, y = age, fill = passengerClass)) +
    geom_boxplot() + labs(title = '') + ggthemes::theme_economist()

data  %>%
  ggplot(aes(x = passengerClass , y = age))+
    geom_boxplot()

data %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
    geom_bar(position = 'fill') + ggthemes::theme_economist()

data %>%
  ggplot(aes(x = age, fill = passengerClass))+
    geom_histogram(color = 'black') +
    facet_wrap(~passengerClass) + ggthemes::theme_economist()

data  %>% 
  ggplot(aes(x = age))+
    geom_histogram()

#####
summary(data)


str(data)
####

mod =glm(survived~ passengerClass + sex + age, family = binomial('logit'), data = data)
summary(mod)

sub = with(data, tibble(passengerClass = passengerClass, sex = sex , age = age))
sub$pred_prob = predict(mod, type = 'response')
sub$survived = data$survived
sub$pred = ifelse(sub$pred_prob > 0.5,'yes','no')

a = xtabs(~sub$survived+ sub$pred)

#####
# plot the model with predicted probabilities 

sub %>%
  ggplot(aes(x = 1:length(pred_prob), y = sort(pred_prob)))+
    geom_point()

#####
# eliminating age since it does not distribute normal

mod_2 = glm(survived ~ passengerClass + sex, data = data, family = binomial('logit'))
summary(mod_2)

lrtest(mod,mod_2)


sub$pred_prob_2 = predict(mod_2, type = 'response')
sub$pred_2 = ifelse(sub$pred_prob_2 > 0.5, 'yes','no')

b = xtabs(~sub$survived + sub$pred_2)

sensitivity(b)
specificity(b)

##### 
# we have to use the model with all the covariates

# Now lets add interaction terms


mod_3 = glm(survived~ sex+age+passengerClass+ age*passengerClass, data = data, family = binomial('logit'))
summary(mod_3)
sub$pred_prob_3 = predict(mod_3,type = 'response')
sub$pred_3 = ifelse(sub$pred_prob_3 > 0.5,'yes','no')

p = xtabs(~survived + pred_3, data = sub)
sensitivity(p)
specificity(p)

lrtest(mod_3, mod)





