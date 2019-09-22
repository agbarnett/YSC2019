# sufficiency.YSC.R
# regression using sufficient statistics a la Brown and Ryan, DOI: 10.1080/00031305.2016.1255659
# version for the course at the Young Stats Conference in September 2019
# Sep 2019
library(dplyr) # for filter, mutate, etc
library(forcats) # for factor recoding
library(stringr) # for str_detect
library(tidyr) # for switch from long to wide format
library(lme4) # for regression model
library(ggplot2) # for plots

#### Section 0: data management ####

# import the data
data = read.csv('data/sa4datafilled.csv', stringsAsFactors = FALSE) %>%
  mutate(statenum = 0)

## data management
# a) add state by finding last case of "No usual address"
changes = which(str_detect(string=data$SA4, pattern='No Usual Address'))
breaks = which(diff(changes)!=1)
breaks = changes[breaks] + 1
data$statenum[breaks] = 1
states = c('NSW','VIC','QLD','SA','WA','TAS','NT','ACT')
data = mutate(data, statenum = cumsum(statenum) + 1,
               state = factor(statenum, levels=1:8, labels=states))
# b) filter data and make nicer factors
data = filter(data, 
               Count > 0,  # only counts above zero
               !str_detect(string=SA4, pattern='Migratory')) %>% # exclude Migratory SA4
  mutate(
    # age
    Age = relevel(factor(Age), ref='25-29 years'), # set reference level
    # Sex
    Sex = relevel(factor(Sex), ref='Female'), # set reference level
    # Sex
    state = relevel(state, ref='ACT'), # set reference level
    # Recode Indigenous;
    Indig = fct_recode(Indig, 
                       "Indigenous" = "Aboriginal",
                       "Indigenous" = "Torres Strait Islander",
                       "Indigenous" = "Both Aboriginal and Torres Strait Islander"),
    Indig = relevel(factor(Indig), ref='Non-Indigenous'), # set reference level
    # Recode eduction:
    Educ = fct_recode(Educ, 
                      'Degree = Yes' = 'Postgraduate Degree Level',
                      'Degree = Yes' = 'Graduate Diploma and Graduate Certificate Level',
                      'Degree = Yes' = 'Bachelor Degree Level' ,
                      'Degree = No' = 'Advanced Diploma and Diploma Level',
                      'Degree = No' = 'Certificate III & IV Level',
                      'Degree = No' = 'Secondary Education - Years 10 and above',
                      'Degree = No' = 'Certificate I & II Level',
                      'Degree = No' = 'Secondary Education - Years 9 and below',
                      'Degree = No' = 'Supplementary Codes',
                      'Degree = No' = 'Not stated',
                      'Degree = No' = 'Not applicable'),
    # Recode LFS to employed and unemployed;
    LFS_Status = fct_recode(LFS_Status,
                            'employed' = 'Employed, worked full-time',
                            'employed' = 'Employed, worked part-time',
                            'employed' = 'Employed, away from work',
                            'unemployed' = 'Unemployed, looking for full-time work',
                            'unemployed' = 'Unemployed, looking for part-time work')				
)

# Collapse data for standard model (sum counts) 
for.model = group_by(data, state, Age, Sex, Indig, Educ, LFS_Status) %>%
  summarise(sum = sum(Count)) %>%
  ungroup() %>%
  spread(LFS_Status, sum) %>% # switch to wide format with side-by-side results for employed and unemployed
  mutate(unemployed = ifelse(is.na(unemployed), 0, unemployed), # replace any missing with zero
    total = employed + unemployed) # make total

#### Section 1: logistic model ####

# logistic model using columns of 'success' and 'failure' (unemployed and employed)
logistic.model = glm(cbind(unemployed, employed) ~ Age + Sex + Indig + Educ + state + Indig:Educ + Age:Sex, family=binomial(), data=for.model)
summary(logistic.model)

### plot interactions
## a) age by sex
# i) get predictions
new.data = expand.grid(Age = levels(data$Age),  # all combinations of
                       Sex = levels(data$Sex), 
                       Indig = levels(data$Indig)[1], # reference level
                       Educ = levels(data$Educ)[1],
                       state = levels(data$state)[1])
new.data$pred = predict.glm(logistic.model, newdata=new.data, type='response') # predicted probabilities
new.data = mutate(new.data, 
                  age = factor(Age, levels=levels(new.data$Age), labels=str_remove(string=levels(new.data$Age), pattern=' years')))
# ii) plot results
iplot = ggplot(new.data, aes(x=age, y=pred, group=factor(Sex), col=factor(Sex)))+
  geom_point(size=3)+
  geom_line(size=1.05)+
  xlab('Age, years')+
  ylab('Probability unemployed')+
  scale_color_manual('', values=c('dark red','dark blue'))+
  theme_bw()
iplot
## b) indigenous by education
# i) get predictions
new.data = expand.grid(Age = levels(data$Age)[1],  # reference level
                       Sex = levels(data$Sex)[1], 
                       Indig = levels(data$Indig), # all combinations of
                       Educ = levels(data$Educ),
                       state = levels(data$state)[1])
new.data$pred = predict.glm(logistic.model, newdata=new.data, type='response') 
# ii) plot results
iplot2 = ggplot(new.data, aes(x=Indig, y=pred, group=factor(Educ), col=factor(Educ)))+
  geom_point(size=3)+
  geom_line(size=1.05)+
  xlab('Age, years')+
  ylab('Probability unemployed')+
  scale_color_manual('Education', values=c('dark red','dark blue'))+
  theme_bw()
iplot2

#### Section 2: Poisson comparison ####

# with offset
poisson.model = glm(unemployed ~ Age + Sex + Indig + Educ + state + Indig:Educ + Age:Sex, family=poisson(), offset=log(total), data=for.model)
summary(poisson.model)

#### Section 3: GLMM logistic ####

# Collapse data for standard model (sum counts) 
for.model.sa4 = group_by(data, state, Age, Sex, Indig, Educ, SA4, LFS_Status) %>%
  summarise(sum = sum(Count)) %>%
  ungroup() %>%
  spread(LFS_Status, sum) %>% # switch to wide format with side-by-side results for employed and unemployed
  mutate(unemployed = ifelse(is.na(unemployed), 0, unemployed), # replace any missing with zero
         total = employed + unemployed) # make total

# same as before, with random intercept (takes a while)
logistic.model.sa4 = glmer(cbind(unemployed, employed) ~ Age + Sex + Indig + Educ + state + Indig:Educ + Age:Sex + (1|SA4), family=binomial(), data=for.model.sa4)
summary(logistic.model.sa4)
## examine SA4 level random intercepts
ranef = ranef(logistic.model.sa4)$SA4
ranef = mutate(ranef, SA4= row.names(ranef))
hist(ranef$`(Intercept)`)
# look at high areas
filter(ranef, `(Intercept)` > 1)
# look at the lower
filter(ranef, `(Intercept)` < -0.5)
