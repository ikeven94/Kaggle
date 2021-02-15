# Following Kaggle with titanic dataset

# “what sorts of people were more likely to survive?”
# Load packages
library(ggplot2)  # visualization
library(ggthemes) # visualization
library(scales)   # visualization
library(dplyr)    # data manipulation
library(mice)     # imputation
library(randomForest) # classification algorithm

#path = C:/Users/USER/Desktop/dataset/kaggle/Titanic data/

# read dataset

train <- read.csv("C:/Users/USER/Desktop/dataset/kaggle/Titanic data/train.csv"
                  ,stringsAsFactors = F)
test <- read.csv("C:/Users/USER/Desktop/dataset/kaggle/Titanic data/test.csv"
                 ,stringsAsFactors = F)
full <- bind_rows(train,test)

str(full)
summary(full)
dim(train);dim(test);dim(full)
str(train); str(test)

head(full$Name,100)
str(full)

# title 변수에서 승객 이름 추출 
full$Name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
full$Title
######
# gsub(a,b,data)
# full$Name 에서 a위치에 있는 것을 b로
# \. ->  진짜 dot 의미  / .-> 모든 글자(any character)
# comma 앞과  dot 뒤에 나오는거 모두 제거
#####
# Show title counts by sex
table(full$Sex, full$Title)

# 별로 등장하지 않는 title >> rare로 취급 
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# mlle ms mme 재할당 
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
# sapply로 list 처리 
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')


# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# family size 이산화 
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# family size vs 생존자 mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# Cabin은 결측치가 많은 변수
full$Cabin[1:28]

# 첫번째 글자는 갑판번호 
strsplit(full$Cabin[2], NULL)[[1]][1] # 

# Deck 변수 생성. deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


#################
# 3. 결측치 처리#
#################

### 3.1 Sensible value imputation

# Passengers 62 and 830 은 정박항 missing
which(full$Embarked %in% "" == TRUE)
full[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

full[c(62,830),] # 1등석에 80$ 인것 >> 운좋게 'C'랑 거의 일치
full$Embarked[c(62, 830)] <- 'C'


# Show row 1044
full[1044, ] # 3등석에 S / Fare = NA


ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# median 값으로 결측치 대체 
median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE) # $8.05
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


### 3.2 Predictive imputation

# Age 변수의 결측값 >> 263개
sum(is.na(full$Age))

# factor형은 factor형 변수로 
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# mice imputation사용 / excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Original data와 Mice imputation data의 분포 비교
# 비슷한 분포를 띄고 있음을 확인 
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# mice imputation 값으로 age variable 채우기
full$Age <- mice_output$Age

# missing Age values / 이제 없다.
sum(is.na(full$Age)) 


## 3.3 Feature Engineering: Round 2

# 모두의 age를 알아낸 상황에서 age와 survive 간의 관계 확인 
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# child 파생변수 생성
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

# 0   1
# Adult 479 271
# Child  70  71

# Mother 파생변수 생성
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# 0   1
# Mother      16  39
# Not Mother 533 303

# 새로운 child와 mother 변수에 대한 factor화
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full) # 빨간색이 결측값 

### 4. Prediction

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# RF model (모든 변수 사용x)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)


# model error plot
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# 변수 중요도 
importance    <- importance(rf_model)
#          MeanDecreaseGini
# Pclass          31.491524
# Sex             54.693254
# Age             45.633386
# SibSp           12.160218
# Parch            8.198338
# Fare            59.297234
# Embarked         9.831000
# Title           71.417265
# FsizeD          17.646436
# Child            4.370018
# Mother           2.206793

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

#          Variables Importance
# Pclass      Pclass      31.49
# Sex            Sex      54.69
# Age            Age      45.63
# SibSp        SibSp      12.16
# Parch        Parch       8.20
# Fare          Fare      59.30
# Embarked  Embarked       9.83
# Title        Title      71.42
# FsizeD      FsizeD      17.65
# Child        Child       4.37
# Mother      Mother       2.21

# importance 기반으로 rank variable 생성
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# ggplot2로 변수 중요도 시각화 
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

####4.4 Prediction!

# test set에 predict
prediction <- predict(rf_model, test)

# solution data
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# 최종적으로 solution >> data
write.csv(solution, file ='rf_mod_Solution.csv', row.names = F)



