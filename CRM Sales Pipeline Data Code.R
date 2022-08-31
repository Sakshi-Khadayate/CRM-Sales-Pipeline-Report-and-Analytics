#Final Project IE6200
#Group 5

library(prob)
library(tidyverse) 
library(e1071) 
library(dplyr)
library(ggplot2)
library(lubridate) 
library(reshape2)
library(triangle)
library(fitdistrplus)
library(corrplot)

setwd("C:/Users/DELL/Desktop/Fall 21 Subjects/IE6200/project/Final Submission")

Sales_Dataset <- read.table('Dataset.csv', header= TRUE, sep= ',')
Sales_Won <- Sales_Dataset[Sales_Dataset$Opportunity.Status == "Won",]

wonloss <- table(Sales_Dataset$Opportunity.Status)
wonlossplot <- barplot(wonloss, beside = TRUE, legend = row.names(wonloss), ylab = "Number of Opportunity", xlab = "Won/Loss", col = c("pink","lavender"))
text(wonlossplot,1,round(wonloss, 1), cex = 1, pos = 3) 

#Quater-wise win/loss Opportunities

Quater <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$Closing.Quarter)
Quaterplot <- barplot(Quater, beside = TRUE, legend = row.names(Quater), ylab = "Number of Opportunity", xlab = "Closing Quater-Wise", col = c("pink","lavender"))
text(Quaterplot,1,round(Quater, 1), cex = 1, pos = 3) 

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Closing.Quarter)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Closing Quarter-Wise')

#Quater-wise Won Opportunity size

Q1 <- Sales_Won[Sales_Won$Closing.Quarter=="Q1",]
Q2 <- Sales_Won[Sales_Won$Closing.Quarter=="Q2",]
Q3 <- Sales_Won[Sales_Won$Closing.Quarter=="Q3",]
Q4 <- Sales_Won[Sales_Won$Closing.Quarter=="Q4",]

A<-matrix(
  c(sum(Q1$Opportunity.Size..USD.)/1000000,sum(Q2$Opportunity.Size..USD.)/1000000,sum(Q3$Opportunity.Size..USD.)/1000000,sum(Q4$Opportunity.Size..USD.)/1000000),
  nrow = 1,
  ncol= 4)
colnames(A) <- c('Q1','Q2','Q3','Q4')
A

Qplot <- barplot(A, beside = TRUE, legend = row.names(A), ylab = "Total revenue", xlab = "Quater-Wise (Millions USD)", col = c("mediumpurple1"))
text(Qplot,1,round(A, 1), cex = 1, pos = 3)


#Win Loss insights

#Technology Primary Win/Loss Opportunities

Tech <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$Technology.Primary)
techplot <- barplot(Tech, beside = TRUE, legend = row.names(Tech), ylab = "Number of Opportunity", xlab = "Technology Primary", col = c("pink","lavender"))
text(techplot,1,round(Tech, 1), cex = 1, pos = 3) 
 
ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Technology Primary')

#Business Size Win/loss

Bussize <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$Business.Size)
Bussizeplot <- barplot(Bussize, beside = TRUE, legend = row.names(Bussize), ylab = "Number of Opportunity", xlab = "Client Business size", col = c("pink","lavender"))
text(Bussizeplot,1,round(Bussize, 1), cex = 1, pos = 3) 

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Business.Size)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Client Business Size')

#Location wise win/Loss

Loc <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$Client.Location)
Locplot <- barplot (Loc, beside = TRUE, legend = row.names(Loc), ylab = "Number of Opportunity", xlab = "Client Location", col = c("pink","lavender"))
text(Locplot,1,round(Loc, 1), cex = 1, pos = 3)

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Client.Location)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Client Location')

#Competitor Insights Win/Loss

Comp <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$Competitor.Insights)
compplot <- barplot(Comp, beside = TRUE, legend = row.names(Comp), ylab = "Number of Opportunity", xlab = "Competitor Insights", col = c("pink","lavender"))
text(compplot,1,round(Comp, 1), cex = 1, pos = 3)

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Competitor.Insights)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Competitor Insights')

#B2B Sales Medium Win/Loss

B2B <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$B2B.Sales.Medium)
B2Bplot<-barplot(B2B, beside = TRUE, legend = row.names(B2B), main = "B2B Sales Medium-Wise" ,ylab = "Number of Opportunity", xlab = "B2B Sales Medium", col = c("pink","lavender"))
text(B2Bplot,1,round(B2B, 1), cex = 1, pos = 3)

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('B2B Sales Medium')

#Client revenue size win/loss

Client <- table(Sales_Dataset$Opportunity.Status, Sales_Dataset$Client.Revenue.Sizing.BINS)
Clientplot<-barplot(Client, beside = TRUE, legend = row.names(Client), main = "Win/Loss by Client Revenue Size" ,ylab = "Number of Opportunity", xlab = "Client Revenue Size", col = c("pink","lavender"))
text(Clientplot,1,round(Client, 1), cex = 1, pos = 3)

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Client.Revenue.Sizing.BINS)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Client Revenue Size')

#Scatter-plot

ggplot(data = Sales_Dataset)+
  geom_point(mapping = aes(x = Client.Revenue.Sizing , y = Opportunity.Size..USD., color = Opportunity.Status ))+  
  geom_hline(yintercept = mean(Sales_Dataset$Opportunity.Size..USD.)) +
  geom_vline(xintercept = mean(Sales_Dataset$Client.Revenue.Sizing)) +
  ylab('Opportunity Size')+
  xlab('Client Revenue Size')
  
#Fit distribution

#opportunity size (discrete data)

ggplot(Sales_Won, aes(Opportunity.Size..USD.)) +
  geom_histogram(bins = 100, color = 'darkolivegreen4', fill = 'darkolivegreen3')+
  xlab('Won Deal Size (USD)')+
  ylab('Frequency')


mean(Sales_Won$Opportunity.Size..USD.)
sd(Sales_Won$Opportunity.Size..USD.)
#Coefficient of Variance
mean(Sales_Won$Opportunity.Size..USD.)/sd(Sales_Won$Opportunity.Size..USD.)*100
skewness(Sales_Won$Opportunity.Size..USD.)
kurtosis(Sales_Won$Opportunity.Size..USD.)

fit_nb <- fitdist(Sales_Dataset$Opportunity.Size..USD., 'nbinom')
summary(fit_nb)

fit_p <- fitdist(Sales_Dataset$Opportunity.Size..USD., 'pois')
summary(fit_p)

gofstat(list(fit_nb, fit_p))

#negative binomial fits better

#Discount (Continuous)

Sales_Won_disc <- Sales_Dataset[Sales_Dataset$Opportunity.Status == "Won"& Sales_Dataset$Discounts.. != 0,]

ggplot(Sales_Won_disc, aes(Discounts..)) +
  geom_histogram(bins = 300, color = 'thistle4', fill = 'thistle1')+
  xlab('Discounts % offered to Clients (Won Deals)')+
  ylab('Frequency')

descdist(Sales_Won_disc$Discounts..)

fit_n <- fitdist(Sales_Won_disc$Discounts.., "norm")
summary(fit_n)

fit_g <- fitdist(Sales_Won_disc$Discounts.., "gamma")
summary(fit_g)

fit_e <- fitdist(Sales_Won_disc$Discounts.., "exp")
summary(fit_e)

fit_l <- fitdist(Sales_Won_disc$Discounts.., "lnorm")
summary(fit_l)


par(mfrow=c(2,2))
plot.legend <- c("lnorm","gamma")
denscomp(list(fit_g), legendtext = plot.legend, xlab = 'Continuous data (x)', xlegend = 'topleft')
cdfcomp (list(fit_g), legendtext = plot.legend, xlab = 'Continuous data (x)')
qqcomp  (list(fit_g), legendtext = plot.legend, xlab = 'Continuous data (x)')
ppcomp  (list(fit_g), legendtext = plot.legend, xlab = 'Continuous data (x)')

par(mfrow=c(2,2))
plot.legend <- c("lnorm","gamma")
denscomp(list(fit_l), legendtext = plot.legend, xlab = 'Continuous data (x)', xlegend = 'topleft')
cdfcomp (list(fit_l), legendtext = plot.legend, xlab = 'Continuous data (x)')
qqcomp  (list(fit_l), legendtext = plot.legend, xlab = 'Continuous data (x)')
ppcomp  (list(fit_l), legendtext = plot.legend, xlab = 'Continuous data (x)')

#Correlation plot for numerical data
Sales_Won_N <-  dplyr::select(Sales_Won, c(6,7,8,10,12,15,18,24))
Sales_Won_N

M <- cor(Sales_Won_N)
corrplot(M, method="circle")
corrplot(M, method="pie")
corrplot(M, method="ellipse")
corrplot(M, method="number")
corrplot(M, method="shade")
corrplot(M, method="color")

#PMF CDF and Joint Probability

Tech_T <- Sales_Won %>% 
  dplyr::select(Technology.Primary, B2B.Sales.Medium, Closing.Quarter, Client.Location, Opportunity.Size..USD.) %>% 
  dplyr::filter(Technology.Primary == 'Suzhi Analytics') %>% 
  group_by(Opportunity.Size..USD.) %>% 
  summarise(number_of_Won_deals_opp = n()) %>% 
  group_by(number_of_Won_deals_opp) %>%
  summarise(number_of_Won_deals = n()) %>% 
  mutate(pmf_T = number_of_Won_deals/sum(number_of_Won_deals))%>%
  mutate(cdf_T = cumsum(pmf_T))

Tech_T

Tech_B <- Sales_Won %>% 
  dplyr::select(Technology.Primary, B2B.Sales.Medium, Closing.Quarter, Client.Location, Opportunity.Size..USD.) %>% 
  dplyr::filter(B2B.Sales.Medium == 'Cold Calling') %>% 
  group_by(Opportunity.Size..USD.) %>% 
  summarise(number_of_Won_deals_opp = n()) %>% 
  group_by(number_of_Won_deals_opp) %>%
  summarise(number_of_Won_deals = n()) %>% 
  mutate(pmf_T = number_of_Won_deals/sum(number_of_Won_deals))%>%
  mutate(cdf_T = cumsum(pmf_T))

Tech_B

joint_freq <- outer(Tech_T$number_of_Won_deals, Tech_B$number_of_Won_deals, FUN = "+")
rownames(joint_freq) <- Tech_T$number_of_Won_deals
colnames(joint_freq) <- Tech_B$number_of_Won_deals
joint_freq

joint_prob <- round(joint_freq/sum(joint_freq),3)
joint_prob


Ana <-Sales_Won %>%
  dplyr::select(Technology.Primary, B2B.Sales.Medium, Closing.Quarter, Client.Location, Opportunity.Size..USD.) %>% 
  dplyr::filter(Technology.Primary == 'Suzhi Analytics') %>% 
  group_by(Opportunity.Size..USD.) %>% 
  summarise(number_of_Won_deals_opp = n()) 

Ana

Cold <-Sales_Won %>%
  dplyr::select(Technology.Primary, B2B.Sales.Medium, Closing.Quarter, Client.Location, Opportunity.Size..USD.) %>% 
  dplyr::filter(B2B.Sales.Medium == 'Cold Calling') %>% 
  group_by(Opportunity.Size..USD.) %>% 
  summarise(number_of_Won_deals_opp = n()) 

Cold

cor(Ana$number_of_Won_deals_opp[1:63], Cold$number_of_Won_deals_opp[1:63])

Dataset_Joint <- melt(joint_freq)
colnames(Dataset_Joint) <- c('Number_of_Analytics_WonDeals', 'Number_of_Cold_Calling_WonDeals', 'number_of_Won_deals')
head(Dataset_Joint, 10)


ggplot(data = Dataset_Joint,aes(x=Number_of_Analytics_WonDeals, y=Number_of_Cold_Calling_WonDeals)) +
  geom_point(aes(size = Number_of_Analytics_WonDeals, color = Number_of_Cold_Calling_WonDeals)) +
  labs(x = 'Suzhi Analytics', y = 'Cold Calling') +
  scale_x_continuous("Won deals for Suzhi Analytics", labels = as.character(Dataset_Joint$Number_of_Analytics_WonDeals),
                     breaks = Dataset_Joint$Number_of_Analytics_WonDeals) +
  scale_y_continuous("Won deals for Cold calling", labels = as.character(Dataset_Joint$Number_of_Cold_Calling_WonDeals),
                     breaks = Dataset_Joint$Number_of_Cold_Calling_WonDeals)

cor(Sales_Dataset$Client.Employee.Size, Sales_Dataset$Client.Revenue.Sizing)
cor(Sales_Dataset$Opportunity.Size..USD., Sales_Dataset$Client.Revenue.Sizing)
cor(Sales_Dataset$Sale.cycle..days.,Sales_Dataset$WiN.cummilative..)

#Hypothesis
#Sampling - assuming data represents entire population, samples are required to run the tests

set.seed(100)
SalesSample <- sample_n(Sales_Dataset, 0.2*nrow(Sales_Dataset))
SalesSample1 <- sample_n(Sales_Dataset, 0.25*nrow(Sales_Dataset))

#1)
#1.1)Two Proportion test

# X = R.V. of Win rate of opportunities
# H0: Sample Won Percentage = 22.59148 (There is no significant difference between the Sample Won percentage and the population Won percentage)
# H1: Sample Won Percentage != 22.59148(There is a significant difference between the Sample Won percentage and the population Won percentage)

SalesSample_Won <- SalesSample[SalesSample$Opportunity.Status == "Won",]


Won_perc_samp <- nrow(SalesSample_Won)/nrow(SalesSample)*100
Won_perc_samp
Won_perc_pop <- nrow(Sales_Won)/nrow(Sales_Dataset)*100
Won_perc_pop

prop.test(x=c(nrow(SalesSample_Won), nrow(Sales_Won)),n=c(nrow(SalesSample),nrow(Sales_Dataset)))

#P value > 0.05
#we fail to reject null hypothesis
#conclude that the Sample Won percentage is equivalent to the population Won percentage 

#1.2)Two Sample Proportion test

SalesSample_Won1 <- SalesSample1[SalesSample$Opportunity.Status == "Won",]

Won_perc_samp1 <- nrow(SalesSample_Won1)/nrow(SalesSample1)*100
Won_perc_samp1

prop.test(x=c(nrow(SalesSample_Won), nrow(SalesSample_Won1)),n=c(nrow(SalesSample),nrow(SalesSample1)))

#P value < 0.05
#we reject null hypothesis
#conclude that the TWO Sample Won percentage has significant difference,

#2)

# X = R.V. of Revenue size of Won deal
# H0: Sample mean of Won deal revenue size = 24322.78 (There is no significant difference between the mean of Sample Won deal revenue and the mean of population Won deal revenue)
# H1: Sample mean of Won deal revenue size  != 24322.78(There is a significant difference between the mean of Sample Won deal revenue and the mean of population Won deal revenue)

sample_rev_mean <- mean(SalesSample_Won$Opportunity.Size..USD.)
sample_rev_mean
pop_rev_mean <- mean(Sales_Won$Opportunity.Size..USD.)
pop_rev_mean

#Z test

z.test <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / sqrt((var/(n))) 
  df<-data.frame("Z_calc"=z,"P_value"=pnorm(z))
  return(df)
}

sample <-SalesSample_Won$Opportunity.Size..USD.
population <-Sales_Won$Opportunity.Size..USD.

z.test(sample, population)

#As p_value > 0.05, we fail to reject the null hypothesis and 
#conclude that the mean of Sample Won deal revenue is equivalent to the population Won deal revenue

t.test(x=sample , mu=pop_rev_mean)

#As p_value > 0.05, we fail to reject the null hypothesis and 
#conclude that the mean of Sample Won deal revenue is equivalent to the population Won deal revenue

#3)

# X = R.V. of Client Revenue Size
# H0: Sample mean client revenue of Won deals = 2985636 (There is no significant difference between the mean of Sample Won deal Client revenue and the mean of population Won deal client revenue)
# H1: Sample mean client revenue of Won deals != 2985636 (There is significant difference between the mean of Sample Won deal Client revenue and the mean of population Won deal client revenue)

sample_clirev_mean <- mean(SalesSample_Won$Client.Revenue.Sizing)
sample_clirev_mean
pop_clirev_mean <- mean(Sales_Won$Client.Revenue.Sizing)
pop_clirev_mean

#Z test

z.test <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / sqrt((var/(n))) 
  df<-data.frame("Z_calc"=z,"P_value"=pnorm(z))
  return(df)
}

sample <-SalesSample_Won$Client.Revenue.Sizing
population <-Sales_Won$Client.Revenue.Sizing

z.test(sample, population)

#As p_value > 0.05, we fail to reject the null hypothesis and 
#conclude that the Sample Won deal Client revenue is equivalent to the population Won deal Client revenue


t.test(x=sample , mu=pop_clirev_mean)

#As p_value > 0.05, we fail to reject the null hypothesis and 
#conclude that the Sample Won deal Client revenue is equivalent to the population Won deal Client revenue


#pipeline Insights

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Technology Primary')

Analytics <- Sales_Dataset[Sales_Dataset$Technology.Primary == "Suzhi Analytics",]

ggplot(data = Analytics , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Suzhi Analytics B2B Sales Medium')

ana_Coldcalling <-  Analytics[Analytics$B2B.Sales.Medium == "Cold Calling",]

ggplot(data = ana_Coldcalling , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x = Client.Location)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Suzhi Analytics Cold calling Location')

Business_Size <- ana_Coldcalling[ana_Coldcalling$Client.Location == "Kolkata",]

ggplot(data = Business_Size , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x = Business.Size)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Suzhi Analytics Cold calling Kolkata Business Size')

#2

ggplot(data = Analytics , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Client.Location)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Suzhi Analytics Location-Wise')

ana_kol <-  Analytics[Analytics$Client.Location == "Kolkata",]


ggplot(data = ana_kol , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Suzhi Analytics for Kolkata Location B2B Sales Medium')

ana_kol_cold <-  ana_kol[ana_kol$B2B.Sales.Medium == "Cold Calling",]

ggplot(data = ana_kol_cold , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Business.Size)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Suzhi Analytics for Kolkata Location Cold Calling')


#Location wise insights
Beng <- Sales_Dataset[Sales_Dataset$Client.Location == "Bengaluru",]

ggplot(data = Beng , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Bengaluru Technology Primary')

ggplot(data = Beng , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Bengaluru B2B Sales Medium')

Chen <- Sales_Dataset[Sales_Dataset$Client.Location == "Chennai",]

ggplot(data = Chen , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Chennai Technology Primary')

ggplot(data = Chen , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Chennai B2B Sales ')

Del <- Sales_Dataset[Sales_Dataset$Client.Location == "Delhi",]

ggplot(data = Del , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Delhi Technology Primary')

ggplot(data = Del , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Delhi B2B Sales ')

Hyd <- Sales_Dataset[Sales_Dataset$Client.Location == "Hyderabad",]

ggplot(data = Hyd , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Hyderabad Technology Primary')

ggplot(data = Hyd , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Hyderabad B2B Sales')


Kol <- Sales_Dataset[Sales_Dataset$Client.Location == "Kolkata",]

ggplot(data = Kol , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Kolkata Technology Primary')

ggplot(data = Kol , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Kolkata B2B Sales')

Mum <- Sales_Dataset[Sales_Dataset$Client.Location == "Mumbai",]

ggplot(data = Mum , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Mumbai Technology Primary')

ggplot(data = Mum , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Mumbai B2B Sales')


Pune <- Sales_Dataset[Sales_Dataset$Client.Location == "Pune",]

ggplot(data = Pune , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Technology.Primary)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Pune Technology Primary')

ggplot(data = Pune , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=B2B.Sales.Medium)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Pune B2B Sales')

#Inbound

Inbound <- Sales_Dataset[Sales_Dataset$B2B.Sales.Medium == "Inbound",]

ggplot(data = Inbound , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Client.Location)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Inbound B2B Sales Medium')


#Client Location and B2B Sales Medium  wise Won Opportunities

ggplot(data = Sales_Won , aes(fill = B2B.Sales.Medium ,y=WiN.cummilative.., x=Client.Location)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('B2B Medium-Wise Won Opportunities')+
  xlab('Client Location')

#Client Location and Technology wise Won Opportunities

ggplot(data = Sales_Won , aes(fill = Technology.Primary ,y=WiN.cummilative.., x=Client.Location)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Technology-Wise Won Opportunities')+
  xlab('Client Location')

#Customer Retention Rate

Business <- Sales_Dataset[Sales_Dataset$Business.from.Client.Last.Year.BINS != "No Business",]
custRet <- table(Business$Opportunity.Status, Business$Business.from.Client.Last.Year.BINS)
custRetplot<-barplot(custRet, beside = TRUE, legend = row.names(custRet), main = "Customer Retention" ,ylab = "Number of Opportunity", xlab = "Opportunity Conversion 2020-2021", col = c("pink","lavender"))
text(custRetplot,1,round(custRet, 1), cex = 1, pos = 3)  

ggplot(data = Business , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Business.from.Client.Last.Year.BINS)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Customer Retention Rate')

#Probability of of Won with Last year Business insight

ggplot(data = Sales_Dataset , aes(fill = Opportunity.Status ,y=WiN.cummilative.., x=Business.from.Client.Last.Year.BINS)) + 
  geom_bar(position="fill", stat="identity")+
  ylab('Opportunity Status')+
  xlab('Last year Business Insight')







