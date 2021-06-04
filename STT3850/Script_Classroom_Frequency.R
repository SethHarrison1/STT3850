berkdata <- read.csv(file = url("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/berkeley.csv"))
summary(berkdata)
status_tab <- table(berkdata$status)
status_prop <- prop.table(status_tab)
round(status_prop, 4)
prop.table(table(berkdata$sex, berkdata$status), 1)

library(dplyr)
berkdata %>% count(sex)
berkdata %>% count(department) %>% arrange(desc(n)) %>% mutate(cum_n = cumsum(n), prop = prop.table(n), cum_prop = cumsum(prop))

library(ggplot2)
ggplot(berkdata, aes(x = sex)) + geom_bar()
ggplot(berkdata, aes(x = sex)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "Berkeley Admissions Data",
       x = "sex of applicant",
       y = "count of applicants",
       subtitle = "Fall 1973")
ggplot(berkdata, aes(x = status, fill = sex)) + geom_bar()
ggplot(berkdata, aes(x = sex, fill = status)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion")
dept_tab <- berkdata %>% count(department)
ggplot(dept_tab, aes(x = department, y = n)) + geom_bar(stat = "identity")

mileage <- read.csv("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/gasmileage.csv")
geyser <- read.csv("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Instructor/oldfaithful.csv")
ggplot(mileage, aes(x = mpg)) + geom_histogram(bins = 10)
ggplot(geyser, aes(x = wait_time)) + geom_histogram(bins = sqrt(272))
ggplot(mileage, aes(x = mpg)) + geom_density()
ggplot(mileage, aes(x = mpg)) + 
  geom_histogram(aes(y=..density..)) + 
  geom_density()
ggplot(geyser, aes(x = wait_time)) + 
  geom_histogram(aes(y=..density..)) + 
  geom_density()
ggplot(mileage, aes(x = mpg)) + stat_ecdf()
ggplot(mileage, aes(x = mpg)) + 
  geom_density() + 
  stat_ecdf()
ggplot(geyser, aes(x = wait_time)) + 
  geom_density() + 
  stat_ecdf()

library(moments)

skewness(mileage$mpg)
kurtosis(mileage$mpg)
mean(mileage$mpg)
median(mileage$mpg)
var(mileage$mpg)
sd(mileage$mpg)

range(mileage$mpg)
fivenum(mileage$mpg)
1.5 * IQR(mileage$mpg)
fivenum(mileage$mpg)[4] + 1.5 * IQR(mileage$mpg)
boxplot.stats(mileage$mpg)

ggplot(mileage, aes(x = 1, y = mpg)) + geom_boxplot() + coord_flip()
ggplot(geyser, aes(x = 1, y = wait_time)) + geom_boxplot() + coord_flip()


flights <- read.csv("https://raw.githubusercontent.com/STAT-JET-ASU/Datasets/master/Chihara/FlightDelays.csv")
ggplot(flights, aes(x = FlightLength, fill = Carrier)) + geom_histogram(position = "Identity", alpha = .5)
ggplot(flights, aes(x = FlightLength, fill = Carrier)) + geom_density(alpha = .5)


flights %>%
  group_by(Carrier) %>%
  summarize(n = n(),
                      minimum = min(FlightLength),
                      Q1 = fivenum(FlightLength)[2],
                      median = median(FlightLength),
                      Q3 = fivenum(FlightLength)[4],
                      maximum = max(FlightLength))
