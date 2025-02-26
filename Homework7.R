#Homework 7
library(tidyverse)
glimpse(iris)
str(iris) #150 obs., 5 variables
iris

iris1 <- iris%>%
  filter(Species == "virginica" | Species == "versicolor", Sepal.Length > 6, Sepal.Width > 2.5)
str(iris1) #56 obs of 5 variables

iris2 <- iris1%>%
  select(Species, Sepal.Length, Sepal.Width)
str(iris2) #56 obs of 3 variables

iris3 <- iris2%>%
  arrange(by=desc(Sepal.Length))
head(iris3)

iris4 <- iris3%>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width)
str(iris3) #56 obs of 4 variables

iris5 <- iris4%>%
  summarize(meanLength=mean(Sepal.Length), meanWidth=mean(Sepal.Width), ss=n())
print(iris5) 

iris6 <- iris4%>%
  group_by(Species)%>%
  summarize(meanLength=mean(Sepal.Length), meanWidth=mean(Sepal.Width), ss=n())
print(iris6)

irisFinal <- iris%>%
  filter(Species == "virginica" | Species == "versicolor", Sepal.Length > 6, Sepal.Width > 2.5)%>%
  select(Species, Sepal.Length, Sepal.Width)%>%
  arrange(by=desc(Sepal.Length))%>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width)%>%
  group_by(Species)%>%
  summarize(meanLength=mean(Sepal.Length), meanWidth=mean(Sepal.Width), ss=n())
print(irisFinal)


iris_longer <- iris%>%
  select(Species, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)%>%
  pivot_longer(cols = Sepal.Length:Sepal.Width:Petal.Length:Petal.Width, names_to="Measure", values_to = "Value")
  
  
  
