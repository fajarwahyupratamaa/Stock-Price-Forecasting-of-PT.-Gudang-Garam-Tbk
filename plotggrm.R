library(readxl)
library(ggplot2)

data <- read_excel("ggrm.xlsx")

ggplot(data=data)+
  geom_line(mapping = aes(x=Date, y=Open, color="Open"))+
  geom_line(mapping = aes(x=Date, y=Close, color="Close"))