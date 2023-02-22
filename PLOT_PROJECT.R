#This steps presents the time series plot and the boxplot for the data set.

#Libraries
library(tidyverse)
library(plotly)
library(ggthemes)

#Step 1 - Load the CSV file
df<- as_tibble(read.csv("df_clean.csv")) 
##View and data type
df 


#Step 2 - Casting 
#Type Casting - Set Date as a date-time variable
df<- df |> mutate(period = as.Date(period)) 

#Step 3 - Boxplot
ggplot(df, aes(x = '' , y = price)) + 
  geom_boxplot() + 
  ylab('Closing Price (in US$)') +
  xlab('') +
  theme_stata() +
  theme(text = element_text(size=11), 
        plot.title = element_text(face = "bold", size = 12),
        axis.title.y =   element_text(face = "bold"))+
  labs(title = 'Boxplot of the gold price') 

##Dynamic plot
p = ggplot(df, aes(x = '' , y = price)) + 
  geom_boxplot() + 
  ylab('Closing Price (in US$)') +
  xlab('') +
  theme_stata() +
  theme(text = element_text(size=11), 
        plot.title = element_text(face = "bold", size = 12),
        axis.title.y =   element_text(face = "bold"))+
  labs(title = 'Boxplot of the gold price') 

ggplotly(p)

##Save the plot to a pdf format
ggsave('boxplot.pdf', plot = p)

##Unusual observations
boxplot.stats(df$price)$out
#2069.4 - 1484.0 - 1479.3 - 1477.9 - 1524.9 - 1486.5 - 1516.7 - 1528.1 (8 observations)
#Note - These observations are not far from the boxplot limits.

#Step 4 - Time series plot
ggplot()+
  xlab('Date') +
  ylab('Closing Price (in US$)') +
  labs(title = "Time series of the gold price", subtitle = "January 2020 - December 2021")+
  geom_line(data = df, aes(x = period, y = price), color = "darkblue", linewidth =0.85)+
  theme_stata()+  
  theme(text = element_text(size=11), plot.title = element_text(face = "bold", size = 12))

##Dynamic plot
p2 = ggplot()+
  xlab('Date') +
  ylab('Closing Price (in US$)') +
  labs(title = "Time series of the gold price", subtitle = "January 2020 - December 2021")+
  geom_line(data = df, aes(x = period, y = price), color = "darkblue", linewidth =0.85)+
  theme_stata()+  
  theme(text = element_text(size=11), plot.title = element_text(face = "bold", size = 12))

ggplotly(p2)

##Save the plot to a pdf format
ggsave('timeseriesplot.pdf', plot = p2)

#Note - The gold price showed a positive trend between January (2020) and August (2020). However, it is relevant to mention the short-term trajectory in March. Between 3 and 16, the gold price decreased, achieving a minimal value - the beginning of the COVID period. The price range has been between 1700 and 1900 since August (2020). Finally, it was possible to see short-term trends.