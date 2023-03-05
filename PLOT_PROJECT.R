#Data visualization

#Libraries
library(tidyverse)
library(plotly)
library(ggthemes)
library(forecast)

#Step 1 - Load the CSV file
df<- as_tibble(read.csv("df_clean.csv")) 
##View and data type
df 

#Step 2 - Casting 
##Type Casting - Set Date as a date-time variable
df<- df |> mutate(period = as.Date(period)) 
##Check
df |> str()

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

##Save the plot to a pdf format
ggsave('boxplot.pdf')

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

##Unusual observations
boxplot.stats(df$price)$out
#2069.4 - 1484.0 - 1479.3 - 1477.9 - 1524.9 - 1486.5 - 1516.7 - 1528.1 (8 observations)


#Step 4 - Time series plot
ggplot()+
  xlab('Date') +
  ylab('Closing Price (in US$)') +
  labs(title = "Time series of the gold price", subtitle = "January 2020 - December 2021")+
  geom_line(data = df, aes(x = period, y = price), color = "darkblue", linewidth =0.85)+
  theme_stata()+  
  theme(text = element_text(size=11), plot.title = element_text(face = "bold", size = 12))

##Save the plot to a pdf format
ggsave('timeseriesplot.pdf')

##Dynamic plot
p2 = ggplot()+
  xlab('Date') +
  ylab('Closing Price (in US$)') +
  labs(title = "Time series of the gold price", subtitle = "January 2020 - December 2021")+
  geom_line(data = df, aes(x = period, y = price), color = "darkblue", linewidth =0.85)+
  theme_stata()+  
  theme(text = element_text(size=11), plot.title = element_text(face = "bold", size = 12))

ggplotly(p2)

#Step 5 - Correlogram - Autocorrelation Function (ACF)
df |> select(price) |>  ggAcf() +
  theme_stata()+  
  labs(title = "Correlogram for the gold price") +
  theme(text = element_text(size=11), plot.title = element_text(face = "bold", size = 12))

#Save the ACF plot
ggsave("correlogram.pdf")

##Dynamic plot
p3 = df |> select(price) |>  ggAcf() +
  theme_stata()+  
  labs(title = "Correlogram for the gold price") +
  theme(text = element_text(size=11), plot.title = element_text(face = "bold", size = 12))

ggplotly(p3)





