library(tidyverse)
library(stargazer)

data <- World_Bank_data

data <= data %>%
  select(-(starts_with("Time")), -(`Country Code`))

names(data) <- sub(" \\[.*", "", names(data))  # removes everything from the ( to the end


ggplot(data = data, aes(as.numberic(`Ease of doing business rank (1=most business-friendly regulations)`),
                      
                        

                        
reg <- lm(data$`GDP per capita (current US$) [NY.GDP.PCAP.CD]` ~ data$`Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` + data$`Ease of doing business score (0 = lowest performance to 100 = best performance) [IC.BUS.DFRN.XQ]`)




geon_text ## use the variable, to know which country is it? 