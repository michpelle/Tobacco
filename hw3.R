
data <- readRDS("TaxBurden_Data.rds")

# 1. Present a line graph showing the average number of packs sold per capita from 1970 to 2018.
q1 <- data %>% filter(Year>=1970 && Year<=2018) %>% group_by(Year) %>% summarise(avgPacksSold = mean(sales_per_capita))
ggplot(q1,aes(x=Year, y=avgPacksSold)) + geom_line()

# 2. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.

# 3. Plot the average tax (in 2012 dollars) on cigarettes from 1970 to 2018.

# 4. Plot the average price of a pack of cigarettes from 1970 to 2018. Over what period did prices appear to increase the most?

# 5. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

# 6. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

# 7. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.


# ATE
# 1. Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the price elasticity of demand over that period. Interpret your results.

# 2. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

# 3. Show the first stage and reduced-form results from the instrument.

# 4. Repeat questions 1-3 focusing on the period from 1991 to 2015.

# 5. Compare your elasticity estimates from 1970-1990 versus those from 1991-2015. Are they different? If so, why?



  
  
  
  