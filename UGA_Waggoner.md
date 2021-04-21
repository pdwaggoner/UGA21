---
title: "R and ggplot2 Workshop"
author: | 
  | Philip Waggoner
  | University of Chicago
  | https://pdwaggoner.github.io/
  | pdwaggoner@uchicago.edu
output: html_document
urlcolor: blue
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
```

## Introductory Remarks

Thanks for joining the workshop! We will start with a "crash course" in R covering core tasks and functions to get everyone on the same page. With this new (or refreshed) knowledge, we will then cover data visualization using the powerful `ggplot2` package, which is a core member of the tidyverse. 

Though we won't be able to cover all of the following together today, I have included more content and code for you to work through on your own, if you want to go deeper. Our topics will be:

  - [R and R Studio] 
  - [Functional Programming] 
  - [Tidy Data Wrangling] 
  - [Tidy Data Visualization via `ggplot2`] 
  - [Simulations] 
  - [R Markdown] 
  - [Feature Engineering for Missing Data] 
  

#### R and R Studio

Let's Look at the basics of getting into R via RStudio, mostly for the benefit of those who have never used R or RStudio. This will be a 2 minute demo. Reach out if questions persist. 


#### Functional Programming

Let's cover some core programming concepts in R, but which are involved in programming in many other languages, especially object oriented languages like Python, Julia, etc. 

Starting with: conditional logic via `if`, `if else`, and user-defined functions.

```{r}
x <- 1 

if(x > 0){
  print("Positive number")
}
```

A simple case with `if else`.

```{r}
x <- -5

if(x > 0){
  print("Positive")
} else {
  print("Negative")
}
```

Redefine `x` and give it a try...

```{r}
x <- -5 # e.g...

if(x > 0){
  print("Positive")
} else {
  print("Negative")
}
```

Though simple, `if` and `if else` are core to writing functions and programming in R. 

Note, we haven't written a function yet, in case that was unclear. So let's do that now. 

```{r}
sq <- function(x) {
  sqn <- x^2
  return(sqn)
}
```

Test it out.

```{r}
sq(2)
```

Adding arguments.

```{r}
exp <- function(x, y) {
  expn <- x^y
  return(expn)
}
```

Test it out.

```{r}
exp(2, 4)
```

Make it more descriptive.

```{r}
exp <- function(x, y) {
  expn <- x^y
  print(paste(x,"raised to the power of", y, "is", expn))
  return(expn) # optional 
}
```

Making arguments optional.

```{r}
exp <- function(x, y = 2) {
  expn <- x^y
  print(paste(x,"raised to the power of", y, "is", expn))
}
```

Test it out.

```{r}
exp(3)
```

And again.

```{r}
exp(3,1)
```

Do something more useful.

```{r}
celsius <- function(f) {
  c <- ((f - 32) * 5) / 9
  return(c)
}
```

Test it out with a vector.

```{r}
fahrenheit <- c(60, 65, 70, 75, 80, 85, 90, 95, 100) 

celsius(fahrenheit)
```

Or a single value.

```{r}
celsius(4)
```

Layering statements in a function.

```{r}

# define
pnz <- function(x) {
  if (x > 0) {
    n <- "Positive"
  }
  else if (x < 0) {
    n <- "Negative"
  }
  else {
    n <- "Zero"
  }
  return(n)
}
```

Test it out.

```{r}
# call
pnz(4)
pnz(-3)
pnz(0)
```

Let's practice defensive programming. 

```{r}
#' Calculate Herfindahl-Hirschman Index Scores
#' 
#' @usage hhi(x, "s")
#' @param x Name of the data frame
#' @param s Name of the vector (variable) from the data frame, x, corresponding with market share values
#' @return hhi A measure of market concentration
#' @note The vector of "share" values == total share of individual firms (e.g., df$s <- c(35, 40, 5, 4)
#' @note 0 is perfect competitiveness and 10,000 is a perfect monopoly


hhi <- function(x, s){
  if(!is.data.frame(x)) {
    stop('"x" must be data frame\n',
         'You have provided an object of class: ', class(x)[1])
  }
  shares <- try(sum(x[ ,s]))
  if(shares < 100 | shares > 100) warning('shares, "s", do not sum to 100')
  d <- x[ ,s]
  if(!is.numeric(d)) {
    stop('"s" must be numeric vector\n',
         'You have provided an object of class: ', class(d)[1])
  }
  if (any(d < 0)) {
    stop('vector "s" must contain only positive values')
  }
  hhi <- sum(d^2)
  
  return(hhi)
}
```

Test it out.

```{r}
a <- c(1,2,3,4) # firm id
b <- c(20,30,40,10) # market share of each firm (totaling 100%)
x <- data.frame(a,b) # create data frame

hhi(x, "b")
```


#### Tidy Data Wrangling

Now, we will going to cover some core concepts on working with data in R.

Let's now work with some social science data, the [2016 American National Election Study](https://electionstudies.org/data-center/anes-2016-pilot-study/) (ANES or NES) pilot data. 

Start by using the `here` package to load the data.

```{R eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
library(here)
library(tidyverse)

NESdta <- read_csv(here("data", "anes_2016.csv"))
```

Most of the functions we will cover are from the `dplyr` package, which is one of the main components of the tidyverse, in line with the tidy philosophy of *consistent syntax*.
  
* `select()`: choose specific variables
* `filter()`: filter data by values
* `group_by()`: group data by categorical values
* `summarise()`: create summaries of variables and data
* `mutate()`: create new variables
* `replace()`: changes particular values within a variable

First, `select()`.

```{R eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
# Select particular variables
NESdta_short <- NESdta %>% 
  select(fttrump, pid3, birthyr, gender, ftobama, state, pid7)

head(NESdta_short, n = 7)
```

`select()` can be combined with other functions like `starts_with()`, `ends_with()`, and `contains()` to select more than one feature at a time. We can also use the `:` to select more than one variable that are consecutive, e.g., for all of the feeling thermometers, we know that they all start with the prefix `ft`.

```{r}
# Select using starts_with()
ft <- NESdta %>% 
  select(starts_with("ft"))

# Or... 

# Selecting using a range (if we new the natural order)
NESdta %>% 
  select(ftobama:ftsci)

# Or multiple features at a time
NESdta %>%
  select(pid3, birthyr, gender, starts_with("ft"))

# Or remove a feature
NESdta_short <- NESdta_short %>%
  select(-pid7)

NESdta_short
```

Next, `filter()` works similar to `select()`, but instead of selecting columns by their _names_, `filter()` allows you to select rows by their _values_.

```{r}
# Select only those respondents who give Trump a 100
NESdta_short %>%
  filter(fttrump == 100)

# Or select only those respondents who give Trump a 100 and Obama a 1
NESdta_short %>%
  filter(fttrump == 100 & ftobama == 1)

# Or select only those respondents who give Trump a 100 or give Obama a 100
NESdta_short %>%
  filter(fttrump == 100 | ftobama == 100)

# Or combine and select only those respondents who both Trump AND Obama 100s or 1s.
NESdta_short %>%
  filter((fttrump == 100 & ftobama == 100) | (fttrump == 1 & ftobama == 1))

# Or select only those respondents who give Trump an approval rating greater than 50
NESdta_short %>%
  filter(fttrump > 50)

# Or select those with higher than average approval of Trump
NESdta_short %>%
  filter(fttrump > mean(fttrump, na.rm = TRUE))
```

Notice the pipe, `%>%`, allowing for stacking functions. Try it out with `group_by()` and `summarise()`. 

```{r}
NESdta_short %>%
  group_by(pid3) %>%
  summarise(average_fttrump = mean(fttrump, na.rm = TRUE),
            median_fttrump = median(fttrump, na.rm = TRUE),
            variance_fttrump = var(fttrump, na.rm = TRUE),
            stddev_fttrump = sd(fttrump, na.rm = TRUE),
            max_fttrump = max(fttrump, na.rm = TRUE),
            min_fttrump = min(fttrump, na.rm = TRUE),
            n_fttrump = n())
```

Another task that you will often find yourself doing is creating new features. The tidy approach for this is `mutate()`.

```{r}
# Create a new variable giving the respondent's age
NESdta_short <- NESdta_short %>%
  mutate(age = 2016 - birthyr)

# Or create a new variable giving the square of respondent's age
NESdta_short <- NESdta_short %>%
  mutate(age2 = age^2)

# Now, get rid of it
NESdta_short <- NESdta_short %>%
  mutate(age2 = NULL)
```

We just summarized support for then-candidate Donald Trump by party affiliation. But what if we want these summaries to be a part of the `NESdta_short` dataset? `mutate()` helps out with this too, along with redefining our data object, `NESdta_short`.

```{r}
# Creating a new variable using group_by() and mutate()
NESdta_short <- NESdta_short %>%
  group_by(pid3) %>%
  mutate(average_fttrump = mean(fttrump, na.rm = TRUE))

# Or calculate deviation from the mean
NESdta_short <- NESdta_short %>%
  mutate(deviation_fttrump = fttrump - average_fttrump)
```

To handle nonsensical values from original data recording, we will combine `mutate()` with `replace()`.

```{r message=FALSE}
# Using replace() to recode values
NESdta_short <- NESdta_short %>%
  mutate(fttrump = replace(fttrump, fttrump > 100, NA),
         ftobama = replace(ftobama, ftobama == 998, NA)) %>% 
  ungroup() # undo the grouping from earlier

glimpse(NESdta_short)

skimr::skim(NESdta_short) # tidy summary
```

Finally, `case_when()`.

```{r}
NESdta_short <- NESdta_short %>%
  mutate(state_name = case_when(state == 1~"Alabama", 
                                state == 2~"Alaska", 
                                state == 4~"Arizona", 
                                state == 5~"Arkansas",
                                state == 6~"California", 
                                state == 8~"Colorado", 
                                state == 9~"Connecticut", 
                                state == 10~"Delaware", 
                                state == 11~"District of Columbia", 
                                state == 12~"Florida", 
                                state == 13~"Georgia",
                                state == 15~"Hawaii", 
                                state == 16~"Idaho", 
                                state == 17~"Illinois", 
                                state == 18~"Indiana",
                                state == 19~"Iowa", 
                                state == 20~"Kansas", 
                                state == 21~"Kentucky", 
                                state == 22~"Louisiana", 
                                state == 23~"Maine", 
                                state == 24~"Maryland", 
                                state == 25~"Massachusetts", 
                                state == 26~"Michigan", 
                                state == 27~"Minnesota",
                                state == 28~"Mississippi", 
                                state == 29~"Missouri", 
                                state == 30~"Montana",
                                state == 31~"Nebraska", 
                                state == 32~"Nevada", 
                                state == 33~"New Hampshire", 
                                state == 34~"New Jersey",
                                state == 35~"New Mexico", 
                                state == 36~"New York", 
                                state == 37~"North Carolina", 
                                state == 38~"North Dakota",
                                state == 39~"Ohio", 
                                state == 40~"Oklahoma", 
                                state == 41~"Oregon",  
                                state == 42~"Pennsylvania",
                                state == 44~"Rhode Island",  
                                state == 45~"South Carolina", 
                                state == 46~"South Dakota", 
                                state == 47~"Tennessee", 
                                state == 48~"Texas", 
                                state == 49~"Utah", 
                                state == 50~"Vermont", 
                                state == 51~"Virginia",
                                state == 53~"Washington", 
                                state == 54~"West Virginia", 
                                state == 55~"Wisconsin", 
                                state == 56~"Wyoming"))
```

You might have noticed the double equal sign, `==`, which is used for comparison. For example, re: the `gender` variable, suppose we want to recode gender (currently 2 = female and 1 = male) to be more descriptive and also on the more common `{0,1}` scale. Using `mutate()` and `ifelse()` (from base R), we create a new `female` feature.

```{r}
NESdta_short <- NESdta_short %>%
  mutate(female = ifelse(gender == 2, 1, 0))

sample_n(NESdta_short,
         size = 5)
```


#### Tidy Data Visualization via `ggplot2`

Now, let's get to the meat of our workshop today: visualization. To do so, we will stick with the `ggplot2` package for the most part. 

In general, my approach will be to walk through "ugly" versions of visualizations using base R graphics, and then compare these to "pretty" versions using the power of `ggplot2`. 

*Note*: It is assumed that everyone is familiar with statistical properties of the visual types and the information they are conveying (e.g., histograms are a form of nonparametric estimation often used for summarizing distributions, etc.). We will instead focus on the steps required to build these figures, and distinguish between base R and `ggplot2` approaches where appropriate. 

Let's start by loading some core libraries, the data, and then taking a look at the data.

```{r message=FALSE, warning=FALSE}
library(here)
library(tidyverse)
library(skimr)

# Load the country data
ctydta_short <- read_csv(here("data", "countries_cleaned.csv"))

skim(ctydta_short)

glimpse(ctydta_short)
```

Base R histogram of "level of democracy" via freedom house scores in 1972 (`fhrate72` = scale from 1 to 7).

```{r message=FALSE, warning=FALSE}
hist(ctydta_short$fhrate72,
     xlab = "Level of Democracy",
     ylab = "Number of Countries",
     main = "Histogram of Democracy in 1972")
```

`ggplot2` version 2 ways.

Way 1: object manipulation

```{r message=FALSE, warning=FALSE}
basic_hist <- ggplot(data =  ctydta_short)

basic_hist <- basic_hist +
  geom_histogram(aes(x = fhrate72), binwidth = 1)

basic_hist <- basic_hist +
  labs(x = "Level of Democracy",
       y = "Number of Countries",
       title = "Histogram of Democracy in 1972")

basic_hist <- basic_hist + theme_minimal()

basic_hist
```

Way 2: layering layers

```{r message=FALSE, warning=FALSE}
ggplot(data = ctydta_short) +
geom_histogram(aes(x = fhrate72), binwidth = 1) +
  labs(x = "Level of Democracy",
       y = "Number of Countries",
       title = "Histogram of Democracy in 1972") +
  theme_minimal()
```

But suppose we wanted to see a histogram across a qualitative feature, like `Region` in this case?

The base R approach is lengthy and requires 2 steps.

```{r message=FALSE, warning=FALSE}
# step 1: subset
industrial <- subset(ctydta_short,
                     Region == "Industrial")

latin_america <- subset(ctydta_short,
                        Region == "Latin America")

africa <- subset(ctydta_short,
                 Region == "Africa & M.E.")

eastern_europe <- subset(ctydta_short,
                         Region == "Eastern Europe")

asia <- subset(ctydta_short,
               Region == "Asia")

# step 2: viz in single pane
par(mfrow = c(3, 2)) 
hist(industrial$fhrate72,
     xlab = "Level of Democracy",
     ylab = "Number of Countries",
     main = "Industrial")
hist(latin_america$fhrate72,
     xlab = "Level of Democracy",
     ylab = "Number of Countries",
     main = "Latin America")
hist(africa$fhrate72,
     xlab = "Level of Democracy",
     ylab = "Number of Countries",
     main = "Africa & M.E.")
hist(eastern_europe$fhrate72,
     xlab = "Level of Democracy",
     ylab = "Number of Countries",
     main = "Eastern Europe")
hist(asia$fhrate72,
     xlab = "Level of Democracy",
     ylab = "Number of Countries",
     main = "Asia")
par(mfrow = c(1, 1))
```

The `ggplot2` approach via `facet_wrap()` is much simpler, requires less code, and looks better (in my humble opinion).

```{r message=FALSE, warning=FALSE}
ggplot(data = ctydta_short) +
  geom_histogram(aes(x = fhrate72), binwidth = 1) +
  theme_minimal() +
  facet_wrap( ~ Region, ncol = 2) +
  labs(x = "Level of Democracy",
       y = "Number of Countries",
       title = "Levels of Democracy by Region in 1972")
```

Update binwidth, color, and drop facet wrap.

```{r message=FALSE, warning=FALSE}
ggplot(data = ctydta_short) +
  geom_histogram(aes(x = fhrate72),
                 binwidth = 1,
                 color = "white",
                 fill = "steelblue") +
  theme_minimal() +
  labs(x = "Level of Democracy",
       y = "Number of Countries",
       title = "Level of Democracy in 1972")
```

Next, let's try a bar plot, which gives counts or proportions in categories. Base R, again, requires a couple steps.

```{r message=FALSE, warning=FALSE}
# region table
region_table <- table(ctydta_short$Region)

# viz
barplot(region_table,
        xlab = "Region",
        ylab = "Number of Countries",
        main = "Distribution of Countries in Regions")
```

Now, the `ggplot2` version. Note, there's no need for a table first. `ggplot2` natively does the calculation for you.

```{r message=FALSE, warning=FALSE}
ggplot(data = ctydta_short) +
  geom_bar(aes(x = Region)) +
  labs(x = "Region",
       y = "Number of Countries",
       title = "Distribution of Countries by Region") +
  theme_minimal()
```

Because `ggplot2` is a part of the tidyverse, there's flexible integration with the rest of the tidyverse to give efficient, clean, human-readable code that does a lot of stuff at once. 

For example...

```{r message=FALSE, warning=FALSE}
ctydta_short %>%
  group_by(Region) %>%
  summarize(mean_democracy_72 = mean(fhrate72, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = Region, y = mean_democracy_72), stat = "identity") +
  labs(x = "Region",
       y = "Mean Democracy Score in 1972",
       title = "Mean Democracy Score By Region in 1972") +
  theme_minimal()
```

Next, let's take a look at scatterplots of log GDP over democracy levels. 

As before, begin with base R.

```{r message=FALSE, warning=FALSE}
plot(ctydta_short$ln_gdppc_71, ctydta_short$fhrate72,
     main="Relationship Between Development and Democracy, 1972",
     xlab="log(per-capita GDP)",
     ylab="Freedom House Level of Democracy")
```

Now, `ggplot2`. 

```{r message=FALSE, warning=FALSE}
ggplot(ctydta_short, aes(x = ln_gdppc_71, 
                         y = fhrate72)) +
  geom_point() +
  geom_smooth(method = lm, alpha = 0.1) +
  labs(x="log(per-capita GDP)",
       y="Freedom House Level of Democracy",
       title="Relationship Between Development & Democracy, 1972") +
  theme_minimal()
```

Much better/more informative.

```{r message=FALSE, warning=FALSE}
ggplot(ctydta_short, aes(x = ln_gdppc_71, 
                         y = fhrate72,
                         color = Region)) + # add this one line... 
  geom_point() +
  geom_smooth(method = lm, alpha = 0.1) +
  labs(x="log(per-capita GDP)",
       y="Freedom House Level of Democracy",
       title="Relationship Between Development & Democracy, 1972",
       fill="Region") +
  theme_minimal()
```

Or, separate these out with our old friend `facet_wrap()`.

```{r message=FALSE, warning=FALSE}
# Use a facet wrap to display the regions
ggplot(ctydta_short, aes(x = ln_gdppc_71, 
                         y = fhrate72)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm, alpha = 0.1) +
  theme_minimal() +
  labs(x="ln(per-capita GDP)",
       y="Freedom House Level of Democracy",
       title="Relationship Between Development & Democracy, 1972")+
  facet_wrap(~ Region, ncol = 2) 
```

Finally, let's take a look at a different, more recently popular plot type: bubble plots, which help to pack a lot of information in a single plot. 

Some details: color was determined by the country’s region and the point’s size was determined by the country’s GDP growth. 

Since we are looking at the change in democracy, it is useful to have a line which indicates no change – in this case a 45 degree line – which we add via `geom_abline()` and specifying a line with a y-intercept at zero and a slope of 1. The points above this line are the countries that have increased their democracy scores, and the ones below this line have decreased their scores.

Finally, we clean up the chart by specifying the axis marks on the x-axis and y-axis using the `scale_x_continuous()` and `scale_y_continuous()` options with the limits deﬁned. In this case, we had the limits go from 0.5 to 7.5 by steps of 1.

```{r message=FALSE, warning=FALSE}
ctydta_short %>%
  mutate(change_gdppc = ln_gdppc_07 - ln_gdppc_71) %>%
  ggplot(aes(x = fhrate72, 
             y = fhrate14)) +
  geom_point(aes(size = change_gdppc, 
                 color = Region),
             position = "jitter") +
  geom_abline(intercept = 0, slope = 1) + 
  scale_y_continuous(limits = c(0.5, 7.5, 1)) +
  scale_x_continuous(limits = c(0.5, 7.5, 1)) +
  labs(x = "Democracy Score 1972", 
       y = "Democracy Score 2014",
       size = "Per-capita GDP Growth") +
  theme_minimal()
```


We touched on this earlier. But what about stacking multiple plots in a single pane. Can't use `par(mfrow = )` with ggplot. Two solutions: `gridExtra` or `patchwork`.

```{r message=FALSE, warning=FALSE}
library(gridExtra)

plot1 <- ggplot(ctydta_short) +
  geom_histogram(aes(x = fhrate72), binwidth = 1)

plot2 <- ggplot(ctydta_short) +
  geom_bar(aes(x = Region)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

plot3 <- ggplot(ctydta_short) +
  geom_histogram(aes(x = ln_gdppc_71), binwidth = 2)

plot4 <- ggplot(ctydta_short) +
  geom_point(aes(x = ln_gdppc_71, y = fhrate72))

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
```

```{r message=FALSE, warning=FALSE}
library(patchwork)

plot1 <- ggplot(ctydta_short) +
  geom_histogram(aes(x = fhrate72), binwidth = 1)

plot2 <- ggplot(ctydta_short) +
  geom_bar(aes(x = Region)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

plot3 <- ggplot(ctydta_short) +
  geom_histogram(aes(x = ln_gdppc_71), binwidth = 2)

plot4 <- ggplot(ctydta_short) +
  geom_point(aes(x = ln_gdppc_71, y = fhrate72))

plot1 +
  plot2 +
  plot3 +
  plot4
```

Here are some more options with `patchwork`.

```{r message=FALSE, warning=FALSE}
library(patchwork)

plot1 <- ggplot(ctydta_short) +
  geom_histogram(aes(x = fhrate72), binwidth = 1)

plot2 <- ggplot(ctydta_short) +
  geom_bar(aes(x = Region)) +
  theme(axis.text.x = element_text(angle = 75,
                                   vjust = 0.5))

plot3 <- ggplot(ctydta_short) +
  geom_histogram(aes(x = ln_gdppc_71), binwidth = 2)

plot4 <- ggplot(ctydta_short) +
  geom_point(aes(x = ln_gdppc_71, y = fhrate72))

four_plots <- plot2 /
  (plot1 + plot3) /
  plot4

four_plots + plot_annotation(
  title = "Four slick plots with annotation",
  subtitle = "Here is a great subtitle!")
```


Let's shift gears a little and cover some more complex visualizations, starting with interactive plots.

Scatterplot first. 

```{r message=FALSE, warning=FALSE}
library(plotly)

scatter <- plot_ly(ctydta_short,
                   x = ~ ln_gdppc_71,
                   y = ~ fhrate72,
                   type = "scatter", # plot type
                   text = paste("Country: ",
                                ctydta_short$Nation), # hover
                   mode = "markers", # object type
                   color = ~ Region,
                   size = ~ ln_gdppc_71
) %>%
  layout(title='Simple Scatterplot',
         xaxis=list(title='ln(per-capita GDP)'),
         yaxis=list(title='Freedom House Democracy Score, 1972'))
scatter
```

Histogram next.

```{r message=FALSE, warning=FALSE}
# Interactive histogram
hist <- plot_ly(ctydta_short,
                x = ~ fhrate72,
                type = "histogram",
                text = paste("Region: ", ctydta_short$Region),
                color = ~ Region
) %>%
  layout(title='Simple Histogram',
         xaxis=list(title='Freedom House Democracy Score, 1972'),
         yaxis=list(title='Number of Countries'))
hist
```

Now, let's take a look at animating visualizations. 

Quick munging of presidential approval data from base R.

```{r message=FALSE, warning=FALSE}
p <- presidents %>%
  as.data.frame() %>% 
  rename(Approval = x) %>% 
  na.omit()

p$Time <- seq(1:nrow(p)) # add a time counter for quarter number
```

The non-anmiated version first.

```{r message=FALSE, warning=FALSE}
ggplot(p) + 
  geom_line(aes(x = Time, y = Approval)) +
  labs(title = "Presidential Overtime Approval by Quarter\nFrom Q1 of 1945 to Q4 of 1974",
       x = "Time (Quarter)",
       y = "Approval %") + 
  theme_minimal()
```

Now, animate the time trend.

```{r message=FALSE, warning=FALSE, echo = TRUE, eval = FALSE}
library(gganimate)

ggplot(p) + 
  geom_line(aes(x = Time, y = Approval)) +
  labs(title = "Presidential Overtime Approval by Quarter\nFrom Q1 of 1945 to Q4 of 1974",
       x = "Time (Quarter)",
       y = "Approval %") + 
  theme_minimal() + 
  transition_reveal(Time) # animation part
```

Let's make some fireworks!

```{r message=FALSE, warning=FALSE, echo = TRUE, eval = FALSE}
# set "American" colors
colors <- c(
  'red',
  'white',
  'blue'
)

# function for a single explosion
explosion <- function(n, radius, x0, y0, time) {
  u <- runif(n, -1, 1)
  rho <- runif(n, 0, 2*pi)
  x <- radius * sqrt(1 - u^2) * cos(rho) + x0
  y <- radius * sqrt(1 - u^2) * sin(rho) + y0
  id <- sample(.Machine$integer.max, n + 1)
  data.frame(
    x = c(x0, rep(x0, n), x0, x),
    y = c(0, rep(y0, n), y0, y),
    id = rep(id, 2),
    time = c((time - y0) * runif(1), rep(time, n), time, time + radius + rnorm(n)),
    color = c('white', rep(sample(colors, 1), n), 'white', rep(sample(colors, 1), n)),
    stringsAsFactors = FALSE
  )
}

# 100 firework explosions
n <- round(rnorm(100, 40, 4))
radius <- round(n + sqrt(n))
x0 <- runif(20, -30, 30)
y0 <- runif(20, 40, 80)
time <- runif(20, max = 100)
july4 <- Map(explosion, 
             n = n, 
             radius = radius, 
             x0 = x0, 
             y0 = y0, 
             time = time)
july4 <- dplyr::bind_rows(july4)

# Happy 4th!
ggplot(july4) + 
  geom_point(aes(x, y, 
                 color = color, 
                 group = id), 
             size = 0.5, shape = 20) + 
  scale_color_identity() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = 'black', color = NA),
        panel.border = element_blank()) + 
  # animation components below
  transition_components(time, exit_length = 20) + 
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, 
              falloff = 'sine-in', exclude_phase = 'enter') + 
  exit_recolor(color = 'black')
```

Now, let's see how animation might help with a statistically useful task.

Her we can take a look at loess smoothing on the `ethanol` dataset in the `lattice` package.

```{r message=FALSE, warning=FALSE}
library(broom)
library(lattice)

mod <- loess(NOx ~ E, 
             data = ethanol, 
             degree = 1, # polynomial degree (default is quadratic)  
             span = .75) # bandwidth (sometimes called span, "s", "b", or "lambda")

fit <- augment(mod)

# non-animated version first
ggplot(fit, aes(E, NOx)) +
  geom_point() +
  geom_line(aes(y = .fitted), color = "red") +
  labs(title = "Local linear regression",
       x = "Equivalence ratio",
       y = "Concentration of nitrogen oxides in micrograms/J") + 
  theme_minimal()
```

One important argument you can control with LOESS is the span/bandwidth, as this influences how smooth the LOESS function will be. 

A larger span will result in a smoother curve, but may not be as accurate.

Let's see different fits for different spans.

```{r message=FALSE, warning=FALSE, echo = TRUE, eval = FALSE}
spans <- c(.25, .5, .75, 1)

# create loess fits, one for each span
fits <- tibble(span = spans) %>%
  group_by(span) %>%
  do(augment(loess(NOx ~ E, ethanol, degree = 1, span = .$span)))

# calculate weights to reproduce this with local weighted fits
dat <- ethanol %>%
  crossing(span = spans, center = unique(ethanol$E)) %>%
  as_tibble %>%
  group_by(span, center) %>%
  mutate(dist = abs(E - center)) %>%
  filter(rank(dist) / n() <= span) %>%
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

# create faceted plot with changing points, local linear fits, and vertical lines, and constant hollow points and loess fit
dat_spans <- ggplot(dat, aes(E, NOx)) +
  geom_point(aes(alpha = weight)) +
  geom_smooth(aes(group = center, weight = weight), method = "lm", se = FALSE) +
  geom_vline(aes(xintercept = center), lty = 2) +
  geom_point(shape = 1, data = ethanol, alpha = .25) +
  geom_line(aes(y = .fitted), data = fits, color = "red") +
  facet_wrap(~span) +
  ylim(0, 5) +
  ggtitle("x0 = ") +
  labs(title = "Centered over {closest_state}",
       # x = "Equivalence ratio",
       y = "Concentration of nitrogen oxides in micrograms/J") +
  transition_states(center,
                    transition_length = 2,
                    state_length = 1) + 
  ease_aes("cubic-in-out") + 
  theme_minimal()

# Animate difference over span size and weights for local points. A bit slow to run, but the extra time is worth it! Stand by...
dat_spans %>% 
  animate(nframes = length(unique(ethanol$E)) * 2)
```

## Concluding Remarks

Much more we *could* cover: polar plots (circular), geospatial, and so on. Please reach out if you'd like to discuss, more code, or you can just check out my recent co-authored book on an Intro to R here: https://i2rss.weebly.com/, or also an older repo from an intro to R workshop I taught a few years ago at William & Mary (https://github.com/pdwaggoner/Intro-to-R).

[Hadley Wickham](http://hadley.nz/) once [told me](https://www.r-bloggers.com/advice-to-young-and-old-programmers-a-conversation-with-hadley-wickham/) that every programmer gets frustrated all the time. This is a good sign because it means you are pushing yourself. Don't resist the frustration; embrace it and learn from it. Keep pushing yourself, and it will certainly become more fun and much better every day you do! The only way to get better at R or anything for that matter, is practice and repetition. 

As a starting place, try to code at least 1 hour everyday. If you don't know what to do, try Googling "complicated R code" and you will quickly be humbled and (hopefully) motivated to learn something new.

## Extra Content

#### Simulations

A key aspect of working with data is the use of simulations, which are more explicit versions of a common tidy concept: mapping (e.g., the `map_*()` family of functions from the tidyverse/`purrr`, which we aren't covering today).

Simulations are incredibly powerful tools allowing for creation of virtually any context you wish, e.g., 

  - testing theories under alternate constraints
  - distributional assumptions
  - real vs. predictions/forecasts, etc. 

Let's start with the `do()` approach from the tidyverse and a hypothetical baseball question. Then we will move to the more widely used and flexible `for` loop from base R and similar to uses in many other languages.

Many factors have been said to affect a baseball player's performance at the plate (i.e., the number of hits a player gets). Throughout a single season consisting of 6 months, most players will range from doing well to doing poorly at the plate, with few players ever performing at a high level for the *duration* of such a long season. The reality of the extreme difficulty of hitting baseballs that travel around 95 miles per hour across only 60 feet and 6 inches (the distance from the pitcher's mount to home plate) makes a lot of people interested in forecasting future hits, or how players will perform over the course of the season. 

> Thus, the problem becomes: *can we predict monthly hits for the average player in the context of an entire season?*

Despite the many models out there that assert they have figured out how to accurately forecast this quantity of interest, though, it turns out that player hits probably look more like random draws from a Gaussian distribution, with a mean number of hits per season for the average player around 165, the monthly average around 28 hits, and a wide standard deviation of about 12 given the extreme difficulty of hitting baseballs as well as the many other factors that go into player performance. In short, average player hits per month are *incredibly hard to forecast*, as they are virtually random and also vary widely. 

Taking the point that you basically can't predict player hits with any degree of reliability, we will take advantage of a simulation to produce several hypothetical seasons worth of hits for the average player, given these known truths (again, *this is hypothetical in the specifics, but probably mirrors the reality of forecasting baseball hits*). Specifically, we will use the information we have -- mean player hits, hits standard deviation, and a Gaussian data generating process -- to make predictions about the future. 

The punch line: **the predicted hits vary so widely, it's virtually impossible to predict player hits with any degree of reliability or certainty**. But let's see this in action.

Quick caveat: `do()` is on its way out as it basically maps functions to other values. So this is not too commonly used. but it's useful to show a tidy-friendly approach to simulation, which will help `for` loops make more sense hopefully.

```{R eval=TRUE, echo=TRUE, warning = FALSE, message = FALSE}
library(tidyverse)

# constants
hits_true <- 28 # ~165/season
hits_se <- 12  # "true" variance
n_sims <- 5 # number of times to simulate

# function for calculating a season's worth (6 months) of monthly hit samples based on true values above
baseball_fun <- function(true_mean, true_sd, months = 6, mu = 0, Iteration = 1) { # takes 5 inputs
  season <- rep(true_mean, months) + rnorm(months, # simulate draws from a normal dist
                                           mean = mu * (1:months), # allows means to vary around the true population mean + some randomness
                                           sd = true_sd) # sets variance of each simulated season
  return(data.frame(hits_number = season, # df to store our simulated values
                    month = as.factor(1:months), 
                    Iteration = Iteration))
}

# now create a df for parameters for sims (variance around mean = sd; and sim number)
# 
baseball <- data.frame(sd = c(0, rep(hits_se, n_sims)), # allows us to apply the SD to each iteration of the season --> 0 for "true" because we know that value already and don't want to sample it
                       Iteration = c("True (Average) Season", paste("Simulated Season", 1:n_sims)))

# simulate
set.seed(123)

baseball_simulation <- baseball %>% 
  group_by(Iteration) %>% 
  do(baseball_fun(true_mean = hits_true,
                  true_sd = .$sd, # this and iteration come from placeholder dataframe (hence the pipe)
                  Iteration = .$Iteration))

# viz
baseball_simulation %>% 
  ggplot(aes(x = month, 
             y = hits_number, 
             color = Iteration, # "season"
             fill = Iteration)) + 
  geom_hline(yintercept = hits_true, 
             linetype = 2) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Iteration) +
  labs(y = "Count of Hits",
       x = "Month") +
  theme_minimal()
```

Sure enough, simulating only 5 seasons based on the ground truth data we started with, there seem to be *no clear patterns*, suggesting that player hits are pretty random and tough to predict.

Now let's give simulations a try with `for` loops.

Suppose we drew a random sample of 50 respondents' self-reported political ideologies on a 7 point scale, where 1 was extremely liberal and 7 was a extremely conservative. The mean of that sample was `3.32`, suggesting the average person in this sample sees herself as generally moderate, or in the middle of the distribution of political ideology. 

Here is the code setting this up:

```{r}
sample_ideology <- c(3, 1, 2, 4, 4, 6, 1, 3, 2, 6, 
                     1, 7, 3, 1, 4, 3, 4, 4, 1, 6, 
                     7, 5, 7, 1, 1, 3, 2, 4, 1, 7, 
                     1, 2, 1, 4, 6, 3, 2, 3, 1, 4, 
                     1, 6, 3, 4, 5, 4, 1, 7, 2, 2)
mean(sample_ideology)
```


Now, suppose we wanted to simulate the reported sample ideology to see whether this random sample was reflective of the broader American population. However, we aren't exactly sure how many times to do this such that it will accurately reflect the population of interest. 

To get traction on this question, the central limit theorem and law of large numbers can help us out. To see how the *shape* of the distribution over many samples and how the *location* of these distributions change, we can use a series of `for` loops, and **plot the different distributions to see when and where (and whether) the samples converge on the underlying population**.

`for` loops allow for iterating some calculation or function over many different observations. This is a simulation. 

The syntax of `for` loops is similar to functions, where they begin with "for" and then start with some value in a sequence in parentheses. Then, within the braces, there is a statement to be evaluated. 

For each chunk below, we start by creating an empty vector in which to store our simulated values (e.g., `sm1`). We then loop sampling with replacement based on the initially-drawn sample (`sample_ideology`), and then take the mean of that iteration and store the mean in the empty vector, `sm*`. We will then plot each simulation and compare side by side via our old friend, the `patchwork` package.

```{r}
library(tidyverse)

# N = 5
sm1 <- rep(NA, 5)

for(i in 1:5){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm1[i] <- mean(samp)
}

# N = 20
sm2 <- rep(NA, 20)

for(i in 1:20){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm2[i] <- mean(samp)
}

# N = 50
sm3 <- rep(NA, 50)

for(i in 1:50){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm3[i] <- mean(samp)
}

# N = 100
sm4 <- rep(NA, 100)

for(i in 1:100){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm4[i] <- mean(samp)
}

# N = 500
sm5 <- rep(NA, 500)

for(i in 1:500){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm5[i] <- mean(samp)
}

# N = 1500
sm6 <- rep(NA, 1500)

for(i in 1:1500){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm6[i] <- mean(samp)
}

# N = 3500
sm7 <- rep(NA, 3500)

for(i in 1:3500){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm7[i] <- mean(samp)
}

# N = 7000
sm8 <- rep(NA, 7000)

for(i in 1:7000){
  samp <- sample(sample_ideology, 30, replace = TRUE)
  sm8[i] <- mean(samp)
}

# Now plot each simulation
library(patchwork)

p1 <- quickplot(sm1, geom = "histogram", main = "N=5", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p2 <- quickplot(sm2, geom = "histogram", main = "N=20", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p3 <- quickplot(sm3, geom = "histogram", main = "N=50", bins = 30) +
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p4 <- quickplot(sm4, geom = "histogram", main = "N=100", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p5 <- quickplot(sm5, geom = "histogram", main = "N=500", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p6 <- quickplot(sm6, geom = "histogram", main = "N=1500", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p7 <- quickplot(sm7, geom = "histogram", main = "N=3500", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

p8 <- quickplot(sm8, geom = "histogram", main = "N=7000", bins = 30) + 
  theme_minimal() + 
  geom_vline(xintercept = 3.32, linetype="dashed", color = "red")

# piece ggplot objects together with the patchwork package
p1 +
  p2 +
  p3 +
  p4 +
  p5 +
  p6 +
  p7 +
  p8
```

#### R Markdown

Now that we know how to do some basic tasks in R, let's talk about R markdown a bit. Basically, R markdown is a document editor and generator allowing for well-formatted technical documents with code and output able to be directly inserted. This document was created with R markdown, in fact. 

Perhaps the biggest value of R markdown is replication. The document will not compile if the inserted code has errors or is missing something, even as small as a single comma in an expression. So while often frustrating, R markdown (and any other markdown languages, e.g., yaml, or document/code editors such as Jupyter Notebooks/Lab) ensures that whatever is written in the document can be successfully run and re-run and re-run, and so on.

And you can (and should) use R markdown from *within* R Studio, as we have today. 

To start a new `.Rmd` file, first select the icon for creating a new document in the upper left portion of the screen. Then, in the drop-down menu, select `R Markdown...`. It will populate the editor pane with a template including some basic sample code. And you're off! 

Once you have finished your document and would like to format, simply select `Knit` at the top menu bar in R Studio. You can either select the type of output document (PDF, HTML, etc.) or prespecify in the metadata at the outset of the document, e.g., `output: html_document` or `output: pdf_document` or `output: word_document`, and so on.

Though there is much more to R markdown syntax, we will cover just a few basics here.

First, formatting text. Though less intuitive than Microsoft Word, at least formatting in R markdown is more straightforward than formatting text in \LaTeX. To italicize, either use `*` or `_` before and after the *section to be emphasized*. For bold, use `**` before and **after**. For footnotes, use `^` with the following text in brackets, like^[this]. And to strike through, use `~~` before and after the text to ~~strike through~~. 

Second, ordered and unordered lists, and block quotes. Lists are simple. For unordered, use either `*` or `-` and then a space, and then the text (if no space, it will think you mean to italicize). For ordered lists, simply write the number followed by a period and a space and then start writing. For nested lists, indent once (or twice) on the next line and use `+` followed by a space. 

An unordered list:

* item 1
  + sub
  + sub
* item 2
* item 3
  + sub
  
And an ordered list:

1. item 1
    + sub
        + sub sub
    + sub
2. item 2
3. item 3
    + sub

You can also include block quotes within text using `>`. For example, here is a line of text that is in my document. But now we should break to emphasize a great quote:

> Here is the great quote. Notice also that when you start a block quote in R markdown, the text is green within RStudio.

Third, code chunks. R markdown allows you to directly insert (and thus evaluate) code via "code chunks", both in R as well as a variety of other programming languages into the document. To start a chunk, type \`\`\` and then the name of the language in braces, e.g., r. First, of course, we have R,

```{R echo = FALSE, eval = FALSE}
print("Hello, World!")
```

You can also insert bash (shell/UNIX-like),

```{bash echo = FALSE, eval = FALSE}
osx:~ $ echo "Hello, world!"
```

Or Python,

```{python echo = FALSE, eval = FALSE}
print('Hello, world!')
```

Or C++, 

```{Rcpp echo = FALSE, eval = FALSE}
#include <iostream>
using namespace std;
int main() 
{
    cout << "Hello, World!";
    return 0;
}
```

Finally, equations, links, and tables. In line equations are denoted by `$` like in LaTeX, as TeX is also supported by R markdown. And also like TeX, use `\` to call mathematical symbols or Greek letters.^[Here is a great resource for mathematical operators, Greek letters, etc. in R markdown: <https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html>] For example, `$\sum_{i=1}^{n} \lambda (x_i-\bar{x})$` gives you $\sum_{i=1}^{n} \lambda (x_i-\bar{x})$. Use double `$$` to separate equations from text and center them, e.g., $$\sum_{i=1}^{n} \lambda (x_i-\bar{x})$$ 

To link to external sites, put the linked text in brackets, `[]`, followed immediately by the url in parentheses `()`. So, e.g., `[R Studio](https://www.rstudio.com/)` produces [R Studio](https://www.rstudio.com/).

And for tables, simply use `-----` for rows and `|` for columns. For example, 

here | is | a | table | with | seven | columns
-----|----|----|-----|----|----|----
and | it | also | has | three | rows | ...
here | is | the | third | row | ...| ...

#### Feature Engineering for Missing Data

The concept of *feature engineering*, though fancy sounding, really just means to create or transform features for some reason or future task. The **reason** could be overcoming limitations in data, and the **future task** could be for a modeling requirement. Learning how to approach contexts necessitating feature engineering at some level requires intimately knowing and thinking carefully about the structure of your data. These skills will serve you well in *any* computational modeling context. 

More specifically for our purposes, we will only scratch the surface here by covering feature engineering *in the context of missing data.* This is meant to get you started down the right path when you encounter one of the most common and most frustrating issues in working with data. 

Some of this discussion was adapted from the excellent and recent Kuhn and Johnson 2019 [book](http://www.feat.engineering/) on feature engineering and selection. I **strongly** encourage you to dive deeper into that book and the supporting code. Both will serve you as excellent resources for a long time. 

Here are the topics to conclude:

  * Diagnosing & Working with Missing Data
  * Essential Feature Engineering for Missing Data

If left undetected or unaddressed, missing data can plague any project. In the social sciences, for example, this is an extremely common occurrence as we are usually dealing with humans, surveys, and other data structures produced by flawed people or institutions. Compared to other realms of data production like computational statistics/simulated data that often result in complete datasets, the social sciences must anticipate that at least some of the data they collect or use will be missing or flawed in some important way. 

Yet, though frustrating, missing data is certainly *not* an insurmountable obstacle.

We will cover the basics of how to diagnose and address missing data, as a common step prior to the "engineering" part of feature engineering. Although missing data and feature engineering are often addressed together as we are doing today, these two don't necessarily have to go hand-in-hand. For example, we may detect missing data, but leave it unaddressed for some project-specific reason. Or alternatively, we may be interested in engineering new features from data, but entirely separate from the question of missingness. Still, researchers most commonly correct for missing data using feature engineering techniques. So we will progress accordingly.

We will first discuss common types of missing data, and thus how best to think about missingness. This step will help with developing steps needed to address the issue. Then, with the structure of missingness in mind, we will proceed to exploring (mostly visually) patterns of missingness. Intimately tied with these two steps, we will conclude with discussion and application of some common solutions to address missing data.

**Structure**

In thinking about the structure of missingness in data, we are really interested in uncovering the *cause* behind the missingness. This revelation will allow for more targeted feature engineering solutions. Some of the more common causes of missingness in data include: 

  1. Recording errors
  2. Unique instances
  3. Missing at random; *See more discussion of the nuance of types of missing data at random in the Little and Rubin (2019) paper*

The first cause of missingness has to do with errors in measurements of the original data collection effort. This can include structural error on the part of the data collectors, or it could be due to something as simple and clerical as data that got lost during the translation of field records, e.g., digitization. Regardless of how this happened, a fix for this type of missingness is entirely dependent on the scope. Solutions could range from going back out in the field to correct the errors, or something like encoding categories (more in a bit). Either way, it's important to first *inspect* the data to understand whether this type of missingness is likely or not. 

The second cause of missingness is case-based, where unique observations are missing recorded values, but at a smaller scale then the previous cause. The difference in scope will translate to difference in solutions. This is what Little and Rubin (2019) call NMAR ("not missing at random"; descriptive and creative to be sure...). These are often the most difficult cases to address, because there is some structural, underlying phenomenon that has led to the data being missing in some detectable pattern. Simple imputation methods may or may not suffice. Even if the values could be imputed, there is no guarantee that they are reflecting any substance about the individual observation, because that value was, at some level, *deterministically* missing. This is honest reporting of your process, and also act as a signal that you are ethically and carefully thinking about the underlying data generating process (as opposed to flippantly manipulating data, assuming all is well, when perhaps it is not). 

The final major cause of missing data is missing at random. This is often (and generally) considered the easiest type of missingness to correct for, mostly because there is no underlying driver behind the missingness. The probability of missing completely at random version is the same for all observations in the set. This is compared to the partially missing at random version, which suggests that the pattern of missingness is dependent on the sampled and thus observed data, but not on the unobserved data from the underlying population. 

In short, always think very carefully about your data, and *never ignore missing cases*. If you do ultimately decide to leave missing data as is in the full set, be prepared to report on your process of reaching this decision. 

**Exploring missingness**

Suppose we have carefully thought about what could be driving these patterns at a more abstract level, we would now be interesting in exploring and inspecting our data to see exactly how these patterns take shape. 

As a first step, I strongly recommend getting right to the point and inspecting where missing values are concentrated. To do so, I really like the `naniar` package, authored and contributed to by some excellent programmers and statisticians (e.g., Hadley Wickham, Di Cook, Nick Tierney, and so on). The reason I use this package as a starting place for much of my work is captured succinctly in the package description: 

> `naniar` is a package to make it easier to summarise and handle missing values in R. It strives to do this in a way that is as consistent with tidyverse principles as possible.

Let's walk through some of the basics of exploration via `naniar`. To do so, we will use the `congress` data, which includes legislator-level features from the 109th and 110th Congresses (i.e., 2005-2009). 

```{r echo = TRUE, eval = FALSE}
library(naniar)
library(tidyverse)
library(here)

congress <- read_csv(here("data", "congress_109_110.csv"))

# cumulative missingness over cases/observations
congress %>% 
  gg_miss_case_cumsum()

# missing data by feature
congress %>% 
  gg_miss_var()

## and also facet within the function call

congress %>% 
  gg_miss_var(latino) # facet by "latino" or not

# intersection of missing cases across features
congress %>% 
  gg_miss_upset()

# check where missing data exist in the full data space (feature-level)
congress %>% 
  select(order(desc(colnames(.)))) %>% # alphabetize for aesthetics only
  gg_miss_which()
```

**Some Solutions** 

So, we've detected missing data. What now? We essentially have two main options, at least practically *defensible* options:

  1. Deletion of the data
  2. Imputation
  
First, we have the option to simply **delete** the features or observations that contain the missing data. I would strongly suggest that, unless you have a very good reason for doing so, that you avoid this option at all costs. This is because you  ideally *never* want to throw away data completely, especially originally collected data. This is due to questions of expense, but also due to not knowing future uses and value of the data. You may not be interested in retaining the data now, but you may in the future (meaning in the future of the current project, as well as future projects entirely). At the very least, allow yourself the option to work both with *and* without the data in question. The simplest solution here is to create a copy of the original data, and work with and manipulate that, rather than manipulating and modeling the original data. In short, go through the other options coming up next very carefully, and if you still thinking deletion is the best path forward, *do so at your own risk*. That said, if you decide to delete the data, you must make the decision to do so at either of the following levels: **feature** (column) or **observational** (row). For the former, you might delete all features in the data with missingness at or above some threshold (e.g., 1, if no missingness is allowed). And for the latter, the same constraint should guide this process, e.g., removing entire rows (and necessarily throwing away other information and cases) if missingness is at or above some threshold. In sum, you *will* be throwing away some amount of useful information when you opt for brute-force-style deletion. You just have to decide whether the trade-off is worth it. This certainly is a case-by-case, project-by-project consideration. 

Second, usually the best option is to **impute** new values for missing cases. This means to make a best guess at what a likely value for the missing case could be, which requires some new calculation to be substituted for the missing value. There are *many* ways to think about how best to calculate the values, and indeed the selection depends on the task at hand, as you might have suspected. For example, you might impute a new value for a missing instance based on the mean or median (or mode for categorical features) of the feature. Or, you might look at a small neighborhood of observations that look similar to the missing case along several other feature values, and then impute based on the real values of the similar observations for the missing value in the given observation or feature. Such an approach is using k-Nearest Neighbors (kNN) for imputing new values. Exactly how you think about and employ imputation depends on the goals, nature and structure of the feature with missing values (e.g., categorical or numeric), assumptions you are willing to take on, and so on. But these aspects aside, imputation boils down to generating some new value as a best guess for the missing value, based on some reasonable metric and thus characterization of that missing case.

Let's walk through a couple of examples of imputing missing values using an excellent R package, `recipes`, written by Max Kuhn and of course, Hadley Wickham. In brief, there are four main functions in the `recipes` package:

  1. `recipe()`: defines combinations of features to use in the imputation procedure, and thus which features matter for imputation
  2. `prep()`: performs parameter estimation
  3. `juice()`: produces new feature values for a *full* set of data
  4. `bake()`: if not juicing, then `bake()` follows the recipe for a different set of data

Here is a simple example where we will impute missing data for the `votepct` feature (vote percentage received by a legislator in their most recent election) from the `congress` data previously used. We will impute based on the mean values for real (non-missing) cases across the feature.

```{r echo=TRUE, eval=FALSE}
library(recipes)
library(skimr)
library(tidyverse)
library(here)

congress <- read_csv(here("data", "congress_109_110.csv"))

# sample model to predict legislative effectiveness (LES) as a function of ideology (dwnom1) and votepct
recipe <- recipe(les ~ dwnom1 + votepct, 
                     data = congress) %>%
  step_meanimpute(votepct) 

mean_impute <- prep(recipe) %>% 
  juice()
```

So we have no imputed our new values. Let's take a look and make sure things look correct. To do so, we will first `skim()` the original feature, expecting to see a `complete_rate < 1.0`, indicating there are some missing cases. This is also denoted as an integer under `n_missing`, indicating the number of missing values across the full set. 

```{r echo=TRUE, eval=FALSE}
skim(congress$votepct) 
```

Now, with this as a baseline, we will then `skim()` the `votepct` feature, but from our new, imputed set, `mean_impute`. We should see a `complete_rate = 1.0` and `n_missing = 0`, suggesting we have fixed the issue. Take a look also at the summary statistics. These should be extremely close, if not identical. If these are way off, then perhaps our imputation method is suboptimal. 

```{r echo=TRUE, eval=FALSE}
skim(mean_impute$votepct) 
```

Importantly, with the `recipes` package, you can do a bunch of other very useful transformations and feature engineering, such as scaling features (e.g., `step_normalize()` or `step_scale()`), compute principal components from the features (e.g., `step_pca()`; more on this in a couple days when we get to PCA and autoencoders), mean-center the data (e.g., `step_center()`), and so on. 

Of note, there are many other approaches to imputing missing data, from Bayesian approaches (e.g., `impute()` from the `bnlearn` package (Scutari, 2009)) to multiple imputation by chaining equations (e.g., `mice()`, `with()`, and `pool()` from the `mice` package (Buuren and GroothuisOudshoorn, 2010)), and further still at the feature level based on averages and statistical summaries of raw feature values (e.g., `step *impute()` from the `recipes` package as we covered above). To reiterate: selection of an imputation method requires careful thought and *justiﬁcation.* For example the former two options are more firmly rooted in statistical theory, compared to the latter option which is less assumption-laden and based on learning patterns from the data as a strategy for imputing missing cases. Rarely is an imputation method or strategy the “right” one.
