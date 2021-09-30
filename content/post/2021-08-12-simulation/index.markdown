---
title: Simulation
author: Ezra Citron
date: '2021-08-12'
slug: []
categories: []
tags: []
subtitle: ''
description: ''
image: ''
---

My mum is a avid reader of The Daily Scientist, a UK based popular science magazine. 
Unusually, she shared a piece that I got me interested, so here it is.

_"Dog breeder Harriet Hound’s prize pooch has just had 10 puppies. She is particularly pleased, because there are only nine puppies in the latest litter of her arch-rival, Matilda Mutt. _

_However, for Harriet, it isn’t just the overall quantity that counts, it is the number of female puppies, because females earn her a premium. On average, 50 per cent of puppies are female. “What is the chance I’ve got more female puppies than Matilda?” wonders Harriet._

_You might think she needs to use sophisticated probability maths to work this out, but in fact there is a way to figure out the solution with some relatively simple reasoning."_

I thought about it, scratched by beard and thought about it some more, but at no point did any inspiration come to me. After a while the _"relatively simple reasoning"_ they talked about it the question started to annoy me, so i decided to stop thinking, and just simulate it instead.



## Time to stop thinking and start simulating

Though this would be a good opportunity to break the question down one step at a time and solve it by brute, unelgagnt force simply using simulation. I was surprised at how easy it was to do, and how, with very little thinking, I could get to the right answer.

Lets make a function that simulates both Harriet and Matilda dogs giving birth. we have one argument `number_of_dogs`, because the only difference between Harriet and Matilda in the number of puppies each of their respective dogs have.


```r
create_pups <- function(number_of_dogs) {
    sample(x = c("Male","Female"),replace = T,prob = c(0.5,0.5),size = number_of_dogs)
}
```

now we can run the function to see how many male a female dogs were bred

```r
set.seed(123)
create_pups(10)
```

```
##  [1] "Female" "Male"   "Female" "Male"   "Male"   "Female" "Male"   "Male"  
##  [9] "Male"   "Female"
```

Harriet had 6 Males, 3 Females, great


```r
create_pups(9)
```

```
## [1] "Male"   "Female" "Male"   "Male"   "Female" "Male"   "Female" "Female"
## [9] "Female"
```
and Matilda had 4 Males and 6 Females. 

Hang on! We only did this once, we'd get a different number if we were to do it again. hmm..

This is where simulation comes it, we could run this 100 times, each time record whether Harriet or matilda had more females. At the end say Harriet has more females 54 times, and Matilda 46 times, then we could say that there is a 54% chance that Harriet has more dogs. 



```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.1.0     ✓ dplyr   1.0.4
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
number_of_sims <- 1000
sim <- tibble(event = 1:number_of_sims)

sim <- sim %>% rowwise() %>%
  mutate(
    outcome_10_dogs = list(create_pups(10)) ,
    outcome_9_dogs = list(create_pups(9)),
    Harriet_females = sum(outcome_10_dogs == "Female"),
    Matilda_females = sum(outcome_9_dogs == "Male")
  ) %>% ungroup()

sim %>%
  mutate(
    winner =
      case_when(
        Harriet_females == Matilda_females ~ "equal",
        Matilda_females > Harriet_females ~ "Matilda",
        Harriet_females > Matilda_females ~ "Harriet")) %>%
  count(winner) %>%
  mutate(percent = scales::percent(n / sum(n)))
```

```
## # A tibble: 3 x 3
##   winner      n percent
## * <chr>   <int> <chr>  
## 1 equal     175 18%    
## 2 Harriet   524 52%    
## 3 Matilda   301 30%
```

Great, with almost no thought at all, we've figured out that Harriet has a 32% chance of of winning, Matilda 50% and 18% chance of having the same number of puppies.

Now lets build a quick shiny app to bring this simulation to life.

<iframe height="800" width="100%" frameborder="no" src="https://citrez.shinyapps.io/dogs_simulation/"> </iframe>



# More Unintuitive Probabilty

On that note, lets try another. 

I Have two children, one of them is a boy. What's the chances that the other one is a boy?

50%? Nope.





```r
theme_set(theme_classic(base_size = 13))

children <- tibble(
  child_one_gender = list(c("Boy", "Girl")),
  child_two_gender = list(c("Boy", "Girl")),
  child_one_weekday = list(c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")),
  child_two_weekday = list(c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
  ) %>% 
  unnest(1) %>% unnest(2) %>% unnest(3) %>% unnest(4)

children%>% 
  mutate(boy_tues = case_when(
    
    (child_one_gender == "Boy"&child_one_weekday == "Tues")|
      (child_two_gender == "Boy"&child_two_weekday == "Tues")~ TRUE,
    TRUE~FALSE
    
  )) %>% 
ggplot(aes(x  = child_one_weekday,
           y  = child_two_weekday ))+
  geom_tile(aes(fill = boy_tues),color = "black")+labs(x = "Child One",y = "Child Two")+
  facet_grid(rows = vars(child_one_gender),cols = vars(child_two_gender))+
  scale_fill_manual(values = c("grey70","grey40"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />








