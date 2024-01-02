# libraries
library(tidyverse)


# read in schedule
df = read_csv("data/schedule.csv")


# text to write 
text_reading = '---
title: "Hello"
---

# Readings


## Required


## Recommended


# Slides

* [Tuesday slides here]()
* [Thursday slides here]()

'

text_assignment = '---
title: "Hello"
---

## Instructions


- For the data, right-click, "save/download file as..."
- Open the data
- Complete all of the tasks associated with that data and keep track of your answers / images
- **Once you are done**, go on Canvas and answer the questions related to the assignment
    - the Canvas assignment is **timed**; if you open it prior to completing the tasks you will run out of time



Skills used: 

- 


[My awkward Google Sheets video tutorials are here](https://www.dropbox.com/sh/rsj9qm67se619pm/AAD1wPt84O8yaIfSPcwN9edua?dl=0)

'

# create the qmds
df |> 
  select(content) |> 
  drop_na() |> 
  mutate(content = str_remove(content, "^/"),
         content = paste0(content, ".qmd")) |> 
  pull() |> 
  map( .f = ~ writeLines(text = text_reading, con = .x))


# create the qmds
df |> 
  select(assignment) |> 
  drop_na() |> 
  mutate(assignment = str_remove(assignment, "^/"),
         assignment = paste0(assignment, ".qmd")) |> 
  pull() |> 
  map( .f = ~ writeLines(text = text_assignment, con = .x))
