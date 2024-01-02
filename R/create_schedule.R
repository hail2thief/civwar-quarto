# libraries
library(tidyverse)


# set the hard dates
start_day = "2024-01-06"
end_day = "2024-03-14"
final = "2024-03-21"
midterm = "2024-02-08"
meets_on = c("Tue", "Thu")

# create class schedule

## all days between start and end
schedule = tibble(dates = seq(from = ymd(start_day), 
                              to = ymd(end_day),
                              by = "days"),
       day = wday(dates, label = TRUE)) |> 
  # filter to meeting days
  filter(day %in% meets_on) |> 
  # filter out breaks
  #filter(!dates %in% ymd(breaks)) |> 
  # add weeks
  mutate(week = isoweek(dates), 
         week = week - (min(week) - 1), 
         week = str_pad(week, width = 2, pad = "0"))


# set homework weeks
homework_on = tibble(week = c(1, 2, 3, 5, 6, 7, 8, 9)) |> 
  mutate(week = paste0("Week ", str_pad(week, width = 2, pad = "0")))

# create condensed version so each row is a week

## pivot wider
schedule = schedule |> 
  mutate(week = paste0("Week ", week)) |> 
  pivot_wider(names_from = day, values_from = dates) |> 
  # add all the extras
  mutate(content = paste0("/content/", str_extract(week, "\\d+"), "-content"),
         assignment = ifelse(week %in% homework_on$week, 
                             paste0("/assignment/", str_extract(week, "\\d+"), "-assignment"),
                             NA)) |> 
  select(week, date = 2, day2 = 3, content, assignment)



# merge in topics
topics = tribble(~week, ~title,
        "Week 01", "Hello + what are civil wars?",
        "Week 02", "Why do people rebel? Grievances, ethnicity",
        "Week 03", "Why do people rebel? Resources",
        "Week 04", "How are wars fought? Insurgency",
        "Week 05", 'Midterm exam <i class="fa-solid fa-star"></i> + film',
        "Week 06", "How are wars fought? Counterinsurgency",
        "Week 07", "Case study: forced displacement in Colombia",
        "Week 08", "Case study: civilian resistance in Peru",
        "Week 09", "Case study: ethnic partition in Ethiopia",
        "Week 10", "How do wars end? Negotiated settlements",
        "Week 11", "Conclusions")


# finish up
schedule = schedule |> 
  left_join(topics, by = "week") |> 
  # add in exams
  add_row(week = "", 
          title = 'Final exam at 3:30pm <i class="fa-solid fa-star"></i>', date = ymd(final))

write_csv(schedule, "data/schedule.csv")
