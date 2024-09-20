# libraries
library(tidyverse)
library(calendar)

# set the hard dates
start_day = "2024-09-25"
end_day = "2024-12-06"
final = "2024-12-10 10:30 AM"
midterm = "2024-10-28"
breaks = c("2024-11-11")
meets_on = c("Mon", "Wed")
class_time = "13:40:00"
meeting_room = "The Grove (Surge III) 1309"

# create class schedule

## all days between start and end
schedule = tibble(dates = seq(from = ymd(start_day), 
                              to = ymd(end_day),
                              by = "days"),
       day = wday(dates, label = TRUE)) |> 
  # filter to meeting days
  filter(day %in% meets_on) |> 
  # filter out breaks
  filter(!dates %in% ymd(breaks)) |> 
  # add weeks
  mutate(week = isoweek(dates), 
         week = week - (min(week)), 
         week = str_pad(week, width = 2, pad = "0")) |> 
  mutate(dates = ymd_hms(paste(dates, class_time)))


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
  select(week, date = sym(meets_on[1]), day2 = sym(meets_on[2]), content, assignment)



# merge in topics
topics = tribble(~week, ~title,
        "Week 00", "Hello",
        "Week 01", "What are civil wars?",
        "Week 02", "Why do people rebel? Grievances, ethnicity",
        "Week 03", "Why do people rebel? Resources",
        "Week 04", "How are wars fought? Insurgency",
        "Week 05", 'Midterm exam <i class="fa-solid fa-star"></i> + in-class film',
        "Week 06", "How are wars fought? Counterinsurgency",
        "Week 07", "Case study: forced displacement in Colombia",
        "Week 08", "Case study: civilian resistance in Peru",
        "Week 09", "Case study: ethnic conflict in Ethiopia",
        "Week 10", "How do wars end? Negotiated settlements")


# finish up
schedule = schedule |> 
  left_join(topics, by = "week") |> 
  # add in exams
  add_row(week = "", 
          title = 'Final exam at 3:30pm <i class="fa-solid fa-star"></i>', 
          date = ymd_hm(final))

write_csv(schedule, "data/schedule.csv")



# make ical
schedule_long = schedule |> 
  # make schedule long
  pivot_longer(cols = c(date, day2), names_to = "type", values_to = "value") |> 
  select(title, value) |> 
  drop_na() |> 
  mutate(title = paste0("POL126F24: ", title))

dtstamp <- ic_char_datetime(now("UTC"), zulu = TRUE)

ical = schedule_long |>
  mutate(id = row_number()) |>
  group_by(id) |>
  nest() |>
  mutate(ical = map(data,
                    ~ic_event(start = .$value[[1]],
                              end = .$value[[1]] + 90*60,
                              summary = .$title[[1]],
                              more_properties = TRUE,
                              event_properties = c("DTSTAMP" = dtstamp,
                                                   "LOCATION" = meeting_room)))) |>
  ungroup() |>
  select(-id, -data) |>
  unnest(ical) |> 
  ical()


calendar::ic_write(ical, "data/schedule.ics")    
