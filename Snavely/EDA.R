library(tidyverse)
library(sportyR)

# skaters <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/skaters.csv")
# Reading in files
HG_Events <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-10-25.Team.H.@.Team.G.-.Events.csv")
HG_Shifts <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-10-25.Team.H.@.Team.G.-.Shifts.csv")
HG_Tracking <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-10-25.Team.H.@.Team.G.-.Tracking.csv")
  
CD_Events <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-11-15.Team.D.@.Team.C.-.Events.csv")
CD_Shifts <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-11-15.Team.D.@.Team.C.-.Shifts.csv")
CD_Tacking <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-11-15.Team.D.@.Team.C.-.Tracking.csv")
  
EF_Events <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-11-16.Team.F.@.Team.E.-.Events.csv")
EF_Shifts <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-11-16.Team.F.@.Team.E.-.Shifts.csv")
EF_Tracking <- read.csv("/Users/lukesnavely/Desktop/CMU/Capstone/2024-11-16.Team.F.@.Team.E.-.Tracking.csv")


## Data cleaning
# Converting tracking to a time object
HG_Tracking <- HG_Tracking |> 
  mutate(seconds = ms(Game.Clock))

# Converting shift start and end to a time object
HG_Shifts <- HG_Shifts |> 
  mutate(shift_start = ms(start_clock),
         shift_end = ms(end_clock))

# Combining events
Events <- rbind(HG_Events, CD_Events, EF_Events)

# Table of events
table(Events$Event)


## Where do takeaways and recoveries happen?
Havoc <- Events |> 
  filter(Event %in% c("Takeaway", "Puck Recovery")) |> 
  mutate(absolute_x = abs(X_Coordinate))

geom_hockey(league = "NHL", display_range = "offense") + 
  geom_point(data = Havoc,
             aes(x = absolute_x, y = Y_Coordinate, col = Event),
             size = 2)

# Takeaway leaders
Havoc_leaders <- Events |> 
  filter(Event %in% c("Takeaway", "Puck Recovery")) |> 
  group_by(Team, Player_Id) |> 
  summarize(
    Takeaways = sum(Event == "Takeaway"),
    Recoveries = sum(Event == "Puck Recovery")
  )

Havoc_leaders |> 
  ggplot(aes(x = Player_Id, y = Recoveries)) +
  geom_col() +
  facet_wrap(~ Team)


## Shift number
shift_number <- numeric(nrow(HG_Tracking))

for (i in 1:nrow(HG_Tracking)) {
  for (j in 1:nrow(HG_Shifts)) {
    if(HG_Tracking$seconds[i] <= HG_Shifts$shift_start[j] &&
       HG_Tracking$seconds[i] >= HG_Shifts$shift_end[j] &&
       HG_Tracking$Period[i] == HG_Shifts$period[j]) {
      shift_number[i] <- HG_Shifts$shift_number[j]
    }
  }
}

# Adding shift number
shift_number <- numeric(nrow(HG_Tracking))
shift_number <- HG_Tracking |> 
  rowwise() |> 
  mutate(
    shift_number = HG_Shifts |> 
      filter(
        seconds >= shift_start,
        seconds <= shift_end,
        Period == period
      ) |> 
      pull(shift_number) |> 
      first()
  ) |> 
  pull(shift_number)
