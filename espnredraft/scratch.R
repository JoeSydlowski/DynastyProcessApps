json_data2 <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/805175?scoringPeriodId=2&view=mRoster&view=mTeam&view=mMatchupScore', flatten = TRUE)

df <- json_data2[["teams"]][["roster.entries"]][[9]] %>%
  select(playerPoolEntry.player.fullName,
         playerPoolEntry.player.eligibleSlots,
         lineupSlotId,
         playerPoolEntry.appliedStatTotal)
