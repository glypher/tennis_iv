# tennis_iv

## Breakdown of columns
There are 49 columns in total. Below is breakdown by data type.

### Categorical Ordered (3):
* best_of: levels(3, 5)
* round: This should be ordered? RXX, QF, SF, F are obvious. But there are also BR, ER, RR (round robin / group stages?)



### Categorical Unordered (13):
* tourney_id
* tourney_name
* tourney_level: Made a mistake, this is un-ordered factor (e.g. G = 'Grand Slam', C = 'Challengers', ect.)
* surface
* winner_entry
* winner_name
* winner_hand: levels(L, R)
* winner_ioc: Three character country code
* loser_entry
* loser_name
* loser_hand: levels(L, R)
* loser_ioc: Three character country code
* winner_id
* loser_id


### Numerical Continuous (2):
* winner_age
* loser_age


### Numerical Discrete (30):
* draw_size
* tourney_date
* match_num
* winner_seed
* winner_ht
* loser_seed
* loser_ht
* minutes
* w_ace
* w_df
* w_svpt
* w_1stIn
* w_1stWon
* w_2ndWon
* w_SvGms
* w_bpSaved
* w_bpFaced
* l_ace
* l_df
* l_svpt
* l_1stIn
* l_1stWon
* l_2ndWon
* l_SvGms
* l_bpSaved
* l_bpFaced
* winner_rank
* winner_rank_points
* loser_rank
* loser_rank_points

### Synthetic (1):
* score (Numerical Discrete)
* year: Obtained from tourney_id
* league: ATP or WTA
