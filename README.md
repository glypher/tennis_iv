# tennis_iv

## Breakdown of columns
There are 49 columns in total. Below is breakdown by data type.

### Categorical Ordered (3):
* `best_of`
  * levels = (3, 5)
* `round` Don't know what ER, Q4 and BR rounds are, but in total there are 279 matches with those labels.
  * levels = (RR, R128, R64, R32, R16, QF, SF, F)
  * RR - Round Robin (Group Stages)




### Categorical Unordered (13):
* `tourney_id`
* `tourney_name`
* `tourney_level` (e.g. G = 'Grand Slam', C = 'Challengers', etc.)
* `surface` Field surface type
  * levels = (Carpet, Clay, Grass, Hard, Indoors)
* `winner_entry`
  * WC - Wild Card. Awarded at the discretion of organisers.
  * Q - Qualifier
  * LL - Lucky Loser. Entered the competition because someone withdrew.
  * PR - Protected Ranking. In case of injury, use your pre injury ranking
  * _  - other types account for less than 1% of data
* `winner_name`
* `winner_hand` Player dominant hand (U - ambidextrous)
  * levels(L, R, U)
* `winner_ioc` International Olympic Committee (IOC) three-letter country codes
* `loser_entry`
* `loser_name`
* `loser_hand`
* `loser_ioc`
* `winner_id`
* `loser_id`


### Numerical Continuous (2):
* `winner_age`
* `loser_age`


### Numerical Discrete (30):
* `draw_size` How many players started in the 'draw' (e.g. 64 for R64, 4 for RR)
* `tourney_date`
* `match_num` Match identifier
* `winner_seed` Player starting position in the bracket
* `winner_ht` Player height
* `loser_seed`
* `loser_ht`
* `minutes`
* `w_ace` Winner aces
* `w_df` Winner Double faults
* `w_svpt` Winner service points (points won when player was the one serving)
* `w_1stIn` Winner number of 1st serves in (inside the court; not resulting in a 2nd serve)
* `w_1stWon` Winner number of 1st serves won
* `w_2ndWon` Winner number of 2nd serves won
* `w_SvGms` Winner number of service games (games in which the player was the one serving)
* `w_bpSaved` Winner break points saved
  * `Break` Player wins when it's not his service game (opponent is serving)
  * `Break point` Point which, if scored by the (serve) receiver will result in a break
* `w_bpFaced` Winner break points faced
* `l_ace`
* `l_df`
* `l_svpt`
* `l_1stIn`
* `l_1stWon`
* `l_2ndWon`
* `l_SvGms`
* `l_bpSaved`
* `l_bpFaced`
* `winner_rank`
* `winner_rank_points`
* `loser_rank`
* `loser_rank_points`

### Synthetic (1):
* `score` (Numerical Discrete)
* `year` Obtained from `tourney_id`
* `league`
  * levels = (ATP or WTA)
* `w_1stPerWon` Winner percentage of 1st serves won if 1st serve was in court (w_1stWon / w_1stIn)
* `l_1stPerWon`
* `w_breaks` Winner number of breaks (l_bpFaced - l_bpSaved)
* `l_breaks`
