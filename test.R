
load_packages <- function(package_names) {
  list.of.packages <- as.vector(package_names)
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  sapply(list.of.packages, require, character.only = TRUE)
}

load_packages(c('rmarkdown', 'tidyverse', 'viridis', 'maps', 'RJSONIO', 'glmnet', "shiny", "shinydashboard"))
load_data <- function(directory, regex, column_types, col_names=TRUE) {
  list.files(path = directory, pattern = regex, full.names = T) %>%
    lapply( (function (file) read_csv(file, col_types = column_types, col_names = col_names)) ) %>% dplyr::bind_rows()
}

column_types = cols(
  best_of = 'c',        # Parsed later (factor)
  round = 'c',          # Parsed later (factor)
  tourney_id = 'c',     # Parsed later (factor)
  tourney_name = 'f',   # Parsed later (factor)
  tourney_level = 'f',  # Parsed later (factor)
  surface = 'f',        # Parsed later (factor)
  winner_entry = 'f',   # Parsed later (factor)
  winner_name = 'f',    # Parsed later (factor)
  winner_hand = 'f',    # Parsed later (factor)
  winner_ioc = 'f',     # Parsed later (factor)
  loser_entry = 'f',    # Parsed later (factor)
  loser_name = 'f',     # Parsed later (factor)
  loser_hand = 'f',     # Parsed later (factor)
  loser_ioc = 'f',      # Parsed later (factor)
  winner_id = 'f',      # Parsed later (factor)
  loser_id = 'f',       # Parsed later (factor)
  
  winner_age = 'd',
  loser_age = 'd',
  
  draw_size = 'c',      # Parsed later (some non numeric in WTA)
  tourney_date = 'c',   # Parsed later (needs to specify format)
  match_num = 'i',
  winner_seed = 'c',    # Parsed later (some non numeric in WTA)
  winner_ht = 'i',
  loser_seed = 'c',     # Parsed later (some non numeric in WTA)
  loser_ht = 'i',
  minutes = 'i',
  w_ace = 'i',
  w_df = 'i',
  w_svpt = 'i',
  w_1stIn = 'i',
  w_1stWon = 'i',
  w_2ndWon = 'i',
  w_SvGms = 'i',
  w_bpSaved = 'i',
  w_bpFaced = 'i',
  l_ace = 'i',
  l_df = 'i',
  l_svpt = 'i',
  l_1stIn = 'i',
  l_1stWon = 'i',
  l_2ndWon = 'i',
  l_SvGms = 'i',
  l_bpSaved = 'i',
  l_bpFaced = 'i',
  winner_rank = "i",
  winner_rank_points = 'i',
  loser_rank = "i",
  loser_rank_points = 'i'
)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

singles_atp_DF <- load_data('./data/tennis_atp', "atp_matches_\\d*.csv", column_types)
singles_wta_DF <- load_data('./data/tennis_wta', "wta_matches_\\d*.csv", column_types)

combined_DF <- bind_rows(list(ATP = singles_atp_DF, WTA = singles_wta_DF), .id = "league") %>%
  separate(col = tourney_id, into=c("year", "tourney_id"), sep="-", extra="merge") %>%
  mutate(league = factor(league),
         best_of = factor(best_of, ordered = TRUE, levels = c("3", "5")),
         round = factor(round, ordered = TRUE),
         
         tourney_name = factor(tourney_name),
         tourney_level = factor(tourney_level),
         surface = factor(surface),
         winner_entry = factor(winner_entry),
         winner_name = factor(winner_name),
         winner_hand = factor(winner_hand),
         winner_ioc = factor(winner_ioc),
         loser_entry = factor(loser_entry),
         loser_name = factor(loser_name),
         loser_hand = factor(loser_hand),
         loser_ioc = factor(loser_ioc),
         winner_id = factor(winner_id),
         loser_id = factor(loser_id),
         
         tourney_id = factor(tourney_id),
         year = as.integer(year),
         tourney_date = parse_date(tourney_date, format="%Y%m%d"),
         
         draw_size = suppressWarnings(as.numeric(draw_size)),
         winner_seed = suppressWarnings(as.numeric(winner_seed)),
         loser_seed = suppressWarnings(as.numeric(loser_seed))
  )


#### Merging ATP and WTA player CSV files

col_names <- c(
  "player_id", "first_name", "last_name", "hand", "birth_date", "ioc"
)

column_types <- cols(
  player_id = "c",  # Parsed later (factor)
  first_name = "c",
  last_name = "c",
  hand = "c",       # Parsed later (factor)
  birth_date = "c", # Parsed later (needs to specify format)     
  ioc = "c"         # Parsed later (factor)
)


players_atp_DF <- load_data('./data/tennis_atp', "atp_players.csv", column_types, col_names)
players_wta_DF <- load_data('./data/tennis_wta', "wta_players.csv", column_types, col_names)

players_DF <- bind_rows(list(ATP = players_atp_DF, WTA = players_wta_DF), .id = "league") %>%
  mutate(
    league = factor(league),
    player_id = factor(player_id),
    hand = factor(hand),
    birth_date = suppressWarnings(parse_date(birth_date, format="%Y%m%d")),
    ioc = factor(ioc))

#### Merging ATP and WTA rankings CSV files

col_names <- c("ranking_date", "rank", "player", "points")
column_types <- cols(
  ranking_date = "c", # Parsed later (needs to specify format)
  rank = "i",
  player = "c",       # Parsed later (factor)
  points = "i"
)


rankings_atp_DF <- load_data('./data/tennis_atp', "atp_rankings_(70|80|90|00|10)s.csv", column_types) %>%
  bind_rows(load_data('./data/tennis_atp', "atp_rankings_(20)s.csv", column_types, col_names))


col_names <- c("ranking_date", "rank", "player", "points", "undefined")
column_types <- cols(
  ranking_date = "c", # Parsed later (needs to specify format)
  rank = "i",
  player = "c",       # Parsed later (factor)
  points = "i",
  undefined = "c"     # Extra column
)

rankings_wta_DF <- load_data('./data/tennis_wta', "wta_rankings_(80|90|00|20)s.csv", column_types, col_names) %>%
  bind_rows(load_data('./data/tennis_wta', "wta_rankings_(10)s.csv", column_types)) %>%
  select(c("ranking_date", "rank", "player", "points"))


rankings_DF <- bind_rows(list(ATP = rankings_atp_DF, WTA = rankings_wta_DF), .id = "league") %>%
  mutate(
    league = factor(league),
    ranking_date = parse_date(ranking_date, format="%Y%m%d"),
    player = factor(player))

#### Load Country Codes CSV file
column_types <- cols(
  country = "c",
  ioc = "c"
)

country_DF <- load_data('./data', "country_codes.csv", column_types)

nrow(singles_atp_DF)
ncol(singles_atp_DF)
sapply(singles_atp_DF, class)

nrow(singles_wta_DF)
ncol(singles_wta_DF)
sapply(singles_wta_DF, class)

nrow(combined_DF)
ncol(combined_DF)
sapply(combined_DF, class)

#doublesDF <- load_data('./data/tennis_atp', "atp_matches_doubles_\\d*.csv",
#                       cols( winner_entry = "c", loser_entry="c"))



split_data <- function(data, test) {
  spec = c(train = 1 - test, test = test)
  N = nrow(data)
  
  set.seed(Sys.time())
  data_split = sample(cut( seq(N), N * cumsum(c(0, spec)), labels = names(spec) ))
  
  return(split(data, data_split))
}

data_set <- combined_DF %>% filter(year > 1980) %>% 
  select(c(winner_rank, league, year, surface, tourney_level, winner_hand, winner_age,
           loser_hand, loser_age, round, minutes, w_ace, w_df, l_ace, l_df)) %>%
  na.omit() %>%
  split_data(0.1)



build_linear_model <- function(type, formula, data, kfolds) {
  
  if (type == "lm") {
    lmodel <- lm(formula, data) # OLS library
    model <- list(lmodel,
                  step(lmodel, trace=0),
                  step(lmodel, trace=0, k=log(nrow(data))))
    names(model) <- list('lm', 'aic', 'bic')
  } else {
    data_matrix = model.matrix(~.+0, data)
    # https://web.stanford.edu/~hastie/TALKS/enet_talk.pdf alpha==0(ridge), alpha==1(lasso)
    alpha <- if (type == "lasso") 1 else 0

    model = cv.glmnet(x=data_matrix[,2:ncol(data_matrix)], y=data_matrix[,1], alpha=alpha,
              lambda=NULL, nfolds=kfolds)
  }

  return(model)
}

lm_models <- build_linear_model("lm", winner_rank ~ ., data_set$train, 5)
lm_models$lasso <- build_linear_model("lasso", winner_rank ~ ., data_set$train, 5)
lm_models$ridge <- build_linear_model("ridge", winner_rank ~ ., data_set$train, 5)

lm_models



ui <- dashboardPage(
  dashboardHeader(title = "Winner Rank"),
  dashboardSidebar(
    div("Regression Models"),
    sidebarMenu(
      menuItem("Linear Model", tabName = "lm", icon = icon("th")),
      menuItem("Linear Model with AIC", tabName = "lm_aic", icon = icon("th")),
      menuItem("Linear Model with BIC", tabName = "lm_bic", icon = icon("th")),
      menuItem("LASSO regression", tabName = "lasso", icon = icon("th")),
      menuItem("RIDGE regression", tabName = "ridge", icon = icon("th"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "lm",
              fluidRow(
                h3("Plain old Linear Model"),
                box(plotOutput("lm", width=600, height = 400))),
              fluidRow(
                box(width=600, h4("Model Summary"),
                    verbatimTextOutput("lm_summary")))
      ),
      
      tabItem(tabName = "lm_aic",
              fluidRow(
                h3("Linear Model with Akaike Information Criterion"),
                box(plotOutput("lm_aic", width=600, height = 400))),
              fluidRow(
                box(width=600, h4("Model Summary"),
                    verbatimTextOutput("lm_aic_summary")))
      ),
      
      tabItem(tabName = "lm_bic",
              fluidRow(
                h3("Plain old Linear Model"),
                box(plotOutput("lm_bic", width=600, height = 400))),
              fluidRow(
                box(width=600, h4("Model Summary"),
                    verbatimTextOutput("lm_bic_summary")))
      ),
      
      tabItem(tabName = "lasso",
              fluidRow(
                h3("Plain old Linear Model"),
                box(plotOutput("lm_lasso", width=600, height = 400))),
              fluidRow(
                box(width=600, h4("Model Summary"),
                    verbatimTextOutput("lm_lasso_summary")))
      ),
      
      tabItem(tabName = "ridge",
              fluidRow(
                h3("Plain old Linear Model"),
                box(plotOutput("lm_ridge", width=600, height = 400))),
              fluidRow(
                box(width=600, h4("Model Summary"),
                    verbatimTextOutput("lm_ridge_summary")))
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  plot_residuals <- function(model, name) {
    residuals <- residuals(model)
    ggplot(data.frame(Residuals=abs(residuals), Predictions=predict(model)), aes(Predictions, Residuals)) +
      geom_point(color="blue") +
      labs(title=paste0("Homoscedascity for residuals of ", name)) +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  }

  output$lm <- renderPlot({
    plot_residuals(lm_models$lm, "Liniar Model")
  })
  output$lm_summary <- renderPrint({
    summary(lm_models$lm)
  })
  
  output$lm_aic <- renderPlot({
    plot_residuals(lm_models$aic, "Liniar Model with AIC criterion")
  })
  output$lm_aic_summary <- renderPrint({
    summary(lm_models$aic)
  })
  
  output$lm_bic <- renderPlot({
    plot_residuals(lm_models$bic, "Liniar Model with BIC criterion")
  })
  output$lm_bic_summary <- renderPrint({
    summary(lm_models$bic)
  })
  
  output$lm_lasso <- renderPlot({
    plot_residuals(lm_models$lasso, "Lasso LM Model")
  })
  output$lm_lasso_summary <- renderPrint({
    summary(lm_models$lasso)
  })
  
  output$lm_ridge <- renderPlot({
    plot_residuals(lm_models$ridge, "Ridge LM Model")
  })
  output$lm_ridge_summary <- renderPrint({
    summary(lm_models$ridge)
  })
  
}

shinyApp(ui, server)

# Create Shiny app ----
#shinyApp(ui, server)
