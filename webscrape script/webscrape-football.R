# Code from Ryo Nakagawara
# Link: https://ryo-n7.github.io/2020-05-14-webscrape-soccer-data-with-R/

# Library required --------------------------------------------------------

library(dplyr)
library(rvest)
library(polite)
library(glue)
library(purrr)


# Checking session --------------------------------------------------------

url <- "https://us.soccerway.com/national/england/premier-league/20182019/regular-season/r48730/"
session <- bow(url)
session


# Extract relevant html ---------------------------------------------------

team_links <- scrape(session) %>% 
  html_nodes("#page_competition_1_block_competition_tables_13_block_competition_league_table_1_table .large-link a") %>% 
  html_attr("href")


# Create appropriate data frame -------------------------------------------

team_links_df <- team_links %>% 
  tibble::enframe(name = NULL) %>%
  tidyr::separate(value, c(NA, NA, NA, "team_name", "team_num"), sep = "/", remove = FALSE) %>%
  mutate(link = glue("https://us.soccerway.com/teams/england/{team_name}/{team_num}/squad/"))


# Functions extracting team name, goal, assist ----------------------------

player_name_info <- function(session) {
  
  player_name_info <- scrape(session) %>% 
    html_nodes("#page_team_1_block_team_squad_7-table .name.large-link") %>% 
    html_text()
}


num_goals_info <- function(session) {
  
  num_goals_info <- scrape(session) %>% 
    html_nodes(".goals") %>% 
    html_text()
  
  ## first value is blank so remove it
  num_goals_info_clean <- num_goals_info[-1]
}


num_assists_info <- function(session) {
  
  num_assists_info <- scrape(session) %>% 
    html_nodes(".assists") %>% 
    html_text()
  
  ## first value is blank so remove it
  num_assists_info_clean <- num_assists_info[-1]
}



# Combine functions into 1 large function ---------------------------------

premier_stats_info <- function(link, team_name) {
  
  team_name <- rlang::enquo(team_name)
  ## `bow()` for every URL link
  session <- bow(link)
  
  ## scrape different stats
  player_name <- player_name_info(session = session)
  
  num_goals <- num_goals_info(session = session)
  
  num_assists <- num_assists_info(session = session)
  
  ## combine stats into a data frame
  resultados <- list(player_name, num_goals, num_assists)
  col_names <- c("name", "goals", "assists") 
  
  premier_stats <- resultados %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names) %>% 
    mutate(team = !!team_name)
  
  ## A little message to keep track of how the function is progressing:
  # cat(team_name, " done!")
  
  return(premier_stats)
}



# To debug ----------------------------------------------------------------

safe_premier_stats_info <- safely(premier_stats_info)


# Iterations --------------------------------------------------------------

goal_contribution_df_ALL <- map2(.x = team_links_df$link, .y = team_links_df$team_name,
                                 ~ safe_premier_stats_info(link = .x, team_name = .y))

glimpse(head(goal_contribution_df_ALL, 4))


### Error check ----

goal_contribution_df_ALL %>% 
  map("error") %>% 
  purrr::discard(~is.null(.))

### Extract result ---
goal_contribution_df <- goal_contribution_df_ALL %>% 
  map("result") %>% 
  bind_rows()



# Export ------------------------------------------------------------------

readr::write_csv(goal_contribution_df, "goals-team.csv")
