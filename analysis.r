library(tidyverse) # data handling
library(tidytuesdayR) # easy grab TT data
library(gridExtra) # plot grid handling
library(schrute) # all dialog from show, and metadata
library(tidylo) # log odds calcs
library(tidytext) # text handling, reorder within facets


# load office script data
script_lines <- schrute::theoffice %>% 
  as.data.frame() %>% 
  select(season, episode, character, text, text_w_direction)
  
episode_metadata <- schrute::theoffice %>% 
  as.data.frame() %>% 
  select(season, episode, episode_name, director, writer, imdb_rating) %>%
  group_by(season, episode, episode_name) %>%
  distinct()


# clean character value:
    # current work gets you from 773 unique chars down to 596
    
    # Handle case insensitivity 'Warehouse Guy' vs 'Warehouse guy' - convert all tolower()
    script_lines$clean_speaker <- script_lines$character %>% tolower()
    
    # Convert multiple speaker syntax to be consistent with a comma separated string
      # todo - this can be better -- instead of doing gsub 4x run it across a set of splitchars <- c("&", "/", " and ")
      # TODO - handle exception: DunMiff/sys shouldn't be DunMiff, sys. Move alias logic earlier. Also Bob Vance, Vance Refrigeration will be off
      script_lines$clean_speaker <- gsub("&", ", ", script_lines$clean_speaker)
      script_lines$clean_speaker <- gsub(", and ", ", ", script_lines$clean_speaker)
      script_lines$clean_speaker <- gsub(" and ", ", ", script_lines$clean_speaker)
      script_lines$clean_speaker <- gsub("/", ", ", script_lines$clean_speaker)
    
    # Handle lines with multiple speakers, create duplicate lines for each speaker.
    # Necessary for later analytics around lines that each character speaks 
    script_lines <- script_lines %>% separate_rows(clean_speaker, sep = ",")
    
    # Remove punctuation
    script_lines$clean_speaker <- str_replace_all(script_lines$clean_speaker, "[[:punct:]]", "")
    
    # Handle leading and trailing spaces
    script_lines$clean_speaker <- script_lines$clean_speaker %>% str_squish()
    
    # Rename speakers with aliases
    # Create map of character aliases to collapse duplicate names (manually created)
    office_aliases <- read.csv("~/R Projects/office-viz/office_aliases.csv")
    # join aliased name list. Keep the aliased name if it is not NA otherwise keep speaker name
    colnames(office_aliases) <- c("clean_speaker","corrected_clean_speaker")
    script_lines <- left_join(script_lines, office_aliases, by = "clean_speaker")
    script_lines$clean_speaker <- ifelse(is.na(script_lines$corrected_clean_speaker), script_lines$clean_speaker, script_lines$corrected_clean_speaker)
    
    # Convert back to initial caps for readability
    script_lines$clean_speaker <- str_to_title(script_lines$clean_speaker)

    #check to confirm character values are all legit -- look at very long ones to make sure they are OK.
    # manual inspection is good, you can ignore
    check_long_lines <- script_lines %>%
      filter(nchar(script_lines$clean_speaker) > 20)
    

# Find the unique values in the cleaned speaker col

speaker_counts <- script_lines %>% 
  filter(clean_speaker != "") %>%
  count(clean_speaker) %>%
  arrange(desc(n)) %>%
  mutate(index_row = row_number()) %>%
  mutate(main_character = ifelse(index_row %in% seq(1,10),1,0))

num_chars <- nrow(speaker_counts)
character_list <- speaker_counts[,1]

# Unique episode list
episode_list <- script_lines %>% distinct(episode, season)

# cross join to character list
episode_character_table <- merge(episode_list, character_list, all=TRUE)

# count lines in each episode
lines_per_episode <- script_lines %>% 
  group_by(clean_speaker, season, episode) %>%
  count() %>%
  rename(char_lines_per_episode = n) %>%
  arrange(season, episode, clean_speaker)

# Merge line counts to the full table, so you can see who had no lines in each episode
characters_episode_line_totals <- merge(lines_per_episode, episode_character_table, all = TRUE) %>%
  mutate(no_lines_flag = ifelse(is.na(char_lines_per_episode),1,0)) %>%
  filter(is.na(clean_speaker) == FALSE) %>%
  group_by(clean_speaker) %>%
  filter(clean_speaker != "")

characters_episodes_no_lines <- characters_episode_line_totals %>%
  summarise(episodes_with_no_lines = sum(no_lines_flag)) %>%
  arrange(episodes_with_no_lines)

characters_episodes_no_lines %>% 
  filter(episodes_with_no_lines < 185) %>%
  ggplot(aes(x = episodes_with_no_lines)) + 
  geom_histogram(binwidth = 10) + 
  labs(title = "Distribution of characters having no lines in episodes",
       x = "Number of episodes with no lines",
       y = "count of characters")

# Get episode metadata from schrute library 
  episode_metadata <- schrute::theoffice %>% 
    as.data.frame() %>% 
    select(season, episode, episode_name, director, writer, imdb_rating) %>%
    group_by(season, episode, episode_name) %>%
    distinct()
  # Split writers out into separate rows so each gets 'credit' for those words
  episode_metadata <- episode_metadata %>%
    separate_rows(writer, sep = ";") 

# Look at writers compared to character line counts  
script_line_totals_with_writers <- episode_metadata %>%
  merge(lines_per_episode, all = TRUE)

# Calc log odds for writers to give lines to characters
script_line_totals_with_writers_full <- script_lines %>%
  merge(episode_metadata, all =)

writer_char_logodds <- script_line_totals_with_writers_full %>%
  count(writer, clean_speaker) %>%
  bind_log_odds(writer, clean_speaker, n) %>%
  arrange(desc(log_odds_weighted)) #%>%
#filter(n >= 100)

# there are 40 writers, let's narrow to only those that are also actors in the show
writer_actors <- c("Mindy Kaling", "B.J. Novak", "Paul Lieberstein", "Michael Schur")

plot_writers_common_chars <- writer_char_logodds %>%
  filter(writer %in% writer_actors) %>%
  group_by(writer) %>%
  slice_max(log_odds_weighted, n=10) %>%
  mutate(clean_speaker = reorder_within(clean_speaker, log_odds_weighted, writer)) %>%
  ggplot(aes(log_odds_weighted, clean_speaker)) + 
  geom_col() + 
  facet_wrap(~writer, scales = "free_y") + 
  scale_y_reordered() + 
  labs(title = "Character line representation, by writer",
       x = "log odds (weighted) of character lines by each writer",
       y = "")

# No -- looks like our writers don't write themselves a lot of lines!
plot_writers_common_chars


# Staying on the track of writers -- J.K. Rowling was recognized as the secret author of 
# a book due to how she writes, her language and style. Are there topics that specific 
# writers favor that might key us into other things they have written?

# merge metadata (writers) with the tokenized script to get which writers contributed to which words
    # Split into word tokenization, remove stopwords

    # filler noted through inspection
    office_filler <- c("yeah", "uh", "hey", "hum")

    tokenized_script <- script_lines %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      filter(!word %in% office_filler)
      # TODO - tackle stemming for more accurate dictionary 


tokenized_with_writers <- episode_metadata %>%
  merge(tokenized_script, all = TRUE)

writer_logodds <- tokenized_with_writers %>%
  count(writer, word) %>% 
  bind_log_odds(writer, word, n) %>%
  arrange(desc(log_odds_weighted)) 


plot_writers_common_words <- writer_logodds %>%
  filter(writer %in% writer_actors) %>%
  group_by(writer) %>%
  slice_max(log_odds_weighted, n=10) %>%
  mutate(word = reorder_within(word, log_odds_weighted, writer)) %>%
  ggplot(aes(log_odds_weighted, word)) + 
  geom_col() + 
  facet_wrap(~ writer, scales = "free_y") + 
  scale_y_reordered() + 
  labs(title = "Most common words, by actor/writers in The Office",
       x = "log odds (weighted) of each writer's words",
       y = "")
plot_writers_common_words

# TODO -- text sentiment -- when aws free monthly rolls back over run through comprehend
