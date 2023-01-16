# Required libraries loading

library(tidyverse)
library(smplot2)
library(cowplot)
library(fs)
library(here)
library(shiny)

# Setting up Here package

here::i_am("Simple_plot_time_series.Rproj")

# Read all datafiles

  ## Create object with the path to the data
data_path <- paste(here("data", "raw_data"), list.files(here("data", "raw_data")), sep = "/")

  ## Creates a list of tibble (representing the dataframes to be read)
df_input <- map(data_path, readxl::read_excel)

  ## Naming the dataframe according to the names of the original files
names(df_input) <- str_replace(list.files(here("data", "raw_data")), pattern = ".xlsx", replacement = "")

  ## Merge the two dataframes in a single one
merged_df <- bind_rows(df_input, .id = 'condition')


# Tidy the data

  ## Tidy

df_tidy <- pivot_longer(merged_df, 
                        cols = -c(condition, "Time (sec)"),
                        names_to = "Sample",
                        values_to = "Value")
#At this stage all numerical data (Time and value) in the tibbles are <dbl> (numerical with decimal)

# Format the data
    ### Test "all in one" formula 
    #### Replaces space (" ") by underscore ("_")
    #### Renames 'Time (sec)' by Time
    #### Deletes the rows with NA values
    #### Removes the " signs
    #### Saves the formatted dataframe

df_tidy_formatted <- df_tidy %>%
  mutate_if(is.character, str_replace_all, " ", "_") %>%
  rename(Time = 'Time (sec)') %>%
  drop_na() %>%
  mutate(across(where(is.character), ~ gsub("\"", "", .x)))
  
  # Old code (changes the datatype from dbl to chr
    # mutate(across(everything(), ~ map_chr(.x, ~ gsub("\"", "", .x))))

      # Test1 change columns Time and value from chr to dbl:
        #df_tidy_no_quote <- df_tidy %>% mutate(Sample=str_remove_all(Sample,'"'))
          #This is ok, but works only for the specified column
      # Test2 change columns Time and value from chr to dbl:
        # df_tidy_no_quote <- df_tidy %>%
          # mutate(across(where(is.character), ~ gsub("\"", "", .x)))
            #This works well -> Replaced in initial code

# Save the formatted data in csv format
df_tidy_formatted %>%
  write.csv(file.path(here("data", "processed_data"), "df_tidy_formatted.csv"),
            row.names = FALSE)


----------------------------
# Plot the data step by step:
  # Generate "empty" plot by providing the axis and data to plot:
    # To do so we use the aesthetics function to specify what we want to display
    # in the figure (here which data on which axis)
  
ggplot(mapping = aes(x = Time, y = Value), data = df_tidy_formatted)

  # Displaying the data using geometry layers:
    # Tell to ggplot how to display the data using geoms (geometry layers)


ggplot(mapping = aes(x=Time, y=Value, group = Sample), data = df_tidy_formatted) +
  geom_line(size = 0.1, aes(color = condition)) +
  theme(legend.position = "none") +
  facet_wrap(~ condition) 
  
  

ggplot(data = df_tidy_formatted,
       aes(x = Time,
           y = Value,
           group = Sample)) + geom_line(size = 0.1, aes(color=condition)) +
  theme(legend.position = "none") +
  facet_wrap(~ condition)

# These chunks are providing a acceptable plot but without stats

## To add stat, we need to compite them before plotting:
  # We attributed these to a new df (sumamrized_data_df)
  # To calculate stats for each Time point we need to group the data by time
    # Then we calculate for all n the mean and the value.

summarized_data_df <- df_tidy_formatted %>% group_by(Time) %>% summarise(n=n(), mean = mean(Value), sd = sd(Value))

# These graphs are ok, need to add the statistics
