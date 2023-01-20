# Required libraries loading

library(tidyverse)
library(smplot2)
library(cowplot)
library(fs)
library(here)
library(shiny)
library(plotly)

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
# At this stage all numerical data (Time and value) in the tibbles are <dbl> (numerical with decimal)

# Format the data
#### Test "all in one" formula 
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


# Save the formatted data in csv format
df_tidy_formatted %>%
  write.csv(file.path(here("data", "processed_data"), "df_tidy_formatted.csv"),
            row.names = FALSE)

# Calculate basic statistics on the data
  # Stats are attributed to a new df: 'stats_df_tidy_formatted'
  # To calculate the stats for each Time point we need to
    # group the data by time using: (group_by(Time))
    # and calculate for all the rows (=n),
     # the mean
     # the median
     # the sd for the column 'Value'.
  # In addition we add the 95CI on the data
  # 95CI need to be calculated:

# Define the desired confidence level:
Confidence_level = 0.95


# Generate the stats for the df

stats_df_tidy_formatted <- df_tidy_formatted %>% 
  group_by(Time, condition) %>% 
  summarise(n=n(),
            mean = mean(Value),
            median=median(Value),
            sd = sd(Value)) %>%
  mutate(sem=sd/sqrt(n),
         mean_CI_lo = mean + qt((1-Confidence_level)/2, n-1) * sem,
         mean_CI_hi = mean - qt((1-Confidence_level)/2, n-1) * sem
  )

# Save the df in csv format

stats_df_tidy_formatted %>%
write.csv(file.path(here("data",
                         "processed_data"),
                    "stats_df_tidy_formatted.csv"),
          row.names = FALSE)


## NOTE: data are grouped by time and condition ##


# Plot the data:
  # Add the first layer = original data traces with geom_line
   # + remove the legend
  # Add the second layer = 95CI as ribbon (ymax and min)
  # Add the third layer = the trace for the mean
   # split the traces by condition (facet_wrap)
plot <- ggplot(df_tidy_formatted) +
  geom_line(data=df_tidy_formatted,
            aes(x=Time,
                y=Value,
                group=condition,
                color=condition),
            size=.1) +
  theme(legend.position = "none") +
  geom_ribbon(data=stats_df_tidy_formatted,
              aes(x=Time,
                  ymin=mean_CI_lo,
                  ymax=mean_CI_hi,
                  group=condition),
              fill='blue',
              alpha=.3) + 
  geom_line(data=stats_df_tidy_formatted,
            aes(x=Time,
                y=mean),
            color='blue',
            linewidth=1,) +
  facet_wrap(~ condition)

# Save the plot (width and height to be modified as desired)

ggsave("output/plot.png", width = 6, height = 6)


# Use Plot_ly to obtain time point for TG answer:
plot_ly <- stats_df_tidy_formatted %>%
  group_by(condition) %>%
  plot_ly(x = ~Time, 
          y = ~ mean) %>%
  add_lines(color = ~condition) %>%
  layout(legend = list(orientation = 'h'))

# Export the interactive plot as png
    # Note that the function plotly::export is deprecated: Use 'orca' instead

plotly::export(p = plot_ly,
               file = "output/plot_ly.png")
# Export the intereactive plot as html (to be read in web browser)

htmlwidgets::saveWidget(plot_ly, "output/test.html")


#####------------------#####
  # Test for implementing calculation of area under the curve #
## Requirements:
    
  # Determine initial Timepoint for calculation
      # Filter the dataset (df_tidy_formatted) according to values from plotly
        # In this example: 1000 s for WT and 1130 for ORAI1
df_TG_answer <- filter(df_tidy_formatted, (grepl("WT", condition) & Time > 1130) | (grepl("ORAI1", condition) & Time > 1000))

  # Determine the duration for which area should be calculated
    # Get the number of point TG answer for each condition
      # Choose the shortest answer time between condition to proceed with calculation
# This determines the number of Timepoints following SOCE answer for each condition and stores them in the df time_point
time_point <- df_TG_answer %>% group_by(condition) %>% summarise(n_distinct(Time))

# This gives the minimum value from the df time_point
min(time_point$'n_distinct(Time)')

# Now we want to keep the same number of time point for all condition.
  # We will filter the df using the minimum number of timepoint determined above.
  # the value for N corresponds to the value from above (min(time_points$'n_distinct(Time)'))
adjusted_SOCE_answer <- df_TG_answer %>% 
  group_by(Sample, condition) %>% 
  slice_head(n=153)


# Now we want to modify the value from Time to "synchronize" our conditions 

df_AU_SOCE <- adjusted_SOCE_answer %>%
  group_by(Sample, condition) %>%
  mutate(Time_AU = seq(from = 0, to = 152*5, by = 5))
  # This works well, pay attention the number 152 comes from the line: (min(time_points$'n_distinct(Time)')) minus 1 (as we start from 0 which is considered one)

# Now we need to adjust normalize the y axis
    # Read this blog for choosing best method: https://thenode.biologists.com/data-normalization/research/

df_SOCE_normalized <- df_AU_SOCE %>%
  group_by(Sample, condition) %>%
             mutate(Norm_value = Value / dplyr::first(Value))

# This looks OK.


# It would be good to plot each step to verify that everything works fine
# We should then be ready to proceed with calculation of area undeer the curve

#### --------------------------------------- ####



# Data from df grouped by Sample and condition and 160 first value stored into the df test


## Test grouping the df by Sample and slice for the first x(=160 => minimum time points defined above)

df_TG_answer_sample <- df_TG_answer %>% group_by(Time)

fitlered_TG_answer <- df_TG_answer_sample %>% slice(160)

# Create a list of unique Time elements      
ulist <- unique(df_TG_answer$Time)  


df_160_test <- df_TG_answer %>% slice(1:160)



    # group_by()
      # Conditions defined with group
    # plot_ly()
      # What to plot defined in x and y
    # add_lines:
      # Plot values as line (link the points and do not show the single points)
    # layout(legend)
      # Position the legend  below the graph



# Integrate the area under the curve.
## Source= https://smin95.github.io/dataviz/calculating-area-under-a-curve.html


#####------------------#####
  #First test done on filtered data
    # Filter for WT

    df_wt <- filter(df_tidy_formatted, condition == '2018-10-30_PC3_WT-TG')

      # Calculate the area under the curve with trapeze method

      auc_WT <- sm_auc_all(data = df_wt,
                     subjects = 'Sample',
                     conditions = 'condition',
                     x = 'Time',
                     values = 'Value')

    # For ORAI transfected

    df_ORAI1 <- filter(df_tidy_formatted, condition == '2018-10-30_PC3+ORAI1mCh-TG')
    
      # Calculate the area under the curve with trapeze method
    
      auc_ORAI1 <- sm_auc_all(data = df_ORAI1,
                         subjects = 'Sample',
                         conditions = 'condition',
                         x = 'Time',
                         values = 'Value')


#This is OK
#####------------------#####

    # Filter the dataset (df_tidy_formatted) according to values from plotly
      # In this example: 1000 s for WT and 1130 for ORAI1
df_TG_answer <- filter(df_tidy_formatted, (grepl("WT", condition) & Time > 1000) | (grepl("ORAI1", condition) & Time > 1130))

      # Get the number of point TG answer for each condition
        # Aim is to subset the df for the smallest value
     time_point <- df_TG_answer %>% group_by(condition) %>% summarise(n_distinct(Time))      
     min(time_point$`n_distinct(Time)`)
     
        # Create a list of unique Time elements      
ulist <- unique(df_TG_answer$Time)  
      


        # Old version filter in two separate df:
          # df_TG_answer_WT <- filter(df_tidy_formatted, grepl("WT",condition) & Time > 1000)
          # df_TG_answer_ORAI1 <- filter(df_tidy_formatted, grepl("ORAI1",condition) & Time > 1130)
# This creates two df with filtered data for each condition 
      # grepl = keep the regular expression (grepl) defined
      # !grepl = drop the regular expression (grepl) defined 

# This filters as desired
  # Need to adjust length of df then merge them. 
    # Determine the length of TG answer for each condition:
#



      


    
      
# Required improvements:
# Automate the filtering for each condition
# Select the time point to analyze
      # Filter the data, example:
#TG_filter <- filter(stats_df_tidy_formatted, Time > 1163 & condition == '2018-10-30_PC3_WT-TG')
# This requires manual input for treatment
    
# Choose the length
# Define an arbitrary scale (images every 5sec --> Start at 0 until 'number_of_points*5' )



      