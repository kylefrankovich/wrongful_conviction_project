library(plotly)
library(dplyr)

df = read.csv("NRE_data.csv") # load in National Register of Exonerations data

unique(df$State)

# add a ones colums, add them to get # of exonerations

df$exoneration = 1 # this is maybe not the most straightforward way to get count data,
# but it's simple and it works

# get state count:

state.count <- df %>%
  group_by(State) %>%
  summarise(
    exoneration_sum = sum(exoneration)
    )

# for the moment I'm removing exonerations labelled by "Fed" (86 total) as opposed to just 
# a state designation; I'll want to add these values back into the normal state 
# counts at some point; assuming these are federal vs. state level convictions; 
# in addition I'm also not looking at PR (3), Guam (1), and DC (15), all of which are 
# included in the dataset

state.count.Fed_only = filter(state.count, grepl("Fed", State))
state.count.PR_only = filter(state.count, grepl("Puerto Rico", State))
state.count.Guam_only = filter(state.count, grepl("Guam", State))
state.count.DC_only = filter(state.count, grepl("District of Columbia", State))

sum(state.count.Fed_only$exoneration_sum) # 86 federal exonerations
sum(state.count.PR_only$exoneration_sum) # 3 PR exonerations
sum(state.count.Guam_only$exoneration_sum) # 1 Guam exonaeration
sum(state.count.DC_only$exoneration_sum) # 15
sum(state.count.Fed_removed$exoneration_sum) # 1595 total exonerations in fed/territory 
# removed dataset
sum(state.count$exoneration_sum) # 1700 total exonerations including fed/territory

state.count.Fed_removed = filter(state.count, !grepl("Fed", State))
state.count.Fed_removed = filter(state.count.Fed_removed, !grepl("Puerto Rico", State))
state.count.Fed_removed = filter(state.count.Fed_removed, !grepl("Guam", State))
state.count.Fed_removed = filter(state.count.Fed_removed, !grepl("District of Columbia", State))

# add state code; apparently plotly likes to use state abbreviations; here we add them
# from the agriculture example

state.count.Fed_removed$code = df_agr$code 

# iris dataset for example
iris_data = iris

sub.mn <- iris_data %>%
  group_by(Species) %>%
  summarise(
    sum_petal_width = sum(Petal.Width),
    mean_petal_length = mean(Petal.Length))


# basic plot of exoneration numbers by state:
# issues: excludes Guam, Puerto Rico, and D.C.; doesn't have handy lables for small east 
# coast states; color bar is continuous, might work out better if binned (states with low 
# exoneration # appear white, and you can't make out state outline)

# ideas moving forward: better color bar + smal e. coast state labels; calculate number of years,
# months, weeks, days, hours, and seconds of injustice; will need to pull in and incorporate other
# datasets: we'll want correlations with total # of comparable convictions w/in each state (i.e.:
# we want to know per capita # of exonerations w/in a state; TX might have the most exonerations, 
# but is that because they convict more than Maine?); other data that would be nice is a total 
# population control, perhaps other innocence project data that shows ongoing investigations?;
# other ideas to control for population/conviction population? need to see if we can identify outliers
# (states with relatively high number of exonerations (or investigations) regardless of population)

plot_ly(state.count.Fed_removed, z = exoneration_sum, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = exoneration_sum, colors = 'Reds',
        marker = list(line = l), colorbar = list(title = "Total Exonerations"),
        filename="r-docs/usa-age-choropleth") %>%
  layout(title = 'Exonerations by State<br>(Hover for breakdown)', geo = g)

p = plot_ly(state.count.Fed_removed, z = exoneration_sum, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = exoneration_sum, colors = 'Reds',
        marker = list(line = l),
        filename="r-docs/usa-age-choropleth") %>%
  layout(title = 'Exonerations by State<br>(Hover for breakdown)', geo = g)


# save and publish

p <- plot_ly(midwest, x = percollege, color = state, type = "box")
plotly_POST(p, filename = "r-docs/exoneration_plot", world_readable=TRUE)

?plot_ly

dplyr::slice(iris, 10:15)




state.count.Fed_removed[is.na(state.count.Fed_removed)] <- 0 # remove "NA"s, maybe that'll fix things?; did not fix things


plot_ly(state.count.Fed_removed, z = age_sum, locations = State, type = 'choropleth',
        locationmode = 'USA-states', color = num_colors, colors = 'Blues',
        marker = list(line = l), colorbar = list(title = "Millions USD"),
        filename="r-docs/usa-choropleth") %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)


alaska_exonerations = filter(df, State == 'Alaska')

wisconsin_exonerations = filter(df, State == 'Wisconsin')

pennsylvania_exonerations = filter(df, State == 'Pennsylvania')

## import state population data; median 2002 ()

pop_data = read.csv("annual_pop_est_2000_2002.csv")

pop_data2 = dplyr::slice(pop_data, 4:54)

colnames(pop_data2)

pop_data2 = filter(pop_data2, !grepl("District of Columbia", table.with.row.headers.in.column.A.and.column.headers.in.row.3)) # remove DC


state.count.Fed_removed$pop = pop_data2$X # add 2002 population data





