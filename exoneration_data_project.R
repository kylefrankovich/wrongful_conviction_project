library(plotly)
library(dplyr)

df2 = read.csv("NRE_data.csv") # load in National Register of Exonerations data

unique(df2$State)

# get state count:

state.count <- df2 %>%
  group_by(State) %>%
  summarise(
    exoneration_sum = sum(exoneration)
    )

iris_data = iris


sub.mn <- iris_data %>%
  group_by(Species) %>%
  summarise(
    sum_petal_width = sum(Petal.Width),
    mean_petal_length = mean(Petal.Length))

iris_data2 = iris 

num_colors = 5

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


state.count.Fed_removed = filter(state.count, !grepl("Fed", State))
state.count.Fed_removed = filter(state.count.Fed_removed, !grepl("Puerto Rico", State))
state.count.Fed_removed = filter(state.count.Fed_removed, !grepl("Guam", State))
state.count.Fed_removed = filter(state.count.Fed_removed, !grepl("District of Columbia", State))

# add state code?

state.count.Fed_removed$code = df$code # this helped


state.count.Fed_removed[is.na(state.count.Fed_removed)] <- 0 # remove "NA"s, maybe that'll fix things?; did not fix things

# add a ones colums, add them to get # of exonerations


df2$exoneration = 1

plot_ly(state.count.Fed_removed, z = age_sum, locations = State, type = 'choropleth',
        locationmode = 'USA-states', color = num_colors, colors = 'Blues',
        marker = list(line = l), colorbar = list(title = "Millions USD"),
        filename="r-docs/usa-choropleth") %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)


alaska_exonerations = filter(df2, State == 'Alaska')

wisconsin_exonerations = filter(df2, State == 'Wisconsin')

pennsylvania_exonerations = filter(df2, State == 'Pennsylvania')


