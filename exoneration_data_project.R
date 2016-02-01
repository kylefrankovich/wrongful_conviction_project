library(plotly)
library(dplyr)

setwd("~/Google Drive/wrongful_conviction_project")

# set plotly account parameters:

Sys.setenv("plotly_username"="kyle.frankovich")
Sys.setenv("plotly_api_key"="r5kz3hc8a1")

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

# add state code; apparently plotly likes to use state abbreviations; use state.abb function:

state.count.Fed_removed$code = state.abb[match(state.count.Fed_removed$State,state.name)]


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

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

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

plotly_POST(p, filename = "r-docs/exoneration_plot", world_readable=TRUE)

dplyr::slice(iris, 10:15)


alaska_exonerations = filter(df, State == 'Alaska')

wisconsin_exonerations = filter(df, State == 'Wisconsin')

pennsylvania_exonerations = filter(df, State == 'Pennsylvania')

## import state population data; median 2002 ()

pop_data = read.csv("annual_pop_est_2000_2002.csv", stringsAsFactors = FALSE, header = FALSE)

pop_data2 = dplyr::slice(pop_data, 5:55) # select rows with actual state pop data

pop_data2 = filter(pop_data2, !grepl("District of Columbia", V1)) # remove DC

# as.numeric(gsub(",","", pop_data2$V2)) # this gets around the data containing commas which
# resulted in NAs being induced by coercion 

state.count.Fed_removed$pop = as.numeric(gsub(",","", pop_data2$V2)) # add 2002 population data

head(state.count.Fed_removed)

# lets take a look at basic correlation b/w exonerations and 2002 population

xvar = state.count.Fed_removed$exoneration_sum

yvar = state.count.Fed_removed$pop

# yvar = as.numeric(state.count.Fed_removed$pop) # this actually gives level codes, not numbers

ggplot(state.count.Fed_removed, aes(x=xvar, y=yvar)) +
  geom_point(shape=1)      # Use hollow circles

ggplot(state.count.Fed_removed, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

ggplot(state.count.Fed_removed, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region


ggplot(state.count.Fed_removed, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region
#> geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.


ggplot(state.count.Fed_removed, aes(x = xvar,y = yvar))+stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')

plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers")

p = plot_ly(data = state.count.Fed_removed, x = exoneration_sum, y = pop, text = State, mode = "markers")

plotly_POST(p, filename = "r-docs/exoneration_scatterplot", world_readable=TRUE)



p %>% add_trace(y = fitted(loess(as.numeric(pop) ~ exoneration_sum)))


ggplotly_example = ggplot(state.count.Fed_removed, aes(x=state.count.Fed_removed$exoneration_sum, y=state.count.Fed_removed$pop)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region


#### ggplotly example ####

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)



(gg <- ggplotly(ggplotly_example))

## breakdown by race:


# get exoneration count by race:

race.count <- df %>%
  group_by(Race) %>%
  summarise(
    exoneration_sum_race = sum(exoneration)
  )

sum(race.count$exoneration_sum_race)

race.count()

p_bar <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar",
  filename="r-docs/simple-bar"
)
p



