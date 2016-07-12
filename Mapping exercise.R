

#########################
library(rjson)
library(blsAPI)
  
df <- blsAPI('SMU47000007072111001', 2, TRUE)

response <- blsAPI('SMU47000007000000001') 
json <- fromJSON(response)


####################################################################################################
####################################################################################################

##    leaflet examples

####################################################################################################
####################################################################################################

# Documentation : 
  # http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way
library(acs)
library(dplyr)
library(tigris)
library(leaflet)

api.key.install(key="")

# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("TN"), county="*", tract = "*")

counties <- counties(state = "47")
tracts <- tracts(state = 'TN', cb=TRUE)
# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

income<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)

income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

# The package tigris has a nice little merge function to do the sometimes difficult merge between the spatial and tabular data.

income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
income_merged <- income_merged[income_merged$ALAND>0,]

##  Make Leaflet

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$percent
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = income_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = income_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 
map3


####################################################################################################
####################################################################################################

##    ChoroplethR examples

####################################################################################################
####################################################################################################
library(choroplethr)
library(choroplethrMaps)
# ?df_pop_county
# ?county_choropleth_acs

# State view of median income
county_choropleth_acs("B19301", num_colors = 1, state_zoom = "tennessee")

# Regional view of Population
county_choropleth_acs(tableId="B01003", endyear = 2011, span = 5, num_colors = 1, 
                      state_zoom = c("tennessee", "georgia", "alabama", "mississippi"))



data(df_pop_county)
# df looks like this:
#   region  value
# 1   1001  54590
# 2   1003 183226
# 3   1005  27469
# 4   1007  22769
# 5   1009  57466
# 6   1011  10779

?county_choropleth
county_choropleth(df_pop_county,
                  title = "US Cou")

