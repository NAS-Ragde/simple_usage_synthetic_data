# Packages
# install.packages("leaflet")
# install.packages("corrplot")

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(corrplot)
library(ggplot2)
library(leaflet)

## Data load per location:
# Alabama
patients.alabama <- read_csv("test-sample/Alabama/patients.csv", col_types = cols(BIRTHDATE = col_date(format = "%m/%d/%Y")))
# head(patients.alabama)

# California
patients.california <- read_csv("test-sample/California/patients.csv", col_types = cols(BIRTHDATE = col_date(format = "%Y-%m-%d")))
# head(patients.california)

# Washington
patients.washington <- read_csv("test-sample/Pennsylvania/patients.csv", col_types = cols(BIRTHDATE = col_date(format = "%Y-%m-%d")))
# head(patients.washington)

# Pennsylvania 
patients.pennsylvania <- read_csv("test-sample/Washington/patients.csv", col_types = cols(BIRTHDATE = col_date(format = "%Y-%m-%d")))
# head(patients.pennsylvania)

# Gather location per city
location.alabama <- select(patients.alabama, LAT,LON)
location.california <- select(patients.california, LAT,LON)
location.pennsylvania <- select(patients.pennsylvania, LAT,LON)
location.washington <- select(patients.washington, LAT,LON)

# Hambung 
leaflet() %>%
  setView( 9.9937, 53.5511, zoom=11) %>%
  addTiles() %>%
  addMarkers(data = location.alabama, ~LON, ~LAT, clusterOptions = markerClusterOptions())

# Berlin
leaflet() %>%
  setView( 13.4050, 52.5200, zoom=11) %>%
  addTiles() %>%
  addMarkers(data = location.california, ~LON, ~LAT, clusterOptions = markerClusterOptions())

# Kahlsruhe
leaflet() %>%
  setView(8.4037, 49.0069, zoom=11) %>%
  addTiles() %>%
  addMarkers(data = location.pennsylvania, ~LON, ~LAT, clusterOptions = markerClusterOptions())

# Passau
leaflet() %>%
  setView(13.4319, 48.5664, zoom=12) %>%
  addTiles() %>%
  addMarkers(data = location.washington, ~LON, ~LAT,  clusterOptions = markerClusterOptions())


#--- Descriptive Data visualization

# Alabama
ggplot(data=patients.alabama, aes(x=lubridate::year(BIRTHDATE), fill=GENDER)) +
  geom_histogram(binwidth=6)

# California
ggplot(data=patients.california, aes(x=lubridate::year(BIRTHDATE), fill=GENDER)) +
  geom_histogram(binwidth=6)

# Washington
ggplot(data=patients.washington, aes(x=lubridate::year(BIRTHDATE), fill=GENDER)) +
  geom_histogram(binwidth=6)

# Pennsylvania 
ggplot(data=patients.pennsylvania, aes(x=lubridate::year(BIRTHDATE), fill=GENDER)) +
  geom_histogram(binwidth=6)

# --- Conditions

# Alabama
patients_condition.alabama <- read_csv("test-sample/Alabama/conditions.csv", 
                                       col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                                        STOP = col_date(format = "%Y-%m-%d")))
# California
patients_condition.california <- read_csv("test-sample/California/conditions.csv",
                                          col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                                           STOP = col_date(format = "%Y-%m-%d")))
# Pennsylvania
patients_condition.pennsylvania <- read_csv("test-sample/Pennsylvania/conditions.csv", 
                                            col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                                             STOP = col_date(format = "%Y-%m-%d")))
# Washingtom
patients_condition.washington <- read_csv("test-sample/Washington/conditions.csv", 
                                          col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                                           STOP = col_date(format = "%Y-%m-%d")))

# --- Diagnosis exploration 

# Alabama
conditions_frequency.alabama <- patients_condition.alabama %>%
  count(DESCRIPTION)  %>%
  arrange(desc(n))

ggplot(data=conditions_frequency.alabama, aes(x=reorder(DESCRIPTION, n), y =n )) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Conditions Alabama", x = "Conditions", y = "Count") +
  theme_minimal()

# California
conditions_frequency.california <- patients_condition.california %>%
  count(DESCRIPTION)  %>%
  arrange(desc(n))

ggplot(data=conditions_frequency.california, aes(x=reorder(DESCRIPTION, n), y =n )) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Conditions California", x = "Conditions", y = "Count") +
  theme_minimal()

# Pennsylvania
conditions_frequency.pennsylvania <- patients_condition.pennsylvania %>%
  count(DESCRIPTION)  %>%
  arrange(desc(n))

ggplot(data=conditions_frequency.pennsylvania, aes(x=reorder(DESCRIPTION, n), y =n )) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Conditions Pennsylvania", x = "Conditions", y = "Count") +
  theme_minimal()

# Washington
conditions_frequency.washington <- patients_condition.washington %>%
  count(DESCRIPTION)  %>%
  arrange(desc(n))

ggplot(data=conditions_frequency.washington, aes(x=reorder(DESCRIPTION, n), y =n )) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Conditions Washington", x = "Conditions", y = "Count") +
  theme_minimal()

# --- Top 10 Diagnostic
# Alabama
ggplot(data=patients_condition.alabama%>% 
         group_by(DESCRIPTION) %>% 
         summarize(FREQ=n()) %>%
         slice_max(FREQ, n=10), 
       aes(x=reorder(DESCRIPTION, -FREQ), y=FREQ)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# California
ggplot(data=patients_condition.california%>% 
         group_by(DESCRIPTION) %>% 
         summarize(FREQ=n()) %>%
         slice_max(FREQ, n=10), 
       aes(x=reorder(DESCRIPTION, -FREQ), y=FREQ)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Pennsylvania
ggplot(data=patients_condition.pennsylvania%>% 
         group_by(DESCRIPTION) %>% 
         summarize(FREQ=n()) %>%
         slice_max(FREQ, n=10), 
       aes(x=reorder(DESCRIPTION, -FREQ), y=FREQ)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Washington
ggplot(data=patients_condition.washington%>% 
         group_by(DESCRIPTION) %>% 
         summarize(FREQ=n()) %>%
         slice_max(FREQ, n=10), 
       aes(x=reorder(DESCRIPTION, -FREQ), y=FREQ)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))