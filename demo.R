#### Demo script on computing CBSA segregation measures ------------------------

#dependencies
library(tidyverse)
library(haven)
library(seg)
library(OasisR)

#load data
ncdb <- read_dta("H:/seg-demo/ncdb.dta")

#for ease of reference later
cbsa_names <- read_csv("H:/seg-demo/cbsa2fipsxw.csv") %>%
  mutate(CBSA = cbsacode,
         CBSA = ifelse(CBSA == 31080, 31100, CBSA), #Los Angeles CBSA
         CBSA = ifelse(CBSA == 46520, 26180, CBSA), #Honolulu CBSA
         CBSA = as.character(CBSA)) %>%
  select(starts_with("cbsa")) %>%
  distinct()

#### Compute measures of segregation -------------------------------------------

#Non-Hispanic Black/Non-Hispanic White Dissimilarity index
aspatial <- ncdb %>%
  filter(CBSA != "") %>%
  group_by(CBSA) %>%
  summarize(tpop2010 = sum(TRCTPOP1),
            dis2010 = (.5) * sum(abs(SHRNHB1N/sum(SHRNHB1N) - SHRNHW1N/sum(SHRNHW1N))))

#NH Black/NH White RCO and SP indices
spatial <- ncdb %>%
  filter(CBSA != "") %>%
  group_by(CBSA) %>%
  summarize(rco2010 = RCO(cbind(SHRNHB1N, SHRNHW1N), AREALAND)[1,2],
            sp2010 = isp(cbind(INTPTLON, INTPTLAT),
                         cbind(SHRNHB1N, SHRNHW1N),
                         nb = spDists(cbind(INTPTLON, INTPTLAT), longlat = T)))

#join up the tables of segregation measures
seg <- inner_join(aspatial, spatial)

#join names of CBSA on now
seg <- inner_join(seg, cbsa_names)

#### Look at the results -------------------------------------------------------

seg %>%
  select(starts_with("cbsa"), everything()) %>%
  top_n(100, tpop2010) %>%
  arrange(desc(dis2010))
