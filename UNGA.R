##select votes since 2000
after2000 <- completeVotes %>%
  filter(year >= 2000) %>%
  filter(vote != 9)

#correct Country and Countryname column of 2019
after2019 <- after2000 %>%
  filter(year >= 2019)
after2019<- after2019[,-6]
  colnames(after2019)[5] <- "Countryname"
after2019 <- after2019 %>%
  transform(Country= countrycode(ccode, "cown", "iso3c"))

##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

after2019 <- arrange.vars(after2019, c("Country"=5))

##combine the cleaned 2019 votes with rest of the votes
after2000_clean1 <- after2000 %>%
  filter(year < 2019) %>%
  rbind(after2019)

##correct ccode == 345 related votes
YUG <- subset(after2000_clean1, ccode == 345)
write.csv(YUG, file = "YUG.csv")
##I corrected the cases in Excel
corrected_YUG_all <- read_excel("corrected_YUG_all1.csv")

##All cleaned records
after2000_all <- after2000_clean1 %>%
  filter(ccode !=345) %>%
  rbind(corrected_YUG_all1)

##All cleaned RES records
after2000_all_res <- after2000_all
after2000_all_res$amend[is.na(after2000_all_res$amend)] <- 0
after2000_all_res$para[is.na(after2000_all_res$para)] <- 0

after2000_all_res <- after2000_all_res %>%
  filter(amend == 0) %>%
  filter(para == 0)

##count each vote type for each casted vote
count_vote_type <- after2000_all %>%
  select(resid, ccode, vote)
count_vote_type$ccode <- as.character(count_vote_type$ccode)

count_vote_type_result <- count_vote_type %>%
  group_by(resid) %>%
  count(vote)

count_vote_type_result_wide <- spread(count_vote_type_result, vote, n)
count_vote_type_result_wide[is.na(count_vote_type_result_wide)] <- 0
names(count_vote_type_result_wide) <- c("resid", "yes", "abstain", "no", "absent")

##calculate each type of vote's number's percent to total vote
count_vote <- count_vote_type_result_wide %>%
  transform(eff_vote= yes+no+abstain, tot_vote_membership= yes+no+abstain+absent) %>%
  transform(yes_percent = yes/eff_vote, abstain_percent= abstain/eff_vote, no_percent = no/eff_vote)

##use the new calculation to replace the orginal voting data
after2000_all_updated_totvote <- after2000_all[,-c(9:11)]
after2000_all_updated_totvote <- after2000_all_updated_totvote %>%
  left_join(count_vote, by = "resid")

after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("yes"=9))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("yes_percent"=10))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("no"=11))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("no_percent"=12))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("abstain"=13))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("abstain_percent"=14))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("eff_vote"=15))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("absent"=16))
after2000_all_updated_totvote <- arrange.vars(after2000_all_updated_totvote, c("tot_vote_membership"=17))

##create a frame for coutry code, iso and name reference
countries <- after2000_all_updated_totvote %>%
  select(ccode, year, Country, Countryname)
countries <- countries[!duplicated(countries),]

##select a frame for topic related calculation
vote_country <- after2000_all_updated_totvote  %>%
  select(resid, ccode, year, vote, me:ec)

##mark topic for each vote. Some votes are listed repeatedly since they have multiple topics
vote_country_gathered <- vote_country %>%
  gather(topic, has_topic, me:ec) %>%
  filter(has_topic != 0)

##find the votes that don't have any topic mark
check_na <- vote_country %>%
  transform(sum = me+nu+di+hr+co+ec) %>%
  filter(sum == 0)

na_for_adding <- check_na %>%
  select(resid, ccode, year, vote) %>%
  transform(topic = "na")

##All votes that listed with it's marked topic. Some votes are listed repeatedly since they have multiple topics 
vote_country_full <- rbind(vote_country_gathered, na_for_adding)

##US and China % of agreement calculation
USA <- vote_country_full %>%
  filter(ccode == 2) %>%
  select(resid, year, topic, vote)

CHN <- vote_country_full %>%
  filter(ccode == 710) %>%
  select(resid, year, topic, vote)

CHNUSA <- CHN %>%
  left_join(USA, by = c("resid", "topic", "year"), suf = c("_CHN", "_USA"))
CHNUSA <- CHNUSA %>%
  transform(same = vote_CHN - vote_USA)

count_CHNUSA <- CHNUSA %>%
  group_by(year, topic) %>%
  count(same) %>%
  filter(same == 0)

count_CHNUSA_topic <- CHNUSA %>%
  group_by(year) %>%
  count(topic)

CHNUSA_final <- count_CHNUSA_topic %>%
  left_join(count_CHNUSA, by = c("year", "topic"), suf = c("_tot", "_same")) %>%
  transform(agreement = n_same/n_tot)
CHNUSA_final <- CHNUSA_final[,-4]
CHNUSA_final[is.na(CHNUSA_final)] <- 0

##US and G7 % of agreement calculation
G7 <- vote_country_full %>%
  filter(ccode %in% c(2, 20, 220, 255, 325, 740, 200))

G7_cal <- G7 %>%
  select(resid, year, topic, ccode, vote) %>%
  spread(ccode, vote)

names(G7_cal) <- c("resid", "year", "topic", "USA", "CAN", "GBR", "FRA", "DEU", "ITA", "JPN")

G7_cal <- G7_cal %>%
  mutate(with_CAN = USA-CAN, with_GBR = USA-GBR, with_FRA = USA-FRA, with_DEU = USA-DEU, with_ITA = USA-ITA, with_JPN = USA-JPN)
##rest of the calculation is conducted in Excel....


##China and ASEAN % of agreement calculation
ASEAN <- vote_country_full %>%
  filter(ccode %in% c(710,835,811, 850, 812, 820, 775, 840, 830, 800, 816)) %>%
  left_join(countries, by = c("ccode", "year")) %>%
  select(resid, year, topic, vote, Country)

ASEAN_cal <- ASEAN %>%
  group_by(year, topic, Country) %>%
  count(vote)

ASEAN_ag <- ASEAN_cal %>%
  mutate(BRN_same = CHN-BRN, IDN_same=CHN-IDN, KHM_same= CHN-KHM, LAO_same=CHN-LAO, MMR_same=CHN-MMR,MYS_same=CHN-MYS, PHL_same=CHN-PHL, SGP_same= CHN-SGP, THA_same=CHN-THA, VNM_same=CHN-VNM)
##rest of the calculation is conducted in Excel....




##code below are not used 
##how many total votes does each country did for each year. Includes na topic cases
count_country_total_vote <- vote_country %>%
group_by(year) %>%
count(ccode)

na_cases <- check_na %>%
select(resid, year) %>%
transform(topic = "na")
na_cases <- na_cases[!duplicated(na_cases),]

##cases that with topic. Doesn't include na topic cases
count_topic_total_vote <- vote_country_gathered %>%
select(resid, year, topic)
count_topic_total_vote <- count_topic_total_vote[!duplicated(count_topic_total_vote),]

##All resid with topic. Some resids appear twice because have multiple topic.
topics <- rbind(count_topic_total_vote, na_cases)
topics_year_total <- topics %>%
  group_by(year) %>%
  count(topic)

##each country in each year vote number for each vote. Doesn't include na topic cases
count_country_topic_vote <- vote_country_gathered %>%
  group_by(year, ccode, topic) %>%
  count(vote)

count_country_topic_vote1<- count_country_topic_vote %>%
  left_join(countries, by = c("year", "ccode")) %>%
  left_join(count_country_total_vote, by = c("year", "ccode"), suf = c("_vote_topic", "_vote_total"))

me_vote <- filter(after2000_all_updated_totvote, me == 1)
nu_vote <- filter(after2000_all_updated_totvote, nu == 1)
di_vote <- filter(after2000_all_updated_totvote, di == 1)
co_vote <- filter(after2000_all_updated_totvote, co == 1)
hr_vote <- filter(after2000_all_updated_totvote, hr == 1)
ec_vote <- filter(after2000_all_updated_totvote, ec == 1)