# Libraries ---------------------------------------------------------------
Libraries <- as.list(c("tidyverse","tidytuesdayR",
                       "ggthemes","janitor",
                       "xml2","rvest","glue",
                       "reactable","reactablefmtr",
                       "fuzzyjoin","report"))

lapply(Libraries,require,character.only=TRUE)
# Import Dataset ----------------------------------------------------------
Data_Files <- tt_load('2021-07-27')

Olympics <- Data_Files[[1]]
Regions <- Data_Files[[2]]

rm(Data_Files)

# Unique Values -----------------------------------------------------------
sapply(Olympics,unique)
sapply(Regions,unique)
colnames(Olympics)
colnames(Regions)

# Country Level Results ---------------------------------------------------
Country_Results <- Olympics %>% 
  group_by(year,season,noc,team,games,event,sport,medal) %>% 
  summarise(result = n(),
            .groups = "drop") %>% 
  mutate(medal = ifelse(is.na(medal)==TRUE,
                        "Participant",medal),
         year = as.numeric(year)) %>% 
  filter(year >= 1990,
         season != "Winter") %>%
  group_by(year,noc,medal) %>% 
  summarise(number = n(),
            .groups = "drop") %>% 
  pivot_wider(names_from = medal,
              values_from = number,
              values_fill = 0) %>% 
  mutate(number_of_medals=Bronze+Silver+Gold)


Regions <- Regions %>% 
  clean_names()


# Clean Output ------------------------------------------------------------
Country_Results <- left_join(Country_Results,Regions) %>% 
  select(-notes)
# Find Flags --------------------------------------------------------------
Flags <- read_html("https://www.countries-ofthe-world.com/flags-of-the-world.html") %>%
  html_nodes(xpath = '//*/img') %>% 
  html_attrs()
# Flag data to data.frame -------------------------------------------------
extract_flags <- function(x){
  Flags <- list()
  for(i in seq_along(1:length(x))){
    Flags[[i]] <- data.frame(flag_link = x[[i]][["src"]]) %>%
      mutate(country = x[[i]][["alt"]])
  message(paste0(x[[i]][["alt"]]))}
Flags <- lapply(Flags,data.frame) %>% 
  bind_rows() %>% 
  mutate(file_link = paste0("https://www.countries-ofthe-world.com/",{flag_link}),
         flag_link = str_replace(flag_link,
                                 "^\\w{1,}[[:punct:]]\\w{1,}[[:punct:]]",
                                 ""))
return(Flags)}

Flags <- extract_flags(Flags)

# Download Flags ----------------------------------------------------------
dir.create("Flags")

obtain_flags <- function(x){
  number_of_flags <- 1:nrow(x)
  for(i in seq_along(number_of_flags)){
    download.file(url = x[i,3],
                  destfile = paste0("./Flags/",x[i,1]),
                  method = "libcurl")
  }
}

obtain_flags(Flags)
# Collect_Flags  ------------------------------------------------------------
Flags <- cbind.data.frame(Flags,
data.frame(flag_files = list.files("./Flags",
                                   full.names = TRUE)))
# Clean Flags ------------------------------------------------------------
Flags <- Flags %>% 
  mutate(country = str_replace(country,
                               "Flag of ",
                               ""))
# Add NOC -----------------------------------------------------------------
Flags <- cbind.data.frame(Flags,
data.frame(noc =countrycode::countrycode(Flags$country,
                                         origin = "country.name",
                                         destination = "ioc")))

Result_Table <- inner_join(Country_Results,Flags)

Result_Table <- Result_Table %>% 
  mutate(country = str_to_title(country)) %>% 
  rename(flag = file_link,
         medals = number_of_medals) %>% 
  clean_names() %>% 
  rename(participants= participant)
# Final Flags ---------------------------------------------------------
Result_Table <- Result_Table %>% 
  select(year,flag,country,noc,medals,participants)


Results_Summary <- reactable(Result_Table,
          columns = list(flag = colDef(cell = embed_img(
            height = 30, width =45)),
            medals = colDef(cell = icon_assign(Result_Table,
                                                         icon = "medal",
                                                         fill_color="#009f3d",
                                                         buckets = 1,
                                                         show_values = "right"),
                            sortable = TRUE),
            country = colDef(sortable = TRUE),
            participants = colDef(cell = data_bars(
              Result_Table, fill_color = "#0085c7",icon = "running",
              icon_color = "#FFFFFF",
              fill_opacity = 0.8,max_value = max(Result_Table$participants),
              min_value =  min(Result_Table$participants),
              align_bars = "left",text_position = "outside-end"
            ),
                                  sortable = TRUE)),
          theme = espn(
            font_family = "Arial Narrow",
            font_size = 12
          ),sortable = TRUE,
          resizable = TRUE) %>% 
  add_title(title = "Olympic Medals",
            font_family = "Arial Narrow",
            align = "center",
            font_weight = "bold") %>% 
  add_subtitle(subtitle = "1992 - 2016: summer olympic medal tally for participating countries.",
               align = 'center', font_family = "Arial Narrow",
               font_style = "italic", font_size = 16)
# Save output -------------------------------------------------------------
save_reactable(Results_Summary,
               "Olympics_Summary.html")
# Cite Packages -----------------------------------------------------------
cite_packages()
sessionInfo()

