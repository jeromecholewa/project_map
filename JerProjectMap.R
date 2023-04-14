library(leaflet)
library(dplyr)
library(readxl)
library(data.table)
library(purrr)
# library(htmltools)


# getwd()
# setwd("~/Documents/Education/Coursera/Datascience with R/Forgestik/project map/")

jer_proj_xl <- as.data.table( read_excel("20221106-Projects Jerome.xlsx", sheet = "Projets Jerome_2"), range = "a1:u37" )

jer_proj_xl <-  jer_proj_xl %>%
  mutate(category = case_when(!(grepl("Forgestik", jer_proj_xl[["Company"]])) ~ "Client",
                              TRUE ~ "Forgestik")) %>%
  arrange(desc(`Estimated Revenue`)) #sort by decreasing revenue

# create icons
projIcons <- iconList(Forgestik =
                        makeIcon("Forgestik.png",
                                 iconWidth = 26,
                                 iconHeight =32,
                                 iconAnchorX = 1,
                                 iconAnchorY = 32,
                                 popupAnchorX = 13,
                                 popupAnchorY = -16
                        ),
                      Client = makeIcon(
                        "MapMarker_Azure.png",
                        iconWidth = 24,
                        iconHeight =32,
                        iconAnchorX = 12,
                        iconAnchorY = 32,
                        popupAnchorX = 1,
                        popupAnchorY = -22)
)

# add factor for groups of revenues
jer_proj_xl <- jer_proj_xl %>%
  mutate(revenue.level = cut(`Estimated Revenue`,
                             c(0,30000,60000,90000, Inf),
                             labels = c('Proj. Rev. < 30k$',
                                        'Proj. Rev. 30 k$ < Rev < 60k$',
                                        'Proj. Rev. 60 k$ < Rev < 90k$',
                                        'Proj. Rev. > 90k$')))

jer_proj_Complete <- jer_proj_xl %>% filter(category == "Client") %>% filter(Status == "Complete")

# keep only active projects
jer_proj_client <- jer_proj_xl %>% filter(category == "Client") %>% filter(Status != "Complete")


jer_proj_Forgestik <- jer_proj_xl %>% filter(category == "Forgestik")


# create list of dataframe for each group of revenues
jer_proj_df <- split(jer_proj_client,
                     jer_proj_client$revenue.level)






# create dataframe of unique clients with aggregate
# project revenues for darkgreen circlemarkers labels
totalRevByComp <- jer_proj_client [, .(Total = sum(`Estimated Revenue`)), by = "Company"] %>%
  arrange(Company, .locale = "fr_CA")

jer_unique_clients<-  distinct(jer_proj_client, Company, .keep_all = TRUE) %>% arrange(Company, .locale = "fr_CA") %>% select(Company, Currency, Location, Lat, Long, Adresse, category)

jer_unique_clients[, "Total"] <- totalRevByComp$Total

# create dataframe of unique clients with aggregate
# COMPLETED project revenues for orange circlemarkers

totalRevByCompComplete <- jer_proj_Complete [, .(Total = sum(`Estimated Revenue`)), by = "Company"] %>%
  arrange(Company, .locale = "fr_CA")

jer_unique_clients_complete<-  distinct(jer_proj_Complete, Company, .keep_all = TRUE) %>% arrange(Company, .locale = "fr_CA") %>% select(Company, Currency, Location, Lat, Long, Adresse, category)

jer_unique_clients_complete[, "Total"] <- totalRevByCompComplete$Total

###########
###########
# add popup_html in jer_unique_clients by going over
# list of jer_proj_client company by company

jer_proj_company <- split(jer_proj_client,
                     jer_proj_client$Company)
# it's ordered by company

# add popup_html in jer_unique_clients by going over
# list of COMPLETED jer_proj_client company by company
jer_proj_company_Complete <- split(jer_proj_Complete,
                                   jer_proj_Complete$Company)
# it's ordered by company

# create a vector of the popup_html for active projects in each company, ordered by ascending company
popup_html <- vector(mode = "character")

names(jer_proj_company) %>%
  walk ( function (compa) {

    compa_popup <- paste("<h3 style='padding:0px; margin:0px'>",jer_proj_company[[compa]][1,"Company"],"</h3><a href=\"",
                         jer_proj_company[[compa]][1,"Location"],"\">",
                         jer_proj_company[[compa]][1,"Adresse"],"</a>")

    # then loop over the project line of that company
    # and add ,
    for (i in 1:jer_proj_company[[compa ]][,.N]) {
      compa_popup <- paste(compa_popup,
                           sprintf("<br><br><h4 style='padding:0px; margin:0px'>%s</h4>Budget d'heures: %s hrs<br>Taux horaire: %s %s<br>Rev. estimé: %s %s<br><a href=\"%s\">Autotask</a>",
                           jer_proj_company[[compa ]][i,"Project Name"],
                           jer_proj_company[[compa ]][i,"Estimated Hours"],                           jer_proj_company[[compa]][i,"Hourly Rate"],                           jer_proj_company[[compa]][i,"Currency"],
                           formatC(jer_proj_company[[compa ]][[i,"Estimated Revenue"]],
                                           big.mark=',',
                                           digits = 0,
                                           format = 'f'),
                           jer_proj_company[[compa ]][i,"Currency"],
                           jer_proj_company[[compa ]][i,"Autotask"])
      )
      }
    popup_html <<- c(popup_html,  compa_popup)
  }
  )

#######  CREATE a vector of popup_html for COMPLETED
#####   projects (one popup per company), ordered by
#####   ascending company)

popup_html_complete <- vector(mode = "character")


names(jer_proj_company_Complete) %>%
  walk ( function (compa) {

    compa_popup <- paste("(Terminé)<br><h3 style='padding:0px; margin:0px'>",jer_proj_company_Complete[[compa]][1,"Company"],"</h3><a href=\"",
                         jer_proj_company_Complete[[compa]][1,"Location"],"\">",
                         jer_proj_company_Complete[[compa]][1,"Adresse"],"</a>")

    # then loop over the project line of that company
    # and add ,
    for (i in 1:jer_proj_company_Complete[[compa ]][,.N]) {
      compa_popup <- paste(compa_popup,
                           sprintf("<br><br><h4 style='padding:0px; margin:0px'>%s</h4>Budget d'heures: %s hrs<br>Taux horaire: %s %s<br>Rev. estimé: %s %s<br><a href=\"%s\">Autotask</a>",
                                   jer_proj_company_Complete[[compa ]][i,"Project Name"],
                                   jer_proj_company_Complete[[compa ]][i,"Estimated Hours"],                                   jer_proj_company_Complete[[compa]][i,"Hourly Rate"],                                   jer_proj_company_Complete[[compa]][i,"Currency"],
                                   formatC(jer_proj_company_Complete[[compa ]][[i,"Estimated Revenue"]],
                                           big.mark=',',
                                           digits = 0,
                                           format = 'f'),
                                   jer_proj_company_Complete[[compa ]][i,"Currency"],
                                   jer_proj_company_Complete[[compa ]][i,"Autotask"])
      )
    }
    popup_html_complete <<- c(popup_html_complete,
                              compa_popup)
  }
  )


######

# CREATE LABELS FOR ALL CLIENTS REVENUES for active proj
# but only for darkgreen circle markers (revenues all clients)
labels_clients <- sprintf("<h3 style='padding:0px; margin:0px'>%s</h3>Rev. estimé: %s %s",
                          jer_unique_clients$Company,
                          formatC(jer_unique_clients$Total,
                                  big.mark=',',
                                  digits = 0,
                                  format = 'f'),
                          jer_unique_clients$Currency ) %>%
  lapply(htmltools::HTML)


# CREATE LABELS FOR COMPLETE PROJECTS (orange circles)
# works even with several completed projects
labels_complete <- sprintf( "(Terminé)<br><h3 style='padding:0px; margin:0px'>%s</h3>Rev. estimé: %s %s",
                            jer_unique_clients_complete$Company,

                            # jer_unique_clients_complete$`Project Name`,
                            formatC(jer_unique_clients_complete$Total,
                                    big.mark=',',
                                    digits = 0,
                                    format = 'f'),
                            jer_unique_clients_complete$Currency
)  %>%
  lapply(htmltools::HTML)


# DEFINE the group names
gp_forgestik <-  sprintf("Forgestik (%s)", length(unlist(jer_proj_Forgestik[,1])))

gp_clients <- sprintf("Projets Clients Actifs (%s)", length(unlist(jer_proj_client[,1])))

gp_complete <- sprintf("Terminés (%s)", length(unlist(jer_proj_Complete[,1])))

gp_rev_tous_clients <- sprintf("Revenus tous clients (%s)", length(unlist(jer_unique_clients[,1])))

# group for revenue categories
gp_rev.cat <- do.call(sprintf, c(jer_proj_client[,.N, by = revenue.level], '%s (%s)'))


# CREATE MAP
m <- leaflet() %>% # leaflet works with the pipe operator
  addTiles() %>% # setup the default OpenStreetMap map tiles

  #add Forgestik markers
  addMarkers(data = jer_proj_Forgestik,
             ~Long, ~Lat,
             label = ~Company,
             icon = ~projIcons[category],
             group = gp_forgestik,
             popup = ~paste(paste("<h3 style='padding:0px; margin:0px'>",Company,"</h3>"),
                            paste("<a href=\"",
                                  Location,"\">",
                                  Adresse,"</a>")
                            )
             ) %>%

  ### Markers des projets actifs
  # as many as unique clients (e.g. 21) but not projects (e.g.23)
  #
  addMarkers(data = jer_unique_clients,
             ~Long, ~Lat,
             label = ~Company,
             icon = ~projIcons[category],
             group = gp_clients,
             popup = popup_html
  ) %>%

  # add markers for completed projects
  addMarkers(data = jer_unique_clients_complete,
             ~Long, ~Lat,
             label = ~Company,
             icon = ~projIcons[category],
             group = gp_complete,
             popup = popup_html_complete     ,
  ) %>%

  addMiniMap(width = 100, height = 100, position = "bottomleft")   %>%
  addTerminator()

m <- m %>%

  #add circleMarkers for all revenues of active projects
  addCircleMarkers(
    data = jer_unique_clients,
    ~Long, ~Lat,
    ~(sqrt(Total)/5),
    weight = 1,
    group =gp_rev_tous_clients,
    color = "darkgreen",
    fillOpacity = 0.3,
    label = labels_clients
  ) %>%

  addCircleMarkers(   # for completed projects
    data = jer_unique_clients_complete,
    ~Long, ~Lat,
    ~(sqrt(Total)/5),
    weight = 1,
    color = "orange",
    group = gp_complete ,
    fillOpacity = 0.5,
    label = labels_complete
  )

# AT THIS POINT WE HAVE A MAP WITH markers +
# CIRCLEMARKERS FOR ALL CLIENTS (ACTIVE AND COMPLETED)

# CREATE A LIST OF LIST OF LABELS FOR EACH REVENUE CATEGORY
lab.rev.cat <- list()  # create empty list

names(jer_proj_df) %>%
  walk ( function (rev.cat) {
    lab.rev.cat[[rev.cat]] <<- c(lab.rev.cat[[rev.cat]],
                                sprintf("<h3 style='padding:0px; margin:0px'>%s</h3><h4 style='padding:0px; margin:0px'>%s</h4>Rev. estimé: %s %s",
                                        jer_proj_client %>%
                                          filter(revenue.level==rev.cat)  %>% select (Company) %>% unlist,
                                        jer_proj_client %>%
                                          filter(revenue.level==rev.cat)  %>% select (`Project Name`) %>% unlist,
                                        formatC(jer_proj_client %>%
                                                  filter(revenue.level==rev.cat)  %>% select (`Estimated Revenue`) %>% unlist,
                                                big.mark=',',
                                                digits = 0,
                                                format = 'f'),
                                        jer_proj_client %>%
                                          filter(revenue.level==rev.cat)  %>%
                                          select (Currency) %>% unlist
                ) %>%
                  lapply(htmltools::HTML) )

    }
    )

#add circleMarkers and markers by group of revenues
rev(names(jer_proj_df)) %>% walk ( function (rev.cat) {
  m <<- m %>%
    addCircleMarkers(data = jer_proj_df[[rev.cat]][category == "Client",],
                     ~Long, ~Lat,
                     ~(sqrt(`Estimated Revenue`)/5),
                     #stroke = F,
                     weight = 1,
                     group=paste0(rev.cat," (",
                                  jer_proj_df[[rev.cat]][,.N],
                                    ")"),
                     fillOpacity = 0.15,
                     label = lab.rev.cat[[rev.cat]] #,
                     )

} )

####################
  # Layers control

m <- m %>%
  addLayersControl(
    overlayGroups = c(gp_forgestik, gp_clients, gp_rev_tous_clients , gp_rev.cat, gp_complete),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c( gp_complete, gp_rev.cat))


m

