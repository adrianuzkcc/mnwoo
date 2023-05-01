#counties <- c("Winona","Wabasha")
#townships <- c("Rock Dell","High Forest","Glasgow","Fountain","Sumner","Lewiston","Dakota", "Homer")

#' Title
#'
#' @param counties this is an analysis at county level
#' @param townships analysis at township level
#'
#' @return one dataframe
#' @export
#'
#' @examples x<-1
#'
pmnwoo <- function(counties, townships) {
  # if (!require("pacman")) install.packages("pacman"); library(pacman)
  # pacman::p_load("data.table", "Rmisc","ggplot2",
  #                "corrplot","ggpubr","dplyr","gtools","ggthemes",
  #                "tidyr","viridis",
  #                "rgdal","sp","maptools","spdplyr",
  #                "rmapshaper","rgdal","maptools",
  #                "data.table","remotes",
  #                "nngeo","modeest","dtplyr","sf",
  #                "ggmap","kableExtra","gridExtra", "grid", "flextable", "jpeg", "png", "RCurl")
  # library(sf)
  # library(tidyr)
  # library(ggplot2)
  # library(gridExtra)
  # library(grid)
  # library(flextable)
  # library(png)
  # library(RCurl)

  load("~/mnwoo/data/final_df.rda")
  final_df <- sf::st_transform(final_df, "+init=epsg:26915")
  townshipsinit <- townships
  if(length(townships)==0){
    townships <- unique(final_df$NAME)
  }
  if(length(counties)==0){
    counties <- unique(final_df$County)
  }

  final_df <- final_df %>%
    dplyr::filter(County %in% counties) %>%
    dplyr::filter(NAME  %in% townships)

  final_df2 <- as.data.frame(final_df) %>%
    select(County, NAME) %>%
    distinct(NAME, .keep_all = TRUE)

  if (length(townshipsinit)!=0){
    b <- aggregate(final_df2$NAME, list(final_df2$County), paste, collapse=", ")%>%
      mutate(text=paste0(Group.1," (",x,")"),
             a=1)
    b <- aggregate(b$text, list(b$a), paste, collapse="; ")
    b <- b$x[1]  #text in graphs
  }else{
    b <- paste(unique(final_df2$County),collapse = ', ')
  }

    #------------------#
    #**   Pop Total  **#  #Total population
    #------------------#

    Township_DF <-
      final_df %>%
      dplyr::mutate(pop_cat_new = cut_interval(ACS_total_pop, n = 5, dig.lab=5))

    pop_graph <-
      ggplot(Township_DF) +
      geom_sf(aes(fill = pop_cat_new), color = "white") +
      labs(title="  2. Population indicators \n \n",
           subtitle =paste(c("Total population: \n",b,"\n"), collapse=""))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_text(angle=90),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title = element_text(size=18,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey.\n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "YlGn",na.value="white")
    #pop_graph
    #------------------#
    #** # Total hh   **#  Number of households
    #------------------#

    Township_DF <-
      Township_DF %>%
      dplyr::mutate(HHTOTAL_new = cut_interval(ACS_HOU_total, n=5, dig.lab=5))

    hh_graph <-
      ggplot(Township_DF)  +
      geom_sf(aes(fill = HHTOTAL_new), color = "white")  +
      labs(title ="4. Households indicators \n\n", subtitle =paste(c("Total households: \n",b,"\n"), collapse = " "))+
      guides(fill=guide_legend(title="Total of houses"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=16,
                                           face="bold",
                                           #family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey.\n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "OrRd")
    #------------------#
    #** #Ownership   **#  #% of ownership
    #------------------#

    Township_DF <-
      Township_DF %>%
      dplyr::mutate(Ownership= (ACS_Ownership/ACS_HOU_total)*100) %>%
      dplyr::mutate(Ownership_new = cut_interval(Ownership, n=5, dig.lab=2))

    ownership_graph <- ggplot(Township_DF)  +
      geom_sf(aes(fill = Ownership_new), color = "white")  +
      labs(title ="", subtitle =paste(c("Ownership in:",b,"\n"), collapse = " "))+
      guides(fill=guide_legend(title="% of Ownership"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=16,
                                           face="bold",
                                           #family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey.\n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "OrRd")

    #--------------------------#
    #** # Median HH Income   **#  #Median household income in the past 12 months (in 2021 inflation-adjusted dollars)
    #--------------------------#

    Township_DF <-
      Township_DF %>%
      dplyr::mutate(inc_new = cut_interval(ACS_median_income,n=6, dig.lab=6))

    income_graph <-
      ggplot(Township_DF)  +
      geom_sf(aes(fill = inc_new), color = "white") +
      labs(title="5. Income indicators \n \n", subtitle=paste(c("Median income: \n",b,"\n"), collapse=""))+
      guides(fill=guide_legend(title="Income"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=16,
                                           face="bold",
                                           #family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "YlGn")
    #income_graph

    #-----------------------#
    #** # Child under 5   **#  #% of child under 5 years
    #-----------------------#

    Township_DF <-
      Township_DF %>%
      dplyr::mutate(child_5 = (ACS_POP_under5/ACS_total_pop)*100) %>%
      dplyr::mutate_at(vars(child_5), funs(round(., 2)))%>%
      dplyr::mutate(child_5= ifelse(is.na(child_5), 0,   child_5))%>%
      dplyr::mutate(child_5_new = cut_interval(child_5, n=5, dig.lab=5))

    child_5_graph <-
      ggplot(Township_DF)  +
      geom_sf(aes(fill = child_5_new), color = "white") +
      labs(title="",subtitle=paste(c("% child under 5: \n",b,"\n"), collapse=""))+
      guides(fill=guide_legend(title="% of Children< 5\n from total population"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=16,
                                           face="bold",
                                           #family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "PuBuGn")
    #------------------#
    #** # Over 65    **#  % of people under 65
    #------------------#


    Township_DF <-
      Township_DF %>%
      dplyr::mutate(over65 = ((ACS_POP_over65)/ACS_total_pop)*100) %>%
      dplyr::mutate_at(vars(over65), funs(round(., 2)))%>%
      dplyr::mutate(over65= ifelse(is.na(over65), 0,   over65))%>%
      dplyr::mutate(over65_new = cut_interval(over65, n = 5, dig.lab=5))

    over_65_graph <-
      ggplot(Township_DF %>% dplyr::filter(County %in% counties))  +
      geom_sf(aes(fill = over65_new), color = "white") +
      labs(title="",subtitle=paste(c("People over 65: ",b,"\n"), collapse=" "))+
      guides(fill=guide_legend(title="% of People>65 \n from total population"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=16,
                                           face="bold",
                                           #family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "Purples")

    #------------------#
    #** # White      **# #% of white people from total population
    #------------------#
    Township_DF <-
      Township_DF %>%
      dplyr::mutate(white = ((ACS_white_alone)/ACS_total_pop)*100) %>%
      dplyr::mutate_at(vars(white), funs(round(., 2)))%>%
      dplyr::mutate(white= ifelse(is.na(white), 0,   white))

    white_graph <-
      ggplot(Township_DF %>%
               dplyr::mutate(white_cat = cut_interval(white, n = 5, dig.lab=5))) +
      geom_sf(aes(fill = factor(white_cat)), color = "white") +
      labs(title="3. Race indicators \n\n", subtitle=paste(c("Proportion of white people:",b, "\n"), collapse=""))+
      guides(fill=guide_legend(title="% from total population"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=18,
                                           face="bold",
                                           #family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "Purples")

    #------------------#
    #** # Non-White  **# #% of non-white people from total population
    #------------------#
    Township_DF <-
      Township_DF %>%
      dplyr::mutate(nonwhite = ((ACS_total_pop-ACS_white_alone)/ACS_total_pop)*100) %>%
      dplyr::mutate_at(vars(nonwhite), funs(round(., 2)))%>%
      dplyr::mutate(nonwhite= ifelse(is.na(nonwhite), 0,   nonwhite)) %>%
      dplyr::mutate(nonwhite_new = cut_interval(nonwhite, n=5, dig.lab=6))

    non_white_graph <-
      ggplot(Township_DF)  +
      geom_sf(aes(fill = nonwhite_new), color = "white") +
      labs(title="",subtitle=paste(c("Proportion of non-white people:",b, "\n"),collapse=" "))+
      guides(fill=guide_legend(title="% from total population"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=14,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "Purples")

    #-------------------------#
    #** # Health Insurance  **# #% of child under 6 /people over 65 with health insurance coverage
    #-------------------------#

    Township_DF <-
      Township_DF %>%
      dplyr::mutate(child_6_PHIC = ((ACS_HEA_NHICfemaleu6+ ACS_HEA_NHICmaleu6)/(ACS_HEA_NHICfemaleu6 + ACS_HEA_HICfemaleu6+ ACS_HEA_HICmaleu6+ ACS_HEA_NHICmaleu6))*100) %>%
      dplyr::mutate_at(vars(child_6_PHIC), funs(round(., 2)))%>%
      dplyr::mutate(child_6_PHIC= ifelse(is.na(child_6_PHIC), 0,   child_6_PHIC))%>%
      dplyr::mutate(old_65_PHIC = ((ACS_POP_over65-ACS_HEA_HICo65-ACS_HEA_HICo652)/(ACS_POP_over65))*100) %>%
      dplyr::mutate_at(vars(old_65_PHIC), funs(round(., 2)))%>%
      dplyr::mutate(old_65_PHIC= ifelse(is.na(old_65_PHIC), 0,   old_65_PHIC))%>%
      dplyr::mutate(child_6_PHIC_new = cut_interval(child_6_PHIC, n=5, dig.lab=6))%>%
      dplyr::mutate(old_65_PHIC_new = cut_interval(old_65_PHIC, n=5, dig.lab=6))

    child_6_HIC_graph <-
      ggplot(Township_DF %>% dplyr::filter(County %in% counties))  +
      geom_sf(aes(fill = child_6_PHIC_new), color = "white") +
      labs(title="6. Health insurance coverage \n\n", subtitle=paste(c("Children under 6 with no Heath Insurance in:",counties,"County by Township \n"), collapse = " "))+
      guides(fill=guide_legend(title="% of Children<6 population"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=18,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "PuBuGn")

    old_65_HIC_graph <-
      ggplot(Township_DF %>% dplyr::filter(County %in% counties))  +
      geom_sf(aes(fill = old_65_PHIC_new), color = "white") +
      labs(title="",subtitle=paste(c("People over 65 with no Heath Insurance in:",counties, "County by Township \n"), collapse=" "))+
      guides(fill=guide_legend(title="% of People>65 \n from total >65 population"))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=18,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "PuBuGn")

    #------------------#
    #** # Nitrate    **#  #Nitrate levels
    #------------------#
    Township_DF <-
      Township_DF %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::mutate_at(vars(InitPrcGtr), funs(round(., 3)))%>%
      dplyr::mutate(InitPrcGtr= ifelse(is.na(InitPrcGtr), 0,   InitPrcGtr))%>%
      dplyr::mutate(nitrate_new = cut_interval(InitPrcGtr,n=7))

    nitrate_graph <-
      ggplot(Township_DF)  +
      geom_sf(aes(fill = nitrate_new), color = "white") +
      labs(title="  1. Nitrate levels \n \n",
           subtitle =paste(c("Nitrate level in:",counties, "County by Township \n"), collapse=" "))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=18,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "PuBuGn")

    #------------------#
    #** # Poverty    **# #% of people with income in the past 12 months bellow poverty level
    #------------------#
    Township_DF <-
      Township_DF %>%
      dplyr::mutate_at(vars(ACS_poverty), funs(round(., 3)))%>%
      dplyr::mutate(vars(ACS_poverty= ifelse(is.na(vars(ACS_poverty), 0,   InitPrcGtr))))%>%
      dplyr::mutate(poverty_new = cut_interval(ACS_poverty, n=5, dig.lab=6))

    poverty_graph <-
      ggplot(Township_DF %>% dplyr::filter(County %in% counties))  +
      geom_sf(aes(fill = poverty_new), color = "white") +
      labs(title="", subtitle=paste(c("Poverty level in:", counties, "County by Township \n"), collapse=" "))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=18,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "PuBuGn")

    #------------------#
    #** # Education  **#  #% of people with 1 or more years in Some college
    #------------------#
    Township_DF <-
      Township_DF %>%
      dplyr::mutate(educ_perc = ((ACS_edu_college_more1year)/(ACS_total_pop))*100) %>%
      dplyr::mutate_at(vars(educ_perc), funs(round(., 3)))%>%
      dplyr::mutate(vars(educ_perc= ifelse(is.na(vars(educ_perc), 0,   InitPrcGtr))))%>%
      dplyr::mutate(educ_new = cut_interval(educ_perc, n=5, dig.lab=6))

    educ_graph <-
      ggplot(Township_DF %>% dplyr::filter(County %in% counties))  +
      geom_sf(aes(fill = educ_new), color = "white") +
      labs(title="7. Education indicators\n \n",subtitle=paste(c("% of people with college>1 year in:", counties, "County by Township \n"),collapse=" "))+
      coord_sf(datum = NA) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            axis.text.x= element_blank(),
            axis.text.y = element_blank(),
            plot.caption = element_text(hjust = 0.5,size=8),
            plot.title       =element_text(size=14,
                                           face="bold",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2))+
      labs(x="", y="",
           caption = "Data source: American Community Survey. \n Website: https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs")+
      scale_fill_brewer(name="Incidence", palette = "PuBuGn")

    ########################################################################################
    # Tables
    ########################################################################################

    #----------------------------------#
    #** #    Child with insurance    **#
    #----------------------------------#
    Table_ACS_pop<- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,ACS_total_pop,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
             "Total Population" =ACS_total_pop)

    Table_ACS_pop_2<- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,ACS_total_pop,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::group_by(County, nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "Total Population" =ACS_total_pop)

    Table_Child <-  as.data.frame(Township_DF)
    Table_Child <- Table_Child %>%
      dplyr::select(#InitPrcGtr,
         County, NAME,child_5_new,child_5,nitrate_new) %>%
      dplyr::filter(County %in% counties)   %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Child < 5" =child_5)

    Table_Child_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,child_5_new,child_5,nitrate_new) %>%
      dplyr::filter(County %in% counties)   %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County,nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Child < 5" =child_5)


    Table_old <-  as.data.frame(Township_DF)
    Table_old <- Table_old %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,over65_new,over65,nitrate_new) %>%
      dplyr::filter(County %in% counties)   %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% People > 65" =over65)

    Table_old_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,over65_new,over65,nitrate_new) %>%
      dplyr::filter(County %in% counties)   %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County, nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% People > 65" =over65)


    Table_ChildHealth <- as.data.frame(Township_DF)
    Table_ChildHealth <- Table_ChildHealth %>%
      dplyr::select(#InitPrcGtr,
          County, NAME,child_6_PHIC_new,child_6_PHIC,nitrate_new) %>%
      dplyr::filter(County %in% counties)   %>%
      dplyr::filter(NAME  %in% townships)%>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Child< 6 HIC" = child_6_PHIC)
    Table_ChildHealth_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,child_6_PHIC_new,child_6_PHIC,nitrate_new) %>%
      dplyr::filter(County %in% counties)   %>%
      dplyr::filter(NAME  %in% townships)%>%
      dplyr::group_by(County, nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Child< 6 HIC" = child_6_PHIC)

    Table_Over65Health <- as.data.frame(Township_DF)
    Table_Over65Health <- Table_Over65Health %>%
      dplyr::select(#InitPrcGtr,
          County, NAME,old_65_PHIC_new,old_65_PHIC,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% People> 65 HIC" =old_65_PHIC)

    Table_Over65Health_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,old_65_PHIC_new,old_65_PHIC,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County, nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% People> 65 HIC" =old_65_PHIC)


    Table_HH <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
          County, NAME,ACS_HOU_total,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "Total Households" =ACS_HOU_total)

    Table_HH_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,ACS_HOU_total,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County, nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "Total Households" =ACS_HOU_total)

    Table_Ownership <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,Ownership, Ownership_new,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Ownership" =Ownership)

    Table_Ownership_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,Ownership, Ownership_new,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County,nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Ownership" =Ownership)

    Table_ACS_poverty <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
          County, NAME,ACS_poverty,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Poverty" =ACS_poverty)

    Table_ACS_poverty_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,ACS_poverty,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County,nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Poverty" =ACS_poverty)

    Table_ACS_income <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
          County, NAME,ACS_median_income,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "Median Income" =ACS_median_income)
    Table_ACS_income_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,ACS_median_income,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County,nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "Median Income" =ACS_median_income)

    Table_ACS_edu <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
          County, NAME,educ_new,educ_perc,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Education" =educ_perc)

    Table_ACS_edu_2 <- as.data.frame(Township_DF) %>%
      dplyr::select(#InitPrcGtr,
        County, NAME,educ_new,educ_perc,nitrate_new) %>%
      dplyr::filter(County %in% counties) %>%
      dplyr::filter(NAME  %in% townships) %>%
      dplyr::group_by(County,nitrate_new) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()%>%
      dplyr::rename("Nitrate Levels" =nitrate_new,
                    "% Education" =educ_perc)


    Table_1 <- dplyr::inner_join(Table_ACS_edu,Table_ACS_income,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_ACS_poverty,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_HH,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_ChildHealth,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_Over65Health,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_old,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_Child,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_ACS_pop,by="Nitrate Levels")
    Table_1 <- dplyr::inner_join(Table_1,Table_Ownership,by="Nitrate Levels")
    names(Table_1) <- c("Nitrate\nLevels", "%\nEducation", "Median\nIncome", "%\nPoverty", "Total\nHouseholds", "% Child\n< 6 HIC", "% People\n> 65 HIC", "% People\n> 65", "% Child\n< 5", "Total\nPopulation", "%\nOwnership")
    new_order = c("Nitrate\nLevels", "Total\nPopulation", "% Child\n< 5", "% People\n> 65","Total\nHouseholds", "%\nOwnership", "Median\nIncome", "%\nPoverty", "% Child\n< 6 HIC", "% People\n> 65 HIC", "%\nEducation")
    Table_1 <- Table_1[, new_order]

    Table_2 <- dplyr::inner_join(Table_ACS_edu_2,Table_ACS_income_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_ACS_poverty_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_HH_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_ChildHealth_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_Over65Health_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_old_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_Child_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_ACS_pop_2,by=c("Nitrate Levels", "County"))
    Table_2 <- dplyr::inner_join(Table_2,Table_Ownership_2,by=c("Nitrate Levels", "County"))

    names(Table_2) <- c("Nitrate\nLevels", "County","%\nEducation", "Median\nIncome", "%\nPoverty", "Total\nHouseholds", "% Child\n< 6 HIC", "% People\n> 65 HIC", "% People\n> 65", "% Child\n< 5", "Total\nPopulation", "%\nOwnership")
    new_order1 = c("County","Nitrate\nLevels", "Total\nPopulation", "% Child\n< 5", "% People\n> 65","Total\nHouseholds", "%\nOwnership", "Median\nIncome", "%\nPoverty", "% Child\n< 6 HIC", "% People\n> 65 HIC", "%\nEducation")
    Table_2 <- Table_2[, new_order1]

    Indicator <- c("Table summary by nitrate levels", "Table (%) race by township", "1. Nitrate Levels", "2. Population indicators", "3. Race indicators", "4. Households indicators", "5. Income indicators", "6. Health insurance coverage", "7. Education indicators", "Appendix")
    Page <-c("  03", "  04", "  05", "  06", "  12", "  14", "  18", "  22", "  26", "  28")
    Index <- data.frame(Indicator, Page)

    Race_DF <-
      Township_DF %>%
      dplyr::select(InitPrcGtr, ACS_white_alone,	ACS_black_alone,	ACS_native_alone,	ACS_asian_alone,	ACS_islander_alone,	ACS_other_alone,	ACS_twoOrmoreraces,	ACS_hispanic_or_latino,NAMELSAD,ACS_total_pop)%>%
      dplyr::mutate(across(ACS_white_alone:ACS_hispanic_or_latino, ~ (.x /ACS_total_pop)*100))%>%
      dplyr::mutate(across(ACS_white_alone:ACS_hispanic_or_latino, round, 1)) %>%
      dplyr::mutate(across(ACS_white_alone:ACS_hispanic_or_latino, ~replace_na(.,0)))

    Race_DF <-
      as.data.frame(Race_DF)%>%
      dplyr::select(InitPrcGtr,ACS_white_alone,	ACS_black_alone,	ACS_native_alone,	ACS_asian_alone,	ACS_islander_alone,	ACS_other_alone,	ACS_twoOrmoreraces,	ACS_hispanic_or_latino,NAMELSAD,ACS_total_pop) %>%
      dplyr::rename("%\nWhite" = ACS_white_alone,
                    "%\nBlack" = ACS_black_alone,
                    "%\nNative" = ACS_native_alone,
                    "%\nAsian" = ACS_asian_alone,
                    "%\nPacific" = ACS_islander_alone,
                    "%\nOther" = ACS_other_alone,
                    "%\nMultiple race" = ACS_twoOrmoreraces,
                    "%\nHispanic" = ACS_hispanic_or_latino,
                    "Mean\nPopulation" =ACS_total_pop,
                    "Township" = NAMELSAD,
                    "Nitrate\nLevels" = InitPrcGtr)%>%
      dplyr::arrange(Township)

    Race_DF$Township  <- gsub('Township', '', Race_DF$Township)

    Race_DF <- Race_DF %>%
      dplyr::group_by(Township) %>%
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()

    my_ttheme <- ttheme_default(
      core = list(bg_params = list(fill = "white", col = "gray"),
                  fg_params = list(col = "black"),
                  border = unit(c(0,1), "pt")),
      rowhead = list(bg_params = list(fill = "white", col = "gray"),
                     fg_params = list(col = "black"),
                     border = unit(c(0,1), "pt")),
      colhead = list(bg_params = list(fill = "white", col = "gray"),
                     fg_params = list(col = "black"),
                     border = unit(c(1,1), "pt"))
    )
    my_ttheme3 <- ttheme_default(
      core = list(bg_params = list(fill = "white", col = "white"),
                  fg_params = list(hjust = 0,
                                   x= 0.05,
                                   col = "black"),
                  border = unit(c(0,1), "pt")),
      rowhead = list(bg_params = list(fill = "white", col = "white"),
                     fg_params = list(col = "black"),
                     border = unit(c(0,1), "pt")),
      colhead = list(bg_params = list(fill = "white", col = "white"),
                     fg_params = list(col = "black"),
                     border = unit(c(1,1), "pt")),
      base_size = 16
    )
    my_ttheme2 <- ttheme_default(
      core = list(bg_params = list(fill = "white", col = "gray"),
                  fg_params = list(col = "black"),
                  border = unit(c(0,1), "pt")),
      rowhead = list(bg_params = list(fill = "white", col = "gray"),
                     fg_params = list(col = "black"),
                     border = unit(c(0,1), "pt")),
      colhead = list(bg_params = list(fill = "white", col = "gray"),
                     fg_params = list(col = "black"),
                     border = unit(c(1,1), "pt")),
      base_size = 10
    )
    url <- "https://mnwoo.org/wp-content/uploads/2018/06/logo.png"
    logo <-  readPNG(getURLContent(url))

    pdf(file= "sample.pdf", width = 10.5, height = 11 )
    grid.raster(logo, x = unit(8.1, "inches"), y = unit(9.8, "inches"), width = unit(2.8, "inches"), height = unit(1.8, "inches"))
    grid.text(paste(c("Descriptive analysis for: \n",counties,"counties \n"), collapse=" ") , x = unit(0.85, "npc"), y = unit(0.65, "npc"), hjust = 1, vjust = 1, gp = gpar(fontsize = 24, fontface = "bold", col = "black"))
    #Summary table
    grid::grid.newpage()
    grid.text("Index" , x = unit(0.54, "npc"), y = unit(0.73, "npc"), hjust = 1, vjust = 1, gp = gpar(fontsize = 24, fontface = "bold", col = "black"))
    gridExtra::grid.table(Index, theme= my_ttheme3,rows=NULL)
    grid::grid.newpage()
    gridExtra::grid.table(Table_1, theme= my_ttheme2,rows=NULL)
    #Race table
    grid::grid.newpage()
    gridExtra::grid.table(Race_DF, theme= my_ttheme2,rows=NULL)
    #nitrate levels
    print(nitrate_graph)
    #population
    print(pop_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_ACS_pop, theme= my_ttheme,rows=NULL)
    print(child_5_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_Child, theme= my_ttheme,rows=NULL)
    print(over_65_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_old, theme= my_ttheme,rows=NULL)
    #race
    print(white_graph)
    print(non_white_graph)
    #Households
    print(hh_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_HH, theme= my_ttheme,rows=NULL)
    print(ownership_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_Ownership, theme= my_ttheme,rows=NULL)
    #poverty
    print(income_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_ACS_income, theme= my_ttheme,rows=NULL)
    print(poverty_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_ACS_poverty, theme= my_ttheme,rows=NULL)
    #table health
    print(child_6_HIC_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_ChildHealth, theme= my_ttheme,rows=NULL)
    print(old_65_HIC_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_Over65Health, theme= my_ttheme,rows=NULL)
    #education
    print(educ_graph)
    grid::grid.newpage()
    gridExtra::grid.table(Table_ACS_edu,theme= my_ttheme,rows=NULL)
    grid::grid.newpage()
    grid.text("Appendix" , x = unit(0.54, "npc"), y = unit(0.9, "npc"), hjust = 1, vjust = 1, gp = gpar(fontsize = 24, fontface = "bold", col = "black"))
    grid.text(b , x = unit(0.54, "npc"), y = unit(0.84, "npc"), hjust = 1, vjust = 1, gp = gpar(fontsize = 14, col = "black"))
    gridExtra::grid.table(Table_2, theme= my_ttheme2,rows=NULL)
    dev.off()
}

#pmnwoo(c("Winona","Wabasha"), townships)
