# DATA VISUALIZATION Of Sales Data #
##  Library Used ##

library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(plotrix)

#-------------------------------

## Loading in Data
Store_data <- read_csv("SalesData.csv")



### Final Dataset creation ###

Store_data_mod <-  Store_data %>%
                    pivot_longer(-(1:8),
                                 names_to = c("measure", "year"),
                                 names_sep = -4) %>%
                    group_by(measure) %>%
                    mutate(row_id = row_number()) %>%
                    pivot_wider(names_from = measure, values_from = value) %>%
                    select(-row_id)

Store_data_mod$Qtr <- NA # Empty Col Creation
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Jan", "Feb", "Mar")] <- "Q1"
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Apr", "May", "Jun")] <-  "Q2"
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Jul", "Aug", "Sep")] <-  "Q3"
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Oct", "Nov", "Dec")] <-  "Q4"

#---------------------------------------------------------


### Ques:1 	Compare Sales by region for 2016 with 2015 using bar chart	


 Ans1 <- Store_data_mod %>%
                group_by(Region, year) %>%
                summarise(total = sum(Sales)) %>% ggplot(aes(x = Region, y = total, fill = year)) +
                geom_col(position = position_dodge(), color = "black") +
                scale_fill_manual(values = c("tomato", "steelblue")) +
                theme_bw() +
                labs( title = "Sales By Region",subtitle = "2015-2016",y = "Total Sales",fill = "Year") +
                scale_y_continuous(labels = scales::label_comma() , breaks = scales::breaks_pretty()) +
                geom_text(
                  aes(label = paste0(format(round(total/1e6,2)),"M")),
                  position = position_dodge(0.9),
                  color = "black",
                  vjust = -1,
                  hjust = 0.5,
                  fontface = "plain")+
                theme(plot.title = element_text(size = 15,face="bold",hjust = 0.5),
                      plot.subtitle = element_text(size=10,hjust = 0.5,face="bold"))

Ans1

### Ques:2 Pie charts for sales for each region in 2016.

## pie charts region wise

region_wise <- Store_data_mod %>%
              filter(year == 2016) %>%
              group_by(Region) %>%
              summarise(total = sum(Sales)) %>% 
              arrange(desc(total)) %>% 
              mutate(perc = total/sum(total),labels = scales::percent(perc))

stack_bar <- region_wise %>%
            ggplot(aes(x = "", y = total, fill = fct_inorder( Region))) +
            geom_bar(stat = "identity",position = position_fill()) +
             geom_text(aes(x = 1.6,label = paste0(Region," ",labels)),
                         position = position_fill(vjust = 0.5))

stack_to_pie <- stack_bar + coord_polar(theta = "y", start = 0)


stack_to_pie <- stack_to_pie + theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            legend.position =  "none",
            panel.background = element_blank(),
            plot.title = element_text(size =20,hjust = 0.5,face = "bold")) +
            labs(x = "",y = "",title = " Region Wise Sales in 2016",fill="Region")
  

stack_to_pie

#---------------
## 3D pie 

three_dim_pie <- plotrix::pie3D(region_wise$total,
                  labels = paste0(region_wise$Region,":",region_wise$labels),
                  theta = 0.8,
                  border = "white",
                  shade = 1,
                  col = c("tomato","steelblue","lightgreen"),
                  explode = 0.1,
                  main="Region Wise Sales in 2016",
                  radius = 0.9)

three_dim_pie
#----------------------------------------------------------------------

### Ques:3 	Compare sales of 2015 and 2016 with Region and Tiers.

    Ans3 <-  Store_data_mod %>%
                group_by(Region, Tier, year) %>%
                summarise(total = sum(Sales)) %>%
                ggplot(aes(x = Tier, y = total, fill = year)) +
                geom_col(position = position_dodge(),color="Black") + facet_wrap( ~ Region) +
                scale_fill_manual(values = c("tomato","steelblue")) +
                theme_bw() +
                labs(title = "Sales By Region and Tier",subtitle = " 2015 ",y = "Total Sales",fill = "Year") +
                scale_y_continuous(labels = scales::label_comma() ,breaks = scales::breaks_pretty())+
                theme(plot.title = element_text(size = 15,face="bold",hjust = 0.5),
                      plot.subtitle = element_text(size=10,hjust = 0.5,face="bold"))
                
Ans3               
  
  #--------------------------------------------------------------------------------

### Ques 4 :	In East region, which state registered a decline in 2016 as compared to 2015?
Ans4 <- Store_data_mod %>%
          filter(Region == "East") %>%
          group_by(State, year) %>%
          summarise(total = sum(Sales)) %>%
          ggplot(aes(x = State, y = total, fill = year)) + 
          geom_col(position = position_dodge(),color="black") +
          labs(fill = "Year")+
          scale_fill_manual(values = c("tomato","steelblue")) +
          theme_bw() +
          labs(title = "East Region Sales",
               subtitle = "2015-2016",y = "Total Sales",fill = "Year",
               caption = "In East region , NY registered  decline in sales in 2016 as compared to 2015. ") +
          scale_y_continuous(labels = scales::label_comma() ,breaks = scales::breaks_pretty())+
          theme(plot.title = element_text(size = 15,face="bold",hjust = 0.5),
                plot.subtitle = element_text(size=10,hjust = 0.5),
                plot.caption = element_text(size = 15,face = "bold",hjust = 0))

Ans4                    
## NY is the only State having lower sales in 2016 as compared to 2015.

#-------------------------------------------

### Ques 5	In all the High tier,
##           which Division saw a decline in number of units sold in 2016 compared to 2015?

Ans5 <- Store_data_mod %>%
        filter(Tier == "High") %>%
        group_by(Division, year) %>%
        summarise(total = sum(Sales)) %>%
        ggplot(aes(x = Division, y = total, fill = year)) +
        geom_col(position = position_dodge(), color = "black",width = 0.8) +
        scale_fill_manual(values = c("tomato","steelblue")) +
        theme_bw() +
        labs(title = "No Of Units Sold By Division ",
        subtitle = "2015-2016",y = "No Of Units",fill = "Year",
        caption = "None of the Division registered less sales in 2016 as compared to 2015.") +
        scale_y_continuous(labels = scales::label_comma() ,breaks = scales::breaks_pretty())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 15,face="bold",hjust = 0.5),
              plot.subtitle = element_text(size=10,hjust = 0.5),
              plot.caption = element_text(size = 15,face = "bold",hjust = 0)) 

Ans5


  #----------------------------------------------------------------------#
# 6.	Create a new column Qtr -

# •	Jan - Mar : Q1
# •	Apr - Jun : Q2
# •	Jul - Sep : Q3
# •	Oct - Dec : Q4

Store_data_mod$Qtr <- NA # Empty Col Creation
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Jan", "Feb", "Mar")] <- "Q1"
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Apr", "May", "Jun")] <- "Q2"
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Jul", "Aug", "Sep")] <- "Q3"
Store_data_mod$Qtr[Store_data_mod$Month %in% c("Oct", "Nov", "Dec")] <- "Q4"



#---------------------------------------------------------------------------------------------

## Ques:7 	Compare Qtr wise sales in 2015 and 2016 in a bar plot


Ans7 <- Store_data_mod %>% group_by(year, Qtr) %>%
    summarise(Total = sum(Sales)) %>%
    ggplot(aes(x = Qtr, y = Total, fill = year)) +
    geom_col(position = "dodge", color = "black") +
    scale_fill_manual(values = c("tomato","steelblue")) +
    theme_bw() +
    labs(title = "Quarter Wise Sales",subtitle = "2015-2016",y = " Total Sales",fill = "Year") +
    scale_y_continuous(labels = scales::label_comma() ,breaks = scales::breaks_pretty())+
    geom_text(aes(label=paste0(format(round(Total/1e6,2)),"M")),position = position_dodge(0.9),
              color="black",vjust = -1,hjust = 0.5,fontface="plain")+
    theme(plot.title = element_text(size = 15,face="bold",hjust = 0.5),
          plot.subtitle = element_text(size=10,hjust = 0.5,face="bold"))
    
Ans7

###--------------------------------------------------------------------

## Ques:8	Determine the composition of Qtr wise sales in 2015 , with regards to all the Tiers in a pie chart

pie_data <- Store_data_mod %>% 
                filter(year == 2015) %>%
                group_by(Qtr, Tier) %>%
                summarise(Total_Sales = sum(Sales)) %>% 
                mutate(per = (Total_Sales /sum(Total_Sales)),
                       labels =  scales::percent(per,accuracy = 0.1))

par(mar=c(1,1,1,1))
par(mfrow=c(2,2))



Qtr1 <- pie(pie_data$Total_Sales[pie_data$Qtr=="Q1"],
          radius = 0.7,col = c("lightgreen", "steelblue", "tomato","blue"),
          labels = paste0(pie_data$Tier,":",pie_data$labels[pie_data$Qtr=="Q1"]),
          main = "Q1" )


Qtr2 <- pie(pie_data$Total_Sales[pie_data$Qtr=="Q2"],
        radius = 0.7,col = c("lightgreen", "steelblue", "tomato","blue"),
        labels = paste0(pie_data$Tier,":",pie_data$labels[pie_data$Qtr=="Q2"]),
        main = "Q2" )

Qtr3 <- pie(pie_data$Total_Sales[pie_data$Qtr=="Q3"],
        radius = 0.7,col = c("lightgreen", "steelblue", "tomato","blue"),
        labels = paste0(pie_data$Tier,":",pie_data$labels[pie_data$Qtr=="Q3"]),
        main = "Q3" )

Qtr4 <- pie(pie_data$Total_Sales[pie_data$Qtr=="Q4"],
          radius = 0.7,col = c("lightgreen", "steelblue", "tomato","blue"),
          labels = paste0(pie_data$Tier,":",pie_data$labels[pie_data$Qtr=="Q4"]),
          main = "Q4" )




