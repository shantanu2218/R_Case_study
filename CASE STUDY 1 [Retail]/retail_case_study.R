                  # Retail case study # 

library(tidyverse)
library(lubridate)
library(cowplot)
# Ques1: Merge all the dataset,keeping all the customers who have done transaction.
### 

## Step 1: Loading the data  ##
customer <- read_csv("pro data/Customer.csv")
product_hierarchy <- read_csv("pro data/prod_cat_info.csv")
transaction <- read_csv("pro data/Transactions.csv")

#----------------------------------------------------------
 ### Part A ::Merging use base merge() function #####

colSums()
customer %>% count(customer_Id) %>% filter(n>1) # customer Id will act as primary key 

# Joining  customer and transaction table #

# Primary key : customer Id in Customer table 
# Foreign key : customer Id in Transactions table 

customer_trans <- merge(x = transaction,y = customer,
                    by.x = "cust_id",by.y = "customer_Id",all.x = T)

# Left join is used to include customer  transaction information.

customer_final <-  merge(x = customer_trans,y = product_hierarchy,
                    by.x = c("prod_cat_code","prod_subcat_code"),
                    by.y = c("prod_cat_code","prod_sub_cat_code"),all.x = T) 

customer_final <- customer_final %>%           
                  select(cust_id,transaction_id,everything(),-prod_cat_code,-prod_subcat_code) 

view(customer_final)   ## Final merged data                 
#-------------------------------------

 ### Part B :: Merging use dplyr merge function #####

customer_final_dplyr <- left_join(x = transaction,y = customer,
                                  by = c("cust_id"="customer_Id")) %>%
                         left_join( product_hierarchy, 
                                  by = c("prod_cat_code" = "prod_cat_code",
                                  "prod_subcat_code" = "prod_sub_cat_code"))

customer_final_dplyr <- customer_final_dplyr %>%
                        select(everything(),-prod_subcat_code,-prod_cat_code)

view(customer_final_dplyr) ## Final merged data

#-----------------------------------------------------------------------------------------------

# Ques 2: Prepare a summary report for the merged data

# a: Column names and corresponding data types

data.frame(DataType = sapply(customer_final_dplyr,FUN = class)) 

# b:Top/bottom 10 observations #

#  Top 10
top_10 <- customer_final_dplyr %>% head(10)
view(top_10)
 
# Bottom 10 
bottom_10 <- customer_final_dplyr %>% tail(10) 
view(bottom_10)


#--------------------------------------------------------------
## Data manipulation  ##
customer_final_dplyr$transaction_id <- as.character(customer_final_dplyr$transaction_id)
customer_final_dplyr$cust_id <- as.character(customer_final_dplyr$cust_id) 
customer_final_dplyr$city_code <- as.character(customer_final_dplyr$city_code) 

# Dividing data into Numerical and categorical

# Numeric
numeric_data <- sapply(X=customer_final_dplyr,FUN = is.numeric)
continuous_data <- customer_final_dplyr[,numeric_data]

# Categorical
character_data <- sapply(X=customer_final_dplyr,FUN = is.character)
freq_data <- customer_final_dplyr[,character_data]

# Handling Dates
customer_final_dplyr$tran_date <- lubridate::dmy(customer_final_dplyr$tran_date)
customer_final_dplyr$DOB <- lubridate::dmy(customer_final_dplyr$DOB)

#------------------------------------------------------------------------

# c: Five number summary

(five_num_summary <- summary(continuous_data))

# d: Frequency table for  categorical variable

list <- lapply(X=freq_data,FUN = plyr::count )

freq_table_gender <- list[["Gender"]] %>% rename("Gender"="x") 
freq_table_gender

freq_table_prodCat <- list[["prod_cat"]] %>% rename("prod_cat"="x")
freq_table_prodCat

freq_table_prodSubcat <- list[["prod_subcat"]] %>% rename("prod_subcat"="x")
freq_table_prodSubcat

freq_table_storeType <- list[["Store_type"]] %>% rename("Store_type"="x")
freq_table_storeType

freq_table_cityCode <- list[["city_code"]] %>% rename("city_code"="x")
freq_table_cityCode

freq_table_CustID <-  list[["cust_id"]] %>% rename("CustId"="x") %>% arrange(desc(freq))
freq_table_CustID %>% head(10) # Showing only top 10

# NOTE: No frequency table for TransId, as every TransId is unique.

#-------------------------------------------------------------------

##Ques3 

#  A:Histograms for all continuous vars.

cont_vars <- function(cont){
                              ggplot() +
                              geom_histogram(aes_string( x=cont),
                                             bins = 5,color="red",fill="lightblue") +
                              theme_bw()
                            }


cont_data_list <- lapply(continuous_data,FUN = cont_vars )

plot_Qty <- cont_data_list[["Qty"]] + labs(x="Quantity",title = "Qty Histogram")
plot_Rate <- cont_data_list[["Rate"]]+labs(x="Rate",title = "Rate Histogram")
plot_Tax <- cont_data_list[["Tax"]]+labs(x="Tax",title = "Tax Histogram")
plot_Total_amt <- cont_data_list[["total_amt"]]+labs(x="Total Amount",title = "Total_Amt Histogram")

cowplot::plot_grid(plot_Qty,plot_Rate,plot_Tax,plot_Total_amt)

#  B:frequency bars for all categorical vars.

# Prod_cat
freq_data %>% ggplot(aes(x=prod_cat)) + geom_bar(fill = "steelblue",color="tomato",alpha=0.5)

# Gender
freq_data %>% ggplot(aes(x=Gender)) + geom_bar(fill = "steelblue",color="tomato",alpha=0.5)

#Store_type   
freq_data %>% ggplot(aes(x=Store_type)) + geom_bar(fill = "steelblue",color="tomato",alpha=0.5)

# City_code  
freq_data %>% ggplot(aes(x=city_code)) + geom_bar(fill = "steelblue",color="tomato",alpha=0.5)

# Prod_Subcat    
freq_data %>% ggplot(aes(y=prod_subcat)) + geom_bar(fill = "steelblue",color="tomato",alpha=0.5)



#-------------------------
#Ques4:	Calculate the following information using the merged dataset :

#   A:	Time period of the available transaction data.
Time_period <- max(customer_final_dplyr$tran_date)-min(customer_final_dplyr$tran_date)
Time_period

#   B:	Count of transactions where the total amount of transaction was negative.
negative_trans_count <-  sum(customer_final_dplyr$Qty<0,na.rm = T)
negative_trans_count
    
# Ques 5:	Analyze which product categories are more popular among females vs male customers.

Male_Vs_Female <- customer_final_dplyr  %>% 
                  count(prod_cat,Gender,sort = T,name = "Count") %>% remove_missing(na.rm = T)

Male_Vs_Female

comp_graph <-  Male_Vs_Female %>%
                ggplot(aes(x=prod_cat,y=Count,fill=Gender))+
                geom_col(position = position_dodge())+
                labs(title = " Popular Product Category ",subtitle = " MALE VS FEMALE ")+
                theme(plot.title = element_text(size = 20,hjust = 0.5),
                      plot.subtitle = element_text(size=15,hjust = 0.5,face="bold"))

comp_graph

# Inference:
# Books, Electronics and Home and Kitchen,all are equally popular among male and female
# Footwear category is more popular among female as compared to male.
# Clothing is more popular among male as compared to female.

#---------------------------------------------------------------------------
# 6: 	Which City code has the maximum customers and -
#      what was the percentage of customers from that city ?

max_cust_city <-  customer_final_dplyr %>% 
                   filter(!is.na(city_code)) %>%
                   count(city_code,sort=T) %>%
                   mutate(prop = paste0 ( round(n/sum(n)*100,1),"%")) %>% 
                   head(1) 
max_cust_city
 
#-----------------------
# Ques7:	Which store type sells the maximum products by value and by quantity?
 
# Max products by quantity
max_prd_by_qty <- customer_final_dplyr %>% count(Store_type,wt=Qty,sort = T) %>% head(1)
max_prd_by_qty

# Max products by amount
max_prd_by_amt <-  customer_final_dplyr %>% count(Store_type,wt=total_amt,sort = T) %>% head(1)
max_prd_by_amt

#------------------------------------------ 
  
# Ques 8: What was the total amount earned from the "Electronics" and "Clothing" 
#            categories from Flagship Stores?

total_by_prod_cat <- customer_final_dplyr %>% 
                      filter(
                      Store_type=="Flagship store",
                      prod_cat %in% c("Electronics","Clothing")) %>% 
                      group_by(Store_type,prod_cat) %>%
                      summarize(Total_sales = sum(total_amt))
total_by_prod_cat
#------------------------------------------------------
# 9.	What was the total amount earned from "Male" customers under the-
#     "Electronics" category?

total_amt_earned <- customer_final_dplyr %>%
                      filter(
                      Gender == "M",
                      prod_cat == "Electronics") %>%
                      group_by(prod_cat) %>% 
                      summarize(Total_Amt_Earned = prettyNum(sum(total_amt),big.mark = ",",decimal.mark = "."))%>%
                      select(Total_Amt_Earned)

total_amt_earned                
 
   #---------------------------------------
 
 # 10.	How many customers have more than 10 unique transactions,
 #  after removing all transactions which have any negative amounts?
 #   
 unique_10_trans <- customer_final_dplyr %>% 
                       filter(total_amt>0) %>%  ## all negative transactions removed
                       group_by(cust_id) %>% 
                       summarise(Count=n()) %>% 
                       filter(Count>=10) %>% 
                       arrange(desc(Count)) %>% 
                        nrow() ## Counting no of customers
 
unique_10_trans


#------------------------------------------
# 11.	For all customers aged between 25 - 35, find out:

### Data To analyze
ques11 <- customer_final_dplyr %>%
          mutate(
          age = eeptools::age_calc(DOB,units = "years",precise = T)) %>% 
          filter(
          age >=25 & age<=35) %>% 
          mutate(Age=round(age)) %>%
          select(-age)

#--------------------
##  a.	What was the total amount spent for “Electronics” and “Books” product categories?

Ans11_A <- ques11 %>% 
           filter(
            prod_cat %in% c("Electronics","Books")) %>%
           group_by(prod_cat) %>% 
           summarize(Total=sum(total_amt))
Ans11_A

#-----------
#   b.	What was the total amount spent by these customers-
#        between 1st Jan, 2014 to 1st Mar, 2014?

Ans11_B <-  ques11 %>%
            filter(
              tran_date >= "2014-01-01" & tran_date <= "2014-03-01") %>%
            summarise(Total_Amt_Spent=sum(total_amt))

Ans11_B




