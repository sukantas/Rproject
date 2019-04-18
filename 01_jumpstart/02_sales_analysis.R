# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)


# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
bikeshos_tbl <- read_excel("00_data/bike_sales/data_raw/bikeshops.xlsx")
orderline_tbl <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")


# 3.0 Examining Data ----





# 4.0 Joining Data ----
left_join(orderline_tbl,bikes_tbl, by = c("product.id"="bike.id"))
bike_orderline_join_tbl <- orderline_tbl %>% 
    left_join(bikes_tbl, by =c("product.id"="bike.id")) %>%
    left_join(bikeshos_tbl, by=c("customer.id"="bikeshop.id"))    


# 5.0 Wrangling Data ----

bike_orderlie_wrangled_tbl <-  bike_orderline_join_tbl %>% 
    separate(description, 
             into = c("category.1", "category.2","frame.material"),
             sep = "-" , 
             remove = TRUE) %>%
    separate(location, into=c("city","state"),
             sep=",", remove = FALSE) %>%
    mutate(total.price=price*quantity) %>%
    select(-"...1") %>%
    #select(-ends_with(".id")) %>%
    bind_cols(bike_orderline_join_tbl %>% select(order.id))%>%
    #reordering columns
    select(contains("date"), everything())%>%
    #rename
    rename(order_date=order.date) %>%
    set_names(names(.) %>% str_replace_all("\\.","_")) 
bike_orderlie_wrangled_tbl %>% glimpse()






# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlie_wrangled_tbl %>%
    select(order_date, total_price) %>%
    mutate(year=year(order_date)) %>%
#groupby year and sum of price
    group_by(year)%>%
    summarise(sales=sum(total_price)) %>%
    ungroup() %>%
#dolar str
    mutate(sales_text = scales ::dollar(sales))

# Step 2 - Visualize
sales_by_year_tbl %>%
 #setup canvas x axis and y axis
    ggplot(aes(x=year,y=sales)) +
    geom_col(fill="#00fff0")+
    geom_label(aes(label=sales_text)) +
#formatting
    theme_tq() +
    scale_y_continuous(labels = scales :: dollar)+
    labs(
        title = "Revenue by year",
        x="",
        y="Revenue"
    )
    


# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat_2_tbl <- bike_orderlie_wrangled_tbl %>%
    select(order_date, total_price,category_2) %>%
    mutate(year=year(order_date)) %>%
    #groupby year and category2 of price
    group_by(year,category_2)%>%
    summarise(sales=sum(total_price)) %>%
    ungroup() %>%
    #dolar str
    mutate(sales_text = scales ::dollar(sales))

sales_by_year_cat_2_tbl

# Step 2 - Visualize
sales_by_year_cat_2_tbl %>%
    #setup canvas x axis and y axis
    ggplot(aes(x=year,y=sales,fill=category_2) )+
    geom_col()+
    #facet
    facet_wrap(~ category_2, ncol=3)+
    geom_smooth(method = "lm", se = FALSE)+
    
#formatting
    theme_tq() +
    scale_fill_tq()+
    scale_y_continuous(labels = scales :: dollar)+
    labs(
        title = "Revenue by Year & Category2",
        x="",
        y="Revenue"
    )


# 7.0 Writing Files ----
fs::dir_create("00_data/data_raw_sukanta_works")

# 7.1 Excel ----
bike_orderlie_wrangled_tbl %>%
    write_xlsx("00_data/data_raw_sukanta_works/bike_orderlie_wrangled_tbl.xlsx")


# 7.2 CSV ----
bike_orderlie_wrangled_tbl %>%
    write_csv("00_data/data_raw_sukanta_works/bike_orderlie_wrangled_tbl.csv")


# 7.3 RDS ----
bike_orderlie_wrangled_tbl %>%
    write_rds("00_data/data_raw_sukanta_works/bike_orderlie_wrangled_tbl.rds")
