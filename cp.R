library(readr)
library(tidyverse)
library(gtsummary)
library(gsubfn)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(ggrepel)
library(scales)
library(gghighlight)

theme_gtsummary_compact()
theme_set(theme_minimal())
pal<-c("#661100", "#882255", "6699CC", "#888888",
       "#332288", "#AA4499", "#44AA99", "#999933",
       "#CC6677", "#DDCC77")

cda<-read_csv("C:/Users/pauli/Desktop/archive/cd.csv")
cda<-cda %>% mutate(Allocation=
as.numeric(gsubfn('([A-Z]|[\\$,])', 
                  list(B='e+9', M='e+6',"$"="", ","=""),
                  cda$`$ Allocation`)), Year=as.Date(as.character(Year), 
                                                     format = "%Y"))

# cda %>% 
#   # mutate(`Allocation in $ `= label_number_si(accuracy=0.1,prefix="$")(Allocation)) %>% 
#   select(Lender,Country,`Invested On`, Allocation ) %>%  
#   tbl_summary(sort = list(everything() ~ "frequency")) %>%
#   bold_labels() 



#Highest Lender(allocation-wise)
t<-cda %>% group_by(Country) %>% summarize(Total=sum(Allocation)) %>% 
  arrange(desc(Total))

#Biggest borrower(allocation-wise)
# How the 50 African countries fared in terms of borrowing.

c<-cda %>% group_by(Country) %>% summarize(Frequency=n()) %>% 
  arrange(desc(Frequency))
c %>% mutate(Rank=seq(1,50))

#Most invested sector
i <- cda %>% group_by(`Invested On`) %>% summarize(Total=sum(Allocation)) %>% 
  arrange(desc(Total))

## ---- a
#Lending trend over the years

t<-cda %>% group_by(Year) %>% summarize(Total=sum(Allocation)) %>% 
   arrange(desc(Total))

cda %>% group_by(Year)%>% summarize(Total=sum(Allocation)) %>% 
ggplot(aes(x=Year,y=Total))+geom_line(color="firebrick",size=1.05)+
scale_x_date(date_breaks = "2 years",
                  date_labels="%Y") + theme_wsj()+
  coord_cartesian(ylim = c(100000000,NA),expand = F) +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = c(500000000,
                      2000000000,5000000000,
                    10000000000,15000000000,20000000000,25000000000))+
  theme(axis.text.y = element_text(size = 10 ))+
theme(axis.text.x = element_text(size = 8 ),
      plot.title = element_text(size=15,colour = "firebrick",
                                hjust = 0.45),
      plot.subtitle = element_text(size=10,colour = "grey",vjust = 0.125))+
  labs(title= "Trend of Chinese Debt to Africa 
       from Year 2000 to 2020",
       subtitle = "Chinese Debt Trap Dataset from Kaggle - 2022",
       y="Total Debt", x="Year")+
  geom_text_repel(
    x = 15,
    y = 23500000000,
    label = "Max. Total Loan Amount: $28.4B \nMin. Total Loan Amount: $139.0M",
    stat="unique",family="Courier", fontface="bold.italic",
    colour="red3",size=2.5)


#s<-c[c(1:10,446:455),c(1:3)] %>% drop_na()


## ---- b 
#Top 5 borrowers vs. Bottom 5 borrowers

cda %>% select(Year,Country,Allocation) %>% 
  filter(Country %in% c("Angola","Ethiopia",
  "Zambia","Kenya", "Egypt")) %>% 
  ggplot(aes(x=Year,y=Allocation,color=Country)) +
  geom_point()+ geom_smooth(method = "lm",fill="darkseagreen4",
                           size=0.75, colour="red4")+ 
  scale_x_date(date_breaks = "2 years",
                           date_labels="%Y") + 
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(axis.text.x = element_text(size = 6))+
  facet_grid(. ~ Country, 
    scales="free_x", space="free_x") +
  scale_color_manual(values=pal[1:5]) + 
  coord_cartesian(ylim = c(1000000,2600000000),expand = F)+
  labs(title="Top 5 African Borrowers",
      subtitle =  "Chinese Debt Trap Dataset from Kaggle - 2022")


cda %>% select(Year,Country,Allocation) %>% 
  filter(Country %in% c("Liberia",
                        "Cape Verde","Seychelles","The Gambia",
                        "Algeria")) %>% 
  ggplot(aes(x=Year,y=Allocation,color=Country)) +
  geom_point()+ geom_smooth(method = "lm",colour="red4",
                            size=0.75)+ scale_x_date(date_breaks = "2 years",
              date_labels="%Y") +
  coord_cartesian(ylim = c(50000,51000000),expand = F)+
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(axis.text.x = element_text(size = 7))+
  facet_grid(. ~ Country, 
             scales="free_x")+
  scale_color_manual(values=pal[6:10])+ 
  labs(title="Bottom 5 African Borrowers",
       subtitle = "Chinese Debt Trap Dataset from Kaggle - 2022")

## ---- c
cda %>% select(Year,`Invested On`,Country,Allocation) %>% 
  filter(Country %in% 
           c("Angola","Ethiopia",
                   "Zambia"),
         `Invested On`%in% 
           c("Environment",
           "Health", "Education","Agriculture","Government",
  "Industry","Mining", "Water", "Trade", "Power")) %>% 
  
  #group_by(`Invested On`) %>% summarize(total=sum(Allocation))
           #arrange(desc(Allocation),Country) 
   slice_head(n=84) %>% 
  ggplot(aes(x=Year,y=Allocation,col=`Invested On`))+
  geom_point()+ geom_line(size=3)+#geom_smooth(method="lm",se=F)+
  scale_y_continuous(labels = scales::dollar_format())+
  scale_x_date(date_breaks = "2 years",
               date_labels="%Y") + theme_wsj()+ 
  theme(legend.position = "bottom",legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10 ),
        axis.text.y = element_text(size = 10 ),
        plot.subtitle = element_text(size=10,colour = "grey",vjust = 0.125),
        plot.title = element_text(size=15)) +
  scale_color_manual(values=pal)+
  labs(title = "What did Africa's Top 3 Borrowers Spend On?",
       subtitle = "Chinese Debt Trap Dataset from Kaggle - 2022")+
     gghighlight(`Invested On` %in% c('Mining',
                'Environment'), use_direct_label = F)
 
 

## ---- d

#For W.Africa:
cda %>% select(Country,Allocation) %>% 
  filter(Country %in% c("Benin","Burkina Faso","Cape Verde","Côte D'Ivoire", 
  "Gambia", "Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania", 
"Niger","Nigeria","Senegal","Sierra Leone","Togo")) %>% 
  ggplot(aes(y=Country,x=Allocation))+
  geom_density_ridges(fill="#2a9754",colour="white") +
  scale_x_continuous(labels = scales::dollar_format(),
                     limits = c(-20000000,200000000))+ 
  labs(title="Spotlight on West Africa: 
       Borrowing Patterns",
       subtitle = "Chinese Debt Trap Dataset from Kaggle - 2022" )

cda %>% select(Country,Allocation) %>% 
filter(Country %in%
  c("Benin","Burkina Faso","Cape Verde","Côte D'Ivoire", 
    "Gambia", "Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania", 
      "Niger","Nigeria","Senegal","Sierra Leone","Togo")) %>%
 group_by(Country) %>%  summarize(Total=sum(Allocation)) %>% 
  mutate(D=label_number_si(accuracy=0.1,prefix="$")(Total))%>% 
  arrange(Total) %>%    
  mutate(Country=factor(Country, levels=Country)) %>%
  ggplot( 
       aes(x=Country, 
           y=Total,label=D)) +
  geom_point(color= "#2a9754", 
             size = 4) +
  geom_segment( aes(xend=Country, yend=0),color="lightgrey") +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip() +
  geom_text_repel(
                  size= 2.5,
                      force = 0.5,
                      nudge_x = 0,
                      direction= "y",
                      hjust = -0.75,
                      segment.size = 0.2)+
  labs (x = "Total Allocation",
        y = "Amount in Dollars",
        title = "Spotlight on West Africa:
        Total Allocation by Country",
        subtitle = "Chinese Debt Trap Dataset from Kaggle - 2022") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## ---- e

gh<-cda %>% select(Year,`Invested On`,Country,Allocation) %>% 
  filter(Country=="Ghana") %>% 
  ggplot(aes(x=Year,y=Allocation,col=`Invested On`))+
  geom_point()+geom_smooth(method="lm",size=2,se=F)+
  scale_y_continuous(labels = scales::dollar_format())+
  scale_x_date(date_breaks = "2 years",
               date_labels="%Y") + theme_wsj()+ 
  theme(legend.position = "bottom",legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10 ))+
  scale_color_manual(values=pal)
gh


gh+
gghighlight(`Invested On` == 'Power', use_direct_label = F)+
geom_text_repel(
  x = 15,
  y = 755000000,
  label = "And yet Ghana still has power issues!",
stat="unique",family="serif", fontface="bold", size=5.75)+
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10 ))+
  directlabels::geom_dl(aes(label = "Highest Allocation: Power!"), 
                    method = list("smart.grid",cex=0.85),vjust=0.25)
  

