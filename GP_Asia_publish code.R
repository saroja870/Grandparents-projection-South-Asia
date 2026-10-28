rm(list = ls())

# Here I show how to run two-sex time-variant models for selected countries using WPP 2024 data, single age and five years period

library(readxl)
library(tidyverse)
library(data.table)
library(httr)

# install.packages("DemoKin")
library(DemoKin)

# 0. Parameters ----

# Keep only data for selected country and required variables
cnt<-c("Afghanistan","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Iran(Islamic Republic Of)","Sri Lanka")
asfr_ages <- seq(0,100,5)

ccode<-c(4,50,64,356,462,524,586,364,144)
selected_columns<-c("Variant","`Region, subregion, country or area *`","`Location code`","Year",
                    "`10-14`","`15-19`","`20-24`","`25-29`","`30-34`","`35-39`","`40-44`","`45-49`","`50-54`")

##Add reg in Iso code so it matches with WIC data ISO

iso_codes <- c("reg42055", "reg502055", "reg642055", "reg3562055", "reg4622055", "reg5242055", "reg5862055","reg1442055","reg3642055")


# 1. Download data from UNWPP2024  


base_url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/"

options(timeout = 600)

read_wpp <- function(file) {
  tmp <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(tmp))
  download.file(paste0(base_url, file), tmp, method = "libcurl", mode = "wb", quiet = TRUE)
  fread(tmp, skip = "LocID")
}

asfrdt     <- read_wpp("WPP2024_Fertility_by_Age5.csv.gz")
mortdt     <- read_wpp("WPP2024_Life_Table_Abridged_Medium_1950-2023.csv.gz")
mortprojdt <- read_wpp("WPP2024_Life_Table_Abridged_Medium_2024-2100.csv.gz")


# 2. Select only required information for female fertility -----

asfrdt<-asfrdt[LocID%in%ccode][,.(variant=Variant,name= Location,country_code=LocID,year=Time,age=AgeGrpStart,asfr=ASFR)] [year%in%seq(1952, 2100, by = 5)] 


asfr_female <-asfrdt %>% mutate(asfr = (asfr/1000),age = factor(age, levels = asfr_ages)) %>% 
  group_by(country_code, year,variant,name) %>%  complete(age, fill = list(asfr = 0)) %>% ungroup() %>% 
  mutate(age = as.numeric(as.character(age)))%>%filter(variant=="Medium")




# 3. Male fertility ----

# Male age-specific fertility rates are unavailable. We therefore assume the same fertility
# level for men and women but shift the female age-specific fertility schedule by the
# country-specific difference in mean age at childbearing (MACB) between men and women.
# Data on male MACB: https://perso.uclouvain.be/bruno.schoumaker/data/
# This is a simplified approach that could be refined in future work.


url <- "https://perso.uclouvain.be/bruno.schoumaker/data/A.%20Estimates%20of%20male%20and%20female%20fertility.xlsx"

tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", method = "libcurl")
tfr_diff <- read_excel(tmp, sheet = 1)
unlink(tmp)


# Calculate the number of years by which we should shift the male distribution to the right
# relative to the female one, while keeping the fertility level same

shift_by <-  tfr_diff %>% 
  filter(`UN Country Code` %in% ccode) %>% 
  select(country_code = `UN Country Code`, male = `Mean age at fatherhood (shown on Figures)`, female = `Mean age at childbearing (shown on Figures)`) %>% 
  mutate(diff = as.numeric(male) - as.numeric(female)) %>% mutate(diff=round((diff)/5)*5)%>%filter(!is.na(diff))%>%select(country_code,diff)

shift_by<-rbind(shift_by,c(country_code=144,diff=5)) # added 5 year difference for sri lanka From Bruno's work

#Now estimate male ASFR
asfr_male <- merge(asfr_female,shift_by,all.x = T)%>%
  mutate(age = (age+diff) )%>% filter(age %in% asfr_ages) %>% 
  mutate(age = factor(age, levels = asfr_ages)) %>% group_by(country_code, year,variant,name) %>%
  complete(age, fill = list(asfr = 0)) %>%  ungroup() %>% 
  mutate(age = as.numeric(as.character(age)))%>%select(-diff)

# Plot male and female fert side by side for Nepal in 2030-2034 to check

asfr_female %>% mutate(sex = "f", variant=="medium") %>%
  bind_rows(asfr_male %>% mutate(sex = "m")) %>%
  filter(year == 2032, name == "Nepal") %>%
  ggplot(aes(x = age, y = asfr, group = sex, linetype = sex)) +
  geom_line() +
  theme_bw()



# 4.  Prepare survival probability and select only medium variant --------

mortdt<-mortdt[LocID%in%ccode][,.(variant=Variant,name= Location,country_code=LocID,year=Time,age=AgeGrpStart,sex=Sex,px)] [year%in%seq(1952, 2100, by = 5)] 

mortprojdt<-mortprojdt[LocID%in%ccode][,.(variant=Variant,name= Location,country_code=LocID,year=Time,age=AgeGrpStart,sex=Sex,px)] [year%in%seq(1952, 2100, by = 5)] 

ltdt<-rbind(mortdt,mortprojdt)
lt_female<-ltdt[sex=="Female"][, sex := NULL]
lt_male<-ltdt[sex=="Male"][, sex := NULL]

with(lt_female,table(variant))



# 5. Create matrices and run model ----
variant<-c("Lower 80 PI","Lower 95 PI", "Medium","Upper 80 PI", "Upper 95 PI")


for (i in 1: length(variant)) {
  var<-variant[3]
  
  fulldt<-tibble()
  
  #  Select country
  countries<-c("Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Iran (Islamic Republic of)","Sri Lanka")
  
  for (i in 1:length(countries)) {
    
    cnt<-countries[i]
    
    
    # Reshape fertility
    
    fm<- asfr_male %>% as.data.frame()%>%filter(variant%in%var & name==cnt)%>%
      select(age, year, asfr) %>%
      pivot_wider(names_from = year, values_from = asfr) %>%
      select(-age) %>%
      as.matrix()
    
    ff <- asfr_female %>% as.data.frame()%>%filter(variant%in%var & name==cnt)%>%
      select(age, year, asfr) %>%
      pivot_wider(names_from = year, values_from = asfr) %>%
      select(-age) %>%
      as.matrix()
    
    
    # Reshape mortality
    
    
    pm <-lt_male %>% as.data.frame()%>%filter(variant%in%var & name==cnt & age>0)%>%
      mutate(age=ifelse(age==1,0,age))%>%
      select(age, year, px) %>%
      pivot_wider(names_from = year, values_from = px) %>%
      select(-age) %>%
      as.matrix()
    
    pf <- lt_female %>% as.data.frame()%>%filter(variant%in%var & name==cnt & age>0)%>%
      mutate(age=ifelse(age==1,0,age))%>%
      select(age, year, px) %>%
      pivot_wider(names_from = year, values_from = px) %>%
      select(-age) %>%
      as.matrix()
    
    # 5.2. Run model --------
    
    kin_out <-  kin2sex(
      pf = pf,
        pm = pm,
        ff = ff,
        fm = fm,
        time_invariant = FALSE,
        # Optional: if not specified, will give you all available data
        # Specifying years, kin types, etc., makes the function a bit faster
        output_period =seq(1952,2100,by=5),
        output_kin = c( "gd", "gm"))
    
    
    newdt<-kin_out$kin_full%>%mutate(year=year,country=cnt, age_kin=age_kin*5,age_focal=age_focal*5,variant=var)
    
    fulldt<-bind_rows(fulldt,newdt)
  }
  
  # Save the combined kinship data for each country as a CSV file
  base_filename <- "_kin_standard1_twosex24_v1"  # Common suffix to identify the model specification
  filepath <- "../Kinedudist/data/output/gpasia/" # Output directory
  filename <- paste0(var, base_filename, "_", ".csv") # Construct filename using the variant and suffix
  write.csv(fulldt, paste0(filepath, filename), row.names = FALSE) # Export the dataset
  
  
}


#6.  Add education data on combined kinship data-----------------------
library (wcde)

 # select coulor and countries
ipal =  c("darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3",
          'black','black')#,"darkslateblue")

apal<-c("#CC79A7","#D55E00","#0072B2","black")

cntcode<-c(4,50,64,356,462,524,586,364,144)

# Read and reshape Kin data and education data from WIC----


 #6.1. Download education distribution by age, sex, country and SSP from Wittgenstein center for 2024  and onwards------

edudt1<-get_wcde(indicator = "prop",pop_edu="six",version = "wcde-v3",scenario = c("1","2","3")); setDT(edudt1)

edudt1<-edudt1[country_code%in%cntcode]

edudt<-edudt1[sex!="Both"][age!="All"][, agest:=substr(age,1,3)][,agest:=readr::parse_number(agest)]
edudt<-edudt[sex!="Both"][,.(prop=(prop/100), age_kin=agest,sex_kin=sex,country=name,year,scenario,education)]

edudt<-edudt[education%in%c("Under 15","No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")]
edudt[.(sex_kin=c("Male","Female"),to=c("m","f")),on="sex_kin",sex_kin:=i.to]
edudt<-edudt[year<2100][,year:=year+2][!(age_kin>=15 & education =="Under 15")]


#Kin data reshape

kindt <- fulldt
kindt<-setDT(kindt);kindt$year<-as.numeric(kindt$year);kindt$age_kin<-as.numeric(kindt$age_kin);kindt$age_focal<-as.numeric(kindt$age_focal)
kindt<-kindt[kin=="gm"][,.(country,year,age_kin,sex_kin,age_focal,pop=living,kin)] 

#6.2. merge kinship and education data-----------------------------------------
fdt<-kindt[edudt,on=.(age_kin,sex_kin,year,country),allow.cartesian = T]
fdt$year<-fdt$year-2

fdt1<-fdt[year>=1950]
fdt1$variant<-ifelse(fdt1$year<=2022,"estimated","projected")

fdt1$sex_kin<-recode_factor(fdt1$sex_kin,"f"="Grandmother","m"="Grandfather")

fdt1$country[fdt1$country=="Iran (Islamic Republic of)"]<-"Iran"

#remove unnecessary data to save memory
rm(edudt); rm(edudt1); rm (kindt); rm(fdt)


#7. Visualization------------------------------------

#7.1 Figure1: Change in number of GP by age and sex over time  ----------------
dff<-fdt1[,.(year,pop,country,age_kin,sex_kin,variant,age_focal)]; dff<-unique(dff)

# make broader age group of GP
dff$age_kin<-ifelse(dff$age_kin<60,"<60",ifelse(dff$age_kin>=60 & dff$age_kin<70,"60-69","70+") )

dff<-dff[,pop1:=sum(pop),by=.(year,country,sex_kin,variant,age_focal,age_kin)]
dff<-unique(dff[,.(year,pop1,country,sex_kin,variant,age_focal,age_kin)])
dff$age_kin<-factor(dff$age_kin,labels = c("<60","60-69","70+"))

# GP by age sex and country
main_data <- dff %>% filter(age_focal == 0)
main_data$age_kin<-factor(main_data$age_kin,levels = c("70+","60-69","<60"))

# total GP of all ages
total_data <- main_data %>% group_by(country, year, sex_kin) %>%
  summarise(pop1 = sum(pop1, na.rm = TRUE), .groups = "drop") %>%
  mutate(age_kin = "Total", variant=ifelse(year<2025,"historical","projection"))

# plot Figure1

ggplot() +
  geom_area(data = main_data ,aes(x = year, y = pop1, fill= age_kin, group = age_kin),
            alpha = 0.8, position = "stack",se=F) +
  geom_line(data = total_data ,aes(x = year, y = pop1, color = "Total",linetype=variant),linewidth = 0.9) +
  facet_grid(sex_kin ~ country) +
  labs(x = "Time", y = "Average number of living grandparents (focal age 0–4)",
       fill = "Grandparent Age Group",color = "",linetype = "") +
  scale_x_continuous(breaks = c(1950,2000,2020,2050,2080),labels =c(1950,2000,2020,2050,2080),limits = c(1950,2080))+
  scale_fill_manual(values = apal) +
  scale_color_manual(values = c("Total" = "black")) +
  theme_bw(base_size = 13.5) +
  theme(legend.position = "top", axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),strip.text = element_text(face = "bold"))




#7.2 Figure2:  Relative change in share of GP by education In India across scenarios--------------

dff<-fdt1[age_focal==0][year%in%c(2020,2080)]

dff<-dff[,epop:=(pop*prop)][,pop1:=sum(epop),by=.(year,country,age_kin,education,scenario,sex_kin,kin,age_focal)]
dff<-dff[ pop1>0]

dff<-dff[,pop2:=sum(pop1),by=.(year,country,scenario,sex_kin,kin,age_focal,education)][,.(
  year,country,age_kin,education,scenario,sex_kin,kin,age_focal,pop2)]


dff<-dff[,eprop:=pop2/sum(pop2),by=.(year,country,scenario,sex_kin,kin,age_focal)][,.(year,country,scenario,age_focal,
     sex_kin,education,eprop)] ;dff<-unique(dff)

# Rename scenario
dff$scenario <- factor(dff$scenario, levels = c(1, 2, 3), labels = c("SSP1", "SSP2", "SSP3"))

# Calculate change in share of GP
dfe_change <- dff %>% pivot_wider(names_from = year, values_from = eprop, names_prefix = "y") %>%
  mutate(change = (((y2080 - y2020)/y2020)*100)) %>%select(-y2080,-y2020)%>%unique()

dfe_change$education<-factor(dfe_change$education, levels=c("No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary"))

#Plot Figure2

ggplot(dfe_change%>%filter(country== "Iran"), aes(y = education, x = change,fill=education)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(scenario~ sex_kin,scales = "free") +
  geom_text(aes(label = round(change, 1), hjust = ifelse(change > 0, 1, 1)),
            position = position_identity(), size = 3.2, colour = "black")+
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.1)))+
  theme_bw() +
  labs(x= "Relative change (%) in share of grandparents from 2020-2024 to 2080-2084, India",
    y = "Education of grandparents",
    fill = "Education") +
  scale_fill_manual(values = ipal)+
  theme(text = element_text(size = 11),legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1),axis.text = element_text(colour = "black",size=10),
        strip.text = element_text(face = "bold",size=10))




#7.3. Figure3: Age, sex, edu pyramid across different time-------------------------------------------------------
dff<-fdt1[year%in%c(2020,2050,2080)]

dff<-dff[,epop:=(pop*prop)][,pop1:=sum(epop),by=.(year,country,age_kin,education,scenario,sex_kin,kin,age_focal)]
dff<-dff[scenario==2 & pop1>0]
dff$year<-paste(dff$year,dff$year+4,sep="-")
dff$education<-factor(dff$education, levels=c("No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary"))
dff$country[dff$country=="Iran (Islamic Republic of)"]<-"Iran"
dff$year[dff$year=="2095-2099"]<-"2095-2100"

df <- dff %>% filter(age_focal == 0, age_kin > 25) %>%mutate(pop_plot = ifelse(sex_kin == "Grandfather", -pop1, pop1))

ggplot(df, aes(x = age_kin, y = pop_plot, fill = education, colour = education)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(year ~ country) +
  coord_flip() +
  geom_text(data = data.frame(age_kin = 100, pop_plot = c(-0.1, 0.1), label = c("M", "F")),
            aes(x = age_kin, y = pop_plot, label = label), inherit.aes = FALSE, size = 3.5) +
  labs(x = "Age of Grandparents", y = "Average Number of Grandparents (focal age 0–4)") +
  scale_y_continuous(breaks = seq(-1, 1, 0.25), labels = abs(seq(-1, 1, 0.25))) +
  scale_colour_manual(values = ipal) +
  scale_fill_manual(values = ipal) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90),legend.position = "top",
        panel.border = element_blank(), axis.text = element_text(colour = "black",size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank(),legend.title = element_blank() )



#7.4. Figure4:Change in GP across Focal's lifecourse---------------------------------------------------------

dff<-fdt1

dff<-dff[,epop:=(pop*prop)][,pop1:=sum(epop),by=.(year,country,age_kin,education,scenario,sex_kin,kin,age_focal)]
dff$year<-paste(dff$year,dff$year+4,sep="-")
dff$education<-factor(dff$education, levels=c("No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary"))

dff<-dff[,pop1:=sum(pop1),by=.(year,country,sex_kin,variant,age_focal,scenario,education)]
dff<-unique(dff[,.(year,pop1,country,sex_kin,variant,age_focal,education, scenario)])
dff$scenario <- factor(dff$scenario, levels = c(1, 2, 3), labels = c("SSP1", "SSP2", "SSP3"))
dff$year[dff$year=="2095-2099"]<-"2095-2100"


# Main data

main_data <- dff %>% filter( age_focal<50 & !is.na (education) & country%in%c("India","Sri Lanka"))%>%mutate(cohort=as.numeric(substr(year,1,4))-age_focal)
main_data<-main_data[,age_focal:=paste(age_focal,age_focal+4,sep = "-")]

age_levels <- c("50-54", "45-49", "40-44", "35-39", "30-34","25-29", "20-24", "15-19", "10-14", "5-9", "0-4")

# Apply the new order to the factor
main_data$age_focal <- factor(main_data$age_focal, levels = rev(age_levels))

main_data$age_kin<-factor(main_data$age_kin,levels = c("70+","60-69","<60"))

# Total lines
total_data <- main_data %>%group_by(country, cohort, sex_kin,age_focal,scenario) %>%
  summarise(pop1 = sum(pop1, na.rm = TRUE), .groups = "drop") %>%
  mutate(age_kin = "Total")%>%unique()%>% filter(cohort %in% c(2020,2030,2050) & scenario=="SSP2" )

lvl <- levels(main_data$age_focal)

md <- main_data %>%filter(cohort %in% c(2020,2030,2050), scenario=="SSP2", sex_kin=="Grandmother") %>%
  mutate(age_focal = factor(age_focal, levels = lvl),
         x = as.numeric(age_focal),cohort=paste(cohort,cohort+4,sep = "-"))%>%mutate(cohort=paste("Focal birth cohort", cohort,sep = " "))

td <- total_data %>% filter(cohort %in% c(2020,2030,2050), scenario=="SSP2") %>%
  mutate(age_focal = factor(age_focal, levels = lvl),
         x = as.numeric(age_focal),cohort=paste(cohort,cohort+4,sep = "-"))%>%mutate(cohort=paste("Focal birth cohort", cohort,sep = " "))

mx <- max(td$pop1, na.rm = TRUE)

# plot Figure 4

ggplot() +
  geom_area(data = md,aes(x = x, y = pop1, fill = education, group = education),alpha = 0.8,position = "fill") +
  geom_line(data = td %>% arrange(x),aes(x = x,
         y = pop1 / mx,linetype = sex_kin,group = sex_kin),colour = "black",linewidth = 1) +
  facet_grid(country~cohort) +
  scale_x_continuous(breaks = seq_along(lvl), labels = lvl) +
  scale_y_continuous(limits = c(0, 1),name = "Grandmother's education composition (in proportion)",
       sec.axis = sec_axis(~ . * mx, name = "Average number of living grandparents") ) +
  scale_fill_manual(values = ipal) +
  scale_linetype_manual(values = c("Grandmother" = "solid", "Grandfather" = "dotted"),name = "")+
  theme_bw(base_size = 12.5) +
  theme(legend.position = "top",axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(face = "bold") ) +
  labs(x = "Age of Focal", fill = "Grandmother's education")

