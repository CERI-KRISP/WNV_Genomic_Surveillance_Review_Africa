library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(ggpubr)
library(cowplot)

####Count of papers published over time (dataset Study_focus.csv)

print(colnames(Study_focus))
Study_focus2 <- Study_focus %>% rename(focus = "Study area (Focused on Africa / elsewhere but contains African sequences)")

df_summary <- Study_focus2 %>%
  group_by(Year, focus) %>%
  summarise(Count = n()) %>%
  ungroup()

print(df_summary)

plot1 <- ggplot(df_summary, aes(x = Year, y = Count, fill = focus)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "Year",
       y = "Number of publications",
       fill = "")+
  theme_bw()+theme(legend.position = "top")+
  theme(axis.title = element_text(face="bold", size=12))+
  theme(axis.text = element_text(size=11))+
  theme(legend.text = element_text(size = 11))+
  scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
  scale_fill_manual(values=c("#e0a890","#475b5a", "#a1cda8"))+
  guides(fill = guide_legend(ncol = 1))

plot1 


####Count of studies - molecular methods (dataset PCR_genes)
PCR_gene_summary <- PCR_genes %>%
  group_by(Method_gene, Surveillance) %>%
  summarise(Count = n())

print(PCR_gene_summary)

PCR_gene_summary$Method_gene <- factor(PCR_gene_summary$Method_gene, levels = c('RT-PCR', 'Non-structural protein 5', 'Whole genome', 'Envelope', "3' Untranslated region", 'Non-structural protein 3', 'Polyprotein gene', 'Transcriptome'))


plot2 <- ggplot(PCR_gene_summary, aes(x = Method_gene, y = Count, fill=Surveillance)) +
 geom_bar(stat = "identity") +
 labs(x = "Molecular method",
     y = "Number of publications")+
theme_classic()+theme(legend.position = "top")+
theme(axis.title = element_text(face="bold", size=12))+
theme(axis.text.x = element_text(angle = 45, vjust=0.55, size=12))+
theme(axis.text.y = element_text(size=10))+
theme(legend.position = 'none')+
scale_fill_manual(values=c('red', 'darkorange', 'gold2', 'springgreen3', 'deepskyblue3'))

plot2 



####Count of studies - lab work done internally/externally (dataset Internal_external)
Internal_external2 <- Internal_external %>%
  group_by(Internal_external) %>%
  summarise(Count = n()) %>%
  ungroup()

#Internal_external2$Internal_external <- factor(Internal_external2$Internal_external, levels = Internal_external2$Internal_external[order(Int_ext$Count, decreasing = TRUE)])

Int_ext <- Int_ext %>%
  mutate(Percentage = Count / sum(Count) * 100,
         Label = paste0(round(Percentage, 1), "%"))

pie1 <-ggplot(Internal_external2, aes(x="", y=Count, fill=Internal_external)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  #geom_text(aes(label = Label), 
   #         position = position_stack(vjust = 0.5), 
  #          size = 5) +
  theme_void()+
  labs(fill = "")+
  theme(legend.text = element_text(size = 16))+
  scale_fill_manual(values=c("#BF812D", "#C7EAE5", "#01665E"))

pie1

remove <- "Internally"
remove2 <- "Unknown"
Ext <- Internal_external %>% filter(Internal_external != remove)
Ext2 <- Ext %>% filter(Internal_external != remove2)

Ext3 <- Ext2 %>%
  group_by(Country) %>%
  summarise(Count = n()) %>%
  ungroup()

#Internal_external2$Internal_external <- factor(Internal_external2$Internal_external, levels = Internal_external2$Internal_external[order(Int_ext$Count, decreasing = TRUE)])

Ext3 <- Ext3 %>%
  mutate(Percentage = Count / sum(Count) * 100,
         Label = paste0(round(Percentage, 1), "%"))

pie2 <-ggplot(Ext3, aes(x="", y=Count, fill=Country)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = Label), 
           position = position_stack(vjust = 0.5), 
           size = 5) +
  theme_void()+
  labs(fill = "")+
  theme(legend.text = element_text(size = 16))+
  scale_fill_brewer(palette="BrBG")

pie2

###Years from sample collection to publication (dataset Turnaround_time.csv)

Turnaround_time$Study_type <- factor(Turnaround_time$'Study_ type')

dot_plot <- ggplot(Turnaround_time, aes(x = Study_type, y = Years, color=Study_type)) +
  geom_boxplot()+
  geom_point(size=4, alpha=0.4, position = position_jitter(width = 0.1)) +
  scale_color_manual(values =c('red', 'darkorange', 'gold2', 'springgreen3', 'deepskyblue3'))+
  labs(x = "",
       y = "Years from collection to publication") +
  theme_classic()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))+
  theme(legend.position = "none")+
  ylim(0,14)
  

dot_plot

####number of studies and public genomes per country per year (datasets genomic_public_years.csv and Study_country_year.csv)

genomic_public_years$Host3 <- factor(genomic_public_years$Host2, levels=c('Human', 'Animal', 'Vector', 'Unknown'))

genomes_studies <-ggplot()  + theme_minimal_hgrid()+
  geom_count(data=Study_country_year, aes(x=date, y=Country),fill='grey30', stroke=0.1,shape=15, col='black', alpha=0.2, size = 4)+
  geom_count(data=genomic_public_years, aes(x=Date, y=Country), fill='darkorange', stroke=0.4,shape=21, col='black', alpha=0.7)+
  xlab("")+ 
  ylab("")+
  theme(axis.text.y = element_text(color="black", size=9))+
  theme(axis.text.x = element_text(color="black", size=10))+
  scale_fill_continuous(name  = 'Genomic data')+
  scale_y_discrete(limits=rev)


genomes_studies
