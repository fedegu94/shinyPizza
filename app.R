library(corrplot)
library(dplyr)
library(tidyr)
library(plotly)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(sodium)
library(skimr)
library(ggplot2)
library(GGally)
library(kableExtra)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(pca3d)
library(shiny)
library(data.table)
library(imputeMissings)
library(tidyverse)
library(shinythemes)
library(reactable)

Pizza <- read_csv("Pizza.csv")
View(Pizza)
skim(Pizza)


reactable(Pizza)

filepath <-"Pizza.csv"

df <- fread(filepath, header ="auto", sep ="auto", dec =".", encoding ="unknown", data.table = FALSE, na.strings = "")



# Selezioniamo le variabili
colnames_selected = c(3,4,5,6,7,8,9)

# Data frame con le variabili selezionate
df <- df[ ,colnames_selected, drop=FALSE]


# Scaling
scale <- TRUE



# Definiamo numero di dimesioni da mantenere nei risultati
ncp <- min(ncol(df),5)


# Perform PCA
pca <- PCA(df, ncp=ncp, scale.unit = scale)



#prima osservazione
data_head_t <- kable(t(head(df, n=5)), digits=3, format="simple")

data_head_t



Pizza_ <- Pizza[-2]


plot_data <-Pizza_ %>% 
  gather("micronutritions", "value", 2:8) 
interactive_boxplot <- plot_ly(plot_data, x = ~brand, y = ~value, color = ~micronutritions, type = "box")


interactive_boxplot






# 
# il coefficiente di pearson
# Il coefficiente di correlazione r (di Pearson) non è altro che la covarianza standardizzata. Il coefficiente di Pearson è compreso tra -1 e +1, ed indica direttamente le dimensioni dell’effetto con le seguenti soglie convenzionali: r = ± 0,1: effetto di piccole dimensioni;
# r = ± 0,3: effetto di medie dimensioni; r = ± 0,5: effetto di grandi dimensioni.
# I valori anomali possono distorcere il valore di r.
# Presupposti per il test di significatività di r sono:
#   - la normalità delle variabili e
# - l’omogeneità della varianza.
# Se una variabile è distribuita normalmente, conosciamo la probabilità associata a ciascun valore. Tuttavia la distribuzione campionaria di r non è normale, poiché le sue code non tendono all’infinito ma si arrestano tra -1 e +1. Si possono normalizzare i suoi valori con la formula di Fisher zr = , dove zr ha un errore standard pari a SEzr = . Ora, la distribuzione campionaria di zr ha una deviazione standard pari a SEzr, quindi la variabile così definita: ha deviazione standard pari a 1, ed è normale. Basta cercare il valore di z osservato in una tabella della distribuzione normale standard per ottenere la significatività del risultato (p-value). Una strategia alternativa è quella di convertire r in:
#   che segue una distribuzione t con N-2 gradi di libertà.



# Pearson Correlations (Matrix)
P <- cor(df, use="complete.obs", method="pearson")

corrplot(P, is.corr=TRUE, method="number", type="lower", tl.col="#396e9f",
         tl.cex = 0.7, cl.cex = 0.7, number.cex = 0.8, cl.align.text="l")



# 
# 
# Correlazione di Kendall 
# Il coefficiente di correlazione τ di Kendall si basa sul conteggio del numero di coppie di punti “concordi” e “discordi”, dove una coppia di punti è detta concorde se la differenza tra i valori delle ascisse ha lo stesso segno della differenza tra i valori delle ordinate:
# • se c’è una relazione monotonica tra le due variabili, tutte le coppie sono concordi;
# • se le due variabili sono statisticamente indipendenti, il numero di coppie concordi è circa pari a quello delle coppie discordi.
# Dato il numero di coppie di punti possibili, ha un costo computazionale elevato per campioni molto grandi, mentre è preferibile al coefficiente di Spearman quando il campione è piccolo e contiene diversi valori con lo stesso rango. Inoltre è una stima migliore della correlazione nella popolazione rispetto al coefficiente di Spearman, sebbene quest’ultimo sia più diffuso. Il τ di Kendall non è basato sulla formula di Pearson:
# • ha un valore in genere 66–75% più piccolo di r o ρ;
# • ciò influenza la sua interpretazione in termini di dimensioni dell’effetto;
# • il quadrato di τ non esprime la proporzione di varianza condivisa e dunque non è una grandezza utile




# Kendall Correlations (Matrix)
K <- cor(df, use="complete.obs", method="kendall")

corrplot(K, is.corr=TRUE, method="number", type="lower", tl.col="#396e9f",
         tl.cex = 0.7, cl.cex = 0.7, number.cex = 0.8, cl.align.text="l")





# Eigenvalues and Proportion Explained

# Gli autovalori sono l'insieme speciale di valori scalari che è associato all'insieme 
# delle equazioni lineari più probabilmente nelle equazioni della matrice.
# Gli autovettori sono anche chiamati radici caratteristiche.
# È un vettore diverso da zero che può essere modificato al massimo dal suo fattore scalare dopo 
# l'applicazione di trasformazioni lineari.


eigentable <- get_eigenvalue(pca)
eigentable

eig <- kable(eigentable, digits=3, col.names = c("Eigenvalue","Variance Explained (%)",
                                                 "Cumulative Variance Explained (%)"), format="simple")
eig




scree_plot <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 80))


scree_plot







# Variables
## Loadings
df_svd <- scale(df, center=TRUE, scale=scale)
tmp <- svd.triplet(df_svd, ncp = ncp)
princomps <- tmp$V
colprincomp <- c("PC1", "PC2", "PC3", "PC4", "PC5")
rownames(princomps) <- colnames(df)
kable(princomps, digits=3, col.names=colprincomp[1:ncp], format="simple")



## Standardized Loadings
correlations <- pca$var$cor[,1:ncp]
kable(correlations, digits=3, format="simple")



## Squared Standardized Loadings
kable(pca$var$cos2, digits=3, format="simple")


## Standardized Loadings Plots (Dimensions 1-2)
biplot <- fviz_pca_var(pca, col.var = "cos2", axes = c(1,2),
                       gradient.cols = c("#ff9900", "#2fa42d", "#396e9f"),
                       title="", ggtheme=theme_minimal(base_size = 22))

biplot
summary_biplot <- head(var$cos2)



## Standardized Loadings Plots (More Dimensions)
for (i in 1:ncp){
  for (j in 1:ncp){
    nam <- paste0(paste0("plot",i),j)
    plot <- fviz_pca_var(pca, axes = c(i,j), title="",
                         ggtheme=theme_minimal(base_size = 8))
    assign(nam, plot)
  } }



# Plots arranged
gridExtra::grid.arrange(grobs = list(plot12,plot13,plot23,plot14,plot24,plot34,
                                     plot15,plot25,plot35,plot45),
                        nrow=4, ncol=3, widths = unit(c(4, 4, 4), "cm"),
                        heights = unit(c(4, 4, 4, 4), "cm"),
                        padding = unit(0.3, "line"))


# cos 2 rappresenta la qualità delle variabili 
#
#Standardized Loadings Plot (Matrix)
col_statsomat <- colorRampPalette(c("#ff9900", "#2fa42d", "#396e9f"))
corrplot(pca$var$cor, is.corr=FALSE, tl.col="#396e9f", col=col_statsomat(10),
         tl.cex = 0.5, cl.cex = 0.5, cl.align.text="l")




#Squared Standardized Loadings Plot (Matrix)
corrplot(pca$var$cos2, is.corr=FALSE, method="color", tl.col="#396e9f",
         tl.cex = 0.5, cl.cex = 0.5, cl.align.text="l")


# Observations
## Scores (Head)
kable(head(pca$ind$coord), digits=3, format="simple")


## Score Plot (Dimensions 1-2)
fviz_pca_ind(pca, axes = c(1,2), title="", col.ind = "#396e9f",
             ggtheme=theme_minimal(base_size = 11))



## Score Plots (More Dimensions)
for (i in 1:ncp){
  for (j in 1:ncp){
    nam <- paste0(paste0("plot",i),j)
    plot <- fviz_pca_ind(pca, axes = c(i,j), col.ind = "#396e9f",
                         title="", ggtheme=theme_minimal(base_size = 8))
    
    assign(nam, plot)
  } 
}



# Plots arranged
gridExtra::grid.arrange(grobs = list(plot12,plot13,plot23,plot14,plot24,plot34,
                                     plot15,plot25,plot35,plot45),
                        nrow=4, ncol=3, widths = unit(c(4, 4, 4), "cm"),
                        heights = unit(c(4, 4, 4, 4), "cm"),
                        padding = unit(0.3, "line"))




## Biplot Dimensions 1-2
fviz_pca_biplot(pca, col.var = "#ff9900", col.ind = "#396e9f", title="")

federico <- HCPC(pca,graph = F)




fviz_cluster(federico,
             repel = TRUE,
             show.clust.cent = TRUE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Factor map"
)


#con un istogramma vogliamo ceracre di capire se particolari
#aziende producono le loro pizze con rapporti nutrizionali simili 
#o se ci sono differenze significative:
Pizza_ <- Pizza[-2]





plot_data <-Pizza_ %>% 
  gather("micronutritions", "value", 2:8) 

plot_ly(plot_data, x = ~brand, y = ~value, color = ~micronutritions, type = "box") 



#guardando gli istogrammi, possiamo vedere che ciascuna delle pizze 
#ha macronutrienti variabili. La maggiore somiglianza tra le pizze 
#può essere vista nel grafico per il grasso variabile,
#mentre la maggiore varianza riguarda la variabile mois, 
#che è la quantità di acqua per 100 grammi di campione.

#-------correlation----------
cor_pizza <- cor(Pizza_[-1])
ggcorr(cor_pizza, label = T, label_round = 2)

#la maggior parte delle variabili ha corretazione predominante 
#positiva.
#------implemention of Pca ------
PCA <- prcomp(Pizza_[,-1], center = TRUE, scale = TRUE)
summary(PCA)
head(summary(PCA))
eig.val <- get_eigenvalue(PCA)
eig.val
#la proporzione di varianza è spiegata nella
#seconda colonna,dove abbiamo il 59% della varianza 
#spiegata dalla prima colonna

fviz_screeplot(PCA, addlabels = TRUE)

#-------graph of variable-----
#Questa funzione fornisce un elenco di matrici 
#contenenti tutti i risultati per le variabili 
#attive (coordinate, correlazione tra variabili ed assi, coseno quadrato e contributi)
var <- get_pca_var(PCA)
var


# Coordinates
head(var$coord)
# Cos2: quality on the factore map
summary_pca <- head(var$cos2)
summary_pca
# Contributions to the principal components
head(var$contrib)

#La correlazione tra una variabile e un componente 
#principale (PC) viene utilizzata come coordinate della 
#variabile sul PC. La rappresentazione delle variabili 
#differisce dal grafico delle osservazioni: le osservazioni 
#sono rappresentate dalle loro proiezioni, ma le variabili 
#sono rappresentate dalle loro correlazioni
# Coordinates of variables

fviz_pca_var(PCA, col.var = "black")

#--------quality of representation--
#La qualità della rappresentazione delle variabili sulla mappa 
#dei fattori è chiamata cos2


corrplot(var$cos2)




# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(PCA, choice = "var", axes = 1:2)
#Un cos2 alto indica una buona rappresentazione della
#variabile sulla componente principale. In questo caso la variabile
#è posizionata vicino alla circonferenza del cerchio di correlazione
# Color by cos2 values: quality on the factor map
fviz_pca_var(PCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
#----Contributions of variables to PCA--------

summary_contrib <- head(var)
summary_contrib

corrplot(var$contrib, is.corr=FALSE) 
# Contributions of variables to PC1
hist_contrib1 <- fviz_contrib(PCA, choice = "var", axes = 1, top = 10)

hist_contrib1

# Contributions of variables to PC2
hist_contrib2 <- fviz_contrib(PCA, choice = "var", axes = 2, top = 10)

hist_contrib2




#----dimension description---
library(factoextra)

individuals_pca_graf <- fviz_pca_ind(
  PCA, col.ind = "cos2", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  legend.title = "Contribution"
)
individuals_pca_graf
####




#####

fviz_pca_ind(PCA, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE 
)

fviz_pca_ind(PCA,
             geom.ind = "point", 
             col.ind = Pizza$brand, 
             addEllipses = TRUE, 
             legend.title = "Groups"
)





PCA_biplot <- fviz_pca_biplot(PCA, 
                              # Individuals
                              geom.ind = "point",
                              fill.ind = Pizza$brand, col.ind = "black",
                              pointshape = 21, pointsize = 2,
                              palette = "jco",
                              addEllipses = TRUE,
                              # Variables
                              alpha.var ="contrib", col.var = "contrib",
                              gradient.cols = "RdYlBu",
                              
                              legend.title = list(fill = "brand", color = "Contrib",
                                                  alpha = "Contrib")
)





PCA_biplot




summary(pca)





X <- subset(Pizza, select = -c(brand))

prin_comp <- prcomp(X, rank. = 3)

components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, Pizza$brand)

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Pizza 3d scatter'

fig_3d <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Pizza$brand, colors = c('#636EFA','#EF553B','#00CC96','#7FFF00','#FF7F50','#DC143C','#00008B','#FF1493','#1E90FF','#FFD700') ) %>%
  add_markers(size = 12)


fig_3d <- fig_3d %>%
  layout(
    title = tit,
    scene = list(bgcolor = "#e5ecf6")
  )

fig_3d


# shiny -------------------------------------------------------------------
#https://data.world/sdhilip/pizza-datasets   link dataset

# A chi piace la pizza? Voglio dire, ci sono così tante cose da apprezzare.
# diamo un'occhiata più da vicino! Il dataset Pizza.
# contiene misure che catturano il tipo di cose che rendono gustosa una pizza.
# Possiamo determinare quale marca di pizza ha migliori nutrienti  e spiegare perché? Le variabili nel set di dati sono:
# brand -- Brand della Pizza (class label)
# id -- Campione analizzato
# mois -- Quantità d'Acqua per 100 grammi nel campione 
# prot -- Quantità Proteine per 100 grammi nel campione 
# fat -- Quantità Grasso per 100 grammi nel campione 
# ash -- Quantità Cenere per 100 grammi nel campione 
# sodium -- Quantità Sodio per 100 grammi nel campione 
# carb -- Quantità Carboidrati per 100 grammi nel campione 
# cal -- Quantità Calorie per 100 grammi nel campione 

header <- dashboardHeader( title = "Pizza",
                           tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/public-profile/settings?trk=d_flagship3_profile_self_view_public_profile" ,icon("linkedin"), "My Profile", target="_blank")),
                           tags$li(class="dropdown",tags$a(href="https://github.com/fedegu94/shinyPizza", icon("github"), "Source Code", target="_blank"))
)

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))




ui<-dashboardPage(header, sidebar, body, skin = "blue") %>% 
  fluidPage(
    tags$script(src = "https://kit.fontawesome.com/8e1af2fd65.js"),
    tags$div(
    )
  )





server <- function(input, output, session) {
  
  output$sidebarpanel <- renderUI({
    sidebarMenu(
      menuItem("Pizza", tabName = "dashboard", icon = icon("fa-duotone fa-pizza-slice")),
      menuItem("Summary", tabName = "secondo", icon = icon("th")),
      menuItem("BoxPlot", tabName = "terzo", icon = icon("th")),
      menuItem("CorrPlot", tabName = "quarto", icon = icon("th")),
      
      menuItem("PCA",
               tabName = "quinto", 
               icon = icon("th"),
               menuSubItem("Scree Plot", tabName = "quinto1"),
               menuSubItem("Biplot", tabName = "quinto2")
               # menuSubItem("Sub Menu Item 3", tabName = "quinto3"),
               # menuSubItem("Sub Menu Item 4", tabName = "quinto4")
               
      ),
      
      menuItem("Conclusioni", tabName = "sesto", icon = icon("th"))
      
      
    )
  })
  
  output$body <- renderUI({
    tabItems(
      # First tab
      tabItem(tabName ="dashboard", class = "active",
              titlePanel(title = span(img(src = "https://www.richs.co.za/wp-content/uploads/2022/08/Pizza-Gif-final.gif", height = 380))),
              tags$h1("Analisi nutrizionale delle Pizze"),
              tags$br(),
              tags$h4("A chi piace la pizza? Voglio dire, ci sono così tante cose da apprezzare.
diamo un'occhiata più da vicino! Il dataset Pizza.
contiene misure che catturano il tipo di cose che rendono gustosa una pizza.
Possiamo determinare quale marca di pizza ha migliori nutrienti  e spiegare perché? Le variabili nel set di dati sono:"),
              tags$br(),
              tags$h5("id -- Campione analizzato"),
              tags$h5("mois -- Quantità d'Acqua per 100 grammi nel campione"),
              tags$h5("prot -- Quantità Proteine per 100 grammi nel campione "),
              tags$h5("fat -- Quantità Grasso per 100 grammi nel campione "),
              tags$h5("ash -- Quantità Cenere per 100 grammi nel campione "),
              tags$h5("sodium -- Quantità Sodio per 100 grammi nel campione "),
              tags$h5("carb -- Quantità Carboidrati per 100 grammi nel campione "),
              tags$h5("cal -- Quantità Calorie per 100 grammi nel campione" ),
              tags$a(href="https://data.world/sdhilip/pizza-datasets", 
                     "Clicca qui per andare al link del dataset ")
              
              
              
              # box(width = 12, plotOutput('results'))
      ),
      
      # secondo tab
      tabItem(tabName ="secondo", class = "active",
              fluidPage(
                box(width = 12, reactableOutput('results2')),
                
              )),
      
      
      # terzo tab = boxPlot
      tabItem(tabName ="terzo", class = "active",
              fluidRow(
                box(width = 12, plotlyOutput('results3')),
                tags$h1("Box Plot Interattivo"),
                tags$br(), # line break
                tags$h3("con un boxblot vogliamo cercare di capire se particolari aziende producono le loro pizze con rapporti nutrizionali simili o se ci sono differenze significative: guardando i boxplots, possiamo vedere che ciascuna delle pizze ha macronutrienti variabili. La maggiore somiglianza tra le pizze può essere vista nel grafico per la variabile del grasso, mentre la maggiore varianza riguarda la variabile mois, che è la quantità di acqua per 100 grammi di campione."),
                
                
              )),
      
      
      # quarto tab
      tabItem(tabName = "quarto",
              tabBox(id="t5", width = 12,
                     tabPanel("Pearson", plotOutput("Pearson_c"), icon = icon("signal"),width = 10,
                              tags$h1("Correlazione di Pearson"),
                              tags$br(),
                              tags$h4("Il coefficiente di pearson Il coefficiente di correlazione r (di Pearson) non è altro che la covarianza standardizzata. Il coefficiente di Pearson è compreso tra -1 e +1, ed indica direttamente le dimensioni dell’effetto con le seguenti soglie convenzionali: r = ± 0,1: effetto di piccole dimensioni; r = ± 0,3: effetto di medie dimensioni; r = ± 0,5: effetto di grandi dimensioni. I valori anomali possono distorcere il valore di r. Presupposti per il test di significatività di r sono: - la normalità delle variabili e - l’omogeneità della varianza. Se una variabile è distribuita normalmente, conosciamo la probabilità associata a ciascun valore. Tuttavia la distribuzione campionaria di r non è normale, poiché le sue code non tendono all’infinito ma si arrestano tra -1 e +1. Si possono normalizzare i suoi valori con la formula di Fisher zr = , dove zr ha un errore standard pari a SEzr = . Ora, la distribuzione campionaria di zr ha una deviazione standard pari a SEzr, quindi la variabile così definita: ha deviazione standard pari a 1, ed è normale. Basta cercare il valore di z osservato in una tabella della distribuzione normale standard per ottenere la significatività del risultato (p-value). Una strategia alternativa è quella di convertire r in: che segue una distribuzione t con N-2 gradi di libertà.")
                     ),
                     tabPanel("Kendall", plotOutput("Kendall_c"), icon = icon("signal"),width = 10,
                              tags$h1("Correlazione di Kendall"),
                              tags$br(),
                              tags$h4("Il coefficiente di correlazione τ di Kendall si basa sul conteggio del numero di coppie di punti “concordi” e “discordi”, dove una coppia di punti è detta concorde se la differenza tra i valori delle ascisse ha lo stesso segno della differenza tra i valori delle ordinate: • se c’è una relazione monotonica tra le due variabili, tutte le coppie sono concordi; • se le due variabili sono statisticamente indipendenti, il numero di coppie concordi è circa pari a quello delle coppie discordi.Dato il numero di coppie di punti possibili, ha un costo computazionale elevato per campioni molto grandi, mentre è preferibile al coefficiente di Spearman quando il campione è piccolo e contiene diversi valori con lo stesso rango. Inoltre è una stima migliore della correlazione nella popolazione rispetto al coefficiente di Spearman, sebbene quest’ultimo sia più diffuso. Il τ di Kendall non è basato sulla formula di Pearson: • ha un valore in genere 66–75% più piccolo di r o ρ; • ciò influenza la sua interpretazione in termini di dimensioni dell’effetto; • il quadrato di τ non esprime la proporzione di varianza condivisa e dunque non è una grandezza utile")
                     )
              )),
      
      # quinto tab
      tabItem(tabName = "quinto1",
              fluidRow(
                box(width = 12, plotOutput('results5'),
                    verbatimTextOutput("stats"),
                    tags$h1("Scree Plot"),
                    tags$br(),
                    tags$h4("Un metodo per determinare il numero di componenti principali è lo scree plot, che è il grafico di autovalori ordinati dal più grande al più piccolo.
                            Nella nostra analisi, risulta che oltre il 92% della varianza osservata nel set di dati può essere spiegata con solo due componenti ( dim1 , dim2)."),
                )
              )),
      
      
      
      
      tabItem(tabName = "quinto2",
              tabBox(id="t6", width = 12,
                     tabPanel("Variable PCA", plotOutput("individuals"), icon = icon("signal"),width = 10,
                              fluidRow(
                                verbatimTextOutput("summary_cos2")
                              ),
                              
                              tags$h1("Variable PCA"),
                              tags$br(),
                              tags$h4("La dimensione 1 oppone individui (a destra del grafico, caratterizzato da una coordinata fortemente positiva sull'asse) agli individui caratterizzati da una coordinata fortemente negativa sull'asse (a sinistra del grafico). 
                              Possiamo notare 3 gruppi, i primi 2 sono caratterizzati da una coordinata positiva sull'asse dove abbiamo valori elevati per mois, prot, ash, sodium, fat, e valori bassi per cal e carb.
                              Il gruppo 3 è caratterizzato da una coordinata negativa sull'asse con valori elevati per carb e valori basi per prot, ash, fat, mois, sodium
                              
                                      "),
                              tags$h4("La dimensione 2 presenta nei primi 2 gruppi valori elevati per carb, sodium, ash, cal, fat, prot, e valori bassi per:  carb, mois,prot, ash, fat, sodium.
                                    Il gruppo 3 caratterizzato da una coordinata negativa sull'asse ha valori elevati per mois, prot, ash e valori bassi per cal e carb"),
                              tags$h4("In fine possiamo notare che lavariabile mois nella dimensione 2 è altamente correlata e questa variabile potrebbe riassumere questa dimensione."),
                              tags$h5("Il cos2 che vedimao nella legenda del grafico rappresenta la qualità delle variabili.
                                     Le variabili che rappresentano un'alta qualità sono: carb, cal, fat, ash")
                     ),
                     tabPanel("Grafico Individui PCA", plotOutput("individuals_pca"), icon = icon("signal"),width = 10,
                              tags$h1("Grafico individui PCA"),
                              tags$br(),
                              tags$h4("Questo grafico ci mostra quali individui stanno contribuendo di pi, possiamo dire che ci sono due gruppi di individui	 in particolare ( in alto a destra e a sinistra) che hanno un’alta contribuzione")
                     ),
                     tabPanel("Biplot", plotOutput("Biplot_u"), icon = icon("signal"),width = 10,
                              tags$h1("Biplot"),
                              tags$br(),
                              tags$h4("In questo biplot ' clusterizzato' possiamo confermare quanto detto precedentemente, ma possiamo osservare la tipologia dei brand presenti nei nostri dati")
                     ),
                     # tabPanel("Biplot comparisson", plotOutput("Biplot_c"), icon = icon("signal"),width = 10,
                     #          tags$h1("Biplot comparisson"),
                     #          tags$br(),
                     #          tags$h4("metti ")
                     # ),
                     tabPanel("Correlazione Dimensioni", plotOutput("correlazione_d"), icon = icon("signal"),width = 10,
                              fluidRow(
                                verbatimTextOutput("corr_dim")
                              ),
                              tags$h1("Correlazione Dimensioni varibili"),
                              tags$br(),
                              tags$h4("Possiamo vedere quali variabili hanno correlazione negativa e positiva nelle varie dimensioni.
Nella prima dimensione abbiamo un’alta correlazione positiva per la variabile ash, fat, sodium, prot e una correlazione negativa per la variabile carb.
Nella dimensione 2 abbiamo un’alta correlazione negativa per la variabile mais, e un’alta correlazione positiva per la variabile cal"),
                     ),
                     tabPanel("Istogramma Dim 1", plotOutput("Hist_d"), icon = icon("signal"),width = 10,
                              fluidRow(
                                verbatimTextOutput("Hist_dimension")
                              ),
                              tags$h1("Istogramma Dimensioni varibili"),
                              tags$br(),
                              tags$h4("Possiamo vedere quali variabili contribuiscono maggiormente nella dimensione 1, in particolare modo la variabile che contribuisce di più è la variabile ash")
                     ),
                     tabPanel("Istogramma Dim 2", plotOutput("Hist_d2"), icon = icon("signal"),width = 10,
                              fluidRow(
                                verbatimTextOutput("Hist_dimension2")
                              ),
                              tags$h1("Istogramma Dimensioni varibili"),
                              tags$br(),
                              tags$h4("Possiamo vedere quali variabili contribuiscono maggiormente nella dimensione 2, in particolare modo la variabile che contribuisce di più è la variabile mois")
                     )
                     
              )),
      
      tabItem(tabName ="sesto", class = "active",
              fluidRow(
                box(width = 12, plotlyOutput('finally')),
                tags$h1("Scatterplot 3d"),
                tags$br(), 
                tags$h3("Abbiamo visto nei grafici precedenti che le variabili prot e carb sono negativamente correlate.
Quindi se la quantità di carboidrati di una pizza è molto bassa rispetto alla media, allora la quantità di proteine sarà molto alta per la stessa pizza.

Una simile osservazione può essere fatta per variabili correlate positivamente come la quantità di grasso
e la quantità di sodio, tranne che in questo caso le variabili variano nella stessa direzione."),
                
                
              ))
      
      
    )
    
    
    
  })
  

  
  output$results2 <- renderReactable({
    reactable(Pizza)
  })
  

  
  
  output$results3 <-  renderPlotly({ # terzo tab
    interactive_boxplot
    
  })
  
  
  output$Pearson_c <- renderPlot({ #4 subTab1 
    corrplot(P, is.corr=TRUE, method="number", type="lower", tl.col="#396e9f",
             tl.cex = 0.7, cl.cex = 0.7, number.cex = 0.8, cl.align.text="l")
    
  })
  
  
  output$Kendall_c <- renderPlot({#4 subTab2
    corrplot(K, is.corr=TRUE, method="number", type="lower", tl.col="#396e9f",
             tl.cex = 0.7, cl.cex = 0.7, number.cex = 0.8, cl.align.text="l")
  })
  
  
  output$results5 <-  renderPlot({
    scree_plot
  })
  
  output$stats <- renderPrint({
    eig
  })
  
  output$individuals <- renderPlot({
    biplot
  })
  output$summary_cos2 <- renderPrint({
    summary_pca
    
  })
  
  output$individuals_pca <- renderPlot({
    individuals_pca_graf
  })
  
  
  output$Biplot_u <-  renderPlot({
    PCA_biplot
    
    
  })
  
  output$correlazione_d <-  renderPlot({
    col_statsomat <- colorRampPalette(c("#ff9900", "#2fa42d", "#396e9f"))
    corrplot(pca$var$cor, is.corr=FALSE, tl.col="#396e9f", col=col_statsomat(12),
             tl.cex = 0.7, cl.cex = 0.7, cl.align.text="l")
    
  })
  
  output$corr_dim <- renderPrint({
    head(var$cor)
  })
  
  output$Hist_d <- renderPlot({
    hist_contrib1
  })
  
  output$Hist_d2 <- renderPlot({
    hist_contrib2
  })
  output$finally <- renderPlotly({
    fig_3d
  })
  
  
  
}

#runApp(list(ui = ui, server = server), launch.browser = TRUE)




shinyApp(ui = ui, server = server)
