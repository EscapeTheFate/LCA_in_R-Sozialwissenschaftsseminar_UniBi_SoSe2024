
# Lade bzw. Installiere Pakete --------------------------------------------
library(poLCA)
library(dplyr)
library(tidyverse)


# Laden eines Beispieldatensatzes (durch poLCA - Dayton (1998, 33 and 85))
data(cheating) 
attach(cheating)

# Information zum Datensatz:
# Dichotomous responses by 319 undergraduates to questions about cheating be-
# havior. Also each student’s GPA, which is useful as a concomitant variable
# [Dayton CM (1998). Latent Class Scaling Analysis. Sage Publications, Thousand Oaks, CA.]

# Übersicht des Datensatzes:
names(cheating) # Manifeste Variablen: LIEEXAM, LIEPAPER, FRAUD, COPYEXAM
                # Nur dichotome manifeste Variablen über Betrugsversuche
                # Erklärende Hilfsvariablen: GPA (Level: 1-5)

head(cheating, 10) # Erste 10 Observationen
summary(cheating) # Detailierte Zusammenfassung
str(cheating) # Kombiniert head() & typeof()

# Erinnerung: 
# - Manifeste variablen müssen lediglich integer-Werte und nicht zwingend
#   vom Typ integer sein, damit die poLCA()-Funktion keinen Fehler hervorruft
# - Die Ausprägungen beginnen vom Wert 1 und sind aufsteigend
# - Die Ausprägungen dürfen nicht negativ sein und sind nur ganze Zahlen (kategoriell)
# - Für Kovariaten gibt es keine Vorgaben

# Definiere 'Formel' für simples latente Klassenmodell --------------------
f <- cbind(LIEEXAM, LIEPAPER, FRAUD, COPYEXAM) ~ 1 # Basic latent class Analysis


# Schätzen eines latenten Klassenmodells ----------------------------------
?poLCA()

mod1 <- poLCA(formula = f, data = cheating, nclass = 1, maxiter = 3000, 
             graphs = F, na.rm = T, probs.start = NULL, nrep = 1)
mod2 <- poLCA(formula = f, data = cheating, nclass = 2, maxiter = 3000, 
              graphs = T, nrep = 10, na.rm = F)
mod3 <- poLCA(formula = f, data = cheating, nclass = 3, maxiter = 3000, 
              graphs = T, nrep = 10)

# Standardwert: nclass = 2; nclass = 1 für log-lin. unabh. Modell (Werte = nimmt 
# lediglich wahre Anteilwerte des vorliegenden Datensatzes; Siehe: mod1$numiter)

# Je mehr Klassen bzw. zusätzliche erklärende Variablen, desto kleiner die
# Degrees of Freedom. Falls diese negativ sind => Modell nicht eindeutig schätzbar
mod4 <- poLCA(formula = f, data = cheating, nclass = 4) # Error: Negative DoF


# Modellauswahl -----------------------------------------------------------

# Funktion, die alle vorgegebenen Prüfgrößen ausliest (+ Individuell erweiterbar!)
pruefgroeßen <- function(mod){
  round(data.frame(AIC = mod$aic,
             BIC = mod$bic, # BIC nimmt allg. Obs.anzahl (mod$N) anstelle von vollständigen (mod$Nobs)
             AIC3 = -2*mod$llik + 3*mod$npar,
             Gsq = mod$Gsq,
             Chisq = mod$Chisq,
             DoF = mod$resid.df,
             crit.val95 = qchisq(0.95, df = mod$resid.df),
             p.Gsq.val95 = 1-pchisq(mod$Gsq, df = mod$resid.df),
             p.Chisq.val95 = 1-pchisq(mod$Chisq, df = mod$resid.df),
             Entropy = poLCA.entropy(mod),
             row.names = deparse(substitute(mod)))
        , 3) # <--- Rundungszahl
}
pruefgroeßen(mod2) # Testen am zweiten Modell

# Übersicht aller Größen für jedes Modell:
rbind(pruefgroeßen(mod1), pruefgroeßen(mod2), pruefgroeßen(mod3)) # Wir wählen 
# Modell 2 nach AIC and BIC (Chi-Quadrat und G-Quadrat sind nicht empfohlen, weil
# mehr als 10-20% der einzelnen Zellen weniger als 5 Observationen haben;
# Siehe: 'mod2$predcell' zeigt, dass dies bei 9 von 16 der Fall ist)

# AIC per Hand (identisch zum Modelloutput):
-2*mod2$llik + 2* mod2$npar

# Chi-Quadrat Tests:
mod2$Chisq
qchisq(0.95, df = mod2$resid.df) # Kritischer Wert bei 12.59159
1-pchisq(mod2$Chisq, df = mod2$resid.df) # < 0.05? => Nein, d.h. nicht signifikant
                                         # => Erw. Werte = geschätzte Werte


# Hilfreiche Output-Daten -------------------------------------------------

### Generelles - Übersicht:
mod2$N # Anzahl an benutzten Observationen im Modell
mod2$Nobs # Anzahl an benutzten vollständig beobachteten Observationen im Modell
mod2$npar # Anzahl geschätzter Parameter
mod2$numiter # Anzahl an Iterationen, die der Algorithmus durchlaufen ist 
mod2$probs.start # Übersicht der Startwerte
mod2$llik # Likelihood


### Klassenhäufigkeiten:
mod2$P # (tatsächliche) latente Klassenanteile nach Modell (nicht erwartete Anzahl nach Modell)
round(mod2$P * mod2$N,0) # Personen pro Klasse
mod2$predcell # Klassentabelle - Modell-Häufigkeiten vs. Erwartete Häufigkeiten
mod2$predclass # Klassentabelle - det. Klassenzugehörigkeiten
poLCA.predcell(lc = mod2, y = c(1,2,1,2)) # erwartete Anteil an Observationen
# mit dieser Ausprägung in manifesten Variablen (mal mod2$N für erwartete Anzahl)
poLCA.predcell(lc = mod2, y = rbind(c(1,1,1,1), c(1,1,1,2))) # mehrere gleichzeitig


### Für weitere Analysen mit Wahrscheinlichkeiten:
# Bedingte Wahrscheinlichkeitsübersicht:
mod2$probs # Liste von geschätzten klassen-bed.Ausprägungsw.keiten pi^(hat)_jrk
mod2$probs$LIEEXAM # Explizites Beispiel
mod2$probs.se # Standardfehler von geschätzten klassen-bed. Ausprägungsw.keiten
poLCA.table(formula = LIEEXAM ~ 1, condition = list(LIEPAPER = 2, # Häufigkeiten
                                                    FRAUD = 1,
                                                    COPYEXAM = 2),
                                                    lc = mod2)
poLCA.table(formula = LIEEXAM ~ LIEPAPER, condition = list(), lc = mod2) # Häufigkeiten


### Wahrscheinlichkeiten für jede beobachtete Observation im Datensatz:
mod2$posterior # (N x R)-matrix von (posterior) Klassenzugehörigkeitsw.keit
cheating <- cbind(cheating, mod2$posterior, mod2$predclass)
names(cheating)[6:8] <- c("C1.prob", "C2.prob", "Det.Class")
head(cheating)
# Analog zu bevor, aber für eigene Werte (äquivalent zu predict()-Funktion)
?poLCA.posterior() 
poLCA.posterior(mod2, y = c(1,1,1,1))


### Histogramme
ind <- which(cheating$Det.Class == 1)
par(mfrow = c(2,1)) # 2 Grafiken übereinander
# Für erste Posteriori Wahrscheinlichkeit (Alle Obs. & Klassen-bed.)
hist(x = cheating$C1.prob, breaks = seq(from = 0, to = 1, by = 0.1))
hist(x = cheating$C1.prob[ind], breaks = seq(from = 0, to = 1, by = 0.1))
# Für zweite Posteriori Wahrscheinlichkeit (Alle Obs. & Klassen-bed.)
hist(x = cheating$C2.prob, breaks = seq(from = 0, to = 1, by = 0.1))
hist(x = cheating$C2.prob[-ind], breaks = seq(from = 0, to = 1, by = 0.1))
par(mfrow = c(1,1)) # Grafiksetting zurücksetzen

# Klassenmittelwerte berechnen --------------------------------------------
calc.class.means <- function(mod){
  
  df <- cbind(mod$y, class = mod$predclass) # Ausprägungswerte für alle Obs.
  dim = dim(mod$y)[2] # Anzahl an definierten manifesten Variablen
  K = length(mod$P) # Klassenanzahl
  output <- data.frame(mod$y[1,]) # Mittelwerttabelle
  
  for(k in 1:K){ 
    index <- which(df["class"] == k) # Alle Observationen in Klasse k
    output[k,] <- apply(df[index,1:dim], 2, mean, na.rm = T) # Mittelwertberechnung über Klasse k
    row.names(output)[k] <- paste("C", k, sep = '') 
  }
  round(output,3)
}
calc.class.means(mod2)


# Advanced latente Klassenregression --------------------------------------
g <- cbind(LIEEXAM, LIEPAPER, FRAUD, COPYEXAM) ~ GPA
mod2.reg <- poLCA(formula = g, data = cheating, nclass = 2, maxiter = 5000, 
                  graphs = T, nrep = 20)
rbind(pruefgroeßen(mod2), pruefgroeßen(mod2.reg))


# Analyse der Überlappung -------------------------------------------------

# Überlappungsanteil entscheidend für die Konvergenz und Stabilität der Ergeb-
# nisse der latenten Profilanalyse

# Einfache (Grobab-)Schätzung des Überlappungsanteils:
# Bei K = 2 Klassen aber nicht sinnvoll, weil Grenzen bei 1/K = 50% gesetzt werden
overlap.rough <- function(mod){
  n = mod$N
  K_div = K = length(mod$P)
  if(K == 2){ # Spezieller Fall für K = 2, da Dummy-codierung 50% als Grenze hätte
    K_div = 2.5 # Hier: Grenze bei 1 / K_div = 1 / 2.5 = 0.4 = 40%
  }
  df <- as.data.frame(mod$posterior)
  for (i in 1:K){
    df[,K+i] <- ifelse(df[,i] > 1 / K_div, 1, 0)
  }
  df <- df[,(K+1):(K+K)]
  df <- df %>% group_by_all() %>% summarise(n = n()) %>% arrange(n)
  # names(df)[1:K] <- ""
  for (i in 1:K){
  names(df)[i] <- paste("C",i, sep = '') 
  }
  
  print(as.data.frame(df), row.names = FALSE)
} 
overlap.dunn <- function(mod){
  n = mod$N
  K = length(mod$P)
  foo <- c(rep.int(0, n))
  for (i in 1:n){
    foo[i] <- sum(mod2$posterior[i,]^2)
  }
  return(((1 / n) * sum(foo) - 1 / K) / (1 - 1 / K))
}
overlap.backer <- function(mod){
  n = mod$N
  K = length(mod$P)
  foo <- 0
  for(g in 1:n){
    for(k in 1:K){
      if(k != K){
        for(m in (1:K)[-k]){
          foo <- foo + min(mod3$posterior[g,k], mod3$posterior[g,m])
        }
      }
    }
  }
  return(1 - (1 / n)*(2 / (K - 1)) * foo)
}

overlap.rough(mod2)  # Fast keine Überlappung im Modell
overlap.dunn(mod2)   # Nahe 1 = Fast keine Überlappung
overlap.backer(mod3) # Nahe 1 = Fast keine Überlappung


# Durchschnittliche Posterior Klassenwahrscheinlichkeiten
round(aggregate(x = mod2$posterior, by = list(mod2$predclass), FUN = "mean"), 2
)

# Posterior Wahrscheinlichkeiten summiert + Ratio:
posterior.sum.table <- function(mod){
  df <- as.data.frame(cbind(mod$posterior, mod$predclass))
  foo <- c(rep.int(0, length(mod$P)))
  
  for (k in 1:length(mod$P)){
    index <- which(df[,length(mod$P)+1] == k) # lese det. Zugehörigkeiten aus
    foo[k] <- sum(df[index,k])
  }
  
  # df <- round(data.frame(post.prob.sum = apply(mod$posterior, 2, sum),
  #            N = as.vector(table(mod$predclass)),
  #            Ratio = apply(mod$posterior, 2, sum)/as.vector(table(mod$predclass)),
  #            'I' = c(rep.int(0, length(mod$P))),
  #            cond.post.prob.sum = foo,
  #            N = as.vector(table(mod$predclass)),
  #            cond.Ratio = foo/as.vector(table(mod$predclass)))
  #       , 3)
  # df[,4] <- c(rep("I", times = length(mod$P)))
  # df
  df1 <- round(data.frame(post.prob.sum = apply(mod$posterior, 2, sum),
                         N = as.vector(table(mod$predclass)),
                         Ratio = apply(mod$posterior, 2, sum)/as.vector(table(mod$predclass)))
               , 3)
                         
  df2 <- round(data.frame(cond.post.prob.sum = foo,
                         N = as.vector(table(mod$predclass)),
                         avg.err = (foo-as.vector(table(mod$predclass)))/as.vector(table(mod$predclass)),
                         cond.Ratio = foo/as.vector(table(mod$predclass)))
               , 3)
              
  print(df1)
  print(df2)
}
posterior.sum.table(mod3) # cond = berechnet bedingt auf Klassenzugehörigkeit K


# Berechne klassischer Entropie Index & relativer Entropie Index:
?poLCA.entropy()
poLCA.entropy(mod2) # Wert von 0-K; Kleine Zahl = Gut getrennte Daten

relative.entropy <- function(mod) {
  1 - ((sum(-mod$posterior*log(mod$posterior)))/(nrow(mod$posterior)*log(ncol(mod$posterior))))
}
relative.entropy(mod2)


# Überprüfung der Annahme der lokalen Unabhängigkeit ----------------------
# Unfinished - Kapitel 14.5 - Bacher, J.; Pöge, A.; Wenzig, K. (2010): Cluster-
# analyse. Anwendungsorientierte Einführung in Klassifikationsverfahren & 
# alternativ Fahrmeir und Hamerle 1984, S. 74-75 für Details
calc.poLCA.cov <- function(mod, class){
  K <- length(mod$P) # Klassenanzahl
  dim <- ncol(mod$y) # Anzahl manifeste Variablen
  df <- cbind(mod$y, posterior = mod$posterior[,class], Det.Class = mod$predclass)
  df <- df %>% filter(Det.Class == class) %>% na.omit() # Nur Obs. mit k = class
  n_class <- sum(df$posterior) # gewichtete (nicht-NA) Obs.anzahl
  cov_class <- cov.wt(df[,1:dim], wt = df$posterior)$cov # gew. COV
  LQ <- -n_class * log(det(cov_class)) # LQ-Test Wert?
  LQ
}
calc.poLCA.cov(mod2, class = 2)
detach(cheating)





# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# tidyLCA Package ---------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
library(tidyLPA)
library(dplyr)


# Information zum Datensatz:
# Student questionnaire data with four continuous variables from the 2015 PISA 
# for students in the United States [Source: http://www.oecd.org/pisa/data/]
?pisaUSA15
data <- pisaUSA15[1:100,] %>% select(-instrumental_mot) %>% na.omit()

# Übersicht des Datensatzes:
names(data)
head(data, 5)
summary(data)
str(data)

# Schätzung eines latent-profile Modells ----------------------------------

# Erinnerung:
# Model 1: Equal variances and covariances fixed to 0
# Model 2: Varying variances and covariances fixed to 0
# Model 3: Equal variances and equal covariances
# Model 6: Varying variances and varying covariances
# Alternativ: Angeben von Variance und Kovarianz

(mod <- data %>% estimate_profiles(n_profiles = 1:3, 
                                   variances = c("equal", "varying"),
                                   covariances = c("zero", "varying")))
# Für weitere Argumente, siehe:
?mclust::mclust # , da die Funktion auf dem Paket/der Funktion basiert

# Modellauswertung und -vergleich -----------------------------------------
get_fit(mod) # Umfangreiche Liste an Fit Indizes
             # Für genaueres/mehr, siehe: 
# https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html

# Automatisierter Modellvergleich anhand Fit Indizes
compare_solutions(mod)
compare_solutions(mod, statistics = c("CAIC", "BIC"))


# Nützliche/Verfügbare Outputs --------------------------------------------
get_data(mod) # Daten samt Klassenwahrscheinlichkeiten und det. Zugehörigkeiten
get_estimates(mod) # Übersicht der Schätzwerte

### Übersichtsgrafik der Klassen und Mittelwerte 
plot(mod)
plot_profiles(mod)
plot_density(mod)
plot_bivariate(mod$model_1_class_2)







# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# depmixS4 Package --------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
library(depmixS4)
data(cheating, package = "poLCA")
# Schätzen eines latenten Klassenmodells ----------------------------------
dep.definition <- depmixS4::mix(list(LIEEXAM ~ 1, LIEPAPER ~ 1, FRAUD ~ 1, COPYEXAM ~ 1),
                        family = list(multinomial("identity"),  # Für jeden Indikator
                                      multinomial("identity"),  # eine Verteilungsfamilie
                                      multinomial("identity"),  # + evtl. Link-Funktion
                                      multinomial("identity")), # spezifizieren
                        data = cheating, # Datensatz für Modell
                        nstates = 2, # Anzahl an Klassen
                        prior =~ 1, # Ort für Kovariaten
                        initdata = cheating # Datensatz für Wahrscheinlichkeiten
)
(mod.depmix <- fit(dep.definition, verbose = T, fixed = NULL, equal = NULL))

# Zusammenfassung der (bed.) Klassenwahrscheinlichkeiten -------------------
summary(mod.depmix) # Übersichtslegende zum Interpretieren:
                    # St1 = State 1 = Klasse 1,
                    # Re1.X = erste definierte Variable (hier: LIEEXAM)
                    # ReX.1 = Erste Ausprägung einer Variable (hier: Wert 1)
                    # Re1.1 = Erste def. Variable mit Ausprägungswert 1 
                    # ReX.Intercept = Mittelwert einer stetigen Verteilung
                    # ReX.sd = Standardabweichung einer stetigen Verteilung
# Einfache Grafik ---------------------------------------------------------
depmixS4.plot.probs <- function(fit.mod) {
  require(gridExtra)
  require(grid)
  require(ggplot2)
  require(reshape2)
  a<-lapply(1:length(fit.mod@response[[1]]), function(y) {
    sapply(fit.mod@response, function(x) x[[y]]@parameters$coefficients)
  })
  names(a)<-sapply(1:length(fit.mod@response[[1]]), function(y) as.character(fit.mod@response[[1]][[y]]@formula[[2]]))
  g<-grid::gList()
  for(i in 1:length(a)) {
    g[[i]]<- ggplotGrob(ggplot(melt(a[[i]]), aes(paste("Class", rev(Var2) ), value, fill=as.factor(Var1), label=round(value, 2)))+
                          geom_col()+geom_text(position=position_stack(vjust=0.5))+coord_flip()+scale_fill_brewer(type="seq", palette=i)+
                          labs(x="", y="", title=names(a)[i], fill="Categories")+theme_minimal())
  }
  grid.arrange(grobs=g, nrow=length(a) )
}
depmixS4.plot.probs(mod.depmix) # Drück 'Zoom' für bessere Übersicht


# Weiteres ----------------------------------------------------------------
posterior(mod.depmix)




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# randomLCA ---------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
library(randomLCA)
data(cheating, package = "poLCA")

# (Basic) Latent Class Modell-Schätzung
random.mod <- randomLCA(patterns = (cheating[,1:4]-1), nclass = 2, notrials = 10)
summary(random.mod) # Outcome probabilities zeigen Wahrscheinlichkeiten für eine Ausprägung 

# Ausprägungswahrscheinlichkeiten innerhalb der Klassen
outcomeProbs(random.mod) # Outcome Wahrscheinlichkeit + (hier fehlerhaften) Standardfehler
                         # => Der Grund sind Wahrscheinlichkeiten sehr nahe am Rand,
                         # die die Berechnung der Standardfehler problematisch macht
outcomeProbs(random.mod, boot = TRUE) # Für bessere (Bootstrap-basierte) Standardfehler

# Plot die Ausprägungswahrscheinlichkeiten innerhalb der Klassen
plot(random.mod, type = "b", pch = 1:2, xlab = "Test",
     ylab = "Outcome Probability",
     scales = list(x = list(at = 1:4, labels = names(cheating)[1:4])),
     key = list(corner = c(0.05, .95), border = TRUE, cex = 1.2,
                text = list(c("Class 1", "Class 2")),
                col = trellis.par.get()$superpose.symbol$col[1:2],
                points = list(pch = 1:2)))

# Klassenanteile
classProbs(random.mod)

# Posterior Klassenanteile nach Ausprägungen
print(postClassProbs(random.mod), row.names = FALSE)



