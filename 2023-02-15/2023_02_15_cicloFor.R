###########################
# USARE I CICLI FOR E WHILE PER CALCOLARE IL POWER
############################
#CICLI FOR
#Il ciclo for ci permette di ripetere una stessa serie di operazioni un certo numero di volte
#Proviamo a scrivere la stessa cosa 10 volte
for(i in 1:10){           #qua iniziamo a scrivere la funzione e specifichiamo il nome della nostra variabile "contatore" (i) e cosa deve contare (da 1 a 10)
  print(i)                #qua diciamo a R di fare qualcosa, in questo caso di scrivere il valore di "i"
  print("Well done!")     #e ogni volta gli diciamo anche di scrivere queste due parole "Well done!"
}                         #e poi chiudiamo la graffa

#Facciamolo piano. Qui, alla funzione precedente aggiungiamo un pezzettino solo per rallentare il ciclo e osservare cosa succede
for(i in 1:10){
  Sys.sleep(0.5)
  print(i)
  print("Well done!")
}

#Simuliamo dei numeri dieci volte
for(i in 1:10){
  Sys.sleep(0.5)
  print(rnorm(1,0,1)) #continuiamo a fare come prima, ma modifichiamo la funzione interna chiedendo a R di estrarre un numero da una distribuzione di punti z
}

#Facciamo un ciclo dentro qualcosa
#Non sempre sappiamo quanto dev'essere lungo il ciclo. A volte infatti vogliamo che faccia qualcosa
#per ogni elemento di un dataset o di una lista, quindi ? meglio dirgli di calcolarsi da solo l'N

complimento <- c("bravo", "very well", "wow", "this is amazing!", "OMG",
                 "you're doing so well", "and getting better", "better than Enrico") #Questa ? la nostra lista di lunghezza N sconosciuta o variabile
for(i in 1:length(complimento)){ #Qui indichiamo che la nostra "i" varier? da 1 fino a quanto ? lunga la lista (il vettore in realt?)
  Sys.sleep(1)
  print(complimento[i]) #di volta in volta diciamo a R di scrivere che elemento c'? nel vettore "complimento" alla posizione "i"
}

#Salviamo i risultati di un ciclo for
#Fino ad ora abbiamo fatto fare cose a R e abbiamo visto cosa succedeva, un po' come fosse uno spettacolo teatrale.
#Solitamente per? ci interessa fare qualcosa di pratico all'interno dei cicli e salvarci quello che ? successo, 
#...o almeno quella piccola cosa che ci interessa, come il p value

iter <- 10   #iter ? il numero di volte che vogliamo far girare il ciclo. 
total <- c() #questo ? invece un vettore vuoto in cui possiamo salvare nuove informazioni. Ci servir? per salvare i valori generati durante il ciclo

for (i in 1:iter) { #Quindi facciamo cicli da 1 a iter volte
  Sys.sleep(0.5)    
  total[i] <- 0 + i #e salviamo nel vettore "total" alla posizione "i" la somma di 0 + "i" ("i" varier? di volta in volta)
  print(total)      #vediamo come si trasforma il vettore a ogni iterazione
}

#Ma possiamo anche far succedere pi? cose nel mezzo
#In questo esempio genereremo l'et? di una persona da una distribuzione normale di et? (quella italiana approssimativamente)
#assegneremo ogni "persona" a un gruppo di anzianit? diverso (minorenni, giovani adulti e adulti adulti)
#calcoleremo la media di et? della popolazione a ogni iterazione
#Il risultato di ogni singola operazione verr? salvato in uno di tre appositi vettori inizialmente vuoti
rm(list = ls()) #Elimino tutto quello che ho salvato nell'environment
iter <- 1000 #Di nuovo il numero di iterazioni e una serie di vettori vuoti
anni <- c()
media <- c()
gruppo <- c()

for (i in 1:iter) {
  #simulo i dati relativi all'et? da una popolazione con eta media distribuita tra 0 e 99 anni
  anni[i] <- runif(1, min = 0, max = 99)
  #assegno a ogni persona un gruppo di appartenenza usando "ifelse" (l'abbiamo visto due volte fa)
  gruppo[i] <- ifelse(anni[i] < 18, "minorenne", 
                      ifelse(anni[i] < 35, "giovane-adulto",
                             "adulto-adulto"))
  #calcolo la media dell'età delle persone simulate fino a questo momento
  media[i] <- mean(anni)
}

plot(media) #plottiamo il risultato
mean(anni)  #calcoliamo la media della popolazione
table(gruppo) #vediamo la numerosit? dei diversi gruppi

#Facciamo qualcosa di utile adesso
rm(list = ls())
###########################
#Feraco et al., 2022
#L'adattabilità predice i voti scolastici?
library(lavaan)
library(MASS)

CovMat<-lav_matrix_lower2full(c(1,
                                .20,1)) #generiamo due variabili correlate (r = .20) come fatto la volta scorsa
colnames(CovMat) <- c("Ad", "Gp") #diamo un nome alle colonne
rownames(CovMat) <- c("Ad", "Gp") #e alle righe della matrice

db <- as.data.frame(mvrnorm(n = 1000, mu = c(0,0), Sigma = CovMat, empirical = T)) #simuliamo 1000 osservazioni a partire dalla matrice di correlazioni
m  <- lm(Gp ~ Ad, data = db) #fittiamo un modello lineare (essendo una dipendente e una indipendente, stiamo facendo una correlazione fondamentalmente)
summary(m)$coefficients[2,4] #questo ? il p value tirato fuori dal summary dei risultati.

library(ggplot2) #qui facciamo un semplice plot delle osservazioni
ggplot(db, mapping = aes(x = Ad, y = Gp)) +
  geom_smooth(method = "lm") +
  geom_point() +
  xlab("Adaptability") +
  ylab("Achievement")

#Ciclo FOR per power analisi
#Iniziamo a fare una power analisi. Per farlo, ci servono:
N = 200 #quanti partecipanti vogliamo testare per vedere se abbiamo il power?
iter = 500 #quante iterazioni facciamo? pi? sono meglio ?
p = c()    #i valori di "p" per vedere quante volte esce significativo
b = c()    #e anche i beta se vogliamo vedere altro (quanto grande ? l'effetto significativo minimo?)

for (i in 1:iter) {
  db <- as.data.frame(mvrnorm(CovMat, n = N, mu = c(0,0), empirical = F)) #simuliamo di volta in volta un nuovo dataset dalla matrice di prima
  m <- lm(Gp ~ Ad, data = db) #fittiamo il modello
  p[i] <- summary(m)$coefficients[2,4] #salviamo il valore di p che ci interessa
  b[i] <- summary(m)$coefficients[2,1] #e il beta di interesse
}
mean(p<.05) #quante volte p <.05? WE HAVE THE POWER!
mean(b>.15) #quante volte la relazione ? almeno di .15? mmmm...

###RENDIAMO IL MODELLO MULTIPLO
#Raramente (mai) i costrutti psicologici sono ortogonali tra di loro e, a volte, ? il caso di controllare per tali correlazioni
#In questi casi ? meglio ipotizzare una matrice di correlazioni pi? completa, che consideri anche altri fattori potenzialmente importanti
#Oltre alla correlazione tra adattabilit? (Ad) e achievement (Gp) consideriamo quindi le emozioni scolastiche (em), la self efficacy (se)
#e l'apprendimento autoregolato (sr) e costruiamo una matrice di correlazione pi? completa usando lo stesso codice di prima

#Matrice di correlazione ipotetica
#                               Ad   Em   Se   Sr   Gp  
CovMat<-lav_matrix_lower2full(c(  1,
                                .30,  1,
                                .40, .30,   1,
                                .40, .30, .35,   1,
                                .20, .15, .45, .30,  1))


colnames(CovMat)<-c("Ad", "Em", "Se", "Sr", "Gp")
rownames(CovMat)<-c("Ad", "Em", "Se", "Sr", "Gp")
CovMat

#Simulare i dati
db<-data.frame(mvrnorm(n=1000, mu=c(0,0,0,0,0), CovMat, empirical = T))
head(db) #vediamo le prime sei righe del dataset creato
round(cor(db),2) #controlliamo che i dati abbiano effettivamente le correlazioni ipotizzate

m <- lm(Gp ~ Ad + Em + Se + Sr, data = db) #fittiamo lo stesso modello di regressione, ma aggiungiamo i predittori multipli
res <- summary(m) #salviamo i risultati
res$coefficients[,4] #prendiamo tutti i p value degli effetti

#Ciclo for per power analisi
#questa volta vogliamo salvarci tutti i p value (non si sa mai), quindi ci servir? una matrice e non pi? un vettore
iter = 1000
N = 5000
p = matrix(nrow = iter, ncol = ncol(db)) #matrice per il salvataggio dei risultati multipli
colnames(p) <- c("intercept", "ad", "em", "se", "sr") #diamo il nome alle colonne della matrice
p <- as.data.frame(p) #trasformiamola in dataframe

for (i in 1:iter) {
  #Simuliamo nuovi dati ogni volta
  db<-data.frame(mvrnorm(n=N, mu=c(0,0,0,0,0), CovMat, empirical = F)) #Empirical = F
  #Fittiamo il modello
  m <- lm(Gp ~ Ad + Em + Se + Sr, data = db)
  #Salviamo i valori di p nella nostra matrice
  p[i,] <- summary(m)$coefficients[,4] #in ogni riga della matrice salviamo i p, in modo che corrispondano alle colonne
}

head(p)
colSums(p < .05)/iter #qual è il power di ogni singolo effetto

#E se volessimo il power di tutti gli effetti?
p$totPower <- rowSums(p[,2:5]<.05)
sum(p$totPower == 4)/iter


#CICLO WHILE...lo vediamo pi? avanti
####PROSEGUIAMO FINCHE NON SI TROVA L'N ADEGUATO
iter = 1000 #quante interazioni
p = c()     #salviamo solo quello che ci interessa
power_at_n = c(0) #qua calcoliamo il power ogni volta che finisce un ciclo
N = 5000    #qua la numerosità del primo ciclo
k = 2       #questo ci serve solo per fare andare il ciclo while
sample = c()#solo per tenere traccia degli N ispezionati

while (power_at_n[k-1] < .80) {#finchè power_at_n non raggiunge .80, continuiamo
  for (i in 1:iter) {
    #Simuliamo nuovi dati ogni volta
    db<-data.frame(mvrnorm(n=N, mu=c(0,0,0,0,0), CovMat, empirical = F)) #Empirical = F
    #Fittiamo il modello
    m <- lm(Gp ~ Ad + Em + Se + Sr, data = db)
    #Salviamo i valori di p nella nostra matrice
    p[i] <- summary(m)$coefficients[2,4]
  }
  power_at_n[k] <- mean(p < .05) #calcoliamo il power
  sample[k-1] <- N #salviamo l'N usato
  N = N + 1000 #Aumentiamo la numerosità campionaria
  k = k+1 #passiamo al prossimo ciclo
}
plot(sample, power_at_n[-1], ylab = "power")
abline(h =.80, col = "red")

