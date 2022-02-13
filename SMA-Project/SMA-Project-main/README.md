PROGETTO DI SOCIAL MEDIA ANALYTICS
Università degli studi di Milano - Bicocca

Nella cartella si troveranno il report (Report_SMA.pdf), la presentazione power point (SMA_project.pptx), i dati e il codice python.

--DATI

-Edges_df6.csv : dataframe contenente gli archi del grafo, utilizzato come input Gephi per la realizzazione delle visualizzazioni
-Nodes_definitivo.csv : dataframe contenente i nodi del grafo, utilizzato come input Gephi per la realizzaizone delle visualizzazioni. Si specifica che questo file è l'output a seguito dell'esecuzione del codice graph.ipynb
-retweeted_sentiment.csv : dataframe contenente i testi dei tweet che sono stati ritwittati. Il file contiene il risultato a seguito dell'esecuzione del codice Sentiment.ipynb
-sentix.txt : lexicon utilizzato per la sentiment analysis con Vader

--CODICE

-Tweets.ipynb : codice per eseguire la raccolta dati tramite Twitter
-graph.ipybn : codice per performare la community detection e la valutazione delle performance
-evaluation.ipynb : codice per perfomare le valutazioni sul grafo e le relative misure
-Sentiment.ipynb : codice per performare la sentiment analysis
