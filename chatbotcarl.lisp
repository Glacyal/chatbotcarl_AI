;Viene utilizzato per verificare se l'input iniziale combacia con un determinato pattern
(defun match (pat in)
  (cond
   ((null pat) (null in))
   ((eq (car pat) '*) (wildcard pat in))
   ((eq (car pat) (car in)) (match (cdr pat) (cdr in)))
   (t nil)))
;Gestisce il match con i caratteri jolly
(defun wildcard (pat in)
  (cond
   ((match (cddr pat) in) (bind (cadr pat) nil) t)
   ((null in) nil)
   ((match pat (cdr in)) (bind (cadr pat) (car in)) t)
   (t nil)))

(defvar *viewpoint* '())

(defun swap (value)
  (let ((a (assoc value *viewpoint*)))
    (if a (cadr a) value)))

(defvar *bindings* nil)

(defun bind (var value)
  (cond
   ((assoc var *bindings*)
    (push (swap value) (cdr (assoc var *bindings*))))
   (t (push (cons var (swap value)) *bindings*))))

(defun subs (list)
  (cond
   ((null list) nil)
   (t
    (let ((a (assoc (car list) *bindings*)))
      (cond
       (a (append (cdr a) (subs (cdr list))))
       (t (cons (car list) (subs (cdr list)))))))))
;Data una lista selezione un elemento random
(defun random-elt (list)
    (nth (random (length list)) list))

;Data una lista selezione l'elemento desiderato tramite indice
(defun extract (list num)
  (nth num list)
)

(defvar *altro*
  '(
     ((* x  ) (non ho capito! ) (chiedimi qualcosa) (sei sicuro di quello che dici) (sei un essere umano?) (chiedimi altro sulle opere di Carl) (chiedimi qualcosa su Carl)(non hai niente da chiedermi su Carl?))
     ((si) (Si e...?) (Devi essere più specifico sono un bot abbastanza stupido) (Rispondo solo a domande su Carl Maria von Weber))
     ((no) (No cosa?) (Devi essere più specifico sono un bot abbastanza stupido) (Rispondo solo a domande su Carl Maria von Weber))
   )
)
;Gestione risposte saluti (dal piu specifico al piu generale ->)
(defvar *saluti*
 '(
    ((* x come stai? * y) (Ciao io sto abbastanza bene grazie e tu?) (bene e tu?)(potrebbe andare meglio))
    ((* x come stai * y) (Ciao io sto abbastanza bene grazie e tu?) (bene e tu?))
    ((* x che fai? * y) (Mi giro i pollici tu?) (Aspetto una tua domanda...))
    ((* x che fai * y) (Posso aiutarti?) (Aspetto una tua domanda...))
    ((* x che pensi * y) (mi merito la sufficienza!!!))
    ((ciao) (Ciao sono Carl come posso aiutarti?) (ciao))
    ((ciao carl) (Ciao sono Carl come posso aiutarti?) (ciao))
    ((Buongiorno) (buongiorno sono carl come posso aiutarti) (buongiorno))
    ((Buongiorno carl) (buongiorno sono carl come posso aiutarti) (buongiorno))
    ((Buonasera) (buonasera sono carl come posso aiutarti) (buonasera))
    ((Buonasera carl) (buonasera sono carl come posso aiutarti) (buonasera))
    ((chi sei?) (Carl...)(Sono io! apri))
    ((* x bene) (ottimo)(chiedimi qualcosa su carl sono qua per questo...))
    ((* x come sta Carl?)(Non lo sento da un pò...)(non si vede in giro da secoli)(nella tomba))
    ((carl) (Dimmi!))
    ((* x ok  * y) (ok.))
  )
)

(defvar *domande-generali*
 '(
     ((* x opera perduta * y) (Il potere dell amore e del vino))
     ((* x opera * z perduta * y) (Il potere dell amore e del vino))
     ((* x opere perdute * y) (Il potere dell amore e del vino))
     ((* x opere * z perdute * y) (Il potere dell amore e del vino))
     ((* x prima opera * y) (La prima opera composta risale al 1798 Il potere dell amore e del vino))
     ((* x opera meno famosa * y) (Non ho un metro di giudizio per questo prova a chiedermi altro))
     ((* x opera * z famosa * y) (Carl Maria von Weber ha composto molte opere di successo ad esempio il franco cacciatore - Abu Hassan - Silvana...))
     ((* x opere * z famose * y) (Carl Maria von Weber ha composto molte opere di successo ad esempio il franco cacciatore - Abu Hassan - Silvana...))
     ((* x quante * z opere maggiori * y) (Carl ha composto 10 opere principali))
     ((* x opere maggiori * y) (Ecco le opere piu importanti di Carl - Il potere dell amore e del vino - La ragazza della foresta - Peter Schmoll e i suoi vicini - Rubezahl - Silvana - Abu Hassan - Il franco cacciatore - I tre Pintos - Euryanthe - Oberon. Se vuoi conoscerne una in particolare digita il nome dell opera!))
     ((* x opera maggiore * y) (Ecco le opere piu importanti di Carl - Il potere dell amore e del vino - La ragazza della foresta - Peter Schmoll e i suoi vicini - Rubezahl - Silvana - Abu Hassan - Il franco cacciatore - I tre Pintos - Euryanthe - Oberon. Se vuoi conoscerne una in particolare digita il nome dell opera!))
     ((* x opere principali * y) (Ecco le opere piu importanti di Carl - Il potere dell amore e del vino - La ragazza della foresta - Peter Schmoll e i suoi vicini - Rubezahl - Silvana - Abu Hassan - Il franco cacciatore - I tre Pintos - Euryanthe - Oberon. Se vuoi conoscerne una in particolare digita il nome dell opera!))
     ((* x opera principale * y) (Ecco le opere piu importanti di Carl - Il potere dell amore e del vino - La ragazza della foresta - Peter Schmoll e i suoi vicini - Rubezahl - Silvana - Abu Hassan - Il franco cacciatore - I tre Pintos - Euryanthe - Oberon. Se vuoi conoscerne una in particolare digita il nome dell opera!))
     ((* x musica concertante * y) (Carl Maria ha composto molta musica concertante per pianoforte e orchestra tra cui due concerti per pianoforte e orchestra in do maggiore op. 11 1810 in mi bemolle maggiore op. 32 1812 - Konzertstuck in fa minore per pianoforte e orchestra op. 79 1821 - Due concerti per clarinetto e orchestra in fa minore op. 73 1811 in mi bemolle maggiore op. 74 1811 - Concertino in mi bemolle maggiore per clarinetto e orchestra op. 26 1810 - Concertino per oboe e fiati in do maggiore -1809- Concerto per fagotto e orchestra in fa maggiore op. 75 1811-1822 - Concertino per corno e orchestra in mi minore op. 45 1806-1815 - Romanza siciliana per flauto e orchestra in sol minore J. 47 1805 - Sei variazioni in do maggiore sul tema -A Schusserl und a Reind rl- per viola e orchestra J. 49 1806 - Grand pot-pourri in re maggiore per violoncello e orchestra op. 20 1808 - Andante e Rondo ungarese versione per viola e orchestra J. 79 1809 - Tema con variazioni per violoncello e orchestra in re minore J. 94 1810 - Adagio e Rondo in fa maggiore per harmonichord e orchestra J. 115 1811 - Andante e Rondo ungarese versione per fagotto e orchestra op. 35 1813))
     ((* x ultima opera * y) (L ultima opera di Carl Maria von Weber risale al 1826 Oberon))
     ((* x quante * z opere * y) (Carl ha composto 10 opere))
     ((* x quante * z musiche di scena * y ) (Carl ha composto 8 musiche di scena))
     ((* x quante * z musiche sacre * y ) (Carl ha composto 3 messe))
     ((* x quante * z messe * y ) (Carl ha composto 3 messe))
     ((* x quante * z musiche orchestrali * y ) (Carl ha composto 10 musiche orchestrali))
     ((* x quante * z musiche concertante * y ) (Carl ha composto 14 musiche concertante))
     ((* x quante * z musiche per pianoforte * y ) (Carl ha composto 6 musiche per pianoforte))
     ((* x quali * z musiche sacre * y ) (Carl ha composto 3 messe))
     ((* x quali * z opere * y) (Il potere dell amore e del vino - La ragazza della foresta - Peter Schmoll e i suoi vicini - Rubezahl - Silvana - Abu Hassan - Il franco cacciatore - I tre Pintos - Euryanthe - Oberon. Se vuoi conoscerne una in particolare digita il nome dell opera!))
     ((* x quali * z messe * y ) (Grande fiera della gioventu - Missa sancta n.1 Freischutzmesse - Missa sancta n.2 Giubelmesse ))
     ((* x genere * z musica * y) (Carl ha composto opere musiche di scena musica sacra musica orchestrale musica concertante musica per pianoforte)) 
     ((* x genere * y) (Carl ha composto opere musiche di scena musica sacra musica orchestrale musica concertante musica per pianoforte)) 
     ((* x tipo * z musica * y) (Carl ha composto opere musiche di scena musica sacra musica orchestrale musica concertante musica per pianoforte)) 
     ((* x opere * y) (Carl ha composto 10 opere))
     ((* x musica * y) (chiedimi qualcosa di piu specifico tra musiche di scena  sacre  orchestrale pianoforte))
     ((* x musiche * y) (chiedimi qualcosa di piu specifico tra musiche di scena  sacre  orchestrale pianoforte))
     ((* x Carl Maria * y) (Carl Maria Friedrich Ernst von Weber - Eutin 18 novembre 1786  Londra 5 giugno 1826 - era un compositore direttore d orchestra e pianista tedesco. La musica di Weber in particolare le sue opere liriche influenzarono grandemente lo sviluppo della musica romantica in Germania))
     ((* x strumenti? * y) (Ha realizzato varie composizioni per pianoforte fagotto clarinetto corno oboe viole violoncello))
     ((* x strumenti * y) (Ha realizzato varie composizioni per pianoforte fagotto clarinetto corno oboe viole violoncello))
  )
)


(defvar *domande-opere*
 '(
     ((* x atti * z Oberon * y ) (Oberon ha 3 atti))
     ((* x atti * z Silvana * y ) (Silvana ha 3 atti))
     ((* x atti * z Euriano * y ) (Euriano ha 3 atti))
     ((* x scene * z Oberon * y ) (Oberon ha 8 scene))
     ((* x numeri * z Oberon * y ) (Oberon ha 21 numeri))
     ((* x atti * z il franco cacciatore * y ) (Il franco cacciatore ha 3 atti))
     ((* x atti * z Abu Hassan * y ) (Abu Hassan ha un solo atto))
     ((* x Il potere dell amore e del vino * y ) (composta nel 1798 ma andata perduta)) 
     ((* x La ragazza della foresta * y ) (composta nel 1800))
     ((* x Peter Schmoll e i suoi vicini * y ) (composta nel 1803))  
     ((* x Rubezahl * y ) (composta tra 1804-1805 ma è rimasta incompiuta))
     ((* x Silvana * y ) (composta nel 1810))
     ((* x Abu Hassan * y ) (Abu Hassan un Singspiel in un atto di Carl Maria von Weber 1786-1826 su un libretto tedesco di Franz Carl Hiemer 1768-1822 tratto su un racconto delle Mille e una notte. Composta da Weber tra l 11 agosto 1810 e il 12 gennaio 1811 fu rappresentata per la prima volta al Teatro Cuvillies di Monaco di Baviera il 4 giugno 1811 con la direzione di Peter Winter 1754-1825))
     ((* x Il franco cacciatore * y ) (Il franco cacciatore -titolo originale Der Freischutz- un Singspiel in tre atti di Carl Maria von Weber su libretto di Johann Friedrich Kind scritto tra il 1817 e il 1821))
     ((* x I tre Pintos * y ) (composta tra 1820-1824 ma rimasta incompiuta terminata da Gustav Mahler))
     ((* x Euriano * y ) (Euriano opera eroico-romantica in tre atti di Carl Maria von Weber su libretto di Helmina von Chezy. La prima esecuzione avvenne al Theater am Karntnertor -Teatro di Porta Carinzia- di Vienna il 25 ottobre 1823 con la direzione dello stesso autore))
     ((* x Oberon * y ) (Oberon opera romantica in 1 prologo 3 atti 8 scene e 21 numeri di Carl Maria von Weber e libretto in lingua inglese del drammaturgo inglese James Robinson Planche ispirata al poema omonimo di Christoph Martin Wieland))
     ((* x potere dell amore * y ) (forse cercavi l'opera Il potere dell amore e del vino composta nel 1798 ma andata perduta))
     ((* x ragazza * y ) (forse cercavi l'opera La ragazza della foresta composta nel 1800))
     ((* x foresta * y ) (forse cercavi l'opera La ragazza della foresta composta nel 1800))
     ((* x vicini * y ) (forse cercavi l'opera Peter Schmoll e i suoi vicini composta nel 1803))  
     ((* x Peter * y ) (forse cercavi l'opera Peter Schmoll e i suoi vicini composta nel 1803 oppure la musica orchestrale Ouverture per Peter Schmoll composta tra 1807)) 
     ((* x Schmoll * y ) (forse cercavi l'opera Peter Schmoll e i suoi vicini composta nel 1803 oppure la musica orchestrale Ouverture per Peter Schmoll composta tra 1807)) 
     ((* x franco * y ) (forse cercavi l'opera Il franco cacciatore -titolo originale Der Freischutz- un Singspiel in tre atti di Carl Maria von Weber su libretto di Johann Friedrich Kind scritto tra il 1817 e il 1821))
     ((* x cacciatore * y ) (forse cercavi l'opera Il franco cacciatore -titolo originale Der Freischutz- un Singspiel in tre atti di Carl Maria von Weber su libretto di Johann Friedrich Kind scritto tra il 1817 e il 1821))
     ((* x tre * y ) (forse cercavi l'opera I tre Pintos composta tra 1820-1824 ma rimasta incompiuta terminata da Gustav Mahler))
     ((* x Pintos * y ) (forse cercavi l'opera I tre Pintos composta tra 1820-1824 ma rimasta incompiuta terminata da Gustav Mahler))
     ((* x potere * y ) (forse cercavi l'opera Il potere dell amore e del vino composta nel 1798 ma andata perduta))
     ((* x amore * y ) (forse cercavi l'opera Il potere dell amore e del vino composta nel 1798 ma andata perduta oppure la musica di scena Amore per amore composta nel 1818))
     ((* x vino * y ) (forse cercavi l'opera Il potere dell amore e del vino composta nel 1798 ma andata perduta))
     ((* x Hassan * y ) (forse cercavi l'opera Abu Hassan un Singspiel in un atto di Carl Maria von Weber 1786-1826 su un libretto tedesco di Franz Carl Hiemer 1768-1822 tratto su un racconto delle Mille e una notte. Composta da Weber tra l 11 agosto 1810 e il 12 gennaio 1811 fu rappresentata per la prima volta al Teatro Cuvillies di Monaco di Baviera il 4 giugno 1811 con la direzione di Peter Winter 1754-1825))
     ((* x atti * y) (atti di quale opera?))
     ((* x scene * y) (scene di quale opera?))
  )
)

(defvar *domande-musiche-di-scena*
 '(
     
    ((* x Turandot * y ) (composta nel 1809))
    ((* x Re Yngurd * y ) (composta nel 1817))
    ((* x Donna Diana * y ) (composta nel 1817))
    ((* x Enrico IV re di Francia * y ) (composta nel 1818))
    ((* x Enrico IV * y ) (forse cercavi la musica di scena Enrico IV re di Francia composta nel 1818))
    ((* x Amore per amore * y ) (composta nel 1818))
    ((* x Il faro * y ) (composta nel 1820))
    ((* x Splendido * y ) (composta nel 1820))
    ((* x Il figlio sassone si è sposato oggi * y )(composta nel 1822))

    ((* x Re * y )(forse cercavi la musica di scena Re Yngurd composta nel 1817 oppure Enrico IV re di Francia composta nel 1818))
    ((* x Yngurd * y )(forse cercavi la musica di scena Re Yngurd composta nel 1817))

    ((* x Diana * y ) (forse cercavi la musica di scena Donna Diana composta nel 1817))
    ((* x Donna * y ) (forse cercavi la musica di scena Donna Diana composta nel 1817))

    ((* x Enrico * y ) (forse cercavi la musica di scena Enrico IV re di Francia composta nel 1818))
    ((* x Francia * y ) (forse cercavi la musica di scena Enrico IV re di Francia composta nel 1818))
    ((* x faro * y ) (forse cercavi la musica di scena composta nel 1820))

    ((* x figlio * y )(forse cercavi la musica Il figlio sassone si è sposato oggi composta nel 1822))
    ((* x sassone * y )(forse cercavi la musica Il figlio sassone si è sposato oggi composta nel 1822))
    ((* x sposato * y )(forse cercavi la musica Il figlio sassone si è sposato oggi composta nel 1822))
    ((* x oggi * y )(forse cercavi la musica Il figlio sassone si è sposato oggi composta nel 1822))
  )
)

(defvar *musica-sacra*
 '(
    ((* x nota grande fiera della gioventu * y) (in mi bemolle maggiore)) 
    ((* x nota Missa sancta n1 * y) (in mi bemolle maggiore)) 
    ((* x nota Missa sancta n2 * y) (in sol maggiore)) 
    ((* x grande fiera della gioventu * y ) (composta nel 1802))
    ((* x missa sancta n1 * y) (composta tra 1817-1818)) 
    ((* x missa sancta n2 * y ) (composta tra 1818-1819))
    ((* x missa sancta * y) (missa sancta n1 composta tra 1817-1818 missa sancta n2 composta tra 1818-1819))


    ((* x gioventu * y) (forse cercavi la musica sacra grande fiera della gioventu composta nel 1802 in mi bemolle maggiore)) 
    ((* x fiera * y) (forse cercavi la musica sacra grande fiera della gioventu composta nel 1802 in mi bemolle maggiore)) 
    ((* x grande * y) (forse cercavi la musica sacra grande fiera della gioventu composta nel 1802 in mi bemolle maggiore)) 
    ((* x missa * y) (missa sancta n1 composta tra 1817-1818 in mi bemolle maggiore missa sancta n2 composta tra 1818-1819 in sol maggiore))
    ((* x sancta * y) (missa sancta n1 composta tra 1817-1818 in mi bemolle maggiore missa sancta n2 composta tra 1818-1819 in sol maggiore))
  )
)
;NOTA-TONALITà
(defvar *musica-orchestrale*
 '(
    ((* x nota Due sinfonie * y) (in Do Maggiore))
    ((* x Due sinfonie * y) (composta tra  1806-1807))
    ((* x Ouverture per Peter Schmoll * y) (composta tra 1807))
    ((* x Ouverture per Beherrscher der Geister * y) (composta tra 1811))
    ((* x Jubel-Ouverture * y) (composta tra 1818))
    ((* x Kleiner Tusch * y) (composta tra 1806))
    ((* x Walzer * y) (composta tra 1812))
    ((* x Deutscher * y) (composta tra 1815))
    ((* x Tedesco * y) (composta tra 1816))
    ((* x Marcia vivace * y) (composta tra 1822))
    ((* x Marcia J. * y) (composta tra 1826))
    ((* x Marcia J * y) (composta tra 1826))

    ((* x Kleiner * y) (forse cercavi la musica orchestrale composta tra 1806))
    ((* x Tusch * y) (forse cercavi la musica orchestrale composta tra 1806))

    ((* x Ouverture * y) (sono presenti tre Ouverture Ouverture per Peter Schmoll - Ouverture per Beherrscher der Geister - Jubel-Ouverture))
    ((* x Beherrscher * y) (forse cercavi la musica orchestrale Ouverture per Beherrscher der Geister composta tra 1811))
    ((* x Geister * y) (forse cercavi la musica orchestrale Ouverture per Beherrscher der Geister composta tra 1811))

    ((* x Jubel * y) (forse cercavi la musica orchestrale Jubel-Ouverture composta tra 1818))
    ((* x sinfonie * y) (forse cercavi la musica orchestrale Due sinfonie composta tra 1806-1807 in Do Maggiore))
    ((* x Due * y) (forse cercavi la musica orchestrale Due sinfonie composta tra 1806-1807 in Do Maggiore))
    ((* x Tusch * y) (forse cercavi la musica orchestrale Kleiner Tusch composta tra 1806))
    ((* x Kleiner * y) (forse cercavi la musica orchestrale Kleiner Tusch composta tra 1806))

    ((* x Marcia * y) (forse cercavi la musica orchestrale Marcia vivace composta tra 1822 oppure Marcia J. composta tra 1826))
    ((* x vivace * y) (forse cercavi la musica orchestrale Marcia vivace composta tra 1822))
  
  )
)

(defvar *musica-per-pianoforte*
 '(
    ((* x nota Quattro Sonate * y) (in Do Maggiore La bemolle Maggiore in re minore in mi minore))
    ((* x Quattro Sonate * y) (la prima sonata composta nel 1812 la seconda nel 1816 la terza 1816 la quarta 1822))
    ((* x nota Momento capriccioso * y) (in Si bemolle Maggiore))
    ((* x nota Rondo Brillante * y) (in Re bemolle Maggiore))
    ((* x Rondo Brillante) (Scritto in forma di rondo di carattere brillante ed esempio di musica a programma l Invito alla danza descrive una scena danzante a ritmo di valzer. Il pezzo comincia con un introduzione lenta che vuole rappresentare l invito a danzare di un cavaliere a una dama La donna esita l uomo insiste e infine lei accetta Si susseguono quindi varie scene di ballo che si concludono con un immaginario inchino -rappresentato dal ritorno del gentile tema d apertura- quando l uomo riaccompagna la dama al proprio posto e si congeda))
 
    
    ((* x Sonate * y) (forse cercavi la musica per pianoforte Quattro Sonate la prima sonata composta nel 1812 la seconda nel 1816 la terza 1816 la quarta 1822))
    ((* x Quattro * y) (forse cercavi la musica per pianoforte Quattro Sonate la prima sonata composta nel 1812 la seconda nel 1816 la terza 1816 la quarta 1822))
    ((* x Momento * y) (forse cercavi la musica per pianoforte Momento capriccioso composta in Si bemolle Maggiore))
    ((* x capriccioso * y) (forse cercavi la musica per pianoforte Momento capriccioso composta in Si bemolle Maggiore))

    ((* x Brillante * y) (forse cercavi la musica per pianoforte Rondò Brillante composta in Re bemolle Maggiore Scritto in forma di rondo di carattere brillante ed esempio di musica a programma l Invito alla danza descrive una scena danzante a ritmo di valzer. Il pezzo comincia con un introduzione lenta che vuole rappresentare l invito a danzare di un cavaliere a una dama La donna esita l uomo insiste e infine lei accetta Si susseguono quindi varie scene di ballo che si concludono con un immaginario inchino -rappresentato dal ritorno del gentile tema d apertura- quando l uomo riaccompagna la dama al proprio posto e si congeda))
    
  )
)

(defvar *musica-concertante*
 '(
   ((* x pianoforte * y) (Forse cercavi due concerti per pianoforte e orchestra in do maggiore op. 11 1810 in mi bemolle maggiore op. 32 1812))
   ((* x piano * y) (Forse cercavi due concerti per pianoforte e orchestra in do maggiore op. 11 1810 in mi bemolle maggiore op. 32 1812))
   ((* x concerti * z pianoforte * y) (Due concerti per pianoforte e orchestra in do maggiore op. 11 1810 in mi bemolle maggiore op. 32 1812))
   ((* x concerti * z piano * y) (Due concerti per pianoforte e orchestra in do maggiore op. 11 1810 in mi bemolle maggiore op. 32 1812))
   ((* x Konzertstuck * y) (Un Konzertstuck realizzato in fa minore per pianoforte e orchestra op. 79 1821))
   ((* x concerti * z clarinetto * y) (Due concerti per clarinetto e orchestra in fa minore op. 73 1811 in mi bemolle maggiore op. 74 1811))
   ((* x concertini * y) (Concertino in mi bemolle maggiore per clarinetto e orchestra op. 26 1810-Concertino per oboe e fiati in do maggiore -1809- Concerto per fagotto e orchestra in fa maggiore op. 75 1811-1822-Concertino per corno e orchestra in mi minore op. 45 1806-1815))
   ((* x concertino * y) (Concertino in mi bemolle maggiore per clarinetto e orchestra op. 26 1810-Concertino per oboe e fiati in do maggiore -1809- Concerto per fagotto e orchestra in fa maggiore op. 75 1811-1822-Concertino per corno e orchestra in mi minore op. 45 1806-1815))
   ((* x Romanza siciliana * y) (Romanza siciliana per flauto e orchestra in sol minore J. 47 1805))
   ((* x variazioni * y) (Sei variazioni in do maggiore sul tema -A Schusserl und a Reind rl- per viola e orchestra J. 49 1806 - Tema con variazioni per violoncello e orchestra in re minore J. 94 1810))
   ((* x Romanza * y) (Forse cercavi questo? Romanza siciliana per flauto e orchestra in sol minore J. 47 1805))
   ((* x siciliana * y) (Forse cercavi questo? Romanza siciliana per flauto e orchestra in sol minore J. 47 1805))
   ((* x pot * z pourri * y) (Grand pot-pourri in re maggiore per violoncello e orchestra op. 20 1808))
   ((* x Andante * z Rondo * y) (Andante e Rondo ungarese versione per viola e orchestra J. 79 1809 - versione per fagotto e orchestra op. 35 1813))
   ((* x Andante * y) (Forse cercavi questo? Andante e Rondo ungarese versione per viola e orchestra J. 79 1809 - versione per fagotto e orchestra op. 35 1813))
   ((* x Adagio * z Rondo * y) (Adagio e Rondo in fa maggiore per harmonichord e orchestra J. 115 1811))
   ((* x Adagio * y) (Forse cercavi questo? Adagio e Rondo in fa maggiore per harmonichord e orchestra J. 115 1811))
   ((* x Rondo  * y) (forse cercavi la musica per pianoforte Rondò Brillante composta in Re bemolle Maggiore Scritto in forma di rondo di carattere brillante ed esempio di musica a programma l Invito alla danza descrive una scena danzante a ritmo di valzer. Il pezzo comincia con un introduzione lenta che vuole rappresentare l invito a danzare di un cavaliere a una dama La donna esita l uomo insiste e infine lei accetta Si susseguono quindi varie scene di ballo che si concludono con un immaginario inchino -rappresentato dal ritorno del gentile tema d apertura- quando l uomo riaccompagna la dama al proprio posto e si congeda))
   ((* x concerti * y) (Carl Maria ha composto molta musica concertante per pianoforte e orchestra tra cui due concerti per pianoforte e orchestra in do maggiore op. 11 1810 in mi bemolle maggiore op. 32 1812 - Konzertstuck in fa minore per pianoforte e orchestra op. 79 1821 - Due concerti per clarinetto e orchestra in fa minore op. 73 1811 in mi bemolle maggiore op. 74 1811 - Concertino in mi bemolle maggiore per clarinetto e orchestra op. 26 1810 - Concertino per oboe e fiati in do maggiore -1809- Concerto per fagotto e orchestra in fa maggiore op. 75 1811-1822 - Concertino per corno e orchestra in mi minore op. 45 1806-1815 - Romanza siciliana per flauto e orchestra in sol minore J. 47 1805 - Sei variazioni in do maggiore sul tema -A Schusserl und a Reind rl- per viola e orchestra J. 49 1806 - Grand pot-pourri in re maggiore per violoncello e orchestra op. 20 1808 - Andante e Rondo ungarese versione per viola e orchestra J. 79 1809 - Tema con variazioni per violoncello e orchestra in re minore J. 94 1810 - Adagio e Rondo in fa maggiore per harmonichord e orchestra J. 115 1811 - Andante e Rondo ungarese versione per fagotto e orchestra op. 35 1813))
  )
)



(defun main ()
    (print
    "Ciao sono Carl come posso aiutarti"
    )
    (loop
        (print ">>")
        
            (let* ((line (read-line))
                 (input (read-from-string (concatenate 'string "(" line ")"))))
                 (when (string= line "q") (return))
                  (when (string= line "quit") (return))
                   (when (string= line "exit") (return))
                    (when (string= line "esci") (return))

           (setq res1
            (dolist (r *saluti*)
             (when (match (first r) input)
                (return 
                  (subs  (random-elt(cdr r)))))))
                 

   ;----------------------------------------------------------------------------------             
     (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *domande-generali*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          ))
              )
       )
     )
    ;----------------------------------------------------------------------------------  
     (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *domande-opere*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          )))
        
       )
     )
     
  ;----------------------------------------------------------------------------------  
   (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *domande-musiche-di-scena*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          )))
        
       )
     )
     
  ;----------------------------------------------------------------------------------  
     (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *musica-sacra*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          )))
        
       )
     )
     
  ;----------------------------------------------------------------------------------  

      (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *musica-orchestrale*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          )))
        
       )
     )
     
  ;---------------------------------------------------------------------------------- 
      (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *musica-per-pianoforte*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          )))
        
       )
     )
  
  ;----------------------------------------------------------------------------------             
     (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *musica-concertante*)
		        (when (match (first q) input)
		           (return 
		            (subs (extract (cdr q) 0)))         
		          ))
              )
       )
     )
  ;----------------------------------------------------------------------------------   
      (cond
       ( 
         (equal res1 nil)
         
         (setq res1
		     (dolist (q *altro*)
		        (when (match (first q) input)
		           (return 
		            (subs (random-elt(cdr q))))         
		          )))
       )
     )
     
  ;----------------------------------------------------------------------------------  
            ;ritorniamo la prima risposta 
            (cond
             ( (not (equal res1 nil)) (write res1));da togliere se aggiungiamo roba innestata
            )
           
            )
    )
)


(main) 