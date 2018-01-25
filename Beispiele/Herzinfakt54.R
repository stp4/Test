#- Herzinfakt Seite 54
library(stp25vers)
Projekt("html", "buch")
set_my_options(prozent=list(digits=c(1,0), style=2))
Herz<-GetData("id	Outcome	Behandlung	Infarktlokalisation
1	verstorben	APSAC	Hinterwand
              2	verstorben	APSAC	Hinterwand
              3	verstorben	APSAC	Hinterwand
              4	verstorben	APSAC	Vorderwand
              5	verstorben	APSAC	Vorderwand
              6	verstorben	APSAC	Vorderwand
              7	verstorben	APSAC	Vorderwand
              8	verstorben	APSAC	Vorderwand
              9	verstorben	APSAC	Vorderwand
              10	verstorben	Heparin	Hinterwand
              11	verstorben	Heparin	Hinterwand
              12	verstorben	Heparin	Hinterwand
              13	verstorben	Heparin	Hinterwand
              14	verstorben	Heparin	Hinterwand
              15	verstorben	Heparin	Hinterwand
              16	verstorben	Heparin	Hinterwand
              17	verstorben	Heparin	Hinterwand
              18	verstorben	Heparin	Hinterwand
              19	verstorben	Heparin	Hinterwand
              20	verstorben	Heparin	Hinterwand
              21	verstorben	Heparin	Vorderwand
              22	verstorben	Heparin	Vorderwand
              23	verstorben	Heparin	Vorderwand
              24	verstorben	Heparin	Vorderwand
              25	verstorben	Heparin	Vorderwand
              26	verstorben	Heparin	Vorderwand
              27	verstorben	Heparin	Vorderwand
              28	verstorben	Heparin	Vorderwand
              29	ueberlebt	APSAC	Hinterwand
              30	ueberlebt	APSAC	Hinterwand
              31	ueberlebt	APSAC	Hinterwand
              32	ueberlebt	APSAC	Hinterwand
              33	ueberlebt	APSAC	Hinterwand
              34	ueberlebt	APSAC	Hinterwand
              35	ueberlebt	APSAC	Hinterwand
              36	ueberlebt	APSAC	Hinterwand
              37	ueberlebt	APSAC	Hinterwand
              38	ueberlebt	APSAC	Hinterwand
              39	ueberlebt	APSAC	Hinterwand
              40	ueberlebt	APSAC	Hinterwand
              41	ueberlebt	APSAC	Hinterwand
              42	ueberlebt	APSAC	Hinterwand
              43	ueberlebt	APSAC	Hinterwand
              44	ueberlebt	APSAC	Hinterwand
              45	ueberlebt	APSAC	Hinterwand
              46	ueberlebt	APSAC	Hinterwand
              47	ueberlebt	APSAC	Hinterwand
              48	ueberlebt	APSAC	Hinterwand
              49	ueberlebt	APSAC	Hinterwand
              50	ueberlebt	APSAC	Hinterwand
              51	ueberlebt	APSAC	Hinterwand
              52	ueberlebt	APSAC	Hinterwand
              53	ueberlebt	APSAC	Hinterwand
              54	ueberlebt	APSAC	Hinterwand
              55	ueberlebt	APSAC	Hinterwand
              56	ueberlebt	APSAC	Hinterwand
              57	ueberlebt	APSAC	Hinterwand
              58	ueberlebt	APSAC	Hinterwand
              59	ueberlebt	APSAC	Hinterwand
              60	ueberlebt	APSAC	Hinterwand
              61	ueberlebt	APSAC	Hinterwand
              62	ueberlebt	APSAC	Hinterwand
              63	ueberlebt	APSAC	Hinterwand
              64	ueberlebt	APSAC	Hinterwand
              65	ueberlebt	APSAC	Hinterwand
              66	ueberlebt	APSAC	Hinterwand
              67	ueberlebt	APSAC	Hinterwand
              68	ueberlebt	APSAC	Hinterwand
              69	ueberlebt	APSAC	Hinterwand
              70	ueberlebt	APSAC	Hinterwand
              71	ueberlebt	APSAC	Hinterwand
              72	ueberlebt	APSAC	Hinterwand
              73	ueberlebt	APSAC	Hinterwand
              74	ueberlebt	APSAC	Hinterwand
              75	ueberlebt	APSAC	Hinterwand
              76	ueberlebt	APSAC	Hinterwand
              77	ueberlebt	APSAC	Hinterwand
              78	ueberlebt	APSAC	Hinterwand
              79	ueberlebt	APSAC	Hinterwand
              80	ueberlebt	APSAC	Hinterwand
              81	ueberlebt	APSAC	Hinterwand
              82	ueberlebt	APSAC	Hinterwand
              83	ueberlebt	APSAC	Hinterwand
              84	ueberlebt	APSAC	Hinterwand
              85	ueberlebt	APSAC	Hinterwand
              86	ueberlebt	APSAC	Hinterwand
              87	ueberlebt	APSAC	Hinterwand
              88	ueberlebt	APSAC	Hinterwand
              89	ueberlebt	APSAC	Hinterwand
              90	ueberlebt	APSAC	Hinterwand
              91	ueberlebt	APSAC	Hinterwand
              92	ueberlebt	APSAC	Hinterwand
              93	ueberlebt	APSAC	Hinterwand
              94	ueberlebt	APSAC	Hinterwand
              95	ueberlebt	APSAC	Hinterwand
              96	ueberlebt	APSAC	Hinterwand
              97	ueberlebt	APSAC	Hinterwand
              98	ueberlebt	APSAC	Hinterwand
              99	ueberlebt	APSAC	Hinterwand
              100	ueberlebt	APSAC	Hinterwand
              101	ueberlebt	APSAC	Hinterwand
              102	ueberlebt	APSAC	Hinterwand
              103	ueberlebt	APSAC	Hinterwand
              104	ueberlebt	APSAC	Hinterwand
              105	ueberlebt	APSAC	Hinterwand
              106	ueberlebt	APSAC	Hinterwand
              107	ueberlebt	APSAC	Hinterwand
              108	ueberlebt	APSAC	Hinterwand
              109	ueberlebt	APSAC	Hinterwand
              110	ueberlebt	APSAC	Hinterwand
              111	ueberlebt	APSAC	Hinterwand
              112	ueberlebt	APSAC	Hinterwand
              113	ueberlebt	APSAC	Hinterwand
              114	ueberlebt	APSAC	Hinterwand
              115	ueberlebt	APSAC	Vorderwand
              116	ueberlebt	APSAC	Vorderwand
              117	ueberlebt	APSAC	Vorderwand
              118	ueberlebt	APSAC	Vorderwand
              119	ueberlebt	APSAC	Vorderwand
              120	ueberlebt	APSAC	Vorderwand
              121	ueberlebt	APSAC	Vorderwand
              122	ueberlebt	APSAC	Vorderwand
              123	ueberlebt	APSAC	Vorderwand
              124	ueberlebt	APSAC	Vorderwand
              125	ueberlebt	APSAC	Vorderwand
              126	ueberlebt	APSAC	Vorderwand
              127	ueberlebt	APSAC	Vorderwand
              128	ueberlebt	APSAC	Vorderwand
              129	ueberlebt	APSAC	Vorderwand
              130	ueberlebt	APSAC	Vorderwand
              131	ueberlebt	APSAC	Vorderwand
              132	ueberlebt	APSAC	Vorderwand
              133	ueberlebt	APSAC	Vorderwand
              134	ueberlebt	APSAC	Vorderwand
              135	ueberlebt	APSAC	Vorderwand
              136	ueberlebt	APSAC	Vorderwand
              137	ueberlebt	APSAC	Vorderwand
              138	ueberlebt	APSAC	Vorderwand
              139	ueberlebt	APSAC	Vorderwand
              140	ueberlebt	APSAC	Vorderwand
              141	ueberlebt	APSAC	Vorderwand
              142	ueberlebt	APSAC	Vorderwand
              143	ueberlebt	APSAC	Vorderwand
              144	ueberlebt	APSAC	Vorderwand
              145	ueberlebt	APSAC	Vorderwand
              146	ueberlebt	APSAC	Vorderwand
              147	ueberlebt	APSAC	Vorderwand
              148	ueberlebt	APSAC	Vorderwand
              149	ueberlebt	APSAC	Vorderwand
              150	ueberlebt	APSAC	Vorderwand
              151	ueberlebt	APSAC	Vorderwand
              152	ueberlebt	APSAC	Vorderwand
              153	ueberlebt	APSAC	Vorderwand
              154	ueberlebt	APSAC	Vorderwand
              155	ueberlebt	APSAC	Vorderwand
              156	ueberlebt	APSAC	Vorderwand
              157	ueberlebt	APSAC	Vorderwand
              158	ueberlebt	APSAC	Vorderwand
              159	ueberlebt	APSAC	Vorderwand
              160	ueberlebt	APSAC	Vorderwand
              161	ueberlebt	APSAC	Vorderwand
              162	ueberlebt	APSAC	Vorderwand
              163	ueberlebt	APSAC	Vorderwand
              164	ueberlebt	APSAC	Vorderwand
              165	ueberlebt	APSAC	Vorderwand
              166	ueberlebt	APSAC	Vorderwand
              167	ueberlebt	APSAC	Vorderwand
              168	ueberlebt	APSAC	Vorderwand
              169	ueberlebt	APSAC	Vorderwand
              170	ueberlebt	APSAC	Vorderwand
              171	ueberlebt	APSAC	Vorderwand
              172	ueberlebt	APSAC	Vorderwand
              173	ueberlebt	APSAC	Vorderwand
              174	ueberlebt	APSAC	Vorderwand
              175	ueberlebt	APSAC	Vorderwand
              176	ueberlebt	APSAC	Vorderwand
              177	ueberlebt	APSAC	Vorderwand
              178	ueberlebt	APSAC	Vorderwand
              179	ueberlebt	APSAC	Vorderwand
              180	ueberlebt	APSAC	Vorderwand
              181	ueberlebt	APSAC	Vorderwand
              182	ueberlebt	Heparin	Vorderwand
              183	ueberlebt	Heparin	Vorderwand
              184	ueberlebt	Heparin	Vorderwand
              185	ueberlebt	Heparin	Vorderwand
              186	ueberlebt	Heparin	Vorderwand
              187	ueberlebt	Heparin	Vorderwand
              188	ueberlebt	Heparin	Vorderwand
              189	ueberlebt	Heparin	Vorderwand
              190	ueberlebt	Heparin	Vorderwand
              191	ueberlebt	Heparin	Hinterwand
              192	ueberlebt	Heparin	Hinterwand
              193	ueberlebt	Heparin	Hinterwand
              194	ueberlebt	Heparin	Hinterwand
              195	ueberlebt	Heparin	Hinterwand
              196	ueberlebt	Heparin	Hinterwand
              197	ueberlebt	Heparin	Hinterwand
              198	ueberlebt	Heparin	Hinterwand
              199	ueberlebt	Heparin	Hinterwand
              200	ueberlebt	Heparin	Hinterwand
              201	ueberlebt	Heparin	Hinterwand
              202	ueberlebt	Heparin	Hinterwand
              203	ueberlebt	Heparin	Hinterwand
              204	ueberlebt	Heparin	Hinterwand
              205	ueberlebt	Heparin	Hinterwand
              206	ueberlebt	Heparin	Hinterwand
              207	ueberlebt	Heparin	Hinterwand
              208	ueberlebt	Heparin	Hinterwand
              209	ueberlebt	Heparin	Hinterwand
              210	ueberlebt	Heparin	Hinterwand
              211	ueberlebt	Heparin	Hinterwand
              212	ueberlebt	Heparin	Hinterwand
              213	ueberlebt	Heparin	Hinterwand
              214	ueberlebt	Heparin	Hinterwand
              215	ueberlebt	Heparin	Hinterwand
              216	ueberlebt	Heparin	Hinterwand
              217	ueberlebt	Heparin	Hinterwand
              218	ueberlebt	Heparin	Hinterwand
              219	ueberlebt	Heparin	Hinterwand
              220	ueberlebt	Heparin	Hinterwand
              221	ueberlebt	Heparin	Hinterwand
              222	ueberlebt	Heparin	Hinterwand
              223	ueberlebt	Heparin	Hinterwand
              224	ueberlebt	Heparin	Hinterwand
              225	ueberlebt	Heparin	Hinterwand
              226	ueberlebt	Heparin	Hinterwand
              227	ueberlebt	Heparin	Hinterwand
              228	ueberlebt	Heparin	Hinterwand
              229	ueberlebt	Heparin	Hinterwand
              230	ueberlebt	Heparin	Hinterwand
              231	ueberlebt	Heparin	Hinterwand
              232	ueberlebt	Heparin	Hinterwand
              233	ueberlebt	Heparin	Hinterwand
              234	ueberlebt	Heparin	Hinterwand
              235	ueberlebt	Heparin	Hinterwand
              236	ueberlebt	Heparin	Hinterwand
              237	ueberlebt	Heparin	Hinterwand
              238	ueberlebt	Heparin	Hinterwand
              239	ueberlebt	Heparin	Hinterwand
              240	ueberlebt	Heparin	Hinterwand
              241	ueberlebt	Heparin	Hinterwand
              242	ueberlebt	Heparin	Hinterwand
              243	ueberlebt	Heparin	Hinterwand
              244	ueberlebt	Heparin	Hinterwand
              245	ueberlebt	Heparin	Hinterwand
              246	ueberlebt	Heparin	Hinterwand
              247	ueberlebt	Heparin	Hinterwand
              248	ueberlebt	Heparin	Hinterwand
              249	ueberlebt	Heparin	Hinterwand
              250	ueberlebt	Heparin	Hinterwand
              251	ueberlebt	Heparin	Hinterwand
              252	ueberlebt	Heparin	Hinterwand
              253	ueberlebt	Heparin	Hinterwand
              254	ueberlebt	Heparin	Hinterwand
              255	ueberlebt	Heparin	Hinterwand
              256	ueberlebt	Heparin	Hinterwand
              257	ueberlebt	Heparin	Hinterwand
              258	ueberlebt	Heparin	Hinterwand
              259	ueberlebt	Heparin	Hinterwand
              260	ueberlebt	Heparin	Vorderwand
              261	ueberlebt	Heparin	Vorderwand
              262	ueberlebt	Heparin	Vorderwand
              263	ueberlebt	Heparin	Vorderwand
              264	ueberlebt	Heparin	Vorderwand
              265	ueberlebt	Heparin	Vorderwand
              266	ueberlebt	Heparin	Vorderwand
              267	ueberlebt	Heparin	Vorderwand
              268	ueberlebt	Heparin	Vorderwand
              269	ueberlebt	Heparin	Vorderwand
              270	ueberlebt	Heparin	Vorderwand
              271	ueberlebt	Heparin	Vorderwand
              272	ueberlebt	Heparin	Vorderwand
              273	ueberlebt	Heparin	Vorderwand
              274	ueberlebt	Heparin	Vorderwand
              275	ueberlebt	Heparin	Vorderwand
              276	ueberlebt	Heparin	Vorderwand
              277	ueberlebt	Heparin	Vorderwand
              278	ueberlebt	Heparin	Vorderwand
              279	ueberlebt	Heparin	Vorderwand
              280	ueberlebt	Heparin	Vorderwand
              281	ueberlebt	Heparin	Vorderwand
              282	ueberlebt	Heparin	Vorderwand
              283	ueberlebt	Heparin	Vorderwand
              284	ueberlebt	Heparin	Vorderwand
              285	ueberlebt	Heparin	Vorderwand
              286	ueberlebt	Heparin	Vorderwand
              287	ueberlebt	Heparin	Vorderwand
              288	ueberlebt	Heparin	Vorderwand
              289	ueberlebt	Heparin	Vorderwand
              290	ueberlebt	Heparin	Vorderwand
              291	ueberlebt	Heparin	Vorderwand
              292	ueberlebt	Heparin	Vorderwand
              293	ueberlebt	Heparin	Vorderwand
              294	ueberlebt	Heparin	Vorderwand
              295	ueberlebt	Heparin	Vorderwand
              296	ueberlebt	Heparin	Vorderwand
              297	ueberlebt	Heparin	Vorderwand
              298	ueberlebt	Heparin	Vorderwand
              299	ueberlebt	Heparin	Vorderwand
              300	ueberlebt	Heparin	Vorderwand
              301	ueberlebt	Heparin	Vorderwand
              302	ueberlebt	Heparin	Vorderwand
              303	ueberlebt	Heparin	Vorderwand
              304	ueberlebt	Heparin	Vorderwand
              305	ueberlebt	Heparin	Vorderwand
              306	ueberlebt	Heparin	Vorderwand
              307	ueberlebt	Heparin	Vorderwand
              308	ueberlebt	Heparin	Vorderwand
              309	ueberlebt	Heparin	Vorderwand
              310	ueberlebt	Heparin	Vorderwand
              311	ueberlebt	Heparin	Vorderwand
              312	ueberlebt	Heparin	Vorderwand
313	ueberlebt	Heparin	Vorderwand
               ")
Herz$Outcome<- factor(Herz$Outcome, rev(levels(Herz$Outcome)) )
Herz$Age<- cut(rnorm(nrow(Herz)), 3 , Cs(low, med, hig))

Herz %>% Tabelle2(Outcome, by=~ Behandlung)
xtab1 <- xtabs(~Age+Outcome, Herz)
xtab2 <- xtabs(~Behandlung+Outcome+Infarktlokalisation, Herz)

xtab <- xtabs(~Behandlung+Outcome, Herz)

dim(xtab)

 # APA2(xtab   )
 # APA2(xtab,  margin=1)
 # APA2(xtab,  margin=2)
 # APA2(xtab, include.total.columns=TRUE, caption="include.total.columns")
 # APA2(xtab, include.total.rows=TRUE, caption="include.total.rows ")
 # APA2(xtab, include.total=TRUE, caption="include.total")



 # dim(xtab1)
 # APA2(xtab1   )
 # APA2(xtab1,  margin=1)
 # APA2(xtab1,  margin=2)
 # APA2(xtab1, include.total.columns=TRUE, caption="include.total.columns")
 # APA2(xtab1, include.total.rows=TRUE, caption="include.total.rows ")
 # APA2(xtab1, include.total=TRUE, caption="include.total")
 #


 # dim(xtab2)


ftable(addmargins(xtab2, 3)  )
 # APA2(xtab2   )
 # APA2(xtab2,  margin=1)
 # APA2(xtab2,  margin=2)

APA2(xtabs(~Outcome+Behandlung , Herz),
     include.total.columns=TRUE,
     caption="include.total.columns")
 APA2(xtabs(~Outcome+Infarktlokalisation+Behandlung, Herz),
        include.total.columns=TRUE, caption="include.total.columns")
 APA2(xtabs(~Outcome+Infarktlokalisation+Behandlung, Herz),
      include.total.columns=TRUE,
      include.total.sub=TRUE,
      caption="include.total.columns + include.total.sub")


# APA2(xtabs(~Outcome+Infarktlokalisation+Behandlung, Herz),
#      include.total=TRUE, caption="include.total ", margin=1)

 APA2(xtab2, include.total.rows=TRUE, caption="include.total.rows ")
 APA2(xtab2, include.total.rows=TRUE, include.total.sub=TRUE,
      caption="include.total.rows + include.total.sub")

 #APA2(xtab2, include.total=TRUE, caption="include.total")


#APA2(xtab, margin=1)
#APA2(xtab, margin=2)

#APA2(xtab, type="fischer", test=TRUE)

fit<- glm(Outcome~Behandlung, Herz, family = binomial())
fit2<- glm(Outcome~Behandlung+Infarktlokalisation, Herz, family = binomial())
#APA_Table(fit, include.odds = TRUE)
#APA_Table(fit2, include.odds = TRUE)

#Herz %>% Tabelle2(Outcome, by=~ Behandlung+Infarktlokalisation)


End()
