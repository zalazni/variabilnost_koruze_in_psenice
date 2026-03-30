# variabilnost_koruze_in_psenice

V kodi Kazalniki_Climind_ agroclim_v5.r se opravijo izračuni:
- kazalnikov za koruzo za obdobje 1981–2010 (kazalniki_OPSI_historical_ GS.rds), 
- za lokaciji Jablje in Rakičan za obdobje 2011-2022 (kazalniki_Jablje_Rakican _2011-2022_GS.rds) in
- izračuni pragov Tmin90p, Tmax90p in Tmax99p za 1981-2010, ki so kasneje uporabljeni v izračunih projekcij kazalnikov (tx90p_referencno_obdobje.rds, tn90p_referencno_obdobje.rds, tx99p_referencno_obdobje.rds)

V kodi pca_analiza_spremenljivk_v5.r se opravijo izračuni:
- Izračun z vrednosti in glavnih komponent za koruzo (kazalniki_1981-2010.rds),
- prikaz trendov in korelacije pridelka z glavnimi komponentami, PCA analiza za obdobje 1981-2010 (slike v mapi faktorji)

V kodi kazalniki_projekcije.r se opravijo izračuni:
- projekcije kazalnikov in faktorjev (kazalniki_71-00_rcp45.rds, kazalniki_71-00_rcp85.rds, kazalniki_41-70_rcp45.rds, kazalniki_41-70_rcp85.rds, kazalniki_11-40_rcp45.rds, kazalniki_11-40_rcp45.rds),
- risanje vseh kart v obliki min, me, max modelov (slike v mapi karte)
