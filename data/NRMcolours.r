oldcolours <- TRUE
if (oldcolours){
  scencolvals <- c(rcp26=120, rcp45=50,rcp60=240,rcp85=13)
  ## scenario colours with old lime green
  scencols <- c(grDevices::grey(0.6), grDevices::hcl(scencolvals, l=c(50,65,50,50)+20, c=c(50,65,50,50)-10))
  scencols2 <- c(grDevices::grey(0.3), grDevices::hcl(scencolvals, l=c(50,65,50,50)-5, c=c(50,65,50,50)+5))
  scencols3 <- c(grDevices::grey(0.8), grDevices::hcl(scencolvals, l=c(50,65,50,50)+35, c=c(50,65,50,50)-20))
  ## new hunter's green for RCP2.6
  ## scencols <- c(grDevices::grey(0.6), grDevices::hcl(scencolvals, l=c(50,65,50,50)+20, c=c(40,65,50,50)-10))
  ## scencols2 <- c(grDevices::grey(0.3), grDevices::hcl(scencolvals, l=c(50,65,50,50)-5, c=c(40,65,50,50)+5))
  ## scencols3 <- c(grDevices::grey(0.8), grDevices::hcl(scencolvals, l=c(50,65,50,50)+35, c=c(40,65,50,50)-20))
  names(scencols3) <- names(scencols2) <- names(scencols) <- c('historical', 'rcp26', 'rcp45', 'rcp60', 'rcp85')
  blackcol <- 1
  modcol <- 'darkgrDevices::grey'
  modcol2 <- 'black'
  obscol <- grDevices::hcl(300, l=50, c=70)
  heatcols <- grDevices::hcl(c(240, 0), l=60, c=40)
  heatcols2 <- grDevices::hcl(c(240, 0), l=30, c=80)
  wetcols <- grDevices::hcl(c(40, 240), l=60, c=40)
  wetcols2 <- grDevices::hcl(c(40, 240), l=30, c=80)
} else {
  ## new scenario colours (from light to dark)
  ## scencols3 <- c(historical='#a4abb6', rcp26='#98acc9', rcp60='#F0c8a1', rcp45='#f3dfab', rcp85='#C2ac98')
  ## scencols <- c(historical='#7e8999', rcp26='#5177a1', rcp60='#e29856', rcp45='#ebc868', rcp85='#8a7360')
  ## scencols2 <- c(historical='#647484', rcp26='#004975', rcp60='#d6771a', rcp45='#ddb307', rcp85='#615142')
  .fincolvals <- round(rbind(
    c(102, 62, 81)*1.2,
    c(136, 96, 113)*1.3,
    c(198, 177, 188)*1.2,
    c(10, 73, 118)*1.2,
    c(96, 130, 162)*1.3,
    c(162, 183, 202)*1.2,
    c(185, 109, 76),
    c(206, 153, 107),
    c(240, 210, 182),
    c(155, 152, 0)*0.9,
    c(199, 198, 89)*0.95,
    c(215, 214, 153)*1.05,
    c(101, 117, 133), 
    c(127, 138, 154)*1.1,
    c(164, 171, 182)*1.1))
  scencols <- rgb(.fincolvals[seq(2,15,3),], maxColorValue=251) ## mid tone
  scencols2 <- rgb(.fincolvals[seq(1,15,3),], maxColorValue=251) ## dar for median
  scencols3 <- rgb(.fincolvals[seq(3,15,3),], maxColorValue=251) ## light for outer shading
  names(scencols) <- names(scencols2) <- names(scencols3) <- c('rcp85', 'rcp45', 'rcp60', 'rcp26', 'historical') 
  blackcol <- '#303E36'
  ## modcol <- '#7e8999'
  modcol <- '#7e8999'
  modcol2 <- '#c2ac98'
  ## obscol <- '#88556c'
  obscol <- blackcol
  #   scencols3 <- c(historical='#a4abb6', rcp26='#E0E0AD', rcp60='#F3DFAB', rcp45='#F0CBA1', rcp85='#C6A6B2')
  #   scencols <- c(historical='#7e8999', rcp26='#D7D58B', rcp60='#EBCB68', rcp45='#E29856', rcp85='#88556C')
  #   scencols2 <- c(historical='#647484', rcp26='#C3C150', rcp60='#DDB307', rcp45='#D6771A', rcp85='#653E51')
  #   blackcol <- '#303E36'
  #   modcol <- '#7e8999'
  #   modcol2 <- '#303E36'
  #   obscol <- '#653e51'
  heatcols <- c('#5177a1', '#88556c')
  heatcols2 <- c('#004975', '#653e51')
  wetcols <- c('#8a7360', '#5177a1')
  wetcols2 <- c('#615142', '#004975')
}
## names for scenarios
## scennames <- c(historical='Natural variability only', rcp85='Highest scenario (RCP8.5)', rcp60='High scenario (RCP6.0)', rcp45='Intermediate scenario (RCP4.5)', rcp26='Lowest scenario (RCP2.6)')
scennames <- c(historical='Natural variability only', rcp85='RCP8.5', rcp60='RCP6.0', rcp45='RCP4.5', rcp26='RCP2.6')
seastxt <- c(DJF='Summer', MAM='Autumn', JJA='Winter', SON='Spring', ANNUAL='Annual', NDJFMA='Summer\nhalf year', MJJASO='Winter\nhalf year', NDJFM='Summer\nhalf year\n(Nov.-Mar.)', AMJJASO='Winter\nhalf year\n(Apr.-Oct.)', `10`='>10 deg. C', `15`='>15 deg. C', `20`='>20 deg. C', `25`='>25 deg. C') 

## long names for NRM clusters
regtxt <- c(MN='Monsoonal North', MNW='Monsoonal North West', MNE='Monsoonal North East', WT='Wet Tropics', R='Rangelands', RE='Rangelands East', RW='Rangelands West', CS='Central Slopes', MB='Murray Basin', EC='East Coast', SS='Southern Slopes', SSWF='Southern and South-Western Flatlands', ECS='East Coast South', ECN='East Coast North', SSWFE='Southern and South-Western Flatlands East', SSWFW='Southern and South-Western Flatlands West', SSTE='Southern Slopes Tasmania East', SSTW='Southern Slopes Tasmania West', SSVW='Southern Slopes Victoria West', SSVE='Southern Slopes Victoria East', SSTW2='Southern Slopes Tasmania West (old)', SSTE2='Southern Slopes Tasmania East (old)', RN='Rangelands North', RS='Rangelands South', EA='Eastern Australia', SA='Southern Australia', `NA`='Northern Australia', north='northern Vietnam', mid='central Vietnam', south='southern Vietnam', GLO='Global land', AUS='Australia', AMZ='Amazon Basin', SSA='Southern South America', CAM='Central America', WNA='Western North America', CNA='Central North America', ENA='Eastern North America', ALA='Alaska', GRL='Greenland', MED='Mediterranean Basin', NEU='Northern Europe', WAF='Western Africa', EAF='Eastern Africa', SAF='Southern Africa', SAH='Sahara', SEA='Southeast Asia', EAS='East Asia', SAS='South Asia', CAS='Central Asia', TIB='Tibet', NAS='North Asia', SAU='Southern Australia', NAU='Northern Australia', SEAUS='Southeast Australia', SWAUS='Southwestern Western Australia', NWAUS='Northern Western Australia', TNAFR='Northern Africa and southern Europe (land and ocean)', TINDO='Central Indonesian Ocean', TTIBE='Tibet and Central Asia', NHILA='Northern Hemisphere high latitudes', SHILA='Southern Hemisphere high latitudes', TROLA='Tropical land', TROAF='Tropical Africa (land only)', TROIN='Indonesia (land only)', TGLO='Global (land and ocean north of 60S)', OCEAN='Global ocean', NHMEA='Northern Hemisphere', SHMEA='Southern Hemisphere', MER1='Northern Hemisphere highlat', MER2='Northern Hemisphere lowlat')
