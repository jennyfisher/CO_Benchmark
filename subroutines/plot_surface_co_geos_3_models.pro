; $Id: plot_surface_co_geos_3_models.pro,v 1.4 2010/10/04 15:21:23 bmy Exp $
pro plot_surface_co_geos_3_models, pref1,  ptop1,  dlat1,   dlon1, model1, $
                                   pref2,  ptop2,  dlat2,   dlon2, model2, $
                                   pref3,  ptop3,  dlat3,   dlon3, model3, $
                                   cat1,   cat2,   cat3,                   $
                                   PLabel1,  PLabel2,   PLabel3,           $
                                   title,  psname, max_sta, filest, debug=debug

   ; For a given set of stations compares CO surface data from cmdl (black
   ; solid line) with surface data from 3 models - with maccm3, dao and giss 
   ; winds, plotted with linestyles 1 to 3 and colors red, green and blue 
   ; correspondently
   ; 
   ; NOTE: Uses GET_PRESSURE_GEOS and GET_SPECIES_GEOS, which can read
   ;       both GEOS-3 and GEOS-4 met fields.  Also updated comments
   ;       and made cosmetic changes. (bmy, 3/8/05)
   ;
   ; NOTE: Now use CTM_INDEX to pick out proper lon & lat indices
   ;       since this works correctly regardless of model (bmy, 7/11/07)
   ;
   ; NOTE: Now use correct station pressures as taken from the file
   ;       "input/cmdl.sigma.levels" (bmy, 12/11/07)
   ;
   ; NOTE: Now reads updated files (lzh, bmy, 5/23/08)

   ; Get defaults (bmy, 6/7/11)
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Plot settings
   !X.OMARGIN=[8,6] 
   !Y.OMARGIN=[6,6]
   !X.THICK=4
   !Y.THICK=4
   !P.CHARTHICK=2.5
   !P.THICK=2.5
   
   Species='CO'

   ; Open file with stations
   openr, usta, filest, /get_lun
   iname_sta=''
   ititle_sta=''
   ipref_sta=''
   
   mmonth = strarr(12)
   mmonth=['Jan','Feb','Mar','Apr','May','Jun',$
           'Jul','Aug','Sep','Oct','Nov','Dec']

   ;color plot
   open_device, olddevice,/ps,/color,filename=psname 
   !p.font=0

   ; Specify directory with surface data
   ; ccc, 10/01/2010 use updated CO data for 2005
   pre = './data/cmdl/Feb09/'

   ; Get MODELINFO structures for the 3 models
   Type1 = CTM_Type( Model1, Res=[ DLon1, DLat1 ] )
   Type2 = CTM_Type( Model2, Res=[ DLon2, DLat2 ] )
   Type3 = CTM_Type( Model3, Res=[ DLon3, DLat3 ] )

   ;========================================================================
   ; --- read station & indice ---
   ;========================================================================

   ; Read in information about stations to be plotted -
   ; 3-letter name(capital), station name, latitude and longitude
   name_sta = strarr(max_sta)
   lon_sta      = fltarr(max_sta)
   lat_sta      = fltarr(max_sta)
   lon_sta_1      = fltarr(max_sta)
   lat_sta_1      = fltarr(max_sta)
   title_sta = strarr(max_sta)
   pref_sta = strarr(max_sta)
   
   for i=0,max_sta-1 do begin
      readf,usta, iname_sta,ititle_sta,  ilat, ilon, ipref_sta,        $
         format='(2x,a3,7x,a15,2x,f6.2,3x,f6.2,4x,a3)'
      name_sta(i) = iname_sta
      lon_sta_1(i)      = round(ilon)
      lat_sta_1(i)      = round(ilat)
      lon_sta(i)      = ilon
      lat_sta(i)      = ilat
      title_sta(i) = ititle_sta
      pref_sta[i] = ipref_sta
   endfor
   
   nrow=4
   ncol=4
   !P.Multi = [0,nrow,ncol,1,0]

   ;=====================================================================
   ; Read and store data from models
   ;=====================================================================
   for i=0,11 do begin

      ; Month name
      mn=strtrim(String(fix(i+1)),2)
      if (strlen(mn) eq 1) then begin
         mn='0'+mn
      endif

      ;==========
      ; 1st model
      ;==========
      name=pref1+mn+'01.nc'
      Pname=Plabel1+mn+'01.nc'
      
      if keyword_set(debug) then print,' -- Reading model file '+name

      ; Get CO
      COtmp = Get_Species_Geos( name, Date=Date, $
                                Species=Cat1+'CO', Lat=Lat, Lon=Lon )

      ; Get pressure
      Pressuretmp = Get_Pressure_Geos( PName, PTOP=PTOP1, Lat=Lat, Lon=Lon )

      if i eq 0 then begin
         D = size(COtmp,/dim)
         CO1 = fltarr(D[0],D[1],D[2],12)
         D = size(Pressuretmp,/dim)
         Pressure1 = fltarr(D[0],D[1],D[2],12)
      endif

      CO1[*,*,*,i] = COtmp
      Pressure1[*,*,*,i] = Pressuretmp
                  
      ;==========
      ; 2nd model
      ;==========
      name=pref2+mn+'01.nc'
      Pname=Plabel2+mn+'01.nc'
      
      if keyword_set(debug) then print,' -- Reading model file '+name

      ; Get CO
      COtmp = Get_Species_Geos( name, Date=Date, $
                                Species=Cat2+'CO', Lat=Lat, Lon=Lon )

      ; Get pressure
      Pressuretmp = Get_Pressure_Geos( PName, PTOP=PTOP2, Lat=Lat, Lon=Lon )

      if i eq 0 then begin
         D = size(COtmp,/dim)
         CO2 = fltarr(D[0],D[1],D[2],12)
         D = size(Pressuretmp,/dim)
         Pressure2 = fltarr(D[0],D[1],D[2],12)
      endif

      CO2[*,*,*,i] = COtmp
      Pressure2[*,*,*,i] = Pressuretmp

      ;==========
      ; 3rd model
      ;==========
      name=pref3+mn+'01.nc'
      Pname=Plabel3+mn+'01.nc'
      
      if keyword_set(debug) then print,' -- Reading model file '+name

      ; Get CO
      COtmp = Get_Species_Geos( name, Date=Date, $
                                Species=Cat3+'CO', Lat=Lat, Lon=Lon )

      ; Get pressure
      Pressuretmp = Get_Pressure_Geos( PName, PTOP=PTOP3, Lat=Lat, Lon=Lon )

      if i eq 0 then begin
         D = size(COtmp,/dim)
         CO3 = fltarr(D[0],D[1],D[2],12)
         D = size(Pressuretmp,/dim)
         Pressure3 = fltarr(D[0],D[1],D[2],12)
      endif

      CO3[*,*,*,i] = COtmp
      Pressure3[*,*,*,i] = Pressuretmp

   endfor

   ;========================================================================
   ; ---  open files ---
   ;========================================================================
   ncount=0
   
   ; --- loop for stations ---
   for k = 1, max_sta do begin

      ncount=ncount+1
      kk = k-1 
      ix = k
      file=''

    ;  file=pre+pref_sta(kk)+'.mn'
    ; lzh 04/28/2008  change to use 2005 CO data
      file=pre+pref_sta(kk)+'.mn.2005'
      if keyword_set(debug) then print,' -- Reading station file '+file
      ilun = k+50
      openr,ilun,file

      maxd = 12
      comean   = fltarr(maxd)
      comedian = fltarr(maxd)
      costd    = fltarr(maxd)
      conum    = fltarr(maxd)
      comin    = fltarr(maxd)
      comax    = fltarr(maxd)
    
      for i=0,11 do begin
         readf,ilun,                                             $
            icomean, icostd,inum, icomin, icomax,icomedian    
         conum(i)    = inum
         comean(i)   = icomean
         comedian(i) = icomedian
         if icostd eq -999 then costd(i)=!Values.f_nan else costd(i) = icostd
         comin(i)    =icomin
         comax(i)    =icomax
      endfor
      
      close, ilun

      ; Put longitude in (-180,180) range
      if lon_sta_1(kk) gt 180 then lon_sta_1(kk)=lon_sta_1(kk)-360
      if lon_sta(kk) gt 180 then lon_sta(kk)=lon_sta(kk)-360

      ; Create station title
      ltitle=''
      ltitle = strtrim(title_sta(kk),2)+$
         ' ('+strtrim(string(fix(lat_sta_1(kk))),1)+$
         ' ,'+strtrim(string(fix(lon_sta_1(kk))),1)+' )'
      
      ;=====================================================================
      ; Extract data from models
      ;=====================================================================
      out=fltarr(12)
      out2=fltarr(12)
      out3=fltarr(12)

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Center=[ Lat_Sta[KK], Lon_Sta[KK] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      for i=0,11 do begin

         ;==========
         ; 1st model
         ;==========
         CO = CO1
         Pressure = Pressure1

         CO_box = CO[Indlon,Indlat,0,i]
         Pressure_box = Pressure[Indlon,Indlat,0,i]
         out[i]=CO_box
         
         ; NOTE: Use INTERPOL to find the level closest in pressure to
         ;       the given station's surface pressure.  Update the pressures
         ;       according to the cmdl.sigma.levels document (bmy, 12/11/07)

         if (pref_sta(kk) eq 'nwr') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(660.))
         
         endif else if (pref_sta(kk) eq 'izo') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                          -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(760.))

         endif else if (pref_sta(kk) eq 'mlo') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(670.))

         endif else if (pref_sta(kk) eq 'spo') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(700.))

         endif else if (pref_sta(kk) eq 'lef') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(913.))

         endif else if (pref_sta(kk) eq 'uum') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(908.))

         endif else if (pref_sta(kk) eq 'uta') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(865.))

         endif else if (pref_sta(kk) eq 'cui') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(900.))

         endif else if (pref_sta(kk) eq 'wlg') then begin
            out[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(640.))
         endif
      
         ;==========
         ; 2nd model
         ;==========
         CO = CO2
         Pressure = Pressure2

         CO_box = CO[Indlon,Indlat,0,i]
         Pressure_box = Pressure[Indlon,Indlat,0,i]
         out2[i]=CO_box
       
         ; NOTE: Use INTERPOL to find the level closest in pressure to
         ;       the given station's surface pressure.  Update the pressures
         ;       according to the cmdl.sigma.levels document (bmy, 12/11/07)

         if (pref_sta(kk) eq 'nwr') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(660.))
         
         endif else if (pref_sta(kk) eq 'izo') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                          -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(760.))

         endif else if (pref_sta(kk) eq 'mlo') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(670.))

         endif else if (pref_sta(kk) eq 'spo') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(700.))

         endif else if (pref_sta(kk) eq 'lef') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(913.))

         endif else if (pref_sta(kk) eq 'uum') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(908.))

         endif else if (pref_sta(kk) eq 'uta') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(865.))

         endif else if (pref_sta(kk) eq 'cui') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(900.))

         endif else if (pref_sta(kk) eq 'wlg') then begin
            out2[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(640.))
         endif

         ;==========
         ; 3rd model
         ;==========
         CO = CO3
         Pressure = Pressure3

         CO_box = CO[Indlon,Indlat,0,i]
         Pressure_box = Pressure[Indlon,Indlat,0,i]
         out3[i]=CO_box
         
         ; NOTE: Use INTERPOL to find the level closest in pressure to
         ;       the given station's surface pressure.  Update the pressures
         ;       according to the cmdl.sigma.levels document (bmy, 12/11/07)

         if (pref_sta(kk) eq 'nwr') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(660.))
         
         endif else if (pref_sta(kk) eq 'izo') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                          -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(760.))

         endif else if (pref_sta(kk) eq 'mlo') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(670.))

         endif else if (pref_sta(kk) eq 'spo') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(700.))

         endif else if (pref_sta(kk) eq 'lef') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(913.))

         endif else if (pref_sta(kk) eq 'uum') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(908.))

         endif else if (pref_sta(kk) eq 'uta') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(865.))

         endif else if (pref_sta(kk) eq 'cui') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(900.))

         endif else if (pref_sta(kk) eq 'wlg') then begin
            out3[i]=interpol(CO[Indlon,Indlat,*,i],$
                         -alog10(Pressure[Indlon,Indlat,*,i]),-alog10(640.))
         endif
         
      endfor
    
      ;======================================================================
      ; Create the plot
      ;======================================================================
      if keyword_set(debug) then print,' -- Plotting'

      ; Define the range for y axis
      loval=0 
      if  min([comean,out]) gt 40 then loval=20
      if  min([comean,out]) gt 70 then loval=50
      if  min([comean,out]) gt 120 then loval=100
      if  min([comean,out]) gt 170 then loval=150
      if  min([comean,out]) gt 220 then loval=200
      loval = floor(.9*min([comean,out])/10.) *10.
      loval = max([0,loval])
      
      highval=100
      if  max([comean,out]) gt 80 then highval=150
      if  max([comean,out]) gt 130 then highval=200
      if  max([comean,out]) gt 180 then highval=250
      if  max([comean,out]) gt 230 then highval=300
      if  max([comean,out]) gt 280 then highval=350
      if  max([comean,out]) gt 330 then highval=400
      highval = ceil(1.1*max([comean,out])/10.) *10.
      
      if name_sta[kk] eq "BAL" then begin highval=400
      endif
    
      if name_sta[kk] eq "STM" then begin highval=300
      endif
    
      if name_sta[kk] eq "TAP" then begin highval=500
      endif

      ; -- plot observed data --
      plot, findgen(12)+1, comean, xstyle=1,ystyle=1,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
         xticks=13, min_val=-900, xrange=[0,13],yrange=[loval,highval],$
         charsize=1.5, xmargin=[3,2], ymargin=[3,1],color=1,$
         xtickname=[' ','J','F','M','A','M','J','J','A','S','O','N','D',' ']
      
      ; Now plot standard deviations
      for w = 0, 11 do begin
         errbar = [comean[w]-costd[w], comean[w]+costd[w]]
         oplot,  [w+1,w+1],errbar,$
            linestyle=0,color=1
      endfor

      ; 1st model
      oplot, findgen(12)+1,out,linestyle=1,color=2
      oplot, findgen(12)+1,out,linestyle=1,psym=2,symsize=0.3,color=2   

      ; 2nd model
      oplot, findgen(12)+1,out2,linestyle=3,color=3
      oplot, findgen(12)+1,out2,linestyle=3,psym=2,symsize=0.3,color=3   

      ; 3rd model
      oplot, findgen(12)+1,out3,linestyle=2,color=4
      oplot, findgen(12)+1,out3,linestyle=2,psym=2,symsize=0.3,color=4   

      xyouts, 0.04, 0.5, 'CO (ppb)', /normal, align=0.5, orientation=90, $
         charsize=1.2,color=1
      xyouts, 0.5, 0.96,title, /normal, align=0.5, charsize=1.2,color=1

;      absdiff = out-out3
;      reldiff = 100.*absdiff/out3
;      if max(abs(absdiff)) ge 10 then $
;         print,name_sta[kk],' Absolute ',min(absdiff),max(absdiff)
;      if max(abs(reldiff)) ge 10 then $
;         print,name_sta[kk],' Relative ',min(reldiff),max(reldiff)
   endfor

   ; Cleanup & quit 
   close_device, /TIMESTAMP
   
   close, /all

   ; Restore defaults (bmy, 6/7/11)
   !X.OMARGIN   = X_OMARGIN   
   !Y.OMARGIN   = Y_OMARGIN   
   !X.MARGIN    = X_MARGIN    
   !Y.OMARGIN   = Y_MARGIN    
   !P.CHARTHICK = P_CHARTHICK 
   !P.THICK     = P_THICK     
   !X.THICK     = X_THICK     
   !Y.THICK     = Y_THICK 

end


