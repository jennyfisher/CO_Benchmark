; $Id: all_stations_cmdl_geos.pro,v 1.3 2008/03/31 18:51:06 bmy Exp $
pro all_stations_cmdl_geos, species1, species, max_sta, pref,  plabel,     $
                            ptop,     dlat,    dlon,    model, ext, debug=debug
           
   ; ALL_STATIONS_CMDL_GEOS: Saves out files of GEOS-Chem data at
   ; various CMDL stations.  Originally written by Inna Megretskaia, 
   ; modified by Bob Yantosca and Philippe Le Sager.
   ;
   ; NOTE: Now use CTM_INDEX to return the Lon & Lat indices
   ;       for each station (bmy, 7/11/07)

   ; Get MODELINFO structure
   Type1 = CTM_Type( Model, Res=[ DLon, DLat ] )

;---
; test data version
   ;filest  = './data/Sites.ground.CO.1'
   filest  = './data/Sites.ground.CO.2005'
   max_sta = 42
;---

   ;PRINT, filest
   openr, usta, filest, /get_lun
   iname_sta=''
   ititle_sta=''
   ipref_sta=''
   
   name_sta  = strarr(max_sta)
   lon_sta   = fltarr(max_sta)
   lat_sta   = fltarr(max_sta)
   lon_sta_1 = fltarr(max_sta)
   lat_sta_1 = fltarr(max_sta)
   title_sta = strarr(max_sta)
   pref_sta  = strarr(max_sta)
   
   if keyword_set(debug) then print,' -- Reading station file '
   for i=0,max_sta-1 do begin
      readf,usta, iname_sta,ititle_sta,  ilat, ilon, ipref_sta,        $
         format='(2x,a3,7x,a15,2x,f6.2,3x,f6.2,4x,a3)'
      name_sta(i)  = iname_sta
      lon_sta_1(i) = round(ilon)
      lat_sta_1(i) = round(ilat)
      lon_sta(i)   = ilon
      lat_sta(i)   = ilat
      title_sta(i) = ititle_sta
      pref_sta[i]  = ipref_sta
   endfor

   ; Read model data first, outside of station loop
   for i = 0,11 do begin

      j = i+1
      mn=strtrim(j,2)
   
      if (strlen(mn) eq 1) then mn='0'+mn
      name=pref+mn+'01.nc'
      Pname=Plabel+mn+'01.nc'

      if keyword_set(debug) then print,' -- Reading model file '+name

      ; Read CO and pressure
      COtmp       = Get_Species_Geos(  Name, Species='IJ-AVG-S__CO' )
      Pressuretmp = Get_Pressure_Geos( PName, PTOP=PTOP, Lat=Lat, Lon=Lon )

      if i eq 0 then begin
         D = size(COtmp,/dim)
         CO = fltarr(D[0],D[1],D[2],12)
         D = size(Pressuretmp,/dim)
         Pressure = fltarr(D[0],D[1],D[2],12)
      endif

      CO[*,*,*,i] = COtmp
      Pressure[*,*,*,i] = Pressuretmp

   endfor
                  
   ; Now extract proper profile for the stations
   ; proper name will be given later, now we read from just one file

   for i=0,max_sta-1 do begin

      ; Put longitude in (-180,180) range
      if lon_sta_1(i) gt 180 then lon_sta_1(i)=lon_sta_1(i)-360
      if lon_sta(i) gt 180 then lon_sta(i )=lon_sta(i)-360

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Center=[ Lat_Sta[I], Lon_Sta[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Loop over months
      for j=1,12 do begin

         ; Take the surface CO data for the given month
         Out = CO[Indlon,Indlat,0,j-1]

         ; Unless it's one of these special stations at altitude,
         ; then interpolate to the proper altitude (bmy, 3/31/08)

         if (pref_sta(i) eq 'nwr') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1],                $
                            -alog10(Pressure[Indlon, Indlat, *,j-1]), $
                            -alog10( 700. ) )
         endif

         if (pref_sta(i) eq 'izo') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1],                  $
                            -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 800.) )
         endif
         
         if (pref_sta(i) eq 'mlo') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1],                  $
                            -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 700.) )
         endif
         
         if (pref_sta(i) eq 'spo') then begin
            out = interpol(CO[Indlon, Indlat, *,j-1], $
                           -alog10( Pressure[Indlon, Indlat, *,j-1]), $
                           -alog10( 900. ) )
         endif
         
         if (pref_sta(i) eq 'lef') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1], $
                            -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 900. ) )
         endif
         
         if (pref_sta(i) eq 'uum') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1], $
                            -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 900. ) )
         endif
         
         if (pref_sta(i) eq 'uta') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1], $
                           -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 900. ) )
         endif

         if (pref_sta(i) eq 'cui') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1], $
                            -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 900. ) )
         endif

         if (pref_sta(i) eq 'wlg') then begin
            out = interpol( CO[Indlon, Indlat, *,j-1], $
                            -alog10( Pressure[Indlon, Indlat, *,j-1] ), $
                            -alog10( 600. ) )
         endif

         ; Output file name
         fileout = 'temp/' + strtrim( name_sta(i), 2 ) + ext
  
         ; Write to file
         iunit = i+50
         if ( j eq 1 ) then begin  
            openw, iunit, fileout
         endif
     
         ; Save data to file
         printf, iunit, out
      endfor

      close, iunit
   endfor


   close,/all
   
   close_device

end
