; $Id: plot_cmdl_3_models_4_months.pro,v 1.2 2008/03/11 17:47:25 bmy Exp $
pro plot_cmdl_3_models_4_months, ext1, ext2, ext3, title,psname

; For a given set of stations compares CO surface data from cmdl (black
; solid line) with surface data from 2 geos models


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
   !X.OMARGIN=[4,2] 
   !Y.OMARGIN=[2,2]
   !X.THICK=4
   !Y.THICK=4
   !P.CHARTHICK=2.5
   !P.THICK=2.5

   mmonth = strarr(12)
   mmonth=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']

   ; bmy put this here for testing
   nrow=2
   ncol=2
   !P.Multi = [0,nrow,ncol,0,0]

   open_device, /ps, /color, filename=psname ;color plot

; Specify directory with surface data 

;---
; test data version
   ;pre = './data/cmdl/newdata/'
   pre = './data/cmdl/Feb09/'
;---
            
; Set max_sta parameter
;---
; test data version
   max_sta=34
   max_sta=34
   ;max_sta=42
;---

   ; Define arrays for storing data
   geos_data=fltarr(max_sta,12)
   geos2_data=fltarr(max_sta,12)
   geos3_data=fltarr(max_sta,12)
   cmdl_data=fltarr(max_sta,12)
   cmdl_std=fltarr(max_sta,12)

; --- read station & indice ---

;---
; test data version
   filest='./data/Sites.ground.CO.lat.2005'
   ;filest='./data/Sites.ground.CO.2005'
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

; ---  open files ---

   ncount=0

; --- loop for stations ---
   for k = 1, max_sta do begin

      ncount=ncount+1
      ; print, 'STATION : ', k
      kk = k-1 
      ix = k
      file=''

;---
; test data version
      ;file=pre+pref_sta(kk)+'.mn'
      file=pre+pref_sta(kk)+'.mn.2005'
;---
; test data version

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
         ; avoid -999
         if icostd lt 0 then costd(i) = 0 else costd(i) = icostd
         comin(i)    =icomin
         comax(i)    =icomax
      endfor
      close, ilun
      ;print, comean
      cmdl_data(kk,*)= comedian
      cmdl_std(kk,*)= costd

      ; Read data from geos model
      co=fltarr(12)
      name_geos=name_sta(kk)+ext1
   
      ; Now read file NAME_GEOS from temp/ directory (bmy, 8/13/03)
      name_geos = 'temp/' + name_geos

      openr,ix, name_geos
      for j = 1,12 do begin     ;12 levels 
         jx = j - 1
         readf, ix, fco
         co[jx]=fco
      endfor

      close,ix
      geos_data(kk,*)= co

      ; Read data from geos2 model

      name_geos2=name_sta(kk)+ext2

   ; Now read file NAME_GEOS2 from temp/ directory (bmy, 8/13/03)
      name_geos2 = 'temp/' + name_geos2

      openr,ix, name_geos2
      for j = 1,12 do begin     ;12 levels 
         jx = j - 1
         readf, ix, fco
         co[jx]=fco
      endfor
      
      close,ix
      geos2_data(kk,*)= co

      ; Read data from geos3 model
      name_geos3=name_sta(kk)+ext3

      ; Now read file NAME_GEOS2 from temp/ directory (bmy, 8/13/03)
      name_geos3 = 'temp/' + name_geos3

      openr,ix, name_geos3
      for j = 1,12 do begin     ;12 levels 
         jx = j - 1
         readf, ix, fco
         co[jx]=fco
      endfor

      close,ix
      geos3_data(kk,*)= co


   endfor

   ; Define the range for y axis

   loval=0
   highval=250 

   ; missing data
   missing = where(cmdl_data eq -999)
   cmdl_data[missing]=!values.f_nan

   ; -- plot observed data --

   plot, lat_sta, cmdl_data(*,0), xstyle=1,ystyle=1,$
      title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
      xticks=6, min_val=-900, xrange=[-90,90],yrange=[loval,highval],$
      charsize=1.5, xmargin=[3,1], ymargin=[1.5,1],color=1,$
      xtickname=['-90','-60','-30','0','30','60','90']
;   print,'jan'
;   for yy=0,34 do print, lat_sta[yy], cmdl_data(yy,0), geos_data(yy,0)

  ; Now plot standard deviations

   for w = 0, 33 do begin
      errbar = [cmdl_data(w,0)-cmdl_std(w,0),cmdl_data(w,0)+cmdl_std(w,0) ]
      oplot,  [lat_sta[w],lat_sta[w]],errbar,$
         linestyle=0,color=1
   endfor

   oplot, lat_sta,geos_data(*,0),linestyle=1,color=2
   oplot, lat_sta,geos_data(*,0),linestyle=1,psym=2,symsize=0.3,color=2   
   oplot, lat_sta,geos2_data(*,0),linestyle=2,color=3
   oplot,lat_sta,geos2_data(*,0),linestyle=2,psym=2,symsize=0.3,color=3

   oplot, lat_sta,geos3_data(*,0),linestyle=3,color=4
   oplot, lat_sta,geos3_data(*,0),linestyle=3,psym=2,symsize=0.3,color=4 

   xyouts,-30,220, mmonth[0], charsize = 1.2, /data, color=1

   ; -- plot observed data --
   plot, lat_sta, cmdl_data(*,2), xstyle=1,ystyle=1,$
      title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
      xticks=6, min_val=-900, xrange=[-90,90],yrange=[loval,highval],$
      charsize=1.5, xmargin=[3,1], ymargin=[1.5,1],color=1,$
      xtickname=['-90','-60','-30','0','30','60','90']
;   print,'mar'
;   for yy=0,34 do print, lat_sta[yy], cmdl_data(yy,2), geos_data(yy,2)

   ; Now plot standard deviations
 
   for w = 0, 33 do begin
      errbar = [cmdl_data(w,2)-cmdl_std(w,2),cmdl_data(w,2)+cmdl_std(w,2)]
      oplot,  [lat_sta[w],lat_sta[w]],errbar,$
         linestyle=0,color=1
   endfor

   oplot, lat_sta,geos_data(*,2),linestyle=1,color=2
   oplot,lat_sta,geos_data(*,2),linestyle=1,psym=2,symsize=0.3,color=2  

   oplot, lat_sta,geos2_data(*,2),linestyle=2,color=3
   oplot, lat_sta,geos2_data(*,2),linestyle=2,psym=2,symsize=0.3,color=3 

   oplot, lat_sta,geos3_data(*,2),linestyle=3,color=4
   oplot, lat_sta,geos3_data(*,2),linestyle=3,psym=2,symsize=0.3,color=4 

   xyouts,-30,220, mmonth[2], charsize = 1.2, /data, color=1

   ; -- plot observed data --

   plot, lat_sta, cmdl_data(*,6), xstyle=1,ystyle=1,$
      title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
      xticks=6, min_val=-900, xrange=[-90,90],yrange=[loval,highval],$
      charsize=1.5, xmargin=[3,1], ymargin=[1.5,1],color=1,$
      xtickname=['-90','-60','-30','0','30','60','90']
;   print,'jul'
;   for yy=0,34 do print, lat_sta[yy], cmdl_data(yy,6), geos_data(yy,6)

   ; Now plot standard deviations

   for w = 0, 33 do begin
      errbar = [cmdl_data(w,6)-cmdl_std(w,6),cmdl_data(w,6)+cmdl_std(w,6)]
      oplot,  [lat_sta[w],lat_sta[w]],errbar,$
              linestyle=0,color=1
   endfor

   oplot, lat_sta,geos_data(*,6),linestyle=1,color=2
   oplot, lat_sta,geos_data(*,6),linestyle=1,psym=2,symsize=0.3,color=2  

   oplot, lat_sta,geos2_data(*,6),linestyle=2,color=3
   oplot, lat_sta,geos2_data(*,6),linestyle=2,psym=2,symsize=0.3,color=3 

   oplot, lat_sta,geos3_data(*,6),linestyle=3,color=4
   oplot, lat_sta,geos3_data(*,6),linestyle=3,psym=2,symsize=0.3,color=4 

   xyouts,-30,220, mmonth[6], charsize = 1.2, /data, color=1

   ; -- plot observed data --
   loval=0
   highval=250
   plot, lat_sta, cmdl_data(*,9), xstyle=1,ystyle=1,$
      title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
      xticks=6, min_val=-900, xrange=[-90,90],yrange=[loval,highval],$
      charsize=1.5, xmargin=[3,1], ymargin=[1.5,1],color=1,$
        xtickname=['-90','-60','-30','0','30','60','90']
;   print,'oct'
;   for yy=0,34 do print, lat_sta[yy], cmdl_data(yy,9), geos_data(yy,9)

   ; Now plot standard deviations
 
   for w = 0, 33 do begin
      errbar = [cmdl_data(w,9)-cmdl_std(w,9),cmdl_data(w,9)+cmdl_std(w,9)]
      oplot,  [lat_sta[w],lat_sta[w]],errbar,$
         linestyle=0,color=1
   endfor

   oplot, lat_sta,geos_data(*,9),linestyle=1,color=2
   oplot, lat_sta,geos_data(*,9),linestyle=1,psym=2,symsize=0.3,color=2  

   oplot, lat_sta,geos2_data(*,9),linestyle=2,color=3
   oplot, lat_sta,geos2_data(*,9),linestyle=2,psym=2,symsize=0.3,color=3 

   oplot, lat_sta,geos3_data(*,9),linestyle=3,color=4
   oplot, lat_sta,geos3_data(*,9),linestyle=3,psym=2,symsize=0.3,color=4 

   xyouts,-30,220, mmonth[9], charsize = 1.2, /data, color=1

   xyouts, 0.04, 0.5, 'CO (ppb)', /normal, align=0.5, orientation=90, $
      charsize=1.2,color=1
   xyouts, 0.5, 0.94, title, /normal, align=0.5, charsize=1.2,color=1

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


