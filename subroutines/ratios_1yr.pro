;-----------------------------------------------------------------------
;+
; NAME:
;        RATIOS_1YR
;
; PURPOSE:
;        Creates ratio maps of tracer at the surface and 500hPa
;        levels from 1-year GEOS-Chem benchmark simulation  output.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        RATIOS_1YR, FILES, TRACERS, VERSIONS, [, Keywords ]
;
; INPUTS:
;        FILES -> A 3-element vector containing the names of files
;             from the "red", 'green", and "blue" GEOS-Chem model 
;             versions that are to be compared. 
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$") to be plotted.
;
;        VERSIONS ->  A 3-element vector containing the model version
;             names from the "red", 'green", and "blue" simulations. 
;
; KEYWORD PARAMETERS:
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output 
;             to a file whose name is specified by this keyword.
;             Default is "tracer_ratio.pro".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines Provided:
;        =========================================
;        PLOTRATIOS
;
;        External Subroutines Required:
;        =========================================
;        OPEN_DEVICE     CLOSE_DEVICE
;        MULTIPANEL      COLORBAR_NDIV (function)
;        CTM_PLOT        CHKSTRU       (function)
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        FILES    = [ PREF1+'0101.nc', PREF2+'0101.nc', PREF3+'0101.nc' ]
;        TRACERS  = [ 1, 2, 4 ]
;        ALTRANGE = [ 0, 20 ]
;        VERSIONS = [ VERS1, VERS2, VERS3 ]
;        PSNAME   = PSDIR + 'Differences_Jan.' + RUNNAME + '.ps'
;
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;             /PS, OUTFILENAME=PSNAME
;
;             ; Creates ratio maps from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;             ; The 
;
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;             /DYNRANGE, /PS, OUTFILENAME=PSNAME
;
;             ; Same as above, but will create the plot using the 
;             ; dynamic range of the data (centered around zero).
;
; MODIFICATION HISTORY:
;        bmy, 09 Nov 2007: VERSION 1.01
;                          - Initial version
;        bmy, 20 Dec 2007: VERSION 1.02
;                          - Now pass the month as a keyword to
;                            put on the plot panel titles
;        bmy, 01 Jun 2011: VERSION 1.03
;                          - Make the colorbar a little wider
;                          - Reduce the character size CsFac to 0.75
;                            to better display long plot titles
;                          - Now compute the ratio within PLOTRATIOS
;                          - Remove subroutine COMPUTERATIO
;                          - Now use appropriate plot settings for
;                            creating ratio plots w/ dynamic range
;                          - Also restore the !MYCT sysvar to defaults
;
;-
; Copyright (C) 2007-2011,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine differences"
;-----------------------------------------------------------------------

pro PlotRatios, Data1,       Data2,       Version1,    Version2, $
                TracerName,  GridInfo,    Unit,        DynRange, $
                Month,       L_Sfc=L_Sfc, L_500=L_500, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTDIFF plots either the surface or 500 hPa
   ; ratio of tracer between old and new versions (bmy, 11/9/07)
   ;====================================================================

   ; Version string
   VerStr = Version2 + ' - ' + Version1
 
   ; Plot title for surface
   if ( Keyword_Set( L_Sfc ) )                                  $
      then Title = VerStr     + '!C!C'                  +       $
                   TracerName + ' - Ratio @ Surface for ' + Month

   ; Plot title for 500 hPa
   if ( Keyword_Set( L_500 ) )                                  $
      then Title = VerStr     + '!C!C'                  +       $
                   TracerName + '- Ratio @ 500 hPa for ' + Month 

   ;=================================================================
   ; Compute ratio of two data blocks
   ;=================================================================

   ; Take the ratio "new" / "old"
   Ratio = Data2 / Data1

   ; Replace non-finite points with a missing-data value
   Ind = Where( ~Finite( Ratio ) )
   if ( Ind[0] ge 0 ) then Ratio[Ind] = -9.99e30

   ; Don't plot the polar latitudes
   XMid  = GridInfo.XMid
   YMid  = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Ratio = Ratio[ *, 1:GridInfo.JMX-2 ]
   
   if ( DynRange ) then begin

      ;=================================================================
      ; Plot ratio data dusing the dynamic range of the data 
      ; Center the plot range symmetrically around zero
      ;=================================================================

      Ind = Where( Ratio gt 0 )

      ; Settings for plot
      MinData =  Min( Ratio[Ind], Max=MaxData )
      Extreme =  Max( [ Abs( MinData ), Abs( MaxData ) ] )
      ;MinData = -Extreme
      ;MaxData =  Extreme
      BotOut  = !MYCT.GRAY50
      Div     = 8
      CBPos   =  [ 0.05, 0.01, 0.95, 0.04 ]

      ; Plot data w/ country boundaries
      TvMap, Ratio, XMid, Ymid,                                     $
             /Countries,         /Coasts,         /Cbar,            $
             Division=Div,       /Sample,         /Grid,            $
             Title=Title,        MinData=MinData, MaxData=MaxData,  $
             CBFormat='(f13.2)', TCsFac=1.0,      CsFac=0.8,        $
             CBPosition=CBPos,   BotOut=BotOut,   /Triangle,        $
             /NoGap,             Min_Valid=MinData, /Isotropic, _EXTRA=e

   endif else begin

      ;=================================================================
      ; Plot ratio data w/in limits of 0.5 - 2.0 (default)
      ;=================================================================

      ; Settings for plot
      MinData = 0.5  
      MaxData = 2.0   
      BotOut  = !MYCT.GRAY50
      Div     = 8
      Annote  = [ '0.50', '0.61', '0.74', '0.90',  $
                  '1.10', '1.34', '1.64', '2.00'  ]
      CBPos   =  [ 0.05,   0.01,   0.95,   0.04   ]
      
      ; Plot data w/ country boundaries
      TvMap, Ratio, XMid, Ymid,                                     $
             /Countries,         /Coasts,         /Cbar,            $
             Division=Div,       /Sample,         /Grid,            $
             Title=Title,        MinData=MinData, MaxData=MaxData,  $
             CBFormat='(f13.3)', BOR_Label=' ',   /Triangle,        $
             CBPosition=CBPos,   /NoGap,          BotOut=BotOut,    $
             Annotation=Annote,  TCsFac=1.0,      CsFac=0.8,        $
             /Log,               /Isotropic,      _EXTRA=e

   endelse

end

;------------------------------------------------------------------------------

pro Ratios_1yr, Files,             Tracers,     Versions,                $
                DynRange=DynRange, PS=PS,       OutFileName=OutFileName, $
                Month=Month,       DiagN=DiagN, _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( Files    ) ne 3 ) then Message, 'Invalid FILES!'
   if ( N_Elements( Versions ) ne 3 ) then Message, 'Invalid VERSIONS!'
   if ( N_Elements( DiagN    ) eq 0 ) then Diagn = 'IJ-AVG-$'
   
   ; Arguments
   DynRange = Keyword_Set( DynRange )
   if ( N_Elements( Month       ) ne 1 ) then Month       = ''
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'ratios.ps'

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Ratio Maps at surface and 500 hPa!C!C'

   ; Save original color table
   TvLct, R, G, B, /Get
   Myct_Orig = !MYCT

   ; This colortable will show 0.9 - 1.0 in the white, and anything
   ; outside that in the red or blue.  Use 8 colorbar divisions to line
   ; everything up properly. (bmy, 5/21/10)
   ;
   ; NOTE: It is really 0.9057 - 1.0104.  Close enough for gov't work.
   MyCt, 63, NColors=14, /MidCol, /White, /Reverse, _EXTRA=e

   ;====================================================================
   ; Read data from the files
   ;====================================================================
   
   ; Read tracers from the 1st file (red data pts)
   CTM_Get_Data, DataInfo_1, 'IJ-AVG-$', $
      File=Files[0], Tracer=Tracers, /Quiet

   ; Read tracers from the 2nd file (green data pts)
   CTM_Get_Data, DataInfo_2, 'IJ-AVG-$', $
      File=Files[1], Tracer=Tracers, /Quiet

   ; Read tracers from the 3rd file (blue data pts)
   CTM_Get_Data, DataInfo_3, 'IJ-AVG-$', $
      File=Files[2], Tracer=Tracers, /Quiet

   ;------------------------------
   ; Error checks!
   ;------------------------------

   ; Stop if both DATAINFOs are incompatible
   if ( N_Elements( DataInfo_3 ) ne N_Elements( DataInfo_1 ) ) $
      then Message, '1st & 3rd files are incompatible!'

   if ( N_Elements( DataInfo_3 ) ne N_Elements( DataInfo_2 ) ) $
      then Message, '2nd & 3rd files are incompatible!'

   ;====================================================================
   ; Process data and create profile plots with CTM_PLOT!
   ;====================================================================

   ; Number of rows & colums on the plot
   Rows = 2
   Cols = 2

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Multiple panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ; Max # of colorbar divisions
   Divisions = Colorbar_NDiv( 6 )

   ; Loop over the data blocks
   for D = 0L, N_Elements( DataInfo_1 )-1L do begin

      ;-----------------------------------------------------------------
      ; Error check grid, tracer name, and data block sizes 
      ;-----------------------------------------------------------------

      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_1[D], ModelInfo_1, GridInfo_1
      GetModelAndGridInfo, DataInfo_2[D], ModelInfo_2, GridInfo_2
      GetModelAndGridInfo, DataInfo_3[D], ModelInfo_3, GridInfo_3

      ; Make sure grids are compatible
      if ( GridInfo_1.IMX ne GridInfo_3.IMX  OR $
           GridInfo_1.JMX ne GridInfo_3.JMX )   $
         then Message, '1-3 resolution mismatch!'

      ; Make sure grids are compatible
      if ( GridInfo_2.IMX ne GridInfo_3.IMX  OR $
           GridInfo_2.JMX ne GridInfo_3.JMX )   $
         then Message, '2-3 resolution mismatch!'

      ; Make sure the tracers correspond to each other
      TracerName_1 = DataInfo_1[D].TracerName
      TracerName_2 = DataInfo_2[D].TracerName
      TracerName_3 = DataInfo_3[D].TracerName
      if ( TracerName_1 ne TracerName_3 ) then Message, '1-3 Tracer mismatch!'
      if ( TracerName_2 ne TracerName_3 ) then Message, '2-3 Tracer mismatch!'

      ; Get full-sized data arrays
      Data_1 = *( DataInfo_1[D].Data )
      Data_2 = *( DataInfo_2[D].Data )
      Data_3 = *( DataInfo_3[D].Data )

      ; Get the dimensions of the data arrays
      Size_1 = Size( Data_1, /Dim )
      Size_2 = Size( Data_2, /Dim )
      Size_3 = Size( Data_3, /Dim )

      ; Stop the run if the data block sizes don't agree
      if ( Size_1[0] ne Size_3[0] ) then Message, '1-3 Longitude mismatch!'
      if ( Size_1[1] ne Size_3[1] ) then Message, '1-3 Latitude mismatch!'
      if ( Size_2[0] ne Size_3[0] ) then Message, '2-3 Longitude mismatch!'
      if ( Size_2[1] ne Size_3[1] ) then Message, '2-3 Latitude mismatch!'

      ; Get unit of data
      Unit = DataInfo_1[D].Unit

      ;-----------------------------------------------------------------
      ; Extract data arrays for surface and 500 hPa
      ;-----------------------------------------------------------------

      ; Extract data at surface -- 1st model
      Data_Sfc_1 = CTM_Extract( Data_1,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at surface -- 2nd model
      Data_Sfc_2 = CTM_Extract( Data_2,                                     $
                                ModelInfo=ModelInfo_2, GridInfo=GridInfo_2, $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at surface -- 3rd model
      Data_Sfc_3 = CTM_Extract( Data_3,                                     $
                                ModelInfo=ModelInfo_3, GridInfo=GridInfo_3, $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at 500hPa -- 1st model
      Data_500_1 = CTM_Extract( Data_1,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )

      ; Extract data at 500hPa -- 2nd model
      Data_500_2 = CTM_Extract( Data_2,                                     $
                                ModelInfo=ModelInfo_2, GridInfo=GridInfo_2, $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )

      ; Extract data at 500hPa -- 3rd model
      Data_500_3 = CTM_Extract( Data_3,                                     $
                                ModelInfo=ModelInfo_3, GridInfo=GridInfo_3, $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )
     
      ; We no longer need the large arrays
      UnDefine, Data_1
      UnDefine, Data_2
      UnDefine, Data_3

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------

      ; "Blue" - "Green" at Sfc
      PlotRatios, Data_Sfc_2,   Data_Sfc_3, Versions[1], Versions[2], $
                  TracerName_1, GridInfo_1, Unit,        DynRange,    $
                  Month,        /L_Sfc,     _EXTRA=e

      ; "Blue" - "Green" at 500 hPa
      PlotRatios, Data_500_2,   Data_500_3, Versions[1], Versions[2], $
                  TracerName_1, GridInfo_1, Unit,        DynRange,    $
                  Month,        /L_500,     _EXTRA=e
 
      ; "Blue" - "Red" at Sfc
      PlotRatios, Data_Sfc_1,   Data_Sfc_3, Versions[0], Versions[2], $
                  TracerName_1, GridInfo_1, Unit,        DynRange,    $
                  Month,        /L_Sfc,     _EXTRA=e

      ; "Blue" - "Red" at 500 hPa
      PlotRatios, Data_500_1,   Data_500_3, Versions[0], Versions[2], $
                  TracerName_1, GridInfo_1, Unit,        DynRange,    $
                  Month,        /L_500,     _EXTRA=e

      ; Plot the top title on each page  
      if ( D*4 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
   endfor

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================

   ; Close plot device
   Close_Device

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to defaults
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Turn off multi-panel settings
   Multipanel, /Off

   ; Quit
   return
end
 
