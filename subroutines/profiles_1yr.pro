;-----------------------------------------------------------------------
;+
; NAME:
;        PROFILES_1YR
;
; PURPOSE:
;        Creates longitudinal difference profiles of tracer along 15S 
;        latitude and 42N latitude.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, [, Keywords ]
;
; INPUTS:
;        FILES -> A 3-element vector containing the names of files
;             from the "red", 'green", and "blue" GEOS-Chem model 
;             versions that are to be compared. 
;
;        ALTRANGE -> A 2-element vector containing the altitude range
;             (in km) of the data to be plotted.  ALTRANGE will be 
;             passed to CTM_EXTRACT.  
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$") to be plotted.
;
;        VERSIONS ->  A 3-element vector containing the model version
;             names from the "red", 'green", and "blue" simulations. 
;
; KEYWORD PARAMETERS:
;        /DYNRANGE -> Set this switch to create plots using the entire
;             dynamic range of the data (centered around zero).  The
;             default is to use pre-defined data ranges.
;
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
;        PLOTPROF
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
;        PSNAME   = PSDIR + 'Lon_Profiles_Jan.' + RUNNAME + '.ps'
;
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;                      /PS, OUTFILENAME=PSNAME
;
;             ; Creates profile plots from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;                      /DYNRANGE, /PS, OUTFILENAME=PSNAME
;
;             ; Same as above, but will create orifuke plots using the
;             ; full dynamic range of the data (centered around zero)
;             ; instead of using pre-defined min & max values.
;
; MODIFICATION HISTORY:
;        bmy, 09 Nov 2007: VERSION 1.01
;                          - Initial version
;        bmy, 20 Dec 2007: VERSION 1.02
;                          - Bug fix: typo prevented different models
;                            from being plotted
;                          - Now pass the month as a keyword to
;                            put on the plot panel titles
;                          - Updated comments
;        bmy, 01 Jun 2011: VERSION 1.03
;                          - Make the colorbar a little wider
;                          - Reduce the character size CsFac to 0.75
;                            to better display long plot titles
;                          - Now call COLORBAR with the UPOS keyword 
;                            to place the colorbar unit string properly
;                          - Now use appropriate settings for creating
;                            plots w/ the full dynamic range (/DYNRANGE)
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


pro PlotProf, Data1,      Data2,    Version1,  Version2, $
              TracerName, GridInfo, Unit,      DynRange, $
              Month,      SLat=SLat,    NLat=NLat, InvDiff=InvDiff, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTPROF plots either the difference lon-alt 
   ; profile along 15S or 42N between old & new versions (bmy, 11/9/07)
   ;====================================================================

   FORWARD_FUNCTION Get_Diff_Range

   ; Version string
   if ( Keyword_Set( InvDiff ) ) then      $
      VerStr = Version1 + ' - ' + Version2 $
   else                                    $
      VerStr = Version2 + ' - ' + Version1

   ; Plot title
   if ( n_elements(SLat) ne 0 ) then $
      latname = string(SLat,'(i2)')+'S'
   if ( n_elements(NLat) ne 0 ) then $
      latname = string(NLat,'(i2)')+'N'

   Title = VerStr  + '!C!C'                   +       $
                   TracerName + ' - Diff along '+     $
		   latname + ' for ' + Month

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 8 )

   ; Define plot ranges
   S    = Size( Data1, /Dim )
   XMid = GridInfo.XMid
   Zmid = GridInfo.ZMid[0:S[1]-1L]

   ; Compute difference
   Diff = Data2 - Data1

   ; Plot Data1 - Data2 if requested
   if ( Keyword_Set( InvDiff ) ) then Diff = -1. * Diff

   ; If /DYNRANGE is set, then plot the entire dyn range of data
   ; (centered upon zero).  Otherwise fix the min & max of the data 
   ; to predetermined limits, as returned by GET_DIFF_RANGE.
   if ( DynRange ) then begin

      ;=================================================================
      ; Create plots using the full dynamic range of the data (centered
      ; around zero) if the /DYNRANGE keyword is set.
      ;=================================================================
      MinData  =  Min( Diff, Max=MaxData )
      Extreme  =  Max( [ Abs( MinData ), Abs( MaxData ) ] )
      MinData  = -Extreme
      MaxData  =  Extreme
      Triangle =  0
      NoGap    =  0
      Upos     =  1.1
      CbPos    =  [ 0.10, 0.01, 0.90, 0.04 ]

   endif else begin

      ;=================================================================
      ; Create plots using the pre-defined min & max values from
      ; function GET_DIFF_RANGE.  This is the default.
      ;=================================================================
      Range    =  Get_Diff_Range( TracerName )
      MinData  =  Range[0] 
      MaxData  =  Range[1]
      BotOut   =  !MYCT.BOTTOM
      Triangle =  1
      NoGap    =  1
      UPos     =  1.02
      CbPos    =  [ 0.05, 0.01, 0.95, 0.04 ]

   endelse

   ; For OH, let's rescale the unit for clarity
   if ( TracerName eq 'OH' ) then begin
      Diff    = Diff    / 1e5
      MinData = MinData / 1e5
      MaxData = MaxData / 1e5
      Unit    = '1e5 molec/cm3'
   endif

   ; X-axis varibles
   XTickV = [ -180, -120, -60, 0, 60, 120, 180 ]
   XTicks = N_Elements( XTickV )-1L
   XMinor = 6

   ; Plot profile difference
   TvPlot, Diff, XMid, ZMid,                                          $
      /Cbar,              Division=Divisions, /Sample,                $
      Title=Title,        MinData=MinData,    MaxData=MaxData,        $
      CBFormat='(f13.3)', BOR_Label=' ',      Unit=Unit,              $
      /XStyle,            XTickV=XTickV,      XTicks=XTicks,          $
      XMinor=XMinor,      XTitle='Longitude', YTitle='Altitude (km)', $
      /YStyle,            Triangle=Triangle,  NoGap=NoGap,            $
      BotOut=BotOut,      CBPosition=CBPos,   CsFac=0.75,             $
      UPos=UPos,          _EXTRA=e

end

;------------------------------------------------------------------------------

pro Profiles_1yr, Files, AltRange, Tracers, Versions,      $
                  Categories = Categories,                 $
                  DynRange=DynRange,       Month=Month,    $
                  SLat=SLat,               NLat=NLat,      $
                  OutFileName=OutFileName, PS=PS, InvDiff=InvDiff, _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName, Get_Diff_Range

   ; Keyword Settings
   if ( N_Elements( Files    ) ne 3 ) then Message, 'Invalid FILES!'
   if ( N_Elements( AltRange ) ne 2 ) then Message, 'Invalid ALTRANGE!'
   if ( N_Elements( Versions ) ne 3 ) then Message, 'Invalid VERSIONS!'

   ; Arguments
   DynRange = Keyword_Set( DynRange )
   if ( N_Elements( Month       ) ne 1 ) then Month       = ''
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'ratios.ps'
   if ( N_elements( Categories  ) eq 0 ) then begin
      Cat1 = 'IJ-AVG-$'
      Cat2 = 'IJ-AVG-$'
      Cat3 = 'IJ-AVG-$'
   endif else begin
      Cat1 = Categories[0]
      Cat2 = Categories[1]
      Cat3 = Categories[2]
   endelse
   if ~Keyword_Set( InvDiff ) then InvDiff = 0
   if ( N_elements( SLat  ) eq 0 ) then SLat = -15
   if ( N_elements( NLat  ) eq 0 ) then NLat = 42

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Longitudinal Difference Profiles!C!C'

   ; Save original color table and !MYCT sysvar
   TvLct, R, G, B, /Get
   Myct_Orig = !MYCT

   ; Load Blue-White-White-Red colortable
   MyCt, /BuWhWhRd, _EXTRA=e
   
   ;====================================================================
   ; Read data from the files
   ;====================================================================
   
   ; Read tracers from the 1st file (red data pts)
   CTM_Get_Data, DataInfo_1, Cat1, $
      File=Files[0], Tracer=Tracers[0], /Quiet

   ; Read tracers from the 2nd file (green data pts)
   CTM_Get_Data, DataInfo_2, Cat2, $
      File=Files[1], Tracer=Tracers[1], /Quiet

   ; Read tracers from the 3rd file (blue data pts)
   CTM_Get_Data, DataInfo_3, Cat3, $
      File=Files[2], Tracer=Tracers[2], /Quiet

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
      if ( ( ModelInfo_1.Name ne ModelInfo_3.Name ) OR $
           ( GridInfo_1.IMX   ne GridInfo_3.IMX   ) OR $
           ( GridInfo_1.JMX   ne GridInfo_3.JMX   ) OR $
           ( GridInfo_1.LMX   ne GridInfo_3.LMX   ) )  $
         then Message, '1-3 resolution mismatch!'

      ; Make sure grids are compatible
      if ( ( ModelInfo_2.Name ne ModelInfo_3.Name ) OR $
           ( GridInfo_2.IMX   ne GridInfo_3.IMX   ) OR $
           ( GridInfo_2.JMX   ne GridInfo_3.JMX   ) OR $
           ( GridInfo_2.LMX   ne GridInfo_3.LMX   ) )  $
         then Message, '2-3 resolution mismatch!'

      ; Make sure the tracers correspond to each other
      TracerName_1 = DataInfo_1[D].TracerName
      TracerName_2 = DataInfo_2[D].TracerName
      TracerName_3 = DataInfo_3[D].TracerName
;      if ( TracerName_1 ne TracerName_3 ) then Message, '1-3 Tracer mismatch!'
;      if ( TracerName_2 ne TracerName_3 ) then Message, '2-3 Tracer mismatch!'

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
      if SLat gt 0 then begin
         SLat_save = SLat
         SLat = -1.*SLat
      endif else SLat_save = SLat

      ; Extract data along SLat -- 1st model
      Data_SLat_1 = CTM_Extract( Data_1,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[SLat,SLat], Alt=AltRange )

      ; Extract data along SLat -- 2nd model
      Data_SLat_2 = CTM_Extract( Data_2,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[SLat,SLat], Alt=AltRange )

      ; Extract data along SLat -- 2nd model
      Data_SLat_3 = CTM_Extract( Data_3,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[SLat,SLat], Alt=AltRange )

      ; Extract data along NLat -- 1st model
      Data_NLat_1 = CTM_Extract( Data_1,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[NLat,NLat], Alt=AltRange )

      ; Extract data along NLat -- 2nd model
      Data_NLat_2 = CTM_Extract( Data_2,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[NLat,NLat], Alt=AltRange )

      ; Extract data along NLat -- 2nd model
      Data_NLat_3 = CTM_Extract( Data_3,                                     $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1, $
                                Lon=[-180,180], Lat=[NLat,NLat], Alt=AltRange )

      ; We no longer need the large arrays
      UnDefine, Data_1
      UnDefine, Data_2
      UnDefine, Data_3
      SLat = SLat_save

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------

      ; "Blue" - "Green" along SLat
      PlotProf, Data_SLat_2,   Data_SLat_3, Versions[1], Versions[2], $
                TracerName_1, GridInfo_1, Unit,        DynRange,    $
                Month,        SLat=SLat,  InvDiff=InvDiff, _EXTRA=e

      ; "Blue" - "Green" along NLat
      PlotProf, Data_NLat_2,   Data_NLat_3, Versions[1], Versions[2], $
                TracerName_1, GridInfo_1, Unit,        DynRange,    $
                Month,        NLat=NLat,  InvDiff=InvDiff, _EXTRA=e
 
      ; "Blue" - "Red" along SLat
      PlotProf, Data_SLat_1,   Data_SLat_3, Versions[0], Versions[2], $
                TracerName_1, GridInfo_1, Unit,        DynRange,    $
                Month,        SLat=SLat,  InvDiff=InvDiff, _EXTRA=e

      ; "Blue" - "Red" along NLat
      PlotProf, Data_NLat_1,   Data_NLat_3, Versions[0], Versions[2], $
                TracerName_1, GridInfo_1, Unit,        DynRange,    $
                Month,        NLat=NLat,  InvDiff=InvDiff, _EXTRA=e

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
 
