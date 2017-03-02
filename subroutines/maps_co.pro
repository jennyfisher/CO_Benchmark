;-----------------------------------------------------------------------
;+
; NAME:
;        MAPS_CO
;
; PURPOSE:
;        Creates lon-lat maps of GEOS-Chem tracers at the 
;        surface and 500 hPa levels; specifically for tagged CO
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        MAPS_CO, FILE, LEVELS, TAUS, TRACERS, VERSION, [, Keywords ]
;
; INPUTS:
;        FILE -> The name of the file containing data to be plotted.
;
;        LEVELS -> A 4-element vector containing the level indices
;             for the GEOS-Chem surface layer and 500 hPa layer.
;             for both models (e.g. SFC_1, SFC_2, 500_1, 500_2).
;             NOTE: This is in Fortran notation (starting from 1!)
;
;        TAU -> The TAU value (hours GMT from /1/1985) corresponding
;             to the data to be plotted.
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$").
;
;        VERSION -> The model version number corresponding to the
;             data to be plotted.
;
; KEYWORD PARAMETERS:
;        /DO_FULLCHEM -> Set this switch to plot the chemically
;             produced OH in addition to the advected tracers.
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
;        ==================================================
;        PlotMap
;
;        External Subroutines Required:
;        ==================================================
;        CLOSE_DEVICE          COLORBAR_NDIV    (function)
;        CTM_GET_DATA          EXTRACT_FILENAME (function)
;        GETMODELANDGRIDINFO   MULTIPANEL
;        MYCT                  OPEN_DEVICE
;        TVMAP                 CHKSTRU          (function)
;        UNDEFINE
;        
; REQUIREMENTS:
;        References routines from the GAMAP package.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1MON.
;
; EXAMPLES:
;        FILE     = 'ctm.bpch.v7-04-11'
;        LEVELS   = [ 1, 1, 13, 13 ]
;        TAUS     = NYMD2TAU( 20010701 )
;        TRACERS  = INDGEN( 43 ) + 1
;        VERSIONS = 'v7-04-11'
;
;        MAPS, FILE, LEVELS, TAU, TRACERS, VERSION, $
;             /DO_FULLCHEM, /PS, OUTFILENAME='myplot.ps'
;
;             ; Creates tracer maps of two GEOS-CHEM versions
;             ; (in this case v7-04-11 / v7-04-10) for July 2001.
;             ; Output is sent to PostScript file "myplot.ps".
;             ; The min & max of each plot panel will correspond
;             ; to the dynamic range of the data.
;
; MODIFICATION HISTORY:
;        bmy, 14 Nov 2007: VERSION 1.01
;                          - Based on "tracer_map.pro" 
;        bmy, 07 May 2008: VERSION 1.06
;                          - Now allow for comparing models on 2
;        bmy, 08 Jun 2011: VERSION 1.07
;                          - Added /DO_FULLCHEM keyword
;                          - Now restore !MYCT sysvar to previous
;                            settings upon exiting the program
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
; or phs@io.as.harvard.edu with subject "IDL routine maps"
;-----------------------------------------------------------------------


pro PlotMap, Data, Level, TracerName, Unit, GridInfo, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTMAP plots either the surface or 500 hPa
   ; map of tracer (bmy, 11/14/07)
   ;====================================================================

   ; Plot title
   if ( Level gt 1 )                                      $
      then Title = 'Tracer Map @ 500 hPa - ' + TracerName $
      else Title = 'Tracer Map @ Surface - ' + TracerName  

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 6 )
      
   ; Don't plot the polar latitudes
   XMid = GridInfo.XMid
   YMid = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Data = Data[ *, 1:GridInfo.JMX-2 ]

   ; For OH, let's rescale the unit for clarity
   if ( TracerName eq 'OH' ) then begin
      Data = Data / 1e5
      Unit = '1e5 molec/cm3'
   endif

   ; Plot data w/ country boundaries
   TvMap, Data, XMid, Ymid,                              $
      /Countries,         /Coasts,            /Cbar,     $
      Division=Divisions, /Sample,            /Grid,     $
      Title=Title,        CBFormat='(f13.4)', Unit=Unit, $
      _EXTRA=e

end

;------------------------------------------------------------------------------

pro Maps_CO, File, Levels, Tau, Tracers, Version, $
          Do_FullChem=Do_FullChem,  PS=PS,     $
          OutFileName=OutFileName, _EXTRA=e
   
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
;   if ( N_Elements( File    ) ne 1 ) then Message, 'Invalid FILES!'
;   if ( N_Elements( Levels  ) ne 4 ) then Message, 'Invalid LEVELS'
;   if ( N_Elements( Tau     ) ne 1 ) then Message, 'Invalid TAUS!'
;   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSIONS!'

   FilesIn = File
   if ( N_Elements( Version ) ne 1 ) then begin
   VersionsIn = Version
   Version = VersionsIn[0]
   for f = 1, n_elements(VersionsIn)-1 do Version=Version+', '+VersionsIn[f]
   endif

   ; Keywords
   Do_FullChem = Keyword_Set( Do_FullChem )
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'ratios.ps'

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem ' + Version            + $
      ' Tracer Maps at Surface and 500 hPa!C!C'

   ; Save original color table information
   TvLct, R, G, B, /Get

   ; Save the original settings of the !MYCT sysvar
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; Load modified spectrum, extended to 12 colors
   MyCt, /whgrylrd, NColors=12

   ;====================================================================
   ; Set up postscript
   ;====================================================================

   ; Number of rows & columns on the plot
   Rows = 3
   Cols = 2

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
 
   ; Multiple panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ;====================================================================
   ; Read data from the files
   ;====================================================================

   For F=0,N_Elements(FilesIn)-1 do begin

   File = FilesIn[F]
   
   ; Read transported tracers
   CTM_Get_Data, DataInfo, 'IJ-AVG-$', $
      File=File, Tau0=Tau, Tracer=Tracers, /Quiet

   ; Read OH and append to DATAINFO
   if ( Do_FullChem ) then begin
      CTM_Get_Data, TmpDataInfo, 'CHEM-L=$', $
                    File=File, Tau0=Taus, Tracer=1, /Quiet
      DataInfo = [ DataInfo, TmpDataInfo ]
      UnDefine, TmpDataInfo
   endif

   ; Loop over all data blocks
   for D = 0L, N_Elements( DataInfo )-1L do begin
   
      ;-----------------------------------------------------------------
      ; Extract data 
      ;-----------------------------------------------------------------

      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo[D], ModelInfo, GridInfo

      ; Get tracername and unit strings
      TracerName = DataInfo[D].TracerName
      Unit       = DataInfo[D].Unit
      
      ; Get data array
      Data       = *( DataInfo[D].Data )

      ; Split into sfc and 500hPa levels
      Data_Sfc   = Data[ *, *, Levels[0]-1L ]
      Data_500   = Data[ *, *, Levels[2]-1L ]

      ; We no longer need the large data araray
      UnDefine, Data

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------      

      ; Plot the surface data
      PlotMap, Data_Sfc, Levels[0], TracerName, Unit, GridInfo, _EXTRA=e
      
      ; Plot the 500hPa data
      PlotMap, Data_500, Levels[2], TracerName, Unit, GridInfo, _EXTRA=e
      
      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ;-----------------------------------------------------------------
      ; Undefine stuff for next iteration
      ;-----------------------------------------------------------------
      UnDefine, Data_500
      UnDefine, Data_Sfc
      UnDefine, ModelInfo
      UnDefine, GridInfo
      UnDefine, TracerName
      UnDefine, Unit

   endfor

   endfor

   ;====================================================================
   ; Cleanup & quit
   ;====================================================================

   ; Cancel previous MultiPanel Settings
   MultiPanel, /Off

   ; Close plot device
   Close_Device

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to original settings
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Rewrite variables
   File = FilesIn
   Version = VersionsIn

   ; Quit
   return

end
 
