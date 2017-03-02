;-----------------------------------------------------------------------
;+
; NAME:
;        EMISSION_RATIOS
;
; PURPOSE:
;        Creates ratio plots ( New/Old ) for GEOS-Chem tracers and OH. 
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        RATIOS, FILES, LEVELS, TAUS, VERSIONS, [, Keywords ]
;
; INPUTS:
;        FILES -> A 2-element vector containing the names of files
;             from the "old" and "new" GEOS-Chem model versions
;             that are to be compared. 
;
;        TAUS -> A 2-element vector contaning TAU values (hours GMT
;             from /1/1985) corresponding to the "old" and "new"
;             GEOS-Chem model versions.
;
;        VERSIONS -> A 2-element vector containing the version
;             numbers for the "old" and "new" GEOS-Chem model
;             versions.
;
; KEYWORD PARAMETERS:
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTDIR -> If /PS is set, then EMISSION_RATIOS will
;             create PostScript files in this directory.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines Included:
;        ===========================================
;        ComputeRatios   PlotRatio
;        CreatePlots
;
;        External Subroutines Required:
;        ============================================
;        OPEN_DEVICE     CLOSE_DEVICE
;        MULTIPANEL      COLORBAR_NDIV    (function)
;        TVMAP           CHKSTRU          (function)
;        UNDEFINE        EXTRACT_FILENAME (function)  
;        CTM_GET_DATA    ADD_SEPARATOR    (function)
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1MON.
;
; EXAMPLES:
;        FILES    = [ 'ctm.bpch.v7-04-10', 'ctm.bpch.v7-04-11' ]
;        TAUS     = [ NYMD2TAU( 20050701 ), NYMD2TAU( 20050701 ) ]
;        VERSIONS = [ 'v9-01-01', 'v9-01-02' ]
; 
;        EMISSION_RATIOS, FILES, TAUS, VERSIONS, $
;             /PS, OUTDIR='v9-01-02/output/'
;
;             ; Creates emission ratio plots of two GEOS-CHEM versions
;             ; (in this case v9-01-02 / v9-01-01) for July 2005.
;
; MODIFICATION HISTORY:
;        bmy, 10 Jun 2011: VERSION 1.00
;                          - Initial version, based on "ratios.pro"
;                          - Make sure directory ends with a path
;                            separator character
;        bmy, 23 Jun 2011: - Add ratio plot for lightning NOx
;        bmy, 27 Jun 2011: - Now split top-title into 2 lines
;        bmy, 11 Aug 2011: VERSION 1.01
;                          - Fix bug by making values less than 0.5
;                            not show up as missing data.
;        bmy, 16 Dec 2011: GAMAP VERSION 2.16
;                          - Remove ACET from dryleaf and ACET from
;                            grass.  These were GEIA diagnostics,
;                            which are now obsoleted.
;
;-
; Copyright (C) 2011, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to yantosca@seas.harvard.edu
; with subject "IDL routine ratios"
;-----------------------------------------------------------------------


function ComputeEmRatio, Files, Taus, Category, Tracer, $
                         Lev=Lev, Grid=Grid, _EXTRA=e

   ;====================================================================
   ; Internal routine COMPUTERATIO computes the ratio of "new" / "old"
   ; version for various emissions data (bmy, 6/10/11)
   ;====================================================================

   ; Set default lons & lats (global size)
   Lon = [ -180, 180 ]
   Lat = [  -90,  90 ]

   ; Assume we will be computing a ratio for the surface level
   if ( N_Elements( Lev ) eq 0 ) then Lev = [ 1, 1 ]

   ; Read data from old file
   Success = CTM_Get_DataBlock( Data1,          Category,                 $
                                File=Files[0],  Tau0=Taus[0],             $
                                Lon=Lon,        Lat=Lat,                  $
                                Lev=Lev,        Tracer=Tracer,            $
                                /Quiet,         /NoPrint,                 $
                                _EXTRA=e )

   ; Error msg
   if ( not Success ) then begin
      ErrMsg = 'EMISSION_RATIOS: Cannot read ' + StrTrim( Category, 2 ) + $
               ' from '                        + StrTrim( Files[0], 2 ) + $
               ' for tracer '                  + Tracer
      Message, ErrMsg
   endif

   ; Read data from new file
   Success = CTM_Get_DataBlock( Data2,         Category,                  $
                                File=Files[1], Tau0=Taus[1],              $
                                Lon=Lon,       Lat=Lat,                   $
                                Lev=Lev,       Tracer=Tracer,             $
                                /Quiet,        /NoPrint,                  $
                                GridInfo=Grid, _EXTRA=e )

   ; Error msg
   if ( not Success ) then begin
      ErrMsg = 'EMISSION_RATIOS: Cannot read ' + StrTrim( Category, 2 ) + $
               ' from '                        + StrTrim( Files[1], 2 ) + $
               ' for tracer '                  + Tracer
      Message, ErrMsg
   endif

   ; If this is a 3-D data block, then take 
   ; the ratio of the column emissions.
   if ( Lev[1] gt 1 ) then begin
      Data1 = Total( Data1, 3 )
      Data2 = Total( Data2, 3 )
   endif

   ; Compute the ratio
   Ratio = Data2 / Data1

   ; Fill NaN and Inf values with a missing data value
   Ind = Where( ~Finite( Ratio ) )
   if ( Ind[0] ge 0 ) then Ratio[Ind] = -9.99e30

   ;-----------------------------------------------------------------------
   ; Dirty hack to make ratios lower than 0.5 not show up as gray
   ; in the plots.  This is OK since we are plotting the data range of
   ; 0.5 - 2.0 and then saturating outside of that.  Maybe think of
   ; of a better fix later on. (bmy, 8/11/11)
   Ind = Where( Ratio gt 0.0 AND Ratio lt 0.5 )
   if ( Ind[0] ge 0 ) then Ratio[Ind] = 0.5e0
   ;-----------------------------------------------------------------------

   ; Free memory
   UnDefine, Data2
   UnDefine, Data1

   ; Return to main program
   return, Ratio

end

;------------------------------------------------------------------------------

pro PlotEmRatio, Data, PlotTitle, Grid, PS=PS, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTRATIO plots either the surface or 
   ; 500 hPa ratio of tracer between old and new versions 
   ; (bmy, 11/9/07, 6/1/11)
   ;====================================================================

   ; Don't plot the polar latitudes
   XMid      = Grid.XMid
   YMid      = Grid.YMid[ 1:Grid.JMX-2 ]
   Data      = Data[ *, 1:Grid.JMX-2 ]

   ; Settings for the plot
   MinData  = 0.5
   MaxData  = 2.0
   BotOut   = !MYCT.GRAY50 
   Triangle = 1
   NoGap    = 1
   Log      = 1                               
   Div      = 8
   Annote   = [ '0.50', '0.61', '0.74', '0.90',                        $
                '1.10', '1.34', '1.64', '2.00'  ]

   ; We need to set the colorbar a little bit lower for PostScript
   if ( Keyword_Set( PS ) ) $
     then CbPos = [ 0.05, -0.02, 0.95, 0.01 ] $
     else CbPos = [ 0.05,  0.00, 0.95, 0.03 ]

   ; Create the title for the top of the plot panel
   if ( StrPos( PlotTitle, 'sink' ) ge 0 )                             $
      then Title = 'Ratio: ' + StrTrim( PlotTitle, 2 )                 $
      else Title = 'Ratio: ' + StrTrim( PlotTitle, 2 ) + ' emissions'

   ; Plot ratios over a world map
   TvMap, Data, XMid, Ymid,                                            $
      /Countries,         /Coasts,             /Cbar,                  $
      Division=Div,       /Sample,             /Grid,                  $
      Title=Title,        MinData=MinData,     MaxData=MaxData,        $
      Min_Valid=0.5,      CBFormat='(f13.3)',  BOR_Label=' ',          $
      Triangle=Triangle,  NoGap=NoGap,         BotOut=BotOut,          $
      Log=Log,            Annotation=Annote,   CbPosition=CbPos,       $
      /isotropic,         _EXTRA=e

end

;------------------------------------------------------------------------------

pro CreatePlots, Files, Taus,     Category, Tracer, $
                 Title, TopTitle, Lev,      PS=PS,  $
                 _EXTRA=e

   ;====================================================================
   ; Internal routine CREATEPLOTS is a convenience wrapper.  It calls
   ; COMPUTERATIO to compute the emission ratio and then PLOTRATIO
   ; to add the plot to the output file. (bmy, 6/10/11)
   ;====================================================================

   ; Number of rows & columns for the plot
   Rows = 3
   Cols = 2

   ; Set multiple panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ; Loop over each diagnostic quantity
   for D = 0L, N_Elements( Category )-1L do begin

      ; Assume that a specified level greater than 1 is calling 
      ; for the ratio of the column data to be printed
      if ( Lev[D] gt 1 )           $
         then LLev = [ 1, Lev[D] ] $
         else LLev = [ 1, 1      ]

      ; Echo info
      print, 'Computing emission ratio for ' + $
             StrTrim( Category[D], 2 )       + $
             ', Tracer # '                   + $
             StrTrim( String( Tracer[D] ), 2 )

      ; Compute the ratio
      Ratio = ComputeEmRatio( Files,            Taus,      Category[D], $
                              Tracer=Tracer[D], Grid=Grid, Lev=LLev,    $
                              _EXTRA=e )

      ; Create the ratio plot
      PlotEmRatio, Ratio, Title[D], Grid, PS=PS, _EXTRA=e

      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ; Undefine variables
      Undefine, Grid
      Undefine, Ratio

   endfor

   ; Cancel multipanel settings
   Multipanel, /Off

end

;------------------------------------------------------------------------------

pro CO_Emission_Ratios, Files, Taus,          Versions, $
                         PS=PS, OutFileName=OutFileName, _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================

   ; External functions
   FORWARD_FUNCTION Add_Separator, ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( Files    ) ne 2 ) then Message, 'Invalid FILES!'
   if ( N_Elements( Taus     ) ne 2 ) then Message, 'Invalid TAUS!'
   if ( N_Elements( Versions ) ne 2 ) then Message, 'Invalid VERSIONS!'

   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'emission_ratios.ps'
   
   ; Top-of-plot title string
   TopTitle = 'GEOS-Chem '               + Versions[1]                  + $
              ' Emission ratio maps!C!C' + Extract_FileName( Files[1] ) + $
              ' / '                      + Extract_FileName( Files[0] ) 

   ; Save original color table
   TvLct, R, G, B, /Get

   ; Save current !MYCT sysvar settings 
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; This colortable will show 0.9 - 1.0 in the white, and anything
   ; outside that in the red or blue.  Use 8 colorbar divisions to line
   ; everything up properly. (bmy, 5/21/10)
   ;
   ; NOTE: It is really 0.9057 - 1.0104.  Close enough for gov't work.
   MyCt, 63, NColors=14, /MidCol, /White, /Reverse, _EXTRA=e

   ;====================================================================
   ; Create emission ratio plots for CO
   ;====================================================================

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Categories for CO
   Category = [ 'CO--SRCE',      'CO--SRCE',   'CO--SRCE',    $
                'CO--SRCE',      'CO--SRCE'                  ]

   ; Plot titles for CO
   Title    = [ 'CO anthro',     'CO biofuel', 'CO biomass',  $
                'CO from CH4',   'CO from NMVOC'               ]

   ; Tracers & levels for CO
   Tracer   = [ 1, 3, 2, 4, 5 ]
   MaxLev   = [ 1, 1, 1, 1, 1 ]

   ; Create the emission ratio plots for CO
   CreatePlots, Files,   Taus,     Category, Tracer,          $
                Title,   TopTitle, MaxLev,   PS=PS,           $
                _EXTRA=e

   ; Cancel multipanel settings
   Multipanel, /Off

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================
Quit:

   ; Close previous MULTIPANEL settings
   MultiPanel, /Off

   ; Close plot device
   Close_Device

   ; Restore original color table
   TvLct, R, G, B

   ; Restore previous !MYCT sysvar settings
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Quit
   return
end
 
