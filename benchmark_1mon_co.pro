;-----------------------------------------------------------------------
;+
; NAME:
;        BENCHMARK_1MON_CO
;
; PURPOSE:
;        Produces maps of CO and other quantities from two
;        GEOS-Chem tagged CO simulations (for model validation).
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        BENCHMARK_1MON_CO, INPUTFILE, [, Keywords ]
;
; INPUTS:
;        INPUTFILE -> A file containing default values for version 
;             numbers, directories, model names, resolutions, etc.
;             Default is "input_bm.1mon"
;
; KEYWORD PARAMETERS:
;        By default, BENCHMARK_1MON will produce the following types
;        of output:
;
;          (a) Table of Ox and CO budgets, mean OH conc. and CH3CCL3 lifetime 
;          (b) Table of emissions totals
;          (c) Frequency distribution histogram
;          (d) Vertical profiles of tracer differences along 15S and 42N 
;          (e) Maps of tracer concentration @ surface and 500 hPa
;          (f) Maps of tracer differences   @ surface and 500 hPa
;          (h) Maps of tracer ratios        @ surface and 500 hPa
;
;        Each of these types of output can be turned off individually
;        with the following keywords:
; 
;        /NO_BUDGET -> Do not create the table of Ox and CO budgets,
;             mean OH concentration and CH3CCl3 lifetime.
;
;        /NO_EMISSIONS -> Do not create the table of emissions totals. 
;
;        /NO_FREQ_DIST -> Do not create the the frequency distribution
;             histogram plot.
;        
;        /NO_PROFILES -> Do not create the plot of vertical profiles 
;             of tracer differences along 15S and 42N.
;
;        /NO_CONC_MAPS -> Do not create the plot the maps of tracer
;             concentrations @ sfc and 500 hPa altitude.
;
;        /NO_DIFFS -> Do not create the maps of tracer differences
;             at the surface and 500 hPa altitude.
;
;        /NO_RATIOS -> Do not create the maps of tracer ratios at
;             the surface and 500 hPa altitude.
;
;        Additional keywords:
;        --------------------
;
;        /DEBUG -> Set this switch to print the values of the various
;             input variables.  Use this to make sure that all values
;             have been created corectly.
;
;        /DYNRANGE -> Set this switch to create additional difference 
;             plots, ratio plots, and profile plots using the whole
;             dynamic range of the data.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines:
;        ====================================================
;        DynOutFile (function)    GetSfc500Levels (function)  
;
;        External Subroutines Required:
;        ====================================================
;        CTM_NAMEXT (function)    CTM_TYPE (function) 
;        DIFFERENCES              FREQ_DIST         
;        FULLCHEM_BUDGET          FULLCHEM_EMISSIONS
;        JV_RATIO                 PROFILES
;        MAPS                     NYMD2TAU (function)
;        RATIOS                   REPLACE_TOKEN
;        STRUADDVAR (function)    MCF_LIFETIME (function)
;
; REQUIREMENTS:
;        References routines from the GAMAP package.
;
; NOTES:
;        BENCHMARK_1MON assumes that the following GEOS-Chem
;        diagnostic quantities have been archived for the 
;        "old" and "new" model versions:
;
;          (a) ND22 ("JV-MAP-$")   (h) ND44 ("DRYD-FLX")
;          (b) ND24 ("EW-FLX-$")   (i) ND45 ("IJ-AVG-$")
;          (c) ND25 ("NS-FLX-$")   (j) ND65 ("PORL-L=$")
;          (d) ND26 ("UP-FLX-$")   (k) ND66 ("DAO-3D-$")
;          (e) ND31 ("PEDGE-$" )   (l) ND67 ("DAO-FLDS")
;          (f) ND43 ("CHEM-L=$")   (m) ND69 ("DXYP"    )
;          (g) ND32 (""NOX-AC-$", "NOX-AN-$", "NOX-BIOB", 
;                     "NOX-FERT", "NOX-LI-$", "NOX-SOIL")
;
; EXAMPLES:
;        BENCHMARK_1MON, 'input.1mon'
;
;            ; Produces the full suite of benchmark output plots.
;
;        BENCHMARK_1MON, 'input.1mon', /DYNRANGE
;
;            ; Produces the full suite of benchmark output plots.
;            ; Will also produce an additional set of difference and
;            ; ratio maps using the full dynamic range of the data.
;
;        BENCHMARK_1MON, 'input.1mon', /DEBUG, /NO_FREQ_DIST
;
;            ; Will produce the standard plots except for the
;            ; frequency distribution histogram.  Also will cause
;            ; extra information to be echoed to the screen.
;
; MODIFICATION HISTORY:
;        bmy, 09 Nov 2007: VERSION 1.01
;                          - based on "benchmark.pro"
;        bmy, 10 Jan 2011: VERSION 1.02
;                          - Now set proper symbolic links to
;                            diaginfo.dat and tracerinfo.dat 
;                          - Set 500hPa level for MERRA
;                          - Added /NO_PROFILES keyword to suppress
;                            printing of vertical profiles
;        bmy, 08 Jun 2011: VERSION 1.03
;                          - Updated comments
;                          - Added /NO_BUDGET, /NO_EMISSIONS, 
;                            /NO_PROFILES, /NO_CONC_MAPS, /NO_DIFFS, 
;                            /NO_JVALUES, /NO_RATIOS, /NO_FULLCHEM 
;                            keywords.
;                          - Now pass _EXTRA=e to all routines
;                          - Now use FILE_WHICH to locate the 
;                            diff_range.1mon file
;                          - Now look for diaginfo.dat and 
;                            tracerinfo.dat in RUNDIR_2.  Do not
;                            use symbolic links anymore.
;        bmy, 10 Jun 2011: - Now call EMISSION_RATIOS
;        bmy, 23 Jun 2011  - Now call ZONAL_DIFF
;        bmy, 18 Jul 2011  - Now pass /PRESSURE keyword to ZONAL_DIFF
;                            to create plots w/ pressure on Y-axis
;        bmy, 11 May 2012: GAMAP VERSION 2.16
;                          - Now do not stop the run if the two model 
;                            grids are equivalent.  This allows
;                            comparisons between GEOS-5, MERRA,
;                            GEOS-5.7 data.  
;                          - Return 500hPa level for GEOS-5.7 met
;
;-
; Copyright (C) 2007-2012,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.harvard.edu with subject "IDL routine benchmark_1mon"
;-----------------------------------------------------------------------

; Resolve external routines
@ ./subroutines/read_diff_range.pro
@ ./subroutines/get_diff_range.pro
@ ./subroutines/maps_co.pro
@ ./subroutines/co_budget.pro
@ ./subroutines/co_emissions.pro
@ ./subroutines/co_emission_ratios.pro

function DynOutFile, File

   ;====================================================================
   ; Internal function DynOutFile appends the the extension 
   ; '.dyn_range' to a PostScript filename in order to denote
   ; that the filename contains the dynamic range of data, as opposed
   ; to the "small" range of data. (bmy, 11/14/07)
   ;====================================================================

   ; Add text and return
   return, StrMid( File, 0, StrLen( File )-3L ) + '.dyn_range.ps' 
end

;------------------------------------------------------------------------------

function GetSfc500Levels, ModelName

   ;====================================================================
   ; Internal function GetSfc500Level returns the surface and 500hPa 
   ; levels, depending on the met field type (bmy, 11/14/07)
   ;====================================================================

   ; Surface level
   Level_Sfc  = 1

   ; 500hPa level
   case ( ModelName ) of
      'geos57' : Level_500 = 23
      'merra'  : Level_500 = 23
      'geos5'  : Level_500 = 23
      'geos4'  : Level_500 = 8
      'geos3'  : Level_500 = 13
      else     : Level_500 = -1
   endcase

   ; Return to calling program
   return, [ Level_Sfc, Level_500 ]
end

;-----------------------------------------------------------------------------

pro BenchMark_1Mon_CO, InputFile,                 $
                    Debug        = Debug,         $
                    DynRange     = DynRange,      $
                    No_Budget    = No_Budget,     $
                    No_Conc_Maps = No_Conc_Maps,  $
                    No_Diffs     = No_Diffs,      $
                    No_Emissions = No_Emissions,  $
                    No_Freq_Dist = No_Freq_Dist,  $
                    No_Profiles  = No_Profiles,   $
                    No_Ratios    = No_Ratios,     $ 
                    No_Zonal     = No_Zonal,      $
                    _EXTRA=e
   
   ;====================================================================
   ; Initialization 
   ;====================================================================

   ; External Functions
   FORWARD_FUNCTION CTM_NamExt,    CTM_Type,  Nymd2Tau, $
                    Replace_Token, StruAddVar

   ; Arguments
   if ( N_Elements( InputFile ) eq 0 ) then InputFile = 'input_bm.1mon_CO'

   ; Keywords
   Debug        =  Keyword_Set( Debug        )
   DynRange     =  Keyword_Set( DynRange     )
   Do_Budget    = ~Keyword_Set( No_Budget    )
   Do_Conc_Maps = ~Keyword_Set( No_Conc_Maps ) 
   Do_Diffs     = ~Keyword_Set( No_Diffs     )
   Do_Emissions = ~Keyword_Set( No_Emissions )
   Do_Freq_Dist = ~Keyword_Set( No_Freq_Dist )
   Do_Profiles  = ~Keyword_Set( No_Profiles  )
   Do_Ratios    = ~Keyword_Set( No_Ratios    )
   Do_Zonal     = ~Keyword_Set( No_Zonal     )
   ; This is for tagged CO; do_fullchem will always be off
   Do_FullChem  = 0
  
   ; Read the default ranges for difference plots
   Read_Diff_Range, File_Which( 'diff_range_CO' )
   
   ;====================================================================
   ; Read default settings from INPUTFILE
   ;====================================================================

   ; Open the input file
   Open_File, InputFile, Ilun, /Get_Lun

   ; Define string variable
   Line = ''
   
   ; Loop thru file
   while ( not EOF( Ilun ) ) do begin

      ; Read line and break on colon
      ReadF, Ilun, Line
      Result = StrTrim( StrBreak( Line, ':' ), 2 )

      ; Parse value of line into individual variables
      case ( StrUpCase( Result[0] ) ) of 
         'VERSION_1'   : Version_1   = Result[1]
         'VERSION_2'   : Version_2   = Result[1]
         'MODEL_1'     : Model_1     = CTM_NamExt( CTM_Type( Result[1] ) )
         'MODEL_2'     : Model_2     = CTM_NamExt( CTM_Type( Result[1] ) )
         'DATE_1'      : Tau0_1      = Nymd2Tau( Long( Result[1] ) )
         'DATE_2'      : Tau0_2      = Nymd2Tau( Long( Result[1] ) )
         'N_TRACERS_1' : N_Tracers_1 = Long( Result[1] )
         'N_TRACERS_2' : N_Tracers_2 = Long( Result[1] )
         'RUNDIR_1'    : RunDir_1    = Result[1]
         'RUNDIR_2'    : RunDir_2    = Result[1]
         'FILE_1'      : File_1      = Result[1]
         'FILE_2'      : File_2      = Result[1]
         'INITFILE_2'  : InitFile_2  = Result[1]
         'FINALFILE_2' : FinalFile_2 = Result[1]
         'OUTPUTDIR'   : OutPutDir   = Result[1]
         'MAX_ALT_KM'  : Max_Alt_Km  = Long( Result[1] )
         'BUDGET'      : Budget      = Result[1] 
         'CONC_MAPS'   : Conc_Plots  = Result[1]
         'DIFFERENCES' : Diff_Plots  = Result[1]
         'EMISSIONS'   : Emissions   = Result[1] 
         'EM_RATIOS'   : Em_Ratios   = Result[1] 
         'FREQ_DIST'   : Freq_Dist   = Result[1]
         'PROFILES'    : Prof_Plots  = Result[1] 
         'RATIOS'      : Ratio_Plots = Result[1]
         'ZONAL_DIFFS' : Zonal_Plots = Result[1] 
         else          : ; Do nothing
      endcase
   endwhile

   ; Close input filename
   Close,    Ilun
   Free_Lun, Ilun

   ;----------------
   ; Error checks!
   ;----------------

   ; Skip error check if we are comparing two different model types
   if ( Do_Freq_Dist ) then begin

      ; Can't plot if the vertical grids are different!
      if ( Model_1 ne Model_2 ) then begin
         
         ; First determine if both grids are equivalent even if the
         ; model names differ (e.g. GEOS-5.2, MERRA, GEOS-5.7)
         Models = [ Model_1, Model_2 ]
         F0     = ( Where( Models eq 'GEOS5_47L'  ) ge 0 )
         F1     = ( Where( Models eq 'MERRA_47L'  ) ge 0 )
         F2     = ( Where( Models eq 'GEOS57_47L' ) ge 0 )

         ; If both grids are not equivalent, then stop w/ error message
         if ( ( F0 + F1 + F2 ) ne 2 ) then  begin
            Message, $
               'The two models have different vertical grids!  Cannot plot!'
         endif
      endif

       ; Can't plot if the # of tracers are different!
      if ( N_Tracers_1 ne N_Tracers_2 ) then begin
         Message, 'The two models have a different # of tracers!  Cannot plot!'
      endif

   endif

   ;====================================================================
   ; Create directory, filename, and other relevant variables
   ;====================================================================

   ; Define structure for token replacement
   Token       = { VERSION_1:Version_1,  VERSION_2:Version_2, $
                   MODEL_1  :Model_1,    MODEL_2  :Model_2   }

   ; Replace tokens in run directory variables
   RunDir_1    = Replace_Token( RunDir_1, Token )
   RunDir_2    = Replace_Token( RunDir_2, Token )

   ; Add run directory variables to TOKEN structure
   Token       = StruAddVar( Token, { RUNDIR_1:RunDir_1, RUNDIR_2:RunDir_2 } )

   ; Replace tokens in output directory variable
   OutputDir   = Replace_Token( OutputDir,   Token )

   ; Add output directory to TOKEN structure
   Token       = StruAddVar( Token, { OUTPUTDIR:OutputDir } )

   ; Replace tokens in the rest of the variables
   File_1      = Replace_Token( File_1,      Token ) 
   File_2      = Replace_Token( File_2,      Token ) 
   InitFile_2  = Replace_Token( InitFile_2,  Token ) 
   FinalFile_2 = Replace_Token( FinalFile_2, Token ) 
   Budget      = Replace_Token( Budget,      Token )
   Conc_Plots  = Replace_Token( Conc_Plots,  Token )
   Diff_Plots  = Replace_Token( Diff_Plots,  Token )
   Emissions   = Replace_Token( Emissions,   Token )
   Em_Ratios   = Replace_Token( Em_Ratios,   Token )
   Freq_Dist   = Replace_Token( Freq_Dist,   Token )
   Prof_Plots  = Replace_Token( Prof_Plots,  Token )
   Ratio_Plots = Replace_Token( Ratio_Plots, Token )
   Zonal_Plots = Replace_Token( Zonal_Plots, Token )

   ; Altitude range (km) for difference profile plots
   AltRange    = [ 0L, Max_Alt_Km ]

   ; Surface and 500hPa levels from both models ( sfc1, sfc2, 500_1, 500_2 )
   Levels_1    = GetSfc500Levels( Model_1 )
   Levels_2    = GetSfc500Levels( Model_2 )
   Levels      = [ Levels_1[0], Levels_2[0], Levels_1[1], Levels_2[1] ]

   ; TAU0 values
   Taus        = [ Tau0_1, Tau0_2 ]

   ; File names
   Files       = [ File_1, File_2 ]

   ; Tracer list
   Tracers     = LindGen( N_Tracers_1 ) + 1L

   ; Model versions
   Versions    = [ Version_1, Version_2 ]

   ;====================================================================
   ; Debug output
   ;====================================================================
   if ( Debug ) then begin

      print, '%%% 1st Model %%%'
      print, 'Version_1   : ', Version_1
      print, 'Model_1     : ', Model_1     
      print, 'Tau0_1      : ', Tau0_1       
      print, 'N_Tracers_1 : ', N_Tracers_1
      print, 'RunDir_1    : ', RunDir_1    
      print, 'File_1      : ', File_1      
      print
      print, '%%% 2nd Model %%%'      
      print, 'Version_2   : ', Version_2
      print, 'Model_2     : ', Model_2     
      print, 'Tau0_2      : ', Tau0_2       
      print, 'N_Tracers_2 : ', N_Tracers_2 
      print, 'RunDir_2    : ', RunDir_2    
      print, 'File_2      : ', File_2      
      print, 'InitFile_2  : ', InitFile_2  
      print, 'FinalFile_2 : ', FinalFile_2
      print
      print, '%%% For Plotting %%%'
      print, 'OutputDir   : ', OutputDir   
      print, 'Altrange    : ', Altrange
      print, 'Freq_Dist   : ', Freq_Dist  
      print, 'Diff Plots  : ', Diff_Plots
      print, 'Ratio Plots : ', Ratio_Plots 
      print, 'Prof_Plots  : ', Prof_Plots
      print, 'Conc_Plots  : ', Conc_Plots
      print, 'Budget      : ', Budget  
      print, 'Emissions   : ', Emissions  
      print, 'Em_Ratios   : ', Em_Ratios  

   endif

   ;====================================================================
   ; Use the metadata from the diaginfo.dat and tracerinfo.dat
   ; files contained in the run directory.  Refresh every time.
   ;====================================================================

   ; File names 
   DInfo = StrTrim( RunDir_2, 2 ) + '/diaginfo.dat'
   TInfo = StrTrim( RunDir_2, 2 ) + '/tracerinfo.dat' 

   ; Load metadata from diaginfo.dat (if we find it)
   if ( File_Exist( Dinfo ) )                                           $
      then CTM_DiagInfo, FileName=DInfo, /Force                         $ 
      else Message, 'Could not find the proper diaginfo.dat file!'

   ; Load metadata from tracerinfo.dat (if we find it)
   if ( File_Exist( Tinfo ) )                                           $
      then CTM_TracerInfo, FileName=Tinfo, /Force                       $ 
      else Message, 'Could not find the proper diaginfo.dat file!'

   ;====================================================================
   ; Make the plots
   ;====================================================================

   ;--------------------------------
   ; Frequency distribution
   ;--------------------------------
   if ( Do_Freq_Dist ) then begin

      ; Echo info
      Message, 'Generating frequency distributions ...', /Info

      ; Create frequency distribution plot
      Freq_Dist, Files, Taus, Tracers, Versions,                        $
                 /LVarTrop,  Do_FullChem=Do_FullChem,                   $
                 /PS,        OutFile=Freq_Dist,                         $
                 _EXTRA=e                                               
                                                                        
   endif                                                                
                                                                        

   ;---------------------------------                                   
   ; J-value ratios @ sfc & 500hPa - not appropriate for tagged CO; removed (jaf,5/17/13)
   ;---------------------------------                                   

   ;---------------------------------
   ; Tracer ratios @ sfc & 500hPa
   ;---------------------------------
   if ( Do_Ratios ) then begin
   
      ; Echo info
      Message, 'Generating tracer ratio maps ...', /Info
      
      ; Create ratio plots with "small" data range
      Ratios, Files, Levels, Taus, Tracers, Versions, /PS,               $
               OutFile=Ratio_Plots, Do_FullChem=Do_FullChem, _EXTRA=e
                 
      ; Create ratio plots with full dynamic range (if necessary)       
      if ( DynRange ) then begin                                        
         Ratios, Files, Levels, Taus, Tracers, Versions,                $
                 /DynRange, Do_FullChem=Do_FullChem,                    $
                 /PS,       OutFile=DynOutFile( Ratio_Plots ),          $
                 _EXTRA=e
      endif

   endif

   ;---------------------------------
   ; Tracer abs diff @ sfc & 500hPa
   ;---------------------------------
   if ( Do_Diffs ) then begin

      ; Echo info
      Message, 'Generating tracer difference maps ...', /Info
   
      ; Create diff plots with "small" data range
      Differences, Files, Levels, Taus, Tracers, Versions,              $
                   Do_FullChem=Do_FullChem, /PS,                        $
                   OutFile=Diff_Plots,      _EXTRA=e                    
                                                                        
      ; Create diff plots with full dynamic range (if necessary)        
      if ( DynRange ) then begin                                        
         Differences, Files, Levels, Taus, Tracers, Versions,           $
                      /DynRange, Do_FullChem=Do_FullChem,               $
                      /PS,       OutFile=DynOutFile( Diff_Plots )
      endif

   endif

   ;---------------------------------
   ; Tracer profiles @ sfc & 500hPa
   ;---------------------------------
   if ( Do_Profiles ) then begin
   
      ; Echo info
      Message, 'Generating difference profiles along 15S and 42N ...', /Info
   
      ; Create lon diff profile plots w/ "small" data range
      Profiles, Files, AltRange, Taus, Tracers, Versions,               $
                Do_FullChem=Do_FullChem, /PS,                           $
                OutFile=Prof_Plots,      _EXTRA=e                       
                                                                        
      ; Create diff plots with full dynamic range (if necessary)        
      if ( DynRange ) then begin                                        
         Profiles, Files, AltRange, Taus, Tracers, Versions,            $
                   /DynRange, Do_FullChem=Do_FullChem,                  $
                   /PS,       OutFile=DynOutFile( Prof_Plots ),         $
                   _EXTRA=e
      endif
   
   endif

   ;---------------------------------
   ; Zonal mean difference maps 
   ;---------------------------------
   if ( Do_Zonal ) then begin

      ; Echo info
      Message, 'Generating zonal mean difference plots ...', /Info
   
      ; Create lon diff profile plots w/ "small" data range
      Zonal_Diff, Files, Taus, Tracers, Versions,                       $
                  Do_FullChem=Do_FullChem, /PS,                         $
                  OutFile=Zonal_Plots,     /Pressure,                   $
                  _EXTRA=e                       
                                                            
      ; Create diff plots with full dynamic range (if necessary)        
      if ( DynRange ) then begin   
         Zonal_Diff, Files, Taus, Tracers, Versions,                    $
                    /DynRange, Do_FullChem=Do_FullChem,                 $
                    /PS,       OutFile=DynOutFile( Zonal_Plots ),       $
                    /Pressure, _EXTRA=e
      endif

   endif

   ;---------------------------------
   ; Tracer concentration maps 
   ;---------------------------------
   if ( Do_Conc_Maps ) then begin

      ; Echo info
      Message, 'Generating tracer concentration maps ...', /Info

      ; Create lon diff profile plots w/ "small" data range
      Maps_CO, Files, Levels, Taus, Tracers, Versions,            $
            Do_FullChem=Do_FullChem,  /PS,                        $
            OutFile=Conc_Plots,       _EXTRA=e

   endif

   ;---------------------------------
   ; Emission totals and maps
   ;---------------------------------
   if ( Do_Emissions ) then begin

      ; Echo info
      Message, 'Generating emission totals ...', /Info
      
      ; Compute table of emissions sums
      CO_Emissions, FileNew=File_2,       FileOld=File_1,            $
                    OutFile=Emissions,    VersionNew=Version_2,      $
                    VersionOld=Version_1, _EXTRA=e

      ; Echo info
      Message, 'Generating emission ratio maps ...', /Info

      ; Compute emission ratio maps
      CO_Emission_Ratios, Files, Taus,              Versions,  $
                          /PS,   OutFile=Em_Ratios, _EXTRA=e

   endif

   ;---------------------------------
   ; Budget printout
   ;---------------------------------
   if ( Do_Budget ) then begin
   
      ; Echo info
      Message, 'Generating budgets ...', /Info

      ; Create budget of Ox & CO, mean OH concentration, and MCF lifetime
      CO_Budget, Tau0=Taus[1],           FileNew=File_2,          $
                 InitialFile=InitFile_2, FinalFile=FinalFile_2,   $
                 OutFile=Budget,         _EXTRA=e

   endif

   ;====================================================================
   ; Cleanup and Quit
   ;====================================================================
Quit:

end
