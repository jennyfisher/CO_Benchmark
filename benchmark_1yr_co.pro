;-----------------------------------------------------------------------
;+
; NAME:
;        BENCHMARK_1YR_CO
;
; PURPOSE:
;        Produces plots of tracers (from 3 different GEOS-Chem 1-year 
;        benchmark simulations) vs. various observations.
;
; CATEGORY:
;        GEOS-CHEM Benchmarking
;
; CALLING SEQUENCE:
;        BENCHMARK_1YR_CO, INPUTFILE, [, Keywords ]
;
; INPUTS:
;        INPUTFILE -> A file containing default values for version 
;             numbers, directories, model names, resolutions, etc.
;             Default is "input/input_bm.1yr_CO"
;
; KEYWORD PARAMETERS:
;        PSDIR -> Specifies the directory path where PostScript output
;             files will be created.  Default is ./output.
;
;        RUNNAME -> A string label that will be appended to each of
;             the PostScritp files, to facilitate identification.
;             Default is to use the 3rd model name.
;
;        DEBUG -> Set this switch to print the values of the various
;             input variables.  Use this to make sure that all values
;             have been created corectly.
;
;        /DO_CMDL -> Set this switch to produce PostScript output 
;             plots for models vs. CMDL data observations.
;             Pair with /CMDL_1PG to only get a subset of sites
;
;        /DO_MOZAIC -> Set this switch to produce PostScript output 
;             plots for models vs. MOZAIC data observations.
;
;        /DO_AIRCRAFT -> Set this switch to produce PostScript output 
;             plots for models vs. observations from various aircraft 
;             missions.
;
;        /DO_PROFILES -> Set this switch to produce PostScript output 
;             plots for longitudinal profiles (lat-alt) of selected tracers.
;
;        /DO_DIFFS -> Set this switch to produce PostScript output 
;             plots for difference maps (lat-lon) of selected tracers 
;             at the surface and at 500hPa.
;        
;        /DO_RATIOS -> Set this switch to produce ratio maps at the surface
;             and at 500 hPa.
;
;        /DYNRANGE -> Set this switch to produce difference, ratio,
;             and longitudinal profile plots using the entire dynamic
;             range of the data (instead of the preset ranges).
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        See list of included subroutines below.
;
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        (1) Input files and the file w/ the default plotting
;             ranges are contained in the input/ subdirectory.
;        (2) Default is to write plots to the output/ subdirectory
;        (3) Temporary data files are written to the temp/ subdirectory
;
; EXAMPLES:
;        BENCHMARK_1YR_CO, /DEBUG
;             ; Test to see if default values were read in correctly,
;             ; but do not create any benchmark plots.
; 
; MODIFICATION HISTORY:
;  lzh & bmy, 07 Nov 2007: VERSION 1.01
;                          - Based on older code by Inna Megretskaia
;        bmy, 10 Mar 2008: VERSION 1.02
;                          - Change /users/trop/iam to /home/iam to reflect
;                            a change in the home directory path.  This is
;                            needed for the newer Linux logins.
;        bmy, 24 Mar 2008: VERSION 1.03
;                          - Now plot all 43 tracers in difference and
;                            profile plots
;                          - Also introduce 43-tracer ratio maps
;  lzh & bmy, 30 May 2008: VERSION 1.04
;                          - Add new types of plots for MOZAIC & CO
;                          - Reads updated data files.
;                          - Removed some repeated CO plots
;        clh, 07 Jul 2009: VERSION 1.05
;                          - Add aerosol benchmarking
;        bmy, 13 Jul 2009  - Now look for PM2.5 data in the pm25_data dir
;        ccc, 01 Oct 2010  - Updated station files for CO for MOZAIC and
;                            surface data. Also, extended the yrange for
;                            MOZAIC seasonal plots close to surface.
;
;-
; Copyright (C) 2007-2009, Inna Megretskaia, Lin Zhang,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.harvard.edu with subject "IDL routine benchmark_1yr"
;-----------------------------------------------------------------------

; Resolve external routines
@ ./subroutines/read_diff_range.pro
@ ./subroutines/get_diff_range.pro
@ ./subroutines/get_pressure_geos.pro
@ ./subroutines/get_species_geos.pro
@ ./subroutines/read_sondes_co_0_5.pro
@ ./subroutines/read_sondes_co_4lev_mozaic_0_5.pro
@ ./subroutines/all_stations_cmdl_geos.pro
@ ./subroutines/all_stations_geos.pro
@ ./subroutines/all_stations_geos_mozaic.pro
@ ./subroutines/all_stations_ships_geos.pro
@ ./subroutines/plot_4lev_co_geos_3_models_mozaic.pro
@ ./subroutines/plot_cmdl_3_models_4_months.pro
@ ./subroutines/plot_gridded_CO_vs_data_geos_3_models.pro
@ ./subroutines/plot_ships_3_models_4_months.pro
@ ./subroutines/plot_ships_3_models_co.pro
@ ./subroutines/plot_station_profiles_co_3_models_0_5.pro
@ ./subroutines/plot_surface_co_geos_3_models.pro
@ ./subroutines/profiles_1yr.pro
@ ./subroutines/differences_1yr.pro
@ ./subroutines/ratios_1yr.pro

;------------------------------------------------------------------------------

pro BenchMark_1yr_CO, InputFile,                                     $
                   RunName=RunName,         PSDir=PSDir,             $
                   Do_CMDL=DO_CMDL,         Do_MOZAIC=Do_MOZAIC,     $
                   Debug=Debug,             Do_Profiles=Do_Profiles, $
                   Do_Diffs=Do_Diffs,       Do_Ratios=Do_Ratios,     $
                   Do_Aircraft=Do_Aircraft, Do_ALL=Do_ALL,           $
                   DynRange=DynRange,       CMDL_1PG = CMDL_1PG,     $
		   _EXTRA=e

   ;====================================================================
   ; Initialization -- read default parameters from input file 
   ;====================================================================

   ; Save original color table
   TvLct, R, G, B, /Get

   ; Load a color table with the old MYCT drawing colors
   MyCt, /WhGrYlRd, /Bright_Colors

   ; Default input file
   if ( N_Elements( InputFile ) eq 0 ) then InputFile = 'input_bm.1yr_CO'

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
         'V1' : Vers1  = Result[1]
         'V2' : Vers2  = Result[1]
         'V3' : Vers3  = Result[1]
         'T1' : Type1  = strlowcase( Result[1] )
         'T2' : Type2  = strlowcase( Result[1] )
         'T3' : Type3  = strlowcase( Result[1] )
         'D1' : Dir1   = Add_Separator( Result[1] )
         'D2' : Dir2   = Add_Separator( Result[1] )
         'D3' : Dir3   = Add_Separator( Result[1] )
         'L1' : Label1 = Result[1]
         'L2' : Label2 = Result[1]
         'L3' : Label3 = Result[1]
         'P1' : PLabel1 = Result[1]
         'P2' : PLabel2 = Result[1]
         'P3' : PLabel3 = Result[1]
         'M1' : Model1 = Result[1]
         'M2' : Model2 = Result[1]
         'M3' : Model3 = Result[1]
         'R1' : Res1   = Long( Result[1] )
         'R2' : Res2   = Long( Result[1] )
         'R3' : Res3   = Long( Result[1] )
         'C1' : Cat1   = Result[1]
         'C2' : Cat2   = Result[1]
         'C3' : Cat3   = Result[1]
         'Y1' : Year1  = Result[1]
         'Y2' : Year2  = Result[1]
         'Y3' : Year3  = Result[1]
         else : ; Do nothing
      endcase
   endwhile

   ; Close input file
   Close,    Ilun
   Free_Lun, Ilun


   ;====================================================================
   ; Define some plotting variables
   ;====================================================================

   ; For 1st model
   PREF1      = Dir1 + Label1
   ModelInfo1 = CTM_Type( Model1, Res=Res1 )
   GridInfo1  = CTM_Grid( ModelInfo1 )
   Dlat1      = GridInfo1.DJ
   Dlon1      = GridInfo1.DI
   Ptop1      = GridInfo1.Pedge[ GridInfo1.LMX ]
   Nalt1      = GridInfo1.LMX
   LongCat1   = Cat1+'-S__CO'
   ShortCat1  = Cat1+'-$'

   ; For 2nd model
   PREF2      = Dir2 + Label2
   ModelInfo2 = CTM_Type( Model2, Res=Res2 )
   GridInfo2  = CTM_Grid( ModelInfo2 )
   Dlat2      = GridInfo2.DJ
   Dlon2      = GridInfo2.DI
   Ptop2      = GridInfo2.Pedge[ GridInfo2.LMX ]
   Nalt2      = GridInfo2.LMX
   LongCat2   = Cat2+'-S__CO'
   ShortCat2  = Cat2+'-$'

   ; For 3rd model
   PREF3      = Dir3 + Label3
   ModelInfo3 = CTM_Type( Model3, Res=Res3 )
   GridInfo3  = CTM_Grid( ModelInfo3 )
   Dlat3      = GridInfo3.DJ
   Dlon3      = GridInfo3.DI
   Ptop3      = GridInfo3.Pedge[ GridInfo3.LMX ]
   Nalt3      = GridInfo3.LMX
   LongCat3   = Cat3+'-S__CO'
   ShortCat3  = Cat3+'-$'

   ; Pressure file specified? If not use regular file
   if n_elements(PLabel1) eq 0 then PLabel1 = PREF1
   if n_elements(PLabel2) eq 0 then PLabel2 = PREF2
   if n_elements(PLabel3) eq 0 then PLabel3 = PREFl3

   ; Create plot title for top of page
   Title1  = 'Red: '   + Strtrim( String( Vers1 ), 2 ) + ' (' $
                       + Strtrim( String( Year1 ), 2 ) + ')'
   Title2  = 'Green: ' + Strtrim( String( Vers2 ), 2 ) + ' (' $
                       + Strtrim( String( Year2 ), 2 ) + ')'
   Title3  = 'Blue: '  + Strtrim( String( Vers3 ), 2 ) + ' (' $
                       + Strtrim( String( Year3 ), 2 ) + ')'
   Title   = Title1 + ';  ' + Title2 + ';  ' + Title3

   ; RUNNAME is the tag for the PostScript files
   if ( N_Elements( RunName ) eq 0 ) then RunName = Vers3

   ; Redirect PostScript output (end w/ slash)
   if ( N_Elements( PSDir ) eq 0 ) then PSDir   = './output/'

   ; Print debug output
   DO_Debug    = Keyword_Set( Debug       )

   ; Keywords for CMDL, MOZAIC, Aircraft, Lon Profile, and difference plots
   Do_CMDL     = Keyword_Set( Do_CMDL     )
   CMDL_1PG    = Keyword_Set( CMDL_1PG    )
   Do_MOZAIC   = Keyword_Set( Do_MOZAIC   )
   Do_Aircraft = Keyword_Set( Do_Aircraft )
   Do_Profiles = Keyword_Set( Do_Profiles )
   Do_Diffs    = Keyword_Set( Do_Diffs    )
   Do_Ratios   = Keyword_Set( Do_Ratios   )   
   DynRange    = Keyword_Set( DynRange    )

   if Keyword_Set( Do_ALL ) then begin
      Do_CMDL     = 1
      Do_MOZAIC   = 1
      Do_Aircraft = 1
      Do_Profiles = 1
      Do_Diffs    = 1
      Do_Ratios   = 1
   endif
   
   ; Define suffix for plots
   if ( DynRange )                    $
      then PsSuffix = '.dyn_range.ps' $
      else PsSuffix = '.ps'       

   ;=================================================================
   ; Debug section -- print out values
   ;=================================================================
   if ( DO_Debug ) then begin
      
      ; 1st model
      print, '%%% 1st model %%%'
      print, 'Vers1  : ', Vers1
      print, 'Dir1   : ', Dir1
      print, 'Label1 : ', Label1
      print, 'Model1 : ', Model1
      print, 'Res1   : ', Res1
      print, 'Year1  : ', Year1
      print, 'Pref1  : ', Pref1
      print, 'Dlat1  : ', Dlat1
      print, 'Dlon1  : ', Dlon1
      print, 'Ptop1  : ', Ptop1    
      print, 'Nalt1  : ', Nalt1

      ; 2nd model
      print
      print, '%%% 2nd model %%%'
      print, 'Vers2  : ', Vers2
      print, 'Dir2   : ', Dir2
      print, 'Label2 : ', Label2
      print, 'Model2 : ', Model2
      print, 'Res2   : ', Res2
      print, 'Year2  : ', Year2
      print, 'Pref2  : ', Pref2
      print, 'Dlat2  : ', Dlat2
      print, 'Dlon2  : ', Dlon2
      print, 'Ptop2  : ', Ptop2    
      print, 'Nalt2  : ', Nalt2

      ; 3rd model
      print
      print, '%%% 3rd model %%%'
      print, 'Vers3  : ', Vers3
      print, 'Dir3   : ', Dir3
      print, 'Label3 : ', Label3
      print, 'Model3 : ', Model3
      print, 'Res3   : ', Res3
      print, 'Year3  : ', Year3
      print, 'Pref3  : ', Pref3
      print, 'Dlat3  : ', Dlat3
      print, 'Dlon3  : ', Dlon3
      print, 'Ptop3  : ', Ptop3    
      print, 'Nalt3  : ', Nalt3

   endif

   ;====================================================================
   ; Set links to the proper diaginfo.dat, tracerinfo.dat files
   ;====================================================================

   ; Remove existing file links
   Cmd = 'rm -f diaginfo.dat tracerinfo.dat'
   if ( DO_Debug ) then begin
      print 
      print, Cmd
   endif
   Spawn, Cmd

   ; Link to the diaginfo.dat file in the 2nd directory
   Cmd = 'ln -s ' + StrTrim( Dir1, 2 )+ 'diaginfo.dat .'
   if ( DO_Debug ) then print, Cmd
   Spawn, Cmd

   ; Link to the tracerinfo.dat file in the 2nd directory
   Cmd = 'ln -s ' + StrTrim( Dir1, 2 )+ 'tracerinfo.dat .'
   if ( DO_Debug ) then print, Cmd
   Spawn, Cmd

   ;====================================================================
   ; Create longitudinal profile plots
   ;====================================================================

   if ( Do_Profiles ) then begin

      ; Echo info
      print, 'PROFILES: Creating longitudinal profiles of data'

      ; Read the default plot ranges 
      Read_Diff_Range, 'diff_range_CO'

      ; Define tracers
      if (Type1 eq 'yes') then Tr1 = 1L else Tr1 = 4L
      if (Type2 eq 'yes') then Tr2 = 1L else Tr2 = 4L
      if (Type3 eq 'yes') then Tr3 = 1L else Tr3 = 4L
      Tracers  = [Tr1, Tr2, Tr3]

      ; Altitude range
      AltRange = [ 0, 20 ]

      ; Version numbers
      Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName   = PSDir + 'Lon_Profiles_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions,        $
                    Categ = [ShortCat1, ShortCat2, ShortCat3], $
                    Month='Jan', DynRange=DynRange,            $
                    /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Lon_Profiles_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions,        $
                    Categ = [ShortCat1, ShortCat2, ShortCat3], $
                    Month='Apr', DynRange=DynRange,            $
                    /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Lon_Profiles_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions,        $
                    Categ = [ShortCat1, ShortCat2, ShortCat3], $
                    Month='Jul', DynRange=DynRange,            $
                    /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Lon_Profiles_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions,        $
                    Categ = [ShortCat1, ShortCat2, ShortCat3], $
                    Month='Oct', DynRange=DynRange,            $
                    /PS,         OutFileName=PSName, _EXTRA=e

   endif

   ;====================================================================
   ; Create difference maps at surface and 500 hPa
   ;====================================================================

   if ( Do_Diffs ) then begin

      ; Echo info
      print, 'DIFFERENCES: Creating difference maps at sfc and 500 hPa'

      ; Read the default plot ranges 
      Read_Diff_Range, 'diff_range_CO'

      ; Define tracers
      Tracers  = 1L

      ; Version numbers
      Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName   = PSDir + 'Differences_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions,                   $
                       Month='Jan', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Differences_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions,                   $
                       Month='Apr', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Differences_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions,                   $
                       Month='Jul', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Differences_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions,                   $
                       Month='Oct', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

   endif

   ;====================================================================
   ; Create ratio maps at surface and 500 hPa
   ;====================================================================

   if ( Do_Ratios ) then begin

      ; Echo info
      print, 'RATIOS: Creating ratio maps at sfc and 500 hPa'

      ; Define tracers
      Tracers  = 1L

      ; Version numbers
      Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName   = PSDir + 'Ratios_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions,                        $
                  Month='Jan', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Ratios_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions,                        $
                  Month='Apr', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Ratios_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions,                        $
                  Month='Jul', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      Files    = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Ratios_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions,                        $
                  Month='Oct', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

   endif

   ;====================================================================
   ; Plot models vs. surface CO data
   ;====================================================================
   if ( DO_CMDL ) then begin
   
;      ;-----------------------------------------------------------------
;      ; %%%%% PLOTS OF MODELS vs. SURFACE CO DATA %%%%%
;      ;-----------------------------------------------------------------
;      print, 'IDL_CO: Plot models vs. surface CO data'
;   
;      if ( CMDL_1PG ) then begin
;      ;-----------------------------------------------------------------
;      Print, '-----Ground CO short'
;      ;-----------------------------------------------------------------
;      FilEst    = './data/Sites.ground.CO.short'
;      Max_Sta   = 16
;      PSName    = PSDir + 'surface.CO.geos.1page.' + RunName + '.ps'
;      Plot_surface_co_geos_3_models, Pref1, Ptop1,  Dlat1,   Dlon1, Model1, $
;                                     Pref2, Ptop2,  Dlat2,   Dlon2, Model2, $
;                                     Pref3, Ptop3,  Dlat3,   Dlon3, Model3, $
;                                     LongCat1,  LongCat2,   LongCat3,       $
;                                     PLabel1,  PLabel2,   PLabel3,          $
;                                     Title, PSName, Max_Sta, FilEst, Debug = Debug
;      endif
;   
;      ;-----------------------------------------------------------------
;      Print, '-----Ground CO 1'
;      ;-----------------------------------------------------------------
;      FilEst    = './data/Sites.ground.CO.2005'
;      Max_Sta   = 42
;      PSName    = PSDir + 'surface.CO.geos.' + RunName + '.ps'
;      Plot_surface_co_geos_3_models, Pref1, Ptop1,  Dlat1,   Dlon1, Model1, $
;                                     Pref2, Ptop2,  Dlat2,   Dlon2, Model2, $
;                                     Pref3, Ptop3,  Dlat3,   Dlon3, Model3, $
;                                     LongCat1,  LongCat2,   LongCat3,       $
;                                     PLabel1,  PLabel2,   PLabel3,          $
;                                     Title, PSName, Max_Sta, FilEst, Debug = Debug
   
      ;-----------------------------------------------------------------
      ; %%%%% PLOTS OF MODELS vs. CMDL and SHIP CO DATA %%%%%
      ;-----------------------------------------------------------------
      print, 'IDL_CO_2: Plot models vs. CMDL and ship CO data'
   
      ;-----------------------------------------------------------------
      print, '-----Reading CMDL data'
      ;-----------------------------------------------------------------
      all_stations_cmdl_geos, $
         'CO',Cat1,39, Pref1, PLabel1, Ptop1, Dlat1, Dlon1, Model1, '.1', $
          Debug = Debug
      all_stations_cmdl_geos, $
         'CO',Cat2,39, Pref2, PLabel2, Ptop2, Dlat2, Dlon2, Model2, '.2', $
          Debug = Debug
      all_stations_cmdl_geos, $
         'CO',Cat3,39, Pref3, PLabel3, Ptop3, Dlat3, Dlon3, Model3, '.3', $
          Debug = Debug
   
      ;-----------------------------------------------------------------
      print, '-----CMDL latitudinal distribution'
      ;-----------------------------------------------------------------
      PSName = PSDir + 'cmdl.latdist.' + RunName + '.ps'
      Plot_cmdl_3_models_4_months, '.1', '.2', '.3', title,PSName
   
   
;      ;-----------------------------------------------------------------
;      print, '-----Reading ship data'
;      ;-----------------------------------------------------------------
;      all_stations_ships_geos, $
;         'CO',Cat1, 13, Pref1, 0, Ptop1, Dlon1, Dlat1, Model1, '.1', Debug = Debug
;      all_stations_ships_geos, $
;         'CO',Cat2, 13, Pref2, 0, Ptop2, Dlon2, Dlat2, Model2, '.2', Debug = Debug
;      all_stations_ships_geos, $
;         'CO',Cat3, 13, Pref3, 0, Ptop3, Dlon3, Dlat3, Model3, '.3', Debug = Debug
;   
;      ;-----------------------------------------------------------------
;      print, '-----Ship CO'
;      ;-----------------------------------------------------------------
;      PSName = PSDir + 'CO.ships.geos.' + RunName + '.ps'
;      Plot_ships_3_models_co, '.1', '.2', '.3', title, PSName
;   
;      ;-----------------------------------------------------------------
;      print, '-----Ship tracks 4 months'
;      ;-----------------------------------------------------------------
;      PSName = PSDir + 'CO.ships.geos.4.months.' + RunName + '.ps'
;      Plot_ships_3_models_4_months, '.1', '.2', '.3', title, PSName
   
      ; Close files & clean up
      Close, /all
      Ctm_Cleanup, /No_GC 

      spawn,'rm -f temp/*'
   
   endif

   ;====================================================================
   ; Plot models vs. MOZAIC data
   ;====================================================================
   if ( Do_MOZAIC ) then begin

      print, 'IDL_MOZAIC: Plot models vs. MOZAIC data'

      ;-----------------------------------------------------------------
      print, '-----Reading MOZAIC stations'
      ;-----------------------------------------------------------------

      all_stations_geos_mozaic, $
         'CO', Cat1, 36, Pref1, Plabel1, Ptop1, Dlon1, Dlat1, Model1, '.1', $
          Debug = Debug
      all_stations_geos_mozaic, $
         'CO', Cat2, 36, Pref2, Plabel3, Ptop2, Dlon2, Dlat2, Model2, '.2', $
          Debug = Debug
      all_stations_geos_mozaic, $
         'CO', Cat3, 36, Pref3, Plabel3, Ptop3, Dlon3, Dlat3, Model3, '.3', $
          Debug = Debug

      ;--------------------------------------------------------
      ; lzh add new locations for MOZAIC CO   04/27/2008
      ;--------------------------------------------------------
      print, '------MOZAIC CO new locations'

      ; (1)  CO profiles
      FilEst  = './data/Sites.CO.prof.mozaic.monthly.new'
      PSName  = PsDir + 'CO.profiles.mozaic.months.' + RunName + '.15sta.ps'
      Max_Station = 15
      Plot_station_profiles_co_3_models_0_5, 1,     4,     7,     10,      $ 
                                       Pref1, Ptop1, Dlat1, Dlon1, Model1, $
                                       Pref2, Ptop2, Dlat2, Dlon2, Model2, $
                                       Pref3, Ptop3, Dlat3, Dlon3, Model3, $
                                       PLabel1,  PLabel2,   PLabel3,       $
                                       title, PSName, Max_Station, FilEst

      ; (2) seasonal plots at 4 levels
      FilEst  = './data/Sites.CO.prof.mozaic.new'
      PSName  = PsDir+'CO.seas.cycle.mozaic.geos.'+RunName+'.4lev.ps'
      Plot_4lev_co_geos_3_models_mozaic, Pref1, Ptop1, Dlat1, Dlon1, Model1, $
                                         Pref2, Ptop2, Dlat2, Dlon2, Model2, $
                                         Pref3, Ptop3, Dlat3, Dlon3, Model3, $
                                         PLabel1,  PLabel2,   PLabel3,       $
                                         title, PSName, 15, FilEst, Debug = Debug

      ; Close files & cleanup
      Close, /all
      Ctm_Cleanup, /No_GC 

      spawn,'rm -f temp/*'

   endif

   ;====================================================================
   ; Plot models vs. aircraft data
   ;====================================================================
   if ( Do_Aircraft ) then begin

      print, 'IDL_AIRCRAFT: Plot models vs. aircraft data'
 
      ;-----------------------------------------------------------------
      print, '-----CO'
      ;-----------------------------------------------------------------

      ; Read stations
      all_stations_geos, $
         'CO', LongCat1, 44, pref1, PLabel1, ptop1, dlon1, dlat1, model1, '.1', $
         debug = debug
      all_stations_geos, $
         'CO', LongCat2, 44, pref2, PLabel2, ptop2, dlon2, dlat2, model2, '.2', $
         debug = debug
      all_stations_geos, $
         'CO', LongCat3, 44, pref3, PLabel3, ptop3, dlon3, dlat3, model3, '.3', $
         debug = debug

      ; Plot
      PSName = PSDir + 'aircraft.profile.CO.geos.' + RunName + '.ps'
      plot_gridded_CO_vs_data_geos_3_models, $
         'CO', 44, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3

      ; Close files & cleanup
      Close,       /All
      Ctm_Cleanup, /No_GC 

      spawn,'rm -f temp/*'

   endif

   ; Restore original color table
   TvLct, R, G, B

   ; Remove file links
   Cmd = 'rm -f diaginfo.dat tracerinfo.dat'
   if ( Debug ) then print, Cmd
   Spawn, Cmd

end
