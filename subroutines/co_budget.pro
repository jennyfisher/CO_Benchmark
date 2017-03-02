;-----------------------------------------------------------------------
;+
; NAME:
;        CO_BUDGET
;
; PURPOSE:
;        Computes the budgets of Ox and CO from the GEOS-CHEM model.
;        for full chemistry benchmark simulations.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        CO_BUDGET [, Keywords ]
;
; INPUTS:
;        None
;
; KEYWORD PARAMETERS:
;        TAU0 -> Time index of the data to plot.  Units are hours 
;             since 0 GMT on 1/1/1985.  Default is 144600D (July 1, 2001).
;
;        FILENEW -> Name of a binary punch file containing model
;             output from a "New" version of the GEOS-CHEM. 
;             If omitted, a dialog box will prompt the user to
;             supply a file name.   
;
;        OUTFILENAME -> Name of the file where budget information
;             will be written to.  Default is "(VERSION).budget.CO", 
;             where VERSION is the version number contained w/in
;             FILENEW.
;
;        INITIALFILE -> Name of a binary file containing the mass of
;             Ox [in kg] at the start of a GEOS-CHEM model run.
;
;        FINALFILE -> Name of a binary file containing the mass of
;             Ox [in kg] at the end of a GEOS-CHEM model run.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ====================================
;        OPEN_FILE     CTM_BOXSIZE (function)   
;        CTM_GET_DATA  TAU2YYMMDD  (function)
;        UNDEFINE      GETMODELANDGRIDINFO 
;
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        (1) Assumes the version # is located at the end of FILENEW.
;
;        (2) Assumes the following GEOS-CHEM diagnostics 
;            have been archived in the input files:
;            (a) ND24 ("EW-FLX-$")   (f) ND65 ("PORL-L=$")
;            (b) ND25 ("NS-FLX-$")   (g) ND66 ("DAO-3D-$")
;            (c) ND26 ("UP-FLX-$")   (h) ND68 ("BXHGHT-$")
;            (d) ND44 ("DRYD-FLX")   (i)      ("TCMASS-$") 
;            (e) ND45 ("CHEM-L=$")   (j)      ("TR-PAUSE")
;
; EXAMPLE:
;        CO_BUDGET, TAU0=144600D,
;                         FILENEW='ctm.bpch.v5-01'
;                         INITIALFILE='CO.mass.initial.v5-01',  $
;                         FINALFILE='CO.mass.final.v5-01',      $
;                         OUTFILENAME='v5-01.budget.CO'
;
; MODIFICATION HISTORY:
;        bmy, 15 Aug 2002: VERSION 1.01
;                          - adapted from Isabelle Bey's "budget.pro"
;        bmy, 14 Jan 2003: VERSION 1.02
;                          - In GEOS-CHEM v5-03+, ND44 saves out tracers
;                            using the CTM tracer number 
;        bmy, 30 Oct 2003: VERSION 1.03
;                          - now call PTR_FREE to free the heap memory
;                            so that we don't run out of memory
;                          - now compute mean mass-weighted OH instead
;                            of methyl chloroform lifetime
;  ccc & bmy, 11 Aug 2010: VERSION 1.04
;                          - Updated computation of Ox budget
;        bmy, 10 Jan 2011: VERSION 1.05
;                          - Updated 200hPa level for MERRA
;        bmy, 08 Jun 2011: - Also print out MCF lifetime
;        bmy, 11 May 2012: GAMAP VERSION 2.16
;                          - Modified for GEOS-5.7.2 met
;
;-
; Copyright (C) 2002-2010, Bob Yantosca, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to bey@io.harvard.edu
; or bmy@io.harvard.edu with subject "IDL routine CO_budget"
;-----------------------------------------------------------------------


pro CO_Budget, Tau0=Tau0,               FileNew=FileNew,     $
               InitialFile=InitialFile, FinalFile=FinalFile, $
               OutFileName=OutFileName, _EXTRA=e

   ;====================================================================
   ; Keyword Settings / Initialization
   ;====================================================================

   ; External functions
   FORWARD_FUNCTION CTM_BoxSize, Tau2YYMMDD

   ; Extract the version number from the file name
   Ind     = StrPos( FileNew, '.', /Reverse_Search )
   Version = StrMid( FileNew, Ind+1, StrLen( FileNew ) - Ind + 1 )

   ; Keyword Settings
   if ( N_Elements( FileNew     ) ne 1 ) then Message, 'FILENEW not passed!'
   if ( N_Elements( Tau0        ) ne 1 ) then Tau0 = 144600D
   if ( N_Elements( OutFileName ) ne 1 ) $
      then OutFileName = Version + '.budget.CO'

   ; Avogadro's number
   Avo     = 6.023d23

   ;==================================================================== 
   ; Read grid box heights, compute grid box volumes
   ;====================================================================
   CTM_Get_Data, DataInfo_N, 'BXHGHT-$', File=FileNew, Tau0=Tau0, Tracer=1
 
   ; Boxheight [m]
   BoxHt = *( DataInfo_N[0].Data )

   ; Get model & grid info
   GetModelAndGridInfo, DataInfo_N[0], ModelInfo_N, GridInfo_N

   ; Surface area [cm2]
   AreaCm2 = Ctm_BoxSize( GridInfo_N, /GEOS, /CM2 )

   ; Box volume [cm3]
   Size_N = Size( BoxHt, /Dim )
   BoxVol = FltArr( Size_N[0], Size_N[1], Size_N[2] )

   for L = 0L, Size_N[2]-1L do begin 
      BoxVol[*,*,L] = BoxHt[*,*,L] * AreaCm2[*,*] * 100e0
   endfor

   ; Undefine stuff
   UnDefine, BoxHt

   ;==================================================================== 
   ; Grid parameters
   ;====================================================================

   ; Get the model and grid information from the DATAINFO strucutre
   GetModelAndGridInfo, DataInfo_N[0], InType, InGrid

   ; Find the level nearest 200hPa, plus the LEV and LEV_t inputs
   ; for the MCF_LIFETIME routine for each model grid (bmy, 5/11/12)
   case ( InType.Name ) of 
      'GEOS57_47L': begin
         L200hPa = 31
         Lev_a   = [ 1, 47 ]
         Lev_t   = [ 1, 38 ]
      end

      'MERRA_47L': begin
         L200hPa = 31
         Lev_a   = [ 1, 47 ]
         Lev_t   = [ 1, 38 ]
      end

      'GEOS5_47L': begin
         L200hPa = 31
         Lev_a   = [ 1, 47 ]
         Lev_t   = [ 1, 38 ]
      end

      'GEOS4_30L': begin
         L200hPa = 14
         Lev_a   = [ 1, 30 ]
         Lev_t   = [ 1, 22 ]
      end

      'GEOS3_30L': begin
         L200hPa = 18
         Lev_a   = [ 1, 30 ]
         Lev_t   = [ 1, 24 ]
      end
   endcase

   ; Convert from Fortran to IDL notation
   L200hPa = L200hPa - 1L

   ;====================================================================
   ; Time quantities
   ;====================================================================

   ; Get TAU0 and TAU1
   ThisTau0 = DataInfo_N[0].Tau0 
   ThisTau1 = DataInfo_N[0].Tau1 

   ; Number of seconds and days in diagnostic interval
   Seconds  = ( ThisTau1 - ThisTau0 ) * 3600D0
   Days     = Seconds / 86400D0

   ; Scaling factor to [1/year]
   ; NOTE: benchmark run is done in a non-leap-year (2001)
   OneYear  = 365d0 / Days

   ; Undefine stuff
   UnDefine, DataInfo_N

   ;====================================================================
   ; Chemical production & loss of CO
   ;====================================================================
   print, 'Still need to define correct tracers for tagged CO P/L - quitting!'
   return

   CTM_Get_Data, DataInfo_N, 'PORL-L=$', File=FileNew, Tau0=Tau0, Tracer=[3,4]

   ;--------------
   ; P(CO)
   ;--------------
   Prod      = *( DataInfo_N[0].Data )
   Size_N    = Size( Prod, /Dim )

   ; Convert from [molec/cm3/s] to [kg/s]
   Prod      = ( Prod * BoxVol[*,*,0:Size_N[2]-1L] ) * ( 28d-3 / Avo )

   ; Compute total P(CO) in [Tg] and [Tg/year]
   PCO       = Total( Prod ) * Seconds / 1d9
   PCOYear   = PCO * OneYear

   ;--------------
   ; L(CO)
   ;--------------
   Loss      = *( DataInfo_N[1].Data )
   Size_N    = Size( Loss, /Dim )

   ; Convert [molec/cm3/s] to [kg/s]
   Loss      = ( Loss * BoxVol[*,*,0:Size_N[2]-1L] ) * ( 28d-3 / Avo )

   ; Compute total L(CO) in [Tg] and [Tg/year]
   LCO       = Total( Loss ) * Seconds / 1d9
   LCOYear   = LCO * OneYear
   
   ;--------------
   ; Net P-L
   ;--------------
   NetCO     = PCO     - LCO
   NetCOYear = PCOYear - LCOYear

   ; Undefine stuff
   UnDefine, DataInfo_N
   UnDefine, Prod
   UnDefine, Loss
   UnDefine, Size_N

   ;====================================================================
   ; Write to file
   ;==================================================================== 

   ; Open file
   Open_File, OutFileName, Ilun, /Get_LUN, /Write

   ; Compute start & end dates for the file
   Result = Tau2YYMMDD( ThisTau0, /NFormat )
   Nymd0  = Result[0]
   Nhms0  = Result[1] / 10000L
   Result = Tau2YYMMDD( ThisTau1, /NFormat )
   Nymd1  = Result[0]
   Nhms1  = Result[1] / 10000L

   ; Format strings
   Fs = '(''Start time: '', i2.2, '' GMT on '', i8.8 )'
   Fe = '(''End   time: '', i2.2, '' GMT on '', i8.8 )'
   F0 = '(a20, f10.3, 3x, f10.3 )'

   ; Header string
   S = '======= GEOS-CHEM ' + Version + ' Tagged CO Benchmark ======='

   ; Print values
   PrintF, Ilun, S
   PrintF, Ilun
   PrintF, Ilun, Nhms0, Nymd0, Format=Fs
   PrintF, Ilun, Nhms1, Nymd1, Format=Fe
   PrintF, Ilun
   PrintF, Ilun, '====================== CO Budget ======================'
   PrintF, Ilun
   PrintF, Ilun, ' SOURCES AND SINKS       Tg CO     Tg CO/yr'
   PrintF, Ilun, ' -----------------       -----     --------'
   PrintF, Ilun
   PrintF, Ilun, '* Chemistry'
   PrintF, Ilun, 'Total P(CO): ',         PCO,    PCOYear,    Format=F0
   PrintF, Ilun, 'Total L(CO): ',         LCO,    LCOYear,    Format=F0
   PrintF, Ilun, '                      --------     --------'
   PrintF, Ilun, 'Net P(CO)-L(CO): ',     NetCO,  NetCOYear,  Format=F0
   PrintF, Ilun
   PrintF, Ilun

   ; Close file
   Close,    Ilun
   Free_LUN, Ilun

   ; Undefine stuff
   UnDefine, ModelInfo_N
   UnDefine, GridInfo_N

   ; Quit
   return
end
