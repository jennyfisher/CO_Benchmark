;-----------------------------------------------------------------------
;+
; NAME:
;       CO_EMISSIONS 
;
; PURPOSE:
;        Prints totals of GEOS-CHEM emission species for two different
;        model versions.  Also prints the difference in emission 
;        totals between the two model versions.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        CO_EMISSIONS [ , Keywords ]
;
; INPUTS:
;        None
;
; KEYWORD PARAMETERS:
;        FILENEW -> Name of a binary punch file containing model
;             output from a "New" version of the GEOS-CHEM.   
;
;        FILEOLD -> Name of a binary punch file containing model
;             output from a "Old" version of the GEOS-CHEM. 
;
;        OUTFILENAME -> Name of the text file where emission totals
;             and differences will be sent.  Default is "emissions.txt".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines:
;        ==================================
;        GetVersionInfo (function)
;        WriteHeader
;        WriteTracers
;
;        External Subroutines Required:
;        ==================================
;        CTM_SUM_EMISSIONS 
;        UNDEFINE
; 
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        (1) Assumes the version number is located at the end of
;            both FILENEW and FILEOLD
;
;        (2) Assumes that both FILENEW and FILEOLD contain the
;            following GEOS-CHEM diagnostic categories:
;            ND29 ("CO--SRCE")
;
; EXAMPLE:
;        CO_EMISSIONS, FILENEW='ctm.bpch.v6-02-05', $
;                            FILEOLD='ctm.bpch.v6-02-04', $
;                            OUTFILENAME='emissions.txt'
;
;             ; Prints emissions & differences between 
;             ; versions 6-02-05 and 6-02-04
;                           
; MODIFICATION HISTORY:
;        bmy, 18 Jun 2001: VERSION 1.00
;        bmy, 20 Jun 2001: VERSION 1.01
;                          - now omit ALD2 (#11) from ANTHROPOGENIC
;        bmy, 20 Sep 2001: VERSION 1.02
;                          - now print ND11 acetone sources, sinks
;        bmy, 15 Aug 2002: VERSION 1.03
;                          - renamed to FULLCHEM_EMISSIONS
;                          - renamed FILE_NEW to FILENEW and 
;                            FILE_OLD to FILEOLD
;        bmy, 17 Jan 2003: VERSION 1.04
;                          - also sum up sulfate emission categories
;        bmy, 27 Mar 2003: VERSION 1.05
;                          - adjust FORMAT string for branch versions
;                          - now also print out NH3-NATU source
;        bmy, 09 Apr 2004: VERSION 1.06
;                          - Now print out emissions of BC/OC tracers
;                          - Now print out hydrophilic BC/OC which
;                            came from hydrophobic BC/OC
;        bmy, 28 Apr 2004: VERSION 1.07
;                          - Now print out dust emissions
;        bmy, 03 May 2004: VERSION 1.08
;                          - Now print out seasalt emissions
;        bmy, 21 May 2004: VERSION 1.09
;                          - Now print out ship exhaust SO2 emissions
;        bmy, 08 Jul 2005: VERSION 1.10
;                          - Updated for 43 tracers
;        bmy, 10 Jan 2011: VERSION 1.11
;                          - Now make numeric fields 13 chars wide to
;                            allow for wider title headers
;        bmy, 16 Dec 2011: GAMAP VERSION 2.16
;                          - Remove ACET from dryleaf and ACET from
;                            grass; these are obsolete GEIA quantities
;
;-
; Copyright (C) 2001-2011, Bob Yantosca, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to bmy@io.harvard.edu
; with subject "IDL routine fullchem_emissions"
;-----------------------------------------------------------------------


function GetVersionInfo, FileName 
 
   ;====================================================================
   ; Internal subroutine GetVersionInfo extracts the GEOS-CHEM
   ; version string (e.g. v4-15) from an input file name.
   ;====================================================================   

   ; Look for the last period in the FILENAME string
   Ind = StrPos( FileName, '.', /Reverse_Search )
 
   ; Version string is between the last period and the end of the string
   if ( Ind[0] ge 0 ) then begin
      Version = StrMid( FileName, Ind[0]+1, StrLen( FileName )-Ind[0]+1 )
   endif else begin
      S = StrTrim( FileName ) + ' does not contain a version string!'
      Message, S 
   endelse
 
   ; Return the version string to the main program
   return, Version
 
end
 
;-----------------------------------------------------------------------------
 
pro WriteHeader, Ilun, Title, Version_New, Version_Old 

   ;====================================================================
   ; Internal subroutine WriteHeader writes a header for each emissions
   ; category with a top title and also the version string information.
   ;====================================================================  

   ; Now use wider format string (bmy, 1/10/11)
   Format = '('' Tracer  '',a11,3x,a11,3x,a11,'' - '',a)'

   PrintF, Ilun, Title
   PrintF, Ilun, Version_New, Version_Old, Version_New, Version_Old, $
                 Format=Format
   PrintF, Ilun, '==============================================================='
 
   ; Return to main program
   return
end
 
;-----------------------------------------------------------------------------
 
pro WriteTracers, Ilun, New, Old, Advance=Advance

   ;====================================================================
   ; Internal subroutine WriteTracers writes tracer sums and
   ; differences for a given emissions category.
   ;====================================================================  

   ; Error check
   if ( N_Elements( New ) ne N_Elements( Old ) ) then begin
      Message, 'NEW and OLD do not have the same # of elements!'
   endif
 
   ; Write totals & difference to OUTFILENAME
   for N = 0L, N_Elements( New ) - 1L do begin
      PrintF, Ilun, New[N].Name, New[N].Sum, Old[N].Sum,  $
                    New[N].Sum - Old[N].Sum, New[N].Unit, $
                    Format='(a6,2(1x,f13.3),3x,f13.3,3x,a6)'
   endfor

   ; Write some spacers -- if /ADVANCE is set
   if ( Keyword_Set( Advance ) ) then begin
      PrintF, Ilun
      PrintF, Ilun
   endif

   ; Return to main program
   return
end
 
;-----------------------------------------------------------------------------
  
pro CO_Emissions, FileNew=FileNew,        $
                  FileOld=FileOld,        $
                  OutFileName=OutFileName,$
		  VersionNew=VersionNew,  $
		  VersionOld=VersionOld

   ;====================================================================
   ; Initialization
   ;====================================================================
 
   ; Pass external functions
   FORWARD_FUNCTION GetVersionInfo

   ; Keyword settings
   if ( N_Elements( FileNew ) ne 1 ) then Message, 'FILENEW not passed!'
   if ( N_Elements( FileOld ) ne 1 ) then Message, 'FILEOLD not passed!'

   ; Get version information from input & output files
   Version_New = VersionNew
   Version_Old = VersionOld
 
   ; Default output file neame
   if ( N_Elements( OutFileName ) ne 1 ) $
      then OutFileName = Version_New + '.emissions.txt'
 
   ; Open output file and get version strings from FILE_NEW and FILE_OLD
   Open_File, OutFileName, Ilun, /Write

   ;====================================================================
   ; Print CO source totals from old & new versions
   ;==================================================================== 
   Tracer = [ 1, 2, 3, 4, 5 ]
 
   CTM_Sum_Emissions, 'CO--SRCE', $
      File=FileNew, Tracer=Tracer, /Cum_Only, Result=New
 
   CTM_Sum_Emissions, 'CO--SRCE', $
      File=FileOld, Tracer=Tracer, /Cum_Only, Result=Old
 
   WriteHeader,  Ilun, 'CO SOURCES', Version_New, Version_Old 
   WriteTracers, Ilun, New, Old, /Advance
 
   UnDefine, New
   UnDefine, Old

   ;====================================================================
   ; Close file and quit
   ;====================================================================
quit:
   Close,    Ilun
   Free_LUN, Ilun
 
   return
end
   
   
 
 
   
