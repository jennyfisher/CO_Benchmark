; $Id: get_diff_range.pro,v 1.1 2007/11/16 14:56:52 bmy Exp $
;-----------------------------------------------------------------------
;+
; NAME:
;        GET_DIFF_RANGE
;
; PURPOSE:
;        Returns a default plotting range for given GEOS-Chem tracers 
;        This will be used to create absolute difference plots for 
;        GEOS-Chem benchmarking.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        RANGE = GET_DIFF_RANGE( TRACERNAME )
;
; INPUTS:
;        TRACERNAME -> Name of the tracer for which a default
;             plotting range will be returned.
;
; KEYWORD PARAMETERS:
;        None
;
; OUTPUTS:
;        RANGE -> A 2 element vector with the [min,max] values
;             to be used in creating a tracer difference plot.
;
; SUBROUTINES:
;        None
;
; REQUIREMENTS:
;        Routine READ_DIFF_RANGE must be called before this routine
;        may be used.  This will normally be done at the top of
;        driver routine BENCHMARK_1MON.
;
; NOTES:
;        (1) Meant to be used in conjunction with the GEOS-Chem 
;            benchmark plotting codes.
; 
;        (2) Default ranges for each tracer are read from a file by the 
;            complementary routine READ_DIFF_RANGE and stored in the
;            GDR common block.
;
; EXAMPLE:
;        READ_DIFF_RANGE, 'diff_range.1mon'
;        PRINT, GET_DIFF_RANGE( 'NOx' )
;            -0.100000  0.100000
;   
;            ; Prints the default plotting range for NOx
;          
;
; MODIFICATION HISTORY:
;        bmy, 14 Nov 2007: VERSION 1.00
;
;-
; Copyright (C) 2007, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine get_diff_range"
;-----------------------------------------------------------------------


function Get_Diff_Range, TracerName
 
   ; Common block
   common GDR, Names, Ranges
 
   ; Match the 
   Ind = Where( Names eq StrUpCase( StrTrim( TracerName, 2 ) ) ) 
 
   ; Return the corresponding range for that tracername
   ; Stop w/ an error message if no match is found
   if ( Ind[0] ge 0 ) then begin
      return, [ -Ranges[Ind], Ranges[Ind] ]
   endif else begin
      S = TracerName + ' is an invalid tracer name!'
      Message, S
   endelse
end
