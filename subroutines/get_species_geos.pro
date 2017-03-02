; $Id: get_species_geos.pro,v 1.2 2008/03/31 18:51:06 bmy Exp $
function Get_Species_Geos, FileName,                         $
                           Species=Species, $
                           Verbose=Verbose, Lat=FileLat, $
                           Lon=FileLon, _EXTRA=e

   ;====================================================================
   ; Keywords / External functions
   ;====================================================================
   FORWARD_FUNCTION NCDF_Get

   if ( N_Elements( FileName ) ne 1 ) then Message, 'FILENAME not passed!'
   Verbose = Keyword_Set( Verbose )

   ;====================================================================
   ; Read Header and Index information from the netCDF file
   ;====================================================================

   ; Define flags
   IsSigma   = 0
   IsEta     = 0
   IsTime    = 0

   ; Expand FILENAME to a full path name
   FileName  = Expand_Path( FileName )

   ; Open file
   fId       = NCDF_Open( FileName )

   ; Test if we have ETA coordinates or SIGMA coordinates (bmy, 3/29/04)
   NStru = NCDF_Inquire( fId )

   ; Loop over netCDF variables
   for N = 0L, NStru.NVars-1L do begin
      VarDesc = NCDF_VarInq( fId, N )

      ; We have a time dimension
      if ( StrTrim( VarDesc.Name ) eq 'time' ) then begin
         IsTime = 1
      endif
     
      ; We have sigma grid
      if ( StrTrim( VarDesc.Name ) eq 'SIGC' ) then begin
         IsSigma = 1
         goto, Next
      endif

      if ( StrTrim( VarDesc.Name ) eq 'ETAC' ) then begin
         IsEta = 1
         goto, Next
      endif

   endfor

Next:

   ; Read TIME from file
   if ( IsTime ) then begin
      FileTime  = NCDF_Get( fId, 'time' )
      N_Time    = N_Elements( FileTime )
   endif

   ; Read LONGITUDE from file
   FileLon   = NCDF_Get( fId, 'LON' )
   N_Lon     = N_Elements( FileLon )

   ; Read LATITUDE from file
   FileLat   = NCDF_Get( fId, 'LAT' )
   N_Lat     = N_Elements( FileLat )

   ; Read SIGMA from file
   if ( IsSigma ) then begin
      FileSigma = NCDF_Get( fId, 'SIGC' )
      N_Alt     = N_Elements( FileSigma )
   endif 

   ; Read ETA from file
   if ( IsEta ) then begin
      FileSigma = NCDF_Get( fId, 'ETAC' )
      N_Alt     = N_Elements( FileSigma )
   endif

   ; If /VERBOSE is set, then print out quantities
   if ( Verbose ) then begin
      ;print, 'Longitudes: '
      ;print, FileLon
      ;print, 'Latitudes: '
      ;print, FileLat
      ;print, 'Sigma: '
      ;print, FileSigma
   endif

   ;====================================================================
   ; Read data from the netCDF file
   ; NOTE: dimensions are: [ lon, lat, alt]
   ;====================================================================

   ; Strip out bad characters so we don't generate a lot of error msgs
   NewSpecies = NCDF_Valid_Name( Species )

   ; Read the SPECIES for the given DATE from the file
   ; NOTE: for now, pull out all lon, lat, lev
   if ( IsTime ) then begin
      OffSet = [ 0,     0,     0,     0 ]
      Count  = [ N_Lon, N_Lat, N_Alt, N_Time ]
   endif else begin
      OffSet = [ 0,     0,     0 ]
      Count  = [ N_Lon, N_Lat, N_Alt ]
   endelse
   Data   = NCDF_Get( fId, NewSpecies, $
                      OffSet=OffSet, Count=Count, _EXTRA=e )

   NCDF_CLOSE,FId
   ; Return to calling program
   return, Data
end
