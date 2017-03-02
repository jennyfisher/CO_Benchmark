pro make_nc

   ; Converts bpch to netCDF for benchmark plots
   inpref='ctm.HO2uptake'
   outpref='fullchem'

   ; Diagnostics
   ;D = [ 'CHEM-L=$', 'IJ-AVG-$', 'PEDGE-$', 'BXHGHT-$', 'OD-MAP-$' ]
   D = [ 'IJ-AVG-$', 'PEDGE-$', 'BXHGHT-$' ]

   ; Tracer names
   ctm_tracerinfo,/force,file='../tracerinfo.dat'
   ctm_diaginfo,/force,file='../diaginfo.dat'

   ; Output file path
   OutFileName = outpref+'.%DATE%.nc'

   ; Make netCDF fles
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200901.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200902.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200903.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200904.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200905.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200906.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200907.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200908.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200909.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200910.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200911.bpch', OutFileName, diagn=D
            
   ctm_cleanup
   bpch2nc, '../bpch/'+inpref+'.200912.bpch', OutFileName, diagn=D

end	
