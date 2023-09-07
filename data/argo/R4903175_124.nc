CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-12T05:01:04Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   =�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       >�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   B�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       C�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       G�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       L�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   P�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       U�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   _   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       `   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    dH   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    gH   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    jH   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  mH   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    mt   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    mx   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    m|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    m�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  m�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    m�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    m�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    m�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         m�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         m�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        m�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    m�Argo profile    3.1 1.2 19500101000000  20220312050104  20220312050104  4903175 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               |A   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @��
<�
1   @��
�Q��@3�V�u�dMx���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         |A   B   F   D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111D��fD�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�FfD��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D3D��3D�3D�C3DÃ3D��3D�3D�C3Dă3D��3D�3D�C3DŃ3D��3D�3D�C3Dƃ3D��3D�3D�C3Dǃ3D��3D�3D�C3Dȃ3D��3D�3D�C3DɃ3D��3D�3D�C3Dʃ3D��3D�3D�C3D˃3D��3D�3D�C3D̃3D��3D�3D�C3D̓3D��3D�3D�C3D΃3D��3D�3D�C3Dσ3D��3D�3D�C3DЃ3D��3D�3D�C3Dу3D��3D�3D�C3D҃3D��3D�3D�C3DӃ3D��3D�3D�C3Dԃ3D��3D�3D�C3DՃ3D��3D�3D�C3Dփ3D��3D�3D�C3D׃3D��3D�3D�C3D؃3D��3D�3D�C3Dك3D��3D�3D�C3Dڃ3D��3D�3D�C3Dۃ3D��3D�3D�@ D܃3D��3D�3D�C3D݃3D��3D�3D�C3Dރ3D��3D�3D�C3D߃3D��3D�3D�C3D��3D��3D�3D�C3D�3D��3D�3D�C3D� D��3D�3D�C3D�3D��fD�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D��3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�L�D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@=?}@=/@=�@=V@<�/@<�/@<��@<�@<z�@<(�@;�m@;t�@;33@:�H@:-@:J@9�^@9��@9��@9hs@97L@8��@8bN@7�;@7|�@7l�@7\)@7K�@7+@6��@6�@6�@6�R@6�+@6V@6$�@5�@5�T@5�h@5�@5V@4��@4��@4z�@4j@4I�@3��@3�m@3�
@3��@3dZ@3"�@2��@2=q@1��@1%@0��@0Q�@0b@0  @/��@/K�@/�@/
=@.��@.�R@.{@.@-@-p�@,��@+�
@+�@+33@+@*��@*�!@*��@*~�@*^5@*M�@*M�@*�@)��@)��@)x�@)&�@(r�@( �@'�@'\)@&��@&ff@&@%�-@%?}@$�@$�@$�/@$��@$�@$Z@$I�@$�@#ƨ@#ƨ@#�F@#�F@#��@#t�@#C�@"�@"��@"��@"^5@"=q@"-@!�^@!7L@ �`@ �9@ ��@ r�@ 1'@  �@ b@ b@   @ b@ b@   @�@|�@;d@+@�y@�R@��@v�@v�@ff@V@V@�@�@/@�@�D@I�@�@�@1@�
@��@��@dZ@33@@�@�H@�H@�H@��@n�@�#@G�@��@��@A�@  @�@��@��@l�@\)@\)@\)@\)@�@��@��@�+@v�@E�@{@��@�-@O�@/@��@�/@z�@Z@I�@I�@I�@9X@�F@��@�@t�@dZ@�@��@�H@��@�!@~�@M�@-@�@�#@��@��@x�@G�@�@��@Ĝ@�@bN@bN@Q�@1'@1'@ �@  @��@��@�@��@��@�P@|�@l�@K�@�@
=@V@�@�@�@�@��@�@z�@(�@�@�@��@ƨ@ƨ@��@C�@o@o@@
�!@
��@
�\41141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441@=?}@=/@=�@=V@<�/@<�/@<��@<�@<z�@<(�@;�m@;t�@;33@:�H@:-@:J@9�^@9��@9��@9hs@97L@8��@8bN@7�;@7|�@7l�@7\)@7K�@7+@6��@6�@6�@6�R@6�+@6V@6$�@5�@5�T@5�h@5�@5V@4��@4��@4z�@4j@4I�@3��@3�m@3�
@3��@3dZ@3"�@2��@2=q@1��@1%@0��@0Q�@0b@0  @/��@/K�@/�@/
=@.��@.�R@.{@.@-@-p�@,��@+�
@+�@+33@+@*��@*�!@*��@*~�@*^5@*M�@*M�@*�@)��@)��@)x�@)&�@(r�@( �@'�@'\)@&��@&ff@&@%�-@%?}@$�@$�@$�/@$��@$�@$Z@$I�@$�@#ƨ@#ƨ@#�F@#�F@#��@#t�@#C�@"�@"��@"��@"^5@"=q@"-@!�^@!7L@ �`@ �9@ ��@ r�@ 1'@  �@ b@ b@   @ b@ b@   @�@|�@;d@+@�y@�R@��@v�@v�@ff@V@V@�@�@/@�@�D@I�@�@�@1@�
@��@��@dZ@33@@�@�H@�H@�H@��@n�@�#@G�@��@��@A�@  @�@��@��@l�@\)@\)@\)@\)@�@��@��@�+@v�@E�@{@��@�-@O�@/@��@�/@z�@Z@I�@I�@I�@9X@�F@��@�@t�@dZ@�@��@�H@��@�!@~�@M�@-@�@�#@��@��@x�@G�@�@��@Ĝ@�@bN@bN@Q�@1'@1'@ �@  @��@��@�@��@��@�P@|�@l�@K�@�@
=@V@�@�@�@�@��@�@z�@(�@�@�@��@ƨ@ƨ@��@C�@o@o@@
�!@
��@
�\41141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�AA�M�A�Q�A�VA�\)A�XA�ZA�ZA�\)A�\)A�^5A�\)A�^5A�ffA�dZA�\)A�Q�A�I�A�G�A�C�A�?}A�?}A�A�A�G�A�G�A�A�A�9XA�5?A�9XA�?}A�A�A�A�A�A�A�C�A�C�A�E�A�E�A�A�A�;dA�A�A�?}A�9XA�;dA�7LA�9XA�=qA�7LA�1'A�-A�&�A� �A�$�A� �A��A��A�{A�{A�bA�VA�bA�bA�bA�JA�JA�bA�oA���A�  A�  A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��mA��`A��mA��HA��/A��
A��A��#A��
A��/A��;A��TA��`A��;A��HA��;A��;A��
A���A���A���A�ĜA�ƨA���A�ƨA��^A��!A��-A��-A��-A��A���A���A���A���A���A���A���A���A���A��uA��hA��DA��DA��A�|�A�z�A�x�A�x�A��A�~�A�z�A�x�A�x�A�x�A�t�A�jA�dZA�bNA�ffA�`BA�XA�S�A�VA�O�A�G�A�C�A�A�A�9XA�5?A�7LA�/A�(�A�$�A� �A��A��A�oA�oA�VA�VA�oA�bA�VA�
=A�
=A�
=A�1A�%A�1A�1A�A�A�A�%A�%A�  A���A��`A��A��
A��A��`A��mA��;A��;A��#A���A���A�ƨA��jA��!A���A���A���A���A���A��uA���A���A���A��DA��A�z�A�v�A�n�A�jA�bNA�`BA�XA�I�A�?}A�/A�$�A� �A��A��A��A�bA�A���A���A��A��HA��
A�ƨA��9A���A���A��\A�|�A�r�A�hsA�`BA�ZA�K�A�?}A�?}A�;dA�-A��A�JA���A��A��mA���A�A��!A���A��7A�jA�G�A�$�A��+A�Z44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444A�AA�M�A�Q�A�VA�\)A�XA�ZA�ZA�\)A�\)A�^5A�\)A�^5A�ffA�dZA�\)A�Q�A�I�A�G�A�C�A�?}A�?}A�A�A�G�A�G�A�A�A�9XA�5?A�9XA�?}A�A�A�A�A�A�A�C�A�C�A�E�A�E�A�A�A�;dA�A�A�?}A�9XA�;dA�7LA�9XA�=qA�7LA�1'A�-A�&�A� �A�$�A� �A��A��A�{A�{A�bA�VA�bA�bA�bA�JA�JA�bA�oA���A�  A�  A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��mA��`A��mA��HA��/A��
A��A��#A��
A��/A��;A��TA��`A��;A��HA��;A��;A��
A���A���A���A�ĜA�ƨA���A�ƨA��^A��!A��-A��-A��-A��A���A���A���A���A���A���A���A���A���A��uA��hA��DA��DA��A�|�A�z�A�x�A�x�A��A�~�A�z�A�x�A�x�A�x�A�t�A�jA�dZA�bNA�ffA�`BA�XA�S�A�VA�O�A�G�A�C�A�A�A�9XA�5?A�7LA�/A�(�A�$�A� �A��A��A�oA�oA�VA�VA�oA�bA�VA�
=A�
=A�
=A�1A�%A�1A�1A�A�A�A�%A�%A�  A���A��`A��A��
A��A��`A��mA��;A��;A��#A���A���A�ƨA��jA��!A���A���A���A���A���A��uA���A���A���A��DA��A�z�A�v�A�n�A�jA�bNA�`BA�XA�I�A�?}A�/A�$�A� �A��A��A��A�bA�A���A���A��A��HA��
A�ƨA��9A���A���A��\A�|�A�r�A�hsA�`BA�ZA�K�A�?}A�?}A�;dA�-A��A�JA���A��A��mA���A�A��!A���A��7A�jA�G�A�$�A��+A�Z44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220312050104                              AO  ARCAADJP                                                                    20220312050104    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220312050104  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220312050104  QCF$                G�O�G�O�G�O�C000            