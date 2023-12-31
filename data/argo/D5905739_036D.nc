CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-29T00:01:45Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  Ll   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  a,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  eT   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ل   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   $   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   @   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180929000145  20210617131504  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               $   $DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؂��^'@؂��^'11  @؂��}(�@؂��}(�@6��1���@6��1����c�AJM+,�c�AJM+,11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?���@��@Fff@�  @�33@�33@�ff@���A��A(  A@  A^ffA�  A���A�  A���A���Aљ�AᙚA���B ffB  B  B��B   B(  B0ffB933BA33BI33BP��BX  B_��Bg33Bo��Bw33B��B�33B�ffB�33B�  B���B�  B�  B�  B�33B�33B�  B�  B�33B�33B�  B���B�  B�  B�ffB�  B�  Bؙ�B���B�ffB㙚B�  B�  B�33B�ffB���B���C �C�3C��C  C  C
33C  C��C�fC�C33C  C��C  C33CL�C �C!�fC$  C&L�C(�C*  C,33C.L�C0�C1�fC4�C6L�C8�C9��C<  C>�C@33CB  CC��CE�fCH�CJ33CLL�CN�CO�fCQ�fCT  CV33CXL�CZ�C[��C]�fC_�fCb�Cd33Cf�Cg��Ci�fCl  Cn�CpL�CrL�Ct33Cv  CxL�Cz�C{��C}�fC��C�&fC��C��3C��C�&fC��C�  C��C�  C��3C��C��3C��3C��C�  C��3C��C�  C��3C��C�  C��3C��C��C�  C�&fC��C��3C��C�  C��3C��C�  C��3C��C�&fC��C��3C��C�  C��fC��3C��C�  C��fC�  C��C��C��fC�  C��C�  C��3C�  C��C��C�&fC�&fC��C�  C�  C�  C��C��C�&fC�&fC�&fC��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC�&fC�33C��C��fC�  C�  C��C��C��3C�  C�  C�ٚC��fC�  C��C�&fC��C��3C�  C�&fC��C��3C�  C��C�  C��fC�  C��C�&fC�33C��fD y�D  DfD�fD�D��DS3D��Dy�D�D��D` D�fD"y�D%3D'� D*l�D-�D/� D2FfD4��D7��D:&fD<� D?Y�DA�fDD�fDGfDI��DL�DN��DQ33DS��DVS3DX� D[� D^�D`�3DcS3Df  Dh��Dk&fDm�fDp` Dr��Du��Dx33Dz� D|�fDL�D�ٚD�	�D�9�D�i�D���D��3D���D�  D�I�D�p D���D���D��fD��D�33D�S3D�vfD��3D��fD���D�3D�6fD�S3D�|�D��fD�� D��fD��D�<�D�\�D��3D���D��3D��fD�  D�S3D��3D��fD��fD��D�FfD�l�D��fD�ɚD�� D�fD�FfD�i�D�� D���D��D��D�33D�S3D�s3D��fD���D�� D� D�33D�VfD�vfDȜ�Dɼ�D�� D�  D��D�C3D�i�DІfDѩ�D��fD��fD�	�D�#3D�@ D�c3D�|�Dڙ�D۶fD��3D�� D� D�)�D�P D�c3D� D��D� D��fD��fD�3D�#3D�<�D�Y�D�p D�3D�3D�3D�3D��fD�� D��fD���D���D�� D�  D� D�  D�fD�	�D�fD�3E   E |�E ��EvfE��EnfE�3Eh E��E�3E�3EI�E<�E	� E E��E^fE�fE�E��ET�E��EfE3Ei�E� E�fE  E��Ek3E�fE +3E!�3E"s3E#�3E%!�E&t�E'�fE) E)�3E+>fE,�fE-�E/4�E0�fE1k3E2�3E4fE5l�E6�fE7� E9	�E:` E;��E>��EA�fEE3EG�EKQ�EN\�EQk3ET� EW� EZ��E^ E`�fEd33Egi�EjH Em��Ep�fEs�fEv� Ey��E}4�E� E���E�E�E�� E�_3E���E�H E���E�� E�6fE�q�E�� E�  E�ZfE�� E� E�T�E��fE�� E�E�E���E�� E�?3E�� E���E�.fE�|�E�� E�3E�nfE���>���>���>���>���>���>L��>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���?   ?   ?333?L��?�  ?���?�ff?�  ?�ff@   @33@   @333@Fff@Y��@fff@�33@���@���@�  @���@���@�33@�  @���@���@�ffA��A	��A��A  A!��A&ffA.ffA6ffA<��AC33AK33AS33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441444441144141441411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?fff?���@,��@fff@�  @�33@�33@�ffAffA��A0  AH  AfffA�  A���A�  A���Ař�Aՙ�A噚A���BffB
  B  B��B"  B*  B2ffB;33BC33BK33BR��BZ  Ba��Bi33Bq��By33B���B�33B�ffB�33B�  B���B�  B�  B�  B�33B�33B�  B�  B�33B�33B�  B���B�  B�  B�ffB�  B�  Bٙ�B���B�ffB䙚B�  B�  B�33B�ffB���B���C ��C33CL�C� C� C
�3C� CL�CffC��C�3C� CL�C� C�3C��C ��C"ffC$� C&��C(��C*� C,�3C.��C0��C2ffC4��C6��C8��C:L�C<� C>��C@�3CB� CDL�CFffCH��CJ�3CL��CN��CPffCRffCT� CV�3CX��CZ��C\L�C^ffC`ffCb��Cd�3Cf��ChL�CjffCl� Cn��Cp��Cr��Ct�3Cv� Cx��Cz��C|L�C~ffC�L�C�ffC�L�C�33C�L�C�ffC�Y�C�@ C�L�C�@ C�33C�L�C�33C�33C�L�C�@ C�33C�Y�C�@ C�33C�Y�C�@ C�33C�Y�C�L�C�@ C�ffC�L�C�33C�Y�C�@ C�33C�L�C�@ C�33C�L�C�ffC�Y�C�33C�Y�C�@ C�&fC�33C�Y�C�@ C�&fC�@ C�L�C�L�C�&fC�@ C�Y�C�@ C�33C�@ C�L�C�Y�C�ffC�ffC�Y�C�@ C�@ C�@ C�Y�C�Y�C�ffC�ffC�ffC�Y�C�ffC�L�C�Y�C�L�C�L�C�Y�C�L�C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�s3C�L�C�&fC�@ C�@ C�Y�C�L�C�33C�@ C�@ C��C�&fC�@ C�Y�C�ffC�Y�C�33C�@ C�ffC�Y�C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�ffC�s3D 3D ��D  D&fD�fD9�DٚDs3D�D��D9�DٚD� D fD"��D%33D'� D*��D-,�D/� D2ffD5�D7��D:FfD<� D?y�DBfDD�fDG&fDI��DL,�DN��DQS3DSٚDVs3DY  D[� D^9�D`�3Dcs3Df  Dh��DkFfDm�fDp� Ds�Du��DxS3Dz� D}fDl�D��D��D�I�D�y�D���D��3D��D�0 D�Y�D�� D���D���D��fD��D�C3D�c3D��fD��3D��fD���D�#3D�FfD�c3D���D��fD�� D�fD�)�D�L�D�l�D��3D���D��3D�fD�0 D�c3D��3D��fD��fD�)�D�VfD�|�D��fD�ٚD�  D�&fD�VfD�y�D�� D���D���D��D�C3D�c3D��3D��fD���D�� D�  D�C3D�ffDǆfDȬ�D���D�� D� D�,�D�S3D�y�DЖfDѹ�D��fD��fD��D�33D�P D�s3Dٌ�Dک�D��fD��3D�  D�  D�9�D�` D�s3D� D��D�� D��fD��fD�3D�33D�L�D�i�D� D�3D�3D�3D��3D��fD�� D��fD���D���D�  D� D�  D� D�fD��D�fD�3E  E ��E�E~fE��EvfE�3Ep E��E�3E�3EQ�ED�E	� E E�EffE�fE�E�E\�E��EfE3Eq�E� E�fE( E��Es3E�fE 33E!�3E"{3E#�3E%)�E&|�E'�fE)  E)�3E+FfE,�fE-�E/<�E0�fE1s3E2�3E4fE5t�E6�fE7� E9�E:h E;��E>��EA�fEE3EG�EKY�ENd�EQs3ET� EW� EZ��E^ E`�fEd;3Egq�EjP Em��Ep�fEs�fEw  Ey��E}<�E�  E���E�I�E�� E�c3E���E�L E���E�� E�:fE�u�E�� E�$ E�^fE�� E� E�X�E��fE�� E�I�E���E�� E�C3E�� E���E�2fE���E�� E�#3E�rfE���?L��G�O�?L��G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�?333?L��G�O�G�O�?L��G�O�?L��G�O�G�O�?fffG�O�?�  ?���?�ff?�  ?���?�ff@   @33@   @333@@  @S33@fff@y��@�33@�33@���@���@�  @���@ə�@�33@�  @���@���A33A	��A��A��A   A)��A.ffA6ffA>ffAD��AK33AS33A[33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441444441144141441411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ �@ �@ @ *@ O@ "�@ )�@ 1'@ 5�@ >@ G�@ Q�@ ^�@ m:@ |?@ ��@ ��@ �5@ ��@ ��@ �|@ �#@ �@ ��@�@@�@-@<@I�@Wb@dZ@p�@}�@��@��@��@�9@�>@є@ލ@�@�~@�@{@""@0x@>@K@X�@g@t�@��@��@�@��@��@��@Ӡ@�T@�@��@�@�@%�@3�@B8@P�@^5@j@uk@��@��@�m@�r@��@�@�[@�`@�@  @J@O@*S@8�@D�@Q=@_�@oF@{�@��@��@��@�-@�w@�|@܀@��@�e@j@�@ @,`@8�@F�@V@dZ@r�@~�@�D@��@�A@�F@Ĝ@��@܀@��@�~@�@�@"�@.l@<�@K@Yn@hs@v@�@�\@�@�Y@��@�J@�O@�T@�@��@
�@�@&�@33@A�@N�@[z@j@v�@�p@�u@�m@�f@�@�c@�[@�@�Y@�Q@@�@(�@8�@D�@Q=@`�@m:@z3@�7@�0@�(@�-@�2@�*@��@�y@��@	@	b@	 @	,`@	8�@	G�@	V@	c�@	oF@	~K@	�P@	��@	��@	��@	�>@	є@	��@	�@	��@
�@
{@
""@
1'@
>�@
M$@
Z�@
hs@
uk@
��@
�@
�a@
�Y@
�@
�W@
�O@
�@
�L@
�E@
�@�@&;@3�@A�@O0@]�@j@x&@�|@�$@��@��@�k@�@׹@�@�@@�@
@)�@5?@DD@Q�@`�@m�@z3@��@�0@��@�!@�&@�*@܀@�y@�@j@@ @+�@:@I@UU@a�@p�@�@��@�U@��@�9@@K�@�@�\@
@e	@�@�@9X@�@�c@V@T�@��@�@-�@uk@��@@Lu@�$@�#@""@i!@��@�q@:�@�@��@	�@P�@��@܀@""@i�@��@��@?}@��@ψ@*@\�@��@��@1'@y�@�&@��@;d@|�@��@��@?}@��@�>@�@@�@�W@�&@��@<�@|?@�@��@7L@uk@��@�@ 2�@ qS@ �r@ �4@!+�@!k.@!��@!�y@"'�@"e�@"�(@"��@# �@#`A@#�a@#��@$g@$`A@$��@$�@%$/@%dZ@%�(@%�@&$/@&b�@&��@&�@' �@'_�@'��@'��@(
@(\�@(�H@(׹@)�@)T�@)��@)Ӡ@*�@*O�@*�P@*�@+	�@+G�@+�@+��@, �@,?}@,|?@,�^@,� @-4�@-r�@-��@-�@.)�@.e�@.�z@.�;@/�@/X�@/�0@/�C@0@0K�@0��@0�J@1  @1;d@1x�@1��@1�@2/@2k�@2�A@2��@3�@3V@3�@3��@4j@4>�@4v�@4�f@4�@5�@5X�@5��@5��@5�9@61'@6g@6�@6�C@7�@7<�@7r�@7��@7��@8@8FQ@8��@9�@9�@: �@:��@;UU@;�^@<Q=@<�@=x&@=�#@>oF@?@?�T@@]@@�I@A33@A��@B/�@Bƨ@C*S@C�w@DV�@D��@EO�@E�@Ft�@Gv@G��@H%�@H�@Ib@I��@J0x@J�2@KQ=@K��@LB8@L��@Mhs@M��@N_�@N�@O�@P@QZ�@R�w@T@UK�@V��@XJ@YZ@Z�@\�@]e	@^��@_�@a]�@b�k@c��@ehs@f�(@g�@i_�@j��@l�@mQ�@n��@p�@qK@r��@s�E@tK@t~�@t�c@u�@uI@u��@u��@v�@vX�@v�m@v�`@w+@wp�@w��@w�@x33@x��@x�c@yV@yS�@y��@y��@z!s@ze	@z��@ G�O�@ G�O�G�O�@ ^G�O�G�O�G�O�G�O�G�O�@ ^@ G�O�G�O�@ G�O�@ G�O�G�O�@ �G�O�@ j@ �@ v@ �@ �@ �@ 
=@ J@ �@ �@ @ @ *@ 6@ �@ �@ [@  �@ ""@ $�@ '�@ )�@ ,`@ /@ 2�@ 4�@ 7L@ :�@ >@ @�@ D�@ F�@ Ji@ M�@ P�@ SI@ V�@ ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�$�A� �A� �A�JA��A��mAӾwAӝ�AӑhA�z�A�I�A�A���Aҧ�Aҗ�A�|�A�v�A�dZA�\)A�S�A�Q�A�S�A�S�A�K�A�E�A�;dA�33A�+A�+A�+A�"�A� �A� �A�"�A� �A�oA�|�A�\)AǑhA���A�"�A���A��A��FA���A��A��A�ĜA�hsA�{A��TA��9A���A�$�A���A�t�A��
A�A�A�"�A��+A�%A���A�(�A�hsA�I�A�G�A�t�A��TA�7LA���A��A�E�A��A��jA��#A��A��A�S�A��A���A�1'A�  A���A�|�A���A���A�  A�p�A�XA���A��A��!A�-A���A��\A�`BA��!A��mA��RA��A��-A��A���A�oA���A�A�K�A�5?A�+A�S�A�\)A�33A�n�A�z�A�A{��AzE�Ax�HAv�Au?}AsAr~�Aq|�Aol�Am�wAlȴAlE�Ak�Ai�-Ag/Af  Ae��Ad{AaA_C�A]�hA\��AZI�AX�`AX^5AW�AVffAT�/ASS�ARA�AQ
=APE�AO�#AO&�AN9XAK7LAI�AHA�AGS�AF��AE�AD�AD�AC`BAB �AAC�A@1'A?��A?G�A>�A=S�A<ffA;��A:�A:VA8�A5&�A4n�A3�A2��A2  A0�`A/��A.�!A.ZA-�FA,jA+7LA)�TA)"�A(1'A&ĜA%;dA#��A!�7A ��A AdZA�AĜAn�A�wA
=A�A�+A�A�^A7LAĜA��A�A+A�
A��Ap�A�TAp�A/A��A �A�A�AbA
�9A	�#A��AI�AC�A~�A��A�7A�9A  A�-A �yA b@�;d@���@��T@���@��@�p�@��9@��@��9@���@�X@��D@�Q�@���@�o@��@�j@�+@�M�@�dZ@�@�@���@߮@�M�@�/@�9X@�
=@٩�@�p�@��m@�
=@υ@�dZ@ēu@�33@�@�@��T@���@��@�t�@��@�9X@���@�-@��u@��@��u@��@�ȴ@�ȴ@�Z@�v�@��R@�/@�`B@��@��@��@��#@�I�@�|�@�v�@�hs@��
@�@���@��H@�n�@���@�A�@��@���@���@���@�V@�ȴ@��@�x�@� �@~$�@z�!@y��@yG�@xb@w�w@w
=@u�-@st�@pA�@nV@m��@m`B@k�F@i�@h�u@f5?@dz�@b�H@a��@_��@^��@^@]�@[��@Z�@X1'@V�R@V$�@U�@R�@QX@P1'@O�w@M/@LI�@K��@J�!@I�#@G�@F�+@D9X@B��@B-@@  @>V@<��@;��@:=q@8��@7�@6�@6$�@5@4��@3��@2��@1X@0  @.��@.{@,�@+��@+C�@*~�@)X@)7L@( �@'K�@&ff@%O�@$j@"~�@!�^@ ��@ A�@|�@��@E�@�@�/@S�@�!@�7@��@��@ȴ@��@O�@V@�F@33@��@-@�@G�@%@��@Q�@�;@|�@��@@`B@��@��@dZ@
~�@	��@	x�@	hs@	%@�@|�@;d@
=@��@5?@@�@��@ƨ@~�@X@ 1'?��m?���?��?���?�33?�hs?�A�??�ƨ?�^?��?�?�9X?��
?�t�?�-?��?��?�j?��m?�dZ?��#?׍P?�
=?�ff?��?ԛ�?�t�?Ұ!?�-?ѩ�?��`?�A�?ϝ�?θR?�O�?�I�?�I�?�ƨ?�?�x�?�b?�ȴ?�Z?�S�?���?���?��;?�;d?�V?��?��m?���?�?���?��#?���?�X?�x�?���?��^?���?�~�?���?�dZ?�1?��D?�/?�O�?��h?��h?��-?��?��?�5??�5??�v�?�v�?���?��R?��?��?���?��?�;d?�\)?�|�?���?��w?��w?��;?� �?�A�A� �A��A��A��A�"�A� �A�&�A�&�A�+A�&�A�(�A�&�A�&�A�(�A�&�A�$�A��A�&�A�$�A�&�A�&�A�+A�&�A�(�A�+A�&�A�"�A��A� �A��A��A� �A�"�A�"�A� �A� �A��A�bA���A��A��yA��A��A��yA��;A���AӮAӟ�Aә�Aӕ�AӓuAӉ7AӅA�x�A�t�A�ffA�S�A�A�A�(�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�$�A�$�A� �A� �A�JA��A��mAӾwAӝ�AӑhA�z�A�I�A�A���Aҧ�Aҗ�A�|�A�v�A�dZA�\)A�S�A�Q�A�S�A�S�A�K�A�E�A�;dA�33A�+A�+A�+A�"�A� �A� �A�"�A� �A�oA�|�A�\)AǑhA���A�"�A���A��A��FA���A��A��A�ĜA�hsA�{A��TA��9A���A�$�A���A�t�A��
A�A�A�"�A��+A�%A���A�(�A�hsA�I�A�G�A�t�A��TA�7LA���A��A�E�A��A��jA��#A��A��A�S�A��A���A�1'A�  A���A�|�A���A���A�  A�p�A�XA���A��A��!A�-A���A��\A�`BA��!A��mA��RA��A��-A��A���A�oA���A�A�K�A�5?A�+A�S�A�\)A�33A�n�A�z�A�A{��AzE�Ax�HAv�Au?}AsAr~�Aq|�Aol�Am�wAlȴAlE�Ak�Ai�-Ag/Af  Ae��Ad{AaA_C�A]�hA\��AZI�AX�`AX^5AW�AVffAT�/ASS�ARA�AQ
=APE�AO�#AO&�AN9XAK7LAI�AHA�AGS�AF��AE�AD�AD�AC`BAB �AAC�A@1'A?��A?G�A>�A=S�A<ffA;��A:�A:VA8�A5&�A4n�A3�A2��A2  A0�`A/��A.�!A.ZA-�FA,jA+7LA)�TA)"�A(1'A&ĜA%;dA#��A!�7A ��A AdZA�AĜAn�A�wA
=A�A�+A�A�^A7LAĜA��A�A+A�
A��Ap�A�TAp�A/A��A �A�A�AbA
�9A	�#A��AI�AC�A~�A��A�7A�9A  A�-A �yA b@�;d@���@��T@���@��@�p�@��9@��@��9@���@�X@��D@�Q�@���@�o@��@�j@�+@�M�@�dZ@�@�@���@߮@�M�@�/@�9X@�
=@٩�@�p�@��m@�
=@υ@�dZ@ēu@�33@�@�@��T@���@��@�t�@��@�9X@���@�-@��u@��@��u@��@�ȴ@�ȴ@�Z@�v�@��R@�/@�`B@��@��@��@��#@�I�@�|�@�v�@�hs@��
@�@���@��H@�n�@���@�A�@��@���@���@���@�V@�ȴ@��@�x�@� �@~$�@z�!@y��@yG�@xb@w�w@w
=@u�-@st�@pA�@nV@m��@m`B@k�F@i�@h�u@f5?@dz�@b�H@a��@_��@^��@^@]�@[��@Z�@X1'@V�R@V$�@U�@R�@QX@P1'@O�w@M/@LI�@K��@J�!@I�#@G�@F�+@D9X@B��@B-@@  @>V@<��@;��@:=q@8��@7�@6�@6$�@5@4��@3��@2��@1X@0  @.��@.{@,�@+��@+C�@*~�@)X@)7L@( �@'K�@&ff@%O�@$j@"~�@!�^@ ��@ A�@|�@��@E�@�@�/@S�@�!@�7@��@��@ȴ@��@O�@V@�F@33@��@-@�@G�@%@��@Q�@�;@|�@��@@`B@��@��@dZ@
~�@	��@	x�@	hs@	%@�@|�@;d@
=@��@5?@@�@��@ƨ@~�@X@ 1'?��m?���?��?���?�33?�hs?�A�??�ƨ?�^?��?�?�9X?��
?�t�?�-?��?��?�j?��m?�dZ?��#?׍P?�
=?�ff?��?ԛ�?�t�?Ұ!?�-?ѩ�?��`?�A�?ϝ�?θR?�O�?�I�?�I�?�ƨ?�?�x�?�b?�ȴ?�Z?�S�?���?���?��;?�;d?�V?��?��m?���?�?���?��#?���?�X?�x�?���?��^?���?�~�?���?�dZ?�1?��D?�/?�O�?��h?��h?��-?��?��?�5??�5??�v�?�v�?���?��R?��?��?���?��?�;d?�\)?�|�?���?��w?��w?��;?� �?�A�A� �A��A��A��A�"�A� �A�&�A�&�A�+A�&�A�(�A�&�A�&�A�(�A�&�A�$�A��A�&�A�$�A�&�A�&�A�+A�&�A�(�A�+A�&�A�"�A��A� �A��A��A� �A�"�A�"�A� �A� �A��A�bA���A��A��yA��A��A��yA��;A���AӮAӟ�Aә�Aӕ�AӓuAӉ7AӅA�x�A�t�A�ffA�S�A�A�A�(�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�}B�}B�wB�wB�wB�wB�wB�wB��B��B��BŢB�B�BJBoB�B�B�B�B�B �B"�B%�B(�B-B0!B33B7LB9XB:^B<jB<jB=qB>wB>wB>wB33Bm�Bt�B{�B�7B�bB�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B~�B~�Bu�BdZB\)BQ�BJ�B?}B/B$�B�B�B�B{BoBPBB��B�B�fB�5B��BɺB��B�dB�?B��B��B��B�=Bz�BdZBZB@�B;dB,BPB
��B
�B
�`B
�B
ȴB
�jB
�-B
�B
��B
�bB
|�B
iyB
]/B
XB
I�B
>wB
5?B
0!B
%�B
hB
	7B
B
B	��B	�yB	�;B	��B	��B	�^B	��B	�hB	�%B	{�B	e`B	]/B	XB	M�B	E�B	9XB	0!B	)�B	-B	,B	(�B	#�B	�B��B��B�B�B�B�B�yB�TB�5B�B��B��B��B��B��B��BɺBƨBÖBÖB�RB�B��B��B��B��B��B�hB�JB�7B�B~�B{�Bv�Bt�Bq�Bo�Bl�BgmBdZBaHB_;B^5B_;B^5B_;BbNBcTBffBe`Be`Be`BdZBcTB`BB`BB_;B]/BYBW
BW
BXBVBT�BT�BT�BQ�BP�BP�BL�BK�BH�BH�BG�BB�BB�B=qB9XB8RB6FB9XB;dB;dB:^B8RB8RB8RB6FB5?B49B5?B2-B2-B1'B0!B1'B1'B1'B49B33B6FB8RB9XB9XB;dB;dB;dB:^B<jB<jB;dB=qB>wBH�BK�BT�B]/BffBm�By�B�B�\B��B�!B�3BÖBɺB��B�B�B��B		7B	oB	�B	1'B	B�B	E�B	XB	ZB	iyB	w�B	�B	�DB	��B	��B	��B	�3B	�FB	�dB	�jB	�}B	�}B	ŢB	��B	��B	��B	�
B	�
B	�;B	�HB	�sB	�sB	�sB	�B	�B	��B	��B	��B
  B
B
1B
DB
PB
\B
bB
uB
uB
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
%�B
'�B
)�B
)�B
+B
.B
.B
1'B
2-B
49B
49B
5?B
6FB
7LB
9XB
9XB
<jB
<jB
<jB
?}B
A�B
B�B
B�B
D�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
L�B
M�B
M�B
O�B
Q�B
Q�B
T�B
T�B
VB
XB
XB
YB
ZB
[#B
]/B
^5B
^5B
_;B
_;B
aHB
aHB
bNB
bNB
dZB
e`B
e`B
ffB
gmB
iyB
jB
iyB
iyB
k�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
x�B
w�B
x�B
x�B
y�B
y�B
z�B
{�B
|�B
~�B
~�B
�B
�B
�B
�B
�+B
�1B
�7B
�=B
�DB
�JB
�PB
�VB
�\B
�bB
�bB
�hB
�hB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�'B
�3B
�3B
�9B
�?B
�?B
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�^B
�XB
�XB�wB�}B�jB�wB�}B�}B�}B�wB�}B�wB�}B�wB�}B��B�wB�}B��B�}B�}B�}B��B�wB�}B�}B�}B�}B�}B�wB�wB�qB�}B�wB�wB�wB�wB�wB�wB�qB�wB�wB�}B�wB�}B�wB�wB�qB�}B��B��B��B��B��B��B��B��BBĜBŢBȴB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B�VB�VB�PB�PB�QB�QB�QB�RB�^B�^B�eB�~B��B�B(BMBlBzBzB�B�B �B"�B%�B(�B,�B0B3B72B9?B:EB<RB<SB=ZB>aB>aB>bB3Bm|Bt�B{�B�#B�OB�7B��B��B��B��B��B��B��B��B��B�sB��B��B��B��B��B��B��B��B��B��B��B�B�tB�UB~�B~�Bu�BdUB\$BQ�BJ�B?zB/B$�B�B�B�B{BoBQBB��B�B�iB�8B��BɾB��B�iB�EB��B��B��B�EBz�BdbBZ&B@�B;nB,BZB
��B
�B
�lB
�#B
��B
�wB
�;B
�"B
��B
�qB
|�B
i�B
]?B
X!B
I�B
>�B
5QB
03B
%�B
{B
	KB
 B
B	��B	�B	�QB	�B	��B	�vB	��B	��B	�>B	| B	ezB	]IB	X+B	M�B	E�B	9tB	0>B	*B	-,B	,&B	)B	#�B	�B�B�B��B�B��B��B�B�wB�YB�;B�#B�B�B�B��B��B��B��BÿB��B�|B�2B�B��B��B��B��B��B�xB�eB�HB)B|Bv�Bt�Bq�Bo�Bl�Bg�Bd�Ba{B_oB^iB_pB^jB_qBb�Bc�Bf�Be�Be�Be�Bd�Bc�B`}B`}B_wB]kBYSBWGBWGBXNBVBBU=BU>BU>BR-BQ&BQ&BMBL	BH�BH�BG�BB�BB�B=�B9�B8�B6�B9�B;�B;�B:�B8�B8�B8�B6�B5�B4�B5�B2zB2zB1uB0pB1vB1wB1wB4�B3�B6�B8�B9�B9�B;�B;�B;�B:�B<�B<�B;�B=�B>�BIBL'BUaB]�Bf�Bm�BzIB��B��B�BB��B��B�B�=B�rBڦB�B�wB		�B	B	?B	1�B	C.B	FDB	X�B	Z�B	j%B	x~B	��B	��B	�?B	��B	��B	��B	�
B	�+B	�4B	�JB	�MB	�uB	ѼB	��B	��B	��B	��B	�!B	�1B	�^B	�aB	�dB	�yB	�B	��B	��B	��B
B
B
	9B
NB
]B
lB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
%
B
'B
))B
+7B
+:B
,CB
/XB
/ZB
2pB
3yB
5�B
5�B
6�B
7�B
8�B
:�B
:�B
=�B
=�B
=�B
@�B
B�B
C�B
DB
FB
H!B
I)B
J2B
J5B
J8B
K@B
KCB
LMB
MVB
N^B
OgB
OjB
QxB
S�B
S�B
V�B
V�B
W�B
Y�B
Y�B
Z�B
[�B
\�B
^�B
_�B
_�B
`�B
`�B
cB
cB
dB
dB
f)B
g2B
g5B
h=B
iGB
kVB
l^B
k[B
k^B
mlB
o{B
nxB
o�B
o�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
v�B
w�B
x�B
w�B
x�B
x�B
x�B
z�B
y�B
z�B
z�B
|B
|B
}B
~B
$B
�5B
�<B
�MB
�`B
�sB
�xB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�"B
�5B
�HB
�BB
�SB
�YB
�mB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�$B
�8B
�OB
�pB
��B
��B
��B
��B
��B
��B
�B
�B
�0B
�FB
�UB
�qB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�;B
�8B
�:B
�>B
�AB
�DB
�GB
�JB
�SB
�PB
�SB
�\B
�YB
�\B
�_B
�hB
�eB
�iB
�rB
�oB
�xB
�{B
�~B
��B
�~B
��B�PB�VB�CB�PB�VB�UB�VB�PB�VB�PB�VB�OB�VB�\B�PB�VB�\B�VB�VB�VB�\B�PB�VB�VB�VB�VB�VB�PB�PB�JB�VB�PB�PB�PB�PB�PB�QB�KB�QB�QB�WB�QB�WB�QB�QB�LB�XB�^B�^B�^B�^B�^B�^B�eB�eB�kB�xB�~BȐB̩G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809290001452021061413554220210614135542202106171313042021061713130420210617131304201809290001452021061413554220210614135542202106171313042021061713130420210617131304PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018092900014520180929000145  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092900014520180929000145QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092900014520180929000145QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150420210617131504IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                