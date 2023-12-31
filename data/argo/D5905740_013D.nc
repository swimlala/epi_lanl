CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:03Z creation      
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
_FillValue                 $  Ld   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  a   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e<   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   $   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   <   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                        SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � dArgo profile    3.1 1.2 19500101000000  20180724220303  20210722161419  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�n�OtsD@�n�OtsD11  @�n�I���@�n�I���@*^$5inY@*^$5inY�cKh��A_�cKh��A_11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?fff@   @Fff@�33@�33@�  @�  A��A33A#33A@  Aa��A�ffA���A�  A�33A�  Aљ�AᙚA�B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBP��BX  B_��Bg��Bp  Bx  B�33B�  B�  B�  B�33B�33B�33B�33B�ffB�33B�ffB�33B�  B�33B�  B���B�  B�ffBǙ�B���B�  B�  B�  B�33B�33B�ffB�33B�33B�33B�  B�ffB�33C   C  C�fC�fC�fC
33C33C33C33C�fC  C�C�fC�CL�C�C�fC"�C$ffC&�C(  C)�3C+�fC.�C033C2� C433C5�fC7�3C9�fC<�C>33C@33CBL�CD33CF�CH  CI�fCL�CN  CO�fCR  CT�CV33CXL�CZffC\L�C^  C`�CbL�CdL�Cf33Ch33Cj�Cl  Cn  Co�fCr�CtL�CvL�Cx33Cz33C|33C~  C�  C�  C��C��C��C��C�&fC��C��fC��3C��3C��3C��3C��3C��C��C�&fC��C�ٚC��fC�  C��C�&fC��C��3C��C�  C��fC�  C��C��3C��fC��3C��C�  C��3C��C�  C��3C��C��C�  C��C��C�  C�&fC��C�  C��C�  C�  C��C��C�  C�  C��3C��C��C�  C��C��C�  C�  C��3C��3C��3C��C�&fC�&fC�&fC�&fC�&fC��C��C�  C�&fC��C��C��C��C��C�  C�  C��3C��C��C��C��C��C�  C�  C�  C��3C��C��C��C��C�  C��3C��fC��C��C�  C�  C��3C��C��C��C��C��3C�&fC��C��C��C��fC��3C��C��C��C��C��C��C�ٚC���D��DٚD� D��D��D� D��D�fD�fD��D ` D#33D%�3D(��D+S3D.�D0��D3ffD6�D8��D;L�D=�fD@l�DB��DE�fDHfDJ��DM&fDO�fDR  DT��DW  DY�fD\l�D_  Da�fDdS3Df�3Di��Dl` Do  Dq� Dt�3Dw` Dz&fD|s3D9�D�  D�i�D�ɚD�)�D�� D��fD�6fD��fD��fD��D�\�D��fD��3D�6fD�p D��fD���D�<�D�y�D���D��D�Y�D��3D���D�9�D��3D��3D�3D�@ D�� D��fD�3D�@ D�|�D��fD��fD�<�D�|�D��3D���D�  D�` D���D��fD�  D�Y�D��3D��fD��D�ffD���D��fD�P DĜ�D��fD�0 Dȃ3D��3D�,�D̃3D���D�33DІfD���D�,�Dԃ3D��3D�&fD�|�D��3D�)�D܃3D��3D�@ D��3D�� D�FfD䙚D���D�@ D��D��3D�0 D�|�D���D� D�S3D�3D�� D��D�C3D�y�D��fD��D��D�33D�\�D���D���D��E �fE&fE� ENfE�3Et�E	�E�fE33E��ET�E��Et�E�E��E	!�E	��E
D�E
� EnfE�E� E  E�fE��E��E��E�3E�3E	�E��E�E E>fEd�E��E8 E^fE �3E!�3E"��E$�fE%��E&��E'�fE)��E*�fE+��E,��E.nfE/��E0��E2,�E3>fE4H E5� E6� E8x E9��E:��E<3E?�EB+3EE8 EH;3EK�fEN��EQ��EU33EX1�E[0 E^!�Ea�3Ed~fEg�fEj��En	�Ep�Et&fEwX Ez|�E}� E�C3E�� E�_3E��3E���E�$ E���E�1�E��fE�X E�3E�| E�3E��fE�!�E���E�8�E���E�a�E�� E�� E�fE���E�0 E��3E�S3E���E�, E��fE��fE��E�x E�� E� E�W3E��3E���E�Q�E���E��fE�L E��3E��3E�*fE���?333?333?333?333?L��?L��?L��?L��?L��?L��?L��?L��?fff?�  ?L��?L��?�  ?�  ?���?���?���?���?�ff?�33?�33?���?�ff@   @33@   @,��@9��@L��@Y��@l��@�  @���@�  @���@�33@�  @���@�33@�33@�  @���@�ffA��A  A  AffA33A#33A)��A333A<��AC33AI��AQ��AY��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144444441144141141114111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ?�33@   @fff@�33@�33@�  @�  A	��A33A+33AH  Ai��A�ffA���A�  A�33A�  Aՙ�A噚A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBR��BZ  Ba��Bi��Br  Bz  B�33B�  B�  B�  B�33B�33B�33B�33B�ffB�33B�ffB�33B�  B�33B�  B���B�  B�ffBș�B���B�  B�  B�  B�33B�33B�ffB�33B�33B�33B�  B�ffB�33C � C� CffCffCffC
�3C�3C�3C�3CffC� C��CffC��C��C��C ffC"��C$�fC&��C(� C*33C,ffC.��C0�3C3  C4�3C6ffC833C:ffC<��C>�3C@�3CB��CD�3CF��CH� CJffCL��CN� CPffCR� CT��CV�3CX��CZ�fC\��C^� C`��Cb��Cd��Cf�3Ch�3Cj��Cl� Cn� CpffCr��Ct��Cv��Cx�3Cz�3C|�3C~� C�@ C�@ C�L�C�Y�C�Y�C�Y�C�ffC�L�C�&fC�33C�33C�33C�33C�33C�L�C�Y�C�ffC�L�C��C�&fC�@ C�L�C�ffC�L�C�33C�L�C�@ C�&fC�@ C�L�C�33C�&fC�33C�L�C�@ C�33C�L�C�@ C�33C�Y�C�L�C�@ C�Y�C�L�C�@ C�ffC�L�C�@ C�Y�C�@ C�@ C�Y�C�L�C�@ C�@ C�33C�L�C�L�C�@ C�Y�C�L�C�@ C�@ C�33C�33C�33C�Y�C�ffC�ffC�ffC�ffC�ffC�L�C�Y�C�@ C�ffC�Y�C�Y�C�Y�C�Y�C�L�C�@ C�@ C�33C�Y�C�Y�C�Y�C�L�C�L�C�@ C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�@ C�33C�&fC�L�C�L�C�@ C�@ C�33C�Y�C�L�C�L�C�L�C�33C�ffC�Y�C�Y�C�Y�C�&fC�33C�Y�C�Y�C�L�C�L�C�L�C�L�C��C��D�D��D	  D�D�D  D��D�fD�fD��D � D#S3D&3D(��D+s3D.,�D0ٚD3�fD6,�D8��D;l�D>fD@��DC�DE�fDH&fDJ��DMFfDO�fDR@ DT��DW@ DY�fD\��D_  Da�fDds3Dg3Di��Dl� Do@ Dr  Dt�3Dw� DzFfD|�3DY�D� D�y�D�ٚD�9�D�� D��fD�FfD��fD��fD�,�D�l�D��fD�3D�FfD�� D��fD�	�D�L�D���D���D��D�i�D��3D���D�I�D��3D��3D�3D�P D�� D��fD�3D�P D���D��fD�fD�L�D���D��3D���D�0 D�p D���D��fD�0 D�i�D��3D��fD�,�D�vfD���D�fD�` DĬ�D��fD�@ Dȓ3D��3D�<�D̓3D���D�C3DЖfD���D�<�Dԓ3D��3D�6fD،�D��3D�9�Dܓ3D��3D�P D�3D�� D�VfD䩚D���D�P D��D��3D�@ D��D���D�  D�c3D�3D�� D��D�S3D���D��fD���D�)�D�C3D�l�D���D�ɚD���E �fE.fE� EVfE�3E|�E�E�fE;3E��E\�E��E|�E�E��E	)�E	��E
L�E
� EvfE	�E� E( E�fE��E��E��E�3E�3E�E��E�E  EFfEl�E��E@ EffE �3E!�3E"��E$�fE%��E&ɚE'�fE)��E*�fE+��E,��E.vfE/��E0��E24�E3FfE4P E5� E6� E8� E9��E:��E<3E?!�EB33EE@ EHC3EK�fEN��EQ��EU;3EX9�E[8 E^)�Ea�3Ed�fEg�fEjɚEn�Ep�Et.fEw` Ez��E}� E�G3E�� E�c3E�3E���E�( E���E�5�E��fE�\ E�3E�� E�#3E��fE�%�E���E�<�E���E�e�E�� E�� E�fE���E�4 E��3E�W3E���E�0 E��fE��fE��E�| E�� E� E�[3E��3E���E�U�E���E��fE�P E��3E��3E�.fE���G�O�G�O�G�O�?���G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�ff?�33G�O�G�O�?�ffG�O�?�  ?���G�O�?���?ٙ�?�ffG�O�?�33@ff@33@   @333@@  @L��@Y��@l��@y��@�ff@�  @���@�  @���@�33@�  @���@�33@�33@�  @���A33A	��A  A  AffA#33A+33A1��A;33AD��AK33AQ��AY��Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144444441144141141114111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      @ %@ �@ *@ �@ "�@ (�@ /�@ 7L@ >�@ E�@ Q�@ `B@ oF@ {�@ ��@ ��@ ��@ ��@ ��@ �*@ �#@ ��@ �q@@�@g@-@:�@H]@V@dZ@p�@}�@�D@��@�A@��@@�7@��@�4@��@�@*@#�@0x@>�@K�@X�@g@t@�@�\@�a@�M@��@��@Ӡ@�H@�@�E@�@�@&;@3�@@�@O�@\�@i�@ww@�p@�@��@�r@�@��@�h@�@�Y@ �@�@�@+@7L@C�@R�@bN@m�@z�@�|@��@��@��@@�*@��@�@�@@o@ @.l@;d@H]@UU@bN@qS@~K@�D@��@��@�F@Ĝ@��@��@�@��@�@�@#�@1'@>@K@X�@e�@t�@��@�h@�a@�@��@��@Ӡ@�H@�@��@�@B@'�@3�@?}@M�@[z@i!@v�@�p@�u@��@�!@�k@�W@խ@�@�@@V@�@)�@6�@B�@Q�@`B@l�@y�@��@��@��@��@��@��@��@�y@�q@	j@	o@	g@	,`@	<@	H]@	UU@	dZ@	p�@	~K@	�P@	�H@	�A@	��@	��@	��@	ލ@	�@	��@
�@
{@
""@
/@
<�@
Ji@
Z@
hs@
v@
��@
�h@
�@
�Y@
��@
��@
խ@
�@
�L@
��@�@�@%�@33@@,@O�@]�@k.@x&@��@��@�m@�@�@��@�h@�`@�@  @�@�@)�@7L@DD@Q�@^�@n�@{�@�7@��@�(@��@��@�*@��@�@�@�@o@g@-@:�@H]@SI@`B@�~@F�@��@��@>�@�\@�;@0x@~�@�*@O@hs@��@��@FQ@��@��@"�@k.@��@��@A�@�|@�@�@V@�U@��@&;@i�@�@�Y@:�@�@�c@�@Z�@�y@��@6�@��@��@�@b�@��@�@9X@�@�C@[@hs@��@��@E�@�P@��@�@^�@�4@�4@1'@t@��@��@ C�@ �+@ �@!�@!Z�@!�@!�m@".l@"t�@"�@"�E@#@�@#�@#��@$V@$Q�@$��@$�h@%�@%bN@%��@%��@&+�@&m:@&�~@&�e@'7�@'~K@'�2@(@(I@(��@(��@)�@)`�@)��@)�@*7�@*~K@*ƨ@+V@+X@+�@+��@,3�@,|?@,�J@-�@-V@-��@-�@./@.x&@.�2@/
�@/V@/�m@/��@0/�@0|?@0Ĝ@1�@1UU@1�U@1�`@2,`@2s_@2�@3  @3D�@3�7@3��@4b@4R�@4��@4�h@5�@5Z�@5��@5�\@66@6Wb@6�<@6��@7�@7X�@7��@7�h@8�@8V@8��@8��@9@9P�@9��@9�o@:�@:D�@:�d@:��@:��@;=q@;}�@;�k@;�,@<6�@<s_@<��@=�Z@>$/@>�a@?�@?��@@S�@@��@AQ�@Aψ@BM$@B�@C��@C�Q@D|?@D��@Ev@F2�@F��@G&;@G��@HS�@H�@IB�@I�^@Jl�@J��@KWb@Lv@Lz2@L�@M��@N
=@N��@O&;@O��@P;d@Q�7@R�h@T%�@Un�@V�@@X5@@Yx&@Z� @\> @]�@^ƨ@`;d@a}�@b�@d)�@e�@fƨ@h+�@i��@j��@l/�@ms_@n�H@p�@q}�@r�h@t,`@u|?@v�W@x5?@yww@z�@|%�@}�C@~ě@�V@���@�_�@��@��@�b�@��@��X@�e	@��@��8@�dZ@�i@�.@�X@�u@���@���@��
@�  @��@�C�@�^5@���@���@�ȴ@��@��@�4�@�O�@�x&G�O�G�O�G�O�@ �G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ v@ %G�O�G�O�@ vG�O�@ �@ �G�O�@ �@ 1@ �G�O�@ 	�@ 
�@ J@ �@ �@ @ o@ �@ �@ 6@ B@ O@ [@ �@  �@ "�@ %�@ (G@ )�@ -@ /�@ 2�@ 4�@ 7L@ :@ =q@ @,@ B8@ E�@ H]@ Lu@ P�@ SI@ V@ Yn@ \�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���Aڥ�A�hsA�1'A�  A��mA���Aٲ-A٣�AًDA�p�A�K�A�-A��A��A��A��A��A��A�"�A�"�A��A��A��A�A��yAة�A�?}A��/A�S�A�C�A��A�bA���A�v�A�;dA���A΍PA�9XAƓuA�A�Aĕ�A�O�A�VA�7LA�|�A�33A�5?A��A���A�?}A���A��+A���A�hsA�v�A���A�C�A��A���A��A�v�A�A� �A�XA�bNA�
=A�5?A�$�A�$�A�p�A�A�A�dZA~�AxAot�Al�!Aj�Ab�A_�TA[l�AV��AOAKl�AI�AH  AB��A>VA<��A;��A8��A7�A7�A77LA7A6�yA6ĜA6�jA6�DA6Q�A65?A6(�A6{A5�A5�#A5ƨA5�-A5��A5��A5x�A5l�A5l�A5\)A5G�A5+A5�A5%A4�A4�!A4�+A4VA4 �A3�A3ƨA3|�A3/A2�/A2�uA2E�A1�
A1?}A0�A0^5A/��A/t�A.�A.I�A-��A,��A+�A+hsA+�A* �A(bNA&��A$��A#�A"=qA"ZA"�A"v�A!p�A!
=A �jA n�A -A�FA��A/AM�A�PA�A�A��A%A-A&�A��A|�AO�A�/A��AffA=qA�;A��AJAl�A��AM�A�#A\)A�A��A�RAn�A�
A;dA�`A��Az�A  A��A"�A��A��AE�A��A+AĜA{Ap�A\)A
�A
Q�A	�A	A��A��A��A	7LA	+AȴA$�A��A��A�FAĜA��A�RA�uA5?A�-AG�A�A�A�DA�A$�A�hAVA��A=qA��AƨA"�A jA   @�33@�@��y@�n�@�p�@���@�b@���@��P@���@��^@��`@��@�ff@�5?@��@��@�9X@웦@�dZ@�w@�b@��@��;@ۮ@ج@�=q@ӝ�@Ѓ@�E�@̴9@�E�@ǍP@�G�@�K�@��u@���@��T@��
@���@���@�`B@�(�@��@��m@�=q@�ƨ@�ȴ@���@�K�@��@��F@��y@���@�v�@�G�@���@���@��9@�S�@��@�Ĝ@�dZ@��@�J@���@���@�V@��w@���@��h@��m@�^5@��h@�Z@K�@}�@}?}@z��@y&�@v��@uO�@r��@p  @m?}@j�H@i�^@g\)@ep�@c�@`��@_K�@]�@[C�@Z~�@YG�@W�;@U��@Tj@R�!@P �@N�y@M�T@L�j@KC�@IX@Gl�@D��@B�!@@�`@?l�@>v�@<�j@;33@9�#@7�@6v�@4�D@2�\@1hs@0 �@.�R@-�h@,z�@+��@*��@)7L@(��@'|�@&V@$��@#�m@"�H@!��@ �@�;@ff@��@�m@33@�@-@�`@ �@l�@�@O�@�@o@��@J@��@G�@bN@;d@�+@�@�/@9X@��@"�@
��@	��@	hs@Ĝ@bN@  @|�@ȴ@E�@@��@�j@�
@C�@�!@�\@J@�^@7L@�@ Ĝ@  �?���?�v�?��h?��?��D?�1?��H?���?��?��y?��?�9X?�J?�G�?�bN?�\)?�V?�O�?�ƨ?ꟾ?��?�X?�Q�?�+?�E�?�?}?��?�33?�\?�7?�Ĝ?߾w?�\)?޸R?�V?ݑh?�/?�j?ۥ�?�~�?ٙ�?�7L?�Q�?և+?��?�S�?�-?� �?θR?�O�?˥�?���?ȴ9?�l�?Ƈ+?ļj?��
?�n�?�Ĝ?�|�?��h?��D?���?���?��?��?���?�1'?��P?�E�?��T?��/?���?���?��F?��?��?��!?�M�?�M�?�J?��\?��?�33?��
?�Z?��/?���?�E�?�
=?���?��?�1'?�1'?�r�?��u?��9?���?���?��?�X?���?��^?��#?���?�=q?�^5?�~�?���A���A���A�A�ĜA�ƨA�ȴA�ƨA�ȴA�ĜA�ƨA�ȴA�ȴA�ȴA���A�ȴA�ȴA�ƨA�ȴA�ȴA�ƨA���A���A���A��
A��A��
A���Aڕ�Aڇ+AځA�t�A�n�A�l�A�O�A�M�A�;dA�&�A�{A�
=A���A���A��A��HA��#A���A�ĜAٸRAٲ-A٩�A٥�A١�Aٕ�AًDAفA�|�A�t�A�jA�dZA�^5A�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      A���Aڥ�A�hsA�1'A�  A��mA���Aٲ-A٣�AًDA�p�A�K�A�-A��A��A��A��A��A��A�"�A�"�A��A��A��A�A��yAة�A�?}A��/A�S�A�C�A��A�bA���A�v�A�;dA���A΍PA�9XAƓuA�A�Aĕ�A�O�A�VA�7LA�|�A�33A�5?A��A���A�?}A���A��+A���A�hsA�v�A���A�C�A��A���A��A�v�A�A� �A�XA�bNA�
=A�5?A�$�A�$�A�p�A�A�A�dZA~�AxAot�Al�!Aj�Ab�A_�TA[l�AV��AOAKl�AI�AH  AB��A>VA<��A;��A8��A7�A7�A77LA7A6�yA6ĜA6�jA6�DA6Q�A65?A6(�A6{A5�A5�#A5ƨA5�-A5��A5��A5x�A5l�A5l�A5\)A5G�A5+A5�A5%A4�A4�!A4�+A4VA4 �A3�A3ƨA3|�A3/A2�/A2�uA2E�A1�
A1?}A0�A0^5A/��A/t�A.�A.I�A-��A,��A+�A+hsA+�A* �A(bNA&��A$��A#�A"=qA"ZA"�A"v�A!p�A!
=A �jA n�A -A�FA��A/AM�A�PA�A�A��A%A-A&�A��A|�AO�A�/A��AffA=qA�;A��AJAl�A��AM�A�#A\)A�A��A�RAn�A�
A;dA�`A��Az�A  A��A"�A��A��AE�A��A+AĜA{Ap�A\)A
�A
Q�A	�A	A��A��A��A	7LA	+AȴA$�A��A��A�FAĜA��A�RA�uA5?A�-AG�A�A�A�DA�A$�A�hAVA��A=qA��AƨA"�A jA   @�33@�@��y@�n�@�p�@���@�b@���@��P@���@��^@��`@��@�ff@�5?@��@��@�9X@웦@�dZ@�w@�b@��@��;@ۮ@ج@�=q@ӝ�@Ѓ@�E�@̴9@�E�@ǍP@�G�@�K�@��u@���@��T@��
@���@���@�`B@�(�@��@��m@�=q@�ƨ@�ȴ@���@�K�@��@��F@��y@���@�v�@�G�@���@���@��9@�S�@��@�Ĝ@�dZ@��@�J@���@���@�V@��w@���@��h@��m@�^5@��h@�Z@K�@}�@}?}@z��@y&�@v��@uO�@r��@p  @m?}@j�H@i�^@g\)@ep�@c�@`��@_K�@]�@[C�@Z~�@YG�@W�;@U��@Tj@R�!@P �@N�y@M�T@L�j@KC�@IX@Gl�@D��@B�!@@�`@?l�@>v�@<�j@;33@9�#@7�@6v�@4�D@2�\@1hs@0 �@.�R@-�h@,z�@+��@*��@)7L@(��@'|�@&V@$��@#�m@"�H@!��@ �@�;@ff@��@�m@33@�@-@�`@ �@l�@�@O�@�@o@��@J@��@G�@bN@;d@�+@�@�/@9X@��@"�@
��@	��@	hs@Ĝ@bN@  @|�@ȴ@E�@@��@�j@�
@C�@�!@�\@J@�^@7L@�@ Ĝ@  �?���?�v�?��h?��?��D?�1?��H?���?��?��y?��?�9X?�J?�G�?�bN?�\)?�V?�O�?�ƨ?ꟾ?��?�X?�Q�?�+?�E�?�?}?��?�33?�\?�7?�Ĝ?߾w?�\)?޸R?�V?ݑh?�/?�j?ۥ�?�~�?ٙ�?�7L?�Q�?և+?��?�S�?�-?� �?θR?�O�?˥�?���?ȴ9?�l�?Ƈ+?ļj?��
?�n�?�Ĝ?�|�?��h?��D?���?���?��?��?���?�1'?��P?�E�?��T?��/?���?���?��F?��?��?��!?�M�?�M�?�J?��\?��?�33?��
?�Z?��/?���?�E�?�
=?���?��?�1'?�1'?�r�?��u?��9?���?���?��?�X?���?��^?��#?���?�=q?�^5?�~�?���A���A���A�A�ĜA�ƨA�ȴA�ƨA�ȴA�ĜA�ƨA�ȴA�ȴA�ȴA���A�ȴA�ȴA�ƨA�ȴA�ȴA�ƨA���A���A���A��
A��A��
A���Aڕ�Aڇ+AځA�t�A�n�A�l�A�O�A�M�A�;dA�&�A�{A�
=A���A���A��A��HA��#A���A�ĜAٸRAٲ-A٩�A٥�A١�Aٕ�AًDAفA�|�A�t�A�jA�dZA�^5A�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	@�B	<jB	=qB	;dB	:^B	9XB	8RB	9XB	9XB	9XB	:^B	;dB	9XB	8RB	9XB	:^B	<jB	>wB	?}B	A�B	B�B	B�B	B�B	B�B	A�B	@�B	=qB	33B	$�B	'�B	2-B	1'B	0!B	.B	,B	,B	 �B	K�B	hsB	ǮB
1B
$�B
bNB
�B
��B
��B
��B
�#BVBJB+B1'B5?BhsB�B�B�FB�!B�9B�?B�-B��B�PBm�BD�B�B
��B
��B
�JB
y�B
G�B
B	�B	n�B	I�B	oB	%B�HB�jB��B�oBl�BA�B;dB,B!�B��BB%B%B�B0!B:^B@�BE�BN�BVBYBffBw�B�B�%B�hB��B�B�LBB��B�/B�B��B	PB	uB	"�B	49B	<jB	L�B	`BB	m�B	u�B	�B	�=B	�{B	��B	��B	�B	�RB	�qB	ŢB	��B	�#B	�;B	�TB	�sB	�B	�B	��B	��B
B
B
%B
%B
B	��B	�B	�B	�mB	�`B	��B
�B
%�B
,B
,B
.B
49B
8RB
9XB
:^B
9XB
9XB
9XB
49B
2-B
7LB
<jB
;dB
;dB
<jB
=qB
A�B
>wB
>wB
?}B
C�B
D�B
I�B
N�B
J�B
I�B
G�B
G�B
G�B
I�B
J�B
J�B
H�B
E�B
E�B
E�B
E�B
E�B
B�B
B�B
C�B
E�B
C�B
C�B
B�B
@�B
?}B
7LB
8RB
8RB
5?B
0!B
)�B
'�B
(�B
,B
,B
2-B
2-B
.B
+B
,B
1'B
33B
+B
/B
33B
1'B
0!B
+B
)�B
+B
/B
-B
,B
/B
,B
-B
,B
,B
)�B
(�B
'�B
&�B
#�B
$�B
#�B
#�B
"�B
 �B
�B
�B
!�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
PB
PB
	7B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B
B
  B
B
1B
PB
\B
hB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
%�B
#�B
%�B
'�B
'�B
+B
+B
+B
/B
/B
1'B
1'B
49B
6FB
5?B
5?B
6FB
7LB
9XB
<jB
>wB
@�B
@�B
B�B
D�B
F�B
F�B
H�B
I�B
K�B
L�B
M�B
N�B
P�B
P�B
Q�B
R�B
Q�B
S�B
W
B
XB
ZB
ZB
ZB
[#B
\)B
^5B
^5B
_;B
_;B
`BB
aHB
bNB
cTB
dZB
dZB
e`B
e`B
ffB
iyB
hsB
jB
k�B
l�B
m�B
m�B
o�B
p�B
p�B
q�B
r�B
t�B
u�B
v�B
v�B
v�B
v�B
w�B
x�B
y�B
x�B
z�B
{�B
|�B
}�B
~�B
~�B
� B
� B
}�B
� B
~�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�1B
�7B
�7B
�7B
�=B
�DB
�JB
�JB
�PB
�VB
�PB
�VB
�\B
�bB
�\B
�hB
�bB
�oB
�oB
�hB
�uB
�uB
�{B
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
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�'B
�-B
�3B
�3B
�9B
�9B
�?B
�?B
�FB
�FB
�LB
�LB
�RB
�XB
�^B
�^B
�jB
�dB
�qB
�qB
�wB
�wB
�}B
��B
��B
��B
B
��B
ÖB
ÖB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ƨB
ŢB
ƨB
ƨB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB	@�B	<jB	@�B	@�B	@�B	@�B	@�B	?}B	@�B	@�B	?}B	?}B	@�B	@�B	?}B	@�B	@�B	@�B	@�B	@�B	A�B	?}B	A�B	@�B	@�B	@�B	8RB	9XB	>wB	=qB	=qB	=qB	=qB	;dB	>wB	8RB	;dB	:^B	:^B	;dB	:^B	9XB	9XB	9XB	8RB	8RB	9XB	9XB	9XB	9XB	9XB	9XB	9XB	9XB	:^B	9XB	:^B	:^B	;dB	;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      B	@]B	<DB	=LB	;?B	:9B	94B	8.B	94B	94B	95B	:;B	;BB	97B	81B	98B	:>B	<KB	>XB	?_B	AlB	BrB	BsB	BsB	BtB	AoB	@iB	=XB	3B	$�B	'�B	2B	1B	0B	-�B	+�B	+�B	 �B	K�B	h_B	ǚB
B
$�B
b<B
�B
��B
��B
��B
�BEB:B*�B1B50BhdB�B�B�9B�B�-B�3B�"B��B�FBm�BD�BwB
��B
��B
�AB
y�B
G�B
	B	��B	n�B	I�B	fB	B�?B�aB��B�gBl�BA�B;\B, B!�B��BBBB�B0B:YB@BE�BN�BVBYBfeBw�B�B�&B�iB��B�B�OBB��B�4B�B��B	WB	|B	"�B	4AB	<sB	L�B	`LB	m�B	u�B	�B	�JB	��B	��B	��B	�*B	�aB	��B	ųB	��B	�5B	�MB	�gB	�B	�B	�B	��B	��B
B
0B
<B
=B
B	��B	��B	�B	�B	�zB	��B
�B
%�B
,%B
,%B
.2B
4XB
8qB
9xB
:~B
9yB
9yB
9zB
4[B
2PB
7oB
<�B
;�B
;�B
<�B
=�B
A�B
>�B
>�B
?�B
C�B
D�B
I�B
OB
J�B
I�B
G�B
G�B
G�B
I�B
J�B
J�B
H�B
E�B
E�B
E�B
E�B
E�B
B�B
B�B
C�B
E�B
C�B
C�B
B�B
@�B
?�B
7�B
8�B
8�B
5yB
0[B
*7B
(+B
)2B
,DB
,EB
2kB
2kB
.SB
+AB
,HB
1gB
3tB
+DB
/]B
3vB
1jB
0eB
+FB
*AB
+GB
/aB
-UB
,OB
/cB
,PB
-WB
,QB
,RB
*GB
)AB
(<B
'5B
$$B
%*B
$%B
$&B
# B
!B
 B
B
"B
##B
 B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
�B
}B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	�{B	�~B
�B
 �B
�B
�B
�B
B
B
B
-B
<B
EB
TB
dB
gB
vB
�B
 �B
 �B
"�B
%�B
&�B
$�B
&�B
(�B
(�B
+�B
+�B
+�B
0B
0B
2$B
2'B
5<B
7LB
6HB
6KB
7UB
8^B
:nB
=�B
?�B
A�B
A�B
C�B
E�B
G�B
G�B
I�B
J�B
L�B
NB
OB
PB
R(B
R+B
S5B
T>B
S;B
UJB
X_B
YgB
[wB
[zB
[}B
\�B
]�B
_�B
_�B
`�B
`�B
a�B
b�B
c�B
d�B
e�B
e�B
f�B
f�B
g�B
k	B
jB
lB
mB
n(B
o1B
o4B
qDB
rNB
rQB
sZB
tcB
vrB
w}B
x�B
x�B
x�B
x�B
y�B
z�B
{�B
z�B
|�B
}�B
~�B
�B
��B
��B
��B
��B
�B
��B
��B
�B
�B
�B
�B
�$B
�'B
�*B
�9B
�<B
�?B
�AB
�JB
�SB
�VB
�YB
�bB
�kB
�tB
�wB
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
�B
�B
� B
�+B
�:B
�EB
�KB
�\B
�bB
�oB
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
�B
�B
�B
�B
�-B
�2B
�:B
�?B
�JB
�KB
�`B
�|B
��B
��B
��B
��B
��B
��B
�B
�B
�(B
�9B
�NB
�_B
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�7B
�GB
�\B
�qB
ƆB
ƖB
ǫB
ƵB
��B
��B
��B
�B
�B
�)B
�8B
�HB
�WB
�gB
�vB
˅B
˕B
˥B
˳B
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
��B	@]B	<DB	@]B	@]B	@]B	@]B	@]B	?WB	@]B	@]B	?WB	?WB	@]B	@]B	?WB	@]B	@]B	@]B	@]B	@]B	AcB	?WB	AcB	@]B	@]B	@]B	8,B	92B	>QB	=LB	=LB	=LB	=LB	;?B	>RB	8-B	;?B	:9B	:9B	;?B	:9B	93B	94B	94B	8.B	8.B	94B	94B	94B	94B	95B	95B	95B	95B	:;B	95B	:;B	:;B	;BB	;BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203032021061413573420210614135734202107221611342021072216113420210722161134201807242203032021061413573420210614135734202107221611342021072216113420210722161134PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030320180724220303  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030320180724220303QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030320180724220303QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141920210722161419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                