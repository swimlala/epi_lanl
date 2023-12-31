CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-25T00:07:35Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
_FillValue                 (  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  aH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ep   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ը   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20181025000735  20210722160154  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               &   &DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؉ P��F@؉ P��F11  @؉ O��p@؉ O��p@6�ڹ�Z@6�ڹ�Z�c�J�!�c�J�!11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?L��@��@L��@�33@�  @�  @�33@���AffA$��A@  A`  A~ffA�  A���A���A�33A�  A���A�  B ffBffB  B��B ��B(��B0ffB8  B@  BH  BP  BX  B`ffBi33Bp��Bx��B�ffB�33B�33B�33B�  B�33B���B�ffB�33B�  B���B�  B�  B���B���B�  B�33B�ffB���B�33B�33B�33B�33B�33B�  B�  B�ffB�33B���B�33B�ffB���C �C��C�fC  C33C
33C�C�C  C  C  C  C  C  C  C  C �C"33C$L�C&  C'��C)�fC,  C.33C0�C1�fC4�C6  C7�fC:33C<�C>  C@  CB  CC�fCE��CH33CJ�CL  CNL�CP33CR�CT  CU��CX�CZ  C[�fC]�fC_��Cb�Cd33Cf�Cg�fCj�Ck�fCm��Co�fCr  Ct33Cv�Cw�fCz  C|33C~L�C��C��3C��3C�  C�  C��C��C��C��C��C��C�  C�  C�  C��3C��3C��3C��3C��fC��C�&fC�33C��C�&fC��C��C��C��C�  C��fC��fC��3C�  C��C�&fC�  C��fC�  C�&fC��C��fC�  C��C�  C��3C��C�  C��3C��C��C�  C��3C��fC�  C��C�33C��C��3C��C��C�&fC��C��3C�  C��C�&fC�  C��fC��3C�  C�&fC��C��fC��C��C�  C��3C�  C��C��C�&fC��C��fC�  C��C�  C��fC�  C��C�&fC��C��3C��C��C��3C��C�&fC��C��3C�  C��C��C�&fC��C�ٚC��fC��fC��3C�  C�  C��C��C�&fC�33C�33C��C�ٚC��C�33C�&fC�&fC�  C�ٚC�ٚC��fC��fC��fC��fD   D �fD  D� D�3D��D�3D��D,�D�3D33D��D9�D� DS3D��D"S3D$� D'ffD)�3D,�3D/�D1�3D43D6��D9�D;�3D>3D@�3DC&fDE��DH�DJ��DM&fDO�fDR&fDT�3DW�DY�fD\fD^y�D`�3Dc` De��Dh&fDj��Dm3Do�fDq�3Dt` Dv�fDy9�D{9�D}�3D� D�FfD��3D���D��D�&fD�Y�D���D��3D� D�L�D��3D�ɚD�3D�<�D�y�D���D�  D�@ D���D�� D�fD�` D�� D��3D�)�D�ffD�� D�ٚD� D�L�D���D��fD���D�,�D�Y�D��fD���D��fD�3D�0 D�\�D�y�D�� D�� D��fD��D�)�D�I�D�i�D��3D���D��D�3D�,�D�L�D�vfDĖfDų3D��3D��3D��D�@ D�p D̜�D�ɚD��3D��D�FfD�vfDө�D��3D��D�FfD�vfD٣3D�� D��fD��D�FfD�i�D���D� D�ٚD���D�fD�6fD�S3D�p D� D� D��3D���D�	�D�&fD�9�D�L�D�VfD�c3D�ffD�vfD�� D��3D��3D���D�s3D�s3D�p D�l�D�c3D�` E ,�E ��E!�E��E�E�3E E� E�E�fE�fE�E  E	|�E
� E�3E��Ex Et�E��E�3E` E�fE�3E� Eq�Ep E�E��El�Ek3E�E �3E"c3E#c3E$�fE%�E'a�E(` E)�E*�E,q�E-t�E.��E/��E1��E2� E43E53E6� E7vfE8�3E:,�E;� E<[3E?��EB� EE�3EIfEK�EO6fERH EUY�EX�fE[�3E^��Ea�fEe	�Eh Ekp En��Eq� Et��Ew�fE{�E~8 E��fE� E�� E�73E�ɚE�73E�~fE���E��E�m�E���E��3E�\ E���E��3E�I�E��fE��3E�4�E�vfE��3E�3E�| E���E��E�\�E�� E�� E�Y�E��3>���?333?��?��?��?   ?   ?��?��?   ?   ?��?333?333?fff?fff?�  ?���?�ff?�  ?ٙ�@   @��@��@   @9��@L��@`  @s33@�33@���@�ff@�33@���@�33@�  @���@���@�ff@�33A   A	��AffA��A��A$��A,��A333A;33AC33AK33AS33A[33Ac33Ak33As33Ay��A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444144411414111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?fff?�ff@,��@l��@�33@�  @�  @�33A��AffA,��AH  Ah  A�33A�  A���A���A�33A�  A���A�  BffB
ffB  B��B"��B*��B2ffB:  BB  BJ  BR  BZ  BbffBk33Br��Bz��B�ffB�33B�33B�33B�  B�33B���B�ffB�33B�  B���B�  B�  B���B���B�  B�33B�ffB���B�33B�33B�33B�33B�33B�  B�  B�ffB�33B���B�33B�ffB���C ��CL�CffC� C�3C
�3C��C��C� C� C� C� C� C� C� C� C ��C"�3C$��C&� C(L�C*ffC,� C.�3C0��C2ffC4��C6� C8ffC:�3C<��C>� C@� CB� CDffCFL�CH�3CJ��CL� CN��CP�3CR��CT� CVL�CX��CZ� C\ffC^ffC`L�Cb��Cd�3Cf��ChffCj��ClffCnL�CpffCr� Ct�3Cv��CxffCz� C|�3C~��C�Y�C�33C�33C�@ C�@ C�L�C�L�C�L�C�Y�C�L�C�L�C�@ C�@ C�@ C�33C�33C�33C�33C�&fC�L�C�ffC�s3C�Y�C�ffC�Y�C�Y�C�L�C�Y�C�@ C�&fC�&fC�33C�@ C�L�C�ffC�@ C�&fC�@ C�ffC�L�C�&fC�@ C�Y�C�@ C�33C�L�C�@ C�33C�Y�C�Y�C�@ C�33C�&fC�@ C�Y�C�s3C�Y�C�33C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�@ C�&fC�33C�@ C�ffC�L�C�&fC�L�C�Y�C�@ C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�@ C�&fC�@ C�L�C�ffC�Y�C�33C�Y�C�L�C�33C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�L�C��C�&fC�&fC�33C�@ C�@ C�L�C�Y�C�ffC�s3C�s3C�L�C��C�L�C�s3C�ffC�ffC�@ C��C��C�&fC�&fC�&fC�&fD   D �fD  D� D�3D��D	3D��DL�D�3DS3DٚDY�D� Ds3D��D"s3D%  D'�fD*3D,�3D/,�D1�3D433D6��D9,�D;�3D>33D@�3DCFfDE��DH9�DJ��DMFfDO�fDRFfDT�3DW9�DY�fD\&fD^��Da3Dc� De��DhFfDj��Dm33Do�fDr3Dt� Dv�fDyY�D{Y�D}�3D�  D�VfD��3D���D���D�6fD�i�D���D��3D�  D�\�D��3D�ٚD�3D�L�D���D�ɚD� D�P D���D�� D�&fD�p D�� D��3D�9�D�vfD�� D��D�  D�\�D���D��fD�	�D�<�D�i�D��fD���D��fD�3D�@ D�l�D���D�� D�� D��fD��D�9�D�Y�D�y�D��3D���D���D�3D�<�D�\�DÆfDĦfD��3D��3D�3D�)�D�P Dˀ D̬�D�ٚD�3D�)�D�VfD҆fDӹ�D��3D�)�D�VfD؆fDٳ3D�� D�fD�)�D�VfD�y�D���D�� D��D��D�&fD�FfD�c3D� D� D�� D��3D���D��D�6fD�I�D�\�D�ffD�s3D�vfD��fD�� D��3D��3D���D��3D��3D�� D�|�D�s3D�p E 4�E ��E)�E��E!�E�3E E� E�E�fE�fE	�E E	��E
� E3E�E� E|�E��E�3Eh E�fE�3E� Ey�Ex E��E��Et�Es3E�E �3E"k3E#k3E$�fE%�E'i�E(h E)��E*��E,y�E-|�E/�E0�E1��E2� E43E53E6� E7~fE8�3E:4�E;� E<c3E?��EB� EE�3EIfEK��EO>fERP EUa�EX�fE[�3E^��Ea�fEe�Eh Ekx En��Eq� Et��Ew�fE{	�E~@ E��fE� E�� E�;3E�͚E�;3E��fE���E��E�q�E���E��3E�` E���E��3E�M�E��fE��3E�8�E�zfE��3E�3E�� E���E� �E�`�E�� E�  E�]�E��3?fffG�O�G�O�G�O�G�O�G�O�?�  G�O�G�O�G�O�?�  ?���G�O�?���G�O�?�33?�  ?���?�ff@   @��@   @,��@9��@@  @Y��@l��@�  @���@�33@���@�ff@�33@���@�33@�  @���@���@�ffA��A  A��AffA��A$��A,��A4��A;33AC33AK33AS33A[33Ac33Ak33As33A{33A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444144411414111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ �@ v@ @ �@ �@ ""@ (�@ 0x@ 5?@ <�@ FQ@ Q�@ _�@ l�@ z�@ ��@ ��@ �(@ �~@ ��@ ��@ �#@ ��@ ��@�@o@ @-@:@G�@UU@b�@p�@~�@��@��@��@�F@�>@��@ލ@�@��@�@�@"�@/�@<�@K@X�@e�@s_@��@�@�a@��@�@ƨ@�O@��@�@��@
=@B@&;@2�@A�@O�@^5@j@v@�p@��@��@�r@�k@�@�
@�@�Y@  @�@O@(�@6�@D�@SI@a�@m:@y�@��@�0@�5@�-@�w@�|@�t@�m@� @@@�@,`@9X@FQ@V�@c�@p�@�W@�P@�H@�A@��@�>@�7@�/@��@��@�@�@"�@/@>@Ji@Wb@e�@t@�@�@�U@��@��@�@��@��@�@@��@
=@�@&;@3�@B8@O0@\�@i�@ww@�@�@��@�f@�@�@׹@�@�@^@�@�@*S@7L@E�@Q�@^5@k�@z3@��@��@��@�~@��@��@܀@��@�e@	j@	o@	�@	+�@	:�@	G�@	T�@	dZ@	r@	~K@	�D@	�<@	�A@	�F@	�J@	є@	�/@	�4@	��@
�@
*@
!s@
/�@
>@
M$@
X�@
e	@
s_@
��@
�h@
��@
�M@
�@
�W@
Ӡ@
��@
��@
�E@�@�@&;@1�@@�@O0@\)@hs@ww@��@��@��@�f@�@�@�[@�`@�e@ �@�@O@)�@7�@FQ@R�@]�@k�@y�@��@�0@��@�-@��@��@�/@��@�q@^@�@!s@.l@<@G�@SI@`�@oF@|�@��@�<@�A@��@@�7@�@Z@��@�@*S@oF@��@�~@<�@��@�@�@P�@�0@�#@ �@hs@�@��@5?@x�@�@@FQ@��@��@�@X@�U@��@&;@j@��@�@3�@x&@�@��@@�@�@�>@%@I�@��@��@@R�@��@�@�@Q�@�#@׹@�@Z�@�a@��@$/@g@��@�@@0x@v@�@��@?}@��@�c@ �@ S�@ ��@ �;@!%�@!i�@!��@!�e@"7�@"z�@"��@#  @#C�@#��@#��@$J@$M�@$��@$�*@%J@%Lu@%��@%��@&�@&I�@&��@&��@'�@'C�@'�W@'��@'�9@(:�@(z3@(�^@(�q@)5�@)s_@)��@)�L@*-@*j@*��@*�@+%�@+ff@+��@+�@,&;@,e	@,�4@,�@-'�@-j@-��@-��@.-�@.m�@.�@.��@/+@/k.@/�M@/�m@0%�@0e	@0�(@0�;@1�@1Yn@1�0@1Ӡ@2@2O0@2�D@2�@3�@3?}@3z3@3��@3�4@4#�@4]�@4�0@4�|@5@5<@5m�@5��@5�t@6b@6D�@6z�@6�!@6�`@7�@7M$@7�d@7�F@7�@8""@8Wb@8�C@8�}@9�I@:�@:�M@;�@;��@<'�@<�c@=5@@=խ@>>�@>�H@?�p@?�@@b�@A
�@Aww@B�@B��@C+@C��@D7L@D�4@EI@E�F@F[z@Fƨ@Gj@G�
@H�d@H�@I�u@J@J��@K�@K��@L/�@L��@MB8@M��@NF�@N��@OoF@P  @P]�@Q�k@S{@T]�@Uȴ@W@Xi!@Y�R@[�@\�p@]�h@_�@`e�@a�@cj@dt@eƨ@gO@h\�@i�9@k�@lww@mě@oj@pX�@q�f@s�@sbN@s�@s�#@t*@tk.@t�5@t��@u6�@ur@u��@v^@v<@v�@v�@w@wT�@w��@w�H@x�@xj@x�@x�Y@y(�@yx�@y�f@ �G�O�G�O�G�O�G�O�G�O�@ jG�O�G�O�G�O�@ j@ G�O�@ �G�O�@ %@ �@ �@ �@ 
=@ �@ �@ @ b@ @ �@ �@ �@ �@ �@ 
@  @ "�@ $.@ &;@ (�@ +�@ /@ 1'@ 3�@ 6�@ :�@ <�@ ?}@ B�@ FQ@ I�@ Lu@ O�@ SI@ V�@ Z@ ]�@ `�@ dZ@ g�@ j@ n�@ qS@ t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AجAز-Aغ^A؛�A�dZA�=qA�VA���A�|�A�/A�oA�
=A�A���A��A��`A��HA�ȴA�JAՉ7A�&�Aԛ�AԁA�v�A�jA�n�A�r�A�ffA�?}A���A�`BA�A�M�A���A�`BA©�A�ZA�-A��RA�-A��A��A�VA��jA��A��wA�n�A�hsA�`BA�
=A���A�5?A�ȴA���A��PA�|�A�A�jA��A��A��A�ȴA���A�M�A�jA���A��A��hA�&�A���A�E�A��A���A�ffA�`BA��A�  A��
A��;A�G�A���A�$�A�O�A�jA�C�A���A��TA�-A�%A��!A�I�A�JA�&�A�VA�/A�M�A�p�A���A�(�A��
A��FA���A�~�A�S�A�ffA���A�$�A��TA�
=A�^5A}��A{�-Az�\Ay��Ax�/Aw�Av�AtffAr�ArM�Aq��AqC�Ap�`An$�Amt�Aln�AjVAehsAc`BAbȴAbbNAa7LA`Q�A_��A_/A^��A^E�A\��A[XAWhsAUG�AS�ARA�AQC�APbAO�^AOG�AN9XAK��AJ�/AHbAFĜAF5?AE�hAE/ADr�AB��A?G�A<��A;��A:�/A:=qA9��A9��A9A6��A4��A3O�A2ȴA2z�A1K�A/x�A-O�A,bNA+��A*ĜA)��A(r�A&��A&��A&z�A%�A%dZA$ȴA#��A"^5A!&�A7LAbNA{A|�AĜA�#AQ�A�!A^5A33A�A�A�#AK�A�HAn�AJA�FA�A%AjA�TA��A��A=qAA9XA�TAl�A
�DA	��A	XA��A�\A&�A7LA�^A v�@�ƨ@��@�@�hs@��@�&�@���@� �@���@�I�@��@�j@�w@�"�@�+@�J@�`B@�A�@���@��@��@�dZ@ް!@ޟ�@�@� �@ڰ!@�$�@ٺ^@�7L@�9X@���@ղ-@Ӿw@҇+@ѡ�@�C�@͡�@�O�@��`@�9X@���@�Q�@��@��@���@���@��#@���@��@�hs@�ƨ@�;d@�=q@�Z@��u@��@��P@��@�@�p�@�  @�l�@���@���@�&�@��w@�=q@��9@���@�33@�$�@�x�@��/@���@�ȴ@�-@�O�@��;@�K�@��#@��7@��j@�1@�dZ@�;d@�
=@��\@���@��
@��@��h@��@\)@~{@{�@z^5@y&�@w�;@v5?@t��@r=q@q&�@o;d@m/@j�!@ihs@g;d@d��@d9X@a�@a7L@_+@]��@[t�@Z�!@X�9@W�@V��@U�@S33@Qhs@Pb@M@LI�@K��@J�\@IX@G��@F��@F{@D(�@B��@@�`@@�@?\)@>ff@<��@;�F@:�@9X@7��@6�@5�-@3�m@2��@17L@.$�@-V@+�m@+t�@+C�@*n�@(Ĝ@( �@'|�@&V@%?}@$�@$9X@"^5@!�@ 1'@�;@�@��@O�@��@��@t�@��@�@�`@�;@
=@{@��@��@1@�F@C�@�H@n�@��@�7@��@�@;d@
=@@��@�m@S�@
�H@
^5@	��@��@A�@�w@|�@��@��@�h@O�@V@�@I�@ƨ@S�@��@M�@&�?���?�5??�C�?��?�1'?�l�?���?���?��`?�w?��?�^5?��#?��?���?㕁?��?�  ?ޗ�?���?�?���?���?��y?Ձ?�?}?���?��?ҏ\?�n�?�G�?�%?���?�v�?���?Ͳ-?͑h?͑h?�I�?��m?�?�=q?�x�?ȴ9?�
=?��/?��?�G�?� �?�|�?���?��D?�ƨ?��?�C�?���?���?�?���?���?��H?��H?���?���?�?�dZ?�ƨ?��m?�j?��?�V?�/?�O�?�p�?��-?��-?��?�{?�5??�5??�v�?���?���?��R?��?��?��?�;d?�\)?�|�?��w?��w?��w?��;?�  AجAخAذ!AجAجAجAخAخAذ!Aز-AخAز-Aز-Aز-Aش9AؼjAش9Aغ^AؼjA���AؾwA���AؾwAظRAذ!Aإ�Aؙ�AؓuA�p�A�bNA�\)A�G�A�;dA�33A�&�A�%A�  A��#A׼jAבhA�hsA�G�A�(�A��A�{A�{A�bA�VA�JA�
=A�%A�A�A�  A���A���A���A���A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        AجAز-Aغ^A؛�A�dZA�=qA�VA���A�|�A�/A�oA�
=A�A���A��A��`A��HA�ȴA�JAՉ7A�&�Aԛ�AԁA�v�A�jA�n�A�r�A�ffA�?}A���A�`BA�A�M�A���A�`BA©�A�ZA�-A��RA�-A��A��A�VA��jA��A��wA�n�A�hsA�`BA�
=A���A�5?A�ȴA���A��PA�|�A�A�jA��A��A��A�ȴA���A�M�A�jA���A��A��hA�&�A���A�E�A��A���A�ffA�`BA��A�  A��
A��;A�G�A���A�$�A�O�A�jA�C�A���A��TA�-A�%A��!A�I�A�JA�&�A�VA�/A�M�A�p�A���A�(�A��
A��FA���A�~�A�S�A�ffA���A�$�A��TA�
=A�^5A}��A{�-Az�\Ay��Ax�/Aw�Av�AtffAr�ArM�Aq��AqC�Ap�`An$�Amt�Aln�AjVAehsAc`BAbȴAbbNAa7LA`Q�A_��A_/A^��A^E�A\��A[XAWhsAUG�AS�ARA�AQC�APbAO�^AOG�AN9XAK��AJ�/AHbAFĜAF5?AE�hAE/ADr�AB��A?G�A<��A;��A:�/A:=qA9��A9��A9A6��A4��A3O�A2ȴA2z�A1K�A/x�A-O�A,bNA+��A*ĜA)��A(r�A&��A&��A&z�A%�A%dZA$ȴA#��A"^5A!&�A7LAbNA{A|�AĜA�#AQ�A�!A^5A33A�A�A�#AK�A�HAn�AJA�FA�A%AjA�TA��A��A=qAA9XA�TAl�A
�DA	��A	XA��A�\A&�A7LA�^A v�@�ƨ@��@�@�hs@��@�&�@���@� �@���@�I�@��@�j@�w@�"�@�+@�J@�`B@�A�@���@��@��@�dZ@ް!@ޟ�@�@� �@ڰ!@�$�@ٺ^@�7L@�9X@���@ղ-@Ӿw@҇+@ѡ�@�C�@͡�@�O�@��`@�9X@���@�Q�@��@��@���@���@��#@���@��@�hs@�ƨ@�;d@�=q@�Z@��u@��@��P@��@�@�p�@�  @�l�@���@���@�&�@��w@�=q@��9@���@�33@�$�@�x�@��/@���@�ȴ@�-@�O�@��;@�K�@��#@��7@��j@�1@�dZ@�;d@�
=@��\@���@��
@��@��h@��@\)@~{@{�@z^5@y&�@w�;@v5?@t��@r=q@q&�@o;d@m/@j�!@ihs@g;d@d��@d9X@a�@a7L@_+@]��@[t�@Z�!@X�9@W�@V��@U�@S33@Qhs@Pb@M@LI�@K��@J�\@IX@G��@F��@F{@D(�@B��@@�`@@�@?\)@>ff@<��@;�F@:�@9X@7��@6�@5�-@3�m@2��@17L@.$�@-V@+�m@+t�@+C�@*n�@(Ĝ@( �@'|�@&V@%?}@$�@$9X@"^5@!�@ 1'@�;@�@��@O�@��@��@t�@��@�@�`@�;@
=@{@��@��@1@�F@C�@�H@n�@��@�7@��@�@;d@
=@@��@�m@S�@
�H@
^5@	��@��@A�@�w@|�@��@��@�h@O�@V@�@I�@ƨ@S�@��@M�@&�?���?�5??�C�?��?�1'?�l�?���?���?��`?�w?��?�^5?��#?��?���?㕁?��?�  ?ޗ�?���?�?���?���?��y?Ձ?�?}?���?��?ҏ\?�n�?�G�?�%?���?�v�?���?Ͳ-?͑h?͑h?�I�?��m?�?�=q?�x�?ȴ9?�
=?��/?��?�G�?� �?�|�?���?��D?�ƨ?��?�C�?���?���?�?���?���?��H?��H?���?���?�?�dZ?�ƨ?��m?�j?��?�V?�/?�O�?�p�?��-?��-?��?�{?�5??�5??�v�?���?���?��R?��?��?��?�;d?�\)?�|�?��w?��w?��w?��;?�  AجAخAذ!AجAجAجAخAخAذ!Aز-AخAز-Aز-Aز-Aش9AؼjAش9Aغ^AؼjA���AؾwA���AؾwAظRAذ!Aإ�Aؙ�AؓuA�p�A�bNA�\)A�G�A�;dA�33A�&�A�%A�  A��#A׼jAבhA�hsA�G�A�(�A��A�{A�{A�bA�VA�JA�
=A�%A�A�A�  A���A���A���A���A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BM�BM�BL�BJ�BJ�BK�BI�BH�BI�BK�BM�BL�BK�BL�BK�BK�BM�BcTB��B0!B,B,B0!B49B7LB<jBD�BG�BC�BE�BM�BZBT�BgmBs�Bo�Bu�Bq�B~�B�+B�+B�+B�=B�PB�7B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�1B�Bz�B\)BQ�BJ�BE�BB�B?}B%�B{BB��B�B�TB�5B�B��B��B�1B�1BC�B�BhBVBBB
��B
�B
�B
��B
��B
�RB
�'B
�B
��B
��B
��B
�B
��B
�LB
��B
��B
��B
�oB
�B
n�B
hsB
bNB
\)B
VB
O�B
G�B
6FB
,B
%�B
�B
�B
�B
%B
B	��B	�B	��B	��B	ǮB	ÖB	�jB	�9B	�'B	�'B	�B	��B	��B	��B	�B	x�B	p�B	gmB	_;B	[#B	W
B	T�B	K�B	C�B	;dB	.B	,B	'�B	#�B	!�B	�B	{B	B��B��B�B�B�B�B�ZB��B��BƨBĜB��B�LB��B��B��B��B�hB�DB�B�B�B~�B|�By�Bw�Bs�Bn�BiyBe`BcTBbNB^5B]/B[#BYB]/B\)BZBW
BXBW
BVBT�BR�BR�BR�BR�BQ�BO�BP�BO�BN�BJ�BJ�BH�BI�BF�BD�BD�BC�B>wB?}B<jB:^B<jB;dB:^B9XB8RB8RB6FB5?B8RB2-B5?B33B0!B1'B1'B0!B0!B0!B0!B1'B/B;dB;dB:^B;dB<jB=qB?}BB�BB�BB�BC�BD�BG�BG�BK�BM�BM�BQ�BW
BW
BW
BW
Bq�Bs�Br�Bu�B�B�1B��B��B�B�B�RB�qB��B�B��B	�B	/B	49B	G�B	K�B	[#B	cTB	l�B	o�B	� B	�=B	��B	�B	�FB	�^B	��B	ĜB	ǮB	��B	�
B	�B	�5B	�BB	�;B	�ZB	�`B	�sB	�yB	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
%B
+B
DB
JB
VB
bB
{B
{B
�B
�B
�B
�B
�B
�B
�B
!�B
%�B
&�B
'�B
)�B
)�B
-B
,B
.B
-B
0!B
2-B
33B
5?B
6FB
6FB
7LB
8RB
:^B
;dB
<jB
>wB
>wB
@�B
@�B
B�B
C�B
D�B
F�B
F�B
G�B
I�B
I�B
J�B
L�B
N�B
O�B
S�B
S�B
S�B
T�B
S�B
VB
XB
XB
YB
ZB
[#B
\)B
\)B
^5B
_;B
`BB
`BB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
ffB
gmB
hsB
iyB
iyB
k�B
jB
k�B
k�B
l�B
l�B
l�B
m�B
o�B
o�B
p�B
p�B
q�B
r�B
s�B
t�B
s�B
t�B
v�B
v�B
w�B
w�B
x�B
y�B
{�B
z�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�%B
�%B
�1B
�DB
�DB
�JB
�PB
�bB
�bB
�hB
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
�B
�B
�B
�B
�'B
�'B
�-B
�9B
�?B
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�jB
�jB
�dB
�dB
�dB
�dB
�dB
�^B
�jB
�dB
�dB
�jB
�jB
�dB
�jB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�jB
�jBM�BM�BM�BN�BN�BM�BB�BN�BN�BM�BN�BM�BM�BM�BN�BL�BN�BN�BM�BM�BM�BL�BL�BL�BJ�BL�BL�BF�BI�BL�BJ�BJ�BJ�BL�BK�BI�BH�BE�BJ�BI�BI�BK�BK�BL�BM�BL�BM�BL�BL�BL�BL�BK�BK�BK�BK�BL�BL�BK�BK�BL�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        BK�BK�BJ�BH�BH�BI�BG�BF�BG�BI�BK�BJ�BI�BJ�BI�BI�BK�BaHB��B.B)�B)�B.B2-B5?B:^BB�BE�BA�BC�BK�BXBR�Be`Bq�Bm�Bs�Bo�B|�B�B�B�B�1B�DB�+B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�DB�%B�Bx�BZBO�BH�BC�B@�B=qB#�BoBB��B�B�HB�)B��BȴB��B�%B�%BA�B�B\BJBB
��B
��B
�B
�
B
��B
�}B
�FB
�B
��B
��B
��B
��B
��B
��B
�?B
��B
��B
��B
�bB
�B
l�B
ffB
`BB
ZB
S�B
M�B
E�B
49B
)�B
#�B
�B
�B
{B
B
B	��B	�B	��B	��B	ŢB	��B	�^B	�-B	�B	�B	�B	��B	��B	�{B	�B	v�B	n�B	e`B	]/B	YB	T�B	R�B	I�B	A�B	9XB	,B	)�B	%�B	!�B	�B	�B	oB	  B��B��B�B�B�B�sB�NB��B��BĜBB�}B�?B��B��B��B��B�\B�7B�B�B~�B|�Bz�Bw�Bu�Bq�Bl�BgmBcTBaHB`BB\)B[#BYBW
B[#BZBXBT�BVBT�BS�BR�BP�BP�BP�BP�BO�BM�BN�BM�BL�BH�BH�BF�BG�BD�BB�BB�BA�B<jB=qB:^B8RB:^B9XB8RB7LB6FB6FB49B33B6FB0!B33B1'B.B/B/B.B.B.B.B/B-B9XB9XB8RB9XB:^B;dB=qB@�B@�B@�BA�BB�BE�BE�BI�BK�BK�BO�BT�BT�BT�BT�Bo�Bq�Bp�Bs�B~�B�%B��B��B��B�B�FB�dB��B�B��B	�B	-B	2-B	E�B	I�B	YB	aHB	jB	m�B	}�B	�1B	�{B	��B	�9B	�RB	�wB	B	ƨB	��B	�B	�B	�/B	�;B	�5B	�TB	�ZB	�mB	�sB	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
%B

=B
DB
PB
\B
uB
uB
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
%�B
&�B
(�B
(�B
,B
+B
-B
,B
/B
1'B
2-B
49B
5?B
5?B
6FB
7LB
9XB
:^B
;dB
=qB
=qB
?}B
?}B
A�B
B�B
C�B
E�B
E�B
F�B
H�B
H�B
I�B
K�B
M�B
N�B
R�B
R�B
R�B
S�B
R�B
T�B
W
B
W
B
XB
YB
ZB
[#B
[#B
]/B
^5B
_;B
_;B
`BB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
ffB
gmB
hsB
iyB
iyB
k�B
jB
k�B
k�B
l�B
l�B
l�B
m�B
o�B
o�B
p�B
p�B
q�B
r�B
s�B
t�B
s�B
t�B
v�B
v�B
w�B
w�B
x�B
y�B
{�B
z�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�%B
�%B
�1B
�DB
�DB
�JB
�PB
�bB
�bB
�hB
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
�B
�B
�B
�!B
�-B
�-B
�3B
�?B
�FB
�RB
�RB
�XB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�}B
�}B
�wB
�wB
�wB
�wB
�wB
�qB
�}B
�wB
�wB
�}B
�}B
�wB
�}B
�wB
�}B
�wB
�}B
�wB
�}B
�}B
�}B
�}BK�BK�BK�BL�BL�BK�B@�BL�BL�BK�BL�BK�BK�BK�BL�BJ�BL�BL�BK�BK�BK�BJ�BJ�BJ�BH�BJ�BJ�BD�BG�BJ�BH�BH�BH�BJ�BI�BG�BF�BC�BH�BG�BG�BI�BI�BJ�BK�BJ�BK�BJ�BJ�BJ�BJ�BI�BI�BI�BI�BJ�BJ�BI�BI�BJ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810250007352021061413524720210614135247202106141746492021061417464920210614174649201810250007352021061413524720210614135247202106141746492021061417464920210614174649PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018102500073520181025000735  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102500073520181025000735QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102500073520181025000735QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015420210722160154IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                