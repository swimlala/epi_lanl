CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-13T17:03:59Z creation      
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
_FillValue                 @  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Q   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  b   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  fP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  wH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  è   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                       HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   <   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180813170359  20210722160151  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�v�~�5@�v�~�511  @�v�}'ؐ@�v�}'ؐ@6}��gw@6}��gw�c��҈�p�c��҈�p11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?L��@ff@L��@�  @�  @�ff@�ffA��A33A&ffAD��Aa��A���A�  A�  A�  A���Aљ�A���A���B ffB  BffBffB   B'��B/��B8ffB@ffBH  BPffBX��Ba33Bh��BpffBw��B��B���B�33B�ffB�33B�  B�33B�ffB�  B�ffB�33B�33B�33B�  B�  B�33B�  B�  B�  B�33B�  B�33B���B���B���B�ffB�33B���B�ffB�ffB�33B�  B���C  C33C33C  C
33CL�C�C��C�fC  C�CL�CffC�C��C��C!��C#��C&33C(�C)��C+��C-�fC/�fC2  C4�C6L�C8�C9�fC<�C>L�C@�CB  CDL�CF33CH�CJ�CL  CN  CP  CQ�fCT  CVL�CXL�CZ33C\33C]��C_�fCa�fCd  Cf�Ch�Cj33ClL�Cn33Cp�Cr  Cs�fCu�fCw��Cz�C|  C}�fC��C��C��C�  C�  C�&fC��C��fC�  C��C��C��C�&fC��C��fC��3C�  C�  C��C��C�&fC�&fC��C��3C��C�  C��3C��C��C��3C��C��C��3C��C��C��3C��C��C�  C��C��C��3C��C��C��C��3C�  C��C��C��3C��C��C�&fC��C�ٚC��fC��fC��fC��fC��fC�  C�  C�  C�  C��C��C��C��C�  C�  C��C��C��C��C�  C��fC��3C�  C��C��C��C�&fC�&fC��C��3C�  C��C�&fC��C��3C��C�  C��fC��3C��C�&fC��C��3C��C�  C��3C��C��C��3C��C��C��C�  C��C�  C��3C��C��C��C��C��3C��C��C�  C��3C��3C�  C�  C��C��3C��C�@ C��C���D � DfDl�D  D�fD�fD
l�D,�D�fDY�D�3DY�D�3D,�D�fD �fD#L�D%��D(fD*` D,��D/33D1��D4,�D6�3D9L�D;�3D>�3DA@ DD�DF�fDI� DLY�DO&fDQ��DT��DWl�DZ�D\� D_` Da��Dd� Dg�Di��Dl,�Dn� Dq�Dsl�Du�3DxFfDz� D|� DfD��3D���D�fD�33D�` D���D�� D��fD� D�<�D�vfD��3D��D�  D�` D��fD��fD�#3D�` D���D��3D� D�I�D�� D��3D��D�  D�Y�D�� D�� D��3D�#3D�S3D��fD���D���D��D�L�D���D���D��fD�3D�9�D�i�D���D��3D��3D�3D�33D�\�D��3D�� D���D���D��fD�fD�,�D�FfD�ffDƉ�DǦfD��fD��D��D�,�D�I�D�` D�s3DЌ�DѦfD��3D�ٚD���D�3D�33D�P D�i�Dڐ D۰ D��3D��3D�fD�<�D�Y�D�y�D㙚D乚D��3D��fD�3D�6fD�` D�3D�3D���D�� D�  D�3D�&fD�C3D�` D�vfD���D��3D��fD�� D��D��D�#3D�I�D�ffE <�E �3EY�E�Ep E�fE��EfE�3E)�E�fE8 EK3EVfE	�fE
�fEl�Ek3E�3E�fE` E\�E� E��ET�EP E��E�fE4�E�fE��E  E��E!ffE"�fE#�3E%0 E&�3E'��E(� E*T�E+I�E,�3E.3E/  E0ffE1��E2� E4)�E5��E6��E7�fE9` E:K3E;��E>�EA�3ED� EHH EK\�ENd�EQq�ETvfEW�3EZ�3E]ٚEa8 Ed;3Eg@ Ej��Em��Ep��Es� Ew Ez�E|��E�:fE�� E�9�E���E�p E��3E�w3E� E���E�?3E���E�ZfE�� E��3E�5�E��3E��3E�*fE�zfE�͚E�$�E�]�E���E�3E�L�E��fE� �E�=�E�� E���E�33E���E�� E�)�E��3E��3E�*fE�jfE��3E��E�T ?��>���>���>���>���?   ?   ?   >���>���>���?��?   ?   ?��?��?L��?fff?���?���?�33?���?�ff@   @��@   @9��@Fff@`  @l��@�  @���@�33@���@�33@�  @���@�ff@�  @���@���@�ffA   A  A  A��A��A!��A)��A0  A8  A>ffAC33AK33AP  AX  A^ffAd��Ak33At��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414444414414111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?fff?�ff@&ff@l��@�  @�  @�ff@�ffA	��A33A.ffAL��Ai��A���A�  A�  A�  A���Aՙ�A���A���BffB
  BffBffB"  B)��B1��B:ffBBffBJ  BRffBZ��Bc33Bj��BrffBy��B���B���B�33B�ffB�33B�  B�33B�ffB�  B�ffB�33B�33B�33B�  B�  B�33B�  B�  B�  B�33B�  B�33B���B���B���B�ffB�33B���B�ffB�ffB�33B�  C L�C� C�3C�3C� C
�3C��C��CL�CffC� C��C��C�fC��CL�C L�C"L�C$L�C&�3C(��C*L�C,L�C.ffC0ffC2� C4��C6��C8��C:ffC<��C>��C@��CB� CD��CF�3CH��CJ��CL� CN� CP� CRffCT� CV��CX��CZ�3C\�3C^L�C`ffCbffCd� Cf��Ch��Cj�3Cl��Cn�3Cp��Cr� CtffCvffCxL�Cz��C|� C~ffC�L�C�Y�C�L�C�@ C�@ C�ffC�L�C�&fC�@ C�L�C�Y�C�Y�C�ffC�L�C�&fC�33C�@ C�@ C�Y�C�Y�C�ffC�ffC�Y�C�33C�Y�C�@ C�33C�Y�C�L�C�33C�Y�C�L�C�33C�Y�C�L�C�33C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�@ C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C��C�&fC�&fC�&fC�&fC�&fC�@ C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�L�C�Y�C�Y�C�@ C�&fC�33C�@ C�L�C�L�C�Y�C�ffC�ffC�L�C�33C�@ C�L�C�ffC�L�C�33C�L�C�@ C�&fC�33C�Y�C�ffC�L�C�33C�L�C�@ C�33C�L�C�L�C�33C�L�C�Y�C�L�C�@ C�Y�C�@ C�33C�Y�C�Y�C�L�C�L�C�33C�Y�C�L�C�@ C�33C�33C�@ C�@ C�Y�C�33C�Y�C�� C�L�D fD � D&fD��D  D�fD�fD
��DL�D�fDy�D�3Dy�D�3DL�D�fD!fD#l�D%��D(&fD*� D,��D/S3D1��D4L�D6�3D9l�D<3D>�3DA` DD,�DF�fDI� DLy�DOFfDR�DTٚDW��DZ9�D\� D_� Db�Dd� Dg9�Di��DlL�Dn� Dq,�Ds��Du�3DxffDz� D|� D&fD��3D���D�fD�C3D�p D���D�� D��fD�  D�L�D��fD��3D���D�0 D�p D��fD��fD�33D�p D���D��3D�  D�Y�D�� D��3D���D�0 D�i�D�� D�� D�3D�33D�c3D��fD�ɚD���D�,�D�\�D���D���D��fD�#3D�I�D�y�D���D��3D�3D�#3D�C3D�l�D��3D�� D���D���D�fD�&fD�<�D�VfD�vfDƙ�DǶfD��fD���D��D�<�D�Y�D�p Dσ3DМ�DѶfD��3D��D�	�D�#3D�C3D�` D�y�Dڠ D�� D��3D�3D�&fD�L�D�i�D≚D㩚D�ɚD��3D�fD�#3D�FfD�p D�3D�3D���D�� D� D�#3D�6fD�S3D�p D��fD���D��3D��fD�� D���D��D�33D�Y�D�vfE D�E �3Ea�E�Ex EfE��EfE�3E1�E�fE@ ES3E^fE	�fE
�fEt�Es3E�3E�fEh Ed�E� E��E\�EX E��E�fE<�E�fE��E E �E!nfE"�fE#�3E%8 E&�3E'��E(� E*\�E+Q�E,�3E.3E/ E0nfE1��E2� E41�E5��E6��E7�fE9h E:S3E;��E>��EA�3ED� EHP EKd�ENl�EQy�ET~fEW�3EZ�3E]�Ea@ EdC3EgH Ej��Em��Ep��Es� Ew  Ez�E}�E�>fE�� E�=�E���E�t E��3E�{3E� E���E�C3E���E�^fE�� E��3E�9�E��3E��3E�.fE�~fE�њE�(�E�a�E���E�3E�P�E��fE��E�A�E�� E���E�73E���E�� E�-�E��3E��3E�.fE�nfE��3E��E�X G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�?�  G�O�?���?�ff?�33?���?ٙ�?�33@ff@33@   @9��@@  @Y��@fff@�  @�ff@�  @���@�33@���@�33@�  @���@�ff@�  @���@���A33A  A  A  A��A$��A)��A1��A8  A@  AFffAK33AS33AX  A`  AfffAl��As33A|��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414444414414111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ �@ v@ V@ �@ O@ ""@ *S@ 1'@ 7L@ >�@ F�@ S�@ `B@ m�@ z�@ ��@ �0@ ��@ ��@ ��@ �|@ �#@ �@ �q@@@
@+�@:�@H]@UU@c�@r@�W@�P@�H@��@�9@��@��@�;@�4@�,@�@�@""@1'@>@K�@Yn@ff@t@�d@�\@�@��@�@��@�O@��@�@@��@�@�@$�@4�@B8@O0@\)@hs@ww@�|@�#@�m@�r@��@�@խ@�@�Y@ �@�@
@)�@5?@B�@P�@^5@n�@{�@�+@��@�(@��@�&@�|@܀@��@�@@@g@,`@<@I@V@c�@p�@~K@��@��@�A@��@Ĝ@є@�;@�(@�~@%@{@"�@0x@>�@M$@Z@g@t@�@��@��@�Y@�R@�J@�O@�@�@��@
=@�@&;@1�@@�@O0@]�@k.@y�@��@�h@��@�@��@��@�h@�@�e@^@�@�@(�@5�@E�@R�@^�@n�@{�@��@��@��@��@��@�|@�t@�y@�q@	�@	�@	 @	-@	9X@	G�@	V�@	c�@	o�@	~�@	�P@	��@	��@	��@	�2@	��@	܀@	�(@	��@
�@
{@
""@
/�@
>@
K�@
Yn@
g@
t@
��@
�@
��@
�@
��@
��@
�C@
��@
��@
�E@
�@B@'�@5?@A�@M�@\)@j@y�@��@�@�@�@�^@ȴ@�h@�@�@�Q@V@O@(G@7L@D�@Q=@`B@n�@{�@��@��@��@��@��@�*@�#@��@�@�@�@�@+�@9X@G�@UU@dZ@o�@�@�\@�H@��@��@�>@�*@��@�4@{�@��@@V@�U@��@$�@hs@��@��@)�@k.@�@�4@,`@n�@�!@�@7�@|�@��@J@S�@�@�y@3�@~K@�@�@e�@��@��@C�@��@Ӡ@B@_�@��@��@1'@t@�F@� @8�@{�@��@�Y@3�@t�@�9@�@3�@t@�9@��@4�@t@�9@� @:�@|�@�&@j@I@�P@��@ {@ X@ �H@ ��@! �@!b�@!��@!�@"(�@"k�@"�@"��@#0x@#qS@#�-@#�@$5@@$v�@$��@$�~@%<@%}�@%�@%�E@&<@&|�@&�w@&�E@'>@'{�@'�@'�~@(7L@(t@(��@(�@@)*S@)g�@)�(@)�;@*�@*Z�@*��@*��@+@+Q=@+��@+�o@,�@,A�@,}�@,��@,�q@-1�@-oF@-�Y@-��@.%�@.a�@.�m@.��@/�@/Yn@/��@/�\@0@0P�@0��@0�o@1�@1E�@1�d@1��@2  @2>@2{�@2��@2��@333@3m�@3��@3�`@4""@4]�@4�<@4�O@5o@5N�@5�+@5�J@6 �@6?}@6|?@6��@6�@70x@7m�@7�A@7�@8g@8[z@8��@8��@9�@9DD@9��@:+�@:��@;C�@;�(@<V�@<� @=bN@>�@>r�@?{@?�W@@$/@@�\@A1�@A�T@B8�@B��@C:�@C�
@DA�@D�/@Ez2@E�@Fz�@G�@G{�@H{@H��@I*@I�@JE�@J��@KC�@K܀@LDD@Lލ@My�@M��@N}�@O�@O|?@P�@Qx&@R�4@S��@Ut@VĜ@X�@Y\�@Z��@\�@]dZ@^��@`�@a`�@b��@d[@ea�@f�@g�,@im:@j��@k��@mk�@n��@o��@q\�@r�R@s�@uM�@v�@x@ybN@z��@|�@|@�@|v�@|��@}�@}Q=@}��@}�@~ @~j@~��@~�`@2�@g@��@� W@�I@�@�@�hs@��@���@��D@��4@��@�1'@�Y�@�u@���@���@�ؿG�O�G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�@ jG�O�@ @ v@ %@ �@ 1@ 	�@ 
�@ J@ �@ b@ @ �@ *@ �@ B@ O@ [@ g@  �@ "�@ %�@ (G@ *S@ ,`@ /@ 2�@ 4�@ 6�@ :@ =q@ ?}@ B�@ D�@ H]@ K@ N�@ Q=@ SI@ V�@ X�@ \)@ ^�@ a�@ dZ@ hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�%A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�AҍPA���Aϲ-A�VA΁A�VAͶFA��A�33A���A��A��A� �A��A���A�`BA���A�7LA��jA�ZA�K�A���A���A�hsA���A�(�A�;dA��HA��A��A���A���A�C�A�ffA�`BA��A���A�r�A�C�A��^A�A��jA�ffA�-A�(�A���A�n�A�bA��9A�(�A�S�A�(�A���A�C�A���A��wA�n�A���A�ZA�VA�=qA���A�`BA���A���A�G�A��\A��9A�A�A�A���A�C�A���A�33A�M�A��A��9A�=qA�ĜA�Q�A��HA��`A���A�?}A�t�A��A�I�A��A��
A��!A��uA�v�A���A���A�?}A��A��HA��A��A��A��-A}�A|Q�Ay��AuVAs��Asx�Apv�An1Am�PAl�Al^5Al1Ak��Aj �Ah^5Ag�Af-Ab�/A_��A[�FAZ�!AY`BAW�^AU��AS�;ASK�AR^5AO��ANA�AMp�ALM�AK�AI�7AG�PAF^5AE|�ADVAB�AA��A@r�A?K�A>�A=;dA<r�A;��A;oA:�+A9O�A7�A5x�A57LA4JA1��A0��A0~�A/��A/;dA.�A.��A.n�A-�A,�`A+�;A*�+A)oA'VA&jA%��A%t�A$�yA#�A"�RA!�
A ȴA $�A
=AjA+A{A�wAr�A+A��A�
A33A�A�9A�A�
AA�wA�!A1'AG�AbAoA
�A	�hA	33AȴA�A��A33A�A  AA��A��Al�AoA �+A   @��^@�1@��u@�  @���@�&�@��j@�F@�;d@��@���@��@�@���@���@�hs@�?}@�@�
=@�\@�=q@���@�t�@�V@��@�&�@㝲@��u@�~�@܃@ۥ�@ёh@�bN@���@�+@�b@�o@�1@�l�@�K�@�p�@��@��@��@�/@�ƨ@�$�@�ƨ@��@�z�@���@���@�?}@��9@��F@�+@�@���@�z�@�;d@���@��j@�(�@�ff@�z�@���@��7@��m@�+@���@��m@��@�1@�v�@�@��y@�$�@��\@��\@���@���@�`B@� �@~$�@}O�@{�m@y��@v�+@u��@t�@r�@q�7@o��@n@k��@j=q@g�P@d�@b�@`��@^�+@^{@[�m@Y�@Y��@X�@U��@TI�@R�\@Q��@P�@O�@M�T@L�@L9X@K"�@I7L@G|�@E��@E`B@D��@C33@Ax�@@�9@>��@=��@;C�@9��@9G�@8 �@6�+@6{@5O�@4Z@2�H@2~�@0��@/�@/�P@/;d@.E�@-O�@,�@+o@*�!@*=q@(��@( �@'+@&{@$�j@$I�@#"�@!��@!hs@ ��@|�@
=@$�@�T@1@33@��@n�@=q@�@�9@��@
=@��@�+@E�@�@�D@9X@"�@��@=q@&�@�9@��@�y@�T@�-@��@�@�@
~�@	�#@	7L@�u@\)@ȴ@�+@$�@{@?}@j@��@��@��@x�@ ��?���?��D?�C�?�b?�?��?��;?�p�?�I�?�?�9?�1'?�P?�j?㕁?�J?��`?�A�?�v�?�5??�p�?܋D?���?�X?�Q�?և+?��?��?ҏ\?�hs?��`?ϝ�?�|�?��?͑h?̬?�1?��m?��H?�=q?��#?���?�K�?�ff?�z�?��?��?���?���?��-?�V?�j?��?�ƨ?���?�?��H?���?�~�?�~�?���?�~�?���?��H?�?�dZ?���?�j?��?��h?�V?���?��w?�bN?�%?�&�?�G�?�G�?��7?���?���?��?��?�-?�-?�M�?�n�?\?°!?��?�o?�33?�S�?�t�?��
?öF?��?�9X?�Z?�z�?ě�?ě�?��/?��/A���A�A���A���A���A�  A�A���A���A���A���A���A��A��A��A���A���A���A���A�%A�1A�%A�A���A�JA�VA��A��A��A��A��A�{A��A��A��A��A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     A���A���A�%A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�AҍPA���Aϲ-A�VA΁A�VAͶFA��A�33A���A��A��A� �A��A���A�`BA���A�7LA��jA�ZA�K�A���A���A�hsA���A�(�A�;dA��HA��A��A���A���A�C�A�ffA�`BA��A���A�r�A�C�A��^A�A��jA�ffA�-A�(�A���A�n�A�bA��9A�(�A�S�A�(�A���A�C�A���A��wA�n�A���A�ZA�VA�=qA���A�`BA���A���A�G�A��\A��9A�A�A�A���A�C�A���A�33A�M�A��A��9A�=qA�ĜA�Q�A��HA��`A���A�?}A�t�A��A�I�A��A��
A��!A��uA�v�A���A���A�?}A��A��HA��A��A��A��-A}�A|Q�Ay��AuVAs��Asx�Apv�An1Am�PAl�Al^5Al1Ak��Aj �Ah^5Ag�Af-Ab�/A_��A[�FAZ�!AY`BAW�^AU��AS�;ASK�AR^5AO��ANA�AMp�ALM�AK�AI�7AG�PAF^5AE|�ADVAB�AA��A@r�A?K�A>�A=;dA<r�A;��A;oA:�+A9O�A7�A5x�A57LA4JA1��A0��A0~�A/��A/;dA.�A.��A.n�A-�A,�`A+�;A*�+A)oA'VA&jA%��A%t�A$�yA#�A"�RA!�
A ȴA $�A
=AjA+A{A�wAr�A+A��A�
A33A�A�9A�A�
AA�wA�!A1'AG�AbAoA
�A	�hA	33AȴA�A��A33A�A  AA��A��Al�AoA �+A   @��^@�1@��u@�  @���@�&�@��j@�F@�;d@��@���@��@�@���@���@�hs@�?}@�@�
=@�\@�=q@���@�t�@�V@��@�&�@㝲@��u@�~�@܃@ۥ�@ёh@�bN@���@�+@�b@�o@�1@�l�@�K�@�p�@��@��@��@�/@�ƨ@�$�@�ƨ@��@�z�@���@���@�?}@��9@��F@�+@�@���@�z�@�;d@���@��j@�(�@�ff@�z�@���@��7@��m@�+@���@��m@��@�1@�v�@�@��y@�$�@��\@��\@���@���@�`B@� �@~$�@}O�@{�m@y��@v�+@u��@t�@r�@q�7@o��@n@k��@j=q@g�P@d�@b�@`��@^�+@^{@[�m@Y�@Y��@X�@U��@TI�@R�\@Q��@P�@O�@M�T@L�@L9X@K"�@I7L@G|�@E��@E`B@D��@C33@Ax�@@�9@>��@=��@;C�@9��@9G�@8 �@6�+@6{@5O�@4Z@2�H@2~�@0��@/�@/�P@/;d@.E�@-O�@,�@+o@*�!@*=q@(��@( �@'+@&{@$�j@$I�@#"�@!��@!hs@ ��@|�@
=@$�@�T@1@33@��@n�@=q@�@�9@��@
=@��@�+@E�@�@�D@9X@"�@��@=q@&�@�9@��@�y@�T@�-@��@�@�@
~�@	�#@	7L@�u@\)@ȴ@�+@$�@{@?}@j@��@��@��@x�@ ��?���?��D?�C�?�b?�?��?��;?�p�?�I�?�?�9?�1'?�P?�j?㕁?�J?��`?�A�?�v�?�5??�p�?܋D?���?�X?�Q�?և+?��?��?ҏ\?�hs?��`?ϝ�?�|�?��?͑h?̬?�1?��m?��H?�=q?��#?���?�K�?�ff?�z�?��?��?���?���?��-?�V?�j?��?�ƨ?���?�?��H?���?�~�?�~�?���?�~�?���?��H?�?�dZ?���?�j?��?��h?�V?���?��w?�bN?�%?�&�?�G�?�G�?��7?���?���?��?��?�-?�-?�M�?�n�?\?°!?��?�o?�33?�S�?�t�?��
?öF?��?�9X?�Z?�z�?ě�?ě�?��/?��/A���A�A���A���A���A�  A�A���A���A���A���A���A��A��A��A���A���A���A���A�%A�1A�%A�A���A�JA�VA��A��A��A��A��A�{A��A��A��A��A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BN�BN�BM�BL�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BN�BM�BO�BO�BP�BR�BW
Bn�B��B�B49BA�BA�BA�B7LB0!B/B,B33BB�BYBW
B^5Bo�BgmBt�Bp�Bx�Bw�Bx�Bv�B|�B}�B�B�B{�B�B�+B�1B�%B�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�DB�7B�B�B�B�Bu�BdZBYBN�B@�BuB+B��B�;B��B��B��BB��B�wB��B��B��BƨBB��B��B��B�DBe`BL�BA�B;dB&�B�B%B
��B
�B
�`B
�HB
�)B
��B
�XB
�B
��B
�7B
q�B
l�B
M�B
=qB
49B
0!B
�B
oB
PB
+B
B	��B	��B	�B	�mB	�NB	�B	ÖB	�!B	��B	��B	�uB	�DB	}�B	x�B	s�B	iyB	`BB	W
B	R�B	J�B	B�B	8RB	)�B	$�B	�B	�B	\B	1B��B��B��B�B�B�yB�mB�TB�#B�
B��B��B��BĜB��B�qB�dB�RB�RB�LB�3B�!B�'B�B��B��B��B�{B�hB�VB�DB�1B�B�B{�By�Bv�Bt�Bn�Bl�BjBcTBaHB^5B^5BaHB\)B[#BXBW
BQ�BO�BQ�BQ�BN�BM�BK�BH�BI�BH�BF�BG�BE�BF�BE�BC�B@�B:^B49B5?B33B0!B.B,B'�B(�B&�B$�B$�B$�B#�B#�B!�B$�B'�B)�B0!B.B.B,B,B0!B0!B2-B.B1'B2-B33B49B2-B/B2-B49B33BG�B^5B^5BYBR�B\)BgmB�{B��B�jBƨB�B��B��B	�B	&�B	;dB	B�B	O�B	H�B	K�B	T�B	bNB	r�B	v�B	�B	�B	�VB	��B	��B	��B	�B	�XB	ÖB	ŢB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
	7B
	7B
	7B

=B
JB
VB
bB
uB
{B
�B
�B
�B
�B
 �B
!�B
#�B
%�B
%�B
&�B
(�B
,B
-B
.B
/B
0!B
1'B
33B
49B
49B
7LB
8RB
:^B
:^B
;dB
=qB
?}B
>wB
A�B
B�B
E�B
E�B
E�B
F�B
H�B
I�B
I�B
J�B
K�B
L�B
N�B
O�B
N�B
O�B
O�B
Q�B
Q�B
S�B
S�B
S�B
VB
W
B
W
B
YB
ZB
ZB
[#B
]/B
]/B
^5B
_;B
_;B
aHB
aHB
cTB
cTB
dZB
dZB
e`B
dZB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
l�B
l�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
y�B
y�B
z�B
y�B
|�B
|�B
}�B
~�B
� B
~�B
� B
�B
�B
�B
�%B
�1B
�DB
�DB
�JB
�PB
�VB
�oB
�oB
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
��B
��B
��B
��B
��B
�B
�B
�B
�B
�-B
�3B
�9B
�?B
�?B
�FB
�FB
�LB
�LB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�dB
�dB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�dBM�BN�BM�BN�BN�BQ�BL�BM�BM�BM�BM�BN�BL�BN�BS�BN�BN�BM�BO�BM�BM�BM�BL�BN�BN�BN�BL�BM�BL�BM�BM�BM�BM�BM�BM�BM�BN�BM�BM�BM�BM�BL�BM�BM�BM�BM�BM�BM�BL�BL�BM�BM�BL�BM�BM�BM�BM�BM�BM�BM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     BL�BL�BK�BJ�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BL�BK�BM�BM�BN�BP�BT�Bl�B��B�B2-B?}B?}B?}B5?B.B-B)�B1'B@�BW
BT�B\)Bm�Be`Br�Bn�Bv�Bu�Bv�Bt�Bz�B{�B�B� By�B�B�B�%B�B�DB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�JB�7B�+B�B�B�B~�Bs�BbNBW
BL�B>wBhBB�B�/B��B��B��B��B�wB�jBȴB��B��BĜB��B��B��B��B�7BcTBJ�B?}B9XB$�B�BB
��B
�B
�TB
�;B
�B
��B
�LB
�B
��B
�+B
o�B
jB
K�B
;dB
2-B
.B
�B
bB
DB
B
  B	��B	��B	�B	�`B	�BB	�B	��B	�B	��B	��B	�hB	�7B	{�B	v�B	q�B	gmB	^5B	T�B	P�B	H�B	@�B	6FB	'�B	"�B	�B	�B	PB	%B��B��B�B�B�B�mB�`B�HB�B��B��B��BɺBÖB��B�jB�^B�LB�LB�FB�-B�B�!B�B��B��B��B�uB�bB�PB�=B�+B�B�Bz�Bx�Bu�Bs�Bm�Bk�BiyBbNB`BB]/B]/B`BB[#BZBW
BVBP�BN�BP�BP�BM�BL�BJ�BG�BH�BG�BE�BF�BD�BE�BD�BB�B?}B9XB33B49B2-B/B-B+B&�B'�B%�B#�B#�B#�B"�B"�B �B#�B&�B(�B/B-B-B+B+B/B/B1'B-B0!B1'B2-B33B1'B.B1'B33B2-BF�B]/B]/BXBQ�B[#BffB�uB��B�dBŢB�
B�B��B	�B	%�B	:^B	A�B	N�B	G�B	J�B	S�B	aHB	q�B	u�B	� B	�B	�PB	��B	��B	��B	�B	�RB	B	ĜB	��B	��B	��B	��B	��B	ɺB	�B	��B	�B	�yB	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
1B
1B
1B
	7B
DB
PB
\B
oB
uB
�B
�B
�B
�B
�B
 �B
"�B
$�B
%�B
&�B
(�B
,B
-B
.B
/B
0!B
1'B
33B
49B
49B
7LB
8RB
:^B
:^B
;dB
=qB
?}B
>wB
A�B
B�B
E�B
E�B
E�B
F�B
H�B
I�B
I�B
J�B
K�B
L�B
N�B
O�B
N�B
O�B
O�B
Q�B
Q�B
S�B
S�B
S�B
VB
W
B
W
B
YB
ZB
ZB
[#B
]/B
]/B
^5B
_;B
_;B
aHB
aHB
cTB
cTB
dZB
dZB
e`B
dZB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
l�B
l�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
y�B
y�B
z�B
y�B
|�B
|�B
}�B
~�B
�B
� B
�B
�B
�B
�B
�+B
�7B
�JB
�JB
�PB
�VB
�\B
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
�B
�B
�B
�!B
�'B
�'B
�9B
�?B
�FB
�LB
�LB
�RB
�RB
�XB
�XB
�^B
�dB
�jB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�}B
�wB
�}B
�}B
�wB
�wB
�}B
�wB
�wB
�}B
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
�}B
��B
�}B
�}B
��B
��B
��B
��B
��B
�}BK�BL�BK�BL�BL�BO�BJ�BK�BK�BK�BK�BL�BJ�BL�BQ�BL�BL�BK�BM�BK�BK�BK�BJ�BL�BL�BL�BJ�BK�BJ�BK�BK�BK�BK�BK�BK�BK�BL�BK�BK�BK�BK�BJ�BK�BK�BK�BK�BK�BK�BJ�BJ�BK�BK�BJ�BK�BK�BK�BK�BK�BK�BK�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808131703592021061413522820210614135228202106141746332021061417463320210614174633201808131703592021061413522820210614135228202106141746332021061417463320210614174633PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018081317035920180813170359  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081317035920180813170359QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018081317035920180813170359QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015120210722160151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                