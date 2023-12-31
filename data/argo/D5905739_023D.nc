CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-27T17:03:37Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �D   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   м   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                       HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                       HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        (   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    @   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	�Argo profile    3.1 1.2 19500101000000  20180727170337  20210617131459  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�r����@�r����11  @�r��>��@�r��>��@6�DUGZ@6�DUGZ�c؆`�V�c؆`�V11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?333@   @@  @�33@�33@�  @�  A��A  A$��AC33Aa��A���A���A�33A�  A���A���A�33A�  B ��BffB  BffB   B(  B0  B8ffB@��BH  BPffBX��B`  Bh  Bp  Bw��B�33B�33B�  B�  B�  B�33B�33B���B�33B�33B�33B�  B���B�  B�33B�33B�33B�  B�  B�33B�  B�ffB�33B�  B���B�33B���B�ffB�33B���B�  B�33C 33C33C  C�3C��C	�fC�fC  C�C33CL�CL�CL�CL�CffC�C��C!�fC$�C&L�C(�C)�fC,  C.L�C0�C1�fC433C6�C8  C9�fC;��C>�C@L�CBL�CD33CF33CH33CJ�CL  CM�fCP  CR  CS�fCVL�CX�CZ�C\  C]�fC_�fCa�fCc�fCe��Ch33Cj�Cl  Cn  Co�fCrL�CtffCv33Cx33Cz33C|33C~�C�  C��3C��C�&fC��C��3C��C�&fC��C��fC��3C��3C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��3C��3C��fC��fC��fC��fC��3C�  C��C��C��C�&fC��C��3C�  C��C�&fC�  C��fC�  C��C�  C��fC�  C��C��C�  C��C��C�  C�  C��3C��C�  C�  C��3C��fC��C��C��3C��C�  C��fC��C��3C��fC��3C��C��C�&fC��C��3C��C��C�&fC��C��3C�  C��C�&fC��C��fC��fC��3C�  C��C��C��C��C�  C�  C��3C��C�&fC��C��C��3C��fC�  C��C�  C��3C��fC�  C��C�  C�  C��fC�  C��C��3C��C��C�  C��3C�33C�&fC�&fC��C��Dl�DٚD	� D&fD��D��DffD@ D�3D�3Dy�D"&fD$��D'�fD*` D-  D/ٚD2� D5�D7�3D:S3D<��D?� DB3DD�fDG,�DI� DLY�DN��DQ��DT,�DV��DYffD\fD^�fDa33Dc��DfL�Dh�3DkS3Dm�fDp9�Dr�3Du&fDw�fDy��D{��D~L�D�P D�� D���D���D�fD�6fD�Y�D���D���D��D�fD�<�D�ffD�� D���D��fD�fD�FfD�vfD�� D��3D�fD�P D���D��fD�fD�C3D�� D��fD���D�#3D�VfD�� D��3D�3D�9�D�s3D���D�� D� D�@ D�p D���D��3D��D�P D�� D��fD���D��D�P D��3D���D��fD�  D�0 D�Y�Dē3D�� D��D�3D�<�D�VfD�y�D̜�D��3D���D���D��D�9�D�\�Dԃ3Dլ�D���D��3D�3D�33D�S3D�|�DݖfD޼�D��fD��3D��D�,�D�I�D�` D�s3D� D�3D� D��3D��3D��fD��fD�	�D��D�,�D�<�D�FfD�P D�\�D�p D�|�D���D��3D���D�� D���D�ɚD�� E   E ��E  E��EI�E� Ex E3E�3E<�E�fEs3EfE8 E
 E;3Ek3E�fE��E� E E6fE�fE�E( EI�E��E E$�E8 E� E��E K3E!L�E"�3E#��E%( E&�3E'� E(�E*C3E+.fE,�fE-�fE/L�E0.fE1��E2��E4D�E5�3E6p E7�fE8��E:H E;��E<�fE?��ECFfEFD�EII�ELh EO�3ER�fEU��EX� E\	�E_1�Eb0 EeffEhq�Ek�fEn� Eq�fEu)�Ex	�E{,�E~~fE�њE�H E��E�3E���E�|�E�+3E�p�E��fE�fE�`�E���E�3E�P�E���E���E�:fE�{3?��?��?��?��?   ?   ?   ?   >���>���>L��>���>���>���>���?   ?   ?   ?   ?��?333?333?L��?�  ?�  ?���?�33?ٙ�?�33@��@33@,��@@  @Y��@l��@�33@���@�33@�33@�  @���@�ff@�33@�  @���@���A��A33A33A��A   A(  A0  A6ffA@  AFffANffAVffA\��AfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444441411411444114114111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?fff?���@   @`  @�33@�33@�  @�  A	��A  A,��AK33Ai��A���A���A�33A�  A���A���A�33A�  B��B
ffB  BffB"  B*  B2  B:ffBB��BJ  BRffBZ��Bb  Bj  Br  By��B�33B�33B�  B�  B�  B�33B�33B���B�33B�33B�33B�  B���B�  B�33B�33B�33B�  B�  B�33B�  B�ffB�33B�  BᙚB�33B���B�ffB�33B���B�  B�33C �3C�3C� C33CL�C
ffCffC� C��C�3C��C��C��C��C�fC��C L�C"ffC$��C&��C(��C*ffC,� C.��C0��C2ffC4�3C6��C8� C:ffC<L�C>��C@��CB��CD�3CF�3CH�3CJ��CL� CNffCP� CR� CTffCV��CX��CZ��C\� C^ffC`ffCbffCdffCfL�Ch�3Cj��Cl� Cn� CpffCr��Ct�fCv�3Cx�3Cz�3C|�3C~��C�@ C�33C�L�C�ffC�L�C�33C�L�C�ffC�L�C�&fC�33C�33C�L�C�@ C�L�C�Y�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�L�C�L�C�@ C�@ C�33C�33C�&fC�&fC�&fC�&fC�33C�@ C�L�C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�@ C�&fC�@ C�L�C�@ C�&fC�@ C�Y�C�L�C�@ C�Y�C�L�C�@ C�@ C�33C�L�C�@ C�@ C�33C�&fC�L�C�L�C�33C�L�C�@ C�&fC�L�C�33C�&fC�33C�L�C�Y�C�ffC�Y�C�33C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�&fC�&fC�33C�@ C�L�C�L�C�L�C�L�C�@ C�@ C�33C�L�C�ffC�Y�C�L�C�33C�&fC�@ C�L�C�@ C�33C�&fC�@ C�Y�C�@ C�@ C�&fC�@ C�L�C�33C�L�C�Y�C�@ C�33C�s3C�ffC�ffC�Y�C�L�D��D��D	� DFfD�D��D�fD` D3D�3D��D"FfD%�D'�fD*� D-@ D/��D2� D59�D7�3D:s3D=�D?� DB33DD�fDGL�DI� DLy�DO�DQ��DTL�DV��DY�fD\&fD^�fDaS3Dc��Dfl�Dh�3Dks3Dm�fDpY�Dr�3DuFfDw�fDz�D|�D~l�D�` D�� D���D���D�&fD�FfD�i�D���D�ɚD���D�&fD�L�D�vfD�� D�ɚD��fD�&fD�VfD��fD�� D��3D�&fD�` D���D��fD�fD�S3D�� D��fD���D�33D�ffD�� D��3D�3D�I�D��3D���D�� D�  D�P D�� D���D��3D�)�D�` D�� D��fD���D�)�D�` D��3D���D��fD� D�@ D�i�Dģ3D�� D���D�#3D�L�D�ffDˉ�D̬�D��3D���D�	�D�,�D�I�D�l�Dԓ3Dռ�D���D�3D�#3D�C3D�c3D܌�DݦfD���D��fD�3D��D�<�D�Y�D�p D�3D� D�3D�� D��3D��3D��fD�fD��D�,�D�<�D�L�D�VfD�` D�l�D�� D���D���D��3D���D�� D���D�ٚD�� E  E ��E( E��EQ�E� E� E3E�3ED�E�fE{3EfE@ E
 EC3Es3E�fE��E� E  E>fE�fE�E0 EQ�E��E E,�E@ E� E��E S3E!T�E"�3E#��E%0 E&�3E'� E(�E*K3E+6fE,�fE-�fE/T�E06fE1��E2��E4L�E5�3E6x E7�fE9�E:P E;��E<�fE?��ECNfEFL�EIQ�ELp EO�3ER�fEU��EX� E\�E_9�Eb8 EenfEhy�Ek�fEn� Eq�fEu1�Ex�E{4�E~�fE�՚E�L E��E��3E���E���E�/3E�t�E��fE�"fE�d�E���E�3E�T�E���E���E�>fE�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�?333?L��G�O�?L��?fffG�O�G�O�G�O�?�  ?���G�O�?���?�ffG�O�?�  ?ٙ�?�33@��@��@,��@333@L��@`  @y��@�ff@�33@���@�33@�33@�  @ə�@�ff@�33@�  @���AffA��A33A33A!��A(  A0  A8  A>ffAH  ANffAVffA^ffAd��AnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444441411411444114114111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ �@ �@ �@ {@ �@ "�@ (�@ /�@ 7L@ =q@ FQ@ SI@ `B@ n�@ {�@ ��@ �0@ ��@ �-@ �w@ ��@ ��@ ��@ ��@@@�@,`@:�@I@UU@c�@r@~K@��@��@��@��@�>@�7@��@�@��@�@�@"�@0x@>@K@X@ff@t�@�d@�@�@��@�@��@��@��@��@��@
�@6@&�@3�@@,@N�@\�@k.@x�@�@��@�@�f@�@�c@׹@�@�e@@�@[@+�@7L@B�@Q=@`B@oF@{�@��@�0@��@�-@�w@�*@�#@�@�@@�@ �@.l@;d@I@V�@c�@p�@}�@��@��@��@��@�>@��@��@��@�~@%@�@ �@1'@>@K@X�@e�@v@�p@��@�a@�@��@ƨ@Ӡ@��@�@��@
�@6@&;@5?@A�@M$@[z@i!@x&@�@�u@��@��@�@��@�h@�@�@ �@@�@)�@6�@DD@Q=@^�@k�@y�@�+@��@�(@�~@��@�|@��@�(@�q@	�@	@	g@	.l@	:@	FQ@	UU@	c�@	p�@	|�@	��@	��@	��@	��@	��@	��@	��@	�@	�~@
�@
{@
""@
/@
<@
K�@
Yn@
e�@
t�@
��@
��@
��@
��@
��@
�J@
�O@
�@
��@
��@	�@�@&�@5?@A�@M�@\)@j@y�@��@�h@�@�f@��@�@׹@�`@�@  @�@�@)�@8�@E�@R�@^�@k�@z�@�7@�0@�(@�!@�&@�*@�t@�@�e@j@�@
@-@;d@G�@T�@e�@r�@�W@�P@�H@ @bN@��@�@>�@��@�O@""@k�@��@�@K�@��@��@,`@ww@��@
=@Q=@�<@��@&�@m:@��@��@>�@�@�@o@Z@��@�y@0x@x&@��@v@Lu@��@խ@�@\�@��@�T@&;@g@��@�;@ @_�@�m@��@!s@dZ@��@��@ �@a�@�z@�@!s@`�@�m@��@ @`�@��@�@ %�@ g@ ��@ �@!/@!r@!�F@!��@"=q@"�@"��@#@#E�@#��@#�@$V@$P�@$�u@$խ@%�@%X�@%��@%�t@&
@&`A@&�z@&�@'%�@'g�@'��@'�(@(,`@(m�@(�f@(��@),`@)m:@)��@)�@*/�@*oF@*��@*�@@+*S@+hs@+��@+�`@,!s@,^5@,�U@,�@-6@-V@-��@-��@.�@.O0@.��@.�@/	�@/E�@/�p@/��@/�E@09X@0v�@0��@0��@1)�@1ff@1�@1�t@2*@2O0@2��@2��@2��@39X@3s_@3�f@3�@4�@4X@4��@4�@5v@5>@5s_@5�@5��@6$/@6_�@6�@6�#@7�@7Wb@7��@7�
@8�@8V�@8��@8�@9�@9]�@9�T@:�@:�@;g�@;�y@<i!@<�(@=i�@=�@>bN@?�@?��@@@@��@AB�@A��@B1�@B�A@CQ�@C�2@DdZ@D�C@Er@E�#@Fww@Go@Gww@HV@H�4@I	�@I��@J5�@J�o@K+�@K�2@LWb@L�(@Mx�@M�
@NbN@N�@Oz�@P@P��@Q�@SQ=@T�<@U��@W6�@X��@Y�[@[FQ@\~K@]��@_:�@`��@a��@c-@d��@e�H@g/@h�I@i�O@k+@l��@m��@o,`@p��@q��@s&;@tx&@u�l@v"�@v^5@v��@v�@w)�@w�@w�k@w� @xLv@x��@x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�@ ^@ G�O�@ @ �G�O�G�O�G�O�@ j@ G�O�@ �@ vG�O�@ �@ 1@ 	�@ �@ �@ @ �@ o@ {@ 6@ B@ �@ 
@ g@ "�@ %�@ '�@ *S@ -@ /�@ 2�@ 5�@ 8�@ ;d@ >�@ A�@ DD@ G�@ K@ M�@ Q�@ T�@ X@ [z@ ^5@ bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�1A�
=A�1A�JA�
=A�VA�VA�bA�oA�oA�{A�VA�bA�{A�{A�VA�bA�bA�bA�oA�bA�JA�
=A�
=A�%A�A��A��Aΰ!A��A̓A��yA�bAƶFA�hsA�z�A���A���A��RA���A�&�A��A���A���A�x�A��9A�oA��^A�7LA�=qA�G�A�&�A�dZA�~�A� �A�JA��
A�+A�bA�A�~�A��^A�Q�A�XA�ĜA�;dA�+A�ffA�XA�~�A�1'A���A���A��mA��A���A�dZA��DA�Q�A��A��uA��mA��wA�E�A��!A�$�A��RA���A�^5A���A��A�9XA��FA��A�
=A��yA��9A�G�A�K�A�G�A��yA��;A��uA�VA���A��;A�"�A��9A��-A�l�A�VA��+A�Q�A�JA���A|�A~��A~-A}oA{�^Az{Ax1Av��Au��Au?}Ar��Ap��Ap{Ao��An��Am��Am�Al9XAiAf�HAd�jAb{A`E�A^ĜA\=qAZ$�AV��ATVAP��AM�TAMoALQ�AK/AJ�AG�AE��ACl�ABVABA�A@�HA?O�A=�hA;�-A9��A7oA4�`A3hsA2�+A1��A/?}A-�7A,�A,�A*�A)�A)?}A(  A'��A&��A%�A$�9A#�A!��A!VA ffA E�A -A�A33A\)Az�A�^A�`Az�AbA�
AXAAI�A/A�\Ax�AjA��A��A�A�7A��A�TA�-A�hA|�A&�Ar�AbNA�hA+AA�AƨAO�A
$�A	�A�\A\)AZA1A��AC�A�A�A��A��AI�A�HA$�A�-AhsA ȴA ^5@�~�@�(�@�1@�/@�K�@��\@�z�@�|�@�dZ@�!@�p�@��`@��T@�9@�p�@�o@�
=@��@�V@�r�@߮@��@ݺ^@ڸR@� �@׍P@ӍP@�p�@��
@�^5@�ƨ@�&�@�"�@���@� �@���@���@�o@��;@��@�ȴ@��R@�l�@��@��P@�
=@��\@��T@�x�@�Q�@�C�@��y@�~�@�V@��h@��j@��
@���@��@�=q@�x�@��9@��@��F@�;d@���@�p�@���@�S�@��+@��@��@�
=@�J@��-@��@��u@�33@��7@��D@��@}p�@z^5@u/@s@q��@qG�@o+@kt�@iX@h�@f�y@eV@cdZ@`A�@]��@\��@Y�@Yx�@W
=@UO�@Tj@S"�@Q��@QG�@P �@Nff@MV@Kt�@J=q@H��@G
=@Fff@E�@Dz�@C�F@B�H@A�@AG�@@�@?
=@=��@=V@<I�@:�\@8�@7�@7
=@5��@4��@49X@2~�@2J@1��@0r�@/�@-�@,j@+�m@*�@)�#@(�9@(A�@'�@&$�@%@$��@$(�@"�H@"M�@!��@!X@ A�@�@{@/@�/@�D@�F@@��@�@bN@\)@�T@�@9X@C�@�@��@G�@�@�y@E�@��@��@�F@
�!@
=q@	��@	hs@��@�w@
=@�y@p�@O�@��@z�@�m@�@o@�@-@�@%@ �?�5??�C�?���?�r�?�K�?�ff?��/?���?��?��?��?�D?��H?陚?�E�?��T?�j?㕁?�&�?ߝ�?ݑh?ܬ?ۅ?ٙ�?�r�?�ff?Ձ?�9X?�S�?�M�?�G�?�bN?ϝ�?θR?Η�?Η�?��?�/?�j?˥�?�dZ?�^5?��?�K�?š�?��
?�o?�n�?�G�?�;d?�v�?�v�?��-?�O�?�j?�?��?�^5?���?���?��?���?�=q?�=q?�^5?�?�C�?�ƨ?�I�?��?�p�?��?�{?�{?�5??�V?�v�?���?��R?��?���?��?�;dA�%A�%A�1A�%A�A�%A�1A�1A�%A�1A�1A�1A�1A�
=A�
=A�
=A�
=A�
=A�JA�
=A�
=A�JA�
=A�JA�VA�
=A�JA�JA�
=A�1A�1A�1A�1A�
=A�
=A�JA�JA�
=A�
=A�JA�JA�VA�JA�bA�bA�bA�bA�oA�oA�bA�bA�oA�oA�oA�{A�{A�{A�bA�VA�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�
=A�1A�
=A�1A�JA�
=A�VA�VA�bA�oA�oA�{A�VA�bA�{A�{A�VA�bA�bA�bA�oA�bA�JA�
=A�
=A�%A�A��A��Aΰ!A��A̓A��yA�bAƶFA�hsA�z�A���A���A��RA���A�&�A��A���A���A�x�A��9A�oA��^A�7LA�=qA�G�A�&�A�dZA�~�A� �A�JA��
A�+A�bA�A�~�A��^A�Q�A�XA�ĜA�;dA�+A�ffA�XA�~�A�1'A���A���A��mA��A���A�dZA��DA�Q�A��A��uA��mA��wA�E�A��!A�$�A��RA���A�^5A���A��A�9XA��FA��A�
=A��yA��9A�G�A�K�A�G�A��yA��;A��uA�VA���A��;A�"�A��9A��-A�l�A�VA��+A�Q�A�JA���A|�A~��A~-A}oA{�^Az{Ax1Av��Au��Au?}Ar��Ap��Ap{Ao��An��Am��Am�Al9XAiAf�HAd�jAb{A`E�A^ĜA\=qAZ$�AV��ATVAP��AM�TAMoALQ�AK/AJ�AG�AE��ACl�ABVABA�A@�HA?O�A=�hA;�-A9��A7oA4�`A3hsA2�+A1��A/?}A-�7A,�A,�A*�A)�A)?}A(  A'��A&��A%�A$�9A#�A!��A!VA ffA E�A -A�A33A\)Az�A�^A�`Az�AbA�
AXAAI�A/A�\Ax�AjA��A��A�A�7A��A�TA�-A�hA|�A&�Ar�AbNA�hA+AA�AƨAO�A
$�A	�A�\A\)AZA1A��AC�A�A�A��A��AI�A�HA$�A�-AhsA ȴA ^5@�~�@�(�@�1@�/@�K�@��\@�z�@�|�@�dZ@�!@�p�@��`@��T@�9@�p�@�o@�
=@��@�V@�r�@߮@��@ݺ^@ڸR@� �@׍P@ӍP@�p�@��
@�^5@�ƨ@�&�@�"�@���@� �@���@���@�o@��;@��@�ȴ@��R@�l�@��@��P@�
=@��\@��T@�x�@�Q�@�C�@��y@�~�@�V@��h@��j@��
@���@��@�=q@�x�@��9@��@��F@�;d@���@�p�@���@�S�@��+@��@��@�
=@�J@��-@��@��u@�33@��7@��D@��@}p�@z^5@u/@s@q��@qG�@o+@kt�@iX@h�@f�y@eV@cdZ@`A�@]��@\��@Y�@Yx�@W
=@UO�@Tj@S"�@Q��@QG�@P �@Nff@MV@Kt�@J=q@H��@G
=@Fff@E�@Dz�@C�F@B�H@A�@AG�@@�@?
=@=��@=V@<I�@:�\@8�@7�@7
=@5��@4��@49X@2~�@2J@1��@0r�@/�@-�@,j@+�m@*�@)�#@(�9@(A�@'�@&$�@%@$��@$(�@"�H@"M�@!��@!X@ A�@�@{@/@�/@�D@�F@@��@�@bN@\)@�T@�@9X@C�@�@��@G�@�@�y@E�@��@��@�F@
�!@
=q@	��@	hs@��@�w@
=@�y@p�@O�@��@z�@�m@�@o@�@-@�@%@ �?�5??�C�?���?�r�?�K�?�ff?��/?���?��?��?��?�D?��H?陚?�E�?��T?�j?㕁?�&�?ߝ�?ݑh?ܬ?ۅ?ٙ�?�r�?�ff?Ձ?�9X?�S�?�M�?�G�?�bN?ϝ�?θR?Η�?Η�?��?�/?�j?˥�?�dZ?�^5?��?�K�?š�?��
?�o?�n�?�G�?�;d?�v�?�v�?��-?�O�?�j?�?��?�^5?���?���?��?���?�=q?�=q?�^5?�?�C�?�ƨ?�I�?��?�p�?��?�{?�{?�5??�V?�v�?���?��R?��?���?��?�;dA�%A�%A�1A�%A�A�%A�1A�1A�%A�1A�1A�1A�1A�
=A�
=A�
=A�
=A�
=A�JA�
=A�
=A�JA�
=A�JA�VA�
=A�JA�JA�
=A�1A�1A�1A�1A�
=A�
=A�JA�JA�
=A�
=A�JA�JA�VA�JA�bA�bA�bA�bA�oA�oA�bA�bA�oA�oA�oA�{A�{A�{A�bA�VA�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBBBBBBB%B%B%B%B1BbBs�B�B$�B33B.B,B>wBK�BF�BI�BT�BhsBw�B|�B� B� B�B�B�B��B��B��B��B��B��B��B�B�9B�-B�'B�!B�!B�!B�B�B��B��B��B��B��B��B��B��B��B�DBy�BYBG�B/BuBbB
=B�BǮB��B�{B�DBhsBO�B33BuBB
��B
�B
�B
�5B
ĜB
�^B
�XB
�dB
�dB
��B
��B
�B
�XB
�
B
�NB
��B
�XB
�3B
�!B
��B
��B
��B
�=B
y�B
s�B
gmB
aHB
^5B
XB
N�B
G�B
9XB
+B
%�B
�B
�B
B	��B	�B	�B	�fB	�NB	�;B	��B	ĜB	�RB	��B	��B	�+B	z�B	k�B	YB	C�B	.B	�B	B��B��B�B�sB�)B�
B��B��BȴB��B�qB�3B�B��B�{B�PB�=B�1B�B{�Bx�Bv�Bt�Bm�Bm�BjBhsBiyBe`BcTB^5BZB\)BZBZBYBYBW
B_;BjBiyBjBiyBjBhsBgmBffBe`BbNBbNBaHB]/B[#B\)BXBW
BT�BS�BS�BS�BQ�BP�BP�BN�BJ�BJ�BJ�BG�BG�BD�BC�BC�B?}B<jB=qB?}B?}B?}B?}B?}B?}B?}B>wBE�BG�BI�BH�BK�BK�BG�BI�BI�BF�BH�BJ�BE�BF�BI�BN�BR�BL�BM�BM�BF�BE�BD�BD�BG�BI�BI�BH�BE�BB�BM�BVB��B��B��B�B��B	DB	PB��B	�B	�B	�B	�B	+B�BB�TB��B	\B	"�B	33B	=qB	I�B	T�B	\)B	e`B	m�B	q�B	w�B	{�B	~�B	�B	�=B	�hB	�uB	��B	��B	��B	�3B	�jB	B	ÖB	ĜB	ŢB	��B	��B	�B	�5B	�TB	�sB	�yB	�B	�B	��B	��B	��B	��B
B
B
+B
1B
	7B
\B
\B
\B
bB
uB
{B
�B
�B
�B
!�B
!�B
%�B
$�B
%�B
(�B
)�B
+B
-B
.B
/B
0!B
1'B
2-B
49B
5?B
7LB
7LB
9XB
9XB
9XB
;dB
<jB
=qB
=qB
>wB
?}B
A�B
A�B
B�B
D�B
E�B
E�B
F�B
G�B
H�B
I�B
J�B
L�B
L�B
L�B
O�B
Q�B
Q�B
Q�B
R�B
S�B
VB
T�B
XB
W
B
YB
YB
ZB
[#B
[#B
\)B
]/B
_;B
_;B
`BB
`BB
`BB
aHB
cTB
bNB
cTB
e`B
ffB
gmB
gmB
ffB
iyB
hsB
iyB
jB
l�B
k�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
s�B
s�B
v�B
u�B
v�B
w�B
x�B
x�B
x�B
x�B
z�B
z�B
{�B
|�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�=B
�=B
�JB
�JB
�\B
�VB
�\B
�bB
�oB
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
�B
�B
�B
�B
�!B
�!B
�!B
�-B
�-B
�3B
�9B
�FB
�?B
�FB
�FB
�LB
�RB
�LB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB
�^B%B%BBB%B%BBB%BBBB%BBBBBBBBBBBBBBBBBBB%BBBBBBBBBBBBBBBBBBBBBBBBBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B B�BBBB	B	BBHBs�B�wB$�B3B-�B+�B>_BK�BF�BI�BT�Bh]Bw�B|�B�B�B��B� B�B��B�vB�wB��B��B��B��B�B�,B� B�B�B�B�B�
B��B��B��B��B��B��B��B��B��B��B�@By�BYBG�B/BsBaB
<B�BǮB��B�|B�EBhtBO�B35BwBB
��B
�B
�B
�:B
ġB
�dB
�^B
�kB
�kB
��B
��B
�B
�aB
�B
�XB
��B
�cB
�>B
�-B
��B
��B
��B
�KB
y�B
s�B
g}B
aXB
^EB
X!B
N�B
G�B
9kB
+B
%�B
�B
�B
!B	��B	��B	�B	�}B	�eB	�SB	�B	ĵB	�kB	��B	��B	�EB	z�B	k�B	Y2B	C�B	.0B	�B	6B�B��B�B�B�HB�)B��B��B��B��B��B�UB�%B��B��B�tB�aB�UB�1B|Bx�Bv�Bt�Bm�Bm�Bj�Bh�Bi�Be�Bc~B^`BZHB\UBZIBZJBYDBYEBW8B_jBj�Bi�Bj�Bi�Bj�Bh�Bg�Bf�Be�Bb�Bb�Ba}B]eB[YB\`BXGBWBBU6BT1BT1BT2BR&BQ BQ!BOBJ�BJ�BJ�BG�BG�BD�BC�BC�B?�B<�B=�B?�B?�B?�B?�B?�B?�B?�B>�BE�BG�BJBH�BLBLBG�BJBJBF�BI BKBE�BF�BJBO'BSABMBN#BN#BF�BE�BD�BD�BHBJBJBIBE�BB�BN)BVZB�B�OB�#B�B�\B	�B	�B�4B	B	0B	!B	B	�B��B��B�EB	�B	#aB	3�B	>B	JSB	U�B	\�B	fB	n7B	rSB	x{B	|�B	�B	��B	��B	�#B	�4B	�IB	�qB	��B	��B	�8B	�`B	�jB	�sB	�|B	˞B	пB	��B	�B	�<B	�^B	�fB	�B	�B	��B	��B	��B	��B
B
#B
1B
	:B

CB
kB
mB
pB
yB
�B
�B
�B
�B
�B
"�B
"�B
'B
&B
'B
*-B
+6B
,>B
.MB
/VB
0`B
1iB
2rB
3{B
5�B
6�B
8�B
8�B
:�B
:�B
:�B
<�B
=�B
>�B
>�B
?�B
@�B
B�B
CB
DB
FB
G$B
G&B
H/B
I8B
JAB
KJB
LTB
NbB
NeB
NhB
Q}B
S�B
S�B
S�B
T�B
U�B
W�B
V�B
Y�B
X�B
Z�B
Z�B
[�B
\�B
\�B
]�B
^�B
aB
a
B
bB
bB
bB
c!B
e0B
d-B
e5B
gDB
hLB
iVB
iYB
hTB
kjB
jfB
koB
lwB
n�B
m�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
x�B
w�B
x�B
y�B
z�B
{B
{B
{B
}B
}B
~"B
,B
�;B
�FB
�[B
�aB
�mB
�zB
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
�B
�)B
�6B
�AB
�NB
�_B
�`B
�zB
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
�B
�#B
�6B
�KB
�hB
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�6B
�SB
�[B
�rB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�+B
�4B
�KB
�MB
�PB
�TB
�WB
�YB
�]B
�`B
�hB
�lB
�iB
�qB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807271703372021061413552920210614135529202106171312262021061713122620210617131226201807271703372021061413552920210614135529202106171312262021061713122620210617131226PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072717033720180727170337  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072717033720180727170337QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072717033720180727170337QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145920210617131459IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                