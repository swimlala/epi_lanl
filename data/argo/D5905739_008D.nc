CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  
   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:44Z creation      
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
resolution        =���   axis      Z        P  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  P8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ҹ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   $   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   @   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220244  20210617131453  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�d�/hab@�d�/hab11  @�d�&�@�d�&�@6�E���@6�E����c�2�E��c�2�E�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @@  @�  @�33@�33@�33A��A  A#33AC33A`  A~ffA�  A�  A�  A���A�  A�  A�  B ffBffB��B  B ffB(  B0ffB8��B?��BG��BPffBX��B`  BhffBp��Bx  B�33B�ffB�33B�33B�33B�33B�  B�  B�  B���B�33B�33B���B�  B�ffB�ffB�  B�33B�33B�  B�  B�  B���B�ffB�ffB�33B�33B�  B�  B���B�ffB�ffC �C�fC�fC33C�C
�C�C  C33C�C�fC  C33C  C�fC�C   C!��C$  C&33C(�C)�fC,  C.L�C/�fC1��C3�fC5�fC8�C:33C<�C=��C@  CB33CD�CF33CH33CJL�CL  CM�3CO��CQ�fCT  CV33CX�CY�fC\  C^33C`  Ca��Cd  Cf33Ch33Cj  Cl  Cm��Cp�Cr  Cs�3Cv  Cw�fCy�3C|  C}�fC��C�  C�&fC��C�  C�&fC��C�  C�&fC��C��C��3C��3C��C��C�  C��C��C�  C��C��C��3C�  C�&fC��C��3C�  C��C��C��fC��3C�  C��C�&fC�33C��C��fC��3C�  C��C�&fC��C��3C��C�&fC��C��3C��C��C�&fC��C�  C��C��C��3C��C�  C��3C��C�  C��3C�&fC��C��C�  C��fC��C��3C��fC�  C�  C��fC��C�  C��fC��C�  C��fC�  C�&fC��C��3C��C�&fC��C��3C��C��C�&fC�&fC��C��3C��C�&fC��C��fC�  C��C�&fC��C��3C��C�&fC��C��3C��C�&fC��C��3C��C��C��C��3C�  C��C�  C��fC�  C��C��C��3C�  C��3C�ٚD� D33D�D
�3DٚD�3Dy�DL�D�D�fD9�D � D#FfD%��D(,�D*�3D-  D/y�D1�3D4l�D6ٚD9l�D;��D>s3D@� DCffDF  DH�3DKFfDM��DP��DS@ DU�fDX��D[S3D]�3D`�fDcl�Df3Dh�3DkS3Dm� DpffDr�fDu� Dw�3Dz` D|S3D~��D���D��fD�� D�fD��D�<�D�` D�|�D���D���D�ٚD��fD��D�<�D�Y�D�s3D���D��fD��3D�� D�3D�,�D�L�D�vfD���D�� D���D��D�L�D�|�D���D��3D�fD�C3D�|�D��3D��fD�3D�@ D�i�D���D���D�� D��3D�3D�0 D�L�D�c3D�y�D��3D��fD��3D�ٚD��fD��D�)�D�I�D�ffDfDà DĹ�D�� D���D�3D��D�33D�I�D�ffD͆fDΠ D��3D���D���D�3D�,�D�I�D�c3D׀ DؖfDٰ D�� D�� D��fD� D�&fD�@ D�Y�D�p D�fD䩚D���D��D�fD��D�9�D�Y�D�s3D�� D�� D��fD���D�  D�6fD�P D�ffD�|�D�� D���D��3D�� D��fD��3D���D��E fE ��EfE�3E+3E��E@ E��ENfEffE��E�E	��E
��E� ES3EY�E^fE��E�EnfEl�E��E�fES3E��E�fEfEs3E[3E�3E!!�E"�3E#t�E$�3E&#3E'��E(y�E)�E+I�E,9�E-�3E.��E03E1y�E2k3E3�3E5A�E6.fE7� E8�fE9�E;L�E<��E?��EC  EE�fEI;3EL>fEO+3ERx EU� EX��E[��E^��EbfEe�Eh.fEkI�En�3Eq�3Et� Ew� E{ E~4�E��fE�;3E��fE�D�E�� E�q�E���E�}�E� E�m�E���E�fE�]�E��3E��3E�1�E���E���E�6fE��3E���E��E�o3E�� E��E�g3E���>���>���>���>���>���>���>���>���>L��>���>���>���?   >���>L��>���>L��>���>���>���>���>���>���>���>���>���?   ?   ?��?333?L��?���?���?�33?�  ?ٙ�?�ff@ff@��@,��@@  @S33@l��@�  @���@�ff@�33@�  @���@ə�@�ff@�  @�  @���A��A33A33A��A   A(  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441141441411441414414111141111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?fff?�  @   @`  @�  @�33@�33@�33A	��A  A+33AK33Ah  A�33A�  A�  A�  A���A�  A�  A�  BffB
ffB��B  B"ffB*  B2ffB:��BA��BI��BRffBZ��Bb  BjffBr��Bz  B�33B�ffB�33B�33B�33B�33B�  B�  B�  B���B�33B�33B���B�  B�ffB�ffB�  B�33B�33B�  B�  B�  B���B�ffB�ffB�33B�33B�  B�  B���B�ffB�ffC ��CffCffC�3C��C
��C��C� C�3C��CffC� C�3C� CffC��C � C"L�C$� C&�3C(��C*ffC,� C.��C0ffC2L�C4ffC6ffC8��C:�3C<��C>L�C@� CB�3CD��CF�3CH�3CJ��CL� CN33CPL�CRffCT� CV�3CX��CZffC\� C^�3C`� CbL�Cd� Cf�3Ch�3Cj� Cl� CnL�Cp��Cr� Ct33Cv� CxffCz33C|� C~ffC�&fC�@ C�ffC�Y�C�@ C�ffC�Y�C�@ C�ffC�L�C�L�C�33C�33C�Y�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�L�C�33C�@ C�ffC�L�C�33C�@ C�Y�C�L�C�&fC�33C�@ C�Y�C�ffC�s3C�Y�C�&fC�33C�@ C�L�C�ffC�L�C�33C�L�C�ffC�L�C�33C�L�C�Y�C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�@ C�33C�Y�C�@ C�33C�ffC�Y�C�L�C�@ C�&fC�L�C�33C�&fC�@ C�@ C�&fC�L�C�@ C�&fC�L�C�@ C�&fC�@ C�ffC�L�C�33C�Y�C�ffC�Y�C�33C�L�C�L�C�ffC�ffC�Y�C�33C�L�C�ffC�L�C�&fC�@ C�Y�C�ffC�L�C�33C�L�C�ffC�Y�C�33C�Y�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�@ C�33C��D� DS3D,�D3D��D�3D��Dl�D,�D�fDY�D � D#ffD%ٚD(L�D*�3D-  D/��D23D4��D6��D9��D<�D>�3DA  DC�fDF  DH�3DKffDN�DP��DS` DVfDX��D[s3D^3D`�fDc��Df33Dh�3Dks3Dn  Dp�fDsfDu� Dx3Dz� D|s3D~ٚD���D��fD�� D�fD�,�D�L�D�p D���D���D�ɚD��D�fD�,�D�L�D�i�D��3D���D��fD��3D�  D�#3D�<�D�\�D��fD���D�� D���D�,�D�\�D���D���D��3D�&fD�S3D���D��3D��fD�#3D�P D�y�D���D���D�� D�3D�#3D�@ D�\�D�s3D���D��3D��fD��3D��D�fD��D�9�D�Y�D�vfDfDð D�ɚD�� D���D�3D�,�D�C3D�Y�D�vfD͖fDΰ D��3D���D�	�D�#3D�<�D�Y�D�s3Dא DئfD�� D�� D�� D�fD�  D�6fD�P D�i�D� D�fD乚D���D���D�fD�,�D�I�D�i�D�3D�� D�� D��fD��D�0 D�FfD�` D�vfD���D�� D���D��3D�� D��fD��3D�	�D��E fE ��E&fE�3E33E��EH E��EVfEnfE�E�E	��E
��E� E[3Ea�EffE��E�EvfEt�E��E�fE[3E��E�fEfE{3Ec3E�3E!)�E"�3E#|�E$�3E&+3E'��E(��E)�E+Q�E,A�E-�3E.��E03E1��E2s3E3�3E5I�E66fE7� E9fE9�E;T�E<��E?��EC EE�fEIC3ELFfEO33ER� EU� EX��E[��E^��EbfEe	�Eh6fEkQ�En�3Eq�3Et� Ew� E{ E~<�E��fE�?3E��fE�H�E�� E�u�E��E���E� E�q�E���E�fE�a�E��3E��3E�5�E���E���E�:fE��3E���E��E�s3E�� E��E�k3E���G�O�?L��G�O�?L��G�O�?L��G�O�G�O�?333?L��G�O�?L��G�O�G�O�?333G�O�?333?L��G�O�G�O�?L��G�O�?L��G�O�G�O�?fffG�O�?�  ?���?���?�ffG�O�?���?�33@   @��@33@&ff@9��@L��@`  @s33@�ff@�  @���@�ff@�33@�  @���@ٙ�@�ff@�  A   AffA��A33A33A!��A(  A0  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441141441411441414414111141111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ �@ �@ �@ {@ O@ "�@ )�@ 0x@ 7L@ =q@ E�@ SI@ _�@ l�@ z�@ ��@ �0@ ��@ �~@ �&@ ��@ �#@ ��@ �@j@�@�@-@;d@F�@T�@c�@r@~K@��@��@�A@��@��@��@ލ@�4@��@�@{@""@/@>@K�@X@ff@uk@�@�\@��@�Y@�R@��@Ӡ@��@�L@��@
�@�@%�@33@@,@O�@]�@j@v�@�p@�#@�@��@�k@�c@�h@�`@�@  @@O@(G@7L@DD@P�@_�@n�@{�@��@�0@��@��@��@�@��@��@� @@�@�@-�@:�@I@V�@e	@p�@|?@��@��@�A@�F@�>@ψ@��@��@�,@v@{@#�@1'@=q@K@Wb@g@t@�@�\@�U@��@�R@�J@�C@�H@��@��@
=@�@&�@33@B�@O0@\�@i!@v�@�|@�u@�m@�r@�@�c@�h@�`@�@  @�@�@(G@6�@E�@R�@^5@l�@z�@��@�<@��@��@��@�@�t@��@��@	@	b@	g@	.l@	:�@	F�@	V@	dZ@	r�@	�@	��@	��@	��@	�9@	�>@	�7@	�/@	��@	�,@
%@
�@
#�@
0x@
=q@
I�@
Yn@
e�@
r�@
��@
�\@
��@
�Y@
�R@
Ĝ@
�O@
�H@
�@
��@J@�@$�@4�@B�@O�@[z@j@x&@�+@��@��@�f@�k@�o@׹@�T@�Y@^@�@�@(G@7L@FQ@SI@^�@n�@|�@�7@��@��@��@��@�@�t@�y@��@@@ @-@9X@G�@T�@`�@�@5?@�@�C@!s@oF@�@1@SI@�H@��@%�@j@�f@�L@1�@t@��@�9@>�@�@�W@�@P�@��@׹@�@e	@��@�~@>�@��@��@�@e	@��@�q@B8@��@�C@�@_�@��@��@/�@r�@��@�(@+�@l�@�@�@*S@e�@�(@�H@
@Z�@�<@խ@o@Q=@��@�o@�@C�@�@�j@��@ :�@ v�@ �9@ �@!1�@!p�@!��@!�@"2�@"s_@"�9@"�q@#7�@#x&@#�@#�E@$>�@$~�@$�&@$��@%<@%z3@%�R@%�q@&3�@&p�@&�f@&��@'$/@'`A@'��@'׹@(@(O�@(�D@(�@)v@)B8@)�@)��@)��@*33@*o�@*�Y@*�m@+"�@+^5@+��@+�h@,{@,R�@,��@,�o@-�@-C�@-�W@-�j@-�,@.4�@.p�@.��@.�@/#�@/_�@/��@/�
@0@0N�@0��@0�@1%@1B�@1�@1�@1��@25@@2qS@2�~@2��@3-�@3i!@3�A@3�@4�@4Z@4��@4�7@5J@5H]@5~K@5��@5�@6.l@6i!@6�(@6܀@76@7O�@7��@7Ĝ@8  @88�@8s_@8��@9�<@:J@:�@;-@;�m@<Lu@<�k@=+�@=�C@>A�@>�l@?S�@?�e@@^�@@��@A�<@A�Q@B��@C-�@C��@D&�@D��@EZ@E��@FO�@F�@G{�@G��@H{�@I*@I{�@JB@J�W@K�@K�@L @L��@MV@M�@NUU@N�@@OR�@O�(@P�W@Q܀@S33@T[z@U��@W$�@XdZ@Y��@[/�@\`B@]��@_v@`v�@a��@cb@dc�@eє@g�@ht@i��@kO@lv@m��@o!s@poF@q�@s�@tn�@u��@w1@xb�@x�r@x�@y.l@y|?@y��@y�Q@z1'@z{�@zƨ@{�@{X@{�7@{��@|�@|b�@|��@|�@@}4�G�O�@ G�O�@ G�O�@ G�O�G�O�@ ^@ G�O�@ G�O�G�O�@ ^G�O�@ ^@ G�O�G�O�@ G�O�@ G�O�G�O�@ �G�O�@ j@ @ �@ vG�O�@ �@ 	�@ 
=@ �@ J@ V@ b@ o@ {@ �@ B@ O@ [@  @ "�@ %�@ (G@ +@ -�@ /�@ 33@ 5�@ 8�@ ;d@ >�@ A�@ DD@ G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA��A��A��A��A��A� �A�(�A�+A�-A�/A�-A�/A�/A�1'A�33A�/A�1'A�33A�33A�5?A�5?A�(�A�&�A�$�A��A�bA��yAÉ7A�hsA��A���A�VA�$�A��mA�v�A��A�9XA�JA��RA���A���A�hsA���A�&�A�ĜA�C�A��A�ffA� �A��yA���A��+A�?}A��A�v�A��yA��!A�oA��9A�ffA� �A�l�A�=qA�(�A��A�v�A���A��A�v�A�+A���A���A�E�A��A��FA���A�bNA���A��#A���A��;A�XA�VA��\A�Q�A��A���A��A�?}A���A�%A�=qA��TA���A�jA���A�A�A�+A��TA��A�dZA�33A��FA�ȴA���A�JA�A�A�XA��A�ȴA���A�ffA���A�x�A�Q�A�wA{�Ay�Av��As��Ap��AnZAl�`Ak�Ai��Ah��AgC�AeK�Ab�9A`ZA]C�A[�PAZbNAY��AX��AVn�ATn�AQ��AQ\)AP�`AO�AN1AMƨAM��AKXAJ=qAI��AH��AG�7AF$�AE��AE�PAE?}AD��AChsAA��A?��A>z�A>JA=/A;�A;�A:��A:I�A9�-A7`BA5`BA4n�A3A3%A2E�A0=qA/��A/l�A/�A-�A,��A,r�A+��A)�hA)/A(^5A'7LA&ZA%�-A#�;A"�RA"1'A!�
A!&�A ^5A JA�A�A�DAl�AVA1A�RA1A��AJAVA��A�^A�AoAO�Ar�A��AG�A(�A��AbNA��A%A �A
�\A	A�mAx�AC�A�A�A�TA�-AXAȴA9XA�A��A�/A��A��AȴAffA�A��A��A�9AbNA�TA�@��w@��@�ƨ@���@�O�@���@��j@�(�@��@�R@��@�@��@�7L@��T@с@�;d@��@�(�@���@�v�@��!@�&�@��F@�r�@��;@��j@�Z@�9X@�G�@���@��@�V@�+@��;@��@�@�@���@�@��@��;@�33@�5?@��h@�Z@�S�@�V@�z�@���@���@�@���@��F@�~�@�@���@���@�~�@�@�/@�Z@~�R@}@y�^@x��@u�@t(�@r~�@q��@pĜ@n��@n$�@k��@j~�@i%@h  @fV@d�@b��@bJ@_�@_\)@^@\��@Y%@X  @Vff@U/@S��@Q��@PA�@N@Lj@K��@K33@J-@Hr�@G�P@E��@EV@E�@CS�@B~�@@�@>v�@=V@;��@:�!@:J@8��@7\)@6�y@5p�@49X@333@2�H@2�\@1��@17L@0r�@.V@-�@,Z@+@*�\@*�@)G�@(�u@'\)@%�@$��@$I�@#�F@"�H@!��@!x�@!%@�@�R@�T@p�@�@ƨ@M�@�u@�;@�@V@�-@�/@I�@o@^5@��@%@b@+@@`B@�/@z�@ƨ@
�H@	�@	G�@Ĝ@r�@�@Q�@��@�@ff@@�@`B@9X@�@33@�H@�\@��@�7@ �9@  �?���?��H?��u?�Z?�&�?���?�?���?���?�$�?�`B?�33?���?��;?޸R?��?�"�?�=q?�1'?և+?�`B?�9X?�n�?щ7?�G�?У�?� �?�\)?���?Η�?�{?�O�?�j?�j?˅?�~�?�^5?�7L?�1'?���?�ȴ?�$�?š�?��?�9X?�S�?��?�&�?�A�?��;?���?�5??��-?�O�?��?�I�?���?�?�^5?��?���?��?��?�=q?�^5?���?��H?�dZ?�ƨ?�(�?��D?�/?���?�V?��?��?�\)?�\)?���?��w?��;?�  ?�  ?�A�?�A�?�bN?��?���?��?���?���?���?�ĜA�A�A�A�A�VA�oA�bA�bA�oA�{A�VA��A�oA�oA�1A�1A�%A�%A�%A�{A�JA�oA��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�(�A�(�A�(�A�(�A�+A�+A�-A�-A�-A�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A�bA��A��A��A��A��A� �A�(�A�+A�-A�/A�-A�/A�/A�1'A�33A�/A�1'A�33A�33A�5?A�5?A�(�A�&�A�$�A��A�bA��yAÉ7A�hsA��A���A�VA�$�A��mA�v�A��A�9XA�JA��RA���A���A�hsA���A�&�A�ĜA�C�A��A�ffA� �A��yA���A��+A�?}A��A�v�A��yA��!A�oA��9A�ffA� �A�l�A�=qA�(�A��A�v�A���A��A�v�A�+A���A���A�E�A��A��FA���A�bNA���A��#A���A��;A�XA�VA��\A�Q�A��A���A��A�?}A���A�%A�=qA��TA���A�jA���A�A�A�+A��TA��A�dZA�33A��FA�ȴA���A�JA�A�A�XA��A�ȴA���A�ffA���A�x�A�Q�A�wA{�Ay�Av��As��Ap��AnZAl�`Ak�Ai��Ah��AgC�AeK�Ab�9A`ZA]C�A[�PAZbNAY��AX��AVn�ATn�AQ��AQ\)AP�`AO�AN1AMƨAM��AKXAJ=qAI��AH��AG�7AF$�AE��AE�PAE?}AD��AChsAA��A?��A>z�A>JA=/A;�A;�A:��A:I�A9�-A7`BA5`BA4n�A3A3%A2E�A0=qA/��A/l�A/�A-�A,��A,r�A+��A)�hA)/A(^5A'7LA&ZA%�-A#�;A"�RA"1'A!�
A!&�A ^5A JA�A�A�DAl�AVA1A�RA1A��AJAVA��A�^A�AoAO�Ar�A��AG�A(�A��AbNA��A%A �A
�\A	A�mAx�AC�A�A�A�TA�-AXAȴA9XA�A��A�/A��A��AȴAffA�A��A��A�9AbNA�TA�@��w@��@�ƨ@���@�O�@���@��j@�(�@��@�R@��@�@��@�7L@��T@с@�;d@��@�(�@���@�v�@��!@�&�@��F@�r�@��;@��j@�Z@�9X@�G�@���@��@�V@�+@��;@��@�@�@���@�@��@��;@�33@�5?@��h@�Z@�S�@�V@�z�@���@���@�@���@��F@�~�@�@���@���@�~�@�@�/@�Z@~�R@}@y�^@x��@u�@t(�@r~�@q��@pĜ@n��@n$�@k��@j~�@i%@h  @fV@d�@b��@bJ@_�@_\)@^@\��@Y%@X  @Vff@U/@S��@Q��@PA�@N@Lj@K��@K33@J-@Hr�@G�P@E��@EV@E�@CS�@B~�@@�@>v�@=V@;��@:�!@:J@8��@7\)@6�y@5p�@49X@333@2�H@2�\@1��@17L@0r�@.V@-�@,Z@+@*�\@*�@)G�@(�u@'\)@%�@$��@$I�@#�F@"�H@!��@!x�@!%@�@�R@�T@p�@�@ƨ@M�@�u@�;@�@V@�-@�/@I�@o@^5@��@%@b@+@@`B@�/@z�@ƨ@
�H@	�@	G�@Ĝ@r�@�@Q�@��@�@ff@@�@`B@9X@�@33@�H@�\@��@�7@ �9@  �?���?��H?��u?�Z?�&�?���?�?���?���?�$�?�`B?�33?���?��;?޸R?��?�"�?�=q?�1'?և+?�`B?�9X?�n�?щ7?�G�?У�?� �?�\)?���?Η�?�{?�O�?�j?�j?˅?�~�?�^5?�7L?�1'?���?�ȴ?�$�?š�?��?�9X?�S�?��?�&�?�A�?��;?���?�5??��-?�O�?��?�I�?���?�?�^5?��?���?��?��?�=q?�^5?���?��H?�dZ?�ƨ?�(�?��D?�/?���?�V?��?��?�\)?�\)?���?��w?��;?�  ?�  ?�A�?�A�?�bN?��?���?��?���?���?���?�ĜA�A�A�A�A�VA�oA�bA�bA�oA�{A�VA��A�oA�oA�1A�1A�%A�%A�%A�{A�JA�oA��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�(�A�(�A�(�A�(�A�+A�+A�-A�-A�-A�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBBBBBBBB%B%B+B	7B6FBgmB�\BǮB��BBBJB�B�B�B�B �BD�BZBffBt�B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�?B�?B�XB�jB�jB�jB�jB�qB�wB�}B�jB�dB�RB�RB�FB�9B�3B��B��B��B��B�PB�1B�B~�Br�Bm�Be`B\)BO�BI�BD�B=qB<jB:^B5?B1'B'�B�B\B��B�BƨB��Bp�BM�B,B�BB
��B
�B
��B
�jB
�B
�{B
{�B
hsB
T�B
C�B
.B
�B	��B	�B	�TB	�
B	��B	ɺB	B	�3B	��B	��B	�7B	�B	x�B	s�B	iyB	]/B	N�B	B�B	=qB	:^B	33B	33B	/B	-B	 �B	�B	�B	{B	
=B	+B	B	B	B	  B��B�B�sB�fB�HB�)B�B�B��B��B��BĜB�wB�dB�LB�?B�B�B��B��B��B��B��B��B��B��B�{B�VB�PB�DB�B� B}�B~�B|�By�Bx�Bw�Bs�Bt�Br�Bn�Bq�Bp�Bl�Bm�Bk�Bl�Bm�Br�Bp�Bo�Bn�Bl�Br�Bs�Bt�Bs�Bu�Bs�Bq�Bo�BdZBVBI�BE�BD�BB�BA�B>wB?}B>wB<jB=qB<jB?}BF�BE�BE�BF�BN�BK�BD�BN�Bn�Bo�Bp�Bn�BjBiyBcTBiyBq�Bq�Br�Bt�Bt�Bo�Bk�Be`BdZBdZBZBN�BR�BQ�B[#B`BB\)Be`Bk�B{�B�PB��B��B�dBɺB��B��B�mB	
=B	#�B	#�B	0!B	9XB	[#B	{�B	�B	�1B	�VB	��B	��B	�B	�B	�3B	�?B	�jB	ÖB	ǮB	��B	��B	��B	�B	�BB	�NB	�`B	�yB	�B	�B	�B	��B	��B	��B
  B
  B
B
%B
1B

=B

=B
PB
PB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
%�B
&�B
(�B
(�B
)�B
-B
-B
0!B
1'B
1'B
2-B
33B
5?B
6FB
7LB
8RB
7LB
9XB
:^B
=qB
@�B
@�B
B�B
B�B
B�B
D�B
F�B
F�B
G�B
H�B
I�B
I�B
J�B
J�B
K�B
L�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
S�B
S�B
T�B
XB
XB
YB
YB
ZB
\)B
\)B
\)B
^5B
_;B
_;B
`BB
aHB
aHB
cTB
e`B
dZB
e`B
ffB
ffB
gmB
gmB
iyB
iyB
iyB
jB
k�B
l�B
m�B
m�B
n�B
o�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
u�B
w�B
x�B
y�B
z�B
z�B
z�B
z�B
{�B
|�B
|�B
~�B
� B
�B
�B
�%B
�7B
�DB
�PB
�PB
�PB
�VB
�bB
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
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�3B
�9B
�?B
�?B
�FB
�LB
�FB
�LB
�LB
�RB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�RB
�RB
�XB
�RB
�XB
�XB
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
�XBBBBBBBBBBB%BB%BBB%BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B BBBB	B6*BgRB�ABǓB��B �B�B2B�B|B�B�B �BD�BZBfRBt�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�4B�5B�5B�OB�aB�bB�bB�cB�kB�qB�xB�eB�`B�NB�OB�CB�7B�2B��B��B��B��B�QB�3B�!B~�Br�Bm�BedB\.BO�BI�BD�B=xB<qB:fB5GB10B'�B�BfB��B�(BƳB��Bp�BM�B,B�BB
��B
�B
��B
�yB
�#B
��B
{�B
h�B
UB
C�B
.%B
�B	�B	��B	�fB	�B	��B	��B	£B	�HB	�B	��B	�MB	�B	x�B	s�B	i�B	]GB	N�B	B�B	=�B	:xB	3MB	3NB	/6B	-*B	 �B	�B	�B	�B	
[B	JB	>B	2B	-B	 !B��B��B�B�B�kB�MB�4B�)B�B�B� B��B��B��B�tB�hB�DB�,B� B�B�B��B��B��B��B��B��B��B�B�tB�OB�1B~%B,B} BzByBxBs�Bt�Br�Bn�Bq�Bp�Bl�Bm�Bk�Bl�Bm�Br�Bp�Bo�Bn�Bl�Br�Bs�Bt�Bs�BvBs�Bq�Bo�Bd�BVEBI�BE�BD�BB�BA�B>�B?�B>�B<�B=�B<�B?�BF�BE�BE�BF�BO#BLBD�BO%Bn�Bo�Bp�Bn�Bj�Bi�Bc�Bi�Bq�Bq�BsBuBuBo�Bk�Be�Bd�Bd�BZwBO6BSRBROB[�B`�B\�Be�Bk�B|\B��B�B�aB��B�>B�B�bB��B	
�B	$iB	$lB	0�B	9�B	[�B	|�B	��B	��B	�B	�AB	�iB	��B	��B	��B	��B	�+B	�ZB	�uB	̑B	ѳB	��B	��B	�B	�(B	�=B	�YB	�tB	��B	�B	��B	��B	��B
 �B
 �B
B
 B
	/B
=B
@B
VB
XB
sB
}B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
'B
(B
*'B
*)B
+2B
.GB
.JB
1`B
2iB
2kB
3tB
4}B
6�B
7�B
8�B
9�B
8�B
:�B
;�B
>�B
A�B
A�B
C�B
C�B
C�B
FB
HB
HB
I&B
J/B
K7B
K:B
LDB
LFB
MOB
NXB
PfB
PiB
QrB
S�B
S�B
S�B
U�B
U�B
V�B
Y�B
Y�B
Z�B
Z�B
[�B
]�B
]�B
]�B
_�B
`�B
`�B
bB
cB
cB
eB
g-B
f)B
g2B
h;B
h=B
iGB
iJB
kYB
k[B
k^B
lfB
moB
nxB
o�B
o�B
p�B
q�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
x�B
w�B
y�B
z�B
{�B
}B
}B
}
B
}B
~B
B
"B
�3B
�@B
�RB
�lB
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
�B
�B
�"B
�'B
�:B
�@B
�RB
�_B
�kB
�rB
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
�B
�	B
�B
�B
�(B
�/B
�<B
�WB
�fB
�zB
��B
��B
��B
��B
��B
��B
�B
�B
�/B
�DB
�YB
�hB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�:B
�IB
�RB
�aB
�kB
�gB
�pB
�tB
�vB
�zB
�|B
�B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202442021061413551520210614135515202106171311432021061713114320210617131143201807242202442021061413551520210614135515202106171311432021061713114320210617131143PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024420180724220244  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024420180724220244QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024420180724220244QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145320210617131453IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                