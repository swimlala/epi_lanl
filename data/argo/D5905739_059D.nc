CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-11T20:09:31Z creation      
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
_FillValue                 @  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Q   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  b   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  fD   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  w<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  Ü   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   |   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        `   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190211200931  20210617131514  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               ;   ;DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ء����F@ء����F11  @ء���I@ء���I@6;����@6;�����c�I'�>��c�I'�>�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?���@��@@  @y��@���@�  @�  @���A  A(  AC33Aa��A�  A���A�  A�  A�  A�  A�33A���B33B��BffB  B   B(  B0ffB8  B@ffBI33BPffBX  B`  Bh  Bo��Bx  B���B�ffB�33B�33B�  B���B���B���B�33B�ffB�33B���B�  B�33B�  B�33B�33B�  BǙ�B�  B�33B�ffBؙ�B�ffB�ffB�  B�33B왚B�33B�B�  B�33C �CL�CffCL�C�C	��C��C��C�fC  C  C  C  C  C�C33C L�C"�C#��C&�C(�C*L�C,33C-�fC0�C2L�C433C6�C833C:�C;�fC>�C@L�CB�CD  CF33CH  CI�fCL�CN�CO��CR�CTL�CV33CX  CZ�C\L�C^33C_�fCb�Cd�Cf33ChL�Cj33Cl  Cm�fCp  Cr  Ct�Cv�Cx�Cz�C|�C~�C�  C�  C��C��C��C��C��C��C��C�&fC�&fC�  C��C�  C��C�33C��C�  C�  C��C��C�&fC��C��3C�  C��C�  C��fC�  C��C��C�  C��fC��3C��3C��C��C�  C��fC��3C�  C��C�  C��fC��3C�  C�  C��C�&fC�33C��C�  C��C�&fC��C��3C��C�  C��3C�  C��C��C��3C��C��C��C��fC��C��C�  C��fC�  C�&fC��C�  C��C��C��3C��C�&fC��C��3C��3C�  C��C�&fC��C��fC��fC�  C��C��C�&fC��C��fC��3C�  C��C��C��C��C��C�&fC�&fC�&fC��C�ٚC��C��C�  C�&fC�&fC�&fC�&fC�&fC�&fC�  C��C��C��fC��fC��3C��3C��3C�  C��C��C��D 3D ��D �3Dy�D  DY�D@ D	�fDY�D��Dy�D�fD@ D��D�fD  DY�D!��D#��D&�D(s3D*� D-fD/L�D1�fD4  D6FfD8��D:�fD=L�D?�fDBFfDD�3DGFfDI��DL33DN�fDQ�DS�fDV  DX�fD[�D]��D`�Db��De  DgffDi�fDl  Dny�Dp��Ds�DuY�Dw��Dy��D{�fD}ٚD�fD�#3D�9�D�VfD�y�D��fD�� D���D��fD�fD�@ D�i�D���D��3D��D��D�P D��fD��3D���D�9�D�s3D��fD�� D�fD�P D���D�� D���D�33D�i�D��3D�� D�� D�#3D�P D��3D��fD�� D�fD�)�D�P D�y�D��fD��3D��fD��fD�3D�0 D�P D�l�D��3D�� D���D��fD��3D�� D��fD�fD�3D�  D�,�D�6fD�6fD�<�D�<�D�<�D�9�D�33D�)�D�#3D� D�3D��3D�� D�ٚD�ɚD��fD�� D׼�Dة�D٠ Dړ3Dۓ3Dܓ3DݖfDޖfDߙ�D���D�3D�fD�fD��3D���D��fD���D�� D��3D��D��fD�� D�� D�� D���D�ٚD�ٚD�ٚD�� D��fD�� D��fD��3D��fD���D�|�D�ffD�VfD�9�D�&fD� D���E t�E�fEK3E;3E��E E��E	c3E
��E� E�fEP E�3E�fE EY�E��EњEfE33ET�E�fE�fEQ�E[3E��E �3E"d�E#�fE$��E%�fE'9�E(x E)� E*��E,H E-� E.� E0fE1NfE2�fE3��E4�fE6q�E7�fE8��E:!�E;.fE<��E?�3EB�fEE�fEH� EL EO0 ERVfEU�fEX� E[��E^�3Ea� Ed� Eh EkT�En|�Eo!�Eo��Epd�Eq�Eq�3ErC3Er�fEs~fEt�Et��EuNfEu� Ev~fEwfEw��Ex� Ey!�Ey� EzNfEz�E{t�E|L�E|�3E}d�E}� E~��ED�EɚE�H�E��fE���E�4 E�w3E��3E��E�}�E��fE��E�^f=���>L��>L��>L��>L��>L��>���>L��>L��>L��>���>L��>L��>���>L��>L��>L��>L��>L��>���>���>L��>���>���>���>���>���>���?   ?333?L��?���?���?�ff?�  ?�ff@ff@33@   @,��@@  @Y��@fff@s33@���@�33@���@���@�ff@�  @���@ٙ�@�ff@�ffA��A	��A  A  A   A(  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441444144144444144141441111141111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?L��?���@,��@`  @���@���@�  @�  AffA  A0  AK33Ai��A�  A���A�  A�  A�  A�  A�33A���B33B
��BffB  B"  B*  B2ffB:  BBffBK33BRffBZ  Bb  Bj  Bq��Bz  B���B�ffB�33B�33B�  B���B���B���B�33B�ffB�33B���B�  B�33B�  B�33B�33B�  Bș�B�  B�33B�ffBٙ�B�ffB�ffB�  B�33B홚B�33B���B�  B�33C ��C��C�fC��C��C
L�CL�CL�CffC� C� C� C� C� C��C�3C ��C"��C$L�C&��C(��C*��C,�3C.ffC0��C2��C4�3C6��C8�3C:��C<ffC>��C@��CB��CD� CF�3CH� CJffCL��CN��CPL�CR��CT��CV�3CX� CZ��C\��C^�3C`ffCb��Cd��Cf�3Ch��Cj�3Cl� CnffCp� Cr� Ct��Cv��Cx��Cz��C|��C~��C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�@ C�L�C�@ C�L�C�s3C�Y�C�@ C�@ C�L�C�Y�C�ffC�L�C�33C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�@ C�&fC�33C�33C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�@ C�&fC�33C�@ C�@ C�Y�C�ffC�s3C�Y�C�@ C�L�C�ffC�L�C�33C�Y�C�@ C�33C�@ C�Y�C�Y�C�33C�Y�C�Y�C�L�C�&fC�L�C�Y�C�@ C�&fC�@ C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�ffC�L�C�33C�33C�@ C�Y�C�ffC�L�C�&fC�&fC�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�ffC�L�C��C�L�C�L�C�@ C�ffC�ffC�ffC�ffC�ffC�ffC�@ C�L�C�L�C�&fC�&fC�33C�33C�33C�@ C�L�C�Y�C�Y�D 33D ��D3D��D  Dy�D` D	�fDy�D�D��DfD` D��DfD@ Dy�D!��D#��D&9�D(�3D*� D-&fD/l�D1�fD4  D6ffD8��D;fD=l�D?�fDBffDD�3DGffDIٚDLS3DN�fDQ,�DS�fDV  DX�fD[,�D]��D`9�Db��De  Dg�fDi�fDl@ Dn��Dp��Ds9�Duy�Dw��Dy��D{�fD}��D�fD�33D�I�D�ffD���D��fD�� D���D�fD�&fD�P D�y�D���D��3D���D�)�D�` D��fD��3D��D�I�D��3D��fD�� D�&fD�` D���D�� D�	�D�C3D�y�D��3D�� D�  D�33D�` D��3D��fD�� D�fD�9�D�` D���D��fD��3D��fD�fD�#3D�@ D�` D�|�D��3D�� D���D��fD��3D�  D�fD�fD�#3D�0 D�<�D�FfD�FfD�L�D�L�D�L�D�I�D�C3D�9�D�33D�  D�3D�3D�� D��D�ٚD��fD�� D���Dع�Dٰ Dڣ3Dۣ3Dܣ3DݦfDަfDߩ�D��D�3D�fD��fD��3D���D��fD���D�� D��3D���D��fD�� D�� D�� D���D��D��D��D�� D��fD�� D��fD��3D��fD���D���D�vfD�ffD�I�D�6fD�  E fE |�E�fES3EC3E��E E�E	k3E
ɚE� EfEX E�3E�fE  Ea�E��EٚEfE;3E\�E�fE�fEY�Ec3E��E �3E"l�E#�fE$��E%�fE'A�E(� E)� E+�E,P E-� E.� E0fE1VfE2�fE3��E4�fE6y�E7�fE8��E:)�E;6fE<��E?�3EB�fEE�fEI  EL  EO8 ER^fEU�fEX� E[��E^�3Ea� Ed� Eh  Ek\�En��Eo)�Eo��Epl�Eq�Eq�3ErK3Er�fEs�fEt!�Et��EuVfEu� Ev�fEwfEw��Ex� Ey)�Ey� EzVfEz�E{|�E|T�E|�3E}l�E}� E~��EL�EњE�L�E��fE���E�8 E�{3E��3E� �E���E��fE�!�E�bf?��G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?333G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�?333G�O�?L��G�O�G�O�?L��?fff?�  ?���?�ffG�O�?���?�ff@   @33@&ff@333@@  @L��@`  @y��@�33@���@���@�33@���@���@�ff@�  @���@陚@�ffA33A	��A��A  A   A(  A0  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441444144144444144141441111141111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ @ �@ @ {@ �@ !s@ (�@ /�@ 5�@ =q@ G�@ SI@ `B@ m:@ {�@ ��@ �0@ ��@ �~@ �w@ �|@ ܀@ �y@ �q@j@@�@-@:@H]@Wb@c�@p�@~K@��@��@�A@��@��@��@ލ@�@�~@%@@"�@1'@>@Ji@X�@g@t@�d@�@�@�M@�R@ƨ@��@�T@�L@��@
=@�@'�@3�@?}@N�@\�@j@y�@��@��@�@��@�^@�@�[@�@�Y@  @�@O@)�@7�@FQ@R�@^5@m�@{�@��@��@�(@�-@�2@�*@�#@�y@�q@�@�@ �@-@:@I@UU@bN@qS@~�@��@�H@�M@�F@@��@��@��@�~@�@*@#�@1�@>�@K@X@ff@t@�d@�@��@�Y@�@ƨ@Ӡ@�H@�@�E@
�@�@&;@4�@B8@P�@^5@i�@x&@�@�u@�(@�r@��@�c@׹@�@�e@ �@�@O@*S@6�@B�@Q�@`B@n�@z�@�+@��@�(@�-@��@��@�@�m@��@	@	@	[@	+�@	:@	G�@	V�@	e	@	s_@	�@	��@	�H@	�M@	��@	��@	є@	��@	��@	�,@
1@
�@
!s@
1'@
>�@
K�@
Wb@
g@
uk@
��@
��@
�@
��@
��@
��@
��@
��@
�@@
�E@J@�@$�@2�@@�@O�@^5@j@v@��@��@�@�r@��@�@խ@�@�Y@ �@V@�@*S@7�@FQ@S�@a�@m�@x�@�7@��@��@��@�2@��@܀@�(@��@j@�@g@+@8�@F�@T�@bN@p�@~�@�P@��@�M@��@�2@ψ@��@�m@m:@�-@�~@>�@�p@ƨ@�@F�@��@@�Q@<�@x�@��@��@6�@t�@��@�@33@qS@�r@�@1'@t�@�@��@A�@�p@�@
�@Lu@�@Ӡ@�@]�@�(@�m@*S@m:@��@�@/�@o�@�r@�@@+�@i!@�5@׹@�@O�@��@�@�@B�@�@��@�~@7�@uk@��@�e@1�@qS@��@�e@6�@x�@�j@�Q@ B�@ ��@ �W@!
=@!Lu@!�\@!�C@"{@"Wb@"�H@"܀@#�@#\)@#�@#ލ@$�@$`A@$��@$�H@% @%^5@%�@%܀@&B@&V@&�#@&є@'V@'K@'��@'�J@( �@(=q@(z3@(�F@(�@)(�@)`�@)��@)�O@*�@*F�@*�@*�F@*�@@+$�@+[z@+�i@+ƨ@+�9@,0x@,b�@,��@,�@,��@-1�@-e	@-��@-�7@.%@.8�@.m:@.�@.׹@/V@/E�@/|?@/��@/��@0"�@0Z@0�#@0�|@1%@1>�@1v�@1�@1�`@2[@2SI@2��@2�&@2��@3+�@3a�@3�<@3��@4j@47�@4m:@4��@4׹@5�@5?}@5o�@5��@5��@6v@67�@6i�@6�U@6��@7l�@8�@8k.@9@9�@:@:�a@;3�@;�0@<(G@<�R@=E�@=�@>UU@>ލ@?g@?�4@@o�@@�L@Ak�@B�@B�p@Cg@C��@D.l@D�4@EI�@Eȴ@FI�@Fψ@GYn@G�H@Hi�@H�e@I��@J�@J�0@K	@K��@L+�@L��@M/@M׹@NT�@N��@Oj@O�/@Pt�@Q��@S�@T^�@U�&@W{@Xff@Y�w@[�@\ff@]�^@_@`\)@a�@c�@dhs@e�2@f�@fM$@f�i@fխ@gB@g]�@g��@g�@h&;@hhs@h��@h�@i+�@il�@i��@j�@jK�@j��@j�@k
�@kI�@k��@k�@l[@lX�@l�!@l�(@m"�@mx&@m� @n�@n@�@nz2@nψ@o�@oZ@o�h@o�@p�@  �G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�@ G�O�G�O�@ @ �@ j@ �@ vG�O�@ �@ �@ 
=@ J@ V@ �@ @ o@ {@ 6@ �@ �@ [@ g@ !s@ $.@ &�@ (�@ +�@ .l@ 1'@ 4�@ 7L@ :�@ =q@ @�@ DD@ G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aã�AÝ�AÏ\AËDA�z�A�hsA�XA�S�A�M�A�I�A�E�A�?}A�;dA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�7LA�5?A�5?A�1'A�+A� �A�{A�1A���A��mA��/A��HA��TA��`A��mA��A��yA��HA��#A��#A��#A��#A��#A��#A��
A���A���A���A���A���A¥�A�A�O�A�5?A�VA�`BA�{A���A�v�A���A���A��wA��mA�5?A���A��RA�A�33A�\)A�^5A���A�dZA��hA�VA���A���A�oA���A�VA�v�A��`A���A�XA��A�(�A���A��A�l�A��A�|�A��;A��HA���A���A��DA��A��PA�A��TA��hA�ȴA��\A�ĜA���A��jA�{A�A��A���A��PA�oA�JA��9A���A�VA��A��A���A��
A�  A��RA�hsA���A~�yA|=qAy�-Av�`AuO�As7LAq�
Aop�Alv�Ak�FAi�^Ag/Afn�Ae��Ac��A`JA]�PA[��A[?}AZ~�AW��AS&�AOp�AN�`ANI�AL��AL�AJȴAJJAI��AI%AIAH��AH��AH�yAHA�AG�;AG�AFM�AE�-AE�AE7LAC��ABn�A@�A?�A?+A>��A=p�A<M�A;��A;�A:{A8ȴA7�A7?}A5S�A1
=A/��A/S�A/VA.��A.^5A.1A-l�A,�A,=qA+K�A*�uA*{A)p�A(��A(�A&�HA&A$n�A!ƨA�A|�A��A��A�hA
=A�mA+A=qA%A��A�7A?}AVAoA(�A`BAv�A
�9A
(�AȴA�uA$�A��A�A��AA�AffA/A J@��@�%@�Z@���@��@��9@�I�@�ƨ@�O�@�b@�"�@��m@�O�@�Q�@�R@�t�@���@�j@�=q@���@�1'@߾w@�\)@��@��@�K�@�
=@ٺ^@؃@ץ�@�o@�
=@���@֧�@�v�@�X@��
@�A�@���@�=q@�33@�\)@�Ĝ@��@�/@�Ĝ@���@���@�I�@��#@��j@�^5@�O�@��
@��@��`@��#@��@�X@�z�@�dZ@�t�@��H@�v�@���@��`@���@�ƨ@���@��@���@�Z@��@��@���@�\)@�+@�
=@��+@�ff@��^@��m@��@��@���@�w@z�\@y�#@x�`@x1'@w+@v��@u@q��@o�P@nv�@l��@l(�@j^5@h��@g;d@fE�@d��@d9X@c@b��@ax�@`b@^5?@]V@]p�@[33@Z=q@X  @U�-@UO�@Tz�@Q��@P�@O��@MO�@Kƨ@KS�@Jn�@H��@HbN@E�h@D9X@B�H@@�u@?�@>��@<�j@;33@9�@8A�@7l�@6E�@5p�@5?}@4�@4j@4(�@3t�@333@2��@1��@0��@/l�@.V@.$�@-@-V@+��@+S�@)�@(��@'�;@&V@%V@#��@"�\@  �@�@E�@O�@��@�m@t�@�!@=q@�@��@|�@��@$�@�h@/@��@�@n�@%@A�@��@ff@@�@j@9X@1@
�H@
J@	�7@	&�@�`@A�@�@\)@�@�R@�R@�y@V@��@�/@1@�m@t�@��@x�@�@ Q�?��R?��m?�=q?�Q�?�K�?��?�  ?��-?�j?�dZ?�^?�
=?�?���?�7?��`?�V?܋D?�dZ?��H?ٙ�?׮?�ȴ?ա�?�z�?�o?�n�?�-?�%?�bN?�;d?�v�?�p�?�V?�(�?��m?�dZ?�^5?��#?��?�X?�1'?Ǯ?ǍP?�?}?�33?�G�?�;d?�5??�(�?���?�x�?�X?���?�r�?��u?��9?��u?��u?���?���?���?���?���?��?�7L?�7L?�X?�x�?�x�?���?���?��^?��#?��^?��^?��#?��#?���?��?�=q?�=q?�^5?�=q?�^5?���?�~�?���?���?��H?�?�?�?�"�?�C�?�dZ?�dZ?���?��AÛ�Aß�AÝ�Aß�AÛ�Aß�Aá�Aá�Aß�Aá�Aá�Aã�Aá�Aá�Aå�Aß�Aã�Aã�Aã�Aã�Aå�Aç�Aç�Aç�Aé�Aç�Aç�Aá�Aá�AÝ�AÛ�AÛ�Aß�Aß�Aß�AÏ\AÍPAÑhAËDAÍPAÉ7AÉ7A�z�A�|�A�z�A�t�A�ffA�\)A�ZA�XA�XA�VA�O�A�O�A�K�A�K�A�I�A�G�A�I�A�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Aã�AÝ�AÏ\AËDA�z�A�hsA�XA�S�A�M�A�I�A�E�A�?}A�;dA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�7LA�5?A�5?A�1'A�+A� �A�{A�1A���A��mA��/A��HA��TA��`A��mA��A��yA��HA��#A��#A��#A��#A��#A��#A��
A���A���A���A���A���A¥�A�A�O�A�5?A�VA�`BA�{A���A�v�A���A���A��wA��mA�5?A���A��RA�A�33A�\)A�^5A���A�dZA��hA�VA���A���A�oA���A�VA�v�A��`A���A�XA��A�(�A���A��A�l�A��A�|�A��;A��HA���A���A��DA��A��PA�A��TA��hA�ȴA��\A�ĜA���A��jA�{A�A��A���A��PA�oA�JA��9A���A�VA��A��A���A��
A�  A��RA�hsA���A~�yA|=qAy�-Av�`AuO�As7LAq�
Aop�Alv�Ak�FAi�^Ag/Afn�Ae��Ac��A`JA]�PA[��A[?}AZ~�AW��AS&�AOp�AN�`ANI�AL��AL�AJȴAJJAI��AI%AIAH��AH��AH�yAHA�AG�;AG�AFM�AE�-AE�AE7LAC��ABn�A@�A?�A?+A>��A=p�A<M�A;��A;�A:{A8ȴA7�A7?}A5S�A1
=A/��A/S�A/VA.��A.^5A.1A-l�A,�A,=qA+K�A*�uA*{A)p�A(��A(�A&�HA&A$n�A!ƨA�A|�A��A��A�hA
=A�mA+A=qA%A��A�7A?}AVAoA(�A`BAv�A
�9A
(�AȴA�uA$�A��A�A��AA�AffA/A J@��@�%@�Z@���@��@��9@�I�@�ƨ@�O�@�b@�"�@��m@�O�@�Q�@�R@�t�@���@�j@�=q@���@�1'@߾w@�\)@��@��@�K�@�
=@ٺ^@؃@ץ�@�o@�
=@���@֧�@�v�@�X@��
@�A�@���@�=q@�33@�\)@�Ĝ@��@�/@�Ĝ@���@���@�I�@��#@��j@�^5@�O�@��
@��@��`@��#@��@�X@�z�@�dZ@�t�@��H@�v�@���@��`@���@�ƨ@���@��@���@�Z@��@��@���@�\)@�+@�
=@��+@�ff@��^@��m@��@��@���@�w@z�\@y�#@x�`@x1'@w+@v��@u@q��@o�P@nv�@l��@l(�@j^5@h��@g;d@fE�@d��@d9X@c@b��@ax�@`b@^5?@]V@]p�@[33@Z=q@X  @U�-@UO�@Tz�@Q��@P�@O��@MO�@Kƨ@KS�@Jn�@H��@HbN@E�h@D9X@B�H@@�u@?�@>��@<�j@;33@9�@8A�@7l�@6E�@5p�@5?}@4�@4j@4(�@3t�@333@2��@1��@0��@/l�@.V@.$�@-@-V@+��@+S�@)�@(��@'�;@&V@%V@#��@"�\@  �@�@E�@O�@��@�m@t�@�!@=q@�@��@|�@��@$�@�h@/@��@�@n�@%@A�@��@ff@@�@j@9X@1@
�H@
J@	�7@	&�@�`@A�@�@\)@�@�R@�R@�y@V@��@�/@1@�m@t�@��@x�@�@ Q�?��R?��m?�=q?�Q�?�K�?��?�  ?��-?�j?�dZ?�^?�
=?�?���?�7?��`?�V?܋D?�dZ?��H?ٙ�?׮?�ȴ?ա�?�z�?�o?�n�?�-?�%?�bN?�;d?�v�?�p�?�V?�(�?��m?�dZ?�^5?��#?��?�X?�1'?Ǯ?ǍP?�?}?�33?�G�?�;d?�5??�(�?���?�x�?�X?���?�r�?��u?��9?��u?��u?���?���?���?���?���?��?�7L?�7L?�X?�x�?�x�?���?���?��^?��#?��^?��^?��#?��#?���?��?�=q?�=q?�^5?�=q?�^5?���?�~�?���?���?��H?�?�?�?�"�?�C�?�dZ?�dZ?���?��AÛ�Aß�AÝ�Aß�AÛ�Aß�Aá�Aá�Aß�Aá�Aá�Aã�Aá�Aá�Aå�Aß�Aã�Aã�Aã�Aã�Aå�Aç�Aç�Aç�Aé�Aç�Aç�Aá�Aá�AÝ�AÛ�AÛ�Aß�Aß�Aß�AÏ\AÍPAÑhAËDAÍPAÉ7AÉ7A�z�A�|�A�z�A�t�A�ffA�\)A�ZA�XA�XA�VA�O�A�O�A�K�A�K�A�I�A�G�A�I�A�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BjBjBiyBjBjBhsBiyBiyBiyBiyBiyBiyBiyBiyBhsBiyBiyBiyBiyBiyBiyBhsBiyBiyBiyBjBl�Bo�Bq�Br�Br�Br�Bt�Bv�Bx�By�Bz�B|�B}�B~�B� B~�B~�B� B~�B~�B� B� B�B�B�B�B�B�B�+B�7B�=B�\B�3B�
B�HB��B(�B9XB9XB:^B:^BA�BB�BH�BG�BK�BM�BK�BL�BS�BR�BXB_;B\)BT�B^5B`BB^5B]/BXBQ�B �B	7B	7B	7B  B  B��B�yB�`B�HB�B��B��B�B��B�JBt�Bm�Bl�BaHBR�BI�B)�B+B
�B
ĜB
�^B
��B
�mB
�TB
ǮB
�B
r�B
v�B
e`B
_;B
\)B
I�B
?}B
.B
hB
  B	�B	�B	�HB	��B	��B	�^B	�B	��B	��B	��B	�7B	v�B	bNB	XB	P�B	J�B	6FB	�B	1B	B��B�B�B�B�B	B	+B	%B	%B	+B	DB	bB	{B	bB	\B	VB	JB		7B	B��B�B�B�yB�B�mB�TB�HB�5B�B��B��BǮB�jB��B��B��B��B��B��B�uB�PB�=B�B{�Bv�Bq�Bk�BhsBcTB\)B\)BS�BQ�BN�BL�BH�BC�BD�BA�B=qB<jB9XB7LB7LB7LB6FB5?B6FB2-B1'B+B1'B,B,B,B)�B(�B)�B&�B&�B(�B&�B%�B%�B$�B#�B#�B$�B#�B#�B#�B#�B#�B#�B#�B&�B%�B!�B'�B'�B(�B)�B,B,B-B-B.B33B49B33B33B49B6FB6FB6FB6FB6FB6FB>wBF�BJ�BO�BZBiyB{�B�=B�VB��B��B�qB�
B�B	%B	�B	0!B	7LB	?}B	G�B	R�B	bNB	l�B	t�B	�%B	�7B	�PB	�VB	��B	��B	��B	�B	�?B	�XB	��B	ɺB	��B	��B	��B	�#B	�;B	�;B	�HB	�ZB	�ZB	�fB	�B	��B	��B	��B	��B
+B
B
+B
1B
	7B

=B

=B
VB
\B
\B
hB
hB
oB
bB
hB
hB
uB
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
%�B
+B
-B
-B
-B
/B
2-B
2-B
5?B
6FB
6FB
6FB
9XB
9XB
:^B
8RB
:^B
<jB
=qB
@�B
@�B
C�B
C�B
D�B
E�B
H�B
I�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
K�B
L�B
M�B
M�B
N�B
O�B
P�B
P�B
R�B
S�B
W
B
XB
ZB
[#B
]/B
_;B
_;B
`BB
`BB
aHB
bNB
cTB
dZB
dZB
e`B
ffB
ffB
gmB
iyB
iyB
jB
jB
k�B
l�B
m�B
m�B
n�B
o�B
p�B
q�B
q�B
q�B
r�B
t�B
t�B
t�B
v�B
v�B
v�B
w�B
v�B
v�B
x�B
w�B
x�B
y�B
x�B
z�B
{�B
{�B
z�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�+B
�1B
�JB
�JB
�PB
�VB
�\B
�bB
�hB
�oB
�{B
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
�B
�B
�!B
�'B
�3B
�3B
�?B
�?B
�FB
�LB
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
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�RB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�XB
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dBk�BjBjBiyBjBjBjBjBjBjBiyBjBjBjBhsBjBk�BjBjBiyBiyBjBjBjBiyBjBjBiyBiyBiyBjBjBjBjBiyBiyBjBiyBiyBjBjBiyBiyBjBjBgmBhsBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBjBhsBiyG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     BjVBjVBiPBjVBjWBhKBiQBiRBiRBiRBiSBiSBiTBiTBhOBiVBiVBiWBiWBiXBiYBhSBiZBiZBi[BjbBlnBo�Bq�Br�Br�Br�Bt�Bv�Bx�By�Bz�B|�B}�B~�B�B~�B~�B�B~�B~�B�B�B��B��B��B�B�B�B�B�,B�2B�RB�)B�B�?B��B(�B9PB9QB:WB:WBA�BB�BH�BG�BK�BM�BK�BL�BS�BR�BXB_:B\)BT�B^6B`CB^7B]2BXBQ�B �B	;B	<B	<B B B��B�B�hB�PB�&B��B��B�&B��B�UBt�Bm�Bl�BaUBS BI�B*B:B
�-B
ĬB
�nB
��B
�B
�fB
��B
�1B
r�B
v�B
esB
_OB
\=B
I�B
?�B
.*B
~B
 B	��B	�B	�`B	��B	��B	�wB	�!B	��B	��B	��B	�RB	v�B	bjB	X,B	QB	J�B	6dB	�B	OB	%B��B��B��B�B��B	.B	MB	HB	HB	OB	hB	�B	�B	�B	�B	}B	rB		_B	5B�B��B�B�B�B�B�B�uB�bB�>B�B��B��B��B�,B�B��B��B��B��B��B��B�qB�MB|Bv�Bq�Bk�Bh�Bc�B\aB\aBT1BR%BOBMBH�BC�BD�BA�B=�B<�B9�B7�B7�B7�B6�B5B6�B2nB1hB+DB1iB,JB,KB,LB*@B);B*AB'/B'/B)<B'0B&*B&+B%%B$ B$ B%'B$"B$"B$"B$#B$$B$$B$%B'7B&2B"B(?B(@B)FB*MB,YB,ZB-`B-aB.hB3�B4�B3�B3�B4�B6�B6�B6�B6�B6�B6�B>�BG
BK&BPFBZ�Bi�B|WB��B��B�*B�vB��B׉B�B	�B	 EB	0�B	7�B	@B	H@B	S�B	b�B	m%B	uYB	��B	��B	��B	��B	�3B	�lB	��B	��B	��B	�B	�@B	�zB	ϜB	ҲB	��B	��B	�
B	�B	�B	�2B	�4B	�CB	�xB	��B	��B	��B	��B
B
B
B
	%B

.B
7B
9B
UB
^B
`B
oB
rB
{B
qB
zB
}B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
"�B
'B
,:B
.IB
.LB
.OB
0^B
3sB
3vB
6�B
7�B
7�B
7�B
:�B
:�B
;�B
9�B
;�B
=�B
>�B
A�B
A�B
EB
EB
FB
GB
J.B
K7B
J3B
K<B
LFB
LHB
LKB
LMB
LPB
KKB
M[B
NcB
OlB
OnB
PvB
QB
R�B
R�B
T�B
U�B
X�B
Y�B
[�B
\�B
^�B
`�B
`�B
a�B
b B
cB
dB
eB
f!B
f#B
g,B
h4B
h7B
i@B
kOB
kQB
lZB
l\B
meB
nmB
ouB
oxB
p�B
q�B
r�B
s�B
s�B
s�B
t�B
v�B
v�B
v�B
x�B
x�B
x�B
y�B
x�B
x�B
z�B
y�B
z�B
{�B
z�B
|�B
~B
~B
}B
�B
�)B
�3B
�@B
�GB
�WB
�eB
�rB
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
�B
�B
�4B
�-B
�@B
�?B
�LB
�^B
�fB
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
�B
�B
�
B
�B
�)B
�4B
�5B
�PB
�qB
��B
��B
��B
��B
��B
��B
�B
�#B
�8B
�GB
�VB
�fB
�|B
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
��B
��B
�B
�B
�B
�B
�Bk\BjVBjVBiPBjVBjVBjVBjVBjVBjVBiPBjVBjVBjVBhJBjVBk\BjVBjVBiPBiPBjVBjVBjVBiPBjVBjVBiPBiPBiPBjVBjVBjVBjVBiPBiPBjVBiPBiPBjVBjVBiQBiQBjWBjWBgEBhKBiQBiQBiQBiQBiRBiRBiRBiRBiRBiRBjXBhMBiSG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201902112009312021061413560520210614135605202106171314092021061713140920210617131409201902112009312021061413560520210614135605202106171314092021061713140920210617131409PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019021120093120190211200931  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019021120093120190211200931QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019021120093120190211200931QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151420210617131514IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                