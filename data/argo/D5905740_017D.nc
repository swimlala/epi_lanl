CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:48Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  PD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ´   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    $   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � $Argo profile    3.1 1.2 19500101000000  20180917231348  20210722161420  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�w@K���@�w@K���11  @�w@DDD`@�w@DDD`@*:8�C@*:8�C�cR_�Ë�cR_�Ë11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?���@   @@  @�33@�33@�  @�ffA33A��A$��A@  A^ffA~ffA�  A�  A�33A���A�ffA���A�  A�33BffB��B  B ffB(ffB0  B8  B@ffBHffBPffBX��B`��Bi33Bq33BxffB�  B�  B�33B�33B�33B�  B�  B�33B�33B�33B���B���B�  B�  B���B�33B�ffB�33B���B�ffB���Bԙ�B�33B�  Bߙ�B�33B���B�33B���B�ffB�  B���C 33C  C��C�CL�C
�C�3C  CL�C�C�CL�C  C  C�C33C ffC"  C$  C&33C(33C*L�C,�C.33C0L�C1�fC3�fC5�fC8  C:�C<�C>33C?��CA�fCD  CF  CH�CJ  CL�CN33CP33CQ�fCT  CV  CX�CZ33C\�C^33C`33Cb33Cd33Cf33Ch33Cj33Cl�Cn�Cp  Cr�Ct  Cv  Cw�fCy�fC{�fC}�fC��C��C��C�  C�  C�  C��3C�  C�  C��3C��C��C��C��C�  C��C��C��3C��C��C��3C��C��C��3C��C��C�  C�&fC��C�  C��3C��fC��C��C�  C�&fC��C��C��C�  C�  C��3C��C��C�  C��C��3C��C��C��3C��C��C��3C��3C��fC��C��C��C�  C��3C��C�  C��fC��C�&fC��C��3C��C��C�33C��C�  C��C�&fC��C��fC��3C��C��C�&fC��C��fC�  C��C��C�&fC��C��3C��3C��C��C��C��fC��C��C�&fC�&fC�  C��fC��fC��3C�  C�  C��C�  C��C�&fC��C��fC��3C��3C�  C��C��C��C��C��C��C��3C�  C��C��3C�ٚC��fC�  C�  D��DFfD	�DٚD� D�3D�3Dy�Dl�DL�D FfD#33D&  D)  D+ٚD.�fD1l�D4@ D7fD9� D<�3D?�fDB@ DEfDGٚDJ�fDMl�DP@ DSfDU�3DX��D[@ D]� D`y�Dc  De��DhS3Dj�3Dm�fDpfDr��Du�Dw� Dz,�D|S3D~ٚD��fD�  D�C3D���D��fD�fD�l�D��fD�  D�S3D��fD��fD�@ D���D��3D�33D���D��fD�#3D�|�D��3D�fD�` D��fD�� D�<�D��fD��fD��D�\�D���D��3D�6fD��fD�ɚD�3D�VfD��3D��D�<�D�|�D���D�fD�ffD���D��3D�FfD��3D��3D��D�P D�D��fD��D�\�Dǣ3D��3D�  D�Y�D̐ D�ɚD�3D�@ Dр Dҳ3D��D�  D�VfD׉�DضfD��fD��D�I�D�y�Dީ�D�ɚD��fD�&fD�L�D�s3D�fD�fD�ٚD���D�&fD�FfD�c3D��DD�ɚD��D�	�D�)�D�S3D�y�D���D��3D��3D�  D�	�D�,�D�S3D�vfD��fE X E �E~fE�E��E0 E� EL�E� Ec3E� E{3E	�E� E#3E�3E	33E	��E
D�E
ɚEffEx E�3EfE!�E0 E� E�fE>fE>fE�3E� E@ E8 E��E�fE VfE!` E"��E$3E%$�E&A�E'� E)3E*&fE+C3E,�fE. E/;3E0Y�E1s3E3!�E4;3E5T�E6� E83E9fE:�3E;��E>�3EA�EE3EH�EK EN� EQnfET� EW�fEZ� E^$�Ea;3EdS3Eg<�Ej�3EmnfEp��Es�3Ev��EzfE})�E�3E���E�G3E��fE�u�E��fE�y�E�.fE���E�=�E���E�X�E��3E��3E��E���E� �E���E�BfE��fE�a�E���E���E�3E��fE�"fE���E�S3E���E��fE�:fE�|�E�ݚE�3E�~f?�  ?fff?���?���?�  ?���?�  ?���?���?���?���?���?���?�ff?�ff?�ff?�33?�  ?ٙ�?ٙ�?�33@   @ff@   @,��@333@Fff@`  @l��@�33@���@���@���@�33@�  @���@ٙ�@�ff@�  @���AffAffA��A��A#33A,��A333A;33AC33AK33AS33AY��Aa��Ak33As33A{33A���A�  A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414141414144111411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?ٙ�@   @`  @�33@�33@�  @�ffA33A��A,��AH  AfffA�33A�  A�  A�33A���A�ffA���A�  B��B
ffB��B  B"ffB*ffB2  B:  BBffBJffBRffBZ��Bb��Bk33Bs33BzffB�  B�  B�33B�33B�33B�  B�  B�33B�33B�33B���B���B�  B�  B���B�33B�ffB�33B���B�ffB���Bՙ�B�33B�  B���B�33B���B�33B���B�ffB�  B���C �3C� CL�C��C��C
��C33C� C��C��C��C��C� C� C��C�3C �fC"� C$� C&�3C(�3C*��C,��C.�3C0��C2ffC4ffC6ffC8� C:��C<��C>�3C@L�CBffCD� CF� CH��CJ� CL��CN�3CP�3CRffCT� CV� CX��CZ�3C\��C^�3C`�3Cb�3Cd�3Cf�3Ch�3Cj�3Cl��Cn��Cp� Cr��Ct� Cv� CxffCzffC|ffC~ffC�&fC�L�C�L�C�@ C�@ C�@ C�33C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�@ C�Y�C�L�C�33C�Y�C�L�C�33C�Y�C�L�C�33C�Y�C�L�C�@ C�ffC�Y�C�@ C�33C�&fC�Y�C�L�C�@ C�ffC�Y�C�Y�C�Y�C�@ C�@ C�33C�L�C�Y�C�@ C�L�C�33C�Y�C�Y�C�33C�Y�C�L�C�33C�33C�&fC�Y�C�L�C�L�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�L�C�33C�L�C�Y�C�s3C�Y�C�@ C�L�C�ffC�L�C�&fC�33C�L�C�L�C�ffC�Y�C�&fC�@ C�L�C�Y�C�ffC�Y�C�33C�33C�L�C�Y�C�Y�C�&fC�L�C�L�C�ffC�ffC�@ C�&fC�&fC�33C�@ C�@ C�L�C�@ C�Y�C�ffC�L�C�&fC�33C�33C�@ C�L�C�Y�C�Y�C�L�C�L�C�Y�C�33C�@ C�Y�C�33C��C�&fC�@ C�@ D��DffD	,�D��D� D�3D�3D��D��Dl�D ffD#S3D&@ D)  D+��D.�fD1��D4` D7&fD:  D<�3D?�fDB` DE&fDG��DJ�fDM��DP` DS&fDU�3DX��D[` D^  D`��Dc@ DeٚDhs3Dk3Dm�fDp&fDr��Du9�Dw� DzL�D|s3D~��D��fD� D�S3D���D��fD�&fD�|�D��fD� D�c3D��fD�fD�P D���D��3D�C3D���D��fD�33D���D��3D�&fD�p D��fD�  D�L�D��fD��fD�)�D�l�D���D�3D�FfD��fD�ٚD�#3D�ffD��3D���D�L�D���D���D�&fD�vfD���D�3D�VfD��3D��3D��D�` D©�D��fD�,�D�l�Dǳ3D��3D�0 D�i�D̠ D�ٚD�3D�P Dѐ D��3D���D�0 D�ffDי�D��fD��fD�)�D�Y�D݉�D޹�D�ٚD�fD�6fD�\�D�3D�fD��fD��D��D�6fD�VfD�s3D��DD�ٚD���D��D�9�D�c3D���D���D��3D��3D� D��D�<�D�c3D��fD��fE ` E �E�fE�E��E8 E� ET�E� Ek3E� E�3E�E� E+3E�3E	;3E	��E
L�E
њEnfE� E�3EfE)�E8 E� E�fEFfEFfE�3E� EH E@ EɚE�fE ^fE!h E"��E$3E%,�E&I�E'� E)3E*.fE+K3E,�fE.  E/C3E0a�E1{3E3)�E4C3E5\�E7  E83E9fE:�3E;��E>�3EA��EE3EH�EK  EN� EQvfET� EW�fE[  E^,�EaC3Ed[3EgD�Ej�3EmvfEp��Es�3Ev��EzfE}1�E�3E���E�K3E��fE�y�E��fE�}�E�2fE���E�A�E���E�\�E��3E��3E�	�E���E�$�E���E�FfE��fE�e�E���E���E�3E��fE�&fE���E�W3E���E��fE�>fE���E��E�#3E��fG�O�?�33G�O�G�O�?�  G�O�?�  G�O�?���G�O�?���G�O�?���G�O�G�O�?�ff?�33@   G�O�@��@��@   @&ff@@  @L��@S33@fff@�  @�ff@�33@���@���@���@�33@�  @���@陚@�ffA   AffAffAffA��A$��A+33A4��A;33AC33AK33AS33A[33Aa��Ai��As33A{33A���A���A�  A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414141414144111411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ 1@ �@ {@ �@ "�@ (�@ 1'@ 7�@ >@ FQ@ Q�@ ^�@ l�@ z�@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ � @j@�@g@,`@:@H]@V@c�@r@�@��@��@��@��@@��@ލ@�4@�,@�@*@"�@0x@<�@Ji@X�@ff@s_@�d@��@��@��@�F@�J@խ@��@��@�9@
�@�@&;@2�@>�@N�@^5@k.@ww@��@�u@�z@��@��@�c@�@�`@�@@�@O@)�@7�@F�@Q�@_�@n�@|?@��@��@�5@��@�w@�@��@�@�q@@o@[@+�@:@G�@V@b�@qS@�@�P@��@�A@��@�>@є@ލ@��@��@1@�@#�@1'@>�@K�@Yn@ff@t�@��@�\@�U@��@��@�J@�C@��@�@��@
=@�@$�@33@@�@M�@]�@k.@x&@��@��@��@��@�@��@׹@�@�@ �@�@�@)�@6�@FQ@SI@_�@l�@y�@��@��@��@��@��@�*@��@�@��@	�@	�@	 @	,`@	:�@	F�@	V�@	dZ@	o�@	�@	��@	��@	��@	��@	��@	��@	ލ@	�@	�~@
�@
{@
 �@
0x@
?}@
K�@
X@
g@
uk@
�p@
��@
�@
�Y@
�^@
ƨ@
�C@
��@
�@
�E@J@B@$.@33@A�@O�@^5@k.@v�@�p@�u@��@�r@�^@�@׹@�@�e@  @J@�@(G@6�@DD@R�@_�@n�@|�@�7@��@�(@��@�&@�|@��@�y@�q@@o@
@,`@;d@F�@SI@a�@p�@~K@	�@R�@�a@��@:@��@�@(G@x�@�W@�@hs@�R@�@T�@�@��@:@��@Ӡ@ �@m�@�R@@Q=@��@�y@6�@�d@��@B@b�@��@�@:@�@�@�@V@�H@��@$�@i�@�r@�(@/@uk@��@ �@FQ@��@є@�@`�@�A@�@7�@�@��@�@V@��@�@ -�@ t�@ �w@!@!Lu@!��@!�h@"�@"e�@"�@"�@#8�@#}�@#�J@$
�@$O�@$��@$܀@%"�@%g�@%��@%�e@&<�@&�@&ȴ@'@'V�@'�U@'��@(*S@(qS@(��@(�~@)=q@)��@)�W@*�@*Q=@*��@*�#@+�@+a�@+��@+�@,)�@,m:@,�~@,�@-5@@-ww@-��@-�9@.;d@.|?@.��@.��@/?}@/�W@/��@/��@0>�@0}�@0�j@0��@17�@1v@1�9@1�@21'@2m�@2�f@2�(@3'�@3e	@3�z@3��@4g@4^5@4��@4�#@5�@5UU@5��@5�@6
�@6I@6�|@6@7 �@7@,@7~�@7�k@7�,@86�@8r�@8�@8�y@9%�@9`�@9��@9�t@:�@:O�@:��@:Ĝ@:��@;7L@;�l@<\)@<�*@=z2@=�4@>_�@?�@?v�@@�@@��@A-�@A��@B=q@B�A@CO1@C�w@Di!@D�t@E�+@E��@Fv@F�@G�A@H @H��@Io@I�@JG�@J��@K> @K�F@Lm�@L�@M^5@N@N�@N�}@O��@P�@Qn�@R��@To@U^5@V�A@X�@Y[z@Z�7@\	�@]m:@^�@`B@ak.@b�M@d�@eM�@f�M@h]@iZ�@j��@l@mQ=@n�k@p%@qM$@r�@tv@uO�@vě@xo@y`�@z��@|�@}e�@~�@�d@��!@�U�@��&@���@�Y�@� �@��@�Yn@��@��f@�O�@��E@��@��]@��_@��@�+@�TO@�pL@���G�O�@ %G�O�G�O�@ �G�O�@ �G�O�@ �G�O�@ �G�O�@ �G�O�G�O�@ �@ 	�@ 
=G�O�@ �@ �@ �@ V@ @ o@ @ *@ �@ B@ �@ 
@  �@ $.@ &;@ (�@ +�@ .l@ 1'@ 33@ 5�@ 9X@ <�@ ?}@ B�@ E�@ I�@ Lu@ O�@ SI@ V�@ Z@ \�@ `B@ dZ@ g�@ k.@ m�@ p�@ t�@ wwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��yA��`A��TA��;A��/A��`A��TA��TA��`A��A���A�ĜA�FA��A�DA�A�\)A�M�A�;dA�1'A�-A�-A�/A���A�ZA�9XA�A�
=Aݟ�A�G�A���A���A�XA�&�A�A���AفA��#A�\)AґhA�O�A�%A��A���A��
A��A�O�A�bNA��\A�A�%A���A�ZA��mA�jA��A��hA���Al�Az��Au�Ap��Am�
AfjAc�Abv�Aa�7A`VA^r�A[AWhsATZAP��AO�AMXAK��AGXAC�hACXAC/ACAB��AB�+A@1A?oA?�A?��AAK�A@��A@^5A>ZA<�!A:�yA8ZA5�A1��A0$�A/�A.Q�A-�PA,��A,�\A, �A+`BA+/A*��A*ZA)�hA(�HA'��A'S�A&��A&�A&��A&1A&VA%ƨA%O�A%S�A$��A#�A#|�A"�!A!�TA!p�A ~�A�-A"�A�RAn�A��A�9A�A�-A\)A7LA��Az�AE�A=qA-A��AC�A
=A�AĜAVAA�A5?A5?Ar�AM�A�A��A33A�A�`A~�A��A�FA��Ap�A+A�yA�uA�FA�AhsAAv�AI�A-A(�A  AAO�A�`AȴAĜA�jAz�A9XA�PA
=A�A�AdZAK�A`BAK�A��A�uA-A�#A��A
ȴA
M�A
$�A
-A	��A	��A	x�A	S�A��A�uA��AVA�TA;dAĜAA�A�^A|�A�Av�A�A`BA��A��AjAZA=qA-A�mA��A`BA%A ��A ��A �+A ~�A v�A �@�dZ@�=q@�`B@��/@��@�Q�@�1'@��m@�;d@��R@�M�@�`B@���@��D@�1@�S�@���@�E�@���@��@���@��@�S�@�o@���@�M�@�P@�K�@�=q@�r�@�G�@��#@Ӯ@�&�@�p�@��
@���@���@�7L@Õ�@�
=@��7@�b@�V@�
=@�x�@��
@�hs@��
@�ff@��u@��+@�  @��H@���@�(�@�@���@�dZ@��@���@���@�V@��@��R@�Ĝ@�dZ@�v�@��@��@��@���@�(�@���@��@���@�33@�5?@�%@���@�o@��T@��@�@}�@{S�@y�#@v��@tZ@rM�@o�w@n�+@m?}@kdZ@i�@g�w@fE�@dI�@b�H@a�@`b@^��@]�h@\�@Y�#@W�;@Vȴ@Tz�@R^5@P�@O�@M`B@K��@I��@HbN@GK�@Ep�@Ct�@A��@@1'@?l�@>E�@<�j@;�@:��@9�7@7�@7
=@5�@4�j@3@2n�@17L@/�@/�@.{@,�/@+33@*��@)7L@(A�@'+@&V@%�@$Z@#��@"�!@!�7@ �u@�P@��@�-@�/@�
@o@��@�@1'@|�@��@��@V@z�@�@o@-@7L@�@�@
=@5?@�h@��@�F@33@
�@	hs@��@r�@�w@l�@��@v�@�-@O�@�@��@��@dZ@"�@��@�@��@�?�|�?�{?���?�"�?���?��?�?�9X?�-?�7?�\)?�R?�/?�I�?���?���?��?�u?�
=?�+?��T?���?�Z?�F?��?�M�?�%?��?�  ?�\)?��?ݲ-?��?�(�?�C�?��H?�=q?�x�?��?�l�?�?���?���?�Ĝ?�;d?�{?�j?�?ə�?�Q�?�ff?��/?��?�Ĝ?���?�v�?�V?�I�?�dZ?�=q?��^?�r�?�K�?��y?�?�?}?�Z?�t�?�S�?���?��?�G�?�G�?��`?��`?�%?�hs?���?�-?���?���?�9X?��/?���?�ff?�
=?���?��u?��9?��?��?�7L?�X?�x�?��^A��mA��mA��;A��TA��/A��;A��;A��HA��TA��;A��;A��HA��TA��TA��`A��mA��mA��mA��yA��A��mA��yA��mA��`A��yA��TA��`A��`A��`A��HA��TA��;A��;A��#A��/A��/A��;A��yA��yA��`A��;A��;A��`A��mA��`A��HA��HA��A���A���A���A�A���A���A���A�ȴA�wA�jA�wA�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A��TA��yA��`A��TA��;A��/A��`A��TA��TA��`A��A���A�ĜA�FA��A�DA�A�\)A�M�A�;dA�1'A�-A�-A�/A���A�ZA�9XA�A�
=Aݟ�A�G�A���A���A�XA�&�A�A���AفA��#A�\)AґhA�O�A�%A��A���A��
A��A�O�A�bNA��\A�A�%A���A�ZA��mA�jA��A��hA���Al�Az��Au�Ap��Am�
AfjAc�Abv�Aa�7A`VA^r�A[AWhsATZAP��AO�AMXAK��AGXAC�hACXAC/ACAB��AB�+A@1A?oA?�A?��AAK�A@��A@^5A>ZA<�!A:�yA8ZA5�A1��A0$�A/�A.Q�A-�PA,��A,�\A, �A+`BA+/A*��A*ZA)�hA(�HA'��A'S�A&��A&�A&��A&1A&VA%ƨA%O�A%S�A$��A#�A#|�A"�!A!�TA!p�A ~�A�-A"�A�RAn�A��A�9A�A�-A\)A7LA��Az�AE�A=qA-A��AC�A
=A�AĜAVAA�A5?A5?Ar�AM�A�A��A33A�A�`A~�A��A�FA��Ap�A+A�yA�uA�FA�AhsAAv�AI�A-A(�A  AAO�A�`AȴAĜA�jAz�A9XA�PA
=A�A�AdZAK�A`BAK�A��A�uA-A�#A��A
ȴA
M�A
$�A
-A	��A	��A	x�A	S�A��A�uA��AVA�TA;dAĜAA�A�^A|�A�Av�A�A`BA��A��AjAZA=qA-A�mA��A`BA%A ��A ��A �+A ~�A v�A �@�dZ@�=q@�`B@��/@��@�Q�@�1'@��m@�;d@��R@�M�@�`B@���@��D@�1@�S�@���@�E�@���@��@���@��@�S�@�o@���@�M�@�P@�K�@�=q@�r�@�G�@��#@Ӯ@�&�@�p�@��
@���@���@�7L@Õ�@�
=@��7@�b@�V@�
=@�x�@��
@�hs@��
@�ff@��u@��+@�  @��H@���@�(�@�@���@�dZ@��@���@���@�V@��@��R@�Ĝ@�dZ@�v�@��@��@��@���@�(�@���@��@���@�33@�5?@�%@���@�o@��T@��@�@}�@{S�@y�#@v��@tZ@rM�@o�w@n�+@m?}@kdZ@i�@g�w@fE�@dI�@b�H@a�@`b@^��@]�h@\�@Y�#@W�;@Vȴ@Tz�@R^5@P�@O�@M`B@K��@I��@HbN@GK�@Ep�@Ct�@A��@@1'@?l�@>E�@<�j@;�@:��@9�7@7�@7
=@5�@4�j@3@2n�@17L@/�@/�@.{@,�/@+33@*��@)7L@(A�@'+@&V@%�@$Z@#��@"�!@!�7@ �u@�P@��@�-@�/@�
@o@��@�@1'@|�@��@��@V@z�@�@o@-@7L@�@�@
=@5?@�h@��@�F@33@
�@	hs@��@r�@�w@l�@��@v�@�-@O�@�@��@��@dZ@"�@��@�@��@�?�|�?�{?���?�"�?���?��?�?�9X?�-?�7?�\)?�R?�/?�I�?���?���?��?�u?�
=?�+?��T?���?�Z?�F?��?�M�?�%?��?�  ?�\)?��?ݲ-?��?�(�?�C�?��H?�=q?�x�?��?�l�?�?���?���?�Ĝ?�;d?�{?�j?�?ə�?�Q�?�ff?��/?��?�Ĝ?���?�v�?�V?�I�?�dZ?�=q?��^?�r�?�K�?��y?�?�?}?�Z?�t�?�S�?���?��?�G�?�G�?��`?��`?�%?�hs?���?�-?���?���?�9X?��/?���?�ff?�
=?���?��u?��9?��?��?�7L?�X?�x�?��^A��mA��mA��;A��TA��/A��;A��;A��HA��TA��;A��;A��HA��TA��TA��`A��mA��mA��mA��yA��A��mA��yA��mA��`A��yA��TA��`A��`A��`A��HA��TA��;A��;A��#A��/A��/A��;A��yA��yA��`A��;A��;A��`A��mA��`A��HA��HA��A���A���A���A�A���A���A���A�ȴA�wA�jA�wA�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	_;B	_;B	_;B	_;B	_;B	_;B	_;B	_;B	`BB	_;B	^5B	_;B	^5B	^5B	^5B	_;B	^5B	_;B	_;B	dZB	k�B	t�B	�B	��B	��B
B
1B
%�B
=qB
I�B
S�B
�B
��B
��B
�oB
�\B
�bB
�VB
�=B
� B
:^B	�;B	��B	�VB	�B
$�B
��B
�B
u�B
aHB
@�B
E�B
7LB
JB	�B	��B	�/B	�B	��B	�FB	��B	�\B	w�B	I�B	$�B	�B	DB	B��B�B�/B��B��B��B�FB�?B�B��BŢBɺBɺB��B��B�
B	DB	5?B	@�B	[#B	��B	ŢB	��B	��B	ŢB	�dB	�9B	��B	�JB	�\B	��B	��B	��B	�B	�B	�3B	�qB	ƨB	ƨB	ƨB	��B	��B	��B	�B	�/B	�sB	�B	��B
B
B
B
PB
JB
VB
\B
VB

=B
DB
DB

=B
DB
DB
	7B
+B
JB
bB
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
&�B
(�B
,B
-B
,B
,B
,B
.B
;dB
?}B
=qB
9XB
;dB
C�B
F�B
F�B
G�B
H�B
I�B
H�B
G�B
H�B
F�B
I�B
I�B
I�B
I�B
K�B
K�B
J�B
L�B
M�B
N�B
O�B
O�B
O�B
O�B
N�B
M�B
K�B
H�B
G�B
F�B
D�B
C�B
D�B
E�B
E�B
B�B
A�B
B�B
C�B
D�B
C�B
D�B
G�B
L�B
K�B
L�B
M�B
O�B
N�B
M�B
R�B
R�B
Q�B
M�B
L�B
I�B
G�B
F�B
C�B
>wB
=qB
;dB
:^B
:^B
9XB
9XB
8RB
9XB
7LB
8RB
7LB
7LB
6FB
5?B
5?B
5?B
49B
33B
33B
33B
49B
33B
33B
33B
2-B
1'B
/B
0!B
/B
0!B
/B
/B
.B
-B
.B
-B
,B
+B
)�B
(�B
(�B
'�B
�B
hB
bB
hB
oB
{B
\B
\B
uB
PB
JB
PB
DB
1B
bB
\B
B
B
1B
+B
1B

=B
DB
PB
VB
VB
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
#�B
$�B
%�B
)�B
+B
,B
/B
/B
1'B
1'B
33B
33B
49B
6FB
6FB
9XB
:^B
:^B
<jB
?}B
?}B
B�B
C�B
E�B
F�B
F�B
F�B
H�B
J�B
J�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
Q�B
Q�B
R�B
T�B
VB
W
B
XB
YB
YB
[#B
\)B
\)B
]/B
^5B
_;B
`BB
bNB
bNB
bNB
cTB
dZB
dZB
ffB
gmB
gmB
gmB
hsB
jB
jB
k�B
l�B
m�B
m�B
m�B
p�B
n�B
p�B
q�B
q�B
s�B
r�B
s�B
t�B
u�B
v�B
w�B
x�B
y�B
y�B
y�B
{�B
{�B
}�B
}�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�1B
�1B
�7B
�7B
�=B
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�VB
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�oB
�oB
�oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�3B
�9B
�9B
�?B
�FB
�LB
�LB
�RB
�XB
�^B
�^B
�dB
�jB
�jB
�qB
�}B
�}B
�}B
��B
��B
B
B
B
ÖB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ƨB
ƨB	_;B	^5B	_;B	_;B	\)B	^5B	^5B	_;B	^5B	_;B	`BB	_;B	_;B	_;B	_;B	_;B	^5B	_;B	_;B	_;B	_;B	_;B	_;B	_;B	^5B	`BB	^5B	_;B	^5B	_;B	_;B	_;B	^5B	_;B	^5B	_;B	`BB	_;B	^5B	_;B	^5B	`BB	_;B	_;B	^5B	_;B	]/B	_;B	^5B	^5B	^5B	_;B	_;B	_;B	^5B	^5B	^5B	_;B	_;B	]/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B	_B	_B	_B	_B	_B	_B	_B	_B	`B	_B	^B	_B	^B	^B	^B	_B	^B	_B	_B	d=B	kiB	t�B	��B	��B	οB
�B
B
%�B
=YB
I�B
S�B
�B
�qB
�lB
�[B
�HB
�OB
�CB
�+B
�B
:LB	�)B	ʭB	�BB	��B
$�B
�}B
�B
u�B
a4B
@oB
E�B
79B
7B	�lB	��B	�B	�B	��B	�4B	��B	�KB	w�B	I�B	$�B	vB	4B	�B��B�B�!B��B�uB�vB�9B�2B��B��BŖBɯBɯBʷB��B�B	<B	57B	@|B	[B	��B	ŝB	��B	�B	ŞB	�`B	�6B	��B	�GB	�ZB	�B	��B	��B	�B	�B	�4B	�rB	ƪB	ƫB	ƫB	��B	��B	��B	�B	�5B	�yB	�B	��B
B
B
"B
ZB
TB
aB
hB
bB

JB
QB
RB

KB
SB
SB
	GB
;B
[B
sB
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
' B
)B
, B
-'B
,!B
,"B
,#B
./B
;�B
?�B
=�B
9vB
;�B
C�B
F�B
F�B
G�B
H�B
I�B
H�B
G�B
H�B
F�B
I�B
I�B
I�B
I�B
K�B
K�B
J�B
L�B
M�B
OB
P	B
P
B
P
B
PB
OB
N B
K�B
H�B
G�B
F�B
D�B
C�B
D�B
E�B
E�B
B�B
A�B
B�B
C�B
D�B
C�B
D�B
G�B
MB
K�B
MB
NB
PB
OB
NB
S-B
S.B
R(B
NB
M
B
I�B
G�B
F�B
C�B
>�B
=�B
;�B
:�B
:�B
9�B
9�B
8�B
9�B
7�B
8�B
7�B
7�B
6�B
5�B
5�B
5�B
4�B
3}B
3~B
3B
4�B
3�B
3�B
3�B
2{B
1vB
/jB
0qB
/lB
0rB
/mB
/mB
.gB
-bB
.hB
-cB
,]B
+XB
*RB
)MB
)MB
(HB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
�B
B
B
B
B
.B
7B
AB
VB
`B
cB
lB
uB
�B
�B
!�B
"�B
#�B
$�B
$�B
%�B
&�B
*�B
+�B
,�B
0B
0B
2"B
2%B
45B
48B
5AB
7QB
7TB
:iB
;rB
;uB
=�B
@�B
@�B
C�B
D�B
F�B
G�B
G�B
G�B
I�B
K�B
K�B
NB
NB
OB
P"B
P%B
P(B
Q1B
SAB
SDB
TMB
V\B
WeB
XnB
YwB
Z�B
Z�B
\�B
]�B
]�B
^�B
_�B
`�B
a�B
c�B
c�B
c�B
d�B
e�B
e�B
g�B
iB
iB
iB
jB
l&B
l(B
m1B
n:B
oCB
oFB
oIB
r_B
pVB
reB
snB
spB
uB
t|B
u�B
v�B
w�B
x�B
y�B
z�B
{�B
{�B
{�B
}�B
}�B
�B
�B
�B
��B
��B
��B
�B
�B
�B
�B
�"B
�$B
�-B
�0B
�2B
�AB
�JB
�MB
�UB
�XB
�aB
�jB
�mB
�vB
�yB
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
�B
�B
�*B
�1B
�<B
�CB
�NB
�aB
�mB
�zB
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
�B
�B
�B
�B
�$B
�,B
�7B
�CB
�JB
�UB
�eB
�{B
��B
��B
��B
��B
��B
��B
��B
�B
�"B
�7B
�NB
�bB
�rB
��B
��B
��B
��B
��B
��B
��B
�B
�,B
�;B
�KB
�`B
�uB
ǌB
ǛB
ǪB
ȿB
��B
��B
��B
�	B
�B
�(B
�<B
�GB
�\B
�eB
�uB
˄B
̚B
ˣB
˲B
��B
��B
��B
��B
��B
��B
��B
��B
��B	_B	^B	_B	_B	\B	^B	^B	_B	^B	_B	`B	_B	_B	_B	_B	_B	^B	_B	_B	_B	_B	_B	_B	_B	^B	`B	^B	_B	^B	_B	_B	_B	^B	_B	^B	_B	`B	_B	^B	_B	^B	`B	_B	_B	^B	_B	]B	_B	^B	^B	^B	_B	_B	_B	^B	^B	^B	_B	_B	]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313482021061413574120210614135741202107221611412021072216114120210722161141201809172313482021061413574120210614135741202107221611412021072216114120210722161141PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134820180917231348  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134820180917231348QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134820180917231348QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216142020210722161420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                