CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-01-09T20:00:35Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  sX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20190109200035  20210722160157  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               4   4DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؙ���@ؙ���11  @ؙ��J�@ؙ��J�@5�,�zxl@5�,�zxl�c�6&,�s�c�6&,�s11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@Fff@�33@�ff@�33@���@���A  A)��AA��Aa��A���A���A�  A�  A���A���A���A�  B ffB��B��B  B   B(ffB0  B7��B@  BH��BPffBX  B`ffBhffBp  BxffB�33B�  B�  B���B�  B�  B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33Bϙ�B���B���B�  B�  B�33B�33B�ffB�33B�33B�  B���C �CL�C33C33C33C
�C�fC�fC�3C  C33C�C�fC�C33C�C �C"�C$  C&  C(  C)�fC,  C-�fC/�fC1��C3�fC5��C7�fC9�fC;�fC=�fC@  CA�fCD  CF  CH  CJ  CL  CM�fCO��CR�CTL�CVL�CX33CZ�C\  C]��C_�3Cb  CdL�Cf�Ch�Cj  Cl  Cn  Co�fCq�fCs�fCu��Cx  Cz33C|33C~�C�  C�  C��3C��3C��fC��C��C��C��C��C�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��3C��C�&fC��C��C��3C��fC�  C��C�  C��C�&fC��C��3C�  C��C��3C�  C��C��C��C�  C��C��C��C��3C��C��C�  C��fC�  C�  C��C��C��C��3C�  C�  C�  C��3C�  C��C�  C��fC�  C��C��3C��C��C��C�  C��fC��C�&fC��C��3C��3C��fC�  C��C��C�  C��3C��C�&fC��C��C��3C��C��C�  C��3C�  C��C��C��3C��fC�  C�  C��3C��C��C�  C��fC�  C��C�  C�  C�ٚC��C�  Ds3D�fD� D	  DffD�3D�3D9�Dl�D��D�3D��D�3DfD!&fD#FfD%ffD'� D)��D+�3D.  D0  D2S3D4��D6��D93D;S3D=� D?�fDB�DDL�DFy�DH��DJ�3DL��DO  DQFfDSffDUffDWy�DY� D[�fD]��D_� DaffDcL�De,�Dg  Di�Dj��DlٚDn�3Dp��Dr� Dt�fDv� Dx� Dz��D|FfD~33D�3D��D�fD���D��3D�� D�ٚD�� D���D��fD���D�y�D�l�D�S3D�C3D�6fD�&fD��D�  D��fD��3D��3D���D���D���D�vfD�c3D�I�D�33D��D�fD�� D��3D�ٚD��fD���D���D��3D���D���D���D���D���D���D���D���D���D��3D���D��3D�ɚD�ɚD�ɚD��fD��3D�ɚD�� D���D�� D�� D���D�ɚD�� D��3D�� D�� D�� D�|�D�ffD�S3D�<�D��D���D�ٚDƳ3Dǌ�D�` D�6fD��D���D˳3D̀ D�I�D��fDϓ3D�,�D���DӐ D�  D��D׉�D�L�D���DڶfD�S3D�� D޶fD�S3D��D�fD�|�D�#3D��3D� D�6fD�  D�3D�|�D�0 D�3D�D� D�l�D�9�D�3D��fD���D�ffD�9�D���D�� E )�Es3E�fE��E4�Ep E�3E� E
3EP E�fE!�EH Ei�E��E�fE�E��E��E��E@ Ea�E��E�E fE#)�E&�E)d�E,� E/�3E2�fE5�fE9  E<�E<��E=t�E=�fE>|�E?A�E?�fE@{3E@� EA� EB8 EB�EC� ED.fED��EEffEFfEF� EGL�EH�EH�3EIA�EI�fEJ~fEJ� EK�3ELI�EL�3EM�3EN9�EN�EO��EO��EP�fEQ>f>���>���>���>���>���>���?   ?   >���>���>���>���>���>���>���>���>���?   >���>���?   ?333?��?L��?�  ?���?�ff?�  ?�ff@   @��@   @333@Fff@`  @s33@�33@���@���@�ff@�  @���@ə�@�ff@�33@�33A   A	��A  A  A!��A)��A0  A9��AA��AK33AS33A\��Ad��AnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141444444414414111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?fff?�  @&ff@fff@�33@�ff@�33@���A��A  A1��AI��Ai��A���A���A�  A�  Ař�A���A���A�  BffB	��B��B  B"  B*ffB2  B9��BB  BJ��BRffBZ  BbffBjffBr  BzffB�33B�  B�  B���B�  B�  B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33BЙ�B���B���B�  B�  B�33B�33B�ffB�33B�33B�  B���C ��C��C�3C�3C�3C
��CffCffC33C� C�3C��CffC��C�3C��C ��C"��C$� C&� C(� C*ffC,� C.ffC0ffC2L�C4ffC6L�C8ffC:ffC<ffC>ffC@� CBffCD� CF� CH� CJ� CL� CNffCPL�CR��CT��CV��CX�3CZ��C\� C^L�C`33Cb� Cd��Cf��Ch��Cj� Cl� Cn� CpffCrffCtffCvL�Cx� Cz�3C|�3C~��C�@ C�@ C�33C�33C�&fC�L�C�Y�C�L�C�L�C�L�C�ffC�ffC�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�@ C�33C�L�C�ffC�L�C�L�C�33C�&fC�@ C�Y�C�@ C�L�C�ffC�L�C�33C�@ C�L�C�33C�@ C�L�C�L�C�Y�C�@ C�L�C�Y�C�L�C�33C�L�C�Y�C�@ C�&fC�@ C�@ C�L�C�Y�C�L�C�33C�@ C�@ C�@ C�33C�@ C�L�C�@ C�&fC�@ C�L�C�33C�L�C�Y�C�L�C�@ C�&fC�L�C�ffC�Y�C�33C�33C�&fC�@ C�Y�C�L�C�@ C�33C�L�C�ffC�Y�C�L�C�33C�L�C�Y�C�@ C�33C�@ C�Y�C�L�C�33C�&fC�@ C�@ C�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�@ C�@ C��C�Y�C�@ D�3D�fD  D	@ D�fD�3D3DY�D��D��D�3D��D3D&fD!FfD#ffD%�fD'� D)��D+�3D.  D0@ D2s3D4��D6��D933D;s3D=� D@fDB9�DDl�DF��DH��DJ�3DM�DO@ DQffDS�fDU�fDW��DY� D[�fD]��D_� Da�fDcl�DeL�Dg@ Di,�Dk�Dl��Dn�3Dp��Dr� Dt�fDv� Dx� Dz��D|ffD~S3D�#3D��D�fD��D�3D�� D��D�� D���D��fD���D���D�|�D�c3D�S3D�FfD�6fD�)�D� D��fD��3D��3D���D���D���D��fD�s3D�Y�D�C3D�,�D�fD�  D��3D��D��fD�ɚD���D��3D���D���D���D���D���D���D���D���D���D��3D���D��3D�ٚD�ٚD�ٚD��fD��3D�ٚD�� D���D�� D�� D���D�ٚD�� D��3D�� D�� D�� D���D�vfD�c3D�L�D�,�D�	�D��D��3Dǜ�D�p D�FfD��D���D��3D̐ D�Y�D��fDϣ3D�<�D���DӠ D�0 D���Dי�D�\�D���D��fD�c3D�  D��fD�c3D�,�D��fD��D�33D��3D� D�FfD� D��3D��D�@ D�3D�ɚD� D�|�D�I�D�#3D��fD���D�vfD�I�D���D�� E 1�E{3E�fE�E<�Ex E�3E� E
#3EX E�fE)�EP Eq�E��EfE�E��E��E��EH Ei�E��E��E &fE#1�E&$�E)l�E,� E/�3E2�fE5�fE9 E<�E<��E=|�E=�fE>��E?I�E?�fE@�3EA  EA� EB@ EB��EC� ED6fED��EEnfEF&fEF� EGT�EH�EH�3EII�EI�fEJ�fEK  EK�3ELQ�EL�3EM�3ENA�EN�EO��EP�EP�fEQFfG�O�G�O�G�O�?L��G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�?L��?fff?�  G�O�?���?�ff?�  ?ٙ�?�ff@   @33@   @,��@@  @S33@fff@�  @���@�33@���@���@�ff@�  @���@ٙ�@�ff@�33A��A  A��A  A   A)��A1��A8  AA��AI��AS33A[33Ad��Al��AvffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141444444414414111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ �@ V@ *@ �@ #�@ )�@ /@ 5?@ =q@ H]@ R�@ `B@ n�@ {�@ ��@ �0@ �5@ �-@ ��@ ��@ �#@ �m@ �@j@@g@,`@9X@G�@V�@c�@p�@~�@��@��@��@��@@�7@�/@�@�,@�@{@"�@0x@>@K�@Yn@g@t�@�d@�\@��@�Y@�@Ĝ@��@��@��@��@
�@�@&�@3�@A�@N�@[z@j@y�@�|@�#@��@��@�@ȴ@��@�@�@ �@�@�@*S@7L@D�@R�@_�@m:@z�@��@�0@�(@��@��@�@�@�m@�@�@b@�@+�@:@G�@UU@b�@p�@}�@��@�H@�M@��@��@��@��@�(@� @�@�@"�@0x@=q@K@X�@e�@s_@�@��@�@�@��@ƨ@Ӡ@�H@�@@��@�@�@&�@3�@A�@O0@^5@k�@x�@�|@�$@��@�r@�@��@׹@�@�@^@V@�@)�@7L@D�@R�@`B@m�@|?@��@�0@�(@�-@�2@�|@�#@�m@�e@	j@	o@	�@	-@	<@	H]@	T�@	b�@	qS@	}�@	��@	�H@	��@	�F@	@	��@	�;@	�4@	�~@
�@
�@
""@
.l@
=q@
K@
Yn@
g�@
t�@
�@
�\@
�@
��@
��@
��@
�O@
�H@
�@
��@
�@6@&;@4�@A�@N�@Z�@j@y�@�|@�@��@��@��@��@׹@�@�@ �@�@�@)�@5�@D�@SI@_�@l�@z�@��@��@�(@�!@�&@��@��@��@� @j@�@�@-�@:@G�@SI@dZ@p�@�(@&;@b�@�m@ލ@[@Z�@��@��@b@I�@�@��@�q@0x@j@��@��@B@S�@�\@�c@v@B8@�@��@�9@:@x&@�9@�L@+�@g�@�y@�/@�@R�@��@�>@��@33@j@��@�
@
�@>�@r@�A@��@@C�@y�@�r@�@�@N�@�@�@��@[@R�@��@�@�@&;@X�@��@@�@&�@X@��@�w@�@"�@V�@��@��@��@ @R�@��@��@��@ [@ O�@ �d@ ��@ �`@!6@!I@!z�@!��@!�T@"�@"I�@"}�@"�-@"�m@#
@#T�@#�D@#�2@#��@$/@$hs@$��@$׹@%b@%H]@%�W@%��@%�@&#�@&Yn@&�i@&�c@&�Q@'6�@'m:@'�(@'�@(�@(A�@(ww@(��@(��@)b@)B8@)t�@)��@)�\@*v@*5@@*c�@*�@*�&@*��@+�@+F�@+t�@+�m@+�o@, @,H]@,��@,ȴ@-""@-ww@-�z@-�9@.$�@.}�@.��@/ �@/X�@/�@/�#@0%@0]�@0��@0��@1
�@1ff@1��@1�@2H]@2s_@2�7@2�E@3Z�@3��@3��@4�@4H]@4��@4խ@54�@5a�@5�w@6�@6��@7;d@7�J@8P�@8խ@9\)@9�@:ff@:��@;p�@;�@<��@=""@=��@>�@>��@?-@?��@@H]@@�2@Ab�@Aލ@BYn@B�q@DQ=@E��@F��@HFQ@I�m@J��@LJi@M��@N��@P?}@P{�@Pխ@Q�@QFQ@Q�I@Qψ@R @RUU@R�A@R��@S-@S~K@S�9@Tv@T9X@T��@T��@U�@UWb@U�P@Uލ@V�@Ve�@V��@V�@W)�@Wn�@W�F@W�E@XD�@X��@X��@Y�@YF�G�O�G�O�G�O�@ G�O�@ �G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�@ @ �@ jG�O�@ @ v@ �@ 1@ �@ 
=@ J@ �@ @ @ @ *@ �@ �@ �@ 
@  �@ #�@ %�@ (G@ +@ -�@ 0x@ 3�@ 6�@ :�@ =q@ @�@ D�@ H]@ K@ O0@ R�@ V�@ Z@ ^5@ a�@ e�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�O�A�I�A�K�A�\)A�^5A�`BA�`BA�\)A�^5A�^5A�dZA�hsA�r�A�r�A�t�A�t�A�v�A�|�Ać+Aĉ7Ać+AčPAċDAčPAčPAčPAď\Aď\Aď\AđhAđhAēuAēuAĕ�Aĕ�Aď\AčPAčPAđhA�~�A�x�A���A�O�A��A��wA��jA�dZA�  A���A��+A�
=A�A�A���A�oA�1'A��A�{A�O�A���A�p�A��A�^5A��jA�`BA�  A��A�?}A�r�A�ƨA���A�-A���A�A�ƨA��-A���A�{A�dZA��A���A��mA���A���A��A�z�A�  A�9XA��-A�9XA�x�A��A�ĜA�\)A�1'A�|�A���A�O�A���A�`BA�G�A���A�n�A���A���A��/A�ȴA�{A�?}A���A��wA�^5A�+A�t�A�jA���A���A�A�A~��Az�Ax��Avv�Au�At�RAs��Ar��Aqx�AohsAl�HAi��AgG�Ae�-Ad$�AbbAa�A`ĜA_�A^��A]t�A\�!A[�PAZJAW�ASXAPffAO�mAM��ALAJZAH=qAGG�AE��AC��AA�mA@�jA=�#A;S�A9G�A8�A7A7��A7�A7O�A6��A5��A4��A4�+A3S�A2�DA1�TA133A0��A0v�A/�-A.��A-�#A,bA+O�A*�HA*��A*�+A*$�A(n�A'��A%�
A$5?A!�-A I�A =qA�#AffA��A�DA33A�
A�hA"�An�A1AC�A/A�hA�!AbA��A��A��A�A+A�A
^5A	S�A	VA��A�A�9A�AQ�AA�!A�A1Ao@��F@��-@�j@�~�@��`@�Q�@�V@��@�%@���@�n�@�G�@�bN@�\@��m@�^5@���@��@�+@��@��@�1@��;@�V@��m@�^5@ܼj@�I�@۾w@�C�@ڇ+@ف@ؼj@�dZ@�{@�x�@�~�@�  @��w@��@��^@���@�5?@��/@�j@��@�`B@���@�@�?}@�j@�33@��-@�A�@�9X@�@��\@���@���@�9X@���@�M�@���@��@�S�@�5?@��9@��y@�C�@�J@��@���@�t�@���@�M�@�p�@��w@�\)@�v�@��h@��@���@�dZ@�{@�/@�b@�dZ@���@��u@�t�@��@��@�-@��T@�Ĝ@��9@�w@~�+@}�@{��@y��@xA�@u��@t(�@r�H@p��@o�@lz�@j�@i�@f�+@e`B@c�@a�@aG�@`1'@_�P@^��@\�/@[C�@Z�\@Y��@X�9@W\)@V��@U�h@T��@TZ@T�@S"�@Q��@P�@O�;@O|�@N@M?}@Lj@J~�@I�#@I%@F��@FE�@D�@C��@B�H@A��@Ahs@@r�@@  @?��@?��@?+@>��@>ff@=V@<�@;"�@9��@7|�@6�R@5�@5`B@5/@4��@4z�@3dZ@2^5@0�9@0b@.�y@.ȴ@,��@,z�@*��@)X@(�`@(b@&ȴ@%�@$�@"�H@"M�@!�#@l�@|�@��@�h@j@�m@~�@7L@��@��@;d@�h@�/@��@�\@-@�@�@�;@��@�-@�@�F@
��@
��@	x�@	%@r�@\)@$�@��@�j@��@�#@ 1'?���?�V?���?��?�F?�G�?�w?��?��?��m?�"�?�~�?���?��?�
=?䛦?�F?�\?�Ĝ?���?�/?�1?ش9?�K�?��
?�G�?��?���?ɺ^?�K�?�?}?��
?�S�?���?\?�J?�J?�hs?�%?�bN?�A�?�  ?�|�?��?��?���?�V?�{?�{?��?���?��-?��?���?��?��D?�1?��?�dZ?�dZ?�?�^5?��#?��#?���?���A�I�A�M�A�I�A�K�A�I�A�M�A�VA�XA�ZA�^5A�ZA�VA�S�A�Q�A�S�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�S�A�Q�A�Q�A�O�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�M�A�M�A�S�A�^5A�bNA�^5A�^5A�\)A�^5A�bNA�bNA�\)A�ZA�\)A�\)A�^5A�^5A�`BA�^5A�^5A�^5A�dZA�hsA�jA�hsA�hsA�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�Q�A�O�A�I�A�K�A�\)A�^5A�`BA�`BA�\)A�^5A�^5A�dZA�hsA�r�A�r�A�t�A�t�A�v�A�|�Ać+Aĉ7Ać+AčPAċDAčPAčPAčPAď\Aď\Aď\AđhAđhAēuAēuAĕ�Aĕ�Aď\AčPAčPAđhA�~�A�x�A���A�O�A��A��wA��jA�dZA�  A���A��+A�
=A�A�A���A�oA�1'A��A�{A�O�A���A�p�A��A�^5A��jA�`BA�  A��A�?}A�r�A�ƨA���A�-A���A�A�ƨA��-A���A�{A�dZA��A���A��mA���A���A��A�z�A�  A�9XA��-A�9XA�x�A��A�ĜA�\)A�1'A�|�A���A�O�A���A�`BA�G�A���A�n�A���A���A��/A�ȴA�{A�?}A���A��wA�^5A�+A�t�A�jA���A���A�A�A~��Az�Ax��Avv�Au�At�RAs��Ar��Aqx�AohsAl�HAi��AgG�Ae�-Ad$�AbbAa�A`ĜA_�A^��A]t�A\�!A[�PAZJAW�ASXAPffAO�mAM��ALAJZAH=qAGG�AE��AC��AA�mA@�jA=�#A;S�A9G�A8�A7A7��A7�A7O�A6��A5��A4��A4�+A3S�A2�DA1�TA133A0��A0v�A/�-A.��A-�#A,bA+O�A*�HA*��A*�+A*$�A(n�A'��A%�
A$5?A!�-A I�A =qA�#AffA��A�DA33A�
A�hA"�An�A1AC�A/A�hA�!AbA��A��A��A�A+A�A
^5A	S�A	VA��A�A�9A�AQ�AA�!A�A1Ao@��F@��-@�j@�~�@��`@�Q�@�V@��@�%@���@�n�@�G�@�bN@�\@��m@�^5@���@��@�+@��@��@�1@��;@�V@��m@�^5@ܼj@�I�@۾w@�C�@ڇ+@ف@ؼj@�dZ@�{@�x�@�~�@�  @��w@��@��^@���@�5?@��/@�j@��@�`B@���@�@�?}@�j@�33@��-@�A�@�9X@�@��\@���@���@�9X@���@�M�@���@��@�S�@�5?@��9@��y@�C�@�J@��@���@�t�@���@�M�@�p�@��w@�\)@�v�@��h@��@���@�dZ@�{@�/@�b@�dZ@���@��u@�t�@��@��@�-@��T@�Ĝ@��9@�w@~�+@}�@{��@y��@xA�@u��@t(�@r�H@p��@o�@lz�@j�@i�@f�+@e`B@c�@a�@aG�@`1'@_�P@^��@\�/@[C�@Z�\@Y��@X�9@W\)@V��@U�h@T��@TZ@T�@S"�@Q��@P�@O�;@O|�@N@M?}@Lj@J~�@I�#@I%@F��@FE�@D�@C��@B�H@A��@Ahs@@r�@@  @?��@?��@?+@>��@>ff@=V@<�@;"�@9��@7|�@6�R@5�@5`B@5/@4��@4z�@3dZ@2^5@0�9@0b@.�y@.ȴ@,��@,z�@*��@)X@(�`@(b@&ȴ@%�@$�@"�H@"M�@!�#@l�@|�@��@�h@j@�m@~�@7L@��@��@;d@�h@�/@��@�\@-@�@�@�;@��@�-@�@�F@
��@
��@	x�@	%@r�@\)@$�@��@�j@��@�#@ 1'?���?�V?���?��?�F?�G�?�w?��?��?��m?�"�?�~�?���?��?�
=?䛦?�F?�\?�Ĝ?���?�/?�1?ش9?�K�?��
?�G�?��?���?ɺ^?�K�?�?}?��
?�S�?���?\?�J?�J?�hs?�%?�bN?�A�?�  ?�|�?��?��?���?�V?�{?�{?��?���?��-?��?���?��?��D?�1?��?�dZ?�dZ?�?�^5?��#?��#?���?���A�I�A�M�A�I�A�K�A�I�A�M�A�VA�XA�ZA�^5A�ZA�VA�S�A�Q�A�S�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�S�A�Q�A�Q�A�O�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�M�A�M�A�S�A�^5A�bNA�^5A�^5A�\)A�^5A�bNA�bNA�\)A�ZA�\)A�\)A�^5A�^5A�`BA�^5A�^5A�^5A�dZA�hsA�jA�hsA�hsA�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�#B�#B�#B�)B�#B�B�#B�B�#B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�B�)B�)B�HBB%BB�Bz�Bu�Bw�Bx�By�B}�B~�B�%B�+B�=B�=B�oB�{B�{B�uB�hB�oB�\B�\B�PB�JB�PB�%B�%B�B� B{�Bv�Bs�Br�Bo�BjBcTB]/BR�B=qB9XB5?B.B&�B �B�B��B�B�HB��BƨB�B��B��B�VB�1B|�Bp�Bt�Bq�Bk�B]/BK�B�B
��B
�NB
��B
��B
ȴB
��B
�B
��B
�DB
}�B
o�B
jB
G�B
5?B
"�B
uB
PB
B	��B	��B	�B	�)B	��B	�-B	��B	��B	�DB	v�B	n�B	hsB	`BB	T�B	L�B	E�B	9XB	(�B	uB��B�yB�ZB�B��BĜB�XB�-B�B��B��B�uB�DB�B|�B� B� B� B�B�+B�1B�+B�+B�B� B~�B{�B�B�B�%B�%B�1B�B~�B{�By�Bw�Bv�Bs�Bq�Bn�BgmB`BB\)BZBYBVBXBW
BS�BQ�BL�BH�BF�BB�BB�B<jB5?B33B1'B1'B1'B0!B.B+B,B&�B(�B(�B'�B%�B&�B)�B)�B+B(�B(�B%�B(�B+B+B-B,B+B-B,B.B2-B2-B1'B0!B0!B2-B2-B33B49B49B33B6FB9XB;dB<jBH�BJ�BL�BL�BL�BN�BN�BN�BN�BQ�BS�BW
BZBgmBiyBs�B�B�+B��B�!B�3BƨB��B�B��B	  B		7B	VB	�B	�B	#�B	33B	9XB	G�B	S�B	[#B	e`B	jB	q�B	t�B	y�B	�B	�B	�B	�7B	�PB	��B	��B	�3B	�LB	�^B	�jB	�wB	��B	ǮB	ȴB	��B	��B	��B	�B	�B	�#B	�;B	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
1B

=B
DB
VB
\B
uB
{B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
%�B
'�B
'�B
'�B
)�B
+B
-B
-B
-B
.B
/B
/B
1'B
1'B
33B
49B
5?B
5?B
8RB
8RB
9XB
;dB
<jB
>wB
=qB
?}B
?}B
@�B
A�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
E�B
G�B
J�B
J�B
L�B
J�B
K�B
K�B
L�B
M�B
N�B
P�B
P�B
Q�B
Q�B
S�B
S�B
W
B
YB
XB
YB
[#B
[#B
\)B
]/B
^5B
^5B
bNB
aHB
cTB
cTB
dZB
e`B
ffB
gmB
gmB
hsB
hsB
iyB
k�B
l�B
m�B
m�B
n�B
n�B
o�B
p�B
q�B
r�B
s�B
t�B
s�B
v�B
w�B
w�B
x�B
z�B
{�B
|�B
}�B
� B
�B
�B
�B
�B
�1B
�1B
�DB
�DB
�JB
�VB
�VB
�bB
�\B
�bB
�bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�!B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�9B
�?B
�9B
�9B
�?B
�FB
�FB
�?B
�FB�#B�#B�)B�#B�#B�B�#B�B�#B�B�#B�B�B�#B�B�#B�#B�#B�#B�#B�B�B�B�B�#B�#B�B�)B�#B�#B�#B�#B�/B�#B�#B�)B�#B�B�#B�B�B�#B�B�B�B�#B�B�B�B�B�B�B�B�#B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B�B�B�B�B�B�
B�B�
B�B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�
B�B�B�
B�
B�
B�B�
B�
B�
B�
B�
B�B�
B�B�B�5B��BB?}Bw�Br�Bt�Bu�Bv�Bz�B{�B�B�B�+B�+B�\B�hB�hB�bB�VB�\B�JB�JB�=B�7B�=B�B�B� B|�Bx�Bs�Bp�Bo�Bl�BgmB`BBZBO�B:^B6FB2-B+B#�B�BoB��B�B�5B��BÖB��B��B��B�DB�By�Bm�Bq�Bn�BhsBZBH�B�B
��B
�;B
��B
��B
ŢB
�qB
��B
�oB
�1B
z�B
l�B
gmB
D�B
2-B
�B
bB

=B
  B	��B	�B	�B	�B	ǮB	�B	��B	��B	�1B	s�B	k�B	e`B	]/B	Q�B	I�B	B�B	6FB	%�B	bB�B�fB�HB��B��B��B�FB�B��B��B��B�bB�1B� By�B|�B|�B|�B� B�B�B�B�B�B|�B{�Bx�B}�B�B�B�B�B�B{�Bx�Bv�Bt�Bs�Bp�Bn�Bk�BdZB]/BYBW
BVBR�BT�BS�BP�BN�BI�BE�BC�B?}B?}B9XB2-B1'B/B/B/B.B,B(�B)�B$�B&�B&�B%�B#�B$�B'�B'�B(�B&�B&�B#�B&�B(�B(�B+B)�B(�B+B)�B,B0!B0!B/B.B.B0!B0!B1'B2-B2-B1'B49B7LB9XB:^BF�BH�BJ�BJ�BJ�BL�BL�BL�BL�BO�BQ�BT�BXBe`BgmBq�B� B�B�{B�B�'BĜB��B�sB�B��B	+B	JB	{B	�B	!�B	1'B	7LB	E�B	Q�B	YB	cTB	hsB	o�B	r�B	w�B	� B	�B	�B	�+B	�DB	��B	��B	�'B	�?B	�RB	�^B	�jB	�}B	ŢB	ƨB	��B	��B	��B	��B	��B	�B	�/B	�HB	�TB	�sB	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B
B
B
%B
1B
	7B
JB
PB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
%�B
%�B
%�B
'�B
(�B
+B
+B
+B
,B
-B
-B
/B
/B
1'B
2-B
49B
49B
7LB
7LB
8RB
:^B
;dB
=qB
<jB
>wB
>wB
?}B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
D�B
F�B
I�B
I�B
K�B
I�B
J�B
J�B
K�B
L�B
M�B
O�B
O�B
P�B
P�B
R�B
R�B
VB
XB
W
B
XB
ZB
ZB
[#B
\)B
]/B
]/B
aHB
`BB
bNB
bNB
cTB
dZB
e`B
ffB
ffB
gmB
gmB
hsB
jB
k�B
l�B
l�B
m�B
m�B
n�B
o�B
p�B
q�B
r�B
s�B
r�B
u�B
v�B
v�B
w�B
y�B
z�B
{�B
|�B
~�B
�B
�B
�B
�B
�+B
�+B
�=B
�DB
�JB
�VB
�VB
�bB
�\B
�bB
�bB
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
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�'B
�'B
�-B
�'B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�?B
�FB
�?B
�?B
�FB
�LB
�LB
�FB
�LB�B�B�B�B�B�
B�B�
B�B�
B�B�
B�
B�B�
B�B�B�B�B�B�
B�
B�
B�
B�B�B�
B�B�B�B�B�B�B�B�B�B�B�
B�B�
B�
B�B�
B�
B�
B�B�
B�
B�
B�
B�
B�
B�
B�B�
B�
B�
B�
B�
B�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201901092000352021061413530920210614135309202106141747032021061417470320210614174703201901092000352021061413530920210614135309202106141747032021061417470320210614174703PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019010920003520190109200035  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010920003520190109200035QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010920003520190109200035QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015720210722160157IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                