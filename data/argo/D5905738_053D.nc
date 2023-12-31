CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-01-13T06:00:29Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   
�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	0      � 	0Argo profile    3.1 1.2 19500101000000  20190113060029  20210722160157  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               5   5DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؚ�2qF@ؚ�2qF11  @ؚ�'҃�@ؚ�'҃�@5�{�@5�{��c�� ѷ�c�� ѷ11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @Fff@�ff@�33@�  @�  A33A��A$��AA��Aa��A���A�  A���A���A���A�  A�ffA�33B   B  B  B��B ffB(ffB0ffB8ffB@ffBH��BPffBW��B_33Bg��Bp  Bx  B�  B�  B�33B�  B���B�  B�33B�33B�33B�  B���B�  B�33B�  B�33B�33B�33B�ffB���B�33B�33B�33B�33Bۙ�Bߙ�B㙚B�  B왚B�ffB�ffB�33B�  B���C�fC�C33C�C	�fC�C33C�C  C�fC�C33C�C  C�fC �C"L�C$L�C&33C(33C*33C,�C.�C0  C2�C433C633C833C:�C;��C>  C@33CB  CC��CF  CH33CJ  CK��CN  CP33CR�CS��CV  CX�CZL�C\�C]��C_��Ca��Cc�fCf  Ch  Cj  Cl  Cm�fCo�fCq�fCs�fCu��Cw�fCy��C{�3C}�3C�  C��C��C��C��C��C��C�  C�  C�  C�  C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��fC��C��C��C�&fC��C��C��C�  C��3C��fC��C��C��C��3C��3C��C�&fC��C��3C��C��C�  C��C�&fC��C�  C��fC��C��C��C��3C�  C��C��3C��C�&fC��C��C�  C��fC��C�33C��C��C��C��C�  C��3C��3C��C��C��C�&fC��C��C��C�  C�  C��3C�  C��3C��fC�  C��C��C�  C��3C�  C�&fC��C�  C��3C��C�&fC�&fC��C��C�  C��3C��C�&fC�  C��3C�  C��C��C��C�&fC��C�  C��C��C��3C�  C��C��C��3C�  C��C��3C��C��3C�ٚC��3C��C��3D� D�3D�D
33DFfDY�Dy�D��D�fD  D  DL�Dy�D�3D!��D$FfD&��D(��D+L�D-�fD/��D2@ D4y�D6�fD9�D;FfD=�fD?��DAٚDC��DF�DH9�DJS3DLY�DN` DP` DRl�DTs3DVy�DX�fDZ��D\��D_3DaS3Dc�3De� Dh&fDjs3Dl�fDn�3Dq33Dsy�Du� Dx�DzS3D|9�D~s3D�Y�D�y�D���D�� D��fD��fD��D���D�3D�#3D�<�D�I�D�Y�D�` D�ffD�p D�y�D��fD���D��fD�� D���D���D��3D���D��3D��3D�� D���D�� D�� D�� D�|�D�ffD�Y�D�FfD�33D��D�3D��D�� D���D�� D���D�� D�p D�\�D�C3D�33D�&fD��D�	�D��fD��fD��3D�� D�� D��3D�� D��3D�s3D�c3D�c3D�Y�D�P D�I�D�<�D�<�D�@ D�6fD�6fD�9�D�9�D�,�D�,�D�)�D�#3D�fD�fD��3D���D��3DЩ�Dѐ D�i�D�P D�,�D�	�D���D�� D׳3D؜�Dك3D�i�D�L�D�9�D� D���D߹�D���D�P D�)�D�3D幚D� D�P D�#3D�ٚD�fD�` D�,�D�� D� D�l�D�C3D�� D���D�c3D�0 D�� D�� D�  D��fE E[3E�3E�3E�ED�Et�E	�EfE� E�3E�3E�fEd�E��E� E1�E<�E�fE��E3Et�E� E��E"��E%�fE)3E,C3E/NfE2��E5vfE8�3E;� E<p E<��E=�3E>VfE>ٚE?�fE@#3E@ٚEAP EB�EB��EC.fEC� ED��EEfEE�fEFy�EF�EG� EHK3EH�EI�fEJ4�EJ�fEKA�EK�fEL� EM)�EM� ENi�EOfEO��EP[3EQ�EQ��ERQ�ER�3ES��ET.f>���>���>���>���>���>���>���>���>���>���>���>���>���?   ?   ?   ?   ?333?L��?�  ?�  ?���?�  ?ٙ�?�33@ff@��@,��@9��@S33@`  @y��@���@�  @���@���@�ff@�ff@�33@���@���A   A��A��A��A��A!��A(  A0  A6ffA>ffAD��AK33AQ��A[33Aa��Ah  Ap  Ax  A|��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141444444144411141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ?fff?�  @   @fff@�ff@�33@�  @�  A33A��A,��AI��Ai��A���A�  A���A���Ař�A�  A�ffA�33B  B
  B  B��B"ffB*ffB2ffB:ffBBffBJ��BRffBY��Ba33Bi��Br  Bz  B�  B�  B�33B�  B���B�  B�33B�33B�33B�  B���B�  B�33B�  B�33B�33B�33B�ffB���B�33B�33B�33B�33Bܙ�B���B䙚B�  B홚B�ffB�ffB�33B�  C ffCffC��C�3C��C
ffC��C�3C��C� CffC��C�3C��C� CffC ��C"��C$��C&�3C(�3C*�3C,��C.��C0� C2��C4�3C6�3C8�3C:��C<L�C>� C@�3CB� CDL�CF� CH�3CJ� CLL�CN� CP�3CR��CTL�CV� CX��CZ��C\��C^L�C`L�CbL�CdffCf� Ch� Cj� Cl� CnffCpffCrffCtffCvL�CxffCzL�C|33C~33C�@ C�Y�C�L�C�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�L�C�L�C�L�C�ffC�L�C�L�C�L�C�@ C�33C�&fC�L�C�Y�C�L�C�33C�33C�L�C�ffC�L�C�33C�L�C�L�C�@ C�Y�C�ffC�Y�C�@ C�&fC�L�C�Y�C�L�C�33C�@ C�L�C�33C�L�C�ffC�L�C�L�C�@ C�&fC�L�C�s3C�Y�C�Y�C�Y�C�L�C�@ C�33C�33C�L�C�L�C�L�C�ffC�L�C�L�C�L�C�@ C�@ C�33C�@ C�33C�&fC�@ C�Y�C�L�C�@ C�33C�@ C�ffC�Y�C�@ C�33C�L�C�ffC�ffC�Y�C�L�C�@ C�33C�L�C�ffC�@ C�33C�@ C�Y�C�L�C�L�C�ffC�L�C�@ C�L�C�L�C�33C�@ C�Y�C�L�C�33C�@ C�L�C�33C�Y�C�33C��C�33C�Y�D �D  D3D,�D
S3DffDy�D��D��D�fD  D@ Dl�D��D�3D"�D$ffD&��D)�D+l�D-�fD0�D2` D4��D6�fD9,�D;ffD=�fD?ٚDA��DD�DF9�DHY�DJs3DLy�DN� DP� DR��DT�3DV��DX�fDZ��D\��D_33Das3Dc�3Df  DhFfDj�3Dl�fDo3DqS3Ds��Du� Dx,�Dzs3D|Y�D~�3D�i�D���D���D�� D��fD��fD���D��D�#3D�33D�L�D�Y�D�i�D�p D�vfD�� D���D��fD���D��fD�� D���D���D��3D�ɚD��3D��3D�� D���D�� D�� D�� D���D�vfD�i�D�VfD�C3D�,�D�3D���D�� D���D�� D���D�� D�� D�l�D�S3D�C3D�6fD�)�D��D�fD��fD��3D�� D�� D��3D�� D��3D��3D�s3D�s3D�i�D�` D�Y�D�L�D�L�D�P D�FfD�FfD�I�D�I�D�<�D�<�D�9�D�33D�&fD�fD�3D���D��3Dй�DѠ D�y�D�` D�<�D��D���D�� D��3Dج�Dٓ3D�y�D�\�D�I�D�  D���D�ɚD���D�` D�9�D�3D�ɚD� D�` D�33D��D�fD�p D�<�D�� D�� D�|�D�S3D�  D�ɚD�s3D�@ D�� D�� D�0 D��fE  Ec3E�3E�3E�EL�E|�E	��EfE� E�3E�3EfEl�EɚE� E9�ED�E�fE�E3E|�E� E��E"��E%�fE)3E,K3E/VfE2��E5~fE8�3E;� E<x E=�E=�3E>^fE>�E?�fE@+3E@�EAX EB	�EB��EC6fEC� ED��EEfEE�fEF��EF��EG� EHS3EH��EI�fEJ<�EJ�fEKI�EK�fEL� EM1�EM� ENq�EOfEO��EPc3EQ	�EQ��ERY�ER�3ES��ET6fG�O�?L��G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�?�  ?���?�ffG�O�?�  ?ٙ�@   @��@��@&ff@9��@L��@Y��@s33@�  @���@���@�  @���@���@�ff@�ff@�33@���@���A  A��A��A��A!��A)��A0  A8  A>ffAFffAL��AS33AY��Ac33Ai��Ap  Ax  A�  A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141444444144411141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      @ �@ �@ �@ *@ �@ "�@ (�@ /�@ 7�@ >@ FQ@ R�@ `B@ m�@ z�@ �7@ ��@ �5@ �~@ ��@ �@ �t@ �@ ��@�@�@g@-@:�@H]@V�@c�@o�@|�@�D@��@�A@��@@��@��@��@�,@�@*@"�@/�@<�@K@Yn@ff@t�@�d@�@�a@��@�@ƨ@�O@��@�@�9@�@�@'�@4�@B8@O0@\)@i!@v�@��@�#@�@�f@�k@��@׹@�@�@ �@@�@(�@5�@D�@S�@a�@n�@|?@��@��@��@�~@��@�*@��@�y@�q@@@ @,`@8�@G�@V�@b�@oF@~K@�P@�H@��@��@�>@�C@ލ@�(@��@v@�@""@/�@=q@K@X@e�@s_@�@��@�U@�M@�F@��@Ӡ@�@�@��@
�@�@&;@33@@�@N�@\)@i!@v�@�@�@��@�f@�@ȴ@�[@�@�@��@V@�@)�@8�@D�@R�@`B@m:@z3@�+@��@�5@�-@�w@�@�#@�(@�q@	�@	�@	g@	,`@	;d@	I�@	V�@	b�@	oF@	~�@	�P@	�H@	��@	��@	�>@	ψ@	ލ@	�@	��@
�@
{@
 �@
0x@
@,@
Lu@
Z@
g�@
t�@
��@
��@
�U@
�Y@
�@
ƨ@
խ@
��@
�@
�E@
=@�@$�@33@@,@M$@\)@k.@x&@�@�@�m@�!@�@�c@�[@�`@�e@@@�@(�@5�@D�@S�@_�@l�@z�@��@��@��@��@��@��@�#@��@�@j@o@g@+�@:@H]@T�@dZ@o�@|?@�D@��@��@@I�@�@��@�q@/@i!@�(@ލ@O@UU@��@�@�@F�@��@�J@%@E�@��@�J@j@@,@~�@�@��@7L@s_@�f@�m@!s@[z@��@�@j@:@r@�M@��@�@SI@�P@�o@�@FQ@�@�>@@>@|�@�^@�~@6�@uk@��@�m@$.@a�@�@܀@�@SI@�P@�@�@>@x&@�9@�@'�@_�@��@�7@�@B8@z3@��@�@ $/@ [z@ �u@ �o@!@!:�@!p�@!��@!�t@"�@"@�@"s_@"�4@"�@#�@#>@#o�@#�@#�C@$j@$5�@$i�@$��@$��@%  @%2�@%c�@%��@%��@%��@&1�@&dZ@&��@&�@&��@'/�@'c�@'�0@'�@'�E@(0x@(g@(��@(�7@)v@)9X@)o�@)�A@)��@*o@*I�@*�W@*�9@*��@+ �@+V@+��@+�@+�@,!s@,R�@,��@,��@,�T@-{@-C�@-r�@-�(@-Ӡ@.@.5�@.g@.�<@.ȴ@.�9@/(�@/X@/�^@/�m@0G�@0v@0��@1@1/�@1�\@1�j@2�@2E�@2��@2ψ@3,`@3X�@3��@3�`@4@�@4k�@4ƨ@4�Y@5N�@5ww@6  @6�P@7{@7�a@8&�@8�r@93�@9��@:8�@:�#@;T�@;�Y@<g�@<��@=o�@>�@>��@?@?��@@�@@��@AFQ@A��@BS�@B�c@D+@Ey�@F�J@H�@I�@J�@L-@Ml�@Nє@P(�@Pff@P�z@P��@Q5�@Qm�@Q��@Q��@RH]@Rz�@Rƨ@S@SF�@S��@S��@T�@TbN@T��@T��@U,`@Uuk@U�k@V�@VFQ@V�C@V�@V�Q@WDD@W�7@W��@X�@XX@X�@X�@Y-@Yv@Y�k@Z]@ZD�@Z��G�O�@ G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�@ j@ �@ vG�O�@ �@ 1@ 
=@ �@ �@ V@ b@ o@ �@ �@ �@ �@ [@ �@  �@ $.@ &�@ *S@ -@ /@ 2�@ 6�@ 8�@ <@ ?}@ A�@ D�@ G�@ K@ M�@ Q=@ S�@ V�@ Yn@ ]�@ `B@ b�@ ff@ i�@ k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aǉ7AǅAǉ7Aǟ�AǬAǥ�Aǥ�Aǡ�Aǣ�Aǩ�AǺ^AǺ^AǸRAǺ^AǺ^AǼjAǼjAǼjA���AǸRAǬAǩ�Aǥ�Aǝ�AǗ�AǋDA�hsA�VA�/A� �A��A��A�bA�%A�  A��A��
A��A�XA�`BA�p�A��;A�(�A�7LA��hA��FA��`A�n�A���A�ȴA��A�A���A�G�A�ȴA��A��^A���A�Q�A��A���A���A��DA�$�A�oA�~�A�bA�`BA�JA�  A��`A���A�
=A��/A���A��-A��A��wA��TA�1'A�jA��A�I�A�G�A�~�A��A�  A�S�A���A�r�A�A�ffA��PA�M�A�|�A�ƨA�
=A���A�A���A��;A���A��mA���A��;A��A�+A�%A���A���A�
=A���A�&�A�O�AdZA~ZAz��Ay�hAx��AxI�Aw��AwVAuXAo�Al1'Ak�Ai��Ah��Ag�Ae��AdI�A_��A[�^AX�!ATjAS��AR  AQl�AP��AOO�AM7LAK��AH�HAF1ADbAB��AAƨA@jA?t�A>�DA<�+A<1A;�#A<5?A;��A;
=A9�
A8�A8~�A7t�A6�`A6  A5;dA4��A4jA4  A3VA2~�A29XA1%A.~�A,��A+�TA+VA*v�A)��A)%A(��A'��A&n�A%�TA%;dA$��A$�uA#�wA"��A!?}A $�A�DAK�A��A�A
=A�wA��A�AĜA5?A��AQ�A�A�A^5At�AffA��A"�AȴA�FA�9A��AO�A
ĜA
  A	�A	��A	�wA	��A	��AȴAE�A-A�A�;At�AoAQ�A�A�A�/A1A$�A�#A��A �\A �@���@���@�%@��y@��@���@�F@�J@@��@���@�(�@땁@�33@�!@�bN@���@�u@�V@���@��H@�{@ݑh@ܣ�@ۮ@��@��@�E�@��@�+@�p�@�(�@�-@�l�@���@��@��`@��@�Ĝ@��\@��`@���@��@���@�bN@�J@���@�G�@���@���@�;d@�G�@�  @��@��-@���@���@�/@�?}@���@�|�@�$�@�V@��@�V@��-@�Ĝ@��R@��h@��@�ff@�{@�%@�1'@���@��R@�O�@�@~$�@|��@{@y7L@x�`@w
=@u�@sƨ@r�H@q%@pr�@nE�@mV@k��@jn�@g��@f�@d�@d�@c��@c33@b�\@aX@_��@_
=@]�@\��@[C�@Z�@X�@V��@V�+@V$�@UO�@SS�@R�\@Q�^@O�;@O�w@N��@Mp�@K�m@J~�@JJ@I��@H�@E�T@D��@D1@Cƨ@C�@B��@@bN@?�@?K�@>ȴ@>�+@=O�@<Z@:��@8��@8 �@7�w@5��@4�/@3�m@2�!@1��@1x�@1&�@/�w@.�y@-�@,�/@,��@,j@,I�@+��@*M�@)%@'\)@';d@';d@&�y@&��@&$�@%O�@$9X@#��@#t�@#o@"��@"^5@!X@��@+@@@��@Z@��@-@�#@�9@  @�@`B@��@�
@��@Q�@1'@��@v�@��@�D@S�@dZ@33@@	7L@��@b@|�@V@�T@�D@C�@��@ b?��-?�I�?��u?���?��
?��?�%??�R?�V?�{?�?���?��#?�b?�?�j?�33?�A�?���?�5??�^5?�+?ӶF?ѩ�?�bN?�O�?˅?�=q?�r�?Ƈ+?�?š�?�`B?���?��
?���?���?öF?\?�&�?�%?��`?���?�A�?� �?� �?�A�?� �?�V?��?��-?��-?���?��D?��D?��?�j?�j?�j?�I�?�j?�j?��D?�(�?�(�?��m?�(�?�(�?�(�AǍPAǏ\AǏ\AǍPAǏ\AǏ\AǍPAǍPAǋDAǍPAǇ+AǇ+Aǉ7A�|�A�~�AǃAǃAǁAǇ+AǅAǅAǇ+AǇ+AǇ+Aǉ7AǋDAǋDAǙ�Aǝ�Aǟ�Aǥ�AǬAǰ!Aǩ�Aǥ�Aǣ�Aǥ�Aǥ�Aǡ�Aǟ�Aǡ�Aǡ�Aǣ�Aǥ�AǮAǶFAǺ^AǺ^AǺ^AǺ^AǺ^AǺ^AǺ^AǸRAǸRAǸRAǺ^AǸRAǺ^AǸRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Aǉ7AǅAǉ7Aǟ�AǬAǥ�Aǥ�Aǡ�Aǣ�Aǩ�AǺ^AǺ^AǸRAǺ^AǺ^AǼjAǼjAǼjA���AǸRAǬAǩ�Aǥ�Aǝ�AǗ�AǋDA�hsA�VA�/A� �A��A��A�bA�%A�  A��A��
A��A�XA�`BA�p�A��;A�(�A�7LA��hA��FA��`A�n�A���A�ȴA��A�A���A�G�A�ȴA��A��^A���A�Q�A��A���A���A��DA�$�A�oA�~�A�bA�`BA�JA�  A��`A���A�
=A��/A���A��-A��A��wA��TA�1'A�jA��A�I�A�G�A�~�A��A�  A�S�A���A�r�A�A�ffA��PA�M�A�|�A�ƨA�
=A���A�A���A��;A���A��mA���A��;A��A�+A�%A���A���A�
=A���A�&�A�O�AdZA~ZAz��Ay�hAx��AxI�Aw��AwVAuXAo�Al1'Ak�Ai��Ah��Ag�Ae��AdI�A_��A[�^AX�!ATjAS��AR  AQl�AP��AOO�AM7LAK��AH�HAF1ADbAB��AAƨA@jA?t�A>�DA<�+A<1A;�#A<5?A;��A;
=A9�
A8�A8~�A7t�A6�`A6  A5;dA4��A4jA4  A3VA2~�A29XA1%A.~�A,��A+�TA+VA*v�A)��A)%A(��A'��A&n�A%�TA%;dA$��A$�uA#�wA"��A!?}A $�A�DAK�A��A�A
=A�wA��A�AĜA5?A��AQ�A�A�A^5At�AffA��A"�AȴA�FA�9A��AO�A
ĜA
  A	�A	��A	�wA	��A	��AȴAE�A-A�A�;At�AoAQ�A�A�A�/A1A$�A�#A��A �\A �@���@���@�%@��y@��@���@�F@�J@@��@���@�(�@땁@�33@�!@�bN@���@�u@�V@���@��H@�{@ݑh@ܣ�@ۮ@��@��@�E�@��@�+@�p�@�(�@�-@�l�@���@��@��`@��@�Ĝ@��\@��`@���@��@���@�bN@�J@���@�G�@���@���@�;d@�G�@�  @��@��-@���@���@�/@�?}@���@�|�@�$�@�V@��@�V@��-@�Ĝ@��R@��h@��@�ff@�{@�%@�1'@���@��R@�O�@�@~$�@|��@{@y7L@x�`@w
=@u�@sƨ@r�H@q%@pr�@nE�@mV@k��@jn�@g��@f�@d�@d�@c��@c33@b�\@aX@_��@_
=@]�@\��@[C�@Z�@X�@V��@V�+@V$�@UO�@SS�@R�\@Q�^@O�;@O�w@N��@Mp�@K�m@J~�@JJ@I��@H�@E�T@D��@D1@Cƨ@C�@B��@@bN@?�@?K�@>ȴ@>�+@=O�@<Z@:��@8��@8 �@7�w@5��@4�/@3�m@2�!@1��@1x�@1&�@/�w@.�y@-�@,�/@,��@,j@,I�@+��@*M�@)%@'\)@';d@';d@&�y@&��@&$�@%O�@$9X@#��@#t�@#o@"��@"^5@!X@��@+@@@��@Z@��@-@�#@�9@  @�@`B@��@�
@��@Q�@1'@��@v�@��@�D@S�@dZ@33@@	7L@��@b@|�@V@�T@�D@C�@��@ b?��-?�I�?��u?���?��
?��?�%??�R?�V?�{?�?���?��#?�b?�?�j?�33?�A�?���?�5??�^5?�+?ӶF?ѩ�?�bN?�O�?˅?�=q?�r�?Ƈ+?�?š�?�`B?���?��
?���?���?öF?\?�&�?�%?��`?���?�A�?� �?� �?�A�?� �?�V?��?��-?��-?���?��D?��D?��?�j?�j?�j?�I�?�j?�j?��D?�(�?�(�?��m?�(�?�(�?�(�AǍPAǏ\AǏ\AǍPAǏ\AǏ\AǍPAǍPAǋDAǍPAǇ+AǇ+Aǉ7A�|�A�~�AǃAǃAǁAǇ+AǅAǅAǇ+AǇ+AǇ+Aǉ7AǋDAǋDAǙ�Aǝ�Aǟ�Aǥ�AǬAǰ!Aǩ�Aǥ�Aǣ�Aǥ�Aǥ�Aǡ�Aǟ�Aǡ�Aǡ�Aǣ�Aǥ�AǮAǶFAǺ^AǺ^AǺ^AǺ^AǺ^AǺ^AǺ^AǸRAǸRAǸRAǺ^AǸRAǺ^AǸRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B%�B(�B2-B6FB8RB9XB<jB?}BA�BG�BM�BK�BaHBy�B�7B�B�FBȴB�/B��BB%BoB�B+B7LB1'B7LBI�BO�BYBffBn�B{�B}�Bx�B�%B~�B�B�B�B~�B~�B|�B{�B{�Bx�Br�Bp�BjBe`B_;BXBO�BD�B33B&�B�B
=B�B��B�?B�B��Bu�BhsBVB8RB+B�BbB	7B
�B
�B
�B
�;B
��B
ǮB
�FB
��B
��B
��B
��B
�bB
�7B
�B
}�B
y�B
q�B
cTB
S�B
L�B
H�B
@�B
;dB
49B
�B	�B	�)B	��B	ĜB	�RB	�B	��B	�JB	\)B	A�B	1'B	{B	bB	1B	1B	+B��B�B�mB��B�^B�B��B��B�{B�\B�=B�1B�bB��B��B�-B�?B�B�B��B�B��B��B��B��B��B��B��B��B��B��B�bB�DB�%B�B�B� Bz�Bx�Bs�Bo�Bm�Bl�Bk�BhsBcTB]/B[#BW
BQ�BN�BM�BJ�BH�BE�BC�BA�BA�B?}B=qB<jB<jB;dB<jB;dB;dB9XB8RB6FB33B2-B-B-B,B.B,B,B/B0!B1'B2-B33B33B2-B33B33B0!B.B-B.B,B/B)�B/B.B0!B0!B/B)�B+B(�B(�B,B-B,B0!B1'B5?B6FB6FB6FB6FB6FB@�B?}BA�BG�BN�BO�BP�BW
BXBZB\)B^5BaHBiyBjBm�B~�B�B�PB��B��B�XBB�B�B��B	+B	{B	!�B	9XB	A�B	L�B	ZB	gmB	t�B	� B	�B	�+B	�DB	�\B	��B	��B	��B	�-B	�-B	�FB	�dB	ĜB	��B	��B	��B	�#B	�;B	�ZB	�sB	�yB	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
+B

=B
DB
JB
VB
\B
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
&�B
'�B
)�B
+B
-B
.B
-B
.B
/B
0!B
1'B
33B
5?B
49B
33B
5?B
7LB
8RB
8RB
:^B
<jB
=qB
>wB
>wB
>wB
>wB
?}B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
G�B
I�B
I�B
I�B
K�B
L�B
M�B
O�B
O�B
N�B
O�B
Q�B
Q�B
S�B
R�B
T�B
S�B
S�B
T�B
T�B
VB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
]/B
^5B
`BB
`BB
bNB
aHB
bNB
cTB
dZB
gmB
ffB
gmB
hsB
iyB
jB
k�B
l�B
k�B
o�B
n�B
o�B
o�B
p�B
r�B
s�B
s�B
s�B
s�B
v�B
v�B
w�B
w�B
x�B
y�B
{�B
}�B
� B
�B
�B
�B
�B
�%B
�7B
�7B
�DB
�JB
�JB
�JB
�PB
�PB
�VB
�\B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�3B
�9B
�9B
�9B
�9B
�?B
�9B
�9B
�9B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      B�B�B�B�B�B�B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B%�B/B33B5?B6FB9XB<jB>wBD�BJ�BH�B^5Bv�B�%B��B�3BŢB�B�B��BB\B�B'�B49B.B49BF�BL�BVBcTBk�Bx�Bz�Bu�B�B{�B�B� B}�B{�B{�By�Bx�Bx�Bu�Bo�Bm�BgmBbNB\)BT�BL�BA�B0!B#�B�B+B�yBȴB�-B��B�oBr�Be`BR�B5?B'�B�BPB%B
�B
�B
�B
�)B
��B
ĜB
�3B
��B
��B
��B
�oB
�PB
�%B
�B
z�B
v�B
n�B
`BB
P�B
I�B
E�B
=qB
8RB
1'B
�B	�B	�B	��B	��B	�?B	��B	��B	�7B	YB	>wB	.B	hB	PB	B	B	B��B�B�ZB��B�LB�B��B��B�hB�JB�+B�B�PB��B��B�B�-B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�PB�1B�B�B~�B|�Bw�Bu�Bp�Bl�BjBiyBhsBe`B`BBZBXBS�BN�BK�BJ�BG�BE�BB�B@�B>wB>wB<jB:^B9XB9XB8RB9XB8RB8RB6FB5?B33B0!B/B)�B)�B(�B+B(�B(�B,B.B.B0!B1'B1'B0!B1'B1'B.B,B+B,B)�B-B'�B-B,B.B.B-B'�B(�B&�B&�B)�B+B)�B.B/B33B49B49B49B49B49B>wB=qB?}BE�BL�BM�BN�BT�BVBXBZB\)B_;BgmBhsBk�B|�B� B�DB��B��B�LB��B�B�B��B	B	oB	�B	7LB	?}B	J�B	XB	e`B	r�B	}�B	�B	�B	�7B	�PB	�{B	��B	��B	�!B	�!B	�9B	�XB	B	ɺB	��B	��B	�B	�/B	�NB	�fB	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
1B
	7B

=B
JB
PB
\B
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
$�B
%�B
'�B
(�B
+B
,B
+B
,B
-B
.B
/B
1'B
33B
2-B
1'B
33B
5?B
6FB
6FB
8RB
:^B
<jB
=qB
=qB
=qB
=qB
>wB
@�B
@�B
A�B
A�B
A�B
B�B
C�B
F�B
H�B
H�B
H�B
J�B
K�B
L�B
N�B
N�B
M�B
N�B
P�B
P�B
R�B
Q�B
S�B
R�B
R�B
S�B
S�B
T�B
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
_;B
_;B
aHB
`BB
aHB
bNB
cTB
ffB
e`B
ffB
gmB
hsB
iyB
jB
k�B
jB
n�B
m�B
n�B
n�B
o�B
q�B
r�B
r�B
r�B
r�B
u�B
u�B
v�B
v�B
w�B
x�B
z�B
|�B
~�B
�B
�B
�B
�B
�B
�1B
�1B
�=B
�JB
�JB
�JB
�PB
�PB
�VB
�\B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�9B
�?B
�?B
�?B
�?B
�FB
�?B
�?B
�?B�B�B�B�B�B�B�B�B�B�BuB�B�B�B{B�B�B�B�B�B�B�B�B�B{B{B�B�B{B�B�B�B{B{B�B{B{BuB{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201901130600292021061413531020210614135310202106141747042021061417470420210614174704201901130600292021061413531020210614135310202106141747042021061417470420210614174704PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019011306002920190113060029  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019011306002920190113060029QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019011306002920190113060029QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015720210722160157IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                