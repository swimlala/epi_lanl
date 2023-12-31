CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-27T06:00:38Z creation      
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
_FillValue                 �  _P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ͐   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181127060038  20210722160155  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               -   -DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؑi�Y�@ؑi�Y�11  @ؑi�}<�@ؑi�}<�@5�Y5�;O@5�Y5�;O�c�I�^5?�c�I�^5?11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  >���?fff@ff@Fff@�33@�33@�33@���A   A  A$��AA��A`  A���A���A���A�  A�  A���A�  A�B ffB  B  B��B!33B(  B0  B7��B?��BG��BP  BX  B`  BhffBpffBx��B�33B�  B�  B�33B�33B�  B�33B�ffB�ffB�ffB�  B�  B�  B���B�  B�33B�ffB�ffB�33B�  B�  B�33B�  B�  B���B�  B癚B�ffB�ffB�33B�33B�  C   C  C  C  C�C
�C�C�C33C33C33C33CL�C�fC��C33C L�C"�C$33C&�C(  C*  C,33C.�C0�C2L�C433C5�fC833C:�C;�fC>33C@  CA�fCD33CF  CG��CJ�CK�fCM��CP�CR�CT  CU��CW�3CZ  C\33C^33C`  Cb33Cd�Cf  Ch33Cj�Cl  CnL�Cp33Cr�Ct  Cu�fCx�CzL�C|33C}�fC�  C��C��C�&fC�&fC��C��fC��fC��3C�  C��C��C��C��fC��3C�  C��C��C��C��3C��3C��C�  C��fC��3C��C��C��C��3C��C�  C��fC�  C�&fC��C��3C��C�  C��fC�  C��C��C�  C��C��C��3C��C�&fC�&fC��C�  C��3C��C��C�  C��C��C�  C��C�  C�  C��C�  C��fC�  C�&fC��C�  C��3C��fC��C�&fC�&fC��C�&fC��C��fC��3C��C��C�  C�ٚC��fC��fC��fC��3C��fC��3C��fC��C��C��C�&fC��C��C��C��C�  C�  C��3C��fC�  C�&fC�&fC��C��C�  C��3C��C�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC��C��fC��3C�  C��3C���D��D�fDS3D�D�fDy�D&fDٚDs3D3D� D   D"��D%9�D'� D*S3D,��D/�fD2` D5&fD7�fD:�3D=FfD?��DB��DEL�DG�3DJ��DML�DO�3DR��DU&fDW� DZ@ D\��D_` Da� Dd�fDg  Di� Dl&fDn�fDq&fDs��Dv,�Dx� D{  D}9�D�fD��D�Y�D��fD���D�3D�,�D�` D���D��3D��fD�  D�#3D�FfD�l�D���D���D��3D�fD�P D���D���D��3D�&fD�S3D���D���D�� D�33D�y�D���D�3D�P D��3D���D�6fD�� D�� D�3D�Y�D��3D��D�)�D�c3D�� D���D��D�VfD��3D��3D�� D��D�L�D�s3D���D��3D��D��D�0 D�I�D�i�Dɉ�Dʰ D�� D��3D���D� D�)�D�I�D�ffDӃ3DԦfD��3D���D���D�fD�0 D�I�D�` D�vfDޓ3Dߩ�D��3D��fD��fD���D�	�D��D�&fD�)�D�6fD�@ D�FfD�P D�\�D�l�D�3D� D�fD��D��D���D��3D���D���D���D�y�D�\�D�Y�D�Y�D�\�D�c3D�p E A�E ɚENfE� E^fE�Ei�E�fEp E� E��E�fE�fE
.fE33E�fE� E�fE|�E�fE��EH Ed�EvfE��E E!�E�fE� E>fED�E A�E!��E#6fE$,�E%� E&� E'� E)X E*D�E+��E- E.d�E/S3E0�fE2 E3d�E4NfE5��E6�3E8L�E9��E:|�E;��E?8 EB!�EEh EHNfEK� EN�fEQ��EU�EX�EZ� E^+3Ea|�Ed�3Eg�fEj� Em�fEp�fEt�EwfEzY�E}a�E�G3E��3E�w3E� E�� E�'3E���E���E�@ E���E���>���>���>���>���>���>���?   >���>���>���>���?   ?��?��?��?��?��?��?L��?�  ?�  ?���?�ff?�  ?ٙ�?�33@��@   @333@Fff@Y��@s33@�33@�  @�ff@�  @���@�  @���@�ff@�33@���@陚@�33A��AffA��A33A33A   A)��A0  A8  A>ffAD��AK33AQ��AY��A`  AfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444411414411444441141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?fff?�33@&ff@fff@�33@�33@�33@���A  A  A,��AI��Ah  A���A���A���A�  A�  A���A�  A���BffB
  B  B��B#33B*  B2  B9��BA��BI��BR  BZ  Bb  BjffBrffBz��B�33B�  B�  B�33B�33B�  B�33B�ffB�ffB�ffB�  B�  B�  B���B�  B�33B�ffB�ffB�33B�  B�  B�33B�  B�  B���B�  B虚B�ffB�ffB�33B�33B�  C � C� C� C� C��C
��C��C��C�3C�3C�3C�3C��CffCL�C�3C ��C"��C$�3C&��C(� C*� C,�3C.��C0��C2��C4�3C6ffC8�3C:��C<ffC>�3C@� CBffCD�3CF� CHL�CJ��CLffCNL�CP��CR��CT� CVL�CX33CZ� C\�3C^�3C`� Cb�3Cd��Cf� Ch�3Cj��Cl� Cn��Cp�3Cr��Ct� CvffCx��Cz��C|�3C~ffC�@ C�L�C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�L�C�33C�33C�L�C�@ C�&fC�33C�L�C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�Y�C�@ C�Y�C�L�C�33C�L�C�ffC�ffC�L�C�@ C�33C�Y�C�L�C�@ C�Y�C�L�C�@ C�Y�C�@ C�@ C�L�C�@ C�&fC�@ C�ffC�L�C�@ C�33C�&fC�L�C�ffC�ffC�L�C�ffC�L�C�&fC�33C�L�C�Y�C�@ C��C�&fC�&fC�&fC�33C�&fC�33C�&fC�Y�C�Y�C�L�C�ffC�Y�C�L�C�L�C�L�C�@ C�@ C�33C�&fC�@ C�ffC�ffC�Y�C�L�C�@ C�33C�L�C�ffC�ffC�L�C�L�C�Y�C�ffC�ffC�ffC�ffC�L�C�&fC�33C�@ C�33C��D��D�fDs3D,�D�fD��DFfD��D�3D33D� D @ D"��D%Y�D'� D*s3D-�D/�fD2� D5FfD8fD:�3D=ffD@�DB��DEl�DH3DJ��DMl�DP3DR��DUFfDW� DZ` D\��D_� Db  Dd�fDg@ Di� DlFfDn�fDqFfDs��DvL�Dx� D{@ D}Y�D�fD�)�D�i�D��fD���D�3D�<�D�p D���D��3D��fD� D�33D�VfD�|�D���D�ɚD��3D�&fD�` D���D���D�3D�6fD�c3D���D�ɚD�  D�C3D���D���D�3D�` D��3D���D�FfD�� D�� D�#3D�i�D��3D���D�9�D�s3D�� D���D�)�D�ffD��3D��3D�  D�,�D�\�D��3D���D��3D���D��D�@ D�Y�D�y�Də�D�� D�� D��3D��D�  D�9�D�Y�D�vfDӓ3DԶfD��3D���D��D�&fD�@ D�Y�D�p D݆fDޣ3D߹�D��3D��fD��fD��D��D�,�D�6fD�9�D�FfD�P D�VfD�` D�l�D�|�D�3D� D�fD��D��D���D��3D���D���D���D���D�l�D�i�D�i�D�l�D�s3D�� E I�E њEVfE� EffE�Eq�E�fEx E� E��E�fE�fE
6fE;3E�fE� E�fE��E�fE��EP El�E~fE��E  E)�E�fE� EFfEL�E I�E!��E#>fE$4�E%� E&� E(  E)` E*L�E+��E- E.l�E/[3E0�fE2 E3l�E4VfE5��E6�3E8T�E9��E:��E;��E?@ EB)�EEp EHVfEK� EN�fEQ��EU�EX�E[  E^33Ea��Ed�3Eg�fEj� Em�fEqfEt$�Ew&fEza�E}i�E�K3E��3E�{3E� E�� E�+3E���E� �E�D E���E���G�O�G�O�G�O�G�O�?L��?fffG�O�?L��G�O�G�O�?fff?�  G�O�G�O�G�O�G�O�G�O�?���?�ffG�O�?�  ?ٙ�?�ff@   @��@��@,��@@  @S33@fff@y��@���@�33@�  @�ff@�  @���@�  @���@�ff@�33@���@���A��A	��AffA��A33A#33A(  A1��A8  A@  AFffAL��AS33AY��Aa��Ah  AnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444411414411444441141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ �@ %@ V@ *@ �@ "�@ )�@ /@ 6�@ =q@ FQ@ R�@ _�@ m�@ |?@ ��@ �0@ ��@ �-@ �&@ �*@ �#@ �@ ��@�@@�@,`@9X@F�@T�@b�@p�@~K@��@�H@��@��@@�7@ލ@�4@�,@�@�@#�@1'@=q@K@X�@e�@t@�d@��@�a@�Y@�R@��@�O@�H@��@��@
=@�@&�@4�@A�@O0@\)@i�@ww@�@��@�@��@�k@�@�h@�@�@^@�@�@'�@7�@FQ@R�@`�@m�@z�@��@��@��@�-@�2@�*@��@�y@�q@�@o@�@+�@;d@G�@S�@c�@o�@|�@��@�H@�A@��@��@�7@�;@��@�,@1@*@""@1'@>@K@Z�@g�@t�@��@��@��@��@��@�J@Ӡ@��@�L@��@J@�@$.@1�@@,@N�@\�@k.@x&@��@�@�m@��@�@�@�[@�@�@  @J@�@)�@7�@D�@Q=@`B@m:@y�@��@�<@��@��@��@��@�@�@� @	�@	@	 @	-@	9X@	H]@	Wb@	e	@	qS@	~K@	�D@	��@	��@	��@	��@	��@	��@	��@	�,@
�@
*@
""@
.l@
=q@
M$@
Yn@
ff@
s_@
�W@
�@
�@
��@
�@
�@
�O@
��@
�@@
�E@�@�@#�@1�@?}@M$@[z@hs@v�@��@�$@��@��@��@��@׹@�`@�@  @�@�@'�@6�@FQ@S�@`�@m�@z�@��@��@��@��@��@�|@��@�(@��@v@@g@+@9X@G�@T�@`B@�@>@��@��@g@i!@�-@��@B�@��@�7@{@Z@��@�@+@s_@�j@�@R�@��@�@0x@z3@��@�@S�@�U@�@.l@uk@�j@j@G�@�P@Ӡ@�@`A@�A@�@0x@t�@�@��@B8@�@�c@�@H]@��@��@o@T�@��@�\@�@Wb@��@��@{@R�@��@ψ@�@M$@��@�*@ @ S�@ ��@ ׹@!B@!Yn@!��@!܀@"�@"c�@"�M@"�@@#3�@#z�@#�>@$	�@$O�@$�0@$��@%"�@%hs@%��@%�e@&8�@&{�@&�&@'�@'FQ@'��@'�|@(V@(N�@(��@(ψ@)V@)M�@)��@)�o@*	�@*G�@*��@*�2@*��@+=q@+z�@+��@+�@,,`@,hs@,��@,�@-g@-]�@-�H@-�\@.�@.O�@.��@.�@/j@/>�@/{�@/��@/�@0-�@0g�@0�(@0܀@16@1O�@1�+@1��@1�,@21'@2i�@2�(@2�/@3�@3Q�@3��@3��@3�~@4/@4g@4�U@4�C@5v@58�@5i!@5�@5խ@6�@6D�@6~K@6�@6�@7+�@7ff@7��@7�#@8�@8Ji@8��@8�R@9c�@9�[@:Ji@:�@;dZ@<b@<�@<�}@=��@>�@>�i@?DD@?��@@2�@@��@AQ�@A�>@Bl�@B��@C�p@C�e@D`B@E �@E�(@FJ@F��@G@G��@H@�@H��@I> @IӠ@Jhs@J�*@KbN@K��@L��@L�@@M�W@N@N�z@O1�@O�i@P[@Q�0@R�O@T:@Uv�@V�@X$�@Y��@Z�T@\+@]m:@^��@`5@@a�|@b��@d{@e�@f��@h'�@io�@j��@l�@mv�@n��@p/@q�@r�t@t/@u�@u��@u�,@vE�@vqSG�O�G�O�G�O�G�O�@ @ �G�O�@ G�O�G�O�@ �@ jG�O�G�O�G�O�G�O�G�O�@ @ vG�O�@ �@ 1@ �@ 
=@ �@ �@ @ @ @ *@ 6@ �@ �@ �@  @ ""@ $.@ %�@ (G@ *S@ -@ /@ 1�@ 3�@ 7L@ 9X@ <@ >�@ B8@ DD@ H]@ K@ N�@ Q=@ S�@ V�@ Yn@ \�@ _�@ bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AϸRA���A���A���A��yA��A��A��A��yA��yA��A��A��A��A��A���A��A��A��A���A���A���A���A���A�A��Aϲ-AϓuA�C�A�+A�1A��A��/Aΰ!A�?}A˃A�`BA�t�A�$�A��A��#A�ƨA���A��FA���A�"�A��A��\A���A���A�33A��!A��A�K�A���A��A��A��
A��mA���A��wA�I�A��A�ffA���A���A�%A��hA�5?A���A��^A�hsA�O�A���A�|�A��A�A�A�l�A�&�A��FA��`A�l�A�-A�dZA��hA�%A��A��HA�
=A�  A��yA��!A���A��A��+A���A�`BA��A�\)A���A���A���A�x�A�1A���A���A�oA�\)A��A���A��PA���A�?}A�9XA�=qA�A}��A{K�Az{AyAv�At�Asx�Ar��AqO�Ap1AnA�AlVAj��AidZAh�RAf�Ad��Abr�Aal�A`��A_�;A]|�AY��AX(�AW33AU�AS�AR�+AQ��AO�mAOXAN�uAMƨAJ��AI\)AH��AH�+AG��AG��AGK�AE�mAD��AC��AB��ABbAAhsA@�!A@{A?K�A>^5A<1'A9�A9&�A7�#A65?A4��A2�9A/��A.��A-oA*=qA(n�A'\)A&�yA&~�A%�A#�wA"��A"VA!�;A �RAK�A�A�AƨAS�A%A��AȴAJA^5At�AK�A?}A;dA+A��Az�A �AA�`A��A�RAƨA�7A\)A
ĜA
�A	��A�AdZAS�A�HA1AA�+A5?A�A�-AC�@��;@�7L@��@���@���@�o@�hs@�(�@��#@��@�@�F@��@ꗍ@���@�V@�V@㕁@���@ᙚ@�?}@�(�@��H@�~�@�-@ݑh@��@�Z@۝�@��@ڧ�@�G�@��m@�Q�@���@�~�@�A�@�b@��@�G�@�@��D@�G�@���@�ƨ@�-@���@��@���@�C�@�"�@�|�@�v�@���@��P@���@���@�V@�`B@��u@��
@�@��@��\@��h@�hs@��;@�M�@�&�@���@��j@�"�@��!@��@��/@���@�\)@��@�ff@���@�`B@~v�@y��@yG�@u@t�@tI�@p�9@oK�@m�T@l�D@k��@jJ@hb@gl�@e�@c�m@`�9@_�w@_�P@^�@Z�H@Y�7@W�@U��@SC�@QX@Pb@N�+@M��@LZ@I�^@H�9@H�9@F{@D��@C�F@B�H@@��@?�w@>V@=O�@<��@;dZ@:��@9�#@81'@7K�@5�T@4�j@4��@4j@4Z@3��@3t�@2��@17L@0 �@/�@.��@-/@,1@+@)�@(  @'l�@&��@%�-@$��@$Z@"~�@!7L@ Ĝ@|�@ff@��@��@�F@�@��@J@�^@�9@+@��@v�@O�@�@33@�\@hs@Ĝ@bN@b@+@?}@�m@
��@
M�@	�7@�@  @��@�P@\)@�@�y@ȴ@�R@�T@@p�@(�@�F@t�@@J@hs@7L@ ��@ �u?���?�(�?���?�b?���?�o?��?��?�;d?��?�?���?�b?�
=?�?��?�!?�7?��?ݑh?�1?ۥ�?���?ش9?׍P?�$�?�z�?��
?��
?�J?�&�?Ͼw?��?�V?�p�?̬?�ƨ?��H?���?�7L?ȴ9?��?Ǯ?Ǯ?�ȴ?š�?�S�?��?���?�p�?�O�?��?��D?�j?�ƨ?�"�?���?��?��^?���?��9?���?�7L?���?���?�=q?���?�dZ?�ƨ?�I�?��?�O�?��h?��-?���?��?��AϼjAϾwAϼjAϼjAϸRAϸRAϰ!Aϲ-Aϲ-Aϴ9Aϴ9A���A�ĜA���A���A�ȴA�A���A���A���A���A���A���A���A���A���A���A��A���A���A��
A��TA��A��A��A��A��A��A��A��A��A��A��A��A��mA��mA��mA��yA��mA��A��A��A��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                            AϸRA���A���A���A��yA��A��A��A��yA��yA��A��A��A��A��A���A��A��A��A���A���A���A���A���A�A��Aϲ-AϓuA�C�A�+A�1A��A��/Aΰ!A�?}A˃A�`BA�t�A�$�A��A��#A�ƨA���A��FA���A�"�A��A��\A���A���A�33A��!A��A�K�A���A��A��A��
A��mA���A��wA�I�A��A�ffA���A���A�%A��hA�5?A���A��^A�hsA�O�A���A�|�A��A�A�A�l�A�&�A��FA��`A�l�A�-A�dZA��hA�%A��A��HA�
=A�  A��yA��!A���A��A��+A���A�`BA��A�\)A���A���A���A�x�A�1A���A���A�oA�\)A��A���A��PA���A�?}A�9XA�=qA�A}��A{K�Az{AyAv�At�Asx�Ar��AqO�Ap1AnA�AlVAj��AidZAh�RAf�Ad��Abr�Aal�A`��A_�;A]|�AY��AX(�AW33AU�AS�AR�+AQ��AO�mAOXAN�uAMƨAJ��AI\)AH��AH�+AG��AG��AGK�AE�mAD��AC��AB��ABbAAhsA@�!A@{A?K�A>^5A<1'A9�A9&�A7�#A65?A4��A2�9A/��A.��A-oA*=qA(n�A'\)A&�yA&~�A%�A#�wA"��A"VA!�;A �RAK�A�A�AƨAS�A%A��AȴAJA^5At�AK�A?}A;dA+A��Az�A �AA�`A��A�RAƨA�7A\)A
ĜA
�A	��A�AdZAS�A�HA1AA�+A5?A�A�-AC�@��;@�7L@��@���@���@�o@�hs@�(�@��#@��@�@�F@��@ꗍ@���@�V@�V@㕁@���@ᙚ@�?}@�(�@��H@�~�@�-@ݑh@��@�Z@۝�@��@ڧ�@�G�@��m@�Q�@���@�~�@�A�@�b@��@�G�@�@��D@�G�@���@�ƨ@�-@���@��@���@�C�@�"�@�|�@�v�@���@��P@���@���@�V@�`B@��u@��
@�@��@��\@��h@�hs@��;@�M�@�&�@���@��j@�"�@��!@��@��/@���@�\)@��@�ff@���@�`B@~v�@y��@yG�@u@t�@tI�@p�9@oK�@m�T@l�D@k��@jJ@hb@gl�@e�@c�m@`�9@_�w@_�P@^�@Z�H@Y�7@W�@U��@SC�@QX@Pb@N�+@M��@LZ@I�^@H�9@H�9@F{@D��@C�F@B�H@@��@?�w@>V@=O�@<��@;dZ@:��@9�#@81'@7K�@5�T@4�j@4��@4j@4Z@3��@3t�@2��@17L@0 �@/�@.��@-/@,1@+@)�@(  @'l�@&��@%�-@$��@$Z@"~�@!7L@ Ĝ@|�@ff@��@��@�F@�@��@J@�^@�9@+@��@v�@O�@�@33@�\@hs@Ĝ@bN@b@+@?}@�m@
��@
M�@	�7@�@  @��@�P@\)@�@�y@ȴ@�R@�T@@p�@(�@�F@t�@@J@hs@7L@ ��@ �u?���?�(�?���?�b?���?�o?��?��?�;d?��?�?���?�b?�
=?�?��?�!?�7?��?ݑh?�1?ۥ�?���?ش9?׍P?�$�?�z�?��
?��
?�J?�&�?Ͼw?��?�V?�p�?̬?�ƨ?��H?���?�7L?ȴ9?��?Ǯ?Ǯ?�ȴ?š�?�S�?��?���?�p�?�O�?��?��D?�j?�ƨ?�"�?���?��?��^?���?��9?���?�7L?���?���?�=q?���?�dZ?�ƨ?�I�?��?�O�?��h?��-?���?��?��AϼjAϾwAϼjAϼjAϸRAϸRAϰ!Aϲ-Aϲ-Aϴ9Aϴ9A���A�ĜA���A���A�ȴA�A���A���A���A���A���A���A���A���A���A���A��A���A���A��
A��TA��A��A��A��A��A��A��A��A��A��A��A��A��mA��mA��mA��yA��mA��A��A��A��A��G�O�G�O�A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�BȴB�B�B�B�B�B�B�B�B�/B�/B�/B�BB�sBJB(�B.B;dB=qB>wB@�BA�B@�B=qBG�BL�Bp�Bx�Bv�Bv�B|�B}�B|�By�B�%B�B�B�B�+B�B�1B�1B�DB�DB�JB�PB�PB�PB�PB�bB�VB�JB�JB�=B�=B�%B�%B�B�B�B~�By�Bt�Bo�Bk�B_;BN�B@�B7LB,B(�B$�B�BJB%B��B�B�`B��BɺBÖB�RB��B��B�bBw�Bn�BiyBT�B2-B)�B!�B	7B
��B
�B
�5B
��B
ŢB
�FB
��B
��B
�PB
q�B
[#B
=qB
,B
{B
	7B	��B	�yB	�B	�B	��B	B	�^B	�B	��B	�{B	�DB	�B	t�B	n�B	ffB	`BB	`BB	[#B	>wB	+B	+B	)�B	'�B	!�B	�B	{B	uB	PB	1B	B��B�B�B�B�B�B�sB�mB�fB�NB�/B�/B�B�
B��B��BǮB�dB�FB�!B��B��B�oB�1B|�By�Bp�BgmBbNB`BB^5B]/BYBW
BYBW
BT�BQ�BQ�BO�BK�BJ�BG�BF�B=qB9XB7LB5?B49B33B2-B1'B0!B/B/B0!B.B+B)�B,B.B,B+B)�B+B'�B-B,B+B(�B+B&�B/B.B,B,B,B)�B,B+B+B(�B+B+B)�B'�B'�B'�B)�B(�B'�B'�B'�B'�B,B+B-B,B,B/B.B.B.B0!B0!B2-B33B33BC�BA�BJ�B\)BdZBk�B{�B�=B��B�B��B�#B�yB��B	oB	&�B	.B	E�B	S�B	VB	q�B	z�B	�hB	��B	��B	��B	�B	�3B	�RB	�jB	ƨB	��B	��B	�B	�B	�5B	�ZB	�mB	�sB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
1B

=B
PB
VB
PB
oB
hB
uB
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
#�B
(�B
(�B
+B
,B
.B
/B
1'B
33B
33B
5?B
8RB
9XB
8RB
;dB
<jB
=qB
>wB
@�B
A�B
B�B
C�B
C�B
D�B
E�B
F�B
H�B
I�B
J�B
L�B
L�B
M�B
L�B
L�B
M�B
O�B
P�B
P�B
P�B
R�B
R�B
S�B
T�B
VB
W
B
W
B
XB
YB
YB
ZB
\)B
^5B
_;B
`BB
aHB
bNB
cTB
dZB
e`B
ffB
ffB
ffB
gmB
hsB
iyB
iyB
k�B
l�B
l�B
l�B
n�B
o�B
p�B
o�B
o�B
r�B
s�B
r�B
s�B
t�B
u�B
v�B
v�B
v�B
u�B
w�B
v�B
w�B
w�B
y�B
x�B
y�B
{�B
{�B
|�B
|�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�1B
�7B
�7B
�=B
�DB
�JB
�PB
�PB
�\B
�bB
�hB
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
�B
�B
�B
�!B
�'B
�3B
�3B
�9B
�?B
�?B
�FB
�FB
�LB
�RB
�RB
�^B
�^B
�^B
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
�jB
�jB
�jB
�qB
�jB�B�B�B�B�B�B�B�B�#B�#B�B�B�)B�B�B�B�B�B�B�B�B�
B�B�B�B�B�)B�
B�B�B�B�B�B�B�B�B�B�
B�B�B�B�B�B�B�B�B�B�B�B�B�
B�B�
B�B�B��B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                            B�
B�B�B�B�B��B�B��B��B��B��BŢB�B��B��B��B��B��B�B��B�B�B�B�/B�`B	7B%�B+B8RB:^B;dB=qB>wB=qB:^BD�BI�Bm�Bu�Bs�Bs�By�Bz�By�Bv�B�B�B� B�B�B�B�B�B�1B�1B�7B�=B�=B�=B�=B�PB�DB�7B�7B�+B�+B�B�B�B�B~�B{�Bv�Bq�Bl�BhsB\)BK�B=qB49B(�B%�B!�BoB	7BB��B�B�NB��BƨB��B�?B��B��B�PBt�Bk�BgmBR�B/B'�B�B%B
��B
�yB
�)B
��B
ÖB
�9B
��B
��B
�DB
o�B
YB
;dB
)�B
oB
+B	��B	�mB	�B	��B	��B	��B	�RB	�B	��B	�oB	�7B	�B	r�B	l�B	dZB	^5B	^5B	YB	<jB	(�B	(�B	'�B	%�B	�B	�B	oB	hB	DB	%B	B�B�B�B�B�yB�sB�fB�`B�ZB�BB�#B�#B�
B��B��B��BŢB�XB�9B�B��B��B�bB�%Bz�Bw�Bn�Be`B`BB^5B\)B[#BW
BT�BW
BT�BR�BO�BO�BM�BI�BH�BE�BD�B;dB7LB5?B33B2-B1'B0!B/B.B-B-B.B,B(�B'�B)�B,B)�B(�B'�B(�B%�B+B)�B(�B&�B(�B$�B-B,B)�B)�B)�B'�B)�B(�B(�B&�B(�B(�B'�B%�B%�B%�B'�B&�B%�B%�B%�B%�B)�B(�B+B)�B)�B-B,B,B,B.B.B0!B1'B1'BA�B?}BH�BZBbNBiyBy�B�1B��B�B��B�B�mB��B	bB	$�B	,B	C�B	Q�B	S�B	o�B	x�B	�\B	��B	��B	��B	��B	�'B	�FB	�^B	ĜB	��B	��B	�B	�B	�)B	�NB	�`B	�fB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
1B
DB
JB
JB
hB
bB
oB
{B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
"�B
'�B
'�B
)�B
+B
-B
.B
0!B
2-B
2-B
49B
7LB
8RB
7LB
:^B
;dB
<jB
=qB
?}B
@�B
A�B
B�B
B�B
C�B
D�B
E�B
G�B
H�B
I�B
K�B
K�B
L�B
K�B
K�B
L�B
N�B
O�B
O�B
O�B
Q�B
Q�B
R�B
S�B
T�B
VB
VB
W
B
XB
XB
YB
[#B
]/B
^5B
_;B
`BB
aHB
bNB
cTB
dZB
e`B
e`B
e`B
ffB
gmB
hsB
hsB
jB
k�B
k�B
k�B
m�B
n�B
o�B
n�B
n�B
q�B
r�B
q�B
r�B
s�B
t�B
u�B
v�B
v�B
u�B
w�B
v�B
w�B
w�B
y�B
x�B
y�B
{�B
{�B
|�B
|�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�1B
�7B
�7B
�=B
�DB
�JB
�PB
�PB
�\B
�bB
�hB
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
�B
�B
�B
�B
�B
�B
�'B
�-B
�9B
�9B
�?B
�FB
�FB
�LB
�LB
�RB
�XB
�^B
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�wB
�qB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�wB�
B�B�
B�
B�
B�
B�
B�
B�B�B�
B�
B�B�B�
B�B�B�B�B�B�B��B�
B�B�B�B�B��B�B�
B�B�B�B�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B�B��B��B��B��B��G�O�G�O�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811270600382021061413525820210614135258202106141746562021061417465620210614174656201811270600382021061413525820210614135258202106141746562021061417465620210614174656PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018112706003820181127060038  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112706003820181127060038QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112706003820181127060038QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015520210722160155IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                