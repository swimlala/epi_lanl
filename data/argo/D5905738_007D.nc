CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:30Z creation      
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
_FillValue                 8  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  f   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    <   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        \   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180724220230  20210722160148  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�d�"�B�@�d�"�B�11  @�d�qР@�d�qР@6��Fs��@6��Fs���c�-M@$��c�-M@$�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@@  @�  @�33@ə�@�ffA33A33A(  AC33Ad��A���A���A���A���A���A�33A�  A���A�33B  B  B  B ffB(ffB0ffB8ffB@ffBH  BP  BXffB_��Bg��BpffBxffB��B�33B�33B�ffB�33B�ffB�33B�33B�33B�33B�33B�  B�33B�33B�  B�33B�ffB�33B���B���B�33B�33B�  Bܙ�B�33B�33B虚B�ffB�33B�33B���B���B���C  C33CL�C33C
  C  C��C  CL�C33C  C33C  C�fC33C 33C"�C$�C%�fC'�fC)�fC,33C.33C033C2�C4�C6  C8  C:  C<  C>  C@  CB  CC��CF33CH�CJ  CL33CN�CP  CRL�CTL�CV�CX33CZ  C\L�C^�C`  Cb33Cd�Ce�fCh�Ci�fCk��Cm�fCp�CrL�Ct�Cu�fCx�Cz33C|33C~�C�fC��C��C�&fC��C��3C�  C��C��C�  C��3C��fC��C�  C��3C��C��C�  C�&fC��C�  C�  C��fC��3C��C��C�  C��3C��fC��C�&fC��C��C��C�  C��C��C��C�&fC��C�  C�&fC��C�  C��C��C��3C��C�  C��3C��C�  C��fC�  C��C��C��fC�  C�&fC�  C��fC�  C��C�  C��fC�  C��C��3C��fC�  C��C�  C��fC�  C�&fC��C��3C��C�  C��3C��C�  C��fC��C�&fC��C��3C��C��3C��fC�  C��C�&fC�  C��3C�  C�&fC�  C��3C��C�&fC��C�  C��C��C��3C�  C��C��3C�ٚC��fC�  C��C�  C��fC��fC��3C�  C��C�&fC�  C��fC��3C�  C��C��C�� C��fC��fD y�DfD��D3Dl�DS3D	��D� D` D  D��D�3D�fDl�D ,�D"��D%�3D(Y�D+fD-�fD033D2��D5L�D7�fD:y�D=3D?��DB&fDD��DG,�DI��DL9�DN��DQ` DS�3DVy�DY3D[� D^Y�Da3Dc�fDfs3Di  Dk�3Dny�Dq  Ds��Dvy�Dy3D{Y�D}�3D�C3D��fD�ɚD��D�S3D��fD���D�  D�\�D��fD��fD��fD�#3D�VfD���D���D��3D�0 D�\�D��3D�ɚD�3D�<�D�s3D��3D�ٚD�3D�P D�� D�� D�� D�3D�P D�� D��fD���D�&fD�VfD�� D��3D�	�D�I�D���D���D� D�I�D���D�ɚD�3D�S3D���D�ɚD�	�D�I�D�y�Dó3D��fD��D�I�Dȃ3Dɹ�D���D�fD�C3D�i�DϖfD��fD��fD��D�C3D�ffD։�Dש�D�ɚD��3D�  D��D�33D�P D�` D�s3D��D�3D�3D�� D���D���D��D�  D� D��D�#3D�,�D�6fD�@ D�FfD�S3D�` D�p D�y�D��3D�� D��3D��fD��fD�� D��fD��fD���D�3E �E � E33E� ENfE�3Eh E��E��E3E��E@ E�3Ec3E� E	�3E
�3ES3EffEx E��E�3E{3EnfE�ES3E;3E� E�E�fEQ�E��E��E .fE!�fE"�3E#��E$�3E&d�E'� E(� E*.fE+� E,�3E-�fE/<�E0��E1��E2��E44�E5� E6h E7�fE9 E:VfE;�fE<��E?� EB� EFQ�EII�ELK3EO��ER�fEU� EY	�E\�E_!�Eb+3Ee�3EhffEk� En��Er�Et�Ex6fE{#3E~C3E���E�^fE��3E�3E���E���E� E��fE�0 E��3E�VfE���E� E�H E�� E��3E�8 E�{3E��3E�$�E��fE���E�3E�p�E���E�3E�O3E��3E��E�I�E���E���E�;3E��3E��fE�+3E��3E���E��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?   >���>���>���?   ?   ?��?333?L��?�  ?���?�ff?�  ?�ff@   @��@   @,��@@  @Y��@fff@y��@���@�ff@�33@�  @�  @�  @���@���@���AffA  AffAffA(  A0  A6ffA@  AH  AP  AY��A`  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144444441414414441411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?�  @&ff@`  @�  @�33@ٙ�@�ffA33A33A0  AK33Al��A���A���A���A���A���A�33A�  A���B��B
  B  B  B"ffB*ffB2ffB:ffBBffBJ  BR  BZffBa��Bi��BrffBzffB���B�33B�33B�ffB�33B�ffB�33B�33B�33B�33B�33B�  B�33B�33B�  B�33B�ffB�33B���B���B�33B�33B�  Bݙ�B�33B�33B陚B�ffB�33B�33B���B���C L�C� C�3C��C�3C
� C� CL�C� C��C�3C� C�3C� CffC�3C �3C"��C$��C&ffC(ffC*ffC,�3C.�3C0�3C2��C4��C6� C8� C:� C<� C>� C@� CB� CDL�CF�3CH��CJ� CL�3CN��CP� CR��CT��CV��CX�3CZ� C\��C^��C`� Cb�3Cd��CfffCh��CjffClL�CnffCp��Cr��Ct��CvffCx��Cz�3C|�3C~��C�33C�L�C�Y�C�ffC�Y�C�33C�@ C�Y�C�L�C�@ C�33C�&fC�L�C�@ C�33C�L�C�L�C�@ C�ffC�Y�C�@ C�@ C�&fC�33C�Y�C�Y�C�@ C�33C�&fC�L�C�ffC�Y�C�L�C�L�C�@ C�Y�C�L�C�L�C�ffC�Y�C�@ C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�@ C�33C�L�C�@ C�&fC�@ C�Y�C�L�C�&fC�@ C�ffC�@ C�&fC�@ C�Y�C�@ C�&fC�@ C�L�C�33C�&fC�@ C�Y�C�@ C�&fC�@ C�ffC�L�C�33C�Y�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�Y�C�33C�Y�C�33C�&fC�@ C�Y�C�ffC�@ C�33C�@ C�ffC�@ C�33C�L�C�ffC�L�C�@ C�Y�C�L�C�33C�@ C�Y�C�33C��C�&fC�@ C�L�C�@ C�&fC�&fC�33C�@ C�L�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�  C�&fD 3D ��D&fD��D33D��Ds3D
�D� D� D@ D�D�3D�fD��D L�D#�D%�3D(y�D+&fD-�fD0S3D2ٚD5l�D8fD:��D=33D?��DBFfDD��DGL�DI��DLY�DN��DQ� DT3DV��DY33D[� D^y�Da33Dc�fDf�3Di@ Dk�3Dn��Dq@ Ds��Dv��Dy33D{y�D~3D�S3D��fD�ٚD��D�c3D��fD���D�0 D�l�D��fD��fD�fD�33D�ffD���D���D�3D�@ D�l�D��3D�ٚD�3D�L�D��3D��3D��D�#3D�` D�� D�� D�� D�#3D�` D�� D��fD���D�6fD�ffD�� D��3D��D�Y�D���D���D�  D�Y�D���D�ٚD�#3D�c3D���D�ٚD��D�Y�D�D��3D��fD�)�D�Y�Dȓ3D�ɚD���D�&fD�S3D�y�DϦfD��fD�fD�)�D�S3D�vfD֙�D׹�D�ٚD��3D� D�)�D�C3D�` D�p D��3D��D�3D��3D�� D���D���D���D� D�  D�,�D�33D�<�D�FfD�P D�VfD�c3D�p D� D�D��3D�� D��3D��fD��fD�� D��fD��fD��D�#3E !�E � E;3E� EVfE�3Ep E�E��E#3E��EH E�3Ek3E� E	�3E
�3E[3EnfE� E�E3E�3EvfE�E[3EC3E� E	�E�fEY�EɚE��E 6fE!�fE"�3E$�E$�3E&l�E'� E(� E*6fE+� E,�3E-�fE/D�E0��E1��E2��E4<�E5� E6p E7�fE9 E:^fE;�fE<��E?� EB� EFY�EIQ�ELS3EO��ER�fEU� EY�E\!�E_)�Eb33Ee�3EhnfEk� En��Er�Et�Ex>fE{+3E~K3E���E�bfE��3E��3E���E���E�  E��fE�4 E��3E�ZfE���E� E�L E�� E��3E�< E�3E��3E�(�E��fE���E�3E�t�E���E�3E�S3E��3E��E�M�E���E���E�?3E��3E��fE�/3E��3E���E��G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�?fffG�O�G�O�G�O�?fffG�O�?�  ?���?���?�ff?�  ?���?�ff@   @33@   @,��@@  @L��@`  @y��@�33@���@���@�ff@�33@�  @�  @�  @���@���AffAffA  AffA&ffA0  A8  A>ffAH  AP  AX  Aa��Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144444441414414441411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ �@ V@ {@ O@ "�@ +@ 1'@ 7�@ >�@ G�@ SI@ a�@ n�@ |?@ ��@ ��@ ��@ ��@ �&@ �|@ ��@ �@ ��@j@�@g@-@:�@H]@UU@b�@qS@}�@�D@�H@��@�9@�>@��@�;@�4@��@�@*@"�@0x@>@K@Yn@g@t@�d@��@��@��@��@ƨ@�O@�H@��@�E@
�@�@&�@3�@A�@M�@[z@hs@ww@�|@��@��@�@��@�@�
@�@�@  @@O@(G@7�@E�@R�@`B@l�@z3@��@��@�5@��@��@�|@�t@�@��@j@@�@,`@8�@I@V@b�@r@~�@��@��@�M@��@��@�7@��@�4@�,@1@*@!s@0x@<�@I�@X@g@v@�d@��@��@�@��@ƨ@��@��@�L@��@�@6@%�@4�@A�@N�@[z@hs@x&@�@�@�@��@��@�o@�h@�@�Y@��@�@�@*S@6�@C�@P�@`B@oF@|?@�7@��@��@��@��@�|@܀@�y@��@	v@	o@	�@	-�@	:�@	F�@	V@	b�@	o�@	~�@	��@	�<@	�A@	�F@	�>@	��@	��@	�@	�,@
v@
{@
#�@
/�@
<@
K@
Yn@
e�@
r�@
��@
��@
�@
�M@
�R@
�@
�O@
��@
�L@
��@	�@�@%�@1�@A�@P�@]�@i!@x�@�p@�h@�m@�r@��@�c@�[@�@�e@  @�@�@+@7L@DD@SI@`B@l�@z�@��@��@��@�!@�&@�|@�t@�@�e@�@@g@.l@:@FQ@T�@b�@qS@�@��@�<@��@�9@�>@є@��@�y@oF@�F@  @K@�0@�@1�@~�@��@�@bN@��@�@>@��@�o@b@V�@��@�@+@o�@��@��@>�@�@ȴ@@UU@��@��@'�@p�@��@@K�@��@��@'�@o�@�R@^@Ji@�h@ψ@�@\�@��@�@+�@qS@�F@��@@�@�p@�W@1@I@�7@��@�@M�@�@Ӡ@ �@ V@ �<@ �#@!
@!`A@!�@!�T@"&;@"i�@"��@"�@#,`@#m�@#�~@#�Y@$4�@$v�@$��@$��@%=q@%�d@%Ĝ@&�@&M$@&�@&�
@'�@'^5@'�z@'��@(-@(o�@(��@(��@)<@)|�@)��@*]@*B�@*��@*ƨ@+�@+Ji@+��@+�@,�@,I@,��@,��@-�@-H]@-�|@-Ĝ@.@.?}@.{�@.�R@.�e@/0x@/m:@/�A@/��@0
@0Yn@0�u@0��@1%@1@,@1y�@1��@1��@2(G@2`A@2��@2є@3
=@3B8@3{�@3��@3��@4'�@4`A@4��@4�O@5@5I@5~K@5��@5�@6/@6j@6��@6�@7 @7\)@7��@7��@8@8O1@8��@8��@9�@9G�@9�|@9��@:@�@:��@;1'@;�;@<T�@<�c@=m�@=��@>�@>�l@?��@@#�@@�|@A�@A��@B�@B�-@CO1@C�@DX@D�@E]�@E��@F`�@F��@G�@Hj@H�T@I6�@I��@J2�@JĜ@KZ�@K��@LP�@L�T@Mt@MӠ@NbN@N�Y@O�@Pb@P��@Q�H@S(�@T��@U��@W*S@X�@Y�@[DD@\�I@]��@_3�@`�@a��@c(G@d��@e�@gFQ@h{�@i�l@k&�@l|?@m�+@o?}@p��@q��@s*T@t��@u�t@w+@xz�@y��@{+@{g@{�&@{�,@|33@|�C@|��@|�Q@}X@}�@}�@~g@~X@~�Z@~��@2�@i!@��@��@�g@�FQ@�_�@��|@���@���@���@�o@�5@@�M$G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�@ G�O�G�O�@ �G�O�G�O�G�O�@ �G�O�@ j@ @ �@ v@ �@ �@ �@ 
=@ J@ �@ @ @ o@ {@ 6@ �@ �@ 
@  @ "�@ %�@ (�@ ,`@ /@ 2�@ 5�@ 9X@ =q@ @,@ C�@ G�@ K@ M�@ Q�@ UU@ X�@ \�@ _�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aǝ�Aǧ�Aǧ�Aǡ�Aǝ�AǛ�AǛ�Aǡ�Aǡ�Aǡ�Aǥ�Aǰ!AǸRAǸRAǼjAǺ^AǺ^AǼjAǾwAǼjAǲ-AǶFAƟ�AËDA�5?A���A��A�;dA�x�A��A�ZA���A��A�1'A�1A�p�A��A���A��FA���A��A�p�A�%A���A�VA�9XA��9A��A�jA�%A�~�A�  A�ZA�JA�z�A��^A���A�v�A�{A�1'A�C�A���A�ZA��A��
A��A�bNA�XA�JA���A���A�33A�bNA��\A�JA���A�dZA��!A���A�/A��
A�VA�+A���A�7LA�  A�XA�A��yA�XA���A�(�A��;A�p�A�hsA��TA�~�A�  A�hsA�{A��-A��uA�-A�VA���A���A��\A�?}A���A��A���A��DA�5?A�ffA�1'A��;A���A�A�=qA���A���A�S�A�"�A��A���A��A�+A��A���A�|�A���A�VA�%A�ffA�A{��AxĜAv�`At�yAtVArE�Ap�yAo��AnI�Ak��AhbNAf�HAex�Ad1'Ac�Ac%Aa�FA_��A]O�A\1'A[�mA[?}AZ��AY��AX��AWx�AU��AR��AP~�AO��ANĜAL��AJ�/AI�TAI�AIK�AF�yAD�`ACK�AB(�AAx�AA+A@9XA>��A<r�A9�^A:n�A:1'A8��A7��A7��A7dZA6�A6 �A4��A2�jA2��A1�mA/dZA-hsA,�uA+t�A)�A&�jA%|�A%;dA$�9A#�PA"�A!�A bNAp�A�A�#A�`AQ�AZAA�A��A��A��A�AA�A�`A�\A7LAjA�A��AQ�Al�A��A1'A�#A%A%A�yA��A5?A|�A
�DA��A?}A�A�!AK�A��A�DA�A  A��A��A Ĝ@���@��T@��j@���@�?}@��;@�x�@���@���@��;@�\@�t�@�^@��;@�R@�@߾w@�C�@�Ĝ@˕�@ũ�@�  @��\@��-@��u@��j@�`B@��@��@�1'@�O�@�O�@���@�  @�J@���@��@�dZ@��!@��@�{@��@��/@�z�@�A�@� �@�@���@��@�9X@���@���@���@���@�@��h@�O�@�(�@�ȴ@��#@�O�@�j@��w@�ȴ@�hs@��9@�b@}p�@{�m@z-@x�@u@t�j@r�@q7L@o�@n�R@m�@l�@ko@jn�@hA�@e��@d�@aX@_�@^��@[�m@Z��@X1'@V��@U?}@SS�@R=q@P�u@N�y@M/@Lj@KdZ@I��@H�9@G��@E�-@D�@D9X@A��@@ �@>��@=@<�D@;�
@:n�@9�7@8 �@6�R@5�-@4�D@2�!@0bN@.ȴ@-��@,�j@+�m@+��@+"�@)�#@(��@'�;@&V@%?}@$��@$9X@$1@#�@"��@!�7@!&�@��@�+@�@9X@S�@n�@J@x�@��@\)@�y@��@�-@�j@�@t�@~�@hs@b@��@��@ff@�-@O�@�/@��@@	��@	�@��@r�@K�@
=@�+@��@$�@/@�@�@Z@ƨ@33@�\@�@x�@ �u@ Q�?�|�?��?�{?��-?�"�?�x�?�b?��T?�Z?��?�R?�V?�~�?�x�?�K�?�?��?��?��?�A�?ޗ�?�p�?�j?�dZ?ٙ�?���?׍P?�ff?�`B?�t�?���?�hs?�A�?�\)?�V?�O�?̬?�ƨ?��H?��#?���?�r�?���?�+?��y?��T?�`B?�Z?Õ�?��?���?�&�?�|�?��R?��?�p�?��?��?�(�?��m?�ƨ?��m?�ƨ?�ƨ?��m?�ƨ?�ƨ?���?�ƨ?�1?�(�?�I�?��?�p�?�{?���?���?��w?�A�?�bN?���?�Ĝ?�Ĝ?�%?�&�?�G�?�hs?��7?���?���?��?�J?�-?�-?�-?�M�?�M�?�J?��?���?�hs?��`?�%?�&�?�G�?��7?��7AǕ�Aǡ�Aǟ�Aǟ�Aǥ�Aǣ�Aǡ�AǙ�Aǟ�AǛ�Aǝ�Aǝ�AǗ�AǙ�AǙ�Aǟ�AǗ�AǙ�Aǡ�Aǣ�Aǝ�AǛ�AǗ�AǗ�AǛ�AǮAǬAǩ�AǮAǰ!Aǰ!Aǥ�Aǥ�Aǣ�Aǣ�Aǡ�Aǟ�Aǟ�Aǝ�AǛ�AǙ�AǛ�AǛ�AǙ�Aǝ�Aǡ�Aǡ�Aǡ�Aǟ�Aǟ�Aǡ�Aǣ�Aǥ�Aǧ�Aǩ�AǬAǰ!AǶFAǸRAǶFG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Aǝ�Aǧ�Aǧ�Aǡ�Aǝ�AǛ�AǛ�Aǡ�Aǡ�Aǡ�Aǥ�Aǰ!AǸRAǸRAǼjAǺ^AǺ^AǼjAǾwAǼjAǲ-AǶFAƟ�AËDA�5?A���A��A�;dA�x�A��A�ZA���A��A�1'A�1A�p�A��A���A��FA���A��A�p�A�%A���A�VA�9XA��9A��A�jA�%A�~�A�  A�ZA�JA�z�A��^A���A�v�A�{A�1'A�C�A���A�ZA��A��
A��A�bNA�XA�JA���A���A�33A�bNA��\A�JA���A�dZA��!A���A�/A��
A�VA�+A���A�7LA�  A�XA�A��yA�XA���A�(�A��;A�p�A�hsA��TA�~�A�  A�hsA�{A��-A��uA�-A�VA���A���A��\A�?}A���A��A���A��DA�5?A�ffA�1'A��;A���A�A�=qA���A���A�S�A�"�A��A���A��A�+A��A���A�|�A���A�VA�%A�ffA�A{��AxĜAv�`At�yAtVArE�Ap�yAo��AnI�Ak��AhbNAf�HAex�Ad1'Ac�Ac%Aa�FA_��A]O�A\1'A[�mA[?}AZ��AY��AX��AWx�AU��AR��AP~�AO��ANĜAL��AJ�/AI�TAI�AIK�AF�yAD�`ACK�AB(�AAx�AA+A@9XA>��A<r�A9�^A:n�A:1'A8��A7��A7��A7dZA6�A6 �A4��A2�jA2��A1�mA/dZA-hsA,�uA+t�A)�A&�jA%|�A%;dA$�9A#�PA"�A!�A bNAp�A�A�#A�`AQ�AZAA�A��A��A��A�AA�A�`A�\A7LAjA�A��AQ�Al�A��A1'A�#A%A%A�yA��A5?A|�A
�DA��A?}A�A�!AK�A��A�DA�A  A��A��A Ĝ@���@��T@��j@���@�?}@��;@�x�@���@���@��;@�\@�t�@�^@��;@�R@�@߾w@�C�@�Ĝ@˕�@ũ�@�  @��\@��-@��u@��j@�`B@��@��@�1'@�O�@�O�@���@�  @�J@���@��@�dZ@��!@��@�{@��@��/@�z�@�A�@� �@�@���@��@�9X@���@���@���@���@�@��h@�O�@�(�@�ȴ@��#@�O�@�j@��w@�ȴ@�hs@��9@�b@}p�@{�m@z-@x�@u@t�j@r�@q7L@o�@n�R@m�@l�@ko@jn�@hA�@e��@d�@aX@_�@^��@[�m@Z��@X1'@V��@U?}@SS�@R=q@P�u@N�y@M/@Lj@KdZ@I��@H�9@G��@E�-@D�@D9X@A��@@ �@>��@=@<�D@;�
@:n�@9�7@8 �@6�R@5�-@4�D@2�!@0bN@.ȴ@-��@,�j@+�m@+��@+"�@)�#@(��@'�;@&V@%?}@$��@$9X@$1@#�@"��@!�7@!&�@��@�+@�@9X@S�@n�@J@x�@��@\)@�y@��@�-@�j@�@t�@~�@hs@b@��@��@ff@�-@O�@�/@��@@	��@	�@��@r�@K�@
=@�+@��@$�@/@�@�@Z@ƨ@33@�\@�@x�@ �u@ Q�?�|�?��?�{?��-?�"�?�x�?�b?��T?�Z?��?�R?�V?�~�?�x�?�K�?�?��?��?��?�A�?ޗ�?�p�?�j?�dZ?ٙ�?���?׍P?�ff?�`B?�t�?���?�hs?�A�?�\)?�V?�O�?̬?�ƨ?��H?��#?���?�r�?���?�+?��y?��T?�`B?�Z?Õ�?��?���?�&�?�|�?��R?��?�p�?��?��?�(�?��m?�ƨ?��m?�ƨ?�ƨ?��m?�ƨ?�ƨ?���?�ƨ?�1?�(�?�I�?��?�p�?�{?���?���?��w?�A�?�bN?���?�Ĝ?�Ĝ?�%?�&�?�G�?�hs?��7?���?���?��?�J?�-?�-?�-?�M�?�M�?�J?��?���?�hs?��`?�%?�&�?�G�?��7?��7AǕ�Aǡ�Aǟ�Aǟ�Aǥ�Aǣ�Aǡ�AǙ�Aǟ�AǛ�Aǝ�Aǝ�AǗ�AǙ�AǙ�Aǟ�AǗ�AǙ�Aǡ�Aǣ�Aǝ�AǛ�AǗ�AǗ�AǛ�AǮAǬAǩ�AǮAǰ!Aǰ!Aǥ�Aǥ�Aǣ�Aǣ�Aǡ�Aǟ�Aǟ�Aǝ�AǛ�AǙ�AǛ�AǛ�AǙ�Aǝ�Aǡ�Aǡ�Aǡ�Aǟ�Aǟ�Aǡ�Aǣ�Aǥ�Aǧ�Aǩ�AǬAǰ!AǶFAǸRAǶFG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�mB
�mB
�mB
�mB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�B+BJ�BYBffBv�B��B�mBJB�B&�B/B1'B<jBH�BH�BG�BE�BD�BD�BK�BYBZBffBo�Br�Bs�Bw�B~�B�+B�oB�{B�oB�\B�PB�B�{B��B�!B�LB�LB�LB�XB�dB�dB�qB�XB�^B�-B�B�!B��B�VB�JB}�Bm�Be`BXBW
BT�B\)B[#BN�B<jB1'B,B#�B$�B&�B�B{B%B�B��B�B�fB�BB�)B�
BɺBǮB�}B�-B��B��B��B�B��B�\B|�BdZBdZBT�BH�B:^B#�BPBPB
=B+BB
��B
�yB
ŢB
�LB
�3B
�B
��B
�B
|�B
y�B
l�B
XB
>wB
0!B
�B
�B
bB
  B	��B	�B	�HB	��B	�^B	�-B	��B	��B	�hB	�B	q�B	^5B	O�B	P�B	S�B	YB	XB	J�B	G�B	5?B	,B	{B	DB	B��B��B�B�B�B�mB�NB�)B�
B��B��B��BɺB��B�RB�dB��B��BB�dB�RB�LB�FB�3B�'B�B�-B�B��B��B��B��B��B�VB�\B�DB�7B�By�Bs�Bn�BgmBgmBiyBiyBe`Be`BdZBYB[#B]/B_;B`BB`BBbNB]/B`BBZBW
BP�BM�BL�BM�BL�BD�BF�BD�BC�BA�BA�B@�BA�BA�BA�B=qB2-B8RB7LB5?B7LB6FB6FB33B33B49B6FB8RB8RB7LB9XB5?B:^B8RB49B2-B-B.B,B,B)�B0!B0!BA�BN�BZBYB]/BcTBq�Bx�B{�B�B�\B��B��B�B�RBŢB��B�B�B��B	B	)�B	P�B	`BB	o�B	�B	�VB	�{B	��B	��B	�B	�9B	�XB	��B	ÖB	ɺB	��B	��B	�B	�B	�5B	�HB	�TB	�fB	�yB	�B	�B	��B	��B	��B	��B	��B
B
B
%B
1B

=B
PB
VB
VB
bB
oB
oB
�B
�B
�B
�B
�B
�B
#�B
"�B
&�B
'�B
)�B
+B
-B
.B
0!B
2-B
2-B
33B
5?B
5?B
7LB
8RB
;dB
:^B
<jB
>wB
@�B
@�B
B�B
C�B
D�B
D�B
G�B
H�B
I�B
I�B
K�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
S�B
S�B
T�B
VB
XB
YB
ZB
YB
ZB
ZB
[#B
\)B
\)B
^5B
_;B
aHB
aHB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
hsB
hsB
jB
k�B
l�B
m�B
m�B
n�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
t�B
u�B
u�B
v�B
w�B
v�B
w�B
x�B
y�B
x�B
y�B
y�B
z�B
{�B
{�B
|�B
}�B
}�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�1B
�=B
�DB
�JB
�PB
�\B
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
��B
��B
��B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�9B
�3B
�9B
�?B
�?B
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�dB
�^B
�dB
�dB
�^B
�^B
�dB
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�jB
�jB
�mB
�sB
�fB
�sB
�fB
�sB
�mB
�`B
�sB
�sB
�mB
�fB
�mB
�yB
�fB
�fB
�sB
�fB
�sB
�fB
�`B
�fB
�mB
�sB
�sB
�fB
�fB
�mB
�mB
�mB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�fB
�fB
�mB
�sB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�mB
�mB
�mB
�mB
�fB
�mB
�mB
�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
�fB
�fB
�fB
�fB
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�B)�BI�BXBe`Bu�B��B�fBDB�B%�B.B0!B;dBG�BG�BF�BD�BC�BC�BJ�BXBYBe`Bn�Bq�Br�Bv�B}�B�%B�hB�uB�hB�VB�JB�B�uB��B�B�FB�FB�FB�RB�^B�^B�jB�RB�XB�'B��B�B��B�PB�DB|�Bl�BdZBW
BVBS�B[#BZBM�B;dB0!B+B"�B#�B%�B�BuBB�B��B�B�`B�;B�#B�BȴBƨB�wB�'B��B��B��B�B��B�VB{�BcTBcTBS�BG�B9XB"�BJBJB	7B%BB
��B
�sB
ĜB
�FB
�-B
��B
��B
�B
{�B
x�B
k�B
W
B
=qB
/B
�B
{B
\B	��B	��B	�B	�BB	��B	�XB	�'B	��B	��B	�bB	�B	p�B	]/B	N�B	O�B	R�B	XB	W
B	I�B	F�B	49B	+B	uB	
=B	B��B��B�B�B�B�fB�HB�#B�B��B��B��BȴB��B�LB�^B��B��B��B�^B�LB�FB�?B�-B�!B�B�'B�B��B��B��B��B��B�PB�VB�=B�1B� Bx�Br�Bm�BffBffBhsBhsBdZBdZBcTBXBZB\)B^5B_;B_;BaHB\)B_;BYBVBO�BL�BK�BL�BK�BC�BE�BC�BB�B@�B@�B?}B@�B@�B@�B<jB1'B7LB6FB49B6FB5?B5?B2-B2-B33B5?B7LB7LB6FB8RB49B9XB7LB33B1'B,B-B+B+B(�B/B/B@�BM�BYBXB\)BbNBp�Bw�Bz�B�B�VB��B��B�B�LBĜB��B�B�B��B	B	)�B	P�B	`BB	o�B	�B	�VB	�{B	��B	��B	�B	�9B	�XB	��B	ÖB	ɺB	��B	��B	�B	�B	�5B	�HB	�TB	�fB	�yB	�B	�B	��B	��B	��B	��B	��B
B
B
%B
1B

=B
PB
VB
VB
bB
oB
oB
�B
�B
�B
�B
�B
�B
#�B
"�B
&�B
'�B
)�B
+B
-B
.B
0!B
2-B
2-B
33B
5?B
5?B
7LB
8RB
;dB
:^B
<jB
>wB
@�B
@�B
B�B
C�B
D�B
D�B
G�B
H�B
I�B
I�B
K�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
S�B
S�B
VB
W
B
YB
ZB
[#B
ZB
[#B
[#B
\)B
]/B
]/B
_;B
`BB
bNB
bNB
dZB
dZB
dZB
e`B
e`B
ffB
gmB
gmB
hsB
hsB
iyB
iyB
k�B
l�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
u�B
v�B
v�B
w�B
x�B
w�B
x�B
y�B
z�B
y�B
z�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�7B
�DB
�JB
�PB
�VB
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
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�-B
�9B
�?B
�?B
�LB
�FB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�jB
�jB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�wB
�}B
�}B
�wB
�wB
�}B
�wB
�}B
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
�fB
�mB
�`B
�mB
�`B
�mB
�fB
�ZB
�mB
�mB
�fB
�`B
�fB
�sB
�`B
�`B
�mB
�`B
�mB
�`B
�ZB
�`B
�fB
�mB
�mB
�`B
�`B
�fB
�fB
�fB
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�`B
�`B
�fB
�mB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�fB
�fB
�fB
�fB
�`B
�fB
�fB
�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202302021061413520420210614135204202106141746162021061417461620210614174616201807242202302021061413520420210614135204202106141746162021061417461620210614174616PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023020180724220230  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023020180724220230QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023020180724220230QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014820210722160148IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                