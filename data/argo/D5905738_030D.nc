CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:46Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        Q    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  b    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        f`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        w`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20180917231346  20210722160152  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�z���#@�z���#11  @�z��[�@�z��[�@6�Ҳ��@6�Ҳ���c�{���m�c�{���m11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?L��@   @@  @�33@�33@�33@���A   A  A#33AC33Aa��A���A���A���A���A���A���AᙚA�  A�33B  B��B  B��B'��B/��B8  B@  BH  BP  BXffB`��BhffBo��Bx  B�  B�  B�  B���B���B���B�  B�ffB�33B�  B�  B�33B�ffB�  B�  B�33B�33B���B�  B�33B�33Bԙ�B�ffB�33B�33B�  B�  B뙚B�ffB�33B�  B�  B���C�C�C�C  C
  C�fC��C  C33C33C  CL�C�C�fC�C   C!�fC$�C&33C(�C)��C+�fC.33C0�C1�fC4  C6�C833C:L�C<�C=�fC@  CB33CDL�CFL�CHL�CJL�CL�CM��CO��CQ��CS��CU�fCX  CZ�C\33C^  C_�fCb  CdL�Cf�Ch  Cj33Cl�Cm�fCo�fCq��Ct�Cv  Cw�fCz33C|�C~  C�fC��3C�&fC��C�  C�  C��3C��C��C��3C�  C��C��C��C��fC��fC�  C�  C��C��C�&fC�  C�  C��C�  C�  C��3C�  C�  C�  C��3C��3C��3C��C��C��fC��C�  C��fC��3C��C�  C�ٚC��fC��3C��3C�  C��C�&fC�  C�ٚC��3C�  C��C��C��C��C��C��C��C��C��C�&fC�&fC��C��fC��3C�  C��C�&fC�&fC��C��3C��C��C�  C��fC�  C��C��C��C��3C�  C��C��C�  C��C�  C��fC�  C��C��C��fC��C�&fC��C��C�  C��3C��C��C�  C�&fC��C�  C��3C��fC��C�  C��fC�  C��C��C��C��fC�  C�  C��C��C��fC�  C��C�  C��fC��3C��C�&fC�33C��3D � DfD�fD�D` DS3D	� DFfD��D9�D� D9�D� D  DY�D�3D"  D$L�D&�3D)&fD+� D.fD0y�D3�D5�fD8FfD;  D=��D@Y�DB��DE� DH3DJ��DM�DO�fDR�DT��DW�DY��D\  D^l�D`�3Dc@ De��Dh�Dj� Dl�fDo33Dq�3Ds�3DvL�Dx� D{  D|��DS3D��fD�fD�,�D�Y�D��3D�� D�� D��D��D�FfD�l�D���D�� D�  D�)�D�S3D�|�D��3D��fD�#3D�\�D��fD���D���D�)�D�` D�� D��3D���D�6fD�p D��3D���D�3D�L�D��fD��3D��3D��D�L�D�|�D���D��fD���D��D�<�D�ffD�� D��fD��fD��fD�  D�fD�0 D�FfD�` D�vfDÃ3DĖfDų3D��fD�ٚD��3D�  D�fD�33D�S3D�p Dω�DЩ�D�ɚD�� D���D��D�9�D�\�D؀ Dٜ�Dڼ�D���D���D� D�0 D�P D�c3D�y�D�fD�fD�3D橚D繚D��fD�� D�ٚD��3D���D�� D�� D���D��fD��D��fD��fD�� D���D��fD���D�� D��3D���D��fD��3D���D��fD�|�E 8 E �3E,�E� E#3E��E� E��E  E� Es3E	l�E
��EI�E;3E��E�E�3E\�E�fE� E.fE��E� E��E�fES3E�fE�3E fE!q�E"� E#�3E%3E&h E'�3E)fE*P E+6fE,��E-� E/1�E0�fE1k3E2�3E4&fE5��E6|�E7�fE9T�E:H E;� E>�3EA�3ED� EH$�EKD�ENVfEQNfET��EW�fEZ��E]�fEa<�EdP EgT�Ej�3Emh Ep�fEs�fEw3Ey�fE};3E�2fE��3E�4�E���E�T E��E���E�fE�S3E�� E���E�X E��3E�� E�73E���E��fE�, E�k3E��fE�3E�nfE��fE��E�Y�E�� E���E�L�E��3E��fE�BfE��3E��3E�/3E�rfE��3E��E�i�E��f?   ?��>���?   ?   ?   >���?��?��?   >���?   ?��?   ?333?L��?L��?�  ?���?���?�  ?�33?�ff@   @��@&ff@333@L��@Y��@l��@�ff@�  @���@�33@�  @���@�ff@�33@���@�ff@�33A   A  A��A33A��A   A(  A,��A4��A;33AC33AI��AP  AVffA\��Ad��Ak33As33Ay��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141444144411411411414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?�  ?�ff@   @`  @�33@�33@�33@���A  A  A+33AK33Ai��A���A���A���A���A���A���A噚A�  B��B
  B��B  B!��B)��B1��B:  BB  BJ  BR  BZffBb��BjffBq��Bz  B�  B�  B�  B���B���B���B�  B�ffB�33B�  B�  B�33B�ffB�  B�  B�33B�33B���B�  B�33B�33Bՙ�B�ffB�33B�33B�  B�  B왚B�ffB�33B�  B�  C L�C��C��C��C� C
� CffCL�C� C�3C�3C� C��C��CffC��C � C"ffC$��C&�3C(��C*L�C,ffC.�3C0��C2ffC4� C6��C8�3C:��C<��C>ffC@� CB�3CD��CF��CH��CJ��CL��CNL�CPL�CRL�CTL�CVffCX� CZ��C\�3C^� C`ffCb� Cd��Cf��Ch� Cj�3Cl��CnffCpffCrL�Ct��Cv� CxffCz�3C|��C~� C�33C�33C�ffC�L�C�@ C�@ C�33C�Y�C�Y�C�33C�@ C�L�C�L�C�Y�C�&fC�&fC�@ C�@ C�Y�C�Y�C�ffC�@ C�@ C�L�C�@ C�@ C�33C�@ C�@ C�@ C�33C�33C�33C�L�C�L�C�&fC�Y�C�@ C�&fC�33C�Y�C�@ C��C�&fC�33C�33C�@ C�L�C�ffC�@ C��C�33C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�ffC�ffC�L�C�&fC�33C�@ C�L�C�ffC�ffC�L�C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�Y�C�L�C�33C�@ C�Y�C�L�C�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�&fC�L�C�ffC�Y�C�L�C�@ C�33C�Y�C�Y�C�@ C�ffC�Y�C�@ C�33C�&fC�L�C�@ C�&fC�@ C�L�C�Y�C�L�C�&fC�@ C�@ C�Y�C�L�C�&fC�@ C�L�C�@ C�&fC�33C�L�C�ffC�s3D �D � D&fD�fD,�D� Ds3D	� DffDٚDY�D� DY�D� D  Dy�D�3D"  D$l�D&�3D)FfD+� D.&fD0��D39�D5�fD8ffD;  D=ٚD@y�DC�DE� DH33DJ��DM,�DO�fDR,�DT��DW9�DY��D\  D^��D`�3Dc` DeٚDh9�Dj� DmfDoS3Dq�3Dt3Dvl�Dx� D{  D}�Ds3D��fD�fD�<�D�i�D��3D�� D�� D���D�,�D�VfD�|�D���D�� D� D�9�D�c3D���D��3D��fD�33D�l�D��fD���D��D�9�D�p D�� D��3D��D�FfD�� D��3D���D�#3D�\�D��fD��3D��3D�)�D�\�D���D���D��fD�	�D�,�D�L�D�vfD�� D��fD��fD��fD� D�&fD�@ D�VfD�p DfDÓ3DĦfD��3D��fD��D�3D� D�&fD�C3D�c3D΀ Dϙ�Dй�D�ٚD�� D��D�)�D�I�D�l�Dؐ D٬�D���D���D��D�  D�@ D�` D�s3D≚D�fD�fD�3D湚D�ɚD��fD�� D��D��3D���D�  D�  D���D��fD���D��fD��fD�  D���D��fD���D�� D��3D���D��fD��3D���D��fD���E @ E �3E4�E� E+3E��E� E��E E  E{3E	t�E
��EQ�EC3E��E	�E�3Ed�E�fE� E6fE��E� E�E�fE[3E�fE�3E fE!y�E"� E#�3E%3E&p E'�3E)fE*X E+>fE,��E-� E/9�E0�fE1s3E2�3E4.fE5��E6��E7�fE9\�E:P E;� E>�3EA�3EE  EH,�EKL�EN^fEQVfET��EW�fEZ��E]�fEaD�EdX Eg\�Ej�3Emp Ep�fEs�fEw3EzfE}C3E�6fE��3E�8�E�ŚE�X E��E���E�fE�W3E�� E���E�\ E��3E�� E�;3E���E��fE�0 E�o3E��fE�#3E�rfE��fE��E�]�E�� E���E�P�E��3E��fE�FfE��3E��3E�33E�vfE��3E��E�m�E��f?�  G�O�?fffG�O�G�O�G�O�?fffG�O�G�O�G�O�?fff?�  G�O�?�  ?���G�O�?�ff?�  G�O�?���G�O�?�33@33@   @,��@Fff@S33@l��@y��@�ff@�ff@�  @���@�33@�  @���@�ff@�33@���@�ffA��A  A  A��A33A!��A(  A0  A4��A<��AC33AK33AQ��AX  A^ffAd��Al��As33A{33A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141444144411411411414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ j@ v@ �@ {@ �@ "�@ )�@ /@ 6�@ =q@ E�@ SI@ `B@ m�@ |?@ �7@ ��@ ��@ �-@ ��@ ��@ ��@ �@ � @j@b@
@+�@:@G�@UU@b�@qS@�@��@��@�A@��@@�7@�/@��@�~@�@�@"�@/�@=q@K�@Z@ff@t@�d@�@�U@��@�@ƨ@խ@�@�@�E@
=@�@$.@4�@A�@N�@\)@hs@x&@��@�u@�m@�@�@�@�
@�@�@  @�@�@(G@7L@DD@Q=@`B@n�@{�@�+@��@�5@�-@�w@��@�#@�y@��@@b@�@-�@<@I�@Wb@e	@qS@|�@��@�<@��@�9@@��@�;@�@�~@�@�@"�@/�@>�@K�@X@e�@r�@�d@�\@�U@�@�@��@��@��@��@�E@
=@�@$�@4�@B8@M�@\)@j@x&@�|@�h@�@�@��@��@�h@�@�Y@  @V@O@(�@5�@DD@Q�@_�@l�@z3@��@��@��@�!@��@��@�@�m@� @	j@	@	[@	+�@	9X@	G�@	V@	e	@	p�@	|?@	�D@	��@	��@	��@	�>@	��@	ލ@	�4@	��@
1@
�@
$.@
1�@
>@
I�@
X@
ff@
t�@
��@
�h@
��@
��@
�@
�W@
Ӡ@
��@
��@
�E@�@�@$�@33@B8@O0@\)@k.@ww@��@��@��@��@�^@�@�@�@�@  @�@�@*S@6�@FQ@SI@_�@l�@y�@�7@�0@�y@�~@��@�*@�#@�@��@j@o@g@+@:@H]@UU@a�@o�@~�@��@�U@��@��@�>@��@�;@�@oF@�~@�q@9X@}�@@%@G�@��@ȴ@�@G�@�|@�@
�@N�@�@��@�@`A@��@�Y@<�@�p@��@@Wb@�U@�;@"�@g�@�f@�@5�@ww@��@�9@=q@�@��@j@D�@��@Ĝ@v@E�@�@��@��@<@|?@�@��@<@{�@�R@��@5@@v�@�F@�@5�@ww@�R@��@7L@v�@�@��@ >@ �@ ��@!%@!F�@!�+@!�c@"
=@"K�@"��@"є@#{@#V@#��@#�#@$
@$`�@$�@$��@%$/@%e�@%��@%�@&&�@&e	@&�(@&��@' @'\)@'��@'�h@(�@(Q�@(�P@(�c@)�@)@�@)|?@)��@)�L@*-@*g�@*�z@*ލ@+�@+SI@+�@+�|@,
=@,FQ@,��@,�2@,��@-9X@-v@-��@-�@./�@.l�@.��@.�m@/$�@/_�@/�@/�t@0*@0P�@0��@0��@0�E@15@@1oF@1��@1�H@2�@2R�@2�D@2@2�,@3/@3dZ@3��@3є@41@4@�@4v�@4�@4��@5{@5H]@5y�@5�r@5��@6�@6Lu@6�@6��@6�y@7[@7Q�@7�|@7�^@8%�@8��@9,`@9�0@:7�@:�z@;?}@;�#@<B8@<�#@=qS@=�h@>r�@?b@?ww@@�@@�!@A{@A�-@B�@B��@CM�@C��@DJi@D��@Ez�@E��@Fn�@G  @G��@H	@H��@I�@I��@J,`@J��@KQ=@K��@LE�@L�/@Mv@M܀@Nv�@O@Oz�@P{@Quk@R�@T
=@Ue	@V�^@X	�@YM�@Z�^@\�@]P�@^��@`�@ai�@b��@d�@eK@f��@g�@ig�@j��@l�@me	@n�@o�q@qI@r�m@s�@uV�@v��@v�@w6�@wn�@w@w��@x33@x�@x�|@y@yQ�@y��@yլ@z!s@ze	@z��@z��@{-�@{ww@{�Z@{�E@|5�@|��@|��@}6@}[z@}��@}�C@~+�@~V@~�5@~�@ jG�O�@ �G�O�G�O�G�O�@ �G�O�G�O�G�O�@ �@ jG�O�@ j@ �G�O�@ v@ �G�O�@ �G�O�@ 	�@ J@ �@ @ �@ @ �@ 6@ B@ �@ �@  �@ "�@ %�@ (G@ *S@ -@ /@ 1'@ 3�@ 6�@ :@ <@ >�@ A�@ DD@ G�@ I�@ M$@ O�@ SI@ V@ X�@ [z@ ^5@ a�@ dZ@ g�@ jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A��A���A���A��A��A��yA��;A��/A��
A���A�ȴA�ƨA�AԾwAԴ9AԴ9AԴ9AԴ9A԰!AԬAԩ�Aԣ�Aԣ�Aԛ�AԑhA�^5A�VA�ȴA��
A�x�A���A΋DA���A˟�A�`BA�`BA�bA���A��A�C�A��#A��wA�n�A�S�A���A�$�A�G�A��A�O�A��wA�C�A�G�A�dZA��A���A��A��A�-A���A��#A�C�A�S�A���A��A�C�A��A��RA�`BA�  A�oA��/A�A���A��+A��RA�$�A��\A�
=A�hsA�r�A�bNA�ffA�bA�33A�(�A��`A�-A�1'A��A���A���A��PA���A��A�VA�VA��uA�ƨA���A��+A���A�A�C�A�G�A��
A���A��A�|�A�$�A���A�~�A�JA��A��jAt�A~r�A}�mA}K�A|n�A{��Az��Az�AzI�Ay��Aw�^Av�/Au7LAo�
Al�\AihsAf�+Ac�-Ac/Ab�Aa�Aa�
Aax�A`�yA`�A_XA^�DA\ZAXjAV��AU��AU%AT(�AShsAO��AI��AI�AHbNAF��AE��AEAD~�AC�#AB��ABQ�A@��A?A=?}A;��A:��A9�#A97LA8jA7ƨA6��A5|�A4E�A3/A2�A29XA1��A1�wA1O�A1
=A0��A/�;A.VA-hsA,I�A*-A({A&�DA%�A$�A"�yA"^5A!ƨA ��A�#A|�A��A|�AA�AoA�TA%AffA�TA\)AA�AA�AbA��AA��AO�A�mA�DA�/A
��A	?}A�mAƨA��AA�A �A`BA��AI�A|�A�A �`A �uA v�A �@���@�ƨ@��D@�`B@��9@���@�O�@�ƨ@�|�@�v�@�%@��;@���@���@�b@�E�@��@���@�@睲@��y@噚@㕁@�+@◍@�7L@�j@��@݉7@�+@���@ˍP@ț�@�\)@�"�@�J@�
=@�33@��w@�@��/@��
@��9@���@��@�9X@���@�p�@�v�@�33@�@��H@��y@�+@��;@��-@���@�x�@�l�@��m@�l�@��P@���@��`@�dZ@�o@�ff@���@�V@�dZ@��h@���@��@��P@��@���@���@� �@|�/@z�@y�@y�@x �@w
=@sdZ@r��@q��@q&�@o��@m?}@j�\@h�9@g
=@d��@cdZ@ahs@_��@^ff@]��@\Z@Z�@Z=q@W�;@U�h@T1@R��@P��@PA�@O�@M��@L��@Kt�@I�@G+@Fv�@E�@EV@CC�@C33@B-@A�7@@��@@ �@?;d@=��@<�@;�m@;�F@:��@9��@8  @6��@5�-@5V@41@2�!@1�@1hs@0��@0bN@/�w@.v�@-/@,I�@+��@*^5@(�9@(Q�@'��@'�@&@%�-@$z�@#�m@"��@"-@!%@�y@��@��@ƨ@o@�@X@�@r�@��@�y@��@��@S�@M�@��@�;@5?@��@�@I�@�
@ƨ@t�@dZ@
��@	��@	G�@��@�w@;d@
=@E�@@��@�@�/@Z@ƨ@t�@"�@��@�\@7L@ b?�j?�=q?�l�?��+?�F?�G�??�O�?�^?�7L?���?�`B?�S�?�!?���?�A�?ߝ�?�|�?�v�?�/?ܬ?ۥ�?�dZ?ٺ^?�Q�?���?�ff?�?}?�Z?�t�?��?��?��`?�|�?θR?�v�?͑h?̬?�1?�ƨ?�?�=q?��?�X?��y?��?�S�?��7?��?��;?���?��R?�{?�/?�j?���?��?��H?�^5?���?�^5?�^5?���?���?���?�"�?�ƨ?��m?�j?��?�O�?��?���?��R?��?���?��?�;d?�\)?�;d?�|�?�|�?���?���?��;?�  ?� �?�A�?��?���?��`?��`?�&�?�&�?�hs?��7?���?���?��?�J?�M�?�M�?\?°!A��A���A���A��A��A���A��A��A��A��A��A���A���A���A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��yA��mA��`A��TA��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A���A���A���A���A���A���A���A��A���A���A��A��A��yA��;A��/A��
A���A�ȴA�ƨA�AԾwAԴ9AԴ9AԴ9AԴ9A԰!AԬAԩ�Aԣ�Aԣ�Aԛ�AԑhA�^5A�VA�ȴA��
A�x�A���A΋DA���A˟�A�`BA�`BA�bA���A��A�C�A��#A��wA�n�A�S�A���A�$�A�G�A��A�O�A��wA�C�A�G�A�dZA��A���A��A��A�-A���A��#A�C�A�S�A���A��A�C�A��A��RA�`BA�  A�oA��/A�A���A��+A��RA�$�A��\A�
=A�hsA�r�A�bNA�ffA�bA�33A�(�A��`A�-A�1'A��A���A���A��PA���A��A�VA�VA��uA�ƨA���A��+A���A�A�C�A�G�A��
A���A��A�|�A�$�A���A�~�A�JA��A��jAt�A~r�A}�mA}K�A|n�A{��Az��Az�AzI�Ay��Aw�^Av�/Au7LAo�
Al�\AihsAf�+Ac�-Ac/Ab�Aa�Aa�
Aax�A`�yA`�A_XA^�DA\ZAXjAV��AU��AU%AT(�AShsAO��AI��AI�AHbNAF��AE��AEAD~�AC�#AB��ABQ�A@��A?A=?}A;��A:��A9�#A97LA8jA7ƨA6��A5|�A4E�A3/A2�A29XA1��A1�wA1O�A1
=A0��A/�;A.VA-hsA,I�A*-A({A&�DA%�A$�A"�yA"^5A!ƨA ��A�#A|�A��A|�AA�AoA�TA%AffA�TA\)AA�AA�AbA��AA��AO�A�mA�DA�/A
��A	?}A�mAƨA��AA�A �A`BA��AI�A|�A�A �`A �uA v�A �@���@�ƨ@��D@�`B@��9@���@�O�@�ƨ@�|�@�v�@�%@��;@���@���@�b@�E�@��@���@�@睲@��y@噚@㕁@�+@◍@�7L@�j@��@݉7@�+@���@ˍP@ț�@�\)@�"�@�J@�
=@�33@��w@�@��/@��
@��9@���@��@�9X@���@�p�@�v�@�33@�@��H@��y@�+@��;@��-@���@�x�@�l�@��m@�l�@��P@���@��`@�dZ@�o@�ff@���@�V@�dZ@��h@���@��@��P@��@���@���@� �@|�/@z�@y�@y�@x �@w
=@sdZ@r��@q��@q&�@o��@m?}@j�\@h�9@g
=@d��@cdZ@ahs@_��@^ff@]��@\Z@Z�@Z=q@W�;@U�h@T1@R��@P��@PA�@O�@M��@L��@Kt�@I�@G+@Fv�@E�@EV@CC�@C33@B-@A�7@@��@@ �@?;d@=��@<�@;�m@;�F@:��@9��@8  @6��@5�-@5V@41@2�!@1�@1hs@0��@0bN@/�w@.v�@-/@,I�@+��@*^5@(�9@(Q�@'��@'�@&@%�-@$z�@#�m@"��@"-@!%@�y@��@��@ƨ@o@�@X@�@r�@��@�y@��@��@S�@M�@��@�;@5?@��@�@I�@�
@ƨ@t�@dZ@
��@	��@	G�@��@�w@;d@
=@E�@@��@�@�/@Z@ƨ@t�@"�@��@�\@7L@ b?�j?�=q?�l�?��+?�F?�G�??�O�?�^?�7L?���?�`B?�S�?�!?���?�A�?ߝ�?�|�?�v�?�/?ܬ?ۥ�?�dZ?ٺ^?�Q�?���?�ff?�?}?�Z?�t�?��?��?��`?�|�?θR?�v�?͑h?̬?�1?�ƨ?�?�=q?��?�X?��y?��?�S�?��7?��?��;?���?��R?�{?�/?�j?���?��?��H?�^5?���?�^5?�^5?���?���?���?�"�?�ƨ?��m?�j?��?�O�?��?���?��R?��?���?��?�;d?�\)?�;d?�|�?�|�?���?���?��;?�  ?� �?�A�?��?���?��`?��`?�&�?�&�?�hs?��7?���?���?��?�J?�M�?�M�?\?°!A��A���A���A��A��A���A��A��A��A��A��A���A���A���A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��yA��mA��`A��TA��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BS�BS�BS�BS�BS�BS�BS�BS�BS�BT�BS�BS�BS�BR�BS�BR�BR�BR�BR�BR�BR�BR�BQ�BR�BR�BQ�BQ�BQ�BQ�BP�BQ�BQ�BO�BS�BVB]/BbNBhsBl�Bz�B�+B�hB��B�/B�ZB�fB�yB�B��BB
=BbBuB�B%�B2-B1'B6FB;dBG�BH�BJ�BL�BP�B\)BaHBe`BiyBgmBl�BjBiyBgmBgmBdZBcTB]/BP�BP�BL�BI�B?}B?}B8RB5?B�B�BhB1B�B�B�mB�B��BÖB�LB��B��B�!BƨB��BɺB�^B�LB��B�JBw�BcTB[#BK�B{B
��B
�)B
�B
��B
��B
��B
�1B
s�B
Q�B
G�B
8RB
2-B
-B
(�B
$�B
�B
�B
�B
�B
oB
	7B
1B	��B	��B	��B	��B	��B	�7B	�B	x�B	t�B	q�B	l�B	e`B	`BB	[#B	S�B	E�B	;dB	5?B	1'B	,B	$�B	�B��B�B��B��B�B�ZB�HB�TB�`B�ZB�;B��B��B��B��BƨB��B�}B�wB�qB�LB�!B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�+B�B�B�1B�B~�B|�By�Bz�B{�B}�By�Bx�Bs�Br�Bo�Bn�Bl�Bk�BiyBhsBhsBffB`BBaHB_;BXBO�BO�BI�BB�BA�B@�B=qB7LB;dB9XB;dB>wBF�BL�BM�BM�BK�BL�BM�BP�BP�BO�BXBZB^5B_;Be`BffBgmBiyBjBl�Bl�Bm�Bk�Bl�Bk�Bn�Bm�Bn�Bn�Bp�Bq�Bp�Bu�Bw�Bv�Bw�B�B�=B�VB��B��BƨB�}BĜB�/B�B	B	�B	%�B	:^B	33B	1'B	5?B	I�B	J�B	D�B	E�B	I�B	O�B	S�B	^5B	iyB	�+B	�VB	�B	�FB	B	��B	ÖB	ǮB	��B	��B	��B	��B	�B	�BB	�fB	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
%B
+B
1B
JB
JB
PB
VB
hB
oB
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
%�B
&�B
&�B
+B
-B
.B
.B
/B
1'B
1'B
49B
33B
7LB
9XB
:^B
<jB
<jB
=qB
?}B
>wB
@�B
@�B
A�B
A�B
B�B
D�B
D�B
F�B
E�B
F�B
G�B
I�B
J�B
K�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
R�B
T�B
T�B
T�B
W
B
YB
YB
YB
ZB
\)B
[#B
]/B
]/B
^5B
_;B
`BB
bNB
aHB
bNB
dZB
dZB
e`B
e`B
ffB
ffB
gmB
iyB
jB
iyB
jB
k�B
jB
m�B
p�B
p�B
q�B
q�B
s�B
r�B
r�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
� B
�B
�B
�B
�+B
�1B
�=B
�DB
�DB
�PB
�bB
�bB
�bB
�uB
�{B
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
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�RB
�XB
�XB
�XB
�XB
�^B
�^B
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�dB
�jB
�jB
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
�jB
�dB
�jB
�jB
�jB
�jB
�dB
�dB
�jB
�dBT�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BR�BT�BS�BS�BS�BT�BT�BS�BQ�BT�BS�BS�BS�BS�BS�BS�BS�BT�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BT�BS�BS�BS�BT�BS�BS�BS�BS�BR�BS�BT�BS�BS�BS�BS�BS�BS�BS�BR�BR�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BQ�BQ�BQ�BP�BQ�BP�BP�BP�BP�BP�BP�BP�BO�BP�BP�BO�BO�BO�BO�BN�BO�BO�BM�BQ�BS�B[#B`BBffBjBx�B�B�\B��B�#B�NB�ZB�mB�B��BB1BVBhB{B#�B0!B/B49B9XBE�BF�BH�BJ�BN�BZB_;BcTBgmBe`BjBhsBgmBe`Be`BbNBaHB[#BN�BN�BJ�BG�B=qB=qB6FB33B�B�B\B%B�B�B�`B�
B��B��B�?B��B��B�BĜB��BǮB�RB�?B��B�=Bu�BaHBYBI�BoB
��B
�B
��B
��B
��B
�uB
�%B
q�B
O�B
E�B
6FB
0!B
+B
&�B
"�B
�B
�B
�B
uB
bB
+B
%B	�B	��B	�}B	��B	�uB	�+B	~�B	v�B	r�B	o�B	jB	cTB	^5B	YB	Q�B	C�B	9XB	33B	/B	)�B	"�B	�B�B�B��B�B�B�NB�;B�HB�TB�NB�/B��B��BɺBȴBĜB�}B�qB�jB�dB�?B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�B�B� B�%B�B|�Bz�Bw�Bx�By�B{�Bw�Bv�Bq�Bp�Bm�Bl�BjBiyBgmBffBffBdZB^5B_;B]/BVBM�BM�BG�B@�B?}B>wB;dB5?B9XB7LB9XB<jBD�BJ�BK�BK�BI�BJ�BK�BN�BN�BM�BVBXB\)B]/BcTBdZBe`BgmBhsBjBjBk�BiyBjBiyBl�Bk�Bl�Bl�Bn�Bo�Bn�Bs�Bu�Bt�Bu�B�B�1B�JB��B��BĜB�wBÖB�)B�B	  B	�B	$�B	9XB	2-B	0!B	49B	H�B	I�B	C�B	D�B	H�B	N�B	R�B	]/B	hsB	�%B	�PB	�B	�?B	��B	�}B	B	ƨB	ɺB	��B	��B	��B	�B	�;B	�`B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
%B
+B
DB
DB
JB
PB
bB
hB
{B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
%�B
%�B
)�B
,B
-B
-B
.B
0!B
0!B
33B
2-B
6FB
8RB
9XB
;dB
;dB
<jB
>wB
=qB
?}B
?}B
@�B
@�B
A�B
C�B
C�B
E�B
E�B
F�B
G�B
I�B
J�B
K�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
R�B
T�B
T�B
T�B
W
B
YB
YB
YB
ZB
\)B
[#B
]/B
]/B
^5B
_;B
`BB
bNB
aHB
bNB
dZB
dZB
e`B
e`B
ffB
ffB
gmB
iyB
jB
iyB
jB
k�B
jB
m�B
p�B
p�B
q�B
q�B
s�B
r�B
r�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
� B
�B
�B
�B
�+B
�1B
�=B
�DB
�DB
�PB
�bB
�bB
�bB
�{B
��B
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
�B
�B
�B
�!B
�-B
�3B
�3B
�9B
�?B
�FB
�LB
�RB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�jB
�jB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�}B
�wB
�}B
�}B
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
�}B
�wB
�}B
�}B
�}B
�}B
�wB
�wB
�}B
�wBR�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BR�BQ�BQ�BQ�BR�BR�BQ�BO�BR�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BQ�BQ�BQ�BR�BQ�BQ�BQ�BQ�BP�BQ�BR�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313462021061413523520210614135235202106141746412021061417464120210614174641201809172313462021061413523520210614135235202106141746412021061417464120210614174641PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134620180917231346  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134620180917231346QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134620180917231346QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015220210722160152IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                