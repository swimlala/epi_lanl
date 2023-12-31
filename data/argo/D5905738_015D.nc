CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:35Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        d0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        tP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    \   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        |   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	�      � 	�Argo profile    3.1 1.2 19500101000000  20180724220235  20210722160150  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�l�Y��r@�l�Y��r11  @�l�O�`@�l�O�`@7����[@7����[�c�{J#9��c�{J#9�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?fff@   @Fff@�33@�  @�  @�33A��A  A#33A>ffAa��A�ffA���A�ffA�33A�33A���AᙚA�B ��B  B��B  B   B(��B0��B8ffB@  BH  BP  BX��B`��Bg��Bo��BxffB�33B�33B�ffB�33B�  B�  B�33B�33B�  B�33B�  B�  B�33B�ffB�ffB�33B�ffB�ffB�ffB̙�B�  B�  B�33B�33B�ffB㙚B���B�  B�  B�33B�  B�  C   C  C�fC�fC�fC	�fC�fC33C�C�C  C�fC  C  C��C�C�fC!�fC$33C&�C'�fC*�C,�C-�fC033C2�C3��C6�C8L�C:33C<  C>33C@�CA�fCD33CF�CH  CI�fCK�fCN33CP33CR  CT  CV  CW�fCY�fC\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCk�fCm�fCp33Cr33Ct�Cv�Cw��Cz�C|  C}��C�  C��3C�ٚC�  C��C��C��3C�  C�&fC��C��3C��C��C�  C��3C�ٚC�  C�  C�&fC��C�  C��C��C��3C��C��C�  C��C��C��3C��C��C��C��C�  C�  C��3C��3C��fC��C�  C��3C��C��C�  C�&fC��C��C��C��3C��C��C�  C��C��C��C�  C��3C��C�  C��3C��C��C�  C��3C��fC�  C�&fC��C��3C��C�  C��3C��C�&fC��C��3C�  C��C��C�&fC��C��fC�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��3C��3C��fC��fC�  C�&fC�&fC��C��C��C��C��C�  C��3C��fC��fC��C�&fC�33C�&fC�&fC�&fC�&fC�&fC��C��C��C�&fC�  D&fD�3DL�D	ٚDs3D  DٚD�3DY�D&fD�3D��D"�3D%��D(S3D+3D-��D0l�D333D5�fD8��D;,�D=�3D@y�DC�DE��DH,�DJ��DM33DO��DR@ DT��DWFfDY� D\FfD^��DaL�Dc�3DfFfDh��Dk3Dm�fDp  Drl�Dt�fDwL�Dy�3D{��D~�D�@ D�p D��fD��3D�fD�S3D��fD�� D���D�0 D�l�D���D��fD�  D�\�D���D�ٚD�#3D�ffD��3D���D�0 D�s3D�� D���D�33D�vfD���D�3D�P D��3D�ٚD�  D�ffD���D�� D�<�D�y�D��fD��fD�9�D�|�D�� D���D�9�D�vfD���D�  D�<�D�y�D��fD���D��D�S3DÆfD�� D��3D�  D�VfD�|�Dʰ D���D�	�D�0 D�S3D�vfDљ�DҼ�D���D��D�33D�\�D؃3D٩�D��fD���D� D�6fD�c3D��fD��D��3D� D�6fD�\�D��D蹚D�ٚD�  D�  D�<�D�` D�fD�fD��fD���D��fD� D�  D�@ D�VfD�s3D�p D���D��fD��fD���D��3E y�E�E��E!�E�fE8 E��EQ�EٚEh E�3E�3E�E�3E��E	�3E	�E� E�E�E+3ES3E3E1�EVfE{3E�fEI�Ec3E|�E�3E>fEX E q�E"	�E# E$$�E%� E&��E'�3E)P E*P E+�3E,�fE.4�E/�3E0��E1�E3Y�E4H E5� E7 E8fE9` E:�3E< E?FfEBfEET�EH�3EK��ENx EQ� EU�EW� E[)�E^@ EaS3Ed� Eg��Ej� Em��EqfEt)�EwD�EznfE}�fE�X E��3E�` E��E���E�fE��3E�:fE��fE�P E���E�ffE��E�Q�E��3E���E�L�E��3E��3E�)�E���E�� E�3E�zfE��fE� ?   >���>���>���>���?��?   ?   ?��?L��?333?fff?�  ?���?�ff?�33?���?�ff@ff@��@,��@9��@S33@`  @s33@�ff@���@���@�ff@�33@���@���@�ff@�33@���@���A��A��A33A33A#33A+33A333A;33AC33AI��AQ��AX  Ac33Ai��Ap  Ax  A���A���A�  A�  A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414411411111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?fff?�33@   @fff@�33@�  @�  @�33A	��A  A+33AFffAi��A�ffA���A�ffA�33A�33A���A噚A���B��B
  B��B  B"  B*��B2��B:ffBB  BJ  BR  BZ��Bb��Bi��Bq��BzffB�33B�33B�ffB�33B�  B�  B�33B�33B�  B�33B�  B�  B�33B�ffB�ffB�33B�ffB�ffB�ffB͙�B�  B�  B�33B�33B�ffB䙚B���B�  B�  B�33B�  B�  C � C� CffCffCffC
ffCffC�3C��C��C� CffC� C� CL�C��C ffC"ffC$�3C&��C(ffC*��C,��C.ffC0�3C2��C4L�C6��C8��C:�3C<� C>�3C@��CBffCD�3CF��CH� CJffCLffCN�3CP�3CR� CT� CV� CXffCZffC\� C^� C`� Cb� Cd� Cf� Ch� CjffClffCnffCp�3Cr�3Ct��Cv��CxL�Cz��C|� C~L�C�@ C�33C��C�@ C�Y�C�L�C�33C�@ C�ffC�Y�C�33C�Y�C�L�C�@ C�33C��C�@ C�@ C�ffC�Y�C�@ C�Y�C�Y�C�33C�Y�C�L�C�@ C�Y�C�L�C�33C�Y�C�L�C�L�C�L�C�@ C�@ C�33C�33C�&fC�L�C�@ C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�L�C�33C�Y�C�L�C�@ C�Y�C�L�C�L�C�@ C�33C�Y�C�@ C�33C�L�C�L�C�@ C�33C�&fC�@ C�ffC�L�C�33C�Y�C�@ C�33C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�@ C�@ C�@ C�33C�33C�33C�33C�33C�@ C�@ C�33C�33C�&fC�&fC�@ C�ffC�ffC�Y�C�Y�C�L�C�Y�C�L�C�@ C�33C�&fC�&fC�L�C�ffC�s3C�ffC�ffC�ffC�ffC�ffC�Y�C�Y�C�Y�C�ffC�@ DFfD�3Dl�D	��D�3D@ D��D�3Dy�DFfD3D��D"�3D%��D(s3D+33D-��D0��D3S3D6fD8��D;L�D=�3D@��DC,�DE��DHL�DJٚDMS3DOٚDR` DT��DWffDY� D\ffD^��Dal�Dc�3DfffDhٚDk33Dm�fDp  Dr��DufDwl�Dy�3D{��D~9�D�P D�� D��fD��3D�&fD�c3D��fD�� D�	�D�@ D�|�D���D��fD�0 D�l�D���D��D�33D�vfD��3D���D�@ D��3D�� D���D�C3D��fD���D�3D�` D��3D��D�0 D�vfD���D�  D�L�D���D��fD�fD�I�D���D�� D��D�I�D��fD���D� D�L�D���D��fD���D�,�D�c3DÖfD�� D�3D�0 D�ffDɌ�D�� D���D��D�@ D�c3DІfDѩ�D���D���D��D�C3D�l�Dؓ3Dٹ�D��fD���D�  D�FfD�s3D�fD���D��3D�  D�FfD�l�D��D�ɚD��D� D�0 D�L�D�p D�fD�fD��fD���D�fD�  D�0 D�P D�ffD��3D�� D���D��fD��fD���D��3E ��E�E��E)�E�fE@ E��EY�E�Ep E�3E�3E�E�3E��E	�3E�E� E�E�E33E[3E3E9�E^fE�3E�fEQ�Ek3E��E�3EFfE` E y�E"�E#  E$,�E%� E&ɚE'�3E)X E*X E+�3E,�fE.<�E/�3E0��E1��E3a�E4P E5� E7  E8fE9h E:�3E<  E?NfEBfEE\�EH�3EKɚEN� EQ� EU	�EW� E[1�E^H Ea[3Ed� Eg��Ej� Em��EqfEt1�EwL�EzvfE}�fE�\ E��3E�d E�	�E���E�"fE��3E�>fE��fE�T E���E�jfE��E�U�E��3E���E�P�E��3E��3E�-�E���E�� E�#3E�~fE��fE� G�O�G�O�G�O�G�O�?fffG�O�G�O�?�  ?���G�O�?���?�33?�  ?���?�ff?�33@ff@33@&ff@9��@L��@Y��@s33@�  @���@�ff@���@���@�ff@�33@���@���@�ff@�33@���AffA��A��A33A#33A+33A333A;33AC33AK33AQ��AY��A`  Ak33Aq��Ax  A�  A���A���A�  A�  A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414411411111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ �@ %@ �@ *@ �@ ""@ (�@ 0x@ 7L@ =q@ E�@ Q=@ `B@ oF@ {�@ �+@ ��@ �(@ �-@ ��@ �*@ ��@ �@ �@j@@ @-�@:�@G�@UU@b�@r@�@�D@��@��@��@�>@є@ލ@�@�,@�@*@""@0x@=q@K@Yn@g�@uk@�d@��@�a@�@�^@��@Ӡ@��@�@��@�@6@%�@33@A�@N�@\)@i�@ww@�p@�@��@�f@�@��@׹@�`@�Y@�Q@�@O@'�@7L@C�@Q=@`�@m�@z3@�7@��@�(@��@��@�o@�#@�(@� @j@o@g@+�@;d@H]@UU@bN@o�@�@�P@��@�A@��@��@ψ@��@�@�,@�@{@""@/�@<�@Ji@X@g�@uk@�d@�@��@�Y@�R@Ĝ@Ӡ@��@��@��@�@�@$�@33@B�@O�@[z@k.@x&@�@�@�a@�@��@�o@�h@�@�@^@�@�@)�@6�@E�@R�@^�@n�@{�@�7@��@��@�~@�w@�@�@��@��@	�@	o@	g@	,`@	<@	I@	V@	c�@	o�@	�@	��@	��@	��@	��@	�>@	�7@	�/@	��@	�,@
%@
*@
"�@
/�@
<�@
I�@
X�@
hs@
t�@
�@
��@
�@
��@
�@
�@
�O@
��@
��@
�E@�@�@&;@1�@@�@N�@\)@i�@v�@�p@�@��@�f@��@�c@�[@�@��@��@�@[@+@7�@E�@R�@`�@m�@z�@��@��@�y@�-@�2@ψ@܀@�(@��@v@@ @-�@;d@I�@UU@��@'�@n�@�9@�9@DD@��@�@$�@qS@��@�@Z�@��@�e@?}@��@є@[@g@�r@� @?}@��@�*@�@Z@��@�T@(G@m:@��@�q@:@~�@��@1@M$@�@��@@V@��@��@g@`�@�y@�h@�@\)@�@�;@"�@dZ@��@�y@,`@oF@�~@�@8�@|?@�&@�@FQ@��@��@ �@ Yn@ ��@ �@!)�@!m:@!��@!�q@";d@"�@"ƨ@#�@#R�@#�<@#��@$#�@$i!@$�@$�@%8�@%|?@%��@&v@&Ji@&�\@&��@'�@'Z@'��@'�@((G@(k�@(�r@(�@)2�@)t�@)�F@)�,@*:�@*z�@*�@*��@+=q@+}�@+��@+��@,:�@,x�@,��@,�@-2�@-s_@-�-@-�@.0x@.oF@.�@.��@/(�@/g�@/��@/�y@0(G@0g@0�A@0�@1$�@1e�@1��@1�T@2""@2_�@2�U@2�t@3B@3V�@3�#@3ψ@4�@4G�@4��@4�&@4��@57L@5m:@5��@5�@6 @6[z@6��@6��@7J@7H]@7��@7��@7��@88�@8t�@8��@8�@9&�@9dZ@9�z@9�;@:Z�@:��@;R�@<�@<��@=�@=�@>]@>�@?:�@?��@@4�@@��@Ag@A�;@BWb@B�*@C�p@C��@Dt�@E"�@E�0@F�@F��@G&;@G��@H=q@H��@ILu@I�9@JS�@J�L@KV@K�4@L��@L�@M�@N�@N�p@O�@O�@P@�@Q�T@R�@T1�@U�#@V�@X�@Ym:@Z��@\�@]�d@^Ӡ@`#�@a��@bխ@d33@ehs@f�/@h-@i�W@j��@l5�@m�@n��@pO@q�@r�O@t'�@u�@v��@x+@yp�@z�O@|@}�@}�F@~%@~:�@~��@~��@�@I@��@��@�P@�49@�M�@�tG�O�G�O�G�O�G�O�@ �G�O�G�O�@ j@ G�O�@ �@ %@ �@ �@ �@ 	�@ 
�@ J@ V@ b@ o@ �@ �@ �@ �@ �@ 
@  �@ #�@ &;@ (G@ +�@ -�@ 0x@ 2�@ 5�@ 8�@ <@ >�@ B8@ E�@ I@ Lu@ O�@ SI@ V@ Yn@ \)@ `�@ c�@ ff@ i�@ m�@ qS@ t@ ww@ {�@ ~�@ �d@ ��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aϕ�Aϛ�Aϰ!AϾwAϼjAϺ^AϾwA���A�A�ĜA�ĜA���A���A���AϾwAϾwA�ĜA�ƨA�ĜA�A�ƨA�ȴA�A϶FAϧ�A�^5A���A��A��
Aʥ�A� �A�I�A�hsA��mA��A�ƨAú^A�l�A���A��HA��A�A�A���A�dZA�C�A��RA�dZA�&�A�A�+A�XA�A���A�G�A�VA���A�/A�A�  A�oA�ĜA�|�A�ƨA�E�A�ĜA��yA���A��A��A�ĜA�ĜA��wA��/A��+A�Q�A�&�A���A�dZA��A��A��+A�A�
=A�VA�G�A�E�A�A�A�v�A�9XA�{A��A���A��9A�r�A��DA��A��/A��hA��-A�oA�p�A�t�A�jA�ĜA�A��FA���A�hsA��A�ƨA�/A�ĜA��;A�ffA�A�9XA�$�A�I�A���A�l�A���A�dZA��RA��A��A���A�1'A�;dA}p�AyVAwO�AvbAt�As�TAp~�Ak��AjȴAh�jAd�RAb�uAa;dA_��A]�A[��A[�
A[��AZ�jAZ9XAZ{AY��AU�TAT��AT�+AS�^AR-AP�+AN��AI�AGx�AE�AC�ABQ�A@��A@�A@jA@bNA@r�A?dZA>�A>9XA=t�A;\)A9G�A8bA7p�A6�`A7�#A8E�A7t�A6-A4��A4 �A2��A0��A.�/A,�A+&�A* �A)�A(��A'O�A&�A%`BA$�RA${A#+A!ƨA �RAG�A^5A�AE�AS�A�A��A�mA�AZA�AdZA�uA�`A1A|�A\)A��A�\A{A�jA�
A
�!A	K�A�HAƨA�!Al�AoAȴA�RA�uA��A�A�A �/@��;@�5?@�O�@�(�@�M�@�Q�@�33@��H@�J@�v�@�r�@���@�&�@�j@��
@�P@���@�@�J@�x�@��@��
@Ӿw@ҏ\@�@��@��`@���@�;d@��#@���@�@���@�V@�5?@���@��#@�S�@��T@�G�@�C�@�?}@��j@�=q@��
@�=q@�A�@��R@��`@�33@�O�@��@��@��w@���@���@��y@���@�o@��
@�K�@��P@�ƨ@�ƨ@�@��u@��@�n�@��@��7@���@�(�@~V@|�D@z��@y7L@v$�@tj@r�H@q�@p1'@o|�@m@kC�@h��@f��@d��@b�!@b^5@ax�@^�@^$�@]��@[�@Y&�@X1'@V��@U/@TZ@R~�@P��@N��@L�@K�m@K"�@I��@HQ�@F�y@E�h@C�F@B��@BM�@@Ĝ@?�;@=��@<I�@;��@;"�@9��@8A�@7�;@5�@4�j@2=q@1X@0Q�@.�y@-�@,��@+33@*-@)G�@'�w@'l�@&E�@%�h@$��@#��@"�@"J@!�7@  �@�w@�@5?@9X@o@n�@�@�^@x�@�`@  @�@V@?}@��@9X@�F@�\@��@�7@�u@�;@�@��@�h@��@�@�
@"�@"�@
~�@
�@	%@A�@��@\)@
=@ff@�T@��@��@��@t�@��@^5@�7@hs@ ��@ 1'?�\)?�5??�I�?��?�b?�`B?���?�M�?�G�?�w?��?�C�?���?�X?��?��?�Z?��?�n�?�G�?��;?�|�?�5??��?�j?ڟ�?���?ؓu?���?��y?�$�?�`B?��?�t�?���?�G�?У�?� �?�\)?�v�?Ͳ-?���?�(�?�"�?�x�?�b?�l�?Ł?�9X?�S�?��?�%?���?���?�V?�p�?�V?��?�(�?�1?�(�?�ƨ?��m?�1?��m?��m?�(�?�j?��?��h?�{?���?��?�|�?�  ?��?��`?�hs?��7?��7?���?���?���?���?���?���?��?�J?�M�?��?�-AϓuAϓuAϏ\AϋDAϧ�Aϣ�Aϧ�Aϙ�Aϙ�Aϕ�Aϕ�AϓuAϓuAϑhAϡ�Aϣ�Aϧ�Aϧ�Aϴ9AϾwAϼjAϾwAϾwAϾwAϾwAϺ^AϺ^AϺ^AϺ^AϺ^AϾwA���A���A���A�A�A�A�ĜA�A�ĜA�A�ĜA�A���A���A���A���A���A���AϾwAϾwAϾwA���A���A���AϾwAϾwAϼjAϾwAϾwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Aϕ�Aϛ�Aϰ!AϾwAϼjAϺ^AϾwA���A�A�ĜA�ĜA���A���A���AϾwAϾwA�ĜA�ƨA�ĜA�A�ƨA�ȴA�A϶FAϧ�A�^5A���A��A��
Aʥ�A� �A�I�A�hsA��mA��A�ƨAú^A�l�A���A��HA��A�A�A���A�dZA�C�A��RA�dZA�&�A�A�+A�XA�A���A�G�A�VA���A�/A�A�  A�oA�ĜA�|�A�ƨA�E�A�ĜA��yA���A��A��A�ĜA�ĜA��wA��/A��+A�Q�A�&�A���A�dZA��A��A��+A�A�
=A�VA�G�A�E�A�A�A�v�A�9XA�{A��A���A��9A�r�A��DA��A��/A��hA��-A�oA�p�A�t�A�jA�ĜA�A��FA���A�hsA��A�ƨA�/A�ĜA��;A�ffA�A�9XA�$�A�I�A���A�l�A���A�dZA��RA��A��A���A�1'A�;dA}p�AyVAwO�AvbAt�As�TAp~�Ak��AjȴAh�jAd�RAb�uAa;dA_��A]�A[��A[�
A[��AZ�jAZ9XAZ{AY��AU�TAT��AT�+AS�^AR-AP�+AN��AI�AGx�AE�AC�ABQ�A@��A@�A@jA@bNA@r�A?dZA>�A>9XA=t�A;\)A9G�A8bA7p�A6�`A7�#A8E�A7t�A6-A4��A4 �A2��A0��A.�/A,�A+&�A* �A)�A(��A'O�A&�A%`BA$�RA${A#+A!ƨA �RAG�A^5A�AE�AS�A�A��A�mA�AZA�AdZA�uA�`A1A|�A\)A��A�\A{A�jA�
A
�!A	K�A�HAƨA�!Al�AoAȴA�RA�uA��A�A�A �/@��;@�5?@�O�@�(�@�M�@�Q�@�33@��H@�J@�v�@�r�@���@�&�@�j@��
@�P@���@�@�J@�x�@��@��
@Ӿw@ҏ\@�@��@��`@���@�;d@��#@���@�@���@�V@�5?@���@��#@�S�@��T@�G�@�C�@�?}@��j@�=q@��
@�=q@�A�@��R@��`@�33@�O�@��@��@��w@���@���@��y@���@�o@��
@�K�@��P@�ƨ@�ƨ@�@��u@��@�n�@��@��7@���@�(�@~V@|�D@z��@y7L@v$�@tj@r�H@q�@p1'@o|�@m@kC�@h��@f��@d��@b�!@b^5@ax�@^�@^$�@]��@[�@Y&�@X1'@V��@U/@TZ@R~�@P��@N��@L�@K�m@K"�@I��@HQ�@F�y@E�h@C�F@B��@BM�@@Ĝ@?�;@=��@<I�@;��@;"�@9��@8A�@7�;@5�@4�j@2=q@1X@0Q�@.�y@-�@,��@+33@*-@)G�@'�w@'l�@&E�@%�h@$��@#��@"�@"J@!�7@  �@�w@�@5?@9X@o@n�@�@�^@x�@�`@  @�@V@?}@��@9X@�F@�\@��@�7@�u@�;@�@��@�h@��@�@�
@"�@"�@
~�@
�@	%@A�@��@\)@
=@ff@�T@��@��@��@t�@��@^5@�7@hs@ ��@ 1'?�\)?�5??�I�?��?�b?�`B?���?�M�?�G�?�w?��?�C�?���?�X?��?��?�Z?��?�n�?�G�?��;?�|�?�5??��?�j?ڟ�?���?ؓu?���?��y?�$�?�`B?��?�t�?���?�G�?У�?� �?�\)?�v�?Ͳ-?���?�(�?�"�?�x�?�b?�l�?Ł?�9X?�S�?��?�%?���?���?�V?�p�?�V?��?�(�?�1?�(�?�ƨ?��m?�1?��m?��m?�(�?�j?��?��h?�{?���?��?�|�?�  ?��?��`?�hs?��7?��7?���?���?���?���?���?���?��?�J?�M�?��?�-AϓuAϓuAϏ\AϋDAϧ�Aϣ�Aϧ�Aϙ�Aϙ�Aϕ�Aϕ�AϓuAϓuAϑhAϡ�Aϣ�Aϧ�Aϧ�Aϴ9AϾwAϼjAϾwAϾwAϾwAϾwAϺ^AϺ^AϺ^AϺ^AϺ^AϾwA���A���A���A�A�A�A�ĜA�A�ĜA�A�ĜA�A���A���A���A���A���A���AϾwAϾwAϾwA���A���A���AϾwAϾwAϼjAϾwAϾwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�VB
�VB
�PB
�PB
�JB
�PB
�PB
�JB
�JB
�PB
�JB
�PB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�VB
�VB
�PB
�VB
�\B
�hB
��B
��B�B�B!�B �B"�B(�B7LB6FB;dBbNB�B�DB�uB��B�uB�BB��B��B�fB�yB�B�BBB	7BPBVB�B'�B(�B#�B+B/B1'B49B<jBA�BA�BF�BC�BB�BB�BaHB��B�'B�3B�?B�?B�FB�'B�B��B��B��B��B��B�JB~�Bk�B`BB^5B[#BXBT�BQ�BH�B9XB2-B.B�B�BbB1B��B�B�BB�BĜB�XB�9B��B�DB^5BM�B@�B;dB33B#�BVB+BB
��B
�mB
�B
�1B
t�B
iyB
e`B
]/B
/B
B	�)B	ÖB	�XB	�3B	�B	�=B	iyB	`BB	B�B	!�B	oB	B�B�B��B	�B	�B	�B	�B	�B	.B	!�B	 �B	�B	�B	1B��B�yB�qB�?B��B��B�=B�B�B�B�JB�oB�hB�\B��B��B�hB�B�B�B�+B��B��B��B��B��B��B��B�uB�=B�1B�B�B~�By�B{�By�Bz�B|�B|�Bx�B}�B{�Bu�Br�Bk�BjBbNBaHBaHB_;B]/B\)BZBZBXBVBR�BQ�BO�BJ�BL�BJ�BE�BD�B>wB=qB:^B8RB>wB<jB;dB;dB:^B8RB7LB7LB7LB9XB8RB:^B9XB7LB7LB<jB>wB>wB>wB=qB=qB=qB=qB=qB=qB<jB:^B:^B=qB=qB>wB=qBT�BXB��B�B�5B��B�dB��B��B��B��B�!B��B�#B�)B�`B	  B	DB	�B	�B	)�B	 �B	.B	E�B	gmB	r�B	w�B	}�B	�B	�B	�DB	��B	�!B	�3B	��B	�B	��B	�FB	�dB	ŢB	��B	�B	�/B	�5B	�B	�fB	�mB	�fB	�B	��B	��B	��B
  B
B
B
B
1B
	7B
JB
PB
bB
oB
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
$�B
&�B
&�B
'�B
(�B
(�B
)�B
,B
/B
0!B
0!B
0!B
2-B
49B
7LB
7LB
9XB
:^B
;dB
=qB
>wB
?}B
B�B
A�B
B�B
D�B
E�B
D�B
G�B
H�B
K�B
M�B
M�B
O�B
Q�B
Q�B
S�B
T�B
T�B
W
B
W
B
XB
YB
ZB
\)B
[#B
]/B
]/B
_;B
_;B
`BB
`BB
cTB
dZB
dZB
e`B
e`B
dZB
ffB
ffB
gmB
hsB
iyB
iyB
jB
l�B
l�B
n�B
m�B
o�B
o�B
p�B
p�B
q�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
{�B
{�B
}�B
|�B
}�B
~�B
�B
� B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�7B
�DB
�DB
�JB
�VB
�VB
�\B
�bB
�hB
�oB
�uB
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
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�?B
�LB
�LB
�LB
�RB
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
�dB
�jB
�dB
�jB
�jB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�dB
�qB
�qB
�VB
�PB
�PB
�\B
�VB
�PB
�JB
�VB
�PB
�VB
�hB
�PB
�VB
�VB
�JB
�PB
�PB
�\B
�PB
�JB
�JB
�PB
�JB
�PB
�JB
�PB
�JB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�PB
�JB
�JB
�JB
�PB
�PB
�PB
�JB
�PB
�PB
�JB
�PB
�PB
�JB
�PB
�JB
�JB
�PB
�JB
�JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        B
�JB
�JB
�DB
�DB
�=B
�DB
�DB
�=B
�=B
�DB
�=B
�DB
�DB
�DB
�=B
�=B
�=B
�=B
�DB
�JB
�JB
�DB
�JB
�PB
�\B
�{B
��B�B�B�B�B �B&�B6FB5?B:^BaHB�B�=B�hB�{B�oB�B��B��B��B�`B�sB�yB�B  BB1BJBPB�B&�B'�B"�B)�B.B0!B33B;dB@�B@�BE�BB�BA�BA�B`BB��B�!B�-B�9B�9B�?B�!B�B��B��B��B��B��B�DB}�BjB_;B]/BZBW
BS�BP�BG�B8RB1'B-B�B�B\B+B��B�yB�;B�BÖB�RB�3B��B�=B]/BL�B?}B:^B2-B"�BPB%B  B
��B
�fB
�B
�+B
s�B
hsB
dZB
\)B
.B
  B	�#B	B	�RB	�-B	��B	�7B	hsB	_;B	A�B	 �B	hB	B�B�B��B	{B	�B	�B	�B	�B	-B	 �B	�B	�B	�B	+B��B�sB�jB�9B��B��B�7B�B� B�B�DB�hB�bB�VB��B��B�bB�B�B�B�%B��B��B��B��B��B��B��B�oB�7B�+B�B� B}�Bx�Bz�Bx�By�B{�B{�Bw�B|�Bz�Bt�Bq�BjBiyBaHB`BB`BB^5B\)B[#BYBYBW
BT�BQ�BP�BN�BI�BK�BI�BD�BC�B=qB<jB9XB7LB=qB;dB:^B:^B9XB7LB6FB6FB6FB8RB7LB9XB8RB6FB6FB;dB=qB=qB=qB<jB<jB<jB<jB<jB<jB;dB9XB9XB<jB<jB=qB<jBS�BW
B��B�
B�/B��B�^BɺB��B��B��B�B��B�B�#B�ZB��B	
=B	{B	�B	(�B	�B	-B	D�B	ffB	q�B	v�B	|�B	�B	�B	�=B	��B	�B	�-B	��B	�B	��B	�?B	�^B	ĜB	ɺB	�
B	�)B	�/B	�B	�`B	�mB	�`B	�B	��B	��B	��B
  B
B
B
B
1B
	7B
JB
PB
bB
oB
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
$�B
&�B
&�B
'�B
(�B
(�B
)�B
,B
/B
0!B
0!B
0!B
2-B
49B
7LB
7LB
9XB
:^B
;dB
=qB
>wB
?}B
B�B
A�B
B�B
D�B
E�B
D�B
G�B
H�B
K�B
M�B
M�B
O�B
Q�B
Q�B
S�B
T�B
T�B
W
B
W
B
XB
YB
ZB
\)B
[#B
]/B
]/B
_;B
_;B
`BB
`BB
cTB
dZB
dZB
e`B
e`B
dZB
ffB
ffB
gmB
hsB
iyB
iyB
jB
l�B
l�B
o�B
n�B
p�B
p�B
q�B
q�B
r�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
|�B
|�B
~�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�=B
�=B
�JB
�JB
�PB
�\B
�\B
�bB
�hB
�oB
�uB
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
�B
�B
�B
�!B
�B
�-B
�3B
�9B
�9B
�?B
�LB
�RB
�RB
�^B
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�}B
�}B
�}B
�}B
��B
�}B
��B
��B
�}B
�}B
�}B
��B
��B
��B
��B
�}B
��B
��B
�JB
�DB
�DB
�PB
�JB
�DB
�=B
�JB
�DB
�JB
�\B
�DB
�JB
�JB
�=B
�DB
�DB
�PB
�DB
�=B
�=B
�DB
�=B
�DB
�=B
�DB
�=B
�DB
�DB
�DB
�DB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�=B
�DB
�=B
�=B
�=B
�DB
�DB
�DB
�=B
�DB
�DB
�=B
�DB
�DB
�=B
�DB
�=B
�=B
�DB
�=B
�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202352021061413521720210614135217202106141746252021061417462520210614174625201807242202352021061413521720210614135217202106141746252021061417462520210614174625PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023520180724220235  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023520180724220235QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023520180724220235QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015020210722160150IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                