CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-13T01:02:32Z creation      
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
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181113010232  20210722160155  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               *   *DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؍�ҭ��@؍�ҭ��11  @؍���� @؍���� @5��?��@5��?���c�Q�Q��c�Q�Q�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?L��@   @9��@�  @�33@�ff@�33A33A��A(  A@  A`  A�  A�  A���A�  A�  A���A�  A�  B ffB��B  BffB��B(  B0ffB8ffB@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B���B���B���B�  B�33B�ffB�ffB�33B�  B�33B�ffB�33B�  B�33B�ffB�33B�  B���B�ffB�ffB�ffB�33B�33B�33B�33B���B�ffB�33B�33B�  B���C �C�C  C33C33C
  CL�C�C�fC�C  C�fC33C�C�fC�C   C!��C#�fC&�C(�C*L�C,  C-��C0  C2�C4  C5�fC8  C:33C<L�C>L�C@�CA�fCD  CF�CH  CI��CK�fCM�fCP33CRL�CT  CU��CW�fCZ  C\�C^33C`�Ca�fCd  Cf�ChL�Cj33Cl  Cn  Co�fCr33CtL�Cv33Cx�CzL�C|  C}�fC��C��C��3C��C��C�  C�  C��3C��C��3C��fC��C�  C��fC�  C�  C��3C��C��C��3C��C��C�  C��C��C��3C��C�&fC��C�  C��C�  C��3C��C�&fC��C��fC�  C��C��C��3C��C��C�  C��3C��C��C�&fC��C��fC��3C�  C��C��C�&fC��C��fC��3C��3C�  C�  C�  C��C��C��C��C��C��fC��fC��fC��fC��fC��fC��fC��fC�  C��3C��3C��3C�  C�  C��C�  C�  C��C��C��C��C��C��C��C�&fC�&fC��C�ٚC��C�33C��C��fC��fC��3C��3C��3C��3C��3C�  C��C��C�&fC��C��3C��3C��C��C��C��3C�  C��C��C��C��3C�  C��3C�33C�@ C�  C��C��D 3D �fD� DL�D
�fDs3D  D� D3D��D�fDS3D� D!FfD#��D&33D(�3D+,�D-�3D0Y�D2��D5y�D8&fD:�3D=y�D@  DB�fDEl�DH  DJ� DM` DP�DR�fDUS3DW��DZ�fD]�D_� Db,�Dd�fDg�Di��DlfDny�Dp�3Ds33Du� Dw�fDzFfD|S3D~� D��fD��3D��D�C3D�y�D�� D�ٚD�	�D�33D�` D��fD�ɚD���D�6fD�p D���D��3D��D�VfD���D���D��3D�)�D�VfD��3D��3D���D�	�D�9�D�i�D���D��3D��3D�  D��D�0 D�I�D�ffD�� D���D���D��fD��3D��fD�fD�3D�)�D�C3D�\�D�s3D���D���D��fD��3D���D��D�33D�P D�ffDă3DŦfD�ɚD���D�	�D��D�9�D�\�D̀ DΠ Dϼ�D���D���D��D�0 D�I�D�c3D�y�Dؐ D٬�D���D��D�	�D�,�D�P D�p D� D�3D��fD��fD�3D�9�D�VfD�vfDꙚD�fD��fD��fD�3D�33D�I�D�ffD�3D���D��fD�� D�� D��D�)�D�&fD�<�D�L�D�i�D�� E L�E � Ea�E�Eq�E��E~fE�E��E EfE�fE��E
<�EC3E��E��E+3E  E�3E~fE��EL�E9�E�fE�3EQ�E>fE��E�fE>fE ��E!��E#�E#��E%C3E&�3E'�3E)3E*T�E+�fE,�fE.3E/VfE0��E1�3E3<�E4�3E5�3E7�E8` E9>fE:�fE;�E>�3EA�fEEC3EH��EKY�EN��EQ�fEU3EW��EZ�E^Y�EaX EdI�Eg{3Ej�3Em��Ep�3Et)�EwfEz4�E}�fE�73E�� E���E�H E��3E��3E�33E�{3E��fE��E�\ E��3E���E�H�E���E��fE�RfE��fE���E�/3E��3E��3E�, E�k3E�ŚE�3E�\�E��fE�� E�P�E���E�� E�O3E���E��3E�A�E�3E��fE��E�m�E��f?333?333?��?��?   ?��?��?333?��?333?333?L��?L��?�  ?�  ?�  ?���?���?�33?�33?���?�ff@   @��@��@,��@9��@L��@fff@s33@�33@�  @���@�33@���@���@�33@�  @���@陚@���A33A  A  A  A   A&ffA0  A4��A;33AD��AK33AQ��AX  A`  Ah  Al��At��A|��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414141414144111411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?�ff@   @Y��@�  @�33@�ff@�33A33A��A0  AH  Ah  A�  A�  A���A�  A�  A���A�  A�  BffB	��B  BffB!��B*  B2ffB:ffBB  BJ  BR  BZffBb  Bi��Br  Bz  B�  B���B���B���B�  B�33B�ffB�ffB�33B�  B�33B�ffB�33B�  B�33B�ffB�33B�  B���B�ffB�ffB�ffB�33B�33B�33B�33B���B�ffB�33B�33B�  B���C ��C��C� C�3C�3C
� C��C��CffC��C� CffC�3C��CffC��C � C"L�C$ffC&��C(��C*��C,� C.L�C0� C2��C4� C6ffC8� C:�3C<��C>��C@��CBffCD� CF��CH� CJL�CLffCNffCP�3CR��CT� CVL�CXffCZ� C\��C^�3C`��CbffCd� Cf��Ch��Cj�3Cl� Cn� CpffCr�3Ct��Cv�3Cx��Cz��C|� C~ffC�Y�C�Y�C�33C�Y�C�L�C�@ C�@ C�33C�Y�C�33C�&fC�L�C�@ C�&fC�@ C�@ C�33C�Y�C�L�C�33C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�@ C�Y�C�@ C�33C�L�C�ffC�L�C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�@ C�33C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�ffC�Y�C�&fC�33C�33C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�@ C�33C�33C�33C�@ C�@ C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�ffC�ffC�L�C��C�L�C�s3C�Y�C�&fC�&fC�33C�33C�33C�33C�33C�@ C�L�C�Y�C�ffC�L�C�33C�33C�L�C�Y�C�L�C�33C�@ C�L�C�Y�C�L�C�33C�@ C�33C�s3C�� C�@ C�L�C�Y�D 33D �fD� Dl�DfD�3D  D� D33D��DfDs3D  D!ffD#ٚD&S3D(�3D+L�D-�3D0y�D3�D5��D8FfD:�3D=��D@@ DB�fDE��DH@ DJ� DM� DP,�DR�fDUs3DX�DZ�fD],�D_� DbL�Dd�fDg9�Di��Dl&fDn��Dp�3DsS3Du� DxfDzffD|s3D~� D��fD��3D��D�S3D���D�� D��D��D�C3D�p D��fD�ٚD��D�FfD�� D���D��3D�,�D�ffD���D���D�3D�9�D�ffD��3D��3D���D��D�I�D�y�D���D��3D��3D� D�,�D�@ D�Y�D�vfD�� D���D���D��fD��3D�fD�fD�#3D�9�D�S3D�l�D��3D���D���D��fD��3D��D�)�D�C3D�` D�vfDē3DŶfD�ٚD���D��D�,�D�I�D�l�D͐ Dΰ D���D���D��D�)�D�@ D�Y�D�s3D׉�Dؠ Dټ�D���D���D��D�<�D�` D�� D� D��3D��fD�fD�#3D�I�D�ffD�fD꩚D��fD��fD�fD�#3D�C3D�Y�D�vfD�3D���D��fD�� D�  D��D�9�D�6fD�L�D�\�D�y�D�� E T�E � Ei�E�Ey�E�E�fE�E��E E&fE�fE��E
D�EK3E��E��E33E( E�3E�fE��ET�EA�E�fE�3EY�EFfE��E�fEFfE ��E!��E#$�E$�E%K3E&�3E'�3E)3E*\�E+�fE,�fE.3E/^fE0��E1�3E3D�E4�3E5�3E7!�E8h E9FfE:�fE;�E>�3EA�fEEK3EH��EKa�EN��EQ�fEU3EW��EZ��E^a�Ea` EdQ�Eg�3Ej�3En�Ep�3Et1�EwfEz<�E}�fE�;3E�� E���E�L E��3E��3E�73E�3E��fE��E�` E��3E���E�L�E���E��fE�VfE��fE���E�33E��3E��3E�0 E�o3E�ɚE�#3E�`�E��fE�� E�T�E���E�� E�S3E���E��3E�E�E��3E��fE��E�q�E��fG�O�G�O�G�O�G�O�?�  G�O�?���G�O�?���G�O�?���G�O�?�ffG�O�G�O�?�  ?���?ٙ�G�O�?�33@ff@33@   @,��@9��@L��@Y��@l��@�33@���@�33@�  @���@�33@���@���@�33@�  @���@���A��A33A  A  A   A(  A.ffA8  A<��AC33AL��AS33AY��A`  Ah  Ap  At��A|��A�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414141414144111411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ v@ �@ �@ O@ "�@ *S@ 0x@ 7�@ ?}@ G�@ Q�@ _�@ m:@ z�@ �7@ �0@ ��@ �-@ �&@ ��@ �#@ �m@ ��@@b@�@-@:�@G�@UU@b�@qS@~K@�D@��@�A@��@��@ψ@�/@�@��@1@�@"�@/�@>@Lu@Yn@ff@t�@�@�@�@��@��@�W@��@��@�@�E@
�@6@&�@3�@A�@N�@Z�@j@x&@�@�#@��@�@��@�@�[@�`@�Y@�Q@@�@(G@7L@DD@P�@^�@m�@{�@��@�0@�z@�~@��@��@��@�@� @v@@g@+�@:@H]@UU@a�@o�@}�@�P@��@�A@��@��@�7@ލ@��@��@%@{@"�@1�@>�@K@X�@e�@uk@��@��@��@��@�R@�J@��@�@�@@��@
�@�@%�@2�@B8@M�@Z�@j@ww@��@��@�m@�f@�@�@�[@�@�@  @@�@(G@7L@FQ@SI@_�@n�@z�@��@��@��@�-@��@��@��@��@�@	@	o@	�@	+�@	:�@	I@	Wb@	c�@	oF@	}�@	��@	�H@	��@	��@	��@	��@	�/@	��@	�,@
�@
{@
"�@
0x@
>�@
Lu@
Yn@
e	@
r�@
�W@
��@
��@
�M@
��@
Ĝ@
Ӡ@
��@
�@@
��@
=@�@&;@33@@�@O0@\�@j@x&@��@�u@�@�!@��@�@��@�`@�@^@J@�@(G@5�@C�@Q=@^�@m:@{�@��@�<@��@��@�w@�|@��@��@�@j@�@ @-@9X@G�@T�@e�@t@~K@��@��@�M@�@DD@��@��@�@\)@�m@�@*S@j@��@�Y@3�@v�@�^@��@B8@�+@ψ@�@[z@��@�@5�@~K@ƨ@@X�@�m@�@1'@x&@�2@1@O0@�$@�t@ @c�@��@��@-@o�@�!@��@/�@qS@�-@�(@,`@n�@�-@�@7L@y�@��@�9@<@{�@��@��@?}@�@��@�@I�@��@ψ@ o@ S�@ ��@ ׹@!�@!Z@!�H@!�#@"�@"Z�@"��@"܀@#[@#\�@#�H@#�
@$�@$N�@$��@$�W@%j@%@,@%z3@%�F@%�@&-�@&g�@&�@&܀@'�@'T�@'�@'�@(	�@(E�@(�d@(�w@(�9@)7L@)t@)�r@)�4@**S@*hs@*��@*�T@+
@+Z�@+��@+�
@,{@,Q=@,��@,�@-�@-DD@-�W@-�j@-��@.33@.o�@.�f@.�(@/'�@/e�@/��@/�H@0�@0\�@0��@0�h@1*@1S�@1��@1�*@2J@2I@2�|@2��@3 �@3>@3y�@3�F@3�@4/@4k.@4�A@4�@5!s@5^5@5�#@5ψ@6	�@6FQ@6��@6��@6�,@73�@7m�@7��@7��@8�@8S�@8��@8��@99X@9��@:R�@:�9@;k.@<
�@<v@=�@=~K@>B@>�@?�@?��@@�@@�f@AB8@A׹@B<�@B��@CbN@C�@D~�@EJ@E�<@E�q@F�@G@G��@H#�@H��@I5�@I�w@JE�@Jψ@K\�@K�y@Ly�@M1@M�#@Ng@N��@O	�@O��@P)�@Qx�@R��@T*S@U�@V�>@X3�@Yv�@Z�@\[@]j@^ލ@`%�@ag@b��@d�@e�7@f��@h-@ii!@j�2@l2�@mi!@m��@nV@nQ�@n��@n��@o�@oX@o�<@o�h@p�@p[z@p�a@p��@q&�@qhr@qě@q�8@rK�@r�@rψ@s%@sX�@s��@s��@t(G@t\�@t��@t�H@u-@uul@u��@v%@v7L@v�p@v��@w	�@wWa@w�7@w��@x �G�O�G�O�G�O�G�O�@ jG�O�@ G�O�@ G�O�@ �G�O�@ vG�O�G�O�@ �@ �@ 1G�O�@ 	�@ 
�@ J@ �@ @ b@ o@ �@ �@ �@ �@ �@ �@  �@ "�@ $�@ (G@ )�@ ,`@ /@ 1�@ 5?@ 7�@ :@ =q@ @�@ DD@ F�@ K@ M$@ O�@ S�@ V�@ Yn@ \)@ _�@ b�@ e	@ hs@ k�@ n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aհ!Aղ-Aհ!Aղ-Aղ-Aմ9AնFAծAղ-Aպ^A�AվwAոRAե�AՉ7AՅAՋDAՑhAՑhAՅA�r�A�p�A�l�A�ffA�^5A�/A�AӴ9A�v�A�VA���A�ĜA�K�A�v�A�G�A�"�A��A��9A��mA��\A���A��DA�
=A��mA��9A�x�A�bA��yA�C�A��wA�=qA�bA�?}A�oA��\A�=qA���A���A�33A�v�A��!A���A�C�A�S�A���A�+A�p�A��#A��9A�x�A�bA�A�A�(�A�1'A�M�A��^A�n�A�l�A�5?A�A�hsA��FA��A���A�$�A��uA�9XA���A���A�jA��A�ZA��TA��-A��DA�-A���A�JA�jA���A���A��yA�S�A��`A���A�A�O�A���A��/A�{A�A{Ay/Ax��Aw�7Av�DAt�HAn�jAm�AlbNAk�mAkx�Ak
=AjM�Ag��Ad^5A`�A_��A_�A^ĜA^M�A^1A[�-AY�;AX��AX��AXE�AX  AW�^AV1'AS&�AQl�API�AL�HAK+AJ(�AH�AF��AE\)AC�#AB=qAA�PA@�9A>(�A<-A:jA9\)A81'A7��A6�A5oA3XA2��A2�A2�+A1?}A/��A.��A.�A-7LA,ȴA,~�A+��A*��A*JA)A)?}A(��A(ZA'��A&��A&9XA%��A%\)A%oA$�yA$r�A#�^A!��A -A��AVAbA�A?}A�!Av�A��A�A�wAt�A�A�AQ�A�FAO�AbNA��AZAVA	��A��A�PA�A��At�AC�AoAjAO�A`B@�@�A�@���@�-@�@�hs@��@�A�@��m@�dZ@�+@�n�@���@�l�@���@@�h@�j@�+@�^5@��@�9@畁@�E�@�X@�Ĝ@��@���@�1'@�;d@�?}@���@�@�~�@ف@���@؃@� �@�o@�$�@ԋD@�I�@�v�@�z�@�n�@��-@��D@�;d@���@�J@��y@���@�^5@���@�^5@��y@���@�5?@�`B@� �@�C�@��+@���@���@��w@�$�@�$�@�$�@��-@��j@�1@���@���@�Q�@��H@�^5@�V@�Z@�1@�S�@��\@���@�G�@��F@�
=@�^5@��h@�Z@�+@���@��#@�/@��u@��@�Q�@}�@{��@zn�@w��@u�@s��@r��@p �@m`B@j�@g+@ep�@c��@bJ@`Ĝ@`1'@_l�@^��@\�/@[C�@Zn�@Y��@X  @V��@Up�@TZ@S�m@Q�#@O��@M@K@I�7@H1'@F�+@D1@BJ@A�@@�u@?�w@=�@=p�@<��@<�@:�H@:-@9��@9G�@7K�@6v�@5p�@5O�@3ƨ@2�!@2M�@1X@1%@/+@.��@-/@+��@*�\@(��@'l�@&v�@&ff@%@%`B@$�D@#�
@#ƨ@"��@!X@ bN@�@�@{@�@ƨ@"�@%@A�@@��@t�@o@�!@��@�`@ �@l�@�@{@�@�/@��@9X@
�@
��@
�!@	��@	G�@  @
=@V@�-@O�@�@�D@I�@ƨ@C�@�\@n�@��@ �`@ Q�?�\)?�V?�/?��?���?���?�`B?�Z?�!?�w?��?�1?�^5?��?�ȴ?�?�S�?�J?��?� �?��?�p�?�(�?�C�?�x�?�l�?�ff?��T?�Z?�t�?�n�?��?�A�?�|�?�{?�p�?��?�I�?�ƨ?�C�?��H?�7L?�r�?�r�?�+?Ƨ�?���?�t�?���?�M�?�G�?�Ĝ?���?�5??��?�1?�1?�j?�ƨ?�^5?���?�^5?���?�^5?���?��H?�?�"�?�dZ?��?��?���?�ƨ?�ƨ?�1?�1?�I�?�j?��D?��?��?���?���?��?�V?�V?�O�?�p�?��h?��-?���?���?���?��?�{?�5??�V?�v�?��R?��R?���?��?�;d?�\)?�|�?���?��w?�  Aղ-Aղ-AծAհ!AծAծAհ!Aհ!AծAմ9Aղ-Aհ!Aհ!AլAծAհ!Aղ-Aհ!Aմ9Aղ-Aղ-Aղ-Aղ-Aղ-Aղ-Aղ-Aհ!AծAհ!Aմ9Aղ-Aղ-Aղ-Aղ-Aղ-Aմ9Aմ9Aմ9AնFAմ9Aհ!AլAծAհ!Aմ9AոRAպ^AռjA�A�ĜA���A���AվwAվwAվwAվwAռjAոRAպ^AոRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Aհ!Aղ-Aհ!Aղ-Aղ-Aմ9AնFAծAղ-Aպ^A�AվwAոRAե�AՉ7AՅAՋDAՑhAՑhAՅA�r�A�p�A�l�A�ffA�^5A�/A�AӴ9A�v�A�VA���A�ĜA�K�A�v�A�G�A�"�A��A��9A��mA��\A���A��DA�
=A��mA��9A�x�A�bA��yA�C�A��wA�=qA�bA�?}A�oA��\A�=qA���A���A�33A�v�A��!A���A�C�A�S�A���A�+A�p�A��#A��9A�x�A�bA�A�A�(�A�1'A�M�A��^A�n�A�l�A�5?A�A�hsA��FA��A���A�$�A��uA�9XA���A���A�jA��A�ZA��TA��-A��DA�-A���A�JA�jA���A���A��yA�S�A��`A���A�A�O�A���A��/A�{A�A{Ay/Ax��Aw�7Av�DAt�HAn�jAm�AlbNAk�mAkx�Ak
=AjM�Ag��Ad^5A`�A_��A_�A^ĜA^M�A^1A[�-AY�;AX��AX��AXE�AX  AW�^AV1'AS&�AQl�API�AL�HAK+AJ(�AH�AF��AE\)AC�#AB=qAA�PA@�9A>(�A<-A:jA9\)A81'A7��A6�A5oA3XA2��A2�A2�+A1?}A/��A.��A.�A-7LA,ȴA,~�A+��A*��A*JA)A)?}A(��A(ZA'��A&��A&9XA%��A%\)A%oA$�yA$r�A#�^A!��A -A��AVAbA�A?}A�!Av�A��A�A�wAt�A�A�AQ�A�FAO�AbNA��AZAVA	��A��A�PA�A��At�AC�AoAjAO�A`B@�@�A�@���@�-@�@�hs@��@�A�@��m@�dZ@�+@�n�@���@�l�@���@@�h@�j@�+@�^5@��@�9@畁@�E�@�X@�Ĝ@��@���@�1'@�;d@�?}@���@�@�~�@ف@���@؃@� �@�o@�$�@ԋD@�I�@�v�@�z�@�n�@��-@��D@�;d@���@�J@��y@���@�^5@���@�^5@��y@���@�5?@�`B@� �@�C�@��+@���@���@��w@�$�@�$�@�$�@��-@��j@�1@���@���@�Q�@��H@�^5@�V@�Z@�1@�S�@��\@���@�G�@��F@�
=@�^5@��h@�Z@�+@���@��#@�/@��u@��@�Q�@}�@{��@zn�@w��@u�@s��@r��@p �@m`B@j�@g+@ep�@c��@bJ@`Ĝ@`1'@_l�@^��@\�/@[C�@Zn�@Y��@X  @V��@Up�@TZ@S�m@Q�#@O��@M@K@I�7@H1'@F�+@D1@BJ@A�@@�u@?�w@=�@=p�@<��@<�@:�H@:-@9��@9G�@7K�@6v�@5p�@5O�@3ƨ@2�!@2M�@1X@1%@/+@.��@-/@+��@*�\@(��@'l�@&v�@&ff@%@%`B@$�D@#�
@#ƨ@"��@!X@ bN@�@�@{@�@ƨ@"�@%@A�@@��@t�@o@�!@��@�`@ �@l�@�@{@�@�/@��@9X@
�@
��@
�!@	��@	G�@  @
=@V@�-@O�@�@�D@I�@ƨ@C�@�\@n�@��@ �`@ Q�?�\)?�V?�/?��?���?���?�`B?�Z?�!?�w?��?�1?�^5?��?�ȴ?�?�S�?�J?��?� �?��?�p�?�(�?�C�?�x�?�l�?�ff?��T?�Z?�t�?�n�?��?�A�?�|�?�{?�p�?��?�I�?�ƨ?�C�?��H?�7L?�r�?�r�?�+?Ƨ�?���?�t�?���?�M�?�G�?�Ĝ?���?�5??��?�1?�1?�j?�ƨ?�^5?���?�^5?���?�^5?���?��H?�?�"�?�dZ?��?��?���?�ƨ?�ƨ?�1?�1?�I�?�j?��D?��?��?���?���?��?�V?�V?�O�?�p�?��h?��-?���?���?���?��?�{?�5??�V?�v�?��R?��R?���?��?�;d?�\)?�|�?���?��w?�  Aղ-Aղ-AծAհ!AծAծAհ!Aհ!AծAմ9Aղ-Aհ!Aհ!AլAծAհ!Aղ-Aհ!Aմ9Aղ-Aղ-Aղ-Aղ-Aղ-Aղ-Aղ-Aհ!AծAհ!Aմ9Aղ-Aղ-Aղ-Aղ-Aղ-Aմ9Aմ9Aմ9AնFAմ9Aհ!AլAծAհ!Aմ9AոRAպ^AռjA�A�ĜA���A���AվwAվwAվwAվwAռjAոRAպ^AոRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B1BVB�B&�BL�BbNBgmBjBn�Bt�By�Bz�B{�B� B�B��B�B�HB�TB��BJBJB{B=qBM�BT�Bn�Bx�Bm�B{�Bt�B|�B�1B�B�B�B�B�+B�B�1B�+B�%B�B�B�B�B~�B}�Bx�Bo�BffBiyBe`BbNBffBbNB]/B_;B]/BZBW
BVBffBhsB\)BVBM�B;dB'�B�BPBB��B�NB��B��B��B�LB��B�Bv�Bn�BjBiyBhsBiyBgmBbNBW
B=qB/BDB
�sB
��B
��B
�oB
�+B
~�B
iyB
_;B
VB
33B
$�B
�B
�B
hB	��B	�5B	��B	��B	ɺB	ǮB	ÖB	�^B	��B	�{B	�B	x�B	t�B	p�B	l�B	iyB	YB	O�B	J�B	G�B	F�B	C�B	B�B	33B	$�B	 �B	uB	B��B�B�yB�TB�B��BŢBŢBÖB�B��B��B��B��B��B�uB�VB�JB�DB�=B�1B�B|�Bz�Bx�Bw�Bu�Bt�Bo�Bk�BiyBiyBl�Bw�Bw�Bs�Bv�Bt�Bu�Bt�Bt�Bt�Bs�Bn�BjBhsBhsBe`BcTBaHB`BBe`BgmBcTBaHB[#BT�BR�BQ�BM�BL�BJ�BF�BC�BA�B:^B:^B8RB2-B7LB5?B49B49B33B1'B1'B/B/B1'B0!B.B.B-B-B-B-B,B,B+B,B-B-B0!B2-B2-B49B49B49B5?B5?B7LB9XB:^B5?B6FB8RB8RB9XB9XB;dB<jB<jB<jB<jB=qB>wB?}BA�BYB`BBr�B�B�bB��B�BĜB�5B�5B�NB��B	B	+B	B	B	
=B	B	\B	�B	'�B	E�B	K�B	[#B	q�B	�B	�bB	��B	��B	��B	�B	�'B	�jB	ƨB	��B	�
B	�#B	�#B	�;B	�TB	�`B	�fB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
%B
+B
	7B
DB
JB
JB
bB
oB
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$�B
%�B
%�B
%�B
(�B
+B
,B
-B
-B
/B
2-B
49B
7LB
8RB
:^B
=qB
=qB
@�B
?}B
@�B
A�B
C�B
B�B
D�B
D�B
F�B
F�B
G�B
G�B
J�B
I�B
J�B
K�B
M�B
N�B
P�B
Q�B
R�B
S�B
T�B
VB
W
B
W
B
ZB
[#B
\)B
[#B
]/B
]/B
]/B
_;B
_;B
`BB
_;B
_;B
`BB
`BB
bNB
bNB
dZB
dZB
ffB
ffB
hsB
jB
k�B
k�B
k�B
l�B
m�B
n�B
n�B
n�B
o�B
q�B
q�B
q�B
r�B
t�B
s�B
s�B
t�B
u�B
v�B
w�B
x�B
x�B
x�B
y�B
z�B
y�B
{�B
{�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�JB
�DB
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
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�!B
�'B
�3B
�9B
�?B
�FB
�?B
�FB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�^B
�^B
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
�dB
�dB
�dB
�jB
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
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BB%B
=BJBPBPBVB\BhB{B{B{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BBDBoB#�BI�B_;BdZBgmBk�Bq�Bv�Bw�Bx�B|�B�B��B��B�5B�BB�B	7B	7BhB:^BJ�BQ�Bk�Bu�BjBx�Bq�By�B�B�B�B� B� B�B� B�B�B�B�B�B�B�B|�B{�Bv�Bm�BdZBgmBcTB`BBdZB`BB[#B]/B[#BXBT�BS�BdZBffBZBS�BK�B9XB%�B�BDBB�B�BB��BɺB�}B�?B��B� Bt�Bl�BhsBgmBffBgmBe`B`BBT�B;dB-B	7B
�fB
�wB
��B
�bB
�B
|�B
gmB
]/B
S�B
1'B
"�B
�B
�B
\B	��B	�)B	��B	��B	ǮB	ŢB	��B	�RB	��B	�oB	~�B	v�B	r�B	n�B	jB	gmB	W
B	M�B	H�B	E�B	D�B	A�B	@�B	1'B	"�B	�B	hB	B��B�B�mB�HB�
B��BÖBÖB��B�B��B��B��B��B��B�hB�JB�=B�7B�1B�%B~�Bz�Bx�Bv�Bu�Bs�Br�Bm�BiyBgmBgmBjBu�Bu�Bq�Bt�Br�Bs�Br�Br�Br�Bq�Bl�BhsBffBffBcTBaHB_;B^5BcTBe`BaHB_;BYBR�BP�BO�BK�BJ�BH�BD�BA�B?}B8RB8RB6FB0!B5?B33B2-B2-B1'B/B/B-B-B/B.B,B,B+B+B+B+B)�B)�B(�B)�B+B+B.B0!B0!B2-B2-B2-B33B33B5?B7LB8RB33B49B6FB6FB7LB7LB9XB:^B:^B:^B:^B;dB<jB=qB?}BW
B^5Bp�B� B�VB��B��BB�)B�)B�BB��B	B	B	B	B	1B	B	PB	�B	%�B	C�B	I�B	YB	o�B	~�B	�VB	�{B	��B	��B	�B	�B	�^B	ĜB	��B	��B	�B	�B	�/B	�HB	�TB	�ZB	�mB	�sB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
%B
1B

=B
DB
DB
\B
hB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
$�B
$�B
$�B
'�B
)�B
+B
,B
,B
.B
1'B
33B
6FB
7LB
9XB
<jB
<jB
?}B
>wB
?}B
@�B
B�B
A�B
C�B
C�B
E�B
E�B
F�B
F�B
I�B
H�B
I�B
J�B
L�B
M�B
O�B
P�B
Q�B
R�B
S�B
T�B
VB
VB
YB
ZB
[#B
ZB
\)B
\)B
\)B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
aHB
aHB
cTB
cTB
e`B
e`B
gmB
iyB
jB
jB
jB
k�B
l�B
m�B
n�B
n�B
o�B
q�B
q�B
q�B
r�B
t�B
s�B
s�B
t�B
u�B
v�B
w�B
x�B
x�B
x�B
y�B
z�B
y�B
{�B
{�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�JB
�DB
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
��B
��B
��B
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�9B
�?B
�FB
�LB
�FB
�RB
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�jB
�jB
�qB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�wB
�}B
�wB
�wB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB+B	7B
=B
=BDBJBVBhBhBhG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811130102322021061413525420210614135254202106141746532021061417465320210614174653201811130102322021061413525420210614135254202106141746532021061417465320210614174653PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018111301023220181113010232  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111301023220181113010232QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111301023220181113010232QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015520210722160155IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                