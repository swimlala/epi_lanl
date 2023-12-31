CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:28Z creation      
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
resolution        =���   axis      Z        (  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  dH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  tp   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                       HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                      HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
8      � 
8Argo profile    3.1 1.2 19500101000000  20180724220228  20210722160148  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�c:��2�@�c:��2�11  @�c:}'�@�c:}'�@6�HU�'(@6�HU�'(�c�d���,�c�d���,11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  ?�33@9��@�  @�  @�33@�33@���A  A$��A>ffA`  A���A���A���A�  A���Aљ�A�  A���B ��BffBffBffB   B(  B0ffB8ffB@��BH  BO��BX  B`ffBh  Bp  Bx  B�ffB�33B�  B�33B�33B�33B���B�  B�  B�  B�33B�33B�  B�ffB�33B�  B�33B�  B���B�ffB�33Bә�B���B�  B���B�ffB���B�  B�33B�ffB���B�  B���C  C33C�C  C
33C33C  C33C  C��C�C  C��C�CL�C L�C"�C$33C&�C'�fC*�C,33C.�C/��C2  C433C6  C7��C9�fC<�C>L�C@�CA��CD  CF33CH33CJ�CK��CN  CP�CRL�CT�CU��CX  CZ�C\33C^L�C`�Ca�fCd  Cf33Ch  Ci��Cl  CnL�Cp33Cr�Cs�fCu��Cx  CzL�C|�C}�fC��C�  C��fC��C�  C��fC�  C��C�&fC�&fC��C�&fC��C�  C��C��C�33C��C��fC��3C��3C�  C�  C�  C�  C��C��C��C��C�&fC��C�  C�33C�&fC�&fC�&fC��C��C��C��C�  C�  C�  C�  C��3C��3C��3C��fC��C��C��C�  C��3C�  C��C��C�  C��fC�  C��C�  C��3C��C�33C�&fC��C��C�  C��3C��3C��fC��fC��fC��fC��3C��3C��3C��3C��fC��fC��3C��3C�  C�  C��C��C�&fC�33C��C�ٚC��fC��fC��3C��C��C��C�  C��fC��3C��C��C�&fC��C��fC��3C��C��C�  C��fC��3C��3C��C��C��C�&fC�&fC�&fC�&fC��C�ٚC��fC��fC��fD�fD&fDs3D�fD
� D3D@ D� D��D��D9�D�3D�fDS3D!�3D$3D&y�D(��D+S3D-��D/�3D2Y�D4�3D7�D9l�D;�fD>,�D@�3DB��DE` DG�3DJFfDL��DO  DQ9�DSs3DU� DW��DZ33D\` D^�fD`ٚDcfDeFfDg��Di�3Dk�fDn9�Dpy�Dr��Dt�fDw,�Dyy�D{` D}��D��D��D�L�D�y�D��3D���D��3D�fD�9�D�Y�D�|�D��fD�ɚD��D�  D��D�6fD�\�D��3D���D�ɚD��3D��D�C3D�` D��fD��fD�ɚD�� D� D�33D�Y�D��3D���D���D��3D��D�@ D�c3D�� D�� D���D��fD��fD�fD��D�,�D�6fD�C3D�I�D�Y�D�i�D�vfD��3D�� D���D�� D��fD���D�� D¶fDü�D��3Dż�D�� D�� D��fD��3D���D�� D��3D���D��fDϹ�Dа DѬ�Dҩ�DӠ Dԙ�DՌ�Dր D�y�D�s3D�ffD�c3D�Y�D�\�D�\�D�VfD�S3D�S3D�L�D�C3D�@ D�<�D�<�D�<�D�0 D�0 D�)�D�#3D��D�fD�fD�fD�3D���D��fD�� D�� D���D��3D��fD��3D���D�� D��3D���D���D�� D��fD�� E@ E�3E� E EfEs3E�E	�fE[3EQ�E��E� E@ E��E� EfEfEk3EɚEfE�3E>fEy�E�E;3E VfE!~fE"��E$fE%q�E&t�E'��E)  E*� E+��E,�E.�E/vfE0� E2�E3<�E4h E5��E6�3E7� E9��E:��E;ɚE>�fEB>fEE)�EH^fEKq�EN��EQ�3ET��EW�EZ� E^8 Ea4�Ed^fEg��Ej� Em�3Ent�Eo  Eo�fEpc3Ep� Eq~fErI�Er� EsX Et$�Et��Eu4�Eu��Evy�Ew<�Ew��ExC3Ey�Ey��EzD�>���>���>���>���>���>���>���>���>L��>���?   >���?   ?   >���?��?��?L��?�  ?�  ?���?�ff?�  ?���?ٙ�@ff@33@&ff@,��@@  @S33@fff@s33@�33@���@�ff@�  @���@�ff@�33@�  @���@�ff@�33A   A	��A  AffAffA#33A+33A1��A;33AC33AI��AQ��AY��A^ffAfffAnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441441141441411411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ?fff?�  @��@Y��@�  @�  @�33@�33A��A  A,��AFffAh  A���A���A���A�  A���Aՙ�A�  A���B��B
ffBffBffB"  B*  B2ffB:ffBB��BJ  BQ��BZ  BbffBj  Br  Bz  B�ffB�33B�  B�33B�33B�33B���B�  B�  B�  B�33B�33B�  B�ffB�33B�  B�33B�  B���B�ffB�33Bԙ�B���B�  BᙚB�ffB���B�  B�33B�ffB���B�  C L�C� C�3C��C� C
�3C�3C� C�3C� CL�C��C� CL�C��C��C ��C"��C$�3C&��C(ffC*��C,�3C.��C0L�C2� C4�3C6� C8L�C:ffC<��C>��C@��CBL�CD� CF�3CH�3CJ��CLL�CN� CP��CR��CT��CVL�CX� CZ��C\�3C^��C`��CbffCd� Cf�3Ch� CjL�Cl� Cn��Cp�3Cr��CtffCvL�Cx� Cz��C|��C~ffC�Y�C�@ C�&fC�L�C�@ C�&fC�@ C�Y�C�ffC�ffC�L�C�ffC�Y�C�@ C�Y�C�Y�C�s3C�L�C�&fC�33C�33C�@ C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�ffC�L�C�@ C�s3C�ffC�ffC�ffC�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�33C�33C�33C�&fC�L�C�Y�C�L�C�@ C�33C�@ C�Y�C�L�C�@ C�&fC�@ C�L�C�@ C�33C�L�C�s3C�ffC�Y�C�L�C�@ C�33C�33C�&fC�&fC�&fC�&fC�33C�33C�33C�33C�&fC�&fC�33C�33C�@ C�@ C�L�C�Y�C�ffC�s3C�L�C��C�&fC�&fC�33C�L�C�Y�C�Y�C�@ C�&fC�33C�L�C�L�C�ffC�L�C�&fC�33C�L�C�Y�C�@ C�&fC�33C�33C�L�C�L�C�Y�C�ffC�ffC�ffC�ffC�L�C��C�&fC�&fC�&fDfDFfD�3D�fD  D33D` D� D��D�DY�D�3DfDs3D!�3D$33D&��D)�D+s3D-��D03D2y�D4�3D7,�D9��D;�fD>L�D@�3DC�DE� DG�3DJffDL��DO  DQY�DS�3DU� DX�DZS3D\� D^�fD`��Dc&fDeffDg��Di�3DlfDnY�Dp��Dr��DufDwL�Dy��D{� D}��D��D�,�D�\�D���D��3D���D�3D�&fD�I�D�i�D���D��fD�ٚD���D� D�)�D�FfD�l�D��3D���D�ٚD�3D�,�D�S3D�p D��fD��fD�ٚD�  D�  D�C3D�i�D��3D���D���D�3D�)�D�P D�s3D�� D�� D�ɚD��fD�fD�fD�)�D�<�D�FfD�S3D�Y�D�i�D�y�D��fD��3D�� D���D�� D��fD���D�� D��fD���D��3D���D�� D�� D��fD��3D���D�� D��3D���D��fD�ɚD�� DѼ�Dҹ�DӰ Dԩ�D՜�D֐ D׉�D؃3D�vfD�s3D�i�D�l�D�l�D�ffD�c3D�c3D�\�D�S3D�P D�L�D�L�D�L�D�@ D�@ D�9�D�33D�)�D�&fD�fD�fD�3D�	�D�fD�  D�  D���D��3D��fD��3D���D�� D��3D���D���D�� D��fD�� EH E�3E� E  EfE{3E�E	�fEc3EY�E��E� EH E��E� EfEfEs3EњE&fE3EFfE��E�EC3E ^fE!�fE#�E$fE%y�E&|�E'��E)( E*� E+��E,��E.�E/~fE0� E2�E3D�E4p E5��E6�3E8  E9��E:��E;њE>�fEBFfEE1�EHffEKy�EN��EQ�3ET��EW��EZ� E^@ Ea<�EdffEg��Ej� Em�3En|�Eo Eo�fEpk3Ep� Eq�fErQ�Er� Es` Et,�Et��Eu<�Ev�Ev��EwD�EwɚExK3Ey�Ey��EzL�G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?333?fffG�O�?fffG�O�G�O�?fffG�O�?���?�ffG�O�?�  ?���?�ff@   @ff@��@&ff@333@Fff@L��@`  @s33@�33@���@�33@���@�ff@�  @���@�ff@�33@�  @���@�ffA��A  A��A  AffA&ffA+33A333A9��AC33AK33AQ��AY��Aa��AfffAnffAvffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441441141441411411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @ �@ �@ �@ �@ O@ ""@ )�@ 0x@ 5?@ =q@ FQ@ Q=@ _�@ m�@ {�@ ��@ �0@ ��@ ��@ �&@ �|@ ��@ ��@ �q@@@�@-@:�@I@UU@bN@p�@~�@��@��@�A@�F@�>@�7@ލ@�4@��@%@{@""@/�@>@K�@X�@g�@t�@��@�@�@��@��@ƨ@�C@��@��@��@�@6@%�@3�@B8@P�@\)@hs@ww@�|@�u@�m@�r@�@�c@�h@�@��@ �@�@�@)�@8�@FQ@R�@`�@m�@z3@�7@��@��@�!@�&@�*@�t@�@�@@@g@+@:@I@V�@c�@oF@~K@��@��@��@��@@��@�;@�@��@%@{@#�@/�@<@K@Z�@g�@t�@�@��@�@��@�@�J@��@�H@�@�E@
=@�@%�@4�@B�@P�@\�@k�@x�@�@�$@��@��@�k@�@�[@�@�Y@  @�@O@)�@7L@E�@SI@a�@m�@z�@�D@�<@��@��@��@�|@�#@��@��@	j@	@	�@	+�@	9X@	F�@	S�@	c�@	r@	~�@	��@	��@	�A@	�F@	�>@	�7@	܀@	�@	��@
�@
�@
"�@
2�@
?}@
Lu@
Yn@
ff@
s_@
�@
��@
��@
�M@
��@
�J@
��@
��@
�@@
�9@�@6@$�@33@@�@O0@]�@k�@z3@��@��@�@��@�@�@�h@�@�Y@��@�@�@)�@8�@D�@P�@^�@m�@|?@��@��@�(@��@��@�|@��@�(@��@v@@g@*S@8�@FQ@S�@�#@�@Wb@�u@�7@J@G�@�@��@��@<�@|�@�j@��@?}@�W@��@�@FQ@�p@Ĝ@%@FQ@�|@�W@�@I@��@�@�@P�@�u@��@{@Q=@��@��@	�@FQ@��@��@��@7L@t�@��@�@)�@i!@��@�@g@]�@�U@�7@@M�@�D@�@J@K�@�D@�@1@FQ@��@��@]@?}@|�@�R@�e@1'@o�@��@�@ +@ j@ ��@ ��@!%�@!dZ@!��@!��@"�@"\)@"�H@"�@#�@#Wb@#��@#�O@$@$Q�@$�@$��@%
=@%FQ@%�@%��@%��@&5@@&o�@&��@&��@'�@'S�@'��@'�W@( �@(:@(r�@(��@(�@)�@)Q�@)��@)��@)��@*/@*ff@*�@*��@+
�@+C�@+z�@+�-@+�m@,�@,P�@,�@,�@,��@-%�@-Z�@-��@-@-��@.-@.`�@.��@.�o@/�@/9X@/n�@/��@/�#@0b@0D�@0z�@0��@0�m@1
@1Q�@1��@1��@1�@2'�@2]�@2��@2�W@2�E@31�@3g�@3�@3Ӡ@4	�@4>@4r@4��@4�/@5{@5H]@5z3@5��@5�@6B@6�@7%�@7��@8,`@8�c@9/@9��@:g@:��@;uk@;ލ@<�W@<�@=��@>+@>��@?/@?��@@-�@@�>@AT�@A��@B<�@B�>@Cr�@C�L@Di!@D�l@E�7@E��@F��@Gv@G�<@H(�@H�&@I1�@I�@J?}@J�/@KUU@K��@Ly�@L�,@M{�@M��@N~K@O'�@O��@Pg@Ql�@R��@Tg@U}�@V�|@X0x@Y�|@Z��@\"�@]i�@^�7@`�@ao�@b�@d%�@e�@e��@e�,@f5�@f��@f��@g	�@g`B@g��@gӠ@h+@he	@h�@h�@i)�@i|�@i��@i��@j?}@jv@j�G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ ^@ �G�O�@ �G�O�G�O�@ �G�O�@ @ vG�O�@ �@ �@ �@ 
=@ 
�@ �@ V@ �@ �@ o@ {@ �@ �@ �@ �@ 
@  @ ""@ $�@ &�@ )�@ ,`@ /@ 1'@ 3�@ 6�@ :�@ =q@ @,@ C�@ E�@ I@ K�@ O�@ SI@ V@ Yn@ \�@ ^�@ bN@ e�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aş�Aş�AżjA�7LA�Q�AǙ�Aǉ7A�|�A�v�A�n�A�5?A���A�-A�hsA�A�A��TAġ�AċDA�v�A�ffA�Q�A� �Aú^A�bA�hsA���A�M�A���A�ffA�C�A��#A�ȴA�;dA�x�A�{A�A�z�A�M�A�A�z�A���A���A�t�A��-A�S�A���A�%A��TA��-A�1A��yA��RA���A�=qA���A�ZA��7A��PA��\A�hsA�p�A��A�I�A���A���A�5?A���A�I�A�JA�|�A��A��A��A�;dA��FA�l�A�%A�A��-A��\A�&�A�E�A�33A�"�A��/A�\)A�z�A��TA��A�oA�=qA�JA�A��!A�ZA��7A�33A���A� �A�+A�x�A��A��;A�G�A���A�bA���A�A��A��A�;dA�E�A�I�A�hsA��hA���A���A��A���A��DA���A��RA�M�AA}t�Az�+At��Aq�^Am�Ak�AkVAj��Aj�Ai��Ai
=Ag�wAe�A`ffA^z�A]A[&�AX$�AW%AVAV�+AW�AW�AV�DAT5?AQ�AP��AN��AN�AL��AKXAJVAIK�AH�AF�AEx�AD��ACl�AB��AA��AA��A@�/A?�A>�`A>{A<~�A;��A7�A6VA5XA5A3�A2�9A0ZA/XA/
=A.$�A+��A*�+A*1'A(��A't�A%p�A$ �A#;dA!K�A�A�AĜA�`AQ�AoA��AC�Al�A�A�uA��AĜA&�Ar�A�^AVA�+A=qA�A9XAI�A�DAE�Ax�A/A��A�;AXA�AjA��AXA
�RA
{A	`BA�AȴAZA�TA��A;dA{A9XA�HAr�AJAoA M�@�dZ@��u@���@�Q�@�x�@�  @�P@�G�@�?}@�@�@�+@�Z@�hs@�  @�ff@�1'@�33@���@͡�@�M�@��^@��
@�x�@�hs@��^@�33@���@��;@�ff@��!@�p�@��@��@�b@�7L@��-@��F@��-@���@���@�`B@��m@�\)@�o@��R@�7L@���@��
@�n�@�(�@�K�@���@���@��m@�+@��y@���@���@���@�K�@��7@���@���@��!@��-@�O�@��/@�@}/@|(�@y�@xbN@w
=@u�@s�m@r��@q%@ol�@m��@lz�@j�@i7L@h��@f��@d��@c@a&�@`bN@_l�@^5?@\�@[ƨ@Zn�@YX@W;d@V�@Vff@U�@Tz�@R~�@P�`@O��@M�@L�@K�F@J�H@I�^@HĜ@H �@GK�@Fff@E��@D�D@Ct�@B^5@Ax�@@�u@?|�@>��@>$�@=�@<��@;"�@:�@8�`@7�@6��@5?}@4�@3dZ@3"�@2��@1�^@0Q�@/\)@.{@-/@,9X@+33@*^5@)��@(�9@(1'@'�@&��@&ff@%�T@$�D@#ƨ@#dZ@"�\@!G�@ r�@��@�T@�h@p�@�D@S�@�@�u@A�@;d@E�@�T@O�@I�@�
@t�@��@J@x�@��@  @ȴ@�+@�T@@��@I�@@	��@	��@	X@	�@�9@�@\)@��@�T@�@�@dZ@�@ A�?�{?��?�C�?�7L?�?���?�t�?�n�?��`?��?�?�dZ?�^5?���?�r�?�ff?��/?��?�\?�  ?�5??��m?�~�?��?��?�?�?}?���?ӕ�?���?щ7?� �?Ͼw?��?��?�/?̋D?��H?���?�7L?�1'?�K�?�+?�E�?�?š�?��/?��?\?�Ĝ?�  ?�\)?�v�?�O�?�j?�1?�ƨ?���?�dZ?�C�?�?��H?��H?��H?��H?��H?��H?�?�"�?�"�?�"�?�C�?�C�?�C�?�C�?�dZ?�dZ?�dZ?�dZ?��?�dZ?��Aŧ�Ať�Aţ�Aş�Aś�Aś�Aş�Aŝ�Aş�Aş�Aş�Aŝ�Aş�Aŝ�Aś�Aŝ�Aŝ�Aŝ�Aŝ�Aš�Aś�Aš�Aŧ�AŬAŰ!A���A��
A���A��A�;dAƏ\A��HA�E�AǃAǙ�Aǝ�AǛ�AǓuAǏ\Aǉ7AǃA�~�A�z�A�v�A�t�A�p�A�p�A�hsA�M�A�33A��A���A��;A�Aơ�AƍPA�ffA�;dA���Aź^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Aş�Aş�AżjA�7LA�Q�AǙ�Aǉ7A�|�A�v�A�n�A�5?A���A�-A�hsA�A�A��TAġ�AċDA�v�A�ffA�Q�A� �Aú^A�bA�hsA���A�M�A���A�ffA�C�A��#A�ȴA�;dA�x�A�{A�A�z�A�M�A�A�z�A���A���A�t�A��-A�S�A���A�%A��TA��-A�1A��yA��RA���A�=qA���A�ZA��7A��PA��\A�hsA�p�A��A�I�A���A���A�5?A���A�I�A�JA�|�A��A��A��A�;dA��FA�l�A�%A�A��-A��\A�&�A�E�A�33A�"�A��/A�\)A�z�A��TA��A�oA�=qA�JA�A��!A�ZA��7A�33A���A� �A�+A�x�A��A��;A�G�A���A�bA���A�A��A��A�;dA�E�A�I�A�hsA��hA���A���A��A���A��DA���A��RA�M�AA}t�Az�+At��Aq�^Am�Ak�AkVAj��Aj�Ai��Ai
=Ag�wAe�A`ffA^z�A]A[&�AX$�AW%AVAV�+AW�AW�AV�DAT5?AQ�AP��AN��AN�AL��AKXAJVAIK�AH�AF�AEx�AD��ACl�AB��AA��AA��A@�/A?�A>�`A>{A<~�A;��A7�A6VA5XA5A3�A2�9A0ZA/XA/
=A.$�A+��A*�+A*1'A(��A't�A%p�A$ �A#;dA!K�A�A�AĜA�`AQ�AoA��AC�Al�A�A�uA��AĜA&�Ar�A�^AVA�+A=qA�A9XAI�A�DAE�Ax�A/A��A�;AXA�AjA��AXA
�RA
{A	`BA�AȴAZA�TA��A;dA{A9XA�HAr�AJAoA M�@�dZ@��u@���@�Q�@�x�@�  @�P@�G�@�?}@�@�@�+@�Z@�hs@�  @�ff@�1'@�33@���@͡�@�M�@��^@��
@�x�@�hs@��^@�33@���@��;@�ff@��!@�p�@��@��@�b@�7L@��-@��F@��-@���@���@�`B@��m@�\)@�o@��R@�7L@���@��
@�n�@�(�@�K�@���@���@��m@�+@��y@���@���@���@�K�@��7@���@���@��!@��-@�O�@��/@�@}/@|(�@y�@xbN@w
=@u�@s�m@r��@q%@ol�@m��@lz�@j�@i7L@h��@f��@d��@c@a&�@`bN@_l�@^5?@\�@[ƨ@Zn�@YX@W;d@V�@Vff@U�@Tz�@R~�@P�`@O��@M�@L�@K�F@J�H@I�^@HĜ@H �@GK�@Fff@E��@D�D@Ct�@B^5@Ax�@@�u@?|�@>��@>$�@=�@<��@;"�@:�@8�`@7�@6��@5?}@4�@3dZ@3"�@2��@1�^@0Q�@/\)@.{@-/@,9X@+33@*^5@)��@(�9@(1'@'�@&��@&ff@%�T@$�D@#ƨ@#dZ@"�\@!G�@ r�@��@�T@�h@p�@�D@S�@�@�u@A�@;d@E�@�T@O�@I�@�
@t�@��@J@x�@��@  @ȴ@�+@�T@@��@I�@@	��@	��@	X@	�@�9@�@\)@��@�T@�@�@dZ@�@ A�?�{?��?�C�?�7L?�?���?�t�?�n�?��`?��?�?�dZ?�^5?���?�r�?�ff?��/?��?�\?�  ?�5??��m?�~�?��?��?�?�?}?���?ӕ�?���?щ7?� �?Ͼw?��?��?�/?̋D?��H?���?�7L?�1'?�K�?�+?�E�?�?š�?��/?��?\?�Ĝ?�  ?�\)?�v�?�O�?�j?�1?�ƨ?���?�dZ?�C�?�?��H?��H?��H?��H?��H?��H?�?�"�?�"�?�"�?�C�?�C�?�C�?�C�?�dZ?�dZ?�dZ?�dZ?��?�dZ?��Aŧ�Ať�Aţ�Aş�Aś�Aś�Aş�Aŝ�Aş�Aş�Aş�Aŝ�Aş�Aŝ�Aś�Aŝ�Aŝ�Aŝ�Aŝ�Aš�Aś�Aš�Aŧ�AŬAŰ!A���A��
A���A��A�;dAƏ\A��HA�E�AǃAǙ�Aǝ�AǛ�AǓuAǏ\Aǉ7AǃA�~�A�z�A�v�A�t�A�p�A�p�A�hsA�M�A�33A��A���A��;A�Aơ�AƍPA�ffA�;dA���Aź^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�XB��B	%�B	��B
�JB
��B
��B
��B
�B
�B
��B
�B
�B
�'B
�!B
�!B
�LB
�RB
�jB
�qB
ǮB
�#B
��B�B;dBN�Bo�B�B��B�XB�dB��B�!B�B�B�B�3B��B��B�BBBDB33BC�BK�BT�B^5Bl�Bm�Bp�Bw�By�By�Bw�Bt�Bp�BZBE�B_;BbNBl�Bv�Bz�Bs�BS�BH�BD�BR�BYBL�B$�B�B$�B0!B,B49B\)B}�B|�B}�B\)B5?BO�B`BB9XB33B2-B6FB�BbB�mB��B�B+B5?B�B�wB�7BuB
��BS�By�B�qB�}B�B�hB}�Bu�Bo�Bk�Be`BR�B.B{BB
�B
�B
ɺB
�RB
��B
�B
]/B
O�B
D�B
5?B
\B	�B	�HB	B	�FB	�!B	�3B	�FB	�!B	�B	��B	�7B	gmB	XB	J�B	=qB	:^B	2-B	/B	@�B	YB	_;B	^5B	J�B	G�B	;dB	:^B	6FB	,B	#�B	�B	�B	DB	DB		7B	%B	  B��B��B��B��B�B�B�yB�;B�
BƨB��B�dB�XB�3B�B��B��B��B��B��B��B��B�JB�+B�B�B� B|�B{�B|�B}�B� B� B�B}�Bk�BhsBffBbNB`BBZBYBW
BT�BS�BXBYBk�BiyBm�Bv�Bt�Bv�Bu�Br�Bp�BjBhsBk�Bn�Bk�Bl�Br�Bp�Bo�Bo�Bp�Br�Bq�Bq�Bn�BiyBiyBiyBgmBgmBhsBbNB]/B[#BW
BJ�BK�BL�BL�BT�BS�BYB[#BT�BN�BL�BK�BH�B>wB@�BQ�BXBYBaHBhsBq�B�B�7B�oB��B��B�wB��B�;B�TB�yB��B	�B	(�B	C�B	O�B	Q�B	VB	iyB	p�B	u�B	}�B	�VB	�uB	��B	��B	�?B	�XB	�}B	ÖB	ǮB	��B	��B	��B	�B	�B	�5B	�ZB	�fB	�yB	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
+B

=B
DB
PB
\B
hB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
%�B
&�B
'�B
'�B
(�B
,B
.B
.B
0!B
1'B
2-B
2-B
49B
5?B
49B
5?B
6FB
7LB
8RB
:^B
;dB
;dB
<jB
>wB
>wB
>wB
@�B
@�B
B�B
D�B
D�B
E�B
F�B
I�B
H�B
J�B
I�B
J�B
L�B
L�B
M�B
O�B
P�B
P�B
R�B
R�B
R�B
S�B
T�B
VB
VB
VB
W
B
XB
YB
ZB
[#B
\)B
]/B
^5B
`BB
_;B
_;B
aHB
cTB
cTB
dZB
e`B
e`B
ffB
ffB
gmB
gmB
hsB
iyB
iyB
iyB
k�B
k�B
k�B
m�B
m�B
n�B
n�B
n�B
p�B
p�B
r�B
r�B
s�B
s�B
s�B
u�B
u�B
u�B
w�B
w�B
w�B
y�B
{�B
}�B
~�B
� B
� B
�B
�B
�B
�%B
�%B
�+B
�7B
�=B
�DB
�DB
�PB
�PB
�\B
�bB
�hB
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
�B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�9B
�?B
�?B
�?B
�FB
�FB
�FB
�LB
�FB
�LB
�FB
�FB
�FB
�LB
�FB
�LB
�LB
�LB
�LB
�FB
�LB
�LB
�LB
�RB
�LB�^B�^B�RB�XB�^B�^B�RB�RB�RB�RB�LB�^B�XB�XB�XB�XB�^BǮBǮB��B�B�yB��B	B	\B	8RB	N�B	�%B	��B	ɺB
hB
ZB
��B
��B
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
�B
�B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           B�RB��B	$�B	��B
�DB
��B
��B
��B
��B
��B
��B
�B
�B
�!B
�B
�B
�FB
�LB
�dB
�jB
ƨB
�B
��B�B:^BM�Bn�B�B��B�RB�^B��B�B�B�B�B�-B��B��B�;BB
=B2-BB�BJ�BS�B]/Bk�Bl�Bo�Bv�Bx�Bx�Bv�Bs�Bo�BYBD�B^5BaHBk�Bu�By�Br�BR�BG�BC�BQ�BXBK�B#�B�B#�B/B+B33B[#B|�B{�B|�B[#B49BN�B_;B8RB2-B1'B5?B�B\B�fB��B�B)�B49B�B�qB�1BoB
��BR�Bx�B�jB�wB�B�bB|�Bt�Bn�BjBdZBQ�B-BuB  B
�B
��B
ȴB
�LB
��B
�B
\)B
N�B
C�B
49B
VB	�B	�BB	��B	�?B	�B	�-B	�?B	�B	��B	��B	�1B	ffB	W
B	I�B	<jB	9XB	1'B	.B	?}B	XB	^5B	]/B	I�B	F�B	:^B	9XB	5?B	+B	"�B	�B	�B	
=B	
=B	1B	B��B��B��B��B��B�B�B�sB�5B�BŢB�}B�^B�RB�-B�B��B��B��B��B��B��B�{B�DB�%B�B�B~�B{�Bz�B{�B|�B~�B~�B�B|�BjBgmBe`BaHB_;BYBXBVBS�BR�BW
BXBjBhsBl�Bu�Bs�Bu�Bt�Bq�Bo�BiyBgmBjBm�BjBk�Bq�Bo�Bn�Bn�Bo�Bq�Bp�Bp�Bm�BhsBhsBhsBffBffBgmBaHB\)BZBVBI�BJ�BK�BK�BS�BR�BXBZBS�BM�BK�BJ�BG�B=qB?}BP�BW
BXB`BBgmBp�B�B�1B�hB��B��B�wB��B�;B�TB�yB��B	�B	(�B	C�B	O�B	Q�B	VB	iyB	p�B	u�B	}�B	�VB	�uB	��B	��B	�?B	�XB	�}B	ÖB	ǮB	��B	��B	��B	�B	�B	�5B	�ZB	�fB	�yB	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
+B

=B
DB
PB
\B
hB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
%�B
&�B
'�B
'�B
(�B
,B
.B
.B
0!B
1'B
2-B
2-B
49B
5?B
49B
5?B
6FB
7LB
8RB
:^B
;dB
;dB
<jB
>wB
>wB
>wB
@�B
@�B
B�B
D�B
D�B
E�B
F�B
I�B
I�B
K�B
J�B
K�B
M�B
M�B
N�B
P�B
Q�B
Q�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
XB
YB
ZB
[#B
\)B
]/B
^5B
_;B
aHB
`BB
`BB
bNB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
hsB
hsB
iyB
jB
jB
jB
l�B
l�B
l�B
n�B
n�B
o�B
o�B
o�B
q�B
q�B
s�B
s�B
t�B
t�B
t�B
v�B
v�B
v�B
x�B
x�B
x�B
z�B
|�B
~�B
� B
�B
�B
�B
�B
�%B
�+B
�+B
�1B
�=B
�DB
�JB
�JB
�VB
�VB
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
�B
�B
�B
�B
�B
�!B
�!B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�^B
�XB
�^B
�^B
�^B
�dB
�^B�XB�XB�LB�RB�XB�XB�LB�LB�LB�LB�FB�XB�RB�RB�RB�RB�XBƨBƨB��B�B�sB��B	B	VB	7LB	M�B	�B	��B	ȴB
bB
YB
��B
��B
��B
��B
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
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202282021061413515820210614135158202106141746132021061417461320210614174613201807242202282021061413515820210614135158202106141746132021061417461320210614174613PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422022820180724220228  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022820180724220228QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022820180724220228QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014820210722160148IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                