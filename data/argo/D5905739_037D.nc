CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-03T20:01:45Z creation      
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
_FillValue                 0  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  vt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                      HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   $   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � <   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar            HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                       SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   \   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � \Argo profile    3.1 1.2 19500101000000  20181003200145  20210617131505  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               %   %DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؃���r@؃���r11  @؃���J @؃���J @6�]9��@6�]9���c�H���c�H��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @@  @y��@�33@�33@�33A   AffA$��AD��Ac33A�  A�33A�33A�33A�  Aљ�A���A�33B   B��BffB��B��B'33B0  B8��B@ffBH  BO��BX  B`ffBh  BpffBxffB�33B�  B�  B�33B�33B�33B���B�33B�ffB�33B�  B�33B�33B�ffB�  B���B���B�33B�33B�  B���B�  B���Bۙ�B�33B�  B���B�33B�  B�B�  B�33C 33C  C�fC33CL�C
�C�3C�fC  C33C�C��C�fC�C33C33C L�C"ffC$�C%��C'�fC*  C,�C.33C033C2L�C4�C5�fC8  C:33C<L�C>�C?�fCB  CD33CF  CG��CJ  CL�CNL�CP33CR  CT33CV�CX  CZ33C\L�C^  C_��Ca��Cd  Cf�Ch33Cj  Ck�fCn  Cp33CrL�Ct�Cu��Cx  Cz  C|�C~33C�  C��fC�  C��C�&fC�  C��fC��3C�  C��C�&fC��C�  C��C��C�  C��C��C��3C�  C��C�  C��fC��3C��C��C��C��fC��C�  C��3C��C�  C��3C��C��C��3C��C�  C��3C��C��C��3C��C�  C��3C��C��C��3C��C�  C��3C�  C��C��C��3C��C�&fC�33C��C��3C�  C��C��C��fC�  C��C�&fC��C��3C��C��C�  C��fC��fC�  C��C��C�&fC��C��3C��3C�  C�  C�  C��C�&fC�&fC�33C��C��3C�  C�  C��C��C��C��C�&fC�33C��C��3C�  C��C��C�&fC��C��3C�  C�  C��C�  C�ٚC��3C�  C��C�&fC�33C��C��fC��3C�  C�  C�ٚC��C�&fC��fC��fC��3D   D �fD ٚD�3DL�D
� Dy�DfD��DFfD�3D�3D` D �D"��D%ffD'�3D*�fD-  D/�fD2,�D4��D73D9�3D<�D>� D@�3DCs3DE�3DH` DJ��DMFfDO��DR@ DT�3DW9�DYٚD\` D^�fDa� Dd3Df��DiY�Dk��Dn��Dq&fDs� DvS3Dx��D{l�D}�fD�fD�C3D��3D���D��fD�33D�p D�� D�� D�3D�9�D�ffD��3D��fD��3D�#3D�P D�|�D�� D��fD�	�D�0 D�ffD�� D���D�fD�<�D�y�D���D��3D��D�Y�D���D���D���D��D�C3D�s3D�� D���D��fD�&fD�L�D�y�D�� D��3D��fD�	�D�)�D�FfD�\�D�p D��fD���D���D��fD��fD��3D���D�� D��fD��D��fD�  D�fD�3D�  D�&fD�33D�<�D�9�D�FfD�I�D�P D�S3D�` D�c3D�l�D�vfDك3Dڐ Dۙ�Dܩ�Dݼ�D�� D�ٚD�� D�� D���D�fD�fD�)�D�<�D�L�D�VfD�` D�i�D�vfD�fD� D��D� D�3D�D�� D��fD���D�� D���D���D�ɚD��fD��fD��3D���D��fD���E K3E �3EI�E�fEA�E��E��E33E+3E�fE� E
+3E$�E� E��E!�EfE��E�3E E�3Ec3EɚE�fE&fE�fE� E�3E` E Y�E!��E#+3E$3E%{3E&� E'�3E)( E*� E+�E,њE.33E/��E0�3E1��E3c3E4\�E5�3E6�3E8+3E9��E:��E;� E>� EB�EEI�EH^fEKP EN��EQ� ET�fEW��E[+3E^a�EaD�Edp Eg� Ej�3Em�3Eq  Et1�EwI�EzffE}vfE�@ E��fE�nfE�  E��fE��fE� E�p�E�� E��E�nfE���E�3E�H E���E��3E�1�E���E��fE�1�E��3E�� E�!�E�p E�� E��E�X E���E���E�P�E��3E��E�4�E�� ?   >���>���>���>���>���>���>���>���>���>���?   >���>���?   >���>���>���>���>���>���>���?   >���>���?��?��?L��?fff?���?�ff?�  ?ٙ�@ff@33@,��@9��@S33@fff@y��@���@�ff@�33@���@�ff@�ff@�  @���@陚@�ffA��A33A��A33A!��A(  A0  A8  A@  AH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414414414144444144141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?fff?�  @   @`  @���@�33@�33@�33A  AffA,��AL��Ak33A�  A�33A�33A�33A�  Aՙ�A���A�33B  B
��BffB��B!��B)33B2  B:��BBffBJ  BQ��BZ  BbffBj  BrffBzffB�33B�  B�  B�33B�33B�33B���B�33B�ffB�33B�  B�33B�33B�ffB�  B���B���B�33B�33B�  B���B�  B���Bܙ�B�33B�  B���B�33B�  B���B�  B�33C �3C� CffC�3C��C
��C33CffC� C�3C��CL�CffC��C�3C�3C ��C"�fC$��C&L�C(ffC*� C,��C.�3C0�3C2��C4��C6ffC8� C:�3C<��C>��C@ffCB� CD�3CF� CHL�CJ� CL��CN��CP�3CR� CT�3CV��CX� CZ�3C\��C^� C`L�CbL�Cd� Cf��Ch�3Cj� ClffCn� Cp�3Cr��Ct��CvL�Cx� Cz� C|��C~�3C�@ C�&fC�@ C�L�C�ffC�@ C�&fC�33C�@ C�Y�C�ffC�L�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�33C�L�C�Y�C�L�C�&fC�L�C�@ C�33C�L�C�@ C�33C�Y�C�L�C�33C�Y�C�@ C�33C�Y�C�L�C�33C�L�C�@ C�33C�L�C�L�C�33C�L�C�@ C�33C�@ C�Y�C�L�C�33C�L�C�ffC�s3C�Y�C�33C�@ C�Y�C�L�C�&fC�@ C�L�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�&fC�@ C�L�C�Y�C�ffC�L�C�33C�33C�@ C�@ C�@ C�Y�C�ffC�ffC�s3C�L�C�33C�@ C�@ C�L�C�L�C�L�C�Y�C�ffC�s3C�Y�C�33C�@ C�L�C�Y�C�ffC�Y�C�33C�@ C�@ C�Y�C�@ C��C�33C�@ C�L�C�ffC�s3C�L�C�&fC�33C�@ C�@ C��C�Y�C�ffC�&fC�&fC�33D   D �fD ��D�3Dl�D  D��D&fD��DffD3D�3D� D ,�D"ٚD%�fD(3D*�fD-@ D/�fD2L�D4��D733D9�3D<,�D>� DA3DC�3DF3DH� DJ��DMffDO��DR` DT�3DWY�DY��D\� D_fDa� Dd33DfٚDiy�Dl�Dn��DqFfDs� Dvs3Dy�D{��D}�fD�fD�S3D��3D�ɚD�fD�C3D�� D�� D�� D�#3D�I�D�vfD��3D��fD�3D�33D�` D���D�� D��fD��D�@ D�vfD�� D���D�fD�L�D���D���D��3D�,�D�i�D���D���D���D�)�D�S3D��3D�� D���D�fD�6fD�\�D���D�� D��3D��fD��D�9�D�VfD�l�D�� D��fD���D���D��fD��fD��3D���D�� D��fD���D�fD� D�fD�#3D�0 D�6fD�C3D�L�D�I�D�VfD�Y�D�` D�c3D�p D�s3D�|�D؆fDٓ3Dڠ D۩�Dܹ�D���D�� D��D�� D�  D�	�D�fD�&fD�9�D�L�D�\�D�ffD�p D�y�D�fD�fD� D��D�� D��3D�ɚD�� D��fD���D�� D���D���D�ٚD��fD��fD��3D���D��fD���E S3E �3EQ�E�fEI�EɚE��E;3E33E�fE� E
33E,�E� E��E)�E&fE��E�3E E3Ek3EњE�fE.fE�fE� E�3Eh E a�E!��E#33E$#3E%�3E&� E'�3E)0 E*� E+�E,ٚE.;3E/��E0�3E1��E3k3E4d�E5�3E6�3E833E9��E:��E;� E>� EB$�EEQ�EHffEKX EN��EQ� ET�fEW��E[33E^i�EaL�Edx Eg� Ej�3Em�3Eq Et9�EwQ�EznfE}~fE�D E��fE�rfE� E��fE��fE� E�t�E�� E��E�rfE���E�3E�L E���E��3E�5�E���E��fE�5�E��3E�� E�%�E�t E�� E��E�\ E���E���E�T�E��3E��E�8�E�� G�O�G�O�?L��G�O�?L��G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�?fffG�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�?fffG�O�?���?�ff?�33?ٙ�?�ff@   @��@&ff@333@L��@Y��@s33@�33@���@���@�ff@�33@���@�ff@�ff@�  @���@���A33A��A33A��A#33A)��A0  A8  A@  AH  AP  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414414414144444144141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ �@ �@ �@ {@ �@ "�@ )�@ 0x@ 6�@ <�@ FQ@ S�@ `�@ m:@ z3@ ��@ ��@ ��@ ��@ ��@ �@ �t@ �y@ �q@�@b@[@,`@;d@H]@UU@bN@p�@~�@��@�H@��@��@@�7@ލ@�4@��@%@*@#�@0x@=q@K�@Yn@g�@t@�@��@��@�Y@�R@�J@Ӡ@��@�@�E@
=@6@&;@33@?}@N�@\�@k.@ww@�p@�#@�z@��@��@ȴ@�
@�@�@��@�@�@*S@7�@FQ@T�@`B@k�@z3@��@��@�5@��@�2@�|@��@�@� @v@�@
@,`@;d@G�@S�@b�@qS@�W@�P@��@��@��@@є@��@�@��@v@{@"�@1'@=q@Ji@X�@g�@v@�d@��@�@��@�@�W@Ӡ@��@��@�E@J@�@$.@2�@@�@O�@^5@j@ww@�|@�u@�m@�r@�k@ȴ@�
@�@�Y@��@�@�@*S@7L@B�@R�@_�@l�@{�@��@��@�5@�-@�w@�*@�t@�m@� @	@	b@	g@	,`@	9X@	H]@	V@	bN@	qS@	~K@	�D@	��@	��@	��@	��@	��@	��@	�@@	��@
%@
{@
#�@
0x@
<@
K@
Yn@
hs@
t�@
�@
�@
�a@
��@
��@
Ĝ@
Ӡ@
��@
�L@
��@
�@6@$�@33@@�@N�@]�@k�@y�@��@�u@��@�@��@�@׹@�`@�@@b@�@(G@6�@D�@SI@a�@n�@z3@��@�0@�5@�~@�@�@�t@��@��@%@�@[@+�@:@G�@SI@dZ@r�@|�@��@��@�A@��@�w@B�@��@�7@6@\�@�5@�4@5?@�W@�c@o@[z@��@�(@0x@ww@�j@^@E�@�+@�o@@Q�@��@�@[@_�@��@�`@*S@m:@�!@�@<�@��@ƨ@�@S�@�U@�@*S@r@�@  @FQ@�P@є@
�@O�@�u@׹@�@]�@�@�@(�@i�@�Y@�(@*S@j@�@�4@-@m:@�f@�@ -�@ oF@ �@ �L@!33@!s_@!�F@!�~@"<@"}�@"��@#�@#FQ@#��@#ȴ@$	�@$I�@$�7@$�@%
=@%Ji@%��@%��@&	�@&I�@&��@&ƨ@'�@'B�@'�W@'�@'�~@(33@(n�@(��@(�@)�@)V�@)�@)ȴ@*  @*7�@*oF@*��@*�H@+B@+R�@+��@+��@+�E@,5�@,k�@,�4@,܀@-{@-K�@-�@-�j@-�@.-�@.g@.�m@.�@/@/M�@/��@/�2@/�,@033@0k�@0�4@0�;@1�@1T�@1��@1�W@2  @28�@2r@2�@2�@3
@3X�@3�@3�@4  @47�@4o�@4�A@4�/@5�@5I�@5|?@5��@5��@6
@6SI@6��@6�@6�@7)�@7^�@7�u@7�@83�@8��@9>�@9��@:N�@:�@;^5@<  @<oF@=�@=}�@> �@>��@?(�@?��@@*S@@�>@A+�@A�J@B^�@B�c@CdZ@D  @Dj@Ev@E�a@F�@F��@G33@G��@H,`@H@IYn@I�k@JSI@J�4@KSI@K�@L��@L�e@M��@M� @N�#@O.l@O�u@P%�@Qm:@R�C@T-@U}�@V�&@X&;@Y��@Z��@\�@]�@^��@`[@aww@b�c@d�@eg�@fӠ@h0x@i�d@j�[@l$�@mp�@n��@p'�@q~K@rլ@sI@sDD@s�u@s�@tC@tk�@t��@t�@u%�@uqS@u�@u��@v7L@v�W@v�W@w�@wQ�@w�$@w�
@xO@x]�@x�@x��@y&;@yqS@y�9@y�@z3�@ztG�O�G�O�@ G�O�@ G�O�G�O�@ G�O�G�O�@ G�O�G�O�@ �G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�@ �G�O�@ @ v@ %@ 1@ �@ 
=@ �@ V@ �@ o@ �@ �@ �@ �@ [@  @ "�@ $�@ &�@ *S@ ,`@ /@ 1�@ 4�@ 8�@ ;d@ >@ B8@ D�@ G�@ K@ N�@ Q�@ UUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�XA�O�A�VA�XA�Q�A�G�A�I�A�C�A�?}A�5?A�-A� �A��A��A��A��A�{A�bA�VA�JA�A�A���A��A��mA��AҸRA҇+A�jA�`BA�XA�G�A�33A��A���A���A�hsAɏ\A�(�A�bAA�&�A�$�A�dZA�z�A�=qA�33A��7A���A���A�=qA�z�A�ĜA�bNA�{A�"�A�v�A��A�bNA�+A��#A�K�A�l�A��A��A��A��-A�+A��TA��uA�"�A�dZA���A�$�A�Q�A�ȴA�1'A��A��`A��^A�
=A�r�A��
A��\A�\)A���A��A�
=A�`BA��A�bNA�VA�-A�\)A��yA��A�oA�A��/A�5?A�Q�A�?}A��yA��jA��A�G�A�=qA���A�
=A��A��PA�{A��uA�A���A�=qA�
=A�ƨA�E�A}�TA|��A|r�A{hsAx�Avz�Au�wAsAp�`Am�7Al^5Aj��Ai/Ah{Agx�AfE�Ac�PAa?}A_A^��A]A[��A[&�AY�wAY+AX�AW�AV  AT=qASx�AR�\AQ�AQ;dAP��AOoAM�;AL��AK�AJv�AJQ�AH=qAG+AFAEhsAD�AD�RAB��A@�A@v�A>z�A;�#A;��A9\)A6$�A3�#A2=qA0�RA0r�A01'A/�hA.�yA-+A+
=A)�TA)/A'�;A'�A'XA&r�A%A#�A"n�A!S�A E�A��At�AS�A%Ar�A1A�wA�A��A�FA��A��AdZAC�A;dAz�A��AJA+AffA��A�+A�^A��A�PA�hA?}Ax�A��AJA;dA�/A��A
E�A	��A��A�A�A�A(�Ap�A�DA1A��A�HAbNA�At�A r�@�ȴ@��7@��u@�|�@�x�@�l�@���@�O�@�7L@��/@�ƨ@�@���@��@���@�I�@��@�7L@�  @�+@�?}@�@җ�@˥�@š�@�  @�;d@��u@�@���@��@�@��!@��@��y@��#@�dZ@�&�@��@��@�33@��@�ȴ@���@���@�J@�ff@�ƨ@�G�@�z�@�;d@��+@�p�@�1'@��@��R@��T@�bN@���@��y@��7@��@���@���@��@��-@�&�@�b@}V@{�m@z-@yx�@y�^@w��@w;d@u��@u�@st�@qx�@p  @m�T@lz�@kƨ@jJ@i��@g�@e��@ep�@cƨ@`��@^�y@]p�@\�@Y��@Xr�@V@T(�@R�!@Q�@P�9@N�@MO�@Lj@L�@J~�@I7L@H  @F5?@D��@C�
@B�@BJ@@bN@?�@?+@>v�@<�j@;S�@:J@8�9@7�@6V@5/@3�m@2��@0�`@0�9@/��@/\)@.E�@,��@+�@*��@)x�@(r�@';d@&��@%�@%?}@$�D@#�@"�@"n�@!x�@ �u@��@��@$�@�@��@C�@�@��@=q@�7@�u@l�@
=@�+@�T@V@��@I�@�@��@J@��@r�@�@�P@;d@ȴ@�-@V@�@�@��@@
^5@	��@	�^@Ĝ@b@�P@;d@��@�T@O�@�@j@dZ@��@��@X?��-?�dZ?���?��u?�ȴ?��?���?���?�%?���?���?ꟾ?���?��?���?��?���?ߝ�?�V?��?�dZ?���?���?׍P?��y?ա�?��/?��
?�o?�M�?�Ĝ?�A�?�  ?���?��?��?Ͳ-?�O�?�I�?�1?�C�?�C�?�?��#?ȓu?ǍP?�ff?��?�t�?�-?��7?� �?��?��?�/?���?�ƨ?�"�?�^5?���?�X?�7L?�7L?�x�?���?�=q?���?�dZ?�1?���?���?���?�V?�/?�p�?��h?��h?��-?��-?��?�{?�{?�5??�v�?�v�?���?��?���?��?��?�\)?�|�?���?���?��;?��;?� �?� �?�bNA�M�A�O�A�Q�A�O�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�S�A�S�A�S�A�VA�VA�XA�VA�XA�ZA�ZA�XA�ZA�ZA�ZA�ZA�XA�XA�XA�XA�VA�O�A�O�A�O�A�Q�A�VA�XA�XA�XA�VA�Q�A�Q�A�O�A�K�A�I�A�C�A�E�A�K�A�E�A�?}A�?}A�=qA�9XA�7LA�33A�33A�/A�-A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            A�S�A�XA�O�A�VA�XA�Q�A�G�A�I�A�C�A�?}A�5?A�-A� �A��A��A��A��A�{A�bA�VA�JA�A�A���A��A��mA��AҸRA҇+A�jA�`BA�XA�G�A�33A��A���A���A�hsAɏ\A�(�A�bAA�&�A�$�A�dZA�z�A�=qA�33A��7A���A���A�=qA�z�A�ĜA�bNA�{A�"�A�v�A��A�bNA�+A��#A�K�A�l�A��A��A��A��-A�+A��TA��uA�"�A�dZA���A�$�A�Q�A�ȴA�1'A��A��`A��^A�
=A�r�A��
A��\A�\)A���A��A�
=A�`BA��A�bNA�VA�-A�\)A��yA��A�oA�A��/A�5?A�Q�A�?}A��yA��jA��A�G�A�=qA���A�
=A��A��PA�{A��uA�A���A�=qA�
=A�ƨA�E�A}�TA|��A|r�A{hsAx�Avz�Au�wAsAp�`Am�7Al^5Aj��Ai/Ah{Agx�AfE�Ac�PAa?}A_A^��A]A[��A[&�AY�wAY+AX�AW�AV  AT=qASx�AR�\AQ�AQ;dAP��AOoAM�;AL��AK�AJv�AJQ�AH=qAG+AFAEhsAD�AD�RAB��A@�A@v�A>z�A;�#A;��A9\)A6$�A3�#A2=qA0�RA0r�A01'A/�hA.�yA-+A+
=A)�TA)/A'�;A'�A'XA&r�A%A#�A"n�A!S�A E�A��At�AS�A%Ar�A1A�wA�A��A�FA��A��AdZAC�A;dAz�A��AJA+AffA��A�+A�^A��A�PA�hA?}Ax�A��AJA;dA�/A��A
E�A	��A��A�A�A�A(�Ap�A�DA1A��A�HAbNA�At�A r�@�ȴ@��7@��u@�|�@�x�@�l�@���@�O�@�7L@��/@�ƨ@�@���@��@���@�I�@��@�7L@�  @�+@�?}@�@җ�@˥�@š�@�  @�;d@��u@�@���@��@�@��!@��@��y@��#@�dZ@�&�@��@��@�33@��@�ȴ@���@���@�J@�ff@�ƨ@�G�@�z�@�;d@��+@�p�@�1'@��@��R@��T@�bN@���@��y@��7@��@���@���@��@��-@�&�@�b@}V@{�m@z-@yx�@y�^@w��@w;d@u��@u�@st�@qx�@p  @m�T@lz�@kƨ@jJ@i��@g�@e��@ep�@cƨ@`��@^�y@]p�@\�@Y��@Xr�@V@T(�@R�!@Q�@P�9@N�@MO�@Lj@L�@J~�@I7L@H  @F5?@D��@C�
@B�@BJ@@bN@?�@?+@>v�@<�j@;S�@:J@8�9@7�@6V@5/@3�m@2��@0�`@0�9@/��@/\)@.E�@,��@+�@*��@)x�@(r�@';d@&��@%�@%?}@$�D@#�@"�@"n�@!x�@ �u@��@��@$�@�@��@C�@�@��@=q@�7@�u@l�@
=@�+@�T@V@��@I�@�@��@J@��@r�@�@�P@;d@ȴ@�-@V@�@�@��@@
^5@	��@	�^@Ĝ@b@�P@;d@��@�T@O�@�@j@dZ@��@��@X?��-?�dZ?���?��u?�ȴ?��?���?���?�%?���?���?ꟾ?���?��?���?��?���?ߝ�?�V?��?�dZ?���?���?׍P?��y?ա�?��/?��
?�o?�M�?�Ĝ?�A�?�  ?���?��?��?Ͳ-?�O�?�I�?�1?�C�?�C�?�?��#?ȓu?ǍP?�ff?��?�t�?�-?��7?� �?��?��?�/?���?�ƨ?�"�?�^5?���?�X?�7L?�7L?�x�?���?�=q?���?�dZ?�1?���?���?���?�V?�/?�p�?��h?��h?��-?��-?��?�{?�{?�5??�v�?�v�?���?��?���?��?��?�\)?�|�?���?���?��;?��;?� �?� �?�bNA�M�A�O�A�Q�A�O�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�S�A�S�A�S�A�VA�VA�XA�VA�XA�ZA�ZA�XA�ZA�ZA�ZA�ZA�XA�XA�XA�XA�VA�O�A�O�A�O�A�Q�A�VA�XA�XA�XA�VA�Q�A�Q�A�O�A�K�A�I�A�C�A�E�A�K�A�E�A�?}A�?}A�=qA�9XA�7LA�33A�33A�/A�-A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVBVBVBVBVBVBVBVBVBVBT�BVBVBVBVBVBT�BVBVBVBVBVBVBVBW
BXBYB\)B`BBaHBaHB`BB_;B_;B`BB^5BT�B[#B^5BdZBn�Bt�Bz�B�B�DB�1B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�oB�DB�By�Bp�Bm�BhsB`BB[#BW
BQ�BG�B<jB5?B-B&�B �BVB
=B%B��B��B�B�B�;B�#B��B��B�}B�B��B�PB�Bv�BjBdZB`BBT�BL�B;dB33B'�B�BVB
�B
�sB
�B
�FB
�B
��B
�uB
�B
x�B
s�B
o�B
hsB
R�B
H�B
@�B
(�B
DB
  B	��B	�yB	�5B	�B	��B	ǮB	�?B	��B	��B	�oB	�+B	� B	y�B	s�B	v�B	u�B	p�B	hsB	ZB	Q�B	O�B	M�B	I�B	B�B	;dB	6FB	1'B	'�B	$�B	!�B	uB	oB	�B	bB	PB	\B	B��B��B�mB�#B�)BĜB�B��B�oB�bB��B��B��B�hB�%B�B|�B{�Bw�Bt�Bs�Bl�BiyBgmBgmBgmBe`BdZBcTBcTBffBhsBhsBhsBhsBgmBhsBe`BaHBaHB`BB_;B\)B[#BZBZBW
BVBR�BT�BS�BR�BR�BP�BR�BR�BL�BK�BH�BG�BF�BE�BB�BA�BA�BA�BA�B@�B>wB>wB>wB=qB=qB<jB=qB;dB;dB;dB:^B9XB9XB9XB:^B:^B9XB8RB8RB6FB8RB8RB8RB7LB9XB6FB7LB7LB7LB?}BE�BE�BN�BVB[#BdZBgmBt�B|�B��B��B��B�3B��BǮB��B�fB�B	DB	\B	"�B	49B	:^B	J�B	cTB	t�B	�VB	��B	��B	��B	�B	�9B	�RB	�RB	�jB	ĜB	��B	��B	��B	��B	�B	�5B	�`B	�sB	�B	�B	�B	�B	��B	��B	��B
B
B
B
B
B
+B
	7B
JB
VB
\B
hB
oB
{B
�B
�B
�B
�B
�B
�B
 �B
#�B
%�B
&�B
)�B
+B
+B
-B
/B
1'B
1'B
2-B
33B
49B
6FB
7LB
8RB
9XB
9XB
:^B
<jB
=qB
>wB
=qB
@�B
A�B
B�B
D�B
E�B
F�B
G�B
H�B
I�B
K�B
M�B
M�B
O�B
P�B
P�B
R�B
S�B
S�B
T�B
VB
W
B
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
]/B
_;B
`BB
bNB
bNB
cTB
cTB
dZB
ffB
ffB
hsB
hsB
hsB
iyB
jB
jB
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
n�B
p�B
q�B
q�B
r�B
t�B
s�B
t�B
t�B
t�B
v�B
v�B
w�B
w�B
y�B
y�B
y�B
z�B
{�B
|�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�+B
�+B
�7B
�=B
�DB
�DB
�JB
�VB
�\B
�bB
�bB
�oB
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
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�3B
�?B
�?B
�FB
�RB
�LB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�XB
�XB
�^B
�XB
�^B
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�^B
�^B
�XB
�^B
�^B
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�^B
�XB
�^B
�XBW
BVBVBVBW
BVBVBVBVBVBT�BT�BVBW
BW
BVBVBT�BVBVBVBVBVBVBVBVBT�BVBVBVBVBVBVBVBVBW
BVBVBVBVBVBVBVBVBT�BVBVBVBVBT�BVBVBVBVBT�BT�BT�BVBVBVG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            BU�BU�BU�BU�BU�BU�BU�BU�BU�BU�BT�BU�BU�BU�BU�BU�BT�BU�BU�BU�BU�BU�BU�BU�BV�BW�BX�B\B`(Ba/Ba/B`*B_$B_$B`,B^BT�B[B^ BdEBn�Bt�Bz�B�B�2B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�iB�>B�By�Bp�Bm�BhpB`?B[!BWBQ�BG�B<jB5?B-B&�B �BXB
@B(B��B��B�B�B�AB�)B��B��B��B�B��B�YB�(Bv�Bj�BdeB`MBU
BL�B;qB3@B'�B�BeB
�B
�B
� B
�VB
�B
��B
��B
�+B
x�B
s�B
o�B
h�B
SB
H�B
@�B
)B
ZB
 B	��B	�B	�MB	�5B	�B	��B	�XB	�B	��B	��B	�FB	�B	y�B	s�B	v�B	u�B	p�B	h�B	Z<B	RB	O�B	M�B	I�B	B�B	;�B	6iB	1JB	(B	%B	!�B	�B	�B	�B	�B	wB	�B	4B�B�B�B�MB�SB��B�?B��B��B��B��B��B��B��B�TB�5B}B|Bw�Bt�Bs�Bl�Bi�Bg�Bg�Bg�Be�Bd�Bc�Bc�Bf�Bh�Bh�Bh�Bh�Bg�Bh�Be�Ba�Ba�B`~B_xB\fB[aBZ[BZ\BWIBVCBS2BU>BT9BS4BS4BQ(BS5BS6BMBLBH�BG�BF�BE�BB�BA�BA�BA�BA�B@�B>�B>�B>�B=�B=�B<�B=�B;�B;�B;�B:�B9�B9�B9�B:�B:�B9�B8�B8�B6�B8�B8�B8�B7�B9�B6�B7�B7�B7�B?�BFBFBO?BVlB[�Bd�Bg�Bu0B}eB�B�NB�|B��B�B�7B�xB��B�B	�B	�B	#lB	4�B	:�B	KeB	c�B	ufB	�B	�=B	�xB	��B	��B	��B	�B	�B	�/B	�dB	ˌB	ΡB	��B	��B	��B	�B	�>B	�TB	�iB	�xB	�B	�B	��B	��B	��B
B
B
B
B
#B
2B

@B
VB
eB
nB
}B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
%B
'B
( B
+5B
,>B
,AB
.PB
0`B
2oB
2rB
3{B
4�B
5�B
7�B
8�B
9�B
:�B
:�B
;�B
=�B
>�B
?�B
>�B
A�B
B�B
DB
FB
GB
H'B
I/B
J8B
K@B
MPB
O_B
OaB
QoB
RxB
RzB
T�B
U�B
U�B
V�B
W�B
X�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\�B
\�B
\�B
^�B
`�B
bB
dB
dB
eB
e B
f)B
h7B
h:B
jIB
jLB
jNB
kWB
l_B
lbB
mkB
mmB
nvB
nxB
o�B
o�B
p�B
p�B
q�B
p�B
r�B
s�B
s�B
t�B
v�B
u�B
v�B
v�B
v�B
x�B
x�B
y�B
y�B
{�B
{�B
{�B
}B
~B
B
�&B
�+B
�2B
�IB
�VB
�bB
�oB
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�1B
�>B
�IB
�]B
�cB
�nB
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
� B
�B
�B
�B
�B
�9B
�PB
�fB
��B
��B
��B
��B
��B
��B
�B
�B
�+B
�GB
�VB
�lB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�*B
�2B
�0B
�:B
�6B
�:B
�<B
�@B
�HB
�EB
�IB
�QB
�TB
�RB
�[B
�^B
�[B
�^B
�aB
�jB
�gB
�jB
�mB
�vB
�sB
�|B
�yB
��B
�BV�BU�BU�BU�BV�BU�BU�BU�BU�BU�BT�BT�BU�BV�BV�BU�BU�BT�BU�BU�BU�BU�BU�BU�BU�BU�BT�BU�BU�BU�BU�BU�BU�BU�BU�BV�BU�BU�BU�BU�BU�BU�BU�BU�BT�BU�BU�BU�BU�BT�BU�BU�BU�BU�BT�BT�BT�BU�BU�BU�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810032001452021061413554320210614135543202106171313072021061713130720210617131307201810032001452021061413554320210614135543202106171313072021061713130720210617131307PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018100320014520181003200145  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100320014520181003200145QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100320014520181003200145QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150520210617131505IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                