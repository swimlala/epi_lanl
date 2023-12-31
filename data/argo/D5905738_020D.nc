CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-29T17:03:25Z creation      
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
_FillValue                 �  Kx   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  rx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                      HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                      HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                       HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   (   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                       SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ߨ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � P      � PArgo profile    3.1 1.2 19500101000000  20180729170325  20210722160151  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�s�^~@�s�^~11  @�s��_@@�s��_@@6��ϕ��@6��ϕ���c�L�W��c�L�W�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?fff@   @@  @�  @�  @���@�33A   AffA#33AA��A`  A~ffA�33A�  A���A���Aљ�A�ffA�  A�33B��BffBffB   B(  B0  B8  B@��BH  BO��BW��B_��Bg��Bp  Bx  B�  B�ffB�33B�  B�33B�  B���B���B���B�33B�ffB�ffB�33B�33B�33B�33B�33B�  B�ffB���B���B���B�  B���B���B���B���B���B�33B�33B�  B���B���C  C�fC�C33C
�C33C�C�C  C  C  C  C  C�C�C �C"�C$�C&  C(�C*  C,�C.  C0  C2  C4�C6�C833C:  C<  C>  C@  CB  CD�CF  CH  CJ  CLL�CN�CP  CRL�CT�CV  CX33CZ33C\  C^33C`�Ca�fCc�fCf�Ch33CjL�Cl  Cm�3Co��Cq�3Cs��Cu�fCw�fCy�fC|  C~�C��C��C�&fC��C��fC��3C�  C��C�&fC�  C��fC�  C��C��C�  C��fC�  C�&fC��C��C�&fC��C��C��C�  C��3C��fC��C��C��C��C��C��C�  C��C��C�&fC��C�33C�  C�  C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C�&fC��3C��3C��C��C�  C�&fC��C��C��3C��fC��3C��C��C�  C�&fC��C��3C��C��C��C��fC�  C��C��C�  C��fC��fC��C��C��C��C��fC��3C��3C�  C�  C��C��C�&fC�  C��fC��3C�  C��C��C�&fC�&fC��C��fC��3C��3C�  C��C��C��C��C��C�&fC��C��3C��3C��3C�  C��C��3D` DfD��D
s3D,�D�fD� DFfD��D��DFfD��D"��D%9�D'�fD*ffD-  D/�3D2  D4�3D7@ D9�3D<l�D>��DA�3DD33DF��DI�fDLL�DOfDQ�fDT�3DW` DZ�D\� D_��DbY�De3Dg�fDjs3Dm  Do��Drs3Du  Dw��Dz�D|9�D~��D�� D���D��D�P D���D���D���D�  D�S3D��fD��3D�fD�C3D�y�D��fD���D�  D�Y�D�� D�ٚD� D�FfD���D���D�	�D�P D�� D��fD��D�c3D��fD�� D��D�VfD�� D�ɚD���D�)�D�\�D��3D���D�� D���D�&fD�P D�� D���D��3D���D��D�L�D�y�D���D��3D�3D�I�Dă3D�� D���D�6fD�l�Dʬ�D���D�#3D�S3DφfDм�D��D��D�L�DՀ D֬�D�� D�fD�C3D�s3Dܣ3D��3D�3D�,�D�\�D�fD��D�� D�#3D�S3D�3D��D���D��D�0 D�Y�D�3D�D�� D��3D��D�<�D�ffD��fD��fD��3D�� D��3D�� D���D��E 3E � EfE��E�E��E  E��E( E��E+3E��E0 E�3E9�EI�E	�fE
� Ea�El�Et�E��E3E��E�fE�fE@ EP E�3E��E�fE��E��E��E FfE!T�E"�fE#��E$�3E&t�E'x E(�3E*  E+y�E,s3E-� E/D�E033E1��E33E3��E5a�E6�3E7�fE9�E:k3E;ɚE>��EA� EEfEH6fEKD�EN<�EQ��ET��EW�3EZ��E]�E`�3Ed0 Eg[3Ej�3Em��Ep��EsٚEw�Ez!�E}fE��E��3E�U�E�� E�_3E��3E�m�E��E�� E�73?   >���>���?   >���?   >���>���?   ?   >���>���>���>���?   >���?   ?��?��?L��?�  ?���?�33?���?�33@��@��@,��@@  @S33@fff@�  @���@���@�ff@�33@���@ə�@ٙ�@陚@�ffA33A	��A33A��A#33A+33A4��A<��AFffANffAT��A`  Ai��Aq��Ay��A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414114444414114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?fff?�33@   @`  @�  @�  @���@�33A  AffA+33AI��Ah  A�33A�33A�  A���Ař�Aՙ�A�ffA�  B��B	��BffBffB"  B*  B2  B:  BB��BJ  BQ��BY��Ba��Bi��Br  Bz  B�  B�ffB�33B�  B�33B�  B���B���B���B�33B�ffB�ffB�33B�33B�33B�33B�33B�  B�ffB���B���B���B�  B���B���B���B���B���B�33B�33B�  B���C L�C� CffC��C�3C
��C�3C��C��C� C� C� C� C� C��C��C ��C"��C$��C&� C(��C*� C,��C.� C0� C2� C4��C6��C8�3C:� C<� C>� C@� CB� CD��CF� CH� CJ� CL��CN��CP� CR��CT��CV� CX�3CZ�3C\� C^�3C`��CbffCdffCf��Ch�3Cj��Cl� Cn33CpL�Cr33CtL�CvffCxffCzffC|� C~��C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�ffC�@ C�&fC�@ C�L�C�Y�C�@ C�&fC�@ C�ffC�Y�C�L�C�ffC�Y�C�Y�C�L�C�@ C�33C�&fC�L�C�Y�C�L�C�L�C�L�C�L�C�@ C�Y�C�Y�C�ffC�Y�C�s3C�@ C�@ C�L�C�@ C�L�C�L�C�Y�C�L�C�Y�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�Y�C�ffC�33C�33C�L�C�L�C�@ C�ffC�Y�C�L�C�33C�&fC�33C�Y�C�L�C�@ C�ffC�Y�C�33C�L�C�Y�C�L�C�&fC�@ C�L�C�Y�C�@ C�&fC�&fC�L�C�Y�C�Y�C�L�C�&fC�33C�33C�@ C�@ C�L�C�L�C�ffC�@ C�&fC�33C�@ C�L�C�L�C�ffC�ffC�L�C�&fC�33C�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�ffC�L�C�33C�33C�33C�@ C�L�C�33D� D&fDٚD
�3DL�DfD� DffD�D��DffD �D"��D%Y�D'�fD*�fD-  D/�3D2@ D4�3D7` D9�3D<��D?�DA�3DDS3DF��DI�fDLl�DO&fDQ�fDT�3DW� DZ9�D]  D_��Dby�De33Dg�fDj�3Dm@ Do��Dr�3Du  Dw��Dz9�D|Y�D~ٚD�� D���D�,�D�` D���D�ɚD���D�0 D�c3D��fD��3D�fD�S3D���D��fD���D�0 D�i�D�� D��D�  D�VfD���D���D��D�` D�� D��fD�,�D�s3D��fD�� D�)�D�ffD�� D�ٚD�	�D�9�D�l�D��3D���D�� D��D�6fD�` D�� D���D��3D��D�,�D�\�D���D���D��3D�#3D�Y�Dē3D�� D��D�FfD�|�Dʼ�D���D�33D�c3DϖfD���D���D�,�D�\�DՐ Dּ�D�� D�&fD�S3Dۃ3Dܳ3D��3D�3D�<�D�l�D�fD���D�  D�33D�c3D�3D��D���D��D�@ D�i�D�3D�D�� D�3D�)�D�L�D�vfD��fD��fD��3D�� D��3D�  D��D��E 3E � EfE��E$�E��E( E��E0 E��E33E��E8 E�3EA�EQ�E	�fE
� Ei�Et�E|�E�E3E��E�fE�fEH EX E�3E��E�fE��E��E��E NfE!\�E"�fE#��E$�3E&|�E'� E)3E* E+��E,{3E-� E/L�E0;3E1��E33E3��E5i�E6�3E7�fE9�E:s3E;њE>��EA� EEfEH>fEKL�END�EQ��ET��EW�3EZ��E]��E`�3Ed8 Egc3Ej�3Em��Ep��Es�Ew�Ez)�E}fE��E��3E�Y�E�� E�c3E��3E�q�E��E�� E�;3G�O�G�O�?fffG�O�?fffG�O�?L��?fffG�O�G�O�G�O�G�O�G�O�?fffG�O�?fff?�  G�O�?���?�ff?�  ?ٙ�?�33@ff@��@,��@9��@L��@`  @s33@�33@�  @���@���@�ff@�33@���@ٙ�@陚@���A33A33A��A33A$��A+33A333A<��AD��ANffAVffA\��Ah  Aq��Ay��A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414114444414114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ �@ %@ �@ {@ O@ ""@ (G@ 0x@ 6�@ <�@ E�@ R�@ _�@ l�@ z3@ ��@ ��@ �5@ ��@ �2@ ��@ ��@ �m@ �q@@@�@,`@:@I@UU@bN@o�@}�@�D@��@�A@��@��@��@��@�4@�,@v@@ �@0x@>�@Lu@Yn@g@t�@�d@�@�@��@��@�J@��@�H@�@@��@	�@6@$�@3�@A�@N�@[z@hs@ww@�p@�u@��@��@�@�@׹@�@�Y@  @�@O@)�@7L@D�@R�@`B@m:@{�@��@��@��@�~@�&@�|@�#@�y@��@j@@�@,`@:�@G�@UU@b�@r�@~�@��@��@��@��@��@є@��@��@��@%@�@"�@1'@?}@K@V�@e	@r@�W@��@�U@��@�R@ƨ@�O@�@��@�E@�@6@%�@3�@B�@N�@Z�@i�@x&@�|@��@�@�@��@��@׹@�@�@^@V@O@(G@5?@D�@SI@`B@m�@{�@�7@�0@�5@��@�2@�*@�/@�@��@	@	@	g@	-@	;d@	H]@	V�@	c�@	r@	�@	�P@	��@	��@	�F@	Ĝ@	є@	��@	��@	�~@
�@
*@
""@
1�@
>�@
K�@
X@
e	@
s_@
�@
�@
�@
��@
��@
�J@
�O@
�@
�@
�9@
=@�@&�@33@?}@M$@\�@k.@x�@��@�h@��@�f@��@�c@׹@�`@�e@  @J@�@(�@7L@D�@S�@a�@m�@y�@��@��@��@�-@��@�*@��@�y@��@@b@
@+�@:@H]@T�@�@0x@z3@Ĝ@@Yn@��@�4@5�@}�@ƨ@@V�@��@�`@-@t@�^@  @FQ@��@�C@B@^�@��@�@4�@~�@��@*@`A@��@�,@C�@�\@��@$�@oF@�@@K@�$@܀@""@g�@�f@�m@+�@p�@�9@�~@:@|�@��@�Q@@�@�d@��@�@Lu@�@�C@�@X@��@܀@ ""@ e	@ �A@ �y@!/@!s_@!��@!��@"@�@"�|@"�@#�@#V�@#��@#܀@$ @$b�@$��@$�@%'�@%i!@%��@%�m@&%�@&e�@&�4@&�@'%�@'e	@'��@'�@(!s@(bN@(�z@(�T@)&;@)g@)�M@)�4@*/�@*s_@*�F@*�~@+<�@+�@+�>@,@,E�@,��@,�@-	�@-Ji@-��@-�@.�@.O�@.�@.��@/�@/R�@/�u@/��@0�@0SI@0��@0�
@1�@1Yn@1�H@1��@2�@2[z@2��@2�@3�@3Wb@3�0@3�O@4@4Q=@4��@4�*@5�@5H]@5~K@5�@5��@6/@6i!@6��@6�t@7b@7I�@7��@7�^@7�Y@8+@8b�@8�I@8є@9�@9@�@9x�@9�-@:&;@:�@;=q@;�`@<Wb@<�@=oF@=�@>��@>�Q@?o�@@O@@�\@A>�@A��@B	@B�o@CA�@C��@DbN@Dխ@E�@E��@F`�@Gv@Gt@HB@H��@I)�@I�#@J/�@J�@K-�@Kƨ@L`�@L�@Mc�@M��@NbN@N�e@O��@Pg@QM$@R��@T�@Ul�@V�^@W��@YqS@Z��@[� @]hs@^�-@_�@a\)@b�F@do@eYn@fĜ@h
�@ie	@j�@k�}@mLv@n�(@po@qff@r��@t%@uE�@v�a@x�@y[zG�O�G�O�@ �G�O�@ �G�O�@ @ �G�O�G�O�G�O�G�O�G�O�@ �G�O�@ �@ jG�O�@ @ v@ �@ 1@ 	�@ 
�@ �@ @ b@ o@ {@ �@ �@ O@ [@  �@ #�@ &;@ (G@ +@ .l@ 1�@ 4�@ 7�@ :�@ >�@ B�@ E�@ I@ M$@ P�@ T�@ X@ Z�@ _�@ c�@ g@ j@ m�@ qS@ t�@ x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jAБhAГuA�~�AЅAЅAЅAЁAЃAЇ+AЇ+AЉ7AБhAН�AП�AС�AС�AС�AУ�AХ�AХ�AХ�AЬAЩ�AЩ�AЩ�AЮAЮAа!AЮA�%A���A��yA�5?A�(�A�ZA�33A��A�jA�A�r�A��A���A��A�33A�VA�S�A�|�A���A��;A�VA���A�VA�^5A��wA��A�jA���A��A�(�A��RA�~�A��A�~�A��RA�I�A��HA���A��7A��\A�A�A���A���A��`A���A�^5A��!A�p�A�Q�A�-A�%A�=qA��9A��PA�;dA�ffA��FA�33A���A���A�/A���A�9XA���A�A��9A��A���A�A�%A�ȴA�A��wA�bNA�p�A��!A�Q�A�
=A�ƨA�S�A��A��A���A�-A�ĜA���A~�DA|�\A{��A{7LAz��Ayx�Av$�As�^ArQ�Ap(�Am�AkdZAgx�Af�Ae�PAd~�A`ĜA_\)A^��A]K�A\��A[�#AZ�AY��AY|�AV�AS;dAQ�AP5?AMdZAKC�AK"�AJ�`AI�AI�AH-AG
=AFZAEt�AD^5AC��AB��AA�;A>�A<��A;�
A;K�A9�7A8Q�A6�yA5�-A3�TA2�uA1t�A/XA.jA-��A,�A,9XA+A+?}A*�RA(�A( �A'��A&ĜA%K�A#
=A" �A!�A �/A ~�A ^5A $�A?}A�Ax�A��A�A��AVA�7A7LA/A�!A��AffA5?A�PA�yA�hAS�A�A^5A��A7LA
=AbNA��A�RA�mA��A�TA
A�A	?}A��A�TAE�A�yAJA�Ar�AJAffA�TA �@���@�@��@�z�@�(�@�$�@��j@�@�
=@�M�@��@�F@홚@�z�@�(�@��m@�@�@�ff@�u@�F@�@�+@�n�@�?}@�Q�@���@�z�@���@Ƨ�@�Z@�X@���@�1@�~�@���@�@�(�@�&�@��@��!@�p�@�M�@��@���@� �@��+@�&�@��D@�b@���@�%@�t�@���@�5?@�&�@�Z@�33@�S�@���@��@�@��-@� �@�1@�ƨ@�+@�ff@�@~E�@z��@y�@wK�@vV@x�u@z�@y�^@vV@uO�@r�H@p��@o\)@nv�@l�D@jJ@h�9@f�R@e�h@c�m@a�^@^��@]�@\z�@[S�@Y�7@X�@W�w@V$�@T�j@Rn�@P�9@O�@N$�@L��@J~�@H��@G��@G;d@F�R@F$�@D��@CdZ@B^5@@��@?�w@>ȴ@=?}@;��@;S�@9��@9&�@8  @7\)@6�y@5`B@4�/@3�m@2��@1�7@0r�@/��@.�+@.@-/@,�@,�@+"�@*J@(��@(b@'�@&$�@%��@%V@$�D@#��@"��@!��@!�#@ �9@|�@K�@V@$�@�h@p�@�/@�D@�!@hs@�9@l�@�@E�@V@5?@��@?}@�D@dZ@��@��@�9@
=@$�@?}@9X@33@
=q@	�#@	��@A�@�y@�T@��@��@(�@��@dZ@dZ@n�@-@��@ �`@ b?��?�"�?���?���?�J?�  ?�p�?�ƨ?�?��#?��y?���?�!?���?��?��?�O�?�(�?�^5?ؓu?�Q�?�+?��y?ԛ�?�9X?Ұ!?�J?У�?� �?�\)?��?�O�?�C�?�^5?��?�x�?�7L?���?��?�+?�ȴ?�`B?��?�J?�%?�;d?�5??�p�?��?�(�?��m?�ƨ?�C�?�"�?�~�?�=q?��?���?�"�?���?�dZ?�?�?��?�ƨ?�1?��D?�V?��h?�{?��?���A�v�A�x�A�v�A�t�A�p�A�p�A�n�A�l�A�hsA�jA�dZA�dZA�bNA�^5A�^5A�VA�p�AЍPAЍPAЉ7AБhAЙ�AЙ�AЛ�AЙ�AЏ\AЉ7AЃA�|�A�|�AЅAЅAЅAЃAЇ+AЃAЇ+AЅAЁA�~�AЁAЅAЇ+AЇ+AЇ+AЉ7AЇ+AЇ+AЇ+AЇ+AЋDAБhAБhAБhAЗ�AН�AП�AС�AП�AП�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�jAБhAГuA�~�AЅAЅAЅAЁAЃAЇ+AЇ+AЉ7AБhAН�AП�AС�AС�AС�AУ�AХ�AХ�AХ�AЬAЩ�AЩ�AЩ�AЮAЮAа!AЮA�%A���A��yA�5?A�(�A�ZA�33A��A�jA�A�r�A��A���A��A�33A�VA�S�A�|�A���A��;A�VA���A�VA�^5A��wA��A�jA���A��A�(�A��RA�~�A��A�~�A��RA�I�A��HA���A��7A��\A�A�A���A���A��`A���A�^5A��!A�p�A�Q�A�-A�%A�=qA��9A��PA�;dA�ffA��FA�33A���A���A�/A���A�9XA���A�A��9A��A���A�A�%A�ȴA�A��wA�bNA�p�A��!A�Q�A�
=A�ƨA�S�A��A��A���A�-A�ĜA���A~�DA|�\A{��A{7LAz��Ayx�Av$�As�^ArQ�Ap(�Am�AkdZAgx�Af�Ae�PAd~�A`ĜA_\)A^��A]K�A\��A[�#AZ�AY��AY|�AV�AS;dAQ�AP5?AMdZAKC�AK"�AJ�`AI�AI�AH-AG
=AFZAEt�AD^5AC��AB��AA�;A>�A<��A;�
A;K�A9�7A8Q�A6�yA5�-A3�TA2�uA1t�A/XA.jA-��A,�A,9XA+A+?}A*�RA(�A( �A'��A&ĜA%K�A#
=A" �A!�A �/A ~�A ^5A $�A?}A�Ax�A��A�A��AVA�7A7LA/A�!A��AffA5?A�PA�yA�hAS�A�A^5A��A7LA
=AbNA��A�RA�mA��A�TA
A�A	?}A��A�TAE�A�yAJA�Ar�AJAffA�TA �@���@�@��@�z�@�(�@�$�@��j@�@�
=@�M�@��@�F@홚@�z�@�(�@��m@�@�@�ff@�u@�F@�@�+@�n�@�?}@�Q�@���@�z�@���@Ƨ�@�Z@�X@���@�1@�~�@���@�@�(�@�&�@��@��!@�p�@�M�@��@���@� �@��+@�&�@��D@�b@���@�%@�t�@���@�5?@�&�@�Z@�33@�S�@���@��@�@��-@� �@�1@�ƨ@�+@�ff@�@~E�@z��@y�@wK�@vV@x�u@z�@y�^@vV@uO�@r�H@p��@o\)@nv�@l�D@jJ@h�9@f�R@e�h@c�m@a�^@^��@]�@\z�@[S�@Y�7@X�@W�w@V$�@T�j@Rn�@P�9@O�@N$�@L��@J~�@H��@G��@G;d@F�R@F$�@D��@CdZ@B^5@@��@?�w@>ȴ@=?}@;��@;S�@9��@9&�@8  @7\)@6�y@5`B@4�/@3�m@2��@1�7@0r�@/��@.�+@.@-/@,�@,�@+"�@*J@(��@(b@'�@&$�@%��@%V@$�D@#��@"��@!��@!�#@ �9@|�@K�@V@$�@�h@p�@�/@�D@�!@hs@�9@l�@�@E�@V@5?@��@?}@�D@dZ@��@��@�9@
=@$�@?}@9X@33@
=q@	�#@	��@A�@�y@�T@��@��@(�@��@dZ@dZ@n�@-@��@ �`@ b?��?�"�?���?���?�J?�  ?�p�?�ƨ?�?��#?��y?���?�!?���?��?��?�O�?�(�?�^5?ؓu?�Q�?�+?��y?ԛ�?�9X?Ұ!?�J?У�?� �?�\)?��?�O�?�C�?�^5?��?�x�?�7L?���?��?�+?�ȴ?�`B?��?�J?�%?�;d?�5??�p�?��?�(�?��m?�ƨ?�C�?�"�?�~�?�=q?��?���?�"�?���?�dZ?�?�?��?�ƨ?�1?��D?�V?��h?�{?��?���A�v�A�x�A�v�A�t�A�p�A�p�A�n�A�l�A�hsA�jA�dZA�dZA�bNA�^5A�^5A�VA�p�AЍPAЍPAЉ7AБhAЙ�AЙ�AЛ�AЙ�AЏ\AЉ7AЃA�|�A�|�AЅAЅAЅAЃAЇ+AЃAЇ+AЅAЁA�~�AЁAЅAЇ+AЇ+AЇ+AЉ7AЇ+AЇ+AЇ+AЇ+AЋDAБhAБhAБhAЗ�AН�AП�AС�AП�AП�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�Bt�B�B8RB33B33B0!B$�B7LB]/Bm�B{�B�=B�hB��B��B��B�JB�DB�+B��B��BƨB�wB�XB�3B�B��B�7B� Bu�Bm�Be`BW
BG�BM�BL�BF�B?}B1'B'�B�B��B�BB�B��BŢB��B��B�Bv�B�oB�Bz�By�Bp�B[#BH�B>wB&�B�B	7B
��B
�B
�B
��B
ŢB
�RB
��B
��B
�\B
�7B
�B
{�B
v�B
iyB
s�B
|�B
k�B
e`B
\)B
\)B
\)B
\)B
\)B
^5B
]/B
O�B
I�B
D�B
>wB
:^B
2-B
�B
DB
%B	��B	�B	�fB	��B	��B	ŢB	�}B	��B	��B	��B	�oB	�VB	�%B	~�B	|�B	w�B	W
B	J�B	8RB	+B	�B	JB		7B	B��B��B��B�B�B�B�sB�mB�fB�5B��BÖBB�wB�?B�-B�B��B��B��B�=B�B� Bz�Bx�Bx�Bv�Bu�Bq�Bl�Bm�BjBhsBe`BaHBcTB_;B_;B^5B^5B[#B[#BZBZBYBXBXBR�BS�BS�BR�BN�BL�BK�BN�BN�BJ�BE�BE�BC�BB�BC�BD�BC�BC�BB�BB�B?}B@�B;dB<jB=qB:^B7LB7LB7LB5?B5?B1'B5?B:^B;dB6FB1'B1'B5?B5?B/B+B+B)�B)�B)�B)�B0!B1'B33B6FB=qB>wB?}B9XB;dBB�BE�BD�BC�B_;B[#B~�B�bB��B��B�3B�?BB��B�#B	1B	B	&�B	I�B	J�B	<jB	2-B	9XB	E�B	H�B	W
B	_;B	e`B	hsB	hsB	u�B	|�B	|�B	�+B	�oB	�uB	{�B	s�B	�B	�7B	��B	�!B	�^B	�wB	��B	�B	�;B	�HB	�mB	�`B	�TB	�ZB	�sB	�sB	��B
  B
B
%B
+B
+B
1B
JB
PB
hB
bB
hB
uB
�B
�B
�B
�B
!�B
"�B
%�B
&�B
'�B
(�B
)�B
+B
-B
/B
0!B
1'B
2-B
5?B
6FB
8RB
8RB
9XB
9XB
;dB
;dB
<jB
=qB
?}B
@�B
A�B
C�B
B�B
D�B
D�B
E�B
F�B
E�B
H�B
H�B
J�B
J�B
L�B
M�B
N�B
O�B
P�B
Q�B
P�B
R�B
S�B
T�B
VB
W
B
T�B
XB
XB
YB
ZB
ZB
YB
ZB
\)B
]/B
^5B
^5B
`BB
aHB
`BB
aHB
bNB
bNB
dZB
ffB
ffB
gmB
hsB
hsB
gmB
hsB
hsB
iyB
jB
k�B
l�B
n�B
n�B
q�B
q�B
r�B
s�B
t�B
u�B
u�B
u�B
w�B
x�B
y�B
{�B
{�B
|�B
{�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
�%B
�=B
�=B
�JB
�PB
�VB
�\B
�bB
�hB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�'B
�-B
�-B
�9B
�9B
�9B
�9B
�FB
�FB
�LB
�RB
�RB
�RB
�RB
�RB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB �B�B �B�B�B�B�B�B�B�B �B�B�B�B�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                         B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�Br�B�B6FB1'B1'B.B"�B5?B[#Bk�By�B�1B�\B��B��B�{B�=B�7B�B��B�wBĜB�jB�LB�'B��B��B�+B}�Bs�Bk�BcTBT�BE�BK�BJ�BD�B=qB/B%�B�B��B�5B�
B��BÖB��B�{B� Bt�B�bB~�Bx�Bw�Bn�BYBF�B<jB$�B�B+B
��B
�B
�yB
��B
ÖB
�FB
��B
�uB
�PB
�+B
�B
y�B
t�B
gmB
q�B
z�B
iyB
cTB
ZB
ZB
ZB
ZB
ZB
\)B
[#B
M�B
G�B
B�B
<jB
8RB
0!B
�B

=B
B	��B	�B	�`B	ɺB	ɺB	ĜB	�wB	��B	��B	��B	�hB	�PB	�B	}�B	{�B	v�B	VB	I�B	7LB	)�B	{B	DB	1B	B��B��B�B�B�B�B�mB�fB�`B�/B��BB��B�qB�9B�'B��B��B��B��B�7B�B~�By�Bw�Bw�Bu�Bt�Bp�Bk�Bl�BiyBgmBdZB`BBbNB^5B^5B]/B]/BZBZBYBYBXBW
BW
BQ�BR�BR�BQ�BM�BK�BJ�BM�BM�BI�BD�BD�BB�BA�BB�BC�BB�BB�BA�BA�B>wB?}B:^B;dB<jB9XB6FB6FB6FB49B49B0!B49B9XB:^B5?B0!B0!B49B49B.B)�B)�B(�B(�B(�B(�B/B0!B2-B5?B<jB=qB>wB8RB:^BA�BD�BC�BB�B^5BZB}�B�\B��B��B�-B�9B��B��B�B	+B	B	%�B	H�B	I�B	;dB	1'B	8RB	D�B	G�B	VB	^5B	dZB	gmB	gmB	t�B	{�B	{�B	�%B	�hB	�oB	z�B	r�B	�B	�1B	��B	�B	�XB	�qB	�}B	�
B	�5B	�BB	�fB	�ZB	�NB	�TB	�mB	�mB	��B	��B
B
B
%B
%B
+B
DB
JB
bB
\B
bB
uB
�B
�B
�B
�B
!�B
"�B
%�B
&�B
'�B
(�B
)�B
+B
-B
/B
0!B
1'B
2-B
5?B
6FB
8RB
8RB
9XB
9XB
;dB
;dB
<jB
=qB
?}B
@�B
A�B
C�B
B�B
D�B
D�B
E�B
F�B
E�B
H�B
H�B
J�B
J�B
L�B
M�B
N�B
O�B
P�B
Q�B
P�B
R�B
S�B
T�B
VB
W
B
T�B
XB
XB
YB
ZB
ZB
YB
ZB
\)B
]/B
^5B
^5B
`BB
aHB
`BB
aHB
bNB
bNB
dZB
ffB
ffB
gmB
hsB
hsB
gmB
hsB
hsB
iyB
jB
k�B
l�B
n�B
n�B
q�B
q�B
r�B
s�B
t�B
v�B
v�B
v�B
x�B
y�B
z�B
|�B
|�B
}�B
|�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�+B
�DB
�DB
�PB
�VB
�\B
�bB
�hB
�oB
�{B
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
�B
�B
�B
�B
�'B
�3B
�9B
�9B
�FB
�FB
�FB
�FB
�RB
�RB
�XB
�^B
�dB
�dB
�dB
�dB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807291703252021061413522320210614135223202106141746302021061417463020210614174630201807291703252021061413522320210614135223202106141746302021061417463020210614174630PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072917032520180729170325  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072917032520180729170325QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072917032520180729170325QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015120210722160151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                