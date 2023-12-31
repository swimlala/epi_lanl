CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ɣ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ۘ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20180724220235  20210722160149  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�k�_1�Q@�k�_1�Q11  @�k�UUl�@�k�UUl�@7��@��@7��@���c�`,���c�`,��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?�  @   @@  @�  @�33@�33@�33A   A��A#33A@  Ac33A�  A���A���A���A���A�33A�  A�  B ffB��B  B��B   B(  B0  B8ffB?��BHffBPffBX  B`ffBh  Bp  Bw��B�33B�ffB�33B���B���B�33B�33B�33B�ffB�33B�  B�  B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�  B�  B���B�33B�  B�  B���B�B�  B�33B���C�C�C��C�C
  C  C�C�C  C33C�C��C�C�C  C �C!�fC#��C&  C(33C*  C+�fC.33C0�C1�fC433C6L�C8�C9�fC<33C>  C?�fCB�CD  CE�fCHL�CJ  CK�fCNL�CP�CR  CT33CV�CW�fCZ33C\33C^  C`33Cb�Cc�fCf�Ch  Ci�fCl33Cn33Cp  Cq�fCs��Cv�Cx�Cz�C|�C}�fC�fC��fC��C��C��C�&fC��C�  C��C��C��3C��C�&fC��C��3C��C��C�  C��fC��3C��C��C��C��fC��3C�  C��C��C�&fC�33C��C�  C��C��C�  C��fC��fC�  C��C�&fC��C��fC��3C�  C��C��C�&fC��C��3C�  C��C�&fC�&fC�  C��fC��3C�  C��C��C�  C��fC��C�&fC��C�  C��C��C�  C��fC��3C�  C��C�&fC��C��3C�  C��C��C�&fC�  C��fC��3C��3C��C��C��C��C��C�&fC��C��fC��3C��3C��3C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C��C��C��C��C�&fC��C��fC��3C�  C��C��C�&fC�  C��fC��3C�  C�&fC��C��3C��C�&fC�ٚC��3D fD l�D ��D��D��DS3D� D
l�D,�D�3Dl�D3D�3D�Dy�D�3D!l�D#� D&s3D(��D+�3D.,�D0�fD3l�D63D8� D;�3D>FfDAfDC� DFffDI  DK��DN&fDP�fDS33DU��DX  DZ��D\��D_ffDa� Dd&fDf��Di  DkffDm��Dp@ Dr��Du3Dw�fDy�3D|fD~l�D�i�D��3D��3D�3D�6fD�l�D��3D�ٚD� D�FfD�y�D��3D��D��D�I�D�� D��3D��D�)�D�i�D���D��D�  D�P D�� D�� D�� D�&fD�Y�D�� D��fD���D�33D�l�D���D���D��fD�3D�33D�P D�l�D���D��3D��3D�ٚD���D��D�0 D�L�D�\�D�l�D��3D��fD���D�� D��D�  D��D�<�D�VfD�vfDǙ�Dȹ�D�ٚD���D��D�<�D�\�DφfDг3D��3D�fD�@ D�i�D֓3D��3D���D��D�<�D�p Dݠ D��fD��3D�  D�FfD�p D� D幚D��3D�� D�	�D�  D�6fD�FfD�` D�vfDD�D��D�� D��3D��D���D�	�D�3D�&fD�33D�&fD�)�D�33D�6fD�<�E   E ��E#3E� E( E��E&fE�3E  E��EfE��E��E	 E
�E�fE��E( E1�E�3E�3E��Ex E��E� E!�E&fE�fE��E�fE^fEc3E �E!� E#l�E$d�E%�fE'A�E(8 E)� E*�3E+��E-VfE.��E/� E1	�E2h E3P E4�fE5��E7S3E8� E9�fE:� E<H E?|�EB� EEi�EH�fEK��EOfER( EU	�EXP E[q�E^�fEaٚEd�3Eg�fEk	�En@ Eq  EtK3Ew� Ez��E}��E�` E���E���E� �E��3E�H E��3E�P�E���E�k3E�3E�d�E���E��E�?3E���E�� E�0 E��fE�� E�  E�{3E��fE��E�q�E���E�3E�L�E��3E�fE�C3E�� E�� E�-�E��f?   >���?   ?   ?��?333?333?333?L��?fff?�  ?���?���?�ff?�  ?���?ٙ�?�33@ff@��@   @333@@  @S33@fff@s33@�33@�  @���@�33@���@�ff@�  @�  @���@�ff@�33A   AffAffA��A33A#33A)��A1��A8  A@  AH  ANffAVffA`  AfffAl��At��A{33A���A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414114411111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?�  ?�  @   @`  @�  @�33@�33@�33A  A��A+33AH  Ak33A�  A���A���A���A���A�33A�  A�  BffB
��B  B��B"  B*  B2  B:ffBA��BJffBRffBZ  BbffBj  Br  By��B�33B�ffB�33B���B���B�33B�33B�33B�ffB�33B�  B�  B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�  B�  B���B�33B�  B�  B���B���B�  B�33C ffC��C��CL�C��C
� C� C��C��C� C�3C��CL�C��C��C� C ��C"ffC$L�C&� C(�3C*� C,ffC.�3C0��C2ffC4�3C6��C8��C:ffC<�3C>� C@ffCB��CD� CFffCH��CJ� CLffCN��CP��CR� CT�3CV��CXffCZ�3C\�3C^� C`�3Cb��CdffCf��Ch� CjffCl�3Cn�3Cp� CrffCtL�Cv��Cx��Cz��C|��C~ffC�33C�&fC�Y�C�Y�C�L�C�ffC�Y�C�@ C�Y�C�Y�C�33C�L�C�ffC�Y�C�33C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�L�C�ffC�s3C�Y�C�@ C�L�C�Y�C�@ C�&fC�&fC�@ C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�ffC�Y�C�33C�@ C�Y�C�ffC�ffC�@ C�&fC�33C�@ C�Y�C�Y�C�@ C�&fC�L�C�ffC�Y�C�@ C�L�C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�Y�C�33C�@ C�L�C�Y�C�ffC�@ C�&fC�33C�33C�L�C�Y�C�Y�C�L�C�Y�C�ffC�L�C�&fC�33C�33C�33C�33C�@ C�@ C�@ C�33C�33C�33C�33C�@ C�@ C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�ffC�@ C�&fC�33C�@ C�ffC�L�C�33C�L�C�ffC��C�33D &fD ��D�D��D�Ds3D� D
��DL�D�3D��D33D�3D,�D��D3D!��D$  D&�3D)�D+�3D.L�D0�fD3��D633D9  D;�3D>ffDA&fDC� DF�fDI  DK��DNFfDP�fDSS3DUٚDX@ DZ��D]�D_�fDa� DdFfDf��Di  Dk�fDm��Dp` Dr��Du33Dw�fDz3D|&fD~��D�y�D��3D��3D�3D�FfD�|�D��3D��D�  D�VfD���D��3D���D�)�D�Y�D�� D��3D���D�9�D�y�D���D���D�0 D�` D�� D�� D�  D�6fD�i�D�� D��fD��D�C3D�|�D���D���D�fD�#3D�C3D�` D�|�D���D��3D��3D��D�	�D�)�D�@ D�\�D�l�D�|�D��3D��fD���D�� D���D� D�,�D�L�D�ffDƆfDǩ�D�ɚD��D�	�D�,�D�L�D�l�DϖfD��3D��3D�&fD�P D�y�D֣3D��3D���D�)�D�L�D܀ Dݰ D��fD�3D�0 D�VfD� D� D�ɚD��3D�  D��D�0 D�FfD�VfD�p D�fDD�D��D�� D��3D���D�	�D��D�#3D�6fD�C3D�6fD�9�D�C3D�FfD�L�E ( E ��E+3E� E0 E��E.fE�3E( E��E&fE��E��E	  E
�E�fE��E0 E9�E�3E�3E��E� E��E� E)�E.fE�fE��E�fEffEk3E �E!� E#t�E$l�E%�fE'I�E(@ E)� E*�3E,�E-^fE.��E/� E1�E2p E3X E4�fE6�E7[3E8� E9�fE:� E<P E?��EB� EEq�EH�fEK��EOfER0 EU�EXX E[y�E^�fEa�Ed�3Eg�fEk�EnH Eq( EtS3Ew� Ez��E}��E�d E� �E���E�$�E��3E�L E��3E�T�E� �E�o3E�3E�h�E���E��E�C3E���E�� E�4 E��fE�� E�$ E�3E��fE��E�u�E���E�3E�P�E��3E�
fE�G3E�� E�� E�1�E��fG�O�?fffG�O�?�  ?���G�O�G�O�?���?�ff?�33?�  ?���?ٙ�?�ff@   @ff@��@��@&ff@9��@@  @S33@`  @s33@�33@���@�33@�  @���@�33@���@�ff@�  @�  @���@�ffA��A  AffAffA��A#33A+33A1��A9��A@  AH  AP  AVffA^ffAh  AnffAt��A|��A���A���A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414114411111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ j@ �@ �@ {@ O@ "�@ )�@ 0x@ 6�@ >@ E�@ Q�@ `�@ m:@ {�@ �7@ ��@ ��@ ��@ �&@ ��@ �#@ �y@ ��@�@@�@,`@:�@F�@V@c�@p�@~�@��@��@��@��@��@��@�/@��@��@�@*@#�@0x@=q@K@Yn@g@t�@�d@��@��@�Y@�@ƨ@�O@�H@��@��@
�@�@%�@2�@?}@N�@\�@i!@x&@��@�h@�@�@��@�@׹@�@�@ �@J@�@)�@6�@D�@Q=@^5@m:@|?@��@��@�5@�-@�w@�*@܀@��@�@�@@
@-@:@F�@Wb@b�@o�@�W@��@��@��@��@��@є@�;@�@��@�@�@"�@/�@<�@Lu@Z@ff@s_@�W@�@��@�Y@�@�J@��@��@�L@��@
�@�@&�@33@B8@O�@[z@j@y�@�|@�@�@�r@��@�@�[@�`@�@ �@J@�@(�@7L@D�@S�@bN@n�@z�@�7@��@��@�!@��@��@��@�(@�q@	@	b@	�@	-@	;d@	I�@	V�@	bN@	p�@	�@	��@	��@	�A@	��@	��@	�7@	�;@	��@	�,@
v@
*@
$.@
1'@
=q@
K�@
Z@
ff@
r�@
�@
�\@
�a@
��@
��@
�J@
Ӡ@
��@
�L@
��@
=@�@$�@2�@A�@O�@]�@j@x�@�+@�u@�@�f@�@ȴ@�[@�@�Y@  @�@�@(G@5�@DD@Q�@`�@n�@|?@��@��@��@�-@��@�@�t@��@� @v@@[@+�@:@I�@V@bN@qS@�W@��@��@��@��@��@є@��@�@z�@��@@Wb@�a@�@+@n�@��@�e@7�@z�@�2@%@M$@�$@�#@#�@k�@�R@@K�@��@�H@)�@p�@��@�E@A�@�+@�@�@O�@�@�O@{@V@��@�t@�@]�@�m@�@$.@g@�M@��@#�@e	@��@��@)�@k.@�f@�@1�@t@�F@��@:�@|�@��@��@@�@�d@Ĝ@ �@ M$@ �i@ խ@!�@!X�@!��@!�t@"�@"`�@"�z@"�@#&�@#i!@#�Y@#�@@$/@$o�@$�r@$�4@%)�@%ff@%�(@%��@&�@&Yn@&��@&�C@'�@'K@'��@'��@'��@(7L@(r@(�f@(�@)'�@)b�@)��@)�/@*B@*V�@*��@*�C@+�@+M$@+�D@+ȴ@,%@,E�@,��@,ƨ@-1@-G�@-�+@-ƨ@.�@.F�@.�+@.�J@/�@/G�@/�|@/ƨ@0�@0E�@0�@0@1@1>@1z�@1��@1�Y@2-�@2g�@2��@2�;@3�@3S�@3��@3�c@4@4?}@4y�@4��@4�4@5&�@5`A@5�#@5�o@6@6;d@6s_@6��@6��@7B@7Q�@7��@7��@7�@8*S@8_�@8��@8�@9qS@9܀@:~K@:��@;��@;�E@<�A@=�@=��@>7�@>��@?X�@?�o@@=q@@��@AX@B%@By�@B��@C�@D]@D��@E{@E�^@F$/@F��@G\�@G��@H_�@H�W@I`B@I�@J��@J�Y@K�7@L�@L��@M�@M��@N7�@N��@O/@O@PUU@Q��@S�@T:�@U��@V��@X[z@Y��@Z�`@\K@]�@^�@`\�@a�#@b��@dH]@e�A@f�H@h;d@i�@j��@l:�@m��@n�4@pO1@q�I@r��@tK@u��@v��@xO1@y��@z�l@{7L@{m�@{�@{�@|>�@|��@|�&@}I@}X�@}��@}��@~'�@~\�@~�	@~��@2�@g@��@��@��@�B�@�h@���@���G�O�@ �G�O�@ j@ G�O�G�O�@ �@ v@ %@ �@ �@ 1@ �@ 
=@ 
�@ �@ �@ V@ b@ @ @ {@ �@ �@ �@ �@ �@  �@ "�@ $.@ &�@ (�@ ,`@ /@ 1'@ 3�@ 6�@ 9X@ <�@ ?}@ B8@ E�@ H]@ K�@ N�@ Q�@ UU@ X@ [z@ _�@ bN@ e	@ hs@ k.@ n�@ r@ t�@ x�@ z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A҅A�;dA�VA��`A�ĜAХ�AЍPAЅA�G�A��`Aϕ�A�C�A�bNA�|�A�\)A�K�A���A��/A�dZAͼjA͋DA�XA�  A�{A��A�z�A���AɼjA�t�A� �A���A��Aǰ!A��A�XAőhA�-A���A���A��
A��DA��A�O�A�^5A��A�33A�VA��A�x�A�M�A�A��jA��`A�p�A��-A�ZA��HA�O�A�A���A�`BA�"�A��yA��A�`BA�33A�VA���A�~�A��yA���A�n�A��`A�VA�z�A�ffA��uA�?}A��A��A��TA��9A���A�1A�XA��RA�dZA�+A��hA��/A��;A��A�/A��^A��!A�A�A��A�C�A���A�dZA��A�&�A�JA��wA�C�A��FA�+A�ĜA���A�`BA�1'A���A�E�A�jA���A�+A��A���A�{A�7LA�v�A�|�A�A��PA33A~9XA|  Ay|�Ax(�AuXAshsAqXAp�RAo�An�9Al~�Ak�^Aj�/AiXAg\)AeXAbE�A_�-A^bA]�A\��A\ȴA\��A\I�AY�AWXAV��AT�AR��AR-AQ�hAP-AO�AN��AN-AMS�AL1AK&�AJn�AJ1AI��AH1'AFA�AEp�AD��AC��AB�jAB5?AB �AAA@5?A?`BA>E�A=��A<r�A:��A8�!A8  A7�-A6bA4n�A3�TA3�A3�A3��A2�9A1ƨA0�/A0A/&�A.VA-��A,ZA*z�A*E�A)�mA(��A'��A%��A$jA$A#�A#"�A!�A �!A (�A�FA��A1'A  AK�A�A{A��A�DA�PA��A��A��A&�A�A��A�
A�AA~�A`BAp�A1'AG�A
�HA
^5AG�A�DAffA�#AG�A
��A
�A1A��A	
=A	��A�AI�A1'AhsA(�A��AK�A|�@���@��y@�x�@���@�Q�@�ƨ@�"�@�I�@�!@���@�@ݩ�@�~�@�Q�@ȼj@�dZ@�?}@��P@��@��^@�9X@�j@�S�@�dZ@�r�@���@���@�J@�l�@��j@��P@�`B@��/@�1'@��@��y@��j@���@��!@�M�@��\@�;d@�M�@�M�@�@�X@�1@�v�@��#@���@���@��
@���@�ȴ@�M�@���@��@�\)@��!@��@�?}@~��@|j@x�9@v�y@t�@s"�@q��@pb@n5?@l�@i�#@h��@g��@e�@d��@cƨ@b�H@b�\@a��@`�u@^ff@\��@Z�!@Y%@W
=@U�-@T9X@R��@QG�@P�@O;d@M`B@L(�@HQ�@Hr�@F@D1@CC�@BM�@AG�@@r�@>�y@=�h@<�@;33@:-@9�7@8�`@8r�@6�R@6@3�m@2�!@1G�@0 �@.�R@-?}@+ƨ@*�H@)�@(�`@(A�@';d@%�T@$�/@#o@"^5@!�#@!hs@ 1'@��@$�@`B@/@��@I�@o@n�@�^@X@Ĝ@ �@��@
=@@�h@�/@�D@��@�@^5@X@Ĝ@b@�R@5?@`B@�@��@ƨ@S�@
�!@	�@	X@�u@�w@\)@�+@E�@@?}@�j@1@ƨ@�@dZ@��@^5@�@&�@ �`?��w?�/?��H?���?�?�9X?��`?�w?��?�?��?�K�?�`B?�z�?��?�-?�%?���?��?ܬ?�(�?�dZ?�x�?�1'?׮?և+?�?}?Լj?��
?Ұ!?ѩ�?�&�?У�?�  ?�|�?�\)?�;d?θR?�V?�p�?���?̬?�j?�ƨ?�~�?ɺ^?���?�
=?�+?�?Õ�?���?���?�G�?��?�|�?��?���?�V?�I�?�I�?�1?�I�?�(�?��?��?�/?�O�?�p�?��h?�{?�v�?��?�\)?��;?�bN?���?���?�Ĝ?��`?��`?�%?�&�?�G�?�hs?��7?���?���?��?�J?�-?�M�?�M�?�n�?�M�?�n�?�-?�M�?�n�?\A҇+A҇+A҃A҃A҅A҃A�|�AҁA�~�A�z�A�`BA�M�A�  Aѥ�A�A�A�=qA� �A�
=A�A���A��A��yA��`A��;A��
A���AмjAд9Aа!AХ�AН�AГuAЋDAЉ7AЅAЃA�jA�A�A�-A���A���AϺ^Aϙ�A�jA�K�A�C�A�?}A�A�A�A�A�M�A�bNA�jA�p�A�v�A�|�AυA�|�A�jA�dZA�\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A҅A�;dA�VA��`A�ĜAХ�AЍPAЅA�G�A��`Aϕ�A�C�A�bNA�|�A�\)A�K�A���A��/A�dZAͼjA͋DA�XA�  A�{A��A�z�A���AɼjA�t�A� �A���A��Aǰ!A��A�XAőhA�-A���A���A��
A��DA��A�O�A�^5A��A�33A�VA��A�x�A�M�A�A��jA��`A�p�A��-A�ZA��HA�O�A�A���A�`BA�"�A��yA��A�`BA�33A�VA���A�~�A��yA���A�n�A��`A�VA�z�A�ffA��uA�?}A��A��A��TA��9A���A�1A�XA��RA�dZA�+A��hA��/A��;A��A�/A��^A��!A�A�A��A�C�A���A�dZA��A�&�A�JA��wA�C�A��FA�+A�ĜA���A�`BA�1'A���A�E�A�jA���A�+A��A���A�{A�7LA�v�A�|�A�A��PA33A~9XA|  Ay|�Ax(�AuXAshsAqXAp�RAo�An�9Al~�Ak�^Aj�/AiXAg\)AeXAbE�A_�-A^bA]�A\��A\ȴA\��A\I�AY�AWXAV��AT�AR��AR-AQ�hAP-AO�AN��AN-AMS�AL1AK&�AJn�AJ1AI��AH1'AFA�AEp�AD��AC��AB�jAB5?AB �AAA@5?A?`BA>E�A=��A<r�A:��A8�!A8  A7�-A6bA4n�A3�TA3�A3�A3��A2�9A1ƨA0�/A0A/&�A.VA-��A,ZA*z�A*E�A)�mA(��A'��A%��A$jA$A#�A#"�A!�A �!A (�A�FA��A1'A  AK�A�A{A��A�DA�PA��A��A��A&�A�A��A�
A�AA~�A`BAp�A1'AG�A
�HA
^5AG�A�DAffA�#AG�A
��A
�A1A��A	
=A	��A�AI�A1'AhsA(�A��AK�A|�@���@��y@�x�@���@�Q�@�ƨ@�"�@�I�@�!@���@�@ݩ�@�~�@�Q�@ȼj@�dZ@�?}@��P@��@��^@�9X@�j@�S�@�dZ@�r�@���@���@�J@�l�@��j@��P@�`B@��/@�1'@��@��y@��j@���@��!@�M�@��\@�;d@�M�@�M�@�@�X@�1@�v�@��#@���@���@��
@���@�ȴ@�M�@���@��@�\)@��!@��@�?}@~��@|j@x�9@v�y@t�@s"�@q��@pb@n5?@l�@i�#@h��@g��@e�@d��@cƨ@b�H@b�\@a��@`�u@^ff@\��@Z�!@Y%@W
=@U�-@T9X@R��@QG�@P�@O;d@M`B@L(�@HQ�@Hr�@F@D1@CC�@BM�@AG�@@r�@>�y@=�h@<�@;33@:-@9�7@8�`@8r�@6�R@6@3�m@2�!@1G�@0 �@.�R@-?}@+ƨ@*�H@)�@(�`@(A�@';d@%�T@$�/@#o@"^5@!�#@!hs@ 1'@��@$�@`B@/@��@I�@o@n�@�^@X@Ĝ@ �@��@
=@@�h@�/@�D@��@�@^5@X@Ĝ@b@�R@5?@`B@�@��@ƨ@S�@
�!@	�@	X@�u@�w@\)@�+@E�@@?}@�j@1@ƨ@�@dZ@��@^5@�@&�@ �`?��w?�/?��H?���?�?�9X?��`?�w?��?�?��?�K�?�`B?�z�?��?�-?�%?���?��?ܬ?�(�?�dZ?�x�?�1'?׮?և+?�?}?Լj?��
?Ұ!?ѩ�?�&�?У�?�  ?�|�?�\)?�;d?θR?�V?�p�?���?̬?�j?�ƨ?�~�?ɺ^?���?�
=?�+?�?Õ�?���?���?�G�?��?�|�?��?���?�V?�I�?�I�?�1?�I�?�(�?��?��?�/?�O�?�p�?��h?�{?�v�?��?�\)?��;?�bN?���?���?�Ĝ?��`?��`?�%?�&�?�G�?�hs?��7?���?���?��?�J?�-?�M�?�M�?�n�?�M�?�n�?�-?�M�?�n�?\A҇+A҇+A҃A҃A҅A҃A�|�AҁA�~�A�z�A�`BA�M�A�  Aѥ�A�A�A�=qA� �A�
=A�A���A��A��yA��`A��;A��
A���AмjAд9Aа!AХ�AН�AГuAЋDAЉ7AЅAЃA�jA�A�A�-A���A���AϺ^Aϙ�A�jA�K�A�C�A�?}A�A�A�A�A�M�A�bNA�jA�p�A�v�A�|�AυA�|�A�jA�dZA�\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�;B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B%BhBoBbB%�B+B33B33B49B1'B33B<jBE�BH�BG�BF�BE�BC�BH�BM�BT�BcTBjB�\B��B�BB�B+B2-B>wBK�BL�BQ�B]/B\)BffBjBjBk�BgmBhsBjBn�Bs�Bw�By�B�B�B� B�B�+B�7B�=B�DB�PB�hB�uB�{B��B��B�oB�oB�oB�hB�+B� Bv�Bn�BjBffB\)BP�BJ�BJ�B:^B.B$�B�B�B%B�sBƨB�LB�-B�'B�B��B��B�'B�RB�!B��B��B�{B�DB{�Bx�Bs�Br�BdZBT�BL�B9XBbB
�B
��B
�B
�VB
{�B
gmB
[#B
P�B
:^B
)�B
�B
	7B	��B	�B	�B	�ZB	�;B	�#B	�TB	�)B	��B	ɺB	�dB	��B	��B	��B	�oB	�bB	�bB	�\B	�JB	w�B	r�B	n�B	_;B	YB	T�B	O�B	G�B	F�B	C�B	@�B	;dB	49B	1'B	.B	+B	&�B	�B	�B	\B	
=B	%B	B	B	B	B	B	B��B��B��B�B�fB�NB�/B��BȴB��B��B��B��B��BȴBƨBB�jB�FB�'B��B��B��B��B��B�oB�B{�Bx�Bv�Br�Bo�Bp�Bp�Bm�Bm�Bm�Bk�BiyBcTBdZBdZBaHB\)BZBVBP�BO�BL�BK�BK�BS�BT�BQ�BP�BT�BW
BP�BO�BS�Bp�B�\B�{B��B��B��B�hB�7B�{B�B�FB��BɺBɺBŢBĜB�jB�!B��B��B��B��B��B��B��B��B��B�uB�bB�PBs�B`BBXBm�B`BBcTBcTB��B��B�BƨBǮB�B��B�mB��B	hB	�B	+B	5?B	>wB	2-B	A�B	1'B	M�B	K�B	K�B	W
B	`BB	x�B	�B	�JB	��B	��B	��B	�B	�?B	��B	ĜB	��B	��B	��B	�5B	�ZB	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B
	7B
DB
JB
VB
bB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
#�B
%�B
&�B
'�B
(�B
+B
.B
/B
/B
1'B
6FB
5?B
7LB
8RB
8RB
9XB
;dB
>wB
A�B
A�B
C�B
D�B
E�B
E�B
F�B
E�B
H�B
I�B
K�B
K�B
M�B
N�B
O�B
Q�B
R�B
S�B
S�B
VB
VB
W
B
XB
ZB
[#B
[#B
\)B
]/B
^5B
`BB
`BB
`BB
`BB
bNB
bNB
dZB
dZB
e`B
e`B
ffB
gmB
hsB
iyB
jB
jB
jB
k�B
k�B
l�B
m�B
n�B
o�B
o�B
q�B
p�B
r�B
s�B
t�B
t�B
t�B
v�B
w�B
w�B
w�B
x�B
x�B
z�B
z�B
{�B
{�B
|�B
}�B
|�B
}�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�1B
�1B
�7B
�=B
�DB
�JB
�PB
�VB
�bB
�hB
�hB
�hB
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
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�3B
�9B
�FB
�FB
�LB
�LB
�RB
�RB
�RB
�XB
�RB
�XB
�^B
�^B
�^B
�^B
�dB
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
�jB
�jB
�jB
�jB
�jB
�5B
�;B
�;B
�;B
�5B
�5B
�/B
�/B
�5B
�/B
�B
�
B
��B
ƨB
�
B
ǮB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
��B
��B
ǮB
��B
��B
��B
��B
�B
�B
�#B
�)B
�/B
�HB
�B
�B
��B
��BB
=BPBhBhBhG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B
�/B
�
B
��B
��B
��B
ɺB
��B
��B
ɺB
ȴB
��B
�B
�yBB\BhB\B$�B(�B2-B2-B33B0!B2-B;dBD�BG�BF�BE�BD�BB�BG�BL�BS�BbNBiyB�VB��B�BB�B)�B1'B=qBJ�BK�BP�B\)B[#Be`BiyBiyBjBffBgmBiyBm�Br�Bv�Bx�B� B� B~�B�B�%B�1B�7B�=B�JB�bB�oB�uB�{B��B�hB�hB�hB�bB�%B~�Bu�Bm�BiyBe`B[#BO�BI�BI�B9XB-B#�B�B�BB�mBŢB�FB�'B�!B��B��B��B�!B�LB�B��B��B�uB�=Bz�Bw�Br�Bq�BcTBS�BK�B8RB\B
�B
��B
��B
�PB
z�B
ffB
ZB
O�B
9XB
(�B
�B
1B	��B	�B	�B	�TB	�5B	�B	�NB	�#B	��B	ȴB	�^B	��B	��B	��B	�hB	�\B	�\B	�VB	�DB	v�B	q�B	m�B	^5B	XB	S�B	N�B	F�B	E�B	B�B	?}B	:^B	33B	0!B	-B	)�B	%�B	�B	{B	VB		7B	B	B	B	B	B	B	  B��B��B��B�B�`B�HB�)B��BǮB��B��B��BɺB��BǮBŢB��B�dB�?B�!B��B��B��B��B��B�hB�Bz�Bw�Bu�Bq�Bn�Bo�Bo�Bl�Bl�Bl�BjBhsBbNBcTBcTB`BB[#BYBT�BO�BN�BK�BJ�BJ�BR�BS�BP�BO�BS�BVBO�BN�BR�Bo�B�VB�uB��B��B��B�bB�1B�uB��B�?B��BȴBȴBĜBÖB�dB�B��B��B��B��B��B��B��B��B��B�oB�\B�JBr�B_;BW
Bl�B_;BbNBbNB�{B��B�BŢBƨB�
B��B�fB��B	bB	�B	)�B	49B	=qB	1'B	@�B	0!B	L�B	J�B	J�B	VB	_;B	w�B	�B	�DB	�{B	��B	��B	�B	�9B	��B	ÖB	ɺB	��B	��B	�/B	�ZB	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B
	7B
DB
JB
VB
bB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
#�B
%�B
&�B
'�B
(�B
+B
.B
/B
/B
1'B
6FB
5?B
7LB
8RB
8RB
9XB
;dB
>wB
A�B
A�B
C�B
D�B
E�B
E�B
F�B
E�B
H�B
I�B
K�B
K�B
M�B
N�B
O�B
Q�B
R�B
S�B
S�B
VB
VB
W
B
XB
ZB
[#B
[#B
\)B
]/B
^5B
`BB
`BB
`BB
`BB
bNB
bNB
dZB
dZB
e`B
e`B
ffB
gmB
hsB
iyB
jB
jB
k�B
l�B
l�B
m�B
n�B
o�B
p�B
p�B
r�B
q�B
s�B
t�B
u�B
u�B
u�B
w�B
x�B
x�B
x�B
y�B
y�B
{�B
{�B
|�B
|�B
}�B
~�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�%B
�7B
�7B
�=B
�DB
�JB
�PB
�VB
�\B
�hB
�oB
�oB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�!B
�-B
�-B
�3B
�9B
�FB
�FB
�LB
�XB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�dB
�jB
�qB
�qB
�qB
�qB
�}B
�wB
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
�}B
�}B
��B
��B
��B
��B
��B
��B
�)B
�/B
�/B
�/B
�)B
�)B
�#B
�#B
�)B
�#B
�B
��B
��B
ĜB
��B
ŢB
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
��B
ɺB
ɺB
ɺB
ɺB
��B
��B
��B
ɺB
ƨB
ɺB
ȴB
ŢB
��B
��B
��B
��B
��B
�B
�B
�B
�#B
�BB
�yB
�B
��B
��B  B1BDB\B\B\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202352021061413521620210614135216202106141746242021061417462420210614174624201807242202352021061413521620210614135216202106141746242021061417462420210614174624PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023520180724220235  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023520180724220235QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023520180724220235QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014920210722160149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                