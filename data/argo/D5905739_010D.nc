CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:46Z creation      
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
_FillValue                 0  at   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ք   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ڴ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180724220246  20210617131454  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               
   
DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�e�wx�@�e�wx�11  @�e�O�p@�e�O�p@6(�@6(��c�3� )��c�3� )�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@Fff@�  @�  @�  @�33A��A��A$��A>ffA`  A�  A�  A���A���A���A���A�  A���B ffB��B33B��B   B'��B0ffB8��B@ffBH  BO��BX  B`ffBg��Bp  BxffB�33B�  B�  B���B���B�33B���B�ffB�  B���B���B�  B�  B�  B�  B�ffB�ffB�33BǙ�B�  B�  B�  B���Bۙ�B�33B�33B�  B�ffB�33B�  B���B�  B���C�C�fC�3C��C
  C33CL�C�C�fC�C33C  C��C  C33C   C!�fC$33C&�C'��C*33C,  C-�fC0�C2�C4  C6  C7��C:�C<�C>  C@  CB�CD�CF  CG�fCI��CL�CN  CO�fCR  CS�fCVL�CXL�CZ33C\33C^33C`33Cb�Cd�Cf�Ch  Ci��Cl�Cn  Co��Cr  Ct33Cv�Cw�fCz�C|L�C~�C��C��3C��C��C��C��fC��3C�  C��C��C�  C��fC��3C�  C��C��C�  C��fC��3C�  C��C�&fC��C��3C�  C��C��3C��fC��3C��C�&fC��C�  C��C�  C��fC��C��3C��fC�  C�  C��fC��C��3C��fC��C�&fC��C�  C�&fC��C�  C�  C��3C��C��3C��fC�  C��C��C�  C�&fC��C�  C��C��C��C�  C��3C��C�  C��3C��C�  C��3C��C��C�  C�&fC��C��C��C��3C��3C��3C��C��C��C��C�  C�  C��3C��C��C�  C��3C��fC�  C��C��C�  C��3C��3C��C�  C�  C��3C��fC��C�  C��fC��C�  C��3C��C��C��3C��C�  C��fC��C��C��C��C��C�@ C��3C�  DfD�fD
��D9�D��D��Dl�D3D�fDffD 3D"�fD%y�D(9�D*��D-� D0@ D2ٚD5�fD8,�D:��D=l�D@fDB� DE3DG� DJ33DL� DOL�DQ�fDTy�DW  DY�3D\33D^ٚDal�DdfDf��Di9�DkٚDn` Dp��Ds� DvfDx�fDz��D}3D�fD���D�9�D�y�D���D�� D�33D�vfD���D���D�C3D�� D��3D��D�@ D�|�D���D�� D�&fD�c3D���D�� D�fD�6fD�i�D���D���D�  D�0 D�` D��3D�� D�� D�  D�\�D�� D�ɚD���D�33D�p D���D��3D�#3D�Y�D��3D���D�	�D�L�D���D���D�3D�@ D��3D�� D��fD�)�D�` DŖfD���D�  D�33D�ffD˓3D̼�D��3D�  D�I�D�p DҜ�D�� D��D� D�9�D�\�Dى�Dڹ�D�� D�	�D�,�D�L�D�y�DᙚD�� D��fD�	�D�,�D�L�D�p D�fD깚D�� D���D� D�)�D�FfD�VfD�p D�3D��3D��fD��3D��3D�� D�� D��3D��fD�� D��D�� D��E {3E  E�3E E�3EfE�3EfE��E!�E��E+3E� E� E	NfE
Q�EX E�fE��EvfE~fE�E�EfE��E�fE#3E�E� E�fE E E��E �3E!��E#` E$S3E%� E'fE(	�E)ffE*�3E,1�E-$�E.��E/�3E0��E2H E3�fE4�3E5�fE7@ E8��E9� E;6fE<fE?$�EB��EE�fEH� EK�fEN� ER9�EU�EXx E[{3E^h Ea��Ed��Eh�Ej��EnI�EqI�EtS3Ew�fEz��E}��E���E��E�� E�)�E�� E�0�E�� E�T�E��E���E� �E�h�E���E��fE�X�E�� E�ݚE�>fE�3E��fE��E�zfE��3E�fE�t E���E� E�e�E���E���E�P E��3E��fE�9�E��fE�� E� E�p�E��3E� >���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���?   >���>���?   ?��?��?L��?fff?�  ?���?�ff?�  ?�ff@   @��@��@333@@  @Y��@l��@y��@���@�ff@���@���@���@�ff@�ff@�  @�  @���A��AffA��A��A$��A+33A4��A;33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414444414414414441411141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?L��?�  @&ff@fff@�  @�  @�  @�33A	��A��A,��AFffAh  A�  A�  A���A���Ař�A���A�  A���BffB
��B33B��B"  B)��B2ffB:��BBffBJ  BQ��BZ  BbffBi��Br  BzffB�33B�  B�  B���B���B�33B���B�ffB�  B���B���B�  B�  B�  B�  B�ffB�ffB�33Bș�B�  B�  B�  B���Bܙ�B�33B�33B�  B�ffB�33B�  B���B�  C ffC��CffC33CL�C
� C�3C��C��CffC��C�3C� CL�C� C�3C � C"ffC$�3C&��C(L�C*�3C,� C.ffC0��C2��C4� C6� C8L�C:��C<��C>� C@� CB��CD��CF� CHffCJL�CL��CN� CPffCR� CTffCV��CX��CZ�3C\�3C^�3C`�3Cb��Cd��Cf��Ch� CjL�Cl��Cn� CpL�Cr� Ct�3Cv��CxffCz��C|��C~��C�&fC�33C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�L�C�33C�@ C�Y�C�33C�&fC�33C�L�C�ffC�L�C�@ C�L�C�@ C�&fC�L�C�33C�&fC�@ C�@ C�&fC�L�C�33C�&fC�L�C�ffC�L�C�@ C�ffC�Y�C�@ C�@ C�33C�L�C�33C�&fC�@ C�Y�C�Y�C�@ C�ffC�L�C�@ C�Y�C�Y�C�L�C�@ C�33C�Y�C�@ C�33C�L�C�@ C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�L�C�33C�33C�33C�Y�C�Y�C�L�C�L�C�@ C�@ C�33C�L�C�L�C�@ C�33C�&fC�@ C�Y�C�L�C�@ C�33C�33C�L�C�@ C�@ C�33C�&fC�L�C�@ C�&fC�L�C�@ C�33C�L�C�L�C�33C�L�C�@ C�&fC�L�C�Y�C�L�C�L�C�Y�C�� C�33C�@ D&fD�fD
��DY�D�D��D��D33D�fD�fD 33D"�fD%��D(Y�D+�D-� D0` D2��D5�fD8L�D:��D=��D@&fDB� DE33DG� DJS3DL� DOl�DRfDT��DW  DY�3D\S3D^��Da��Dd&fDf��DiY�Dk��Dn� Dq�Ds� Dv&fDx�fD{�D}33D�fD��D�I�D���D���D�  D�C3D��fD�ɚD��D�S3D�� D��3D��D�P D���D�ɚD�  D�6fD�s3D���D�� D�fD�FfD�y�D���D���D� D�@ D�p D��3D�� D�  D�0 D�l�D�� D�ٚD��D�C3D�� D���D��3D�33D�i�D��3D���D��D�\�D���D���D�3D�P D��3D�� D�fD�9�D�p DŦfD���D� D�C3D�vfDˣ3D���D�3D�0 D�Y�Dр DҬ�D�� D���D�  D�I�D�l�Dٙ�D�ɚD�� D��D�<�D�\�D���DᩚD�� D��fD��D�<�D�\�D� D�fD�ɚD�� D�	�D�  D�9�D�VfD�ffD� D�3D��3D��fD��3D��3D�� D�� D��3D��fD�� D���D�  D���E �3E E�3E E�3EfE�3EfE��E)�E��E33E� E� E	VfE
Y�E` E�fE��E~fE�fE�E�E&fE��E�fE+3E$�E� E�fE E E��E!3E!��E#h E$[3E%� E'&fE(�E)nfE*�3E,9�E-,�E.��E/�3E0��E2P E3�fE4�3E5�fE7H E8��E9� E;>fE<fE?,�EB��EE�fEH� EK�fEO  ERA�EU	�EX� E[�3E^p Ea��Ed��Eh!�Ej��EnQ�EqQ�Et[3Ew�fEz��E}��E���E��E�� E�-�E�� E�4�E�� E�X�E���E���E��E�l�E���E��fE�\�E�� E��E�BfE��3E��fE� �E�~fE��3E�fE�x E���E� E�i�E���E���E�T E��3E��fE�=�E��fE�� E�  E�t�E��3E�  G�O�?333G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�?333G�O�G�O�G�O�?L��G�O�?L��?fff?�  G�O�?���?�ff?�33?�  ?���?�ff@   @33@   @,��@9��@S33@`  @y��@�ff@���@���@�ff@���@���@ə�@�ff@�ff@�  A   A��A��AffA��A$��A,��A333A<��AC33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414444414414414441411141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ @ �@ V@ *@ O@ ""@ (�@ 0x@ 7L@ >@ FQ@ Q=@ _�@ m:@ z�@ �7@ ��@ �5@ �-@ �&@ �|@ �#@ �y@ ��@�@@
@-@;d@H]@UU@bN@p�@~�@�D@��@��@��@@�7@�/@��@��@�@�@""@/@<�@K@X�@ff@t@�@��@��@�M@�R@��@Ӡ@��@�@�E@
�@�@&�@3�@@�@P�@\)@i!@x&@�p@��@�@�@�@�o@׹@�@�@^@�@�@(�@7�@DD@Q=@`�@m�@y�@��@�0@�(@�-@��@��@�t@�@�q@@@�@-@:�@G�@T�@a�@qS@~K@�D@��@��@��@Ĝ@є@�;@��@��@�@*@"�@/�@<@K�@X�@e	@t@�@�@�U@�Y@�^@ƨ@�C@��@�@��@
�@�@$�@33@A�@O�@\)@hs@v�@�@�u@��@�@�^@ȴ@�
@�`@�e@ �@�@O@*S@5�@B�@Q=@`B@oF@{�@��@��@��@�!@��@�@�@�@��@	@	�@	
@	+@	:�@	I�@	V@	b�@	r�@	�@	��@	��@	��@	��@	��@	��@	��@	��@	��@
�@
�@
"�@
/�@
>�@
Lu@
Yn@
ff@
s_@
�@
�\@
�U@
�Y@
�R@
�J@
��@
��@
��@
��@�@�@&;@2�@@,@M�@]�@k.@x&@��@��@�m@�f@�k@�@�
@�@��@  @@�@(�@5�@C�@R�@_�@m:@z3@�+@��@��@�!@��@��@��@��@�q@�@�@�@+@:�@I@V@c�@r@��@�D@��@0x@{�@�W@b@Z@�5@�L@8�@�d@�@@\�@��@�@;d@�@��@�@\�@�5@��@4�@{�@�&@v@K@�h@�
@�@c�@��@��@5?@|�@�J@�@R�@��@��@(G@m:@��@�,@>@�d@�J@��@A�@�p@�@J@M�@��@׹@�@a�@��@�4@33@x&@�@��@@,@��@��@ 1@ K�@ ��@ �7@!o@!SI@!��@!�\@"6@"X�@"��@"�t@#�@#\)@#�@#��@$!s@$b�@$��@$�m@%)�@%m:@%��@%�@&7L@&y�@&�j@&�Q@'B�@'��@'�@(b@(R�@(�0@(�#@)�@)`�@)�z@)�@*&�@*i!@*��@*�4@+-�@+m�@+�f@+�@,/�@,oF@,�@,�@@-,`@-k�@-��@-�(@.(G@.hs@.�M@.�@/'�@/e�@/�(@/�T@0 �@0_�@0�a@0܀@1�@1X@1�0@1��@2@2Q�@2��@2�c@3v@3B8@3|?@3�R@3�@4-@4g�@4�@4�#@5{@5N�@5�d@5��@5�Y@6+@6b�@6�<@6є@7
=@7B8@7z�@7��@7��@8#�@8[z@8��@8�|@9v@9>�@9ww@9�@:��@;@;t@<	@<��@=5�@=��@>R�@>�2@?2�@?խ@@C�@@�y@AS�@A��@BbN@Cj@Cp�@D�@D�r@E�@E��@F�@F��@GM�@G�-@HF�@H�;@Ix&@I��@Jx&@Ko@Ky�@L@L��@M1@M�T@N/�@N��@OQ�@O��@P<�@Q��@SJ@T[z@U�A@V�l@XN�@Y�-@Z��@\\)@]�4@^�@`FQ@a�z@c�@d?}@e�Z@f�@h>�@i��@j��@lS�@m��@n��@pDD@q��@r��@t7L@u�m@v�`@xE�@y��@z��@{:�@{t�@{� @|�@|@�@|x�@|�o@}�@}S�@}�7@}�@~(G@~^5@~�@~��@/�@|?@�r@��@�""@�;d@�`�@���@���@�΂@��f@�
�@�/r@�S�G�O�@ ^G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�@ ^G�O�G�O�G�O�@ G�O�@ @ �@ jG�O�@ @ v@ %@ �@ �@ �@ 
=@ J@ �@ @ b@ @ {@ 6@ B@ �@ [@  @ !s@ $�@ '�@ *S@ -�@ /�@ 33@ 5?@ 8�@ <�@ ?}@ B�@ FQ@ I@ M$@ O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A���A���A�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A��
A���A���AǸRA�7LA�jA��A��A��A�VAź^Aŉ7AŇ+AŁA�r�A�+A\A��A�l�A���A���A�?}A��A�A��+A�`BA��A�JA�~�A�1A���A�-A�\)A��uA�?}A��FA��-A��A�ƨA���A��DA��A�r�A�S�A�9XA�{A��TA�ĜA�l�A�bNA�ZA�JA��yA�hsA��A�&�A�`BA���A���A��uA��hA�^5A���A���A���A�VA��`A���A���A���A���A�1A��A��A��HA���A���A���A��A�"�A�ĜA��A�|�A�z�A��-A���A��A��\A�ƨA���A��
A�=qA�M�A���A�JA�Q�A�bA�n�A��HA�  A�E�A���A��A���A�hA|�Az~�AyK�Aw�TAw`BAs�;As"�AqdZAop�An�Am
=AjAh�9Agl�Ae��Ae;dAdv�Ac��Ab^5A`A�A^��A]��A]33A]VA\�A\�RAZVAXA�AW
=AU��AQK�AO+AN �ALĜAK�-AKK�AH�AG�7AFI�AD�`AD�!AD �AC��AC�AAl�A@�A?�
A?K�A>VA;��A9C�A7�A6��A5A3��A2ZA1��A0�yA.�jA-G�A+��A*5?A)7LA(��A'x�A&M�A%��A%��A%;dA$��A#S�A"��A!�TA  �A|�A
=A�DA(�AdZA7LA �A1A��A��A��A�-A��AI�A
=A��A�RA �AC�A�A{A��A�wA33AĜA�;A
�A
ZA	�A	&�A��A�\A�FA�HA��A^5AG�A��AA�AXA I�@�@��@��!@�$�@�Z@�1'@�ff@�Q�@�M�@�x�@�w@�D@��j@�"�@�w@�+@홚@���@�=q@�D@柾@�o@��@�b@��
@պ^@�X@�J@�@�|�@��T@��@�n�@�j@�o@�dZ@�V@��H@��7@��y@��7@��@�o@��@�(�@��!@��y@�~�@�p�@�7L@��m@�o@�V@�Ĝ@�l�@�^5@���@��9@��;@�@��@��@�z�@�dZ@���@�b@�o@��!@���@���@|�@}V@{"�@zJ@y&�@w��@u?}@sC�@qG�@n5?@l�@j�@j~�@i��@g�w@ep�@c�m@bM�@a�^@`��@_�@^ff@\Z@Y�^@X��@W�@W;d@Vff@Up�@S�
@R��@PA�@N�@M`B@L��@K��@I�@HQ�@F@E`B@D1@Ct�@A��@A�@?�@=�T@<��@;t�@:J@8��@7�@6��@6E�@5�h@49X@2~�@0��@/��@/;d@.v�@-�T@,�j@+�
@+t�@*��@)�^@)�@(��@(1'@&�@&{@%�h@#t�@#o@!��@!�@ A�@\)@{@O�@�D@�F@@-@%@�w@��@@�@�
@�H@^5@�#@��@r�@\)@�@ȴ@�-@�@�@9X@
��@
=q@	G�@r�@��@�y@v�@{@O�@�D@�@��@�@n�@�#@ �u@ 1'?��?�{?��-?�(�?�?�^5?���?�l�?�Z?�\?�w?�V?���?�C�?�7L?�1'?�K�?�?��?���?�M�?�bN?�|�?�p�?ܬ?ۥ�?ڟ�?�x�?�1'?׍P?և+?�z�?ӶF?�J?�Ĝ?��?Η�?͑h?̬?˅?�~�?�^5?���?�x�?ȓu?�b?�K�?�ȴ?�ff?�?}?�Z?�t�?�n�?��?�|�?��R?�{?�p�?���?�(�?�dZ?��H?��?��?�^5?�=q?�^5?�~�?�~�?���?�?�dZ?�ƨ?�j?��?�O�?��?�v�?��?���?�A�?�bN?��?��?���?�Ĝ?�Ĝ?�%?�&�?�G�?�hs?���?���?���?���?���?���?��?��?��?�J?�J?��?���?���?���?��7?���?���?�JA�A�A�A�A���AǾwA���A���A�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA���A�A�A���A�A���A���A�AǾwA�A�AǾwA���A�A�AǾwA�ĜA�A���AǾwAǾwA�A�ĜA�ƨA�ĜA�ƨA�ƨA�ȴA�ƨA�ƨA�ƨA���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�A���A���A�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A��
A���A���AǸRA�7LA�jA��A��A��A�VAź^Aŉ7AŇ+AŁA�r�A�+A\A��A�l�A���A���A�?}A��A�A��+A�`BA��A�JA�~�A�1A���A�-A�\)A��uA�?}A��FA��-A��A�ƨA���A��DA��A�r�A�S�A�9XA�{A��TA�ĜA�l�A�bNA�ZA�JA��yA�hsA��A�&�A�`BA���A���A��uA��hA�^5A���A���A���A�VA��`A���A���A���A���A�1A��A��A��HA���A���A���A��A�"�A�ĜA��A�|�A�z�A��-A���A��A��\A�ƨA���A��
A�=qA�M�A���A�JA�Q�A�bA�n�A��HA�  A�E�A���A��A���A�hA|�Az~�AyK�Aw�TAw`BAs�;As"�AqdZAop�An�Am
=AjAh�9Agl�Ae��Ae;dAdv�Ac��Ab^5A`A�A^��A]��A]33A]VA\�A\�RAZVAXA�AW
=AU��AQK�AO+AN �ALĜAK�-AKK�AH�AG�7AFI�AD�`AD�!AD �AC��AC�AAl�A@�A?�
A?K�A>VA;��A9C�A7�A6��A5A3��A2ZA1��A0�yA.�jA-G�A+��A*5?A)7LA(��A'x�A&M�A%��A%��A%;dA$��A#S�A"��A!�TA  �A|�A
=A�DA(�AdZA7LA �A1A��A��A��A�-A��AI�A
=A��A�RA �AC�A�A{A��A�wA33AĜA�;A
�A
ZA	�A	&�A��A�\A�FA�HA��A^5AG�A��AA�AXA I�@�@��@��!@�$�@�Z@�1'@�ff@�Q�@�M�@�x�@�w@�D@��j@�"�@�w@�+@홚@���@�=q@�D@柾@�o@��@�b@��
@պ^@�X@�J@�@�|�@��T@��@�n�@�j@�o@�dZ@�V@��H@��7@��y@��7@��@�o@��@�(�@��!@��y@�~�@�p�@�7L@��m@�o@�V@�Ĝ@�l�@�^5@���@��9@��;@�@��@��@�z�@�dZ@���@�b@�o@��!@���@���@|�@}V@{"�@zJ@y&�@w��@u?}@sC�@qG�@n5?@l�@j�@j~�@i��@g�w@ep�@c�m@bM�@a�^@`��@_�@^ff@\Z@Y�^@X��@W�@W;d@Vff@Up�@S�
@R��@PA�@N�@M`B@L��@K��@I�@HQ�@F@E`B@D1@Ct�@A��@A�@?�@=�T@<��@;t�@:J@8��@7�@6��@6E�@5�h@49X@2~�@0��@/��@/;d@.v�@-�T@,�j@+�
@+t�@*��@)�^@)�@(��@(1'@&�@&{@%�h@#t�@#o@!��@!�@ A�@\)@{@O�@�D@�F@@-@%@�w@��@@�@�
@�H@^5@�#@��@r�@\)@�@ȴ@�-@�@�@9X@
��@
=q@	G�@r�@��@�y@v�@{@O�@�D@�@��@�@n�@�#@ �u@ 1'?��?�{?��-?�(�?�?�^5?���?�l�?�Z?�\?�w?�V?���?�C�?�7L?�1'?�K�?�?��?���?�M�?�bN?�|�?�p�?ܬ?ۥ�?ڟ�?�x�?�1'?׍P?և+?�z�?ӶF?�J?�Ĝ?��?Η�?͑h?̬?˅?�~�?�^5?���?�x�?ȓu?�b?�K�?�ȴ?�ff?�?}?�Z?�t�?�n�?��?�|�?��R?�{?�p�?���?�(�?�dZ?��H?��?��?�^5?�=q?�^5?�~�?�~�?���?�?�dZ?�ƨ?�j?��?�O�?��?�v�?��?���?�A�?�bN?��?��?���?�Ĝ?�Ĝ?�%?�&�?�G�?�hs?���?���?���?���?���?���?��?��?��?�J?�J?��?���?���?���?��7?���?���?�JA�A�A�A�A���AǾwA���A���A�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA���A�A�A���A�A���A���A�AǾwA�A�AǾwA���A�A�AǾwA�ĜA�A���AǾwAǾwA�A�ĜA�ƨA�ĜA�ƨA�ƨA�ȴA�ƨA�ƨA�ƨA���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�BDBoBhBhBoB�B�B�B�B!�B'�B�B�fB)�B0!B/B1'B33B@�BK�BL�BVB`BBdZBr�B�B�VB}�Bn�Bz�B�DB��B�bB�B� B{�Bv�B~�B~�B�%B��B��B��B��B��B��B��B��B��B�!B�B��B��B��B��B�hB�JB�+By�Bs�Bp�Bm�BffBYBO�BE�B2-B0!B.B2-B7LB�B�B!�B�B�BbB
=B�ZB��B�B�}B��B��B~�B^5BH�B#�BuB	7B
�fB
�#B
��B
��B
�wB
�FB
�B
��B
�B
jB
K�B
=qB
0!B
&�B
�B
+B
B	�B	�B	�ZB	��B	ĜB	�dB	�3B	�B	��B	��B	��B	��B	��B	�hB	�DB	�1B	�%B	�B	�B	t�B	jB	ffB	XB	B�B	7LB	0!B	�B	�B	bB	B��B�B�`B�fB��B��B�B�ZB�TB�BB�B��B��B�wB�^B�3B�3B�B�B��B��B��B�{B�+B~�B� B{�Bw�Bu�Bt�Bq�Bo�Bo�Bk�Bl�BhsBcTBe`BffBgmBffBr�Bs�Bs�Bu�Bu�Bt�Bp�Bp�Bm�BgmBaHBK�BJ�BH�BF�BI�BG�BH�BG�BH�BG�BE�BC�BE�BD�BC�B>wB:^B8RB7LB5?B33B1'B5?B1'B5?B33B;dBC�BD�BF�B<jB49B33B/B1'B;dBW
BdZBhsBdZB`BB]/BYBVBL�BG�BC�B6FB0!B1'B0!B8RB;dBI�BT�B^5BhsBk�B}�B�\B��BĜB�
B�)B	%B	�B	�B	0!B	A�B	F�B	XB	gmB	w�B	�B	�=B	�PB	��B	��B	��B	�B	�?B	�^B	��B	ƨB	ɺB	��B	��B	��B	�#B	�BB	�ZB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
%B
1B

=B
VB
bB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
$�B
&�B
&�B
'�B
)�B
+B
,B
.B
/B
1'B
1'B
2-B
49B
6FB
8RB
8RB
:^B
:^B
<jB
=qB
>wB
?}B
A�B
A�B
C�B
D�B
D�B
F�B
E�B
G�B
I�B
J�B
J�B
L�B
L�B
M�B
N�B
P�B
P�B
P�B
R�B
S�B
S�B
T�B
T�B
W
B
W
B
XB
ZB
ZB
[#B
\)B
\)B
]/B
_;B
`BB
`BB
aHB
bNB
bNB
cTB
e`B
e`B
gmB
gmB
hsB
hsB
iyB
k�B
jB
k�B
l�B
m�B
m�B
m�B
n�B
n�B
o�B
r�B
r�B
r�B
t�B
u�B
u�B
v�B
v�B
w�B
x�B
y�B
y�B
z�B
z�B
{�B
}�B
|�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�7B
�=B
�DB
�JB
�VB
�VB
�\B
�bB
�hB
�bB
�oB
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
�B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�3B
�9B
�?B
�?B
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�^B
�XB
�^B
�^B
�dB
�^B
�^B
�dB
�^B
�dB
�^B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B
ΰB
αB
αB
αB
αB
ͬB
βB
βB
ͭB
γB
ͭB
δB
δB
εB
ζB
ζB
ϽB
��B
θB
��B
�B%BPBJBKBRB~B�B�B�B!�B'�B��B�MB)�B0	B/B1B3B@mBK�BL�BU�B`.BdGBr�B� B�DB}�Bn�Bz�B�5B�~B�TB�B�B{�Bv�B~�B~�B�B��B��B��B��B��B��B��B��B��B�B�B��B��B��B�B�fB�IB�*By�Bs�Bp�Bm�BfhBYBO�BE�B21B0%B.B22B7RB�B�B!�B�B�BkB
FB�dB�B�B��B�B��BB^BBH�B#�B�B	EB
�tB
�2B
��B
��B
��B
�WB
�B
��B
�$B
j�B
K�B
=�B
05B
&�B
�B
@B
(B	�B	�B	�qB	�B	ĴB	�|B	�LB	�-B	�B	�B	��B	��B	��B	��B	�`B	�NB	�BB	�=B	�+B	t�B	j�B	f�B	X0B	B�B	7mB	0BB	�B	�B	�B	)B��B�B�B�B��B��B��B�B�|B�jB�FB�B��B��B��B�^B�^B�@B�.B�)B�)B��B��B�ZB)B�/B|Bw�Bu�Bt�Bq�Bo�Bo�Bk�Bl�Bh�Bc�Be�Bf�Bg�Bf�Br�Bs�Bs�Bu�Bu�Bt�Bp�Bp�Bm�Bg�Ba�BLBJ�BH�BF�BI�BG�BH�BG�BH�BG�BE�BC�BE�BD�BC�B>�B:�B8�B7�B5�B3zB1oB5�B1pB5�B3}B;�BC�BD�BF�B<�B4�B3�B/iB1uB;�BWZBd�Bh�Bd�B`�B]�BYjBVWBM!BHBC�B6�B0wB1}B0xB8�B;�BJBUdB^�Bh�Bk�B~eB��B�TB�B׈BܪB	�B	9B	 HB	0�B	BB	G<B	X�B	hB	xlB	��B	��B	��B	�CB	�qB	��B	��B	��B	�B	�?B	�gB	�|B	̌B	ѭB	��B	��B	�B	�.B	�PB	�_B	�uB	�~B	��B	��B	��B	��B	��B	��B
�B
B
	+B
:B
VB
eB
uB
xB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
'B
&	B
(B
(B
)%B
+4B
,=B
-FB
/TB
0^B
2mB
2pB
3yB
5�B
7�B
9�B
9�B
;�B
;�B
=�B
>�B
?�B
@�B
B�B
B�B
EB
FB
FB
H#B
G B
I/B
K>B
LHB
LJB
NYB
N\B
OeB
PnB
R}B
R�B
R�B
T�B
U�B
U�B
V�B
V�B
X�B
X�B
Y�B
[�B
[�B
\�B
]�B
]�B
^�B
aB
bB
bB
cB
d!B
d#B
e,B
g;B
g>B
iMB
iPB
jYB
j\B
kdB
msB
lpB
mxB
n�B
o�B
o�B
o�B
p�B
p�B
q�B
t�B
t�B
t�B
v�B
w�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
}B
}
B
~B
�!B
B
�&B
�/B
�2B
�4B
�=B
�EB
�PB
�^B
�pB
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
�B
�B
�-B
�+B
�?B
�LB
�PB
�]B
�bB
�vB
��B
��B
��B
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
�B
�!B
�'B
�,B
�HB
�_B
�tB
��B
��B
��B
��B
��B
��B
�	B
�B
�-B
�CB
�YB
�gB
�~B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�"B
�7B
�GB
�VB
�`B
�uB
�~B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ΰB
ΰB
ΰB
ΰB
ͪB
ΰB
ͪB
ΰB
ͪB
ΰB
ͪB
ͪB
ΰB
ͪB
ΰB
ΰB
ͪB
ΰB
ͪB
ΰB
ͪB
ΰB
ΰB
ͪB
ͪB
ΰB
ΰB
ΰB
ͪB
αB
ͫB
ͫB
αB
ͫB
ϷB
αB
αB
ͫB
αB
αB
αB
ͫB
βB
ͬB
βB
ͬB
βB
ͬB
βB
βB
βB
ͭB
ͭB
ͭB
γB
γB
ͭB
ͭB
δB
δG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202462021061413551720210614135517202106171311502021061713115020210617131150201807242202462021061413551720210614135517202106171311502021061713115020210617131150PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024620180724220246  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024620180724220246QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024620180724220246QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145420210617131454IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                