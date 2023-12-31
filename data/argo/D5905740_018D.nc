CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-19T17:02:21Z creation      
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
resolution        =���   axis      Z        8  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  dd   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
�Argo profile    3.1 1.2 19500101000000  20180919170221  20210722161420  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�{	N�6@�{	N�611  @�{	�_p@�{	�_p@)���T��@)���T���cF�V���cF�V��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?���@   @Fff@�33@�  @�  @�ffA33A��A(  AA��A^ffA���A�ffA���A�  A�33A�33A�33A�  B   B  B  B��B ��B(ffB0ffB8ffB@  BH  BP  BXffB`ffBh  BpffBx  B�  B���B�  B���B�33B�  B���B�ffB�33B�  B�33B�33B�  B�  B�  B�33B�33B���B�ffB�33B���B�ffB�33B���Bߙ�B�33B�  B왚B�ffB�33B���B���B�ffC  CffCL�C33C
�C�fC�fC�CffC33C�fC  C�C�C33C �C!�fC#��C&  C(�C*�C,�C.�C0  C2  C4  C6  C8  C:  C<  C>�C@33CB33CDL�CF�CH  CJ�CL  CM�fCP33CR�CS�fCU�fCW�fCZ33C\L�C^�C`  Ca�fCd33CfL�ChL�Cj�Cl33CnL�Cp�Cr  CtL�Cv  Cx  CzL�C|33C~�C�  C��fC��C��C�  C��3C�  C�&fC��C��C��C��C��C��C��C�  C��fC��fC�  C��C�&fC��C�  C�&fC��C�  C��C��C��C��C��C��C��C��C��3C��C��C�  C��3C��fC��C��C��3C��C�  C��fC�  C��3C��fC��3C��C��C��fC��3C��C��C��3C��C�  C��fC�  C�&fC��C��3C��C��C��3C��C�  C��fC��C�  C�  C��3C��3C�&fC��C��C��C��C��C��C��C��C�&fC��3C��3C��C�&fC��C��C��C��C��C�&fC��fC��fC��C�  C��3C��C��C�  C��3C��fC��C��3C��fC�  C��C��C��3C��C��C��fC�  C��C��C��3C��C��C��C��fC�  C��C�33C��3C��D �D l�D ��D� D�D�fD
@ D�D�3D�fD� DY�D33DfD ٚD#�fD&�fD)�fD,s3D/� D2y�D5l�D8s3D;Y�D>FfDA@ DDS3DGFfDJ,�DL��DO�fDR��DUL�DW��DZ� D]s3D`@ Db�3De�fDhS3Dk  Dm� Dpy�Ds&fDu� Dx� D{Y�D}��D�9�D��fD���D�\�D�� D�  D�y�D���D�FfD���D� D�s3D��3D�0 D���D���D�C3D���D���D�Y�D���D�3D�i�D��fD�  D�|�D�ٚD�&fD�y�D���D��D�i�D���D�	�D�\�D���D�� D��D�\�D��fD�� D�  D�<�D�i�D���D��3D�fD�33D�` D���D���D���D�3D�9�D�ffDǜ�D�� D�3D�@ D�vfDͳ3D���D�&fD�c3DҦfD�� D�#3D�ffDש�D��3D�<�DۆfD���D� D�Y�D���D��3D�  D�c3D� D���D� D�I�D� D빚D���D�,�D�Y�D�3D�� D�3D�6fD�l�D���D�ɚD�  D�6fD�VfD�� D���D�3E   E ��EX E�3E�3E4�EњEl�EfE��E4�E�fEffE�E�fE	8 E	�fE
d�E
�3E�fE!�E� EP E�Es3E�E$�E<�EVfE��E�E  E6fE��E�E��E�3E�fE� E [3E!vfE"�3E$$�E%;3E&X E's3E)fE*8 E+Q�E,�3E.fE/,�E0>fE1� E2�fE4ffE5h E6�fE7�3E9c3E:[3E;��E>�3EBfEE�EG�3EKI�EN��EQ\�ET� EW��EZ�fE]�fEa�EdD�Eg�fEjh Em��Ep��Es� Ew�Ey�fE}1�E�.fE��fE�K3E�� E�x�E��fE�� E� �E�� E�?3E��fE�C3E�њE���E��E���E�, E�� E�S3E�� E�l E�  E��3E�3E�� E�4 E��3?�  ?�  ?���?�  ?���?���?���?���?���?�  ?���?���?���?���?���?���?�ff?�ff?�33?�33?���?���?�33?�33@ff@��@   @&ff@9��@S33@Y��@s33@�33@���@�ff@���@���@�ff@�  @���@���@�33@�  A   A��A	��A��A  A   A&ffA0  A6ffA<��AD��AK33AS33A[33Ac33Ai��Aq��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144444144414141414141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?���@   @fff@�33@�  @�  @�ffA33A��A0  AI��AfffA���A�ffA���A�  A�33A�33A�33A�  B  B
  B  B��B"��B*ffB2ffB:ffBB  BJ  BR  BZffBbffBj  BrffBz  B�  B���B�  B���B�33B�  B���B�ffB�33B�  B�33B�33B�  B�  B�  B�33B�33B���B�ffB�33B���B�ffB�33B���B���B�33B�  B홚B�ffB�33B���B���C 33C� C�fC��C�3C
��CffCffC��C�fC�3CffC� C��C��C�3C ��C"ffC$L�C&� C(��C*��C,��C.��C0� C2� C4� C6� C8� C:� C<� C>��C@�3CB�3CD��CF��CH� CJ��CL� CNffCP�3CR��CTffCVffCXffCZ�3C\��C^��C`� CbffCd�3Cf��Ch��Cj��Cl�3Cn��Cp��Cr� Ct��Cv� Cx� Cz��C|�3C~��C�@ C�&fC�L�C�L�C�@ C�33C�@ C�ffC�L�C�Y�C�L�C�L�C�L�C�L�C�Y�C�@ C�&fC�&fC�@ C�Y�C�ffC�Y�C�@ C�ffC�L�C�@ C�Y�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�33C�Y�C�L�C�@ C�33C�&fC�Y�C�L�C�33C�Y�C�@ C�&fC�@ C�33C�&fC�33C�Y�C�L�C�&fC�33C�Y�C�Y�C�33C�L�C�@ C�&fC�@ C�ffC�Y�C�33C�Y�C�L�C�33C�L�C�@ C�&fC�L�C�@ C�@ C�33C�33C�ffC�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�33C�33C�Y�C�ffC�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�&fC�&fC�L�C�@ C�33C�Y�C�L�C�@ C�33C�&fC�L�C�33C�&fC�@ C�L�C�L�C�33C�L�C�L�C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�@ C�Y�C�s3C�33C�L�D ,�D ��D�D� D,�D�fD
` D9�D�3D�fD� Dy�DS3D&fD ��D#�fD&�fD)�fD,�3D/� D2��D5��D8�3D;y�D>ffDA` DDs3DGffDJL�DM�DO�fDR��DUl�DX�DZ� D]�3D`` Dc3De�fDhs3Dk  Dm� Dp��DsFfDv  Dx� D{y�D}��D�I�D��fD��D�l�D�� D�0 D���D���D�VfD���D�  D��3D��3D�@ D���D���D�S3D���D��D�i�D���D�#3D�y�D��fD�0 D���D��D�6fD���D���D�,�D�y�D�ɚD��D�l�D���D�� D�,�D�l�D��fD�� D� D�L�D�y�D���D��3D�fD�C3D�p D���D���D���D�#3D�I�D�vfDǬ�D�� D�3D�P D̆fD��3D���D�6fD�s3DҶfD�� D�33D�vfD׹�D�3D�L�DۖfD���D�  D�i�Dਗ਼D��3D�0 D�s3D� D���D�  D�Y�D� D�ɚD�	�D�<�D�i�D�3D�� D�3D�FfD�|�D���D�ٚD� D�FfD�ffD�� D���D�3E ( E ��E` E�3E�3E<�EٚEt�EfE��E<�E�fEnfE	�E�fE	@ E	�fE
l�E3E�fE)�E� EX E�E{3E	�E,�ED�E^fE�E�E( E>fE��E�E�E�3E�fE� E c3E!~fE"�3E$,�E%C3E&` E'{3E)&fE*@ E+Y�E-3E.fE/4�E0FfE1� E2�fE4nfE5p E6�fE7�3E9k3E:c3E;��E>�3EB&fEE�EG�3EKQ�EN��EQd�ET� EW��EZ�fE]�fEa	�EdL�Eg�fEjp Em��Ep��Et  Ew	�Ey�fE}9�E�2fE��fE�O3E�� E�|�E��fE�� E��E�� E�C3E��fE�G3E�՚E���E��E���E�0 E�� E�W3E�� E�p E� E��3E�3E�� E�8 E��3G�O�?�  G�O�?�  G�O�G�O�G�O�G�O�G�O�?�  G�O�G�O�G�O�?���G�O�?ٙ�G�O�?�ffG�O�?�33G�O�@ffG�O�@��@&ff@,��@@  @Fff@Y��@s33@y��@���@�33@���@�ff@���@���@�ff@�  @���@���@�33A   A  A��A��A��A   A(  A.ffA8  A>ffAD��AL��AS33A[33Ac33Ak33Aq��Ay��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144444144414141414141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ �@ �@ *@ �@ ""@ (�@ 1'@ 7�@ >@ G�@ R�@ ^�@ m�@ |�@ ��@ �0@ �(@ ��@ �w@ ��@ �t@ �@ ��@�@o@g@-@:�@G�@UU@b�@qS@~�@��@�H@�A@��@��@�7@�/@�4@�,@%@�@"�@/�@>@K�@X�@ff@t@�d@�@�U@��@�@ȴ@��@��@�@@�9@
�@O@'�@4�@A�@M�@Z�@g�@ww@��@��@��@��@�@ȴ@׹@�m@�@�Q@�@�@)�@7�@D�@Q=@^5@m:@{�@�7@��@��@�~@�&@��@�t@�@��@j@�@ @-�@<@H]@UU@c�@p�@}�@�P@�H@��@�9@��@є@��@�4@�,@%@�@$.@1�@>@Lu@Z�@g@t@��@�\@�@��@��@ƨ@Ӡ@��@�@�E@
=@6@%�@5?@A�@O�@\�@j@x&@��@�$@�m@��@�^@�c@�h@�@�@  @�@�@(�@7�@D�@R�@`�@m�@{�@�7@��@�(@��@��@��@��@�@� @	@	b@	 @	,`@	8�@	G�@	T�@	a�@	o�@	�@	��@	�<@	��@	�F@	��@	ψ@	ލ@	�@	��@
�@
�@
#�@
/@
>�@
K�@
X@
g@
t@
�W@
�@
�@
��@
��@
�J@
խ@
�@
�L@
��@
�@B@&�@4�@B8@P�@[z@i!@x�@�+@�$@��@�r@�@��@�@�T@��@ �@�@�@*S@7L@DD@Q=@^5@m�@z3@�+@�0@��@�-@�w@�|@�#@�@��@�@�@
@-@;d@H]@S�@b�@r@�@�D@�H@��@��@��@�7@�;@t�@�&@�@Wb@��@�Y@@,@��@�#@(G@x&@ƨ@*@e	@�R@	�@Z@��@��@K�@�@��@A�@��@�/@)�@uk@��@	�@UU@�@�@5?@~�@�@@\)@��@�@:@�@ψ@@Z�@�4@�@<�@��@Ӡ@[@i!@�F@@N�@�H@�`@/�@z3@�J@ V@ X@ �(@ �@!9X@!�d@!�o@"�@"_�@"��@"�e@#;d@#��@#�@$�@$Z�@$�z@$�(@%2�@%v�@%��@%�Q@&C�@&�|@&�c@'
=@'M�@'��@'ψ@(�@(SI@(�u@(Ӡ@)�@)T�@)��@)�O@*@*SI@*��@*�
@+�@+\)@+�a@+��@,$�@,g�@,�Y@,�L@-33@-x&@-�@.@.H]@.��@.��@/�@/_�@/��@/�(@00x@0t@0�@0��@1@,@1��@1Ĝ@2�@2I�@2��@2ψ@3�@3R�@3�0@3׹@4B@4[z@4��@4܀@5�@5`�@5�a@5�H@6$�@6g@6��@6�4@7/�@7r@7�F@7�9@8> @8�W@8��@9@9B�@9�p@9�J@:�@:Ji@:��@:�@;J@;Lu@;�C@;�@<
=@<K@<�7@<�W@=@=�W@=��@>o�@?"�@?�<@@�@@�p@A5@@A�Z@Bg@B��@C@,@C�R@Dk.@D�@EZ@F�@F�@F�,@Gr@H(G@H�m@I�@I�*@JF�@J��@K2�@K��@LM�@L�}@Mff@N�@Nx�@OB@O�@P$/@Q`�@R��@T�@UO�@V�k@X$/@YS�@Z��@\ �@]T�@^��@` �@ae	@bψ@dj@ehs@fĜ@h�@ic�@j�(@l�@ma�@n��@p	�@q_�@r��@s��@u_�@v��@x  @ybN@zě@{�@}I@~��@��@���@�Zt@��@���@�W
@�@��~@�Z@�]@��`@�W
@���G�O�@ �G�O�@ �G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�@ �G�O�@ 1G�O�@ �G�O�@ 	�G�O�@ 
�G�O�@ �@ V@ @ @ �@ �@ �@ 6@ �@ �@ 
@  @ !s@ $.@ &�@ (�@ +�@ /@ 0x@ 33@ 6�@ 8�@ :�@ >@ @�@ DD@ F�@ K@ M�@ P�@ S�@ V�@ Z@ ]�@ `�@ c�@ gG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�S�A�\)A�XA�XA�S�A�S�A�S�A�O�A�K�A���A�M�A��A��mA��HA��/A��
A���A���A�ĜA�jA�A���A�hsA�$�Aߗ�A��A���A�9XA���A�~�Aև+A�{A�jA���A�ƨAƕ�AŃAÑhA�dZA�1'A�$�A��-A�&�A�`BA�ƨA���A��hA�O�A��A��/A�v�A���A��A�ȴA�ZA��\A���A�ĜA�jA���A�A�A��wA���A�7A|��Av�yAtM�Apv�AhjAd�DAc�A`ȴA_hsA^{A\r�AYAU/AT�\AS?}AP��AO��AM��ALVAK|�AJ��AJ=qAIp�AH5?AF5?AD�9AC�-AC�AB�/AAƨA@I�A?�wA?"�A?"�A>�/A>1'A=p�A<��A<z�A;�#A;l�A;dZA;dZA;C�A9�TA9XA9/A9�A9%A8�RA8^5A81A7��A6�HA6r�A6(�A5��A5+A4�A3�PA2�uA1��A1O�A1%A0Q�A0bA/�
A/�hA.�`A.~�A-�^A,ȴA,{A+��A++A*�!A*bA)hsA(�A(��A(ĜA(E�A'��A'�^A'&�A&�A&��A&1'A%l�A%%A$�A$=qA$(�A$1A#ƨA#�hA#dZA"�/A"5?A!�A!�#A!ƨA!��A!C�A �A v�A 1'A�TA��AO�A��A��A��A5?A�AA\)A��A�jA�+AVA{A�^Ax�A�A�DAE�AJAK�A��AZA �A�-A"�A��A�A�!A$�A�TAA��A�AȴA��Ar�A5?A�A��Al�A��A�+A�wA/A��A^5A  At�A&�A%A��A��AM�A1AG�A�9AQ�A��A7LA
��A
�\A
5?A	��A	��A	O�A�9A��A��A�A&�A��AZA �AA��A|�At�A`BA�Az�A-A��A�A/A��AAdZA �A ��A M�A   @��@���@��@���@�@�@�V@���@�G�@�+@�Ĝ@�@�"�@�&�@��#@�;d@ț�@š�@�ƨ@��@��-@�o@���@��y@� �@��@�V@�ȴ@�z�@�v�@�I�@�V@���@��H@��@� �@���@�Ĝ@�K�@��-@���@�@���@�z�@��@�V@��9@��R@�hs@�Q�@��!@���@�Ĝ@�dZ@�=q@��@�j@�
=@��@���@�@}/@z�H@xĜ@v��@t��@r~�@o�w@m�-@k�@i��@g+@e�-@dZ@cC�@a7L@`�@]�@\�/@[t�@Y�@X �@V�R@U?}@S@Qhs@O��@NV@MO�@LI�@K@I�7@G�;@F@D��@CdZ@A7L@?l�@>��@>{@<�/@;S�@9X@8��@7K�@5O�@4z�@3"�@2M�@0��@/|�@/\)@.{@-�@+�F@+"�@*�@)�7@(1'@&�@%�@$�/@$1@#@"�@ ��@�y@p�@�D@@��@��@�@K�@�-@O�@�@��@�H@��@&�@Q�@�w@�@ff@�h@?}@�@��@��@S�@
��@
=q@	�^@�`@Q�@��@��@V@�T@`B@�@I�@�m@dZ@�@�\@=q@�@�7@ �`@ b?�{?��?���?�x�?�Q�?�E�?���?�F?��?�;d?�V?�O�?�?�1?�"�?��H?�^5?陚?�r�?��?�+?�?�`B?�z�?��?�S�?���?��?�%?�  ?��?�V?�p�?ܬ?���?ش9?�
=?Լj?ҏ\?У�?��?���?�ƨ?��?ȓu?�K�?��T?�9X?°!?��7?�bN?�\)?�p�?�1?���?���?�b?�l�?��y?�?���?���?�z�?���?�t�?��?�n�?��?��?���?��?�-?�M�?��!?�o?���?��?��/?�`B?�E�?��y?��A�S�A�VA�S�A�S�A�O�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�K�A�M�A�G�A�M�A�M�A�M�A�K�A�K�A�VA�VA�Q�A�S�A�Q�A�Q�A�VA�ZA�ZA�\)A�\)A�XA�XA�VA�XA�ZA�S�A�Q�A�S�A�XA�S�A�S�A�VA�XA�O�A�O�A�M�A�O�A�^5A�bNA�"�A���A��A�^A�PA�p�A�bNA�E�A� �A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�O�A�S�A�\)A�XA�XA�S�A�S�A�S�A�O�A�K�A���A�M�A��A��mA��HA��/A��
A���A���A�ĜA�jA�A���A�hsA�$�Aߗ�A��A���A�9XA���A�~�Aև+A�{A�jA���A�ƨAƕ�AŃAÑhA�dZA�1'A�$�A��-A�&�A�`BA�ƨA���A��hA�O�A��A��/A�v�A���A��A�ȴA�ZA��\A���A�ĜA�jA���A�A�A��wA���A�7A|��Av�yAtM�Apv�AhjAd�DAc�A`ȴA_hsA^{A\r�AYAU/AT�\AS?}AP��AO��AM��ALVAK|�AJ��AJ=qAIp�AH5?AF5?AD�9AC�-AC�AB�/AAƨA@I�A?�wA?"�A?"�A>�/A>1'A=p�A<��A<z�A;�#A;l�A;dZA;dZA;C�A9�TA9XA9/A9�A9%A8�RA8^5A81A7��A6�HA6r�A6(�A5��A5+A4�A3�PA2�uA1��A1O�A1%A0Q�A0bA/�
A/�hA.�`A.~�A-�^A,ȴA,{A+��A++A*�!A*bA)hsA(�A(��A(ĜA(E�A'��A'�^A'&�A&�A&��A&1'A%l�A%%A$�A$=qA$(�A$1A#ƨA#�hA#dZA"�/A"5?A!�A!�#A!ƨA!��A!C�A �A v�A 1'A�TA��AO�A��A��A��A5?A�AA\)A��A�jA�+AVA{A�^Ax�A�A�DAE�AJAK�A��AZA �A�-A"�A��A�A�!A$�A�TAA��A�AȴA��Ar�A5?A�A��Al�A��A�+A�wA/A��A^5A  At�A&�A%A��A��AM�A1AG�A�9AQ�A��A7LA
��A
�\A
5?A	��A	��A	O�A�9A��A��A�A&�A��AZA �AA��A|�At�A`BA�Az�A-A��A�A/A��AAdZA �A ��A M�A   @��@���@��@���@�@�@�V@���@�G�@�+@�Ĝ@�@�"�@�&�@��#@�;d@ț�@š�@�ƨ@��@��-@�o@���@��y@� �@��@�V@�ȴ@�z�@�v�@�I�@�V@���@��H@��@� �@���@�Ĝ@�K�@��-@���@�@���@�z�@��@�V@��9@��R@�hs@�Q�@��!@���@�Ĝ@�dZ@�=q@��@�j@�
=@��@���@�@}/@z�H@xĜ@v��@t��@r~�@o�w@m�-@k�@i��@g+@e�-@dZ@cC�@a7L@`�@]�@\�/@[t�@Y�@X �@V�R@U?}@S@Qhs@O��@NV@MO�@LI�@K@I�7@G�;@F@D��@CdZ@A7L@?l�@>��@>{@<�/@;S�@9X@8��@7K�@5O�@4z�@3"�@2M�@0��@/|�@/\)@.{@-�@+�F@+"�@*�@)�7@(1'@&�@%�@$�/@$1@#@"�@ ��@�y@p�@�D@@��@��@�@K�@�-@O�@�@��@�H@��@&�@Q�@�w@�@ff@�h@?}@�@��@��@S�@
��@
=q@	�^@�`@Q�@��@��@V@�T@`B@�@I�@�m@dZ@�@�\@=q@�@�7@ �`@ b?�{?��?���?�x�?�Q�?�E�?���?�F?��?�;d?�V?�O�?�?�1?�"�?��H?�^5?陚?�r�?��?�+?�?�`B?�z�?��?�S�?���?��?�%?�  ?��?�V?�p�?ܬ?���?ش9?�
=?Լj?ҏ\?У�?��?���?�ƨ?��?ȓu?�K�?��T?�9X?°!?��7?�bN?�\)?�p�?�1?���?���?�b?�l�?��y?�?���?���?�z�?���?�t�?��?�n�?��?��?���?��?�-?�M�?��!?�o?���?��?��/?�`B?�E�?��y?��A�S�A�VA�S�A�S�A�O�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�K�A�M�A�G�A�M�A�M�A�M�A�K�A�K�A�VA�VA�Q�A�S�A�Q�A�Q�A�VA�ZA�ZA�\)A�\)A�XA�XA�VA�XA�ZA�S�A�Q�A�S�A�XA�S�A�S�A�VA�XA�O�A�O�A�M�A�O�A�^5A�bNA�"�A���A��A�^A�PA�p�A�bNA�E�A� �A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�`B	�fB	�ZB	�TB	�ZB	�fB	�fB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�B	��B
	7B
bB
{B
oB
\B
�B
&�B
6FB
K�B
N�B
YB
aHB
aHB
_;B
]/B
_;B
~�B
��B
��B
��B
ƨB
�TB
��B
�`B
��B
�B
R�B	�B	�`B	��B	��B	�B	M�B	33B	1'B	=qB	=qB	0!B	2-B	'�B	"�B	oB	uB	DB	B	+B��B��B	B	6FB	C�B	YB	��B	��B	�5B
�B
>wB
YB
hsB
�%B
�DB
�JB
��B
��B
�LB
�'B
��B
�B
�LB
ĜB
��B
�TB
�mB
�B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�sB
�mB
�mB
�ZB
�TB
�NB
�HB
�;B
�/B
�/B
�#B
�B
�
B
�
B
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
ȴB
ǮB
ǮB
ĜB
ÖB
��B
��B
��B
�}B
�wB
�qB
�qB
�jB
�^B
�RB
�RB
�RB
�LB
�FB
�?B
�9B
�3B
�-B
�!B
�!B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�bB
�\B
�VB
�PB
�JB
�DB
�=B
�7B
�1B
�%B
�%B
�B
�B
�B
�B
� B
� B
}�B
z�B
x�B
w�B
t�B
r�B
q�B
o�B
n�B
m�B
l�B
k�B
jB
hsB
ffB
ffB
dZB
bNB
`BB
_;B
]/B
[#B
[#B
ZB
XB
VB
S�B
S�B
R�B
O�B
N�B
N�B
M�B
L�B
K�B
L�B
K�B
K�B
H�B
H�B
G�B
E�B
F�B
C�B
B�B
B�B
>wB
?}B
>wB
<jB
<jB
49B
0!B
,B
'�B
#�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
$�B
$�B
&�B
(�B
,B
-B
/B
.B
-B
-B
0!B
0!B
1'B
33B
49B
7LB
6FB
:^B
:^B
;dB
<jB
@�B
A�B
B�B
C�B
D�B
D�B
D�B
E�B
F�B
G�B
I�B
I�B
J�B
J�B
M�B
P�B
R�B
S�B
S�B
T�B
T�B
T�B
VB
W
B
W
B
YB
YB
YB
[#B
]/B
]/B
_;B
`BB
`BB
aHB
bNB
cTB
cTB
cTB
e`B
ffB
gmB
gmB
hsB
jB
jB
jB
k�B
l�B
m�B
l�B
m�B
n�B
o�B
o�B
o�B
p�B
q�B
r�B
s�B
s�B
s�B
u�B
u�B
v�B
x�B
y�B
x�B
{�B
|�B
|�B
|�B
~�B
�B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�7B
�7B
�7B
�7B
�=B
�DB
�DB
�DB
�JB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�oB
�oB
�oB
�uB
�uB
�uB
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
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�'B
�'B
�3B
�3B
�?B
�?B
�?B
�LB
�LB
�RB
�XB
�^B
�dB
�dB
�jB
�qB
�qB
�wB
�}B
��B
��B
��B
��B
��B
B
B
ĜB
ĜB
ĜB
ŢB
ĜB
ĜB
ŢB
ŢB
ƨB
ƨB
ƨB
ŢB
ƨB
ŢB
ƨB
ŢB	�mB	�fB	�mB	�fB	�mB	�mB	�fB	�mB	�mB	�mB	�fB	�mB	�sB	�mB	�sB	�mB	�mB	�fB	�fB	�mB	�mB	�mB	�yB	�fB	�mB	�mB	�sB	�mB	�fB	�mB	�`B	�mB	�fB	�fB	�mB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�fB	�`B	�mB	�fB	�mB	�5B	�`B	�TB	�HB	�TB	�`B	�TB	�TB	�TB	�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B	�HB	�HB	�AB	�BB	�BB	�BB	�CB	�=B	�CB	�8B	�2B	�9B	�EB	�FB	�@B	�AB	�BB	�BB	�CB	�CB	�DB	�EB	�EB	�LB	�LB	�TB	�B	��B
	B
KB
dB
XB
FB
}B
&�B
60B
K�B
N�B
YB
a4B
a4B
_(B
]B
_)B
~�B
�oB
��B
��B
ƗB
�CB
��B
�PB
��B
��B
R�B	�B	�PB	��B	��B	�B	M�B	3#B	1B	=bB	=bB	0B	2B	'�B	"�B	aB	hB	7B	B	B��B��B	 �B	6;B	C�B	YB	��B	��B	�-B
�B
>pB
YB
hmB
�B
�?B
�EB
�}B
��B
�IB
�$B
��B
� B
�KB
ěB
��B
�UB
�nB
�B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�}B
�}B
�kB
�eB
�`B
�ZB
�NB
�BB
�CB
�8B
�,B
� B
� B
�B
�	B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ĺB
õB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�vB
�wB
�wB
�rB
�mB
�fB
�aB
�[B
�VB
�JB
�KB
�>B
�?B
�:B
�4B
�#B
�#B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�~B
�xB
�sB
�mB
�bB
�bB
�]B
�WB
�QB
�FB
�@B
�AB
~5B
{#B
yB
xB
t�B
r�B
q�B
o�B
n�B
m�B
l�B
k�B
j�B
h�B
f�B
f�B
d�B
b�B
`�B
_�B
]{B
[pB
[pB
ZkB
X_B
VSB
THB
THB
SCB
P0B
O+B
O+B
N&B
M!B
LB
M"B
LB
LB
I
B
IB
HB
E�B
GB
C�B
B�B
B�B
>�B
?�B
>�B
<�B
<�B
4�B
0�B
,qB
(]B
$GB
!8B
.B
B

B
 B
#B
B
B
B
B
B
B
"B
B
#B
,B
*B
-B
6B
.B
1B
:B
CB
RB
VB
fB
iB
rB
{B
�B
�B
 �B
 �B
!�B
#�B
%�B
%�B
'�B
)�B
,�B
.B
0B
/B
.B
.B
1&B
1*B
23B
4BB
5KB
8bB
7_B
;zB
;}B
<�B
=�B
A�B
B�B
C�B
D�B
E�B
E�B
E�B
F�B
G�B
H�B
KB
KB
LB
LB
O(B
R>B
TNB
UWB
UZB
VcB
VfB
ViB
WqB
XzB
X}B
Z�B
Z�B
Z�B
\�B
^�B
^�B
`�B
a�B
a�B
b�B
c�B
d�B
d�B
d�B
f�B
hB
iB
iB
jB
l+B
l.B
l1B
m:B
nCB
oLB
nIB
oRB
p]B
qfB
qiB
qlB
ruB
s~B
t�B
u�B
u�B
u�B
w�B
w�B
x�B
z�B
{�B
z�B
}�B
~�B
~�B
~�B
��B
�	B
�B
�B
�B
� B
�*B
�3B
�6B
�?B
�HB
�KB
�ZB
�]B
�`B
�cB
�lB
�vB
�yB
�|B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�#B
�2B
�CB
�HB
�\B
�aB
�lB
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
��B
��B
�B
�B
�B
�B
�&B
�1B
�8B
�=B
�JB
�^B
�uB
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�6B
�FB
�dB
�qB
��B
��B
��B
��B
��B
��B
�B
�B
�&B
�<B
�RB
�`B
�vB
ƄB
ƔB
ǪB
ǺB
��B
��B
��B
�
B
�B
�"B
�8B
�GB
�\B
�lB
�{B
˄B
̙B
ˣB
̸B
��B	�HB	�AB	�HB	�AB	�HB	�HB	�AB	�HB	�HB	�HB	�AB	�HB	�NB	�HB	�NB	�HB	�HB	�AB	�AB	�HB	�HB	�HB	�TB	�AB	�HB	�HB	�NB	�HB	�AB	�HB	�<B	�IB	�BB	�BB	�IB	�BB	�BB	�BB	�BB	�BB	�CB	�CB	�=B	�=B	�=B	�CB	�=B	�JB	�CB	�KB	�B	�>B	�2B	�&B	�2B	�>B	�3B	�3B	�3B	�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809191702212021061413574220210614135742202107221611432021072216114320210722161143201809191702212021061413574220210614135742202107221611432021072216114320210722161143PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091917022120180919170221  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091917022120180919170221QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091917022120180919170221QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216142020210722161420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                