CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-03T17:01:26Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       P    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       d    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                      HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                       HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        <   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	�      � 	�Argo profile    3.1 1.2 19500101000000  20181103170126  20210722160154  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               (   (DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؋o�3��@؋o�3��11  @؋o��g@@؋o��g@@6�9m	@6�9m	�c�ﲪ��c�ﲪ�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?333@   @@  @�33@�ff@�ff@�33A   A  A$��AA��A`  A�  A���A���A�33A���A�  A�33A�  B ��B��BffBffB��B'��B0ffB8��B@ffBHffBP  BXffB`ffBhffBo��Bw��B��B�  B�33B�33B�33B�ffB�33B�  B�33B�33B�33B�  B���B�  B�33B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B�33B���C  CL�C�C�fC
  C�C�fC�3C�fC  C�C33C�C��C�fC   C"33C$  C%��C(�C*33C,�C.  C0�C2�C3�fC6�C8�C:  C<33C>�C@  CB  CD  CFL�CH33CJ�CL  CN  CO�fCR  CT  CU�fCXL�CZ33C\�C^�C`  Cb33Cd�Cf  Ch33Cj�Cl�Cn  Co�fCr�CtL�Cv33Cx  CzL�C|33C~  C��C��C��fC��3C��C��C�&fC��C��fC��3C��3C�  C�  C��C��C�&fC�&fC��C��3C�  C�  C��C��C��C��fC��3C�  C��C�33C��C�  C��C�  C��fC�  C��3C��3C��C��C�  C��C�  C��3C�  C��C��C��3C��C��C��fC�  C�&fC��C��3C��C��3C��fC�  C��C��C��3C��C��C��C��3C�  C��C�&fC��C��3C��C�&fC��C��3C��3C��C��C�&fC��C��3C�  C��C��C��C�&fC��C��3C��3C�  C��C��3C�  C��C��3C�  C��C��C�&fC�&fC�  C��C��C��3C��C��C��C�  C��C�  C��fC�  C��C�&fC��C��fC�  C��C�&fC��C�  C��C��C��3C�&fC��C�33C�33C�33D �D l�D �3Dl�D�3D��D	l�D  Dy�D  D� D  D��DffDfD �fD#Y�D&�D(� D+y�D.,�D0� D3��D6&fD8� D;S3D=��D@��DC�DE��DH@ DJ� DM,�DO� DR�DT� DV��DYffD[�fD^ffD`�3DcS3De��Dh@ Dj� Dm33Do��Dr�Dt��Dw�Dy�fD{��D}�3D�&fD�Y�D���D���D���D��D�L�D�y�D���D�� D�3D�C3D�p D���D�ɚD���D�&fD�L�D�s3D�� D���D���D�,�D�\�D���D���D��fD�fD�<�D�c3D���D��fD��fD�3D�FfD�p D�� D�� D�fD�9�D�l�D���D�� D�	�D�C3D�|�D�� D��3D� D�@ D�p D��fD�ٚD�fD�0 D�Y�D3Dã3D��fD��D� D�33D�P D�c3Dˀ D̓3Dͩ�D��fD��fD�3D��D�33D�VfD�p D֐ D׳3D���D��D�3D�,�D�P D�s3Dߐ D� D��3D��fD�fD�,�D�S3D�y�D��D��D�� D���D�&fD�I�D�i�D���D��D�ɚD���D�fD�&fD�C3D�ffD�� D��3D�� D���D��fD��3E 3E �fE E�fE.fE��E@ E� EP E��E[3E��E��Ex E	{3E
� E[3EK3E� E!�E3Es3EٚE�3E Ea�E��E� E3E��E�3E�E 3E!� E"~fE#��E%a�E&S3E'��E)�E)��E+VfE,��E.	�E.��E0T�E1��E33E4T�E5+3E6q�E7�3E9 E:@ E;{3E<�fE?��EC8 EF EII�EL�3EO� ER�3EVfEXٚE\ E_T�Eb�Eex Eh|�Ek��En�fEq��Eu8 Ex@ E{0 E~~fE�� E�O3E��fE�h E�� E�H E��fE���E�5�E�� E�� E��E�h�?��?��>���?   ?   ?��?   >���?��?��?   ?   ?   ?   ?��?��?333?333?L��?fff?�  ?���?�  ?ٙ�?�33@   @33@&ff@333@Fff@`  @s33@�33@�  @���@�ff@�  @���@�ff@�  @ٙ�@���@�ffA33A	��AffAffA��A$��A,��A333A9��A@  AI��AP  AVffA\��Ac33Ah  Aq��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414444414141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?�  ?���@   @`  @�33@�ff@�ff@�33A  A  A,��AI��Ah  A�  A���A���A�33A���A�  A�33A�  B��B
��BffBffB!��B)��B2ffB:��BBffBJffBR  BZffBbffBjffBq��By��B���B�  B�33B�33B�33B�ffB�33B�  B�33B�33B�33B�  B���B�  B�33B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B�33C L�C� C��C��CffC
� C��CffC33CffC� C��C�3C��CL�CffC � C"�3C$� C&L�C(��C*�3C,��C.� C0��C2��C4ffC6��C8��C:� C<�3C>��C@� CB� CD� CF��CH�3CJ��CL� CN� CPffCR� CT� CVffCX��CZ�3C\��C^��C`� Cb�3Cd��Cf� Ch�3Cj��Cl��Cn� CpffCr��Ct��Cv�3Cx� Cz��C|�3C~� C�Y�C�L�C�&fC�33C�Y�C�Y�C�ffC�L�C�&fC�33C�33C�@ C�@ C�L�C�Y�C�ffC�ffC�L�C�33C�@ C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�Y�C�s3C�Y�C�@ C�L�C�@ C�&fC�@ C�33C�33C�Y�C�Y�C�@ C�L�C�@ C�33C�@ C�L�C�L�C�33C�L�C�L�C�&fC�@ C�ffC�L�C�33C�L�C�33C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�@ C�L�C�ffC�L�C�33C�L�C�ffC�L�C�33C�33C�L�C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�Y�C�ffC�Y�C�33C�33C�@ C�L�C�33C�@ C�Y�C�33C�@ C�L�C�Y�C�ffC�ffC�@ C�Y�C�L�C�33C�L�C�Y�C�Y�C�@ C�L�C�@ C�&fC�@ C�Y�C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�@ C�L�C�L�C�33C�ffC�Y�C�s3C�s3C�s3D 9�D ��D3D��D3D�D	��D  D��D  D� D@ DٚD�fD&fD �fD#y�D&,�D(� D+��D.L�D1  D3��D6FfD8� D;s3D>�D@��DC9�DEٚDH` DJ� DML�DO� DR,�DT� DW�DY�fD\fD^�fD`�3Dcs3De��Dh` Dj� DmS3DoٚDr,�Dt��Dw,�Dy�fD{��D~3D�6fD�i�D���D�ɚD���D�,�D�\�D���D���D�� D�#3D�S3D�� D���D�ٚD�	�D�6fD�\�D��3D�� D���D�	�D�<�D�l�D���D�ɚD��fD�&fD�L�D�s3D���D��fD��fD�#3D�VfD�� D�� D�� D�fD�I�D�|�D���D�� D��D�S3D���D�� D��3D�  D�P D�� D��fD��D�fD�@ D�i�D3Dó3D��fD���D�  D�C3D�` D�s3Dː Ḍ3D͹�D��fD��fD�3D�,�D�C3D�ffDՀ D֠ D��3D���D���D�3D�<�D�` Dރ3Dߠ D�� D��3D�fD�&fD�<�D�c3D牚D��D���D�� D�	�D�6fD�Y�D�y�D�D��D�ٚD���D�fD�6fD�S3D�vfD�� D��3D�� D���D��fD�3E 3E �fE  E�fE6fE��EH E� EX E��Ec3E��E��E� E	�3E
� Ec3ES3E� E)�E3E{3E�E�3E Ei�E��E� E#3E��E�3E�E 3E!� E"�fE#��E%i�E&[3E'��E)�E*�E+^fE,��E.�E.��E0\�E1��E33E4\�E533E6y�E7�3E9 E:H E;�3E<�fE?��EC@ EF EIQ�EL�3EO� ER�3EVfEX�E\ E_\�Eb$�Ee� Eh��Ek��En�fEq��Eu@ ExH E{8 E~�fE�� E�S3E��fE�l E�� E�L E��fE���E�9�E�� E�� E�!�E�l�G�O�G�O�?fffG�O�?�  G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�?�  G�O�?���G�O�?���?�ff?�33?�  ?ٙ�@   @��@��@   @333@Fff@S33@fff@�  @���@�33@�  @���@�ff@�  @ə�@�ff@�  @陚@���A33A33A��AffAffA$��A,��A4��A;33AA��AH  AQ��AX  A^ffAd��Ak33Ap  Ay��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414444414141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ j@ �@ �@ {@ �@ #�@ *S@ 0x@ 6�@ =q@ FQ@ R�@ _�@ m:@ {�@ �7@ ��@ ��@ �~@ �w@ ��@ ��@ �y@ �q@@b@
@-@;d@H]@V@b�@qS@~�@��@��@��@�9@@��@ލ@�4@��@�@{@"�@0x@>@K@X@ff@t�@��@�\@��@��@�R@��@Ӡ@�H@��@��@
=@�@%�@33@@,@O0@\�@hs@ww@�+@�u@��@�@�k@ȴ@��@�@�Y@ �@@�@'�@5�@DD@SI@_�@k�@{�@��@��@��@�-@��@�@�#@��@��@�@�@�@,`@:@I�@V�@c�@p�@~K@�D@��@�A@�9@Ĝ@є@ލ@�4@�,@1@*@""@1'@>@K�@X�@e�@t�@��@��@�@��@��@��@��@��@�@��@�@B@'�@3�@?}@M�@[z@i�@ww@��@�$@�y@�!@�k@ȴ@�
@�@�@^@V@�@(G@6�@E�@T�@`�@m:@{�@��@��@��@��@�w@�*@��@�@�q@	j@	b@	�@	-@	:�@	F�@	V@	c�@	oF@	~K@	��@	�H@	��@	��@	��@	��@	��@	��@	��@
%@
*@
#�@
0x@
<�@
K@
Yn@
hs@
t�@
�@
�@
�@
�Y@
��@
�J@
�O@
��@
��@
�E@	�@�@&;@4�@B8@P�@]�@i!@v�@�@�u@��@�@�@ȴ@�
@�`@�@@�@O@*S@7L@C�@R�@`�@n�@z�@�7@�0@�y@�~@��@��@�#@�@��@@@g@,`@:�@H]@T�@e	@r@�@��@�U@��@��@�2@�*@܀@dZ@��@��@2�@ww@��@j@Ji@�u@�#@"�@l�@�F@  @Ji@�$@��@&�@m�@��@�9@C�@��@ψ@6@\)@�m@�@%�@g�@��@�@@0x@t�@�@�9@?}@�@��@
=@M$@�@є@�@Z@��@��@�@V�@�<@�@�@[z@�U@�/@[@^5@�m@��@"�@b�@�(@�T@$/@dZ@�(@��@ ""@ bN@ �z@ �@!$�@!e�@!��@!�@"&�@"e�@"��@"�@##�@#dZ@#��@#�@$%�@$ff@$�A@$�y@%+@%l�@%��@%��@&1�@&t�@&��@&�,@':�@'z�@'��@'��@(>�@(�W@(��@)  @)?}@)~�@)�j@)��@*8�@*ww@*��@*�Y@+-@+i�@+��@+��@,�@,Z@,��@,��@-V@-Lu@-��@-��@.@.@,@.|�@.�@.�~@/6�@/t�@/�~@/��@0-@0k.@0��@0�@1"�@1a�@1��@1�/@2O@2Wb@2��@2խ@3@3P�@3��@3�o@4	�@4E�@4�@4��@4��@5:@5qS@5�@5��@6&�@6c�@6�a@6��@7{@7Q=@7�C@7ƨ@8  @8:@8t@8��@8�@9 �@9��@::@:��@;G�@;�@<I@<�@=~�@=�@>|?@?*@?uk@@�@@��@A.l@A��@B-�@B�@C7�@C�@DE�@D�@ET�@E�e@F�@F� @G��@H"�@H��@I�@I��@JA�@J��@K<@K��@L`�@L��@MLu@M׹@NdZ@N�Y@Oww@O��@P�p@Q�@SK@T~K@U��@WB8@X��@Y�@[Q=@\��@]�H@_I�@`y�@a�@c1�@d��@e��@g'�@h�m@i�@k,`@l��@mӠ@o2�@p��@q�
@s(�@sp�@s��@t�@t;e@t��@t��@u^@uA�G�O�G�O�@ �G�O�@ jG�O�G�O�@ �G�O�G�O�G�O�G�O�G�O�@ jG�O�@ G�O�@ �@ v@ %@ �@ 1@ 
=@ �@ �@ �@ �@ �@ @ *@ �@ �@ �@ �@  �@ #�@ %�@ '�@ *S@ ,`@ .l@ 2�@ 4�@ 7�@ :�@ <�@ @,@ B�@ FQ@ I�@ Lu@ O0@ Q�@ V@ X�@ [z@ ^5@ `�@ b�@ gG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�ƨA���A���A���A���A���A��A��HA��/A��#A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��;A��
A�ȴAԲ-Aԉ7A�=qA���AґhA�ĜA�O�A̮Aǲ-A���Aú^A�1'A�bNA�JA���A�M�A���A�`BA�7LA�I�A�oA���A�dZA���A��A�Q�A�A�A�"�A�  A���A���A���A�r�A�|�A�A�A���A���A�-A��TA�{A�jA�;dA�I�A�7LA��\A��wA�{A��7A�ĜA�K�A�S�A��9A�7LA��A��A��A���A�C�A��DA�Q�A��TA���A��`A���A��A���A�jA���A�dZA�hsA�C�A�x�A�bNA�
=A��A��A�bNA�oA���A�"�A���A���A���A��uA~��A|A{dZA{�Az$�Ax�yAx{Au�TAt9XAr5?Ao�An(�AmdZAm/Al^5AkS�Ait�Ag�Afz�AfAc��Ab(�A`ZA\bAX��AYVAY�AX$�AWG�AU�-AS�AQ?}AP��AO+AM"�AK"�AJ$�AIt�AH�`AG
=AE�AC�PABjA@�jA>�RA>{A<��A:ffA9;dA89XA7G�A5A4ZA3`BA1�
A1�PA133A0��A0��A0JA.VA,�RA+��A*�A)�A)G�A)%A(E�A&�RA%�A$n�A#�A#�^A#�A!��A!|�A ��A   A/A��A{A;dA{A��A�-A(�A/Az�AbA�A��AE�A�A�+A��A�A1'At�A?}A��A�uA�7A
�A	�A��A5?At�A�mA�A\)A&�A��A�
AdZAjA ��A 5?@�ff@���@�J@���@��-@��`@�r�@�w@�@��@�=q@�r�@�hs@���@��@⟾@��@��;@ޗ�@݉7@���@�\)@��@ڰ!@�-@�G�@�5?@��@�%@�Z@��
@�{@�`B@��@��@ʗ�@�9X@�?}@��@�E�@�`B@�x�@��-@�`B@�(�@�p�@��@�S�@��/@���@���@�Ĝ@�-@��@�r�@��@��@�K�@��H@�$�@�&�@�j@�S�@��H@���@�/@��@�33@�J@���@�&�@��;@��;@�o@�E�@�?}@�9X@��@�-@���@���@�S�@��+@�p�@�Q�@;d@~E�@|��@z�@w��@tZ@r�@q�@pA�@n{@mp�@k�
@j�H@iG�@g\)@e�T@e@c��@b�H@`�`@_��@]�@[33@Zn�@Y%@X��@W
=@U�-@T�/@R��@QG�@O�@N$�@LZ@K33@Ix�@Hr�@E��@D1@B-@AG�@?��@?�@=�@<z�@;dZ@:��@9��@97L@8��@8 �@6�y@6E�@5`B@4�D@41@2�!@1��@0��@01'@-�T@-O�@,�@*��@(��@&�@$�@#"�@"~�@!X@ ��@�;@�@v�@�@��@��@��@7L@&�@  @+@��@/@�D@��@C�@�@=q@x�@bN@;d@�@ff@@�-@��@I�@ƨ@@
�\@
-@	hs@	7L@�u@�P@K�@��@V@�h@�/@��@��@o@��@��@X@ �`?��;?��?�v�?�V?���?��?��?���?�F?��?��?�V?���?���?���?��?�`B?�9X?���?�&�?߾w?�|�?ޗ�?�j?��m?ڟ�?�X?�ff?�`B?���?�t�?���?ѩ�?Ѓ?�;d?�V?��?�/?�/?�j?˥�?�?�~�?��#?��?ȴ9?�+?�$�?ļj?�t�?�J?�%?��;?�5??��-?���?��m?�"�?���?�~�?�=q?�=q?�^5?�=q?��#?���?��#?�=q?���?�?���?�1?���?�p�?��h?��-?���?��?�{?�5??�V?�v�A�ȴA�ĜA�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA�ȴA�ƨA���A���A�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��#A��;A��HA��;A��#A��#A��#A��A��#A��`A��yA��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�ĜA�ƨA���A���A���A���A���A��A��HA��/A��#A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��;A��
A�ȴAԲ-Aԉ7A�=qA���AґhA�ĜA�O�A̮Aǲ-A���Aú^A�1'A�bNA�JA���A�M�A���A�`BA�7LA�I�A�oA���A�dZA���A��A�Q�A�A�A�"�A�  A���A���A���A�r�A�|�A�A�A���A���A�-A��TA�{A�jA�;dA�I�A�7LA��\A��wA�{A��7A�ĜA�K�A�S�A��9A�7LA��A��A��A���A�C�A��DA�Q�A��TA���A��`A���A��A���A�jA���A�dZA�hsA�C�A�x�A�bNA�
=A��A��A�bNA�oA���A�"�A���A���A���A��uA~��A|A{dZA{�Az$�Ax�yAx{Au�TAt9XAr5?Ao�An(�AmdZAm/Al^5AkS�Ait�Ag�Afz�AfAc��Ab(�A`ZA\bAX��AYVAY�AX$�AWG�AU�-AS�AQ?}AP��AO+AM"�AK"�AJ$�AIt�AH�`AG
=AE�AC�PABjA@�jA>�RA>{A<��A:ffA9;dA89XA7G�A5A4ZA3`BA1�
A1�PA133A0��A0��A0JA.VA,�RA+��A*�A)�A)G�A)%A(E�A&�RA%�A$n�A#�A#�^A#�A!��A!|�A ��A   A/A��A{A;dA{A��A�-A(�A/Az�AbA�A��AE�A�A�+A��A�A1'At�A?}A��A�uA�7A
�A	�A��A5?At�A�mA�A\)A&�A��A�
AdZAjA ��A 5?@�ff@���@�J@���@��-@��`@�r�@�w@�@��@�=q@�r�@�hs@���@��@⟾@��@��;@ޗ�@݉7@���@�\)@��@ڰ!@�-@�G�@�5?@��@�%@�Z@��
@�{@�`B@��@��@ʗ�@�9X@�?}@��@�E�@�`B@�x�@��-@�`B@�(�@�p�@��@�S�@��/@���@���@�Ĝ@�-@��@�r�@��@��@�K�@��H@�$�@�&�@�j@�S�@��H@���@�/@��@�33@�J@���@�&�@��;@��;@�o@�E�@�?}@�9X@��@�-@���@���@�S�@��+@�p�@�Q�@;d@~E�@|��@z�@w��@tZ@r�@q�@pA�@n{@mp�@k�
@j�H@iG�@g\)@e�T@e@c��@b�H@`�`@_��@]�@[33@Zn�@Y%@X��@W
=@U�-@T�/@R��@QG�@O�@N$�@LZ@K33@Ix�@Hr�@E��@D1@B-@AG�@?��@?�@=�@<z�@;dZ@:��@9��@97L@8��@8 �@6�y@6E�@5`B@4�D@41@2�!@1��@0��@01'@-�T@-O�@,�@*��@(��@&�@$�@#"�@"~�@!X@ ��@�;@�@v�@�@��@��@��@7L@&�@  @+@��@/@�D@��@C�@�@=q@x�@bN@;d@�@ff@@�-@��@I�@ƨ@@
�\@
-@	hs@	7L@�u@�P@K�@��@V@�h@�/@��@��@o@��@��@X@ �`?��;?��?�v�?�V?���?��?��?���?�F?��?��?�V?���?���?���?��?�`B?�9X?���?�&�?߾w?�|�?ޗ�?�j?��m?ڟ�?�X?�ff?�`B?���?�t�?���?ѩ�?Ѓ?�;d?�V?��?�/?�/?�j?˥�?�?�~�?��#?��?ȴ9?�+?�$�?ļj?�t�?�J?�%?��;?�5??��-?���?��m?�"�?���?�~�?�=q?�=q?�^5?�=q?��#?���?��#?�=q?���?�?���?�1?���?�p�?��h?��-?���?��?�{?�5??�V?�v�A�ȴA�ĜA�ĜA�ĜA�ĜA�ƨA�ĜA�ƨA�ĜA�ĜA�ȴA�ƨA���A���A�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��#A��;A��HA��;A��#A��#A��#A��A��#A��`A��yA��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�!B�B�B�B�9B�qB�FB�3BŢBƨBƨBǮBƨBǮBƨBƨBǮBƨBƨBɺB��B��B��B��B�B�B�NB�BB!�B>wBJ�BXBbNB{�B�\B�1B�JB�PB�DB�JB�JB�\B��B��B��B�VB�Bx�Bq�Bs�B�B�JB��B��B�uB�bB�PB�VB�1B�B�B�B~�Bt�Bn�BiyBgmBYBM�BB�B/B&�B�B�BB��B�B�B�B�B�fB�HB�)B��B�wB��B}�BYBG�B?}B6FB2-B8RB49B �BPB
��B
��B
�B
�;B
�B
��B
��B
ŢB
��B
�LB
��B
��B
�7B
s�B
bNB
ZB
R�B
I�B
<jB
1'B
�B
DB	�B	�`B	�B	�
B	��B	��B	ǮB	B	�wB	�jB	�LB	��B	��B	� B	XB	R�B	aHB	}�B	~�B	|�B	t�B	jB	bNB	\)B	Q�B	B�B	8RB	2-B	0!B	.B	#�B	�B	oB	VB	  B��B�B�HB�B��B��BȴBŢBƨB�wB�RB�LB�9B�9B�3B�B��B��B��B�uB�bB�JB�DB�%B�B� B{�Bz�Bx�Bv�Bt�Bs�Bo�BjBiyBgmBgmBbNB^5BZBYBS�BQ�BN�BM�BN�BM�BK�BJ�BI�BG�BF�BA�BA�B@�B@�B?}B<jB>wB<jB<jB<jB9XB;dB:^B9XB9XB8RB8RB8RB6FB9XB8RB6FB7LB6FB5?B49B49B33B2-B0!B2-B33B33B7LB9XB7LB6FB;dB:^B:^B:^B<jB<jB<jB<jB;dB<jB<jBC�BF�BE�BF�BJ�BK�BI�BM�BM�B^5BdZBe`Bt�B|�B�VB��B�LB�wB��B�yB�B	B	!�B	1'B	C�B	P�B	ZB	bNB	t�B	w�B	�B	�=B	�oB	��B	��B	�-B	�9B	�XB	�XB	�qB	ÖB	ɺB	��B	��B	�B	�/B	�NB	�TB	�fB	�B	�B	��B	��B	��B	��B	��B
B
B
1B
1B

=B
JB
PB
\B
hB
uB
uB
�B
�B
�B
�B
 �B
!�B
"�B
#�B
%�B
%�B
&�B
(�B
,B
-B
-B
/B
/B
0!B
2-B
2-B
49B
6FB
7LB
7LB
:^B
;dB
<jB
=qB
?}B
@�B
B�B
B�B
C�B
D�B
F�B
F�B
G�B
G�B
I�B
I�B
J�B
J�B
L�B
K�B
L�B
L�B
M�B
N�B
O�B
O�B
O�B
Q�B
S�B
T�B
VB
W
B
YB
[#B
\)B
\)B
^5B
^5B
`BB
_;B
`BB
`BB
bNB
bNB
dZB
e`B
e`B
gmB
gmB
hsB
k�B
jB
k�B
l�B
m�B
m�B
m�B
o�B
p�B
q�B
q�B
p�B
q�B
r�B
r�B
s�B
t�B
u�B
u�B
w�B
v�B
w�B
y�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�DB
�JB
�PB
�\B
�bB
�hB
�hB
�oB
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
�B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�?B
�FB
�FB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
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
�jB
�dB
�dB
�dB
�dB
�dB��B��B�B�B�B�B�B�B�B�B�B�B�!B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�'B�!B�B�B�B�B�B�B�B�B�B�B�XB�jB�wB�jB�3B�9B�3B�!B�LBBƨBƨBǮBƨBƨBƨBŢBƨBƨG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B��B�B�B�B��B�B�B�'B�^B�3B�'BÖBĜBĜBŢBĜBŢBĜBĜBŢBĜBĜBǮBȴB��B��B��B��B�B�BB�BB�B<jBH�BVB`BBy�B�PB�%B�=B�DB�7B�=B�=B�PB��B��B�{B�JB� Bv�Bo�Bq�B�B�=B�{B��B�hB�VB�DB�JB�%B�B�B�B|�Br�Bl�BgmBe`BW
BK�B@�B-B$�B�B�BB��B�B�B�B�B�ZB�;B�B��B�jB��B{�BW
BE�B=qB49B0!B6FB2-B�BDB
��B
�B
�B
�/B
�B
��B
��B
ÖB
�}B
�?B
��B
��B
�+B
q�B
`BB
XB
P�B
G�B
:^B
/B
�B
	7B	�B	�TB	�
B	��B	��B	��B	ŢB	��B	�jB	�^B	�?B	��B	�uB	}�B	VB	P�B	_;B	{�B	|�B	z�B	r�B	hsB	`BB	ZB	O�B	@�B	6FB	0!B	.B	,B	!�B	�B	bB	JB��B�B�B�;B�B��B��BƨBÖBĜB�jB�FB�?B�-B�-B�'B�B��B��B��B�hB�VB�=B�7B�B� B}�By�Bx�Bv�Bt�Br�Bq�Bm�BhsBgmBe`Be`B`BB\)BXBW
BQ�BO�BL�BK�BL�BK�BI�BH�BG�BE�BD�B?}B?}B>wB>wB=qB:^B<jB:^B:^B:^B7LB9XB8RB7LB7LB6FB6FB6FB49B7LB6FB49B5?B49B33B2-B2-B1'B0!B.B0!B1'B1'B5?B7LB5?B49B9XB8RB8RB8RB:^B:^B:^B:^B9XB:^B:^BA�BD�BC�BD�BH�BI�BG�BK�BK�B\)BbNBcTBr�Bz�B�JB��B�?B�jB��B�mB�B	  B	�B	/B	A�B	N�B	XB	`BB	r�B	u�B	� B	�1B	�bB	��B	��B	�!B	�-B	�LB	�LB	�dB	��B	ǮB	ȴB	��B	�B	�#B	�BB	�HB	�`B	�B	�B	�B	��B	��B	��B	��B
B
B
+B
+B
	7B
DB
JB
VB
bB
oB
oB
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
$�B
%�B
'�B
+B
,B
,B
.B
.B
/B
1'B
1'B
33B
5?B
6FB
6FB
9XB
:^B
;dB
<jB
>wB
?}B
A�B
A�B
B�B
C�B
E�B
E�B
F�B
F�B
H�B
H�B
I�B
I�B
K�B
J�B
K�B
K�B
L�B
M�B
N�B
N�B
N�B
P�B
R�B
S�B
T�B
VB
XB
ZB
[#B
[#B
]/B
]/B
_;B
^5B
_;B
_;B
aHB
aHB
cTB
dZB
dZB
ffB
ffB
gmB
jB
jB
k�B
l�B
m�B
m�B
m�B
o�B
p�B
q�B
q�B
p�B
q�B
r�B
r�B
s�B
t�B
u�B
u�B
w�B
v�B
w�B
y�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�DB
�JB
�PB
�\B
�bB
�hB
�hB
�oB
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
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�qB
�qB
�qB
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
�wB��B��B��B��B��B��B�B��B�B��B��B�B�B��B�B��B�B��B�B�B�B��B�B�B�B�B�B�B�B�B�B��B��B��B�B�B��B�B�B�B�B�FB�XB�jB�XB�!B�'B�'B�B�?B�}BÖBĜBĜBĜBÖBĜBÖBĜBĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811031701262021061413525220210614135252202106141746512021061417465120210614174651201811031701262021061413525220210614135252202106141746512021061417465120210614174651PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018110317012620181103170126  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110317012620181103170126QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110317012620181103170126QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015420210722160154IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                