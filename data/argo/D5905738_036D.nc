CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-15T07:00:59Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   
   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181015070059  20210722160154  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               $   $DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؆�&�A�@؆�&�A�11  @؆�"",@؆�"",@6$�fQm�@6$�fQm��c�����[�c�����[11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?333?�33@@  @�  @�  @�33@�33A��A��A$��AA��Aa��A�  A�  A���A���A���A�  A�  A�B ��B  BffB��B   B(ffB0ffB8  B@ffBHffBP  BX  B`ffBhffBpffBxffB�33B�33B�  B���B���B�  B�ffB���B�ffB�ffB�33B�33B�  B�  B���B�33B�  B�  B�33B�ffB�33B�  B�  B�  B�33B�33B�33B�  B�  B�ffB�ffB�33C   C  CL�C33C�C
  C  C  C�fC�C33C�C  C  C  C�fC�fC!�fC$33C&�C(  C*  C,  C.�C0�C2  C4�C6�C8  C:  C<  C>L�C@L�CB33CD�CF  CHL�CJ33CL�CN  CO�fCR33CT33CV  CXL�CZ33C\33C^�C`  Cb  Cc��Cf  Ch33Cj33Ck�fCn33Cp33Cr  CtL�Cv33Cx  Cz  C{�fC~�C�  C��fC�  C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��3C�  C�  C�  C��C��C��C��C�&fC��3C��3C��3C��3C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��3C��C��C��C��C��3C��C��C�  C�&fC��C��C��C��C�  C��3C��C��C��3C��C�  C��3C��C��3C��3C�  C�&fC��C��3C��C��C��C��C��fC�  C��C��C�&fC��C��3C�  C�  C��C��C�&fC�33C�  C��fC��fC��fC��fC��3C��3C��3C��3C�  C�  C��C��C�&fC�33C�&fC��C��fC��3C�  C��C��C��C�  C��fC��3C��C��C�  C��fC��3C�  C��C��3C���D� DL�D  D
��D�3D�3D@ D��D�fDFfD��D y�D#  D%� D(�D*�3D-3D/��D2&fD4�3D7FfD9�fD<L�D>�fDAl�DC��DF�fDI�DK� DN  DP�fDSfDU��DXfDZy�D\ٚD_S3Da�fDdl�Df� DiY�Dk�3Dn@ Dp�fDs�Du` DwٚDz` D|y�DfD��3D�3D�C3D��3D��3D�	�D�FfD��fD��3D�3D�S3D��fD�� D�6fD��fD�� D��D�ffD���D��3D�9�D�� D�� D��D�Y�D���D�� D�  D�i�D�� D��3D�0 D�vfD���D�fD�C3D��fD��3D���D�6fD�vfD��3D���D�)�D�VfD��fD��3D���D���D�  D�C3D�i�D���D©�D�ɚD��fD�fD��D�<�D�VfD�vfDː D̩�D��3D��fD��D�33D�Y�D�y�DԜ�D��fD�� D� D�6fD�\�Dی�DܶfD��fD�fD�FfD�l�D♚D㹚D�� D���D�#3D�@ D�ffD��D멚D�ɚD���D��D�)�D�FfD�ffD�|�D���D�� D��3D�� D��D��fD��3D�	�D��D�33D�L�E 0 E �3EA�E��ENfE� E[3E�fEd�E�3Eq�E��E��EfE�3E	� E
� E>fEC3E�3E�fEh Es3EvfE�EfE�3E��E3E$�E33EɚEٚE nfE!��E"�fE$( E%;3E&I�E'��E(� E*k3E+h E,�E-�fE/S3E0<�E1��E2��E4\�E5K3E6� E7��E9fE:��E;y�E<�E@>fEC�EFnfEINfEL��EO�3ER��EV�EY#3E\fE_�Eb\�Ee�3Ehi�Ek��En��Eq��Et�Ex  E{T�E~�3E��fE�O3E���E�s3E��fE��fE� E���E�?3E��fE�NfE��3E��3E�ɚE�3E�vfE���E�fE�W3?��?   >���?��?   ?��?   ?   ?��?��?��?��?333?333?�  ?���?�ff?�33?���?�ff@   @33@&ff@333@L��@Y��@l��@�  @���@�33@�  @���@�ff@�33@�  @ٙ�@陚@�ffA33A  A  A  AffA$��A,��A333A9��A@  AH  AP  AVffA^ffAd��Ak33At��A{33A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414441411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?fff?���@��@`  @�  @�  @�33@�33A	��A��A,��AI��Ai��A�  A�  A���A���A���A�  A�  A���B��B
  BffB��B"  B*ffB2ffB:  BBffBJffBR  BZ  BbffBjffBrffBzffB�33B�33B�  B���B���B�  B�ffB���B�ffB�ffB�33B�33B�  B�  B���B�33B�  B�  B�33B�ffB�33B�  B�  B�  B�33B�33B�33B�  B�  B�ffB�ffB�33C � C� C��C�3C��C
� C� C� CffC��C�3C��C� C� C� CffC ffC"ffC$�3C&��C(� C*� C,� C.��C0��C2� C4��C6��C8� C:� C<� C>��C@��CB�3CD��CF� CH��CJ�3CL��CN� CPffCR�3CT�3CV� CX��CZ�3C\�3C^��C`� Cb� CdL�Cf� Ch�3Cj�3ClffCn�3Cp�3Cr� Ct��Cv�3Cx� Cz� C|ffC~��C�@ C�&fC�@ C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�ffC�33C�@ C�@ C�@ C�L�C�L�C�L�C�Y�C�ffC�33C�33C�33C�33C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�L�C�L�C�Y�C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�L�C�L�C�@ C�33C�Y�C�L�C�33C�L�C�@ C�33C�L�C�33C�33C�@ C�ffC�L�C�33C�L�C�Y�C�Y�C�L�C�&fC�@ C�L�C�Y�C�ffC�L�C�33C�@ C�@ C�Y�C�Y�C�ffC�s3C�@ C�&fC�&fC�&fC�&fC�33C�33C�33C�33C�@ C�@ C�L�C�L�C�ffC�s3C�ffC�L�C�&fC�33C�@ C�L�C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�33C��D� Dl�D@ D�D�3D�3D` D�D�fDffD�D ��D#  D%� D(,�D*�3D-33D/��D2FfD4�3D7ffD9�fD<l�D?fDA��DD�DF�fDI,�DK� DN@ DP�fDS&fDU��DX&fDZ��D\��D_s3DbfDd��Dg  Diy�Dk�3Dn` Dp�fDs,�Du� Dw��Dz� D|��D&fD��3D�3D�S3D��3D��3D��D�VfD��fD��3D�#3D�c3D��fD�  D�FfD��fD�� D�)�D�vfD���D�3D�I�D�� D�� D�)�D�i�D���D�� D�0 D�y�D�� D�3D�@ D��fD���D�fD�S3D��fD��3D��D�FfD��fD��3D���D�9�D�ffD��fD��3D���D��D�0 D�S3D�y�D���D¹�D�ٚD��fD�fD�,�D�L�D�ffDʆfDˠ D̹�D��3D��fD��D�C3D�i�DӉ�DԬ�D��fD�  D�  D�FfD�l�Dۜ�D��fD��fD�&fD�VfD�|�D⩚D�ɚD�� D��D�33D�P D�vfD��D빚D�ٚD���D��D�9�D�VfD�vfD��D���D�� D��3D�� D���D�fD�3D��D�)�D�C3D�\�E 8 E �3EI�E��EVfE� Ec3E�fEl�E�3Ey�E�E��EfE�3E	� E
� EFfEK3E�3E�fEp E{3E~fE	�EfE�3E��E#3E,�E;3EњE�E vfE!��E"�fE$0 E%C3E&Q�E'��E(� E*s3E+p E,�E-�fE/[3E0D�E1��E3�E4d�E5S3E6� E7��E9fE:��E;��E<�E@FfEC!�EFvfEIVfEL��EO�3ER��EV�EY+3E\fE_�Ebd�Ee�3Ehq�Ek��En��Eq��Et�Ex( E{\�E~�3E��fE�S3E���E�w3E��fE��fE� E���E�C3E��fE�RfE�3E��3E�͚E�3E�zfE���E�fE�[3G�O�G�O�?fffG�O�?�  G�O�G�O�?�  G�O�G�O�G�O�?���G�O�?���?�  ?ٙ�?�ff?�33@ff@33@   @333@Fff@S33@l��@y��@�ff@�  @���@�33@�  @���@�ff@�33@�  @陚@���A33A33A  A  A   A&ffA,��A4��A;33AA��AH  AP  AX  A^ffAfffAl��As33A|��A���A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414441411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ �@ �@ �@ {@ O@ ""@ )�@ 0x@ 7L@ ?}@ FQ@ R�@ `B@ m:@ z�@ �7@ ��@ ��@ �~@ �&@ �*@ ��@ �@ �q@�@@g@-@:@H]@V@b�@p�@~�@��@�H@��@��@�>@�7@�/@�(@�,@1@�@#�@1'@>@K�@X�@ff@s_@�d@�\@�@�Y@��@ƨ@Ӡ@�H@��@�E@
�@�@%�@33@B8@O�@\�@i�@ww@�+@�#@�@�@��@�c@�[@�`@�@ �@�@O@(�@5�@C�@Q=@`�@m�@z�@��@�0@��@�-@�&@�|@�#@�@��@j@@ �@-�@:�@G�@Wb@dZ@qS@~K@�D@��@��@��@Ĝ@є@�;@�4@�,@�@@""@1'@>�@Ji@Z@g�@t@��@��@�@��@��@ƨ@Ӡ@��@��@��@�@�@&;@3�@A�@O0@\�@j@x&@��@�u@��@�!@�@�c@�
@�@�@ �@V@�@+@5�@C�@Q=@^�@m�@{�@�7@��@��@��@��@�*@�#@��@� @	j@	@	
@	-�@	;d@	H]@	V@	bN@	r@	~�@	��@	��@	��@	��@	�>@	��@	��@	��@	��@
�@
�@
"�@
/�@
<�@
K�@
X@
e�@
t@
��@
�@
�U@
�Y@
��@
�W@
�O@
��@
��@
�E@�@�@&;@2�@@�@N�@]�@k.@y�@��@��@�@��@�^@�@�[@�@�@�Q@�@O@)�@7L@FQ@T�@a�@m�@y�@��@�0@��@�-@��@��@�@�m@�q@�@@[@+�@:@H]@T�@`B@��@7�@�@є@[@k�@��@�Q@H]@�@�h@
@b�@�A@��@1�@v@�@ �@FQ@��@��@�@\�@��@�m@-@r@�R@��@>@�d@ȴ@
�@M�@��@�C@�@]�@�m@�@'�@i�@�Y@��@,`@o�@��@�@@3�@x&@�j@ �@D�@�7@��@o@V�@��@��@&;@n�@��@��@B8@��@��@ �@ Z�@ �@ �@!,`@!t@!�^@!��@"B�@"��@"��@#@#X�@#��@#�H@$&�@$l�@$��@$�q@%;d@%~�@%��@&�@&I@&��@&ψ@'@'SI@'�#@'�O@(�@(Q=@(�\@(�|@)J@)I�@)�+@)Ĝ@*]@*>�@*z3@*��@*�@+1'@+m:@+�M@+�`@,#�@,bN@,�@,��@-[@-[z@-��@-�t@.�@.V�@.��@.�\@/�@/V�@/��@/�h@06@0Wb@0��@0Ӡ@1b@1O0@1��@1��@2	�@2FQ@2��@2��@2�Q@3<@3x�@3�F@3�@4/@4i�@4��@4��@5�@5SI@5�7@5Ĝ@5��@6:�@6v�@6�~@6��@7&;@7^5@7��@7Ӡ@8�@8C�@8|�@8�F@8�@9)�@9c�@9�T@:I@:��@;,`@;�[@<E�@<�L@=bN@>
=@>|?@>��@?�u@@�@@��@A�@A��@B1�@B�4@CR�@Cƨ@Ds_@D��@E[z@F
=@F�@F�@G�@H@H�F@I""@I�>@J/@J�*@K1�@K�@L^5@L�e@MZ@M��@N\�@N�}@O��@O�E@P�I@R%@S> @T��@U�@WR�@X�@Z@[V@\�4@]�l@_+@`��@a��@c)�@d�W@e�e@g8�@h{�@i��@k<@l��@m�@o2�@p�I@q��@s'�@t��@u�
@w*T@x��@y��@{$.@|��@}��@~�@~X@~� @~��@'�@o�G�O�G�O�@ �G�O�@ jG�O�G�O�@ jG�O�G�O�G�O�@ G�O�@ �@ �@ 1@ �@ 	�@ 
�@ J@ �@ �@ �@ @ �@ 6@ B@ O@ [@ g@ ""@ $.@ &�@ )�@ ,`@ .l@ 1�@ 4�@ 7�@ :@ =q@ @�@ C�@ FQ@ I�@ Lu@ O0@ Q�@ UU@ X�@ [z@ ^�@ a�@ dZ@ hs@ k.@ n�@ qS@ t�@ wwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A֋DAցA֏\A֟�A֗�A֧�Aִ9Aְ!A֥�A֮Aְ!Aְ!Aֲ-Aְ!A֩�A֮Aְ!A֬A֣�A֛�A֓uA֍PA�v�A�l�A�`BA�5?A���Aԉ7A�jA�=qAΩ�AʍPA�/Aš�A�jA���A�`BA�v�A�(�A� �A��HA��A��A�ĜA��yA�~�A��mA��
A�bA���A��\A�XA���A�
=A��A�\)A��#A�jA���A��A��HA��A��A��!A�x�A�v�A�p�A���A��/A�+A��A�G�A��DA�bNA��/A���A��\A� �A�^5A�=qA���A���A� �A���A��+A��A�1'A��mA�Q�A�1'A��A��A��A���A�^5A��A�n�A� �A��yA��-A�%A�-A��A��/A�I�A��#A��\A��A���A�S�A�K�A�oA��TA��A���A�G�A��A�(�A�VA��TA�ffA�(�A�&�A��A��A���A��yA�n�A��9A�~�A;dA{S�Ax�Au�wAs�^Al9XAjn�Ai�-Ah��Ag��Aex�Ab�Aa�Aax�A`  A^Q�A]p�A\�jAY�PAW"�AV��AVjAUATE�AQ�TAO�PANQ�AM�
AM�FAM��AL��AJ�yAH��AF(�AC�AC�AC"�AB�ABZAB1AA�FAA�A@�A?"�A>-A<�RA;O�A81'A5��A3`BA17LA.�+A-`BA,=qA+`BA*ZA(��A(�A(I�A(1'A'A&ȴA%��A$�uA#�A#�7A#�A!��A!�A!
=A ��A ZA�A�
A`BA�\A
=A��A"�A��A��A=qAO�A�A  A��A��AbNA$�A�A��A��A�A�A	�A(�Al�A�A �AĜAA ��A I�@���@�"�@��@���@��@�Z@�C�@�5?@��;@�v�@�/@���@��D@�ƨ@��-@�I�@�J@� �@�ȴ@�j@��@އ+@�hs@�G�@�$�@��@�=q@��@�l�@��@��9@��@�ff@��@��@��@�K�@�j@�V@��-@�J@���@�z�@�A�@��F@���@���@�^5@��`@��
@��;@���@�(�@�  @�ȴ@�^5@�-@�O�@��9@���@��@���@�?}@�ƨ@��@��@�`B@��/@��
@��T@�bN@�dZ@�@�x�@��@;d@}��@{t�@zM�@w�@v{@t�/@s@pQ�@n��@lz�@k@hĜ@g�P@d��@b��@a�@`b@^V@\�@\I�@[@Y��@W�;@Wl�@U�T@S�@R~�@P�u@N�y@N{@L9X@J^5@I7L@HA�@G�P@Fȴ@C��@Ct�@B^5@A7L@?�w@?+@>�+@=@<��@<(�@;S�@:n�@8r�@7+@5V@41@2��@1��@0��@/l�@.5?@-O�@,9X@*�!@)��@(Q�@&��@%�T@$�@#��@#C�@"~�@"=q@!hs@�w@K�@��@9X@�H@�^@�7@X@�@|�@\)@��@E�@�@��@��@@~�@x�@&�@�u@;d@��@�@�+@{@?}@Z@o@
M�@	x�@	X@	�@�;@��@E�@{@�T@`B@��@9X@��@S�@^5@�@&�@ �9@  �?�\)?���?���?��?��?�-?�A�?�V?���?�1?�?陚?�l�?�?䛦?���?�M�?�G�?��?�V?�O�?�dZ?���?ٙ�?�l�?�E�?���?�9X?�o?���?ѩ�?�A�?��;?�\)?θR?�v�?Ͳ-?�/?�I�?�?���?��?ɺ^?�r�?��?�?ļj?Õ�?�o?�G�?�A�?��?�p�?�V?�(�?��?�"�?�"�?�?�?��H?��H?���?���?�?�C�?�dZ?���?�1?�I�?��?�p�?��?���?��?�|�?�  ?��`?�hs?��7?���?���?��?�-?�-AփAօA֋DA֏\A֓uA�z�A�~�A�|�AփA�z�A�|�A�|�A�|�A�~�A�~�AցAօAփAփA֋DA֑hA֙�A֟�A֟�A֝�A֟�A֝�A֓uA֓uA֛�A֬Aֲ-Aֲ-AֶFAֲ-Aֲ-A֬A֧�A֣�A֥�A֬Aְ!Aְ!A֮Aְ!A֮Aְ!Aְ!Aֲ-Aְ!Aֲ-Aֲ-Aְ!Aֲ-Aְ!Aְ!Aֲ-Aְ!A֮A֩�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A֋DAցA֏\A֟�A֗�A֧�Aִ9Aְ!A֥�A֮Aְ!Aְ!Aֲ-Aְ!A֩�A֮Aְ!A֬A֣�A֛�A֓uA֍PA�v�A�l�A�`BA�5?A���Aԉ7A�jA�=qAΩ�AʍPA�/Aš�A�jA���A�`BA�v�A�(�A� �A��HA��A��A�ĜA��yA�~�A��mA��
A�bA���A��\A�XA���A�
=A��A�\)A��#A�jA���A��A��HA��A��A��!A�x�A�v�A�p�A���A��/A�+A��A�G�A��DA�bNA��/A���A��\A� �A�^5A�=qA���A���A� �A���A��+A��A�1'A��mA�Q�A�1'A��A��A��A���A�^5A��A�n�A� �A��yA��-A�%A�-A��A��/A�I�A��#A��\A��A���A�S�A�K�A�oA��TA��A���A�G�A��A�(�A�VA��TA�ffA�(�A�&�A��A��A���A��yA�n�A��9A�~�A;dA{S�Ax�Au�wAs�^Al9XAjn�Ai�-Ah��Ag��Aex�Ab�Aa�Aax�A`  A^Q�A]p�A\�jAY�PAW"�AV��AVjAUATE�AQ�TAO�PANQ�AM�
AM�FAM��AL��AJ�yAH��AF(�AC�AC�AC"�AB�ABZAB1AA�FAA�A@�A?"�A>-A<�RA;O�A81'A5��A3`BA17LA.�+A-`BA,=qA+`BA*ZA(��A(�A(I�A(1'A'A&ȴA%��A$�uA#�A#�7A#�A!��A!�A!
=A ��A ZA�A�
A`BA�\A
=A��A"�A��A��A=qAO�A�A  A��A��AbNA$�A�A��A��A�A�A	�A(�Al�A�A �AĜAA ��A I�@���@�"�@��@���@��@�Z@�C�@�5?@��;@�v�@�/@���@��D@�ƨ@��-@�I�@�J@� �@�ȴ@�j@��@އ+@�hs@�G�@�$�@��@�=q@��@�l�@��@��9@��@�ff@��@��@��@�K�@�j@�V@��-@�J@���@�z�@�A�@��F@���@���@�^5@��`@��
@��;@���@�(�@�  @�ȴ@�^5@�-@�O�@��9@���@��@���@�?}@�ƨ@��@��@�`B@��/@��
@��T@�bN@�dZ@�@�x�@��@;d@}��@{t�@zM�@w�@v{@t�/@s@pQ�@n��@lz�@k@hĜ@g�P@d��@b��@a�@`b@^V@\�@\I�@[@Y��@W�;@Wl�@U�T@S�@R~�@P�u@N�y@N{@L9X@J^5@I7L@HA�@G�P@Fȴ@C��@Ct�@B^5@A7L@?�w@?+@>�+@=@<��@<(�@;S�@:n�@8r�@7+@5V@41@2��@1��@0��@/l�@.5?@-O�@,9X@*�!@)��@(Q�@&��@%�T@$�@#��@#C�@"~�@"=q@!hs@�w@K�@��@9X@�H@�^@�7@X@�@|�@\)@��@E�@�@��@��@@~�@x�@&�@�u@;d@��@�@�+@{@?}@Z@o@
M�@	x�@	X@	�@�;@��@E�@{@�T@`B@��@9X@��@S�@^5@�@&�@ �9@  �?�\)?���?���?��?��?�-?�A�?�V?���?�1?�?陚?�l�?�?䛦?���?�M�?�G�?��?�V?�O�?�dZ?���?ٙ�?�l�?�E�?���?�9X?�o?���?ѩ�?�A�?��;?�\)?θR?�v�?Ͳ-?�/?�I�?�?���?��?ɺ^?�r�?��?�?ļj?Õ�?�o?�G�?�A�?��?�p�?�V?�(�?��?�"�?�"�?�?�?��H?��H?���?���?�?�C�?�dZ?���?�1?�I�?��?�p�?��?���?��?�|�?�  ?��`?�hs?��7?���?���?��?�-?�-AփAօA֋DA֏\A֓uA�z�A�~�A�|�AփA�z�A�|�A�|�A�|�A�~�A�~�AցAօAփAփA֋DA֑hA֙�A֟�A֟�A֝�A֟�A֝�A֓uA֓uA֛�A֬Aֲ-Aֲ-AֶFAֲ-Aֲ-A֬A֧�A֣�A֥�A֬Aְ!Aְ!A֮Aְ!A֮Aְ!Aְ!Aֲ-Aְ!Aֲ-Aֲ-Aְ!Aֲ-Aְ!Aְ!Aֲ-Aְ!A֮A֩�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B}�B{�B{�Bz�Bz�B{�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�By�By�Bx�Bx�Bx�Bx�Bx�Bw�Br�B~�B�PB�+B��B�qB��B�B�B��B��B	7BbB�B!�B$�B)�B1'B33B2-B<jBB�BG�BG�BH�BN�BT�B[#B[#B]/B^5BbNBe`Be`Be`BdZBcTBbNBaHB`BB\)BXBXBVBP�BK�BE�BD�B:^B8RB5?B/B0!B/B,B-B#�B�BbBB�B�B�jB�-B�B��B��B�hB�DB�B~�B{�Bw�Bm�BJ�B:^B$�B"�B�B�BJB+BB
�B
�HB
��B
ĜB
�9B
�-B
�B
�B
��B
��B
�{B
�hB
�\B
�VB
�DB
�%B
|�B
s�B
l�B
XB
C�B
+B
�B
PB	��B	�B	�B	�yB	�fB	�;B	��B	ƨB	��B	�qB	�3B	��B	��B	��B	�+B	�%B	�B	�B	z�B	q�B	iyB	]/B	W
B	R�B	O�B	M�B	F�B	:^B	,B	�B	uB	bB	JB	
=B	+B	B	B��B��B�B�sB�/B��B��B�FB��B��B�VB�DB�+B�B}�B}�B{�B{�Bz�Bx�Bw�Bw�By�Bx�Bv�Bu�Bt�Bv�Bv�Bu�Bt�Bu�Bs�Bs�Bp�Bl�Bk�Bk�BhsBe`BbNB`BB^5BXBYBT�BS�BR�BO�BN�BK�BL�B>wB;dB:^B6FB49B1'B2-B0!B/B/B.B-B-B.B/B.B.B.B0!B0!B/B/B.B-B/B.B.B2-B1'B2-B2-B1'B7LB5?B:^B@�BG�BO�BW
BbNBm�B}�B� B��B��B�'BȴB��B�B	B	bB	�B	.B	K�B	YB	dZB	jB	y�B	�B	�7B	�7B	�PB	��B	��B	�-B	�3B	�RB	�jB	��B	B	B	B	��B	�B	�5B	�HB	�ZB	�mB	�B	�B	��B	��B	��B	��B	��B	��B
B
B
%B
	7B
	7B

=B
PB
bB
hB
{B
�B
�B
�B
�B
 �B
 �B
"�B
$�B
$�B
%�B
&�B
'�B
(�B
(�B
)�B
-B
-B
/B
2-B
33B
49B
6FB
7LB
8RB
8RB
:^B
=qB
=qB
=qB
?}B
A�B
A�B
A�B
C�B
C�B
D�B
E�B
F�B
H�B
I�B
K�B
L�B
M�B
N�B
O�B
P�B
R�B
S�B
T�B
VB
W
B
YB
[#B
[#B
[#B
]/B
]/B
^5B
^5B
^5B
`BB
`BB
bNB
bNB
dZB
e`B
e`B
e`B
ffB
hsB
gmB
hsB
iyB
iyB
k�B
k�B
m�B
l�B
n�B
m�B
n�B
q�B
q�B
q�B
p�B
q�B
q�B
t�B
u�B
u�B
v�B
v�B
v�B
x�B
x�B
z�B
y�B
z�B
{�B
{�B
|�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�+B
�1B
�DB
�JB
�JB
�PB
�VB
�\B
�\B
�hB
�hB
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
��B
�B
�B
�B
�B
�!B
�'B
�3B
�9B
�?B
�?B
�FB
�LB
�LB
�LB
�LB
�RB
�XB
�XB
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
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�qB|�B{�B}�B|�B{�B|�Bz�B|�B{�B{�B|�B{�B{�B{�B{�B|�Bz�B{�B{�B{�B{�Bz�Bz�Bz�Bz�Bz�By�Bz�B{�B~�Bz�By�Bz�By�Bz�Bz�Bz�Bz�Bz�B{�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�Bz�Bz�Bz�Bz�By�Bz�By�Bz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    B{�By�By�Bx�Bx�By�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bu�Bp�B|�B�DB�B��B�dB��B�B�yB��B��B+BVBuB�B"�B'�B/B1'B0!B:^B@�BE�BE�BF�BL�BR�BYBYB[#B\)B`BBcTBcTBcTBbNBaHB`BB_;B^5BZBVBVBS�BN�BI�BC�BB�B8RB6FB33B-B.B-B)�B+B!�B{BVBB�B�B�^B�!B��B��B��B�\B�7B�B|�By�Bu�Bk�BH�B8RB"�B �B�B{B
=BB  B
�B
�;B
��B
B
�-B
�!B
�B
��B
��B
��B
�oB
�\B
�PB
�JB
�7B
�B
z�B
q�B
jB
VB
A�B
(�B
�B
DB	��B	�sB	�sB	�mB	�ZB	�/B	��B	ĜB	�}B	�dB	�'B	��B	��B	��B	�B	�B	� B	~�B	x�B	o�B	gmB	[#B	T�B	P�B	M�B	K�B	D�B	8RB	)�B	�B	hB	VB	
=B	1B	B	B	  B��B�B�B�fB�#B��B�wB�9B��B��B�JB�7B�B�B{�B{�By�By�Bx�Bv�Bu�Bu�Bw�Bv�Bt�Bs�Br�Bt�Bt�Bs�Br�Bs�Bq�Bq�Bn�BjBiyBiyBffBcTB`BB^5B\)BVBW
BR�BQ�BP�BM�BL�BI�BJ�B<jB9XB8RB49B2-B/B0!B.B-B-B,B+B+B,B-B,B,B,B.B.B-B-B,B+B-B,B,B0!B/B0!B0!B/B5?B33B8RB>wBE�BM�BT�B`BBk�B{�B}�B�{B��B�BƨBȴB�B	  B	VB	�B	,B	I�B	W
B	bNB	hsB	w�B	�B	�+B	�+B	�JB	��B	��B	�'B	�-B	�LB	�dB	�}B	��B	��B	��B	��B	�B	�/B	�BB	�TB	�fB	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
1B
1B
	7B
JB
\B
bB
uB
{B
�B
�B
�B
�B
�B
!�B
#�B
#�B
$�B
%�B
&�B
'�B
'�B
(�B
,B
,B
.B
1'B
2-B
33B
5?B
6FB
7LB
7LB
9XB
<jB
<jB
<jB
>wB
@�B
@�B
@�B
B�B
B�B
C�B
D�B
E�B
G�B
H�B
J�B
K�B
L�B
M�B
N�B
O�B
Q�B
R�B
S�B
T�B
VB
XB
ZB
ZB
ZB
\)B
]/B
^5B
^5B
^5B
`BB
`BB
bNB
bNB
dZB
e`B
e`B
e`B
ffB
hsB
gmB
hsB
iyB
iyB
k�B
k�B
m�B
l�B
n�B
m�B
n�B
q�B
q�B
q�B
p�B
q�B
q�B
t�B
u�B
u�B
v�B
v�B
v�B
x�B
x�B
z�B
y�B
z�B
{�B
{�B
|�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�+B
�1B
�DB
�JB
�JB
�PB
�VB
�\B
�\B
�hB
�hB
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
�B
�B
�B
�B
�B
�B
�'B
�-B
�9B
�FB
�LB
�LB
�RB
�XB
�XB
�XB
�XB
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�wB
�wB
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
��B
�}B
��Bz�By�B{�Bz�By�Bz�Bx�Bz�By�By�Bz�By�By�By�By�Bz�Bx�By�By�By�By�Bx�Bx�Bx�Bx�Bx�Bw�Bx�By�B|�Bx�Bw�Bx�Bw�Bx�Bx�Bx�Bx�Bx�By�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bx�Bx�Bx�Bx�Bw�Bx�Bw�Bx�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810150700592021061413524420210614135244202106141746472021061417464720210614174647201810150700592021061413524420210614135244202106141746472021061417464720210614174647PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018101507005920181015070059  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101507005920181015070059QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101507005920181015070059QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015420210722160154IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                