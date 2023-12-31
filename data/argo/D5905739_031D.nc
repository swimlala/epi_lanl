CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:46Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   
X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180917231346  20210617131502  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�|l*���@�|l*���11  @�|l""8@�|l""8@6�l�<K
@6�l�<K
�c�|�hs�c�|�hs11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?���@ff@Fff@�33@�33@�ff@�33A��A��A&ffAC33Ac33A���A�  A���A�  A�ffA���A�ffA�B   B��B  BffB��B'��B0ffB8  B@ffBHffBP  BXffB`��Bh  Bp  BxffB��B�33B�ffB�33B�33B�ffB�ffB�ffB�33B�  B���B�  B�  B�33B�33B�33B�  B�  BǙ�B˙�B���B�33B�ffB�33Bߙ�B�  B�33B���B�ffB���B�  B�33C 33C�C��C�fC  C
�CL�C  C�3C�fC  C�C33C  C�fC33C�fC!��C$  C&33C(�C)�fC,�C.L�C0�C2  C4�C6  C8  C:33C<�C>  C@  CA��CD  CF33CH�CI�fCL�CN�CO��CR  CT33CVffCX33CZ  C\�C^33C`�Ca��Cd  Cf�Ch33Cj33ClL�Cn  Co�3Cq�3Cs�fCv  Cx  Cz33C|L�C~�C��C��3C��3C�  C��C�&fC�33C��C��fC��3C��C��C�&fC��C��fC��3C�  C��C�  C��fC�  C��C��C��C��fC�  C��C�  C��fC�  C��C��C�  C��C��C��3C��C�  C��3C��C�&fC��C�  C�&fC��C��3C�  C�&fC�  C��3C�  C��C��C�  C��C��C��fC�  C��C��C�&fC��C��3C��C�&fC��C��3C��C�&fC��C��3C�  C��C�&fC�&fC��C�  C��C��C�&fC�&fC�&fC�  C��C�&fC�33C��C�  C��C�&fC�33C�&fC�  C��C��3C�ٚC��fC�  C�  C��C��C��C�&fC�&fC��C�ٚC��fC��3C�  C�  C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��3C��3C��fC�ٚC�&fD33D�fD��DY�D�3DL�D�3D&fD�3DfDffD��D!FfD#��D&&fD(�3D*�3D-S3D/�fD233D4�fD6��D9L�D;��D>,�D@�3DC3DEy�DG�fDJ` DL� DOl�DQ�3DT� DV��DY�3D\,�D^�fDa` Dc�3Df��Di3Dk��Dn3Dp� Ds,�Du�fDx9�Dz�3D|� D@ D��3D� D�@ D��3D���D��3D�,�D�VfD�� D���D��3D�  D�#3D�I�D�c3D��fD��fD��3D��3D�  D�fD�33D�I�D�\�D�y�D��3D�ɚD���D��D�33D�VfD�|�D��3D��fD�� D�fD�9�D�` D���D��3D���D� D�@ D�p D��3D��fD�  D�,�D�c3D���D���D���D�  D�I�D�y�D���D��fD�  D�&fD�P D�p Dǐ DȰ D�ɚD��D��D�0 D�I�D�i�DІfDќ�Dҳ3D��3D�� D��D���D�	�D��D�&fD�0 D�0 D�33D�0 D�0 D�0 D�6fD�6fD�<�D�FfD�I�D�S3D�\�D�p D�vfD�|�D�fD�3D��D��D﹚D��fD�� D�� D���D�fD�3D�  D�33D�9�D�FfD�@ D�FfD�FfD�I�D�L�E ,�E ��E,�E��E0 E��E+3E6fE��E�3E0 E	)�E
�3E� E�E��E�3E� E�fEa�EٚE� ES3ENfE�3E�3EI�EFfE��E�3E!Q�E"T�E#ٚE$�E%�3E'vfE(~fE*�E+�E,y�E-vfE.��E0L�E1<�E2�fE3�3E4�fE6)�E7y�E8њE:  E;t�E<VfE?� EB��EE�3EI9�EL>fEO+3ERa�EU�3EX� E[��E^��Eb3Ee�Eh6fEk6fEn��Eq��Et� Ew��Ez�fE}�3E���E�fE���E�@ E�ٚE� E�nfE�� E� E�X�E���E� ?   >���?   >���>���>L��?   >���>���>���>���>���>���>���>���>���>���>���>���?   >���?   ?   ?333?333?L��?�  ?���?���?�33?�  ?ٙ�?�33@   @33@��@,��@Fff@L��@`  @l��@�33@���@�ff@�33@�  @�  @���@�ff@�33@�  @���A��AffA33A33A#33A)��A0  A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441441441414144141414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?fff?���@&ff@fff@�33@�33@�ff@�33A	��A��A.ffAK33Ak33A���A�  A���A�  A�ffA���A�ffA���B  B	��B  BffB!��B)��B2ffB:  BBffBJffBR  BZffBb��Bj  Br  BzffB���B�33B�ffB�33B�33B�ffB�ffB�ffB�33B�  B���B�  B�  B�33B�33B�33B�  B�  Bș�B̙�B���B�33B�ffB�33B���B�  B�33B���B�ffB���B�  B�33C �3C��CL�CffC� C
��C��C� C33CffC� C��C�3C� CffC�3C ffC"L�C$� C&�3C(��C*ffC,��C.��C0��C2� C4��C6� C8� C:�3C<��C>� C@� CBL�CD� CF�3CH��CJffCL��CN��CPL�CR� CT�3CV�fCX�3CZ� C\��C^�3C`��CbL�Cd� Cf��Ch�3Cj�3Cl��Cn� Cp33Cr33CtffCv� Cx� Cz�3C|��C~��C�&fC�33C�33C�@ C�L�C�ffC�s3C�L�C�&fC�33C�L�C�L�C�ffC�L�C�&fC�33C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�L�C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�Y�C�@ C�33C�L�C�ffC�L�C�@ C�ffC�L�C�33C�@ C�ffC�@ C�33C�@ C�Y�C�L�C�@ C�Y�C�L�C�&fC�@ C�L�C�Y�C�ffC�L�C�33C�L�C�ffC�Y�C�33C�L�C�ffC�L�C�33C�@ C�Y�C�ffC�ffC�Y�C�@ C�L�C�L�C�ffC�ffC�ffC�@ C�L�C�ffC�s3C�Y�C�@ C�Y�C�ffC�s3C�ffC�@ C�L�C�33C��C�&fC�@ C�@ C�L�C�L�C�Y�C�ffC�ffC�L�C��C�&fC�33C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�33C�33C�&fC��C�ffDS3D�fD	�Dy�D�3Dl�D�3DFfD�3D&fD�fD��D!ffD#��D&FfD(�3D+3D-s3D/�fD2S3D4�fD7�D9l�D;��D>L�D@�3DC33DE��DHfDJ� DM  DO��DR3DT� DW�DY�3D\L�D^�fDa� Dd3Df��Di33Dk��Dn33Dp� DsL�Du�fDxY�Dz�3D|� D` D��3D�  D�P D��3D�ɚD�3D�<�D�ffD�� D���D��3D� D�33D�Y�D�s3D��fD��fD��3D��3D� D�&fD�C3D�Y�D�l�D���D��3D�ٚD���D��D�C3D�ffD���D��3D��fD�  D�&fD�I�D�p D���D��3D���D�  D�P D�� D��3D��fD� D�<�D�s3D���D���D�	�D�0 D�Y�D���D���D��fD� D�6fD�` Dƀ DǠ D�� D�ٚD���D��D�@ D�Y�D�y�DЖfDѬ�D��3D��3D�� D���D�	�D��D�)�D�6fD�@ D�@ D�C3D�@ D�@ D�@ D�FfD�FfD�L�D�VfD�Y�D�c3D�l�D� D�fD��D�fD�3D���D��D�ɚD��fD�� D�� D���D�fD�#3D�0 D�C3D�I�D�VfD�P D�VfD�VfD�Y�D�\�E 4�E ��E4�E��E8 E��E33E>fE��E�3E8 E	1�E
�3E� E!�E��E�3E  E�fEi�E�E� E[3EVfE�3E�3EQ�ENfE��E�3E!Y�E"\�E#�E$�E%�3E'~fE(�fE*�E+	�E,��E-~fE.��E0T�E1D�E2�fE43E4�fE61�E7��E8ٚE:( E;|�E<^fE?� EB��EE�3EIA�ELFfEO33ERi�EU�3EX� E[��E^��Eb#3Ee�Eh>fEk>fEn��Eq��Et� Ex�E{fE}�3E���E�"fE�ŚE�D E�ݚE� E�rfE�� E�  E�\�E���E� G�O�?fffG�O�G�O�G�O�?333G�O�G�O�?L��G�O�G�O�?L��G�O�?L��G�O�?L��G�O�G�O�?fffG�O�?fffG�O�?�  G�O�?���?�ff?�  ?���?ٙ�?�33@   @��@��@   @333@9��@L��@fff@l��@�  @�ff@�33@���@�ff@�33@�  @�  @���@�ff@�33A   A��A��AffA33A#33A+33A1��A8  AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441441441414144141414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ �@ �@ V@ *@ �@ "�@ *S@ 0x@ 7L@ >@ F�@ SI@ `�@ m�@ z�@ �7@ �0@ �z@ �-@ �2@ �*@ �t@ �m@ ��@@b@
@-@:@H]@V@b�@qS@�@��@��@��@�9@�>@є@ލ@�4@��@1@�@"�@/�@<�@K@X�@g@t�@�d@�\@�@�M@��@�J@�O@�@�@�9@
=@�@$�@1'@@,@N�@\�@k.@x&@��@�@�m@��@��@�c@��@�@�Y@ �@@O@(G@7�@C�@P�@_�@n�@{�@��@��@��@�-@�&@�|@�t@�@� @@@�@+@:@I@V@bN@qS@~�@��@��@��@��@��@�7@ލ@��@��@v@{@"�@1'@>�@M$@X�@dZ@r@�@�\@�@�@�^@ƨ@�C@��@�@@��@
�@�@(G@3�@?}@M�@\�@j@y�@��@�h@��@�@�@�c@խ@�@�@^@V@�@(�@7�@DD@P�@_�@n�@{�@��@��@��@��@��@��@��@��@��@	@	@	 �@	-@	9X@	G�@	Wb@	b�@	o�@	~K@	�P@	�H@	�A@	�F@	�>@	��@	��@	�4@	��@
�@
*@
!s@
0x@
?}@
Lu@
X@
g@
v@
�d@
��@
�@
�@
�^@
�@
��@
�H@
�@
�E@J@�@'�@33@A�@P�@^�@k.@ww@�|@��@�(@�!@��@�@�[@�@��@  @�@�@)�@7�@FQ@S�@`B@k.@y�@��@�0@��@�-@��@�|@�#@��@�q@@�@g@-@:@G�@UU@bN@o�@|�@��@��@�@Yn@��@�/@ �@dZ@��@��@+@m�@��@�L@3�@uk@�@�9@<@|�@��@@A�@�@��@�@I@��@��@b@R�@�0@�t@ @e	@��@�@@5?@|?@�>@
=@P�@��@܀@!s@e	@��@�L@7L@z3@��@��@:@{�@�&@  @D�@�+@�@�@Lu@��@�@
�@K@�7@�@@B8@�@�j@��@ 6�@ r@ ��@ �(@!$�@!a�@!�@!��@"
@"[z@"�H@"�h@#6@#V@#�#@#Ӡ@$o@$P�@$�\@$ψ@%V@%M�@%�\@%�7@&@&R�@&�#@&Ӡ@'�@'V@'�<@'��@(�@(X�@(�<@(�@)�@)Z@)��@)�h@*�@*UU@*��@*�7@+J@+I�@+��@+��@,@,?}@,|?@,��@,�@--@-ff@-�z@-܀@.�@.P�@.��@.@.�,@/0x@/ff@/�@/Ӡ@0�@0B8@0z3@0��@0�(@1"�@1[z@1�0@1�*@2%@2>�@2x&@2��@2��@3$/@3]�@3�0@3�7@4	�@4E�@4~�@4�R@4�@5+@5dZ@5��@5є@61@6?}@6v�@6�!@6�@7[@7S�@7��@7�2@7� @8i!@9J@9x�@:O@:��@;&�@;��@<3�@<�C@=;d@=�t@>C�@>��@?�d@?��@@��@@��@A�a@B1@B��@C�@C�2@D1'@D�O@EB�@E��@FYn@F��@Gs_@G�@H��@H�q@I��@J�@J�@K8�@K�@L5�@L��@M+�@M�@NH]@N�#@Oi�@O�9@P[z@Q��@S$/@Tg�@U�#@W$�@XdZ@Y�>@[B@\ff@]�!@_�@`x�@a��@c�@d[z@e�o@gJ@hi�@i�|@k6@lZ@m�F@o�@pn�@q��@so@sDD@s�h@s�t@t%�@tYn@t��@t��G�O�@ �G�O�G�O�G�O�@ ^G�O�G�O�@ G�O�G�O�@ G�O�@ G�O�@ G�O�G�O�@ �G�O�@ �G�O�@ jG�O�@ �@ v@ �@ �@ 1@ 	�@ 
=@ �@ �@ �@ �@ b@ o@ *@ �@ �@ B@ �@ 
@  @ "�@ %�@ (�@ +�@ -�@ 0x@ 33@ 5?@ 8�@ <�@ >�@ B8@ E�@ H]@ K@ O0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�VA�oA��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�JA�  A��#A�p�A�ffA�r�A�;dA��!A��\A��#A�oA���A�p�A���A�1A��HA��uA�z�A�M�A��yA�jA��A��A���A�;dA��PA�  A�l�A�ȴA�~�A��hA���A�^5A�VA���A�S�A�A�O�A��yA�1'A��hA�O�A�=qA���A�ƨA�?}A��HA�t�A���A��;A�hsA�t�A��HA��`A���A���A���A�jA��A�(�A�1A��#A�&�A���A���A�M�A�`BA���A��A�K�A��^A���A�(�A�ffA��
A�Q�A���A�/A�ȴA�\)A��#A�|�A+A}XA|�yA{�
Az�`AzVAyS�Ax�jAw�mAv�HAu7LAtZAr�Ar�uArQ�AqO�ApjAo�7Am�TAl{Ah��Ag?}Af��AeG�Ac�FAbZA`  A_?}A^�HA^$�A\�RA[%AY|�AXA�AW�AV��AS��ASS�AR��AQ�TAPVAN�!ANAM��AL��AK�AJn�AG�#AF  AE�AD��ADZAD1'AC?}AA�TA>��A;��A;%A:r�A8��A7�A7%A6�A6�yA6�`A6�jA5;dA3�A2^5A1VA/�A.�jA.�A-�7A,�A,1'A+�A*��A*bNA*�A)�hA)�A(A�A'��A'
=A%�7A$ �A"��A!33A�A��Al�A��A�^Ap�A&�A��A�A��A9XA��AK�A��A �A?}Ax�A�PA�RA7LAA�A�yAjA��A	ƨA�^A��A�\AVAJAƨAG�A=qA33A~�A��AdZA �A �DA �@��y@��@��@��+@�x�@�Q�@�33@��#@�"�@�`B@�(�@�o@��#@�j@�@��#@�V@�9X@��y@�M�@��#@��@�p�@�D@�A�@���@�K�@�+@�=q@�^5@�v�@�Q�@�9X@š�@�-@��@���@�E�@�X@�v�@���@���@��
@��@��@���@�ƨ@�~�@��@�ff@�Ĝ@��/@��
@�$�@��F@��@�Ĝ@��;@���@��@��w@�v�@���@�  @�"�@��@��9@�(�@�E�@��@�  @�"�@�n�@�bN@��P@���@��#@��@�j@��F@�^5@�`B@�@�P@}�@|�/@{�@y��@v�@u�-@r�H@p�@n�+@l�@j��@hQ�@g��@e�T@d9X@c��@b�@aG�@_\)@^��@]�h@\j@[t�@Z�H@Y�@WK�@V�+@Up�@T�j@S��@Qhs@P��@N��@M?}@L�/@K33@I�#@Hr�@G�@G\)@E�h@D�j@B�\@@Ĝ@?K�@>$�@<�@<9X@;33@9�^@8A�@6��@5@41@2��@1��@1��@1%@0r�@01'@/�@.ȴ@.E�@,��@+��@+dZ@*��@)�@(Ĝ@'�@&��@%�T@$�/@#��@#33@!��@ �`@��@l�@5?@/@j@ƨ@�!@�#@r�@��@ff@$�@p�@�/@�
@t�@�H@x�@%@bN@��@+@$�@�h@?}@�j@�@�@
��@	&�@��@�9@�;@l�@;d@+@�@5?@E�@v�@ff@��@�@��@G�@ b?��?���?�?�S�?���??��-?�ƨ?��#?�9?�Q�?�
=?��?��
?��?�hs?�|�?�{?�V?�1?��#?��?��?ա�?�9X?�n�?щ7?�Ĝ?��;?�|�?�|�?�;d?��?�5??��?��m?�"�?���?��?��#?��?��y?š�?ě�?�S�?�o?���?���?�bN?���?�O�?�V?�I�?��m?���?�=q?��#?�x�?�7L?��^?���?��?�~�?��H?��?�I�?��?�/?�O�?�p�?��h?��-?��?��A��A��A�oA�oA�oA�oA�oA�bA�{A�oA�{A�{A�oA�{A�oA�oA�VA�bA�bA�bA�
=A�JA�1A�
=A�1A�JA�oA�bA�oA�bA�VA�bA�VA�oA�{A��A��A��A��A�{A�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�oA�VA�oA��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�JA�  A��#A�p�A�ffA�r�A�;dA��!A��\A��#A�oA���A�p�A���A�1A��HA��uA�z�A�M�A��yA�jA��A��A���A�;dA��PA�  A�l�A�ȴA�~�A��hA���A�^5A�VA���A�S�A�A�O�A��yA�1'A��hA�O�A�=qA���A�ƨA�?}A��HA�t�A���A��;A�hsA�t�A��HA��`A���A���A���A�jA��A�(�A�1A��#A�&�A���A���A�M�A�`BA���A��A�K�A��^A���A�(�A�ffA��
A�Q�A���A�/A�ȴA�\)A��#A�|�A+A}XA|�yA{�
Az�`AzVAyS�Ax�jAw�mAv�HAu7LAtZAr�Ar�uArQ�AqO�ApjAo�7Am�TAl{Ah��Ag?}Af��AeG�Ac�FAbZA`  A_?}A^�HA^$�A\�RA[%AY|�AXA�AW�AV��AS��ASS�AR��AQ�TAPVAN�!ANAM��AL��AK�AJn�AG�#AF  AE�AD��ADZAD1'AC?}AA�TA>��A;��A;%A:r�A8��A7�A7%A6�A6�yA6�`A6�jA5;dA3�A2^5A1VA/�A.�jA.�A-�7A,�A,1'A+�A*��A*bNA*�A)�hA)�A(A�A'��A'
=A%�7A$ �A"��A!33A�A��Al�A��A�^Ap�A&�A��A�A��A9XA��AK�A��A �A?}Ax�A�PA�RA7LAA�A�yAjA��A	ƨA�^A��A�\AVAJAƨAG�A=qA33A~�A��AdZA �A �DA �@��y@��@��@��+@�x�@�Q�@�33@��#@�"�@�`B@�(�@�o@��#@�j@�@��#@�V@�9X@��y@�M�@��#@��@�p�@�D@�A�@���@�K�@�+@�=q@�^5@�v�@�Q�@�9X@š�@�-@��@���@�E�@�X@�v�@���@���@��
@��@��@���@�ƨ@�~�@��@�ff@�Ĝ@��/@��
@�$�@��F@��@�Ĝ@��;@���@��@��w@�v�@���@�  @�"�@��@��9@�(�@�E�@��@�  @�"�@�n�@�bN@��P@���@��#@��@�j@��F@�^5@�`B@�@�P@}�@|�/@{�@y��@v�@u�-@r�H@p�@n�+@l�@j��@hQ�@g��@e�T@d9X@c��@b�@aG�@_\)@^��@]�h@\j@[t�@Z�H@Y�@WK�@V�+@Up�@T�j@S��@Qhs@P��@N��@M?}@L�/@K33@I�#@Hr�@G�@G\)@E�h@D�j@B�\@@Ĝ@?K�@>$�@<�@<9X@;33@9�^@8A�@6��@5@41@2��@1��@1��@1%@0r�@01'@/�@.ȴ@.E�@,��@+��@+dZ@*��@)�@(Ĝ@'�@&��@%�T@$�/@#��@#33@!��@ �`@��@l�@5?@/@j@ƨ@�!@�#@r�@��@ff@$�@p�@�/@�
@t�@�H@x�@%@bN@��@+@$�@�h@?}@�j@�@�@
��@	&�@��@�9@�;@l�@;d@+@�@5?@E�@v�@ff@��@�@��@G�@ b?��?���?�?�S�?���??��-?�ƨ?��#?�9?�Q�?�
=?��?��
?��?�hs?�|�?�{?�V?�1?��#?��?��?ա�?�9X?�n�?щ7?�Ĝ?��;?�|�?�|�?�;d?��?�5??��?��m?�"�?���?��?��#?��?��y?š�?ě�?�S�?�o?���?���?�bN?���?�O�?�V?�I�?��m?���?�=q?��#?�x�?�7L?��^?���?��?�~�?��H?��?�I�?��?�/?�O�?�p�?��h?��-?��?��A��A��A�oA�oA�oA�oA�oA�bA�{A�oA�{A�{A�oA�{A�oA�oA�VA�bA�bA�bA�
=A�JA�1A�
=A�1A�JA�oA�bA�oA�bA�VA�bA�VA�oA�{A��A��A��A��A�{A�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�PBt�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�VB�+B�B~�B|�By�Br�Bp�Bl�BdZBbNB_;BYBS�BN�BM�BD�B0!B!�BB�B�5BǮB�3B��B��B�uB�Bk�B\)BN�B?}B,B&�B�B  B
�sB
�)B
ƨB
�dB
�B
��B
��B
��B
x�B
dZB
]/B
G�B
;dB
7LB
1'B
.B
'�B
%�B
0!B
;dB
=qB
;dB
33B
,B
)�B
$�B
�B
{B
JB
B	�B	�;B	�B	��B	ȴB	�jB	�-B	��B	��B	��B	��B	��B	�DB	�B	{�B	y�B	p�B	aHB	_;B	XB	T�B	H�B	D�B	>wB	<jB	8RB	5?B	1'B	#�B	 �B	�B	�B	�B	uB	bB	1B��B�yB�TB�5B��B��B��B��B��B��B��BŢBB�^B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�VB�7B�%B�%B�B� B}�Bz�Bw�Bu�Bt�Bs�Bq�Bm�B]/BXBN�BF�B@�B8RB7LB7LB0!B0!B/B.B-B,B,B+B(�B/B6FB:^B:^B;dB;dB:^B:^B9XB6FB33B1'B1'B1'B/B.B.B-B,B,B,B,B.B.B1'B1'B1'B2-B33B33B5?B6FB7LB6FB6FB5?BI�BK�B]/Be`Bo�B�B�1B�{B��B�-BǮB��B�B��B	+B	bB	bB	&�B	49B	?}B	D�B	L�B	N�B	ZB	e`B	u�B	|�B	�=B	�\B	��B	��B	��B	��B	�3B	�LB	�^B	�}B	ŢB	ǮB	��B	�
B	�B	�/B	�5B	�HB	�mB	�yB	�B	�B	�B	�B	��B	��B	��B
B
B
B
%B
1B
DB
JB
\B
bB
oB
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
"�B
%�B
%�B
&�B
%�B
'�B
(�B
)�B
+B
-B
.B
1'B
1'B
33B
49B
33B
7LB
8RB
9XB
:^B
:^B
<jB
=qB
>wB
A�B
A�B
B�B
C�B
B�B
D�B
F�B
H�B
I�B
J�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
Q�B
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
\)B
\)B
^5B
_;B
`BB
`BB
bNB
bNB
cTB
dZB
e`B
e`B
gmB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
m�B
l�B
m�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
s�B
s�B
u�B
t�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
y�B
~�B
� B
� B
�B
�B
�+B
�%B
�+B
�+B
�7B
�DB
�PB
�PB
�PB
�PB
�\B
�oB
�oB
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
�B
�B
�B
�B
�B
�!B
�!B
�-B
�-B
�9B
�9B
�FB
�FB
�LB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�?B�4Bt�B�qB�~B�~B��B��B��B�yB��B�hB��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�fB�HB�B�B~�B|�By�Br�Bp�Bl�BdQBbEB_3BYBS�BN�BM�BD�B0B!�BB�B�1BǫB�0B��B��B�tB�Bk�B\)BN�B?~B,	B&�B�B B
�vB
�,B
ƬB
�hB
� B
��B
��B
��B
x�B
daB
]7B
G�B
;mB
7UB
11B
.B
'�B
%�B
0-B
;pB
=~B
;qB
3AB
,B
*B
$�B
�B
�B
[B
B	�B	�MB	�#B	��B	��B	�~B	�AB	��B	��B	��B	��B	��B	�[B	�1B	{�B	y�B	p�B	abB	_UB	X+B	UB	H�B	D�B	>�B	<�B	8pB	5]B	1FB	#�B	 �B	�B	�B	�B	�B	�B	TB��B�B�xB�YB�B�
B�B��B��B��B��B��B¸B��B�]B�EB�3B�-B�B�B��B��B��B��B�B� B�&B�'B�!B�	B��B��B��B��B��B��B��B�lB�[B�\B�IB�8B~,B{BxBu�Bt�Bs�Bq�Bm�B]kBXLBOBF�B@�B8�B7�B7�B0aB0aB/[B.UB-OB,JB,KB+EB):B/_B6�B:�B:�B;�B;�B:�B:�B9�B6�B3|B1qB1qB1rB/fB.`B.`B-[B,UB,VB,WB,WB.dB.dB1xB1xB1yB2B3�B3�B5�B6�B7�B6�B6�B5�BJBL%B]�Be�BpB�oB��B��B�NB��B�%B�SB�B�yB	�B	�B	�B	'tB	4�B	@B	E/B	McB	OrB	Z�B	e�B	vdB	}�B	��B	�B	�:B	�hB	��B	��B	��B	�B	�B	�?B	�gB	�vB	ЪB	��B	��B	�B	�B	�"B	�JB	�YB	�tB	��B	�B	�B	��B	��B	��B
�B
 B
B
%B
	4B
IB
RB
gB
pB
B
�B
�B
�B
�B
�B
 �B
 �B
"�B
"�B
"�B
#�B
'B
'B
(B
'B
)+B
*4B
+<B
,EB
.TB
/]B
2rB
2uB
4�B
5�B
4�B
8�B
9�B
:�B
;�B
;�B
=�B
>�B
?�B
B�B
B�B
DB
EB
D
B
FB
H)B
J8B
KAB
LJB
NYB
ObB
OeB
OgB
PpB
PsB
Q{B
Q~B
R�B
S�B
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
]�B
]�B
_�B
aB
bB
bB
dB
d B
e)B
f1B
g:B
g<B
iKB
kZB
k\B
k_B
lgB
ljB
mrB
muB
mwB
o�B
n�B
o�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
u�B
u�B
w�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
{B
|B
�4B
�?B
�FB
�WB
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
�B
�B
�B
�"B
�)B
�4B
�AB
�LB
�`B
�_B
�jB
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
�B
�
B
�B
�B
�'B
�EB
�`B
�tB
��B
��B
��B
��B
��B
��B
�
B
�B
�6B
�DB
�`B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�&B
�0B
�3B
�6B
�9B
�<B
�@B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313462021061413553720210614135537202106171312482021061713124820210617131248201809172313462021061413553720210614135537202106171312482021061713124820210617131248PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134620180917231346  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134620180917231346QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134620180917231346QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150220210617131502IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                