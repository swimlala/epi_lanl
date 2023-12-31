CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:45Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  PP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                       HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 0      � 0Argo profile    3.1 1.2 19500101000000  20180917231345  20210722160152  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�{�<�*@�{�<�*11  @�{�8�@@�{�8�@@6Ac
�@6Ac
��c�J���E�c�J���E11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
AB  AA  AA  ?�  @   @Fff@�33@�  @�  @�  A��A  A&ffA@  A^ffA���A���A�  A�  A���A�  A�  A�  B ffB��B  BffB   B(ffB0  B8  B@ffBG��BP  BXffB`ffBg��Bp  BxffB�  B�  B�  B�  B�  B���B�  B�  B�33B�ffB�ffB�  B���B�  B�ffB�33B�33B�  B�  B�33B�33B�33B�33B�ffBߙ�B�  B�  B�  B�  B���B���B�  C   C�fC33CL�C33C
33C  C  C��C�C  C��C�C  C��C�C   C!�fC$33C&33C(  C*  C+�fC.33C0L�C233C433C5�fC8L�C:L�C<33C>  C?�fCB33CD�CF  CH33CJ  CL  CN33CP�CR  CS�fCU��CX�CZ�C\  C^33C`33Ca�fCd33Cf  Cg��Cj  Cl33Cn  Co��Cr  CtL�Cv�Cx  Cy�fC{�fC~�C�fC��fC�  C��C�  C��3C��C��C�  C�  C��3C��C��C�  C�  C��3C�  C�&fC��C�  C��3C��fC�  C�&fC��C�  C��C�  C��fC��C�&fC��C�  C��C�  C��fC��3C��C��C�  C��3C�  C��C�  C��3C��C��C�  C��C��C�  C��C��C��3C�&fC��C�  C�&fC�&fC��C��C��C�  C��3C�&fC��C��C��C��C��C��C��C��C��C�  C�  C��3C��C��C��C�  C��3C�&fC��C�  C�  C��3C��C��C��3C��C��C�  C�&fC��C��C�  C��fC��C�  C��fC��C��C�  C�  C��3C��C�&fC��C��3C��C�&fC��C��3C��C��C�  C��3C��C��C��C��fC�  C�&fC�ٚC��fC�  C��D fD �3D3Ds3D��Dy�DffD	�fD��D  D�fD9�D�3Ds3D  D�fD!@ D#��D&��D),�D+� D.� D1,�D3�fD6� D9FfD;��D>��DA�DC�fDE��DHy�DK  DM� DO��DRl�DT�3DWl�DY��D\l�D^��Da��Dd@ Df��Dil�Dl3Dn� DqFfDs� Dv��Dy33D{ffD~3D�Y�D��3D��D�&fD�` D���D��fD� D�FfD�� D��3D�� D��D�@ D�p D��fD��3D�fD�33D�i�D���D�� D��D�L�D�� D��3D��3D�&fD�VfD��3D��fD�3D�S3D���D�� D�,�D�vfD���D�,�D��3D�� D�0 D���D��fD�&fD�|�D�� D��D�l�D���D�	�D�S3D��3D��D�33D�|�D��3D� D�P DʖfD��3D�	�D�C3Dπ DжfD��D��D�C3D�i�D֐ D׹�D���D���D��D�9�D�VfD�l�D߆fD� D�3D���D���D���D�3D�0 D�@ D�P D�` D�p D�|�D�3D�3D� D��fD��fD��D�  D�3D�&fD�33D�L�D�\�D�l�D�i�D�y�D���D���D���E [3E �3Eh E��Eq�E�fEy�E3E�3E�E�fEfE� E$�E�fE��E	� E^fEl�E	�E3E1�EFfE�fE�E�E��E�3E�3E��E��E��E�3E� E �3E!�fE"�fE$�3E%��E&�3E'�E)�E*�fE+ٚE,�3E. E/�3E0�3E28 E36fE4� E5�3E6��E8[3E9D�E:��E;�3E>�EB4�EE3EH` EK�3EN��EQ�3EU  EX1�E[�E^a�Ea�3Ed��Eg� Ej��En�Eq Et�Ew  Ez` E}�3E�VfE�� E�bfE��3E��3E�3E���E�, E�ɚE�`�E��fE�o3E���E�a�E���E��3E�33E��3E���E�&fE��fE���E��E�| ?333?333?333?L��?L��?L��?fff?�  ?���?�ff?�33?�33?���?���?ٙ�?�ff@   @   @��@33@��@,��@9��@L��@`  @l��@�33@�  @�ff@�  @���@�33@�  @ə�@�ff@�  @���@���A33A	��A  AffAffA&ffA,��A4��A;33AC33AI��AQ��AX  A^ffAd��Ak33As33Ay��A���A�  A�33A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441441111141411141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?�  @   @fff@�33@�  @�  @�  A	��A  A.ffAH  AfffA���A���A�  A�  A���A�  A�  A�  BffB	��B  BffB"  B*ffB2  B:  BBffBI��BR  BZffBbffBi��Br  BzffB�  B�  B�  B�  B�  B���B�  B�  B�33B�ffB�ffB�  B���B�  B�ffB�33B�33B�  B�  B�33B�33B�33B�33B�ffB���B�  B�  B�  B�  B���B���B�  C � CffC�3C��C�3C
�3C� C� CL�C��C� CL�C��C� CL�C��C � C"ffC$�3C&�3C(� C*� C,ffC.�3C0��C2�3C4�3C6ffC8��C:��C<�3C>� C@ffCB�3CD��CF� CH�3CJ� CL� CN�3CP��CR� CTffCVL�CX��CZ��C\� C^�3C`�3CbffCd�3Cf� ChL�Cj� Cl�3Cn� CpL�Cr� Ct��Cv��Cx� CzffC|ffC~��C�33C�&fC�@ C�Y�C�@ C�33C�Y�C�Y�C�@ C�@ C�33C�Y�C�L�C�@ C�@ C�33C�@ C�ffC�Y�C�@ C�33C�&fC�@ C�ffC�Y�C�@ C�Y�C�@ C�&fC�L�C�ffC�Y�C�@ C�Y�C�@ C�&fC�33C�L�C�Y�C�@ C�33C�@ C�Y�C�@ C�33C�Y�C�L�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�ffC�Y�C�@ C�ffC�ffC�Y�C�Y�C�L�C�@ C�33C�ffC�Y�C�Y�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�@ C�@ C�33C�Y�C�L�C�L�C�@ C�33C�ffC�Y�C�@ C�@ C�33C�Y�C�Y�C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�&fC�L�C�@ C�&fC�L�C�L�C�@ C�@ C�33C�L�C�ffC�Y�C�33C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C�33C�L�C�Y�C�L�C�&fC�@ C�ffC��C�&fC�@ C�L�D &fD �3D33D�3D�D��D�fD
fD��D@ D�fDY�D�3D�3D  D�fD!` D$�D&��D)L�D,  D.� D1L�D4fD6� D9ffD<�D>��DA,�DC�fDF�DH��DK  DM� DP�DR��DU3DW��DZ�D\��D_�Da��Dd` Df��Di��Dl33Dn� DqffDt  Dv��DyS3D{�fD~33D�i�D��3D���D�6fD�p D���D��fD�  D�VfD�� D��3D�� D��D�P D�� D��fD��3D�fD�C3D�y�D���D�� D��D�\�D�� D��3D�3D�6fD�ffD��3D��fD�#3D�c3D���D�� D�<�D��fD���D�<�D��3D�� D�@ D���D��fD�6fD���D�� D�,�D�|�D�ɚD��D�c3D��3D���D�C3DŌ�D��3D�  D�` DʦfD��3D��D�S3Dϐ D��fD���D�)�D�S3D�y�D֠ D�ɚD���D�	�D�)�D�I�D�ffD�|�DߖfD� D��3D���D���D�	�D�#3D�@ D�P D�` D�p D� D��D��3D�3D�� D��fD��fD���D� D�#3D�6fD�C3D�\�D�l�D�|�D�y�D���D���D���D���E c3E �3Ep E��Ey�E�fE��E3E�3E�E�fE&fE� E,�E�fE��E	� EffEt�E�E#3E9�ENfE�fE��E�E��E�3E�3E��E��E��E�3E� E �3E!�fE"�fE$�3E%��E&�3E'�E)�E*�fE+�E,�3E. E/�3E0�3E2@ E3>fE4� E5�3E7�E8c3E9L�E:��E;�3E>�EB<�EE3EHh EK�3EN��EQ�3EU EX9�E[!�E^i�Ea�3Ed��Eg� Ej��En�Eq  Et�Ew( Ezh E}�3E�ZfE�� E�ffE��3E��3E�3E���E�0 E�͚E�d�E��fE�s3E���E�e�E���E��3E�73E��3E���E�*fE��fE���E��E�� G�O�G�O�?���G�O�G�O�?�ff?�33?�  ?ٙ�?�ffG�O�?�33G�O�@ff@��@33G�O�@   @,��@333@9��@L��@Y��@l��@�  @�ff@�33@�  @�ff@�  @���@�33@�  @ٙ�@�ff@�  @���AffA33A��A  AffA&ffA.ffA4��A<��AC33AK33AQ��AY��A`  AfffAl��As33A{33A���A���A�  A�33A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441441111141411141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ �@ *@ �@ ""@ (�@ /�@ 7L@ =q@ F�@ Q�@ ^�@ m�@ |?@ ��@ �0@ ��@ �~@ �&@ ��@ �#@ �m@ ��@@@g@,`@:@H]@T�@b�@qS@~�@�D@��@��@��@@�7@��@�@�~@�@{@"�@1'@>�@K@Wb@ff@uk@�d@�@�@��@�@ƨ@�O@��@�L@�9@
=@�@%�@33@@,@M�@\)@i�@v�@�|@��@��@�r@��@�c@խ@�`@�Y@��@V@O@'�@7L@DD@Q=@`�@n�@z�@��@��@�5@��@��@�*@��@�(@��@�@@
@-�@:�@G�@V�@b�@p�@�@��@��@��@��@�>@��@��@��@��@%@�@""@.l@=q@Lu@X�@e	@t@��@�@�@��@��@ƨ@��@��@��@��@
=@6@&�@4�@@�@N�@[z@k.@x&@�@��@��@�@��@��@�
@�@��@  @�@�@(�@7�@DD@P�@`B@oF@|?@��@��@��@�!@�w@�|@��@�@�@	j@	o@	�@	+�@	;d@	H]@	UU@	dZ@	qS@	~K@	�P@	�H@	��@	��@	��@	�7@	��@	�@	��@
1@
*@
""@
/@
?}@
Lu@
Z@
g@
t�@
�@
��@
�a@
�@
��@
��@
Ӡ@
��@
�L@
�E@
�@�@$�@5?@B8@N�@\)@i!@x�@�|@�@��@��@��@�o@�h@�`@�Y@��@V@O@'�@7L@D�@Q�@_�@l�@{�@��@��@�(@��@�2@�|@��@��@� @j@b@g@-�@:�@FQ@UU@e	@n�@|�@��@�H@��@��@Ĝ@��@�/@��@qS@��@��@DD@�7@ψ@�@^5@��@�4@33@|?@�J@�@UU@�@�@0x@z�@�>@�@SI@��@�#@
@bN@�A@�@/@r@��@��@>�@�@ȴ@b@X�@�a@�@.l@t@�j@j@Lu@��@��@�@a�@��@�@1'@t@��@��@=q@�@@@DD@�p@��@�@I@�7@��@ 
�@ M$@ ��@ �7@!�@!X@!��@!�#@"g@"`�@"��@"�`@#*S@#m�@#�-@#�~@$=q@$�p@$��@%�@%^�@%��@%�Y@&:@&��@&��@'o@'[z@'��@'��@(2�@(y�@(�2@)�@)O0@)��@)�#@*!s@*g@*�@*�Y@+7�@+{�@+��@, �@,DD@,�|@,�@-�@-H]@-�+@-��@.v@.C�@.�W@.��@.�9@/7�@/s_@/�r@/�@0&;@0bN@0�U@0�@1*@1Q�@1��@1��@2  @2:@2s_@2��@2��@3""@3]�@3��@3�C@4�@4H]@4�@4�j@4�~@52�@5l�@5�z@5܀@6�@6Q=@6�D@6��@6��@76�@7oF@7��@7��@8�@8SI@8��@8Ĝ@8�Q@99X@9p�@9�M@9��@:S�@:��@;v�@;�(@<�I@=@=��@=��@>��@?g@?�<@@M$@@��@A;d@A�@Bc�@Bލ@CZ@D�@D�|@E�@E}�@F4�@F��@G&�@G��@H �@H��@IR�@I��@J@�@J��@K]�@L
=@Lv�@M{@M|?@N@N��@OJ@O��@P*S@QqS@R܀@T�@U~K@V�@X �@Y�@Z�H@\> @]{�@^��@`B8@a�@bӠ@d<@e��@f��@h"�@ip�@jӠ@l;d@m��@n@pZ@qi!@r��@t$�@ux�@v@x#�@y~�@zě@|�@}g�@}��@~ �@~<�@~v�@~ψ@
�@FP@�@�#@�
�@�4�G�O�G�O�@ �G�O�G�O�@ v@ %@ �@ 1@ �G�O�@ 	�G�O�@ 
�@ �@ JG�O�@ �@ @ �@ b@ o@ �@ �@ �@ B@ �@ �@  @ ""@ $�@ &;@ (�@ +@ -�@ /�@ 2�@ 5�@ 7�@ :�@ =q@ @,@ C�@ F�@ I�@ M$@ O�@ SI@ V@ Yn@ \)@ ^�@ a�@ dZ@ g�@ j@ m�@ p�@ s_@ wwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AۮAۮA۰!A۬A۩�A۬A۬AۮAۮAۧ�Aۣ�Aۛ�A�|�A�bNA��Aէ�A�-A�A�"�Aк^A�C�A��#A�~�A�A�A��A��A�x�AͼjA���Aǝ�A���A��DA��A��;A��A�ZA���A�ƨA���A��!A���A�;dA��A���A�ƨA�Q�A�jA��A��jA��PA���A�+A���A��PA�p�A�`BA��A�O�A���A�I�A� �A�bA��`A�9XA���A�E�A���A��-A��hA��HA�M�A�z�A�v�A�A�7LA��hA�r�A�hsA�ZA�v�A���A�jA��/A��+A�%A��-A�dZA�A�9XA���A��A��A���A�G�A�~�A���A�I�A�~�A�Q�A��A�?}A���A��A��PA���A�|�A�1'A�$�A��A�-A�A��A���A�=qA���A���A��DA�ƨA~ZA}�A|A�A{7LAz��Ayl�AwVAt�Aq�AoAnVAlbNAj��Ah�!Afn�Ae�^Ad��Ac�Aa��A]\)AZ��AY�AY�AXA�AW%AT�\ASoAQS�AN�AM�wAL�HAK�AJ-AI��AI��AH�`AHZAGp�AF��AE"�AC�AC�AC"�AB5?A?��A?A>�jA>��A>E�A<��A:�!A9��A8�yA7��A65?A41A3�A3t�A2��A0��A/33A.��A.  A-K�A,�RA+t�A)�A(v�A(JA'�hA&��A&�A%?}A#dZA"5?A!�A �uA (�A�A�A1'A��A�A�-A=qAp�AȴA=qA7LA�-A-A�HAr�A�A�A �A��AO�A�A �A&�A
�9A
E�A
bA	ƨA	+A�AbA�AE�A1A��A�Ar�Av�AG�A ^5@�
=@���@�@���@��T@�o@���@�;d@��T@�Z@�b@�ƨ@��@���@��;@�x�@�V@���@�@�j@�!@�z�@�33@�=q@ܼj@��@���@��@�-@Դ9@�V@ȓu@��m@�=q@�@�C�@ă@�J@���@�Ĝ@�&�@�Z@��H@���@�5?@�J@��@��y@�b@��9@�@�o@�@��;@��h@�I�@�"�@��h@���@�+@���@�z�@�l�@�K�@�n�@���@�%@���@��m@��w@�J@�G�@��`@�Q�@�S�@�M�@�?}@��u@|�@~��@|�@z�@w\)@v@uO�@r��@r~�@oK�@l��@k��@ihs@i�@g�@e��@b��@b=q@aG�@a&�@_+@^ff@^{@]O�@[�
@["�@XA�@V��@T�j@R��@O�;@M@L�@L(�@K33@HbN@Fv�@D�D@B^5@A��@@�9@?|�@>5?@=�@;�@:n�@9hs@9%@8Q�@7\)@6ȴ@4�@2-@1G�@0Q�@/��@/
=@.V@.5?@,j@+"�@*M�@)�@(1'@'��@&E�@%�T@%`B@$I�@"�\@!�#@ Q�@l�@��@��@O�@Z@��@"�@�@�@K�@
=@�T@�@1@��@J@�7@�`@  @�P@�@v�@`B@/@�@j@�@
�H@	�#@	��@	7L@	7L@��@��@v�@@`B@��@��@t�@C�@@^5@��@��@ �`@ �?��w?���?�;d?��?�1?��?�ff?��?���?�&�?�p�?�=q?�X?�ff?�?}?�Z?㕁?��?�J?�Ĝ?��?�O�?��?���?ۥ�?ۥ�?�ƨ?�ƨ?�^5?�1'?��y?ӕ�?ҏ\?�G�?�G�?�G�?�A�?�V?�5??Ͳ-?�V?��?��m?��?���?���?�+?Ł?�t�?�hs?�\)?�{?��?��?�V?��?�I�?�(�?��?�dZ?�C�?�"�?�~�?�^5?�^5?��H?�?�?��?��m?�j?���?�O�?���?���?�;d?��;?��?�&�?���?��?�J?�-?�M�?\?°!?���?��?�33?�33?�33A۩�AۮA۩�AۮA۰!A۰!A۲-AۮA۰!AۮAۮAۮA۬AۮAۮAۮA۬A۰!AۮA۰!A۰!A۰!A۰!A۰!A۰!AۮA۬A۬A۩�A۬A۩�A۬A۬AۮA۬A۩�AۮAۮAۮAۮAۮA۬A۩�A۩�Aۥ�Aۡ�Aۣ�Aۣ�Aۣ�Aۣ�Aۡ�Aۛ�Aۗ�AۑhAہA�~�AہA�x�A�v�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 AۮAۮA۰!A۬A۩�A۬A۬AۮAۮAۧ�Aۣ�Aۛ�A�|�A�bNA��Aէ�A�-A�A�"�Aк^A�C�A��#A�~�A�A�A��A��A�x�AͼjA���Aǝ�A���A��DA��A��;A��A�ZA���A�ƨA���A��!A���A�;dA��A���A�ƨA�Q�A�jA��A��jA��PA���A�+A���A��PA�p�A�`BA��A�O�A���A�I�A� �A�bA��`A�9XA���A�E�A���A��-A��hA��HA�M�A�z�A�v�A�A�7LA��hA�r�A�hsA�ZA�v�A���A�jA��/A��+A�%A��-A�dZA�A�9XA���A��A��A���A�G�A�~�A���A�I�A�~�A�Q�A��A�?}A���A��A��PA���A�|�A�1'A�$�A��A�-A�A��A���A�=qA���A���A��DA�ƨA~ZA}�A|A�A{7LAz��Ayl�AwVAt�Aq�AoAnVAlbNAj��Ah�!Afn�Ae�^Ad��Ac�Aa��A]\)AZ��AY�AY�AXA�AW%AT�\ASoAQS�AN�AM�wAL�HAK�AJ-AI��AI��AH�`AHZAGp�AF��AE"�AC�AC�AC"�AB5?A?��A?A>�jA>��A>E�A<��A:�!A9��A8�yA7��A65?A41A3�A3t�A2��A0��A/33A.��A.  A-K�A,�RA+t�A)�A(v�A(JA'�hA&��A&�A%?}A#dZA"5?A!�A �uA (�A�A�A1'A��A�A�-A=qAp�AȴA=qA7LA�-A-A�HAr�A�A�A �A��AO�A�A �A&�A
�9A
E�A
bA	ƨA	+A�AbA�AE�A1A��A�Ar�Av�AG�A ^5@�
=@���@�@���@��T@�o@���@�;d@��T@�Z@�b@�ƨ@��@���@��;@�x�@�V@���@�@�j@�!@�z�@�33@�=q@ܼj@��@���@��@�-@Դ9@�V@ȓu@��m@�=q@�@�C�@ă@�J@���@�Ĝ@�&�@�Z@��H@���@�5?@�J@��@��y@�b@��9@�@�o@�@��;@��h@�I�@�"�@��h@���@�+@���@�z�@�l�@�K�@�n�@���@�%@���@��m@��w@�J@�G�@��`@�Q�@�S�@�M�@�?}@��u@|�@~��@|�@z�@w\)@v@uO�@r��@r~�@oK�@l��@k��@ihs@i�@g�@e��@b��@b=q@aG�@a&�@_+@^ff@^{@]O�@[�
@["�@XA�@V��@T�j@R��@O�;@M@L�@L(�@K33@HbN@Fv�@D�D@B^5@A��@@�9@?|�@>5?@=�@;�@:n�@9hs@9%@8Q�@7\)@6ȴ@4�@2-@1G�@0Q�@/��@/
=@.V@.5?@,j@+"�@*M�@)�@(1'@'��@&E�@%�T@%`B@$I�@"�\@!�#@ Q�@l�@��@��@O�@Z@��@"�@�@�@K�@
=@�T@�@1@��@J@�7@�`@  @�P@�@v�@`B@/@�@j@�@
�H@	�#@	��@	7L@	7L@��@��@v�@@`B@��@��@t�@C�@@^5@��@��@ �`@ �?��w?���?�;d?��?�1?��?�ff?��?���?�&�?�p�?�=q?�X?�ff?�?}?�Z?㕁?��?�J?�Ĝ?��?�O�?��?���?ۥ�?ۥ�?�ƨ?�ƨ?�^5?�1'?��y?ӕ�?ҏ\?�G�?�G�?�G�?�A�?�V?�5??Ͳ-?�V?��?��m?��?���?���?�+?Ł?�t�?�hs?�\)?�{?��?��?�V?��?�I�?�(�?��?�dZ?�C�?�"�?�~�?�^5?�^5?��H?�?�?��?��m?�j?���?�O�?���?���?�;d?��;?��?�&�?���?��?�J?�-?�M�?\?°!?���?��?�33?�33?�33A۩�AۮA۩�AۮA۰!A۰!A۲-AۮA۰!AۮAۮAۮA۬AۮAۮAۮA۬A۰!AۮA۰!A۰!A۰!A۰!A۰!A۰!AۮA۬A۬A۩�A۬A۩�A۬A۬AۮA۬A۩�AۮAۮAۮAۮAۮA۬A۩�A۩�Aۥ�Aۡ�Aۣ�Aۣ�Aۣ�Aۣ�Aۡ�Aۛ�Aۗ�AۑhAہA�~�AہA�x�A�v�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�JB
�JB
�JB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�\B
�{B
��B
�B�B/B^5Bz�By�By�By�By�Bx�Bx�Bv�Bw�Bn�B_;BL�BQ�BffBjBu�B}�B{�Bw�B�B�+B�7B�7B�=B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�\B�PB�=B�%B�B|�Bu�Bm�BdZB^5BN�B<jB1'B$�B{B	7B1BB��B��B�B�B�5B��B��B��B��B{�Bo�Be`BYBD�B:^B)�B�B�B"�B�B�BbB
��B
�B
�NB
�)B
�B
��B
��B
��B
ĜB
�B
�{B
�+B
q�B
hsB
`BB
XB
S�B
I�B
>wB
2-B
 �B
�B

=B	��B	�B	�)B	��B	ǮB	�wB	�-B	��B	�JB	�B	}�B	x�B	u�B	n�B	e`B	aHB	ZB	S�B	J�B	E�B	>wB	;dB	:^B	8RB	33B	0!B	-B	%�B	�B	�B	�B	{B	bB		7B	%B	B	B	  B��B�B�B�`B�NB�B��B��B��BƨB�}B�jB�^B�FB�3B�'B�B��B��B��B��B��B��B�hB�DB�DB�B�B~�B{�By�Bs�Bo�BjBffB_;B[#BYBW
BN�BM�BF�BC�BC�B@�B@�B@�B>wBB�B>wB=qB<jB:^B:^B8RB7LB8RB:^B<jB?}BD�BG�BG�BG�BB�BK�BB�B?}B9XB5?B5?B33B0!B.B/B0!B-B+B)�B)�B+B,B-B/B49B6FB5?B5?B33B7LB8RB8RB8RB:^B;dB=qB=qB=qBD�B]/Be`B��B��B�B�wB��B�B��B��B�B�BB�NB�B��B	�B	bB	+B	�B	M�B	?}B	e`B	y�B	}�B	�+B	�oB	�oB	�DB	��B	�B	�dB	ƨB	ȴB	ȴB	��B	��B	��B	�B	�5B	�fB	�sB	�B	�B	�B	��B	��B	��B	��B
  B
B
%B

=B
DB
JB
\B
VB
uB
�B
�B
�B
�B
�B
�B
 �B
�B
!�B
 �B
#�B
#�B
#�B
$�B
%�B
'�B
+B
,B
.B
0!B
33B
49B
5?B
6FB
7LB
:^B
<jB
=qB
?}B
?}B
@�B
B�B
C�B
D�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
K�B
N�B
N�B
P�B
O�B
P�B
Q�B
Q�B
S�B
T�B
VB
VB
XB
YB
YB
YB
YB
\)B
]/B
]/B
^5B
`BB
`BB
aHB
aHB
cTB
dZB
dZB
ffB
gmB
hsB
hsB
iyB
iyB
k�B
l�B
l�B
m�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
t�B
u�B
u�B
v�B
u�B
v�B
v�B
x�B
y�B
y�B
y�B
{�B
{�B
{�B
{�B
~�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�=B
�=B
�PB
�\B
�\B
�hB
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
��B
��B
�B
�B
�B
�!B
�'B
�3B
�3B
�3B
�9B
�?B
�FB
�FB
�LB
�LB
�LB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
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
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�jB
�dB
�jB
�jB
�PB
�JB
�PB
�JB
�PB
�JB
�JB
�PB
�JB
�JB
�JB
�PB
�PB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�JB
�JB
�=B
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�PB
�VB
�\B
�\B
�hB
�uB
�{B
�uB
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B
�=B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�PB
�oB
��B
�B�B-B\)Bx�Bw�Bw�Bw�Bw�Bv�Bv�Bt�Bu�Bl�B]/BJ�BO�BdZBhsBs�B{�By�Bu�B~�B�B�+B�+B�1B�bB�{B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B��B��B��B�\B�PB�PB�DB�1B�B�Bz�Bs�Bk�BbNB\)BL�B:^B/B"�BoB+B%B��B��B�B�B�sB�)B��B��B��B�uBy�Bm�BcTBW
BB�B8RB'�B�B�B �B�BuBVB
��B
�sB
�BB
�B
�
B
��B
��B
ȴB
B
�B
�oB
�B
o�B
ffB
^5B
VB
Q�B
G�B
<jB
0!B
�B
{B
1B	��B	�B	�B	��B	ŢB	�jB	�!B	��B	�=B	�B	{�B	v�B	s�B	l�B	cTB	_;B	XB	Q�B	H�B	C�B	<jB	9XB	8RB	6FB	1'B	.B	+B	#�B	�B	�B	�B	oB	VB	+B	B	B	  B��B�B�B�B�TB�BB�B��B��B��BĜB�qB�^B�RB�9B�'B�B��B��B��B��B��B��B��B�\B�7B�7B�B� B|�By�Bw�Bq�Bm�BhsBdZB]/BYBW
BT�BL�BK�BD�BA�BA�B>wB>wB>wB<jB@�B<jB;dB:^B8RB8RB6FB5?B6FB8RB:^B=qBB�BE�BE�BE�B@�BI�B@�B=qB7LB33B33B1'B.B,B.B/B+B)�B(�B(�B)�B+B,B.B33B5?B49B49B2-B6FB7LB7LB7LB9XB:^B<jB<jB<jBC�B\)BdZB��B��B��B�qB��B�
BɺB��B�
B�;B�HB�B��B	{B	\B	)�B	�B	L�B	>wB	dZB	x�B	|�B	�%B	�hB	�hB	�=B	��B	�B	�^B	ŢB	ǮB	ǮB	��B	��B	��B	�B	�/B	�`B	�mB	�yB	�B	�B	�B	��B	��B	��B	��B
B
B
	7B

=B
DB
VB
PB
oB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
"�B
"�B
"�B
#�B
$�B
&�B
)�B
+B
-B
/B
2-B
33B
49B
5?B
6FB
9XB
;dB
=qB
?}B
?}B
@�B
B�B
C�B
D�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
K�B
N�B
N�B
P�B
O�B
P�B
Q�B
Q�B
S�B
T�B
VB
VB
XB
YB
YB
YB
YB
\)B
]/B
]/B
^5B
`BB
`BB
aHB
aHB
cTB
dZB
dZB
ffB
gmB
hsB
hsB
iyB
iyB
k�B
l�B
l�B
m�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
t�B
u�B
u�B
v�B
u�B
v�B
v�B
x�B
y�B
y�B
y�B
{�B
{�B
{�B
{�B
~�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�DB
�DB
�VB
�bB
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
��B
��B
��B
�B
�B
�'B
�-B
�3B
�?B
�?B
�?B
�FB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
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
�}B
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�wB
�}B
�wB
�}B
�}B
�DB
�=B
�DB
�=B
�DB
�=B
�=B
�DB
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�7B
�=B
�=B
�1B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�JB
�DB
�JB
�PB
�PB
�\B
�hB
�oB
�hB
�uB
�uB
�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313452021061413523220210614135232202106141746372021061417463720210614174637201809172313452021061413523220210614135232202106141746372021061417463720210614174637PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134520180917231345  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134520180917231345QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134520180917231345QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015220210722160152IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                