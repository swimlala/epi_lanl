CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:33Z creation      
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
_FillValue                 4  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   (   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       |   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180724220233  20210722160149  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�i>3���@�i>3���11  @�i>-��0@�i>-��0@7	9��Y@7	9��Y�c�"&�r�c�"&�r11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?fff@ff@Fff@�  @�  @�33@�33A   A��A&ffAD��Ac33A�  A�  A���A�  A�  A�  A�  A�33A�33B  B  B  B   B(ffB0  B8  B@��BG��BPffBX��B`��Bh��Bp��BxffB�  B�  B�33B�ffB�33B���B���B�  B�33B�  B�  B���B���B���B�  B�33B�33B�ffB�33B�ffB�33B�ffB�33B�  B���B�ffB�ffB�  B���B�B�33B�  C   C�fC�fC33C  C	�fC33C�C  CL�C33C33C�C  C  C�fC 33C"33C$  C&  C'�fC*33C,L�C-��C/�fC233C433C6�C8�C:�C<�C>�C@33CB�CD�CF33CH33CJ33CL33CN�CP�CR�CT�CV  CX  CY�fC\33C^33C`33Cb33Cd  Cf  Cg�fCj33Cl�Cn  Cp33Cr�Cs��Cv�Cw�fCy��C{�fC~�C�&fC��C��3C��C�&fC��C�  C��C�  C��3C��C�&fC��3C��fC��3C�  C��C��C��3C��C��C��C�  C��C��C��3C��C�&fC��C��3C��C�&fC��C�  C�&fC�&fC��C��3C��C��3C�ٚC��fC�  C�&fC��C��3C��C��C��C��3C�  C�&fC��C��3C��C��C��C��3C��C��C��C��3C��C��C�&fC��C��fC��3C�  C��C��C��C��C��3C�  C��C��C�  C��fC�  C��C�&fC��C��3C��C��C��C�  C��C�&fC��C��3C�  C��C�  C��fC��3C�  C��C�  C��fC�  C��C��C�&fC��C��3C��3C��3C��C��C�&fC�&fC�33C��C��fC�  C�  C�  C�  C��C��C�  C�ٚC��fC��fC��3C��fC��fD y�D  Dy�DfD� D	S3D�3D� D  D��DffD�3D� D3D ��D#3D%�fD(fD*y�D,�3D/@ D1�3D4,�D6��D9,�D;�3D>L�D@ٚDCs3DF�DH��DK@ DM�fDP�fDS9�DU�fDX��D[33D]��D`` Db�fDel�Dg��Dj�fDmfDo��Dr�Dt��Dw3Dy��D{��D~3D�@ D�s3D��3D��fD�  D�,�D�P D�y�D���D��fD�3D�0 D�` D���D�� D���D�	�D�6fD�\�D��fD��3D���D�  D�&fD�FfD�i�D���D���D���D�	�D�0 D�S3D�|�D���D�ٚD��D�33D�Y�D��fD���D�ɚD��3D�  D�FfD�s3D�� D�ɚD��fD�  D�C3D�i�D�� D�� D��fD�fD�I�D�s3D�D�� D�ٚD�  D��D�C3D�l�Dʓ3D�� D��D��D�0 D�VfD�y�DҖfDӬ�D�� D��D�  D��D�<�D�Y�D�vfDܙ�Dݰ D�ɚD���D��3D��D�&fD�C3D�\�D�|�D��D�3D��fD���D�	�D�#3D�9�D�S3D�l�D�D�fD��3D���D��3D�	�D�  D�<�D�P D�C3D�S3D�` D�i�D�p E @ E �fEH EɚEK3E��EL�E�fEP E� E�EffEc3E	� E
�fET�EP EɚE� E>fE8 E��EfE�E� E� E��E\�EQ�E�3E�3E ,�E!��E"��E$3E%�E&��E'|�E(� E)�3E+nfE,�3E-� E/P E0L�E1�fE2��E4�E5��E6x E7� E9T�E:D�E;� E>� EB�EE!�EH0 EK.fENd�EQ��ET�3EW��EZ�3E]�fE`��EdH Egi�Ej[3EmvfEp� Es�3Ew3Ez3E}�E�fE��3E�< E���E�\�E�� E�q�E�	�E��fE�1�E�t E��3E�fE�\�E�� E� �E�`�E���E��fE�< E��3E��3E�G3E���E��3E�-�E��fE���E�3E�q�E��3E� �E�VfE�� E���?   >���>���?   ?   >���?   ?   ?   ?��?��?333?333?333?L��?fff?���?�33?�  ?�ff@   @ff@33@333@@  @Y��@l��@�  @�ff@�33@�33@���@�ff@�33@���@���@�ff@�33A   A  A��A��A��A#33A)��A0  A9��AA��AI��ANffAVffA^ffAh  Al��At��A{33A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441441441414411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?�  ?�33@&ff@fff@�  @�  @�33@�33A  A��A.ffAL��Ak33A�  A�  A���A�  A�  A�  A�  A�33B��B
  B  B  B"  B*ffB2  B:  BB��BI��BRffBZ��Bb��Bj��Br��BzffB�  B�  B�33B�ffB�33B���B���B�  B�33B�  B�  B���B���B���B�  B�33B�33B�ffB�33B�ffB�33B�ffB�33B�  B���B�ffB�ffB�  B���B���B�33B�  C � CffCffC�3C� C
ffC�3C��C� C��C�3C�3C��C� C� CffC �3C"�3C$� C&� C(ffC*�3C,��C.L�C0ffC2�3C4�3C6��C8��C:��C<��C>��C@�3CB��CD��CF�3CH�3CJ�3CL�3CN��CP��CR��CT��CV� CX� CZffC\�3C^�3C`�3Cb�3Cd� Cf� ChffCj�3Cl��Cn� Cp�3Cr��CtL�Cv��CxffCzL�C|ffC~��C�ffC�L�C�33C�L�C�ffC�L�C�@ C�L�C�@ C�33C�L�C�ffC�33C�&fC�33C�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�33C�L�C�ffC�L�C�@ C�ffC�ffC�L�C�33C�L�C�33C��C�&fC�@ C�ffC�L�C�33C�L�C�Y�C�L�C�33C�@ C�ffC�L�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�L�C�Y�C�L�C�33C�@ C�Y�C�Y�C�@ C�&fC�@ C�Y�C�ffC�Y�C�33C�L�C�Y�C�Y�C�@ C�L�C�ffC�Y�C�33C�@ C�L�C�@ C�&fC�33C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�L�C�33C�33C�33C�L�C�L�C�ffC�ffC�s3C�L�C�&fC�@ C�@ C�@ C�@ C�L�C�Y�C�@ C��C�&fC�&fC�33C�&fD 3D ��D  D��D&fD  D	s3D3D� D@ D��D�fD3D� D33D ��D#33D%�fD(&fD*��D,�3D/` D1�3D4L�D6��D9L�D;�3D>l�D@��DC�3DF,�DH��DK` DNfDP�fDSY�DVfDX��D[S3D]��D`� DcfDe��Dh�Dj�fDm&fDo��Dr9�Dt��Dw33Dy��D{��D~33D�P D��3D��3D��fD� D�<�D�` D���D���D��fD�3D�@ D�p D���D�� D���D��D�FfD�l�D��fD��3D���D� D�6fD�VfD�y�D���D�ɚD���D��D�@ D�c3D���D���D��D��D�C3D�i�D��fD���D�ٚD�3D�0 D�VfD��3D�� D�ٚD�fD�0 D�S3D�y�D�� D�� D��fD�&fD�Y�D��3D©�D�� D��D� D�,�D�S3D�|�Dʣ3D�� D���D��D�@ D�ffDщ�DҦfDӼ�D�� D���D� D�,�D�L�D�i�DۆfDܩ�D�� D�ٚD���D�3D��D�6fD�S3D�l�D��D��D��3D��fD���D��D�33D�I�D�c3D�|�D�D�fD��3D���D�3D��D�0 D�L�D�` D�S3D�c3D�p D�y�D�� E H E �fEP EњES3E��ET�E�fEX E� E�EnfEk3E	� E
�fE\�EX EњE� EFfE@ E��E&fE�E� E� E��Ed�EY�E�3E�3E 4�E!��E"��E$3E%�E&��E'��E)  E)�3E+vfE,�3E-� E/X E0T�E1�fE2��E4$�E5��E6� E7� E9\�E:L�E;� E>� EB�EE)�EH8 EK6fENl�EQ��ET�3EWɚEZ�3E]�fEa�EdP Egq�Ejc3Em~fEp� Es�3Ew3Ez3E}!�E�fE��3E�@ E���E�`�E�� E�u�E��E��fE�5�E�x E��3E�fE�`�E�� E��E�d�E���E�fE�@ E��3E��3E�K3E���E��3E�1�E��fE�ŚE�3E�u�E��3E��E�ZfE�� E���G�O�G�O�?fffG�O�G�O�?fffG�O�G�O�?�  G�O�?���G�O�G�O�?���?�ff?�33?ٙ�?�33@   @33@   @&ff@333@S33@`  @y��@�ff@�  @�ff@�33@�33@���@�ff@�33@���@���@�ffA��A  A  A��A��A$��A+33A1��A8  AA��AI��AQ��AVffA^ffAfffAp  At��A|��A���A���A���A���A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441441441414411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ j@ %@ V@ *@ O@ ""@ )�@ 0x@ 6�@ >@ F�@ S�@ `�@ m:@ z�@ ��@ �0@ ��@ �~@ �&@ �@ ��@ �@ ��@j@@g@,`@:@I@T�@c�@r@�@�P@��@��@��@@��@�;@�4@�~@%@{@"�@/�@=q@Ji@X@e�@t@�d@�@�a@�Y@��@ƨ@��@��@��@��@�@B@%�@2�@?}@O0@\)@i�@v�@�p@�#@�m@�f@�@�@�
@�@�@^@V@O@(�@5�@E�@SI@_�@m:@z3@��@�<@�z@��@��@�*@�#@��@�q@@�@ @-@:�@I@V�@dZ@r@~�@��@�H@��@��@@ψ@�;@��@��@1@{@""@/@>�@K�@X�@g�@t�@�W@�@�U@�M@��@ƨ@խ@��@�@@�E@J@�@%�@3�@@�@M�@\�@k�@v�@��@�@�m@�r@�k@ȴ@׹@�@�@  @@�@(G@7L@FQ@SI@^�@m�@|�@�7@�0@��@��@��@�@�#@�m@�@	@	@	 �@	-@	9X@	H]@	V�@	c�@	o�@	~K@	��@	�H@	��@	��@	��@	��@	�/@	�4@	��@
�@
�@
"�@
1'@
?}@
K�@
Wb@
e�@
t@
�d@
�@
�a@
�Y@
��@
��@
��@
�@
��@
�9@
=@B@'�@4�@@,@O0@]�@k.@ww@��@��@��@�f@��@�@�
@�T@�@  @@O@'�@6�@D�@SI@a�@m�@z3@��@��@��@�-@�2@��@�/@��@�e@j@@�@,`@:�@I@UU@`�@oF@|�@�D@�<@��@�9@@ψ@ލ@b�@��@�@33@z�@��@
�@P�@�0@܀@!s@e	@��@�4@/@oF@�~@�e@7�@|?@��@v@Lu@�@�@ @g�@�@�q@>@��@��@B@a�@��@��@3�@x�@�@@H]@�P@��@�@Z�@��@�h@�@\)@��@ލ@ @_�@��@��@[@^�@�a@ލ@�@_�@�@��@
@^5@�a@�/@ �@ \�@ �U@ �t@!B@!V�@!��@!�C@"@"Q=@"�i@"�7@#V@#M�@#��@#��@$b@$O0@$��@$�*@%�@%I�@%�7@%�c@&1@&H]@&��@&�@'1@'G�@'��@'Ĝ@(j@(DD@(�@(��@)v@)D�@)��@)@)��@*=q@*z3@*�@*�~@+7L@+ww@+��@+�@,33@,r@,�!@,��@-(G@-ff@-�z@-��@.�@.X@.��@.є@/�@/K@/�+@/��@/�E@09X@0uk@0�-@0�@@1+�@1i!@1��@1�@2
@2Z�@2��@2�C@3V@3Ji@3�+@3��@4 �@4<�@4x&@4��@4��@5+�@5ff@5�H@5�O@6�@6FQ@6~K@6�R@6�@7(�@7`B@7��@7��@8v@8<�@8t@8��@9g@9�J@:1'@:Ӡ@;@,@;��@<K@<�4@=X�@=�}@>b�@?@?��@@�@@�A@A@A��@BI�@B�-@CO�@C��@DWb@D�q@E`B@E��@Fhs@G
�@Gv@H�@H�@I$�@I��@J/�@J��@K8�@K�[@L<@L�@Mr�@M�t@Nww@O@Oy�@P@Qm:@R�o@T�@Ui�@V��@X�@Ye	@Z��@\V@]`�@^�9@_�E@aff@b�k@c��@eQ=@f�m@g� @idZ@j�@k�E@mFP@n�9@o��@qD�@r��@t�@uI@v�5@w��@yV�@y�\@y�@zZ@zV@z��@z��@{3�@{k.@{�^@{��@|<�@|��@|��@}@}M�@}��@}�T@~�@~^�@~�	@~�/@&;@oF@��@��G�O�G�O�@ �G�O�G�O�@ �G�O�G�O�@ jG�O�@ G�O�G�O�@ �@ v@ %@ 1@ 	�@ 
=@ J@ �@ V@ �@ @ {@ 6@ B@ O@ �@ g@ "�@ $.@ &�@ )�@ +�@ /@ 1'@ 3�@ 6�@ :@ <@ ?}@ B�@ E�@ H]@ K@ O0@ R�@ V@ X@ [z@ ^�@ b�@ e	@ hs@ k.@ n�@ r@ t�@ wwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�`BA�\)A�Q�A�=qA�7LA���A��#A̼jA̺^A̶FA�z�A�;dA�{A�%A���A��A�ĜA�ȴA�`BAŮA�M�A�$�A��A�ȴAò-AìAá�AÓuA�|�A�XA���A�A�l�A�G�A���A�x�A�bNA�A�VA�
=A���A�r�A���A�ĜA�t�A��PA��9A��A��A�ƨA��A�&�A��A�M�A��A��#A�`BA�JA�S�A���A�/A��A���A�ĜA���A�"�A�VA�dZA��A�$�A��\A��DA���A�n�A���A�A��TA�G�A��PA�1'A�`BA��yA���A��TA�1A��!A�\)A�x�A�%A�33A�(�A���A��A��DA��mA�"�A�A��A���A��A�ƨA��FA��A�JA�ȴA�ƨA�7LA���A�|�A���A�M�A���A��DA��A�ȴA�oA�(�A�bNA~��A~1A}"�Az�Aw��Au��At�`AtA�Ar�HAp�RAm�Ai7LAfE�Ae��Ae��Ae��Ad�\Ab1A`(�A]�
A\�`A\$�A[hsAX1'AV�!AT�ASG�ARI�AP�ANȴAM�-ALz�AJ=qAI�AHbNAF��AFv�AFbNAFA�AEp�AC��AB �AA33A?t�A="�A;hsA:�jA9�7A7��A6jA5�A4�A4�DA4=qA2��A17LA0�uA/�;A.��A.A-��A-�7A-`BA,�`A+�A*z�A*�A)|�A)&�A(��A(1'A'�wA'7LA&�A&�uA&A�A%�TA$��A"��A"jA!�A!l�A!�A �yA �A ��A A^5A;dAA�A�;A��A�A�A=qA&�AA�yA�jA�TA�HA��A�HAZA��A~�A�hAQ�AȴA�A�PAA
A	/A�AĜA��A/A�A�A�
A�A�9A�
AO�A M�@��F@�&�@�dZ@��R@�X@��m@�+@�ff@��@���@���@��^@���@�/@�@��H@���@�@� �@��y@��@�J@��/@�Z@���@���@�7L@�hs@�=q@�|�@��@�O�@�%@�A�@��@�ƨ@�&�@���@�Ĝ@�A�@�-@�Q�@��
@��@���@���@��@���@�"�@���@��H@�"�@�V@���@�G�@��D@��
@�;d@�E�@�?}@��@��y@��7@��/@���@�n�@�5?@��`@� �@~V@}`B@{�@y�@w�@v��@u@t(�@qX@o�;@nv�@m�@k33@hĜ@g+@fȴ@fff@dI�@co@a&�@`  @^��@]/@\�@["�@X��@W�;@W
=@U�@T��@SS�@Q%@O\)@Nv�@M`B@K�m@KC�@IX@Hb@G+@EV@C��@B�!@A%@@ �@?;d@>�+@=�-@<��@<1@;��@:=q@9�@7+@6E�@6@4�@3��@3o@1��@0A�@/l�@.��@.@,9X@+@*-@)x�@'+@&@%p�@$��@$z�@#��@"�!@!�@!��@ ��@��@�R@E�@��@I�@�
@�@-@��@;d@v�@��@p�@�j@�@S�@��@�@�@�9@�@K�@�@E�@�@��@��@z�@�
@"�@
��@
~�@	�#@	7L@�9@  @��@v�@$�@@/@�@9X@�F@�@�^?��;?���?�ƨ?�X?���?���?�\?�bN?�w??�1?��?�Q�?�+?���?�o?�%?�A�?�{?�O�?ۅ?�=q?ش9?�ff?�?�z�?��
?�S�?ҏ\?�-?�hs?�Ĝ?�A�?�  ?��?�5??�p�?��?�j?�C�?���?��?�X?��?�ff?�?}?��
?���?��7?�G�?�%?�\)?���?�5??��h?�V?��D?�ƨ?���?�C�?�"�?�dZ?�dZ?�ƨ?�1?�j?��?���?�p�?���?�V?��R?�\)?��;?�  ?�  ?� �?�A�?�bN?��?���?�Ĝ?��`?��`?�%?�&�?�&�?�G�?�hs?��7?��7?��7?���?���?���?��?�J?�-?�-A�bNA�bNA�dZA�jA�bNA�dZA�bNA�`BA�^5A�bNA�dZA�`BA�dZA�dZA�dZA�`BA�S�A�\)A�XA�XA�^5A�^5A�\)A�VA�K�A�Q�A�C�A�;dA�;dA�K�A�E�A�{A�A���A��A��HA���A�ȴA̶FA̴9A̴9A���A̾wA̼jA̴9A̧�A̗�Ả7A�n�A�\)A�S�A�?}A�33A�(�A��A��A�bA�VA�
=A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�bNA�`BA�\)A�Q�A�=qA�7LA���A��#A̼jA̺^A̶FA�z�A�;dA�{A�%A���A��A�ĜA�ȴA�`BAŮA�M�A�$�A��A�ȴAò-AìAá�AÓuA�|�A�XA���A�A�l�A�G�A���A�x�A�bNA�A�VA�
=A���A�r�A���A�ĜA�t�A��PA��9A��A��A�ƨA��A�&�A��A�M�A��A��#A�`BA�JA�S�A���A�/A��A���A�ĜA���A�"�A�VA�dZA��A�$�A��\A��DA���A�n�A���A�A��TA�G�A��PA�1'A�`BA��yA���A��TA�1A��!A�\)A�x�A�%A�33A�(�A���A��A��DA��mA�"�A�A��A���A��A�ƨA��FA��A�JA�ȴA�ƨA�7LA���A�|�A���A�M�A���A��DA��A�ȴA�oA�(�A�bNA~��A~1A}"�Az�Aw��Au��At�`AtA�Ar�HAp�RAm�Ai7LAfE�Ae��Ae��Ae��Ad�\Ab1A`(�A]�
A\�`A\$�A[hsAX1'AV�!AT�ASG�ARI�AP�ANȴAM�-ALz�AJ=qAI�AHbNAF��AFv�AFbNAFA�AEp�AC��AB �AA33A?t�A="�A;hsA:�jA9�7A7��A6jA5�A4�A4�DA4=qA2��A17LA0�uA/�;A.��A.A-��A-�7A-`BA,�`A+�A*z�A*�A)|�A)&�A(��A(1'A'�wA'7LA&�A&�uA&A�A%�TA$��A"��A"jA!�A!l�A!�A �yA �A ��A A^5A;dAA�A�;A��A�A�A=qA&�AA�yA�jA�TA�HA��A�HAZA��A~�A�hAQ�AȴA�A�PAA
A	/A�AĜA��A/A�A�A�
A�A�9A�
AO�A M�@��F@�&�@�dZ@��R@�X@��m@�+@�ff@��@���@���@��^@���@�/@�@��H@���@�@� �@��y@��@�J@��/@�Z@���@���@�7L@�hs@�=q@�|�@��@�O�@�%@�A�@��@�ƨ@�&�@���@�Ĝ@�A�@�-@�Q�@��
@��@���@���@��@���@�"�@���@��H@�"�@�V@���@�G�@��D@��
@�;d@�E�@�?}@��@��y@��7@��/@���@�n�@�5?@��`@� �@~V@}`B@{�@y�@w�@v��@u@t(�@qX@o�;@nv�@m�@k33@hĜ@g+@fȴ@fff@dI�@co@a&�@`  @^��@]/@\�@["�@X��@W�;@W
=@U�@T��@SS�@Q%@O\)@Nv�@M`B@K�m@KC�@IX@Hb@G+@EV@C��@B�!@A%@@ �@?;d@>�+@=�-@<��@<1@;��@:=q@9�@7+@6E�@6@4�@3��@3o@1��@0A�@/l�@.��@.@,9X@+@*-@)x�@'+@&@%p�@$��@$z�@#��@"�!@!�@!��@ ��@��@�R@E�@��@I�@�
@�@-@��@;d@v�@��@p�@�j@�@S�@��@�@�@�9@�@K�@�@E�@�@��@��@z�@�
@"�@
��@
~�@	�#@	7L@�9@  @��@v�@$�@@/@�@9X@�F@�@�^?��;?���?�ƨ?�X?���?���?�\?�bN?�w??�1?��?�Q�?�+?���?�o?�%?�A�?�{?�O�?ۅ?�=q?ش9?�ff?�?�z�?��
?�S�?ҏ\?�-?�hs?�Ĝ?�A�?�  ?��?�5??�p�?��?�j?�C�?���?��?�X?��?�ff?�?}?��
?���?��7?�G�?�%?�\)?���?�5??��h?�V?��D?�ƨ?���?�C�?�"�?�dZ?�dZ?�ƨ?�1?�j?��?���?�p�?���?�V?��R?�\)?��;?�  ?�  ?� �?�A�?�bN?��?���?�Ĝ?��`?��`?�%?�&�?�&�?�G�?�hs?��7?��7?��7?���?���?���?��?�J?�-?�-A�bNA�bNA�dZA�jA�bNA�dZA�bNA�`BA�^5A�bNA�dZA�`BA�dZA�dZA�dZA�`BA�S�A�\)A�XA�XA�^5A�^5A�\)A�VA�K�A�Q�A�C�A�;dA�;dA�K�A�E�A�{A�A���A��A��HA���A�ȴA̶FA̴9A̴9A���A̾wA̼jA̴9A̧�A̗�Ả7A�n�A�\)A�S�A�?}A�33A�(�A��A��A�bA�VA�
=A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�FB
�LB
�FB
�LB
�LB
�3B
�FB
�FB
�FB
�LB
�FB
�FB
�FB
�RB
�RB
�XB
�dB
��B
�mB�9B��B�B	7BDB
=B	7B	7B	7B	7B+B1BJBVB\BbB�B�B&�B%�B-B=qBW
BXBO�BO�BO�BQ�BS�B\)BcTBiyBiyBn�Bp�Bs�Bw�Bu�By�B{�B� B�B�1B�JB�%B�1B�B�Bu�B_;B[#BQ�B@�B7LB33B.B$�B�BPBB��B��B�B�B��BB�?B�'B��B��B��B�%Bt�BZBK�B9XB1'B�BhB
=BB
��B
��B
�`B
��B
��B
ɺB
�LB
��B
��B
|�B
|�B
y�B
p�B
ffB
bNB
]/B
Q�B
F�B
;dB
'�B
"�B
�B
PB	��B	�B	�B	�fB	�;B	��B	�-B	�hB	�B	{�B	~�B	�B	|�B	iyB	[#B	K�B	M�B	H�B	A�B	'�B	 �B	hB	DB	B��B�fB�BB��B��B��B��BŢBŢBĜBB�jB�XB�FB�-B��B��B��B��B�hB�VB�JB�PB�VB�hB�hB�DB�DB�7B�B~�B{�B{�B|�B|�B{�By�B~�B~�B}�B}�B}�B}�B~�B�B� B�B~�B~�Bx�B{�Bz�Bz�B~�B�B�B�B�B�B}�B|�By�Bz�Bx�Bv�Bv�Bv�Bw�Bv�Bu�Bt�Bq�Bm�Bm�Bk�BiyBffBdZBaHB\)BXBVBS�BO�BL�BM�BG�BE�BD�BD�BC�BD�BI�BJ�BG�BH�BI�BF�BC�B>wB?}B<jB>wB=qB>wB@�BG�BN�BR�BYB\)BaHBe`B^5BcTBhsBe`BhsBgmB@�B=qBD�BE�BL�BR�B[#BcTBdZBp�Bv�B� B�{B��B��B�jB�#B�B��B	B	�B	�B	(�B	:^B	I�B	`BB	jB	}�B	�+B	��B	��B	��B	�B	�LB	�RB	�dB	�jB	B	ƨB	��B	��B	�
B	�B	�;B	�ZB	�`B	�yB	�B	�B	�B	��B	��B	��B
  B
B
B
B
1B
DB
JB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
$�B
%�B
&�B
(�B
)�B
,B
.B
0!B
2-B
33B
49B
6FB
7LB
7LB
7LB
8RB
:^B
:^B
<jB
<jB
=qB
?}B
@�B
?}B
A�B
A�B
C�B
C�B
D�B
E�B
F�B
F�B
H�B
K�B
L�B
M�B
M�B
M�B
O�B
O�B
R�B
S�B
T�B
T�B
W
B
XB
ZB
[#B
\)B
]/B
]/B
^5B
`BB
`BB
aHB
aHB
bNB
cTB
dZB
ffB
gmB
gmB
gmB
hsB
hsB
iyB
jB
jB
l�B
m�B
m�B
n�B
m�B
o�B
o�B
p�B
o�B
p�B
q�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
w�B
w�B
x�B
x�B
y�B
x�B
z�B
{�B
{�B
}�B
�B
�B
�B
�B
�%B
�+B
�=B
�=B
�DB
�JB
�PB
�VB
�bB
�hB
�hB
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
�B
�B
�B
�B
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
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�^B
�dB
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
�jB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�LB
�?B
�LB
�FB
�?B
�RB
�FB
�RB
�FB
�LB
�FB
�LB
�FB
�FB
�LB
�LB
�RB
�FB
�FB
�LB
�LB
�?B
�?B
�FB
�LB
�LB
�?B
�LB
�XB
�RB
�B
�3B
�FB
�FB
�?B
�FB
�?B
�?B
�FB
�FB
�RB
�FB
�LB
�?B
�FB
�FB
�?B
�9B
�FB
�LB
�FB
�FB
�LB
�FB
�LB
�LB
�RB
�RB
�RB
�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B
�?B
�FB
�?B
�FB
�FB
�-B
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�LB
�LB
�RB
�^B
�}B
�fB�3B��B�B1B
=B	7B1B1B1B1B%B+BDBPBVB\B�B�B%�B$�B,B<jBVBW
BN�BN�BN�BP�BR�B[#BbNBhsBhsBm�Bo�Br�Bv�Bt�Bx�Bz�B~�B�B�+B�DB�B�+B�B�Bt�B^5BZBP�B?}B6FB2-B-B#�B�BJBB��B��B�B��B��B��B�9B�!B��B��B��B�Bs�BYBJ�B8RB0!B�BbB	7BB
��B
��B
�ZB
��B
��B
ȴB
�FB
��B
��B
{�B
{�B
x�B
o�B
e`B
aHB
\)B
P�B
E�B
:^B
&�B
!�B
�B
JB	��B	�B	�B	�`B	�5B	��B	�'B	�bB	� B	z�B	}�B	�B	{�B	hsB	ZB	J�B	L�B	G�B	@�B	&�B	�B	bB	
=B	B��B�`B�;B��B��BɺB��BĜBĜBÖB��B�dB�RB�?B�'B��B��B��B��B�bB�PB�DB�JB�PB�bB�bB�=B�=B�1B�B}�Bz�Bz�B{�B{�Bz�Bx�B}�B}�B|�B|�B|�B|�B}�B� B~�B� B}�B}�Bw�Bz�By�By�B}�B� B�B�B�B�B|�B{�Bx�By�Bw�Bu�Bu�Bu�Bv�Bu�Bt�Bs�Bp�Bl�Bl�BjBhsBe`BcTB`BB[#BW
BT�BR�BN�BK�BL�BF�BD�BC�BC�BB�BC�BH�BI�BF�BG�BH�BE�BB�B=qB>wB;dB=qB<jB=qB?}BF�BM�BQ�BXB[#B`BBdZB]/BbNBgmBdZBgmBffB?}B<jBC�BD�BK�BQ�BZBbNBcTBo�Bu�B~�B�uB��B��B�dB�B�B��B	B	�B	�B	'�B	9XB	H�B	_;B	iyB	|�B	�%B	�{B	��B	��B	�B	�FB	�LB	�^B	�jB	B	ƨB	��B	��B	�
B	�B	�;B	�ZB	�`B	�yB	�B	�B	�B	��B	��B	��B
  B
B
B
B
1B
DB
JB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
$�B
%�B
&�B
(�B
)�B
,B
.B
0!B
2-B
33B
49B
6FB
7LB
7LB
7LB
8RB
:^B
:^B
<jB
<jB
=qB
?}B
@�B
?}B
A�B
A�B
C�B
C�B
D�B
E�B
F�B
F�B
H�B
K�B
L�B
M�B
M�B
M�B
O�B
O�B
R�B
S�B
T�B
T�B
W
B
XB
ZB
[#B
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
e`B
gmB
hsB
hsB
hsB
iyB
iyB
jB
k�B
k�B
m�B
n�B
n�B
o�B
n�B
p�B
p�B
q�B
p�B
q�B
r�B
s�B
t�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
x�B
y�B
y�B
z�B
y�B
{�B
|�B
|�B
~�B
�B
�B
�B
�%B
�+B
�1B
�DB
�DB
�JB
�PB
�VB
�\B
�hB
�oB
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
�B
�B
�!B
�'B
�'B
�-B
�3B
�?B
�FB
�FB
�LB
�RB
�XB
�XB
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�wB
�}B
�wB
�}B
�wB
�}B
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
�}B
�}B
�}B
�}B
�}B
��B
�}B
��B
�}B
��B
��B
�FB
�9B
�FB
�?B
�9B
�LB
�?B
�LB
�?B
�FB
�?B
�FB
�?B
�?B
�FB
�FB
�LB
�?B
�?B
�FB
�FB
�9B
�9B
�?B
�FB
�FB
�9B
�FB
�RB
�LB
�B
�-B
�?B
�?B
�9B
�?B
�9B
�9B
�?B
�?B
�LB
�?B
�FB
�9B
�?B
�?B
�9B
�3B
�?B
�FB
�?B
�?B
�FB
�?B
�FB
�FB
�LB
�LB
�LB
�LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202332021061413521320210614135213202106141746222021061417462220210614174622201807242202332021061413521320210614135213202106141746222021061417462220210614174622PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023320180724220233  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023320180724220233QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023320180724220233QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014920210722160149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                