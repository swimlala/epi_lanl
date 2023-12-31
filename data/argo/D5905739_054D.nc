CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-24T03:00:55Z creation      
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
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181224030055  20210617131512  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               6   6DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؘ:B��@ؘ:B��11  @ؘ:8� @ؘ:8� @6l�~��M@6l�~��M�c�=F�k��c�=F�k�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @Fff@�33@�33@�ff@�33A��A��A$��AA��A`  A���A���A���A�  A�  A�  A�  A���B   BffB��B  B ffB(ffB/��B8  B@ffBH  BPffBX��B`ffBg��Bp  Bx��B�33B�  B�  B�  B�33B���B�ffB�33B�33B�  B�  B�33B�33B�33B�33B�33B�ffB�ffB���B���B�ffB�  B�  B�33B���B�  B�33B왚B�B�ffB�33B�33C �C33C33C33CL�C
L�C�C��C��C�fC�C33CffC33C  C�C   C!��C#�fC&33C(  C)�fC,  C.L�C0�C2  C433C6�C7�fC:33C<�C=�fC@�CBL�CD�CE�fCH  CJ  CL�CN33CPL�CR�CS�fCV  CX�CZ33C\L�C^  C`�Cb  Cd�CfffChL�CjL�Cl  Cn�CpL�CrffCt�Cu��Cw��Cy�3C{��C~  C�  C��C��C��C��C��C��C�&fC�&fC�&fC��C��fC��fC��3C�  C�  C��C��C�&fC��C��fC�  C�  C�  C��C��C�&fC�&fC��C��fC��fC��3C�  C��C��C��C�&fC��C��fC�ٚC�  C�&fC�&fC��C��C��C��C��C��C��C�  C��3C��3C��3C��fC��fC�ٚC��C�&fC�33C�&fC�  C��fC��fC��fC��fC��fC��fC��fC��3C��3C�  C�  C��C��C��C��C��C��3C��3C�  C�  C��C��C�&fC�&fC��C��fC��3C�  C�  C��C��C�&fC��C��3C�  C��C��C��C��C�&fC�&fC�&fC�  C��fC��C��C��C�&fC�&fC��C��C��C��C��C�  C��3C��fC�  C��C�  C��3C�33C��C�  C�@ C�&fD fD � D� D�3D	fDS3D�fD��DffD��D33D�3D9�D�fD!ffD$fD&�3D)&fD+� D.Y�D0��D3��D633D8�3D;� D>�D@��DCl�DF�DH�fDKL�DM��DP� DS�DU� DX,�DZ��D]&fD_��DbfDdl�Df�fDi  Dk� DmٚDp@ Dr��DtٚDw9�Dy�3D{� D}��D�3D�@ D�ffD��3D���D��fD� D�6fD�ffD���D�ɚD���D�,�D�` D�� D���D�� D�#3D�P D�� D��3D��D�  D�VfD�� D�� D��3D�&fD�c3D��3D�� D��3D�)�D�VfD��3D��fD��3D���D�&fD�S3D�y�D��3D�� D���D�#3D�P D�y�D��3D���D��fD�#3D�P D�vfD��fD��fD�ٚD���D��D�0 D�L�D�c3D�vfDȆfDɐ Dʠ D˩�D̶fD�� D��fD���D��3D��fD���D���D���D��fD��3D��fD�ٚD���D��fD�� D���D��3D��D��3D���D�fD� D�#3D�33D�<�D�L�D�S3D�Y�D�c3D�l�D�|�D�fD��D�fD�fD�D�D��D���D��3D�� D��fD�� D���D�p D�l�D�l�D�` D�\�D�Y�E ( E � E( E�fE,�E6fE�fE�fE;3E1�E	��E
� E[3EI�E�fE�E��EVfE� E�3E Eh E� E  EK3E��Ek3E�fE 3E!Y�E"��E#��E%I�E&,�E'��E(��E*&fE+s3E,�3E-��E/33E0d�E1�fE2�3E44�E5I�E6��E7� E8�E:FfE;�fE>��EA� EE�EG�3EK6fEN33EQ� ETh EW� EZ� E]ٚE`�3Ed�Eg( Ej� Em�fEp��Eq.fEq� Er��Es&fEs��EtC3Eu3Eu��Ev$�Ev� Ewp Ex1�Ex� Eys3Ey�>L��>L��>L��>���=���>L��>L��=���>���>L��>���>L��>L��=���>L��>���>���>L��>���>���>���>���>���>���?��?333?fff?fff?�  ?���?�33?�  ?ٙ�?�33@ff@��@&ff@9��@L��@`  @s33@�33@���@�ff@�33@�  @���@�ff@�  @���@陚@�ffA   A  A��A��A��A!��A(  A0  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414144411141141441114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?L��?�  @   @fff@�33@�33@�ff@�33A	��A��A,��AI��Ah  A���A���A���A�  A�  A�  A�  A���B  B
ffB��B  B"ffB*ffB1��B:  BBffBJ  BRffBZ��BbffBi��Br  Bz��B�33B�  B�  B�  B�33B���B�ffB�33B�33B�  B�  B�33B�33B�33B�33B�33B�ffB�ffB���B���B�ffB�  B�  B�33BᙚB�  B�33B홚B�B�ffB�33B�33C ��C�3C�3C�3C��C
��C��CL�CL�CffC��C�3C�fC�3C� C��C � C"L�C$ffC&�3C(� C*ffC,� C.��C0��C2� C4�3C6��C8ffC:�3C<��C>ffC@��CB��CD��CFffCH� CJ� CL��CN�3CP��CR��CTffCV� CX��CZ�3C\��C^� C`��Cb� Cd��Cf�fCh��Cj��Cl� Cn��Cp��Cr�fCt��CvL�CxL�Cz33C|L�C~� C�@ C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�ffC�ffC�Y�C�&fC�&fC�33C�@ C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�@ C�@ C�L�C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�@ C�L�C�Y�C�Y�C�ffC�L�C�&fC��C�@ C�ffC�ffC�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�L�C�@ C�33C�33C�33C�&fC�&fC��C�L�C�ffC�s3C�ffC�@ C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�@ C�@ C�Y�C�Y�C�L�C�Y�C�L�C�33C�33C�@ C�@ C�L�C�Y�C�ffC�ffC�Y�C�&fC�33C�@ C�@ C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�Y�C�Y�C�Y�C�ffC�ffC�ffC�@ C�&fC�L�C�L�C�L�C�ffC�ffC�Y�C�Y�C�Y�C�Y�C�L�C�@ C�33C�&fC�@ C�Y�C�@ C�33C�s3C�L�C�@ C�� C�ffD &fD � D� D�3D	&fDs3D�fD�D�fD��DS3D�3DY�D�fD!�fD$&fD&�3D)FfD+� D.y�D1�D3��D6S3D8�3D;� D>9�D@��DC��DF,�DH�fDKl�DN�DP� DS,�DU� DXL�DZ��D]FfD_��Db&fDd��Df�fDi@ Dk� Dm��Dp` Dr��Dt��DwY�Dy�3D{� D}��D�#3D�P D�vfD��3D���D��fD�  D�FfD�vfD���D�ٚD�	�D�<�D�p D�� D���D�  D�33D�` D�� D��3D���D�0 D�ffD�� D�� D�3D�6fD�s3D��3D�� D�3D�9�D�ffD��3D��fD��3D��D�6fD�c3D���D��3D�� D�	�D�33D�` D���D��3D���D�fD�33D�` D��fD��fD��fD��D��D�)�D�@ D�\�D�s3DǆfDȖfDɠ Dʰ D˹�D��fD�� D��fD���D��3D��fD���D���D���D��fD��3D��fD��D���D��fD�� D���D��3D���D�3D��D�fD�  D�33D�C3D�L�D�\�D�c3D�i�D�s3D�|�D��D�fD��D�fD�fD�D�D��D���D��3D�� D��fD�� D���D�� D�|�D�|�D�p D�l�D�i�E 0 E � E0 E�fE4�E>fE�fE�fEC3E9�E	��E  Ec3EQ�E�fE�E��E^fE� E�3E Ep E� E ES3E��Es3E�fE 3E!a�E"��E$�E%Q�E&4�E'��E(��E*.fE+{3E,�3E.�E/;3E0l�E1�fE2�3E4<�E5Q�E6��E7� E8�E:NfE;�fE>��EA� EE�EG�3EK>fEN;3EQ� ETp EW� EZ� E]�E`�3Ed�Eg0 Ej� Em�fEp��Eq6fEq� Er��Es.fEs��EtK3Eu3Eu��Ev,�Ev� Ewx Ex9�Ex� Ey{3Ey��G�O�G�O�?333G�O�?��G�O�G�O�?��G�O�?333G�O�G�O�G�O�?��?333?L��G�O�?333?L��G�O�?L��G�O�G�O�?fff?���?���G�O�?�33?�  ?ٙ�?�33@   @��@��@&ff@9��@Fff@Y��@l��@�  @���@�33@���@�ff@�33@�  @ə�@�ff@�  @���@���A33A  A  A��A��A!��A)��A0  A8  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414414144411141141441114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ @ �@ �@ *@ �@ "�@ *S@ 0x@ 7L@ >@ FQ@ R�@ _�@ m�@ |?@ �7@ �0@ ��@ �~@ �&@ �|@ �t@ ��@ � @j@�@g@+�@:@H]@UU@c�@r@~�@�D@��@��@��@@�7@��@�4@�9@1@*@"�@/�@=q@K�@Yn@g@t�@�d@��@�a@��@��@�W@Ӡ@�H@�@��@
=@�@'�@5?@B8@O0@\�@j@x�@�|@�#@�z@�!@�k@�@խ@�@�@^@b@�@(�@7L@DD@P�@^�@n�@z�@��@�0@��@�-@�&@�*@�#@�m@� @@b@g@.l@:�@F�@UU@b�@qS@�@��@�H@��@��@�>@є@��@�@��@�@*@$�@1�@?}@K@Yn@hs@v�@�d@��@��@��@��@��@Ӡ@��@�L@��@�@B@&�@5?@B�@P�@]�@hs@v@�p@��@�m@��@�@�o@׹@�T@�Y@  @�@�@*S@8�@FQ@R�@^5@k�@z3@��@��@�5@��@�2@�|@�@�@��@	v@	@	 @	-�@	;d@	I@	V@	dZ@	qS@	~K@	�D@	��@	��@	��@	�2@	�*@	ލ@	�@	��@
�@
{@
 �@
.l@
<@
I�@
Wb@
e	@
r�@
�@
��@
�@
��@
��@
�W@
�O@
�@
�@
��@	�@�@%�@3�@B8@P�@^5@k.@v@�p@��@�m@��@�@�o@׹@�@�Y@ �@@�@*S@8�@FQ@S�@_�@k�@{�@�7@��@��@��@��@�*@��@�y@�q@j@b@[@,`@;d@G�@T�@e�@qS@~K@�\@��@��@�R@(�@a�@��@܀@�@[z@��@�;@ �@e	@��@�@7L@~�@Ĝ@
�@Q�@��@�;@(G@oF@��@  @F�@��@�h@ @g@�r@� @=q@�@�c@@SI@��@��@�@]�@��@��@�@^�@�m@�;@
@^�@�@Ӡ@o@R�@��@є@�@Q=@��@�7@@O�@�i@�C@@T�@�0@�
@6@X�@�H@�t@ O@ \�@ �@ �H@!#�@!ff@!�A@!��@"*S@"m�@"��@"��@#0x@#r�@#��@#�@$1'@$qS@$��@$�L@%0x@%oF@%��@%��@&.l@&m�@&�@&�@'-@'l�@'�@'�4@(,`@(k.@(��@(�@)$/@)bN@)�@)�t@*6@*R�@*�P@*�W@+  @+:@+r�@+�@+�@,�@,T�@,��@,��@,��@-2�@-i!@-�a@-�O@.�@.B�@.z3@.�r@.�@/
@/V@/��@/ƨ@/�Q@07�@0p�@0�Y@0�`@1
@1X@1�@1�@2 �@29X@2s_@2�@2�@3�@3SI@3��@3�2@3�~@4/@4g@4�@4є@5�@5<@5m:@5�(@5��@6�@6C�@6y�@6�@6�@7O@7T�@7��@7��@8�(@9b@9��@:�@:��@;K@;�@<H]@<�/@=v@=�@>o�@?
=@?n�@@�@@��@A(�@A��@BB8@B�|@C*S@C��@DE�@D׹@Ehs@E�q@F��@F�@Gx&@H�@H��@I&�@I��@J:�@J��@KB�@K�J@LB8@L�T@MYn@M��@Nm:@N��@Oz2@Pb@Q\�@R��@T@ULu@V�9@W��@Yb�@Z�m@\�@]Q�@^��@_�L@aP�@b�m@d�@eX@f��@f�l@g""@g�W@g�w@g�9@h7�@h��@h�o@iv@i\)@i��@i�`@jO@jn�@j��G�O�G�O�@ ^G�O�@  �G�O�G�O�@  �G�O�@ ^G�O�G�O�G�O�@  �@ ^@ G�O�@ ^@ G�O�@ G�O�G�O�@ �@ @ �G�O�@ %@ �@ 1@ 	�@ 
=@ �@ �@ V@ b@ �@ �@ �@ �@ �@ �@ 
@  @ "�@ %�@ '�@ *S@ ,`@ /@ 1�@ 4�@ 6�@ :@ <@ ?}@ A�@ D�@ G�@ KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�7LA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�?}A�E�A�A�A�?}A�A�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�E�A�E�A�E�A�E�A�C�A�E�A�G�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�M�A�O�A�O�A�O�A�O�A�M�A�M�A�Q�A�E�A�/A�JA���A�$�A��\A�?}A��A��TA���A���A�jA���A��uA�$�A�G�A���A�E�A��wA���A���A�  A�?}A�z�A�Q�A��A���A�VA�l�A�K�A�;dA�
=A��hA�;dA��+A��
A��7A�"�A��mA�jA�JA��uA�7LA�+A���A���A�A�VA�bNA��A��A��A� �A�K�A���A�oA��A�1'A�I�A�S�A���A�+A���A�%A�?}A��uA�ZA�(�A�jA���A���A�
=A~��A}�A|�AwG�Ar{Aq;dAp�9ApI�Ao��Am�#AlffAi�Ah��Ag\)Ae?}AadZA`A�A_�mA_K�A^9XA]�7A[��AY��AY
=AX9XAW`BAUK�ATI�AS�AS�7AR�ARA�AQC�AP�DAOoAMAL-AK&�AH�/AG%AFr�AF1AE�ACC�AA?}A@�/A@1'A?C�A=�A;��A9��A8��A8�9A81A6�DA5��A5�A4ffA3&�A0�A01A.��A-;dA+�mA++A*�jA)+A(jA&z�A%"�A$�9A$$�A#|�A"�A!��A ��A/A�A�yA��AZA�PAA��AbA��A��A�;AffA��A�AjA1A-A�AbNA?}A�A&�A
ZA	dZA�+A�
A`BA�+A-AbA�A�TAƨAC�AE�A33AM�A��A�A $�@�dZ@�o@���@��+@�@�1'@���@�&�@�l�@�z�@�7@�ȴ@�J@���@�dZ@��@��@��@�x�@�w@�-@���@�(�@߮@ܴ9@�&�@���@���@���@�n�@��+@��y@�A�@���@��+@��#@���@�O�@��P@�33@�^5@�
=@�dZ@���@���@�1@���@��9@�ȴ@���@��@�7L@���@�5?@�X@�ƨ@�
=@���@��@�j@��m@���@�S�@�=q@���@�1@��P@���@���@� �@��@�5?@�@��`@��@��D@��@
=@�@|�@zM�@x�9@xA�@u?}@q��@nv�@lz�@k"�@h�u@fV@e/@cdZ@ahs@`��@_
=@]O�@\�j@["�@Yhs@W��@VE�@U`B@S��@Q7L@P��@O+@L��@K33@J��@J�@H��@H�u@G�;@F$�@E�@D��@D��@C"�@A�^@@  @>v�@<�@<(�@;@:^5@8��@7�@6ff@5�-@5/@4(�@3�@2��@1�@/�@/\)@.�R@-�@+��@*��@)G�@'��@&ȴ@%�h@$j@#ƨ@#@"^5@!��@ Q�@��@�@ff@�T@�h@O�@V@��@"�@�\@�\@7L@b@�@\)@;d@V@��@�h@��@(�@ƨ@��@�@G�@%@�9@ �@�P@�@5?@@O�@��@(�@�F@o@
��@
-@	hs@�@�w@|�@�+@ff@$�@�-@��@��@�7@ 1'?�5??�?���?��?���?�`B?�33?�M�?�bN?��;??��?�?���?��?䛦?�o?�J?�Ĝ?ݑh?ۥ�?�C�?�7L?׮?�+?��T?ԛ�?ӕ�?���?�&�?У�?�bN?θR?Ͳ-?̬?˥�?��H?���?�7L?�1'?�l�?Ƨ�?�`B?�9X?�33?�Ĝ?��w?�v�?�{?��?��?��m?�C�?�=q?��#?��#?��9?��9?���?���?��?�7L?�7L?�X?���?�x�?�x�?���?���?���?��^?��^?��#?���A�/A�1'A�+A�+A�/A�+A�/A�1'A�/A�/A�-A�-A�/A�1'A�33A�33A�33A�5?A�33A�5?A�5?A�5?A�33A�1'A�33A�7LA�9XA�9XA�9XA�9XA�7LA�9XA�;dA�;dA�9XA�;dA�9XA�9XA�=qA�=qA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�C�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�1'A�7LA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�?}A�E�A�A�A�?}A�A�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�E�A�E�A�E�A�E�A�C�A�E�A�G�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�M�A�O�A�O�A�O�A�O�A�M�A�M�A�Q�A�E�A�/A�JA���A�$�A��\A�?}A��A��TA���A���A�jA���A��uA�$�A�G�A���A�E�A��wA���A���A�  A�?}A�z�A�Q�A��A���A�VA�l�A�K�A�;dA�
=A��hA�;dA��+A��
A��7A�"�A��mA�jA�JA��uA�7LA�+A���A���A�A�VA�bNA��A��A��A� �A�K�A���A�oA��A�1'A�I�A�S�A���A�+A���A�%A�?}A��uA�ZA�(�A�jA���A���A�
=A~��A}�A|�AwG�Ar{Aq;dAp�9ApI�Ao��Am�#AlffAi�Ah��Ag\)Ae?}AadZA`A�A_�mA_K�A^9XA]�7A[��AY��AY
=AX9XAW`BAUK�ATI�AS�AS�7AR�ARA�AQC�AP�DAOoAMAL-AK&�AH�/AG%AFr�AF1AE�ACC�AA?}A@�/A@1'A?C�A=�A;��A9��A8��A8�9A81A6�DA5��A5�A4ffA3&�A0�A01A.��A-;dA+�mA++A*�jA)+A(jA&z�A%"�A$�9A$$�A#|�A"�A!��A ��A/A�A�yA��AZA�PAA��AbA��A��A�;AffA��A�AjA1A-A�AbNA?}A�A&�A
ZA	dZA�+A�
A`BA�+A-AbA�A�TAƨAC�AE�A33AM�A��A�A $�@�dZ@�o@���@��+@�@�1'@���@�&�@�l�@�z�@�7@�ȴ@�J@���@�dZ@��@��@��@�x�@�w@�-@���@�(�@߮@ܴ9@�&�@���@���@���@�n�@��+@��y@�A�@���@��+@��#@���@�O�@��P@�33@�^5@�
=@�dZ@���@���@�1@���@��9@�ȴ@���@��@�7L@���@�5?@�X@�ƨ@�
=@���@��@�j@��m@���@�S�@�=q@���@�1@��P@���@���@� �@��@�5?@�@��`@��@��D@��@
=@�@|�@zM�@x�9@xA�@u?}@q��@nv�@lz�@k"�@h�u@fV@e/@cdZ@ahs@`��@_
=@]O�@\�j@["�@Yhs@W��@VE�@U`B@S��@Q7L@P��@O+@L��@K33@J��@J�@H��@H�u@G�;@F$�@E�@D��@D��@C"�@A�^@@  @>v�@<�@<(�@;@:^5@8��@7�@6ff@5�-@5/@4(�@3�@2��@1�@/�@/\)@.�R@-�@+��@*��@)G�@'��@&ȴ@%�h@$j@#ƨ@#@"^5@!��@ Q�@��@�@ff@�T@�h@O�@V@��@"�@�\@�\@7L@b@�@\)@;d@V@��@�h@��@(�@ƨ@��@�@G�@%@�9@ �@�P@�@5?@@O�@��@(�@�F@o@
��@
-@	hs@�@�w@|�@�+@ff@$�@�-@��@��@�7@ 1'?�5??�?���?��?���?�`B?�33?�M�?�bN?��;??��?�?���?��?䛦?�o?�J?�Ĝ?ݑh?ۥ�?�C�?�7L?׮?�+?��T?ԛ�?ӕ�?���?�&�?У�?�bN?θR?Ͳ-?̬?˥�?��H?���?�7L?�1'?�l�?Ƨ�?�`B?�9X?�33?�Ĝ?��w?�v�?�{?��?��?��m?�C�?�=q?��#?��#?��9?��9?���?���?��?�7L?�7L?�X?���?�x�?�x�?���?���?���?��^?��^?��#?���A�/A�1'A�+A�+A�/A�+A�/A�1'A�/A�/A�-A�-A�/A�1'A�33A�33A�33A�5?A�33A�5?A�5?A�5?A�33A�1'A�33A�7LA�9XA�9XA�9XA�9XA�7LA�9XA�;dA�;dA�9XA�;dA�9XA�9XA�=qA�=qA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�C�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bu�Bu�Bt�Bt�Bt�Bu�Bt�Bu�Bt�Bu�Bt�Bu�Bu�Bt�Bu�Bt�Bu�Bt�Bu�Bt�Bu�Bt�Bt�Bu�Bt�Bt�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bt�Bt�Bu�Bu�Bu�Bu�Bv�Bu�Bv�Bv�Bv�Bw�By�B� B�7B�DB�=B�=B�B�Bm�B�B�Bz�Bz�B}�B}�B~�B|�Bx�B�+B�%B�B�B�B�B� B�B�B�B�B�B�B� B~�B|�Bu�BiyBffBZBJ�BH�BB�B;dB7LB1'B+B�B%B��B�fB�#B��B�wB�^B�?B�B��B�oBjBVBI�B.B"�B�BJB1B
��B
�B
�B
ŢB
�FB
��B
��B
�JB
jB
W
B
N�B
?}B
�B
B	��B	��B	��B	�B	�BB	�
B	ȴB	�wB	�3B	��B	�1B	�B	}�B	w�B	t�B	m�B	bNB	XB	S�B	N�B	H�B	:^B	7LB	49B	1'B	-B	)�B	#�B	�B	�B	oB	PB	1B��B�B�B�B�ZB�
B��BɺBĜB�jB�B��B��B��B��B�oB�DB�\B�JB��B�=B� B}�Bz�Bu�Bp�Bo�Bo�BiyBhsBbNBbNBaHB_;B_;B[#BZBVBS�BS�BQ�BP�BN�BN�BM�BK�BJ�BJ�BG�BE�BD�BC�BC�BB�B@�B;dB<jB:^B8RB7LB9XB9XB7LB7LB7LB6FB6FB6FB6FB6FB6FB49B2-B1'B0!B0!B.B.B1'B0!B/B.B/B2-B33B33B33B0!B33B1'B1'B0!B0!B49B7LB7LB8RB;dB<jB=qB=qB=qB=qBA�BO�BR�B]/Bs�Br�B�PB��B��B�B�B�FB�}B�B�`B�HB�B�B	DB	�B	�B	0!B	.B	'�B	/B	7LB	F�B	VB	gmB	m�B	v�B	{�B	z�B	|�B	�oB	��B	��B	�FB	�qB	ŢB	��B	��B	��B	�#B	�BB	�fB	�yB	�B	�B	�B	��B	��B	��B	��B
B
+B
	7B
DB
JB
\B
bB
{B
�B
�B
�B
�B
 �B
 �B
 �B
�B
!�B
#�B
#�B
&�B
(�B
(�B
+B
,B
-B
0!B
1'B
49B
6FB
9XB
:^B
:^B
;dB
;dB
<jB
=qB
>wB
?}B
?}B
A�B
A�B
A�B
C�B
C�B
D�B
E�B
E�B
F�B
G�B
I�B
I�B
I�B
J�B
K�B
L�B
N�B
O�B
O�B
P�B
R�B
R�B
T�B
VB
W
B
XB
YB
ZB
\)B
\)B
\)B
]/B
_;B
_;B
`BB
aHB
aHB
bNB
aHB
bNB
dZB
dZB
dZB
dZB
ffB
gmB
gmB
hsB
gmB
iyB
iyB
jB
jB
k�B
l�B
n�B
m�B
m�B
n�B
n�B
p�B
p�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
w�B
w�B
x�B
y�B
z�B
y�B
y�B
z�B
|�B
~�B
�B
�B
�B
�B
�B
�B
�+B
�1B
�=B
�7B
�DB
�JB
�JB
�PB
�VB
�\B
�hB
�oB
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
�B
�B
�B
�!B
�'B
�'B
�'B
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
�XB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XBv�Bv�Bu�Bv�Bu�Bu�Bu�Bu�Bv�Bu�Bu�Bv�Bu�Bt�Bv�Bv�Bu�Bu�Bv�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Bu�Bt�Bt�Bu�Bt�Bu�Bt�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Bt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Bu�Bu�Bt�Bt�Bt�Bu�Bt�Bu�Bt�Bu�Bt�Bu�Bu�Bt�Bu�Bt�Bu�Bt�Bu�Bt�Bu�Bt�Bt�Bu�Bt�Bt�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bt�Bt�Bu�Bu�Bu�Bu�Bv�Bu�Bv�Bv�Bv�Bw�By�B�B�(B�6B�0B�0B� B�Bm�B�B��Bz�Bz�B}�B}�B~�B|�Bx�B�#B�B�B�B�B�B�B�B�B�	B�B�B�B�B~�B|�Bu�BizBfhBZBJ�BH�BB�B;hB7QB1,B+B�B,B��B�nB�+B��B��B�hB�IB�B��B�{Bj�BVBI�B.!B"�B�BYB@B
��B
�B
� B
ųB
�WB
�B
��B
�\B
j�B
WB
N�B
?�B
�B
-B	�	B	��B	��B	�B	�YB	�!B	��B	��B	�LB	��B	�JB	� B	~B	w�B	t�B	m�B	bjB	X-B	TB	N�B	H�B	:}B	7kB	4YB	1GB	-/B	*B	#�B	�B	�B	�B	tB	UB�B��B��B�B�B�1B��B��B��B��B�DB�&B��B��B��B��B�pB��B�wB��B�kB�.B~#B{Bu�Bp�Bo�Bo�Bi�Bh�Bb�Bb�Ba|B_oB_pB[XBZSBV:BT/BT/BR$BQBOBOBNBLBJ�BJ�BG�BE�BD�BC�BC�BB�B@�B;�B<�B:�B8�B7�B9�B9�B7�B7�B7�B6�B6�B6�B6�B6�B6�B4�B2uB1oB0jB0jB.^B.^B1rB0lB/gB.aB/hB2{B3�B3�B3�B0qB3�B1xB1xB0sB0sB4�B7�B7�B8�B;�B<�B=�B=�B=�B=�BA�BP;BSPB]�BtBsB��B�B� B��B��B��B��B،B��B��B�#B�2B	�B	!B	 PB	0�B	.�B	(�B	/�B	7�B	GLB	V�B	hB	n>B	wzB	|�B	{�B	}�B	�,B	�`B	��B	�B	�:B	�nB	̕B	аB	��B	��B	�B	�BB	�XB	�sB	�}B	�B	��B	��B	��B	��B
B
#B

1B
AB
JB
_B
hB
�B
�B
�B
�B
�B
!�B
!�B
!�B
 �B
"�B
$�B
$�B
(B
*#B
*&B
,5B
->B
.GB
1]B
2eB
5zB
7�B
:�B
;�B
;�B
<�B
<�B
=�B
>�B
?�B
@�B
@�B
B�B
B�B
B�B
EB
EB
FB
GB
GB
H!B
I*B
K9B
K;B
K>B
LHB
MQB
NYB
PhB
QqB
QsB
R|B
T�B
T�B
V�B
W�B
X�B
Y�B
Z�B
[�B
]�B
]�B
]�B
^�B
`�B
`�B
a�B
cB
cB
dB
cB
dB
f$B
f'B
f)B
f,B
h:B
iDB
iFB
jOB
iKB
kZB
k\B
leB
lgB
mpB
nxB
p�B
o�B
o�B
p�B
p�B
r�B
r�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
y�B
y�B
z�B
{�B
}B
{�B
|B
}
B
B
�0B
�AB
�NB
�_B
�lB
�sB
�zB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�"B
�/B
�AB
�TB
�TB
�gB
�tB
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
�B
�B
�B
�B
�0B
�7B
�KB
�gB
�}B
��B
��B
��B
��B
��B
��B
�B
�'B
�<B
�LB
�aB
�xB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��Bv�Bv�Bu�Bv�Bu�Bu�Bu�Bu�Bv�Bu�Bu�Bv�Bu�Bt�Bv�Bv�Bu�Bu�Bv�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Bu�Bt�Bt�Bu�Bt�Bu�Bt�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Bt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812240300552021061413560020210614135600202106171313532021061713135320210617131353201812240300552021061413560020210614135600202106171313532021061713135320210617131353PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018122403005520181224030055  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122403005520181224030055QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122403005520181224030055QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151220210617131512IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                