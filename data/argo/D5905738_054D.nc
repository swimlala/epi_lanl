CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-01-22T18:00:45Z creation      
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
_FillValue                  � 	�      � 	�Argo profile    3.1 1.2 19500101000000  20190122180045  20210722160157  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               6   6DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؝-�r@؝-�r11  @؝-�qP@؝-�qP@5��hH��@5��hH���c�+��a�c�+��a11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?L��@ff@Fff@�  @���@�  @�33A��A33A$��AA��Ac33A�  A�  A�  A�  A�33A�33A���A���B ffBffBffB��B   B(  B0ffB8  B@ffBH  BP  BX  B`ffBhffBo��Bx  B�  B���B�  B�  B�  B�  B�  B�33B�ffB�33B�ffB�33B�  B�  B�  B���B�  B�33B���B˙�Bϙ�B���B�  B�  B�33B�ffB虚B�33BB���B�  B�33C 33CL�C  C��C�fC
  C�C�C�C33C33CL�CL�C  C��C�fC�fC!�fC#�fC%�fC'�fC*  C,  C-�fC0  C1�fC4�C6L�C833C:�C<  C=��C@�CB33CD�CF  CG��CJ  CL33CN�CP�CR�CT�CV�CX�CZ�C\�C^33C`33CbL�Cd�Ce��Cg�fCj  Cl  Cn�Cp33Cr33CtL�Cv�Cw��Cy�fC{�fC~  C�  C��C�  C�  C�  C��3C�  C��3C��fC�ٚC��C�&fC��C��C��C�  C��3C��C�&fC��C�  C�  C��C�&fC��C�  C��3C��fC��C��C�&fC��C��C��C�  C��3C��fC�  C�&fC�&fC��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C��fC��3C��fC��fC��fC��fC��3C��fC��fC��fC��3C��3C��3C��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��C�33C��C��fC��3C��fC��fC��fC��fC��C��C��C�  C��C�&fC��C��3C��C��C�  C��C�&fC��C��3C�  C��C�  C��C��C��3C��C��C��3C�  C�&fC��C��C��C��3C�  C�  C��C��fC��3C�&fC��3C��fC��D   D �3D�fD�fD� D�D9�DffD�fD�fD�3D��D  D9�Ds3D �3D"��D%S3D'� D)��D,33D.� D0�fD3fD5S3D7�fD9��D;� D>�D@9�DB` DD�3DF�fDIfDK33DMs3DO�fDQ�fDS�3DV�DX&fDZ33D\9�D^@ D`FfDbL�DdY�Df` Dh` DjffDll�Dnl�Dpy�Dr��Dt��Dv��Dx��Dz�3D|��D~� D�ffD�|�D�� D��3D�� D�� D���D���D�� D�3D�fD�#3D�&fD�,�D�9�D�9�D�9�D�<�D�33D�#3D�fD�	�D�3D��3D��fD���D���D�� D���D��3D�� D�l�D�\�D�FfD�0 D��D�fD��3D��fD���D�� D�� D�` D�C3D�#3D��D�� D�� D���D��fD��fD���D�� D���D��3D�y�D�s3D�s3D�i�D�ffD�i�D�ffD�l�D�s3D�|�D��3D��3D��3DfDÆfDă3DŃ3D�y�D�s3D�l�D�l�D�ffD�\�D�P D�L�D�C3D�0 D�&fD�  D� D�  D��3D�� D�� D�� D׼�DئfDى�D�s3D�\�D�@ D�3D���D߹�D��D�p D�C3D�#3D��D��D�ɚD�3D�3D�` D�6fD�3D��3D��D��D�s3D�P D�9�D��D��D���D�l�D�fE �3E�E[3E� E�3E<�EvfE	� E
ٚE3E0 E� E� E�EI�E|�E� E�fE��E��E� E( EVfE� E �3E#њE&��E)��E-,�E0S3E38 E6` E9��E<� E=K3E>�E>��E?.fE?��E@� EAfEA�fEBT�EBٚEC��ED&fED� EE~fEF3EF�3EGNfEH	�EH� EIH EI��EJ�3EK�EK�3EL6fEL�EM��EN3ENњEO�3EO�3EP��EQX ER3ERt�ES ES�3ETk3>���>���?   >���>���?   ?   ?��?   ?   ?   ?   ?��?��?��?L��?L��?�  ?�  ?���?���?�33?ٙ�?�33@   @33@   @333@@  @Y��@l��@�  @���@�33@���@�ff@�33@���@�33@�  @���@陚@�ffA   A  AffAffA��A#33A,��A333A9��AC33AH  AP  AX  A`  AfffAl��At��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414144441441414111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?fff?�ff@&ff@fff@�  @���@�  @�33A	��A33A,��AI��Ak33A�  A�  A�  A�  A�33A�33A���A���BffB
ffBffB��B"  B*  B2ffB:  BBffBJ  BR  BZ  BbffBjffBq��Bz  B�  B���B�  B�  B�  B�  B�  B�33B�ffB�33B�ffB�33B�  B�  B�  B���B�  B�33B���B̙�BЙ�B���B�  B�  B�33B�ffB陚B�33B�B���B�  B�33C �3C��C� CL�CffC
� C��C��C��C�3C�3C��C��C� CL�CffC ffC"ffC$ffC&ffC(ffC*� C,� C.ffC0� C2ffC4��C6��C8�3C:��C<� C>L�C@��CB�3CD��CF� CHL�CJ� CL�3CN��CP��CR��CT��CV��CX��CZ��C\��C^�3C`�3Cb��Cd��CfL�ChffCj� Cl� Cn��Cp�3Cr�3Ct��Cv��CxL�CzffC|ffC~� C�@ C�L�C�@ C�@ C�@ C�33C�@ C�33C�&fC��C�L�C�ffC�Y�C�L�C�L�C�@ C�33C�L�C�ffC�Y�C�@ C�@ C�L�C�ffC�Y�C�@ C�33C�&fC�L�C�Y�C�ffC�Y�C�Y�C�L�C�@ C�33C�&fC�@ C�ffC�ffC�Y�C�Y�C�L�C�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�@ C�&fC�33C�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�L�C�s3C�L�C�&fC�33C�&fC�&fC�&fC�&fC�L�C�Y�C�L�C�@ C�L�C�ffC�L�C�33C�L�C�Y�C�@ C�Y�C�ffC�L�C�33C�@ C�Y�C�@ C�L�C�L�C�33C�L�C�Y�C�33C�@ C�ffC�L�C�L�C�Y�C�33C�@ C�@ C�L�C�&fC�33C�ffC�33C�&fC�L�D @ D �3D�fD�fD	  D,�DY�D�fD�fD�fD�3D�D  DY�D�3D �3D#�D%s3D'� D*�D,S3D.� D0�fD3&fD5s3D7�fD9��D<  D>,�D@Y�DB� DD�3DF�fDI&fDKS3DM�3DO�fDQ�fDT3DV9�DXFfDZS3D\Y�D^` D`ffDbl�Ddy�Df� Dh� Dj�fDl��Dn��Dp��Dr��Dt��Dv��DxٚDz�3D|��D~� D�vfD���D�� D��3D�� D�� D���D���D�  D�3D�&fD�33D�6fD�<�D�I�D�I�D�I�D�L�D�C3D�33D�&fD��D�3D�3D��fD���D���D�� D���D��3D�� D�|�D�l�D�VfD�@ D�)�D�fD�3D��fD�ɚD�� D�� D�p D�S3D�33D��D�  D�� D���D��fD��fD���D�� D���D��3D���D��3D��3D�y�D�vfD�y�D�vfD�|�D��3D���D��3D��3D��3DfDÖfDē3Dœ3DƉ�Dǃ3D�|�D�|�D�vfD�l�D�` D�\�D�S3D�@ D�6fD�0 D�  D� D�3D�  D�� D�� D���DضfDٙ�Dڃ3D�l�D�P D�3D���D�ɚD��D� D�S3D�33D��D���D�ٚD��3D�3D�p D�FfD�#3D�3D���D��D�3D�` D�I�D��D���D���D�|�D�fE �3E�Ec3E� E3ED�E~fE	� E
�E3E8 E� E� E!�EQ�E��E� E�fE��E��E� E0 E^fE� E �3E#ٚE&��E)��E-4�E0[3E3@ E6h E9��E<� E=S3E>!�E>��E?6fE@�E@� EAfEA�fEB\�EB�EC��ED.fEE  EE�fEF3EF�3EGVfEH�EH� EIP EI��EJ�3EK	�EK�3EL>fEL�EM��EN#3ENٚEO�3EP3EP��EQ` ER3ER|�ES  ES�3ETs3?L��?fffG�O�G�O�?L��G�O�?�  G�O�G�O�G�O�G�O�?�  G�O�G�O�?���G�O�?�ffG�O�?�  ?���?ٙ�?�33@��@��@   @333@@  @S33@`  @y��@�ff@�  @���@�33@���@�ff@�33@���@�33@�  @���@���A33A  A  AffAffA$��A+33A4��A;33AA��AK33AP  AX  A`  Ah  AnffAt��A|��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114414144441441414111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ �@ v@ V@ *@ O@ !s@ (�@ 0x@ 7L@ >�@ FQ@ R�@ `�@ m:@ z�@ ��@ �0@ �(@ ��@ ��@ �|@ �#@ ��@ �q@�@@�@-@:@H]@UU@b�@p�@~�@��@��@�A@��@��@�7@��@�@�,@�@*@#�@0x@>�@K�@X�@ff@t@�@�\@��@��@��@Ĝ@��@�H@��@�E@�@�@&;@1�@@,@N�@\�@k.@y�@�@�h@��@�@�k@�@׹@�@�@@�@O@'�@5�@C�@Q=@^�@l�@z3@��@�0@�(@�~@�w@�|@܀@�y@�q@j@�@g@-�@:�@G�@S�@b�@r@~�@��@�H@��@��@�>@��@ލ@��@��@�@*@ �@/@=q@K@Yn@g�@uk@��@�@��@��@��@��@Ӡ@��@��@��@
=@6@%�@2�@?}@Lu@\�@k�@x�@��@�u@�m@�f@�k@�o@�h@�@�Y@ �@�@�@(�@5�@B�@R�@`�@oF@|?@��@��@��@��@��@��@܀@�(@� @	�@	�@	 @	-@	:�@	H]@	UU@	b�@	p�@	~K@	��@	��@	��@	��@	�>@	��@	��@	�(@	�~@
v@
@
 �@
.l@
<�@
I�@
Wb@
e	@
s_@
�@
��@
��@
�M@
��@
Ĝ@
�C@
��@
�@
��@�@�@(G@3�@?}@M�@Z�@hs@v@��@�u@��@��@��@�@�@�`@�@ �@@O@*S@8�@D�@Q=@_�@n�@z�@�7@��@�(@�-@��@�@�t@�(@�q@@o@
@,`@:@H]@S�@bN@r�@}�@��@�H@��@��@&;@\�@��@��@b@K�@��@��@�9@4�@m:@��@�@$.@bN@�y@�H@ @^5@�@�#@�@Wb@�u@�*@
=@E�@�@��@��@3�@qS@��@�(@&;@`A@��@�\@V@FQ@}�@��@�4@#�@[z@��@�c@ �@7�@n�@��@�;@6@O�@��@�2@�L@(�@dZ@��@�t@*@N�@��@��@��@6�@qS@�@�`@�@T�@��@Ĝ@�9@2�@g@�H@�*@ @ 7L@ j@ �a@ ��@!%@!:@!k�@!��@!�7@"�@"5�@"g�@"��@"�o@"��@#0x@#`�@#�i@#@#�Y@$""@$R�@$�d@$�9@$�@%�@%Ji@%|?@%�r@%�T@&�@&M$@&�d@&��@&�4@'"�@'Wb@'�P@'Ĝ@'��@(2�@(j@(�(@(�#@)�@)H]@)�@)�F@)�4@*"�@*Wb@*��@*��@*�~@+-�@+bN@+�0@+�@, �@,33@,g�@,�@,�7@-j@-7L@-m:@-�m@-Ӡ@.%@.7�@.hs@.�H@.�@.��@/\�@/�D@/�^@0
@0N�@0�-@0��@1�@1y�@1�M@1�#@2>@2m:@2є@3 �@30x@3�@3��@4&;@4UU@4�+@4��@5�@5y�@5��@6g�@6�@7�W@8�@8��@9*S@9��@:9X@:��@;> @;�@<=q@<�@=i�@=��@>j@>�@?p�@?�@@��@A)�@A�@B33@B�9@C33@D�@E�`@G5@@Hy�@I�T@K;d@Lww@M�7@O&;@P��@P��@Q�@QWb@Q�@Q��@R""@R[z@R��@R�(@S"�@Sy�@S��@T
=@TC�@T|?@Tє@U	�@UYn@U�\@U�H@V�@Vg�@V��@V��@W!s@Wm�@W�k@W�L@X> @X��@X�@Y�@YQ�@Y��@Y�o@Z@ZZ@Z��@ @ �G�O�G�O�@ G�O�@ jG�O�G�O�G�O�G�O�@ jG�O�G�O�@ G�O�@ vG�O�@ �@ �@ 1@ 	�@ �@ �@ �@ �@ @ @ {@ 6@ B@ O@ [@ g@  �@ #�@ &;@ (G@ )�@ ,`@ /@ 1�@ 4�@ 6�@ :@ <�@ @,@ B�@ E�@ I�@ Lu@ O0@ SI@ UU@ X�@ \)@ _�@ bN@ e	@ hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�t�AŋDAŋDAœuAř�Ař�Ař�Ař�Aś�Aś�Aş�Aš�Aş�Aš�Aţ�Ať�Aş�A�|�A�JA��TA���AľwAĮAĩ�Aħ�Aħ�Aħ�Aħ�Aħ�Aħ�Aģ�Ać+AăA�n�A�I�A�33A��A��A�ȴAÓuA�33APA�\)A��A�?}A�ffA�A�A��;A���A�-A��A���A���A��A��A���A��A���A��jA��PA�hsA�/A��A��jA���A���A�/A���A�1'A��A��A�K�A��\A��mA��yA�bA��mA�"�A�ƨA�(�A���A�A���A�dZA��-A���A���A�~�A�+A��A�ffA��A��A��A�7LA��^A�n�A���A�C�A�ffA�|�A���A��mA�jA��wA��/A��;A��-A�7LA�bA��A�&�A~ffA|M�A{%AyG�Au�hAqoApv�Ao��AoS�AnZAj9XAg�Af�!Af-Ae7LAdI�Ac��Ac�Ac�Ab�RAa��A_XA\�A[�AY�mAXffAW
=AU��AT�9AS��AQ�APbAMx�AL{AK�7AJbNAH��AG/AC�AA33A>��A=�^A<-A:ZA9S�A8�A733A6�A4ȴA3�wA2��A/�A.��A.VA-�^A+�TA*�jA)?}A(v�A'�FA&ĜA&5?A$�/A"ZA!x�A 1'An�A�FA��Az�A�/A�TAC�A�AVA�+A�mA��AK�AZA%AZA�AS�A�uA�wA�uA
ZA	t�A�\A��Al�A%AA�A�#AXA��AƨA|�A%A�
AoA �yA �+@��@��h@�z�@�+@���@�?}@��u@��@��-@�A�@�@�7L@�?}@��@�+@�!@睲@�J@�S�@��@�~�@��@���@ް!@���@܃@���@�(�@�b@�dZ@���@ם�@�\)@�ƨ@�@�G�@ӝ�@��m@�(�@�Ĝ@�=q@���@�V@���@̼j@��;@�@�M�@��P@���@�n�@�x�@��D@��@�/@���@��!@�V@���@��@�n�@�bN@��D@�S�@�j@�S�@��@�bN@��!@���@�-@���@��m@���@�o@�{@�hs@�9X@��@�1'@���@���@�p�@�/@�I�@��@�M�@���@���@�E�@��/@��
@��@��#@��-@��
@���@�@��/@� �@|�@}�@|��@{�F@z�\@x�`@w��@v�+@v$�@u��@tI�@s"�@q��@o�;@n�@m�T@l��@k"�@iX@h1'@f�+@dZ@b��@a7L@`1'@_�@]@\(�@[��@XbN@W\)@V�@VV@Tz�@S33@Q�^@QX@PA�@O;d@N��@M�@Kƨ@J�@HĜ@GK�@F��@E�-@Dz�@CS�@A��@@�9@?�@>ff@>$�@<��@;�@:��@9G�@7�@6��@4�/@41@3"�@2M�@1%@0bN@0Q�@.ȴ@-�h@-�@,�@+t�@*~�@)X@)%@(bN@'��@&�R@%�h@%O�@$��@$I�@#dZ@"-@!�7@!7L@ �u@l�@ȴ@E�@��@@�h@9X@��@��@^5@�@r�@A�@�@+@��@@��@�@��@t�@�`@��@@j@dZ@
��@
��@	�@�`@ �@
=@E�@��@1@�@o@M�@ Ĝ?���?��H?��?�l�?�?}?��/?�9X?�?�Ĝ?��;?�V?�?�x�?���?��T?�j?�\?�&�?��?޸R?���?ݑh?܋D?�"�?���?��T?���?�M�?�|�?�/?�1?ə�?�l�?��T?�?���?ļj?�z�?�t�?�33?�o?��?�o?°!?°!?���?���?�hs?�G�?�Ĝ?�bN?� �?� �?��w?���?��?�v�?�{?��?���?�p�?�O�?�/?�/?�/?���?��?��D?��?��?��?��A�jA�hsA�dZA�bNA�`BA�^5A�bNA�ffA�l�A�n�A�jA�jA�t�A�r�A�r�A�r�A�x�A�~�A�~�AŃAŁAŇ+Aŏ\Aŏ\AōPAŉ7AŁAŉ7Aŉ7Aŏ\AőhAŏ\Aŗ�Aŗ�Ař�Ař�Ař�Ař�Ař�Aŗ�Ař�Ař�Ař�Ař�Aś�Aś�Aś�Aś�Aŝ�Aś�Aŝ�Aŝ�Aš�Aş�Aş�Aš�Aš�Aţ�Aš�Aš�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�dZA�t�AŋDAŋDAœuAř�Ař�Ař�Ař�Aś�Aś�Aş�Aš�Aş�Aš�Aţ�Ať�Aş�A�|�A�JA��TA���AľwAĮAĩ�Aħ�Aħ�Aħ�Aħ�Aħ�Aħ�Aģ�Ać+AăA�n�A�I�A�33A��A��A�ȴAÓuA�33APA�\)A��A�?}A�ffA�A�A��;A���A�-A��A���A���A��A��A���A��A���A��jA��PA�hsA�/A��A��jA���A���A�/A���A�1'A��A��A�K�A��\A��mA��yA�bA��mA�"�A�ƨA�(�A���A�A���A�dZA��-A���A���A�~�A�+A��A�ffA��A��A��A�7LA��^A�n�A���A�C�A�ffA�|�A���A��mA�jA��wA��/A��;A��-A�7LA�bA��A�&�A~ffA|M�A{%AyG�Au�hAqoApv�Ao��AoS�AnZAj9XAg�Af�!Af-Ae7LAdI�Ac��Ac�Ac�Ab�RAa��A_XA\�A[�AY�mAXffAW
=AU��AT�9AS��AQ�APbAMx�AL{AK�7AJbNAH��AG/AC�AA33A>��A=�^A<-A:ZA9S�A8�A733A6�A4ȴA3�wA2��A/�A.��A.VA-�^A+�TA*�jA)?}A(v�A'�FA&ĜA&5?A$�/A"ZA!x�A 1'An�A�FA��Az�A�/A�TAC�A�AVA�+A�mA��AK�AZA%AZA�AS�A�uA�wA�uA
ZA	t�A�\A��Al�A%AA�A�#AXA��AƨA|�A%A�
AoA �yA �+@��@��h@�z�@�+@���@�?}@��u@��@��-@�A�@�@�7L@�?}@��@�+@�!@睲@�J@�S�@��@�~�@��@���@ް!@���@܃@���@�(�@�b@�dZ@���@ם�@�\)@�ƨ@�@�G�@ӝ�@��m@�(�@�Ĝ@�=q@���@�V@���@̼j@��;@�@�M�@��P@���@�n�@�x�@��D@��@�/@���@��!@�V@���@��@�n�@�bN@��D@�S�@�j@�S�@��@�bN@��!@���@�-@���@��m@���@�o@�{@�hs@�9X@��@�1'@���@���@�p�@�/@�I�@��@�M�@���@���@�E�@��/@��
@��@��#@��-@��
@���@�@��/@� �@|�@}�@|��@{�F@z�\@x�`@w��@v�+@v$�@u��@tI�@s"�@q��@o�;@n�@m�T@l��@k"�@iX@h1'@f�+@dZ@b��@a7L@`1'@_�@]@\(�@[��@XbN@W\)@V�@VV@Tz�@S33@Q�^@QX@PA�@O;d@N��@M�@Kƨ@J�@HĜ@GK�@F��@E�-@Dz�@CS�@A��@@�9@?�@>ff@>$�@<��@;�@:��@9G�@7�@6��@4�/@41@3"�@2M�@1%@0bN@0Q�@.ȴ@-�h@-�@,�@+t�@*~�@)X@)%@(bN@'��@&�R@%�h@%O�@$��@$I�@#dZ@"-@!�7@!7L@ �u@l�@ȴ@E�@��@@�h@9X@��@��@^5@�@r�@A�@�@+@��@@��@�@��@t�@�`@��@@j@dZ@
��@
��@	�@�`@ �@
=@E�@��@1@�@o@M�@ Ĝ?���?��H?��?�l�?�?}?��/?�9X?�?�Ĝ?��;?�V?�?�x�?���?��T?�j?�\?�&�?��?޸R?���?ݑh?܋D?�"�?���?��T?���?�M�?�|�?�/?�1?ə�?�l�?��T?�?���?ļj?�z�?�t�?�33?�o?��?�o?°!?°!?���?���?�hs?�G�?�Ĝ?�bN?� �?� �?��w?���?��?�v�?�{?��?���?�p�?�O�?�/?�/?�/?���?��?��D?��?��?��?��A�jA�hsA�dZA�bNA�`BA�^5A�bNA�ffA�l�A�n�A�jA�jA�t�A�r�A�r�A�r�A�x�A�~�A�~�AŃAŁAŇ+Aŏ\Aŏ\AōPAŉ7AŁAŉ7Aŉ7Aŏ\AőhAŏ\Aŗ�Aŗ�Ař�Ař�Ař�Ař�Ař�Aŗ�Ař�Ař�Ař�Ař�Aś�Aś�Aś�Aś�Aŝ�Aś�Aŝ�Aŝ�Aš�Aş�Aş�Aš�Aš�Aţ�Aš�Aš�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B+B%B+B%B+B+B+B%B%B%B+B+B+B+B+B1B�B7LBz�B�1B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�dB�wBĜB��B��B�NB�sB�B�fB��B��B�B��B��BB�B!�B'�B,B.B2-B1'B1'B0!B0!B/B/B(�B%�B%�B"�B�B�B{BDB%BB��B�BB%�B+B,B"�B�B�B{B\BbBbBVB	7BB�B�fB�5B�B��BȴB�9B��B�BcTBP�B?}BoB
��B
�ZB
�wB
�B
��B
v�B
G�B
;dB
+B
�B
\B
	7B	��B	�)B	��B	ɺB	B	�dB	�-B	�oB	�B	|�B	x�B	r�B	k�B	iyB	iyB	jB	dZB	ZB	G�B	6FB	,B	%�B	 �B	�B	{B	bB	B��B�B�)B�B��B��BȴB��B�!B��B��B��B�JB�=B�1B�B�B�B}�B{�Bu�Bs�Bo�Bm�BjBdZBbNB`BB]/B\)B\)B[#BW
BVBW
BO�BS�BQ�BL�BJ�BE�BD�BA�BA�B>wB<jB<jB:^B9XB7LB7LB7LB6FB49B2-B1'B0!B1'B.B,B,B)�B'�B)�B)�B)�B(�B)�B)�B+B+B+B-B-B/B/B/B/B0!B0!B/B0!B1'B.B2-B1'B2-B6FB5?B7LB;dB6FB:^B9XB9XB;dB<jB=qB=qB?}BH�BS�BXB]/Bk�BjBr�By�B{�Bx�Br�BgmBhsBk�Bv�Bz�B�B�bB�oB�uB��B�!BĜB�B�`B�B��B��B	DB	�B	&�B	(�B	'�B	&�B	33B	E�B	G�B	S�B	_;B	bNB	iyB	o�B	{�B	�B	�%B	��B	��B	��B	��B	�B	�dB	ÖB	��B	��B	��B	�B	�B	�/B	�NB	�TB	�fB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
+B
1B
	7B
	7B

=B
JB
VB
VB
oB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
&�B
'�B
'�B
,B
,B
-B
-B
1'B
2-B
49B
49B
5?B
6FB
6FB
7LB
9XB
:^B
;dB
<jB
<jB
>wB
>wB
>wB
A�B
B�B
B�B
D�B
C�B
F�B
G�B
G�B
I�B
J�B
J�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
R�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
XB
XB
YB
[#B
ZB
\)B
\)B
\)B
^5B
_;B
^5B
`BB
aHB
aHB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
gmB
gmB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
l�B
l�B
k�B
n�B
o�B
q�B
s�B
s�B
t�B
t�B
v�B
v�B
x�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
� B
�B
�B
�B
�B
�%B
�+B
�+B
�1B
�7B
�=B
�JB
�\B
�\B
�bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�3B
�9B
�3B%B%B+BB	7B+B+B+B1B%B+B%B%B+B%B+B1B1B+B%B+B1B%B%BBB+B%B1B%B%B+B%B+B+B%B+B%B+B+B+B+B%B+B%B%B%B+B%B%B%B+B%B+B+B+B+B+B+B+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         BBBBBBBBBBBBBBBBBBuB49Bw�B�B�DB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�RB�dB��BǮB��B�;B�`B�B�TB��B�B�B��B��BB{B�B$�B(�B+B/B.B.B-B-B,B,B%�B"�B"�B�B�B�BhB1BB  B�B�B��B"�B'�B(�B�B�B�BhBJBPBPBDB%B��B�B�TB�#B��B��BŢB�'B��B~�B`BBM�B<jB\B
�B
�HB
�dB
��B
�{B
s�B
D�B
8RB
'�B
�B
JB
%B	��B	�B	��B	ƨB	�}B	�RB	�B	�\B	~�B	y�B	u�B	o�B	hsB	ffB	ffB	gmB	aHB	W
B	D�B	33B	(�B	"�B	�B	�B	hB	PB	B�B�B�B�B��B��BŢB�qB�B��B��B�{B�7B�+B�B� B�B~�Bz�Bx�Br�Bp�Bl�BjBgmBaHB_;B]/BZBYBYBXBS�BR�BS�BL�BP�BN�BI�BG�BB�BA�B>wB>wB;dB9XB9XB7LB6FB49B49B49B33B1'B/B.B-B.B+B(�B(�B&�B$�B&�B&�B&�B%�B&�B&�B'�B'�B'�B)�B)�B,B,B,B,B-B-B,B-B.B+B/B.B/B33B2-B49B8RB49B8RB7LB6FB8RB:^B;dB;dB<jBF�BQ�BVB[#BiyBhsBp�Bw�By�Bv�Bp�Be`BffBiyBt�Bx�B�B�VB�bB�hB��B�BB�B�TB�B�B��B		7B	�B	$�B	&�B	%�B	$�B	1'B	C�B	E�B	Q�B	]/B	`BB	gmB	m�B	y�B	�B	�B	��B	��B	��B	��B	�B	�XB	��B	ȴB	��B	��B	�
B	�B	�#B	�BB	�HB	�ZB	�mB	�yB	�B	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
+B
1B

=B
JB
JB
bB
oB
uB
uB
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
$�B
%�B
%�B
)�B
)�B
+B
+B
/B
0!B
2-B
2-B
33B
49B
49B
5?B
7LB
8RB
9XB
:^B
:^B
<jB
<jB
<jB
?}B
@�B
@�B
C�B
B�B
E�B
F�B
F�B
H�B
I�B
I�B
L�B
L�B
L�B
M�B
N�B
O�B
O�B
Q�B
R�B
R�B
R�B
S�B
T�B
VB
VB
W
B
W
B
XB
ZB
YB
[#B
[#B
[#B
]/B
^5B
]/B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
ffB
ffB
e`B
ffB
ffB
gmB
gmB
hsB
hsB
k�B
k�B
jB
m�B
n�B
p�B
r�B
r�B
s�B
s�B
u�B
u�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
~�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�1B
�7B
�DB
�\B
�\B
�bB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�?B
�9B
�9B
�?B
�9BBBBB%BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201901221800452021061413531220210614135312202106141747052021061417470520210614174705201901221800452021061413531220210614135312202106141747052021061417470520210614174705PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019012218004520190122180045  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012218004520190122180045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012218004520190122180045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015720210722160157IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                