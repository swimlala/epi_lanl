CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:37Z creation      
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
_FillValue                  � 	�      � 	�Argo profile    3.1 1.2 19500101000000  20180724220237  20210722160150  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�p��F@�p��F11  @�p�$p@�p�$p@6�|ß�a@6�|ß�a�c��q���c��q��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?L��@ff@Fff@�ff@�33@�33@�  A   A  A#33A@  Aa��A���A�  A�  A�  A�  A���A�  A���B ��B��B33B��B��B(  B0ffB8ffB@��BHffBO��BX  B`ffBhffBpffBxffB�33B�  B�  B�  B�ffB�33B���B�  B���B�  B�ffB�33B�33B�33B�33B�  B�  B�33B�33B�33B�  B�  B�33B�  B���B�  B�  B�33B�33B�  B���B�33C �C�C  C�fC33C
�C�C  C�fCL�C�C  C�fC��C33C�C   C"33C$�C&�C(�C*  C,  C-�fC/�fC1��C433C633C8  C:  C<  C>33C@�CB�CD33CF�CG�fCJ�CL�CN  CP33CR�CS�fCV�CX�CZ  C\  C]�fC`33Cb�Cd  Cf33Ch�Cj  Cl33Cn33Cp�Cr  Cs�fCv  Cx  Cy��C|  C~L�C��C��3C��C��C��fC��C�  C��3C��C��C��3C��C�  C��fC��C�&fC�&fC�  C�&fC��C�  C��C�  C��3C��C��3C��fC�  C��C��C��3C��C�  C��fC��3C��C��C��fC��C��3C��fC��C�  C��fC��C�  C�  C��C��C��C�&fC��C�  C��C��C�  C��C��C�  C��C�  C��fC��C�&fC��C��3C��C�&fC��C��3C��C��C�  C��fC�  C��C�&fC��C��3C��C��C�  C��fC��3C�  C��C��C��fC�  C��C�&fC��C�  C��C�  C��fC�  C��C�&fC��C��3C��C�  C��fC�  C��C�&fC��C��3C��C��C��C��3C��C�&fC��C��fC�  C�  C��C�  C��fC��3D�D�3D&fD	� D` D�D� Dy�D,�D��D��Ds3D"&fD$�fD'�3D*@ D,� D/� D2�D4� D7�D9�fD<,�D>�fDAL�DC�fDFs3DIfDK� DN,�DP�fDSY�DU��DX�fD[9�D]� D`�fDc@ De�3Dh� Dk33Dm�3Dpy�Ds&fDu�fDxffD{�D}L�D�3D�FfD�� D���D� D�P D���D��fD�33D�vfD��3D��D�P D���D�ٚD��D�Y�D��fD��fD�)�D�l�D��3D�  D�C3D��fD���D�	�D�S3D���D��D�33D�y�D�ɚD�fD�ffD��fD�3D�P D���D�� D�&fD�p D���D�fD�S3D��fD��fD�6fD��3D��fD� D�L�D�D�ɚD�  D�<�D�p DȦfD��3D�3D�0 D�\�DΉ�D϶fD��3D�3D�FfD�vfDթ�D��fD�3D�9�D�vfD۩�D��fD�fD�0 D�c3D�fD�ɚD���D�6fD�i�D��D�ٚD� D�I�D� D���D�3D�9�D�p D� D��3D�fD�9�D�l�D���D�ɚD�� D��3D�3D�<�D�c3E A�E �3EX E��Eh E�fEt�E�fE��E	�E� E�E��E3E��E�E�fE	�E
$�E!�E�3E� E3E3E{3E�fE��Ec3Ec3E� E� E\�EY�E��E��EVfE L�E!� E#.fE$�E%��E&��E'� E)<�E*��E+��E,�E.X E/A�E0��E2�E3  E4i�E5�3E6��E8+3E9��E:�fE;�3E?3EB�EE+3EHA�EKffEN��EQ�3ET� EX1�E[0 E^�EaFfEdQ�Eg� Ej� Em��Ep�fEt Ew1�EzVfE}Q�E�I�E��fE�p E��E���E�'3E�� E�, E���E�L E��fE�FfE�� E�� E��E���E��fE�3E�]�E���E��E�X E��3E��fE�P E���E��f>���>���?   ?   ?   ?��>���?��?   ?��>���?��?   ?333?��?333?L��?fff?�  ?���?�ff?�  ?ٙ�?�33@��@��@,��@@  @S33@Y��@y��@�ff@�  @�  @�ff@�ff@�33@���@ٙ�@�33@�33A   A  A��A33A��A#33A)��A1��A8  A@  AH  AK33AS33A[33A`  Ah  Ap  At��A|��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414141414141111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?fff?�ff@&ff@fff@�ff@�33@�33@�  A  A  A+33AH  Ai��A���A�  A�  A�  A�  A���A�  A���B��B	��B33B��B!��B*  B2ffB:ffBB��BJffBQ��BZ  BbffBjffBrffBzffB�33B�  B�  B�  B�ffB�33B���B�  B���B�  B�ffB�33B�33B�33B�33B�  B�  B�33B�33B�33B�  B�  B�33B�  B���B�  B�  B�33B�33B�  B���B�33C ��C��C� CffC�3C
��C��C� CffC��C��C� CffCL�C�3C��C � C"�3C$��C&��C(��C*� C,� C.ffC0ffC2L�C4�3C6�3C8� C:� C<� C>�3C@��CB��CD�3CF��CHffCJ��CL��CN� CP�3CR��CTffCV��CX��CZ� C\� C^ffC`�3Cb��Cd� Cf�3Ch��Cj� Cl�3Cn�3Cp��Cr� CtffCv� Cx� CzL�C|� C~��C�L�C�33C�L�C�L�C�&fC�L�C�@ C�33C�Y�C�L�C�33C�Y�C�@ C�&fC�L�C�ffC�ffC�@ C�ffC�L�C�@ C�Y�C�@ C�33C�L�C�33C�&fC�@ C�Y�C�L�C�33C�L�C�@ C�&fC�33C�Y�C�L�C�&fC�L�C�33C�&fC�L�C�@ C�&fC�L�C�@ C�@ C�Y�C�Y�C�L�C�ffC�L�C�@ C�Y�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�@ C�&fC�L�C�ffC�Y�C�33C�L�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�33C�@ C�Y�C�L�C�&fC�@ C�Y�C�ffC�L�C�@ C�L�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�L�C�@ C�&fC�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�L�C�ffC�L�C�&fC�@ C�@ C�Y�C�@ C�&fC�33D9�D�3DFfD	� D� D,�D� D��DL�D�D��D�3D"FfD%fD'�3D*` D-  D/� D29�D4� D79�D9�fD<L�D>�fDAl�DDfDF�3DI&fDK� DNL�DP�fDSy�DV�DX�fD[Y�D^  D`�fDc` Df3Dh� DkS3Dm�3Dp��DsFfDu�fDx�fD{,�D}l�D�	�D�VfD�� D���D�  D�` D���D��fD�C3D��fD��3D��D�` D���D��D�,�D�i�D��fD��fD�9�D�|�D��3D� D�S3D��fD���D��D�c3D���D���D�C3D���D�ٚD�&fD�vfD��fD�3D�` D���D�� D�6fD�� D���D�fD�c3D��fD��fD�FfD��3D��fD�  D�\�D�D�ٚD� D�L�Dǀ DȶfD��3D�3D�@ D�l�DΙ�D��fD��3D�#3D�VfDԆfDչ�D��fD�3D�I�DچfD۹�D��fD�fD�@ D�s3D�fD�ٚD��D�FfD�y�D��D��D�  D�Y�D� D���D�3D�I�D� D� D��3D�fD�I�D�|�D���D�ٚD�� D�3D�#3D�L�D�s3E I�E �3E` E��Ep E�fE|�EfE��E�E� E�E��E#3E��E!�E�fE	$�E
,�E)�E�3E� E3E3E�3E�fE��Ek3Ek3E� E� Ed�Ea�E��E��E^fE T�E!� E#6fE$$�E%��E&��E'� E)D�E*��E+��E,��E.` E/I�E0��E2�E3 E4q�E5�3E6ɚE833E9��E:�fE;�3E?3EB$�EE33EHI�EKnfEN��EQ�3ET� EX9�E[8 E^�EaNfEdY�Eg� Ej� Em��Ep�fEt Ew9�Ez^fE}Y�E�M�E��fE�t E��E���E�+3E�� E�0 E���E�P E��fE�JfE�� E�� E��E���E��fE�3E�a�E���E��E�\ E��3E��fE�T E���E��fG�O�?fffG�O�G�O�?�  G�O�?fffG�O�?�  G�O�?fffG�O�?�  G�O�?���?���?�ff?�33?�  ?���?�ff@   @��@��@,��@9��@L��@`  @s33@y��@���@�ff@�  @�  @�ff@�ff@�33@���@陚@�33A��A  A  A��A33A$��A+33A1��A9��A@  AH  AP  AS33A[33Ac33Ah  Ap  Ax  A|��A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414141414141111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ �@ v@ V@ *@ �@ "�@ )�@ /�@ 6�@ =q@ E�@ Q�@ `B@ n�@ z�@ ��@ �0@ ��@ �-@ �&@ �|@ ��@ �m@ �e@�@b@�@-@:�@I@V@bN@p�@~�@��@�H@��@��@@�7@��@��@��@%@{@!s@/�@>�@K�@Yn@g@t�@��@�\@��@�Y@�@��@Ӡ@��@��@��@
=@�@&;@3�@@�@M�@\�@j@x&@�@�@��@��@�k@�c@�[@�@�@  @�@�@*S@7L@DD@SI@`B@m�@{�@��@�0@�(@��@��@�*@��@�@��@j@o@g@-@;d@H]@T�@c�@qS@~K@�P@�H@��@��@�>@�7@��@��@��@�@{@#�@0x@=q@Lu@Z@g@t@�@�\@�@�M@�R@�@�O@��@�@�E@�@�@%�@2�@B8@O0@[z@k.@ww@��@�u@�y@�!@��@�o@׹@�@�@  @�@�@(G@5?@DD@SI@`B@l�@{�@��@��@�(@��@��@�o@�#@�m@�e@	@	@	[@	-@	:@	G�@	V�@	dZ@	qS@	�W@	��@	��@	��@	��@	@	є@	�;@	�@	��@
�@
@
"�@
1�@
>�@
Ji@
Yn@
hs@
t�@
�@
�@
�a@
��@
��@
��@
��@
�T@
�@
��@
�@B@%�@1�@@,@N�@]�@j@v@�@�$@�y@��@��@�@�
@�T@�Y@ �@�@�@(G@7L@DD@P�@_�@n�@|�@�7@��@��@��@��@�@�#@�(@�q@@@�@-�@:@FQ@T�@��@$.@j@�~@�,@B8@��@�\@ @k.@�F@@K�@��@��@(�@p�@�R@�Q@DD@��@�|@o@Yn@�a@�`@+@qS@�R@��@D�@�D@є@�@bN@��@�@=q@�+@�7@�@^5@��@�@7L@~�@�W@�@M$@�#@�t@
@b�@�A@�@@4�@{�@��@�@M�@��@�@[@bN@��@��@ 1'@ v@ �@! �@!G�@!��@!є@"6@"Z�@"�@"�m@#.l@#t�@#�^@$@$I@$��@$�h@%g@%ff@%��@%�Y@&7�@&~K@&�J@'�@'R�@'��@'�;@(&�@(m�@(��@(�,@)<�@)�W@)Ĝ@*�@*Ji@*��@*�*@+V@+O0@+�\@+ψ@,�@,O�@,�@,��@-o@-SI@-��@-��@.*@.Wb@.��@.܀@/�@/]�@/�@/ލ@0 @0a�@0�(@0�@1'�@1i!@1��@1��@21�@2ww@2�@2�E@3?}@3��@3@4@4E�@4�+@4ȴ@5�@5I�@5��@5��@5�E@6<�@6{�@6�@6�@7/�@7hs@7��@7�/@8�@8Q=@8��@8�>@8��@95@@9m�@9�4@9܀@:�@:F�@:�W@:��@;\�@<]@<m:@=�@=uk@>o@>��@?[@?�@@*S@@��@A:@A܀@BH]@B�@CZ�@C��@De	@E  @E��@Fv@F�@G8�@G�@H5@@H��@I5@@I�@Jb�@Jƨ@K\�@K��@L_�@L��@M�#@M��@N�#@O0x@O��@P*S@Q�|@R�C@T @UqS@Vȴ@X<@Y�@Z��@\> @]�@^�2@`	@aj@b��@d�@e�+@f��@h�@ix&@jψ@l*@mx�@nլ@p(�@qk.@r�@t/@uz�@v@x�@ym:@z�1@{Z@{X�@{�0@{є@|-@|hr@|��@|�@}"�@}�@}��@}�}@~5?@~�\@~�@G�O�@ �G�O�G�O�@ jG�O�@ �G�O�@ jG�O�@ �G�O�@ jG�O�@ @ �@ v@ %@ �@ �@ �@ 
=@ �@ �@ @ b@ o@ {@ �@ 6@ �@ �@ �@ ""@ #�@ &�@ )�@ +�@ .l@ 0x@ 3�@ 6�@ :@ <@ >�@ B�@ E�@ H]@ K�@ N�@ Q�@ UU@ V�@ Z@ ]�@ _�@ b�@ ff@ hs@ k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��mA���A�A�A�  A���A���A�
=A�JA�
=A�1A�
=A�
=A�
=A�JA���A��TAΕ�A̗�A�C�A�33A�bA��A�bNA�hsA�M�A��A���A�(�A��A�ZA�&�A��A��9A���A�O�A��A�jA�p�A�oA���A��A���A��;A�r�A�(�A��PA���A�z�A���A���A���A�ffA���A�|�A��jA�ȴA�jA�9XA�bA��A���A�&�A�ĜA��A��A�?}A��HA�JA���A�5?A�9XA���A�VA��A���A�S�A��A��A��A�A�A�=qA�G�A��A���A�n�A��A�%A��7A�hsA��wA���A��DA���A�~�A�G�A� �A���A�x�A���A�?}A�E�A���A��A���A��A�v�A��`A�K�A�  A�t�A���A���AC�A|n�Az(�Ay��Axn�AxJAw`BAv�Aux�Aq��Ap=qAmp�AfjAex�Ac�^Aa�A_�A]hsA\n�A[��AZ�\AY��AY�7AY33AXĜAW��AVAT��AP�yAM�mALffAKG�AJ^5AIhsAHI�AGx�AE�#AD��ADffAC��AA�mA@��A@$�A?VA>�A=�A<�/A;��A;p�A:�A:�9A:E�A9?}A8=qA7�#A7�7A6I�A5?}A49XA3��A2�`A1;dA/x�A-�A-C�A,�uA+hsA*�A(��A'��A'�A&��A&��A&bNA$�A$ZA#%A!�A �yA (�A�A~�A�#A�!A�hA�#A-A�A��A�mA�A�A�A~�At�AoA��AQ�AA��A�A	7LAjAƨA�AS�AȴAZAJAhsAƨA j@�t�@�@�E�@�Ĝ@�  @��@�-@���@�
=@�G�@�S�@���@�J@���@�D@�(�@��@��@�z�@�|�@�;d@��@�@���@�ff@���@�bN@�33@�n�@���@���@�@�ff@Ų-@�Q�@�v�@�ff@���@�G�@��`@�ff@�\)@��R@��T@�hs@�ȴ@�?}@���@�
=@��^@���@���@���@���@�|�@�
=@���@�$�@�{@�/@��j@��u@�Ĝ@�z�@��h@���@��y@�M�@��#@���@���@�&�@�b@}��@|��@|�@{@{dZ@zJ@xb@v{@t�D@r��@q%@pQ�@ol�@n��@n$�@l�/@kdZ@i��@hbN@d��@c��@b~�@`1'@^��@]p�@Z��@Y&�@W�;@W;d@V�@U@S��@SdZ@R-@Q%@O;d@M��@Lj@KS�@I�@H��@HbN@F��@D�@A��@@��@?�P@?l�@=�@<��@;��@9�@7�;@7l�@6V@5p�@4�/@4Z@3@1�^@0�u@/�@-p�@,I�@+S�@*=q@(�`@'�@&��@&��@%�T@$j@#ƨ@#t�@"��@!G�@|�@ff@@O�@��@�@dZ@�H@J@��@&�@r�@ �@��@�R@v�@�+@�T@��@S�@n�@�7@�u@;d@V@/@�/@��@
��@
-@	��@	�@A�@�@�R@��@�/@Z@S�@n�@��@ �u?�|�?�;d?�v�?��h?�O�?��D?�1?�^5?���?�1'?��+?���?�o?�G�?��;?���?��H?�x�?��?�?��
?�n�?��?�|�?��?�p�?�C�?���?ٺ^?ؓu?׮?��?ӕ�?�S�?ѩ�?�A�?��;?��?�V?��?�O�?�(�?�dZ?�~�?��#?ə�?���?�Q�?�1'?�+?š�?��?�-?��7?�A�?���?�5??��-?�/?��D?�j?�C�?��H?�?��H?�^5?���?��H?��H?��H?�?�dZ?�ƨ?�I�?��D?�V?�p�?��?���?�;d?��;?��?���?�Ĝ?�Ĝ?��`?�&�?�G�?�hs?��7?���?���?���?��?�-?�M�?\?°!A��A��A��yA��A��A��A��yA��A��A��A��A��/A��TA��HA��#A��;A��;A��#A��/A��;A���A�A�A���A���A���A�A�A�A�A�  A�A�A�  A���A���A���A���A���A���A�%A�JA�
=A�
=A�JA�
=A�
=A�
=A�
=A�
=A�1A�1A�1A�%A�
=A�
=A�JA�1A�
=A�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A��A��mA���A�A�A�  A���A���A�
=A�JA�
=A�1A�
=A�
=A�
=A�JA���A��TAΕ�A̗�A�C�A�33A�bA��A�bNA�hsA�M�A��A���A�(�A��A�ZA�&�A��A��9A���A�O�A��A�jA�p�A�oA���A��A���A��;A�r�A�(�A��PA���A�z�A���A���A���A�ffA���A�|�A��jA�ȴA�jA�9XA�bA��A���A�&�A�ĜA��A��A�?}A��HA�JA���A�5?A�9XA���A�VA��A���A�S�A��A��A��A�A�A�=qA�G�A��A���A�n�A��A�%A��7A�hsA��wA���A��DA���A�~�A�G�A� �A���A�x�A���A�?}A�E�A���A��A���A��A�v�A��`A�K�A�  A�t�A���A���AC�A|n�Az(�Ay��Axn�AxJAw`BAv�Aux�Aq��Ap=qAmp�AfjAex�Ac�^Aa�A_�A]hsA\n�A[��AZ�\AY��AY�7AY33AXĜAW��AVAT��AP�yAM�mALffAKG�AJ^5AIhsAHI�AGx�AE�#AD��ADffAC��AA�mA@��A@$�A?VA>�A=�A<�/A;��A;p�A:�A:�9A:E�A9?}A8=qA7�#A7�7A6I�A5?}A49XA3��A2�`A1;dA/x�A-�A-C�A,�uA+hsA*�A(��A'��A'�A&��A&��A&bNA$�A$ZA#%A!�A �yA (�A�A~�A�#A�!A�hA�#A-A�A��A�mA�A�A�A~�At�AoA��AQ�AA��A�A	7LAjAƨA�AS�AȴAZAJAhsAƨA j@�t�@�@�E�@�Ĝ@�  @��@�-@���@�
=@�G�@�S�@���@�J@���@�D@�(�@��@��@�z�@�|�@�;d@��@�@���@�ff@���@�bN@�33@�n�@���@���@�@�ff@Ų-@�Q�@�v�@�ff@���@�G�@��`@�ff@�\)@��R@��T@�hs@�ȴ@�?}@���@�
=@��^@���@���@���@���@�|�@�
=@���@�$�@�{@�/@��j@��u@�Ĝ@�z�@��h@���@��y@�M�@��#@���@���@�&�@�b@}��@|��@|�@{@{dZ@zJ@xb@v{@t�D@r��@q%@pQ�@ol�@n��@n$�@l�/@kdZ@i��@hbN@d��@c��@b~�@`1'@^��@]p�@Z��@Y&�@W�;@W;d@V�@U@S��@SdZ@R-@Q%@O;d@M��@Lj@KS�@I�@H��@HbN@F��@D�@A��@@��@?�P@?l�@=�@<��@;��@9�@7�;@7l�@6V@5p�@4�/@4Z@3@1�^@0�u@/�@-p�@,I�@+S�@*=q@(�`@'�@&��@&��@%�T@$j@#ƨ@#t�@"��@!G�@|�@ff@@O�@��@�@dZ@�H@J@��@&�@r�@ �@��@�R@v�@�+@�T@��@S�@n�@�7@�u@;d@V@/@�/@��@
��@
-@	��@	�@A�@�@�R@��@�/@Z@S�@n�@��@ �u?�|�?�;d?�v�?��h?�O�?��D?�1?�^5?���?�1'?��+?���?�o?�G�?��;?���?��H?�x�?��?�?��
?�n�?��?�|�?��?�p�?�C�?���?ٺ^?ؓu?׮?��?ӕ�?�S�?ѩ�?�A�?��;?��?�V?��?�O�?�(�?�dZ?�~�?��#?ə�?���?�Q�?�1'?�+?š�?��?�-?��7?�A�?���?�5??��-?�/?��D?�j?�C�?��H?�?��H?�^5?���?��H?��H?��H?�?�dZ?�ƨ?�I�?��D?�V?�p�?��?���?�;d?��;?��?���?�Ĝ?�Ĝ?��`?�&�?�G�?�hs?��7?���?���?���?��?�-?�M�?\?°!A��A��A��yA��A��A��A��yA��A��A��A��A��/A��TA��HA��#A��;A��;A��#A��/A��;A���A�A�A���A���A���A�A�A�A�A�  A�A�A�  A���A���A���A���A���A���A�%A�JA�
=A�
=A�JA�
=A�
=A�
=A�
=A�
=A�1A�1A�1A�%A�
=A�
=A�JA�1A�
=A�
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B%�B0!BN�B��B\B\BhB{BbBJBBBBBB{B"�B(�B33BK�BN�BQ�BaHBm�Bm�Bl�Bk�Be`Bs�B|�B~�B�B�7B�JB��B��B�B�?B�^B�qB�XB�-B�B�B�B�B�B�-B�3B�'B�B��B�{B�DB�1B�%B�B{�By�B�B�uB�JBm�B^5BW
BH�B.B��B�B�B�
BĜB��B�Bq�B�B�=B�B�FB�XB�FB�-B��B�1Bt�B[#BF�B9XB49B
��B
�fB
��B
�?B
��B
�=B
[#B
F�B
)�B
�B	�B	�fB	��B
\B
bB
bB
VB	��B	�NB	�ZB	�'B	y�B	� B	� B	�B	x�B	gmB	\)B	T�B	J�B	J�B	K�B	J�B	J�B	B�B	=qB	33B	�B	oB	DB	B��B��B�B�B�BB�5B�)B�B��B��BƨBĜBÖBÖB�XBBÖBŢBŢBÖBÖB�dB�LB�3B�B�'B�!B�3B�B��B��B�uB��B�oB�VB�\B�JB�VB�VB�PB�JB�=B�+B�%B�B�B~�B~�B{�Bt�Bm�BjBffBcTB[#BW
BT�BK�BI�BH�BH�BE�BC�BC�BC�BC�BC�BB�B?}B>wB>wB7LB:^B9XB7LB7LB6FB49B49B2-B33B2-B0!B1'B,B1'B49B1'B0!B1'B33B0!B33B8RB6FB7LB7LB:^B;dBB�BD�BH�BI�BA�B@�B?}B@�BB�BC�BC�BP�BW
B`BBr�B��B�qB�qB�B��B��B�}BǮB��B�B�/B��B	B	B		7B	uB	{B	5?B	8RB	1'B	&�B	=qB	K�B	D�B	L�B	^5B	l�B	t�B	�+B	��B	��B	��B	��B	��B	�B	�!B	�^B	�wB	B	ǮB	��B	��B	�B	�)B	�TB	�ZB	�sB	�B	�B	��B
  B
B
1B
	7B
\B
\B
bB
oB
�B
�B
�B
�B
�B
 �B
$�B
$�B
%�B
&�B
'�B
'�B
)�B
+B
,B
-B
/B
0!B
1'B
1'B
1'B
49B
49B
6FB
9XB
:^B
;dB
<jB
<jB
>wB
>wB
?}B
A�B
C�B
C�B
D�B
E�B
E�B
E�B
G�B
H�B
J�B
J�B
M�B
N�B
O�B
O�B
R�B
R�B
S�B
S�B
W
B
YB
YB
YB
ZB
[#B
]/B
_;B
_;B
`BB
aHB
aHB
bNB
cTB
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
jB
k�B
k�B
m�B
n�B
n�B
p�B
r�B
s�B
t�B
t�B
v�B
w�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�1B
�+B
�7B
�7B
�=B
�DB
�PB
�PB
�PB
�\B
�\B
�bB
�oB
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
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�FB
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�^B
�^B
�^B
�^B
�dB
�^B
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
�dB
�dB
�dB
�jB
�dB
�jB
�jB
�dB
�jB
�dB
�dB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B.BL�B��BPBPB\BoBVB
=BBBBB  BoB �B&�B1'BI�BL�BO�B_;Bk�Bk�BjBiyBcTBq�Bz�B|�B�B�+B�=B��B��B�B�3B�RB�dB�LB�!B�B�B��B��B�B�!B�'B�B��B��B�oB�7B�%B�B� By�Bw�B~�B�hB�=Bk�B\)BT�BF�B,B�B�B�B��BÖB��B� Bo�B�B�1B��B�?B�LB�?B�'B��B�+Bs�BZBE�B8RB33B
��B
�`B
��B
�9B
��B
�7B
ZB
E�B
(�B
�B	�B	�`B	��B
VB
\B
\B
PB	��B	�HB	�TB	�!B	x�B	~�B	~�B	�B	w�B	ffB	[#B	S�B	I�B	I�B	J�B	I�B	I�B	A�B	<jB	2-B	�B	hB	
=B	B��B��B�B�B�;B�/B�#B�
BɺBɺBŢBÖBBB�RB��BBĜBĜBBB�^B�FB�-B�B�!B�B�-B�B��B��B�oB��B�hB�PB�VB�DB�PB�PB�JB�DB�7B�%B�B� B� B}�B}�Bz�Bs�Bl�BiyBe`BbNBZBVBS�BJ�BH�BG�BG�BD�BB�BB�BB�BB�BB�BA�B>wB=qB=qB6FB9XB8RB6FB6FB5?B33B33B1'B2-B1'B/B0!B+B0!B33B0!B/B0!B2-B/B2-B7LB5?B6FB6FB9XB:^BA�BC�BG�BH�B@�B?}B>wB?}BA�BB�BB�BO�BVB_;Bq�B��B�jB�jB�B��B��B�wBƨB��B�B�)B�B	B	B	1B	oB	uB	49B	7LB	0!B	%�B	<jB	J�B	C�B	K�B	]/B	k�B	s�B	�%B	��B	��B	��B	��B	��B	�B	�B	�XB	�qB	��B	ƨB	��B	��B	��B	�#B	�NB	�TB	�mB	�B	�B	��B	��B
B
+B
	7B
\B
\B
bB
oB
�B
�B
�B
�B
�B
 �B
$�B
$�B
%�B
&�B
'�B
'�B
)�B
+B
,B
-B
/B
0!B
1'B
1'B
1'B
49B
49B
6FB
9XB
:^B
;dB
<jB
<jB
>wB
>wB
?}B
A�B
C�B
C�B
D�B
E�B
E�B
E�B
G�B
H�B
J�B
J�B
M�B
N�B
O�B
O�B
R�B
R�B
S�B
S�B
W
B
YB
YB
YB
ZB
[#B
]/B
_;B
_;B
`BB
aHB
aHB
bNB
cTB
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
jB
k�B
k�B
m�B
n�B
n�B
p�B
r�B
s�B
t�B
u�B
w�B
x�B
x�B
x�B
y�B
z�B
{�B
|�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�+B
�+B
�1B
�1B
�7B
�1B
�=B
�=B
�DB
�JB
�VB
�VB
�VB
�bB
�bB
�hB
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
��B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�9B
�?B
�FB
�LB
�RB
�RB
�^B
�^B
�dB
�dB
�dB
�dB
�jB
�qB
�qB
�qB
�qB
�wB
�qB
�wB
�}B
�wB
�wB
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
��B
�}B
��B
��B
�}B
��B
�}B
�}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202372021061413522020210614135220202106141746282021061417462820210614174628201807242202372021061413522020210614135220202106141746282021061417462820210614174628PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023720180724220237  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023720180724220237QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023720180724220237QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015020210722160150IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                