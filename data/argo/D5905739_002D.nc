CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  E   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:40Z creation      
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
resolution        =���   axis      Z        
(  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  E�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
(  H�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
(  U<   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
(  _d   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  i�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
(  l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
(  x�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
(  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
(  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
(  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ̌   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ̨   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ̰   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �,Argo profile    3.1 1.2 19500101000000  20180724220240  20210617131451  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�cQ�Sّ@�cQ�Sّ11  @�cQww�@�cQww�@6���G�@6���G��c������c�����11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @L��@�ff@�33@�33@�  A   AffA!��A@  Aa��A�  A�  A���A���A���A�ffA���A�  A�33B  BffB  B ffB(��B0  B8ffB@ffBH  BP  BX  B`  BhffBp  BxffB�  B�  B�  B�ffB���B�33B���B�  B���B���B�  B�33B�  B�  B�33B�33B�  B�  B���B�  B�ffBԙ�B�  B�  B�33B�33B�33B�33B�  B�33B�33B�  C   C�C  C  C�fC
�C�C  C�C33C�C  C33C33C  C�C 33C"33C$33C&�C(�C*33C,�C.33C0  C1��C4  C6  C8  C:  C<�C>�C?�fCB  CD�CF  CH  CJ  CL�CN�CP33CRL�CT�CV  CX�CZ  C[�fC]�fC_�fCb  Cd�Cf  Ch�Cj33Cl33Cn33Cp�Cr33Ct33Cv�Cx�Cz�C|�C~�C��C��C�  C��C��C��C�  C�  C��C�  C��3C��C��C�  C��C��C��C��C�  C��C��C��C�  C�  C��C��C��3C�  C��C��C��C��C��C��C��3C�  C�  C�  C�  C�  C�  C��C��C�  C��C��C��C��C�  C��C��C��C��C��C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C��C��C��C��C��C��C�  C�  C��C�  C��C��C��C��C�  C�  C�  C��C��C�  C�  C��C�  C��C��C�  C�  C�  C��C��C�  C��C�  C�  C�  C��3C��C�  C�  C��C��C�  C�  C�  C��C��C��C��C��C��C��C�  C��C���C��D FfDٚDS3D  D�3D�D��D	ffD
��D� D3DffD� DfD��D3Dy�D� DS3DfD` D�fD�fDfD�fD�fD ��D"3D#FfD$�fD&fD's3D(ٚD)�fD*�3D,S3D-��D.�fD/� D0�fD2,�D3ffD4��D5��D7ffD8� D:  D;@ D<L�D=�3D>�fD?��DA3DBFfDC�fDD� DF@ DG� DH� DJ�DKFfDLY�DM` DN� DO��DQ@ DR�fDS� >���>���>���>���>���>���>���?   >���>���>���>���>���>���>���?   ?   ?��?��?��?333?L��?fff?fff?�  ?���?�ff?�33?���?�ff@   @��@   @333@@  @S33@`  @y��@�33@�  @���@�33@�  @���@�ff@�33@�  @���@���A33A33A��A��A!��A(  A1��A9��AA��AH  AL��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444144444411414411141111111111111111111111111111111111111                                                                                                                                                                                                                                                                           ?L��?�33@   @l��@�ff@�33@�33@�  A  AffA)��AH  Ai��A�  A�  A���A���A���A�ffA���A�  B��B
  BffB  B"ffB*��B2  B:ffBBffBJ  BR  BZ  Bb  BjffBr  BzffB�  B�  B�  B�ffB���B�33B���B�  B���B���B�  B�33B�  B�  B�33B�33B�  B�  B���B�  B�ffBՙ�B�  B�  B�33B�33B�33B�33B�  B�33B�33B�  C � C��C� C� CffC
��C��C� C��C�3C��C� C�3C�3C� C��C �3C"�3C$�3C&��C(��C*�3C,��C.�3C0� C2L�C4� C6� C8� C:� C<��C>��C@ffCB� CD��CF� CH� CJ� CL��CN��CP�3CR��CT��CV� CX��CZ� C\ffC^ffC`ffCb� Cd��Cf� Ch��Cj�3Cl�3Cn�3Cp��Cr�3Ct�3Cv��Cx��Cz��C|��C~��C�Y�C�L�C�@ C�L�C�L�C�L�C�@ C�@ C�L�C�@ C�33C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�Y�C�L�C�@ C�@ C�L�C�L�C�33C�@ C�L�C�L�C�L�C�L�C�Y�C�L�C�33C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�@ C�L�C�Y�C�Y�C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�@ C�L�C�@ C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�@ C�@ C�@ C�33C�L�C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C��C�L�D ffD��Ds3D  D�3D,�D��D	�fD�D� D33D�fD� D&fD��D33D��D  Ds3D&fD� D�fDfD&fD�fD�fD!�D"33D#ffD$�fD&&fD'�3D(��D)�fD+3D,s3D-��D.�fD0  D1fD2L�D3�fD4��D6�D7�fD8� D:@ D;` D<l�D=�3D>�fD?��DA33DBffDC�fDE  DF` DG� DH� DJ9�DKffDLy�DM� DN� DP�DQ` DR�fDS� G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�?L��?fffG�O�?�  G�O�G�O�?���?���?�ffG�O�?�33?�  ?���?�ff?�33@ff@33@   @,��@@  @S33@`  @s33@�  @���@�33@�  @���@�33@�  @���@�ff@�33@�  @���A��A33A33A��A!��A)��A0  A9��AA��AI��AP  AT��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444144444411414411141111111111111111111111111111111111111                                                                                                                                                                                                                                                                           @ @ %@ �@ �@ �@ "�@ )�@ /�@ 6�@ <�@ D�@ Q�@ `B@ m:@ z�@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ �@ �q@j@�@ @,`@:�@H]@UU@b�@p�@~K@��@��@��@��@@�7@�;@�@��@%@{@!s@/@=q@K�@X�@ff@t�@�d@�\@�@��@�R@�W@խ@�H@��@�E@
�@�@&;@33@A�@O0@\)@i�@x&@�@��@��@��@�k@�c@׹@�@�@  @@�@(�@7L@E�@SI@`�@m�@{�@��@��@�5@�~@��@��@�t@�@��@@�@
@,`@:�@G�@UU@b�@qS@~�@�P@��@��@��@�>@�7@�/@��@�~@�@*@""@0x@>�@Lu@Z@g@uk@�@�@��@�Y@�@ƨ@��@��@��@�E@
�@�@%�@33@A�@N�@[z@j@x&@�@�u@�@��@�k@�c@׹@�@�@  @�@�@)�@5�@DD@R�@`B@m�@{�@��@��@�(@�~@�&@��@�t@�@��@	@	�@	�@	-@	:�@	H]@	V@	b�@	qS@	~�@	��@	�H@	��@	��@	�>@	�7@	��@	�@	�,@
�@
*@
""@
/�@
=q@
K@
Yn@
ff@
t@
�d@
�@
��@
�Y@
�@
ƨ@
Ӡ@
�H@
�@
��@
�@B@&�@3�@@�@N�@\)@j@x&@�@��@�@�@�k@�@�
@�@�Y@ �@V@O@)�@6�@DD@Q�@^�@m�@z�@��@��@��@�~@�&@��@�#@��@�q@@�@g@-@:@H]@R�@�H@��@��@@{@?}@g�@|?@��@є@�@J@0x@UU@x&@��@�!@�\@��@$.@7L@\)@~�@�@��@��@@+�@I�@j@�@��@܀@�@*@<@a�@�p@��@@ލ@^@"�@C�@g�@�@�@�t@�,@�@<@R�@uk@�<@�@ލ@  @%�@N�@i�@��@��@�@�@
=@-�@Q�@t�@��G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ @ �G�O�@ jG�O�G�O�@ @ �@ vG�O�@ %@ �@ �@ �@ 	�@ 
�@ J@ �@ @ @ @ {@ �@ �@ �@ �@ �@  �@ "�@ %�@ (G@ *S@ -@ /�@ 2�@ 5?@ 7�@ ;d@ >@ A�@ D�@ G�@ K�@ O0@ R�@ UU@ WbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AƸRAƺ^A�ĜA�ȴA��
A��;A��HA��#A���A���AƶFA�|�A��yA�Q�A���Aė�A�jA�A�A��A��mAç�A�x�A�&�A��A¬A+A�p�A�^5A�M�A��TA�S�A�v�A�z�A���A�v�A�^5A���A�C�A�+A��;A��A�S�A��hA���A��A���A��+A�
=A�dZA��HA��A���A�-A���A�r�A�G�A�A��A�/A�JA���A�"�A���A���A���A��A�-A��
A�?}A�5?A��A�Q�A��A��hA���A��PA�9XA�VA�$�A�\)A�bNA��A��;A��A���A��A���A��mA��+A��yA�ZA�VA��uA�jA��+A�p�A��A��PA�I�A�n�A�7LA���A�1'A�ȴA�-A��jA�?}A�l�A���A�C�A�p�A��#A�A�=qA��hA��A��A�oA�t�A��A�33A��RA�n�A�bA}|�A{�TAv�DAs�;Ar�yAq;dAn�/Al9XAi��AhjAg�wAf�Ae�7Adn�Aa�TA`(�A\ȴAYdZAW�AV�9AU�^AT��AT�AT�ATȴATĜAT�ATv�AS��ARz�AQ\)AO��ANȴAM�FAK|�AI��AIG�AI%AH��AH�!AF$�AC
=A@1A=|�A<M�A;"�A5l�A1�TA0{A0A/�A/�7A.bNA-��A-�PA-`BA-%A,jA+l�A+
=A*5?A)/A(��A(��A((�A&�uA&ffA&-A%t�A$ȴA$bA#hsA#�A"��A"�uA"�A!t�A �Az�AƨAt�A&�A�AĜA��A�FA`BAVAl�A�+A�FAoAbNAoA$�A�A;dA�uA��A��Ar�A�
A�A
�yA
��A
ȴA
�jA	�A	l�A	�A��A�AA^5A�`A9XAbA��A�A�9AA�FAA ��A ff@�-@��@� �@���@�$�@��h@��@���@�@�E�@�@�C�@�J@��@ѡ�@�r�@Ƈ+@ŉ7@��y@���@��@���@�^5@��@��9@��;@���@���@��H@��7@���@��P@��D@���@��P@�ff@��-@�Z@���@�K�@���@�@�{@��P@��
@�  @���@��@�I�@�(�@�33@�@��T@��j@���@���@��@��@�&�@��u@�33@�V@�@���@���@��@�`B@�G�@���@�Z@�C�@��!@��7@�?}@�&�@���AƼjAƾwA�A���AƶFAƴ9AƶFAƶFAƸRAƸRAƸRAƸRAƶFAƴ9AƸRAƸRAƺ^AƸRAƸRAƺ^AƸRAƺ^AƸRAƸRAƶFAƼjAƺ^A�ƨA�ƨA�ĜAƾwA�ƨA�ĜA�ȴA�ƨA�ƨA���A���A��A��#A��/A��;A��HA��HA��;A��#A��#A��A���A���A���A���A�ȴAư!AƧ�AƧ�Aƙ�AƅA�dZA�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           AƸRAƺ^A�ĜA�ȴA��
A��;A��HA��#A���A���AƶFA�|�A��yA�Q�A���Aė�A�jA�A�A��A��mAç�A�x�A�&�A��A¬A+A�p�A�^5A�M�A��TA�S�A�v�A�z�A���A�v�A�^5A���A�C�A�+A��;A��A�S�A��hA���A��A���A��+A�
=A�dZA��HA��A���A�-A���A�r�A�G�A�A��A�/A�JA���A�"�A���A���A���A��A�-A��
A�?}A�5?A��A�Q�A��A��hA���A��PA�9XA�VA�$�A�\)A�bNA��A��;A��A���A��A���A��mA��+A��yA�ZA�VA��uA�jA��+A�p�A��A��PA�I�A�n�A�7LA���A�1'A�ȴA�-A��jA�?}A�l�A���A�C�A�p�A��#A�A�=qA��hA��A��A�oA�t�A��A�33A��RA�n�A�bA}|�A{�TAv�DAs�;Ar�yAq;dAn�/Al9XAi��AhjAg�wAf�Ae�7Adn�Aa�TA`(�A\ȴAYdZAW�AV�9AU�^AT��AT�AT�ATȴATĜAT�ATv�AS��ARz�AQ\)AO��ANȴAM�FAK|�AI��AIG�AI%AH��AH�!AF$�AC
=A@1A=|�A<M�A;"�A5l�A1�TA0{A0A/�A/�7A.bNA-��A-�PA-`BA-%A,jA+l�A+
=A*5?A)/A(��A(��A((�A&�uA&ffA&-A%t�A$ȴA$bA#hsA#�A"��A"�uA"�A!t�A �Az�AƨAt�A&�A�AĜA��A�FA`BAVAl�A�+A�FAoAbNAoA$�A�A;dA�uA��A��Ar�A�
A�A
�yA
��A
ȴA
�jA	�A	l�A	�A��A�AA^5A�`A9XAbA��A�A�9AA�FAA ��A ff@�-@��@� �@���@�$�@��h@��@���@�@�E�@�@�C�@�J@��@ѡ�@�r�@Ƈ+@ŉ7@��y@���@��@���@�^5@��@��9@��;@���@���@��H@��7@���@��P@��D@���@��P@�ff@��-@�Z@���@�K�@���@�@�{@��P@��
@�  @���@��@�I�@�(�@�33@�@��T@��j@���@���@��@��@�&�@��u@�33@�V@�@���@���@��@�`B@�G�@���@�Z@�C�@��!@��7@�?}@�&�@���AƼjAƾwA�A���AƶFAƴ9AƶFAƶFAƸRAƸRAƸRAƸRAƶFAƴ9AƸRAƸRAƺ^AƸRAƸRAƺ^AƸRAƺ^AƸRAƸRAƶFAƼjAƺ^A�ƨA�ƨA�ĜAƾwA�ƨA�ĜA�ȴA�ƨA�ƨA���A���A��A��#A��/A��;A��HA��HA��;A��#A��#A��A���A���A���A���A�ȴAư!AƧ�AƧ�Aƙ�AƅA�dZA�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�-B
�dB
��B
��B
��B
��B
�B
�5B
�mB
�B
��B
��BB1B\B{B�B�B�B8RBt�B�DB��B��B��B��B�qB��B��B�)B�B��BB�B6FBW
Bu�By�B�{B��B��B�B�'B�9B�9B�9B�LB�RB�XB�XB�dB�qB�qB�dB�qB�^B�FB�9B�B�B��B��B��B��B��B�BJ�B0!B!�BoB$�B1'B0!B1'B1'B1'B,B �B�B�BPB	7B��BBJBPB%B��B�B�yB�)B��B��BÖB�dB��B�\B�B`BBI�B5?B(�B�BB  BB
�B
�HB
ŢB
��B
�+B
|�B
v�B
q�B
YB
G�B
(�B
�B
�B
oB
B	�B	�5B	��B	��B	ŢB	�wB	�FB	��B	�\B	jB	ZB	P�B	H�B	H�B	P�B	Q�B	Q�B	P�B	P�B	P�B	P�B	L�B	E�B	A�B	<jB	49B	.B	"�B	�B	�B	�B	�B	�B		7B��B�yB�NB�/B�B��B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�hB�bB�JB�7B�1B�+B�B�B�B� B� B~�B|�B{�B|�Bq�Bp�Bo�Bn�Bo�Bp�Bs�Bv�B{�Bz�Bu�Bu�Bv�Bv�Bv�Bs�Bu�Bv�Bw�Bv�Bt�Bv�Bv�Bw�Bw�Bu�Bt�Bt�Bt�Bs�Bt�Bs�Bs�Bs�Br�Bq�Bn�Bo�Bp�Bo�Br�Br�Bs�Br�Br�Br�Br�Br�Bm�BffB^5BiyBjBffBL�BP�Bm�B\)BE�B;dB>wB;dBI�BQ�BP�BVBYBXB_;B_;BjBs�Bw�B{�B|�B�7B�DB�JB��B��B�9B�^B��B��B��B�B�B�fB�B�B�B�B	+B	1B	 �B	+B	-B	2-B	9XB	A�B	L�B	R�B	W
B	YB	cTB	e`B	k�B	u�B	� B	�B	�B	�+B	�+B	�7B	�=B	�VB	�oB	��B	��B	�B	�B	�B	�!B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
��B
��B
�B
�9B
�FB
�FB
�LB
�^B
��B
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           B
��B
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
�?B
̨B
��B
��B
��B
��B
�B
�LB
�pB
��B
��B �BB>B^BpB~B�B87Bt�B�*B��B��B��B��B�YB̶B��B�B�|B��B�B�B63BV�Bu�By�B�jB��B��B��B�B�+B�+B�,B�?B�FB�MB�MB�ZB�gB�hB�[B�iB�VB�?B�2B�B�B��B��B��B��B��B�	BJ�B0B!�BnB$�B1'B0"B1(B1)B1*B,B �B�B�BUB	=B� BBQBXB-B��B�B�B�3B��B��BâB�qB��B�iB�B`PBI�B5NB)B�B/B BB
�B
�ZB
ŴB
��B
�>B
}B
v�B
q�B
Y,B
G�B
)B
�B
�B
�B
B	�B	�MB	�
B	��B	ŻB	��B	�`B	��B	�wB	j�B	Z8B	QB	H�B	H�B	QB	R
B	RB	QB	QB	QB	QB	L�B	E�B	A�B	<�B	4\B	.8B	"�B	�B	�B	�B	�B	�B		^B��B�B�vB�XB�@B�B�JB�&B� B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�jB�eB�_B�TB�AB�<B�7B�7B2B}&B| B}'Bq�Bp�Bo�Bn�Bo�Bp�Bs�BwB|%B{BvBvBw	Bw	Bw
Bs�BvBwBxBwBu BwBwBxBxBv	BuBuBuBs�BuBt Bt BtBr�Bq�Bn�Bo�Bp�Bo�Br�Br�BtBs BsBsBsBsBm�Bf�B^�Bi�Bj�Bf�BM$BQ=Bm�B\�BE�B;�B>�B;�BJBROBQIBViBY}BXxB_�B_�Bj�Bt#Bx>B|WB}_B��B��B��B��B�DB��B��B� B�EB�eBօBڠB��B�B�7B�B�.B	�B	�B	!SB	+�B	-�B	2�B	9�B	BB	MdB	S�B	W�B	Y�B	c�B	e�B	l%B	veB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�JB	��B	��B	��B	��B	��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
� B
�'B
�9B
�^B
�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202402021061413550920210614135509202106171311242021061713112420210617131124201807242202402021061413550920210614135509202106171311242021061713112420210617131124PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024020180724220240  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024020180724220240QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024020180724220240QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145120210617131451IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                