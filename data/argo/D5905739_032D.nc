CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:47Z creation      
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
_FillValue                   	<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � |Argo profile    3.1 1.2 19500101000000  20180917231347  20210617131503  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                    DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�}�@�}�11  @�}��� `@�}��� `@6ҕBC@6ҕBC�c���{ 2�c���{ 211  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @@  @�33@�  @�  @�33A   AffA!��A@  A`  A�  A���A���A���A���Aљ�A���A���B ffBffBffB��B   B(ffB0��B8  B@ffBH  BO��BX  B`��Bi33Bq33By33B�33B���B���B�33B���B�33B�  B�33B�33B�ffB�ffB�ffB�33B���B�  B�33B�ffB�33B�  B�33B���B�33B�33B�  B�ffB�  B癚B�  B�33B���B�33B���B���C��C�fC��C��C	�fC��C��C��C�3C�3C�fC�fC  C  C�C 33C"L�C$�C%�fC(�C*  C+��C.  C0L�C233C4�C6  C7��C:  C<L�C>33C@�CBL�CDL�CF�CH  CI��CL33CN  CP  CR33CT�CV�CX  CY�fC\�C^�C_�fCb�Cd33CfffCh�Ci��Cl  Cn�Cp�CrL�Ct�Cu��Cw�fCz�C|�C~�C��C��C��C�&fC�&fC�&fC��3C�ٚC��fC��3C�  C��C�&fC�  C��fC��3C�  C��C�  C��3C��3C��C��C�&fC�  C��fC��3C�  C�&fC��C��3C��C��C�  C��C��C��3C��C��C��C��3C��3C��C��C�  C�&fC��C��C��3C��3C��C��C��3C�  C��C��fC��C��C��3C��C��C��C�  C��3C��C��C�  C�&fC��C�  C��C��C��3C�  C�&fC��C��fC�  C��C�&fC��C��fC�  C��C��C�&fC��C��3C�  C��C��C�&fC��C��fC��3C�  C��C��C�&fC��C��fC�  C��C�&fC��C��3C�  C��C��C�&fC�  C�ٚC��fC��fC��fC��3C��3C��3C�  C�  C�  C�  C��fC��C��C��C��C�&fD 3D ��DfD�fDfD��D	S3D�3DFfD��D�D` D�3DfDL�D� D ��D#L�D%��D(fD*y�D,�3D/s3D1��D4�3D7@ D9�3D<� D?S3DB�DD� DG` DJ  DL��DO33DQ��DTL�DV�3DYFfD[��D^33D`�fDc3De��Dg��Dj` Dl� Do  Dq��Ds�fDv@ Dx��D{fD}  DL�D���D��3D�fD�@ D�ffD���D��fD�ɚD��fD�  D�I�D�� D��fD��fD��D�P D��fD���D��D�&fD�\�D��3D��3D��fD�&fD�P D�� D��fD�ٚD��D�6fD�` D��fD�� D��fD��3D��D�33D�I�D�Y�D�c3D�i�D�y�D���D��3D��3D�ɚD�� D�  D��D�6fD�S3D�p D��3D��fD�ٚD���D�&fD�S3DĀ Dũ�D���D�	�D�@ D�s3D˩�D�� D��D�P DЃ3DѬ�D��fD�3D�)�D�VfD�y�D؜�Dٹ�D�ٚD�  D�fD�6fD�FfD�Y�D�i�D�y�D�fD�3D�3D� D繚D��D�� D��fD��fD�ɚD�� D���D��fD��D�fD� D�fD���D���D�|�D�p D�` D�P D�FfD�&fD��D�3D�fD���D��E p E �3Ed�E� E\�E��EњEH E��E��E	,�E
&fE�3E��E3El�EVfE��E  EQ�E�fE�3E��E;3E�fE�3EɚE3Eq�E �3E"( E#�E$� E%��E&� E(p E)i�E*�fE+�3E-VfE.P E/�fE0� E2P E3NfE4� E60 E7!�E8�fE9� E;$�E<k3E?Y�EB� EE�fEH� EK�fEN� ER( EU�EXH E[��E^vfEa�3Ed�fEg��Ek#3En1�Eq�Et^fEw~fEz� E}��E�a�E��3E��3E�!�E�� E�� E�L�>���?   ?   >���>���>���?   >���?   ?   >���>���>���>���>���?   >���>���?   >���?   >���?   ?��?��?333?333?fff?�  ?�ff?�ff?���?�ff@��@   @,��@@  @Y��@s33@�33@�  @���@�ff@�33@�  @���@���@�ff@�ffA33A33A��A33A   A+33A333A<��AD��AL��AVffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441414444441441414114141114111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?fff?�33@   @`  @�33@�  @�  @�33A  AffA)��AH  Ah  A�  A���A���A���Ař�Aՙ�A���A���BffB
ffBffB��B"  B*ffB2��B:  BBffBJ  BQ��BZ  Bb��Bk33Bs33B{33B�33B���B���B�33B���B�33B�  B�33B�33B�ffB�ffB�ffB�33B���B�  B�33B�ffB�33B�  B�33B���B�33B�33B�  B�ffB�  B虚B�  B�33B���B�33B���C ffCL�CffCL�CL�C
ffCL�CL�CL�C33C33CffCffC� C� C��C �3C"��C$��C&ffC(��C*� C,L�C.� C0��C2�3C4��C6� C8L�C:� C<��C>�3C@��CB��CD��CF��CH� CJL�CL�3CN� CP� CR�3CT��CV��CX� CZffC\��C^��C`ffCb��Cd�3Cf�fCh��CjL�Cl� Cn��Cp��Cr��Ct��CvL�CxffCz��C|��C~��C�L�C�L�C�L�C�ffC�ffC�ffC�33C��C�&fC�33C�@ C�Y�C�ffC�@ C�&fC�33C�@ C�Y�C�@ C�33C�33C�L�C�Y�C�ffC�@ C�&fC�33C�@ C�ffC�L�C�33C�Y�C�L�C�@ C�Y�C�Y�C�33C�Y�C�Y�C�L�C�33C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�33C�33C�L�C�L�C�33C�@ C�L�C�&fC�L�C�L�C�33C�Y�C�Y�C�L�C�@ C�33C�L�C�L�C�@ C�ffC�L�C�@ C�Y�C�L�C�33C�@ C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�@ C��C�&fC�&fC�&fC�33C�33C�33C�@ C�@ C�@ C�@ C�&fC�Y�C�Y�C�Y�C�Y�C�ffD 33D ��D&fD�fD&fD��D	s3D�3DffD��D,�D� D�3D&fDl�D� D!�D#l�D%��D(&fD*��D-3D/�3D2�D4�3D7` D:3D<� D?s3DB,�DD� DG� DJ  DL��DOS3DQٚDTl�DV�3DYffD[��D^S3D`�fDc33De��Dh�Dj� Dl� Do@ Dq��DtfDv` Dx��D{&fD}  Dl�D���D�3D�&fD�P D�vfD���D��fD�ٚD�fD�0 D�Y�D�� D��fD��fD�)�D�` D��fD�ɚD���D�6fD�l�D��3D��3D�fD�6fD�` D�� D��fD��D��D�FfD�p D��fD�� D��fD�3D�)�D�C3D�Y�D�i�D�s3D�y�D���D���D��3D��3D�ٚD�� D� D�,�D�FfD�c3D�� D��3D��fD��D��D�6fD�c3DĐ DŹ�D���D��D�P Dʃ3D˹�D�� D�,�D�` DГ3DѼ�D��fD�3D�9�D�ffD׉�Dج�D�ɚD��D� D�&fD�FfD�VfD�i�D�y�D≚D�fD�3D�3D�� D�ɚD���D�� D��fD��fD�ٚD�� D���D��fD���D��fD�� D�fD���D���D���D�� D�p D�` D�VfD�6fD�)�D�#3D�fD�	�D���E x E �3El�E� Ed�E��EٚEP E��E��E	4�E
.fE�3E��E3Et�E^fE��E EY�E�fE�3E��EC3E�fE�3EњE#3Ey�E �3E"0 E#!�E$� E&�E'  E(x E)q�E*�fE+�3E-^fE.X E/�fE0� E2X E3VfE4� E68 E7)�E8�fE9� E;,�E<s3E?a�EB� EE�fEH� EK�fEN� ER0 EU�EXP E[��E^~fEa�3Ed�fEh�Ek+3En9�Eq!�EtffEw�fEz� E}��E�e�E�3E��3E�%�E�� E�� E�P�?fffG�O�G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�?L��G�O�?fffG�O�?fff?�  G�O�?���G�O�?���?�33?�  G�O�?�ff@ff@33@,��@@  @L��@`  @y��@���@�33@�  @���@�ff@�33@�  @���@���@�ffA33A33A33A��A#33A(  A333A;33AD��AL��AT��A^ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441414444441441414114141114111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ �@ %@ �@ {@ �@ ""@ (�@ 0x@ 6�@ <�@ D�@ Q�@ _�@ m:@ |?@ ��@ ��@ �5@ ��@ ��@ �|@ �#@ ��@ �q@�@@g@-�@:@H]@UU@bN@p�@�@��@��@�M@��@��@ψ@ލ@�@��@�@*@"�@1'@>�@Lu@Yn@e�@t@�d@��@��@��@�@�J@�O@��@��@��@
=@�@%�@3�@B�@O0@Z�@i!@v@�p@�h@�@�f@�^@�@խ@�@�L@�Q@�@O@(�@7L@E�@S�@`B@l�@{�@��@��@��@��@��@�|@�t@�@��@v@o@g@.l@<@H]@UU@a�@r@~K@��@��@��@��@@ψ@ލ@�4@�~@�@�@$�@0x@<@K@Yn@g@v@�d@��@�U@�Y@�@ƨ@�O@��@�@��@J@�@$�@1'@?}@M�@\)@k.@y�@�@�h@��@�@�@�c@�[@�@�@^@�@O@'�@5�@DD@S�@`B@l�@|?@�7@�0@�5@��@�w@�*@��@��@�@	�@	o@	g@	,`@	<@	I@	V@	bN@	o�@	~�@	��@	��@	�A@	��@	�2@	��@	ލ@	��@	��@
1@
*@
""@
/@
>@
K�@
X�@
hs@
t�@
��@
��@
��@
��@
�R@
�@
�O@
��@
��@
�E@J@�@$.@33@A�@O�@^5@j@v�@�@�u@��@�!@�k@�@�[@�@�@^@�@�@'�@6�@D�@S�@`B@l�@z�@�7@��@��@�~@�@�o@�@�@�@�@b@�@,`@:@G�@S�@dZ@r@�@�P@��@�M@�F@�>@��@ލ@`�@��@�(@-@n�@�r@��@.l@m�@�@�@+�@k.@�@�4@/@r�@��@��@B�@��@խ@�@hs@��@��@DD@��@��@�@^�@�5@�(@-@r@��@�q@8�@|?@�w@  @@�@��@��@@DD@�|@ƨ@��@;d@z3@�@� @6�@uk@�9@�L@.l@n�@�@�@/�@r@��@�e@6�@x�@�^@�9@ >�@ �@ �>@!@!E�@!�|@!��@"�@"E�@"�+@"ȴ@#1@#G�@#�|@#��@$�@$A�@$�W@$�j@$��@%1�@%j@%�z@%܀@&�@&R�@&��@&�@'j@'@�@'}�@'��@'�q@(33@(qS@(�r@(�@)+�@)k.@)�Y@)�@*+@*l�@*��@*��@+0x@+r�@+��@+�~@,:@,{�@,�@,��@-:�@-y�@-��@-��@.5�@.r�@.�!@.��@/*S@/g�@/��@/܀@0�@0P�@0��@0�>@0�E@16�@1oF@1��@1��@2�@2Lu@2��@2��@2�@3&�@3[z@3��@3��@3��@4.l@4bN@4��@4�c@4��@5/�@5dZ@5�#@5�@5�E@61'@6e	@6�<@6��@7]@75@@7i�@7�@7�C@8> @8��@9|�@9�@:�+@:�@;��@;�E@<��@=1�@=��@>'�@>�R@?H]@?��@@;d@@��@A`�@A�@B�|@B�`@Cuk@D�@D��@E/�@E��@F33@F�C@G=q@G��@HH]@H�l@ISI@I�@J_�@K�@Kp�@L{@L�@M�@M��@N"�@N�@OK@O�@PdZ@Q��@S  @T[z@U�~@V�Q@X@�@Y��@Z�(@\G�@]��@^��@`Lu@a�(@b�9@dSI@e�@fލ@hC�@i��@j�,@l1�@m�P@n�@@p<�@q��@r�H@s,`@st�@ �G�O�G�O�G�O�G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�@ G�O�@ �G�O�@ �@ jG�O�@ G�O�@ �@ %@ �G�O�@ �@ 
�@ J@ @ @ o@ {@ 6@ �@ �@ �@  �@ #�@ &;@ (�@ +�@ /@ 1'@ 4�@ 7�@ ;d@ >@ B8@ DD@ I@ Lu@ P�@ S�@ Wb@ [zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�`BA�ffA�jA�n�A�p�A�n�A�p�A�r�A�r�A�t�A�t�A�t�A�z�AυAχ+Aχ+Aχ+Aω7Aω7AϋDAύPAω7AσA�|�A�t�A�~�A�r�A��A�ZA�1A�M�A¼jA��;A�r�A��A��A�33A���A���A�S�A���A��FA�M�A�O�A���A��mA�K�A�JA���A��+A��hA���A��A��^A�A�A�\)A��uA���A���A�VA�1A�G�A�hsA�;dA��7A��A�A��A�|�A��A�"�A�v�A��A�  A�ffA�&�A���A�%A�ZA��hA�^5A��hA��A��TA��A���A���A��A��^A��!A�^5A�x�A�  A��`A�  A���A�5?A�5?A��A�33A��\A�{A�S�A�ȴA�;dA���A�S�A�$�A�~�A��-A�K�A�;dA{x�Ay�FAw`BAuAtjAr�DApn�Am\)Ah�Af��Ae�Ae
=Ad�HAd�+Ac\)Abz�A`�9A_33A\VA[%AY�7AX�9AW��AW?}AVv�AU��AUS�AT��AQ��AOK�AN1AMhsAL�HAL=qAKt�AJ~�AH��AG+AFAEK�AE
=AD��ADr�AC�AA�7A@=qA@�A?�hA>bA=ƨA=/A<VA:�A:r�A9\)A8�/A8bNA8  A6��A5�;A5K�A3�^A29XA1��A0�RA/��A.�`A-�;A-&�A,�DA+A+?}A*�A*�RA*��A* �A(�yA'�wA'l�A'�A&bA$A�A#oA"~�A!�hA!;dA �/A33A��AffA�-A��AdZA$�AA5?A�-A&�A��A�^A��A\)A�^AI�A�
A��A�AZA�^AhsA�A�+AVA$�A\)A
�\A	�7A��A�;AG�AA�A�AVA�A�A�AdZA?}A�A ��@�33@��T@���@�M�@�@���@�j@��@��#@���@�@�V@�K�@�j@�l�@�E�@�7L@�Q�@��;@���@�l�@�o@�M�@Ӆ@�-@��T@��T@�33@�(�@�5?@�ff@���@�ƨ@�S�@���@�5?@�|�@��@��@�p�@��@���@�|�@� �@���@��\@��m@��@�-@�$�@�{@��j@��@���@�l�@�o@�@�Q�@��@�p�@�z�@�ƨ@�l�@��7@�r�@�;d@���@���@��9@���@�ff@��-@�/@��j@�r�@|�@~v�@|z�@z�@y�#@x�`@v��@u/@r�!@p��@o�P@m?}@kC�@iX@f�y@e��@d��@c��@b~�@`��@_|�@]��@]`B@[��@Z^5@X��@W��@V$�@Up�@S�@R�\@R�@P��@P �@N��@L�j@K"�@JJ@H��@G��@E?}@CC�@B^5@A%@@�u@?|�@?
=@>$�@<�D@<�@:�@9G�@8Q�@6E�@4��@49X@3�@3�@2-@01'@.�@.E�@-O�@,��@+S�@*��@)hs@(�`@'�;@&v�@%�@$�/@$(�@#��@#S�@"M�@!�7@!X@ 1'@�w@�@@O�@C�@��@hs@%@Q�@  @l�@��@�R@�+@ff@$�@��@�j@��@��@��@=q@�@�@�;@\)@{@/@��@��@�@
�@
�\@	�@	��@	7L@��@Q�@b@;d@�+@�@C�@�^@ A�?���?���?�
=?���?�33?���?�|�?�h?�?ꟾ?�r�?�P?�33?�  ?��?�5??��?ۥ�?��H?ٙ�?׮?�+?�$�?ա�?ӕ�?ҏ\?Ѓ?�A�?�|�?�|�?�V?���?�O�?��?˥�?���?���?��?ɺ^?�X?ȓu?�l�?��T?��
?�t�?�-?��?��`?��w?�;d?�5??�p�?��D?��?���?�^5?���?�x�?���?�7L?�x�?���?�~�?�?��?�I�?��?�/?�/A�Q�A�XA�S�A�Q�A�O�A�VA�\)A�O�A�Q�A�n�A�hsA�l�A�hsA�^5A�dZA�dZA�^5A�bNA�\)A�ZA�\)A�^5A�^5A�bNA�ZA�XA�ffA�jA�`BA�^5A�ZA�bNA�dZA�dZA�l�A�jA�jA�l�A�n�A�n�A�n�A�p�A�p�A�n�A�p�A�n�A�p�A�n�A�p�A�r�A�r�A�r�A�t�A�r�A�x�A�v�A�t�A�r�A�r�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A�\)A�`BA�ffA�jA�n�A�p�A�n�A�p�A�r�A�r�A�t�A�t�A�t�A�z�AυAχ+Aχ+Aχ+Aω7Aω7AϋDAύPAω7AσA�|�A�t�A�~�A�r�A��A�ZA�1A�M�A¼jA��;A�r�A��A��A�33A���A���A�S�A���A��FA�M�A�O�A���A��mA�K�A�JA���A��+A��hA���A��A��^A�A�A�\)A��uA���A���A�VA�1A�G�A�hsA�;dA��7A��A�A��A�|�A��A�"�A�v�A��A�  A�ffA�&�A���A�%A�ZA��hA�^5A��hA��A��TA��A���A���A��A��^A��!A�^5A�x�A�  A��`A�  A���A�5?A�5?A��A�33A��\A�{A�S�A�ȴA�;dA���A�S�A�$�A�~�A��-A�K�A�;dA{x�Ay�FAw`BAuAtjAr�DApn�Am\)Ah�Af��Ae�Ae
=Ad�HAd�+Ac\)Abz�A`�9A_33A\VA[%AY�7AX�9AW��AW?}AVv�AU��AUS�AT��AQ��AOK�AN1AMhsAL�HAL=qAKt�AJ~�AH��AG+AFAEK�AE
=AD��ADr�AC�AA�7A@=qA@�A?�hA>bA=ƨA=/A<VA:�A:r�A9\)A8�/A8bNA8  A6��A5�;A5K�A3�^A29XA1��A0�RA/��A.�`A-�;A-&�A,�DA+A+?}A*�A*�RA*��A* �A(�yA'�wA'l�A'�A&bA$A�A#oA"~�A!�hA!;dA �/A33A��AffA�-A��AdZA$�AA5?A�-A&�A��A�^A��A\)A�^AI�A�
A��A�AZA�^AhsA�A�+AVA$�A\)A
�\A	�7A��A�;AG�AA�A�AVA�A�A�AdZA?}A�A ��@�33@��T@���@�M�@�@���@�j@��@��#@���@�@�V@�K�@�j@�l�@�E�@�7L@�Q�@��;@���@�l�@�o@�M�@Ӆ@�-@��T@��T@�33@�(�@�5?@�ff@���@�ƨ@�S�@���@�5?@�|�@��@��@�p�@��@���@�|�@� �@���@��\@��m@��@�-@�$�@�{@��j@��@���@�l�@�o@�@�Q�@��@�p�@�z�@�ƨ@�l�@��7@�r�@�;d@���@���@��9@���@�ff@��-@�/@��j@�r�@|�@~v�@|z�@z�@y�#@x�`@v��@u/@r�!@p��@o�P@m?}@kC�@iX@f�y@e��@d��@c��@b~�@`��@_|�@]��@]`B@[��@Z^5@X��@W��@V$�@Up�@S�@R�\@R�@P��@P �@N��@L�j@K"�@JJ@H��@G��@E?}@CC�@B^5@A%@@�u@?|�@?
=@>$�@<�D@<�@:�@9G�@8Q�@6E�@4��@49X@3�@3�@2-@01'@.�@.E�@-O�@,��@+S�@*��@)hs@(�`@'�;@&v�@%�@$�/@$(�@#��@#S�@"M�@!�7@!X@ 1'@�w@�@@O�@C�@��@hs@%@Q�@  @l�@��@�R@�+@ff@$�@��@�j@��@��@��@=q@�@�@�;@\)@{@/@��@��@�@
�@
�\@	�@	��@	7L@��@Q�@b@;d@�+@�@C�@�^@ A�?���?���?�
=?���?�33?���?�|�?�h?�?ꟾ?�r�?�P?�33?�  ?��?�5??��?ۥ�?��H?ٙ�?׮?�+?�$�?ա�?ӕ�?ҏ\?Ѓ?�A�?�|�?�|�?�V?���?�O�?��?˥�?���?���?��?ɺ^?�X?ȓu?�l�?��T?��
?�t�?�-?��?��`?��w?�;d?�5??�p�?��D?��?���?�^5?���?�x�?���?�7L?�x�?���?�~�?�?��?�I�?��?�/?�/A�Q�A�XA�S�A�Q�A�O�A�VA�\)A�O�A�Q�A�n�A�hsA�l�A�hsA�^5A�dZA�dZA�^5A�bNA�\)A�ZA�\)A�^5A�^5A�bNA�ZA�XA�ffA�jA�`BA�^5A�ZA�bNA�dZA�dZA�l�A�jA�jA�l�A�n�A�n�A�n�A�p�A�p�A�n�A�p�A�n�A�p�A�n�A�p�A�r�A�r�A�r�A�t�A�r�A�x�A�v�A�t�A�r�A�r�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�hB�bB�hB�bB�bB�bB�bB�bB�hB�bB�bB�bB�bB�bB�hB�bB�bB�bB�bB�bB�hB�bB�bB�\B�VB�\B�\B�VB�%Bp�B�B��B�!B�RBƨBBŢBŢBǮB��B��B�#B�`B�ZB�B�B�#B��B��B�wB�RB�-B�3B�!B�B��B��B��B��B��B��B�uB�7B�1B}�B|�B|�By�BcTBW
BS�BM�BD�BG�B49B0!B-BhB��B�B�NB��BǮB�RB�!B�B�B��B��B�{B�1B~�Bq�BiyBVBI�BD�B:^B+B�B\BB
��B
�B
�yB
�HB
��B
ÖB
�wB
�3B
��B
��B
�1B
dZB
YB
A�B
2-B
!�B
oB	��B	�)B	�-B	�B	��B	�-B	�'B	�B	��B	��B	��B	��B	��B	��B	�PB	�JB	�1B	�B	~�B	{�B	y�B	q�B	aHB	XB	R�B	O�B	M�B	I�B	I�B	E�B	=qB	8RB	2-B	0!B	.B	,B	)�B	%�B	�B	�B	�B	hB	
=B	1B	B��B��B�B�sB�ZB�BB�)B��B��B��BÖB�}B�dB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�bB�+B�B|�Bz�By�B{�Bs�Bu�Bs�Bp�Bl�Bo�Bm�Bl�Bn�Bl�Bm�Bm�Bl�BhsBffBdZBcTB`BB^5B[#B\)B]/B\)B[#B[#BZBXBVBT�BR�BS�BQ�BQ�BO�BL�BL�BJ�BI�BI�BF�BF�BE�BD�BB�B@�B@�B=qB8RB5?B33B2-B2-B33B33B49B2-B1'B1'B2-B2-B33B33B6FB6FB8RB9XB?}BC�BG�BVBbNBz�B�%B�BĜB��B�
B�BB�fB��B��B	PB	�B	&�B	:^B	G�B	W
B	YB	T�B	\)B	YB	dZB	w�B	�B	�1B	�bB	��B	��B	�'B	�?B	�jB	ĜB	��B	��B	�B	�B	�)B	�TB	�mB	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
1B
	7B
JB
PB
bB
oB
uB
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
'�B
&�B
(�B
+B
-B
-B
/B
/B
0!B
1'B
1'B
2-B
2-B
2-B
5?B
6FB
8RB
9XB
:^B
<jB
<jB
>wB
?}B
>wB
@�B
@�B
B�B
C�B
C�B
D�B
G�B
H�B
I�B
K�B
K�B
L�B
L�B
N�B
O�B
P�B
Q�B
R�B
R�B
T�B
T�B
VB
W
B
XB
ZB
ZB
[#B
[#B
\)B
\)B
^5B
^5B
^5B
_;B
`BB
`BB
aHB
bNB
dZB
dZB
e`B
ffB
ffB
ffB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
l�B
l�B
l�B
m�B
n�B
o�B
o�B
o�B
q�B
r�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
w�B
x�B
{�B
~�B
� B
�B
�%B
�+B
�1B
�1B
�7B
�DB
�DB
�PB
�PB
�\B
�\B
�hB
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
�B
�B
�B
�B
�B
�B
�!B
�-B
�-B
�9B
�3B
�FB
�FB
�LB
�LB
�RB
�RB
�RB
�XB
�RB
�RB
�XB
�XB
�XB
�RB
�^B�hB�hB�hB�hB�oB�uB�\B�hB�hB�VB�oB�VB�bB�{B�bB�bB�\B�\B�hB�oB�bB�hB�oB�bB�bB�hB�hB�\B�bB�bB�bB�bB�hB�hB�bB�bB�bB�bB�bB�bB�bB�bB�bB�hB�bB�bB�bB�bB�hB�bB�bB�bB�bB�hB�bB�bB�bB�bB�bB�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    B�@B�:B�@B�;B�;B�;B�<B�<B�BB�<B�=B�=B�>B�?B�EB�@B�@B�AB�BB�BB�IB�DB�DB�?B�9B�@B�AB�;B�Bp�B��B��B�B�9BƐB�wBŋBŋBǘBͽB��B�B�LB�FB�
B�B�B��B̼B�fB�AB�B�#B�B��B��B��B��B��B��B�vB�jB�-B�'B}�B|�B|�By�BcLBWBS�BM�BD�BG�B44B0B-
BeB��B�B�LB��BǭB�RB�!B�B�B��B��B�~B�4B~�Bq�Bi~BV	BI�BD�B:dB+	B�BdB'B
��B
�B
�B
�RB
��B
áB
��B
�?B
��B
��B
�?B
dhB
Y%B
A�B
2<B
!�B
B	��B	�:B	�>B	�B	�B	�?B	�:B	�(B	�B	�B	�B	��B	��B	��B	�gB	�aB	�IB	�*B	B	| B	y�B	q�B	acB	X+B	SB	O�B	M�B	I�B	I�B	E�B	=�B	8qB	2LB	0AB	.5B	,)B	*B	&B	�B	�B	�B	�B	
bB	VB	8B�B��B��B�B�B�kB�RB�B�B��B��B��B��B�lB�ZB�BB�6B�1B�B�B��B��B�B�B�B��B��B��B��B��B�aB�<B}$B{BzB|Bs�Bu�Bs�Bp�Bl�Bo�Bm�Bl�Bn�Bl�Bm�Bm�Bl�Bh�Bf�Bd�Bc�B`�B^wB[eB\kB]rB\mB[gB[hBZbBXVBVJBUEBS9BT@BR4BR5BP(BMBMBKBJBJBF�BF�BE�BD�BB�B@�B@�B=�B8�B5�B3�B2B2B3�B3�B4�B2�B1|B1|B2�B2�B3�B3�B6�B6�B8�B9�B?�BC�BHBVkBb�B{MB��B�sB�B�VBׄB�B��B�WB�~B	�B	<B	'vB	:�B	HAB	W�B	Y�B	U�B	\�B	Y�B	e B	xxB	��B	��B	�B	�BB	�wB	��B	��B	�,B	�`B	̎B	��B	��B	��B	��B	�*B	�EB	�`B	�iB	�B	��B	��B	��B	��B	��B
 �B
�B
	B
B
!B
*B
	2B

;B
QB
ZB
oB
~B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
%B
&B
)"B
(B
*-B
,<B
.KB
.NB
0^B
0`B
1iB
2rB
2uB
3}B
3�B
3�B
6�B
7�B
9�B
:�B
;�B
=�B
=�B
?�B
@�B
?�B
A�B
A�B
DB
EB
EB
FB
I0B
J9B
KAB
MQB
MTB
N]B
N`B
PnB
QwB
R�B
S�B
T�B
T�B
V�B
V�B
W�B
X�B
Y�B
[�B
[�B
\�B
\�B
]�B
]�B
_�B
_�B
` B
aB
bB
bB
cB
d&B
f4B
f7B
g?B
hHB
hJB
hMB
j\B
j_B
jaB
jdB
klB
knB
lwB
lyB
n�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
s�B
t�B
u�B
t�B
u�B
v�B
v�B
w�B
w�B
x�B
w�B
x�B
x�B
x�B
z B
{B
~&B
�>B
�KB
�\B
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
�B
�B
�!B
�+B
�2B
�>B
�KB
�QB
�cB
�jB
�wB
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
�B
�B
�B
�B
�B
�-B
�CB
�XB
�zB
��B
��B
��B
��B
��B
��B
�	B
�B
�4B
�=B
�_B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�(B
�%B
�5B�@B�@B�@B�@B�GB�MB�4B�@B�@B�.B�GB�.B�:B�SB�:B�:B�4B�4B�@B�GB�:B�@B�GB�:B�:B�@B�@B�4B�:B�:B�:B�:B�@B�@B�:B�:B�;B�;B�;B�;B�;B�;B�;B�AB�<B�<B�<B�<B�BB�<B�<B�<B�=B�CB�=B�=B�=B�=B�>B�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313472021061413553820210614135538202106171312522021061713125220210617131252201809172313472021061413553820210614135538202106171312522021061713125220210617131252PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134720180917231347  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134720180917231347QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134720180917231347QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150320210617131503IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                