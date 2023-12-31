CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-11T11:00:40Z creation      
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
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181211110040  20210722160156  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               0   0DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؔ��4բ@ؔ��4բ11  @ؔ�}'�@ؔ�}'�@5��n/@5��n/�c��B��9�c��B��911  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?fff@ff@@  @�  @�  @�  @�  @���A  A&ffA@  Aa��A���A�ffA�  A�  A�  A���A���A���A�33B��B  B��B   B(ffB0ffB8��B@ffBHffBPffBX��B`��Bh  Bp  BxffB�33B�  B�33B�  B���B�  B�33B�  B���B�33B�33B�ffB�33B�  B�  B�ffB�33B�  BǙ�B���B�33B�ffBؙ�B���B�  B�  B�33B�33B�  B���B���B�ffC �C  C�fC��C  C
  C�fC�C�C��C  C33C33C  C33C�C��C!�fC$�C&33C'�fC)��C+�fC.�C0  C1��C4�C633C8  C9��C;�fC>�C@  CA��CD�CF  CG��CJ  CL33CN�CO��CR�CTL�CV�CX  CZ33C\�C]�fC`33Cb�Cc�fCf�Ch�Ci��Cl�CnL�Cp33Cr�Ct33Cv  Cx33CzL�C|L�C~  C��C��C�  C��C�  C��3C��C�&fC��C��3C�  C��C�  C��fC�  C��C��C�&fC��C��3C��3C�  C��C��C�&fC��C��fC�  C�  C�  C��C��C�&fC��C��fC��3C�  C��C�&fC��C��3C��C��C��C��3C�  C�&fC�  C��3C��C��C�  C��fC��C�  C��fC�  C�&fC��C�  C��C�  C��fC��C��C��C��3C��C�  C��fC��C�&fC��C�  C��C�&fC��C�  C��C�&fC�&fC�  C��C��C�  C��fC��fC��3C��C�&fC��C�33C��C��fC��fC��3C�  C��C�  C��C��C�&fC�&fC��C��fC��fC��3C��3C��3C��C�33C��C�&fC�&fC��C��C�&fC�&fC��C��C�&fC�&fC�  C�ٚC��fC��3C�  C�  D �D ��D�D��D�3D��D	ffD��Dl�D  D��D&fD�fD,�D��D 3D"��D%  D's3D)�fD,` D.� D1S3D3��D6  D8��D:��D=S3D?��DB3DDffDF�fDI  DK�fDM��DPl�DR� DUffDW�fDZffD\��D_ffDaٚDdY�DfٚDiY�Dk� DnS3Dp�fDs9�Du��DxfDzl�D|� D~�3D���D�� D�3D�<�D�l�D��fD��fD�ٚD�fD�  D�9�D�\�D�s3D��fD�� D���D�ٚD�� D�3D�33D�P D�ffD�� D��fD�� D��3D�� D��D�33D�` D���D���D���D�  D�S3D�� D�� D��3D�fD�@ D�l�D���D���D��fD�&fD�VfD�� D���D��fD�  D�,�D�Y�D��3D��fD��fD��fD�3D�9�D�Y�D�s3DŌ�DƬ�D���D��3D��D�)�D�C3D�\�D΀ DϖfDг3D���D�� D��fD��D�,�D�I�D�c3Dـ Dڜ�D۬�Dܳ3Dݹ�D��3D���D��3D��3D��3D��fD��3D���D��fD�  D�3D�3D�fD�3D�3D�  D���D��fD��fD��fD���D�  D�3D�	�D� D�3D�3D��D�  D�  D���D���D�3E �E ��E3E��E E�3E�3E�3E+3E,�E�3E	�3E E�Ep E�E� ENfEL�EњE�3EH E@ E��E3E	�Ep EњE��E   E!�3E"t�E#�3E%4�E&�fE'~fE(��E*A�E+6fE,� E-��E/X E0>fE1��E2�fE4@ E5� E6y�E7��E9fE:q�E;� E>�fEA��ED�3EH3EKfENffEQP ETi�EW�3EZ��E]�fEa)�Ed!�EgI�EjNfEmt�Ep� Es��Ev�3Ey�3E} E}� E~D�EfE�3E�+3E�p�E��3?   >���>���>���?   ?   ?   ?   >���?   ?��?��?��?333?L��?L��?fff?�  ?���?���?�ff?�  ?ٙ�?�33@��@��@&ff@333@Fff@Y��@l��@�  @�ff@�33@���@�ff@���@���@�ff@�33@�33@���@�ffA��A	��A  AffAffA$��A.ffA4��A;33AC33AI��AS33A[33Aa��Ah  AnffAvffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441144114111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?�  ?�33@&ff@`  @�  @�  @�  @�  AffA  A.ffAH  Ai��A���A�ffA�  A�  A�  A���A���A���B��B	��B  B��B"  B*ffB2ffB:��BBffBJffBRffBZ��Bb��Bj  Br  BzffB�33B�  B�33B�  B���B�  B�33B�  B���B�33B�33B�ffB�33B�  B�  B�ffB�33B�  Bș�B���B�33B�ffBٙ�B���B�  B�  B�33B�33B�  B���B���B�ffC ��C� CffCL�C� C
� CffC��C��CL�C� C�3C�3C� C�3C��C L�C"ffC$��C&�3C(ffC*L�C,ffC.��C0� C2L�C4��C6�3C8� C:L�C<ffC>��C@� CBL�CD��CF� CHL�CJ� CL�3CN��CPL�CR��CT��CV��CX� CZ�3C\��C^ffC`�3Cb��CdffCf��Ch��CjL�Cl��Cn��Cp�3Cr��Ct�3Cv� Cx�3Cz��C|��C~� C�Y�C�Y�C�@ C�Y�C�@ C�33C�L�C�ffC�L�C�33C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�L�C�33C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�@ C�@ C�Y�C�Y�C�ffC�L�C�&fC�33C�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�@ C�ffC�@ C�33C�L�C�Y�C�@ C�&fC�L�C�@ C�&fC�@ C�ffC�Y�C�@ C�Y�C�@ C�&fC�L�C�Y�C�L�C�33C�L�C�@ C�&fC�Y�C�ffC�Y�C�@ C�Y�C�ffC�Y�C�@ C�L�C�ffC�ffC�@ C�L�C�Y�C�@ C�&fC�&fC�33C�Y�C�ffC�Y�C�s3C�Y�C�&fC�&fC�33C�@ C�L�C�@ C�L�C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�33C�33C�L�C�s3C�Y�C�ffC�ffC�Y�C�Y�C�ffC�ffC�L�C�L�C�ffC�ffC�@ C��C�&fC�33C�@ C�@ D ,�D ��D9�D��D3D�D	�fD�D��D  D��DFfD�fDL�D��D 33D"��D%  D'�3D*fD,� D/  D1s3D3ٚD6@ D8��D;�D=s3D?ٚDB33DD�fDF�fDI@ DK�fDN�DP��DS  DU�fDXfDZ�fD]�D_�fDa��Ddy�Df��Diy�Dl  Dns3Dp�fDsY�Du��Dx&fDz��D|� D3D���D�� D�#3D�L�D�|�D��fD��fD��D�fD�0 D�I�D�l�D��3D��fD�� D���D��D�  D�#3D�C3D�` D�vfD�� D��fD�� D��3D�  D��D�C3D�p D���D���D���D�0 D�c3D�� D�� D��3D�&fD�P D�|�D���D���D�fD�6fD�ffD�� D���D��fD� D�<�D�i�D��3D��fD��fD��fD�#3D�I�D�i�Dă3DŜ�DƼ�D���D�3D��D�9�D�S3D�l�Dΐ DϦfD��3D���D�� D�fD��D�<�D�Y�D�s3Dِ Dڬ�Dۼ�D��3D�ɚD��3D���D��3D��3D��3D��fD�3D�	�D�fD� D�3D�3D�fD�3D�3D� D�	�D�fD�fD�fD��D� D�3D��D�  D�#3D�#3D�,�D� D� D��D��D�3E 	�E ��E3E��E E�3E�3E�3E33E4�E�3E	�3E E	�Ex E�E� EVfET�EٚE�3EP EH E��E#3E�Ex EٚE��E ( E!�3E"|�E#�3E%<�E&�fE'�fE(��E*I�E+>fE,� E.�E/` E0FfE1��E2�fE4H E5� E6��E7��E9&fE:y�E;� E>�fEA��ED�3EH3EKfENnfEQX ETq�EW�3EZ��E]�fEa1�Ed)�EgQ�EjVfEm|�Ep� Es��Ev�3Ey�3E}  E}� E~L�E&fE�3E�/3E�t�E��3G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�?L��?�  G�O�G�O�?���?���G�O�?�ff?�33?�  ?���?ٙ�?�ff@   @��@��@,��@9��@Fff@S33@fff@y��@�ff@�  @�ff@�33@���@�ff@���@ə�@�ff@�33@�33@���A33A	��A��A  AffA&ffA,��A6ffA<��AC33AK33AQ��A[33Ac33Ai��Ap  AvffA~ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441144114111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ j@ %@ V@ {@ O@ ""@ (�@ /�@ 5�@ =q@ F�@ Q�@ `B@ m�@ y�@ ��@ �0@ ��@ �-@ ��@ �|@ ��@ �m@ ��@�@@g@-@;d@H]@V@c�@r@�@��@��@��@��@@��@��@��@�,@�@{@!s@0x@>@Lu@Yn@ff@t@�@�@�@�M@��@ƨ@��@�T@�@@��@
=@�@&;@33@B�@P�@]�@j@ww@�p@�h@�m@�@�@�@׹@�T@�Y@^@@O@*S@7L@B�@Q=@`B@n�@z3@�+@��@��@�~@��@�|@��@�@�e@�@�@�@+@:�@G�@S�@b�@r@~�@��@�H@�M@��@@є@ލ@��@��@�@�@"�@0x@<@K�@Z�@g�@t�@�@�\@�a@��@�^@��@��@�@��@��@
=@6@&;@5?@A�@M�@\)@k.@ww@��@��@�@�r@��@�@�[@�@�Y@ �@@[@)�@5?@DD@Q�@_�@n�@|?@��@��@�y@��@�&@�*@܀@��@�@	@	o@	g@	+�@	:@	I�@	UU@	bN@	qS@	�@	��@	�<@	��@	��@	�2@	�7@	��@	��@	�,@
1@
{@
 �@
0x@
>�@
K�@
X@
g@
t@
�W@
��@
�@
�@
�R@
�W@
խ@
�@
��@
�E@J@�@%�@3�@B8@N�@Z�@hs@v�@�|@��@��@��@�@�@խ@�@�Y@ �@�@�@*S@8�@FQ@R�@^5@k�@z3@��@��@��@�9@��@��@܀@�y@� @v@@g@-@<@I�@UU@`�@oF@}�@��@��@��@�F@�J@��@܀@dZ@��@��@1'@ww@�w@@H]@�P@ψ@@V�@��@܀@g@b�@�A@�(@+�@m:@�r@�@1�@s_@��@�@3�@t@��@�~@;d@~K@�>@�@K�@�@�O@6@[z@��@�@(�@k�@��@�@4�@t�@�F@��@1�@s_@��@�q@5�@v�@�F@�@1�@r@�@�(@(G@c�@�a@�t@6@S�@�\@�|@ 
�@ G�@ �@ �&@ ��@!6�@!t�@!�~@!�@@"-@"m:@"��@"�@@#/@#p�@#�-@#�Y@$33@$t�@$�F@$��@%5�@%v�@%��@%� @&7�@&x�@&�R@&�~@'7�@'ww@'��@'��@(7L@(uk@(��@(�L@)0x@)oF@)��@)��@*$�@*bN@*��@*ލ@+�@+Wb@+�u@+ψ@,�@,I@,��@,��@,��@-7�@-s_@-��@-�@.)�@.ff@.�(@.�/@/*@/M$@/��@/�w@/�q@0-@0g@0�a@0׹@1�@1E�@1~K@1��@1�4@2#�@2Yn@2�@2��@2�9@31'@3g�@3�a@3�\@4�@4D�@4|�@4��@4�4@5"�@5[z@5��@5@5�~@6/@6g@6��@6�O@7�@7B�@7z�@7��@8&�@8��@9>�@9��@:I@:��@;Q�@;�k@<X�@<�q@=_�@=�Q@>k�@?�@?�@@�@@��@A'�@A��@B&;@B�&@CV@C�^@DQ�@D��@EP�@E�@F|�@G�@Gv�@HJ@H��@I�@I��@J:�@J�7@K2�@K�>@LT�@L�@Mz�@M�#@Nk�@N��@O��@P�@Qi!@R��@T1@U]�@V��@Xb@YN�@Z�@[� @]P�@^��@`�@aV@b��@c�}@eP�@f��@hv@iI@j��@k��@l:@l|�@l��@mB@m^�@m�I@m��G�O�G�O�G�O�@ �G�O�G�O�G�O�G�O�@ @ jG�O�G�O�@ @ �G�O�@ v@ %@ �@ �@ 1@ �@ 
=@ �@ �@ @ b@ �@ @ *@ 6@ B@ O@ �@ g@ !s@ #�@ $�@ '�@ *S@ -@ 0x@ 2�@ 4�@ 7L@ :�@ =q@ @,@ C�@ FQ@ Ji@ M$@ O�@ SI@ V@ Z@ ]�@ `B@ b�@ e�@ i!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�^5A�\)A�O�A�5?A�A���Aͥ�A͑hA̓A�|�A�x�A�p�A�l�A�ffA�bNA�`BA�ZA�ZA�ZA�XA�VA�S�A�Q�A�M�A�K�A�I�A�G�A�E�A�G�A�A�A�^5A�"�A���A�bNA�hsA�oA�S�A� �A�dZA���A���A��uA�=qA��A�E�A� �A�O�A���A���A��DA��`A�S�A�bNA��^A�l�A��;A�9XA��TA��wA�-A�S�A��-A��DA�?}A��#A�bNA�^5A�VA�$�A���A�oA��TA�JA�jA�/A��PA�G�A��A�A��A�{A�~�A���A��A�ƨA��A��A�9XA���A��9A���A�ƨA�t�A�E�A�O�A�5?A�$�A���A���A�XA�jA�bA��/A���A�p�A�`BA��
A���A�&�A�jA�n�A�jA���A~�/A~�A~JA}XA|M�Az��Ay�TAw+AuƨAuAt^5AsC�Ap�An��Al-Ah��Ag+AfM�AedZAd~�Ab�`A`jA]�7AZ��AW��AV��AU��AT�AR1'AO�PAM��AMoAKXAJv�AIC�AH^5AG�mAG�AF��AFbAD��AC�mAB1'A@�!A?��A>^5A<�A;\)A9ƨA7x�A6M�A4jA3�A3x�A1hsA/�A-�A,-A*�A)A(�HA(n�A(bA'��A&E�A$ȴA#��A#7LA"1'A!&�A ��A =qA�-Az�A��A�A�hA�A�A�^AC�A��A��A�A/A��AXA�Av�A��A
�A
n�A
�A	�wA	%An�A�A��A��AhsAC�A��Al�A��A�jA �A�^A+@�t�@���@�O�@�M�@�1@��T@�(�@�@�w@��-@�V@�b@�33@�v�@�V@��y@�!@�ff@�7L@ݩ�@�E�@��/@�Z@׾w@֟�@պ^@���@�1@�ƨ@ӥ�@�^5@��@��;@�K�@�o@Η�@�-@Ͳ-@��@��/@�Ĝ@��@�o@�V@���@���@�%@��P@��w@��@��
@�5?@���@�v�@��h@��m@�/@���@�S�@�|�@�
=@�J@� �@��@��+@�v�@��@�V@�C�@�n�@�hs@�K�@��#@���@�33@�@��@���@�-@��@�Ĝ@��@��`@�1@�o@��@��P@��+@�=q@��@�Z@K�@~$�@{��@y&�@w�P@s��@s��@s�
@r�!@nV@l�@j-@hbN@f5?@e@cƨ@`r�@_�@_|�@_
=@]�-@\z�@Z�!@X��@W�@V�+@T��@S33@R-@Q�7@Qhs@Q&�@OK�@M�T@M�@L��@L(�@Jn�@HA�@G�;@F�y@D��@D(�@BJ@A7L@@Q�@>V@<�j@:n�@9%@7l�@5�@3ƨ@2n�@1x�@0�u@0 �@-@-p�@,��@,�D@+�F@+@)�#@)X@(bN@(  @(b@'l�@&�y@&ff@%p�@$9X@"-@!�#@!�7@ bN@��@V@�T@��@�/@�@��@�@�H@�9@��@�@1'@  @�;@�@��@p�@(�@ƨ@��@Q�@�@@@�h@�/@�@��@1@t�@o@	�@Ĝ@  @�@�y@ff@�@�@j@�m@�@�!@��@ �`?�\)?�v�?��?��#?�r�?�?��
?��?���??�p�?�D?�?�^5?��#?�r�?�l�?��/?�t�?�A�?�V?�V?�?ڟ�?�X?�b?�l�?�ȴ?�`B?��?���?ҏ\?�J?Ѓ?�\)?���?͑h?�ƨ?�C�?���?���?�=q?��?���?�b?�ȴ?��T?š�?��/?ě�?�33?�hs?���?��D?�C�?�^5?�^5?��#?�X?�x�?���?��^?���?���?�~�?��H?�"�?��m?�I�?�j?��?��?��?�V?�p�?�p�A�`BA�ffA�`BA�`BA�`BA�dZA�bNA�`BA�\)A�^5A�^5A�^5A�\)A�ZA�XA�\)A�XA�`BA�`BA�dZA�`BA�\)A�\)A�^5A�\)A�VA�VA�\)A�O�A�?}A�9XA�33A�1'A��A�%A���A��A��#A�ĜAͶFAͥ�A͗�A͑hA͏\A͇+A̓ÁA�~�A�|�A�z�A�x�A�x�A�x�A�v�A�r�A�r�A�p�A�p�A�n�A�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A�`BA�^5A�\)A�O�A�5?A�A���Aͥ�A͑hA̓A�|�A�x�A�p�A�l�A�ffA�bNA�`BA�ZA�ZA�ZA�XA�VA�S�A�Q�A�M�A�K�A�I�A�G�A�E�A�G�A�A�A�^5A�"�A���A�bNA�hsA�oA�S�A� �A�dZA���A���A��uA�=qA��A�E�A� �A�O�A���A���A��DA��`A�S�A�bNA��^A�l�A��;A�9XA��TA��wA�-A�S�A��-A��DA�?}A��#A�bNA�^5A�VA�$�A���A�oA��TA�JA�jA�/A��PA�G�A��A�A��A�{A�~�A���A��A�ƨA��A��A�9XA���A��9A���A�ƨA�t�A�E�A�O�A�5?A�$�A���A���A�XA�jA�bA��/A���A�p�A�`BA��
A���A�&�A�jA�n�A�jA���A~�/A~�A~JA}XA|M�Az��Ay�TAw+AuƨAuAt^5AsC�Ap�An��Al-Ah��Ag+AfM�AedZAd~�Ab�`A`jA]�7AZ��AW��AV��AU��AT�AR1'AO�PAM��AMoAKXAJv�AIC�AH^5AG�mAG�AF��AFbAD��AC�mAB1'A@�!A?��A>^5A<�A;\)A9ƨA7x�A6M�A4jA3�A3x�A1hsA/�A-�A,-A*�A)A(�HA(n�A(bA'��A&E�A$ȴA#��A#7LA"1'A!&�A ��A =qA�-Az�A��A�A�hA�A�A�^AC�A��A��A�A/A��AXA�Av�A��A
�A
n�A
�A	�wA	%An�A�A��A��AhsAC�A��Al�A��A�jA �A�^A+@�t�@���@�O�@�M�@�1@��T@�(�@�@�w@��-@�V@�b@�33@�v�@�V@��y@�!@�ff@�7L@ݩ�@�E�@��/@�Z@׾w@֟�@պ^@���@�1@�ƨ@ӥ�@�^5@��@��;@�K�@�o@Η�@�-@Ͳ-@��@��/@�Ĝ@��@�o@�V@���@���@�%@��P@��w@��@��
@�5?@���@�v�@��h@��m@�/@���@�S�@�|�@�
=@�J@� �@��@��+@�v�@��@�V@�C�@�n�@�hs@�K�@��#@���@�33@�@��@���@�-@��@�Ĝ@��@��`@�1@�o@��@��P@��+@�=q@��@�Z@K�@~$�@{��@y&�@w�P@s��@s��@s�
@r�!@nV@l�@j-@hbN@f5?@e@cƨ@`r�@_�@_|�@_
=@]�-@\z�@Z�!@X��@W�@V�+@T��@S33@R-@Q�7@Qhs@Q&�@OK�@M�T@M�@L��@L(�@Jn�@HA�@G�;@F�y@D��@D(�@BJ@A7L@@Q�@>V@<�j@:n�@9%@7l�@5�@3ƨ@2n�@1x�@0�u@0 �@-@-p�@,��@,�D@+�F@+@)�#@)X@(bN@(  @(b@'l�@&�y@&ff@%p�@$9X@"-@!�#@!�7@ bN@��@V@�T@��@�/@�@��@�@�H@�9@��@�@1'@  @�;@�@��@p�@(�@ƨ@��@Q�@�@@@�h@�/@�@��@1@t�@o@	�@Ĝ@  @�@�y@ff@�@�@j@�m@�@�!@��@ �`?�\)?�v�?��?��#?�r�?�?��
?��?���??�p�?�D?�?�^5?��#?�r�?�l�?��/?�t�?�A�?�V?�V?�?ڟ�?�X?�b?�l�?�ȴ?�`B?��?���?ҏ\?�J?Ѓ?�\)?���?͑h?�ƨ?�C�?���?���?�=q?��?���?�b?�ȴ?��T?š�?��/?ě�?�33?�hs?���?��D?�C�?�^5?�^5?��#?�X?�x�?���?��^?���?���?�~�?��H?�"�?��m?�I�?�j?��?��?��?�V?�p�?�p�A�`BA�ffA�`BA�`BA�`BA�dZA�bNA�`BA�\)A�^5A�^5A�^5A�\)A�ZA�XA�\)A�XA�`BA�`BA�dZA�`BA�\)A�\)A�^5A�\)A�VA�VA�\)A�O�A�?}A�9XA�33A�1'A��A�%A���A��A��#A�ĜAͶFAͥ�A͗�A͑hA͏\A͇+A̓ÁA�~�A�|�A�z�A�x�A�x�A�x�A�v�A�r�A�r�A�p�A�p�A�n�A�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�By�By�Bx�Bx�Bv�Bt�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bv�Bv�Bv�Bw�Bw�Bw�Bw�By�B�JBŢB�fB{BQ�B[#BgmBq�Bo�B� B�%B�=B�=B�+B�%B�=B�DB�\B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B�{B�{B�\B�oB�oB�bB�bB�=B�DB�7B� Bz�BjBP�BD�B1'B"�BhB
=B��B�)B��B�?B�B��B�{B�VBy�BiyB_;BXBB�B5?B!�BoB	7B
��B
�B
�5B
ɺB
�dB
�3B
�'B
�B
��B
�PB
�B
aHB
]/B
ZB
J�B
?}B
:^B
49B
/B
'�B
�B
oB
B	��B	�B	�B	�B	�HB	��B	�FB	��B	�hB	�DB	�%B	}�B	y�B	k�B	^5B	W
B	C�B	=qB	6FB	,B	�B	\B	
=B��B��B�B�B�B�yB�fB�NB�#B�#B�B��B��B��BŢB�}B�jB�9B�B��B��B��B��B�oB�JB�%B� By�Bt�Bp�Bn�Bm�BjBe`BbNB_;B^5BYBXBW
BS�BP�BL�BK�BC�B@�B<jB:^B7LB6FB6FB2-B33B.B/B,B-B+B'�B'�B'�B'�B&�B&�B&�B%�B#�B$�B&�B&�B%�B%�B%�B%�B$�B$�B$�B$�B'�B$�B&�B%�B&�B'�B'�B'�B'�B)�B-B,B,B'�B.B.B-B,B0!B49B49B33B33B33B49B5?B7LB7LB7LB9XB<jB?}BA�BA�BB�BE�BG�BI�BI�BJ�BK�BN�BO�BaHB|�B�B�bB��B��B�RBB��B�`B�B�B	B	�B	,B	?}B	R�B	]/B	p�B	s�B	z�B	z�B	�B	�B	�DB	�hB	��B	��B	�B	�!B	�^B	ĜB	��B	��B	�
B	�B	�)B	�#B	�NB	�ZB	�mB	�B	�B	��B	��B	��B	��B	��B	��B
B
%B
+B
DB
DB

=B
PB
hB
oB
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
"�B
$�B
&�B
'�B
)�B
+B
,B
-B
/B
/B
0!B
/B
2-B
49B
49B
49B
5?B
6FB
7LB
8RB
9XB
=qB
=qB
@�B
@�B
@�B
B�B
D�B
F�B
G�B
I�B
J�B
L�B
M�B
O�B
O�B
O�B
R�B
R�B
R�B
S�B
S�B
T�B
VB
VB
XB
XB
W
B
XB
YB
YB
ZB
[#B
]/B
]/B
]/B
^5B
_;B
`BB
aHB
aHB
bNB
aHB
aHB
bNB
cTB
ffB
e`B
e`B
e`B
e`B
ffB
gmB
hsB
iyB
jB
jB
l�B
m�B
n�B
p�B
o�B
o�B
q�B
q�B
q�B
s�B
s�B
s�B
t�B
v�B
w�B
w�B
w�B
x�B
z�B
z�B
z�B
{�B
{�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�1B
�+B
�1B
�DB
�JB
�PB
�VB
�VB
�VB
�\B
�bB
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
�B
�B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�^B
�XB
�^B
�^B
�XB
�XBz�Bz�Bw�By�By�By�Bx�Bz�Bx�Bz�By�By�By�Bz�Bz�Bx�Bz�By�By�Bz�Bx�By�Bz�Bz�By�Bw�Bz�By�Bv�Bw�Bx�Bx�Bw�Bt�Bv�Bu�Bw�Bt�Bt�Bu�Bu�Bv�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Bv�Bv�Bv�Bu�Bu�Bs�Bq�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bt�Bt�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bv�B�7BB�TBhBN�BXBdZBn�Bl�B|�B�B�+B�+B�B�B�+B�1B�JB�oB�{B�{B�uB�{B��B��B�uB��B�{B�{B�bB�oB�uB�hB�hB�JB�\B�\B�PB�PB�+B�1B�%B|�Bw�BgmBM�BA�B.B�BVB+B��B�BɺB�-B��B��B�hB�DBv�BffB\)BT�B?}B2-B�B\B%B
��B
�B
�#B
ƨB
�RB
�!B
�B
�B
��B
�=B
� B
^5B
ZB
W
B
G�B
<jB
7LB
1'B
,B
$�B
�B
\B
  B	��B	�B	�B	�mB	�5B	��B	�3B	��B	�VB	�1B	�B	{�B	v�B	hsB	[#B	T�B	@�B	;dB	49B	)�B	�B	PB	1B��B��B�B�B�sB�mB�ZB�BB�B�B�B��B��BɺBÖB�qB�^B�-B�B��B��B��B��B�bB�=B�B}�Bw�Br�Bn�Bl�Bk�BhsBcTB`BB]/B\)BW
BVBT�BQ�BN�BJ�BI�BA�B>wB:^B8RB5?B49B49B0!B1'B,B-B)�B+B(�B%�B%�B%�B%�B$�B$�B$�B#�B!�B"�B$�B$�B#�B#�B#�B#�B"�B"�B"�B"�B%�B"�B$�B#�B$�B%�B%�B%�B%�B'�B+B)�B)�B%�B,B,B+B)�B.B2-B2-B1'B1'B1'B2-B33B5?B5?B5?B7LB:^B=qB?}B?}B@�BC�BE�BG�BG�BH�BI�BL�BM�B_;Bz�B�B�VB��B��B�FB��B��B�TB�B�B	B	�B	)�B	=qB	P�B	[#B	n�B	q�B	x�B	x�B	� B	�B	�7B	�\B	��B	��B	��B	�B	�RB	B	��B	��B	��B	�B	�B	�B	�BB	�NB	�`B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
	7B
	7B
1B
DB
\B
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
&�B
(�B
)�B
+B
,B
.B
.B
/B
.B
1'B
33B
33B
33B
49B
5?B
6FB
7LB
8RB
<jB
<jB
?}B
?}B
?}B
A�B
C�B
E�B
F�B
H�B
I�B
K�B
L�B
N�B
N�B
N�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
W
B
W
B
VB
W
B
XB
XB
YB
ZB
\)B
\)B
\)B
]/B
^5B
_;B
`BB
`BB
aHB
`BB
`BB
aHB
bNB
e`B
dZB
dZB
dZB
dZB
e`B
ffB
gmB
hsB
iyB
iyB
k�B
l�B
m�B
o�B
n�B
n�B
p�B
p�B
p�B
r�B
r�B
r�B
s�B
u�B
v�B
v�B
v�B
w�B
y�B
y�B
y�B
z�B
z�B
|�B
}�B
� B
�B
�B
�B
�B
�B
�%B
�1B
�+B
�1B
�DB
�JB
�PB
�VB
�VB
�VB
�\B
�bB
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
�B
�B
�B
�B
�B
�B
�'B
�3B
�9B
�?B
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
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
�dB
�dBw�Bw�Bt�Bv�Bv�Bv�Bu�Bw�Bu�Bw�Bv�Bv�Bv�Bw�Bw�Bu�Bw�Bv�Bv�Bw�Bu�Bv�Bw�Bw�Bv�Bt�Bw�Bv�Bs�Bt�Bu�Bu�Bt�Bq�Bs�Br�Bt�Bq�Bq�Br�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812111100402021061413530320210614135303202106141746592021061417465920210614174659201812111100402021061413530320210614135303202106141746592021061417465920210614174659PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018121111004020181211110040  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121111004020181211110040QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121111004020181211110040QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015620210722160156IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                