CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-18T17:02:00Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        d$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        tD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	�Argo profile    3.1 1.2 19500101000000  20181018170200  20210617131506  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               (   (DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؇bޠ�@؇bޠ�11  @؇b�-�@@؇b�-�@@6ڡ��͊@6ڡ��͊�c��V��`�c��V��`11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?�  @ff@@  @�  @�  @�  @�33A33A��A$��A@  A^ffA~ffA�  A���A���A���A�33A�33A���B ffB��B  B��B   B'��B0ffB8��B@ffBHffBP��BXffB`  BhffBp��BxffB�  B�  B�33B�  B�  B�  B�  B�ffB�ffB���B�33B�33B�33B�ffB�ffB�ffB�33B�33BǙ�B�  B�  B���B�ffB�ffB�33B�ffB�  B�33B���B���B���B���B���C  C��C33C33C
33C33C  CL�C33C  C  C�fC33C�C�C�fC!�3C#�fC&�C(L�C*  C+�fC.�C033C2  C3��C5��C8  C:  C<�C>�C@33CB33CD�CE��CG��CI�fCK�fCN�CP33CR33CT  CV33CX�CZ  C\33C^�C_�fCb33Cd�Ce�fCh�Cj  Ck�fCn  Cp33Cr�Cs�fCv33Cx  Cy��C|  C~33C�&fC�  C��fC��3C��C��C�  C��fC��3C��C�&fC�&fC��C��3C�  C��C�  C��fC��3C��C�&fC�33C��C��3C��C��C��C�&fC��C�  C��C��C��C��3C��C��C�&fC��C��3C�  C��C��C�  C��fC�  C��C��C�&fC��C��fC��3C��C��C�&fC��C��fC�  C��C��C�&fC��C�  C�&fC��C��3C�  C��C��C��C��fC��3C�  C��C��C�&fC��C��fC��3C��C��C�&fC��C��fC��fC��3C��3C�  C�  C��C�  C��C�  C�  C��3C�  C��fC��3C��3C�  C�  C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C�&fC�&fC��C��fC��3C��3C��3C�  C�ٚC��C��C��C�&fC�33D �D l�D ��Dl�D	�D��DL�D�3D��DffD�D� D��D!Y�D$fD&��D)` D,fD.��D1�D3�3D5��D8` D:� D=&fD?�3DBfDDs3DF� DI9�DK� DN  DPl�DR�3DU@ DW��DZ,�D\��D_33Da��Dd,�Df��DiY�Dk��Dn� DqfDs��Dv3Dx��D{,�D}FfDٚD�33D�vfD���D�fD�L�D��fD���D�#3D�l�D���D�� D�33D�vfD���D���D�<�D�� D���D�  D�<�D��fD�� D��fD�)�D�` D��3D�� D��D��D�C3D�ffD��3D���D���D�fD�0 D�Y�D��fD��fD���D�� D� D�6fD�Y�D�� D��fD���D��3D��D�<�D�\�D���D���D���D�� D�3D�33D�\�D�y�DǠ Dȹ�D��3D��fD��3D�fD�fD�)�D�9�D�L�D�Y�D�ffD�vfDՉ�D֙�Dף3Dج�DٶfDڹ�D�� D��3D��fD��fD�ٚD�ٚD�� D�ٚD���D�ٚD��fD�� D�ٚD�� D��D��3D���D�fD� D��D�  D�&fD�)�D�,�D�@ D�FfD�P D�` D�s3D��3D�vfD��3D��fD��fD���D��3E t�E �fE��E E��E$�E��E4�EK3E` Et�E�fE
0 ED�E` E  E E&fE�3E� E�3E��E��E��E&fE&fE� E�3EfE  E k3E!��E#�E$^fE%��E&� E(  E).fE*X E+�fE-fE.4�E/S3E0њE1�fE33E4@ E5t�E73E8H E9�3E:�fE<fE?�EB EE�EH;3EK` EN�EQ�fET��EW�fEZ��E^\�EaC3Edl�Egy�Ej��Em�3EpњEt Ew\�Ez@ E}\�E�U�E���E�m�E���E��fE�3E��fE�"fE�͚E��E�`�E��3E��fE�FfE��f?   >���?   ?   ?   ?   ?   >���?   >���>���?   >���?   >���?   >���>���?   ?��?��?333?fff?���?���?�33?�  ?ٙ�?�33@��@��@&ff@@  @`  @l��@�33@���@�ff@���@���@���@�ff@�33@�33@�  A   A��A��A��AffA$��A,��A333A<��AD��AL��AS33A[33Ac33Ai��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444414414141441141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?�  ?�  @&ff@`  @�  @�  @�  @�33A33A��A,��AH  AfffA�33A�  A���A���A���A�33A�33A���BffB	��B  B��B"  B)��B2ffB:��BBffBJffBR��BZffBb  BjffBr��BzffB�  B�  B�33B�  B�  B�  B�  B�ffB�ffB���B�33B�33B�33B�ffB�ffB�ffB�33B�33Bș�B�  B�  B���B�ffB�ffB�33B�ffB�  B�33B���B���B���B���C ffC� CL�C�3C�3C
�3C�3C� C��C�3C� C� CffC�3C��C��C ffC"33C$ffC&��C(��C*� C,ffC.��C0�3C2� C4L�C6L�C8� C:� C<��C>��C@�3CB�3CD��CFL�CHL�CJffCLffCN��CP�3CR�3CT� CV�3CX��CZ� C\�3C^��C`ffCb�3Cd��CfffCh��Cj� ClffCn� Cp�3Cr��CtffCv�3Cx� CzL�C|� C~�3C�ffC�@ C�&fC�33C�L�C�Y�C�@ C�&fC�33C�Y�C�ffC�ffC�L�C�33C�@ C�L�C�@ C�&fC�33C�Y�C�ffC�s3C�Y�C�33C�L�C�L�C�Y�C�ffC�Y�C�@ C�L�C�Y�C�L�C�33C�L�C�Y�C�ffC�Y�C�33C�@ C�L�C�Y�C�@ C�&fC�@ C�Y�C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�Y�C�ffC�Y�C�@ C�ffC�L�C�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�ffC�L�C�&fC�&fC�33C�33C�@ C�@ C�L�C�@ C�L�C�@ C�@ C�33C�@ C�&fC�33C�33C�@ C�@ C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�Y�C�Y�C�L�C�L�C�Y�C�Y�C�Y�C�ffC�ffC�L�C�&fC�33C�33C�33C�@ C��C�L�C�L�C�Y�C�ffC�s3D 9�D ��D�D��D	,�D��Dl�D3D��D�fD9�D  D��D!y�D$&fD&��D)� D,&fD.��D19�D3�3D6�D8� D:� D=FfD?�3DB&fDD�3DG  DIY�DK� DN  DP��DR�3DU` DWٚDZL�D\ٚD_S3Da��DdL�Df��Diy�Dl�Dn� Dq&fDs��Dv33Dx��D{L�D}ffD��D�C3D��fD���D�fD�\�D��fD���D�33D�|�D���D�  D�C3D��fD�ɚD��D�L�D�� D���D� D�L�D��fD�� D�fD�9�D�p D��3D�� D���D�)�D�S3D�vfD��3D�ɚD���D�fD�@ D�i�D��fD��fD���D�  D�  D�FfD�i�D�� D��fD���D�3D�)�D�L�D�l�D���D���D���D�  D�#3D�C3D�l�DƉ�Dǰ D�ɚD��3D��fD�3D�fD�&fD�9�D�I�D�\�D�i�D�vfDԆfDՙ�D֩�D׳3Dؼ�D��fD�ɚD�� D��3D��fD��fD��D��D�� D��D���D��D��fD�� D��D�� D���D�3D��D�fD�  D�)�D�0 D�6fD�9�D�<�D�P D�VfD�` D�p D��3D��3D��fD��3D��fD��fD�ɚD��3E |�EfE��E E��E,�E��E<�ES3Eh E|�E�fE
8 EL�Eh E E E.fE�3E� E�3E��E��E��E.fE.fE� E�3EfE E s3E!��E#!�E$ffE%��E&� E( E)6fE*` E+�fE-fE.<�E/[3E0ٚE1�fE3#3E4H E5|�E73E8P E9�3E:�fE<fE?�EB  EE$�EHC3EKh EN�EQ�fET��EW�fE[�E^d�EaK3Edt�Eg��Ej��Em�3EpٚEt Ewd�EzH E}d�E�Y�E���E�q�E���E��fE�3E��fE�&fE�њE��E�d�E��3E��fE�JfE��fG�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�?fffG�O�?fffG�O�?fffG�O�G�O�?fff?�  G�O�?���?���?�33?���?ٙ�?�33@   @��@��@,��@9��@Fff@`  @�  @�ff@�33@���@�ff@���@���@ə�@�ff@�33@�33A   A  A��A��A��A&ffA,��A4��A;33AD��AL��AT��A[33Ac33Ak33Aq��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444414414141441141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ j@ �@ V@ {@ O@ ""@ (�@ 0x@ 7�@ >@ FQ@ Q�@ ^�@ l�@ z�@ ��@ ��@ ��@ ��@ �w@ �|@ �#@ �m@ ��@�@@
@-@;d@H]@V@dZ@qS@~K@��@��@��@��@@��@��@�@�,@�@�@#�@/@>@K�@Yn@g�@uk@�@�@��@�M@�R@��@��@�@�L@�E@�@�@&;@2�@@,@M�@[z@i!@ww@��@�#@��@�r@�@�c@�@�@�Y@  @�@�@)�@7L@C�@O�@^�@m�@|�@��@��@��@��@�&@�o@�@�@��@@�@ @-�@:�@FQ@S�@bN@o�@~�@�P@��@�A@�F@�>@�7@�;@�4@�~@1@*@!s@0x@=q@Ji@X�@g�@t�@�@��@�@�M@�R@�W@խ@�H@�@��@
�@B@%�@1�@@,@O�@^5@k�@x&@�p@��@�@�@�^@ȴ@�h@�@�@^@�@�@)�@7�@FQ@SI@_�@m�@|?@�7@��@��@��@�2@�*@��@�@�q@	�@	@	[@	,`@	;d@	I@	Wb@	c�@	oF@	}�@	��@	��@	�M@	��@	�2@	�7@	ލ@	��@	�9@
1@
{@
$.@
0x@
<�@
K@
Yn@
g�@
t�@
�W@
��@
�@
�Y@
��@
�@
�O@
��@
�@@
�E@�@�@&;@1�@?}@M�@[z@i�@ww@��@��@�@�@��@ȴ@�
@�T@�@�Q@�@O@)�@7�@E�@SI@`�@n�@|�@��@��@��@�-@��@�*@��@�(@��@@�@
@+�@9X@G�@SI@c�@qS@�@��@�U@��@��@��@V�@�a@�@-�@v@��@
�@T�@�m@��@5�@~�@�W@@Yn@��@�@'�@i!@��@�@-@oF@�-@�e@6�@v�@�R@�,@;d@|�@�&@�@E�@�D@��@o@V�@�a@�@*S@p�@��@��@?}@�p@��@@Ji@�@��@�@`�@��@��@2�@x&@�w@�@G�@��@є@�@[z@��@�@ (G@ m:@ ��@ � @!:@!|?@!��@"  @"A�@"��@"�2@#@#A�@#�@#��@#��@$<�@$|?@$��@$�9@%;d@%x�@%��@%��@&33@&r@&�!@&��@'-�@'l�@'�Y@'�(@((G@(e�@(��@(�T@)!s@)_�@)��@)�#@*�@*Wb@*�0@*�C@+V@+I@+�d@+�@+� @,1�@,k�@,��@,��@-B@-SI@-��@-�@. �@.9X@.r@.�M@.�H@/�@/O�@/��@/�2@/��@0/�@0e	@0�U@0�C@11@1=q@1v@1�@1�@2g@2X@2��@2�c@3@3:@3r@3�M@3��@4O@4SI@4��@4��@5 �@5:�@5n�@5��@5�@6�@6Wb@6�u@6��@7	�@7B�@7~K@7�@7�e@8.l@8hs@8�;@9UU@9�o@:@,@:��@;k�@;�@<�0@=
=@=�@>1'@>�A@?�@?�@@<@@��@AX@A�J@Bff@Bє@Co�@C�
@Dr@Ev@E��@F!s@F��@G,`@G�@H/@H�@I/@I��@JS�@J�*@KqS@K�@@Lk.@L�@Mk�@NB@N�m@O&�@O��@P9X@Q��@R�7@T�@Un�@V��@XD�@Y�C@ZĜ@\�@]oF@^��@`�@av@b�>@d$/@e�|@f��@h""@i��@j��@l�@m�@n׹@p&�@qj@r�o@t6@uoF@v�^@x&�@xYn@x��@x�@y �@yhr@y�yG�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�@ �G�O�@ �G�O�@ �G�O�G�O�@ �@ jG�O�@ @ �@ %@ �@ 1@ 	�@ 
=@ �@ �@ @ b@ �@ {@ �@ B@ �@ [@  @ !s@ $�@ '�@ *S@ -@ 0x@ 33@ 6�@ 8�@ <@ ?}@ C�@ FQ@ I�@ Lu@ P�@ S�@ Wb@ Z@ ]�@ `�@ c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A؋DA؏\A؅A؍PAؕ�Aؙ�Aؙ�Aؙ�Aء�Aأ�Aا�Aا�Aا�AجAخAذ!Aز-AضFAخA��A�ZA��`Aԝ�A�S�A��A�|�A�Aҩ�A�9XA͗�Aɣ�A�1'Aũ�A�M�A\A��hA�5?A�p�A���A���A���A�hsA��7A��uA�bNA�bNA�"�A�7LA� �A��FA�v�A�1'A�JA��/A��wA�`BA���A���A�p�A�bA�A�  A���A��A��A�|�A�M�A�M�A��wA��A��A��mA��TA��7A��RA���A�;dA���A�oA���A���A���A�S�A�"�A��A��jA�XA�1A�\)A��A�ffA��A���A�p�A���A���A�`BA�%A�t�A��PA��A��A���A�-A�1'A��/A�K�A�
=A��PA��A��A�t�A�A��!A���A�{A��A�C�A�9XA���A��A�ĜA��#A���A��A��uA�Q�A�VA�v�A�(�A�AA|�jAz�Ax��Aw&�Au�mAt�yAr�!Aq�7Ap�HAodZAo�Am�
AkAj5?Af$�Ac�Ae��AfE�Ad��Ac�Aa��A_�;A^�A^ �A[��AVjAT��ASK�AR�!AQ�
AQC�AO�PAMl�AJȴAI��AI7LAH��AG��AE�-ABQ�A?��A?x�A?"�A?%A>�`A>=qA=�A;�hA4�\A2�jA2r�A25?A0�uA.��A-��A,1'A*�A)hsA'p�A&5?A%33A$ �A"�A �HA A�TA��A\)Ar�A��A\)AAr�A;dAA��A�9Av�A{A�#A��At�A��A9XA��A�\A"�A��A�9A��AZA�+A	|�A�DA{AĜA�
A�9A7LA�#AdZA;dA �9A 9XA b@��F@�33@���@�@�O�@���@�@�^5@��@��@�P@�|�@�K�@��-@�ƨ@�ȴ@���@�Ĝ@�(�@�v�@�7@��@�u@�t�@�+@���@�=q@�v�@�{@��@\@�dZ@��@��@���@�S�@��@���@�Q�@��!@���@���@���@��@��T@�b@��-@��`@�1'@�\)@��@���@� �@��@�hs@���@�ȴ@��^@���@�/@���@�C�@�-@�M�@��T@�z�@��@�l�@��!@�-@���@�1@�+@��@��@~�@|��@{@w|�@u`B@tI�@s"�@q�7@o�P@nv�@m�h@k�
@g��@fȴ@e��@c@bJ@a��@_�;@^{@\��@[t�@Yx�@X �@Vv�@UO�@Sƨ@QG�@PbN@O+@Mp�@Lz�@K�m@I��@I%@G�@F5?@E�h@D(�@Ct�@A�#@@��@?�P@?;d@=�-@=/@;C�@9�#@8�9@7�;@7�P@7;d@65?@5p�@5�@41@2�!@1��@0bN@/�w@.�@.{@,��@,9X@,1@*�\@)x�@(��@'�@%�T@$��@#ƨ@#@!hs@ r�@|�@E�@�-@p�@I�@�
@o@=q@G�@A�@��@ff@`B@z�@��@�@@��@hs@�9@A�@K�@�R@V@@��@1@33@
�\@
^5@	�@	x�@�`@1'@|�@�y@5?@�@��@��@Z@�
@dZ@��@^5@-@��@�7@&�@ Q�?�;d?�5??��H?���?���?��j?�!?�%?�h?�D?��m?���?��?�9?�ff?�j?�S�?���?�7?�A�?�v�?ݑh?ۅ?ٺ^?�1'?�ff?��T?�?}?��?��?�M�?У�?ϝ�?���?�/?�I�?˅?�?�=q?�7L?���?��?�+?Ł?��
?�o?�n�?���?�Ĝ?� �?���?�v�?�p�?�1?�(�?��H?�x�?���?���?���?���?�7L?�x�?���?�~�?�?�dZ?��m?��D?�/?���?�v�?�;d?�;d?�\)?���?���?��w?��;A؋DA؃A؃A؃A؉7A؋DA؉7A؇+A؋DA؇+A؋DA؉7A؏\A؏\A؏\A؏\A؍PA؍PA؍PA؏\AؑhAؑhA؏\A؏\A؍PA؋DA؋DA؇+A؅A؅A؇+A؋DA؍PA؍PA؋DAؗ�A؝�A؛�A؛�Aؗ�Aؙ�Aؗ�Aؙ�Aؙ�Aؙ�A؟�Aأ�Aأ�Aإ�Aا�Aا�Aإ�Aة�Aإ�Aإ�Aة�Aإ�Aا�Aة�Aة�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A؋DA؏\A؅A؍PAؕ�Aؙ�Aؙ�Aؙ�Aء�Aأ�Aا�Aا�Aا�AجAخAذ!Aز-AضFAخA��A�ZA��`Aԝ�A�S�A��A�|�A�Aҩ�A�9XA͗�Aɣ�A�1'Aũ�A�M�A\A��hA�5?A�p�A���A���A���A�hsA��7A��uA�bNA�bNA�"�A�7LA� �A��FA�v�A�1'A�JA��/A��wA�`BA���A���A�p�A�bA�A�  A���A��A��A�|�A�M�A�M�A��wA��A��A��mA��TA��7A��RA���A�;dA���A�oA���A���A���A�S�A�"�A��A��jA�XA�1A�\)A��A�ffA��A���A�p�A���A���A�`BA�%A�t�A��PA��A��A���A�-A�1'A��/A�K�A�
=A��PA��A��A�t�A�A��!A���A�{A��A�C�A�9XA���A��A�ĜA��#A���A��A��uA�Q�A�VA�v�A�(�A�AA|�jAz�Ax��Aw&�Au�mAt�yAr�!Aq�7Ap�HAodZAo�Am�
AkAj5?Af$�Ac�Ae��AfE�Ad��Ac�Aa��A_�;A^�A^ �A[��AVjAT��ASK�AR�!AQ�
AQC�AO�PAMl�AJȴAI��AI7LAH��AG��AE�-ABQ�A?��A?x�A?"�A?%A>�`A>=qA=�A;�hA4�\A2�jA2r�A25?A0�uA.��A-��A,1'A*�A)hsA'p�A&5?A%33A$ �A"�A �HA A�TA��A\)Ar�A��A\)AAr�A;dAA��A�9Av�A{A�#A��At�A��A9XA��A�\A"�A��A�9A��AZA�+A	|�A�DA{AĜA�
A�9A7LA�#AdZA;dA �9A 9XA b@��F@�33@���@�@�O�@���@�@�^5@��@��@�P@�|�@�K�@��-@�ƨ@�ȴ@���@�Ĝ@�(�@�v�@�7@��@�u@�t�@�+@���@�=q@�v�@�{@��@\@�dZ@��@��@���@�S�@��@���@�Q�@��!@���@���@���@��@��T@�b@��-@��`@�1'@�\)@��@���@� �@��@�hs@���@�ȴ@��^@���@�/@���@�C�@�-@�M�@��T@�z�@��@�l�@��!@�-@���@�1@�+@��@��@~�@|��@{@w|�@u`B@tI�@s"�@q�7@o�P@nv�@m�h@k�
@g��@fȴ@e��@c@bJ@a��@_�;@^{@\��@[t�@Yx�@X �@Vv�@UO�@Sƨ@QG�@PbN@O+@Mp�@Lz�@K�m@I��@I%@G�@F5?@E�h@D(�@Ct�@A�#@@��@?�P@?;d@=�-@=/@;C�@9�#@8�9@7�;@7�P@7;d@65?@5p�@5�@41@2�!@1��@0bN@/�w@.�@.{@,��@,9X@,1@*�\@)x�@(��@'�@%�T@$��@#ƨ@#@!hs@ r�@|�@E�@�-@p�@I�@�
@o@=q@G�@A�@��@ff@`B@z�@��@�@@��@hs@�9@A�@K�@�R@V@@��@1@33@
�\@
^5@	�@	x�@�`@1'@|�@�y@5?@�@��@��@Z@�
@dZ@��@^5@-@��@�7@&�@ Q�?�;d?�5??��H?���?���?��j?�!?�%?�h?�D?��m?���?��?�9?�ff?�j?�S�?���?�7?�A�?�v�?ݑh?ۅ?ٺ^?�1'?�ff?��T?�?}?��?��?�M�?У�?ϝ�?���?�/?�I�?˅?�?�=q?�7L?���?��?�+?Ł?��
?�o?�n�?���?�Ĝ?� �?���?�v�?�p�?�1?�(�?��H?�x�?���?���?���?���?�7L?�x�?���?�~�?�?�dZ?��m?��D?�/?���?�v�?�;d?�;d?�\)?���?���?��w?��;A؋DA؃A؃A؃A؉7A؋DA؉7A؇+A؋DA؇+A؋DA؉7A؏\A؏\A؏\A؏\A؍PA؍PA؍PA؏\AؑhAؑhA؏\A؏\A؍PA؋DA؋DA؇+A؅A؅A؇+A؋DA؍PA؍PA؋DAؗ�A؝�A؛�A؛�Aؗ�Aؙ�Aؗ�Aؙ�Aؙ�Aؙ�A؟�Aأ�Aأ�Aإ�Aا�Aا�Aإ�Aة�Aإ�Aإ�Aة�Aإ�Aا�Aة�Aة�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B7LBffBl�Bw�B}�B�B�JB�{B�B��B��B��B�5B�ZB�B��B��B��BPB{B �B/B9XBQ�Be`Bp�Bn�BS�BZBo�Bo�Bp�Bq�Bp�Bq�Br�By�B�+B�=B�PB�JB�DB�PB�bB�oB�VB�PB�DB�+B}�Bx�Bz�By�Bu�Br�BjBgmB`BB_;B^5B]/B]/BYBZBYBVBT�BP�BI�B<jB:^B6FB6FB8RB8RB<jB:^B2-B)�B �B�BhB��B�#B��B��Bt�BI�B+B!�B�B{BDBB
�B
�sB
��B
�?B
��B
��B
�hB
�B
r�B
bNB
XB
N�B
L�B
F�B
=qB
9XB
8RB
5?B
"�B
�B
JB
+B
  B	��B	�`B	�;B	�B	��B	��B	ĜB	�jB	�B	�JB	�\B	ǮB	��B	ŢB	�dB	�!B	��B	��B	��B	z�B	S�B	K�B	D�B	=qB	7LB	.B	!�B	JB	B��B��B��B�B�HB��B��BȴBĜBB��B�jB�LB�B�{B��B�{B�uB�PB�=B�B{�Bv�Bq�BjBgmBe`BaHB^5BYBXBW
BR�BR�BQ�BK�BQ�BO�BM�BK�BK�BK�BI�BH�BH�BG�BG�BF�BF�BF�BE�BB�BC�BC�BC�BB�BC�B=qBA�B>wB<jB:^B:^B7LB:^B9XB9XB8RB7LB8RB8RB8RB8RB8RB;dB:^B:^B;dB:^B9XB;dB:^B9XB9XB9XB<jB>wB?}BC�BC�BA�BD�BD�BD�BA�BL�BT�BW
B]/B^5B^5B`BBk�B{�B|�B�=B�uB��B��B�3B�}B��B�B�fB�B��B	+B	�B	�B	%�B	0!B	9XB	E�B	J�B	R�B	^5B	�B	��B	��B	��B	�B	�9B	�9B	�^B	ŢB	ȴB	��B	��B	��B	��B	��B	�#B	�5B	�HB	�`B	�mB	�B	�B	��B	��B
B
B
B
B
1B
	7B

=B

=B
JB
DB
DB
\B
uB
�B
�B
�B
!�B
!�B
#�B
%�B
(�B
)�B
)�B
-B
.B
/B
0!B
1'B
2-B
5?B
5?B
8RB
8RB
:^B
9XB
;dB
=qB
=qB
>wB
?}B
A�B
B�B
D�B
E�B
F�B
G�B
F�B
G�B
H�B
J�B
I�B
J�B
K�B
K�B
M�B
O�B
P�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
VB
XB
XB
YB
YB
[#B
]/B
^5B
_;B
_;B
`BB
bNB
bNB
cTB
e`B
e`B
e`B
gmB
iyB
iyB
jB
jB
k�B
k�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
o�B
q�B
r�B
s�B
s�B
s�B
t�B
u�B
v�B
w�B
w�B
w�B
y�B
z�B
y�B
z�B
z�B
{�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�1B
�7B
�=B
�JB
�JB
�PB
�VB
�VB
�\B
�bB
�oB
�oB
�uB
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
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�?B
�9B
�FB
�FB
�RB
�RB
�LB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        B
�B
�B
�~B
�~B
�xB
�lB
�sB
�yB
��B
��B
�B
�{B
��B
��B
��B
�B
�B
�B
�B
��B7.BfHBlnBw�B}�B�B�/B�aB��B˭BʧB��B�B�BB�B��B��B��B:BeB �B/B9DBQ�BeMBp�Bn�BS�BZBo�Bo�Bp�Bq�Bp�Bq�Br�By�B�B�2B�EB�@B�:B�GB�ZB�gB�OB�IB�=B�%B}�Bx�Bz�By�Bu�Br�Bj}BgkB`AB_:B^5B]0B]0BYBZBYBVBUBP�BI�B<pB:dB6LB6MB8ZB8ZB<sB:gB27B*B �B�BtB��B�/B��B��Bt�BI�B+B!�B�B�BTB/B
��B
�B
�B
�QB
��B
��B
�{B
�-B
r�B
bcB
X%B
N�B
L�B
F�B
=�B
9pB
8jB
5XB
"�B
�B
dB
FB
 B	��B	�|B	�WB	�:B	��B	��B	ĻB	��B	�3B	�jB	�|B	��B	��B	��B	��B	�DB	�B	��B	��B	{B	TB	K�B	D�B	=�B	7sB	.;B	!�B	rB	GB�B��B��B��B�sB�)B��B��B��B¼B��B��B�{B�=B��B��B��B��B��B�nB�DB|Bv�Bq�Bj�Bg�Be�Ba}B^jBYMBXFBWABS)BS*BR$BL BR%BPBNBLBLBLBI�BH�BH�BG�BG�BF�BF�BF�BE�BB�BC�BC�BC�BB�BC�B=�BA�B>�B<�B:�B:�B7�B:�B9�B9�B8�B7�B8�B8�B8�B8�B8�B;�B:�B:�B;�B:�B9�B;�B:�B9�B9�B9�B<�B>�B?�BC�BC�BA�BD�BD�BD�BA�BM%BUVBWcB]�B^�B^�B`�Bk�B|SB}]B��B��B�B�AB��B��B�dBٟB��B�,B�xB	�B	1B	 WB	&B	0�B	9�B	FGB	KiB	S�B	^�B	��B	�5B	�nB	��B	��B	��B	��B	�#B	�jB	�B	ˏB	ΤB	гB	��B	��B	� B	�B	�+B	�GB	�VB	�}B	�B	��B	��B
�B
B
B
B
	5B

>B
GB
JB
ZB
WB
ZB
uB
�B
�B
�B
 �B
"�B
"�B
%B
'B
**B
+3B
+6B
.JB
/SB
0]B
1fB
2oB
3wB
6�B
6�B
9�B
9�B
;�B
:�B
<�B
>�B
>�B
?�B
@�B
B�B
C�B
FB
GB
HB
I'B
H$B
I-B
J5B
LEB
KAB
LKB
MSB
MVB
OeB
QsB
R|B
S�B
T�B
T�B
T�B
U�B
V�B
V�B
W�B
Y�B
Y�B
Z�B
Z�B
\�B
^�B
_�B
`�B
`�B
bB
dB
dB
e B
g/B
g1B
g4B
iCB
kQB
kTB
l\B
l_B
mgB
miB
oxB
ozB
p�B
q�B
q�B
r�B
r�B
q�B
s�B
t�B
u�B
u�B
u�B
v�B
w�B
x�B
y�B
y�B
y�B
{�B
|�B
{�B
|�B
}B
~B
B
B
�B
�"B
�%B
�-B
�2B
�>B
�IB
�TB
�iB
�tB
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
�B
�B
�B
�,B
�0B
�CB
�PB
�VB
�bB
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
�B
�B
�B
�%B
�2B
�MB
�bB
�qB
��B
��B
��B
��B
��B
��B
�B
�)B
�1B
�NB
�]B
�yB
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�B
�'B
�<B
�EB
�TB
�eB
�gB
�jB
�nB
�vB
�yB
�|B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�~B
�B
�~B
�~B
�~B
�~B
�B
�~B
�~B
�~B
�~B
�xB
�rB
�lB
�lB
�sB
�sB
�sB
�yB
�yB
�yB
��B
��B
��B
��B
�B
��B
�B
�{B
��B
�{B
�{B
�{B
�B
�{B
�|G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810181702002021061413554620210614135546202106171313142021061713131420210617131314201810181702002021061413554620210614135546202106171313142021061713131420210617131314PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018101817020020181018170200  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101817020020181018170200QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101817020020181018170200QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150620210617131506IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                