CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-28T07:02:50Z creation      
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
_FillValue                 8  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  f    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ¨   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ׸   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   @   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       $   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � p      � pArgo profile    3.1 1.2 19500101000000  20180828070250  20210722160152  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�z���0@�z���011  @�z����@�z����@6&DR?g�@6&DR?g��c�o�5�J�c�o�5�J11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
AB  AA  AA  ?L��?�33@@  @�ff@�  @���@�  A   A��A&ffAC33A`  A�  A�  A�  A���A���A���A���A���B ffB��B  BffB ffB(ffB0ffB8  B@ffBHffBPffBXffB_��Bh  Bp  Bx  B�33B�33B�ffB�ffB�33B�  B�  B�33B�ffB�ffB�  B�33B�33B�  B�  B�  B�33B�33B�  B�ffB�  Bә�B�  B�  B�33B�  B癚B���B�33B���B���B�  C �C�C�fC33C  C	�fC�fC  C33C�C  C�fC�fC�C�fC��C �C"L�C$33C&�C(  C)�fC,L�C.33C0  C2L�C433C6  C8  C9�fC<33C>33C@  CBL�CD�CF  CH33CJ�CL  CN33CP33CR�CT�CV  CX  CY�fC[�fC]�fC`33Cb33Cd33Cf33Ch�Cj�Cl�Cn33Cp�Cr  Cs�fCv33Cx33Cz�C|�C~  C�fC��fC��C��C��3C��C��C��3C��C��C��C��C�  C�  C��3C��C��C��3C��C��C��3C��C��C��3C��C�&fC��C��3C�  C��C�&fC��C��3C�  C�  C��C��C�&fC�&fC�33C��C��C�33C��C��fC��3C�  C��C�  C��C��C��C��fC�  C��C�  C��fC��fC��3C�  C��C�  C�ٚC��fC��3C��C��C��C��3C�  C��C��C��3C��C�  C��fC��C�  C��fC��C��3C��3C��C��C�  C�  C��fC��C�  C��3C��C��C��3C��C��C��C�  C��3C��C��C��3C��C��C�  C�&fC��C��C�  C�  C��C��C�  C��C��C��3C��C�  C��fC��C�  C��fC��C��fC�&fC�@ C��3C��C�&fD 3D � D ��D�fD3DffD��D
fD� DfDl�D�3D�3D@ D� D�fD!s3D$FfD'�D)�3D,�fD/� D2FfD5  D7� D:s3D=3D?��DBS3DDٚDGffDI��DL�fDO3DQ�fDT@ DV��DY�3D\9�D^�3DaffDc��Df�fDi3Dk�3Dn  Dp` Dr�fDu9�Dw�3Dz�D|,�D~� D���D���D�	�D�P D��3D��fD�3D�\�D���D�� D�6fD��3D���D� D�c3D��fD���D�@ D��fD���D�FfD��fD�3D�\�D��fD�3D�\�D�� D�3D�` D���D�fD�` D���D� D�ffD���D�3D�i�D��fD�  D�@ D��3D��fD��fD�3D�9�D�l�D��fD��3D��3D� D�9�D�\�DŐ D�� D��D�  D�S3Dˀ D̬�D��fD�fD�0 D�\�D�|�DӜ�DԹ�D��3D��D�	�D�#3D�6fD�L�D�\�D�s3Dހ Dߐ D�3DṚD��3D�� D�  D��D�6fD�S3D�i�D�|�D�fD� D��3D���D�3D�  D�9�D�Y�D�vfD���D�� D���D��3D� D�3D�0 D�P D�i�D��fE P E � EnfE� E�fE�E��E8 EɚE\�E�Ey�E3E��E+3E�fE	D�E
c3E E  E9�E� E� E	�E��E��E� EfE��E��E  E�fEd�E��EɚE!H E"FfE#��E$�3E&a�E'q�E(� E*3E+( E,,�E-�fE.��E03E1{3E2k3E3�3E5�E6d�E7�fE8�E:( E;[3E<��E?� EB�3EE�fEH�EL( EO$�ER�fEU�fEX��E[�fE^�3EbfEe3Ehx Ekl�EnX EqɚEtɚEx�Ez��E~K3E���E�H�E���E�\�E�� E���E��fE�� E�-�E��3E�0 E�ŚE�T�E��3E�� E�W3E���E��fE�(�E��fE��fE�3E�a�E��fE�3E�d E��3E� E�A�E�� E��fE�:fE���E�� E�$ E�x E��fE�+3E�i�E���?333?   ?333?   ?��?��?��?��?��?333?��?333?L��?L��?�  ?���?�ff?�  ?���?�33@��@&ff@333@Fff@Y��@s33@�ff@�  @���@�ff@�33@���@ə�@ٙ�@�ff@�33A   AffA��A��A33A#33A)��A0  A8  A@  AFffAL��AS33A\��Ac33Ai��As33Ax  A���A�  A�33A�ffA�33A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441411411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?�ff@��@`  @�ff@�  @���@�  A  A��A.ffAK33Ah  A�  A�  A�  A���A���A���A���A���BffB
��B  BffB"ffB*ffB2ffB:  BBffBJffBRffBZffBa��Bj  Br  Bz  B�33B�33B�ffB�ffB�33B�  B�  B�33B�ffB�ffB�  B�33B�33B�  B�  B�  B�33B�33B�  B�ffB�  Bԙ�B�  B�  B�33B�  B虚B���B�33B���B���B�  C ��C��CffC�3C� C
ffCffC� C�3C��C� CffCffC��CffCL�C ��C"��C$�3C&��C(� C*ffC,��C.�3C0� C2��C4�3C6� C8� C:ffC<�3C>�3C@� CB��CD��CF� CH�3CJ��CL� CN�3CP�3CR��CT��CV� CX� CZffC\ffC^ffC`�3Cb�3Cd�3Cf�3Ch��Cj��Cl��Cn�3Cp��Cr� CtffCv�3Cx�3Cz��C|��C~� C�33C�&fC�L�C�L�C�33C�Y�C�L�C�33C�Y�C�L�C�L�C�L�C�@ C�@ C�33C�Y�C�L�C�33C�Y�C�L�C�33C�Y�C�L�C�33C�Y�C�ffC�Y�C�33C�@ C�Y�C�ffC�L�C�33C�@ C�@ C�L�C�Y�C�ffC�ffC�s3C�L�C�L�C�s3C�Y�C�&fC�33C�@ C�L�C�@ C�Y�C�Y�C�L�C�&fC�@ C�L�C�@ C�&fC�&fC�33C�@ C�L�C�@ C��C�&fC�33C�L�C�Y�C�L�C�33C�@ C�Y�C�L�C�33C�L�C�@ C�&fC�L�C�@ C�&fC�L�C�33C�33C�L�C�Y�C�@ C�@ C�&fC�L�C�@ C�33C�Y�C�L�C�33C�Y�C�Y�C�L�C�@ C�33C�L�C�L�C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�@ C�Y�C�L�C�@ C�Y�C�Y�C�33C�Y�C�@ C�&fC�L�C�@ C�&fC�L�C�&fC�ffC�� C�33C�Y�C�ffD 33D � D�D�fD33D�fD��D
&fD� D&fD��D3D�3D` D  D�fD!�3D$ffD',�D)�3D,�fD/� D2ffD5  D7� D:�3D=33D?��DBs3DD��DG�fDJ�DL�fDO33DQ�fDT` DW�DY�3D\Y�D^�3Da�fDd�Df�fDi33Dk�3Dn  Dp� Dr�fDuY�Dw�3Dz9�D|L�D~� D���D���D��D�` D��3D��fD�#3D�l�D���D�  D�FfD��3D���D�  D�s3D��fD�	�D�P D��fD���D�VfD��fD�3D�l�D��fD�#3D�l�D�� D�3D�p D�ɚD�fD�p D���D�  D�vfD�ɚD�#3D�y�D��fD� D�P D��3D��fD��fD�#3D�I�D�|�D��fD��3D��3D�  D�I�D�l�DŠ D�� D���D�0 D�c3Dː D̼�D��fD�fD�@ D�l�DҌ�DӬ�D�ɚD��3D���D��D�33D�FfD�\�D�l�D݃3Dސ Dߠ D�3D�ɚD��3D�� D� D�,�D�FfD�c3D�y�D��D�fD�� D��3D���D�3D�0 D�I�D�i�D�fD���D�� D���D�3D�  D�#3D�@ D�` D�y�D��fE X E � EvfE  E�fE�E��E@ EњEd�E�E��E3E��E33E�fE	L�E
k3E E( EA�E� E  E�E��E��E� EfE��E��E E�fEl�E��EњE!P E"NfE#��E$�3E&i�E'y�E(� E*#3E+0 E,4�E-�fE.��E03E1�3E2s3E3�3E5�E6l�E7�fE8�E:0 E;c3E<��E?� EB�3EE�fEH�EL0 EO,�ER�fEU�fEX��E[�fE^�3EbfEe3Eh� Ekt�En` EqњEtњEx�E{�E~S3E���E�L�E���E�`�E�� E���E��fE�� E�1�E��3E�4 E�ɚE�X�E��3E�� E�[3E���E��fE�,�E��fE��fE�#3E�e�E��fE�3E�h E��3E� E�E�E�� E��fE�>fE���E�� E�( E�| E��fE�/3E�m�E���G�O�?�  G�O�?�  G�O�G�O�G�O�G�O�?���G�O�?���?���G�O�?�ff?�  ?���?�ff@   @ff@��@,��@Fff@S33@fff@y��@���@�ff@�  @���@�ff@�33@���@ٙ�@陚@�ffA��A  AffA��A��A#33A+33A1��A8  A@  AH  ANffAT��A[33Ad��Ak33Aq��A{33A�  A���A�  A�33A�ffA�33A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441411411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ v@ �@ {@ �@ ""@ (G@ /�@ 6�@ >@ F�@ SI@ _�@ m:@ z�@ ��@ ��@ ��@ �-@ ��@ �|@ �#@ �y@ ��@@�@g@-@:@H]@V@c�@qS@}�@��@��@�A@��@�>@є@�;@�4@�,@�@*@#�@1'@=q@K�@Yn@ff@t@��@�@��@��@��@��@�C@�H@��@�E@
=@�@$�@3�@@,@M$@\)@j@x&@�p@�#@�m@�f@�@�c@�h@�`@�Y@�Q@�@�@(G@5?@D�@S�@`�@m�@z�@��@�<@�5@�~@�2@�*@�t@�@�@�@o@�@.l@:�@G�@V�@c�@p�@�@�P@�H@��@��@@ψ@�/@��@��@1@�@#�@0x@>@K�@Z@g@t@�@��@�a@�Y@�@��@��@��@�@�E@	�@B@&;@2�@B8@O0@\�@j@ww@�@�@��@��@�@��@׹@�@�@ �@�@�@+@7�@C�@Q�@`�@oF@{�@��@�0@��@�-@��@��@܀@��@�q@	@	�@	 @	+@	9X@	G�@	V@	b�@	r@	�@	��@	�<@	�A@	��@	@	��@	܀@	��@	�,@
�@
{@
 @
.l@
<�@
K�@
Z@
g@
s_@
��@
��@
��@
��@
�@
��@
�C@
��@
��@
�9@
�@6@$�@3�@B8@N�@\)@hs@x&@�@�@��@��@�@��@�h@�`@�Y@�Q@V@�@(G@7�@D�@Q�@a�@n�@{�@��@�0@�5@�-@�&@�*@��@�m@� @j@�@g@,`@8�@H]@S�@e	@t@}�@�P@��@�M@�R@��@��@��@��@uk@�@  @A�@�@�@�@X�@�m@�4@8�@��@є@[@j@�R@@N�@��@�T@+@r@�^@�Q@D�@�D@��@�@\�@��@��@5?@}�@Ĝ@
�@Q=@��@܀@ �@b�@��@�`@(G@k�@�f@�@(�@l�@��@�e@:@~�@��@�@M�@��@�t@ @g@�f@�Y@:�@�@�@ �@ V�@ ��@ �y@!4�@!~�@!ȴ@"o@"\�@"�(@"�@#3�@#~K@#�@$@$X�@$�(@$�@%4�@%|�@%ƨ@&�@&V�@&�@&�H@'&;@'g�@'��@'��@('�@(i!@(��@(�`@)&;@)ff@)��@)�@*%�@*ff@*��@*�@+)�@+i�@+��@+�y@,*S@,i�@,��@,�m@-$�@-a�@-��@-�@.�@.R�@.�P@.ȴ@/�@/>@/ww@/�~@/�4@0'�@0`A@0�@0�t@16@1SI@1�@1�o@2%@2B8@2~K@2�j@2�~@33�@3p�@3��@3�(@4&�@4e	@4�m@4�/@5�@5X�@5�@5��@6
=@6FQ@6�@6�&@6��@79X@7t@7��@7�4@8+@8i�@8��@8�@9"�@9`B@9�a@9܀@:B@:T�@:�i@;�@;�&@<6�@<��@=b�@=�t@>O1@>�9@?hs@?�
@@v�@A�@A�d@B""@B�C@C'�@CĜ@D-@D�7@E<�@E�T@FSI@F�E@GqS@G�@H�#@I�@Iv@J�@J��@K @K��@L @L�F@MB�@M�C@N^�@N�`@Om:@O�L@Pm�@Q��@SB@To�@U��@WO@Xa�@Y�/@[!s@\��@]��@_ @`o�@a�@c/�@dr�@e�~@g)�@hqS@i�@k�@l�@m��@o-@pg@q�|@s
=@t{�@u��@w%�@xx�@y�J@{
=@|dZ@}�@}�}@~6�@~��@~є@I@H]@�y@��@�P@�)�@�T�@�pL@���@��@�ߓ@��q@�O@�@,@�`�@���@��"@��D@��@��@�4�@�O1@�p�G�O�@ jG�O�@ jG�O�G�O�G�O�G�O�@ G�O�@ @ �G�O�@ v@ �@ �@ �@ 
=@ 
�@ �@ @ �@ @ *@ 6@ �@ �@ �@  �@ #�@ &;@ (G@ +@ .l@ 1'@ 3�@ 6�@ 9X@ <@ ?}@ B8@ E�@ H]@ K@ N�@ Q�@ T�@ Wb@ Z@ ^5@ `�@ c�@ g�@ i�@ m�@ p�@ s_@ v@ z3@ |?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��/A��HA��TA��TA��`A��`A��TA؁A�ȴA��mA��yAղ-A�^5A�ƨA�"�AӰ!AҍPAЍPA��A��TA��;A��#AΉ7A�VA���A�(�A�7LA���A�|�A�$�A���A��DA���A���A���A�O�A���A�I�A�oA��mA��A�I�A�
=A��A��DA��A���A�I�A��/A�O�A�9XA�%A��HA���A�^5A��A�\)A��A���A��;A�1A�~�A���A�jA�I�A�|�A��FA���A��TA�G�A��RA�?}A�M�A��wA��mA�(�A���A���A�G�A��-A�p�A���A�ȴA�A�x�A���A��9A�  A� �A�l�A��A���A�A�A�v�A���A���A�G�A��A�%A���A�-A�S�A���A}�FA|I�A{�AyƨAw�PAt-As�Ar�9ArbNAr$�Aq;dAoO�AnE�Al�AjbNAi�#AidZAh  Agt�Af��Af�Ae�AdI�Ab�A`v�A^��A[��AY��AXz�AW�AV��AU��AT�/AS�7AR��AQƨAP��AO�FAN��AN$�AMG�AK�#AKK�AJ~�AI`BAHAF��AE�-AC�
AB�!AB�AA�PAA33A@�A??}A;��A9�A9��A9O�A81'A6��A4��A3?}A3"�A2�A1�TA1��A1�PA1p�A1dZA1S�A1�A/��A-��A-t�A-�hA,�HA,ZA,=qA+�;A+&�A*ȴA*�A*I�A)�;A)�7A);dA(�A(�A(bA$jA#t�A#7LA#VA ĜA�hA�AjA��A��AE�A  AG�A�A�yA �A�#A�AC�A�9A�
A�AoAdZA�;A�AXA�/A��A�7AVA�A
JA7LAjA�#A�PA�AdZA��A��A ��@��y@�X@�|�@���@��@�I�@�{@��u@�=q@��@��@��m@�+@���@���@�n�@�j@�V@�x�@��@�I�@�=q@�+@�x�@ם�@�;d@�`B@�r�@Ӿw@�v�@�@�33@�
=@�%@�
=@�@��H@�@�1'@�J@���@���@��+@��u@���@�K�@�M�@��@�@�x�@���@�\)@�v�@�$�@�A�@�33@�^5@�@�;d@��\@�@��j@���@�$�@�1'@�t�@���@���@��@�V@���@�bN@� �@�  @��@���@���@�%@�j@�P@~��@}/@z�@w�@uV@q�7@p�9@lj@hĜ@c�@bJ@` �@^�@]��@\z�@Z��@YX@XQ�@U`B@SS�@RJ@Q��@PQ�@O�P@Nȴ@K33@Jn�@H��@F��@E?}@Dz�@B�H@Bn�@?�@<�D@<Z@:�!@8�@7��@6��@4�j@4(�@3�@2�@1hs@/��@.5?@-O�@,��@,9X@+�F@+@*^5@(��@'�w@%�h@$(�@#�F@"��@ ��@ Q�@��@v�@/@��@�m@t�@@=q@�@bN@�P@ȴ@$�@�@��@z�@o@�@��@%@  @�@�y@V@$�@?}@��@
��@
�@	hs@��@ �@�;@��@\)@ff@�@I�@(�@t�@�!@~�@��@%@ bN?�|�?��?�{?��h?��?�1?�1?��m?�?�X?�7L?��?�b?�K�?�E�?���?��?��?�w?��?�v�?�ƨ?ꟾ?���?�b?�ȴ?��T?�?}?㕁?�!?�M�?�7?�bN?�;d?ݲ-?���?�j?�"�?��?׮?�K�?�
=?�ff?�?�z�?�9X?ӕ�?�-?Ͼw?��?Ͳ-?̬?��m?�ƨ?���?�7L?ȴ9?�$�?�9X?��7?���?�5??�O�?�1?�ƨ?�"�?�~�?�^5?��#?��^?��^?���?�=q?�^5?���?�C�?�"�?�ƨ?�(�?��?�/?��?�v�?���?���?�A�?��?�Ĝ?��7?���?���?�J?�-?�-?�M�?�n�?°!?���?��?�o?�33?�t�?Õ�?öF?��
?���?��?�9X?�z�?ě�?ļj?��/?��?�?}?Ł?š�A���A���A��A���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A��
A��
A��
A��#A��TA��TA��HA��HA��;A��HA��TA��TA��TA��TA��`A��`A��TA��`A��`A��`A��;Aذ!A؇+A�l�A�\)A�=qA��HAבhA�t�A�S�A�bA���A�t�A��A��A��/A���A���A���Aմ9AծG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A���A���A��/A��HA��TA��TA��`A��`A��TA؁A�ȴA��mA��yAղ-A�^5A�ƨA�"�AӰ!AҍPAЍPA��A��TA��;A��#AΉ7A�VA���A�(�A�7LA���A�|�A�$�A���A��DA���A���A���A�O�A���A�I�A�oA��mA��A�I�A�
=A��A��DA��A���A�I�A��/A�O�A�9XA�%A��HA���A�^5A��A�\)A��A���A��;A�1A�~�A���A�jA�I�A�|�A��FA���A��TA�G�A��RA�?}A�M�A��wA��mA�(�A���A���A�G�A��-A�p�A���A�ȴA�A�x�A���A��9A�  A� �A�l�A��A���A�A�A�v�A���A���A�G�A��A�%A���A�-A�S�A���A}�FA|I�A{�AyƨAw�PAt-As�Ar�9ArbNAr$�Aq;dAoO�AnE�Al�AjbNAi�#AidZAh  Agt�Af��Af�Ae�AdI�Ab�A`v�A^��A[��AY��AXz�AW�AV��AU��AT�/AS�7AR��AQƨAP��AO�FAN��AN$�AMG�AK�#AKK�AJ~�AI`BAHAF��AE�-AC�
AB�!AB�AA�PAA33A@�A??}A;��A9�A9��A9O�A81'A6��A4��A3?}A3"�A2�A1�TA1��A1�PA1p�A1dZA1S�A1�A/��A-��A-t�A-�hA,�HA,ZA,=qA+�;A+&�A*ȴA*�A*I�A)�;A)�7A);dA(�A(�A(bA$jA#t�A#7LA#VA ĜA�hA�AjA��A��AE�A  AG�A�A�yA �A�#A�AC�A�9A�
A�AoAdZA�;A�AXA�/A��A�7AVA�A
JA7LAjA�#A�PA�AdZA��A��A ��@��y@�X@�|�@���@��@�I�@�{@��u@�=q@��@��@��m@�+@���@���@�n�@�j@�V@�x�@��@�I�@�=q@�+@�x�@ם�@�;d@�`B@�r�@Ӿw@�v�@�@�33@�
=@�%@�
=@�@��H@�@�1'@�J@���@���@��+@��u@���@�K�@�M�@��@�@�x�@���@�\)@�v�@�$�@�A�@�33@�^5@�@�;d@��\@�@��j@���@�$�@�1'@�t�@���@���@��@�V@���@�bN@� �@�  @��@���@���@�%@�j@�P@~��@}/@z�@w�@uV@q�7@p�9@lj@hĜ@c�@bJ@` �@^�@]��@\z�@Z��@YX@XQ�@U`B@SS�@RJ@Q��@PQ�@O�P@Nȴ@K33@Jn�@H��@F��@E?}@Dz�@B�H@Bn�@?�@<�D@<Z@:�!@8�@7��@6��@4�j@4(�@3�@2�@1hs@/��@.5?@-O�@,��@,9X@+�F@+@*^5@(��@'�w@%�h@$(�@#�F@"��@ ��@ Q�@��@v�@/@��@�m@t�@@=q@�@bN@�P@ȴ@$�@�@��@z�@o@�@��@%@  @�@�y@V@$�@?}@��@
��@
�@	hs@��@ �@�;@��@\)@ff@�@I�@(�@t�@�!@~�@��@%@ bN?�|�?��?�{?��h?��?�1?�1?��m?�?�X?�7L?��?�b?�K�?�E�?���?��?��?�w?��?�v�?�ƨ?ꟾ?���?�b?�ȴ?��T?�?}?㕁?�!?�M�?�7?�bN?�;d?ݲ-?���?�j?�"�?��?׮?�K�?�
=?�ff?�?�z�?�9X?ӕ�?�-?Ͼw?��?Ͳ-?̬?��m?�ƨ?���?�7L?ȴ9?�$�?�9X?��7?���?�5??�O�?�1?�ƨ?�"�?�~�?�^5?��#?��^?��^?���?�=q?�^5?���?�C�?�"�?�ƨ?�(�?��?�/?��?�v�?���?���?�A�?��?�Ĝ?��7?���?���?�J?�-?�-?�M�?�n�?°!?���?��?�o?�33?�t�?Õ�?öF?��
?���?��?�9X?�z�?ě�?ļj?��/?��?�?}?Ł?š�A���A���A��A���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A��
A��
A��
A��#A��TA��TA��HA��HA��;A��HA��TA��TA��TA��TA��`A��`A��TA��`A��`A��`A��;Aذ!A؇+A�l�A�\)A�=qA��HAבhA�t�A�S�A�bA���A�t�A��A��A��/A���A���A���Aմ9AծG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B�BG�BO�B\)By�B��B�!B�`B<jBdZBhsBgmBgmBffBe`BdZBiyBiyBZBN�BR�BO�BW
B\)BgmBo�Bv�By�B~�B�B�B�B�+B�7B�7B�7B�{B��B�{B��B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�\B�%B�Bz�Br�Bk�BaHB]/BYBL�B49B�BuB%B��B��B�BB��B�}B�'B��Br�B^5BJ�B/B!�B�B�BoB
=B
��B
�mB
��B
�wB
�B
u�B
gmB
ZB
O�B
6FB
!�B
�B
uB
\B
DB
B	��B	�B	�TB	�B	�B	��B	��B	��B	��B	��B	��B	ȴB	��B	�FB	�B	��B	��B	�\B	�=B	�B	� B	y�B	p�B	n�B	hsB	cTB	_;B	[#B	VB	N�B	F�B	D�B	?}B	8RB	/B	%�B	�B	uB	\B	DB	%B	B	  B�B�B��B��BɺB��B�9B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�PB�=B�+B�B�B~�B{�Bz�Bq�Bl�BjBhsBgmB\)B^5B\)B[#BW
BVBYB^5B`BB\)BW
BT�BR�BW
BW
BW
BVBQ�BQ�BL�BL�BJ�BJ�BG�BF�BE�BB�B?}B=qB>wB=qB;dB;dB9XB7LB33B/B33B2-B/B2-B2-B2-B-B/B,B/B/B/B/B0!B2-B49B/B6FB>wB>wB<jB:^B6FB@�B?}BB�BD�BD�BI�BH�BK�BK�BT�B]/Be`Bm�B}�B�bB��B��B�B�B	B	B	%�B	/B	A�B	]/B	hsB	v�B	�B	�7B	�=B	��B	��B	��B	��B	�B	�-B	ŢB	ȴB	��B	��B	�B	�)B	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
  B
B
%B
1B
JB
hB
bB
�B
�B
 �B
 �B
"�B
$�B
$�B
%�B
'�B
)�B
)�B
.B
0!B
0!B
0!B
1'B
0!B
1'B
5?B
5?B
7LB
:^B
;dB
;dB
<jB
=qB
@�B
C�B
B�B
D�B
F�B
F�B
G�B
I�B
J�B
J�B
L�B
L�B
N�B
P�B
P�B
P�B
Q�B
Q�B
S�B
R�B
T�B
VB
YB
ZB
ZB
[#B
]/B
]/B
^5B
_;B
`BB
aHB
bNB
bNB
cTB
cTB
e`B
e`B
ffB
ffB
hsB
hsB
jB
iyB
l�B
l�B
l�B
l�B
n�B
n�B
o�B
p�B
p�B
q�B
r�B
t�B
u�B
u�B
v�B
w�B
x�B
x�B
x�B
y�B
z�B
|�B
|�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�7B
�=B
�DB
�JB
�PB
�VB
�VB
�\B
�bB
�hB
�oB
�oB
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
��B
��B
��B
�B
�B
�!B
�'B
�-B
�3B
�9B
�?B
�?B
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�dB
�jB
�qB
�dB
�jB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�dB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�dB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��BBB
=BuB!�B-B>wBE�BI�BK�BJ�BL�BN�BO�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�B
��B�BE�BM�BZBw�B��B�B�TB:^BbNBffBe`Be`BdZBcTBbNBgmBgmBXBL�BP�BM�BT�BZBe`Bm�Bt�Bw�B|�B~�B~�B�B�B�+B�+B�+B�oB��B�oB�{B�bB��B��B�{B��B��B��B��B��B��B��B��B��B��B�{B��B��B�hB�VB�PB�B~�Bx�Bp�BiyB_;B[#BW
BJ�B2-B�BhBB��B�B�5B��B�qB�B��Bp�B\)BH�B-B�B�B{BbB1B
��B
�`B
��B
�jB
�B
s�B
e`B
XB
M�B
49B
�B
�B
hB
PB
	7B	��B	�B	�B	�HB	�B	�B	��B	��B	��B	��B	��B	��B	ƨB	�wB	�9B	�B	��B	�{B	�PB	�1B	� B	}�B	w�B	n�B	l�B	ffB	aHB	]/B	YB	S�B	L�B	D�B	B�B	=qB	6FB	-B	#�B	�B	hB	PB		7B	B	B��B�B�
B��BɺBǮB�}B�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�1B�B� B� B|�By�Bx�Bo�BjBhsBffBe`BZB\)BZBYBT�BS�BW
B\)B^5BZBT�BR�BP�BT�BT�BT�BS�BO�BO�BJ�BJ�BH�BI�BE�BE�BD�BA�B>wB<jB=qB<jB:^B:^B8RB6FB2-B.B2-B1'B.B1'B1'B1'B,B.B+B.B.B.B.B/B1'B33B.B5?B=qB=qB;dB9XB5?B?}B>wBA�BC�BC�BH�BG�BJ�BJ�BS�B\)BdZBl�B|�B�\B��B��B�B�B	B	B	$�B	.B	@�B	\)B	gmB	u�B	�B	�1B	�7B	��B	��B	��B	��B	�B	�'B	ĜB	ǮB	��B	��B	��B	�#B	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
+B
DB
bB
\B
{B
�B
�B
�B
!�B
#�B
#�B
$�B
&�B
(�B
(�B
-B
/B
/B
/B
0!B
/B
0!B
49B
49B
6FB
:^B
;dB
;dB
<jB
=qB
@�B
C�B
B�B
D�B
F�B
F�B
G�B
I�B
J�B
J�B
L�B
L�B
N�B
P�B
P�B
P�B
Q�B
Q�B
S�B
R�B
T�B
VB
YB
ZB
ZB
[#B
]/B
]/B
^5B
_;B
`BB
aHB
bNB
bNB
cTB
cTB
e`B
e`B
ffB
ffB
hsB
hsB
jB
iyB
l�B
l�B
l�B
l�B
n�B
n�B
o�B
p�B
p�B
q�B
r�B
t�B
u�B
u�B
v�B
w�B
x�B
x�B
x�B
y�B
z�B
|�B
|�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�7B
�DB
�JB
�PB
�VB
�\B
�\B
�bB
�hB
�oB
�uB
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
��B
��B
��B
��B
�B
�B
�B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�qB
�qB
�jB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�}B
��B
�wB
�}B
�}B
�}B
�}B
�}B
�wB
��B
��B
��B
�}B
��B
��B
�}B
��B
��B
��B
��B
�}B
��B
��B
��B
��B
�}B
�}B
��B
��B
�B
�B
�B
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
��B
��B
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
��B
��B
��B
��BBB1BhB�B+B<jBC�BG�BI�BH�BJ�BL�BM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808280702502021061413523120210614135231202106141746362021061417463620210614174636201808280702502021061413523120210614135231202106141746362021061417463620210614174636PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018082807025020180828070250  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082807025020180828070250QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082807025020180828070250QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015220210722160152IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                