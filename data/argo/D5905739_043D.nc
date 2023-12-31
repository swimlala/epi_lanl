CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-02T07:00:58Z creation      
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
resolution        =���     �  e�   TEMP         
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
_FillValue                 8     PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ׬   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   $   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   <   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                        SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � dArgo profile    3.1 1.2 19500101000000  20181102070058  20210617131507  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               +   +DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؋X�+�@؋X�+�11  @؋O� P@؋O� P@6���Y��@6���Y���c��B�؄�c��B�؄11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  BB  BB  >���?�  @   @Fff@�33@�33@�33@�  A��A��A$��AA��Ac33A���A�  A���A���A���A���A���A�  A�33B  BffB  B ffB(��B0ffB8  B?��BH  BPffBX  B`ffBi33Bp��BxffB��B���B�ffB�ffB�33B�  B���B�33B�  B���B�  B�  B�33B�ffB�33B�  B�  B���B�ffB���B�ffB�33B�33Bܙ�B�ffB�  B�ffB�ffB�  B���B�33B���C   C33C  C�fC  C
33C�C��C  CL�C�C��C33C  C�fC  C   C!��C$  C%�fC'��C*  C,L�C.33C0�C2  C3�fC633C833C:  C<L�C>33C@�CB�CC�fCF33CH33CJ�CL�CN  CP  CQ�fCT�CV  CX  CZ33C\33C^�C`�Cb  Cc�fCe�fCh33Cj33Cl�Cn�Co�fCrL�Ct�Cv  CxL�Cz33C{�fC~33C��C��fC�  C��C�&fC��C�  C��C��3C��fC��fC�  C��C�&fC��C��3C��C�&fC��C�  C��C�  C��fC��C�&fC��C�  C��C�&fC��C��3C��C��3C��fC�  C��C�&fC��C�  C��C��C�  C��fC�  C��C�&fC��C��fC�  C��C�&fC�  C�ٚC��3C��C��C�  C��fC�  C��C��C�&fC�  C��fC��3C��C��C�&fC�  C��fC�  C��C��C�  C��fC��fC��3C��3C�  C�  C�  C��C��C��C��C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��fC��C��C��fC�ٚC��3C��3C�  C�  C�  C��C��C��C��C��C��C�&fC�&fC��C��fC��3C��3C��C�&fC��fC��3D fD �3D3Ds3DfD��D� D
y�DL�DfD�fD� D` D�D�3D �fD#9�D%� D(l�D+  D-� D0FfD2ٚD5l�D7��D:� D=,�D?� DB&fDD��DG,�DI�fDL  DN��DQ,�DS��DV�DX� D[�D]��D`�Db��De3Dg� Dj,�Dl� DoY�Dq��Dt��Dw33DyٚD|fD~�fD��3D��3D�@ D���D��3D�3D�@ D�y�D��3D��D�#3D�` D���D�ٚD�fD�Y�D���D��fD�fD�I�D��3D��3D�� D�&fD�S3D���D���D��D�fD�@ D�l�D��fD��3D��fD�&fD�S3D�|�D���D�� D��D�@ D�y�D���D���D��D�9�D�ffD���D��fD�3D�FfD�vfD��fD���D�fD�<�D�p DŖfD��3D��3D�3D��D�9�D�P D�p D�|�Dό�DЦfDѰ D�� D���D�ٚD��fD���D��3D���D���D�3D�	�D�fD�fD�fD��D�  D�#3D�)�D�33D�<�D�I�D�P D�\�D�` D�ffD�p D�|�D��D�3D��D�fD��D� D�D��fD��fD�� D�� D���D�fD���D�3D� D�#3D�33E !�E ��E,�E�3E9�E�3EL�E�fEa�E�3Es3EfE�E	��E
��E�3E( E+3E��E��E E� E��E� E��ET�E�3E� E!�E� E�3E� E �E"X E#�fE$��E&+3E'#3E(��E*fE+	�E,�fE-�3E.�3E/�3E1d�E2\�E3ɚE5.fE6�E7|�E8�fE:( E;t�E<��E?��EB��EE�EH� EL3EOQ�ER.fEU` EX� E[� E^� Eb3EeC3Eh�Eky�Enl�Eq��Et� Ew��E{fE~!�E��fE�0�E��fE�4�E���E�m�E�� E��3E�fE�bfE���E���E�Q�E��3E��E�5�E��fE���E�-�E�|�E���E� E�k3E���E�3E�T�E�� E� E�NfE��fE���E�1�E���E��E�-�E�y�>���>���>���>���>���>���?   >���>���>���>���?   ?��?L��?333?fff?L��?���?���?�ff?�ff?���?ٙ�@   @ff@   @,��@9��@S33@`  @s33@�  @�  @���@�33@���@���@�ff@�33@�  @�  @���A��A��A��A��A$��A,��A4��A>ffAD��ANffAVffA`  AfffAnffAt��A|��A�ffA�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441444111141414141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?fff?�  @   @fff@�33@�33@�33@�  A	��A��A,��AI��Ak33A���A�  A���A���A���A���A���A�  B��B
  BffB  B"ffB*��B2ffB:  BA��BJ  BRffBZ  BbffBk33Br��BzffB���B���B�ffB�ffB�33B�  B���B�33B�  B���B�  B�  B�33B�ffB�33B�  B�  B���B�ffB���B�ffB�33B�33Bݙ�B�ffB�  B�ffB�ffB�  B���B�33B���C � C�3C� CffC� C
�3C��CL�C� C��C��CL�C�3C� CffC� C � C"L�C$� C&ffC(L�C*� C,��C.�3C0��C2� C4ffC6�3C8�3C:� C<��C>�3C@��CB��CDffCF�3CH�3CJ��CL��CN� CP� CRffCT��CV� CX� CZ�3C\�3C^��C`��Cb� CdffCfffCh�3Cj�3Cl��Cn��CpffCr��Ct��Cv� Cx��Cz�3C|ffC~�3C�L�C�&fC�@ C�L�C�ffC�L�C�@ C�L�C�33C�&fC�&fC�@ C�L�C�ffC�L�C�33C�L�C�ffC�Y�C�@ C�Y�C�@ C�&fC�L�C�ffC�Y�C�@ C�L�C�ffC�L�C�33C�L�C�33C�&fC�@ C�Y�C�ffC�Y�C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�Y�C�&fC�@ C�L�C�ffC�@ C��C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�@ C�&fC�33C�L�C�L�C�ffC�@ C�&fC�@ C�L�C�Y�C�@ C�&fC�&fC�33C�33C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�33C�33C�&fC�L�C�L�C�&fC��C�33C�33C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�L�C�&fC�33C�33C�Y�C�ffC�&fC�33D &fD �3D33D�3D&fD��D� D
��Dl�D&fDfD� D� D9�D�3D �fD#Y�D&  D(��D+  D-� D0ffD2��D5��D8�D:� D=L�D?� DBFfDD��DGL�DI�fDL@ DN��DQL�DS��DV9�DX� D[9�D]��D`9�Db��De33Dg� DjL�Dl� Doy�Dr�Dt��DwS3Dy��D|&fD~�fD��3D�3D�P D���D��3D�3D�P D���D��3D���D�33D�p D���D��D�&fD�i�D���D��fD�&fD�Y�D��3D��3D�  D�6fD�c3D���D���D���D�&fD�P D�|�D��fD��3D�fD�6fD�c3D���D���D�� D��D�P D���D���D���D��D�I�D�vfD���D��fD�#3D�VfD��fD��fD���D�fD�L�DĀ DŦfD��3D��3D�3D�,�D�I�D�` D̀ DΌ�DϜ�DжfD�� D�� D���D��D��fD���D�3D�	�D��D�3D��D�&fD�&fD�&fD�,�D�0 D�33D�9�D�C3D�L�D�Y�D�` D�l�D�p D�vfD� D��D��D�3D��D�fD��D�� D�ɚD��fD��fD�� D�  D��D�fD��D�3D�  D�33D�C3E )�E ��E4�E�3EA�E�3ET�E�fEi�E�3E{3EfE�E	��E
��E�3E0 E33E��E��E  E� E��E  E��E\�E�3E� E)�E� E�3E� E �E"` E#�fE$��E&33E'+3E(��E*fE+�E,�fE-�3E/3E/�3E1l�E2d�E3њE56fE6$�E7��E8�fE:0 E;|�E<��E?��EB��EE�EI  EL3EOY�ER6fEUh EX� E[� E^� Eb#3EeK3Eh�Ek��Ent�Eq��Et� Ew��E{fE~)�E��fE�4�E��fE�8�E���E�q�E�� E��3E�
fE�ffE���E���E�U�E��3E��E�9�E��fE���E�1�E���E���E�  E�o3E���E�3E�X�E�� E� E�RfE��fE���E�5�E���E��E�1�E�}�G�O�?L��G�O�G�O�G�O�?fffG�O�G�O�G�O�?L��?fff?�  ?���G�O�?���G�O�?�ffG�O�?���G�O�?�ff@ff@��@   @&ff@@  @L��@Y��@s33@�  @���@�  @�  @���@�33@���@���@�ff@�33@�  A   A��A��A��A��A$��A,��A4��A<��AFffAL��AVffA^ffAh  AnffAvffA|��A�ffA�ffA�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441444111141414141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ �@ �@ *@ �@ "�@ )�@ /�@ 7L@ >@ FQ@ R�@ `�@ m�@ z�@ �7@ ��@ ��@ �-@ ��@ ��@ ��@ �@ �q@j@�@ @-@:@F�@UU@c�@p�@~�@��@��@��@�9@��@є@�;@�4@�,@%@*@""@/@=q@K@Yn@g�@t�@��@�\@�U@��@��@�W@�O@��@��@��@
=@B@&�@33@B�@O0@[z@i�@x�@�@�@�m@�r@�k@�@�
@�@�@��@@O@(G@6�@DD@P�@_�@l�@y�@��@�<@�5@�-@�&@�@��@�y@��@v@o@g@-@9X@I@V�@c�@qS@~K@��@��@��@��@@є@�;@�4@��@�@�@!s@1'@>�@K�@Yn@e�@v@�d@�\@�@�@��@�W@�O@��@��@�E@J@�@%�@3�@@,@M$@Z�@i�@x&@�+@�u@��@��@��@��@�
@�@�Y@��@V@[@*S@6�@D�@S�@`B@l�@{�@��@��@��@��@�2@�*@�t@��@� @	j@	�@	�@	-@	<@	I@	S�@	b�@	qS@	�W@	��@	��@	��@	��@	��@	�7@	܀@	�@	��@
1@
�@
""@
.l@
<�@
K�@
Yn@
hs@
t@
�W@
�\@
��@
�@
�R@
Ĝ@
�C@
��@
�@@
��@
=@�@&;@3�@A�@O0@\�@j@x&@�@�u@�@�@��@�c@�
@�@�Y@  @�@O@(G@5�@B�@R�@`B@k�@x�@��@��@��@�~@�&@�|@�#@��@�q@�@o@ �@.l@:�@FQ@T�@bN@r@�W@��@��@��@��@Ĝ@��@ލ@��@z�@�J@o@\�@�Y@��@@�@�D@խ@g@i!@�~@� @=q@�@�|@�@Z@��@�@-�@p�@��@��@>�@�d@��@�@O�@�$@�\@O@^�@��@�m@*S@oF@��@��@@�@��@�*@�@^5@��@��@)�@qS@�@  @FQ@�7@�|@@S�@��@�@�@_�@�z@�@*S@oF@��@� @ ;d@ |�@ ��@! �@!DD@!�|@!ƨ@"�@"Ji@"��@"��@#
=@#Ji@#��@#�@$�@$Lu@$��@$�@%�@%N�@%��@%�7@&@&S�@&��@&�\@'�@'V�@'��@'��@(g@(`�@(��@(�@)$�@)dZ@)��@)�@*&�@*g@*��@*��@+
@+Z�@+�0@+Ӡ@,�@,F�@,�@,��@,��@-/@-hs@-��@-��@.�@.I�@.�@.�@.��@/*S@/`�@/��@/ψ@0�@0>@0v@0��@0�m@1 �@1X�@1�@1�c@2]@2:@2s_@2�f@2�`@3
@3V�@3��@3��@3��@47�@4r@4��@4�@5
@5V�@5�D@5�>@5��@67L@6qS@6�Z@6�T@7[@7V�@7�@7��@8v@8@,@8{�@8�F@8�L@9�T@:�@:��@;'�@;�#@<:@<��@=K�@=��@>UU@>�Y@?\�@?��@@e	@@��@A��@Bj@B��@C:@C��@D=q@D��@EDD@E��@FH]@F�@GO�@G�@H��@H��@I�T@J1@J��@Ko@K�!@L�@L��@MM�@M��@NI�@N�/@Om:@O�9@P��@Q��@SV@TqS@U�&@W�@Xt�@Y�f@[
=@\m:@]Ӡ@_{@`x�@aє@c@dx&@e�^@g�@hX�@i�!@k	@lm�@m��@o�@pZ�@q�Z@s�@tk.@u�R@w6@xWa@x��@x�@y&�@yr@y�@y��@z4�@z|�@z�>@{1@{K�@{�@{Ӡ@|6@|Z@|�T@|ލ@}33@}t@}��@}�e@~3�@~ul@~��@�@Lv@�PG�O�@ G�O�G�O�G�O�@ �G�O�G�O�G�O�@ @ �@ j@ G�O�@ �G�O�@ vG�O�@ �G�O�@ �@ 
�@ �@ �@ V@ @ o@ �@ �@ �@ �@ O@ �@  �@ "�@ $�@ (G@ *S@ -@ /�@ 33@ 5?@ 8�@ <@ ?}@ B�@ FQ@ I�@ M$@ Q=@ S�@ X@ [z@ _�@ bN@ e�@ hs@ k�@ oF@ s_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�E�A�K�A�^5A�\)A�bNA�dZA�hsA�dZA�bNA�\)A�\)A�`BA�^5A�VA�S�A�I�A�33A�{A�{A�
=A�%A���AӴ9Aӗ�AӇ+A�|�A�|�AӅAӅA�XA�;dAҴ9A�Q�A�  Aʣ�A��A���A���A�(�A��A�M�A��A�v�A��hA��hA���A���A�&�A�1'A���A�7LA��RA�K�A��A�(�A���A�-A�1A��HA�v�A��A�VA�$�A��`A���A�`BA�ffA��
A�?}A��7A��`A��^A�oA�G�A�~�A��A���A��A���A��wA��A�l�A��A��`A��/A���A��TA��A�Q�A��A��
A�1A���A��A���A��wA�7LA�9XA���A���A�E�A�~�A�JA��9A���A�C�A���A�\)A�I�A�z�A���A��`A~��A|jAzjAyO�Aw�PAv-Au\)At��As;dAq�TAp�Ao�An^5Al$�Aj�jAil�AhjAf�Ac��Ab�/Ab��Ab��Aa�;Aa33AaVA`�9A_�PA[33AYƨAXJAUO�AU%AS�ARr�AQ��AQVAPZAO33AM�;AM
=ALA�AK�PAJ�AI��AHZAF�HAEAD�\AC��AA��A@�A@�A?�wA?&�A>�9A>M�A>{A<ȴA;�7A:=qA9��A8ȴA7�wA6~�A5ƨA4  A1��A1|�A1/A01A.  A-�A,I�A*�RA*bA)�PA(�/A(1'A't�A&��A&ZA%��A%
=A#G�A!�TA!l�A �yA n�A $�A��AO�A{A�/A;dA�\A-A��A7LA�A �AC�A�7A�\A9XA?}A�-A�`A�;AjA�AO�A	��A�RA��At�AVA�DAA�A�A�yAA�A ��A (�@�@��@�?}@��P@��@�K�@�Ĝ@�
=@��@�C�@��@�h@��
@�33@���@�n�@�7@� �@�V@ܴ9@�I�@�l�@�n�@���@�`B@�b@�\)@�5?@�x�@�G�@�?}@��@�r�@�Z@���@�j@�hs@��7@��@�(�@�C�@�V@�@���@��T@�1@��T@�Q�@��w@�"�@��y@���@��`@��@��@�z�@��
@�r�@��P@�$�@��@���@�b@�@��7@��@�1'@�{@�p�@��9@��;@�V@���@��7@���@�
=@�x�@l�@~��@~ff@}�@{�@yX@x  @v�R@t�@q&�@m�T@l(�@j�@jJ@h��@f�@e`B@cS�@aX@_l�@^@\j@Y�^@X��@Xb@V�+@T�j@S�@R�\@QX@O�;@N{@L��@Kt�@I�^@H�u@G|�@E/@D�@CdZ@B~�@A��@@ �@>��@<j@;33@9��@8�9@7�@6ȴ@5�h@49X@2�@2=q@0r�@0Q�@/�@.ȴ@.ff@-p�@,�@,1@+��@*��@*�@)�7@'l�@&��@%�h@$�@#�F@"��@"=q@!hs@ Ĝ@�@K�@��@$�@�T@(�@�m@C�@J@X@bN@�@+@��@$�@?}@�/@(�@�@o@-@��@�@
=@$�@�h@�@��@ƨ@C�@
�\@	��@	hs@�9@A�@��@|�@��@��@5?@p�@/@�@z�@(�@�m@C�@33@�@�\@7L@ ��?��?���?�^5?���?���?��?�33?�|�?�?���?���?�b?��?�F?�n�?��?ߝ�?��?�I�?�"�?���?�b?��y?Ձ?���?�9X?�33?�J?��`?Ѓ?�|�?�\)?��?͑h?�I�?˅?˅?�=q?��#?�x�?ȴ9?�r�?��y?���?��?���?���?�%?� �?��?�v�?���?�/?�1?�dZ?�"�?�?�^5?�=q?�=q?�^5?���?��H?�C�?���?�ƨ?�I�?���?�O�?��?���?��?�\)?�|�?���?���?��w?�  ?�  ?� �?�A�?�A�?��?���?�Ĝ?��`?�%?�&�?�G�?�hs?�hs?��7?���?��?��?�-?�-?�n�?\A�A�A�G�A�E�A�G�A�G�A�A�A�M�A�I�A�G�A�C�A�?}A�E�A�I�A�A�A�C�A�;dA�?}A�G�A�I�A�I�A�G�A�I�A�G�A�G�A�K�A�XA�`BA�bNA�^5A�ZA�XA�\)A�`BA�`BA�bNA�dZA�dZA�dZA�hsA�hsA�ffA�dZA�dZA�bNA�`BA�\)A�\)A�\)A�ZA�ZA�\)A�^5A�^5A�`BA�bNA�bNA�`BA�`BA�^5A�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�E�A�E�A�K�A�^5A�\)A�bNA�dZA�hsA�dZA�bNA�\)G�O�G�O�A�^5A�VA�S�A�I�A�33A�{A�{A�
=A�%A���AӴ9Aӗ�AӇ+A�|�A�|�AӅAӅA�XA�;dAҴ9A�Q�A�  Aʣ�A��A���A���A�(�A��A�M�A��A�v�A��hA��hA���A���A�&�A�1'A���A�7LA��RA�K�A��A�(�A���A�-A�1A��HA�v�A��A�VA�$�A��`A���A�`BA�ffA��
A�?}A��7A��`A��^A�oA�G�A�~�A��A���A��A���A��wA��A�l�A��A��`A��/A���A��TA��A�Q�A��A��
A�1A���A��A���A��wA�7LA�9XA���A���A�E�A�~�A�JA��9A���A�C�A���A�\)A�I�A�z�A���A��`A~��A|jAzjAyO�Aw�PAv-Au\)At��As;dAq�TAp�Ao�An^5Al$�Aj�jAil�AhjAf�Ac��Ab�/Ab��Ab��Aa�;Aa33AaVA`�9A_�PA[33AYƨAXJAUO�AU%AS�ARr�AQ��AQVAPZAO33AM�;AM
=ALA�AK�PAJ�AI��AHZAF�HAEAD�\AC��AA��A@�A@�A?�wA?&�A>�9A>M�A>{A<ȴA;�7A:=qA9��A8ȴA7�wA6~�A5ƨA4  A1��A1|�A1/A01A.  A-�A,I�A*�RA*bA)�PA(�/A(1'A't�A&��A&ZA%��A%
=A#G�A!�TA!l�A �yA n�A $�A��AO�A{A�/A;dA�\A-A��A7LA�A �AC�A�7A�\A9XA?}A�-A�`A�;AjA�AO�A	��A�RA��At�AVA�DAA�A�A�yAA�A ��A (�@�@��@�?}@��P@��@�K�@�Ĝ@�
=@��@�C�@��@�h@��
@�33@���@�n�@�7@� �@�V@ܴ9@�I�@�l�@�n�@���@�`B@�b@�\)@�5?@�x�@�G�@�?}@��@�r�@�Z@���@�j@�hs@��7@��@�(�@�C�@�V@�@���@��T@�1@��T@�Q�@��w@�"�@��y@���@��`@��@��@�z�@��
@�r�@��P@�$�@��@���@�b@�@��7@��@�1'@�{@�p�@��9@��;@�V@���@��7@���@�
=@�x�@l�@~��@~ff@}�@{�@yX@x  @v�R@t�@q&�@m�T@l(�@j�@jJ@h��@f�@e`B@cS�@aX@_l�@^@\j@Y�^@X��@Xb@V�+@T�j@S�@R�\@QX@O�;@N{@L��@Kt�@I�^@H�u@G|�@E/@D�@CdZ@B~�@A��@@ �@>��@<j@;33@9��@8�9@7�@6ȴ@5�h@49X@2�@2=q@0r�@0Q�@/�@.ȴ@.ff@-p�@,�@,1@+��@*��@*�@)�7@'l�@&��@%�h@$�@#�F@"��@"=q@!hs@ Ĝ@�@K�@��@$�@�T@(�@�m@C�@J@X@bN@�@+@��@$�@?}@�/@(�@�@o@-@��@�@
=@$�@�h@�@��@ƨ@C�@
�\@	��@	hs@�9@A�@��@|�@��@��@5?@p�@/@�@z�@(�@�m@C�@33@�@�\@7L@ ��?��?���?�^5?���?���?��?�33?�|�?�?���?���?�b?��?�F?�n�?��?ߝ�?��?�I�?�"�?���?�b?��y?Ձ?���?�9X?�33?�J?��`?Ѓ?�|�?�\)?��?͑h?�I�?˅?˅?�=q?��#?�x�?ȴ9?�r�?��y?���?��?���?���?�%?� �?��?�v�?���?�/?�1?�dZ?�"�?�?�^5?�=q?�=q?�^5?���?��H?�C�?���?�ƨ?�I�?���?�O�?��?���?��?�\)?�|�?���?���?��w?�  ?�  ?� �?�A�?�A�?��?���?�Ĝ?��`?�%?�&�?�G�?�hs?�hs?��7?���?��?��?�-?�-?�n�?\A�A�A�G�A�E�A�G�A�G�A�A�A�M�A�I�A�G�A�C�A�?}A�E�A�I�A�A�A�C�A�;dA�?}A�G�A�I�A�I�A�G�A�I�A�G�A�G�A�K�A�XA�`BA�bNA�^5A�ZA�XA�\)A�`BA�`BA�bNA�dZA�dZA�dZA�hsA�hsA�ffA�dZA�dZA�bNA�`BA�\)A�\)A�\)A�ZA�ZA�\)A�^5A�^5A�`BG�O�G�O�A�`BA�`BA�^5A�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=B	7B
=B	7B	7B	7B	7B	7B
=B
=B
=B
=B�/B+B%BB+B+B�B&�B+B,B33B>wB>wB@�B@�BB�BF�BJ�BI�BT�BR�B]/BcTB{�Bt�Bv�Bq�Bq�Bs�B�B�=B�7B�uB�{B��B��B�uB��B��B��B��B��B��B�{B��B��B��B�{B�oB�\B�hB�JB�+B�7B�B}�B�B|�Bu�BjBiyB\)BR�BF�B=qB$�BB��B�`B��B�XB�-B��B�uB}�Bp�BiyBdZBXBH�B<jB$�B�B�B�B1B
��B
�B
�BB
ȴB
�jB
�?B
�?B
�?B
�qB
ÖB
��B
�bB
�?B
��B
�DB
w�B
aHB
VB
M�B
G�B
@�B
:^B
6FB
+B
!�B
�B
�B
1B	��B	��B	�B	�ZB	��B	ɺB	ÖB	B	�}B	�qB	�jB	�^B	�RB	��B	�bB	�7B	r�B	k�B	ffB	_;B	^5B	^5B	^5B	[#B	VB	L�B	K�B	F�B	A�B	;dB	49B	-B	$�B	�B	�B	hB		7B	%B	B	B��B��B��B��B�B�B�B�B�mB�NB�)B�BǮBȴBĜB��B�'B��B��B��B�bB�bB�PB�7B�%B�B�B�B~�B{�Bu�Bv�Bv�Bt�Bs�Bq�Bp�Bo�Bm�BiyBgmBe`BcTBaHB`BB_;B^5BZBXBVBS�BO�BN�BM�BI�BG�BD�BA�BD�BC�BC�BB�BC�BB�BB�B:^B8RB33B49B5?B33B2-B/B0!B0!B0!B1'B1'B33B0!B9XB7LB9XB9XB9XB8RB9XB:^B:^B=qB<jB<jB<jB=qB=qB=qB?}B@�BB�BC�BA�BYBl�BgmBm�Bq�B~�B��B�B��B�BB�B	+B	�B	%�B	6FB	A�B	D�B	G�B	J�B	?}B	C�B	F�B	G�B	k�B	�7B	�DB	�=B	��B	��B	�B	�3B	�RB	�qB	�jB	�qB	�wB	��B	��B	�B	�B	�TB	�fB	�fB	�B	�B	��B	��B	��B	��B
  B
B
B
%B
+B

=B
PB
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
%�B
'�B
(�B
+B
-B
-B
.B
0!B
2-B
2-B
49B
6FB
7LB
9XB
;dB
:^B
;dB
=qB
=qB
>wB
@�B
B�B
C�B
D�B
E�B
F�B
G�B
H�B
J�B
K�B
K�B
N�B
M�B
M�B
O�B
O�B
P�B
Q�B
R�B
Q�B
S�B
S�B
VB
XB
XB
ZB
ZB
[#B
\)B
\)B
]/B
^5B
_;B
_;B
`BB
aHB
aHB
cTB
cTB
dZB
e`B
e`B
ffB
gmB
gmB
hsB
hsB
iyB
jB
k�B
l�B
l�B
m�B
o�B
n�B
o�B
p�B
q�B
r�B
q�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
{�B
z�B
z�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
� B
� B
�B
�B
�B
�%B
�%B
�7B
�1B
�JB
�VB
�PB
�\B
�\B
�hB
�hB
�uB
��B
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
�B
�B
�B
�B
�B
�!B
�'B
�-B
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
�RB
�RB
�XB
�RB
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�XB
�XB
�XB
�^B
�^B
�XB
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B
�^B
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dBJB	7B	7B	7B	7B
=B1BDB1B	7B
=B
=B	7B	7B1B
=B
=B	7B
=B	7B	7B	7B	7B	7BDB
=B
=B	7B	7B1B
=B
=B1B	7B	7B	7B	7B	7B	7B	7B	7B
=B	7B
=B	7B	7B
=B
=B
=B
=B
=B
=B
=B
=B	7BXB1B+B%BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B
B	B
B	B	B	B	B	B
B
B
G�O�G�O�BBB�B
BBzB&�B*�B+�B3B>ZB>[B@hB@hBBuBF�BJ�BI�BT�BR�B]Bc=B{�Bt�Bv�Bq�Bq�Bs�B��B�)B�#B�bB�hB�uB�uB�dB��B��B��B��B�~B�sB�mB�yB�tB�uB�oB�dB�QB�^B�@B�"B�.B�B}�B��B|�Bu�BjyBitB\$BR�BF�B=nB$�BB��B�_B��B�WB�-B��B�vB}�Bp�Bi{Bd]BXBH�B<nB$�B�B�B�B8B
��B
�B
�JB
ȽB
�sB
�IB
�IB
�JB
�|B
âB
��B
�oB
�LB
��B
�RB
w�B
aWB
VB
M�B
G�B
@�B
:oB
6WB
+B
!�B
�B
�B
EB	��B	��B	�B	�pB	��B	��B	íB	§B	��B	��B	��B	�xB	�lB	�B	�}B	�RB	r�B	k�B	f�B	_XB	^SB	^SB	^TB	[BB	V$B	L�B	K�B	F�B	A�B	;�B	4\B	-1B	%B	�B	�B	�B		]B	KB	9B	3B�B�B�
B�B��B��B��B�B�B�zB�VB�DB��B��B��B��B�WB��B��B��B��B��B��B�jB�XB�SB�AB�;B0B|Bu�Bw BwBt�Bs�Bq�Bp�Bo�Bm�Bi�Bg�Be�Bc�Ba�B`�B_yB^tBZ\BXPBVDBT9BP BOBNBI�BG�BD�BA�BD�BC�BC�BB�BC�BB�BB�B:�B8�B3|B4�B5�B3}B2xB/fB0lB0mB0mB1tB1tB3�B0oB9�B7�B9�B9�B9�B8�B9�B:�B:�B=�B<�B<�B<�B=�B=�B=�B?�B@�BB�BC�BA�BYvBl�Bg�Bm�BrBiB�B��B�FB�B�B	�B	B	&kB	6�B	BB	E-B	HBB	KXB	@B	D3B	GHB	HQB	l+B	��B	��B	��B	�3B	�aB	��B	��B	�B	�2B	�-B	�7B	�@B	ˍB	ЮB	��B	��B	�-B	�BB	�EB	�zB	�B	��B	��B	��B	��B
 �B
�B
B
"B
+B
@B
VB
qB
sB
}B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
'B
'B
)"B
**B
,9B
.HB
.KB
/TB
1cB
3rB
3uB
5�B
7�B
8�B
:�B
<�B
;�B
<�B
>�B
>�B
?�B
A�B
C�B
EB
FB
GB
HB
I&B
J/B
L?B
MHB
MKB
P`B
O]B
O`B
QnB
QqB
RzB
S�B
T�B
S�B
U�B
U�B
W�B
Y�B
Y�B
[�B
[�B
\�B
]�B
]�B
^�B
_�B
`�B
`�B
bB
cB
cB
eB
e!B
f)B
g2B
g4B
h=B
iFB
iIB
jQB
jTB
k\B
leB
mmB
nvB
nxB
o�B
q�B
p�B
q�B
r�B
s�B
t�B
s�B
u�B
v�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
z�B
z�B
{�B
~	B
}B
}B
~B
~B
B
B
"B
�+B
�-B
�AB
�FB
�YB
�dB
�vB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�(B
�)B
�@B
�GB
�QB
�_B
�rB
�wB
��B
��B
��B
��B
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
�B
�B
�B
�)B
�/B
�KB
�eB
�{B
��B
��B
��B
��B
��B
��B
�B
� B
�6B
�KB
�`B
�pB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�0B
�@B
�OB
�^B
�mB
�jB
�nB
�pB
�yB
�}B
�yB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B"B	B	B	B	B
B
BB
B	B
B
B	B	B
B
B
B	B
B	B	B	B	B	BB
B
B	B	B
B
B
BB	B	B	B	B	B	B	B	B
B	B
B	B	B
B
B
B
B
B
B
B
G�O�G�O�BBBB�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811020700582021061413554920210614135549202106171313212021061713132120210617131321201811020700582021061413554920210614135549202106171313212021061713132120210617131321PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018110207005820181102070058  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110207005820181102070058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110207005820181102070058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�4000            4000            PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150720210617131507IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                