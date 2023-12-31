CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T17:01:49Z creation      
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
_FillValue                 4  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ɣ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ۘ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20181005170149  20210722160153  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               "   "DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؄5� ��@؄5� ��11  @؄5���@؄5���@6"��m3	@6"��m3	�c�����c����11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?fff@   @@  @�  @�33@�33@�  A   A��A#33A@  A`  A�  A���A�  A���A�  A�  A�  A�  B   BffBffB��B   B(  B0ffB8  B?��BH  BP  BXffB`ffBhffBpffBxffB�33B���B�  B�33B���B���B�33B�33B���B�33B�ffB�  B�  B�33B�33B�ffB�33B���B�ffB���B�  B�  B�33B�  B�33B�33B�33B�33B�33B�33B�33B�  C   C�fC�fC  C�fC	��C�fC�fC�fC��C33C33C�C�C  CL�C L�C"�C$  C&�C(  C)�fC,33C.�C0  C2L�C4L�C6�C8�C9�fC<33C>�C@�CA�fCC�fCF�CH  CI��CL  CN33CP�CR  CT33CV�CW�fCZ  C\33C^33C`33Cb�Cc��Cf  Ch  Cj�Cl33CnL�Cp  Cq��Cs�fCv  Cx33CzL�C|�C}��C�fC��C��C��C�&fC��C��3C��C��C�  C��3C��C�  C��3C�  C��C�  C��fC�  C�  C��fC��C�  C��3C��3C��fC��C��C��C��C��C��C��C��C�  C�  C��3C��C��C��C��C��C��C��C��3C��fC��C��C��3C�  C��3C��C�  C��fC��C�  C��3C��C��C�  C�&fC��C�  C��C�  C��fC�  C��C�  C��fC��3C�  C�&fC��C��3C��C��3C��fC�  C��C��C��fC��C�&fC�  C��fC��3C�  C��C�&fC��C��3C��3C��C�  C��3C��C�&fC��C��fC��C�&fC��C��3C��C�&fC��C��3C��C��C��fC�  C��C�33C��C�  C��C�&fC��C��3C�  C��C��fD33D��DL�D	�fDy�D  Dy�D�D�fDFfD�3D� D!�fD$L�D'�D)��D,��D/` D233D5fD7��D:ٚD=��D@y�DC33DE��DH�3DK@ DMٚDPl�DR��DUs3DW��DZ` D\�fD_y�Da��DdffDf�fDiS3DkٚDny�DqfDs�fDv3Dx�3D{&fD}S3DٚD�@ D���D��fD�#3D�s3D��fD�3D�` D���D���D�L�D�� D���D�9�D���D��3D�  D�ffD��fD�	�D�S3D��fD���D�6fD��fD��3D�#3D�p D��fD���D�9�D�|�D�ɚD�  D�0 D�l�D��fD��fD�#3D�` D���D��fD�3D�S3D��3D���D��D�S3D���D���D��D�fD�@ D�Y�D�y�DƓ3Dǣ3DȶfD��fD��3D���D���D�ٚD�ٚD���D�ٚD��fD��fD�� D���D��fD�� D׹�Dذ D٣3Dڜ�Dۙ�Dܜ�Dݠ Dޣ3DߦfD� DṚD�� D��fD�� D��3D� D�33D�L�D�l�D�3D�fD��fD�	�D�0 D�Y�D�y�D��D��fD��fD��D�,�D�L�D�s3D�� D�� D��3D��3E �E ��E&fE�3EFfE��Ec3E��Ey�E�E�3E�E�3E)�E;3E	FfE
�fE�fEI�EC3E�fE�3E�Ei�E� E��E#3E� E� E� E� EVfE��E� E!3E"t�E#a�E$��E&<�E'1�E(� E*3E+�E,�fE-� E.�3E/��E1h E2d�E3�fE5\�E6\�E7ٚE8��E:K3E;>fE<�3E?�3EB��EE��EH�fEL�EO#3ERNfEUffEXt�E[� E^��Ea��Ee,�Eh^fEk.fEn�3Eqp Et��Ew� Ez�fE}� E��3E�( E���E�T�E���E�T�E���E�D�E�� E���E�<�E��3E�� E�0 E�t�E��fE� �E�d�E��fE� E�P�E�� E�� E�>fE��fE��E�&fE��fE���E�, E�l E��3E� E�k3E��3E�
fE�H�E��fE��fE�@�E���E���E�+3E��3E��3E��?��?   >���?   ?   ?   ?   ?   ?   ?   ?333?333?L��?L��?�  ?���?���?�ff?�  ?ٙ�?�ff@ff@33@&ff@333@Fff@S33@fff@s33@�33@�  @�ff@�33@���@���@�33@�  @ٙ�@�33@�33A   A  A��A��A33A#33A)��A1��A9��A>ffAFffANffAT��A^ffAd��Ai��Aq��Ay��A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444444141411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?�  ?�33@   @`  @�  @�33@�33@�  A  A��A+33AH  Ah  A�  A���A�  A���A�  A�  A�  A�  B  B
ffBffB��B"  B*  B2ffB:  BA��BJ  BR  BZffBbffBjffBrffBzffB�33B���B�  B�33B���B���B�33B�33B���B�33B�ffB�  B�  B�33B�33B�ffB�33B���B�ffB���B�  B�  B�33B�  B�33B�33B�33B�33B�33B�33B�33B�  C � CffCffC� CffC
L�CffCffCffCL�C�3C�3C��C��C� C��C ��C"��C$� C&��C(� C*ffC,�3C.��C0� C2��C4��C6��C8��C:ffC<�3C>��C@��CBffCDffCF��CH� CJL�CL� CN�3CP��CR� CT�3CV��CXffCZ� C\�3C^�3C`�3Cb��CdL�Cf� Ch� Cj��Cl�3Cn��Cp� CrL�CtffCv� Cx�3Cz��C|��C~L�C�33C�L�C�Y�C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C�33C�L�C�@ C�33C�@ C�Y�C�@ C�&fC�@ C�@ C�&fC�L�C�@ C�33C�33C�&fC�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�@ C�@ C�33C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�33C�&fC�L�C�L�C�33C�@ C�33C�L�C�@ C�&fC�L�C�@ C�33C�L�C�Y�C�@ C�ffC�L�C�@ C�L�C�@ C�&fC�@ C�Y�C�@ C�&fC�33C�@ C�ffC�L�C�33C�L�C�33C�&fC�@ C�Y�C�L�C�&fC�L�C�ffC�@ C�&fC�33C�@ C�Y�C�ffC�L�C�33C�33C�Y�C�@ C�33C�L�C�ffC�L�C�&fC�L�C�ffC�L�C�33C�L�C�ffC�Y�C�33C�L�C�L�C�&fC�@ C�Y�C�s3C�Y�C�@ C�L�C�ffC�L�C�33C�@ C�L�C�&fDS3D��Dl�D
fD��D  D��D9�D�fDffD3D� D!�fD$l�D',�D)��D,��D/� D2S3D5&fD8�D:��D=��D@��DCS3DF�DH�3DK` DM��DP��DS�DU�3DX�DZ� D]fD_��Db�Dd�fDgfDis3Dk��Dn��Dq&fDs�fDv33Dx�3D{FfD}s3D��D�P D���D��fD�33D��3D��fD�#3D�p D���D�	�D�\�D�� D���D�I�D���D��3D�0 D�vfD��fD��D�c3D��fD���D�FfD��fD��3D�33D�� D��fD�	�D�I�D���D�ٚD� D�@ D�|�D��fD��fD�33D�p D���D��fD�#3D�c3D��3D���D�,�D�c3D���D���D���D�&fD�P D�i�Dŉ�Dƣ3Dǳ3D��fD��fD��3D���D���D��D��D���D��D��fD��fD�� D���D��fD�� D�ɚD�� Dٳ3Dڬ�D۩�Dܬ�Dݰ D޳3D߶fD�� D�ɚD�� D��fD�� D�3D�  D�C3D�\�D�|�D�3D��fD��fD��D�@ D�i�D�D��D��fD��fD��D�<�D�\�D��3D�� D�� D��3D��3E �E ��E.fE�3ENfE��Ek3E��E��E�E�3E�E�3E1�EC3E	NfE
�fE�fEQ�EK3E�fE�3E�Eq�E� EɚE+3E� E� E� E� E^fE��E� E!3E"|�E#i�E$��E&D�E'9�E(� E*3E+�E,�fE-� E/3E/��E1p E2l�E3�fE5d�E6d�E7�E8��E:S3E;FfE<�3E?�3EB��EE��EH�fEL!�EO+3ERVfEUnfEX|�E[� E^��Ea��Ee4�EhffEk6fEn�3Eqx Et��Ew� Ez�fE}� E��3E�, E���E�X�E���E�X�E� �E�H�E�� E���E�@�E��3E�� E�4 E�x�E��fE�$�E�h�E��fE� E�T�E�� E�  E�BfE��fE��E�*fE��fE���E�0 E�p E��3E� E�o3E��3E�fE�L�E��fE��fE�D�E���E���E�/3E��3E��3E��G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�?�  G�O�?���G�O�?�ff?�  ?���?ٙ�?�ff@   @��@33@&ff@333@Fff@S33@fff@s33@�33@���@�33@�  @�ff@�33@���@ə�@�33@�  @陚@�33A��A  A  A��A��A#33A+33A1��A9��AA��AFffANffAVffA\��AfffAl��Aq��Ay��A���A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444444141411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ j@ %@ �@ {@ O@ "�@ )�@ /�@ 6�@ >@ E�@ Q�@ _�@ m:@ {�@ ��@ ��@ ��@ �~@ �&@ ��@ �t@ ��@ �q@�@@�@-@:@F�@UU@b�@qS@~�@��@�H@��@��@��@�7@ލ@��@�~@�@*@!s@0x@>�@K@X�@g@t�@�@�@�U@��@��@��@Ӡ@��@��@�E@
�@�@&;@3�@A�@O0@\)@i�@v�@�p@��@��@��@�@ȴ@�[@�T@�@^@V@�@(�@8�@FQ@R�@_�@m�@z�@��@��@��@�~@�2@��@�#@��@�@�@�@g@+�@9X@H]@UU@a�@p�@�@��@��@��@��@��@�7@�;@��@��@�@@""@/�@>@Lu@Z�@ff@r�@�@�\@�a@��@�@Ĝ@��@��@�L@��@J@�@$�@3�@B8@N�@[z@j@ww@�p@��@��@�@�^@�c@�
@�T@�@  @�@�@'�@7L@D�@R�@`B@m�@|?@�7@��@��@�~@�w@�*@��@��@�q@	@	�@	g@	+�@	8�@	H]@	V@	bN@	p�@	}�@	��@	��@	��@	��@	@	ψ@	ލ@	��@	�,@
�@
*@
""@
0x@
=q@
I�@
X�@
g�@
t@
�W@
��@
�@
��@
�@
�J@
�O@
��@
�@
��@�@�@$.@3�@B�@N�@Z�@i!@ww@�|@��@�@�f@�@��@�
@�@�@@V@�@)�@8�@D�@Q=@`B@oF@|?@��@��@��@�!@�&@�*@�/@�y@��@@@g@+�@:@H]@S�@�T@&�@n�@��@��@@�@�p@�@�@Yn@�y@��@:�@�|@є@�@i!@��@@O0@�a@�@@;d@��@�C@�@e	@�@�@;d@�@Ĝ@1@K@�@�\@B@\�@�@�T@(G@o�@��@��@?}@��@�@v@Ji@��@�@ @g@��@� @>@�@�@@[z@��@��@1�@y�@��@ �@ Lu@ �#@ ܀@!"�@!k.@!��@!� @">�@"��@"�|@#{@#Z@#�@#�T@$(G@$oF@$�~@$�Y@%5�@%x�@%�@& �@&DD@&�+@&��@'V@'R�@'��@'�/@(!s@(c�@(��@(�m@)'�@)g�@)�A@)�T@* �@*\�@*��@*є@+�@+D�@+}�@+�9@+�(@, �@,X@,��@,��@,��@-/�@-e�@-��@-�7@.v@.:@.m�@.�(@.�@/b@/G�@/~�@/�F@/��@0'�@0_�@0��@0Ӡ@1V@1K@1�7@1�J@2�@2A�@2�@2��@2��@3=q@3|�@3�^@3�~@47�@4uk@4�9@4�@5/@5m�@5�A@5�@6"�@6`A@6�@6�#@7�@7Z@7��@7�C@8@8I�@8��@8�2@8��@95@@9r@9�Z@: @:�@;9X@;��@<H]@<��@=M�@=��@>M$@>�`@?~K@?�`@@|?@A�@A~K@BO@B�@C!s@C�^@D�@D�F@EP�@E��@FP�@F�@GV@G�Y@H��@H�E@I�T@J�@J��@K@K�~@L[@L�w@Ma�@M��@NqS@N܀@O|?@O�@P�@Q�F@S�@TZ�@U�w@W*@X`�@Y�@[�@\Z�@]�7@_v@`hs@a�@c$�@dX@eє@gj@hYn@i��@k�@lUU@m��@o@pg�@qƨ@s
�@tV@u��@u�E@v:@v�$@v��@w�@wG�@w�m@w�#@x�@xm�@x��@x�T@y:@yqS@y��@zj@z<@z�h@z��@{@{Wa@{�@{�H@|�@|i!@|�m@|�@}(G@}y�@}��@}��@~1�@~�d@~�+@ �@Ji@��@��@��G�O�G�O�@ �G�O�G�O�G�O�G�O�G�O�G�O�@ jG�O�@ �G�O�@ v@ �@ �@ 1@ �@ 
=@ �@ J@ V@ �@ �@ @ *@ �@ �@ �@ �@ �@  @ "�@ $�@ '�@ )�@ ,`@ .l@ 0x@ 3�@ 6�@ :@ <@ ?}@ B8@ E�@ H]@ K�@ O0@ Q=@ T�@ X@ Z�@ ^�@ a�@ c�@ g@ j@ m:@ p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԩ�Aԩ�AԬAԬAԮAԮAԮA԰!A԰!AԲ-A԰!A԰!Aԥ�Aԛ�AԓuA�l�A�33A�(�A� �A��A��A��A�{A�A���A���A�v�A�ĜA�VA�bNAɣ�A��mA�l�A�t�A��DA�^5A�M�A��!A�jA�(�A�  A��/A�n�A��jA��hA��A��HA���A���A�~�A��A���A�l�A���A�A�A��`A�O�A�JA�I�A��wA�z�A�S�A�A��wA�S�A�  A�7LA��A�XA��A���A�|�A��TA��A��mA��A��mA�
=A�|�A�G�A�oA��A���A���A�M�A��A���A�~�A�^5A�Q�A�5?A��A��FA��#A���A���A�M�A�$�A���A��
A�K�A�l�A��/A���A���A�?}A�C�A��\A��wA�G�A���A���A�^5A�z�A�+A�A�A��A���A���A�(�A�XA��wA�Q�A�bA�9XA�AdZA~bNA|�jAw��At1Ao�^Al^5Ajr�Ah��Af��Ac��Aa�A`1'A_�PA^~�A]�TA]\)A\��A[33AX�9AWARA�AOAM�wAK��AJ�\AG��AFjAEO�AC�ABQ�A@ȴA?�A>��A>$�A=��A<��A:�/A8Q�A8(�A81A5A3�#A2��A1K�A/l�A-7LA+��A+\)A+S�A+O�A+%A)�-A(��A({A&�A$��A#&�A"1A!?}A I�A��AXA+A�HAM�A�^An�AA�A�FA��A�yA��A�uA-A"�A�mA�\AAVAp�AffAJA��A33A��A�9A��AI�A+A
9XA	?}Az�A�mAC�A�A�wAx�A �@�@�~�@�{@��@�bN@��@���@��R@�"�@���@���@���@�Ĝ@�\@�{@��T@���@�x�@�7L@���@��
@��@�j@��H@�p�@�Ĝ@�Z@� �@��;@�^5@�h@�^5@�  @�ȴ@ǝ�@Å@�Ĝ@���@���@�33@���@�ȴ@��H@�dZ@���@��u@��@��H@�^5@��@��@�9X@�^5@��/@�
=@�M�@�{@�\)@�E�@�?}@���@�l�@���@���@��\@��@�K�@��7@��
@��/@�\)@��+@���@���@��@���@�n�@���@���@��D@+@}O�@z~�@x��@v�+@uV@s��@s�@r-@o�@o\)@m�T@l�@i��@gl�@d��@c��@b=q@`bN@_��@^ȴ@]�-@\I�@Z�H@Yhs@V�y@V@T1@SS�@Q�@N�y@N@K�
@KdZ@Jn�@Ihs@HQ�@G
=@E`B@C�m@C��@C"�@Ax�@@b@>��@=p�@;�@;C�@:�@:^5@9��@8�`@8�u@7+@6$�@4�j@3��@2�H@1��@0r�@/|�@-@-p�@,Z@+�m@*��@)��@)7L@(�u@'��@'�@%`B@#��@#t�@!��@!X@!hs@   @K�@ff@V@"�@��@&�@r�@ �@��@K�@�y@��@�@�D@�F@"�@J@X@b@|�@;d@@V@�@�
@
�@
�@	hs@r�@�;@ff@�h@�@�@9X@�F@@~�@��@��@��@%@ ��@ A�?��?�ƨ?��H?��9?���?�t�?��?�R?��?�ƨ?�~�?�Q�?��y?䛦?���?�Ĝ?��?�V?���?�ƨ?��H?ٺ^?���?�b?֧�?�`B?�9X?ӶF?�n�?щ7?�%?�|�?Η�?���?�O�?�j?�(�?˅?˅?��H?�~�?ɺ^?�X?���?�b?Ł?�z�?�S�?�n�?�&�?��w?���?��-?��?�(�?�dZ?�?���?�~�?���?���?�~�?���?�?�?�C�?���?�1?�(�?��?�O�?���?���?��?�{?�5??�V?�v�?�v�?���?��R?��?���?��?�;d?�|�?�|�?��w?��w?�  ?�  ?� �?�bN?�A�?���?���?�Ĝ?��`?�%?�&�?�G�?�G�?��7?���?���?��?�J?�-?�M�?�n�?\Aԧ�Aԥ�Aԧ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԧ�AԬAԩ�Aԩ�Aԩ�Aԧ�Aԩ�Aԩ�AԬAԩ�AԬAԩ�Aԩ�AԬAԬAԩ�AԬAԬAԬAԮAԮAԬAԮAԮAԮAԬAԮAԮA԰!A԰!A԰!A԰!A԰!AԲ-AԲ-AԲ-A԰!A԰!AԲ-A԰!A԰!A԰!A԰!AԮAԩ�Aԧ�Aԣ�Aԟ�Aԝ�Aԙ�Aԛ�Aԙ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Aԩ�Aԩ�AԬAԬAԮAԮAԮA԰!A԰!AԲ-A԰!A԰!Aԥ�Aԛ�AԓuA�l�A�33A�(�A� �A��A��A��A�{A�A���A���A�v�A�ĜA�VA�bNAɣ�A��mA�l�A�t�A��DA�^5A�M�A��!A�jA�(�A�  A��/A�n�A��jA��hA��A��HA���A���A�~�A��A���A�l�A���A�A�A��`A�O�A�JA�I�A��wA�z�A�S�A�A��wA�S�A�  A�7LA��A�XA��A���A�|�A��TA��A��mA��A��mA�
=A�|�A�G�A�oA��A���A���A�M�A��A���A�~�A�^5A�Q�A�5?A��A��FA��#A���A���A�M�A�$�A���A��
A�K�A�l�A��/A���A���A�?}A�C�A��\A��wA�G�A���A���A�^5A�z�A�+A�A�A��A���A���A�(�A�XA��wA�Q�A�bA�9XA�AdZA~bNA|�jAw��At1Ao�^Al^5Ajr�Ah��Af��Ac��Aa�A`1'A_�PA^~�A]�TA]\)A\��A[33AX�9AWARA�AOAM�wAK��AJ�\AG��AFjAEO�AC�ABQ�A@ȴA?�A>��A>$�A=��A<��A:�/A8Q�A8(�A81A5A3�#A2��A1K�A/l�A-7LA+��A+\)A+S�A+O�A+%A)�-A(��A({A&�A$��A#&�A"1A!?}A I�A��AXA+A�HAM�A�^An�AA�A�FA��A�yA��A�uA-A"�A�mA�\AAVAp�AffAJA��A33A��A�9A��AI�A+A
9XA	?}Az�A�mAC�A�A�wAx�A �@�@�~�@�{@��@�bN@��@���@��R@�"�@���@���@���@�Ĝ@�\@�{@��T@���@�x�@�7L@���@��
@��@�j@��H@�p�@�Ĝ@�Z@� �@��;@�^5@�h@�^5@�  @�ȴ@ǝ�@Å@�Ĝ@���@���@�33@���@�ȴ@��H@�dZ@���@��u@��@��H@�^5@��@��@�9X@�^5@��/@�
=@�M�@�{@�\)@�E�@�?}@���@�l�@���@���@��\@��@�K�@��7@��
@��/@�\)@��+@���@���@��@���@�n�@���@���@��D@+@}O�@z~�@x��@v�+@uV@s��@s�@r-@o�@o\)@m�T@l�@i��@gl�@d��@c��@b=q@`bN@_��@^ȴ@]�-@\I�@Z�H@Yhs@V�y@V@T1@SS�@Q�@N�y@N@K�
@KdZ@Jn�@Ihs@HQ�@G
=@E`B@C�m@C��@C"�@Ax�@@b@>��@=p�@;�@;C�@:�@:^5@9��@8�`@8�u@7+@6$�@4�j@3��@2�H@1��@0r�@/|�@-@-p�@,Z@+�m@*��@)��@)7L@(�u@'��@'�@%`B@#��@#t�@!��@!X@!hs@   @K�@ff@V@"�@��@&�@r�@ �@��@K�@�y@��@�@�D@�F@"�@J@X@b@|�@;d@@V@�@�
@
�@
�@	hs@r�@�;@ff@�h@�@�@9X@�F@@~�@��@��@��@%@ ��@ A�?��?�ƨ?��H?��9?���?�t�?��?�R?��?�ƨ?�~�?�Q�?��y?䛦?���?�Ĝ?��?�V?���?�ƨ?��H?ٺ^?���?�b?֧�?�`B?�9X?ӶF?�n�?щ7?�%?�|�?Η�?���?�O�?�j?�(�?˅?˅?��H?�~�?ɺ^?�X?���?�b?Ł?�z�?�S�?�n�?�&�?��w?���?��-?��?�(�?�dZ?�?���?�~�?���?���?�~�?���?�?�?�C�?���?�1?�(�?��?�O�?���?���?��?�{?�5??�V?�v�?�v�?���?��R?��?���?��?�;d?�|�?�|�?��w?��w?�  ?�  ?� �?�bN?�A�?���?���?�Ĝ?��`?�%?�&�?�G�?�G�?��7?���?���?��?�J?�-?�M�?�n�?\Aԧ�Aԥ�Aԧ�Aԩ�Aԩ�Aԩ�Aԩ�Aԩ�Aԧ�AԬAԩ�Aԩ�Aԩ�Aԧ�Aԩ�Aԩ�AԬAԩ�AԬAԩ�Aԩ�AԬAԬAԩ�AԬAԬAԬAԮAԮAԬAԮAԮAԮAԬAԮAԮA԰!A԰!A԰!A԰!A԰!AԲ-AԲ-AԲ-A԰!A԰!AԲ-A԰!A԰!A԰!A԰!AԮAԩ�Aԧ�Aԣ�Aԟ�Aԝ�Aԙ�Aԛ�Aԙ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Br�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bv�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bs�Bs�Br�Bp�Bv�B�VB��B��B�B�B��B��BB1B	7B
=BDBJB\BuB�B!�B&�B&�B,B1'B8RB8RB8RB@�BC�BD�BI�BK�BQ�BW
BYBYB[#B\)B_;BaHBe`BjBiyBhsBgmBffBe`BgmBffBaHB`BB[#BVBJ�BF�B>wB8RB.B �B�BoB\BVBPB
=B1B1BB�B�TB�NB�HB�B�B�BÖB��B�hB��B��B��B�uB�1B� Bt�B^5B1'B#�BB
�HB
��B
�XB
��B
��B
�PB
�B
w�B
n�B
`BB
XB
S�B
I�B
6FB
%B	�HB	�FB	�!B	��B	�oB	�+B	q�B	hsB	aHB	\)B	XB	T�B	R�B	R�B	Q�B	E�B	:^B	;dB	9XB	)�B	"�B	�B	%B��B��B�B�`B�/B�B��B��B��BĜB�dB�XB�?B�3B��B��B��B�{B�bB�+B�%B�%B�%B�B�B�B~�By�Bp�Bt�Bl�Bk�BiyBe`BdZBdZBbNBaHB^5B\)BXBVBT�BN�BP�BN�BM�BK�BI�BI�BJ�BJ�BF�BE�BF�BE�BD�BC�BD�BC�BC�BF�BJ�BL�BN�BM�BQ�BO�BL�BG�B=qBA�B?}B>wB<jB=qB>wB>wB<jB;dB49B33B2-B2-B0!B49B5?B49B49B49B33B49B49B6FB7LB:^B<jB=qB=qB=qB=qB?}BA�BW
BffB|�B�+B�oB��B��B�LBȴB��B�
B�B��B	%B	hB	49B	5?B	7LB	F�B	J�B	W
B	^5B	o�B	s�B	|�B	~�B	�B	�bB	��B	��B	��B	��B	�!B	�3B	�qB	��B	��B	�
B	�NB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
1B

=B
JB
PB
VB
hB
bB
oB
uB
�B
�B
�B
�B
 �B
"�B
"�B
#�B
$�B
%�B
%�B
&�B
+B
,B
,B
/B
1'B
49B
49B
5?B
6FB
6FB
7LB
9XB
9XB
;dB
<jB
=qB
=qB
>wB
@�B
A�B
B�B
E�B
E�B
F�B
F�B
G�B
H�B
G�B
H�B
J�B
K�B
M�B
M�B
N�B
P�B
Q�B
R�B
R�B
T�B
T�B
W
B
XB
XB
XB
YB
ZB
[#B
^5B
^5B
_;B
`BB
_;B
aHB
aHB
aHB
bNB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
l�B
k�B
m�B
l�B
m�B
n�B
p�B
q�B
p�B
q�B
r�B
s�B
s�B
t�B
u�B
v�B
w�B
w�B
z�B
z�B
z�B
{�B
|�B
}�B
}�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�+B
�+B
�=B
�DB
�JB
�PB
�VB
�\B
�hB
�oB
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
��B
�B
�B
�B
�B
�B
�'B
�-B
�3B
�3B
�?B
�?B
�FB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jBr�Bs�Br�Br�Br�Br�Br�Bs�Bs�Br�Br�Br�Br�Bs�Br�Br�Br�Bs�Br�Br�Bs�Bs�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Bs�Bs�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Bt�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bq�Bq�Bp�Bn�Bt�B�JB��B��B�sB�B�B��BB%B+B1B	7B
=BPBhB�B�B$�B$�B)�B/B6FB6FB6FB>wBA�BB�BG�BI�BO�BT�BW
BW
BYBZB]/B_;BcTBhsBgmBffBe`BdZBcTBe`BdZB_;B^5BYBS�BH�BD�B<jB6FB,B�B{BbBPBJBDB1B%B%B��B�sB�HB�BB�;B�B�B�B��B��B�\B�{B��B��B�hB�%B}�Br�B\)B/B!�BB
�;B
��B
�LB
��B
��B
�DB
�B
u�B
l�B
^5B
VB
Q�B
G�B
49B
B	�;B	�9B	�B	��B	�bB	�B	o�B	ffB	_;B	ZB	VB	R�B	P�B	P�B	O�B	C�B	8RB	9XB	7LB	'�B	 �B	uB	B��B��B�B�TB�#B��B��B��BȴBB�XB�LB�3B�'B��B��B��B�oB�VB�B�B�B�B�B�B�B|�Bw�Bn�Br�BjBiyBgmBcTBbNBbNB`BB_;B\)BZBVBS�BR�BL�BN�BL�BK�BI�BG�BG�BH�BH�BD�BC�BD�BC�BB�BA�BB�BA�BA�BD�BH�BJ�BL�BK�BO�BM�BJ�BE�B;dB?}B=qB<jB:^B;dB<jB<jB:^B9XB2-B1'B0!B0!B.B2-B33B2-B2-B2-B1'B2-B2-B49B5?B8RB:^B;dB;dB;dB;dB=qB?}BT�BdZBz�B�B�bB��B��B�?BƨB��B��B�B�B	B	\B	2-B	33B	5?B	D�B	I�B	VB	]/B	n�B	r�B	{�B	}�B	�B	�\B	��B	��B	��B	��B	�B	�-B	�jB	ɺB	��B	�B	�HB	�`B	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
+B
	7B
DB
JB
PB
bB
\B
hB
oB
�B
�B
�B
�B
�B
!�B
!�B
"�B
#�B
$�B
$�B
%�B
)�B
+B
+B
.B
0!B
33B
33B
49B
5?B
5?B
6FB
8RB
8RB
:^B
;dB
<jB
<jB
=qB
?}B
@�B
A�B
D�B
D�B
E�B
E�B
F�B
G�B
F�B
G�B
I�B
J�B
L�B
L�B
M�B
O�B
Q�B
R�B
R�B
T�B
T�B
W
B
XB
XB
XB
YB
ZB
[#B
^5B
^5B
_;B
`BB
_;B
aHB
aHB
aHB
bNB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
l�B
k�B
m�B
l�B
m�B
n�B
p�B
q�B
p�B
q�B
r�B
s�B
s�B
t�B
u�B
v�B
w�B
w�B
z�B
z�B
z�B
{�B
|�B
}�B
}�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�+B
�+B
�=B
�DB
�JB
�PB
�VB
�\B
�hB
�oB
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
��B
�B
�B
�B
�B
�B
�-B
�9B
�?B
�?B
�LB
�LB
�RB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�qB
�qB
�wB
�wB
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
�}B
�}B
�}B
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
�}B
�}B
�}B
�}B
�}B
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
�}Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bq�Bp�Bp�Bq�Bq�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Bq�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810051701492021061413524120210614135241202106141746452021061417464520210614174645201810051701492021061413524120210614135241202106141746452021061417464520210614174645PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018100517014920181005170149  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100517014920181005170149QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100517014920181005170149QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015320210722160153IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                