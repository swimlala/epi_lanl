CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-01-12T08:00:39Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190112080039  20210617131513  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               8   8DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؚ�����@ؚ�����11  @ؚ���@�@ؚ���@�@6N��i�;@6N��i�;�cݦ���cݦ��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @@  @�  @�33@�  @���A   A33A$��A@  Ac33A�  A�33A�33A�  A���A���A�  A���B ��BffBffB  B��B'��B0  B8��B@ffBH  BP  BW��B_��Bg��Bo��Bx  B�ffB�33B�  B�33B�33B�33B�33B�33B�  B�33B�  B���B�  B�33B�33B�  B�  B�33B�33B�  B���B�ffB�ffB�  B�ffB�33B癚B���B���B�  B�33B�33C 33CL�C�C��C�fC	�fC�fC�fC�fC  C  C  C�C�C�C�C 33C"  C#��C&�C(  C)�fC,33C.33C/�fC1��C433C6L�C833C:L�C<33C>�C@�CB  CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX  CY�fC[�fC^33C`33Cb  CdL�Cf33Ch�Cj33Cl�Cm�fCp  Cr�Ct�Cv33CxL�CzffC|�C}��C��C��fC��fC��3C�  C�  C�  C�  C��3C�  C�  C�  C��C��C��C��C�&fC�&fC��C��fC��fC��3C�  C��C�&fC�33C��C��3C��C��C�&fC��C��3C��C��C��C�&fC��C��fC�  C��C��C��C��3C��C�&fC��C�  C��C�  C��3C��C��C��C��3C�  C��C�&fC��C��3C�  C��C��C�33C��C��3C�  C��C�&fC�&fC��C��fC��3C��3C��3C�  C��C��C��C��C��C��C��C��C��C�  C�  C��3C��3C��3C��fC��fC��fC��fC�ٚC�  C��C��C��C��C�&fC��C��C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C��C��C��C��C�  C�&fC�&fC�33C�@ C��fDffD��D	�3D,�D�fD@ DٚDFfD�fDL�D��D Y�D"� D%` D'ٚD*S3D,��D/�D1� D3��D6L�D8�fD;fD=ffD?��DB  DD9�DF�3DH�fDKL�DM��DP  DRS3DT�3DWfDY` D[��D^&fD`� Db� De,�Dg��Di�fDlL�Dn��Dq3Ds` Du��Dx  DzL�D|9�D~� D�ffD��fD��3D���D��3D��fD�fD�6fD�P D�i�D��fD���D�� D���D��fD�fD�  D�@ D�c3D��3D��3D��3D��3D�fD�,�D�S3D�y�D��fD���D�ٚD���D�#3D�C3D�` D�y�D��3D���D�ɚD��D�	�D�)�D�I�D�l�D�� D��3D�ٚD�  D�#3D�@ D�Y�D�y�D���D���D�� D���D���D�	�D�fD�#3D�0 D�9�D�I�D�L�D�VfD�\�D�` D�\�D�\�D�S3D�VfD�S3D�I�D�<�D�6fD�0 D�#3D�fD�fD��fD��fD�� D�� Dس3D٦fDڠ Dۙ�Dܓ3D݃3D�y�D�p D�l�D�s3D�l�D�l�D�ffD�\�D�\�D�\�D�\�D�\�D�S3D�L�D�I�D�@ D�33D�)�D�&fD�&fD��D��D�fD� D� D� D� D� D�3D���D���D�fD�fD�3E �3E�fE� EFfET�E` E� E�E
ffEd�E�3E�fE\�EX E� EC3E9�E��E��E�fEQ�E�3E�fE�3E Ea�E � E!�3E#0 E$nfE%�fE'3E(H E)��E*� E,  E-` E.�fE/�fE13E20 E3P E4��E6�E74�E8X E9��E:� E<3E?\�EBk3EEvfEH� EK��EO�EQ��EU�EX4�E[a�E^��Ea�3Ed�fEg� Ej��En9�En�fEoQ�EoٚEp��Eq;3EqɚEr�fEs,�Es�3EtH >���>L��>���>���>���>L��>L��>���>L��>L��=���>���>L��>���>���>���>L��>L��>���>���>���>���>���>���>���>���?333?333?333?fff?���?�ff?�  ?�ff@   @33@&ff@333@Fff@Y��@l��@�  @���@���@�  @���@���@�ff@�33@�33@�33A   A  AffAffA   A(  A1��A8  A>ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411444144414144441144441414411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?L��?�  @   @`  @�  @�33@�  @���A  A33A,��AH  Ak33A�  A�33A�33A�  Ař�A���A�  A���B��B
ffBffB  B!��B)��B2  B:��BBffBJ  BR  BY��Ba��Bi��Bq��Bz  B�ffB�33B�  B�33B�33B�33B�33B�33B�  B�33B�  B���B�  B�33B�33B�  B�  B�33B�33B�  B���B�ffB�ffB�  B�ffB�33B虚B���B���B�  B�33B�33C �3C��C��CL�CffC
ffCffCffCffC� C� C� C��C��C��C��C �3C"� C$L�C&��C(� C*ffC,�3C.�3C0ffC2L�C4�3C6��C8�3C:��C<�3C>��C@��CB� CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX� CZffC\ffC^�3C`�3Cb� Cd��Cf�3Ch��Cj�3Cl��CnffCp� Cr��Ct��Cv�3Cx��Cz�fC|��C~L�C�&fC�&fC�&fC�33C�@ C�@ C�@ C�@ C�33C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�@ C�L�C�ffC�s3C�Y�C�33C�L�C�Y�C�ffC�L�C�33C�L�C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�Y�C�L�C�33C�L�C�ffC�L�C�@ C�Y�C�@ C�33C�L�C�Y�C�L�C�33C�@ C�Y�C�ffC�L�C�33C�@ C�Y�C�Y�C�s3C�L�C�33C�@ C�Y�C�ffC�ffC�L�C�&fC�33C�33C�33C�@ C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�@ C�@ C�33C�33C�33C�&fC�&fC�&fC�&fC��C�@ C�Y�C�L�C�L�C�Y�C�ffC�L�C�L�C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�L�C�Y�C�L�C�L�C�Y�C�Y�C�L�C�Y�C�Y�C�@ C�ffC�ffC�s3C�� C�&fD�fD�D	�3DL�D�fD` D��DffD�fDl�D��D y�D#  D%� D'��D*s3D,��D/9�D1� D4�D6l�D8�fD;&fD=�fD?ٚDB  DDY�DF�3DIfDKl�DM��DP  DRs3DT�3DW&fDY� D[��D^FfD`� Dc  DeL�Dg��DjfDll�DnٚDq33Ds� DuٚDx  Dzl�D|Y�D~� D�vfD��fD��3D���D��3D�fD�&fD�FfD�` D�y�D��fD���D�� D���D��fD�fD�0 D�P D�s3D��3D��3D��3D��3D�fD�<�D�c3D���D��fD�ɚD��D�	�D�33D�S3D�p D���D��3D���D�ٚD���D��D�9�D�Y�D�|�D�� D��3D��D� D�33D�P D�i�D���D���D�ɚD�� D���D��D��D�&fD�33D�@ D�I�D�Y�D�\�D�ffD�l�D�p D�l�D�l�D�c3D�ffD�c3D�Y�D�L�D�FfD�@ D�33D�&fD�fD�fD��fD�� D�� D��3DٶfDڰ D۩�Dܣ3Dݓ3Dމ�D߀ D�|�D�3D�|�D�|�D�vfD�l�D�l�D�l�D�l�D�l�D�c3D�\�D�Y�D�P D�C3D�9�D�6fD�6fD�)�D�,�D�&fD�  D�  D�  D�  D�  D�#3D�	�D��D�fD�fD�#3E �3E�fE� ENfE\�Eh E� E�E
nfEl�E�3E�fEd�E` E� EK3EA�E��E��E�fEY�E�3E�fE�3E  Ei�E � E!�3E#8 E$vfE%�fE'3E(P E)��E*� E,( E-h E.�fE/�fE13E28 E3X E4��E6�E7<�E8` E9��E:� E<3E?d�EBs3EE~fEH� EKɚEO	�EQ��EU!�EX<�E[i�E^��Ea�3Ed�fEg� Ej��EnA�En�fEoY�Eo�Ep��EqC3EqњEr�fEs4�Es�3EtP G�O�?333?L��G�O�G�O�G�O�?333G�O�G�O�G�O�?��G�O�?333G�O�G�O�G�O�G�O�?333?L��G�O�G�O�G�O�G�O�?L��G�O�?fffG�O�G�O�?���?�33?ٙ�?�ff@   @33@   @333@Fff@S33@fff@y��@�ff@�  @���@���@�  @���@ə�@�ff@�33@�33A��A  A  AffAffA(  A0  A9��A@  AFffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411444144414144441144441414411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @ @ �@ �@ {@ O@ "�@ (�@ /@ 6�@ >�@ FQ@ Q�@ `�@ m:@ z3@ ��@ �0@ �5@ �-@ �&@ �|@ ��@ ��@ �q@j@b@
@,`@;d@H]@UU@b�@o�@}�@�D@��@�A@�F@�>@�7@ލ@�4@��@�@*@""@0x@=q@Ji@X�@g@t�@��@�\@��@�Y@�R@�J@��@�@��@��@
�@�@$�@2�@@�@O0@\�@k.@y�@��@�h@��@�f@�@ȴ@�[@�@�Y@  @V@�@)�@7L@E�@Q�@^5@m�@z�@��@��@�5@��@��@�*@܀@�y@��@�@�@g@,`@:�@H]@V@c�@qS@~�@��@�H@��@��@@ψ@�/@��@��@�@�@#�@0x@>�@K�@X@ff@t�@�d@��@�@�f@�@Ĝ@�C@��@�@��@
=@�@%�@33@@,@N�@\)@i�@x&@��@�$@��@�!@��@�@խ@�T@�@  @V@[@+�@7�@C�@R�@`�@oF@{�@��@��@��@��@�2@�|@�@�@�q@	�@	�@	
@	-@	<@	H]@	UU@	dZ@	p�@	}�@	��@	��@	��@	�9@	@	є@	��@	�4@	�~@
�@
�@
#�@
2�@
>@
Ji@
X�@
g�@
v@
��@
�@
��@
��@
��@
�J@
Ӡ@
��@
�L@
��@�@B@&�@3�@B8@O�@\)@i�@v�@�p@�@�@��@�^@�@��@�@�@ �@V@�@+@7L@D�@Q�@`B@m:@z�@��@�0@��@�~@�w@��@�#@�y@�q@@o@ @-@;d@I@UU@e	@r�@�@�\@�<@g@e�@��@�@:�@~K@�J@�@K�@��@��@�@_�@��@�m@+@k.@�f@��@1'@r@�-@�@3�@s_@�~@�@@.l@m�@�r@�L@/�@oF@�!@�@/�@r@�-@�Y@33@r@��@�@4�@v�@��@��@5�@t@��@�m@%�@dZ@��@ލ@�@V@�#@є@@K@�+@��@��@:@v�@��@�L@,`@i�@��@�`@ "�@ `A@ ��@ ��@!�@!Yn@!�<@!��@"@"P�@"��@"�|@#
�@#G�@#��@#��@#��@$8�@$v@$��@$��@%.l@%l�@%��@%��@&'�@&ff@&��@&�H@'[@'Z�@'�<@'խ@(@(M�@(��@(�2@(��@)3�@)m:@)��@)��@*6@*O�@*��@*�&@*�@++�@+`A@+��@+�|@,@,5�@,k.@,�m@,�O@-1@-;d@-n�@-��@-Ӡ@.�@.:�@.n�@.��@.�@/V@/A�@/v@/��@/��@0�@0M�@0�p@0��@0�@@1$�@1[z@1�@1ȴ@1�E@22�@2hs@2�@2��@3v@3;d@3r@3��@3�/@4o@4G�@4~K@4��@4�@5""@5Yn@5��@5��@5��@61'@6j@6�;@7T�@7��@8o�@8�T@9UU@9�,@:g@;�@;y�@<�@<��@=+@=�0@>6�@>��@?> @?׹@@> @@�[@Aj@A��@B[z@B�y@Ct@D �@D��@E�@E�m@F(G@F�9@GB8@G��@HWb@H�@Ip�@I�,@J}�@K�@K�|@L�@L��@M+@M�M@N+@N�A@OI�@O@P;d@Q��@R�@T@,@U�@V�@XR�@Y�i@Z�4@\?}@]�I@^�q@`>�@a��@b��@d5@@e��@e��@f�@fV@f�!@f��@g)�@g�p@g�2@g��@h:G�O�@ ^@ G�O�G�O�G�O�@ ^G�O�G�O�G�O�@  �G�O�@ ^G�O�G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�G�O�@ G�O�@ �G�O�G�O�@ �@ %@ 1@ �@ 
=@ J@ �@ �@ �@ @ *@ 6@ B@ O@ 
@  �@ ""@ $�@ '�@ *S@ -@ 0x@ 3�@ 6�@ :@ <�@ @,@ DD@ G�@ K�@ N�@ Q=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�G�A�E�A�G�A�I�A�K�A�I�A�K�A�M�A�Q�A�M�A�C�A�bA���A��A´9A�^5A�G�A�C�A�?}A�=qA�=qA�5?A�33A�(�A�"�A��A�bA�1A�1A�%A�A�  A���A���A���A���A���A���A���A���A��yA��#A���A��!A�S�A��A���A��;A��A�G�A�bNA��wA��HA�ĜA�=qA���A�(�A�ĜA�dZA�A��A�
=A�ffA�p�A��DA��A���A��A��wA�p�A�A�A���A���A���A��HA�v�A��/A��A��A�dZA���A��/A�C�A��A���A�p�A�bA���A�G�A��
A���A�|�A�jA�^5A�G�A�JA��PA�K�A��mA���A�O�A�;dA�ȴA���A�5?A��A�$�A��-A��HA�|�A��`A��/A�~�A��;A���A�33A��A��A}O�A{;dAy�Aw��Au?}ArM�Ap��An�uAk�-Ai\)AiAhv�Ag�-Ad�\A_�A]\)A\��AZ��AXA�AV�yAR�RAM�TAK��AJ�/AJ1AH��AE�TACx�ABVAA?}A@A�A=�A;�PA8�9A6�A5�;A5x�A4^5A3�mA3��A2�A2I�A1��A1;dA0�yA0�DA.�jA-��A-t�A-/A, �A+oA*A�A)�7A(�HA'��A&~�A$��A$1'A#VA!��A!33A!oA �HA Q�A 1A|�A�mAffA��A/A�A&�A�AI�A�A�jA�`AA-A�A��AVAE�A-A{A��A/A�Az�A=qA�-AVA
9XA-AA�A;dA"�A�`A|�A��AdZA ��A -@��y@�ff@�`B@�C�@�`B@�r�@��\@���@��@��h@�j@�R@���@��@�b@�33@�=q@�n�@�hs@��/@�C�@��@�&�@��
@�^5@� �@�v�@�(�@�%@أ�@�Z@�"�@�=q@�bN@�+@�
=@��@�{@�`B@�-@���@��T@�;d@���@���@�O�@��@� �@��H@� �@�~�@�ff@��@��D@��^@��@���@��@�\)@�^5@�Ĝ@���@�b@��-@��P@��#@�?}@�9X@���@�@���@�dZ@��R@�x�@��j@�S�@�ȴ@�@���@���@�l�@��R@�5?@�7L@�Ĝ@|�@~5?@|Z@{"�@y�^@w\)@u�@t�D@r��@pA�@n@lI�@j=q@i%@h�9@gK�@d�@c33@b�!@a�#@a%@_�@_+@]�h@\j@[S�@X  @W;d@U/@T��@S33@R��@O�w@NV@M��@L�@J��@J-@IX@H�9@G�@F��@F@E`B@E/@D(�@B=q@A%@?�@?
=@=p�@<9X@;@:J@8��@8b@7�w@7\)@6�+@5?}@3�
@3C�@2�@1��@/�@/K�@.��@-�@,�@+S�@)�#@'|�@&5?@%�h@%/@$��@"�!@!�#@!X@!�@�@��@��@?}@�@j@S�@�@��@Ĝ@�@�@��@��@�-@�j@ƨ@"�@�@=q@hs@��@bN@�w@\)@+@
=@{@p�@�/@ƨ@
��@
�@	�7@	�@r�@�w@|�@��@v�@ff@$�@p�@�@Z@�F@o@=q@%@ bN?�j?��?���?���?�S�?��?��;?�h?��m?�=q?�1'?�+?��?�Z?��
?�o?�&�?�;d?ݲ-?���?�1?ڟ�?ٺ^?���?�K�?�?��/?�t�?Ұ!?щ7?У�?�  ?��?�V?�p�?�ƨ?�C�?�~�?�~�?ə�?���?�b?��?�K�?Ƨ�?�?Ł?�n�?��?��?�V?��?�p�?��D?�I�?�1?�1?�?���?�^5?��^?��^?�x�?�X?�x�?�x�?�x�?�x�?�x�?�X?�7L?��A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�I�A�M�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�I�A�G�A�I�A�I�A�K�A�K�A�K�A�G�A�G�A�K�A�K�A�G�A�G�A�C�A�E�A�G�A�C�A�G�A�E�A�E�A�G�A�I�A�K�A�I�A�M�A�K�A�K�A�K�A�G�A�K�A�K�A�O�A�K�A�M�A�Q�A�O�A�M�A�M�A�M�A�K�A�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A�K�A�G�A�E�A�G�A�I�A�K�A�I�A�K�A�M�A�Q�A�M�A�C�A�bA���A��A´9A�^5A�G�A�C�A�?}A�=qA�=qA�5?A�33A�(�A�"�A��A�bA�1A�1A�%A�A�  A���A���A���A���A���A���A���A���A��yA��#A���A��!A�S�A��A���A��;A��A�G�A�bNA��wA��HA�ĜA�=qA���A�(�A�ĜA�dZA�A��A�
=A�ffA�p�A��DA��A���A��A��wA�p�A�A�A���A���A���A��HA�v�A��/A��A��A�dZA���A��/A�C�A��A���A�p�A�bA���A�G�A��
A���A�|�A�jA�^5A�G�A�JA��PA�K�A��mA���A�O�A�;dA�ȴA���A�5?A��A�$�A��-A��HA�|�A��`A��/A�~�A��;A���A�33A��A��A}O�A{;dAy�Aw��Au?}ArM�Ap��An�uAk�-Ai\)AiAhv�Ag�-Ad�\A_�A]\)A\��AZ��AXA�AV�yAR�RAM�TAK��AJ�/AJ1AH��AE�TACx�ABVAA?}A@A�A=�A;�PA8�9A6�A5�;A5x�A4^5A3�mA3��A2�A2I�A1��A1;dA0�yA0�DA.�jA-��A-t�A-/A, �A+oA*A�A)�7A(�HA'��A&~�A$��A$1'A#VA!��A!33A!oA �HA Q�A 1A|�A�mAffA��A/A�A&�A�AI�A�A�jA�`AA-A�A��AVAE�A-A{A��A/A�Az�A=qA�-AVA
9XA-AA�A;dA"�A�`A|�A��AdZA ��A -@��y@�ff@�`B@�C�@�`B@�r�@��\@���@��@��h@�j@�R@���@��@�b@�33@�=q@�n�@�hs@��/@�C�@��@�&�@��
@�^5@� �@�v�@�(�@�%@أ�@�Z@�"�@�=q@�bN@�+@�
=@��@�{@�`B@�-@���@��T@�;d@���@���@�O�@��@� �@��H@� �@�~�@�ff@��@��D@��^@��@���@��@�\)@�^5@�Ĝ@���@�b@��-@��P@��#@�?}@�9X@���@�@���@�dZ@��R@�x�@��j@�S�@�ȴ@�@���@���@�l�@��R@�5?@�7L@�Ĝ@|�@~5?@|Z@{"�@y�^@w\)@u�@t�D@r��@pA�@n@lI�@j=q@i%@h�9@gK�@d�@c33@b�!@a�#@a%@_�@_+@]�h@\j@[S�@X  @W;d@U/@T��@S33@R��@O�w@NV@M��@L�@J��@J-@IX@H�9@G�@F��@F@E`B@E/@D(�@B=q@A%@?�@?
=@=p�@<9X@;@:J@8��@8b@7�w@7\)@6�+@5?}@3�
@3C�@2�@1��@/�@/K�@.��@-�@,�@+S�@)�#@'|�@&5?@%�h@%/@$��@"�!@!�#@!X@!�@�@��@��@?}@�@j@S�@�@��@Ĝ@�@�@��@��@�-@�j@ƨ@"�@�@=q@hs@��@bN@�w@\)@+@
=@{@p�@�/@ƨ@
��@
�@	�7@	�@r�@�w@|�@��@v�@ff@$�@p�@�@Z@�F@o@=q@%@ bN?�j?��?���?���?�S�?��?��;?�h?��m?�=q?�1'?�+?��?�Z?��
?�o?�&�?�;d?ݲ-?���?�1?ڟ�?ٺ^?���?�K�?�?��/?�t�?Ұ!?щ7?У�?�  ?��?�V?�p�?�ƨ?�C�?�~�?�~�?ə�?���?�b?��?�K�?Ƨ�?�?Ł?�n�?��?��?�V?��?�p�?��D?�I�?�1?�1?�?���?�^5?��^?��^?�x�?�X?�x�?�x�?�x�?�x�?�x�?�X?�7L?��A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�I�A�M�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�I�A�G�A�I�A�I�A�K�A�K�A�K�A�G�A�G�A�K�A�K�A�G�A�G�A�C�A�E�A�G�A�C�A�G�A�E�A�E�A�G�A�I�A�K�A�I�A�M�A�K�A�K�A�K�A�G�A�K�A�K�A�O�A�K�A�M�A�Q�A�O�A�M�A�M�A�M�A�K�A�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BS�BS�BS�BS�BS�BS�BT�BT�BS�BR�BR�BS�B\)B`BBbNBjBw�B{�B|�B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B~�B}�B~�B}�B~�B~�B~�B~�B~�B}�B~�B~�B�B|�B� B{�B�B�B�B~�B�B�B�B�B�B�B�B�B�B�B�B� B}�Bz�Bw�Bx�Bw�Bw�Bw�Bu�Bs�BhsBbNB_;BYBT�BO�BK�B5?B"�B�B�BJB%B  B��B�B�B�B�yB�mB�fB�ZB�/B�
B�FB��B�PB�%BiyB>wB6FB+B �B�BVBB
��B
�B
�!B
��B
�uB
�VB
q�B
e`B
M�B
@�B
1'B
%�B
�B
	7B
  B	�B	�ZB	��B	ÖB	�wB	�RB	�'B	�oB	y�B	k�B	dZB	S�B	D�B	:^B	�B	  B�B�B�ZB�#BɺB�qB�?B�!B��B��B��B�PB�+B�B�B}�B|�By�Bx�Bv�Bu�Bs�Br�Bo�Bo�Bo�Bo�Bm�BjBjBhsBffBcTBbNBZB_;B]/BZBXBXBW
BVBVBVBS�BM�BT�BR�BP�BQ�BO�BN�BM�BL�BI�BF�BF�BA�B=qB?}B>wB=qB<jB;dB:^B;dB9XB:^B9XB9XB9XB7LB8RB6FB5?B49B33B49B2-B2-B49B49B7LB7LB7LB7LB8RB7LB7LB7LB6FB6FB6FB6FB7LB6FB6FB6FB49B6FB7LB5?B6FB7LB7LB8RB8RB9XB:^B9XB=qB<jB;dB;dB=qB?}BC�BC�BC�BA�BP�BZBgmBt�B{�B�\B�{B��B��B�jB��B�TB�B	+B	PB	�B	.B	;dB	B�B	O�B	^5B	n�B	n�B	{�B	�B	�=B	�DB	�bB	��B	��B	�B	�-B	�?B	�wB	ǮB	��B	��B	��B	�B	�
B	�B	�)B	�;B	�BB	�sB	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
1B

=B

=B
PB
VB
VB
bB
oB
uB
�B
�B
�B
�B
 �B
"�B
"�B
"�B
&�B
'�B
+B
(�B
+B
+B
.B
1'B
33B
33B
5?B
6FB
7LB
7LB
8RB
;dB
=qB
=qB
?}B
@�B
B�B
B�B
D�B
C�B
E�B
F�B
G�B
H�B
J�B
I�B
K�B
K�B
L�B
M�B
N�B
O�B
P�B
P�B
S�B
S�B
R�B
T�B
VB
VB
XB
XB
XB
YB
ZB
YB
[#B
[#B
\)B
]/B
^5B
^5B
_;B
aHB
bNB
bNB
dZB
dZB
e`B
ffB
gmB
gmB
hsB
hsB
iyB
k�B
k�B
l�B
l�B
m�B
n�B
o�B
n�B
o�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
u�B
u�B
v�B
v�B
w�B
w�B
x�B
y�B
y�B
y�B
z�B
y�B
z�B
{�B
|�B
~�B
~�B
�B
�B
�B
�B
�+B
�1B
�1B
�=B
�=B
�JB
�PB
�VB
�\B
�bB
�hB
�hB
�hB
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
��B
�B
�B
�B
�!B
�!B
�'B
�-B
�-B
�9B
�9B
�9B
�FB
�FB
�FB
�LB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XBR�BR�BS�BR�BS�BR�BR�BS�BR�BS�BS�BR�BS�BS�BR�BS�BS�BR�BS�BS�BS�BR�BS�BS�BS�BS�BS�BT�BR�BR�BS�BS�BS�BS�BS�BT�BS�BS�BT�BS�BT�BS�BS�BR�BS�BS�BS�BT�BS�BT�BR�BS�BS�BR�BR�BR�BR�BR�BR�BS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  BS�BS�BS�BS�BS�BS�BT�BT�BS�BR�BR�BS�B\B`Bb*Bj[Bw�B{�B|�B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B~�B}�B~�B}�B~�B~�B~�B~�B~�B}�B~�B~�B��B|�B�B{�B�B��B�
B~�B��B�B�B�B�B�B�B�B�B�B��B�B}�Bz�Bw�Bx�Bw�Bw�Bw�Bu�Bs�BhpBbKB_9BYBT�BO�BK�B5?B"�B�B�BLB(B B��B�B�B�B�B�sB�mB�bB�7B�B�OB��B�ZB�/Bi�B>�B6QB+B �B�BcB B
��B
�B
�0B
��B
��B
�fB
q�B
eqB
M�B
@�B
19B
%�B
�B
	JB
 B	�B	�nB	��B	ëB	��B	�hB	�>B	��B	y�B	k�B	drB	TB	D�B	:wB	�B	 B��B�B�uB�>B��B��B�\B�>B�B��B��B�oB�JB�>B�&B~B}By�Bx�Bv�Bu�Bs�Br�Bo�Bo�Bo�Bo�Bm�Bj�Bj�Bh�Bf�Bc}BbxBZGB_fB]ZBZIBX<BX=BW7BV2BV2BV3BT(BNBU.BS#BQBRBPBOBNBMBI�BF�BF�BA�B=�B?�B>�B=�B<�B;�B:�B;�B9�B:�B9�B9�B9�B7�B8�B6�B5}B4xB3sB4yB2mB2nB4zB4{B7�B7�B7�B7�B8�B7�B7�B7�B6�B6�B6�B6�B7�B6�B6�B6�B4�B6�B7�B5�B6�B7�B7�B8�B8�B9�B:�B9�B=�B<�B;�B;�B=�B?�BC�BC�BC�BA�BQ@BZ{Bg�Bu B|NB��B��B�4B�hB��B�pB��B�B	�B	�B	?B	.�B	;�B	CB	PqB	^�B	o0B	o2B	|�B	��B	��B	��B	�
B	�,B	�lB	��B	��B	��B	�0B	�iB	�B	̈B	ӶB	��B	��B	��B	��B	�B	�B	�KB	�eB	�{B	�B	�B	��B	��B	��B	��B
 �B
�B
B
	)B
7B
:B
PB
XB
[B
iB
yB
�B
�B
�B
�B
�B
!�B
#�B
#�B
#�B
(B
)B
,,B
*#B
,1B
,4B
/IB
2_B
4mB
4pB
6B
7�B
8�B
8�B
9�B
<�B
>�B
>�B
@�B
A�B
C�B
C�B
E�B
D�B
GB
HB
IB
J!B
L1B
K-B
M<B
M?B
NHB
OPB
PYB
QbB
RjB
RmB
U�B
U�B
T�B
V�B
W�B
W�B
Y�B
Y�B
Y�B
Z�B
[�B
Z�B
\�B
\�B
]�B
^�B
_�B
_�B
`�B
b�B
dB
d	B
fB
fB
g"B
h+B
i4B
i6B
j?B
jAB
kIB
mXB
mZB
nbB
neB
omB
pvB
qB
p{B
q�B
r�B
r�B
r�B
s�B
t�B
t�B
u�B
w�B
w�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
{�B
|�B
{�B
|�B
}�B
B
�B
�B
�-B
�2B
�LB
�XB
�iB
�vB
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
�B
�B
�!B
�+B
�7B
�DB
�JB
�WB
�cB
�oB
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
�B
�B
�B
�B
�(B
�9B
�?B
�UB
�pB
��B
��B
��B
��B
��B
��B
�B
�B
�$B
�@B
�PB
�_B
�sB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BR�BR�BS�BR�BS�BR�BR�BS�BR�BS�BS�BR�BS�BS�BR�BS�BS�BR�BS�BS�BS�BR�BS�BS�BS�BS�BS�BT�BR�BR�BS�BS�BS�BS�BS�BT�BS�BS�BT�BS�BT�BS�BS�BR�BS�BS�BS�BT�BS�BT�BR�BS�BS�BR�BR�BR�BR�BR�BR�BS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201901120800392021061413560220210614135602202106171314002021061713140020210617131400201901120800392021061413560220210614135602202106171314002021061713140020210617131400PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019011208003920190112080039  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019011208003920190112080039QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019011208003920190112080039QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151320210617131513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                