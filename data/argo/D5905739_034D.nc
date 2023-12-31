CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-19T07:01:19Z creation      
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
_FillValue                 4  �|   TEMP_ADJUSTED            
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
_FillValue                 4  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ƈ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ی   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180919070119  20210617131503  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               "   "DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؀VH�@؀VH�11  @؀UU\�@؀UU\�@6�E����@6�E�����c���E��c���E�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @y��@�  @�  @�  A��A��A#33A>ffA`  A�ffA���A���A���A�  A���AᙚA���B   B��B��B��B33B(  B0ffB733B?33BH  BP��BX��B`ffBh��BpffBxffB�ffB�  B�  B�  B���B���B�  B�33B�33B�33B�  B�33B�33B�33B�33B���B���B�  B�  B�ffB�33B�  B���B�33B�33B�  B虚B�ffB�  B�ffB�ffB�  C 33C�C  CL�C33C
�C  C��C�C33C�C��C�CL�C�C  C 33C"�C#�fC&33C(�C*  C+�fC-��C0�C2  C3�fC633C8  C9�fC;��C=�3C@  CB33CD  CE��CH  CJL�CL�CN  CP33CR  CS�fCV  CX33CZ  C[�fC^�C`33CbffCd33Ce�fCh�Cj33Cl�Cm��Co�fCr  Ct  Cv�CxL�Cz�C{�fC~  C��C�&fC��C��3C��C��C��C��fC��C�&fC�  C��3C��C�&fC�&fC��C�&fC��C��3C��C�&fC��C��fC�  C��C�&fC�  C��fC�  C��C�  C��fC�  C��C�&fC��C��3C�  C��C�  C��3C��3C��C�&fC��C��3C��C��C�  C��fC��3C��C�  C��fC��3C��C�&fC��C��3C��C�&fC��C��3C��C��C��C��3C��C�  C��3C��C�  C��3C��C�&fC��C�  C�&fC��C�  C��C��C�  C��C��C��3C��C��C��3C��C�  C��fC�  C�&fC��C�  C��C�  C��3C�  C��C�  C��3C��C�&fC��C��3C��C��C�&fC�&fC��C��fC�  C��C��C�&fC�  C��fC��3C�  C��C��3C�&fC�33C��fC�  C�  D fD �fD�D��D�3D�D	��D` D  D��D&fD��DFfD�fDL�D � D#  D%��D'�3D*ffD,ٚD/FfD1�fD4fD6l�D8�fD;` D=�fD@33DB��DE�DGl�DI��DL33DN��DP� DSL�DU� DX33DZ�3D],�D_��Db9�Dd�3Dg9�Di� Dl9�Dn� DqFfDs��DvY�DxٚD{ffD}� D�  D�FfD�� D�� D�&fD�p D���D�3D�P D���D��fD�,�D�l�D��fD���D�#3D�L�D�y�D��3D���D���D�#3D�FfD�p D��fD���D���D���D�  D�L�D�s3D���D��3D��fD��fD� D�)�D�S3D�s3D�� D��fD�� D�fD�,�D�Y�D�|�D���D�ٚD�3D�9�D�c3D�� D��fD��3D�  D�I�D�vfDã3D��3D���D�#3D�FfD�p Dʙ�D�� D��3D�3D�#3D�@ D�\�D�p Dӓ3D԰ D�� D��D�	�D�  D�6fD�VfD�i�D�vfDކfDߓ3D���D�fD� D��D��3D���D���D�� D���D�fD��D�fD�  D�&fD�0 D�<�D�FfD�L�D�P D�VfD�` D�` D�c3D�c3D�c3D�ffD�I�D�L�D�FfD�I�D�L�E ( E ��E,�E��E6fE� E;3E�3EI�EX Ea�E��E�3E
q�Ep E��EfE� E�3E��E0 E33E��E� EC3E@ E�fE�3E Eq�EX E ��E!��E#D�E$�3E%�fE&��E(.fE)` E*��E,fE-D�E.nfE/��E0�3E2^fE3� E4�E63E7\�E8��E9�3E;&fE<k3E?d�EBT�EE��EH�fEK� EN�fER#3EU�EX\�E[S3E^�fEa� Ed� Eg�3Ek	�EnfEqX Et{3Ewt�Ez��E}�3E�e�E��E��3E�"fE��3E�� E�D�E�� E�� E�-�E�� E���E�8 E�t�E�� E�3E�ffE���E��E�P E��fE�� E�RfE���E�� E�<�E���E�ɚE��E�p�E�� E��E�\�E���>���>���>���?   >���>���>���>���>���?   >���?   ?   >���?   ?   >���?   ?   ?   ?333?   ?333?L��?fff?���?���?�  ?���?�33@ff@33@&ff@333@Fff@Y��@fff@�  @�ff@�ff@���@���@�ff@�  @ə�@�ff@�  @���@���A��AffA33A��A#33A+33A333A;33AC33AH  AQ��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444441414414414414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?fff?�33@   @`  @���@�  @�  @�  A	��A��A+33AFffAh  A�ffA���A���A���A�  A���A噚A���B  B	��B��B��B!33B*  B2ffB933BA33BJ  BR��BZ��BbffBj��BrffBzffB�ffB�  B�  B�  B���B���B�  B�33B�33B�33B�  B�33B�33B�33B�33B���B���B�  B�  B�ffB�33B�  B���B�33B�33B�  B陚B�ffB�  B�ffB�ffB�  C �3C��C� C��C�3C
��C� CL�C��C�3C��CL�C��C��C��C� C �3C"��C$ffC&�3C(��C*� C,ffC.L�C0��C2� C4ffC6�3C8� C:ffC<L�C>33C@� CB�3CD� CFL�CH� CJ��CL��CN� CP�3CR� CTffCV� CX�3CZ� C\ffC^��C`�3Cb�fCd�3CfffCh��Cj�3Cl��CnL�CpffCr� Ct� Cv��Cx��Cz��C|ffC~� C�Y�C�ffC�L�C�33C�L�C�Y�C�L�C�&fC�L�C�ffC�@ C�33C�L�C�ffC�ffC�L�C�ffC�L�C�33C�L�C�ffC�L�C�&fC�@ C�Y�C�ffC�@ C�&fC�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�@ C�Y�C�@ C�33C�33C�L�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�33C�L�C�@ C�&fC�33C�Y�C�ffC�Y�C�33C�Y�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�L�C�@ C�33C�L�C�@ C�33C�L�C�ffC�Y�C�@ C�ffC�Y�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�L�C�33C�L�C�@ C�&fC�@ C�ffC�L�C�@ C�Y�C�@ C�33C�@ C�Y�C�@ C�33C�L�C�ffC�L�C�33C�L�C�Y�C�ffC�ffC�L�C�&fC�@ C�L�C�L�C�ffC�@ C�&fC�33C�@ C�Y�C�33C�ffC�s3C�&fC�@ C�@ D &fD �fD,�D��D3D9�D	��D� D  D��DFfDٚDffD�fDl�D � D#@ D%��D(3D*�fD,��D/ffD1�fD4&fD6��D9fD;� D=�fD@S3DB��DE,�DG��DI��DLS3DN��DQ  DSl�DU� DXS3DZ�3D]L�D_��DbY�Dd�3DgY�Di� DlY�Dn� DqffDs��Dvy�Dx��D{�fD}� D� D�VfD�� D�� D�6fD�� D���D�3D�` D���D��fD�<�D�|�D��fD���D�33D�\�D���D��3D���D�	�D�33D�VfD�� D��fD���D���D�	�D�0 D�\�D��3D���D��3D��fD�fD�  D�9�D�c3D��3D�� D��fD�� D�fD�<�D�i�D���D���D��D�3D�I�D�s3D�� D��fD�3D�0 D�Y�DfDó3D��3D�	�D�33D�VfDɀ Dʩ�D�� D��3D�3D�33D�P D�l�DҀ Dӣ3D�� D�� D���D��D�0 D�FfD�ffD�y�D݆fDޖfDߣ3Dਗ਼D�fD�� D���D��3D���D���D�  D�	�D�fD��D�&fD�0 D�6fD�@ D�L�D�VfD�\�D�` D�ffD�p D�p D�s3D�s3D�s3D�vfD�Y�D�\�D�VfD�Y�D�\�E 0 E ��E4�E��E>fE� EC3E�3EQ�E` Ei�E��E�3E
y�Ex E�EfE� E�3E��E8 E;3E��E� EK3EH E�fE�3E Ey�E` E ��E"�E#L�E$�3E%�fE'�E(6fE)h E*��E,fE-L�E.vfE/��E0�3E2ffE3� E4�E6#3E7d�E8��E9�3E;.fE<s3E?l�EB\�EE��EH�fEK� EN�fER+3EU�EXd�E[[3E^�fEa� Ed� Eh3Ek�EnfEq` Et�3Ew|�Ez��E}�3E�i�E��E��3E�&fE��3E�� E�H�E�� E�� E�1�E�� E���E�< E�x�E�� E�3E�jfE���E��E�T E��fE�  E�VfE���E�� E�@�E���E�͚E�!�E�t�E�� E��E�`�E���G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�?fffG�O�G�O�?fffG�O�G�O�?fffG�O�G�O�?�  G�O�?�  ?���?�ff?�33?���?ٙ�@   @ff@��@&ff@333@Fff@S33@fff@y��@�33@�  @�ff@�ff@���@���@�ff@�  @ٙ�@�ff@�  @���AffA��AffA33A$��A+33A333A;33AC33AK33AP  AY��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444441414414414414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ �@ %@ �@ {@ �@ ""@ (�@ /�@ 7L@ >@ E�@ Q=@ _�@ oF@ |?@ �7@ ��@ ��@ �-@ ��@ �|@ �t@ �m@ �@�@�@�@-@8�@FQ@UU@dZ@r@~�@�P@�H@��@�F@@�7@��@��@�~@�@*@"�@0x@=q@K�@Yn@g@t�@�@��@�@��@��@ƨ@Ӡ@��@�@�E@
=@�@&�@33@B8@O�@\)@k.@x&@�@��@��@��@��@�@׹@�@�@��@V@[@)�@6�@E�@R�@^�@n�@{�@��@��@�z@�-@�&@�@��@�@�@@@�@-�@:@FQ@UU@e	@qS@~K@�P@��@��@��@��@�7@�/@�4@��@	�@�@!s@0x@>�@K�@Wb@e�@t@��@�@�@�Y@��@��@��@�T@�@��@
�@B@&;@1�@A�@P�@\)@i!@x&@�+@��@�@�!@�k@ȴ@׹@�@�@��@�@�@+@6�@B�@Q�@`�@m:@y�@��@��@��@�-@�w@��@��@�@�@	�@	�@	 �@	-@	9X@	H]@	V�@	b�@	oF@	}�@	��@	��@	��@	�9@	��@	�C@	�;@	��@	��@
�@
*@
!s@
0x@
>�@
K�@
X@
g@
t@
�@
�@
�@
��@
�@
�@
��@
�H@
��@
��@
=@B@&;@33@B8@O0@[z@j@x&@�p@�u@�m@��@��@�o@׹@�@�@  @�@O@*S@6�@C�@R�@a�@m�@z3@�7@��@��@��@��@�o@�t@��@�q@v@@[@+�@:@I@T�@e	@s_@|�@��@��@��@��@��@��@܀@i!@�r@�,@@�@��@�|@�@Yn@��@�@%�@ff@��@�(@-@o�@�-@�@3�@uk@�@��@>@�W@��@�@E�@�|@�@1@G�@��@��@�@S�@��@��@!s@e	@��@��@2�@ww@�j@^@F�@�D@��@
=@N�@�#@�t@""@g�@�@�@:�@��@�@@T�@��@��@!s@c�@�(@�T@ "�@ bN@ �z@ ��@! @!_�@!�a@!�/@"�@"Wb@"�0@"�\@#*@#Q=@#�@#�*@$�@$G�@$��@$�>@% �@%=q@%|?@%��@%��@&9X@&y�@&��@&�~@'8�@'x&@'�^@'��@(:@(|?@(�j@(��@)<@)|?@)�j@)�E@*<@*{�@*��@*�,@+8�@+ww@+��@+�@,0x@,m:@,��@,�@-"�@-_�@-�@-�@.�@.Q�@.�P@.��@/v@/>�@/x�@/�-@/�(@0#�@0\)@0��@0�|@1%@1@,@1z�@1��@1��@2$�@2]�@2�0@2�*@3�@3@,@3x�@3��@3�@4 @4X�@4�\@4ƨ@4�E@53�@5k.@5��@5��@61@6?}@6v�@6�@6�`@7[@7V@7��@7��@7��@87�@8qS@8�@9V@9�9@:k.@;�@;~K@<'�@<��@=@�@=��@>"�@>��@?;d@?��@@Q�@@� @Ab�@B�@Bk.@Cj@C�I@C��@D��@E�@E�M@F1'@F�@G?}@G��@HDD@HĜ@Il�@I�@Jl�@J�@Kn�@L�@L��@M-@M��@N<@Nƨ@OO�@O��@PdZ@Q�M@R�(@TP�@U�f@Wj@XF�@Y��@Z�@\P�@]�#@^�Q@`=q@a�m@b��@dH]@e��@f�,@hO�@i��@j��@lE�@m��@n�@pF�@q��@r�@@s""@sm�@s�R@tj@t4�@t~K@t�@u�@uK�@u��@u�@v�@vdZ@v�r@v�H@w+@wt@w��@w�@x:@x��@x�+@x��@yE�@y��@yӠ@z�@zV@z��G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�@ �G�O�G�O�@ �G�O�G�O�@ �G�O�G�O�@ jG�O�@ j@ �@ v@ %@ �@ 1@ 
=@ 
�@ �@ V@ �@ �@ @ *@ 6@ �@ O@ �@  @ !s@ $.@ &�@ (�@ +@ -�@ /�@ 2�@ 5�@ 8�@ <�@ >�@ B�@ E�@ I@ Lu@ O�@ SI@ UU@ YnG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A��A���AԺ^A�ffA�I�AҶFAґhA҇+A�p�A�ZA�G�A�/A�{A�%A���A��A��yA��
A�ĜAѰ!Aљ�Aщ7A�v�A�bNA�S�A�A�A�1'A�$�A��;AЗ�AЁA�%A�z�A�I�Aŝ�A�|�A��-A�t�A��RA�$�A�z�A�9XA�dZA���A�/A�bNA�r�A�+A���A�"�A���A��A��9A�oA��`A���A��A��TA���A��A���A�ĜA��9A�A�bNA�A�A�z�A��/A�r�A���A�r�A���A�r�A�oA���A���A��-A��#A�p�A���A���A�ȴA�XA�VA��A��-A�Q�A�&�A���A��
A���A��9A���A��9A�5?A�E�A��A�+A���A���A�`BA���A�ĜA�ffA���A��A�bNA�  A��uA�?}A��RA���A�~�A�7LA��A}7LAy�Aw��Av�/AvffAs��ArbNApM�Ak�TAjz�Ai��Ahz�Ag�
AgAfZAe`BAd�!Abr�A`��A`9XA_S�A^jA\jAZE�AXjAW�hAWl�AWoAV�AV(�AU33AS��AR��ARffAR1AQ�hAP-ANv�AMt�ALȴAK�AJ�yAI�mAH(�AF�AD�yAC��AB�`AB1'AA��AA
=A@ �A>�DA=��A<�/A;�^A;&�A9��A8�A77LA5�A5��A4��A3�A2�`A2z�A1��A0ĜA01A/+A.M�A-\)A,�HA,ĜA+ƨA*1A'�wA&VA&JA%��A%/A#��A"bNA!�A -A�DA��Ap�A9XA-A�
A�^A1'A"�A�A��Ap�A��A��A�A��A1'AAȴA|�A�+A$�A��A��A
�RA	�A�!A
=A$�A�\AK�A  A ��A ĜA �A �@��H@��`@�{@���@�+@��@��T@��#@��`@�  @�t�@�9@�ȴ@�{@�b@�G�@��y@���@���@�ȴ@�h@ޗ�@ܴ9@�
=@أ�@���@�^5@Ցh@��T@�M�@�Q�@��@�33@���@���@�G�@���@��#@�K�@�?}@��w@�Z@�E�@�5?@��@���@�J@�t�@��R@�b@�X@��@���@�(�@�o@��R@�5?@��j@�S�@���@�Ĝ@��P@�x�@�1'@��@�hs@�j@�t�@��\@���@��@�I�@��@��F@���@�J@�V@�w@}�-@|��@zJ@xA�@vv�@u/@t(�@r�H@o�P@l��@j��@hA�@f��@d�D@c�m@bJ@`1'@^��@]/@[�@Z~�@Y�@W�;@V�y@U�@S��@R�@R=q@PbN@Nȴ@Nff@L��@KdZ@I��@I%@G�;@F�@F$�@D�D@C�@C@A%@@Q�@>�y@>V@=?}@<�j@;��@:�!@9�@9x�@7�;@6$�@5�@4�D@3�F@1�7@0�9@0  @.��@-@,�@+��@)�^@)X@( �@'�@'+@&�y@%�-@$�@#S�@"^5@!��@!%@  �@K�@�h@�@�@=q@7L@Q�@��@;d@@/@ƨ@n�@x�@7L@�;@�P@K�@�@�j@j@�F@C�@@
�@
�!@
J@	��@	x�@	7L@r�@  @�w@�P@+@�@5?@`B@��@j@��@C�@�@�!@~�@�@X@ A�?�5??��?���?���?��?���?�S�?�M�?��?�5??�/?�(�?ꟾ?�9?�P?�`B?�j?�M�?�v�?�O�?ۅ?ٺ^?�1'?׍P?�ff?Լj?�t�?�J?���?���?�Ĝ?�  ?ϝ�?ϝ�?Η�?���?̬?̋D?��H?�=q?��#?���?�1'?�ȴ?š�?�Z?��?�-?���?�|�?��?�{?�V?�V?���?�I�?�dZ?��?��^?���?���?�X?���?��^?�^5?��H?�dZ?�1?��?��?��?�V?�O�?�p�?��h?��h?��?��?�{?�V?�V?���?���?��?���?��?�\)?�\)?��w?��w?�  ?� �?�A�?�bN?��?���?��`?��`AԾwA���AԼjAԺ^AԾwAԺ^A�AԼjAԾwA���A�ĜA���A���A���A�ȴA���A���A���A�ȴA���A���A���A���A���A���A���A���A���A��
A��#A��#A��A���A���A���A���A���AԾwAԥ�AԋDA�l�A�9XAӾwA�7LA��`A�ȴAҶFAҥ�Aҕ�AҍPAҋDAҁA�v�A�p�A�jA�bNA�ZA�XA�Q�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A�ƨA���A��A���AԺ^A�ffA�I�AҶFAґhA҇+A�p�A�ZA�G�A�/A�{A�%A���A��A��yA��
A�ĜAѰ!Aљ�Aщ7A�v�A�bNA�S�A�A�A�1'A�$�A��;AЗ�AЁA�%A�z�A�I�Aŝ�A�|�A��-A�t�A��RA�$�A�z�A�9XA�dZA���A�/A�bNA�r�A�+A���A�"�A���A��A��9A�oA��`A���A��A��TA���A��A���A�ĜA��9A�A�bNA�A�A�z�A��/A�r�A���A�r�A���A�r�A�oA���A���A��-A��#A�p�A���A���A�ȴA�XA�VA��A��-A�Q�A�&�A���A��
A���A��9A���A��9A�5?A�E�A��A�+A���A���A�`BA���A�ĜA�ffA���A��A�bNA�  A��uA�?}A��RA���A�~�A�7LA��A}7LAy�Aw��Av�/AvffAs��ArbNApM�Ak�TAjz�Ai��Ahz�Ag�
AgAfZAe`BAd�!Abr�A`��A`9XA_S�A^jA\jAZE�AXjAW�hAWl�AWoAV�AV(�AU33AS��AR��ARffAR1AQ�hAP-ANv�AMt�ALȴAK�AJ�yAI�mAH(�AF�AD�yAC��AB�`AB1'AA��AA
=A@ �A>�DA=��A<�/A;�^A;&�A9��A8�A77LA5�A5��A4��A3�A2�`A2z�A1��A0ĜA01A/+A.M�A-\)A,�HA,ĜA+ƨA*1A'�wA&VA&JA%��A%/A#��A"bNA!�A -A�DA��Ap�A9XA-A�
A�^A1'A"�A�A��Ap�A��A��A�A��A1'AAȴA|�A�+A$�A��A��A
�RA	�A�!A
=A$�A�\AK�A  A ��A ĜA �A �@��H@��`@�{@���@�+@��@��T@��#@��`@�  @�t�@�9@�ȴ@�{@�b@�G�@��y@���@���@�ȴ@�h@ޗ�@ܴ9@�
=@أ�@���@�^5@Ցh@��T@�M�@�Q�@��@�33@���@���@�G�@���@��#@�K�@�?}@��w@�Z@�E�@�5?@��@���@�J@�t�@��R@�b@�X@��@���@�(�@�o@��R@�5?@��j@�S�@���@�Ĝ@��P@�x�@�1'@��@�hs@�j@�t�@��\@���@��@�I�@��@��F@���@�J@�V@�w@}�-@|��@zJ@xA�@vv�@u/@t(�@r�H@o�P@l��@j��@hA�@f��@d�D@c�m@bJ@`1'@^��@]/@[�@Z~�@Y�@W�;@V�y@U�@S��@R�@R=q@PbN@Nȴ@Nff@L��@KdZ@I��@I%@G�;@F�@F$�@D�D@C�@C@A%@@Q�@>�y@>V@=?}@<�j@;��@:�!@9�@9x�@7�;@6$�@5�@4�D@3�F@1�7@0�9@0  @.��@-@,�@+��@)�^@)X@( �@'�@'+@&�y@%�-@$�@#S�@"^5@!��@!%@  �@K�@�h@�@�@=q@7L@Q�@��@;d@@/@ƨ@n�@x�@7L@�;@�P@K�@�@�j@j@�F@C�@@
�@
�!@
J@	��@	x�@	7L@r�@  @�w@�P@+@�@5?@`B@��@j@��@C�@�@�!@~�@�@X@ A�?�5??��?���?���?��?���?�S�?�M�?��?�5??�/?�(�?ꟾ?�9?�P?�`B?�j?�M�?�v�?�O�?ۅ?ٺ^?�1'?׍P?�ff?Լj?�t�?�J?���?���?�Ĝ?�  ?ϝ�?ϝ�?Η�?���?̬?̋D?��H?�=q?��#?���?�1'?�ȴ?š�?�Z?��?�-?���?�|�?��?�{?�V?�V?���?�I�?�dZ?��?��^?���?���?�X?���?��^?�^5?��H?�dZ?�1?��?��?��?�V?�O�?�p�?��h?��h?��?��?�{?�V?�V?���?���?��?���?��?�\)?�\)?��w?��w?�  ?� �?�A�?�bN?��?���?��`?��`AԾwA���AԼjAԺ^AԾwAԺ^A�AԼjAԾwA���A�ĜA���A���A���A�ȴA���A���A���A�ȴA���A���A���A���A���A���A���A���A���A��
A��#A��#A��A���A���A���A���A���AԾwAԥ�AԋDA�l�A�9XAӾwA�7LA��`A�ȴAҶFAҥ�Aҕ�AҍPAҋDAҁA�v�A�p�A�jA�bNA�ZA�XA�Q�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�1B�hB�JB��B��B��B�B��B�-B�9B�-B�'B�'B�B�RB�wB�jB�wB�jB�dB�?B�FB�?B�9B�B�B��B��B��B��B��B��B�hB�DB�DB�B�Bx�Bm�BhsBcTB]/BT�BN�B@�B7LB2-B$�B�BB�B�/BɺB�9B�B��B��B��B��B�7Br�BcTBZBH�BB�B8RB&�B�B	7B
��B
�B
�B
�LB
��B
��B
��B
�uB
�VB
�%B
�B
�B
}�B
v�B
aHB
K�B
?}B
?}B
7LB
&�B
!�B
VB	�B	�B	�sB	�5B	�)B	�B	��B	ƨB	��B	�B	��B	��B	��B	�oB	�B	v�B	jB	e`B	ffB	k�B	jB	hsB	k�B	o�B	m�B	l�B	iyB	hsB	`BB	[#B	W
B	R�B	L�B	H�B	?}B	6FB	33B	.B	(�B	$�B	 �B	�B	�B	uB	DB		7B	B	  B��B��B�B�yB�fB�NB�;B�B�
B��B��B��BɺBŢB��B�wB�qB�dB�RB�B��B��B��B��B��B��B�hB�PB�+B�B{�Bp�BhsB\)B^5BdZBT�BW
BW
BYBT�B\)BYBXBXBW
BVBN�BG�B@�B@�BA�BD�B@�B=qB8RB6FB6FB2-B1'B1'B1'B0!B/B.B.B)�B+B)�B'�B&�B%�B%�B$�B%�B%�B%�B(�B%�B%�B(�B+B+B,B/B/B49B9XB<jB@�B@�B@�BB�BN�BQ�B\)BdZBo�Bx�B�B�VB��B��B�B�dB��B�B�`B��B	
=B	{B	%�B	7LB	I�B	Q�B	e`B	m�B	y�B	�B	� B	�PB	��B	��B	��B	��B	�-B	�XB	ÖB	��B	��B	��B	�B	�/B	�NB	�`B	�fB	�B	�B	�B	�B	��B	��B	��B
  B
  B
B
+B
+B

=B

=B
PB
bB
oB
oB
uB
�B
�B
�B
!�B
"�B
#�B
%�B
(�B
(�B
+B
,B
.B
/B
/B
0!B
1'B
33B
6FB
49B
7LB
8RB
8RB
9XB
:^B
;dB
;dB
=qB
>wB
>wB
@�B
@�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
G�B
I�B
J�B
J�B
K�B
M�B
N�B
N�B
P�B
Q�B
R�B
R�B
VB
VB
W
B
W
B
XB
XB
YB
\)B
\)B
]/B
]/B
^5B
_;B
_;B
bNB
cTB
dZB
e`B
e`B
ffB
gmB
gmB
iyB
iyB
k�B
m�B
n�B
n�B
o�B
o�B
o�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
t�B
t�B
t�B
v�B
v�B
v�B
v�B
x�B
x�B
x�B
z�B
z�B
z�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�%B
�%B
�1B
�1B
�7B
�=B
�JB
�JB
�PB
�\B
�\B
�hB
�hB
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
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�9B
�9B
�FB
�LB
�RB
�RB
�RB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�RB
�XB
�XB
�^B
�RB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�RB
�XB
�RB
�XB
�XB
�XB
�XB�B��B�B�B�B�B�B�B�B�B�B�B��B��B�B�B��B�B�B��B�B��B��B��B�B��B��B�B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�QB�4B��B��B��B��B��B�B�%B�B�B�B�	B�AB�fB�ZB�gB�[B�UB�1B�8B�2B�-B�B�B��B��B��B��B��B��B�`B�=B�=B�B�Bx�Bm�BhoBcPB],BT�BN�B@�B7JB2,B$�B�BB�B�0BɼB�;B�B��B��B��B��B�<Br�BcZBZ$BH�BB�B8ZB&�B�B	@B
�B
�B
�'B
�WB
�B
��B
��B
��B
�cB
�3B
�-B
�B
~B
v�B
aXB
K�B
?�B
?�B
7^B
&�B
!�B
iB	��B	��B	�B	�JB	�?B	�-B	��B	ƿB	��B	�3B	�B	��B	��B	��B	�!B	v�B	j�B	e|B	f�B	k�B	j�B	h�B	k�B	o�B	m�B	l�B	i�B	h�B	`dB	[EB	W-B	SB	L�B	H�B	?�B	6kB	3XB	.:B	)B	%B	 �B	�B	�B	�B	mB		aB	DB	 +B�B��B��B�B�B�|B�iB�FB�9B�.B�B��B��B��B��B��B��B��B��B�CB�%B�&B�B��B��B��B��B��B�dB�FB|!Bp�Bh�B\dB^qBd�BU;BWGBWHBYUBU=B\hBYWBXPBXQBWKBVFBOBG�B@�B@�BA�BD�B@�B=�B8�B6�B6�B2uB1oB1pB1pB0kB/eB._B._B*HB+NB*IB(=B'7B&1B&2B%-B&3B&4B&4B)HB&5B&6B)IB+VB+VB,]B/pB/pB4�B9�B<�B@�B@�B@�BB�BO7BRMB\�Bd�BpByBB�vB��B�B�0B��B��B�HBٚB��B�EB	
�B	
B	&uB	7�B	JRB	R�B	e�B	n2B	zB	��B	��B	��B	�BB	�EB	�`B	��B	��B	�B	�VB	̊B	ѫB	��B	��B	��B	� B	�5B	�>B	�lB	�vB	�B	��B	��B	��B	��B
 �B
 �B
B
$B
'B
<B
?B
UB
jB
zB
}B
�B
�B
�B
�B
"�B
#�B
$�B
'B
*B
* B
,/B
-8B
/GB
0PB
0SB
1\B
2dB
4sB
7�B
5B
8�B
9�B
9�B
:�B
;�B
<�B
<�B
>�B
?�B
?�B
A�B
A�B
B�B
C�B
EB
EB
FB
GB
GB
GB
I.B
K<B
LFB
LIB
MRB
OaB
PiB
PlB
R{B
S�B
T�B
T�B
W�B
W�B
X�B
X�B
Y�B
Y�B
Z�B
]�B
]�B
^�B
^�B
_�B
`�B
`�B
dB
eB
f&B
g.B
g1B
h9B
iCB
iEB
kTB
kVB
meB
osB
p}B
pB
q�B
q�B
q�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
v�B
v�B
v�B
x�B
x�B
x�B
x�B
z�B
z�B
z�B
}B
}B
}B
B
B
B
B
�%B
�(B
�3B
�>B
�QB
�VB
�jB
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
�B
�B
�B
�'B
�,B
�>B
�PB
�WB
�cB
�vB
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
��B
�B
�B
�B
�'B
�-B
�CB
�WB
�sB
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�CB
�RB
�nB
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
�+B
�.B
�2B
�5B
�7B
�:B
�>B
�;B
�DB
�GB
�CB
�MB
�PB
�ZB
�PB
�YB
�\B
�`B
�bB
�_B
�iB
�lB
�hB
�qB
�oB
�xB
�{B
�~B
��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809190701192021061413554020210614135540202106171312582021061713125820210617131258201809190701192021061413554020210614135540202106171312582021061713125820210617131258PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091907011920180919070119  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091907011920180919070119QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091907011920180919070119QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150320210617131503IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                