CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-30T17:02:07Z creation      
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
_FillValue                 (  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  aH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ep   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ը   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20180930170207  20210722160153  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               !   !DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؃	w��@؃	w��11  @؃	ww|@؃	ww|@6�s�@6�s��c�c�e���c�c�e��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?L��@ff@Fff@�33@�33@�ff@�ffA   A��A$��AA��Aa��A���A�  A�  A�33A�33A�  A�  A���A�33BffB33B��B ��B(ffB0  B8  B@  BH  BP  BX  B`  Bg��BpffBxffB�33B�33B�33B�33B�33B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�33B�  B�33B�33B̙�B���B�  B�33B�  B�  B�  B�  B���B�ffB�  B���B���B���C33CL�C�fC��C
�C�C�fCL�C33C  C  C�fC�C  C  C �C"L�C$33C&�C(  C)��C,  C.�C0  C1�fC4�C6  C7��C:  C<33C>�C?��CB�CD33CF  CG�fCI�fCL33CN33CP  CR33CT�CU�fCX33CZ�C[��C^�C`  Cb  Cd�Ce�fCg�fCj33Cl�Cn33Cp�Cr�Ct�Cv�Cx�Cz  C{�fC}��C��C�&fC��C�  C��C��C��3C��C��C�  C��C�33C��C��3C��C�  C��fC��C�&fC�33C��C��3C��C�&fC��C�  C��C��C��3C��C��C��C��fC��3C�  C��C��C�&fC�&fC��C��3C��C�&fC��C�  C��C��C�&fC��C��3C��C�&fC��C��3C��C�&fC��C��3C��C��C�33C��C�  C��C�  C��fC��C��C��C�  C��C��C�  C��C�  C�  C��C��C�  C�&fC��C��3C��C��C�  C��C��C��3C��C��C��fC��fC�  C��C�&fC��C��3C��C��C��C��C��C��fC��fC��3C��C��C��C�&fC��C�ٚC�  C�&fC��C��C��C��C��C��C�&fC��C��C��3C��C��C��C��C�  D   D �fD  D� D�fD	L�DfD�fD��D� D` D9�D9�D &fD#�D%�3D(�3D+��D.s3D1�D3�3D6l�D9�D;��D>@ D@��DCS3DE�3DHS3DJ�fDMY�DO��DRl�DU  DW� DZ9�D\�fD_` DbfDd��DgFfDi� Dl�fDo&fDq� Dt��Dw33DyٚD|�D~�3D���D�0 D��fD�ٚD�6fD���D��fD�0 D�s3D���D��fD�9�D�p D��fD���D�3D�L�D�� D���D��fD�&fD�L�D�s3D��fD�� D�� D���D�	�D�)�D�I�D�ffD��3D��3D��fD��3D�3D�33D�Y�D��3D��3D��fD��fD�fD�33D�S3D�y�D�� D�� D��fD�&fD�VfD��3D��3D��D��D�P DÆfD�� D�  D�<�D�|�Dɩ�D��3D�fD�C3D�y�DϬ�D��fD�fD�,�D�P D�y�D֓3D׳3D��fD���D��D�0 D�I�D�c3D�s3D��3D�fD�fD㹚D�ٚD��3D� D�  D�9�D�L�D�c3D�s3D��DD��D���D���D���D���D� D�  D�0 D�C3D�VfD�ffD�Y�D�l�D�|�D��3D�� E X E � EffE� EvfE  E��E�E�fE1�E�3EC3E��E�fE��E
��E��E�fE4�E4�E� E��E@ EA�E��E� E+3E$�E��E�fEfE E�fE � E!�E#l�E$s3E%�E&�3E([3E)NfE*��E,33E-+3E.� E/��E1�E2x E3x E4��E5�fE7nfE8c3E9ٚE:�fE<A�E?� EBT�EE�fEH�3EK�3EN�fER3EUc3EXa�E[S3E^� Ea��EdɚEg��Ej�fEnX Eqi�Eth Ew�3Ez�3E}��E�}�E� E���E�2fE��fE�<�E���E��E�3E�ŚE�3E�VfE�� E�
fE�P�E�� E���E�?3E���E�� E�0 E�rfE��3E��E�\�E���E�fE�d E�� E��E�@ E��fE��3?   ?   ?��?��?��?��>���?��?��?333?333?L��?�  ?���?���?�ff?�33?ٙ�?�ff@   @33@   @,��@@  @S33@`  @s33@�33@���@�ff@���@���@�  @���@ə�@�  @�  @陚@�ffA   AffAffA33A��A   A(  A.ffA8  A<��AD��AK33AS33AY��Aa��Ai��AnffAt��A~ffA�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444141411111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?�  ?�ff@&ff@fff@�33@�33@�ff@�ffA  A��A,��AI��Ai��A���A�  A�  A�33A�33A�  A�  A���B��B
ffB33B��B"��B*ffB2  B:  BB  BJ  BR  BZ  Bb  Bi��BrffBzffB�33B�33B�33B�33B�33B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�33B�  B�33B�33B͙�B���B�  B�33B�  B�  B�  B�  B���B�ffB�  B���B���C ffC�3C��CffCL�C
��C��CffC��C�3C� C� CffC��C� C� C ��C"��C$�3C&��C(� C*L�C,� C.��C0� C2ffC4��C6� C8L�C:� C<�3C>��C@L�CB��CD�3CF� CHffCJffCL�3CN�3CP� CR�3CT��CVffCX�3CZ��C\L�C^��C`� Cb� Cd��CfffChffCj�3Cl��Cn�3Cp��Cr��Ct��Cv��Cx��Cz� C|ffC~L�C�L�C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�L�C�@ C�L�C�s3C�L�C�33C�L�C�@ C�&fC�L�C�ffC�s3C�L�C�33C�L�C�ffC�L�C�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�33C�@ C�Y�C�Y�C�ffC�ffC�L�C�33C�L�C�ffC�L�C�@ C�L�C�Y�C�ffC�L�C�33C�L�C�ffC�L�C�33C�L�C�ffC�L�C�33C�L�C�Y�C�s3C�Y�C�@ C�Y�C�@ C�&fC�L�C�Y�C�Y�C�@ C�Y�C�L�C�@ C�Y�C�@ C�@ C�Y�C�Y�C�@ C�ffC�L�C�33C�Y�C�L�C�@ C�L�C�L�C�33C�L�C�L�C�&fC�&fC�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�Y�C�Y�C�L�C�&fC�&fC�33C�L�C�L�C�Y�C�ffC�L�C��C�@ C�ffC�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�ffC�Y�C�Y�C�33C�Y�C�Y�C�Y�C�L�C�@ D   D �fD  D� D�fD	l�D&fD�fD��D� D� DY�DY�D FfD#9�D&3D(�3D+��D.�3D19�D3�3D6��D9,�D;��D>` D@��DCs3DE�3DHs3DJ�fDMy�DP�DR��DU  DW� DZY�D\�fD_� Db&fDd��DgffDj  Dl�fDoFfDr  Dt��DwS3Dy��D|9�D~�3D���D�@ D��fD��D�FfD���D��fD�@ D��3D�ɚD�fD�I�D�� D��fD���D�#3D�\�D�� D���D�fD�6fD�\�D��3D��fD�� D�� D���D��D�9�D�Y�D�vfD��3D��3D��fD�3D�#3D�C3D�i�D��3D��3D��fD��fD�fD�C3D�c3D���D�� D�� D�fD�6fD�ffD��3D��3D���D�,�D�` DÖfD�� D� D�L�DȌ�Dɹ�D��3D�&fD�S3DΉ�Dϼ�D��fD�fD�<�D�` DՉ�D֣3D��3D��fD��D�)�D�@ D�Y�D�s3D߃3D��3D�fD�fD�ɚD��D�3D�  D�0 D�I�D�\�D�s3D�3D��DD���D���D���D���D��D�  D�0 D�@ D�S3D�ffD�vfD�i�D�|�D���D��3D�� E ` E � EnfE� E~fE E��E!�E�fE9�E�3EK3E��E�fE��E
��E��E�fE<�E<�E� E��EH EI�E��E� E33E,�E��E�fEfE E�fE!  E!��E#t�E${3E%��E&�3E(c3E)VfE*ɚE,;3E-33E.� E/��E1�E2� E3� E5�E5�fE7vfE8k3E9�E:�fE<I�E?� EB\�EE�fEH�3EK�3EN�fER3EUk3EXi�E[[3E^� Ea��EdњEg��Ej�fEn` Eqq�Etp Ew�3Ez�3E}��E���E� E���E�6fE��fE�@�E���E��E��3E�ɚE�3E�ZfE�� E�fE�T�E�� E���E�C3E���E�� E�4 E�vfE��3E��E�`�E���E�fE�h E�� E��E�D E��fE��3G�O�?�  G�O�G�O�G�O�G�O�?fffG�O�?���G�O�?���?�ff?�  ?���?ٙ�?�ff?�33@��@33@   @333@@  @L��@`  @s33@�  @���@�33@���@�ff@���@���@�  @���@ٙ�@�  @�  @���A33A  AffAffA33A!��A(  A0  A6ffA@  AD��AL��AS33A[33Aa��Ai��Aq��AvffA|��A�33A�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444141411111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ j@ v@ V@ *@ �@ "�@ *S@ 1'@ 6�@ >@ FQ@ R�@ `B@ m�@ z�@ ��@ ��@ �(@ �~@ �&@ �|@ ��@ ��@ ��@�@o@g@,`@:@G�@UU@b�@p�@~K@�D@�H@��@��@�>@��@ލ@�4@�,@�@*@""@/@=q@K@X�@ff@t@�d@�\@��@�Y@�^@�J@Ӡ@��@��@��@
=@�@$�@4�@@�@M$@Z�@i!@x�@�+@�@�@��@�k@ȴ@�@�@�Y@  @�@�@(�@6�@D�@S�@`�@m�@z�@�+@�0@��@�~@�w@�|@�t@�@��@�@�@[@-@;d@G�@T�@bN@r@�@��@��@��@�9@��@��@܀@�4@�,@�@*@!s@/@>�@K�@Z@g@t�@�d@�@��@��@��@Ĝ@�O@�T@�L@��@�@�@$�@3�@A�@N�@\�@l�@x&@�p@�u@�m@��@�k@�o@��@�`@�@ �@�@�@(�@7�@D�@Q=@`B@n�@{�@�+@��@��@��@��@��@܀@��@�@	@	@	g@	,`@	:�@	I@	Wb@	c�@	o�@	~�@	��@	�H@	��@	��@	Ĝ@	��@	�/@	�4@	��@
	�@
�@
""@
1'@
=q@
I�@
Yn@
g�@
uk@
��@
��@
��@
��@
��@
��@
Ӡ@
�@
�L@
��@J@�@$�@4�@A�@N�@\�@j@v�@��@�u@�@��@��@��@�@�`@�@ �@@�@*S@7L@B�@P�@^�@m�@{�@��@�<@��@�r@�&@��@��@�y@� @@o@ @.l@;d@I@T�@dZ@r@�@��@��@�A@��@@�7@\�@�5@�@:�@��@�
@%�@s_@�J@*@e�@��@@O�@��@�@.l@uk@�@�@K@��@խ@�@^5@�@�m@-�@r@�R@  @F�@��@Ӡ@�@dZ@�Y@�Y@:�@�d@��@�@^5@��@�@.l@z3@��@@Wb@��@��@4�@z�@��@v@I@��@�7@o@T�@��@��@ O@ ^�@ ��@ �@!!s@!`A@!�a@!�t@"�@"T�@"�i@"��@#J@#I@#��@#�>@$]@$A�@$~�@$�j@$�9@%:�@%x&@%�F@%�@&1'@&qS@&��@&�@',`@'m:@'�@'��@(-�@(m�@(��@(��@)2�@)t@)�F@)�,@*=q@*�@*�J@+v@+H]@+��@+�@,J@,M�@,�P@,�*@-�@-K@-��@-ƨ@.@.B8@.�@.��@.�,@/5@@/qS@/�Y@/�`@0 @0Z@0��@0�C@1V@1K@1�@1�2@1��@27L@2qS@2�f@2�(@3$�@3^�@3��@3��@4�@4G�@4��@4��@4�q@51'@5k.@5�@5��@6�@6O0@6��@6@6��@75�@7p�@7��@7�@8 �@8\�@8��@8�O@9@9I@9��@9��@:oF@;g@;�i@<�@<��@=�@=�&@>.l@>Ӡ@?A�@?�T@@N�@@��@AWb@A��@Be�@C%@Cp�@Db@D�@E�@E�^@F*S@F�|@G7�@G��@H<�@H�#@Ix�@I�@J��@J�(@K�+@L%�@L��@M7L@M�(@NC�@N�@OK�@O�9@PR�@Q�&@R�(@Te�@U�F@V�@XM�@Y�a@[�@\R�@]�#@_  @`P�@a��@b�(@d<�@e�~@g �@hG�@i�r@k�@lI�@m�5@n��@p> @q�M@r�@tA�@u�u@uє@v/@vk.@v��@v�@w%�@w�W@w�k@w��@xLv@x��@x�>@yO@yUU@y��@y�@z�@zV@z�Z@z�T@{6�@{m:@{�@{�Y@|?}@|�pG�O�@ jG�O�G�O�G�O�G�O�@ �G�O�@ G�O�@ �@ v@ �@ �@ 1@ �@ 	�@ �@ J@ �@ �@ @ o@ {@ �@ �@ �@ �@ 
@  @ !s@ $.@ %�@ (G@ +@ ,`@ /�@ 1�@ 4�@ 6�@ 9X@ <�@ >�@ A�@ DD@ G�@ Ji@ N�@ P�@ S�@ V�@ Z@ \�@ `B@ c�@ e�@ hs@ l�@ oF@ rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�^5A�ffA�ffA�&�Aף�A��;A��A�E�A�  A��;A�z�A�1'A� �A�oA���A��A��mA��#A���A���A�ĜAӾwAӶFAӬAӥ�Aӟ�Aә�AӑhAӇ+A�~�A�v�A�l�A�^5A�E�AѶFA�
=A���A�\)A��A�O�A�1'A���A��^A��7A�K�A�A��A���A��A��jA���A�bNA�ƨA�5?A��#A�VA��TA�bNA��A���A��\A�1A��7A�C�A�n�A�`BA�=qA�t�A���A�M�A�/A��A�bA��;A���A�?}A�`BA�ZA���A��DA�hsA�oA���A�hsA�p�A�9XA�
=A��^A��`A���A�  A�dZA�ƨA�E�A���A��jA�I�A��FA��\A��TA���A�1A���A���A�;dA�A��#A�7LA���A�bNA�VA�=qA���A�33A�  A��FA�G�A�bA�hsA}�#Az�Ay�Ax��Ax�jAxA�Awx�Av��AtbAr��Ap�Ao%Am�Al��AkdZAk"�Aj��AjM�Ai�PAhVAg�AgC�AfĜAf-AcC�Aa�PA`�`A`�\A`M�A`  A^ĜA\��A[��A[G�AZ1'AYXAX��AV9XAQ�;AN=qAK�7AJM�AIS�AH�!AH=qAG��AG��AE;dA@bA9C�A5��A3��A2=qA0ZA-�PA,��A,A+�A+hsA+C�A*��A*ĜA)�mA(�\A'�-A'?}A&��A&z�A%�A#�wA!�FA�
A�uA��A9XA��A�A�FA�yA�A7LA�A%A(�A��A��A7LA��A�/A$�A�FA+A��A�+A �A~�A
��A
��A
�A	��A��A��At�AK�A��A�A=qA��A��Ax�AbAp�A �@�ƨ@�v�@��^@�/@���@�A�@��F@�v�@�bN@�`B@�r�@�@�@���@�z�@��H@�j@�o@�"�@��u@�@��@݁@��@ܴ9@�dZ@�hs@أ�@��m@և+@ӍP@��@�(�@� �@��;@°!@�V@���@�V@��@��m@��
@�x�@��@���@���@��@� �@�~�@��u@��
@�hs@�33@�O�@�t�@�@��@���@�9X@�z�@��@��+@�?}@��m@��@��@�b@�M�@�G�@�1'@�dZ@�=q@��^@��/@���@�ȴ@���@�Ĝ@
=@}O�@z�\@xb@wl�@uO�@t1@r^5@q�7@p�`@n5?@k33@j��@i�@h�u@g�@eO�@co@`��@_�w@^5?@]�h@\��@[�F@Z~�@Xr�@U@T��@S��@R-@Q��@Pb@N�R@NE�@K��@J��@J�\@J~�@I��@G�@E��@E/@D��@DZ@B�@@A�@>{@=?}@<��@<z�@;��@;dZ@9�^@7�@7l�@7
=@6ff@5��@4j@2�H@1��@0  @.v�@-�-@,�/@,I�@*��@)��@(��@(bN@'�w@'�@%�@%`B@#C�@#"�@ �@`B@��@�@t�@n�@X@X@�w@�R@�T@?}@(�@�@��@�7@��@��@�@ȴ@V@{@@�m@33@
�!@	�^@��@Q�@�P@K�@�@@�T@`B@/@��@ƨ@��@�H@-@��@x�@&�@ A�?���?���?���?��H?���?��T?�?}?�S�?��??�O�?�"�?�P?�?�Z?�o?�G�?߾w?�V?�O�?�1?�C�?���?�X?�r�?׮?�l�?�?Լj?Ұ!?�J?��`?�\)?��?Ͳ-?���?̋D?�1?˅?�~�?�=q?ɺ^?ə�?��?���?�b?��?�33?��7?��?�V?��?��m?�ƨ?�C�?�"�?�=q?��?���?��^?��#?��#?��#?��#?�^5?�~�?���?���?�C�?���?��m?�I�?��?��h?��h?��-?���?�{?�{?�5??�V?�v�?�v�?��R?��?��?��?�;d?�;d?�\)?�\)?�|�?���?��w?��;?�  ?� �?�A�?�bN?��A�\)A�^5A�^5A�^5A�\)A�^5A�^5A�\)A�\)A�\)A�bNA�dZA�bNA�`BA�`BA�`BA�^5A�bNA�dZA�hsA�hsA�jA�hsA�ffA�hsA�`BA�I�A�+A�  A��;A�ȴAח�A�Q�A�JA��A֟�A�E�A��AՃA�7LA��A�A���A��A��yA��A�ȴAԬAԉ7A�ffA�M�A�?}A�5?A�/A�(�A�&�A�"�A�"�A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A�^5A�^5A�ffA�ffA�&�Aף�A��;A��A�E�A�  A��;A�z�A�1'A� �A�oA���A��A��mA��#A���A���A�ĜAӾwAӶFAӬAӥ�Aӟ�Aә�AӑhAӇ+A�~�A�v�A�l�A�^5A�E�AѶFA�
=A���A�\)A��A�O�A�1'A���A��^A��7A�K�A�A��A���A��A��jA���A�bNA�ƨA�5?A��#A�VA��TA�bNA��A���A��\A�1A��7A�C�A�n�A�`BA�=qA�t�A���A�M�A�/A��A�bA��;A���A�?}A�`BA�ZA���A��DA�hsA�oA���A�hsA�p�A�9XA�
=A��^A��`A���A�  A�dZA�ƨA�E�A���A��jA�I�A��FA��\A��TA���A�1A���A���A�;dA�A��#A�7LA���A�bNA�VA�=qA���A�33A�  A��FA�G�A�bA�hsA}�#Az�Ay�Ax��Ax�jAxA�Awx�Av��AtbAr��Ap�Ao%Am�Al��AkdZAk"�Aj��AjM�Ai�PAhVAg�AgC�AfĜAf-AcC�Aa�PA`�`A`�\A`M�A`  A^ĜA\��A[��A[G�AZ1'AYXAX��AV9XAQ�;AN=qAK�7AJM�AIS�AH�!AH=qAG��AG��AE;dA@bA9C�A5��A3��A2=qA0ZA-�PA,��A,A+�A+hsA+C�A*��A*ĜA)�mA(�\A'�-A'?}A&��A&z�A%�A#�wA!�FA�
A�uA��A9XA��A�A�FA�yA�A7LA�A%A(�A��A��A7LA��A�/A$�A�FA+A��A�+A �A~�A
��A
��A
�A	��A��A��At�AK�A��A�A=qA��A��Ax�AbAp�A �@�ƨ@�v�@��^@�/@���@�A�@��F@�v�@�bN@�`B@�r�@�@�@���@�z�@��H@�j@�o@�"�@��u@�@��@݁@��@ܴ9@�dZ@�hs@أ�@��m@և+@ӍP@��@�(�@� �@��;@°!@�V@���@�V@��@��m@��
@�x�@��@���@���@��@� �@�~�@��u@��
@�hs@�33@�O�@�t�@�@��@���@�9X@�z�@��@��+@�?}@��m@��@��@�b@�M�@�G�@�1'@�dZ@�=q@��^@��/@���@�ȴ@���@�Ĝ@
=@}O�@z�\@xb@wl�@uO�@t1@r^5@q�7@p�`@n5?@k33@j��@i�@h�u@g�@eO�@co@`��@_�w@^5?@]�h@\��@[�F@Z~�@Xr�@U@T��@S��@R-@Q��@Pb@N�R@NE�@K��@J��@J�\@J~�@I��@G�@E��@E/@D��@DZ@B�@@A�@>{@=?}@<��@<z�@;��@;dZ@9�^@7�@7l�@7
=@6ff@5��@4j@2�H@1��@0  @.v�@-�-@,�/@,I�@*��@)��@(��@(bN@'�w@'�@%�@%`B@#C�@#"�@ �@`B@��@�@t�@n�@X@X@�w@�R@�T@?}@(�@�@��@�7@��@��@�@ȴ@V@{@@�m@33@
�!@	�^@��@Q�@�P@K�@�@@�T@`B@/@��@ƨ@��@�H@-@��@x�@&�@ A�?���?���?���?��H?���?��T?�?}?�S�?��??�O�?�"�?�P?�?�Z?�o?�G�?߾w?�V?�O�?�1?�C�?���?�X?�r�?׮?�l�?�?Լj?Ұ!?�J?��`?�\)?��?Ͳ-?���?̋D?�1?˅?�~�?�=q?ɺ^?ə�?��?���?�b?��?�33?��7?��?�V?��?��m?�ƨ?�C�?�"�?�=q?��?���?��^?��#?��#?��#?��#?�^5?�~�?���?���?�C�?���?��m?�I�?��?��h?��h?��-?���?�{?�{?�5??�V?�v�?�v�?��R?��?��?��?�;d?�;d?�\)?�\)?�|�?���?��w?��;?�  ?� �?�A�?�bN?��A�\)A�^5A�^5A�^5A�\)A�^5A�^5A�\)A�\)A�\)A�bNA�dZA�bNA�`BA�`BA�`BA�^5A�bNA�dZA�hsA�hsA�jA�hsA�ffA�hsA�`BA�I�A�+A�  A��;A�ȴAח�A�Q�A�JA��A֟�A�E�A��AՃA�7LA��A�A���A��A��yA��A�ȴAԬAԉ7A�ffA�M�A�?}A�5?A�/A�(�A�&�A�"�A�"�A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BjBjBk�BiyBffBffBcTB`BBbNBdZBcTBcTBgmBgmBgmBgmBgmBffBffBffBe`Be`Be`Be`Be`Be`Be`Be`Be`BdZBdZBe`BdZBdZBcTB^5B�B��B��BǮB�B��B��B��B��B��B��B��BB�B!�B'�B,B/B6FB7LB;dB?}BD�BF�BI�BI�BN�BT�BW
BW
BgmBdZBe`BgmBgmBffBgmBffBe`Be`BdZBaHB[#B]/B\)BYBYBP�BT�BG�B=qB:^B<jB6FB-B�B{B	7B��B�B�#B�BɺB��B�\B�7B}�Bw�BjBcTBL�B2-B�B\B  B
��B
��B
�qB
�'B
�B
��B
�DB
T�B
A�B
#�B
�B
\B
DB
	7B
B
  B	��B	�mB	�5B	��B	ȴB	�wB	�LB	�'B	�B	�B	��B	��B	��B	��B	�oB	�\B	�7B	t�B	p�B	l�B	iyB	gmB	dZB	[#B	S�B	M�B	I�B	C�B	@�B	;dB	+B	�B	hB	bB	DB	B	B	  B��B��B�B�B�B��B�hB�DB� Bu�Br�Bn�Bl�BjBiyBhsBgmBcTB`BB^5B\)B[#BXBT�BO�BVBS�BR�BM�BI�BL�BJ�BI�BG�BH�BF�BF�BH�BG�BF�BI�BH�BH�BG�BH�BI�BI�BJ�BI�BI�BF�BN�BL�BK�BB�BI�BF�BF�BC�BB�BB�B?}B<jB6FB6FB8RB5?B49B49B49B33B2-B1'B2-B2-B2-B2-B6FB9XB8RB9XB;dB;dB9XB;dB<jB<jB=qB=qB;dB<jB;dB;dB<jB>wBC�BF�BL�BXBXBy�B�=B��B��B��B�B��B�/B�B��B��B	!�B	5?B	8RB	J�B	ZB	`BB	q�B	u�B	�B	�hB	�bB	��B	��B	��B	��B	��B	�B	�-B	�FB	�wB	ÖB	��B	��B	��B	�B	�)B	�HB	�fB	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
+B
+B

=B

=B
JB
PB
VB
hB
�B
{B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
%�B
&�B
'�B
+B
-B
/B
0!B
1'B
2-B
2-B
1'B
5?B
5?B
5?B
5?B
6FB
8RB
;dB
;dB
;dB
;dB
?}B
@�B
D�B
D�B
D�B
E�B
F�B
E�B
H�B
I�B
I�B
J�B
K�B
K�B
L�B
N�B
N�B
P�B
Q�B
R�B
T�B
T�B
W
B
W
B
YB
YB
YB
ZB
\)B
\)B
^5B
\)B
`BB
bNB
bNB
bNB
cTB
e`B
ffB
e`B
hsB
iyB
iyB
jB
k�B
l�B
n�B
n�B
m�B
m�B
p�B
p�B
p�B
q�B
q�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
{�B
|�B
|�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�%B
�1B
�1B
�7B
�JB
�JB
�\B
�\B
�hB
�hB
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
��B
��B
��B
��B
�B
�B
�B
�!B
�-B
�3B
�9B
�9B
�FB
�?B
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�^B
�XB
�XB
�^B
�dB
�^B
�^B
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�qB
�qB
�qBk�Bk�Bk�BjBk�BjBiyBjBk�BjBjBjBjBk�BjBjBjBk�BjBk�Bk�BjBjBk�BiyBffBiyBdZBe`BiyBcTBhsBdZBcTBdZBcTBaHB_;B_;BcTBe`BdZBdZBdZBcTBbNBcTBbNBaHBcTBffBgmBgmBgmBgmBgmBgmBgmBgmBgmG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        BhsBhsBiyBgmBdZBdZBaHB^5B`BBbNBaHBaHBe`Be`Be`Be`Be`BdZBdZBdZBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBbNBcTBbNBbNBaHB\)B~�B��B��BŢB�B��B��B��B��B��B��B��BB�B�B%�B)�B-B49B5?B9XB=qBB�BD�BG�BG�BL�BR�BT�BT�Be`BbNBcTBe`Be`BdZBe`BdZBcTBcTBbNB_;BYB[#BZBW
BW
BN�BR�BE�B;dB8RB:^B49B+B�BoB+B��B�sB�B�BǮB��B�PB�+B{�Bu�BhsBaHBJ�B0!B�BPB
��B
��B
��B
�dB
�B
�B
��B
�7B
R�B
?}B
!�B
�B
PB
	7B
+B
B	��B	��B	�`B	�)B	��B	ƨB	�jB	�?B	�B	�B	��B	��B	��B	��B	�{B	�bB	�PB	�+B	r�B	n�B	jB	gmB	e`B	bNB	YB	Q�B	K�B	G�B	A�B	>wB	9XB	(�B	�B	\B	VB		7B	B	  B��B��B��B�B��B�B��B�\B�7B}�Bs�Bp�Bl�BjBhsBgmBffBe`BaHB^5B\)BZBYBVBR�BM�BS�BQ�BP�BK�BG�BJ�BH�BG�BE�BF�BD�BD�BF�BE�BD�BG�BF�BF�BE�BF�BG�BG�BH�BG�BG�BD�BL�BJ�BI�B@�BG�BD�BD�BA�B@�B@�B=qB:^B49B49B6FB33B2-B2-B2-B1'B0!B/B0!B0!B0!B0!B49B7LB6FB7LB9XB9XB7LB9XB:^B:^B;dB;dB9XB:^B9XB9XB:^B<jBA�BD�BJ�BVBVBw�B�1B�{B��B��B��BɺB�#B�B��B��B	�B	33B	6FB	I�B	YB	_;B	p�B	t�B	�B	�bB	�\B	�{B	��B	��B	��B	��B	�B	�'B	�?B	�qB	B	��B	��B	��B	�
B	�#B	�BB	�`B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
%B
%B
	7B
	7B
DB
JB
PB
bB
{B
uB
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
$�B
%�B
&�B
)�B
,B
.B
/B
0!B
1'B
1'B
0!B
49B
49B
49B
49B
5?B
7LB
:^B
:^B
:^B
:^B
>wB
?}B
C�B
C�B
C�B
D�B
E�B
D�B
G�B
H�B
H�B
I�B
J�B
J�B
K�B
N�B
N�B
P�B
Q�B
R�B
T�B
T�B
W
B
W
B
YB
YB
YB
ZB
\)B
\)B
^5B
\)B
`BB
bNB
bNB
bNB
cTB
e`B
ffB
e`B
hsB
iyB
iyB
jB
k�B
l�B
n�B
n�B
m�B
m�B
p�B
p�B
p�B
q�B
q�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
{�B
|�B
|�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�%B
�1B
�1B
�7B
�JB
�JB
�\B
�\B
�hB
�hB
�oB
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
��B
��B
��B
�B
�B
�B
�!B
�'B
�3B
�9B
�FB
�FB
�RB
�LB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�jB
�dB
�dB
�jB
�qB
�jB
�qB
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�wB
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
��B
��B
��BiyBiyBiyBhsBiyBhsBgmBhsBiyBhsBhsBhsBhsBiyBhsBhsBhsBiyBhsBiyBiyBhsBhsBiyBgmBdZBgmBbNBcTBgmBaHBffBbNBaHBbNBaHB_;B]/B]/BaHBcTBbNBbNBbNBaHB`BBaHB`BB_;BaHBdZBe`Be`Be`Be`Be`Be`Be`Be`Be`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809301702072021061413524020210614135240202106141746442021061417464420210614174644201809301702072021061413524020210614135240202106141746442021061417464420210614174644PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018093017020720180930170207  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018093017020720180930170207QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018093017020720180930170207QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015320210722160153IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                