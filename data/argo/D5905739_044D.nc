CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-07T01:02:21Z creation      
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
_FillValue                 ,  L|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  aP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   <   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�           HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � lArgo profile    3.1 1.2 19500101000000  20181107010221  20210617131508  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               ,   ,DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @،E� ��@،E� ��11  @،E�t�@،E�t�@6���*�@6���*��c�Q֌i/�c�Q֌i/11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?fff@   @Fff@�33@�33@�  @�33A   A��A&ffA@  A^ffA|��A�ffA�ffA���A���A���A���A���A�33B��B��BffB!33B(��B0ffB8ffB@ffBHffBP  BX  B_��Bg��Bo��BxffB�ffB�33B�  B�  B�33B�33B�ffB�  B�  B�33B�ffB�33B�  B���B���B�  B�33B�  BǙ�B���B�ffB�ffB�33B�33B�ffB�ffB�33B�33B���B�ffB���B�ffC 33C�C  C�fC  C	��C33C33C�C�C33C33C�C33C�C33C �C"  C$  C&  C(  C*  C,  C.  C0  C233C433C633C833C:L�C<33C>33C@�CB�CD�CF�CH  CJ  CK�fCN33CPL�CR33CT33CV�CX�CZ  C\  C^  C_�fCa��Cd  Cf  Ch  Cj  Cl  CnL�Cp33Cr�Ct�Cv  Cx  Cy��C|33C~33C�  C�  C��3C��C��C�  C�  C��3C��C��C��3C��3C��fC��C��C��3C��C��C�  C��C��C��C�&fC��C��C�  C��fC��C�&fC��C�  C�&fC��C�  C��C�  C��3C��C�  C��fC�  C��C�&fC��C��3C�  C��C�&fC��C��3C��C�  C��fC�  C��C��C��3C�  C��C��C�  C��C��C��fC��C�  C��fC�  C��C��C��3C��C�  C��3C�  C�&fC��C��fC��C�&fC��C��3C��C�  C��fC�  C�&fC��C��fC��C�&fC�  C��3C�  C��C��C��3C��C��C�&fC�  C��fC��fC��3C��C��C�&fC��C��3C�  C��C��C�&fC�  C��fC�  C��C�&fC��C��fC��3C�  C��fD@ D�fD�3D
L�D��D��DFfD  D� D9�D��DY�D!ٚD$ffD&��D)y�D+�3D.ffD0��D3ffD5� D8l�D;3D=�3D@Y�DB��DE��DH33DJ� DMy�DP3DR�3DUL�DWٚDZl�D\��D_� Da��Dd�fDf��Diy�Dl3Dn�fDq33Ds� Dv��Dy@ D{� D~@ D�y�D��fD�,�D�|�D��3D��D�P D��fD�� D�)�D�i�D���D�� D��D�P D���D�� D��fD�&fD�S3D�s3D���D�� D��3D�	�D�6fD�\�D�� D���D�� D���D���D�  D�<�D�c3D���D�� D���D�3D�)�D�VfD�|�D���D�� D���D�#3D�L�D�|�D���D�ٚD�	�D�)�D�L�D�l�D���D���D�� D�ٚD��3D�3D�&fD�9�D�S3D�ffD�y�D̃3D͓3DΙ�DϦfDг3DѼ�D��3D�ɚD��3D�� D��D���D�3D�fD�	�D�3D�fD�3D�  D�  D��D�  D�&fD�)�D�0 D�9�D�C3D�I�D�S3D�\�D�` D�l�D�3D� D署D� D��3D���D�ٚD��3D�� D�  D�  D�fD��D�  D�fD��D�3D��E �E ��EfE�fE E��EfE�fE;3E@ EFfE��E	�3EVfEY�E��E�fEd�Ei�E� E��E` EVfE��E� E+3E��E��EfE�E x E!�E"� E$NfE%L�E&ɚE'ɚE)D�E*I�E+�3E,њE.X E/l�E1fE23E3�E4��E5�3E6� E8X E9Y�E:� E;� E>�3EB+3EE^fEHc3EK�fEN��EQ� ET�fEW� E[  E^Y�Eal�EdnfEgɚEj��Em�3Ep�fEt  Ewh EzL�E}q�E�I�E��3E�` E�3E���E�%�E��fE�&fE�� E�K3E�ٚE�9�E�zfE�� E��E�3E���E��E�ffE���E�fE�` E���E���E�;3E���E�՚E�2fE���E�� E�+3E�ffE���E��E�P E��3E��3E�@ E�� >���>���>���?   ?   >���>���>���>���>���>���?   >���?   >���>���?   >���>���?��?   ?   ?333?L��?�  ?���?���?�33?ٙ�?�ff@ff@   @333@Fff@Y��@s33@�  @�  @���@�ff@�33@�  @ə�@�ff@�ff@���@���A33A33A��A  AffA&ffA,��A4��A<��AD��AI��AS33A[33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411444144114144144144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ?fff?�33@   @fff@�33@�33@�  @�33A  A��A.ffAH  AfffA�ffA�ffA�ffA���Ař�A���A���A���B��B	��B��BffB#33B*��B2ffB:ffBBffBJffBR  BZ  Ba��Bi��Bq��BzffB�ffB�33B�  B�  B�33B�33B�ffB�  B�  B�33B�ffB�33B�  B���B���B�  B�33B�  Bș�B���B�ffB�ffB�33B�33B�ffB�ffB�33B�33B���B�ffB���B�ffC �3C��C� CffC� C
L�C�3C�3C��C��C�3C�3C��C�3C��C�3C ��C"� C$� C&� C(� C*� C,� C.� C0� C2�3C4�3C6�3C8�3C:��C<�3C>�3C@��CB��CD��CF��CH� CJ� CLffCN�3CP��CR�3CT�3CV��CX��CZ� C\� C^� C`ffCbL�Cd� Cf� Ch� Cj� Cl� Cn��Cp�3Cr��Ct��Cv� Cx� CzL�C|�3C~�3C�@ C�@ C�33C�Y�C�L�C�@ C�@ C�33C�L�C�L�C�33C�33C�&fC�L�C�L�C�33C�Y�C�L�C�@ C�Y�C�Y�C�L�C�ffC�Y�C�L�C�@ C�&fC�L�C�ffC�Y�C�@ C�ffC�L�C�@ C�Y�C�@ C�33C�Y�C�@ C�&fC�@ C�L�C�ffC�Y�C�33C�@ C�Y�C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�@ C�Y�C�L�C�@ C�Y�C�L�C�&fC�L�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�@ C�33C�@ C�ffC�L�C�&fC�L�C�ffC�L�C�33C�L�C�@ C�&fC�@ C�ffC�Y�C�&fC�L�C�ffC�@ C�33C�@ C�Y�C�L�C�33C�L�C�L�C�ffC�@ C�&fC�&fC�33C�L�C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�@ C�&fC�@ C�L�C�ffC�L�C�&fC�33C�@ C�&fD` DfD�3D
l�D�D��DffD  D� DY�D��Dy�D!��D$�fD'�D)��D,3D.�fD1�D3�fD6  D8��D;33D=�3D@y�DC�DE��DHS3DK  DM��DP33DR�3DUl�DW��DZ��D]�D_� Db�Dd�fDg�Di��Dl33Dn�fDqS3Dt  Dv��Dy` D{� D~` D���D��fD�<�D���D��3D��D�` D��fD�� D�9�D�y�D���D�� D�)�D�` D���D�� D�fD�6fD�c3D��3D���D�� D��3D��D�FfD�l�D�� D���D�� D���D��D�0 D�L�D�s3D���D�� D���D�3D�9�D�ffD���D���D�� D�	�D�33D�\�D���D���D��D��D�9�D�\�D�|�D���D���D�� D��D�3D�#3D�6fD�I�D�c3D�vfDˉ�D̓3Dͣ3DΩ�D϶fD��3D���D��3D�ٚD��3D�� D���D��D�3D�fD��D�#3D�&fD�#3D�0 D�0 D�,�D�0 D�6fD�9�D�@ D�I�D�S3D�Y�D�c3D�l�D�p D�|�D�3D� D﹚D�� D��3D���D��D��3D�  D� D� D�fD��D� D�fD��D�#3D�,�E �E ��EfE�fE  E��E&fE�fEC3EH ENfE��E	�3E^fEa�E��E�fEl�Eq�E� E��Eh E^fE��E� E33E��E��EfE�E � E!�E"� E$VfE%T�E&њE'њE)L�E*Q�E+�3E,ٚE.` E/t�E1fE23E3$�E4��E5�3E6� E8` E9a�E:� E;� E>�3EB33EEffEHk3EK�fEN��EQ� ET�fEX  E[ E^a�Eat�EdvfEgњEj��Em�3Ep�fEt( Ewp EzT�E}y�E�M�E��3E�d E�3E���E�)�E��fE�*fE�� E�O3E�ݚE�=�E�~fE�� E� �E��3E���E��E�jfE���E�fE�d E���E� �E�?3E���E�ٚE�6fE���E�� E�/3E�jfE���E��E�T E��3E��3E�D E�� G�O�?L��?fffG�O�G�O�G�O�?L��G�O�G�O�?L��?fffG�O�?L��G�O�G�O�?fffG�O�G�O�?fffG�O�G�O�?�  ?���?�ff?�  ?���?ٙ�?�33@��@33@&ff@@  @S33@fff@y��@���@�  @�  @���@�ff@�33@�  @ٙ�@�ff@�ff@���A��A33A33A��A   A&ffA.ffA4��A<��AD��AL��AQ��A[33Ac33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411444144114144144144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @ �@ %@ �@ *@ �@ "�@ (�@ 0x@ 6�@ >@ F�@ Q�@ ^�@ k�@ y�@ �+@ ��@ �5@ �-@ ��@ �|@ ��@ �m@ �@@@ @-@:�@H]@V@b�@p�@}�@�D@��@��@�F@�>@�7@��@�4@��@1@{@""@0x@>�@K�@X�@e�@s_@��@�@�@�M@��@�W@��@��@�@��@�@�@&;@2�@B8@P�@]�@k.@x&@�@�@�m@��@�@��@׹@�`@�@^@V@�@)�@7�@D�@Q�@_�@m:@z�@��@�0@��@�~@��@�*@��@�y@��@�@o@g@-@:�@H]@UU@b�@o�@�@��@��@��@��@�>@�7@��@�@�~@v@{@""@/�@=q@K@Z�@g�@t�@�d@�\@�@�M@��@�W@Ӡ@�H@�@@��@
�@�@%�@2�@A�@O0@[z@i!@v@��@�u@��@�r@�k@�c@�h@�@�@@@�@(�@5?@D�@S�@`�@m:@|�@�7@�0@�5@�~@�w@�*@�t@�@��@	@	@	 @	+�@	:@	I@	Wb@	c�@	o�@	�@	��@	�<@	�A@	�F@	�>@	ψ@	��@	��@	��@
�@
�@
"�@
.l@
>@
K@
Wb@
ff@
uk@
�d@
��@
��@
��@
��@
��@
խ@
��@
�@
�E@J@�@$�@3�@@�@M$@\)@k�@x�@��@�u@�y@�@�@�c@�h@�`@�@ �@V@[@(�@5?@B�@Q=@`B@m�@|�@�7@��@��@�-@��@��@�t@�@��@@@g@+@9X@G�@S�@�@-@v@��@	�@Q=@�H@�@,`@s_@��@�Q@C�@�7@�*@�@Wb@�H@�;@"�@ff@�@�e@<@�p@�@�@Z�@��@��@1�@y�@��@%@Lu@�@�
@�@`A@�(@�m@.l@t�@�^@j@Lu@�0@Ӡ@�@hs@��@��@C�@�7@ψ@{@Z@�m@�@+@oF@�~@�e@6�@y�@��@��@ >�@ ~�@ �j@ ��@!:@!x&@!��@!� @"5�@"t@"��@"��@#+�@#i!@#�A@#�@$"�@$a�@$�m@$��@%g@%^5@%�a@%�/@&[@&\)@&��@&�#@'�@'[z@'�U@'܀@([@(Z�@(��@(�\@)�@)Q=@)��@)�@*@*A�@*|?@*��@*�@+-�@+hs@+�@+�#@,@,Lu@,��@,�w@,�q@-.l@-g@-�m@-�@.�@.K�@.�@.�^@.�@/*S@/`A@/��@/�7@0%@0=q@0uk@0��@0�@1[@1V@1��@1ƨ@1�Q@26�@2o�@2�Y@2�@3 �@3X�@3�u@3�@4v@4>@4ww@4�~@4�@5 @5X@5��@5��@5��@63�@6l�@6��@6�#@7�@7Ji@7��@7�@7�@8+�@8�h@9G�@9��@:^5@:�*@;s_@;��@<��@<�}@=��@>
�@>�@?�@?��@@$�@@�2@A,`@A�W@Bg@B�C@Co�@C�@Dww@E�@Ez�@F�@F�+@G)�@G��@H8�@H��@ILu@I�k@Jb�@J�@K��@K��@Lk�@M�@M�P@M��@N�A@O*@O�&@P,`@Qn�@R�h@T5�@U�@V�l@X �@Y��@Z��@\%�@]p�@^ލ@`.l@av�@b�`@d'�@ek.@f�@h(�@i�\@j�o@l"�@mx�@n�@pO@q�p@r�#@t-�@uv@v��@xO@yl�@z��@{o@{I�@{�@{�O@|(G@|`B@|�I@|�@}"�@}r�@}@}� @~H]@~}�@~�+@^@P�@�a@Ӡ@�o@�+�@�P�@�r�@��\@���@��I@���@�G�O�@ @ �G�O�G�O�G�O�@ G�O�G�O�@ @ �G�O�@ G�O�G�O�@ �G�O�G�O�@ �G�O�G�O�@ j@ �@ v@ �@ �@ 1@ 	�@ �@ J@ V@ @ @ *@ 6@ �@ O@ �@ !s@ #�@ &;@ (�@ +@ -�@ 1'@ 2�@ 5?@ 7�@ ;d@ >@ @�@ C�@ F�@ I�@ M$@ P�@ S�@ V@ Z@ ]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�K�A�O�A�M�A�O�A�S�A�S�A�ZA�VA�Q�A�O�A�K�A�;dA�-A��A��A��A��A���A���A��A��A��;A��#A��
A���A���A���A���A���A���A���A�ȴA�ƨA�A���AӺ^AӬAӇ+A�I�A�bAҝ�A��yA�K�A�bA���A�\)Aĩ�A���A��A��;A���A�/A�=qA���A��9A��A�%A��A�Q�A���A��/A�hsA��uA���A��A�1A��/A�33A���A�M�A��wA�ƨA���A��A�ƨA�"�A��wA�33A��A�+A�O�A�;dA��DA�"�A�?}A��+A�\)A�1A�ĜA�"�A�x�A��A���A��TA�/A�A�ƨA���A���A�r�A�ffA�z�A��RA�7LA��A��yA���A�(�A�PA~��A}dZA{�Ay�Ax~�Av��Au��At�/AtVArffAp��Ao�An�!AnVAnJAl �Aj�AjZAh�DAf��Af�Ad�RAd=qAc"�Ab�Aa�A_�wA]��A\�!A[A["�AZ�AY�wAV��AV5?AT�RAS?}AQ�;AP�/AOx�AMƨAL��AK�-AJ��AIK�AEƨAC�AC"�AB��AA�A@��A@{A?�7A>�RA>Q�A=�FA="�A<��A<I�A:��A89XA7��A6�A6z�A5��A4bNA3XA2~�A1�
A0ĜA0Q�A/A.��A-�
A-�-A-�A+l�A)G�A'�A&M�A%��A%��A%�A%��A$�`A#�;A#��A#VA!�#A!+A�
AQ�A`BAdZAO�Ar�A��A"�A��A�wA+A=qA��A�A^5AVA=qA��A�A��A�
AXA(�AAbNA�#A
��A
{A	�mA	|�A	
=A�AƨAdZA^5A�PAXAoA5?A�/A��A ��@��;@��w@�Z@�G�@�;d@��@��D@���@��@�n�@�-@��;@��@�G�@� �@җ�@�J@�dZ@Ȭ@�O�@�z�@�ȴ@��F@��@�E�@��@�5?@���@�@�`B@��u@��P@�-@��@���@�E�@��j@�v�@�7L@��D@�~�@�bN@��@���@���@�r�@��@�Z@�t�@��@��#@���@���@��#@��u@��@�X@�I�@�\)@��+@��@�%@��D@�w@~$�@|z�@z�!@xA�@w��@u�h@t�@rn�@p �@nv�@m�@m`B@j�\@hA�@f$�@d�D@c"�@aG�@_+@\z�@Z�!@Y��@XbN@Vȴ@U?}@SC�@Q�@P  @OK�@N@L�@K33@Ix�@G�@G\)@FV@EO�@D��@D��@C�
@A�^@@Ĝ@?�@>�@=�h@<Z@;"�@:n�@9��@9�@8A�@7�P@4�/@3@2�@0�`@/�;@.�y@.$�@,�@,(�@,1@+t�@)&�@(��@'�;@'l�@&��@%�-@$z�@#��@"�H@!�7@�@�R@/@��@�m@��@�@Ĝ@�@ȴ@ff@��@�j@�m@33@-@X@bN@��@��@ff@@��@?}@�/@I�@�
@
�@
n�@	�@	x�@�`@�9@+@ȴ@v�@�T@`B@Z@ƨ@S�@o@~�@n�@�@hs@hs@7L@ �9?�5??�(�?��#?�l�?��T?�33?��?�;d?�p�?��m?ꟾ?�u?�P?�?䛦?�\?�%?�|�?��?���?�dZ?��#?�7L?�b?֧�?��/?�9X?�o?�M�?�%?Ͼw?θR?�5??���?�1?ʟ�?ʟ�?��#?ȴ9?�r�?�+?�
=?Ƈ+?�?�`B?��?�M�?�Ĝ?�A�?�;d?��R?���?�O�?��?�(�?�dZ?���?���?�?�C�?�?�"�?�"�?�?�"�?�dZ?��?���?�1?�I�?���?�O�?��-?�V?��?���?�A�?�bN?�bN?���?�Ĝ?�%?�%?�&�?�G�?�hs?��7?��7?���?�J?�J?�M�?�M�?�M�?°!?\?��?�o?�33?�S�?�t�?Õ�?��
?���?��A�VA�XA�VA�XA�S�A�VA�S�A�S�A�Q�A�M�A�O�A�O�A�O�A�K�A�I�A�K�A�K�A�K�A�M�A�K�A�I�A�I�A�I�A�G�A�I�A�I�A�M�A�O�A�O�A�O�A�Q�A�O�A�Q�A�M�A�K�A�K�A�M�A�S�A�S�A�S�A�S�A�S�A�VA�ZA�ZA�\)A�VA�VA�Q�A�Q�A�Q�A�Q�A�O�A�M�A�K�A�K�A�M�A�I�A�E�A�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A�Q�A�K�A�O�A�M�A�O�A�S�A�S�A�ZA�VA�Q�A�O�A�K�A�;dA�-A��A��A��A��A���A���A��A��A��;A��#A��
A���A���A���A���A���A���A���A�ȴA�ƨA�A���AӺ^AӬAӇ+A�I�A�bAҝ�A��yA�K�A�bA���A�\)Aĩ�A���A��A��;A���A�/A�=qA���A��9A��A�%A��A�Q�A���A��/A�hsA��uA���A��A�1A��/A�33A���A�M�A��wA�ƨA���A��A�ƨA�"�A��wA�33A��A�+A�O�A�;dA��DA�"�A�?}A��+A�\)A�1A�ĜA�"�A�x�A��A���A��TA�/A�A�ƨA���A���A�r�A�ffA�z�A��RA�7LA��A��yA���A�(�A�PA~��A}dZA{�Ay�Ax~�Av��Au��At�/AtVArffAp��Ao�An�!AnVAnJAl �Aj�AjZAh�DAf��Af�Ad�RAd=qAc"�Ab�Aa�A_�wA]��A\�!A[A["�AZ�AY�wAV��AV5?AT�RAS?}AQ�;AP�/AOx�AMƨAL��AK�-AJ��AIK�AEƨAC�AC"�AB��AA�A@��A@{A?�7A>�RA>Q�A=�FA="�A<��A<I�A:��A89XA7��A6�A6z�A5��A4bNA3XA2~�A1�
A0ĜA0Q�A/A.��A-�
A-�-A-�A+l�A)G�A'�A&M�A%��A%��A%�A%��A$�`A#�;A#��A#VA!�#A!+A�
AQ�A`BAdZAO�Ar�A��A"�A��A�wA+A=qA��A�A^5AVA=qA��A�A��A�
AXA(�AAbNA�#A
��A
{A	�mA	|�A	
=A�AƨAdZA^5A�PAXAoA5?A�/A��A ��@��;@��w@�Z@�G�@�;d@��@��D@���@��@�n�@�-@��;@��@�G�@� �@җ�@�J@�dZ@Ȭ@�O�@�z�@�ȴ@��F@��@�E�@��@�5?@���@�@�`B@��u@��P@�-@��@���@�E�@��j@�v�@�7L@��D@�~�@�bN@��@���@���@�r�@��@�Z@�t�@��@��#@���@���@��#@��u@��@�X@�I�@�\)@��+@��@�%@��D@�w@~$�@|z�@z�!@xA�@w��@u�h@t�@rn�@p �@nv�@m�@m`B@j�\@hA�@f$�@d�D@c"�@aG�@_+@\z�@Z�!@Y��@XbN@Vȴ@U?}@SC�@Q�@P  @OK�@N@L�@K33@Ix�@G�@G\)@FV@EO�@D��@D��@C�
@A�^@@Ĝ@?�@>�@=�h@<Z@;"�@:n�@9��@9�@8A�@7�P@4�/@3@2�@0�`@/�;@.�y@.$�@,�@,(�@,1@+t�@)&�@(��@'�;@'l�@&��@%�-@$z�@#��@"�H@!�7@�@�R@/@��@�m@��@�@Ĝ@�@ȴ@ff@��@�j@�m@33@-@X@bN@��@��@ff@@��@?}@�/@I�@�
@
�@
n�@	�@	x�@�`@�9@+@ȴ@v�@�T@`B@Z@ƨ@S�@o@~�@n�@�@hs@hs@7L@ �9?�5??�(�?��#?�l�?��T?�33?��?�;d?�p�?��m?ꟾ?�u?�P?�?䛦?�\?�%?�|�?��?���?�dZ?��#?�7L?�b?֧�?��/?�9X?�o?�M�?�%?Ͼw?θR?�5??���?�1?ʟ�?ʟ�?��#?ȴ9?�r�?�+?�
=?Ƈ+?�?�`B?��?�M�?�Ĝ?�A�?�;d?��R?���?�O�?��?�(�?�dZ?���?���?�?�C�?�?�"�?�"�?�?�"�?�dZ?��?���?�1?�I�?���?�O�?��-?�V?��?���?�A�?�bN?�bN?���?�Ĝ?�%?�%?�&�?�G�?�hs?��7?��7?���?�J?�J?�M�?�M�?�M�?°!?\?��?�o?�33?�S�?�t�?Õ�?��
?���?��A�VA�XA�VA�XA�S�A�VA�S�A�S�A�Q�A�M�A�O�A�O�A�O�A�K�A�I�A�K�A�K�A�K�A�M�A�K�A�I�A�I�A�I�A�G�A�I�A�I�A�M�A�O�A�O�A�O�A�Q�A�O�A�Q�A�M�A�K�A�K�A�M�A�S�A�S�A�S�A�S�A�S�A�VA�ZA�ZA�\)A�VA�VA�Q�A�Q�A�Q�A�Q�A�O�A�M�A�K�A�K�A�M�A�I�A�E�A�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB%B%BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB%B1B\B�B �B,B>wBXBp�Bp�Br�Bw�B�B�B�1B�hB��B�bB��B��B��B��B��B��B�uB�{B�oB�oB�VB�VB�VB�B�B}�B|�Bt�Br�Bk�Be`BaHBQ�BB�B5?B&�B�BB�TBB��B��B�B^5BK�BB�B6FB-B&�B�BoB\B
��B
��B
�HB
��B
�B
��B
�bB
�7B
o�B
aHB
Q�B
G�B
?}B
9XB
.B
 �B
uB
JB
PB
�B
�B
VB	��B	��B	�B	�B	�sB	�`B	�B	�#B	�
B	ɺB	ŢB	��B	��B	��B	ƨB	ŢB	�}B	�3B	�B	��B	��B	��B	�uB	�PB	�B	{�B	r�B	jB	cTB	^5B	S�B	L�B	G�B	B�B	;dB	2-B	�B	�B	uB	bB	
=B	+B	B	B	  B��B��B��B��B��B�B�fB�HB�;B�/B�B��BɺBɺBŢB��B�wB�RB�B�!B�B�B��B�{B�\B�1B�=B�7B�=B�VB�hB�\B�JB�JB�+B�Bz�Bp�Bo�Bq�Bu�Br�Bn�Be`BbNB`BBZBYBXBT�BQ�BP�BP�BN�BK�BJ�BH�BG�BE�BC�BB�BA�B>wB@�B?}B?}B>wB;dB;dB9XB9XB:^B9XB9XB8RB:^B8RB<jB:^B=qB9XB<jB;dB;dB6FB7LB9XB7LB7LB7LB8RB6FB>wBE�BI�BK�BP�BYBiyBs�B��B��BB��B��B�B�B	�B	#�B	)�B	6FB	@�B	F�B	YB	jB	q�B	x�B	�B	�bB	�oB	��B	��B	��B	�B	�B	�?B	�dB	ĜB	ŢB	��B	��B	��B	�B	�NB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
%B
1B
DB
JB
\B
bB
hB
{B
�B
�B
�B
�B
�B
!�B
#�B
%�B
&�B
'�B
)�B
+B
.B
/B
1'B
1'B
2-B
49B
49B
6FB
8RB
8RB
8RB
9XB
:^B
:^B
<jB
>wB
?}B
?}B
@�B
B�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
J�B
K�B
L�B
M�B
N�B
O�B
P�B
R�B
R�B
Q�B
S�B
VB
VB
XB
XB
XB
YB
[#B
[#B
\)B
]/B
_;B
`BB
bNB
bNB
cTB
dZB
e`B
ffB
gmB
hsB
hsB
iyB
jB
k�B
k�B
m�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
t�B
u�B
u�B
v�B
w�B
v�B
x�B
y�B
y�B
y�B
z�B
{�B
{�B
|�B
|�B
~�B
|�B
~�B
� B
~�B
~�B
� B
�B
�B
�B
�B
�+B
�1B
�7B
�DB
�JB
�JB
�PB
�\B
�bB
�hB
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
��B
��B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�-B
�9B
�9B
�?B
�?B
�?B
�?B
�LB
�LB
�LB
�RB
�RB
�XB
�RB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^BBBBB%BB%BBB%B%BB%B%B%BB%BBB%B%B%B%B%B%B%B%B%B%B%BBBBB%BB%BBBBBBBBBBBBBBBBBBBBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBBBIBoB �B+�B>fBW�Bp�Bp�Br�Bw�B��B�B�#B�ZB��B�UB��B��B��B��B��B��B�kB�rB�fB�gB�NB�OB�OB� B�B}�B|�Bt�Br�Bk�Be]BaFBQ�BB�B5>B&�B�BB�UBB��B��B�B^7BK�BB�B6JB-B&�B�BuBbB
��B
��B
�PB
��B
�B
��B
�kB
�AB
o�B
aRB
Q�B
G�B
?�B
9dB
.!B
 �B
�B
XB
_B
�B
�B
fB	��B	��B	�B	��B	�B	�sB	�$B	�7B	�B	��B	ŸB	��B	��B	��B	��B	źB	��B	�LB	�(B	��B	��B	��B	��B	�lB	�.B	|B	r�B	j�B	crB	^TB	TB	L�B	G�B	B�B	;�B	2OB	�B	�B	�B	�B	
aB	OB	>B	+B	 &B�B�B�B��B��B�B�B�rB�eB�ZB�0B�B��B��B��B��B��B��B�CB�QB�EB�EB�B��B��B�dB�pB�kB�qB��B��B��B��B��B�cB�QB{Bp�Bo�Bq�Bu�Br�Bn�Be�Bb�B`BZZBYUBXOBU=BR,BQ%BQ&BOBL	BKBH�BG�BE�BC�BB�BA�B>�B@�B?�B?�B>�B;�B;�B9�B9�B:�B9�B9�B8�B:�B8�B<�B:�B=�B9�B<�B;�B;�B6�B7�B9�B7�B7�B7�B8�B6�B>�BE�BJBL)BQJBYBi�Bt$B��B�WB�B�;B�{B��B�-B	B	$`B	*�B	6�B	AB	G=B	Y�B	kB	rHB	yvB	��B	�	B	�B	�MB	�oB	��B	��B	��B	��B	�$B	�_B	�hB	̐B	ѱB	��B	��B	�#B	�WB	�`B	�iB	�fB	�|B	��B	��B	��B	��B	��B
�B
B
B
%B
	4B
JB
SB
hB
qB
zB
�B
�B
�B
�B
�B
 �B
"�B
% B
'B
(B
)!B
+0B
,8B
/MB
0WB
2fB
2hB
3qB
5�B
5�B
7�B
9�B
9�B
9�B
:�B
;�B
;�B
=�B
?�B
@�B
@�B
A�B
C�B
EB
E	B
FB
GB
H#B
H&B
I/B
LDB
MMB
NVB
O^B
PgB
QpB
RxB
T�B
T�B
S�B
U�B
W�B
W�B
Y�B
Y�B
Y�B
Z�B
\�B
\�B
]�B
^�B
`�B
a�B
dB
dB
eB
f B
g(B
h1B
i:B
jBB
jEB
kMB
lVB
m^B
maB
ooB
pyB
p{B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
v�B
w�B
w�B
x�B
y�B
x�B
z�B
{�B
{�B
{�B
|�B
}�B
~B
B
B
�B
B
� B
�)B
�%B
�(B
�0B
�DB
�VB
�[B
�hB
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
�B
�B
�B
�,B
�6B
�=B
�HB
�OB
�iB
�nB
��B
��B
��B
��B
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
�B
�B
�%B
�-B
�8B
�RB
�nB
��B
��B
��B
��B
��B
��B
��B
�B
�#B
�8B
�GB
�WB
�fB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�.B
�=B
�LB
�ZB
�dB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811070102212021061413555020210614135550202106171313242021061713132420210617131324201811070102212021061413555020210614135550202106171313242021061713132420210617131324PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018110701022120181107010221  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110701022120181107010221QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018110701022120181107010221QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150820210617131508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                