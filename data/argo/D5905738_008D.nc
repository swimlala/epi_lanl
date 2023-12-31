CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:31Z creation      
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
_FillValue                 4  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   (   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       |   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180724220231  20210722160148  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�ebj��@�ebj��11  @�e[�`@�e[�`@6�8�4֡@6�8�4֡�cɂ@��4�cɂ@��411  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @�33@�ff@�33@�  A��A  A#33AA��Ac33A�  A�  A���A���A���A���A�  A�33A�33B33B33B  B ffB(  B0  B8  B@ffBHffBPffBXffB`  Bg33Bo��Bx  B��B���B�  B���B�33B�33B���B�  B���B�  B���B���B�  B�33B�33B�33B�  B�  B�ffB���B�  Bә�B�ffB�33B�  B�ffB�ffB�33B�  B���B�ffB�33C   C  C�fC33CL�C
�C�C  C�C�C�C�3C�fC33CffC  C 33C"L�C$  C%�fC(  C)�fC+�fC-�fC/�fC1�fC433C633C8�C:�C<  C>L�C@33CB33CD33CF�CH  CI�fCL33CN�CO�fCR33CT�CU�fCX33CZ33C\�C^�C`�Cb  Cd  Cf  Cg��Cj�Cl�Cm�fCp�Cr33Ct�Cu�fCx�Cz33C|�C~  C��C�&fC��C�  C��C�&fC��C��C�&fC��C��C��C��C�  C��C�&fC��C��3C��C�&fC�33C��C�  C�&fC�  C��fC�  C��C�&fC��C��3C��C��C��3C��C��C��fC��C�  C��3C��C��C��3C��C��C��C��C��C��C��C��C�  C�  C��3C��3C��3C��3C�  C��3C��3C�&fC�&fC��C�&fC��C��C��C�&fC��C�&fC��C�  C�  C��C��C��3C��C��C��C��C�  C��C��C�  C��C��C�  C��C��C��fC��C��C��C�  C��C��C��fC��3C��C��C��C��3C�ٚC��3C��3C�  C��C��C�&fC��C��3C��3C�  C��C��C��C�&fC�33C��C��fC��fC��fC�  C��C��C��C�&fC�@ C��fD � D�D�3D�Dy�D` D
  D�3Dl�D33D�DٚD� DffD 9�D"��D%��D(ffD+�D-��D0Y�D2�3D5�fD89�D:� D=ffD?��DB�fDE  DG��DJ9�DL��DOl�DR  DT�3DW&fDY��D\s3D_�Da�3DdS3Df�3Di�3Dl33Dn��Dql�Dt�Dv� Dy@ D{ffD~fD�I�D���D��D�9�D���D��fD�,�D�|�D��3D�fD�L�D���D��3D�fD�\�D���D���D�  D�c3D��3D��fD�&fD�` D���D��3D�&fD�ffD��fD�� D�3D�C3D�s3D���D��D��D�I�D�y�D��fD���D�  D�,�D�VfD��3D�� D�ٚD�fD�33D�` D���D��3D���D��D�@ D�l�D  D���D���D�,�D�Y�Dȉ�DɶfD��fD��D�S3D΃3D϶fD��3D��D�VfDԆfDճ3D�ٚD�	�D�6fD�ffDۓ3Dܼ�D��3D���D��D�@ D�i�D��D�fD�ٚD���D�  D�<�D�Y�D�s3D��D��D� D��3D���D���D���D�  D�	�D�3D��D��D��D��D�fD�  D���D���D���D���E |�E ��Et�E�EnfE� Ec3E��EY�EњEK3EɚEɚE�3E	FfE
H E�3E��E)�EfE��E�fE�E!�E��E�3E;3EH ET�E� E  EfE�fE � E"S3E#c3E$t�E&fE'fE(6fE)�3E*� E+�3E-�fE.��E/�3E1#3E23E3� E5  E5�fE7L�E8��E9� E:�3E<&fE?4�EB�3EE�fEH�fEK� EN� ER4�EU8 EX)�E[�fE^�fEa�3Ed�fEh( Ej�3EnfEq33EtA�Ew��Ez�3E}�fE�c3E�� E���E� E���E�< E�� E�` E��fE���E�3E�k3E��3E��fE�U�E���E��3E�G3E���E�� E�5�E�x E���E� E�u�E��fE�fE�NfE�� E��3E�T�E�� E��fE�D E���E���E�0 >���>���>���>���>���?   ?   ?   ?��?��?��?��?333?333?L��?L��?�  ?fff?���?���?�33?�33?ٙ�?�33@ff@��@&ff@9��@L��@Y��@l��@y��@�ff@�  @���@�ff@�  @���@�ff@�33@�  @���@���AffA33A��A33A#33A+33A1��A;33AA��AI��AP  AX  A^ffAfffAnffAt��A|��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414441414141114111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?fff?�33@   @`  @�33@�ff@�33@�  A	��A  A+33AI��Ak33A�  A�  A���A���A���A���A�  A�33B��B	33B33B  B"ffB*  B2  B:  BBffBJffBRffBZffBb  Bi33Bq��Bz  B���B���B�  B���B�33B�33B���B�  B���B�  B���B���B�  B�33B�33B�33B�  B�  B�ffB���B�  Bԙ�B�ffB�33B�  B�ffB�ffB�33B�  B���B�ffB�33C � C� CffC�3C��C
��C��C� C��C��C��C33CffC�3C�fC� C �3C"��C$� C&ffC(� C*ffC,ffC.ffC0ffC2ffC4�3C6�3C8��C:��C<� C>��C@�3CB�3CD�3CF��CH� CJffCL�3CN��CPffCR�3CT��CVffCX�3CZ�3C\��C^��C`��Cb� Cd� Cf� ChL�Cj��Cl��CnffCp��Cr�3Ct��CvffCx��Cz�3C|��C~� C�L�C�ffC�Y�C�@ C�L�C�ffC�Y�C�L�C�ffC�Y�C�L�C�Y�C�L�C�@ C�L�C�ffC�L�C�33C�L�C�ffC�s3C�Y�C�@ C�ffC�@ C�&fC�@ C�L�C�ffC�L�C�33C�Y�C�L�C�33C�L�C�L�C�&fC�L�C�@ C�33C�Y�C�L�C�33C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�33C�33C�33C�33C�@ C�33C�33C�ffC�ffC�Y�C�ffC�Y�C�Y�C�Y�C�ffC�Y�C�ffC�L�C�@ C�@ C�Y�C�L�C�33C�Y�C�Y�C�L�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�L�C�@ C�Y�C�L�C�&fC�L�C�Y�C�Y�C�@ C�Y�C�L�C�&fC�33C�L�C�Y�C�Y�C�33C��C�33C�33C�@ C�Y�C�Y�C�ffC�L�C�33C�33C�@ C�L�C�L�C�Y�C�ffC�s3C�L�C�&fC�&fC�&fC�@ C�L�C�L�C�Y�C�ffC�� D 3D � D,�D�3D9�D��D� D
  D�3D��DS3D,�D��D� D�fD Y�D#�D%��D(�fD+,�D-ٚD0y�D33D5�fD8Y�D;  D=�fD@�DB�fDE@ DG��DJY�DL��DO��DR  DT�3DWFfDY��D\�3D_9�Da�3Dds3Dg3Di�3DlS3Dn��Dq��Dt,�Dv� Dy` D{�fD~&fD�Y�D���D���D�I�D���D��fD�<�D���D��3D�fD�\�D���D��3D�&fD�l�D���D���D�0 D�s3D��3D��fD�6fD�p D���D��3D�6fD�vfD��fD�� D�#3D�S3D��3D���D���D�)�D�Y�D���D��fD���D� D�<�D�ffD��3D�� D��D�fD�C3D�p D���D��3D���D��D�P D�|�D° D���D�	�D�<�D�i�Dș�D��fD��fD�)�D�c3DΓ3D��fD��3D�)�D�ffDԖfD��3D��D��D�FfD�vfDۣ3D���D��3D��D�,�D�P D�y�D��D��fD��D��D�0 D�L�D�i�D�3D��D���D�� D��3D���D���D�	�D� D��D�#3D�)�D�)�D�,�D�)�D�fD� D�	�D�	�D�	�E �E ��E�E|�E��EvfE� Ek3E��Ea�EٚES3EњEњE�3E	NfE
P E�3E��E1�E&fE��E�fE$�E)�E��E�3EC3EP E\�E� E EfE�fE � E"[3E#k3E$|�E&fE'&fE(>fE)�3E*� E+�3E-�fE.��E/�3E1+3E2#3E3� E5 E5�fE7T�E8��E:  E:�3E<.fE?<�EB�3EE�fEH�fEK� EN� ER<�EU@ EX1�E[�fE^�fEa�3Ed�fEh0 Ej�3EnfEq;3EtI�Ew��Ez�3E}�fE�g3E�� E���E�  E���E�@ E�� E�d E��fE���E�3E�o3E��3E��fE�Y�E���E��3E�K3E���E�� E�9�E�| E���E� E�y�E��fE�fE�RfE�� E�3E�X�E�� E��fE�H E���E���E�4 G�O�G�O�G�O�G�O�?fffG�O�G�O�?�  G�O�G�O�G�O�?���G�O�?���G�O�?�ffG�O�?�33?���?ٙ�G�O�?�33@��@��@&ff@9��@Fff@Y��@l��@y��@�ff@���@�ff@�  @���@�ff@�  @���@�ff@�33@�  @���A��AffA33A��A#33A+33A333A9��AC33AI��AQ��AX  A`  AfffAnffAvffA|��A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414441414141114111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ �@ %@ �@ {@ �@ #�@ )�@ /�@ 7L@ =q@ E�@ R�@ `�@ m:@ z�@ �7@ ��@ ��@ �-@ �&@ �@ ��@ �@ �e@j@�@�@,`@:@H]@V@c�@qS@~K@��@��@�A@�9@��@�7@�/@�4@��@%@{@!s@/�@<@I�@X�@g@t�@�d@�\@�@��@��@��@�C@�@�@��@�@B@&;@33@@,@O�@\�@i�@ww@�p@�#@�z@��@�k@�c@׹@�`@�@��@�@�@+�@6�@E�@S�@_�@l�@z�@��@��@�(@��@�w@�*@��@��@�q@j@@ @-�@;d@H]@UU@bN@r@~�@�D@��@��@�9@��@є@ލ@�4@��@�@{@""@.l@>@K�@X@g@uk@�d@��@��@�@�@��@�O@�T@�L@��@
�@�@&�@3�@B�@O�@\�@k.@x&@�@�u@�y@��@�@�@�@�m@�@  @�@O@'�@6�@D�@S�@`B@l�@|?@�7@��@��@�-@��@�|@�t@�m@� @	@	b@	 @	-�@	:�@	H]@	V@	c�@	qS@	~�@	��@	��@	��@	�9@	��@	ψ@	��@	��@	�~@
�@
�@
#�@
1�@
>�@
Lu@
Z@
hs@
uk@
��@
�@
�@
��@
��@
ƨ@
��@
�@
�L@
�E@
�@�@&�@4�@@�@O�@\�@i�@x�@��@�h@�@�r@�@�c@�h@�`@��@�Q@V@�@*S@5�@B8@Q=@^�@m:@|?@��@�<@��@��@�w@��@�#@��@� @v@�@g@+@8�@FQ@UU@c�@qS@�@��@�@��@��@��@�C@��@��@p�@�R@@Lu@�<@�@2�@~K@�@6@bN@�@�q@>�@��@ψ@�@\�@��@��@3�@z3@��@�@Lu@�@�h@ @ff@��@�@;d@��@�@@Z�@�y@�(@1�@x�@��@1@N�@�0@��@�@^5@��@�@5@@|�@��@�@T�@�H@�;@$�@i!@��@�@9X@}�@��@ �@ K�@ �@ ��@!B@!\)@!��@!�`@"*S@"n�@"��@"��@#7L@#x&@#�@#��@$?}@$�W@$�2@%@%B8@%�@%@&�@&B8@&�d@&@'@'B8@'�d@'@(@(A�@(�@(��@)j@)C�@)�@)�J@*v@*F�@*�+@*�@+1@+I@+��@+�|@,V@,O�@,�@,�C@-�@-V�@-��@-խ@.�@.V�@.��@.׹@/6@/R�@/�@/ψ@0�@0M$@0�D@0��@1�@1F�@1�@1��@1��@2:�@2v�@2��@2�@3&;@3bN@3�U@3խ@4�@4FQ@4~�@4��@4�@5$�@5Z�@5�P@5@5��@6.l@6e	@6��@6�C@7�@7<@7qS@7��@7�t@8@8B�@8x&@8�Z@8�;@9*@9�d@9�L@:�@;  @;�a@<
�@<��@=b@=�!@>	@>Ĝ@?3�@?܀@@Lu@@�@Aff@A�@B��@B��@Cs_@D!s@D�0@EB8@E�F@F+@F��@GM�@G�J@Huk@H�@I]�@J�@J�W@J�L@K�#@K��@L�@M:@M��@N5@@N�@OX�@O��@PF�@Q��@S1@TJi@U�9@V�Y@X:@Y�!@Z�,@\:�@]��@^�@`;d@a��@c�@d;d@e�@f�y@h7L@i��@j�L@lT�@m��@n�@pSI@q�0@r�@t@�@u��@v��@xM$@y�a@z�@{<�@{v�@{� @|�@|>�@|z2@|��@}@}G�@}�a@}�
@~)�@~_�@~�r@~�@5?@hr@��@�Q@�$/@�=q@�d@��=@���@�ɺ@���G�O�G�O�G�O�G�O�@ �G�O�G�O�@ jG�O�G�O�G�O�@ G�O�@ �G�O�@ vG�O�@ %@ �@ 1G�O�@ 	�@ �@ �@ V@ b@ �@ �@ �@ 6@ B@ �@ �@ �@  �@ #�@ %�@ (G@ *S@ -@ /�@ 2�@ 5?@ 9X@ ;d@ ?}@ B8@ E�@ I@ K�@ O�@ R�@ V@ X�@ \)@ ^�@ bN@ e�@ hs@ k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ƨA��;A��TA��`A��TA��TA��TA��TA��TA��;A��TA��yA��A��A��A��A���A���A���A���A���A���A���A���A�M�AŅA¸RA��/A��A�{A�l�A���A�bA���A��A�ffA�33A�1A�|�A�A��A���A�bNA�%A�VA��9A�(�A���A���A�ffA�bA��A��+A��A��A��TA��A���A���A�bA���A�5?A�JA��A�bA�t�A��A�dZA���A�bA��A�VA�ffA��yA�A��A���A�hsA�9XA�A��7A���A��\A�S�A���A�M�A��!A�r�A���A�7LA���A�l�A�+A���A��A��`A�+A�ĜA���A��A�l�A�A�"�A��A��mA�1'A���A�v�A��#A��;A��#A��^A�^5A���A��A�bA�I�A�A�A�9XA�1'A�VA��A��A�ƨA�v�A�
=A&�A|�A{|�Az�`Ay�hAv�+At��Aq��Aox�AhĜAc"�A`��A_��A]��A\�HA\$�A[S�AZffAYl�AXQ�AV�ATĜAS%AQ�AP��APr�AP9XAOS�AN1AM33AL��AL�DALbAJ�jAI�TAH$�AEC�AD �AC��AC�AB�AB{A@ȴA>1A=��A<�A:�A9�A8��A5�A49XA3p�A1�#A0��A/`BA.M�A+�wA)��A)��A)t�A(�`A((�A&^5A%\)A$�A$I�A#t�A"n�A"$�A!�TA!�;A!�TA!x�A �/A $�A��AA�
Az�AȴA��A
=A�uA�AĜAA�mA��A�A�hA?}A��A��A^5AAz�AA�hAS�AoAz�A�#AC�A	�
A	l�A��A��A33A�Ar�A?}A��A-A �D@���@��@�b@�-@���@��7@�;d@�@��@�F@�
=@�9@�@�
=@��@��@�Z@�1@�w@�;d@旍@�n�@�V@�V@�E�@��@�x�@ͺ^@�"�@� �@�z�@�33@�5?@�=q@���@�\)@�K�@�dZ@��\@�O�@�`B@��@��;@���@���@�^5@�V@���@�S�@���@��#@�ff@��@���@���@��w@��m@��F@��!@�@�/@�S�@���@���@��u@�33@�V@�7L@���@��@�$�@�p�@��j@��@K�@}?}@z��@y�^@x�`@v{@so@qhs@o�@m�-@kS�@i��@g\)@e�T@d1@bn�@ax�@`1'@^$�@]�h@\1@Y�^@X�u@W�@UO�@T(�@R^5@P�`@P1'@O��@Nȴ@L��@Kt�@I�7@G�@F�+@E�@D(�@Ahs@>�@>v�@=/@:�!@8Ĝ@8bN@7
=@6V@5?}@4z�@2�@1x�@/�@.�@.��@.v�@-��@-V@+�m@+33@*~�@)&�@(A�@'�@&ȴ@$�@#��@"�!@!�#@!&�@   @�@@`B@�/@�@n�@X@��@bN@�w@��@@Z@S�@�\@�@X@Ĝ@A�@  @�@�R@5?@�h@?}@��@�F@@
-@	�7@bN@K�@��@ff@�T@�-@O�@�/@�m@C�@�@^5@��@ Ĝ@ ��@ r�?�\)?�v�?��-?�I�?�~�?���?��j?�33?�Ĝ?�  ?�v�?�O�?�?���?�+?�j?�S�?��?��;?��?�/?��m?�"�?ٺ^?���?�b?��y?ա�?Լj?���?���?�-?��`?� �?ϝ�?��?�V?Ͳ-?��?�j?˅?��H?ʟ�?ɺ^?��?�1'?��?�
=?�?}?��?���?�hs?��`?��w?���?�5??�p�?���?�I�?�1?��m?�dZ?�dZ?�"�?�dZ?�?�dZ?�dZ?��?���?��m?�j?��?�V?��h?��?�v�?���?���?� �?�bN?�bN?��?���?��`?��`?�&�?�&�?�G�?�&�?�G�?�hs?��7?�hs?�G�?�G�?�%?�Ĝ?�A�?�bN?��?��?�Ĝ?��`?�%?�&�A�ĜA�ƨA�ȴA���A���A���A���A��
A���A���A���A���A�A���A���A�ĜA���A���A���A�A�ƨA�ƨA��
A��/A��`A��TA��TA��TA��TA��`A��`A��TA��TA��`A��TA��TA��HA��HA��TA��TA��TA��TA��HA��TA��TA��TA��HA��;A��/A��;A��HA��HA��TA��mA��yA��mA��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A���A�ƨA��;A��TA��`A��TA��TA��TA��TA��TA��;A��TA��yA��A��A��A��A���A���A���A���A���A���A���A���A�M�AŅA¸RA��/A��A�{A�l�A���A�bA���A��A�ffA�33A�1A�|�A�A��A���A�bNA�%A�VA��9A�(�A���A���A�ffA�bA��A��+A��A��A��TA��A���A���A�bA���A�5?A�JA��A�bA�t�A��A�dZA���A�bA��A�VA�ffA��yA�A��A���A�hsA�9XA�A��7A���A��\A�S�A���A�M�A��!A�r�A���A�7LA���A�l�A�+A���A��A��`A�+A�ĜA���A��A�l�A�A�"�A��A��mA�1'A���A�v�A��#A��;A��#A��^A�^5A���A��A�bA�I�A�A�A�9XA�1'A�VA��A��A�ƨA�v�A�
=A&�A|�A{|�Az�`Ay�hAv�+At��Aq��Aox�AhĜAc"�A`��A_��A]��A\�HA\$�A[S�AZffAYl�AXQ�AV�ATĜAS%AQ�AP��APr�AP9XAOS�AN1AM33AL��AL�DALbAJ�jAI�TAH$�AEC�AD �AC��AC�AB�AB{A@ȴA>1A=��A<�A:�A9�A8��A5�A49XA3p�A1�#A0��A/`BA.M�A+�wA)��A)��A)t�A(�`A((�A&^5A%\)A$�A$I�A#t�A"n�A"$�A!�TA!�;A!�TA!x�A �/A $�A��AA�
Az�AȴA��A
=A�uA�AĜAA�mA��A�A�hA?}A��A��A^5AAz�AA�hAS�AoAz�A�#AC�A	�
A	l�A��A��A33A�Ar�A?}A��A-A �D@���@��@�b@�-@���@��7@�;d@�@��@�F@�
=@�9@�@�
=@��@��@�Z@�1@�w@�;d@旍@�n�@�V@�V@�E�@��@�x�@ͺ^@�"�@� �@�z�@�33@�5?@�=q@���@�\)@�K�@�dZ@��\@�O�@�`B@��@��;@���@���@�^5@�V@���@�S�@���@��#@�ff@��@���@���@��w@��m@��F@��!@�@�/@�S�@���@���@��u@�33@�V@�7L@���@��@�$�@�p�@��j@��@K�@}?}@z��@y�^@x�`@v{@so@qhs@o�@m�-@kS�@i��@g\)@e�T@d1@bn�@ax�@`1'@^$�@]�h@\1@Y�^@X�u@W�@UO�@T(�@R^5@P�`@P1'@O��@Nȴ@L��@Kt�@I�7@G�@F�+@E�@D(�@Ahs@>�@>v�@=/@:�!@8Ĝ@8bN@7
=@6V@5?}@4z�@2�@1x�@/�@.�@.��@.v�@-��@-V@+�m@+33@*~�@)&�@(A�@'�@&ȴ@$�@#��@"�!@!�#@!&�@   @�@@`B@�/@�@n�@X@��@bN@�w@��@@Z@S�@�\@�@X@Ĝ@A�@  @�@�R@5?@�h@?}@��@�F@@
-@	�7@bN@K�@��@ff@�T@�-@O�@�/@�m@C�@�@^5@��@ Ĝ@ ��@ r�?�\)?�v�?��-?�I�?�~�?���?��j?�33?�Ĝ?�  ?�v�?�O�?�?���?�+?�j?�S�?��?��;?��?�/?��m?�"�?ٺ^?���?�b?��y?ա�?Լj?���?���?�-?��`?� �?ϝ�?��?�V?Ͳ-?��?�j?˅?��H?ʟ�?ɺ^?��?�1'?��?�
=?�?}?��?���?�hs?��`?��w?���?�5??�p�?���?�I�?�1?��m?�dZ?�dZ?�"�?�dZ?�?�dZ?�dZ?��?���?��m?�j?��?�V?��h?��?�v�?���?���?� �?�bN?�bN?��?���?��`?��`?�&�?�&�?�G�?�&�?�G�?�hs?��7?�hs?�G�?�G�?�%?�Ĝ?�A�?�bN?��?��?�Ĝ?��`?�%?�&�A�ĜA�ƨA�ȴA���A���A���A���A��
A���A���A���A���A�A���A���A�ĜA���A���A���A�A�ƨA�ƨA��
A��/A��`A��TA��TA��TA��TA��`A��`A��TA��TA��`A��TA��TA��HA��HA��TA��TA��TA��TA��HA��TA��TA��TA��HA��;A��/A��;A��HA��HA��TA��mA��yA��mA��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBBBBBBBBB%B�B.B� B�)BuB'�B.B8RBC�BP�BO�BS�B\)B]/BXBffBiyBl�Bp�Bt�B�B�JB�oB��B��B��B��B�\B�Bx�Bx�B�B�B�=B�oB��B��B��B��B��B��B��B��B��B�uB�1B�Bt�Bm�BffBM�BF�B@�B<jB7LB7LB2-B&�B�B�BoBPB\BB%BBDBbB\BPBJB
=B��B��BȴB�9B��B��B��B��B��Be`BM�B$�B+B
��B
�B
��B
�'B
�B
��B
�\B
�\B
�PB
�JB
�PB
�=B
w�B
p�B
bNB
_;B
XB
O�B
6FB
1'B
)�B
�B
DB
  B	�yB	�#B	��B	�%B	}�B	x�B	jB	dZB	_;B	YB	VB	Q�B	M�B	C�B	6FB	/B	$�B	 �B	�B	�B	�B	PB	1B	%B	B��B��B�B�TB�/B�#B�#B�B�B��B��BɺBɺB��B�LB�3B�B��B��B��B�bB�bB�\B�+B�B� B~�B{�Bx�Bt�Br�Bs�Bq�Bo�Bm�Bm�Bl�BjBjBk�BhsBgmBe`BcTB_;B_;BYBW
BXBS�BR�BN�BL�BM�BK�BJ�BJ�BJ�BH�BG�BF�BF�BE�BD�BF�BF�BE�BD�BB�BD�B?}B>wB>wB<jB;dB<jB;dB:^B7LB8RB8RB6FB8RB33B5?B33B1'B.B1'B/B-B0!B0!B-B.B-B-B/B-B-B-B,B.B0!B1'B2-B5?B0!B;dB:^BM�BR�BT�BdZBe`Bk�Bw�B��B��B��B�B�!B�wBȴB�B�TB�B	  B	�B	#�B	33B	5?B	W
B	m�B	�B	�=B	��B	��B	�!B	�9B	�dB	��B	ĜB	��B	��B	�B	�B	�)B	�;B	�`B	�fB	�B	�B	�B	�B	��B	��B	��B	��B
  B
  B
B
	7B
	7B
JB
\B
uB
uB
�B
�B
�B
�B
�B
�B
!�B
 �B
"�B
$�B
%�B
&�B
)�B
)�B
,B
.B
.B
.B
/B
2-B
33B
5?B
6FB
8RB
:^B
;dB
>wB
?}B
@�B
A�B
D�B
F�B
F�B
G�B
G�B
I�B
J�B
K�B
L�B
N�B
N�B
N�B
O�B
P�B
Q�B
Q�B
R�B
S�B
T�B
VB
VB
W
B
YB
ZB
\)B
\)B
\)B
^5B
_;B
_;B
`BB
`BB
aHB
cTB
dZB
dZB
dZB
e`B
ffB
gmB
iyB
iyB
iyB
jB
k�B
l�B
m�B
l�B
n�B
n�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
s�B
u�B
u�B
v�B
w�B
x�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
}�B
|�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
�%B
�7B
�7B
�=B
�DB
�JB
�VB
�bB
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
��B
��B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�9B
�9B
�9B
�?B
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�^B
�dB
�^B
�dB
�^B
�dB
�dB
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
�qB
�jB
�jB
�qB
�jB
�jB
�qB
�jBBBBBBB  B
��BBBBBBBBBBBBBBBBB  BBBBBBBBBBBBBBBBB  BBBBBBBBBBBBB  BBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B  BB  B  B  B  BB  B  B  B  BB  B  B  B  B  B  B  B  B  B  B  B  BB�B-B~�B�#BoB&�B-B7LBB�BO�BN�BR�B[#B\)BW
Be`BhsBk�Bo�Bs�B�B�DB�hB��B��B��B��B�VB�Bw�Bw�B� B�B�7B�hB��B��B��B��B��B��B��B��B��B�oB�+B�Bs�Bl�Be`BL�BE�B?}B;dB6FB6FB1'B%�B�B�BhBJBVBBBB
=B\BVBJBDB	7B��B��BǮB�3B��B��B��B��B�{BdZBL�B#�B%B
��B
�B
��B
�!B
�B
��B
�VB
�VB
�JB
�DB
�JB
�7B
v�B
o�B
aHB
^5B
W
B
N�B
5?B
0!B
(�B
�B

=B	��B	�sB	�B	��B	�B	|�B	w�B	iyB	cTB	^5B	XB	T�B	P�B	L�B	B�B	5?B	.B	#�B	�B	�B	�B	�B	JB	+B	B	B��B��B�B�NB�)B�B�B�
B��B��B��BȴBȴB�}B�FB�-B�B��B��B��B�\B�\B�VB�%B�B~�B}�Bz�Bw�Bs�Bq�Br�Bp�Bn�Bl�Bl�Bk�BiyBiyBjBgmBffBdZBbNB^5B^5BXBVBW
BR�BQ�BM�BK�BL�BJ�BI�BI�BI�BG�BF�BE�BE�BD�BC�BE�BE�BD�BC�BA�BC�B>wB=qB=qB;dB:^B;dB:^B9XB6FB7LB7LB5?B7LB2-B49B2-B0!B-B0!B.B,B/B/B,B-B,B,B.B,B,B,B+B-B/B0!B1'B49B/B:^B9XBL�BQ�BS�BcTBdZBjBv�B�{B��B��B�B�B�qBǮB�B�NB�B��B	�B	"�B	33B	5?B	W
B	m�B	�B	�=B	��B	��B	�!B	�9B	�dB	��B	ĜB	��B	��B	�B	�B	�)B	�;B	�`B	�fB	�B	�B	�B	�B	��B	��B	��B	��B
  B
  B
B
	7B
	7B
JB
\B
uB
uB
�B
�B
�B
�B
�B
�B
!�B
 �B
"�B
$�B
%�B
&�B
)�B
)�B
,B
.B
.B
.B
/B
2-B
33B
5?B
6FB
8RB
:^B
;dB
>wB
?}B
@�B
A�B
D�B
F�B
F�B
G�B
G�B
I�B
J�B
K�B
L�B
N�B
N�B
N�B
O�B
P�B
Q�B
Q�B
R�B
S�B
T�B
VB
VB
XB
ZB
[#B
]/B
]/B
]/B
_;B
`BB
`BB
aHB
aHB
bNB
dZB
e`B
e`B
e`B
ffB
gmB
hsB
jB
jB
jB
k�B
l�B
m�B
n�B
m�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
t�B
v�B
v�B
w�B
x�B
y�B
x�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
~�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�+B
�=B
�=B
�DB
�JB
�PB
�\B
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
��B
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�9B
�?B
�LB
�LB
�LB
�RB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�wB
�wB
�wB
�}B
�}B
�wB
�}B
�wB
�}B
�wB
�}B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BBBBB  BB
��B
��BB  BBB  BBB  B  BB  BB  BBB  B
��B  B  B  B  B  B  B  B  B  B  B  BBB  B  B  B
��B  B  B  B  B  B  BBB  BBB  B
��B  B  B  B  B  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202312021061413520620210614135206202106141746172021061417461720210614174617201807242202312021061413520620210614135206202106141746172021061417461720210614174617PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023120180724220231  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023120180724220231QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023120180724220231QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014820210722160148IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                