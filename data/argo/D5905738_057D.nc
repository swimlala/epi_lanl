CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-21T06:00:28Z creation      
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
resolution        =���   axis      Z        @  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  P0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                       HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � (      � (Argo profile    3.1 1.2 19500101000000  20190221060028  20210722160158  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               9   9DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؤJ>Kr@ؤJ>Kr11  @ؤJ8� @ؤJ8� @5����@5�����c�'�0�c�'�011  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @�  @�  @�  @�33A��A��A$��A>ffAa��A�ffA���A�  A�  A���A�  A���A���B ffBffB��B��B ffB(ffB0ffB8ffB@ffBH  BP  BX  B`  Bh  Bp��Bx��B�33B�33B�  B�33B�ffB�ffB�33B���B�  B�ffB�  B���B�33B�  B�  B�33B�33B�33B�33B̙�B�33B�33B�ffB�33B���B�33B�33B�ffB�ffB�ffB���B���C   C  C�C�C33C
33CL�C�fC  C  C�fC  C�fC�fC  C  C �C"L�C#�fC&�C(33C)�fC+�fC.  C0  C2�C4�C6L�C8  C:  C<L�C=�fC@  CB  CC��CE�fCH�CI��CK�fCN�CO��CQ�fCT  CV�CX�CZ�C\33C]�fC_�fCa�fCc�fCfL�ChL�Cj33Cl33Cn33Cp�Cr�Ct33Cv33Cx33Cz�C|�C~�C��C�  C��3C��3C��fC��3C��3C��3C��3C��3C��C�&fC��C��C��C��C�  C��C��C��C��C�&fC�&fC�&fC��C�&fC��C��C��C��C��C��C��fC��3C�  C��3C�  C�  C�  C��C��C�  C��C��3C��3C�  C�&fC�&fC��C��C��C��C�  C��3C��fC��C�  C�  C�&fC�&fC��C��C�  C�&fC�&fC��C��C�  C��3C��fC��C�  C��fC��C�  C��3C�&fC��C�  C��C��C�  C�  C��3C��C��C��C�  C��3C�&fC�&fC��C�33C�&fC��C�&fC��C�  C��C��C��3C�&fC��C��3C��C��C�  C��C�&fC��C��C��C��C�  C��C��3C�  C�  C�  C��fC��C��C��fC�ٚC��C�33C�  C���D   D �3D  DffD�D
�D  D3D  D� D�fD` D3D ��D#� D&FfD)fD+��D.` D1�D3� D6y�D9,�D;��D>��DAffDD,�DF��DI��DL�3DOffDR@ DU&fDX�DZ��D]��D`��Dc��Dg  Dj  Dl� Do��Dr�3Dus3DxL�D{�D}�fD�)�D��3D���D�ffD��3D�33D��3D��fD�FfD�� D���D�VfD�� D�3D�P D�� D��3D�I�D��fD�ٚD��D�Y�D���D��D�9�D�� D��fD�	�D�P D���D�� D��D�Y�D���D�� D�<�D��3D���D���D�<�D��3D��fD�  D�i�D���D�� D�9�D�vfD���D�  D�I�D3D�ٚD�#3D�s3D�� D�fD�VfDˣ3D��D�<�Dϐ D��fD�  D�l�DԳ3D�fD�` Dع�D�	�D�\�Dܩ�D��fD�9�D�y�D��3D� D�VfD�3D��fD�3D�L�DꉚD�ɚD�fD�6fD�i�D��D��fD�fD�<�D�l�D��fD��fD��fD�3D�&fD�P D�� D��3D�ɚE s3E�E�3E!�E�fE<�E��E\�E�E��E E�3E8 E�3ES3E�fE	y�E
�E
� E0 E�3ET�E�E� EfEP E��E��E��E  E�E#3ES3E� E�3E�fE!�E^fE��E ��E!�3E#�fE$� E%��E'( E(L�E)y�E*��E,a�E-� E.��E/��E0� E2�fE3��E4�3E5��E7fE8��E9��E:��E<�E?\�EB�3EE� EI	�EK� EO  ERL�EU\�EXFfE[��E^� Ea��EefEh0 Ek,�En3Eqt�EtH Ewi�Ez�fE}��E�vfE� E��fE�&fE��3E�8�E�՚E�u�E�fE�zfE�fE�� E�8�E��fE�NfE��fE�t�E�3E��fE�0�E���E�D E��fE��E�m�E��fE�3E�^f>���>���>L��>L��>���>���>���>���>���>���>���>L��>���>���>���>���>���?   ?��?333?333?333?L��?fff?�  ?���?���?�33?���?�ff@��@��@,��@@  @S33@l��@�  @���@�ff@�  @���@�33@�33@���@���@陚@���A��AffA��AffA)��A1��A9��AC33AK33AT��A\��AfffAnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444114441441144411144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?L��?�33@   @`  @�  @�  @�  @�33A	��A��A,��AFffAi��A�ffA���A�  A�  A���A�  A���A���BffB
ffB��B��B"ffB*ffB2ffB:ffBBffBJ  BR  BZ  Bb  Bj  Br��Bz��B�33B�33B�  B�33B�ffB�ffB�33B���B�  B�ffB�  B���B�33B�  B�  B�33B�33B�33B�33B͙�B�33B�33B�ffB�33BᙚB�33B�33B�ffB�ffB�ffB���B���C � C� C��C��C�3C
�3C��CffC� C� CffC� CffCffC� C� C ��C"��C$ffC&��C(�3C*ffC,ffC.� C0� C2��C4��C6��C8� C:� C<��C>ffC@� CB� CDL�CFffCH��CJL�CLffCN��CPL�CRffCT� CV��CX��CZ��C\�3C^ffC`ffCbffCdffCf��Ch��Cj�3Cl�3Cn�3Cp��Cr��Ct�3Cv�3Cx�3Cz��C|��C~��C�L�C�@ C�33C�33C�&fC�33C�33C�33C�33C�33C�Y�C�ffC�Y�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�Y�C�ffC�ffC�ffC�Y�C�ffC�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�&fC�33C�@ C�33C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�33C�33C�@ C�ffC�ffC�Y�C�Y�C�L�C�L�C�@ C�33C�&fC�L�C�@ C�@ C�ffC�ffC�Y�C�L�C�@ C�ffC�ffC�L�C�L�C�@ C�33C�&fC�L�C�@ C�&fC�L�C�@ C�33C�ffC�L�C�@ C�Y�C�L�C�@ C�@ C�33C�Y�C�L�C�L�C�@ C�33C�ffC�ffC�L�C�s3C�ffC�Y�C�ffC�Y�C�@ C�Y�C�L�C�33C�ffC�Y�C�33C�Y�C�L�C�@ C�Y�C�ffC�Y�C�Y�C�L�C�L�C�@ C�L�C�33C�@ C�@ C�@ C�&fC�Y�C�Y�C�&fC��C�L�C�s3C�@ C��D   D �3D  D�fD9�D
,�D@ D33D  D  D�fD� D33D ��D#� D&ffD)&fD+ٚD.� D19�D3� D6��D9L�D<�D>��DA�fDDL�DG�DI��DL�3DO�fDR` DUFfDX,�D[�D^�Da�Dd�Dg  Dj  Dm  DoٚDr�3Du�3Dxl�D{9�D}�fD�9�D��3D��D�vfD��3D�C3D��3D��fD�VfD�� D�	�D�ffD�� D�3D�` D�� D�3D�Y�D��fD��D�)�D�i�D���D���D�I�D�� D��fD��D�` D���D�� D�,�D�i�D���D�  D�L�D��3D���D��D�L�D��3D��fD�0 D�y�D���D�  D�I�D��fD���D� D�Y�D£3D��D�33Dƃ3D�� D�fD�ffD˳3D���D�L�DϠ D��fD�0 D�|�D��3D�fD�p D�ɚD��D�l�Dܹ�D�fD�I�D���D��3D�  D�ffD�3D��fD�#3D�\�DꙚD�ٚD�fD�FfD�y�D��D��fD�fD�L�D�|�D��fD��fD��fD�#3D�6fD�` D�� D��3D�ٚE {3E�E�3E)�E�fED�E��Ed�E��E��E  E�3E@ E�3E[3E�fE	��E
�E
� E8 E�3E\�E�E� EfEX E��E��E��E( E�E+3E[3E� E�3E�fE)�EffE��E ��E!�3E#�fE$� E&�E'0 E(T�E)��E*��E,i�E-� E.��E/��E0� E2�fE3��E4�3E5��E7fE8ɚE9��E;�E<�E?d�EB�3EE� EI�EK� EO( ERT�EUd�EXNfE[��E^� Ea��EefEh8 Ek4�En#3Eq|�EtP Ewq�Ez�fE}��E�zfE� E��fE�*fE��3E�<�E�ٚE�y�E�fE�~fE�fE�� E�<�E��fE�RfE��fE�x�E�3E��fE�4�E���E�H E��fE��E�q�E��fE�3E�bfG�O�G�O�G�O�?333?L��G�O�G�O�G�O�?L��G�O�G�O�?333?L��G�O�G�O�G�O�?fff?�  ?���G�O�G�O�?���?�ff?�33?�  ?���?ٙ�?�33@ff@33@,��@9��@L��@`  @s33@�ff@�  @���@�ff@�  @���@�33@�33@���@���@���A��A��AffA��A&ffA1��A9��AA��AK33AS33A\��Ad��AnffAvffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444114441441144411144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ @ %@ �@ {@ O@ ""@ (�@ 0x@ 7L@ >@ FQ@ Q=@ `B@ oF@ |?@ ��@ �0@ ��@ �~@ ��@ �|@ �#@ ��@ � @�@�@g@-@:�@H]@UU@b�@p�@~K@��@��@��@��@�>@�7@ލ@��@��@�@�@""@1'@=q@Ji@Yn@ff@t@�d@�@��@�Y@�^@ƨ@�O@�@�@��@
�@�@&�@4�@B8@M�@[z@i�@ww@��@�u@��@�r@��@ȴ@�
@�@�@  @�@�@(�@6�@D�@S�@^�@m�@|?@��@��@��@�~@��@�|@܀@�@��@v@b@�@,`@8�@F�@V@a�@o�@~�@��@��@�A@��@�>@��@�;@��@�~@%@�@$.@1�@>�@Lu@Z@g@t�@�@��@�a@�Y@�@ƨ@�O@�H@�@@��@�@6@$�@2�@@,@M�@]�@k�@x�@��@�u@�@�@�k@�@׹@�@�e@@�@�@+@7�@E�@SI@`B@n�@|?@�+@��@��@��@�&@��@�t@��@�q@	j@	�@	
@	+�@	:@	I�@	Wb@	dZ@	r@	~�@	��@	��@	��@	��@	�>@	�7@	��@	�@	�9@
1@
*@
""@
1�@
?}@
K�@
Yn@
ff@
s_@
�W@
�@
�@
�M@
�@
��@
��@
�T@
�@
��@�@�@%�@33@@,@O�@\�@j@ww@�p@��@�y@��@�w@�o@�h@�@�@  @@�@(G@8�@E�@Q=@`�@m�@z�@��@�<@�5@��@��@�|@�t@��@�@j@@�@+@;d@I@S�@`�@qS@�@��@��@�A@��@@�|@i!@��@�@^5@�@��@H]@��@܀@&�@p�@�j@�@Q=@��@�@,`@v�@��@�@V�@�@��@7�@�@�C@g@m:@�j@�@[z@�f@�Q@Q=@��@��@DD@�@��@.l@|?@ȴ@
�@Wb@��@�@>�@��@׹@"�@k.@�F@  @I�@�#@��@&;@m:@��@�E@ FQ@ �P@ �C@!�@!Z�@!��@!�@".l@"t@"��@"��@#DD@#�D@#�7@$�@$Wb@$�U@$�@%+�@%qS@%�9@%�~@&<�@&�d@&��@'@'Wb@'�U@'�H@('�@(k.@(��@(��@)<@)�d@)�@*V@*V@*�@*�@+*S@+qS@+��@+�Q@,G�@,�P@,Ӡ@-�@-`A@-��@-�Y@.<@.��@.�@/@/Z@/�@/�T@0)�@0p�@0�F@0��@1>�@1�d@1�J@2�@2M$@2��@2є@3@3T�@3��@3�h@4�@4[z@4��@4�h@5B@5Yn@5�#@5Ӡ@6{@6R�@6�i@6�*@7J@7I@7��@7��@7��@8<@8y�@8�@8��@96�@9uk@9�~@9��@:*S@:i!@:��@:�@;%�@;b�@;��@;��@<g@<_�@<��@=%�@=��@>-@>�!@?33@?�q@@|?@@��@A��@B�@B��@C
�@C�@D*@D��@E�@E��@FQ�@F�C@GQ�@G��@HO1@H��@I��@J
=@J�+@K@K~K@L5�@L�!@M&�@M�z@N	@N�O@OM$@O�@P<@Q��@S�@TUU@Uƨ@V�,@X_�@Y�^@[�@\F�@]�^@^�,@`^5@a�w@c@dWb@e��@gv@h:@i�@j�l@l> @m�@o  @p8�@q�@s%@t> @u�a@w^@x^5@y��@z�@|G�@}�@~��@�!s@���@�y�@�(�@�Ԧ@�~�@�)M@�΂@�s_@���@���@��/@���@�!sG�O�G�O�G�O�@ ^@ G�O�G�O�G�O�@ G�O�G�O�@ ^@ G�O�G�O�G�O�@ �@ j@ G�O�G�O�@ �@ v@ %@ �@ �@ 1@ 	�@ 
�@ J@ @ b@ o@ {@ �@ B@ O@ [@  @ ""@ $�@ &;@ )�@ +�@ /@ 1�@ 5?@ 8�@ <�@ ?}@ C�@ H]@ K�@ O0@ SI@ V�@ Z�@ ^5@ bN@ e�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�/A�33A�-A�+A�1'A�+A�/A�;dA�;dA�9XA�7LA�5?A�7LA�9XA�=qA�?}A�E�A�E�A�E�A�G�A�I�A�I�A�K�A�M�A�K�A�O�A�O�A�O�A�A��;A��mA��HA��;A��A���A���A���A���A�ĜA���A���A���A���A���A���A���A���A�A��RA��A���A���A���A��hA��A�dZA�I�A�~�A��
A���A�+A���A��FA�{A�M�A��A���A�ffA�9XA�(�A��A���A���A��jA���A�  A�1A�XA��A�^5A�%A��HA���A���A�M�A���A�1A�bA�~�A�dZA�E�A�ĜA�hsA�bA���A��A���A��A��#A���A���A��^A�t�A���A��wA�VA�%A���A�x�A�(�A�  A�XA�%A�r�A��A�$�A��`A��A��FA���A���A~�A}G�A|��AzE�Ax��AuhsAp�/Am�#Am�-Amx�Ai��Ad��Aa�7A^ĜA]��A\��A[��AZ��AY�TAYK�AX��AT~�AP�AJ�AF�!AE�7AD �AC;dA@E�A?33A=�^A<�yA;t�A:�HA:ȴA:�uA8�DA6{A5�PA5G�A5A4�A3\)A1A/dZA-XA(��A'?}A&�jA& �A$=qA"ĜA!�7A �uA��A��AQ�A��A\)A
=A�\A��A�-A
=AffA�A��A��AffA|�A��AA�A��A"�A�A�PAp�A33A
9XA	�#A	K�A	�A�A��A�DA��A-A��At�AC�A�A1'AAx�A�AA�A �+A A�@�33@�=q@�V@��
@��@��+@�M�@�`B@��@��9@�33@�V@�$�@��@���@�j@�1@��@�ff@���@�X@��@�"�@�^@�bN@�  @�  @�w@�C�@�ȴ@�J@�-@�`B@��@�I�@�ȴ@���@߶F@��@�I�@�\)@�J@��@�t�@���@�@��@�n�@�j@��!@�V@�l�@�@��9@�S�@�$�@���@�dZ@���@�l�@��@��@��/@��w@�~�@��#@�G�@��@�@���@��`@�1@�~�@�bN@�o@���@��u@���@���@��`@�bN@��@�V@���@��/@��/@�C�@���@�z�@�@~@|�D@{t�@y�#@w|�@uO�@t�@r��@qG�@o��@m�T@l��@j^5@h�`@g+@eO�@c��@a�@` �@]��@[�
@Z��@Y��@X �@V�+@T�j@R��@RJ@P�9@O�@N��@M�T@L1@J~�@H�9@G�@F��@EO�@CC�@A�7@@��@@1'@?
=@=�T@<�@;�F@:n�@8bN@7\)@6��@4��@2�H@1��@1hs@/�@.��@-�T@-?}@+�@*=q@)�#@)7L@(Q�@'l�@&�+@%/@$�@#��@#��@"��@!��@ �u@�w@;d@��@�@�D@�@��@��@7L@b@�w@��@p�@O�@�j@�@��@�\@�^@&�@�@\)@5?@O�@�j@(�@�
@
��@	�7@	G�@	�7@�@|�@�y@��@�-@`B@�j@I�@�F@dZ@�!@��@�7@ Q�@ b?��R?��?�(�?��?���?�$�?�$�?�?�&�?��?�I�?ꟾ?�7L?�?�`B?䛦?�F?��?�-?�A�?��;?��?�V?�1?���?�x�?�1'?և+?��T?Լj?���?ӕ�?�J?щ7?�%?У�?�  ?Η�?���?�{?θR?�/?�?�7L?�Q�?�
=?š�?��/?�Z?�S�?�M�?�G�?��;?�\)?��R?�5??���?�/?���?��D?�I�?�I�?�I�?�(�?��m?�I�?�I�?�I�?�I�?��?�V?�V?�O�?��-?�{?���?��?��?���?��;?�A�?� �?��?�bN?�|�?�\)?�|�?�\)?�\)?���A�bA�VA�bA�VA�oA�oA�{A��A�oA��A��A��A��A�{A�JA� �A��A�+A�&�A�$�A�$�A�1'A�5?A�5?A�5?A�7LA�5?A�1'A�5?A�7LA�33A�-A�+A�-A�-A�-A�(�A�(�A�1'A�33A�1'A�-A�+A�&�A�&�A�5?A�=qA�9XA�9XA�;dA�7LA�9XA�7LA�7LA�7LA�7LA�5?A�33A�33A�7LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            A��A�/A�33A�-A�+A�1'A�+A�/A�;dA�;dA�9XA�7LA�5?A�7LA�9XA�=qA�?}A�E�A�E�A�E�A�G�A�I�A�I�A�K�A�M�A�K�A�O�A�O�A�O�A�A��;A��mA��HA��;A��A���A���A���A���A�ĜA���A���A���A���A���A���A���A���A�A��RA��A���A���A���A��hA��A�dZA�I�A�~�A��
A���A�+A���A��FA�{A�M�A��A���A�ffA�9XA�(�A��A���A���A��jA���A�  A�1A�XA��A�^5A�%A��HA���A���A�M�A���A�1A�bA�~�A�dZA�E�A�ĜA�hsA�bA���A��A���A��A��#A���A���A��^A�t�A���A��wA�VA�%A���A�x�A�(�A�  A�XA�%A�r�A��A�$�A��`A��A��FA���A���A~�A}G�A|��AzE�Ax��AuhsAp�/Am�#Am�-Amx�Ai��Ad��Aa�7A^ĜA]��A\��A[��AZ��AY�TAYK�AX��AT~�AP�AJ�AF�!AE�7AD �AC;dA@E�A?33A=�^A<�yA;t�A:�HA:ȴA:�uA8�DA6{A5�PA5G�A5A4�A3\)A1A/dZA-XA(��A'?}A&�jA& �A$=qA"ĜA!�7A �uA��A��AQ�A��A\)A
=A�\A��A�-A
=AffA�A��A��AffA|�A��AA�A��A"�A�A�PAp�A33A
9XA	�#A	K�A	�A�A��A�DA��A-A��At�AC�A�A1'AAx�A�AA�A �+A A�@�33@�=q@�V@��
@��@��+@�M�@�`B@��@��9@�33@�V@�$�@��@���@�j@�1@��@�ff@���@�X@��@�"�@�^@�bN@�  @�  @�w@�C�@�ȴ@�J@�-@�`B@��@�I�@�ȴ@���@߶F@��@�I�@�\)@�J@��@�t�@���@�@��@�n�@�j@��!@�V@�l�@�@��9@�S�@�$�@���@�dZ@���@�l�@��@��@��/@��w@�~�@��#@�G�@��@�@���@��`@�1@�~�@�bN@�o@���@��u@���@���@��`@�bN@��@�V@���@��/@��/@�C�@���@�z�@�@~@|�D@{t�@y�#@w|�@uO�@t�@r��@qG�@o��@m�T@l��@j^5@h�`@g+@eO�@c��@a�@` �@]��@[�
@Z��@Y��@X �@V�+@T�j@R��@RJ@P�9@O�@N��@M�T@L1@J~�@H�9@G�@F��@EO�@CC�@A�7@@��@@1'@?
=@=�T@<�@;�F@:n�@8bN@7\)@6��@4��@2�H@1��@1hs@/�@.��@-�T@-?}@+�@*=q@)�#@)7L@(Q�@'l�@&�+@%/@$�@#��@#��@"��@!��@ �u@�w@;d@��@�@�D@�@��@��@7L@b@�w@��@p�@O�@�j@�@��@�\@�^@&�@�@\)@5?@O�@�j@(�@�
@
��@	�7@	G�@	�7@�@|�@�y@��@�-@`B@�j@I�@�F@dZ@�!@��@�7@ Q�@ b?��R?��?�(�?��?���?�$�?�$�?�?�&�?��?�I�?ꟾ?�7L?�?�`B?䛦?�F?��?�-?�A�?��;?��?�V?�1?���?�x�?�1'?և+?��T?Լj?���?ӕ�?�J?щ7?�%?У�?�  ?Η�?���?�{?θR?�/?�?�7L?�Q�?�
=?š�?��/?�Z?�S�?�M�?�G�?��;?�\)?��R?�5??���?�/?���?��D?�I�?�I�?�I�?�(�?��m?�I�?�I�?�I�?�I�?��?�V?�V?�O�?��-?�{?���?��?��?���?��;?�A�?� �?��?�bN?�|�?�\)?�|�?�\)?�\)?���A�bA�VA�bA�VA�oA�oA�{A��A�oA��A��A��A��A�{A�JA� �A��A�+A�&�A�$�A�$�A�1'A�5?A�5?A�5?A�7LA�5?A�1'A�5?A�7LA�33A�-A�+A�-A�-A�-A�(�A�(�A�1'A�33A�1'A�-A�+A�&�A�&�A�5?A�=qA�9XA�9XA�;dA�7LA�9XA�7LA�7LA�7LA�7LA�5?A�33A�33A�7LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BL�BK�BL�BL�BK�BL�BM�BK�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BL�BK�BK�BK�BK�BL�BL�Bl�B�B�JB�PB�\B�bB�oB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB�fB��B�B7LBG�BW
BgmBiyBgmBP�B>wBD�B?}BD�BD�B<jB=qB:^B)�B%�B�B�B{BuBuB�BhB%B��B�5B�B��B��B��B�!B�B��B��B��B�B^5BaHB[#BB�B7LB(�BVB
��B
�fB
�
B
�
B
�B
�)B
ǮB
�RB
�%B
o�B
l�B
dZB
^5B
O�B
33B
�B

=B
  B	��B	�HB	��B	�3B	��B	�PB	�7B	�+B	hsB	S�B	<jB	6FB	+B	#�B	�B	�B	bB	JB	%B�B�B�}B�3B�B��B��B��B�VB�=B�B~�B~�B}�B|�Bo�Bs�Bq�Bn�Bl�BhsBaHBl�Bp�BgmBgmBiyBffBdZB`BBe`BcTBdZBbNBbNBaHB^5BbNB`BB\)B\)B]/BXBQ�BR�BL�BM�BL�BO�BQ�BYBXBXBYB\)B]/B]/B^5B`BBbNBn�Br�Bu�Bx�Bw�B{�B�B�B�B�B�B�B�%B�+B�+B�+B�%B�B�B�B�B�B�B�B�B�B}�B�B�B�B�B�B�B�B�+B�DB�\B�\B�\B�VB�oB�oB��B�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB��B��B�B�^BɺB��B�B�B�B��B	PB	49B	<jB	C�B	P�B	YB	ffB	`BB	iyB	u�B	�1B	�uB	��B	��B	��B	�B	�B	�'B	�3B	��B	ǮB	��B	��B	�/B	�TB	�fB	�B	�B	��B	��B	��B	��B	��B
B
+B

=B
PB
hB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
'�B
(�B
,B
,B
/B
/B
2-B
33B
33B
6FB
6FB
8RB
:^B
:^B
;dB
<jB
<jB
?}B
@�B
B�B
C�B
C�B
E�B
E�B
H�B
I�B
I�B
J�B
K�B
L�B
M�B
M�B
N�B
Q�B
Q�B
R�B
VB
XB
YB
XB
YB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
_;B
`BB
aHB
aHB
cTB
cTB
cTB
dZB
ffB
gmB
gmB
ffB
hsB
iyB
iyB
jB
l�B
l�B
m�B
m�B
n�B
p�B
o�B
p�B
q�B
r�B
q�B
s�B
s�B
u�B
u�B
v�B
v�B
x�B
x�B
x�B
z�B
y�B
y�B
z�B
{�B
|�B
{�B
|�B
{�B
{�B
|�B
|�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�+B
�7B
�+B
�=B
�DB
�JB
�VB
�\B
�\B
�bB
�hB
�oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�-B
�-B
�?B
�?B
�FB
�FB
�LB
�RB
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�wB
�qB
�wB
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
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖBL�BL�BM�BM�BM�BM�BM�BM�BM�BK�BM�BL�BK�BK�BN�BI�BM�BL�BK�BK�BM�BO�BL�BM�BK�BL�BL�BL�BL�BK�BJ�BL�BL�BL�BK�BK�BL�BM�BK�BJ�BK�BK�BL�BL�BL�BM�BK�BK�BK�BK�BL�BK�BL�BL�BK�BL�BK�BL�BL�BK�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            BI�BI�BH�BI�BI�BH�BI�BJ�BH�BH�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BH�BH�BI�BH�BH�BH�BH�BI�BI�BiyB�B�7B�=B�JB�PB�\B�hB�hB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�?B�TB��B�B49BD�BS�BdZBffBdZBM�B;dBA�B<jBA�BA�B9XB:^B7LB&�B"�B�B�BhBbBbBoBVBB��B�#B��B��BɺB�wB�B�B��B��B�uB� B[#B^5BXB?}B49B%�BDB
��B
�TB
��B
��B
�
B
�B
ĜB
�?B
�B
l�B
iyB
aHB
[#B
L�B
0!B
�B
+B	��B	�B	�5B	��B	�!B	�{B	�=B	�%B	�B	e`B	P�B	9XB	33B	'�B	 �B	�B	oB	PB		7B	B�yB��B�jB�!B�B��B��B�oB�DB�+B� B{�B{�Bz�By�Bl�Bp�Bn�Bk�BiyBe`B^5BiyBm�BdZBdZBffBcTBaHB]/BbNB`BBaHB_;B_;B^5B[#B_;B]/BYBYBZBT�BN�BO�BI�BJ�BI�BL�BN�BVBT�BT�BVBYBZBZB[#B]/B_;Bk�Bo�Br�Bu�Bt�Bx�B~�B�B� B~�B�B�B�B�B�B�B�B�B� B� B� B�B�B�B�B� Bz�B�B�B� B� B� B� B�B�B�1B�JB�JB�JB�DB�\B�\B�oB�\B�bB�bB�hB�oB�{B��B��B�{B��B��B��B��B��B��B�{B�uB�bB�PB��B��B�B�RBǮB��B�
B�B�B��B	DB	2-B	:^B	A�B	N�B	W
B	dZB	^5B	gmB	s�B	�%B	�hB	��B	��B	��B	�B	�B	�B	�'B	�}B	ŢB	ɺB	��B	�#B	�HB	�ZB	�yB	�B	�B	��B	��B	��B	��B
B
B
1B
DB
\B
VB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
%�B
&�B
)�B
)�B
-B
-B
0!B
1'B
1'B
49B
49B
6FB
8RB
8RB
9XB
:^B
:^B
=qB
>wB
@�B
A�B
A�B
C�B
C�B
F�B
G�B
H�B
I�B
J�B
K�B
L�B
L�B
M�B
P�B
P�B
Q�B
T�B
W
B
XB
W
B
XB
YB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
^5B
_;B
`BB
`BB
bNB
bNB
bNB
cTB
e`B
ffB
ffB
e`B
gmB
hsB
hsB
iyB
k�B
k�B
l�B
l�B
m�B
o�B
n�B
o�B
p�B
q�B
p�B
r�B
r�B
t�B
t�B
u�B
u�B
w�B
w�B
w�B
y�B
x�B
x�B
y�B
z�B
{�B
z�B
{�B
z�B
z�B
{�B
{�B
|�B
}�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�%B
�1B
�%B
�7B
�DB
�JB
�VB
�\B
�\B
�bB
�hB
�oB
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
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�3B
�3B
�FB
�FB
�LB
�LB
�RB
�XB
�^B
�dB
�dB
�jB
�qB
�wB
�wB
�wB
�}B
�}B
��B
�}B
��B
��B
��B
��B
��B
��B
��B
ÖB
B
ÖB
ĜB
ĜB
ĜB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨBI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BH�BJ�BI�BH�BH�BK�BF�BJ�BI�BH�BH�BJ�BL�BI�BJ�BH�BI�BI�BI�BI�BH�BG�BI�BI�BI�BH�BH�BI�BJ�BH�BG�BH�BH�BI�BI�BI�BJ�BH�BH�BH�BH�BI�BH�BI�BI�BH�BI�BH�BI�BI�BH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201902210600282021061413531620210614135316202106141747082021061417470820210614174708201902210600282021061413531620210614135316202106141747082021061417470820210614174708PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019022106002820190221060028  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019022106002820190221060028QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019022106002820190221060028QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015820210722160158IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                