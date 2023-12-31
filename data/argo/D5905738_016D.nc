CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:36Z creation      
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
resolution        =���   axis      Z        p  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   LP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  Pl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �     HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                       	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �        �  Argo profile    3.1 1.2 19500101000000  20180724220236  20210722160150  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�n-����@�n-����11  @�n-��`�@�n-��`�@6�ѷX�@6�ѷX��cװ�{���cװ�{��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  >���?fff@   @Fff@�ff@�  @�  @�33A   A  A!��A>ffA\��A�  A���A���A���A���A���AᙚA���B ffB  B��BffB ��B(ffB/��B8  B@  BHffBPffBX��B`ffBh  Bo��Bx  B�33B�  B�  B�33B�  B�  B�33B�  B���B�  B�ffB�33B�  B���B�  B�  B�33B�ffBǙ�B�  BЙ�B�ffB�33Bܙ�B�ffB�33B�  B뙚B�33B�  B���B�ffC �C  C  C��C�C
�C�fC�C  C��C�fC�C  C  C33C�C   C!�fC#��C&�C(L�C*33C,  C.�C0L�C233C4�C633C8�C9�fC<33C>�C@  CB33CD�CF  CHL�CJ�CK��CN�CO�fCQ��CT  CVL�CX33CZ�C[�fC^  C`33Cb�Cd  CfL�Ch33Cj�ClL�Cn�Co��Cr�CtL�Cv�Cw�fCz�C|L�C~�C�fC�  C��C�&fC��C��3C��C�  C��fC�  C��C��C��3C��C�&fC��C��C�  C�  C��3C��3C��C�&fC��C��C�  C��C��C�  C��3C��3C��C��C��3C�  C��fC��C��C�  C��C�  C��3C��C�&fC��C�  C��C��C�  C��C�  C��3C��C�  C��3C��C�  C��3C��C�  C��fC��C�&fC�  C��fC�  C��C��C��3C�  C�&fC��C��3C�  C��C�  C��fC��3C��C�&fC��C��3C��C��C��3C��C�&fC��C��fC�  C�&fC��C��C�  C��fC�  C��C�&fC�&fC��C��C��C�  C��C��C�  C��C��C��fC�  C��C�&fC��C��3C��C��C�&fC��C��fC��3C�  C��C��3C��C��C�&fC��C�&fD �D s3D ��Dy�D��D��D	y�D  D� D,�D�3D�fD,�DٚDl�D!3D#� D&9�D(� D+FfD-� D0� D3  D5��D8S3D:ٚD=l�D@  DB��DE&fDG��DJFfDL� DO,�DQ�3DT9�DV�3DY�D[�fD]�fD`L�Db�fDe33Dg� Dj&fDl��Do33Dq�3Dt@ DvٚDyffD{� D~9�D�ffD���D��3D�C3D��3D�� D�)�D�� D���D�fD�` D�� D���D�C3D���D�ٚD��D�Y�D��fD��fD�3D�P D���D�� D���D�9�D�i�D���D��fD�fD�<�D�l�D���D�ɚD�  D�33D�` D���D���D���D�  D�L�D�� D�� D��fD�3D�FfD�� D��3D��fD�3D�C3D�vfD��3D�ٚD��D�@ D�p DǦfD�ٚD�  D�<�D�p D͜�D���D���D�,�D�Y�DӉ�DԼ�D��fD�)�D�\�Dٓ3D��fD���D�,�D�\�Dߌ�D�3D�ٚD�	�D�33D�Y�D�3D穚D��3D���D�  D�I�D�|�DD�� D�  D�)�D�S3D�3D��3D��fD�  D�  D�<�D�C3D�c3D�� D���D���E h E �3E��E�E� E!�E��E.fE�fE;3E� E>fE� EC3EɚENfE	�3E
�fE�fEx E��E�E�E��E��E�3E` EffE� E��E�E�fE��E E  E!�3E"�3E#��E%9�E&<�E'��E(�3E*S3E+P E,њE-�fE/D�E0;3E1�3E3�E43E5i�E6�fE7� E90 E:�3E;�3E<�fE@,�EC3EF` EI.fELVfEOt�ER�fEU��EY�E\�E_1�EbT�Ee~fEh� Ek�3En�Er Eu,�ExFfE{FfE~��E�њE�d E��fE�nfE��E���E�5�E�� E�8�E�ɚE�O3E�� E���E�BfE�� E��3E�/3E���E�ٚE� E�� E���E�$�E�g3E�ɚE��>���>���>���>���>���>���>���>���>���>���?   >���>���>���>���>���>���?   ?   ?   ?��?333?fff?���?�ff?���?�ff@   @��@,��@9��@L��@`  @s33@�ff@�  @���@�ff@�33@�  @�  @���@陚@�ffA33A33A33A��A!��A+33A1��A9��AA��AI��AQ��AY��A`  Ah  Aq��Ax  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441444144414414411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?fff?�33@   @fff@�ff@�  @�  @�33A  A  A)��AFffAd��A�  A���A���A���A���A���A噚A���BffB
  B��BffB"��B*ffB1��B:  BB  BJffBRffBZ��BbffBj  Bq��Bz  B�33B�  B�  B�33B�  B�  B�33B�  B���B�  B�ffB�33B�  B���B�  B�  B�33B�ffBș�B�  Bљ�B�ffB�33Bݙ�B�ffB�33B�  B왚B�33B�  B���B�ffC ��C� C� CL�C��C
��CffC��C� CL�CffC��C� C� C�3C��C � C"ffC$L�C&��C(��C*�3C,� C.��C0��C2�3C4��C6�3C8��C:ffC<�3C>��C@� CB�3CD��CF� CH��CJ��CLL�CN��CPffCRL�CT� CV��CX�3CZ��C\ffC^� C`�3Cb��Cd� Cf��Ch�3Cj��Cl��Cn��CpL�Cr��Ct��Cv��CxffCz��C|��C~��C�33C�@ C�Y�C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�L�C�@ C�@ C�33C�33C�Y�C�ffC�L�C�Y�C�@ C�Y�C�Y�C�@ C�33C�33C�Y�C�L�C�33C�@ C�&fC�Y�C�L�C�@ C�Y�C�@ C�33C�L�C�ffC�L�C�@ C�Y�C�Y�C�@ C�Y�C�@ C�33C�L�C�@ C�33C�Y�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�@ C�&fC�@ C�Y�C�L�C�33C�@ C�ffC�L�C�33C�@ C�Y�C�@ C�&fC�33C�L�C�ffC�L�C�33C�Y�C�L�C�33C�L�C�ffC�L�C�&fC�@ C�ffC�Y�C�L�C�@ C�&fC�@ C�Y�C�ffC�ffC�L�C�Y�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�L�C�&fC�@ C�L�C�ffC�L�C�33C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�33C�Y�C�Y�C�ffC�Y�C�ffD 9�D �3D�D��D�D�D	��D  D� DL�D�3D�fDL�D��D��D!33D#� D&Y�D(� D+ffD.  D0� D3@ D5ٚD8s3D:��D=��D@  DB��DEFfDGٚDJffDL� DOL�DQ�3DTY�DV�3DY9�D[�fD^fD`l�Db�fDeS3Dg� DjFfDl��DoS3Dq�3Dt` Dv��Dy�fD{� D~Y�D�vfD���D�3D�S3D��3D�� D�9�D�� D���D�&fD�p D�� D��D�S3D���D��D�,�D�i�D��fD��fD�#3D�` D���D�� D��D�I�D�y�D���D��fD�fD�L�D�|�D���D�ٚD� D�C3D�p D���D���D���D�0 D�\�D�� D�� D��fD�#3D�VfD�� D��3D��fD�#3D�S3D��fD��3D��D��D�P Dƀ DǶfD��D� D�L�D̀ Dͬ�D���D��D�<�D�i�Dә�D���D�fD�9�D�l�D٣3D��fD��D�<�D�l�Dߜ�D��3D��D��D�C3D�i�D�3D繚D��3D��D�0 D�Y�D��DD�� D� D�9�D�c3D��3D��3D��fD� D�0 D�L�D�S3D�s3D�� D���D�ɚE p E �3E��E�E� E)�E��E6fE�fEC3E� EFfE� EK3EњEVfE	�3E
�fE�fE� E��E�E$�E��E��E�3Eh EnfE� E�E	�E�fE��E E   E!�3E"�3E#��E%A�E&D�E'��E(�3E*[3E+X E,ٚE-�fE/L�E0C3E1�3E3!�E43E5q�E6�fE7� E98 E:�3E;�3E<�fE@4�EC3EFh EI6fEL^fEO|�ER�fEU��EY!�E\$�E_9�Eb\�Ee�fEh� Ek�3En�Er Eu4�ExNfE{NfE~��E�՚E�h E��fE�rfE��E���E�9�E�� E�<�E�͚E�S3E�� E� �E�FfE�� E��3E�33E���E�ݚE�  E�� E���E�(�E�k3E�͚E��?L��G�O�?L��G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�?fffG�O�G�O�?�  ?���?���?�33?ٙ�?�ff@ff@33@   @9��@L��@Y��@l��@�  @���@�ff@�  @���@�ff@�33@�  @�  @���@���A33A33A33A33A!��A)��A333A9��AA��AI��AQ��AY��Aa��Ah  Ap  Ay��A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441444144414414411111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @ �@ %@ �@ *@ �@ ""@ (�@ 0x@ 6�@ =q@ D�@ Q=@ ^5@ m:@ |?@ �7@ ��@ ��@ �-@ ��@ �|@ �#@ �@ �@@o@g@+�@:@G�@V@c�@r@~�@��@��@�A@��@@�7@ލ@�@�,@�@{@!s@/�@>�@K�@X�@e�@t@��@�@�a@�M@�R@�@��@��@��@��@
�@�@$.@3�@@�@M�@]�@j@ww@�@�h@�@��@�@�@�
@�T@�@ �@�@O@*S@7L@DD@Q=@^5@m�@|�@��@�0@��@��@��@�|@��@��@�@�@�@�@-�@:�@G�@Wb@c�@oF@~�@�D@�<@�A@��@��@��@�/@�@��@�@{@$.@1'@>@M$@Yn@e	@t�@��@�@�U@�Y@�^@ƨ@��@�H@�L@��@
�@6@&�@33@?}@N�@]�@j@v�@��@��@��@��@��@�c@�[@�@�@@V@�@(�@7�@E�@Q�@^�@l�@|?@�7@��@��@�!@��@�|@�t@�y@��@	�@	�@	 �@	-@	:@	I@	V�@	b�@	r@	~K@	�D@	�H@	�A@	�9@	��@	�7@	�/@	�4@	�,@
v@
*@
$.@
/�@
<@
K@
Z@
g@
s_@
��@
�h@
��@
��@
�R@
�W@
Ӡ@
��@
�@@
�E@J@�@$�@4�@A�@M�@\�@k�@x&@��@��@�y@�r@�k@�c@խ@�@�@@�@�@*S@7L@DD@SI@`�@m:@|?@�7@��@��@�-@�2@�|@��@��@� @v@�@[@+�@:@H]@T�@dZ@r@�W@�P@��@��@��@��@ψ@�/@e�@��@��@6�@|?@Ĝ@V@V�@��@�@.l@t@�@  @D�@��@Ӡ@O@bN@�M@�@@4�@z�@��@�@M�@�u@�
@B@^5@�(@�@(G@j@�Y@��@0x@r�@��@��@>�@��@�@�@T�@�H@�
@
@dZ@��@�@7L@~�@��@J@UU@�U@�@(�@p�@��@�E@C�@��@ψ@ @ V�@ ��@ ލ@!""@!e�@!�A@!��@".l@"oF@"��@"�@#4�@#v�@#��@#��@$8�@$z�@$�j@$��@%<@%}�@%�w@&  @&@,@&��@&@'�@'D�@'�|@'�c@(
�@(Lu@(��@(�|@)@)O0@)�i@)��@*{@*UU@*��@*�@+�@+[z@+�@+�/@,
@,^�@,��@,��@- �@-bN@-�4@-�@.(G@.j@.�@.�@@//@/o�@/��@/�@0.l@0oF@0��@0�@1-@1k�@1�Y@1��@2(�@2hs@2��@2�(@3(�@3i�@3�M@3��@4)�@4j@4��@4�@5%�@5bN@5�H@5׹@6{@6Q=@6��@6�c@7�@7A�@7|�@7�R@7�@8-@8e�@8��@8�h@9@9F�@9~K@9�F@9�@:(G@:��@;C�@;�9@<\)@<�|@=x&@=�(@>�<@?J@?�@@(�@@��@A@�@A��@B"�@Bȴ@C8�@C��@DN�@D� @Eg�@E׹@F~�@F�@G��@H�@H�@I�@I�k@J(G@J�@K1'@K�*@Lj@Lє@Mg@N�@Ni�@Oj@O��@P]@P��@Q��@S7�@T��@U�[@W/@X��@Y�@['�@\�@]�(@_:�@`�i@a��@c=q@d�#@e�4@gG�@h��@i�@@k5�@l�I@m��@oDD@p�h@q܀@sB8@t{�@u�M@w@�@x�d@y�h@{$�@{~K@{�@{�e@|K@|�p@|�w@}�@}O�@}��@}��@~*@~j@~�(@~� @-@ G�O�@ G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�@ �G�O�G�O�@ j@ @ �@ %@ 1@ �@ 
�@ J@ �@ b@ o@ �@ �@ �@ �@ �@ �@  �@ #�@ &;@ (�@ ,`@ /@ 1�@ 4�@ 7�@ ;d@ >�@ A�@ D�@ I@ K�@ O0@ R�@ V@ Yn@ \�@ _�@ b�@ g@ i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�bA�{A��A� �A� �A�"�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A�-A�1'A�1'A�33A�33A�33A�1'A�1'A�1'A�1'A�33A�33A�33A�1'A�-A�+A��A�1A���A�Aȥ�A�bA� �A�G�A��A�r�A�A��A�z�A�1A��HA��FA�|�A�
=A�t�A��HA�+A���A�{A��A���A�hsA�E�A�C�A�z�A�+A�ĜA� �A�\)A�&�A�VA��A�oA���A�^5A��A�G�A��A���A�ȴA���A��uA�/A���A��A���A�K�A�"�A���A�7LA�7LA�v�A��DA�5?A��
A�VA���A��wA�p�A�VA��A��TA��A�t�A�%A��PA�%A��`A�+A���A��mA��`A�x�A���A�x�A��A�A��
A�$�A��A���A�x�A��#A���A���A���A��A���A�M�A�ƨA�VA�bNA�9XA��
A�\)A�  A���A��RA���A��#A�t�A���Az��AyVAw?}AqƨAm�AgAcp�A`9XA]�A\z�AZ�jAX~�AU�AS��AQt�AO��AN$�AJI�AIhsAG/AE��AD��ACG�A@�A?��A?XA=&�A;��A;`BA:z�A9��A9��A9�
A8n�A6I�A5oA4jA29XA0ffA.��A,n�A*��A)dZA(��A'��A'�A&�/A&�\A%�
A$�!A$$�A#�7A"��A!��A r�AdZA�`A9XA�
Ar�AbA�AXAȴA��A��A(�A�TAAXAJA��A  A33AbNA��A��A�A��A��A��A��A^5A"�A
ĜA	��AM�AjAȴA�AZA\)A �HA r�@�\)@�@���@�dZ@��@��@���@���@�7L@�p�@�A�@��@��@�7@��@�r�@�1@�|�@���@�5?@�G�@�I�@�@��@��@�h@ߕ�@��@݁@�hs@�7L@ܬ@�(�@ڗ�@�7L@�K�@Л�@�?}@ͺ^@Ϯ@ͺ^@��m@��@���@�ff@���@�7L@�V@�v�@��\@�ƨ@���@���@���@���@���@��@��^@��h@�bN@���@��F@���@�G�@�1@�1@�Q�@���@�@� �@�A�@��F@�7L@��@�z�@���@���@��@�1'@~E�@|��@{��@z��@x�9@w�P@vV@u�T@u�T@sƨ@q7L@o
=@nV@l9X@k33@i�7@gK�@e�@d�j@ct�@`�@^@\��@\9X@Y%@V��@V5?@U�-@SS�@QX@O�;@N{@L1@Ix�@G�@Fv�@E�h@DZ@C�
@C@BM�@@�u@>�y@>V@=��@<�@<(�@:��@9�@8Q�@81'@7�@6��@5@5/@4(�@2~�@1G�@0bN@/��@.ȴ@-�-@-p�@,Z@+ƨ@*-@)7L@'�P@&��@%�@$I�@"�H@!��@!��@ A�@�+@��@V@�D@��@J@��@b@��@��@E�@�@9X@ƨ@o@~�@-@x�@�`@�@l�@�y@$�@�@(�@��@o@
=q@	��@	G�@	%@Ĝ@r�@  @��@l�@;d@��@5?@@��@?}@I�@�F@"�@��@��@��@ �`@ �u?�|�?��?��?�~�?�r�?�?�?�n�?�&�?� �?�R?�j?�"�?�r�?�$�?�9X?�t�?�%?߾w?�V?���?ۥ�?���?���?�1'?�l�?և+?�`B?Լj?�J?щ7?�bN?� �?�\)?�;d?���?�(�?�dZ?�?���?�X?ȓu?�b?���?�K�?��T?�9X?�o?��7?�Ĝ?�\)?���?��-?�I�?�dZ?�dZ?�?���?�=q?�^5?�=q?��?�=q?�~�?���?�"�?�dZ?��m?�I�?��?�O�?���?�v�?���?�|�?� �?�Ĝ?��`?�%?�&�?�hs?��7?��7?���?��?�J?�M�?�M�?\?°!?���?��A�  A�A�A�A�VA�bA�{A�{A�oA�oA�oA��A�bA�oA�bA�VA�bA�bA�oA�oA�bA�bA�bA�bA�bA�VA�oA��A��A��A��A� �A��A��A�"�A� �A� �A� �A�"�A�"�A�$�A�&�A�"�A�"�A�$�A�&�A�&�A�&�A�$�A�&�A�$�A�&�A�$�A�&�A�&�A�&�A�$�A�&�A�&�A�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A�VA�bA�{A��A� �A� �A�"�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A�-A�1'A�1'A�33A�33A�33A�1'A�1'A�1'A�1'A�33A�33A�33A�1'A�-A�+A��A�1A���A�Aȥ�A�bA� �A�G�A��A�r�A�A��A�z�A�1A��HA��FA�|�A�
=A�t�A��HA�+A���A�{A��A���A�hsA�E�A�C�A�z�A�+A�ĜA� �A�\)A�&�A�VA��A�oA���A�^5A��A�G�A��A���A�ȴA���A��uA�/A���A��A���A�K�A�"�A���A�7LA�7LA�v�A��DA�5?A��
A�VA���A��wA�p�A�VA��A��TA��A�t�A�%A��PA�%A��`A�+A���A��mA��`A�x�A���A�x�A��A�A��
A�$�A��A���A�x�A��#A���A���A���A��A���A�M�A�ƨA�VA�bNA�9XA��
A�\)A�  A���A��RA���A��#A�t�A���Az��AyVAw?}AqƨAm�AgAcp�A`9XA]�A\z�AZ�jAX~�AU�AS��AQt�AO��AN$�AJI�AIhsAG/AE��AD��ACG�A@�A?��A?XA=&�A;��A;`BA:z�A9��A9��A9�
A8n�A6I�A5oA4jA29XA0ffA.��A,n�A*��A)dZA(��A'��A'�A&�/A&�\A%�
A$�!A$$�A#�7A"��A!��A r�AdZA�`A9XA�
Ar�AbA�AXAȴA��A��A(�A�TAAXAJA��A  A33AbNA��A��A�A��A��A��A��A^5A"�A
ĜA	��AM�AjAȴA�AZA\)A �HA r�@�\)@�@���@�dZ@��@��@���@���@�7L@�p�@�A�@��@��@�7@��@�r�@�1@�|�@���@�5?@�G�@�I�@�@��@��@�h@ߕ�@��@݁@�hs@�7L@ܬ@�(�@ڗ�@�7L@�K�@Л�@�?}@ͺ^@Ϯ@ͺ^@��m@��@���@�ff@���@�7L@�V@�v�@��\@�ƨ@���@���@���@���@���@��@��^@��h@�bN@���@��F@���@�G�@�1@�1@�Q�@���@�@� �@�A�@��F@�7L@��@�z�@���@���@��@�1'@~E�@|��@{��@z��@x�9@w�P@vV@u�T@u�T@sƨ@q7L@o
=@nV@l9X@k33@i�7@gK�@e�@d�j@ct�@`�@^@\��@\9X@Y%@V��@V5?@U�-@SS�@QX@O�;@N{@L1@Ix�@G�@Fv�@E�h@DZ@C�
@C@BM�@@�u@>�y@>V@=��@<�@<(�@:��@9�@8Q�@81'@7�@6��@5@5/@4(�@2~�@1G�@0bN@/��@.ȴ@-�-@-p�@,Z@+ƨ@*-@)7L@'�P@&��@%�@$I�@"�H@!��@!��@ A�@�+@��@V@�D@��@J@��@b@��@��@E�@�@9X@ƨ@o@~�@-@x�@�`@�@l�@�y@$�@�@(�@��@o@
=q@	��@	G�@	%@Ĝ@r�@  @��@l�@;d@��@5?@@��@?}@I�@�F@"�@��@��@��@ �`@ �u?�|�?��?��?�~�?�r�?�?�?�n�?�&�?� �?�R?�j?�"�?�r�?�$�?�9X?�t�?�%?߾w?�V?���?ۥ�?���?���?�1'?�l�?և+?�`B?Լj?�J?щ7?�bN?� �?�\)?�;d?���?�(�?�dZ?�?���?�X?ȓu?�b?���?�K�?��T?�9X?�o?��7?�Ĝ?�\)?���?��-?�I�?�dZ?�dZ?�?���?�=q?�^5?�=q?��?�=q?�~�?���?�"�?�dZ?��m?�I�?��?�O�?���?�v�?���?�|�?� �?�Ĝ?��`?�%?�&�?�hs?��7?��7?���?��?�J?�M�?�M�?\?°!?���?��A�  A�A�A�A�VA�bA�{A�{A�oA�oA�oA��A�bA�oA�bA�VA�bA�bA�oA�oA�bA�bA�bA�bA�bA�VA�oA��A��A��A��A� �A��A��A�"�A� �A� �A� �A�"�A�"�A�$�A�&�A�"�A�"�A�$�A�&�A�&�A�&�A�$�A�&�A�$�A�&�A�$�A�&�A�&�A�&�A�$�A�&�A�&�A�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�9B
�9B
�9B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�FB
�RB
�dBB<jBR�BcTBjB�{B�?B�^B�}B��B��B��B��B�B�B�;B�yBoBB�BVB`BBk�Bp�B�B�DB�VB�PB�bB�B�7B�=B�\B��B�B�B�B��B��B��B��B��B�uB��B��B�{B�hB�VB�PB�JB�=B|�Bq�BffB^5B[#BT�BL�BD�BB�B:^B6FB49B-B+B�B�BoBDB��B��B�B��B�^B�jB�B��B��B��B�\B�+B�B{�Br�BgmBS�BA�B5?B"�B1B
�B
��B
�RB
�9B
�B
��B
��B
��B
�uB
}�B
v�B
aHB
N�B
:^B	��B	�mB	��B	��B	|�B	H�B	.B	uB	\B	1B��B�yB�
BŢB�dB�-B��B��B��B��B��B��B��B�bB�PB�7B{�B|�Bz�Bz�B}�B�B�oB�7B|�B|�B�%B�PB�B{�Br�Bl�Bp�Bm�Bm�BjBiyBhsBe`BffBdZBdZB`BBaHB^5B^5B^5B^5B\)B[#B[#BYBXBR�BW
BR�BQ�BQ�BP�BM�BM�BJ�BK�BJ�BH�BJ�BG�BE�BF�BE�BB�B@�B?}B@�B=qB;dB>wB<jB@�B>wB@�B>wB=qB<jB<jB:^B=qB?}B=qB<jB9XB6FB2-B49B5?B6FB:^B9XB8RB9XB8RB8RB8RB9XB7LB8RB8RB:^B;dB;dB<jB?}BK�BP�BQ�BR�BQ�BVBT�BZBz�B��B�9B��B�sB��B��B�XBŢBƨB��B��B�#B�B��B	B	B	DB	uB	/B	33B	F�B	XB	`BB	_;B	dZB	|�B	�B	z�B	z�B	�B	��B	�B	�!B	�-B	�B	�'B	�RB	�XB	ÖB	B	ŢB	��B	��B	�)B	�HB	�NB	�`B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
	7B
bB
�B
�B
�B
�B
 �B
!�B
"�B
$�B
'�B
'�B
'�B
+B
-B
.B
/B
2-B
6FB
6FB
7LB
7LB
8RB
9XB
9XB
:^B
;dB
=qB
<jB
=qB
>wB
@�B
B�B
D�B
E�B
D�B
F�B
F�B
G�B
G�B
I�B
K�B
K�B
M�B
N�B
N�B
O�B
N�B
P�B
P�B
S�B
S�B
VB
W
B
XB
XB
ZB
[#B
[#B
^5B
_;B
aHB
`BB
aHB
cTB
cTB
dZB
ffB
ffB
gmB
gmB
hsB
jB
jB
jB
k�B
k�B
l�B
l�B
n�B
n�B
n�B
p�B
q�B
r�B
s�B
r�B
t�B
t�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
y�B
x�B
y�B
y�B
z�B
{�B
{�B
}�B
}�B
|�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�7B
�DB
�PB
�PB
�\B
�bB
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
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�9B
�?B
�FB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�?B
�9B
�9B
�?B
�?B
�?B
�3B
�3B
�9B
�9B
�9B
�3B
�9B
�9B
�9B
�9B
�9B
�9B
�3B
�3B
�3B
�9B
�9B
�3B
�9B
�9B
�9B
�9B
�3B
�3B
�3B
�-B
�9B
�3B
�-B
�9B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�-B
�3B
�3B
�3B
�3B
�-B
�3B
�3B
�3B
�3B
�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  B
�-B
�-B
�-B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�9B
�FB
�XB
��B:^BP�BaHBhsB�oB�3B�RB�qB��B��B��B��B��B�
B�/B�mBbB@�BS�B^5BiyBo�B�B�=B�PB�JB�VB�B�1B�7B�VB��B��B��B�B��B��B��B��B��B�oB��B��B�uB�bB�PB�JB�DB�7B{�Bp�Be`B]/BZBS�BK�BC�BA�B9XB5?B33B,B)�B�B�BhB
=B��B��B�B��B�XB�dB��B��B��B��B�VB�%B�Bz�Bq�BffBR�B@�B49B!�B+B
�B
��B
�LB
�3B
�B
��B
��B
��B
�oB
|�B
u�B
`BB
M�B
9XB	��B	�fB	��B	��B	{�B	G�B	-B	oB	VB	+B��B�sB�BĜB�^B�'B��B��B��B��B��B��B�{B�\B�JB�1Bz�B{�By�By�B|�B�B�hB�1B{�B{�B�B�JB�Bz�Bq�Bk�Bo�Bl�Bl�BiyBhsBgmBdZBe`BcTBcTB_;B`BB]/B]/B]/B]/B[#BZBZBXBW
BQ�BVBQ�BP�BP�BO�BL�BL�BI�BJ�BI�BG�BI�BF�BD�BE�BD�BA�B?}B>wB?}B<jB:^B=qB;dB?}B=qB?}B=qB<jB;dB;dB9XB<jB>wB<jB;dB8RB5?B1'B33B49B5?B9XB8RB7LB8RB7LB7LB7LB8RB6FB7LB7LB9XB:^B:^B;dB>wBJ�BO�BP�BQ�BP�BT�BS�BYBy�B��B�3B��B�mB��B��B�RBĜBŢB��B��B�B�B��B	B	B	
=B	oB	.B	2-B	E�B	W
B	_;B	^5B	cTB	{�B	� B	y�B	y�B	�B	��B	�B	�B	�'B	�B	�!B	�LB	�RB	B	��B	ĜB	ɺB	��B	�#B	�BB	�HB	�ZB	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
	7B
bB
�B
�B
�B
�B
 �B
!�B
"�B
$�B
'�B
'�B
'�B
+B
-B
.B
/B
2-B
6FB
6FB
7LB
7LB
8RB
9XB
9XB
:^B
;dB
=qB
<jB
=qB
>wB
@�B
B�B
D�B
E�B
D�B
F�B
F�B
G�B
G�B
I�B
K�B
K�B
M�B
N�B
N�B
O�B
N�B
P�B
P�B
S�B
S�B
VB
W
B
XB
XB
ZB
[#B
[#B
^5B
_;B
aHB
`BB
aHB
cTB
cTB
dZB
ffB
ffB
gmB
gmB
hsB
jB
jB
jB
k�B
k�B
l�B
l�B
n�B
n�B
n�B
q�B
r�B
s�B
t�B
s�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
z�B
y�B
z�B
z�B
{�B
|�B
|�B
~�B
~�B
}�B
� B
� B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�7B
�=B
�JB
�VB
�VB
�bB
�hB
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
�B
�B
�B
�B
�B
�B
�'B
�-B
�3B
�9B
�?B
�FB
�FB
�LB
�RB
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�wB
�wB
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
��B
�}B
�}B
��B
�}B
�}B
�}B
��B
�3B
�-B
�-B
�3B
�3B
�3B
�'B
�'B
�-B
�-B
�-B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�'B
�'B
�'B
�-B
�-B
�'B
�-B
�-B
�-B
�-B
�'B
�'B
�'B
�!B
�-B
�'B
�!B
�-B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�!B
�'B
�'B
�'B
�'B
�!B
�'B
�'B
�'B
�'B
�'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202362021061413521820210614135218202106141746262021061417462620210614174626201807242202362021061413521820210614135218202106141746262021061417462620210614174626PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023620180724220236  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023620180724220236QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023620180724220236QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015020210722160150IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                