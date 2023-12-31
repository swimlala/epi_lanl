CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-11T20:09:30Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   
�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	0      � 	0Argo profile    3.1 1.2 19500101000000  20190211200930  20210722160157  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               8   8DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ء�;�\�@ء�;�\�11  @ء�33H�@ء�33H�@5����1@5����1�c�!�5�`�c�!�5�`11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@ff@Fff@�33@�  @�33@�ffA��A  A#33AA��Ac33A���A���A���A���A���Aљ�A���A���B ffB��B  B  B ��B(��B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�33B�33B�33B�ffB�33B�33B�33B�ffB�33B�  B�33B�33B�33B�  B�33B�ffB�33B�  B�ffB�33B�33B�33B�ffB�33B�  B�33B�ffB�B�  B���B�  C �CL�C�C  C33C
�C�fC  C33C�C�fC33C  C��C�C33C �C!�fC$  C&33C(�C)��C+�fC.  C033C233C4L�C6L�C8ffC:�C;��C=�fC?�fCA�fCC�fCE�fCG��CI��CK�3CM��CO��CQ��CT�CVL�CXL�CZ�C[�3C]��C_��Ca�fCc�fCe�fCg�fCi��Ck�3Cm��Co��Cq�fCs�fCu�fCw�fCy�fC{�fC}��C��C��fC��fC��3C��3C�  C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C��3C��fC��fC��fC�  C�33C�&fC��C��C��C��C�  C��3C��fC��fC��fC��C��C��C��C�  C��3C�  C�  C��3C��C�&fC��C��C��3C��3C�  C��C�  C��3C��3C��C��C��C�  C�  C��3C��fC�  C��C��C��C�  C��3C��3C��fC��C�  C�  C��C��C��C��C��C��C�  C��3C�  C��C��C�  C��fC�  C��C�  C��fC��C��C��3C�  C��C�  C�ٚC�  C�  C�  C�  C��3C�  C��C��3C��C��C��3C�  C��C��C��C�  C�  C��C��3C�  C��C�  C��3C��fC��fC�&fC��C��D   D y�DL�D�3D
  DL�D��D�fD  D,�DS3Dy�D� D� D� D"  D$�D&  D(@ D*ffD,�3D.� D0�3D3&fD5@ D7l�D9� D;��D>  D@9�DB` DD��DF� DH��DK3DMFfDOl�DQ� DS�fDV&fDXl�DZ�3D]fD_S3Da� Dc��Df,�Dhs3Dj�3Dl�3Do�DqFfDs�fDu� Dw� Dy�3D{�3D}�3D�fD�  D�fD� D��D��D�  D�,�D�9�D�@ D�@ D�<�D�FfD�I�D�FfD�C3D�C3D�I�D�S3D�Y�D�Y�D�ffD�c3D�` D�ffD�ffD�ffD�s3D�y�D��3D���D��fD�� D��3D�� D��fD��fD���D���D���D��3D��fD���D���D�� D��fD�� D��3D�� D��fD���D��3D��3D��3D�� D���D���D��3D���D��3D�|�D�y�D�s3D�p D�ffD�c3D�c3D�` D�` D�Y�D�L�D�9�D�#3D�3D�  D��D�ٚD��fDͶfDΠ Dϓ3DІfD�s3D�c3D�S3D�<�D�,�D��D��D���D�� D�� D��fD۳3Dܙ�D݃3D�ffD�S3D�<�D�#3D��D��fD��3D�� D� D�fD�l�D�<�D�fD��fD�� D�ffD�@ D�fD�� D��D�� D�c3D�  D���D�� D�VfD��3E�E8 Ed�E��E��E6fE\�E	{3EfE,�ES3Ey�E�3EA�EvfE� E�E ED�Ep E�E)�EL�Ec3E � E#��E'�E*3E-<�E0X E3S3E6c3E9� E<� E=d�E=� E>�3E?)�E?�fE@a�EAfEA��EBK3EC	�EC�3EDP ED�fEE�3EF EF� EGX EG�fEH�3EII�EI��EJ��EK EK� ELFfEM�EM� EN8 EN�3EOp EP#3EP� EQT�ERfER|�ES,�ES�fET� >���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?   ?��?��?��?333?fff?�  ?���?���?�  ?ٙ�?�33@��@   @333@Fff@`  @y��@�ff@�33@�  @���@�ff@�33@�  @�  @���@���AffA33A33A33A#33A)��A1��A9��AA��AH  AP  AVffA`  Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444144444414141144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ?L��?�33@&ff@fff@�33@�  @�33@�ffA	��A  A+33AI��Ak33A���A���A���A���A���Aՙ�A���A���BffB
��B  B  B"��B*��B2  B:  BB  BJ  BR  BZ  BbffBj  Br  Bz  B�  B�33B�33B�33B�ffB�33B�33B�33B�ffB�33B�  B�33B�33B�33B�  B�33B�ffB�33B�  B�ffB�33B�33B�33B�ffB�33B�  B�33B�ffB�B�  B���B�  C ��C��C��C� C�3C
��CffC� C�3C��CffC�3C� CL�C��C�3C ��C"ffC$� C&�3C(��C*L�C,ffC.� C0�3C2�3C4��C6��C8�fC:��C<L�C>ffC@ffCBffCDffCFffCHL�CJL�CL33CNL�CPL�CRL�CT��CV��CX��CZ��C\33C^L�C`L�CbffCdffCfffChffCjL�Cl33CnL�CpL�CrffCtffCvffCxffCzffC|ffC~L�C�&fC�&fC�&fC�33C�33C�@ C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�33C�&fC�&fC�&fC�@ C�s3C�ffC�Y�C�L�C�L�C�L�C�@ C�33C�&fC�&fC�&fC�L�C�Y�C�Y�C�L�C�@ C�33C�@ C�@ C�33C�Y�C�ffC�Y�C�L�C�33C�33C�@ C�Y�C�@ C�33C�33C�L�C�Y�C�L�C�@ C�@ C�33C�&fC�@ C�Y�C�Y�C�L�C�@ C�33C�33C�&fC�L�C�@ C�@ C�Y�C�Y�C�Y�C�L�C�Y�C�L�C�@ C�33C�@ C�Y�C�L�C�@ C�&fC�@ C�Y�C�@ C�&fC�L�C�Y�C�33C�@ C�Y�C�@ C��C�@ C�@ C�@ C�@ C�33C�@ C�L�C�33C�Y�C�Y�C�33C�@ C�Y�C�Y�C�Y�C�@ C�@ C�L�C�33C�@ C�L�C�@ C�33C�&fC�&fC�ffC�L�C�L�D   D ��Dl�D�3D
  Dl�D��D�fD  DL�Ds3D��D� D� D   D"  D$,�D&@ D(` D*�fD,�3D.� D13D3FfD5` D7��D9� D;��D>  D@Y�DB� DD��DF� DI�DK33DMffDO��DQ� DTfDVFfDX��DZ�3D]&fD_s3Da� Dd�DfL�Dh�3Dj�3Dm3Do,�DqffDs�fDu� Dx  Dz3D{�3D}�3D�3D� D�fD�  D�)�D�)�D�0 D�<�D�I�D�P D�P D�L�D�VfD�Y�D�VfD�S3D�S3D�Y�D�c3D�i�D�i�D�vfD�s3D�p D�vfD�vfD�vfD��3D���D��3D���D��fD�� D��3D�� D��fD��fD���D���D���D��3D��fD�ɚD�ɚD�� D��fD�� D��3D�� D��fD���D��3D��3D��3D�� D���D���D��3D���D��3D���D���D��3D�� D�vfD�s3D�s3D�p D�p D�i�D�\�D�I�D�33D�#3D� D���D��D��fD��fDΰ Dϣ3DЖfDу3D�s3D�c3D�L�D�<�D�,�D��D��D�  D�� D��fD��3Dܩ�Dݓ3D�vfD�c3D�L�D�33D��D�fD��3D�� D�� D�fD�|�D�L�D�&fD��fD�� D�vfD�P D�fD�� D���D�� D�s3D�0 D��D�� D�ffD��3E�E@ El�E��E��E>fEd�E	�3EfE4�E[3E��E�3EI�E~fE� E�E  EL�Ex E	�E1�ET�Ek3E � E#��E'�E*3E-D�E0` E3[3E6k3E9� E<� E=l�E=� E>�3E?1�E?�fE@i�EAfEA��EBS3EC�EC�3EDX ED�fEE�3EF  EF� EG` EG�fEH�3EIQ�EI��EJ��EK EK� ELNfEM	�EM� EN@ EN�3EOx EP+3EP� EQ\�ERfER��ES4�ES�fET� G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�?fff?�  G�O�G�O�?���?���?�33?�  ?���?ٙ�@   @��@��@,��@@  @S33@fff@�  @���@�ff@�33@�  @���@�ff@�33@�  @�  @���A��AffA33A33A#33A+33A1��A9��AA��AI��AP  AX  A^ffAh  Ap  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444144444414141144111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      @ @ %@ V@ *@ �@ ""@ )�@ 1'@ 7L@ =q@ E�@ R�@ `�@ m�@ {�@ ��@ ��@ ��@ ��@ ��@ �|@ �#@ �y@ ��@j@o@ @,`@:@G�@UU@b�@p�@~�@��@��@�A@��@�>@��@ލ@��@��@�@*@#�@0x@=q@K�@Yn@g@t@�d@��@��@��@��@ƨ@�O@��@�L@�E@
=@�@&�@5?@@�@M�@\)@j@y�@��@��@��@��@�@�c@�h@�`@�@^@�@�@)�@7�@D�@Q=@_�@n�@{�@�+@��@��@��@��@��@܀@��@�q@@b@
@+�@9X@F�@S�@a�@n�@|�@��@�<@��@��@Ĝ@��@��@�(@��@%@�@!s@/@<@I@Wb@e	@s_@�@��@�U@��@��@Ĝ@�C@��@�@��@	�@�@&;@3�@B8@O0@\�@j@x&@��@�u@�@��@��@�c@�
@�@�Y@�Q@J@�@'�@6�@F�@S�@`�@m�@{�@�7@�0@�(@�!@��@�o@�#@�y@� @	@	@	
@	,`@	:@	F�@	V�@	e	@	r@	~�@	�D@	��@	�A@	�F@	@	ψ@	�/@	�4@	��@
�@
{@
""@
/@
<@
K@
Z@
g�@
t�@
��@
��@
�U@
�M@
�@
��@
Ӡ@
�@
�L@
��@
�@B@&;@33@@,@N�@]�@j@ww@��@��@��@�@�^@�@�h@�@�Y@^@�@B@(�@6�@DD@Q�@^�@m:@{�@��@��@�5@��@�&@�*@��@�y@��@j@�@
@,`@:�@G�@T�@a�@oF@�W@��@�H@�A@�9@7�@y�@�R@� @4�@qS@�@�y@$.@^�@��@Ӡ@�@G�@�@�R@�Y@-@hs@��@��@�@UU@��@��@1@DD@�@��@� @33@n�@�M@�`@ @\)@�H@׹@�@S�@�u@�C@@O�@�P@�o@�@FQ@�@�j@��@6�@p�@�M@�@@K�@�@�@��@.l@e	@�@�\@�@G�@~K@�9@��@$/@Z@�@ƨ@��@7L@oF@��@�;@ *@ K@ �@ ��@ �L@!)�@!a�@!�H@!��@"�@"DD@"{�@"�~@"�y@# @#Wb@#��@#�J@#�E@$4�@$k�@$�z@$�
@%@%DD@%{�@%�~@%�@&
@&V@&��@&�>@&�,@'/@'e�@'�H@'ψ@(@(5�@(k�@(�@(�
@)�@)A�@)x&@)�@)�@*�@*M�@*�W@*�-@*�`@+�@+I�@+|�@+�r@+�@,{@,H]@,|?@,��@,��@-*@-F�@-z3@-�f@-��@.�@.G�@.z�@.�@.ލ@/�@/A�@/r@/��@/�\@0�@09X@0k.@0��@0�7@16�@1dZ@1��@1�~@2&�@2�|@2��@3o@3@�@3��@3�7@3�Q@4b�@4�@4��@5
@5z�@5��@6\�@7J@7�\@8�@8��@9
�@9��@:.l@:��@;Q=@;��@<Lu@<�@=Lu@=��@>}�@?j@?�7@@
=@@��@A
=@A��@B3�@B�!@C&�@D��@E׹@GH]@H��@I�(@K=q@L�@Mє@O/�@Pz�@P��@Q�@QZ@Q�@Q��@R*@RbN@R�!@R�@S7L@Sn�@S@S�}@TLu@T�@T��@U�@UC�@U�#@U��@V�@Vj@V�m@V�Y@W(G@Wx&@W�@W��@XLu@X��@X�*@Yj@YP�@Y�T@Y��@Z�@ZbN@Z��G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�@ G�O�@ �@ jG�O�G�O�@ @ �@ %@ �@ �@ 1@ 
=@ �@ �@ @ @ @ *@ �@ �@ �@ g@ ""@ $.@ &�@ )�@ ,`@ /�@ 2�@ 5?@ 9X@ ;d@ >�@ B8@ E�@ H]@ K�@ O0@ R�@ UU@ X�@ [z@ _�@ b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�9XA�=qA�A�A�G�A�I�A�K�A�I�A�M�A�S�A�S�A�^5A�bNA�bNA�dZA�ffA�dZA�ffA�ffA�ffA�dZA�dZA�\)A�VA�-A�33A���A��TA���A��RA��\A�~�A�z�A�z�A�~�A��A��PA���A���A���A��hA��hA���A��PA��7A��PA��\A��uA��hA���A��A�ZA�A�~�A��`A��!A�;dA��A�A�{A���A���A�ZA�p�A� �A���A�=qA�9XA�9XA��A���A�x�A���A�$�A��PA�A�A�1A�+A�r�A�bA��#A��+A�9XA�A�33A��PA��A��A���A���A���A��TA��A�~�A�ZA�G�A�5?A�"�A�%A�&�A��TA��A���A���A��A�+A�9XA�5?A�"�A��A�VA��wA�=qA���A��A��+A��mA�S�A��A���A��A{�^Az(�Ay��Ay�7AxĜAvM�Ar��Aq�wAp-AlȴAh��Af�`Ae�PAc��AcK�Aa��A^A�A[G�AW�;AUS�AR��ARAQ��AQS�AQ+AP�ANjAK�^AI�7AG��AE�-AEC�AD5?ABbNA?�hA>E�A=A=��A<�yA;��A;S�A:��A9G�A8jA6=qA4bNA2jA21'A1��A0z�A/�A,��A+33A)��A(��A(JA't�A&�A& �A%�A$v�A#�A"�yA"^5A!?}A Q�AȴA��AbNA1'At�A�A��A�#A��A��A{A�A-A��A�Av�AVAt�A�A  A��AXA
�A
�HA
�A
�!A
�\A
bA	hsAr�A�FA�hAt�A+A�/A�HA9XAbA�-A�`Ax�A �/@�"�@�@�j@�(�@��m@�l�@�S�@�ȴ@���@��+@�ff@�n�@�n�@�^5@�E�@�-@�z�@�bN@�!@�7@�j@��@�n�@�%@��m@�33@�-@��@�$�@�-@�/@ާ�@��H@�/@�&�@�x�@�A�@��@���@�V@�Z@���@�bN@��w@�r�@��y@�O�@��\@�{@�?}@�Ĝ@���@���@�Ĝ@�(�@��F@�dZ@��@��@�Z@�ff@��D@�b@���@��@���@��\@��-@��9@�\)@��+@��-@���@��
@�@�E�@�J@�O�@�(�@���@��y@�O�@���@�  @��+@���@�O�@�z�@�1'@�S�@�o@�^5@�7L@��u@�w@}V@z=q@x�u@xbN@v�R@st�@r�!@q7L@p �@nff@n5?@m��@k��@j~�@i��@h�@h  @g��@f�+@e@d�@c��@a��@a�@` �@_�@]��@[��@Z��@Y�@X�@V{@T�/@T1@R��@Q�^@P�`@P  @Nff@M�@M�@MO�@L�/@Lz�@K��@J�\@J=q@HĜ@Fȴ@F�+@E�-@E/@C�
@B��@A��@?l�@=�h@=/@<(�@9��@97L@8�`@81'@7\)@7
=@6�+@5?}@4Z@4�@333@2^5@2M�@1�#@1hs@0�9@01'@/�w@-�T@-/@+��@*�\@)��@(  @'�@'��@'
=@&V@%O�@$�/@$�@#�
@"��@!G�@ A�@+@v�@V@1@dZ@~�@X@�`@��@�T@�D@��@��@hs@�9@�@ȴ@��@�D@��@"�@	��@	�7@A�@ȴ@�@��@��@J@�@ b?���?���?���?��9?�?��?�hs?�|�?��-?���?��H?��?�?��?�S�?�!?�J?�\)?���?ڟ�?�Q�?���?�J?�|�?Ͳ-?�ƨ?�^5?��#?��#?���?ȴ9?�1'?Ǯ?�
=?�ȴ?�$�?Ł?�?}?���?���?�z�?�Z?��
?�S�?���?�-?�J?���?���?�hs?�hs?�&�?��`?���?� �?�  ?��w?�|�?�;d?��?��?���?��?���?�v�A�A�A�A�A�C�A�?}A�?}A�1'A�(�A�;dA�=qA�C�A�?}A�9XA�;dA�7LA�;dA�;dA�=qA�;dA�;dA�;dA�9XA�7LA�5?A�7LA�9XA�;dA�;dA�?}A�=qA�=qA�=qA�=qA�?}A�A�A�A�A�E�A�G�A�I�A�G�A�K�A�K�A�K�A�K�A�G�A�K�A�K�A�M�A�Q�A�S�A�S�A�S�A�VA�ZA�^5A�^5A�`BA�bNA�`BA�bNA�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      A�;dA�9XA�=qA�A�A�G�A�I�A�K�A�I�A�M�A�S�A�S�A�^5A�bNA�bNA�dZA�ffA�dZA�ffA�ffA�ffA�dZA�dZA�\)A�VA�-A�33A���A��TA���A��RA��\A�~�A�z�A�z�A�~�A��A��PA���A���A���A��hA��hA���A��PA��7A��PA��\A��uA��hA���A��A�ZA�A�~�A��`A��!A�;dA��A�A�{A���A���A�ZA�p�A� �A���A�=qA�9XA�9XA��A���A�x�A���A�$�A��PA�A�A�1A�+A�r�A�bA��#A��+A�9XA�A�33A��PA��A��A���A���A���A��TA��A�~�A�ZA�G�A�5?A�"�A�%A�&�A��TA��A���A���A��A�+A�9XA�5?A�"�A��A�VA��wA�=qA���A��A��+A��mA�S�A��A���A��A{�^Az(�Ay��Ay�7AxĜAvM�Ar��Aq�wAp-AlȴAh��Af�`Ae�PAc��AcK�Aa��A^A�A[G�AW�;AUS�AR��ARAQ��AQS�AQ+AP�ANjAK�^AI�7AG��AE�-AEC�AD5?ABbNA?�hA>E�A=A=��A<�yA;��A;S�A:��A9G�A8jA6=qA4bNA2jA21'A1��A0z�A/�A,��A+33A)��A(��A(JA't�A&�A& �A%�A$v�A#�A"�yA"^5A!?}A Q�AȴA��AbNA1'At�A�A��A�#A��A��A{A�A-A��A�Av�AVAt�A�A  A��AXA
�A
�HA
�A
�!A
�\A
bA	hsAr�A�FA�hAt�A+A�/A�HA9XAbA�-A�`Ax�A �/@�"�@�@�j@�(�@��m@�l�@�S�@�ȴ@���@��+@�ff@�n�@�n�@�^5@�E�@�-@�z�@�bN@�!@�7@�j@��@�n�@�%@��m@�33@�-@��@�$�@�-@�/@ާ�@��H@�/@�&�@�x�@�A�@��@���@�V@�Z@���@�bN@��w@�r�@��y@�O�@��\@�{@�?}@�Ĝ@���@���@�Ĝ@�(�@��F@�dZ@��@��@�Z@�ff@��D@�b@���@��@���@��\@��-@��9@�\)@��+@��-@���@��
@�@�E�@�J@�O�@�(�@���@��y@�O�@���@�  @��+@���@�O�@�z�@�1'@�S�@�o@�^5@�7L@��u@�w@}V@z=q@x�u@xbN@v�R@st�@r�!@q7L@p �@nff@n5?@m��@k��@j~�@i��@h�@h  @g��@f�+@e@d�@c��@a��@a�@` �@_�@]��@[��@Z��@Y�@X�@V{@T�/@T1@R��@Q�^@P�`@P  @Nff@M�@M�@MO�@L�/@Lz�@K��@J�\@J=q@HĜ@Fȴ@F�+@E�-@E/@C�
@B��@A��@?l�@=�h@=/@<(�@9��@97L@8�`@81'@7\)@7
=@6�+@5?}@4Z@4�@333@2^5@2M�@1�#@1hs@0�9@01'@/�w@-�T@-/@+��@*�\@)��@(  @'�@'��@'
=@&V@%O�@$�/@$�@#�
@"��@!G�@ A�@+@v�@V@1@dZ@~�@X@�`@��@�T@�D@��@��@hs@�9@�@ȴ@��@�D@��@"�@	��@	�7@A�@ȴ@�@��@��@J@�@ b?���?���?���?��9?�?��?�hs?�|�?��-?���?��H?��?�?��?�S�?�!?�J?�\)?���?ڟ�?�Q�?���?�J?�|�?Ͳ-?�ƨ?�^5?��#?��#?���?ȴ9?�1'?Ǯ?�
=?�ȴ?�$�?Ł?�?}?���?���?�z�?�Z?��
?�S�?���?�-?�J?���?���?�hs?�hs?�&�?��`?���?� �?�  ?��w?�|�?�;d?��?��?���?��?���?�v�A�A�A�A�A�C�A�?}A�?}A�1'A�(�A�;dA�=qA�C�A�?}A�9XA�;dA�7LA�;dA�;dA�=qA�;dA�;dA�;dA�9XA�7LA�5?A�7LA�9XA�;dA�;dA�?}A�=qA�=qA�=qA�=qA�?}A�A�A�A�A�E�A�G�A�I�A�G�A�K�A�K�A�K�A�K�A�G�A�K�A�K�A�M�A�Q�A�S�A�S�A�S�A�VA�ZA�^5A�^5A�`BA�bNA�`BA�bNA�bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	7B	7B	7B	7B	7B1B1B	7B	7B1B1B	7B1B1B	7B	7B	7B	7B	7B1B	7BJB�BZB��B��B�B�LB�dB�-B�9B�FB�LB�RB�XB�^B�qB�}B��B��B�wB�}B��B�}B��BBÖBǮBɺB��B��B��B�NB��B
=B2-B@�BL�BM�BM�BVBffBiyBk�BjBffB_;BT�BQ�BB�B=qB9XB49B8RBI�BR�BT�BT�BS�BJ�B<jB:^B6FB/B%�B�B�B{BoBhBVB%B  B��B��B��B��B��B�B�NB�BǮB�RB��B�+B\)BG�B33B �B
�HB
�}B
��B
��B
��B
�JB
� B
v�B
l�B
[#B
L�B
�B	��B	�B	�yB	�NB	�#B	��B	�-B	��B	��B	�B	hsB	aHB	S�B	L�B	F�B	;dB	#�B	�B	+B��B�B�B�sB�fB�ZB�/B��BƨB�^B�B��B��B��B�PB�7B�DB�PB�\B�hB�7B�+B� B~�B|�Bx�Bv�B{�By�Bw�Bu�Bp�Bk�BiyBiyBk�BiyBiyBgmBgmBe`BffBe`BffBcTBe`Be`BcTBaHB_;B^5BYBW
BXBYB\)B\)BXB[#BYBS�BR�BP�BO�BI�BE�BG�BJ�BS�B]/B^5B_;BbNBm�Bs�Bu�By�B}�B|�B~�B� B}�B� B�B� B�B~�B|�Bz�Bx�B}�B�B�B�B�B�B�B�B�%B�%B�%B�%B�%B�B�B�B�B�B�%B�B�1B�1B�{B��B��B��B��B��B��B��B��B�{B��B�bB��B��B�B��B�^B�qB��B��B�NB	B	�B	&�B	:^B	B�B	H�B	M�B	]/B	^5B	k�B	o�B	u�B	v�B	�B	�7B	�VB	��B	��B	�B	�?B	�XB	�}B	ĜB	ɺB	��B	��B	�
B	�B	�/B	�HB	�TB	�fB	�mB	�yB	�B	�B	�B	�B	�B	��B	��B
  B
B
B
%B
+B
1B
DB
JB
\B
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
#�B
&�B
&�B
)�B
+B
)�B
-B
,B
+B
,B
+B
,B
/B
0!B
2-B
33B
49B
7LB
8RB
8RB
9XB
:^B
;dB
;dB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
A�B
@�B
B�B
D�B
D�B
D�B
E�B
G�B
G�B
G�B
K�B
L�B
L�B
M�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
S�B
T�B
S�B
VB
T�B
T�B
VB
VB
VB
W
B
W
B
ZB
YB
ZB
\)B
]/B
`BB
_;B
^5B
`BB
`BB
aHB
bNB
aHB
aHB
cTB
dZB
ffB
gmB
ffB
hsB
jB
jB
jB
l�B
l�B
m�B
n�B
p�B
p�B
s�B
s�B
s�B
u�B
t�B
v�B
v�B
u�B
u�B
v�B
w�B
x�B
z�B
z�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�+B
�1B
�=B
�DB
�DB
�VB
�VB
�\B
�bB
�hB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�3B1B	7B1B	7B%B	7BJB	7B
=B+B1B1B	7B	7B	7B	7B1B1B	7B	7B	7B1B
=B	7B	7B1B	7B1B1B	7B1B	7B	7B1B	7B1B	7B1B	7B1B1B1B1B	7B1B1B	7B1B1B1B1B1B	7B	7B1B	7B1B1B1B1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      B%B%B%B%B%BBB%B%BBB%BBB%B%B%B%B%BB%B	7BuBW
B��B��B��B�9B�RB�B�'B�3B�9B�?B�FB�LB�^B�jB�qB�qB�dB�jB�qB�jB�qB�}B��BĜBƨB��B��B��B�;B��B+B/B=qBI�BJ�BJ�BR�BcTBffBhsBgmBcTB\)BQ�BN�B?}B:^B6FB1'B5?BF�BO�BQ�BQ�BP�BG�B9XB7LB33B,B"�B�B{BhB\BVBDBB��B��B��B��B��B�B�B�;B��BĜB�?B��B�BYBD�B0!B�B
�5B
�jB
��B
��B
�oB
�7B
|�B
s�B
iyB
XB
I�B
�B	��B	�B	�fB	�;B	�B	�qB	�B	��B	��B	�B	e`B	^5B	P�B	I�B	C�B	8RB	 �B	�B	B��B�B�yB�`B�TB�HB�B��BÖB�LB��B��B��B��B�=B�%B�1B�=B�JB�VB�%B�B|�B{�By�Bu�Bs�Bx�Bv�Bt�Br�Bm�BhsBffBffBhsBffBffBdZBdZBbNBcTBbNBcTB`BBbNBbNB`BB^5B\)B[#BVBS�BT�BVBYBYBT�BXBVBP�BO�BM�BL�BF�BB�BD�BG�BP�BZB[#B\)B_;BjBp�Br�Bv�Bz�By�B{�B|�Bz�B|�B~�B|�B}�B{�By�Bw�Bu�Bz�B~�B~�B~�B�B�B�B�B�B�B�B�B�B�B�B� B~�B�B�B�B�B�B�hB��B��B��B��B��B��B��B��B�hB�oB�VB��B��B��B��B�RB�dBɺB��B�BB��B	�B	$�B	8RB	@�B	F�B	K�B	[#B	\)B	iyB	m�B	s�B	t�B	� B	�+B	�JB	��B	��B	��B	�3B	�LB	�qB	B	ǮB	��B	��B	��B	�B	�#B	�;B	�HB	�ZB	�`B	�mB	�B	�B	�B	�B	�B	�B	��B	��B
B
B
B
B
%B
	7B

=B
PB
VB
hB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
$�B
'�B
(�B
'�B
+B
)�B
(�B
)�B
(�B
)�B
-B
.B
0!B
1'B
2-B
5?B
6FB
6FB
7LB
8RB
9XB
9XB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
?}B
>wB
@�B
C�B
C�B
C�B
D�B
F�B
F�B
F�B
J�B
K�B
K�B
L�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
R�B
S�B
R�B
T�B
S�B
S�B
T�B
T�B
T�B
VB
VB
YB
XB
YB
[#B
\)B
_;B
^5B
]/B
_;B
_;B
`BB
aHB
`BB
`BB
bNB
cTB
e`B
ffB
e`B
gmB
iyB
iyB
iyB
k�B
k�B
l�B
m�B
o�B
o�B
r�B
r�B
r�B
t�B
s�B
u�B
u�B
t�B
t�B
u�B
v�B
w�B
y�B
y�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�%B
�+B
�7B
�DB
�DB
�VB
�VB
�\B
�bB
�hB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�B
�!B
�!B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�9B
�9BB%BB%BB%B	7B%B+BBBB%B%B%B%BBB%B%B%BB+B%B%BB%BBB%BB%B%BB%BB%BB%BBBBB%BBB%BBBBBB%B%BB%BBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201902112009302021061413531420210614135314202106141747072021061417470720210614174707201902112009302021061413531420210614135314202106141747072021061417470720210614174707PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019021120093020190211200930  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019021120093020190211200930QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019021120093020190211200930QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015720210722160157IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                