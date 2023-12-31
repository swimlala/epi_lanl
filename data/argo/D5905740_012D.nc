CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:03Z creation      
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
_FillValue                 (  Ll   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  a,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  eT   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ل   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   $   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   @   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220303  20210722161419  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�m�D�Y@�m�D�Y11  @�m�>��@�m�>��@*R'�(��@*R'�(���cL��0$�cL��0$11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?fff@   @@  @�  @�33@�ff@�ffA33A��A!��AA��Ac33A�  A���A���A�  A�  A���A���A���B ffBffBffB  B ffB(ffB0ffB8  B@ffBHffBO33BW33B`  Bh  Bp��BxffB�  B�33B�  B�  B�  B���B�  B�33B�ffB�ffB�ffB�ffB�  B���B�  B�ffB�ffB�33B�  B�  B���B�ffB�ffB�33B���B㙚B�  B왚B�ffB�33B�ffB�ffC �C  C  CL�C33C
  C�fC  C  C�C��C�fC33C�fC�C33C   C!��C$�C&L�C(�C*33C,L�C.  C033C2L�C4  C6  C8�C:33C<L�C>  C@  CB  CD  CF  CG�fCJ33CL�CN  CO�fCQ�fCT33CV33CX  CZL�C\33C^  C`  Ca�fCd�Ce�fCg��Cj  Cl33Cn  Co�fCr�Ct33Cv  Cw��Cy�fC|�C~�C�&fC��C��3C�  C��C��C��3C��C��C��fC�  C��C�&fC��C��fC��3C��C��C�&fC�  C��fC��3C��3C��C��C�&fC�&fC��C��fC��3C�  C��C�&fC�  C��3C�  C��C�&fC�33C�&fC��C��C�33C�&fC�  C��C��C��fC�  C��C��C�&fC��C��3C��C�&fC��C�  C��C�  C��fC��3C��C�&fC��C��3C��C�  C��3C��C��C�&fC�&fC��C�  C��C�&fC��C��3C��C��C�  C��fC�  C��C�  C��3C�  C��C�  C��fC�  C��C�&fC��C�  C��C�33C�&fC��C�  C�  C��C��C��3C��C�&fC��C��C�&fC�&fC��C��C��3C��C�&fC��C�  C��C��C��3C��C��C��3D�3D` D33D
��D�fD��DFfD  D�3D�fD  D �3D#ffD%��D(�3D+�D-��D09�D2�fD5@ D7�fD:Y�D<�3D?�fDB&fDD�fDGl�DJ  DL�3DOFfDQ�3DT��DWY�DZ�D\��D_S3Da�3Dd��DgS3Di�3Dl��Do&fDq�fDtY�Dv��Dy�fD{��D~@ D�p D��3D�3D�` D�� D�fD�c3D��3D� D�l�D��3D�fD�p D��fD�  D�s3D��3D�  D�s3D��fD��D�s3D��fD�  D�y�D��fD��D�ffD���D�� D�6fD��3D��3D�3D�S3D���D�� D�)�D�l�D���D��fD�&fD�i�D���D�� D�&fD�p D��3D���D�<�D�|�D¶fD�3D�I�DƆfD�� D��fD�0 D�p D̬�D�� D�33D�l�DѦfD��fD�3D�S3D֐ D��3D��D�9�D�p DܦfD��fD�	�D�@ D�y�D��D��fD�  D�&fD�I�D�vfD� D���D�� D�fD�9�D�VfD�y�D� D��fD�� D�fD�33D�Y�D��3D�� D��3D��3D�#3D�L�D�y�E S3E �fE|�E�E��E<�E��E` E�E�3E�E�3E>fE�fEh E��E	�fE
fE
�fE4�EQ�Ed�Es3E3E�E��E� E3E!�E� E��E��E�3Ec3E� E� E X E!X E"��E#�E%h E&p E'|�E)�E*3E+� E,��E-�3E/@ E0L�E1� E2�3E3��E5� E6� E7�3E9NfE:ffE;x E=�E@H EB� EFfEI�3EL�3EO��ER�fEV3EY3E\ E_ Eb{3Ee� Eh�fEk��En�fEq��Et��Ex E{i�E~S3E�ŚE�bfE�� E�}�E��E�3E�!�E��fE�< E�ٚE�u�E���E���E�3E�� E� �E�͚E�>fE��fE�e�E� E�}�E� E��fE�8�E��3E�� E�.fE�� E�� E� E�t�E�� E��E�bfE��fE���E�T E��3E��3E�<�E��fE��3?fff?L��?fff?L��?fff?L��?fff?fff?fff?L��?fff?fff?L��?fff?fff?fff?fff?L��?fff?fff?fff?�  ?�  ?�  ?���?�ff?�33?���?���?�33@   @��@   @,��@@  @S33@l��@�  @���@�ff@�33@�  @�  @���@ٙ�@�ff@�  A   AffAffA��A��A!��A)��A333A9��AA��AH  AP  AY��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141444144144441441441111411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?�33@   @`  @�  @�33@�ff@�ffA33A��A)��AI��Ak33A�  A���A���A�  A�  A���A���A���BffB
ffBffB  B"ffB*ffB2ffB:  BBffBJffBQ33BY33Bb  Bj  Br��BzffB�  B�33B�  B�  B�  B���B�  B�33B�ffB�ffB�ffB�ffB�  B���B�  B�ffB�ffB�33B�  B�  B���B�ffB�ffB�33B���B䙚B�  B홚B�ffB�33B�ffB�ffC ��C� C� C��C�3C
� CffC� C� C��CL�CffC�3CffC��C�3C � C"L�C$��C&��C(��C*�3C,��C.� C0�3C2��C4� C6� C8��C:�3C<��C>� C@� CB� CD� CF� CHffCJ�3CL��CN� CPffCRffCT�3CV�3CX� CZ��C\�3C^� C`� CbffCd��CfffChL�Cj� Cl�3Cn� CpffCr��Ct�3Cv� CxL�CzffC|��C~��C�ffC�L�C�33C�@ C�L�C�L�C�33C�L�C�L�C�&fC�@ C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�ffC�@ C�&fC�33C�33C�L�C�L�C�ffC�ffC�L�C�&fC�33C�@ C�L�C�ffC�@ C�33C�@ C�Y�C�ffC�s3C�ffC�L�C�Y�C�s3C�ffC�@ C�Y�C�L�C�&fC�@ C�L�C�Y�C�ffC�Y�C�33C�L�C�ffC�Y�C�@ C�Y�C�@ C�&fC�33C�L�C�ffC�L�C�33C�Y�C�@ C�33C�L�C�L�C�ffC�ffC�Y�C�@ C�Y�C�ffC�Y�C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�@ C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�ffC�L�C�@ C�Y�C�s3C�ffC�L�C�@ C�@ C�L�C�L�C�33C�L�C�ffC�Y�C�L�C�ffC�ffC�Y�C�L�C�33C�L�C�ffC�L�C�@ C�Y�C�L�C�33C�Y�C�L�C�33D�3D� DS3D�D�fD��DffD  D�3D�fD@ D �3D#�fD&�D(�3D+9�D-��D0Y�D2�fD5` D7�fD:y�D=3D?�fDBFfDD�fDG��DJ  DL�3DOffDR3DT��DWy�DZ,�D\��D_s3Db3Dd��Dgs3Dj3Dl��DoFfDq�fDty�Dw�Dy�fD{��D~` D�� D��3D�#3D�p D�� D�fD�s3D��3D�  D�|�D��3D�&fD�� D��fD�0 D��3D��3D�0 D��3D��fD�,�D��3D��fD�0 D���D��fD�)�D�vfD���D�  D�FfD��3D��3D�#3D�c3D���D�� D�9�D�|�D���D��fD�6fD�y�D���D�  D�6fD�� D��3D��D�L�D���D��fD�3D�Y�DƖfD�� D�fD�@ Dˀ D̼�D�  D�C3D�|�DѶfD��fD�#3D�c3D֠ D��3D��D�I�Dۀ DܶfD��fD��D�P DቚD��D��fD� D�6fD�Y�D�fD� D���D�  D�&fD�I�D�ffD���D� D��fD�  D�&fD�C3D�i�D��3D�� D��3D�3D�33D�\�D���E [3E �fE��E�E��ED�E��Eh E��E�3E�E�3EFfE�fEp E	�E	�fE
fE
�fE<�EY�El�E{3E3E�E��E� E#3E)�E� E��E�E�3Ek3E� E� E ` E!` E"��E#�E%p E&x E'��E)�E*3E+� E,��E-�3E/H E0T�E1� E2�3E4�E5� E6� E7�3E9VfE:nfE;� E=�E@P EB� EFfEI�3EL�3EOɚER�fEV3EY3E\ E_ Eb�3Ee� Eh�fEkɚEn�fEq��Eu�Ex E{q�E~[3E�ɚE�ffE�� E���E��E��3E�%�E��fE�@ E�ݚE�y�E���E���E�3E�� E�$�E�њE�BfE��fE�i�E� E���E� E��fE�<�E��3E�� E�2fE�� E�� E�  E�x�E�� E��E�ffE��fE���E�X E��3E��3E�@�E��fE��3G�O�?�ffG�O�?�ffG�O�?�ffG�O�G�O�G�O�?�ffG�O�G�O�?�ffG�O�G�O�G�O�G�O�?�ffG�O�G�O�?�33G�O�G�O�?�  ?ٙ�?�ff?�33G�O�@ff@��@   @,��@@  @L��@`  @s33@�ff@�  @���@�ff@�33@�  @�  @���@陚@�ffA   A  AffAffA��A!��A)��A1��A;33AA��AI��AP  AX  Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141444144144441441441111411111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ %@ �@ {@ O@ "�@ *S@ 1'@ 7�@ >@ D�@ R�@ `�@ m:@ {�@ �7@ �0@ ��@ �-@ ��@ �|@ �#@ ��@ �q@j@�@g@-@:@H]@V@a�@oF@~K@��@��@��@��@�>@�7@��@�@�~@�@*@#�@1'@>�@Lu@X�@e�@t@�@��@��@��@�R@�J@��@�@�@��@�@�@'�@4�@A�@O�@]�@j@ww@�@��@��@�@�@�c@�
@�`@��@�Q@@�@)�@7�@DD@P�@`B@oF@{�@��@�<@��@��@�2@��@�t@��@� @v@@�@,`@:@G�@T�@dZ@qS@~K@�D@��@��@�F@@�C@�;@�@�,@%@*@!s@.l@=q@Lu@X�@e�@t�@�@�\@��@��@�@ƨ@խ@��@�@@��@
�@�@$�@3�@A�@M$@\)@k.@y�@��@�h@��@��@�@�o@�
@�T@�@�Q@V@�@+@8�@D�@P�@^�@m:@{�@��@�0@�(@�~@��@��@�/@�(@�q@	�@	�@	 �@	,`@	;d@	H]@	S�@	b�@	qS@	�@	��@	��@	��@	��@	Ĝ@	є@	��@	��@	�,@
v@
�@
"�@
1�@
>@
Ji@
Z@
ff@
s_@
�d@
�@
�@
��@
��@
��@
��@
�T@
�L@
��@
�@B@%�@1�@@�@O0@\)@i!@ww@�|@��@�@�@�@�o@׹@�@�@�@�@�@(�@6�@D�@R�@^�@m�@|�@��@��@��@��@��@�|@��@��@��@@@ @-@9X@I@V@bN@��@:@�+@��@g@k.@��@  @M$@��@��@$.@m�@�9@�9@@,@�|@�@�@UU@�H@��@'�@m�@��@�E@E�@��@�C@�@e	@�f@�~@B8@��@�C@�@bN@��@�e@;d@�d@�@b@X@��@�h@�@ff@��@�q@=q@�@�*@�@c�@��@�@>@�|@�7@B@b�@�Y@�@ =q@ ��@ �*@!6@!`A@!�@!�Y@"<@"�@"�o@#o@#X@#�@#�@$)�@$m�@$��@$��@%?}@%�@%�o@&b@&S�@&��@&��@' �@'e�@'��@'��@(33@(x&@(�w@)�@)F�@)��@)��@*�@*Z@*�@*�;@+""@+ff@+��@+��@,3�@,v�@,��@,��@->@-�d@-��@.
�@.M�@.��@.�7@/o@/SI@/��@/�
@0�@0[z@0��@0�t@1B@1Wb@1��@1�
@26@2UU@2�#@2�C@3@3M$@3��@3��@4
=@4I@4��@4Ĝ@5@5DD@5~�@5��@6 �@6@,@6�W@6��@6�Q@7?}@7~�@7��@7��@8<@8z�@8�@8� @95@@9uk@9�9@9�@:33@:qS@:�f@:��@;(G@;e	@;ލ@<S�@<�W@=uk@=�@>��@>�,@?�a@@V@@�@A�@A��@B�@B��@CX�@C��@Di�@D�
@E|�@E�4@F��@Gj@Gv@H[@H�P@I6�@I�M@J�@J��@K8�@K�@LV�@L�@Mww@M�@N`�@Ob@O��@O��@P�M@R
=@S,`@T�p@V�@WE�@X��@Y��@[O�@\�a@]�@_(�@`��@a�Y@cC�@d��@e��@g<@h�+@iӠ@kD�@l�@m�@oB�@p�0@q�y@s4�@tz2@u�<@wB�@x�@y�@{E�@|{�@}�@<�@�=q@��s@��g@�<�@��!@��*@�Ic@��@��@�F�@���@�#@�.@�T�@�z�@��{@��@���@��,@�@�EJ@�^�@���@��`@�Ţ@��.@��@�49@�LuG�O�@ vG�O�@ vG�O�@ vG�O�G�O�G�O�@ vG�O�G�O�@ vG�O�G�O�G�O�G�O�@ vG�O�G�O�@ %G�O�G�O�@ �@ 1@ �@ 	�G�O�@ 
�@ �@ �@ @ @ o@ {@ �@ B@ O@ 
@  @ "�@ %�@ (�@ +�@ .l@ 1'@ 33@ 6�@ 9X@ <�@ ?}@ A�@ D�@ H]@ Lu@ O0@ R�@ UU@ X�@ \�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aز-AخAجAخAجAجAا�Aإ�Aؗ�Aؗ�Aؕ�Aؗ�A؛�A؛�Aؕ�A�~�A�x�A�x�A�x�A�x�A�v�A�t�A�v�A�v�A�v�A�x�A�z�A؁A؉7A�t�A�=qA�A�A��`A�%A�C�A�A�A�7LA�/A���A�n�A��A��FA�-A�VA�l�A���A��hA�l�A��A���A�l�A��PA���A��A��A�Q�A��;A���A��`A�~�A�K�A���A�+A�ȴA��A��A�dZA�+A�VA�dZA���A{�TAt�`Ap{Ak�
AfȴA_�AVA�AS��APv�AL  AGp�AC��ABbNA@ȴA?��A=VA:�HA9|�A8r�A7�
A7x�A6��A6��A6bNA6(�A5��A5�-A5�A5dZA533A5A4�yA4�9A4~�A4~�A4�+A4v�A49XA4A3�#A3�FA3��A3XA333A3+A2��A2��A2v�A2ZA2A�A2$�A1��A1�TA1�;A1�^A1��A1l�A0�`A/ƨA-��A,^5A*��A*^5A)�wA(A�A'|�A'�PA'\)A't�A'�
A'��A&�jA&1A%&�A$I�A#33A"1'A!�wA ��AoA��A��A-AS�A�A��AM�A�A��A�\A�A~�A5?A+AA�+AJA�A$�A~�AVAE�A�DAv�AM�A�A�wAx�AAĜAoA�+A�\A�!AK�A�A �AA��An�A$�A�A��A�AhsA�A~�AZA{AK�AVA��A�A�Ar�A^5A�!A�A1'A�A��AG�A��A�RAVA��A�At�A�A
��A
��A
(�A	�wA	?}A�jAz�A��A�yA1A�A�A"�A�HAffA��A/Av�A1'AJA�A �A ��A -@���@�C�@��@��/@�1'@�dZ@��7@�1'@���@�"�@��H@��\@�X@�%@�b@��@ᙚ@�5?@��@؛�@��@�l�@��/@�S�@���@ȣ�@�v�@�ƨ@��@�G�@��!@�?}@�  @��+@�1@�v�@��`@��\@��/@�|�@��`@�M�@��u@��-@���@��y@��7@���@�@��@��9@�(�@��@�G�@�9X@���@��T@�bN@��!@�?}@��@��@�hs@�r�@���@�@��u@���@�V@���@��@}�@{dZ@x��@wl�@tz�@r��@p�9@n�+@mV@jn�@h��@g\)@f@dI�@b��@`�`@_�@]�@[t�@Y�^@X  @VE�@T1@R�@QX@PA�@M�@L��@K@J�@H��@G�@F5?@E/@Ct�@B�H@@��@@Q�@>�R@=��@<9X@:��@:=q@9�@7�w@7;d@5��@41@2J@0�@/|�@.��@-�@,I�@*�!@)��@(r�@'�;@&�y@&5?@%�@$z�@#�m@"^5@!X@ �@��@5?@?}@Z@�@M�@hs@��@1'@�@��@5?@`B@�@�
@t�@M�@X@  @K�@v�@�-@�j@1@dZ@
�\@
��@
�!@	G�@Ĝ@ �@�@\)@��@$�@`B@�D@�F@dZ@@^5@�^@G�@ ��@ r�?���?�V?�/?�dZ?��?�b?�ȴ?�`B?�S�?�n�?�bN?�|�?��?��?�?�^5?�7L?�Q�?�P?�+?�?�9X?���?�S�?�J?�hs?�Ĝ?�  ?�|�?ޗ�?�{?�/?�j?�dZ?�^5?ٺ^?���?�b?�K�?�ff?��?ӕ�?ҏ\?�hs?�  ?θR?Ͳ-?�j?�dZ?���?�r�?��y?��/?�33?���?� �?��?��h?���?�I�?�?�^5?�7L?�b?�K�?�ff?�$�?��?��?���?�z�?���?�33?��\?�-?�M�?�J?��?�M�?��!?�o?��F?�z�?��/?��?�ff?��y?�
=?�+?�+?��P?�l�?��?��?�b?�1'?�r�?�Q�?��u?��9?���?���?��?�7L?�XAز-Aش9Aش9AضFAش9Aش9AظRAش9Aذ!AضFAز-Aش9Aز-Aش9Aش9Aش9AضFAضFAش9Aز-Aذ!AخAخAخAذ!AخAجAخAذ!Aذ!AجAخAخAخAا�AخAخAخAخAجAجAجAجAة�Aا�Aة�Aا�Aإ�Aإ�Aؗ�Aؕ�Aؗ�Aؙ�Aؗ�AؓuAؕ�Aؗ�Aؗ�Aؗ�Aؗ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Aز-AخAجAخAجAجAا�Aإ�Aؗ�Aؗ�Aؕ�Aؗ�A؛�A؛�Aؕ�A�~�A�x�A�x�A�x�A�x�A�v�A�t�A�v�A�v�A�v�A�x�A�z�A؁A؉7A�t�A�=qA�A�A��`A�%A�C�A�A�A�7LA�/A���A�n�A��A��FA�-A�VA�l�A���A��hA�l�A��A���A�l�A��PA���A��A��A�Q�A��;A���A��`A�~�A�K�A���A�+A�ȴA��A��A�dZA�+A�VA�dZA���A{�TAt�`Ap{Ak�
AfȴA_�AVA�AS��APv�AL  AGp�AC��ABbNA@ȴA?��A=VA:�HA9|�A8r�A7�
A7x�A6��A6��A6bNA6(�A5��A5�-A5�A5dZA533A5A4�yA4�9A4~�A4~�A4�+A4v�A49XA4A3�#A3�FA3��A3XA333A3+A2��A2��A2v�A2ZA2A�A2$�A1��A1�TA1�;A1�^A1��A1l�A0�`A/ƨA-��A,^5A*��A*^5A)�wA(A�A'|�A'�PA'\)A't�A'�
A'��A&�jA&1A%&�A$I�A#33A"1'A!�wA ��AoA��A��A-AS�A�A��AM�A�A��A�\A�A~�A5?A+AA�+AJA�A$�A~�AVAE�A�DAv�AM�A�A�wAx�AAĜAoA�+A�\A�!AK�A�A �AA��An�A$�A�A��A�AhsA�A~�AZA{AK�AVA��A�A�Ar�A^5A�!A�A1'A�A��AG�A��A�RAVA��A�At�A�A
��A
��A
(�A	�wA	?}A�jAz�A��A�yA1A�A�A"�A�HAffA��A/Av�A1'AJA�A �A ��A -@���@�C�@��@��/@�1'@�dZ@��7@�1'@���@�"�@��H@��\@�X@�%@�b@��@ᙚ@�5?@��@؛�@��@�l�@��/@�S�@���@ȣ�@�v�@�ƨ@��@�G�@��!@�?}@�  @��+@�1@�v�@��`@��\@��/@�|�@��`@�M�@��u@��-@���@��y@��7@���@�@��@��9@�(�@��@�G�@�9X@���@��T@�bN@��!@�?}@��@��@�hs@�r�@���@�@��u@���@�V@���@��@}�@{dZ@x��@wl�@tz�@r��@p�9@n�+@mV@jn�@h��@g\)@f@dI�@b��@`�`@_�@]�@[t�@Y�^@X  @VE�@T1@R�@QX@PA�@M�@L��@K@J�@H��@G�@F5?@E/@Ct�@B�H@@��@@Q�@>�R@=��@<9X@:��@:=q@9�@7�w@7;d@5��@41@2J@0�@/|�@.��@-�@,I�@*�!@)��@(r�@'�;@&�y@&5?@%�@$z�@#�m@"^5@!X@ �@��@5?@?}@Z@�@M�@hs@��@1'@�@��@5?@`B@�@�
@t�@M�@X@  @K�@v�@�-@�j@1@dZ@
�\@
��@
�!@	G�@Ĝ@ �@�@\)@��@$�@`B@�D@�F@dZ@@^5@�^@G�@ ��@ r�?���?�V?�/?�dZ?��?�b?�ȴ?�`B?�S�?�n�?�bN?�|�?��?��?�?�^5?�7L?�Q�?�P?�+?�?�9X?���?�S�?�J?�hs?�Ĝ?�  ?�|�?ޗ�?�{?�/?�j?�dZ?�^5?ٺ^?���?�b?�K�?�ff?��?ӕ�?ҏ\?�hs?�  ?θR?Ͳ-?�j?�dZ?���?�r�?��y?��/?�33?���?� �?��?��h?���?�I�?�?�^5?�7L?�b?�K�?�ff?�$�?��?��?���?�z�?���?�33?��\?�-?�M�?�J?��?�M�?��!?�o?��F?�z�?��/?��?�ff?��y?�
=?�+?�+?��P?�l�?��?��?�b?�1'?�r�?�Q�?��u?��9?���?���?��?�7L?�XAز-Aش9Aش9AضFAش9Aش9AظRAش9Aذ!AضFAز-Aش9Aز-Aش9Aش9Aش9AضFAضFAش9Aز-Aذ!AخAخAخAذ!AخAجAخAذ!Aذ!AجAخAخAخAا�AخAخAخAخAجAجAجAجAة�Aا�Aة�Aا�Aإ�Aإ�Aؗ�Aؕ�Aؗ�Aؙ�Aؗ�AؓuAؕ�Aؗ�Aؗ�Aؗ�Aؗ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	\B	 �B	%�B	&�B	:^B	dZB	� B
<jB
}�B
��B
�jB
B
B
��B
�HB
��BoB�B%B
�B
�yB
�B
��B
�ZB  B
�5B
��B
�B
��B
�B
�9B
��B
�JB
z�B
ffB
O�B
+B	��B	�/B	ȴB	�#B	��B	�B	VB	&�B	�B	B�B��B��B�uBp�BA�B-B �B�B�B�B�B+B;dBJ�BT�BW
BbNBn�Bu�B}�B�%B�hB��B��B�B�qBǮB�
B�ZB�ZB�fB��B	\B	"�B	6FB	@�B	K�B	N�B	T�B	^5B	dZB	q�B	�VB	��B	��B	��B	�B	�3B	�}B	ŢB	��B	��B	�;B	�)B	�B	��B	��B	��B	��B	��B	��B	�B	�NB	�B
1B
JB
JB
DB
	7B
JB
JB
VB
DB
+B
%B

=B

=B
JB
VB
JB
DB
JB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
2-B
=qB
B�B
F�B
O�B
R�B
W
B
XB
W
B
YB
XB
\)B
e`B
dZB
dZB
gmB
v�B
u�B
l�B
iyB
l�B
k�B
jB
x�B
{�B
y�B
y�B
v�B
t�B
v�B
s�B
n�B
l�B
k�B
hsB
e`B
aHB
dZB
k�B
k�B
k�B
jB
iyB
gmB
ffB
gmB
e`B
dZB
dZB
bNB
bNB
aHB
`BB
]/B
\)B
YB
W
B
T�B
R�B
R�B
N�B
M�B
K�B
I�B
G�B
C�B
B�B
@�B
=qB
>wB
>wB
<jB
:^B
8RB
6FB
5?B
33B
1'B
.B
.B
,B
(�B
)�B
)�B
'�B
(�B
'�B
,B
'�B
�B
{B
uB
hB
VB
JB

=B
bB
VB
VB
PB
	7B
%B
B
1B
DB

=B
DB
JB
DB
PB
VB
\B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
"�B
#�B
%�B
'�B
(�B
+B
-B
/B
0!B
1'B
33B
33B
6FB
6FB
7LB
;dB
:^B
<jB
=qB
?}B
>wB
@�B
A�B
C�B
D�B
D�B
G�B
H�B
I�B
J�B
J�B
K�B
L�B
M�B
O�B
P�B
P�B
R�B
S�B
VB
VB
XB
XB
ZB
ZB
[#B
[#B
\)B
^5B
^5B
^5B
`BB
^5B
`BB
aHB
cTB
bNB
dZB
e`B
dZB
ffB
gmB
gmB
hsB
hsB
jB
k�B
l�B
l�B
l�B
m�B
n�B
p�B
p�B
q�B
r�B
r�B
r�B
t�B
s�B
v�B
u�B
w�B
x�B
y�B
z�B
z�B
{�B
|�B
|�B
}�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�+B
�+B
�+B
�1B
�7B
�JB
�=B
�DB
�DB
�=B
�JB
�JB
�JB
�PB
�VB
�\B
�\B
�bB
�\B
�bB
�hB
�hB
�oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�'B
�'B
�-B
�3B
�3B
�3B
�9B
�?B
�FB
�LB
�LB
�LB
�RB
�XB
�^B
�^B
�dB
�dB
�jB
�wB
�wB
�}B
��B
��B
��B
��B
��B
B
ÖB
ÖB
ĜB
ÖB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ƨB
ƨB
ŢB
ŢB
ŢB
ĜB
ƨB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ŢB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B	DB	 �B	%�B	&�B	:HB	dCB	�B
<TB
}�B
��B
�UB
�zB
�{B
��B
�5B
��B]B�BB
�B
�hB
�B
��B
�JB
��B
�%B
��B
�pB
��B
�B
�+B
��B
�=B
z�B
fYB
O�B
*�B	��B	�#B	ȨB	�B	��B	�B	U�B	&�B	�B	B�B��B��B�kBp�BAB-B �B�BBB�B*�B;]BJ�BT�BWBbIBn�Bu�B}�B�#B�fB��B��B�B�rBǯB�B�\B�]B�jB��B	aB	"�B	6LB	@�B	K�B	N�B	UB	^>B	dcB	q�B	�aB	��B	��B	��B	�B	�AB	��B	űB	��B	�B	�KB	�:B	�.B	��B	�B	�B	��B	��B	�B	�B	�cB	��B
HB
aB
bB
\B
	PB
cB
dB
pB
_B
FB
AB

YB

ZB
hB
tB
iB
cB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
%B
2TB
=�B
B�B
F�B
PB
SB
W4B
X;B
W5B
YCB
X<B
\VB
e�B
d�B
d�B
g�B
v�B
u�B
l�B
i�B
l�B
k�B
j�B
y	B
|B
zB
zB
v�B
t�B
wB
s�B
n�B
l�B
k�B
h�B
e�B
a�B
d�B
k�B
k�B
k�B
j�B
i�B
g�B
f�B
g�B
e�B
d�B
d�B
b�B
b�B
a�B
`�B
]tB
\oB
Y]B
WQB
UEB
S:B
S:B
O"B
NB
LB
JB
G�B
C�B
B�B
@�B
=�B
>�B
>�B
<�B
:�B
8�B
6�B
5�B
3�B
1yB
.gB
.gB
,\B
)JB
*QB
*QB
(FB
)LB
(GB
,eB
(PB
B
�B
�B
�B
�B
�B

�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B

�B
�B
�B
�B
�B
�B
B
)B
 B
5B
>B
GB
XB
aB
jB
mB
vB
yB
�B
 �B
�B
 �B
"�B
$�B
#�B
$�B
&�B
(�B
)�B
+�B
-�B
0B
1B
2B
4.B
41B
7GB
7KB
8TB
<oB
;lB
={B
>�B
@�B
?�B
A�B
B�B
D�B
E�B
E�B
H�B
I�B
J�B
K�B
K�B
MB
NB
OB
Q&B
R/B
R2B
TBB
UKB
WZB
W]B
YlB
YoB
[B
[�B
\�B
\�B
]�B
_�B
_�B
_�B
a�B
_�B
a�B
b�B
d�B
c�B
e�B
f�B
e�B
g�B
iB
iB
jB
jB
l#B
m,B
n5B
n8B
n;B
oDB
pMB
r]B
r_B
siB
tqB
ttB
twB
v�B
u�B
x�B
w�B
y�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
�B
�B
��B
��B
�B
�B
�B
�B
�B
�$B
�'B
�/B
�,B
�5B
�>B
�@B
�CB
�LB
�UB
�kB
�aB
�jB
�mB
�iB
�yB
�|B
�~B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�B
�B
�B
�1B
�<B
�DB
�TB
�[B
�gB
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�+B
�1B
�<B
�DB
�IB
�TB
�cB
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�EB
�ZB
�iB
�xB
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�BB
�WB
�eB
�uB
ƋB
ƚB
ǰB
��B
��B
��B
��B
�	B
�B
�(B
�=B
�LB
�\B
�lB
�zB
̏B
˚B
˩B
̲B
ˮB
̸B
˵B
̾B
��B
˿B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B�B�B�B�B�B�B�~B�B�B�~B�B�~B�B�B�B�~B�~B�B�xB�~B�~B�B�B�~B�~B�~B�~B�B�~B�~B�B�B�~B�~B�B�B�B�B�B�B�B�B�B��B��B��B�zB�B�tB��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203032021061413573220210614135732202107221611332021072216113320210722161133201807242203032021061413573220210614135732202107221611332021072216113320210722161133PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030320180724220303  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030320180724220303QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030320180724220303QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141920210722161419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                