CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-28T07:00:54Z creation      
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
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    LL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  Pl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  e   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  u|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ü   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar            HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � tArgo profile    3.1 1.2 19500101000000  20181028070054  20210617131507  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               *   *DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؉�es�/@؉�es�/11  @؉�`� @؉�`� @6�@�jU@6�@�jU�cץ�q!��cץ�q!�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @�  @���@�  @�  A   A��A$��AA��Aa��A�  A���A���A���A�33A�  A���A���B   B33B  B��B ��B(ffB0ffB7��B?��BG33BO��BW��B_��Bh  BpffBxffB�33B�33B�33B�  B�  B���B�33B�ffB�33B���B���B�33B���B�ffB�ffB�33B�  B�  B�ffB���B�33B�  B�  Bܙ�B�ffB�ffB�33B���B�33B�  B���B�33C 33C�C�fC�C33C
33C�fC33C�C�fC33C�C  C  C�fC33C   C!�fC$33C&  C'�fC*33C,�C-��C0�C2  C4  C633C8L�C:�C<  C=��C@  CB33CD�CE�fCH�CJL�CL33CN  CPL�CRL�CT  CV33CX33CZ  C\L�C^33C`�Cb  Cc�fCf33Ch�Ci��Cl  Cn33Cp33Cr  CtL�Cv�Cx  Cz33C|L�C~33C�  C�&fC��C�  C��C��C��fC��C�&fC��C��3C��C�&fC��C�  C�&fC��C�  C��C��C��3C��C�&fC��C�  C��C��C�  C��C��C��3C��C�  C��fC��C�  C��3C��C�  C��3C��C��C�  C��C��C�  C�&fC��C��3C��C��C��3C��C��C�  C��C�  C��fC��3C��C��C��C��fC��fC��3C�  C�  C�  C��C��C��C��C��C��C�33C�  C�  C�  C��C�33C�&fC�&fC�  C�  C��C��C��C��C��3C��3C��fC��C�33C��C��C��C��C�  C�  C��fC��fC�  C�&fC�&fC��C��C��C��C��C��C��C��C��C��C�&fC��C�  C�ٚC��3C��C��C�&fC��C��3C�  C��C�33C�33C��fD � DfD�3D��D	33D��D@ D�3D  Dy�DٚD9�D�fD  D!S3D#� D&@ D(�fD+L�D-��D0�fD3@ D5�3D8� D;,�D=��D@S3DB� DEs3DG�3DJl�DL��DOy�DRfDTs3DV��DYs3D[�fD^L�D`��Dc,�De��Dh  Dj�fDm  Do� Dq�3DtffDv�fDyffD{s3D}��D�9�D��3D�ɚD��D�P D��3D���D��D�c3D��3D�� D�)�D�l�D��fD���D�9�D�|�D���D��3D�0 D�ffD��fD���D���D�#3D�L�D�y�D��3D���D���D�fD�9�D�\�D�� D�� D�� D�ٚD��fD�fD�<�D�ffD���D��3D��fD���D��D�<�D�c3D���D���D�ɚD��D� D�0 D�VfD�� D¦fD�ɚD���D��D�0 D�Y�DɆfDʦfD���D��3D�  D�FfD�s3Dѓ3DҼ�D�� D�	�D�0 D�Y�D؆fDٳ3D��fD��fD��D�<�D�Y�D�y�D� D� D��3D��D���D�3D�33D�L�D�c3D�y�D� D�� D��D��D�ɚD�� D�� D�� D�	�D��D�0 D�<�D�P D�c3D�` D�s3D���D���D��fE \�E � Eh E�fEvfE�fE��E	�E��E�E�fE� EI�E	[3E
l�E�E�E�fE�fE��E>fEA�E�fE��E.fE#3E�fE E Et�E` E��E!�E"t�E#` E$� E&!�E'�3E(p E)ٚE+<�E,0 E-�3E/fE/��E1Y�E2�fE3��E53E6p E7\�E8�fE:)�E;�fE<h E?� EBٚEFfEH�3EL3EO�ER��EU��EX� E[�E^�Eb3Ee0 Eh3Ekd�Enc3Eq� Et��Ew��Ez�fE~�E�� E�$ E��3E�Q�E��fE�q�E�� E���E� E�t E���E��3E�VfE��3E�� E�:fE���E�� E�A�E���E��3E��E�w3E��3E��>���>���>���>���>���>���>���>���>���>���>���?   >���>���>���?   ?��?   ?��?��?333?L��?L��?L��?�  ?���?���?�33?�  ?ٙ�?�ff@��@33@&ff@,��@L��@Y��@fff@�33@���@�33@���@�ff@�33@�  @ə�@�ff@�ff@�33@���AffAffA��A��A$��A,��A4��A<��AD��AL��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414414411141411441111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?fff?�33@   @`  @�  @���@�  @�  A  A��A,��AI��Ai��A�  A���A���A���A�33A�  A���A���B  B	33B  B��B"��B*ffB2ffB9��BA��BI33BQ��BY��Ba��Bj  BrffBzffB�33B�33B�33B�  B�  B���B�33B�ffB�33B���B���B�33B���B�ffB�ffB�33B�  B�  B�ffB���B�33B�  B�  Bݙ�B�ffB�ffB�33B���B�33B�  B���B�33C �3C��CffC��C�3C
�3CffC�3C��CffC�3C��C� C� CffC�3C � C"ffC$�3C&� C(ffC*�3C,��C.L�C0��C2� C4� C6�3C8��C:��C<� C>L�C@� CB�3CD��CFffCH��CJ��CL�3CN� CP��CR��CT� CV�3CX�3CZ� C\��C^�3C`��Cb� CdffCf�3Ch��CjL�Cl� Cn�3Cp�3Cr� Ct��Cv��Cx� Cz�3C|��C~�3C�@ C�ffC�Y�C�@ C�Y�C�L�C�&fC�L�C�ffC�Y�C�33C�L�C�ffC�Y�C�@ C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�ffC�L�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�@ C�&fC�L�C�@ C�33C�Y�C�@ C�33C�Y�C�Y�C�@ C�Y�C�L�C�@ C�ffC�Y�C�33C�Y�C�L�C�33C�Y�C�Y�C�@ C�Y�C�@ C�&fC�33C�L�C�Y�C�L�C�&fC�&fC�33C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�s3C�@ C�@ C�@ C�L�C�s3C�ffC�ffC�@ C�@ C�Y�C�Y�C�L�C�L�C�33C�33C�&fC�L�C�s3C�Y�C�Y�C�Y�C�L�C�@ C�@ C�&fC�&fC�@ C�ffC�ffC�Y�C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�Y�C�@ C��C�33C�L�C�Y�C�ffC�L�C�33C�@ C�Y�C�s3C�s3D 3D � D&fD�3DٚD	S3DٚD` D�3D@ D��D��DY�D�fD  D!s3D#� D&` D(�fD+l�D.�D0�fD3` D63D8� D;L�D=��D@s3DC  DE�3DH3DJ��DM�DO��DR&fDT�3DW�DY�3D\fD^l�D`ٚDcL�De��Dh@ Dj�fDm  Do� Dr3Dt�fDwfDy�fD{�3D~�D�I�D��3D�ٚD��D�` D��3D���D�)�D�s3D��3D�� D�9�D�|�D��fD��D�I�D���D�ɚD�3D�@ D�vfD��fD���D���D�33D�\�D���D��3D���D���D�&fD�I�D�l�D�� D�� D�� D��D�fD�&fD�L�D�vfD���D��3D��fD�	�D�)�D�L�D�s3D���D���D�ٚD���D�  D�@ D�ffD�� D¶fD�ٚD���D��D�@ D�i�DɖfDʶfD���D�3D�0 D�VfDЃ3Dѣ3D���D�� D��D�@ D�i�DؖfD��3D��fD�fD�)�D�L�D�i�D���D� D�� D��3D���D��D�#3D�C3D�\�D�s3D뉚D� D�� D��D���D�ٚD�� D�� D�  D��D�,�D�@ D�L�D�` D�s3D�p D��3D���D���D��fE d�E � Ep E�fE~fEfE��E�E��E$�E�fE� EQ�E	c3E
t�E	�E�E�fE�fE��EFfEI�E�fE��E6fE+3E�fE E E|�Eh E��E!!�E"|�E#h E$� E&)�E'�3E(x E)�E+D�E,8 E-�3E/fE/��E1a�E2�fE3��E53E6x E7d�E8�fE:1�E;�fE<p E?� EB�EF&fEH�3EL3EO!�ER��EU��EX� E[�E^�Eb#3Ee8 Eh#3Ekl�Enk3Eq� Et��Ew��Ez�fE~�E�� E�( E��3E�U�E��fE�u�E�� E���E� E�x E���E��3E�ZfE��3E�� E�>fE���E�� E�E�E���E��3E� �E�{3E��3E��?L��G�O�?L��G�O�G�O�?L��G�O�?L��G�O�G�O�?L��G�O�G�O�?L��?fff?�  G�O�?�  G�O�?���?���G�O�G�O�?�ff?�  ?���?ٙ�?�33@   @��@33@,��@333@Fff@L��@l��@y��@�33@�33@���@�33@���@�ff@�33@�  @ٙ�@�ff@�ffA��AffAffAffA��A$��A,��A4��A<��AD��AL��AT��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414414411141411441111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ �@ %@ �@ {@ O@ !s@ (�@ /�@ 6�@ >@ FQ@ R�@ `B@ m:@ {�@ ��@ ��@ �(@ �~@ ��@ �|@ �t@ �@ ��@�@o@g@-@9X@F�@S�@bN@o�@}�@��@�H@��@��@�>@��@��@�@�~@�@�@"�@/@<�@K�@Z�@g�@uk@�d@�\@�@��@��@ƨ@Ӡ@�H@��@��@�@�@$�@3�@@�@M$@\�@k.@x&@�p@�u@��@�r@�@��@׹@�@�@ �@�@O@(G@7�@DD@Q=@`�@m:@z3@��@��@�z@�-@�&@��@��@�(@�q@j@�@�@-�@:�@F�@V@e	@r@~K@��@��@�A@�F@��@�7@��@��@��@�@�@#�@0x@<@K@Z@g�@t@��@�@�@�@�^@�W@Ӡ@�T@�L@��@�@�@$.@3�@B�@O�@[z@j@y�@�|@��@�y@�r@��@��@׹@�@�@@V@O@*S@7L@DD@SI@`B@l�@{�@��@��@��@�~@�w@�*@�t@�m@� @	�@	@	 @	-@	:@	I�@	V�@	bN@	r@	~�@	�D@	��@	��@	��@	��@	�7@	܀@	��@	��@
1@
*@
 �@
.l@
<�@
K@
X�@
ff@
t�@
�d@
�@
��@
�@
��@
ȴ@
Ӡ@
�H@
��@
�E@�@�@'�@33@@�@O�@]�@j@x&@�p@�@�@��@�w@��@�h@�@�@  @�@�@'�@6�@FQ@S�@`�@m�@{�@�7@��@��@��@��@�*@��@�(@� @j@@
@-@;d@I�@V@bN@p�@�@��@�U@��@��@�>@�C@^�@�y@�m@,`@oF@�~@�@2�@s_@��@��@5?@ww@��@ �@E�@�P@�O@�@hs@�~@� @>�@��@�c@�@S�@��@�/@!s@g@�M@�@@1�@t�@�F@�~@;d@�@@@G�@��@��@�@V@�H@�C@6@Z�@�@�@+�@p�@��@��@?}@��@�@�@S�@��@�;@$�@hs@�f@��@ 3�@ ww@ ��@ ��@!9X@!z3@!�j@!��@"<@"{�@"�@"�~@#7�@#v@#�9@#�Y@$/�@$m:@$�M@$�@%#�@%bN@%��@%��@&g@&]�@&��@&�@'6@'V@'��@'��@(�@(M$@(��@(�c@)1@)G�@)�|@)Ĝ@*�@*@,@*~K@*��@*��@+;d@+z3@+�@+�,@,7�@,x&@,��@,�@-33@-r�@-�~@-��@.1'@.qS@.�r@.��@/+@/i!@/��@/�T@0�@0\)@0�H@0խ@1b@1K�@1�7@1�J@2 �@2<@2ww@2�~@2��@3$�@3^5@3�0@3�7@4
=@4FQ@4�@4��@4�@5/�@5j@5�m@5�#@66@6Q=@6��@6Ĝ@6��@76�@7o�@7��@7�@8[@8V@8�@8�o@9%@9z�@:&;@:��@;�@;�k@<0x@<��@=M�@=��@>e�@>�O@?v�@?�T@@�@@�y@A�C@B(�@B��@C.l@C��@D'�@D�k@EP�@E��@FK@F��@Gx�@G��@Hx&@I�@Iww@Jo@J�f@K@K�Z@L@,@L��@M>�@M�
@N<@N�[@Om�@P�@Pb�@Q@S"�@T��@U�F@W�@X\�@Y׹@[�@\��@]�O@_�@`x�@a�c@c1@doF@e�F@g%�@hk�@i�o@k�@le	@m�R@o�@p^�@q��@sZ@tn�@u��@w
�@x_�@x��@x��@y$�@yv@y�f@z  @z8�@z�P@z��@{C@{O�@{��@{�O@|!s@|T�@|��@ G�O�@ G�O�G�O�@ G�O�@ G�O�G�O�@ G�O�G�O�@ @ �@ jG�O�@ jG�O�@ @ �G�O�G�O�@ v@ �@ �@ 1@ 	�@ 
=@ �@ J@ @ �@ �@ o@ �@ 6@ �@ �@ [@ g@ !s@ #�@ &;@ (�@ +@ -�@ 1'@ 3�@ 5�@ 9X@ <�@ ?}@ B�@ FQ@ I�@ M$@ P�@ S�@ WbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�l�AӁAӃAӃAӁAӁAӁAӃAӅAӃAӃAӃAӅAӅA�hsA�5?A�+A�/A�;dA�7LA�5?A�5?A�1'A�+A�&�A�"�A��A��A��A�{A�bA�A��A҉7A�33A�&�A��A�K�A�Q�A��uA���A�=qA��#A�|�A�/A���A�r�A���A�$�A�ȴA��A�9XA�JA��mA�ĜA�S�A�%A���A�x�A�ȴA�A��7A���A�7LA�+A�7LA�|�A��mA�t�A��#A���A�XA���A��A�ȴA��A�(�A�ZA���A�33A��A�A�A��mA���A�1'A�VA��DA�%A�ȴA�l�A���A���A��HA�r�A��yA�M�A�G�A���A�v�A�hsA��+A��jA��A���A��TA�n�A�Q�A��uA�I�A��/A�=qA���A�p�A��wA�E�A���A�jA��HA��;A��7A��A�  A��yA��jA�bNA�
A{��Az{Ax5?Av�+At��Aqp�Am��Am|�Al��AkO�AjbNAihsAh1'Af�+AfbNAfI�AfA�Ae��Ad�Ac�AaS�A`�A_�FA^�uA\�\A[��A[S�AZjAYƨAYC�AW
=AU�-AS7LAM��AL��AL1AJA�AIO�AF�yAC�
ABbAAp�AA�A@�RA?��A;�hA:$�A9�7A7��A5�A5+A4�9A4bNA45?A3?}A2bA1`BA0bNA/�wA.��A.5?A,��A+�A*�`A*�A*z�A)��A'�-A%�A$bA#K�A!�AAp�A��AffAM�A�A��AC�AA�AC�A$�A��A��AO�AƨA�A��A�RAM�A��AjA�wA
�`A
�DA	�A	"�A�!A~�A(�At�A��A(�A�wA��AG�A�^A7LA �9A A�A �@�ff@���@�E�@�?}@� �@�ȴ@���@�bN@�w@�|�@�V@��@��D@�~�@��@�@��y@�`B@��;@�33@���@柾@���@�K�@��@�33@�n�@ͩ�@�X@ɲ-@ŉ7@��@���@�v�@��\@�1@��@�~�@�n�@�V@���@�$�@��+@��@��h@�Ĝ@��w@��@�b@�X@�  @�&�@�S�@�M�@�^5@���@��j@�b@��u@��@���@�x�@��`@�1'@���@�$�@��j@��
@�V@��@�/@�(�@|�@zn�@xr�@t�@s@p��@n@mO�@lI�@k�F@i�7@g��@f@e?}@c��@b��@bM�@a&�@`A�@^�+@\�/@[�@Y��@Yx�@XQ�@V$�@U�@T�j@S@Qx�@O��@NV@MO�@L��@K�m@J��@HĜ@H �@G|�@F��@E?}@DI�@BM�@@��@?|�@>E�@<��@;�F@:�H@9�#@9�@8b@7+@5�@3ƨ@2�!@1x�@0��@0  @/
=@-��@,�j@,j@*n�@)hs@(Q�@'�P@'K�@%�-@$�D@$1@#��@"�@!�@ r�@ b@+@�+@@z�@��@33@�^@r�@�y@{@/@�@Z@�
@o@�#@x�@�@r�@�;@��@{@�@/@Z@ƨ@�@o@
�!@
^5@	�@	x�@��@��@�@ȴ@{@�h@�@(�@ƨ@dZ@"�@�\@��@ Ĝ@  �?���?�{?�I�?�^5?���?�$�?�9X?��?�&�?��?�O�?�j?�?�^?�Q�?��y?�?}?��?���?�&�?��?޸R?�1?ڟ�?��?�+?�?�9X?�33?�-?�G�?Ѓ?�|�?�;d?�5??��?�5??�O�?�j?��m?�?ʟ�?�=q?��?�1'?���?�$�?��/?���?��?���?���?�A�?���?��?�O�?���?�1?�C�?���?��#?�7L?��?��?�x�?���?���?�^5?��H?�"�?�ƨ?�j?�V?��h?�V?���?��?�;d?�;d?�;d?�\)?�|�?�|�?���?��w?��;?� �?�A�?�bN?�bN?���?��`A�bNA�bNA�bNA�`BA�bNA�\)A�`BA�ZA�XA�\)A�\)A�\)A�ZA�dZA�hsA�n�A�ffA�hsA�^5A�`BA�^5A�dZA�ffA�n�A�p�A�r�A�|�A�|�AӁA�~�AӁAӃAӁAӁAӃAӁAӃAӃAӁAӃAӁAӁA�~�A�~�AӃAӁAӁAӁAӃAӃAӃAӅAӅAӃAӃAӃAӃAӃAӃAӃG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     A�`BA�l�AӁAӃAӃAӁAӁAӁAӃAӅAӃAӃAӃAӅAӅA�hsA�5?A�+A�/A�;dA�7LA�5?A�5?A�1'A�+A�&�A�"�A��A��A��A�{A�bA�A��A҉7A�33A�&�A��A�K�A�Q�A��uA���A�=qA��#A�|�A�/A���A�r�A���A�$�A�ȴA��A�9XA�JA��mA�ĜA�S�A�%A���A�x�A�ȴA�A��7A���A�7LA�+A�7LA�|�A��mA�t�A��#A���A�XA���A��A�ȴA��A�(�A�ZA���A�33A��A�A�A��mA���A�1'A�VA��DA�%A�ȴA�l�A���A���A��HA�r�A��yA�M�A�G�A���A�v�A�hsA��+A��jA��A���A��TA�n�A�Q�A��uA�I�A��/A�=qA���A�p�A��wA�E�A���A�jA��HA��;A��7A��A�  A��yA��jA�bNA�
A{��Az{Ax5?Av�+At��Aqp�Am��Am|�Al��AkO�AjbNAihsAh1'Af�+AfbNAfI�AfA�Ae��Ad�Ac�AaS�A`�A_�FA^�uA\�\A[��A[S�AZjAYƨAYC�AW
=AU�-AS7LAM��AL��AL1AJA�AIO�AF�yAC�
ABbAAp�AA�A@�RA?��A;�hA:$�A9�7A7��A5�A5+A4�9A4bNA45?A3?}A2bA1`BA0bNA/�wA.��A.5?A,��A+�A*�`A*�A*z�A)��A'�-A%�A$bA#K�A!�AAp�A��AffAM�A�A��AC�AA�AC�A$�A��A��AO�AƨA�A��A�RAM�A��AjA�wA
�`A
�DA	�A	"�A�!A~�A(�At�A��A(�A�wA��AG�A�^A7LA �9A A�A �@�ff@���@�E�@�?}@� �@�ȴ@���@�bN@�w@�|�@�V@��@��D@�~�@��@�@��y@�`B@��;@�33@���@柾@���@�K�@��@�33@�n�@ͩ�@�X@ɲ-@ŉ7@��@���@�v�@��\@�1@��@�~�@�n�@�V@���@�$�@��+@��@��h@�Ĝ@��w@��@�b@�X@�  @�&�@�S�@�M�@�^5@���@��j@�b@��u@��@���@�x�@��`@�1'@���@�$�@��j@��
@�V@��@�/@�(�@|�@zn�@xr�@t�@s@p��@n@mO�@lI�@k�F@i�7@g��@f@e?}@c��@b��@bM�@a&�@`A�@^�+@\�/@[�@Y��@Yx�@XQ�@V$�@U�@T�j@S@Qx�@O��@NV@MO�@L��@K�m@J��@HĜ@H �@G|�@F��@E?}@DI�@BM�@@��@?|�@>E�@<��@;�F@:�H@9�#@9�@8b@7+@5�@3ƨ@2�!@1x�@0��@0  @/
=@-��@,�j@,j@*n�@)hs@(Q�@'�P@'K�@%�-@$�D@$1@#��@"�@!�@ r�@ b@+@�+@@z�@��@33@�^@r�@�y@{@/@�@Z@�
@o@�#@x�@�@r�@�;@��@{@�@/@Z@ƨ@�@o@
�!@
^5@	�@	x�@��@��@�@ȴ@{@�h@�@(�@ƨ@dZ@"�@�\@��@ Ĝ@  �?���?�{?�I�?�^5?���?�$�?�9X?��?�&�?��?�O�?�j?�?�^?�Q�?��y?�?}?��?���?�&�?��?޸R?�1?ڟ�?��?�+?�?�9X?�33?�-?�G�?Ѓ?�|�?�;d?�5??��?�5??�O�?�j?��m?�?ʟ�?�=q?��?�1'?���?�$�?��/?���?��?���?���?�A�?���?��?�O�?���?�1?�C�?���?��#?�7L?��?��?�x�?���?���?�^5?��H?�"�?�ƨ?�j?�V?��h?�V?���?��?�;d?�;d?�;d?�\)?�|�?�|�?���?��w?��;?� �?�A�?�bN?�bN?���?��`A�bNA�bNA�bNA�`BA�bNA�\)A�`BA�ZA�XA�\)A�\)A�\)A�ZA�dZA�hsA�n�A�ffA�hsA�^5A�`BA�^5A�dZA�ffA�n�A�p�A�r�A�|�A�|�AӁA�~�AӁAӃAӁAӁAӃAӁAӃAӃAӁAӃAӁAӁA�~�A�~�AӃAӁAӁAӁAӃAӃAӃAӅAӅAӃAӃAӃAӃAӃAӃAӃG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�)BPBN�BZB^5BcTBe`Be`BgmBhsBhsBiyBjBl�Bl�Bl�Bl�Bl�Bl�Bl�BjB_;Bn�B�B�B��B�oB��B��B��B�B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�bB�DB�7B�By�Bw�Bt�Bp�BjBgmBe`BbNB]/BYBVBJ�BH�BF�BA�B:^B49B1'B%�BuB	7BB��B�B�ZB�B��BɺB��B�3B�B�B��B�\B�Bs�BjBW
BR�BN�B?}B1'B-B�BB
�B
�`B
�B
��B
��B
��B
��B
��B
�oB
�\B
�JB
�1B
�B
r�B
bNB
XB
I�B
C�B
49B
�B
PB
+B
B	��B	�B	�yB	�BB	�B	�
B	��B	��B	��B	ɺB	B	�jB	�XB	�?B	�B	��B	��B	��B	��B	�{B	�\B	�B	}�B	e`B	P�B	H�B	@�B	6FB	,B	�B	B��B�B�B�yB�5B��B�jB�LB�B�B��B��B��B��B��B��B��B��B�{B�oB�VB�7B�%B�B�B�Bx�Bp�BffBdZB_;BW
BZBVBW
BT�BS�BQ�BO�BN�BO�BM�BM�BH�BK�BI�BI�BI�BF�BG�BG�BF�BF�BE�BD�BC�BB�B@�B>wB>wB=qB>wB=qB=qB;dB:^B6FB8RB7LB5?B5?B49B49B33B6FB5?B49B49B49B5?B5?B49B49B5?B49B49B7LB7LB8RB8RB:^B:^B:^B:^B:^B33B9XB`BBk�B|�B�7B�hB��B��B��B�dB�qB��B��B�5B�B	  B	\B	VB	VB	�B	�B	(�B	6FB	I�B	R�B	_;B	cTB	o�B	|�B	�7B	��B	��B	��B	�B	�dB	ƨB	��B	��B	��B	�#B	�/B	�TB	�ZB	�`B	�fB	�`B	�sB	�B	�B	��B	��B	��B	��B
  B
B
B
B

=B
PB
JB
VB
hB
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
%�B
(�B
'�B
)�B
,B
-B
/B
/B
0!B
1'B
2-B
33B
5?B
6FB
7LB
8RB
9XB
:^B
<jB
>wB
>wB
>wB
?}B
A�B
A�B
B�B
C�B
D�B
F�B
G�B
J�B
H�B
J�B
L�B
M�B
N�B
O�B
Q�B
Q�B
S�B
T�B
VB
W
B
W
B
YB
ZB
[#B
[#B
]/B
]/B
_;B
^5B
_;B
_;B
aHB
aHB
cTB
cTB
e`B
gmB
hsB
hsB
jB
jB
jB
k�B
k�B
l�B
m�B
m�B
n�B
o�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
t�B
s�B
s�B
t�B
s�B
v�B
v�B
v�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�+B
�1B
�7B
�7B
�DB
�DB
�PB
�PB
�VB
�\B
�bB
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
��B
��B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�RB
�XB
�RB
�RB
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     BͫBͬB̦B̦B̧B̧B̧BͭB̨B̨BͮBͯB̪BͰB�B.BN�BY�B^Bc5BeABeBBgPBhVBhWBi]BjdBlqBlqBlrBlrBlsBltBltBjiB_%Bn�B��B��B��B�ZB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�ZB�<B�0B�By�Bw�Bt�Bp�Bj{BgjBe]BbLB]-BYBVBJ�BH�BF�BA�B:`B4;B1*B%�ByB	;BB��B�B�`B�B��B��B��B�<B�$B�B��B�gB�Bs�Bj�BWBR�BN�B?�B16B-B�BB
��B
�qB
�.B
�
B
��B
��B
�B
��B
��B
�pB
�_B
�GB
�(B
r�B
beB
X'B
I�B
C�B
4RB
�B
iB
EB
 B	��B	�B	�B	�^B	�4B	�'B	�B	�B	��B	��B	¯B	��B	�yB	�`B	�*B	�B	��B	��B	��B	��B	��B	�1B	~B	e�B	QB	H�B	@�B	6nB	,0B	�B	BB��B��B��B�B�`B��B��B�xB�;B�/B�B�B�B�B��B��B��B��B��B��B��B�jB�YB�SB�AB�<ByBp�Bf�Bd�B_sBWBBZUBV=BWDBU8BT3BR'BPBOBPBNBNBH�BLBI�BI�BI�BF�BG�BG�BF�BF�BE�BD�BC�BB�B@�B>�B>�B=�B>�B=�B=�B;�B:�B6�B8�B7�B5�B5�B4�B4�B3�B6�B5�B4�B4�B4�B5�B5�B4�B4�B5�B4�B4�B7�B7�B8�B8�B:�B:�B:�B:�B:�B3�B9�B`�Bk�B}RB��B��B�%B�:B�[B��B��B��B�zB޴B�8B	 �B	�B	�B	�B	B	@B	)�B	6�B	JXB	S�B	_�B	c�B	pGB	}�B	��B	�EB	�mB	��B	��B	�"B	�iB	ΗB	ЦB	��B	��B	��B	�'B	�0B	�9B	�AB	�>B	�TB	�iB	�B	��B	��B	��B	��B
 �B
B
B
B
?B
UB
RB
aB
vB
�B
�B
�B
�B
�B
�B
!�B
#�B
% B
%B
'B
*'B
)$B
+3B
-AB
.JB
0ZB
0]B
1eB
2nB
3wB
4�B
6�B
7�B
8�B
9�B
:�B
;�B
=�B
?�B
?�B
?�B
@�B
B�B
B�B
DB
EB
FB
H#B
I+B
LAB
J7B
LGB
NUB
O^B
PgB
QpB
SB
S�B
U�B
V�B
W�B
X�B
X�B
Z�B
[�B
\�B
\�B
^�B
^�B
`�B
_�B
`�B
`�B
c
B
cB
eB
eB
g-B
i=B
jFB
jHB
lWB
lZB
l\B
meB
mhB
npB
oyB
o|B
p�B
q�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
v�B
u�B
u�B
v�B
u�B
x�B
x�B
x�B
z�B
z�B
{�B
}B
~
B
~B
B
B
�!B
�*B
�,B
�5B
�=B
�@B
�QB
�`B
�kB
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
�
B
�B
�B
�/B
�9B
�FB
�LB
�YB
�dB
�qB
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
�B
�B
�B
�B
�0B
�4B
�PB
�fB
�vB
��B
��B
��B
��B
��B
��B
�B
� B
�6B
�KB
�`B
�vB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�1B
�@B
�NB
�^B
�mB
�qB
�sB
�vB
�zB
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
��BͫBαBαBαBαBͫBαBͫBαBͫBͫBͫBϷBͫB̥B̥B̦BͫBͬBβBͬBͬBͬBͬB̦BβBˠB̦BˠB̦B̦BˠBˠB̦BˠB̦BˠBˠB̧B̧B̧B̧B̧B̧B̧B̧B̧BͮB̨B̨BͮB̨B̨B̨BͮBͯBͯB̩BͯBͯG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810280700542021061413554820210614135548202106171313182021061713131820210617131318201810280700542021061413554820210614135548202106171313182021061713131820210617131318PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018102807005420181028070054  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102807005420181028070054QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102807005420181028070054QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150720210617131507IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                