CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  "   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-16T04:00:31Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       Q4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  bD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       f�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       w�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Ȥ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  ٴ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        \   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181216040031  20210722160156  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               1   1DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؖ)L�U�@ؖ)L�U�11  @ؖ)I���@ؖ)I���@5��lvK@5��lvK�c�~��L��c�~��L�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?L��@   @Fff@�ff@�ff@�33@�  A��A��A)��A@  Aa��A�ffA���A�  A���A���A���A�  A�B ffBffB��B��B ffB(��B/��B7��B@  BH  BP  BX  B_33Bg��Bp  Bw��B�33B�33B�33B�  B�33B�ffB�ffB�ffB�ffB�  B�  B�33B�  B�  B�33B�33B�33B�  BǙ�B�  B�ffB�ffB�ffB�33B���B���B癚B�ffB�33B���B�ffB�ffC   C33C  C��C  C
33C  C��C  C33C�C�fC�CL�C  C�fC �C"L�C$33C&  C(33C*  C+��C.  C0  C2�C3�fC5��C7�fC:�C<33C>  C?��CB�CDL�CF33CH  CJ33CL  CM�fCP�CRL�CT�CU�fCX  CZ33C\  C]�fC`  Cb�Cd  Ce��Ch  Cj33Cl  Cm��Co�fCr  Ct�CvL�Cx  Cy�fC|�C~L�C��C��3C��C�&fC��C��fC�  C��C�&fC��C��fC�  C��C��C��3C��C��C�  C��3C��3C��C��C��C��fC�  C��C��C�  C��fC��3C��C�&fC��C��3C��C��C��C��3C��C��C��3C��C��C�&fC��C��3C�  C��C�  C�ٚC��fC��fC��fC��3C��C��C�&fC��C��fC��3C��3C��3C�  C�  C��C��C��C��C�&fC��C��fC�ٚC��fC��3C��3C��3C��3C��3C�  C��C�&fC��C��3C��C��C��C�  C��fC��3C�  C�  C��C��C�&fC��C��3C�  C�  C��C�  C��fC��fC��fC��3C��3C��C�&fC��C��3C�  C�  C��C��C��C��C��fC�  C��C�&fC��C��3C��3C�  C��C�&fC�&fC�&fC�&fD 3DFfD��D
Y�D� Ds3D3D�fDFfD�fD� D  D!�3D$S3D&�fD)ffD+�fD.� D1�D3� D69�D8��D;s3D>�D@��DCFfDE�3DHffDJ�fDMS3DO��DRFfDT�fDW@ DY� D\FfD^��Da33Dc��Df  Dh�fDk�Dms3Do�3DrffDt�fDwffDy� D{��D~l�D�l�D�� D�� D�&fD�` D��fD�� D�fD�FfD�y�D�� D���D�  D�P D��3D��3D�� D�fD�0 D�P D�p D�� D���D��3D�3D�#3D�@ D�ffD�� D��3D��3D��D�,�D�VfD�|�D���D��fD��3D��D�I�D�p D���D�� D���D��D�I�D�vfD���D��3D���D� D�33D�VfD�|�D��fD���D���D��fD�3D�0 D�I�D�i�DȆfDɦfD��3D�� D���D� D�,�D�I�D�` D�|�Dә�D԰ D��fD���D�  D�fD�0 D�P D�p D݌�Dެ�D��fD�ٚD���D�3D�&fD�9�D�P D�ffD� D��D�fD�ɚD��3D�� D�	�D�fD�&fD�6fD�FfD�L�D�L�D�S3D�i�D��3D���D���D��fD�ٚD�� D�	�E 3E ��E6fE� E^fE�3E��E3E��E4�E\�E��E��E	�E�E�fE�3E��E3E�fE�3E#3EfE��E� E�fEQ�E��E� E�fEL�E � E!�3E"��E$ffE%P E&� E(fE)nfE*Y�E+�fE-  E.3E/� E0�3E1�3E3D�E4��E5�3E6��E8NfE9��E:� E;��E>�EB@ EE.fEHl�EK��EN��EQ��ET�fEW�E[  E^�Eak3EdS3Eg��Ej� Em� Ep� Et�Ew9�Ezd�Ez��E{h E|8 E|�3E}a�E}�fE~�3E<�E�fE�@ E���E��3E�?3E���E���E�3E�~fE�� E� E�c3E��3E��E�bfE���E�fE�M�E�� E��3E�, E�w3E�� E�,�E�t E��fE�3E�`�E��3E�3E�ZfE���E�� E�1�E��3E��3E�73E�y�E��fE�( >���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���?   >���?��?   ?   ?��?��?L��?L��?�  ?���?�33?���?�ff@   @��@&ff@9��@S33@`  @y��@�ff@�  @�  @���@���@ə�@ٙ�@�ff@�33A��A  A��A  A!��A)��A0  A6ffA@  AI��AQ��AY��Ac33Ai��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414141444414141441414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ?L��?�ff@   @fff@�ff@�ff@�33@�  A	��A��A1��AH  Ai��A�ffA���A�  A���A���A���A�  A���BffB
ffB��B��B"ffB*��B1��B9��BB  BJ  BR  BZ  Ba33Bi��Br  By��B�33B�33B�33B�  B�33B�ffB�ffB�ffB�ffB�  B�  B�33B�  B�  B�33B�33B�33B�  Bș�B�  B�ffB�ffB�ffB�33B���B���B虚B�ffB�33B���B�ffB�ffC � C�3C� CL�C� C
�3C� CL�C� C�3C��CffC��C��C� CffC ��C"��C$�3C&� C(�3C*� C,L�C.� C0� C2��C4ffC6L�C8ffC:��C<�3C>� C@L�CB��CD��CF�3CH� CJ�3CL� CNffCP��CR��CT��CVffCX� CZ�3C\� C^ffC`� Cb��Cd� CfL�Ch� Cj�3Cl� CnL�CpffCr� Ct��Cv��Cx� CzffC|��C~��C�L�C�33C�L�C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�@ C�33C�33C�L�C�Y�C�L�C�&fC�@ C�L�C�Y�C�@ C�&fC�33C�L�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C�33C�@ C�Y�C�@ C��C�&fC�&fC�&fC�33C�L�C�Y�C�ffC�L�C�&fC�33C�33C�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�ffC�L�C�&fC��C�&fC�33C�33C�33C�33C�33C�@ C�Y�C�ffC�Y�C�33C�L�C�L�C�Y�C�@ C�&fC�33C�@ C�@ C�L�C�Y�C�ffC�Y�C�33C�@ C�@ C�Y�C�@ C�&fC�&fC�&fC�33C�33C�L�C�ffC�L�C�33C�@ C�@ C�L�C�Y�C�Y�C�L�C�&fC�@ C�L�C�ffC�L�C�33C�33C�@ C�L�C�ffC�ffC�ffC�ffD 33DffD��D
y�D  D�3D33D�fDffDfD� D@ D!�3D$s3D&�fD)�fD,fD.� D1,�D3� D6Y�D8��D;�3D>,�D@��DCffDE�3DH�fDKfDMs3DO��DRffDT�fDW` DY� D\ffD^ٚDaS3DcٚDf@ Dh�fDk,�Dm�3Dp3Dr�fDufDw�fDz  D|�D~��D�|�D�� D�  D�6fD�p D��fD�� D�fD�VfD���D�� D���D�0 D�` D��3D��3D�� D�fD�@ D�` D�� D�� D�ɚD��3D�3D�33D�P D�vfD�� D��3D��3D��D�<�D�ffD���D���D��fD�3D�,�D�Y�D�� D���D�� D���D�)�D�Y�D��fD���D��3D���D�  D�C3D�ffD���D��fD���D���D�fD�#3D�@ D�Y�D�y�DȖfDɶfD��3D�� D�	�D�  D�<�D�Y�D�p DҌ�Dө�D�� D��fD���D� D�&fD�@ D�` D܀ Dݜ�D޼�D��fD��D�	�D�#3D�6fD�I�D�` D�vfD� D��D��fD�ٚD��3D�  D��D�&fD�6fD�FfD�VfD�\�D�\�D�c3D�y�D��3D���D���D��fD��D�  D��E 3E ��E>fE� EffE�3E��E3E��E<�Ed�E��E��E	�E�E�fE�3E��E3E�fE�3E+3E&fE��E� E�fEY�E��E� EfET�E � E!�3E#�E$nfE%X E&� E(fE)vfE*a�E+�fE-( E.3E/� E0�3E1�3E3L�E4��E5�3E6��E8VfE9��E:� E;��E>�EBH EE6fEHt�EK��EN��EQ��ET�fEW��E[ E^�Eas3Ed[3Eg��Ej� Em� Ep� Et�EwA�Ezl�Ez��E{p E|@ E|�3E}i�E~fE~�3ED�E�fE�D E���E��3E�C3E���E���E�#3E��fE�� E� E�g3E��3E��E�ffE���E�fE�Q�E�� E��3E�0 E�{3E�� E�0�E�x E��fE�#3E�d�E��3E�3E�^fE���E�� E�5�E��3E��3E�;3E�}�E��fE�, G�O�?L��G�O�G�O�?333G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�?L��G�O�G�O�?�  G�O�?���G�O�?�ff?�  ?���?�33@ff@33@   @9��@Fff@Y��@s33@�  @���@�ff@�  @�  @���@���@ٙ�@陚@�ffA��A	��A  A��A   A)��A1��A8  A>ffAH  AQ��AY��Aa��Ak33Aq��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414141444414141441414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      @ @ v@ �@ *@ �@ #�@ )�@ /�@ 7L@ ?}@ H]@ Q�@ `B@ oF@ {�@ ��@ ��@ ��@ �-@ �&@ �*@ �#@ ��@ � @�@�@ @+�@9X@G�@UU@b�@p�@|�@�D@��@��@��@�>@��@��@�4@��@1@�@#�@/�@=q@K�@X�@ff@t�@�d@�@�@�M@�R@�W@��@�@�@��@	�@�@&�@3�@@,@O�@]�@i�@x�@�@�h@�m@�r@��@�@�
@�@�@�Q@V@[@(�@5�@D�@S�@`�@m:@|?@��@��@��@�~@��@�@�@�m@�q@�@@[@-@<@I@UU@dZ@p�@}�@��@��@��@�9@@є@��@��@�,@�@{@ �@/�@>�@K@Wb@e�@t@�d@�h@�@��@�@�@�O@��@�@��@
�@�@%�@3�@B�@O0@Z�@i�@x�@��@�@�@�r@��@ȴ@�[@�`@�@ �@J@O@)�@7�@DD@P�@^�@m�@|�@�7@��@��@��@��@�@��@��@�@	@	o@	 �@	-@	9X@	G�@	V�@	b�@	n�@	|�@	��@	�<@	��@	��@	��@	�C@	ލ@	�(@	�~@
%@
�@
""@
/�@
>@
K�@
Z@
g�@
v@
�d@
��@
��@
�M@
��@
�J@
��@
��@
�@@
��@�@�@&�@2�@A�@O0@]�@i�@v@�p@��@�m@��@�@�o@�h@�@�Y@  @@O@'�@5?@B�@Q=@^�@m�@|�@�7@��@��@�~@��@�*@��@��@�e@j@�@ �@-@9X@F�@UU@c�@r�@�W@��@��@�M@7L@|?@��@�@M$@��@�#@"�@j@�~@�,@?}@�+@�@�@V@�@�@(�@o�@�F@��@E�@�P@�O@�@`A@��@�@*S@m�@�-@��@:@~�@��@v@Ji@��@�|@o@S�@�<@�#@g@c�@�A@�;@#�@e�@��@��@1'@t@�F@�,@;d@�@�2@j@F�@��@�c@
�@K�@��@��@ 
=@ G�@ �@ @!@!A�@!~�@!�j@!�,@"7�@"ww@"��@"�q@#5�@#s_@#��@#�@$/@$n�@$��@$�@@%.l@%m:@%��@%�@&+�@&k�@&��@&��@'+�@'j@'��@'�@(&;@(dZ@(�(@(�;@)
@)[z@)��@)�O@*@*M$@*��@*�W@+�@+A�@+~K@+�^@+��@,2�@,oF@,��@,�m@-$/@-_�@-��@-�\@.{@.O�@.��@.�c@/�@/C�@/�@/�@/��@05@@0qS@0�@0�@1""@1]�@1��@1�\@2o@2M$@2��@2@2��@37�@3r@3�@3�@4
@4T�@4��@4�@5@5?}@5v@5��@5��@6,`@6hs@6�4@6�T@7!s@7_�@7��@7�;@8[@8Z@8��@8խ@9S�@9�C@:S�@:�O@;S�@<�@<�|@<��@=r@>!s@>�P@?4�@?��@@?}@@�A@A@,@A׹@Bk�@B�7@Ci!@C��@D�\@D�@E��@F$�@F��@G�@G�9@HJi@H��@IC�@I��@JE�@J�H@Kx�@K�@L|�@M@Mx�@N�@N�(@O7L@O��@P'�@Qt�@R�H@T!s@U��@V�@X$/@Y|�@Z��@\"�@]p�@^�2@`-�@ak.@b܀@d/�@ei�@f@h �@i{�@jխ@kJ@kDD@k�@k��@l�@l^�@l��@l�@m+�@mp�@m�F@m�Q@nJi@n��@n�@o	�@oZ�@o��@o�7@p	@pi!@p�r@p��@q> @q�W@q��@r  @r@,@r~K@r�w@sO@sYn@s�0@s�C@t(G@t`B@t�9@t��@u5?@ug@u�~@u��@v<�@v��@v�@w�@wB�@w��G�O�@ G�O�G�O�@ ^G�O�@ G�O�@ G�O�G�O�G�O�G�O�@ G�O�@ G�O�@ G�O�G�O�@ jG�O�@ G�O�@ v@ �@ �@ 	�@ 
�@ J@ �@ b@ �@ �@ �@ �@ �@ �@ �@ ""@ $.@ (G@ +@ .l@ 1'@ 3�@ 7L@ :@ >@ @�@ D�@ H]@ K@ M�@ Q�@ V@ Yn@ \�@ `�@ c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʛ�Aʛ�Aʧ�A���A���A���A���A���A���A���A���A���A�ȴA���A��#A��/A��;A��HA��HA��;A��;A��;A��;A��;A��HA��HA��TA��TA��TA��TA��`A��`A��mA��mA��mA��mA��A��A��yA��A��A��A��A��AʾwA��A�/A���A�r�A�1A�=qA��DA��A�bA�t�A��A��`A�?}A���A��/A�I�A�^5A���A��-A�M�A�^5A���A�A�A���A�|�A��PA���A���A�|�A�K�A���A���A�ƨA��A��A��A�\)A���A��^A�oA��PA��/A���A�~�A���A�/A�7LA�oA�C�A���A�C�A��hA�|�A���A�ZA��TA��A��A��A��A��9A�1A���A�1A�I�A���A�p�A�9XA�mA|JA{
=Ay��Ax�Aw��Aw��AwoAu��As��AshsAsVArM�Ap�Aml�Ak�PAi?}Af�/Ae�
Ae;dAd��Ad��Aa\)A^r�A]��A\�jA[�hAZ�AY
=AW�7AV��AU\)ATZAS�
ASC�AR  AQl�AP�yAO�AM"�AKS�AI��AG��AFA�ADĜAD�AC|�AA�wA@=qA?+A<�\A:ȴA8�HA8(�A7\)A5�A4�A3��A3/A25?A1�#A0�HA.��A.�A.A-��A,��A*�A(1A&�A%�-A%��A%XA$��A$  A#��A!��A �+A��A�A��A/A�A(�A5?AbA|�A��A�-A��A�Az�A�-A9XA��An�A��A
ffA��AffA�A�A�-AZA�wA��A��A`B@��@�ff@�O�@�bN@��@�?}@�;d@���@��@��
@��@�b@���@��m@陚@�bN@��@��y@�%@��H@�+@�V@��T@���@���@���@�Q�@���@۝�@�C�@�$�@؃@�-@�Ĝ@ԋD@�I�@ҸR@�|�@�"�@�E�@̼j@�t�@�j@�=q@�%@��^@��#@���@��@�\)@�A�@�^5@���@�;d@��u@���@�n�@�hs@�z�@� �@��@�b@���@�{@�Q�@���@��u@�ƨ@�"�@��@�O�@�I�@��P@���@��@�ƨ@�;d@���@�$�@�`B@�z�@���@��@���@�@��+@�x�@�+@�^5@��^@���@}�T@|��@{C�@y�@v$�@up�@so@q7L@o|�@k��@h�u@g�@gK�@e`B@c�m@b-@`1'@]�@\Z@[C�@ZM�@Z-@XbN@WK�@U�T@R��@Q�^@Qhs@N�+@M?}@K@J��@J�@IX@H �@Gl�@Fȴ@E�T@D�/@Dz�@C��@CdZ@BM�@Ax�@?�@>V@<9X@9%@8  @6�@6@4�@4j@3@1&�@0�@/l�@.�y@-��@,z�@*��@)G�@(�@&V@&5?@%�@#��@"�H@"��@!G�@��@;d@�@��@@O�@�@��@S�@��@��@��@�@Q�@|�@\)@v�@{@�@"�@~�@=q@�7@&�@�9@��@�y@�@��@S�@@
�\@
�!@
^5@	��@�`@b@|�@��@��@��@�@S�@"�@@��@=q@�#@x�@hs@7L@ ��?�|�?�j?�ƨ?�7L?�+?���?�n�?� �?��m?��?�Q�?�?�?}?��?�7?߾w?�V?�j?�1?ۅ?�~�?�X?���?׍P?Ձ?��?���?Ұ!?�G�?�A�?�|�?��?�{?�O�?�I�?�I�?�ƨ?�C�?�"�?ʟ�?��?���?���?�r�?�ȴ?ě�?�33?���?�bN?�V?�O�?��?�1?�"�?���?�?��#?��?�=q?�x�?��?���?�=q?���?��H?���?��H?�"�?�"�?�?�"�?�C�?�dZ?��?��?��?��?���?�ƨ?��m?�1?�(�?�(�?�I�?�j?��D?��?���?��?�/?�V?�/?�V?�/?�O�?�p�?�/?�/?�p�?�O�?��h?�O�?�p�?��h?�O�?�O�?��?�V?�p�?�/?�O�?�p�Aʣ�Aʛ�Aʗ�Aʕ�Aʗ�Aʗ�Aʗ�Aʗ�Aʙ�Aʙ�Aʙ�Aʛ�Aʟ�Aʟ�Aʟ�Aʟ�Aʟ�Aʟ�Aʝ�Aʙ�Aʛ�Aʛ�Aʟ�Aʛ�Aʛ�Aʛ�Aʛ�Aʝ�Aʟ�Aʣ�AʮAʮAʴ9Aʺ^A�A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Aʛ�Aʛ�Aʧ�A���A���A���A���A���A���A���A���A���A�ȴA���A��#A��/A��;A��HA��HA��;A��;A��;A��;A��;A��HA��HA��TA��TA��TA��TA��`A��`A��mA��mA��mA��mA��A��A��yA��A��A��A��A��AʾwA��A�/A���A�r�A�1A�=qA��DA��A�bA�t�A��A��`A�?}A���A��/A�I�A�^5A���A��-A�M�A�^5A���A�A�A���A�|�A��PA���A���A�|�A�K�A���A���A�ƨA��A��A��A�\)A���A��^A�oA��PA��/A���A�~�A���A�/A�7LA�oA�C�A���A�C�A��hA�|�A���A�ZA��TA��A��A��A��A��9A�1A���A�1A�I�A���A�p�A�9XA�mA|JA{
=Ay��Ax�Aw��Aw��AwoAu��As��AshsAsVArM�Ap�Aml�Ak�PAi?}Af�/Ae�
Ae;dAd��Ad��Aa\)A^r�A]��A\�jA[�hAZ�AY
=AW�7AV��AU\)ATZAS�
ASC�AR  AQl�AP�yAO�AM"�AKS�AI��AG��AFA�ADĜAD�AC|�AA�wA@=qA?+A<�\A:ȴA8�HA8(�A7\)A5�A4�A3��A3/A25?A1�#A0�HA.��A.�A.A-��A,��A*�A(1A&�A%�-A%��A%XA$��A$  A#��A!��A �+A��A�A��A/A�A(�A5?AbA|�A��A�-A��A�Az�A�-A9XA��An�A��A
ffA��AffA�A�A�-AZA�wA��A��A`B@��@�ff@�O�@�bN@��@�?}@�;d@���@��@��
@��@�b@���@��m@陚@�bN@��@��y@�%@��H@�+@�V@��T@���@���@���@�Q�@���@۝�@�C�@�$�@؃@�-@�Ĝ@ԋD@�I�@ҸR@�|�@�"�@�E�@̼j@�t�@�j@�=q@�%@��^@��#@���@��@�\)@�A�@�^5@���@�;d@��u@���@�n�@�hs@�z�@� �@��@�b@���@�{@�Q�@���@��u@�ƨ@�"�@��@�O�@�I�@��P@���@��@�ƨ@�;d@���@�$�@�`B@�z�@���@��@���@�@��+@�x�@�+@�^5@��^@���@}�T@|��@{C�@y�@v$�@up�@so@q7L@o|�@k��@h�u@g�@gK�@e`B@c�m@b-@`1'@]�@\Z@[C�@ZM�@Z-@XbN@WK�@U�T@R��@Q�^@Qhs@N�+@M?}@K@J��@J�@IX@H �@Gl�@Fȴ@E�T@D�/@Dz�@C��@CdZ@BM�@Ax�@?�@>V@<9X@9%@8  @6�@6@4�@4j@3@1&�@0�@/l�@.�y@-��@,z�@*��@)G�@(�@&V@&5?@%�@#��@"�H@"��@!G�@��@;d@�@��@@O�@�@��@S�@��@��@��@�@Q�@|�@\)@v�@{@�@"�@~�@=q@�7@&�@�9@��@�y@�@��@S�@@
�\@
�!@
^5@	��@�`@b@|�@��@��@��@�@S�@"�@@��@=q@�#@x�@hs@7L@ ��?�|�?�j?�ƨ?�7L?�+?���?�n�?� �?��m?��?�Q�?�?�?}?��?�7?߾w?�V?�j?�1?ۅ?�~�?�X?���?׍P?Ձ?��?���?Ұ!?�G�?�A�?�|�?��?�{?�O�?�I�?�I�?�ƨ?�C�?�"�?ʟ�?��?���?���?�r�?�ȴ?ě�?�33?���?�bN?�V?�O�?��?�1?�"�?���?�?��#?��?�=q?�x�?��?���?�=q?���?��H?���?��H?�"�?�"�?�?�"�?�C�?�dZ?��?��?��?��?���?�ƨ?��m?�1?�(�?�(�?�I�?�j?��D?��?���?��?�/?�V?�/?�V?�/?�O�?�p�?�/?�/?�p�?�O�?��h?�O�?�p�?��h?�O�?�O�?��?�V?�p�?�/?�O�?�p�Aʣ�Aʛ�Aʗ�Aʕ�Aʗ�Aʗ�Aʗ�Aʗ�Aʙ�Aʙ�Aʙ�Aʛ�Aʟ�Aʟ�Aʟ�Aʟ�Aʟ�Aʟ�Aʝ�Aʙ�Aʛ�Aʛ�Aʟ�Aʛ�Aʛ�Aʛ�Aʛ�Aʝ�Aʟ�Aʣ�AʮAʮAʴ9Aʺ^A�A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�`B)�Bm�Bt�Bw�B}�B�B�B~�B�B�B�1B�1B�+B�7B�DB�VB�bB�hB��B��B��B��B�{B�{B�oB�{B�{B�oB�oB�JB�JB�DB�B� B~�Bx�Bv�Bq�Bk�BffB_;BS�BJ�B;dB'�B�B1B�sBȴB�wB��B�Bs�B^5BG�B2-B"�B
��B
�yB
�B
�qB
�hB
�B
q�B
iyB
cTB
\)B
E�B
/B
%�B
�B
oB
JB

=B
B	��B	�B	�B	�mB	�NB	��B	B	�?B	�!B	��B	��B	��B	��B	��B	{�B	dZB	ffB	aHB	]/B	XB	K�B	G�B	A�B	7LB	33B	/B	)�B	#�B	!�B	�B	oB	JB	  B��B�B�B�fB�NB�5B��B��BǮB�}B�LB�'B�-B�B��B��B��B��B��B�{B�VB�DB�7B�1B�B}�Bw�Bq�Bn�Bk�BiyBgmBe`BdZB_;B\)BXBS�BO�BL�BI�BI�B@�B?}B>wB:^B8RB9XB;dB<jB:^B9XB9XB=qB<jB<jB:^B;dB<jB9XB:^B7LB9XB7LB5?B5?B1'B1'B0!B-B-B-B-B(�B(�B'�B(�B+B(�B+B)�B/B1'B1'B1'B+B2-B2-B2-B1'B/B.B2-B33B2-B2-B1'B33B2-B7LB9XB:^B9XB8RB@�B>wB>wB@�BW
B_;BgmB�7B��B�B�9B�^BƨB�B�sB	B	{B	!�B	&�B	5?B	=qB	J�B	S�B	_;B	dZB	k�B	z�B	�B	�VB	��B	��B	��B	�'B	�?B	�dB	�}B	ÖB	ɺB	��B	�B	�#B	�5B	�TB	�fB	�B	�B	�B	�B	��B	��B	��B
  B
  B
  B
B
B
B
	7B

=B
DB
\B
bB
bB
{B
�B
�B
�B
�B
�B
�B
!�B
$�B
%�B
&�B
'�B
'�B
(�B
)�B
,B
/B
0!B
0!B
49B
49B
7LB
6FB
8RB
8RB
9XB
;dB
;dB
<jB
<jB
=qB
?}B
?}B
?}B
@�B
A�B
C�B
F�B
H�B
H�B
I�B
J�B
J�B
K�B
N�B
N�B
O�B
P�B
P�B
Q�B
R�B
T�B
W
B
W
B
YB
XB
XB
\)B
[#B
[#B
^5B
_;B
_;B
`BB
_;B
aHB
bNB
aHB
bNB
bNB
dZB
ffB
e`B
ffB
ffB
gmB
hsB
hsB
hsB
jB
l�B
m�B
m�B
m�B
n�B
n�B
o�B
p�B
s�B
r�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
w�B
x�B
y�B
y�B
z�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�7B
�=B
�VB
�bB
�bB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�!B
�'B
�-B
�9B
�9B
�?B
�FB
�LB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�^B
�dB
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
�dB
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
�dB
�dB
�dB
�^B
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      B��B��B��B��BɺBɺB��B��BɺB��B��B��B��B��B��BɺBɺBɺBɺBɺBɺB��BɺB��BɺB��B��BɺB��B��B��B��B��B��B��B��BɺB��B��BɺB��BɺB��B��B��B�NB&�BjBq�Bt�Bz�B}�B~�B{�B� B�B�B�B�B�%B�1B�DB�PB�VB�oB�oB�{B�uB�hB�hB�\B�hB�hB�\B�\B�7B�7B�1B}�B|�B{�Bu�Bs�Bn�BhsBcTB\)BP�BG�B8RB$�B�BB�`BŢB�dB��B� Bp�B[#BD�B/B�B
��B
�fB
��B
�^B
�VB
}�B
n�B
ffB
`BB
YB
B�B
,B
"�B
�B
\B
	7B
+B
B	��B	�B	�yB	�ZB	�;B	��B	�}B	�-B	�B	��B	��B	��B	�{B	�oB	x�B	aHB	cTB	^5B	ZB	T�B	H�B	D�B	>wB	49B	0!B	,B	&�B	 �B	�B	�B	bB	
=B��B��B�B�sB�ZB�BB�)B��B��BŢB�qB�?B�B�!B��B��B��B��B��B�{B�oB�JB�7B�+B�%B�B{�Bu�Bo�Bl�BiyBgmBe`BcTBbNB]/BZBVBQ�BM�BJ�BG�BG�B>wB=qB<jB8RB6FB7LB9XB:^B8RB7LB7LB;dB:^B:^B8RB9XB:^B7LB8RB5?B7LB5?B33B33B/B/B.B+B+B+B+B&�B&�B%�B&�B(�B&�B(�B'�B-B/B/B/B(�B0!B0!B0!B/B-B,B0!B1'B0!B0!B/B1'B0!B5?B7LB8RB7LB6FB>wB<jB<jB>wBT�B]/Be`B�+B��B��B�-B�RBĜB�
B�fB��B	oB	�B	$�B	33B	;dB	H�B	Q�B	]/B	bNB	iyB	x�B	�B	�JB	�uB	��B	��B	�B	�3B	�XB	�qB	��B	ǮB	��B	��B	�B	�)B	�HB	�ZB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
+B
1B
	7B
PB
VB
VB
oB
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
$�B
&�B
&�B
'�B
(�B
+B
.B
/B
/B
33B
33B
6FB
5?B
7LB
7LB
8RB
:^B
:^B
;dB
;dB
<jB
>wB
>wB
>wB
?}B
@�B
B�B
E�B
G�B
G�B
H�B
I�B
I�B
J�B
M�B
M�B
N�B
O�B
O�B
P�B
Q�B
S�B
VB
VB
XB
W
B
W
B
[#B
ZB
ZB
]/B
^5B
^5B
_;B
^5B
`BB
aHB
`BB
aHB
aHB
cTB
e`B
dZB
e`B
e`B
ffB
gmB
gmB
gmB
iyB
k�B
l�B
l�B
l�B
m�B
m�B
n�B
o�B
r�B
q�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
v�B
w�B
x�B
x�B
y�B
{�B
{�B
{�B
{�B
|�B
|�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�7B
�=B
�VB
�bB
�bB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�!B
�'B
�-B
�3B
�?B
�?B
�FB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�jB
�jB
�jB
�qB
�jB
�qB
�qB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�jB
�wB
�wB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBȴBɺBɺBɺBɺB��BɺB��BɺB��BɺBɺB��BɺB��B��B��BɺB��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812160400312021061413530420210614135304202106141747002021061417470020210614174700201812160400312021061413530420210614135304202106141747002021061417470020210614174700PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018121604003120181216040031  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121604003120181216040031QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121604003120181216040031QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015620210722160156IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                