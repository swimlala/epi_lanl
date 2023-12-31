CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  &   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-21T13:34:40Z creation      
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  M   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  Q\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  b�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  f�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ɬ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   `   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   |   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   	    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �        �  Argo profile    3.1 1.2 19500101000000  20181221133440  20210722160156  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               2   2DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؗX��p�@ؗX��p�11  @ؗX����@ؗX����@5�_E��@5�_E���c��?����c��?���11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @@  @�  @�  @���@���A33A��A&ffAC33A`  A~ffA�  A�  A�  A���A�33A�  A���B   B  BffBffB��B(  B0ffB8��B@ffBH  BPffBXffB`  BhffBp  Bx  B�33B�  B���B�33B�33B�33B�ffB�ffB�ffB�33B�33B�33B�33B�33B�33B�  B�33B�33B�  B�  B�  B�ffB�33B���B�33B䙚B�33B���B�  B�  B�ffB�  B���C  C�CL�C�C	�fC�CL�CL�C33C  C�fC33C33C  C  C��C"33C$�C&  C(L�C*L�C,�C.  C/�fC233C433C6�C8�C9�fC<33C>�C@  CB33CD�CF  CHL�CJ�CL  CN  CO�fCR33CT33CV�CX  CY�fC\33C^33C`�Cb  Cc�fCf�ChL�Cj�Ck�fCn  Cp33CrL�Ct�Cu��Cx  Cz�C|L�C~�C�3C��fC��fC��fC��fC��3C��3C�  C�  C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��C��C��C��C�&fC��C��fC��3C�  C��C�  C��3C�  C�&fC��C��C��3C��fC�  C��C�  C��fC�  C��C�&fC��C��3C��C��C�  C��3C��C�  C��fC�  C��C��C��fC�  C��C��C��fC�  C��C�&fC��C��3C�  C��C�&fC��C��3C�  C�  C�  C��C�&fC��C��3C�  C��C�  C��fC�  C��C��C��C��C��fC��3C��fC��3C��3C�  C��C��C��C�&fC�&fC��C��fC��fC��3C�  C��C��C�&fC�  C��C��C��fC��3C��C��C��C�&fC�33C��C��fC��3C��C��C�&fC��C�ٚC��3C��3C��3C�  D   D � DffD��D
,�D��D  D��DfD��D  Ds3D�fD ` D"ٚD%L�D'�fD*L�D,ٚD/l�D1��D4� D7�D9�3D<�D>s3D@� DCFfDE�fDH�DJs3DLٚDO33DQ��DT  DV` DX�3D[&fD]� D`3Db�fDd��Dg� Di��Dll�Dn�3Dqs3Ds�fDvY�Dx� D{,�D}33D�fD�3D�9�D�c3D��3D�� D��3D�#3D�VfD��3D�� D���D�	�D�9�D�i�D��3D���D�3D�33D�i�D��3D�� D�3D�0 D�` D�� D��fD��fD�,�D�ffD���D���D���D�)�D�S3D�|�D�� D��3D� D�<�D�l�D���D��3D��D� D�6fD�S3D�s3D��fD��fD���D��D�3D��D�9�D�Y�D�vfD Dã3D�� D�ٚD���D�  D�fD�,�D�C3D�S3D�ffD�vfDσ3DЖfDѠ DҶfD��fD�� D�� D��3D���D�� D��D��D��D��D��D��fD���D��3D���D���D�  D�3D�3D��D� D�3D��D�&fD�&fD�,�D�,�D�33D�9�D�<�D�C3D�@ D�FfD�@ D�9�D�6fD�<�D�9�D�33D�  D��D�fD�fD�fE 3E ��EfE�3E  E{3E��E�3Es3EnfE�fE	� ET�EVfE��E��E!�E��E� E��EQ�E�3E��E�E  El�E�3EњE@ E �3E!�3E#3E$c3E%A�E&� E'�E)( E*i�E+�3E,��E.$�E/` E0��E1ٚE33E4VfE5��E6�fE8fE9A�E:|�E;� E?1�EB EE;3EHVfEK� EN�fEQ� ET� EW�fE[  E^L�Eah Ed�fEg�fEj�3Em�3Ep��Et#3Et��Euc3Ev3Ev�3EwC3Ew�3Ex� Ey  Ey�3Eza�E{�E{� E|9�E|ٚE}t�E~ E~�fE<�E�fE�5�E���E���E�5�E�}�E���E�0 E�u�E��fE�"fE�d E���E� E�BfE���E��E�BfE�� E��E�&fE��fE�ŚE�$�E�ffE��3E�3E�]�E���E��E�E�E�� ?   >���>���>���>���>���>���>���>���>���?   ?   ?��?   ?   ?333?333?L��?L��?fff?fff?���?�ff?�  ?�33?�33@��@&ff@@  @S33@fff@�33@���@���@�ff@�ff@�  @�33@���@陚@���A33A  AffA��A33A#33A)��A0  A8  A>ffAFffAL��AT��A[33Ac33Ak33Aq��AvffA|��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144444141441414141111411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?fff?�33@   @`  @�  @�  @���@���A33A��A.ffAK33Ah  A�33A�  A�  A�  A���A�33A�  A���B  B
  BffBffB!��B*  B2ffB:��BBffBJ  BRffBZffBb  BjffBr  Bz  B�33B�  B���B�33B�33B�33B�ffB�ffB�ffB�33B�33B�33B�33B�33B�33B�  B�33B�33B�  B�  B�  B�ffB�33B���B�33B噚B�33B���B�  B�  B�ffB�  C L�C� C��C��C��C
ffC��C��C��C�3C� CffC�3C�3C� C� C L�C"�3C$��C&� C(��C*��C,��C.� C0ffC2�3C4�3C6��C8��C:ffC<�3C>��C@� CB�3CD��CF� CH��CJ��CL� CN� CPffCR�3CT�3CV��CX� CZffC\�3C^�3C`��Cb� CdffCf��Ch��Cj��ClffCn� Cp�3Cr��Ct��CvL�Cx� Cz��C|��C~��C��C�&fC�&fC�&fC�&fC�33C�33C�@ C�@ C�33C�33C�33C�33C�&fC�&fC�33C�33C�33C�33C�L�C�L�C�Y�C�Y�C�ffC�Y�C�&fC�33C�@ C�L�C�@ C�33C�@ C�ffC�Y�C�L�C�33C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�ffC�Y�C�33C�L�C�Y�C�@ C�33C�L�C�@ C�&fC�@ C�Y�C�L�C�&fC�@ C�Y�C�L�C�&fC�@ C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�33C�@ C�@ C�@ C�Y�C�ffC�L�C�33C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�Y�C�L�C�&fC�33C�&fC�33C�33C�@ C�L�C�Y�C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�@ C�L�C�Y�C�ffC�@ C�L�C�L�C�&fC�33C�L�C�Y�C�Y�C�ffC�s3C�L�C�&fC�33C�L�C�Y�C�ffC�L�C��C�33C�33C�33C�@ D   D � D�fDٚD
L�D��D@ D��D&fD��D  D�3DfD � D"��D%l�D'�fD*l�D,��D/��D2�D4� D7,�D9�3D<,�D>�3DA  DCffDE�fDH,�DJ�3DL��DOS3DQ��DT  DV� DX�3D[FfD]� D`33Db�fDe�Dg� Dj�Dl��Do3Dq�3DtfDvy�Dx� D{L�D}S3D�fD�3D�I�D�s3D��3D�� D�3D�33D�ffD��3D�� D���D��D�I�D�y�D��3D���D�3D�C3D�y�D��3D�� D�3D�@ D�p D�� D��fD�fD�<�D�vfD���D���D��D�9�D�c3D���D�� D��3D�  D�L�D�|�D���D��3D���D�  D�FfD�c3D��3D��fD��fD���D���D�3D�)�D�I�D�i�D��fD  Dó3D�� D��D���D� D�&fD�<�D�S3D�c3D�vfDΆfDϓ3DЦfDѰ D��fD��fD�� D�� D��3D���D�  D���D���D���D���D���D��fD���D�3D�	�D��D� D�3D�3D��D�  D�#3D�,�D�6fD�6fD�<�D�<�D�C3D�I�D�L�D�S3D�P D�VfD�P D�I�D�FfD�L�D�I�D�C3D�0 D�)�D�&fD�&fD�&fE 3E ��EfE�3E E�3E�E�3E{3EvfE�fE	� E\�E^fE��E��E)�E��E� E��EY�E�3E��E�E Et�E�3EٚEH E �3E!�3E#3E$k3E%I�E&� E'�E)0 E*q�E+�3E,��E.,�E/h E0��E1�E33E4^fE5��E6�fE8fE9I�E:��E;� E?9�EB EEC3EH^fEK� EN�fEQ� EU  EW�fE[( E^T�Eap Ed�fEg�fEj�3Em�3Eq�Et+3Et��Euk3Ev3Ev�3EwK3Ew�3Ex� Ey( Ey�3Ezi�E{	�E{� E|A�E|�E}|�E~ E~�fED�E�fE�9�E���E���E�9�E���E���E�4 E�y�E��fE�&fE�h E���E� E�FfE���E��E�FfE�� E��E�*fE��fE�ɚE�(�E�jfE��3E�3E�a�E���E��E�I�E�� G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�?�  G�O�G�O�?�  G�O�?���G�O�?�ffG�O�?�33?���?�ff@   G�O�@��@,��@Fff@`  @s33@�33@�33@���@���@�ff@�ff@�  @�33@���@���A��A33A  AffA��A#33A+33A1��A8  A@  AFffANffAT��A\��Ac33Ak33As33Ay��A~ffA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144444141441414141111411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ �@ %@ �@ {@ O@ ""@ (G@ /@ 7�@ >@ F�@ SI@ _�@ l�@ z�@ ��@ �0@ ��@ ��@ �&@ �|@ �t@ �@ �q@@b@�@-@;d@H]@UU@c�@qS@~K@��@��@�A@��@@ψ@ލ@�4@��@1@�@#�@0x@>@K�@Yn@g@t�@��@�@��@��@�R@��@��@��@�@@�E@J@�@$�@33@@�@O�@\)@hs@ww@��@��@�@�f@�k@�o@�@�@�Y@�Q@@�@(�@6�@B�@SI@`B@m:@|�@��@��@��@��@��@�*@�#@��@�@�@�@�@-�@:�@G�@Wb@c�@p�@~K@�D@��@��@��@@ψ@�;@��@��@�@�@"�@1�@>@Ji@X�@g�@v@�d@��@�@�Y@�^@ƨ@є@��@�@�9@�@6@$�@33@@�@M�@[z@i!@v�@��@�h@��@�f@�@ȴ@׹@�`@�@^@�@�@'�@5�@DD@R�@_�@l�@z�@��@��@��@��@��@��@��@�@�e@	j@	o@	 �@	-�@	9X@	H]@	V�@	b�@	o�@	~�@	��@	�<@	�A@	�F@	�>@	��@	��@	��@	��@
v@
{@
#�@
1�@
>@
Ji@
X�@
g@
v@
�d@
��@
�@
��@
�R@
�W@
խ@
��@
�@@
��@�@�@$.@33@A�@O�@]�@j@v@�p@�h@��@�f@��@�@�h@�@�e@@V@�@'�@5�@DD@R�@`�@oF@z�@�7@��@�y@��@��@�*@��@�(@�~@@�@
@-@;d@I�@V@`�@o�@}�@�D@��@�A@��@:�@z3@�@^@DD@��@�@@Q�@��@׹@O@^�@��@�`@*S@o�@�F@��@@�@�|@�o@@P�@��@�O@*@V�@�<@��@�@Z@�@��@[@`A@��@�@)�@l�@�~@�@7�@|�@�2@@F�@��@��@@D�@��@�@�@H]@��@�@
�@Lu@��@��@�@M$@��@��@�@Q=@�u@�O@ �@ Yn@ ��@ �#@!O@!\)@!�@!�;@" @"bN@"�4@"�@#(G@#i!@#�M@#��@$(G@$i�@$�Y@$�@%+�@%l�@%��@%�4@&+@&i�@&��@&�`@'"�@'`�@'�a@'��@(�@(R�@(��@(�o@)�@)E�@)��@)�j@)�,@*5@@*o�@*��@*�@+!s@+\�@+��@+є@,�@,D�@,�@,�R@,�@--�@-ff@-�m@-׹@.b@.G�@.|�@.��@.�(@/ �@/Wb@/�P@/�J@/�E@05@@0l�@0��@0�#@1�@1Ji@1��@1�@1�@2*S@2`�@2��@2ψ@3�@3?}@3v�@3��@3�@4�@4Q�@4�+@4�@4�@5+@5`A@5��@5�@5��@64�@6k.@6��@6׹@7�@7B8@7ww@7�@8O1@8��@9]�@9ȴ@:i!@:�7@;r�@;��@<�W@<�@=~�@>�@>��@?[@?��@@Lu@@��@AO1@A��@BP�@B��@CV@C�Y@D�P@D�@E��@F#�@F�d@G{@G�@H,`@H��@IB8@I�@JM$@JӠ@KYn@K��@Lg�@L�@M{�@N�@N��@O
�@O�i@PO@Q�u@R�c@T&�@Uz2@V�H@X/@Yww@Z��@\!s@]~K@^�@`,`@a�p@b��@d�@e�|@f�C@h*S@hoF@h��@h� @i;d@i�@i��@j�@jK@j��@j�O@k�@k\)@k��@k��@l$/@lff@l��@l�@m(G@mg�@m�W@n@nB8@n�@n�k@o�@oSI@o��@o�@p�@pV@p�A@p܀@q-@q�@q��@q��@rB8@ry�@r�o@s^@sR�@s��@s�/@tb@t]�@t��@t�@u#�@um:G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�G�O�@ jG�O�@ �G�O�@ vG�O�@ %@ �@ �@ 
=G�O�@ �@ @ �@ {@ �@ �@ �@ 
@  �@ #�@ &�@ (�@ -@ /@ 1�@ 5?@ 7�@ :@ <�@ ?}@ B8@ E�@ H]@ K@ N�@ Q=@ T�@ Wb@ Z�@ ]�@ `�@ dZ@ g@ i!@ k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��`A��A���A���A��A���A���A���A���A���A�A�A�A�A�%A�%A�1A�%A�%A�%A�%A�1A�
=A�
=A�1A�
=A�JA�
=A�JA�JA�VA�VA�VA�VA�bA�bA�bA�
=A�  A���A���A���AȁAœuA��wA��A��hA���A�oA�z�A���A��TA�bA�p�A�^5A���A��A�ZA�;dA��A��jA�1A��7A�5?A�"�A��9A���A�/A��mA���A��A��`A��yA�9XA�ȴA��A�ȴA��A�=qA��A�ĜA�I�A��A�S�A���A�G�A�p�A�$�A�VA�l�A��yA��A��DA�ȴA�I�A�bA��A�x�A��
A�5?A�VA�=qA��yA�oA�jA��#A��RA��A~bA{�Ay��Axn�AwAw?}Av��Au��AtbNAq?}Ao�AooAl�\AkO�Ai�-Ah�Ahn�AgG�AdE�Ab{A_��A^{A\ �AZ�AY�;AX9XAVn�AS�AQx�AP�DAOAN��AN=qAL�AJ��AH��AF��AF$�AEp�AD�DABr�AA�A@Q�A>��A=�wA<n�A;7LA:E�A8�jA7�A6�/A6��A6�\A69XA5��A3�A0�A0VA/�
A/G�A.~�A-33A,z�A,VA,=qA+��A*��A)��A(�HA'��A&�A&ffA$�\A#�FA!�PA E�AO�A��A-AVA�FA��A�uAv�A-AJAE�A�
A33A%A�HA��A�uAz�A��A�A��Ar�A�hAO�A�/A�uAVAbA�mA��A
=A	�hA�uA{AC�A�+A�AI�A`BA5?A��A �`A 1'@��\@��^@��/@�Z@��@�l�@�n�@�;d@@�$�@�G�@��@�"�@�v�@�X@�9@�S�@�-@�j@�C�@�-@�hs@܋D@��;@�K�@��y@ڧ�@�n�@�7L@�9X@��@׮@�;d@���@��@̓u@ɑh@�o@�&�@��`@�Ĝ@�$�@�1'@���@��@���@���@� �@��`@���@�ff@�/@��P@���@�E�@�bN@�dZ@�$�@���@�Q�@�ff@�j@���@���@��w@��!@��@�p�@���@�^5@��7@��u@���@�n�@��@�Q�@�dZ@���@���@���@��@��@���@��@�bN@;d@|�j@z��@y�^@x�@v�y@t�@sƨ@q��@ol�@nE�@l�@l��@j=q@h�9@h �@d��@b^5@_�@]p�@[��@[C�@Z-@XĜ@V��@S�m@Qhs@O+@L�j@LZ@L1@Kt�@K"�@JJ@I�@Gl�@Fv�@F@Dz�@D1@C@@ �@?�P@>�R@=�@<9X@<1@;�m@;@:�!@:-@9�7@81'@5��@3ƨ@3"�@1X@0�`@0  @/;d@.��@-��@-`B@,�/@*�@)�@(Q�@&ff@&@$�@#�
@!��@�w@�@��@�T@j@^5@�7@r�@�@l�@
=@�T@?}@��@Z@(�@ƨ@C�@n�@7L@bN@�@�@��@V@p�@�@1@��@
�@
��@	�7@A�@�P@ȴ@�+@�@��@p�@�@z�@33@�H@��@��@��@��@%@ �u@ 1'?��?��D?�?��P?�ff?�`B?�n�?��`?��?�O�?�V?�"�?��?�u?��?��y?�j?�Z?㕁?���?�|�?ݲ-?܋D?ۥ�?׮?��T?�9X?�9X?��?��?���?�G�?�|�?�\)?���?θR?̋D?ʟ�?�=q?�^5?ɺ^?ȓu?�l�?�+?�ȴ?�E�?š�?�9X?��?��?�A�?�{?��?��m?��H?�~�?���?���?��9?�b?�1'?�Q�?�Q�?��9?�r�?��u?��u?���?���?��?�7L?�7L?�7L?�x�?�x�?��^?���?��#?���?���?��?�=q?�~�?���?���?��H?�?�?�C�?�dZ?�dZ?��?��?�ƨ?�1?�1?�(�?�I�?�j?��?��?���?��?�V?�V?�/?�O�?�p�?��h?��h?��-?���?���?��-?��A���A��A��A���A���A���A��A��A��A��mA��yA��yA��A��`A��`A��HA��HA��HA��HA��`A��mA��mA��yA��A��A��A���A���A���A���A���A���A���A��A��A��A���A���A��A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�A�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A��A��`A��A���A���A��A���A���A���A���A���A�A�A�A�A�%A�%A�1A�%A�%A�%A�%A�1A�
=A�
=A�1A�
=A�JA�
=A�JA�JA�VA�VA�VA�VA�bA�bA�bA�
=A�  A���A���A���AȁAœuA��wA��A��hA���A�oA�z�A���A��TA�bA�p�A�^5A���A��A�ZA�;dA��A��jA�1A��7A�5?A�"�A��9A���A�/A��mA���A��A��`A��yA�9XA�ȴA��A�ȴA��A�=qA��A�ĜA�I�A��A�S�A���A�G�A�p�A�$�A�VA�l�A��yA��A��DA�ȴA�I�A�bA��A�x�A��
A�5?A�VA�=qA��yA�oA�jA��#A��RA��A~bA{�Ay��Axn�AwAw?}Av��Au��AtbNAq?}Ao�AooAl�\AkO�Ai�-Ah�Ahn�AgG�AdE�Ab{A_��A^{A\ �AZ�AY�;AX9XAVn�AS�AQx�AP�DAOAN��AN=qAL�AJ��AH��AF��AF$�AEp�AD�DABr�AA�A@Q�A>��A=�wA<n�A;7LA:E�A8�jA7�A6�/A6��A6�\A69XA5��A3�A0�A0VA/�
A/G�A.~�A-33A,z�A,VA,=qA+��A*��A)��A(�HA'��A&�A&ffA$�\A#�FA!�PA E�AO�A��A-AVA�FA��A�uAv�A-AJAE�A�
A33A%A�HA��A�uAz�A��A�A��Ar�A�hAO�A�/A�uAVAbA�mA��A
=A	�hA�uA{AC�A�+A�AI�A`BA5?A��A �`A 1'@��\@��^@��/@�Z@��@�l�@�n�@�;d@@�$�@�G�@��@�"�@�v�@�X@�9@�S�@�-@�j@�C�@�-@�hs@܋D@��;@�K�@��y@ڧ�@�n�@�7L@�9X@��@׮@�;d@���@��@̓u@ɑh@�o@�&�@��`@�Ĝ@�$�@�1'@���@��@���@���@� �@��`@���@�ff@�/@��P@���@�E�@�bN@�dZ@�$�@���@�Q�@�ff@�j@���@���@��w@��!@��@�p�@���@�^5@��7@��u@���@�n�@��@�Q�@�dZ@���@���@���@��@��@���@��@�bN@;d@|�j@z��@y�^@x�@v�y@t�@sƨ@q��@ol�@nE�@l�@l��@j=q@h�9@h �@d��@b^5@_�@]p�@[��@[C�@Z-@XĜ@V��@S�m@Qhs@O+@L�j@LZ@L1@Kt�@K"�@JJ@I�@Gl�@Fv�@F@Dz�@D1@C@@ �@?�P@>�R@=�@<9X@<1@;�m@;@:�!@:-@9�7@81'@5��@3ƨ@3"�@1X@0�`@0  @/;d@.��@-��@-`B@,�/@*�@)�@(Q�@&ff@&@$�@#�
@!��@�w@�@��@�T@j@^5@�7@r�@�@l�@
=@�T@?}@��@Z@(�@ƨ@C�@n�@7L@bN@�@�@��@V@p�@�@1@��@
�@
��@	�7@A�@�P@ȴ@�+@�@��@p�@�@z�@33@�H@��@��@��@��@%@ �u@ 1'?��?��D?�?��P?�ff?�`B?�n�?��`?��?�O�?�V?�"�?��?�u?��?��y?�j?�Z?㕁?���?�|�?ݲ-?܋D?ۥ�?׮?��T?�9X?�9X?��?��?���?�G�?�|�?�\)?���?θR?̋D?ʟ�?�=q?�^5?ɺ^?ȓu?�l�?�+?�ȴ?�E�?š�?�9X?��?��?�A�?�{?��?��m?��H?�~�?���?���?��9?�b?�1'?�Q�?�Q�?��9?�r�?��u?��u?���?���?��?�7L?�7L?�7L?�x�?�x�?��^?���?��#?���?���?��?�=q?�~�?���?���?��H?�?�?�C�?�dZ?�dZ?��?��?�ƨ?�1?�1?�(�?�I�?�j?��?��?���?��?�V?�V?�/?�O�?�p�?��h?��h?��-?���?���?��-?��A���A��A��A���A���A���A��A��A��A��mA��yA��yA��A��`A��`A��HA��HA��HA��HA��`A��mA��mA��yA��A��A��A���A���A���A���A���A���A���A��A��A��A���A���A��A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�A�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BBVBP�B�B�B{�B�B�1B�B�DB�PB�PB�\B�oB�bB�uB�\B�VB�oB�VB�DB�%B�B|�Bt�Br�Bo�Bk�BdZBXBS�BH�B:^B5?B1'B�BoB��B�B�BÖB�dB�'B��B�oB�Bs�Bk�B`BBG�B9XB�B1B
��B
�B
�B
�#B
�qB
�?B
��B
��B
��B
�7B
{�B
e`B
XB
B�B
.B
!�B
�B
oB
VB
1B	��B	��B	�5B	�/B	��B	ĜB	�RB	�-B	�B	��B	��B	�hB	�B	z�B	m�B	bNB	ZB	R�B	G�B	<jB	/B	#�B	�B	�B	�B	\B		7B��B�B�sB�ZB�5B�B��B��B��B�qB�XB�-B�'B��B��B��B��B��B��B��B��B�PB�PB�=B�1B�%B�B� B� B~�B}�B{�By�By�Bw�Bu�Bs�Bo�Bo�BjBgmBdZBe`BdZBaHB]/B\)BZBZBYBXBXBQ�BP�BP�BM�BL�BK�BK�BJ�BF�BD�BE�BB�BD�BB�BA�B@�B@�B?}B@�B?}B;dB>wB>wB<jB;dB:^B:^B8RB7LB6FB6FB5?B33B49B33B33B2-B/B2-B0!B-B49B49B33B49B33B=qBB�BA�BB�BA�B>wB>wB=qBE�BK�BL�BM�BM�BN�BN�BN�BR�BR�BS�BS�BS�BVB|�B� B�+B�VB�\B��B�B�jB��B�;B�NB�yB��B	uB	�B	!�B	%�B	1'B	>wB	Q�B	e`B	u�B	�B	�VB	�oB	��B	��B	��B	�!B	�B	�9B	�jB	B	ɺB	��B	�B	�B	�B	�/B	�HB	�fB	�mB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
%B
1B

=B
DB
PB
bB
hB
oB
oB
�B
�B
�B
�B
�B
!�B
$�B
%�B
&�B
'�B
(�B
+B
/B
0!B
33B
6FB
5?B
5?B
6FB
7LB
8RB
9XB
;dB
<jB
<jB
>wB
=qB
>wB
A�B
A�B
B�B
C�B
D�B
D�B
D�B
F�B
E�B
G�B
F�B
H�B
K�B
M�B
L�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
VB
VB
W
B
YB
XB
ZB
[#B
^5B
aHB
aHB
aHB
bNB
dZB
ffB
ffB
gmB
hsB
hsB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
l�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
r�B
s�B
s�B
s�B
t�B
v�B
x�B
w�B
x�B
x�B
y�B
z�B
y�B
z�B
z�B
|�B
}�B
|�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�%B
�%B
�7B
�=B
�=B
�JB
�PB
�VB
�bB
�hB
�oB
�oB
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
�B
��B
�B
�B
�B
�B
�B
�B
�'B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�RB
�^B
�^B
�XB
�^B
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�jB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�dB
�jB
�dB
�dB
�dB
�dB
�jB
�jB
�dB
�jB
�dB
�jB
�dB
�jB
�dB
�dB
�jB
�dB
�jB
�jB
�jB
�jB
�qB
�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�/BDBM�B�B}�Bx�B�B�B� B�1B�=B�=B�JB�\B�PB�bB�JB�DB�\B�DB�1B�B}�By�Bq�Bo�Bl�BhsBaHBT�BP�BE�B7LB2-B.B{B\B��B�yB��B��B�RB�B��B�\B�Bp�BhsB]/BD�B6FB�BB
�B
�B
�mB
�B
�^B
�-B
��B
��B
�oB
�%B
x�B
bNB
T�B
?}B
+B
�B
{B
\B
DB
B	��B	�B	�#B	�B	��B	��B	�?B	�B	��B	��B	��B	�VB	�B	w�B	jB	_;B	W
B	O�B	D�B	9XB	,B	 �B	�B	�B	uB	JB	%B��B�B�`B�HB�#B�BɺBȴB�wB�^B�FB�B�B��B��B��B��B��B��B��B�{B�=B�DB�+B�B�B�B}�B}�B|�B{�By�Bw�Bw�Bu�Bs�Bq�Bm�Bm�BhsBe`BbNBcTBbNB_;B[#BZBXBXBW
BVBVBO�BN�BN�BK�BJ�BI�BI�BH�BD�BB�BC�B@�BB�B@�B?}B>wB>wB=qB>wB=qB9XB<jB<jB:^B9XB8RB8RB6FB5?B49B49B33B1'B2-B1'B1'B0!B-B0!B.B+B2-B2-B1'B2-B1'B;dB@�B?}B@�B?}B<jB<jB;dBC�BI�BJ�BK�BK�BL�BL�BL�BP�BP�BQ�BQ�BQ�BS�Bz�B}�B�B�JB�PB�uB�B�^B��B�/B�BB�mB��B	hB	�B	�B	#�B	/B	<jB	O�B	cTB	s�B	�B	�JB	�bB	�uB	��B	��B	�B	�B	�-B	�^B	��B	ǮB	��B	��B	��B	�
B	�#B	�;B	�ZB	�`B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
1B
	7B
DB
VB
\B
bB
bB
{B
�B
�B
�B
�B
�B
"�B
#�B
$�B
%�B
&�B
(�B
.B
/B
2-B
5?B
49B
49B
5?B
6FB
7LB
8RB
:^B
;dB
;dB
=qB
<jB
=qB
@�B
@�B
A�B
B�B
C�B
C�B
C�B
E�B
D�B
F�B
E�B
G�B
J�B
L�B
K�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
T�B
T�B
VB
XB
W
B
YB
ZB
]/B
`BB
`BB
`BB
aHB
cTB
e`B
e`B
ffB
gmB
gmB
gmB
hsB
iyB
jB
jB
jB
jB
jB
k�B
m�B
m�B
m�B
n�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
s�B
u�B
w�B
v�B
w�B
w�B
x�B
y�B
x�B
y�B
y�B
{�B
|�B
{�B
}�B
}�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�%B
�%B
�7B
�=B
�=B
�JB
�PB
�VB
�bB
�hB
�oB
�oB
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
�B
��B
�B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�XB
�XB
�dB
�dB
�^B
�dB
�jB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�wB
�qB
�wB
�qB
�wB
�wB
�wB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�wB
�qB
�wB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�wB
�qB
�wB
�qB
�wB
�qB
�qB
�wB
�qB
�wB
�wB
�wB
�wB
�}B
�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812211334402021061413530620210614135306202106141747012021061417470120210614174701201812211334402021061413530620210614135306202106141747012021061417470120210614174701PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018122113344020181221133440  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122113344020181221133440QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018122113344020181221133440QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015620210722160156IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                