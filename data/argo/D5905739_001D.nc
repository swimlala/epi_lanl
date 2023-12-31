CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  B   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:40Z creation      
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
resolution        =���   axis      Z        
  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  E�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
  Hh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Rx   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
  T�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
  _   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  i   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
  k�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
  x4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ˄   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ˠ   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ˨   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �$Argo profile    3.1 1.2 19500101000000  20180724220240  20210617131450  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�c7��LG@�c7��LG11  @�c7�}C�@�c7�}C�@6�RiY_�@6�RiY_��c��-#N��c��-#N�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >L��?�  @ff@L��@�33@�ff@�33@�33A��A  A&ffAC33Ac33A�  A�  A���A���A���A�33A���A�B ffB  B  B��B!33B(ffB/��B8  B@ffBH  BO33BW��B`ffBh��BpffBx  B�  B�ffB�  B���B�  B�33B�33B�  B���B�  B�ffB���B�  B�  B�  B�ffB�ffB�33B�33B�ffB���B���Bי�B���B�ffB�ffB�  B�  BB���B�  B���B���C��C�CL�C33C
  C  C  C��C�3C  C33C�C  C  C�fC�fC!��C#�3C&  C(33C*33C,�C.  C0�C2L�C433C633C8  C:33C<ffC>�C?�fCA��CD�CF�CH  CJ�CL�CN�CP33CQ�fCT  CU�fCW�fCZ�C\  C]�fC_�fCa�fCd�Cf  Ch�Cj  Cl  Cn  Co�fCr33Ct33Cu�fCx  Cz  C|  C~�C�fC�  C�  C��C��C��3C�  C�  C�  C��C�  C�  C��C�  C��C��C��3C�  C�  C��C��C��C��C��C��C��C��C�  C��C��C��C�  C��C��C��C��C��C�  C�  C�  C�  C��C��C��C�  C�  C��3C��3C�  C��3C��3C��C��C�  C��C��C�  C��C��C�  C��C�  C��3C��C��3C��3C�  C��C��C��3C��C��C��C��C��C��C�  C��C��C��3C��3C��C��C��C��C�&fC�&fC��C��C��3C��3C�  C��C��C��C��3C�  C��C��C�  C��3C�  C��C�  C��3C��C�  C��3C�  C��C��C�  C�&fC��C��C��C��C��C�  C��3C��C��C��fC�� Ds3D� D�3D��D	��D�fD�3D��D�D,�DFfD` Ds3D� D��D ��D"� D$��D&s3D(S3D*,�D,fD-�3D/��D1�fD3` D59�D73D8�3D:ٚD<� D>��D@� DB�fDDl�DFY�DH33DJ�DK��DM�fDO��DQ�fDS` DU9�DW3DX� DZ��D\� D^Y�D`@ Db�Dc��De�fDg�3Di��Dk��Dmy�DoffDqFfDs3Dt��DvٚDx��Dzs3D{�3>���>L��=���>L��>���=���>L��=���>L��>L��>L��>L��>���>L��>L��=���=���>L��>���>L��=���>L��>���>L��>���>���>���?   ?��?333?fff?���?���?�  ?�ff@   @��@   @9��@Fff@`  @s33@�ff@���@���@�ff@�  @���@ə�@�ff@�33@�  @���AffA33A33A33A#33A)��A0  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441141414441444411441141441111111111111111111111111111111111                                                                                                                                                                                                                                                                      ?333?�  @&ff@l��@�33@�ff@�33@�33A	��A  A.ffAK33Ak33A�  A�  A���A���A���A�33A���A���BffB
  B  B��B#33B*ffB1��B:  BBffBJ  BQ33BY��BbffBj��BrffBz  B�  B�ffB�  B���B�  B�33B�33B�  B���B�  B�ffB���B�  B�  B�  B�ffB�ffB�33B�33B�ffB���B���Bؙ�B���B�ffB�ffB�  B�  B�B���B�  B���C L�CL�C��C��C�3C
� C� C� CL�C33C� C�3C��C� C� CffC ffC"L�C$33C&� C(�3C*�3C,��C.� C0��C2��C4�3C6�3C8� C:�3C<�fC>��C@ffCBL�CD��CF��CH� CJ��CL��CN��CP�3CRffCT� CVffCXffCZ��C\� C^ffC`ffCbffCd��Cf� Ch��Cj� Cl� Cn� CpffCr�3Ct�3CvffCx� Cz� C|� C~��C�33C�@ C�@ C�L�C�Y�C�33C�@ C�@ C�@ C�L�C�@ C�@ C�Y�C�@ C�L�C�L�C�33C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�L�C�@ C�Y�C�Y�C�L�C�@ C�Y�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�L�C�L�C�L�C�@ C�@ C�33C�33C�@ C�33C�33C�L�C�L�C�@ C�L�C�L�C�@ C�Y�C�L�C�@ C�L�C�@ C�33C�L�C�33C�33C�@ C�L�C�L�C�33C�L�C�Y�C�L�C�L�C�Y�C�L�C�@ C�Y�C�L�C�33C�33C�L�C�L�C�Y�C�Y�C�ffC�ffC�L�C�L�C�33C�33C�@ C�L�C�Y�C�L�C�33C�@ C�L�C�Y�C�@ C�33C�@ C�Y�C�@ C�33C�L�C�@ C�33C�@ C�Y�C�L�C�@ C�ffC�L�C�L�C�Y�C�Y�C�L�C�@ C�33C�Y�C�L�C�&fC�  D�3D� D�3D��D	��D�fD�3D�D9�DL�DffD� D�3D� D��D ��D"� D$��D&�3D(s3D*L�D,&fD-�3D/��D1�fD3� D5Y�D733D93D:��D<� D>ٚD@� DB�fDD��DFy�DHS3DJ,�DL�DM�fDO��DQ�fDS� DUY�DW33DY  DZٚD\� D^y�D`` Db9�Dd�De�fDg�3Di��Dk��Dm��Do�fDqffDs33Du�Dv��Dx��Dz�3D|3G�O�G�O�?��?333G�O�?��G�O�?��G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�?��?333G�O�G�O�?��?333G�O�?333G�O�G�O�?L��?�  ?���?���?�33?���?ٙ�@   @33@   @,��@@  @Y��@fff@�  @���@�ff@���@���@�ff@�  @���@ٙ�@�ff@�33A   A��AffA33A33A#33A+33A1��A8  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441141414441444411441141441111111111111111111111111111111111                                                                                                                                                                                                                                                                      @ ^@ �@ V@ �@ �@ #�@ )�@ 0x@ 7L@ =q@ F�@ SI@ `�@ m:@ z�@ �7@ ��@ ��@ ��@ ��@ �*@ �#@ �@ ��@�@@g@+�@:@H]@UU@a�@o�@~�@�P@�H@�A@��@��@�7@�/@�@��@�@{@!s@/�@>�@Ji@X�@ff@t@�@��@��@�Y@��@�J@��@��@�@@��@�@�@%�@1�@@,@N�@[z@hs@v@��@��@��@�@��@�c@խ@�@�Y@^@V@O@(�@5�@C�@P�@]�@m:@|?@��@��@��@�-@�2@�*@��@�@� @%@�@
@+@:�@H]@UU@c�@qS@~�@�P@��@�A@�9@��@��@��@��@�~@%@*@""@0x@=q@K@X�@e�@uk@�@��@�@��@�R@ƨ@��@�H@��@�E@�@6@%�@33@@�@O0@\)@i�@x�@�@�u@�@�f@��@�c@׹@�`@�@ �@V@�@*S@7L@DD@SI@`�@m�@z�@��@��@��@�-@��@��@�t@�@��@	@	�@	g@	,`@	:@	F�@	T�@	b�@	o�@	}�@	��@	�H@	�A@	��@	�>@	�7@	�;@	�4@	�,@
�@
{@
!s@
0x@
<�@
Ji@
X�@
g@
t�@
�@
�@
�a@
�Y@
�@
�W@
�O@
�H@
�L@
�E@	�@6@&;@3�@B8@O�@^5@k�@x&@��@�@��@�@�k@��@׹@�@�Y@ �@@O@(G@6�@E�@Q�@^�@m�@z�@��@�0@�5@�-@�&@��@�#@��@� @�@�@�@+�@;d@H]@S�@_�@��@�@?}@v�@�r@��@ �@[z@��@�*@�@@�@y�@�~@��@!s@X�@�P@�2@�e@&�@Yn@��@�@�@""@T�@�+@�^@�@@""@X@��@��@�@(G@Z�@�P@��@�@&�@Yn@��@�w@��@""@T�@�@��@�@
@O�@�d@��@��@ @T�@�7@�j@�@!s@T�@�|@��@��G�O�G�O�@  �@ ^G�O�@  �G�O�@  �G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�@  �@ ^G�O�G�O�@  �@ ^G�O�@ ^G�O�G�O�@ @ j@ @ �@ %@ �@ 1@ 
=@ J@ �@ @ @ �@ *@ �@ �@ �@ 
@  �@ #�@ %�@ (G@ +@ -�@ 0x@ 33@ 5?@ 9X@ ;d@ >�@ B8@ E�@ H]@ KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�5?A�5?A�33A�1'A�1'A�1'A�/A�/A�/A�&�A��A�{A�{A�{A�oA�bA�VA�JA�1A���A�+AüjA�S�ADA�K�A�G�A�I�A�E�A�7LA�A�^5A��A�p�A�ƨA�ffA��A���A�VA���A�bA��\A�A�A�+A�"�A�/A�M�A���A�jA��A��A��A�(�A���A���A�x�A�A��FA�hsA��HA�-A��A�{A��TA��#A��\A��HA���A���A���A�n�A�hsA���A��DA�&�A��9A�I�A�I�A���A�t�A���A�G�A��A��\A�/A�n�A�"�A��9A��7A��9A��RA�E�A�&�A�"�A���A���A�K�A��TA��!A��FA��TA�ZA�ĜA�1'A�=qA�{A��HA��uA�~�A��jA���A�bA��A�;dA��A�jA�(�A��A�XA�n�A�A}G�A{��A{?}AzffAx�+AvȴAu/As��Ar��Aq�FAp5?AnVAl�RAj�HAi�Ae�7Ac��Ab�9Aa;dA_�7A^�A[�AY�AXjAV�RAU33AR��AP��AN�!ALjAK��AJ�AJ1'AI��AIhsAI�AH��AG��AFbAE�ADVAA��A?A<��A;%A:r�A:-A9�A9�7A8�A7�A6 �A5��A333A1�;A1�A0��A0�A//A.(�A-+A,{A+XA*��A*$�A)O�A(�\A'�^A&��A$��A#hsA!��A�`A7LA�DA��A�`A�+A|�AƨAC�AS�A&�A�A��A9XA  A��A�Ap�A��A��AZA�A��A��A5?A��A�jA��A�AM�A��A\)A��A�mAdZA
�A
z�A
ffA
�A�`AjA9XA`BAA�A|�A;dA�yA��A��AG�A�A�#A ��@�n�@���@��+@���@��\@�w@��@�G�@�R@�O�@�p�@�G�@�-@ύP@�I�@��y@�t�@��\@���@���@��w@��H@��u@�5?@�Ĝ@��;@�o@�z�@���@��-@�l�@��T@�r�@���@���@���@�X@��`@�1'@��F@�=q@��-@���@��h@�G�@�V@�1@��H@�~�@�^5@�%@���@��H@�E�@�-@�J@�&�@�  @��@�^5@��7@��`@�(�@��@�~�@��h@��u@��
@�;d@���@�$�@��-@�`B@�7L@�Q�@�A�1'A�33A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�5?A�1'A�5?A�33A�1'A�1'A�33A�33A�5?A�33A�33A�1'A�/A�1'A�1'A�1'A�33A�33A�5?A�5?A�7LA�7LA�33A�5?A�33A�33A�33A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�/A�1'A�/A�/A�/A�/A�/A�-A�/A�-A�+A�$�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      A�33A�5?A�5?A�33A�1'A�1'A�1'A�/A�/A�/A�&�A��A�{A�{A�{A�oA�bA�VA�JA�1A���A�+AüjA�S�ADA�K�A�G�A�I�A�E�A�7LA�A�^5A��A�p�A�ƨA�ffA��A���A�VA���A�bA��\A�A�A�+A�"�A�/A�M�A���A�jA��A��A��A�(�A���A���A�x�A�A��FA�hsA��HA�-A��A�{A��TA��#A��\A��HA���A���A���A�n�A�hsA���A��DA�&�A��9A�I�A�I�A���A�t�A���A�G�A��A��\A�/A�n�A�"�A��9A��7A��9A��RA�E�A�&�A�"�A���A���A�K�A��TA��!A��FA��TA�ZA�ĜA�1'A�=qA�{A��HA��uA�~�A��jA���A�bA��A�;dA��A�jA�(�A��A�XA�n�A�A}G�A{��A{?}AzffAx�+AvȴAu/As��Ar��Aq�FAp5?AnVAl�RAj�HAi�Ae�7Ac��Ab�9Aa;dA_�7A^�A[�AY�AXjAV�RAU33AR��AP��AN�!ALjAK��AJ�AJ1'AI��AIhsAI�AH��AG��AFbAE�ADVAA��A?A<��A;%A:r�A:-A9�A9�7A8�A7�A6 �A5��A333A1�;A1�A0��A0�A//A.(�A-+A,{A+XA*��A*$�A)O�A(�\A'�^A&��A$��A#hsA!��A�`A7LA�DA��A�`A�+A|�AƨAC�AS�A&�A�A��A9XA  A��A�Ap�A��A��AZA�A��A��A5?A��A�jA��A�AM�A��A\)A��A�mAdZA
�A
z�A
ffA
�A�`AjA9XA`BAA�A|�A;dA�yA��A��AG�A�A�#A ��@�n�@���@��+@���@��\@�w@��@�G�@�R@�O�@�p�@�G�@�-@ύP@�I�@��y@�t�@��\@���@���@��w@��H@��u@�5?@�Ĝ@��;@�o@�z�@���@��-@�l�@��T@�r�@���@���@���@�X@��`@�1'@��F@�=q@��-@���@��h@�G�@�V@�1@��H@�~�@�^5@�%@���@��H@�E�@�-@�J@�&�@�  @��@�^5@��7@��`@�(�@��@�~�@��h@��u@��
@�;d@���@�$�@��-@�`B@�7L@�Q�@�A�1'A�33A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�5?A�1'A�5?A�33A�1'A�1'A�33A�33A�5?A�33A�33A�1'A�/A�1'A�1'A�1'A�33A�33A�5?A�5?A�7LA�7LA�33A�5?A�33A�33A�33A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�/A�1'A�/A�/A�/A�/A�/A�-A�/A�-A�+A�$�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�3B
�3B
�3B
�?B
�LB
�XB
�qB
��B
ƨB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��BuB�B�B�B�B�B$�BVBz�B�B��BBÖB��B�BB��B��BDBoB�B�B&�B7LBffB�JB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�bB�PB�\B�=B�hB��B�+BgmBbNBk�B\)BC�B9XB_;B~�B�oB�=B�Bv�Bu�Bn�BiyBaHBJ�BD�B>wB@�B1'B�B.BoB+B%�B�BB��B�jB��B��B�uB�=Bz�Bs�Bn�BVB<jB'�B�BDB
��B
�ZB
ƨB
�^B
�3B
�B
��B
� B
iyB
\)B
L�B
B�B
?}B
9XB
33B
!�B
�B
�B
hB
VB
B	��B	�`B	�#B	��B	�3B	��B	��B	��B	�hB	�=B	v�B	l�B	ffB	^5B	T�B	G�B	?}B	33B	B	#�B	�B	�B	�B	�B	�B	oB	DB	B	  B��B�B�`B�)B�B��B��B��B��BɺBŢB�}B�wB�?B�'B�B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�7B�B�B{�Bu�Bs�Bq�Br�Br�Br�Bp�BhsBm�Bx�By�Bz�By�Bx�Bw�Bw�Bx�Bv�Br�Bo�Bn�Bq�Bs�Bt�Bt�Bu�Bu�Bu�Bt�Br�Bu�Bu�Bu�Bu�Bt�Bt�Bs�Bs�Br�Bq�Bs�Bq�Bo�Bo�Bp�Bp�Bp�Bp�Bo�Bp�Bn�Bp�Bn�Bu�Bw�Bx�Bv�Br�Bk�B`BBYB[#BT�BF�BB�BC�BF�BL�BR�BW
B]/BjBu�B�hB��B��B��B�LB�qBB��B�#B�B��B	%B	 �B	'�B	+B	?}B	P�B	YB	dZB	ffB	z�B	�B	�B	�%B	�7B	�PB	��B	��B	��B	��B	��B	�B	�?B	�RB	�XB	�dB	��B	ƨB	��B	��B	��B	��B	�B	�B	�5B	�HB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	��B
�9B
�3B
�9B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�-B
�9B
�3B
�3B
�3B
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
�9B
�9B
�9B
�RB
�LB
�FB
�LB
�RB
�XB
�dB
�jB
�wB
��B
��B
��B
ŢB
ƨB
ȴB
ȴB
ȴB
ȴB
ɺB
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      B
�
B
�
B
�B
�B
�$B
�1B
�JB
�\B
ƁB
ȎB
ɔB
̨B
̨B
ͯB
ͯB
ζB
ηB
ηB
θB
ϿB
��B
��B
�B
��BWB�B�BwB~B�B$�BU�Bz�B��B�kB�wB�~B˰B�+B��B��B/BZBmB�B&�B7:BfUB�9B�kB�kB�xB�rB�sB��B��B��B��B��B��B��B�wB�YB�GB�TB�5B�aB��B�%BggBbIBk�B\%BC�B9UB_9B~�B�nB�<B�Bv�Bu�Bn�Bi{BaKBJ�BD�B>{B@�B1,B�B.BvB+	B%�B�BB��B�sB�B��B�B�HBz�Bs�Bn�BVB<wB'�B�BRB
��B
�iB
ƷB
�nB
�CB
�%B
��B
�B
i�B
\;B
L�B
B�B
?�B
9lB
3HB
!�B
�B
�B
B
mB
1B	��B	�xB	�<B	��B	�MB	�B	��B	��B	��B	�YB	v�B	l�B	f�B	^SB	UB	G�B	?�B	3RB	8B	#�B	�B	�B	�B	�B	�B	�B	hB	0B	 %B�B��B�B�OB�+B�B�B�B��B��B��B��B��B�jB�SB�:B�)B�B��B��B��B��B��B��B��B��B��B��B�jB�LB�:B|Bu�Bs�Bq�Br�Br�Br�Bp�Bh�Bm�ByBzB{BzByBxBxByBwBr�Bo�Bn�Bq�Bs�Bt�Bt�BvBvBvBt�Br�BvBvBvBv	BuBuBs�Bs�Br�Bq�Bt Bq�Bo�Bo�Bp�Bp�Bp�Bp�Bo�Bp�Bn�Bp�Bn�BvBx By&BwBsBk�B`�BYjB[wBURBF�BB�BC�BGBM,BSSBWnB]�Bj�Bv-B��B��B�#B�EB��B��B�
B�bBۢB�2B�rB	�B	!MB	(zB	+�B	@B	QvB	Y�B	d�B	f�B	{{B	��B	��B	��B	��B	��B	�/B	�oB	��B	��B	��B	��B	��B	�	B	�B	� B	�GB	�hB	˃B	͑B	ЦB	ӻB	��B	��B	�B	�B	�4B	�CB	�WB	�fB	�nB	�}B	�B	�B	��B	��B
�B
�
B
�B
�
B
�B
�
B
�
B
�
B
�
B
�
B
�B
�
B
�
B
�
B
�
B
�
B
�B
�B
�
B
�
B
�
B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�*B
�$B
�B
�$B
�*B
�1B
�=B
�CB
�PB
�\B
�bB
�\B
�{B
ƂB
ȎB
ȎB
ȎB
ȎB
ɔB
ʛG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202402021061413550820210614135508202106171311212021061713112120210617131121201807242202402021061413550820210614135508202106171311212021061713112120210617131121PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024020180724220240  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024020180724220240QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024020180724220240QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145020210617131450IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                