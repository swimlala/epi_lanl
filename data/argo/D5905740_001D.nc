CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  K   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:55Z creation      
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
X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  F,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
X  H�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
X  U�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
X  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  jd   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
X  l�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wT   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
X  y�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
X  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
X  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
X  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ΄   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    Π   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     Ψ   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �$Argo profile    3.1 1.2 19500101000000  20180724220255  20210722161417  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�jpؿ=�@�jpؿ=�11  @�jp�y @�jp�y @*f�k�v�@*f�k�v��cH�4m���cH�4m��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?���@   @@  @y��@���@�  @�  A33A  A$��A@  A`  A���A���A���A�  A�  Aљ�A���A�  B ffB  B��B  B��B(ffB0��B8��B@  BH  BPffBXffB`��Bh  BpffBxffB�  B�  B�  B�33B�ffB�ffB�33B���B�  B�33B�ffB�ffB�ffB�33B�ffB�33B�33Bę�B���B���B�  B���B�ffB�33B���B�  B�33B�ffB�ffB�ffB�  B���C   C�C�fC  C�C
  C�fC  C�C  C��C  C�C  C��C  C 33C"�C$  C%��C(  C*�C+�fC-�fC0  C2  C4  C6  C8�C:33C<33C>�C@�CB�CD�CF�CH�CJ�CL33CN33CPL�CR33CT33CV33CX33CZ33C\33C^  C_��Ca�fCc�fCf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C{�fC}�fC�fC��3C��3C��C��C�  C��C��C��C�  C��fC��3C��C��C�&fC�  C��3C�  C�  C��3C�  C�  C��C��C��C�  C��fC��3C�  C��3C�  C��C��C��C��C��3C�  C��C�&fC��C��C�  C��C�  C��3C��3C��3C��C�&fC��C�&fC��C��C��C�  C��3C�  C��C��C�  C�  C��C�  C�  C��C��C�  C�  C��C��C��3C�  C��C��C��C��C��C��C�  C��C��C��C�  C�  C�  C��3C��3C��3C�  C��C��3C�  C�  C��C��C��C��C��C�  C��3C�  C�  C��C��C��3C��C��C��3C�  C�  C�  C��C��C��C��C��3C�  C��C��3C�  C��C��C��C�  C�ٚC��fC��fD� D��D�fDffD
FfD  D��D��D�3D` D33D�3D�3D` D3D��DffD!&fD"��D$��D&��D(L�D*&fD,3D.3D0fD2  D3��D6fD8fD:3D<  D>  D@&fDB  DD�DF  DG��DI�3DK�fDMs3DO,�DP� DR��DT@ DU��DW��DY  DZ�fD\�D]�3D_  D`y�Da� DcFfDd��Df&fDg��DifDj� Dl  Dm��DofDp��Dr�Ds�3Du` Dv��Dx� Dzy�D|  >���>L��>L��>L��>L��>���>���>���>L��>���>L��>���=���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���?   ?   ?��?L��?fff?�  ?���?�33?�  ?�ff@   @33@&ff@333@Fff@Y��@fff@y��@�ff@�33@�  @�ff@�ff@���@ə�@�33@�33@���A   AffA��A33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414141414114444414441441141111111111111111111111111111111                                                                                                                                                                                                                                                                                 ?L��?���@   @`  @���@���@�  @�  A33A  A,��AH  Ah  A���A���A���A�  A�  Aՙ�A���A�  BffB
  B��B  B!��B*ffB2��B:��BB  BJ  BRffBZffBb��Bj  BrffBzffB�  B�  B�  B�33B�ffB�ffB�33B���B�  B�33B�ffB�ffB�ffB�33B�ffB�33B�33Bř�B���B���B�  B���B�ffB�33B���B�  B�33B�ffB�ffB�ffB�  B���C � C��CffC� C��C
� CffC� C��C� CL�C� C��C� CL�C� C �3C"��C$� C&L�C(� C*��C,ffC.ffC0� C2� C4� C6� C8��C:�3C<�3C>��C@��CB��CD��CF��CH��CJ��CL�3CN�3CP��CR�3CT�3CV�3CX�3CZ�3C\�3C^� C`L�CbffCdffCf� Ch� CjffCl� Cn� Cp� Cr� Ct� CvffCx� Cz� C|ffC~ffC�33C�33C�33C�L�C�L�C�@ C�Y�C�L�C�L�C�@ C�&fC�33C�L�C�L�C�ffC�@ C�33C�@ C�@ C�33C�@ C�@ C�L�C�Y�C�L�C�@ C�&fC�33C�@ C�33C�@ C�Y�C�L�C�L�C�L�C�33C�@ C�L�C�ffC�Y�C�L�C�@ C�L�C�@ C�33C�33C�33C�L�C�ffC�Y�C�ffC�Y�C�Y�C�L�C�@ C�33C�@ C�Y�C�L�C�@ C�@ C�Y�C�@ C�@ C�L�C�Y�C�@ C�@ C�Y�C�L�C�33C�@ C�Y�C�Y�C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�@ C�@ C�@ C�33C�33C�33C�@ C�L�C�33C�@ C�@ C�L�C�L�C�L�C�Y�C�Y�C�@ C�33C�@ C�@ C�L�C�Y�C�33C�L�C�L�C�33C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�33C�@ C�L�C�33C�@ C�Y�C�Y�C�Y�C�@ C��C�&fC�&fD� D��D�fD�fD
ffD@ D�D��D�3D� DS3D3D�3D� D33DٚD�fD!FfD#�D$ٚD&��D(l�D*FfD,33D.33D0&fD2  D4�D6&fD8&fD:33D<@ D>@ D@FfDB@ DD9�DF  DH�DI�3DK�fDM�3DOL�DQ  DR��DT` DV�DW��DY@ DZ�fD\9�D]�3D_  D`��Db  DcffDdٚDfFfDg��Di&fDj� Dl  Dm��Do&fDp��Dr9�Ds�3Du� Dw�Dx� Dz��D|  G�O�G�O�G�O�G�O�?333G�O�?L��G�O�?333G�O�?333G�O�?��?L��G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?L��G�O�G�O�?L��?fffG�O�?�  ?���?�ff?�33?�  ?ٙ�?�33@   @33@   @333@Fff@S33@fff@y��@�33@���@�ff@�33@�  @�ff@�ff@���@ٙ�@�33@�33@���A  AffA��A33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414141414114444414441441141111111111111111111111111111111                                                                                                                                                                                                                                                                                 @ @ �@ �@ {@ �@ !s@ (�@ /�@ 7�@ =q@ FQ@ Q�@ _�@ m�@ {�@ ��@ �0@ ��@ ��@ ��@ ��@ �#@ �@ �@j@b@g@-�@;d@G�@UU@c�@qS@�@��@�H@��@��@@�7@ލ@��@��@�@�@""@0x@>�@Lu@Z@g@uk@�d@�@�@��@��@��@��@�@�@��@
=@�@&�@4�@B8@N�@Z�@i�@x&@�p@��@�@�@�@�c@׹@�@��@  @V@O@'�@6�@E�@R�@_�@k�@z�@�7@��@�(@�~@�&@��@�t@��@� @�@�@g@-@:�@H]@V@c�@r@�@��@��@��@�F@��@є@�;@�@��@%@�@""@/�@<�@K@X�@ff@t@��@��@�@��@��@�J@��@��@�@@�E@
�@�@&�@3�@A�@N�@Z�@i!@x&@��@��@�m@�f@��@�c@�[@�@�Y@ �@@�@(�@5?@C�@Q�@^�@m:@|?@�7@��@��@��@�&@�|@܀@�y@�q@	j@	�@	�@	+�@	9X@	F�@	V@	e	@	r@	�W@	�P@	��@	��@	��@	��@	�7@	�;@	�4@	�,@
�@
�@
""@
/�@
>@
Lu@
X�@
ff@
uk@
�d@
��@
�@
�@
��@
ƨ@
�O@
��@
�@
��@
�@�@&;@33@@�@N�@[z@i!@v�@�@�u@��@�@��@�@׹@�`@�@^@�@�@(�@6�@D�@SI@^�@m�@{�@��@�0@��@�~@��@�|@��@�y@�@j@�@
@,`@;d@I@V�@b�@n�@|�@��@�Y@$�@Yn@��@��@�Y@$�@V�@�+@�R@�(@�@I�@ww@��@��@ �@0x@`�@�@��@�@&;@Z�@�h@ƨ@��@2�@j@�@�@@G�@~�@��@��@�@SI@�+@�@�(@B@G�@v�@��@є@��@(�@R�@z3@�y@�c@�@�@>@e�@��@�9@�#@j@,`@V�@~�@��@��@��@,`@X@��@��@�HG�O�G�O�G�O�G�O�@ ^G�O�@ G�O�@ ^G�O�@ ^G�O�@  �@ G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ G�O�G�O�@ @ �G�O�@ j@ @ v@ %@ �@ 1@ 	�@ 
=@ J@ �@ �@ �@ @ *@ 6@ �@ �@ �@ g@ ""@ #�@ &�@ (G@ +@ -@ 0x@ 2�@ 6�@ 9X@ <@ >�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�  A�A�A�A�A�A�A�A�A�A�A�A�%A�%A�%A�1A�
=A�JA�JA�JA�VA�VA�bA�bA�oA�bA�oA�JA�JA�
=A�
=A�JA�
=A�1A�A�A�A���A���A��A��;A��A���AčPAÝ�A�|�A���A��TA�(�A���A���A���A���A�p�A�M�A���A�dZA�oA�|�A�9XA���A�|�A��A���A�A�A�M�A���A���A�1A�r�A���A��HA���A��A{��AsAp�AlbNAi��Ad�!A_�AX�jAUl�AS7LAQ\)AO/AO�AOVAK�
AGoAD5?AC&�AB�jAB$�A@�`A?��A=\)A;�A:�HA:E�A9dZA9VA8jA6��A6$�A5l�A4jA3��A3�7A2�yA2�A1ƨA1p�A0��A/�TA.�A.��A.�\A.z�A-�A,E�A+�mA+��A+�A*n�A)��A(A�A'&�A&z�A&�A%�PA$�yA$ĜA$��A#��A#�PA#\)A"ȴA"1'A!��A!\)A!�A �`A n�A bAx�A�A��A^5A��A\)A�AȴA�+A^5A1'A�AC�A��A�;A/A��A�DAffAE�A-A��A��A/A�yA�uAE�A��A�A;dA�A�;AdZA�`A�9AVA�#A|�A;dA�yAn�A�TAt�A��AĜAjA9XA�A�A�A�+A�AC�AVA
�yA
��A
A�A	�
A	dZA	A��AZA��A"�A��AZA�A�A�-A�A?}AbNAJA�wA��AK�A;dA�A�A��A$�A��A`BAoA ȴA �uA r�@��@�+@�M�@���@��@���@��@��@���@�33@��@��\@�5?@��h@��j@�I�@��
@���@�K�@��y@��@���@�^5@��@���@�dZ@�
=@�^5@�z�@���@�%@�@�V@��@�hs@�;d@܃@���@���@��
@�^5@�  @�X@� �@�o@ͩ�@�K�@�?}@ȼj@Ə\@�Q�@���@��7@�-@��9@���@�Z@�S�@���@�r�@��F@�S�@���@�Q�@�l�@�~�@�n�@�j@���@���@�x�@�|�@���@���@���@�;d@�ff@���@�A�@�o@�5?@�&�@�1'@�@��+@��@���@���@�=q@�p�@�9X@���@�{@��/@�b@�@���@�%A���A���A���A���A���A���A���A�  A�A�  A�A�A�A�  A�A���A�A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 A���A�  A�A�A�A�A�A�A�A�A�A�A�A�%A�%A�%A�1A�
=A�JA�JA�JA�VA�VA�bA�bA�oA�bA�oA�JA�JA�
=A�
=A�JA�
=A�1A�A�A�A���A���A��A��;A��A���AčPAÝ�A�|�A���A��TA�(�A���A���A���A���A�p�A�M�A���A�dZA�oA�|�A�9XA���A�|�A��A���A�A�A�M�A���A���A�1A�r�A���A��HA���A��A{��AsAp�AlbNAi��Ad�!A_�AX�jAUl�AS7LAQ\)AO/AO�AOVAK�
AGoAD5?AC&�AB�jAB$�A@�`A?��A=\)A;�A:�HA:E�A9dZA9VA8jA6��A6$�A5l�A4jA3��A3�7A2�yA2�A1ƨA1p�A0��A/�TA.�A.��A.�\A.z�A-�A,E�A+�mA+��A+�A*n�A)��A(A�A'&�A&z�A&�A%�PA$�yA$ĜA$��A#��A#�PA#\)A"ȴA"1'A!��A!\)A!�A �`A n�A bAx�A�A��A^5A��A\)A�AȴA�+A^5A1'A�AC�A��A�;A/A��A�DAffAE�A-A��A��A/A�yA�uAE�A��A�A;dA�A�;AdZA�`A�9AVA�#A|�A;dA�yAn�A�TAt�A��AĜAjA9XA�A�A�A�+A�AC�AVA
�yA
��A
A�A	�
A	dZA	A��AZA��A"�A��AZA�A�A�-A�A?}AbNAJA�wA��AK�A;dA�A�A��A$�A��A`BAoA ȴA �uA r�@��@�+@�M�@���@��@���@��@��@���@�33@��@��\@�5?@��h@��j@�I�@��
@���@�K�@��y@��@���@�^5@��@���@�dZ@�
=@�^5@�z�@���@�%@�@�V@��@�hs@�;d@܃@���@���@��
@�^5@�  @�X@� �@�o@ͩ�@�K�@�?}@ȼj@Ə\@�Q�@���@��7@�-@��9@���@�Z@�S�@���@�r�@��F@�S�@���@�Q�@�l�@�~�@�n�@�j@���@���@�x�@�|�@���@���@���@�;d@�ff@���@�A�@�o@�5?@�&�@�1'@�@��+@��@���@���@�=q@�p�@�9X@���@�{@��/@�b@�@���@�%A���A���A���A���A���A���A���A�  A�A�  A�A�A�A�  A�A���A�A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	hsB	ffB	ffB	gmB	gmB	gmB	gmB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	iyB	hsB	hsB	hsB	hsB	hsB	iyB	hsB	iyB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	gmB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	hsB	ffB	S�B	�-B
.B
Q�B
��B
�RB
��B�B5?B-B
��B
�BBDB$�B7LBo�Bv�BP�B
��B
aHB
)�B
-B
<jB
@�B
�B	��B	�/B	ŢB	��B	��B	�+B	|�B	8RB	
=B��B�B�fB�NB�)B�B��B��B�qB�
B�HB�yB��B	1B	\B	�B	+B	33B	49B	<jB	]/B	|�B	�DB	�oB	��B	��B	�B	��B	�
B	�;B	�B	�B
+B
�B
'�B
-B
2-B
8RB
>wB
@�B
G�B
G�B
H�B
K�B
M�B
K�B
J�B
G�B
E�B
E�B
D�B
E�B
G�B
I�B
I�B
J�B
J�B
K�B
M�B
N�B
L�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
P�B
P�B
R�B
T�B
T�B
W
B
W
B
W
B
W
B
W
B
VB
S�B
T�B
T�B
P�B
P�B
O�B
P�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
P�B
P�B
N�B
N�B
M�B
M�B
L�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
G�B
H�B
I�B
I�B
I�B
H�B
G�B
F�B
E�B
C�B
B�B
A�B
A�B
@�B
?}B
>wB
>wB
>wB
>wB
=qB
=qB
9XB
9XB
7LB
6FB
6FB
6FB
6FB
49B
49B
33B
33B
49B
33B
49B
6FB
7LB
7LB
7LB
5?B
5?B
2-B
2-B
2-B
2-B
2-B
0!B
/B
.B
,B
-B
-B
.B
-B
-B
/B
/B
/B
.B
,B
,B
,B
,B
+B
)�B
)�B
(�B
(�B
'�B
$�B
$�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
oB
hB
oB
\B
\B
hB
uB
uB
uB
hB
bB
VB
PB
JB
PB
JB
JB
PB
PB
VB
PB
VB
\B
\B
{B
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
!�B
!�B
"�B
$�B
$�B
%�B
&�B
'�B
'�B
)�B	iyB	iyB	hsB	hsB	iyB	iyB	jB	hsB	hsB	iyB	hsB	hsB	gmB	iyB	hsB	iyB	hsB	iyB	hsB	hsB	gmB	gmB	hsB	hsB	hsB	hsB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	gmB	gmB	gmB	ffB	gmB	hsB	gmB	gmB	gmB	gmB	gmB	gmB	hsB	hsB	gmB	hsB	gmB	hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 B	hMB	f@B	f@B	gHB	gHB	gHB	gHB	hOB	hOB	hOB	hPB	hPB	hQB	hRB	hRB	hSB	iYB	hTB	hUB	hUB	hVB	hVB	i]B	hWB	i^B	hYB	hYB	hZB	h[B	h[B	h\B	h\B	gWB	h^B	h^B	h_B	h_B	h`B	h`B	haB	hbB	fUB	S�B	�B
.B
Q�B
��B
�AB
��B�B5/B,�B
��B
�BB5B$�B7>Bo�Bv�BP�B
��B
a;B
)�B
-B
<^B
@xB
�B	��B	�$B	ŘB	��B	��B	�"B	|�B	8IB	
4B��B�|B�^B�FB�!B�B��B��B�jB�B�BB�tB��B	,B	WB	�B	*�B	30B	47B	<hB	]-B	|�B	�CB	�oB	��B	��B	�B	��B	�B	�>B	�B	�B
0B
�B
'�B
-B
24B
8ZB
>B
@�B
G�B
G�B
H�B
K�B
M�B
K�B
J�B
G�B
E�B
E�B
D�B
E�B
G�B
I�B
I�B
J�B
J�B
K�B
M�B
N�B
L�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
P�B
P�B
SB
UB
UB
W&B
W&B
W'B
W'B
W(B
V#B
TB
UB
UB
QB
QB
PB
QB
RB
SB
TB
SB
SB
SB
SB
SB
SB
RB
QB
QB
OB
OB
M�B
M�B
L�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
G�B
H�B
I�B
I�B
I�B
H�B
G�B
F�B
E�B
C�B
B�B
A�B
A�B
@�B
?�B
>�B
>�B
>�B
>�B
=�B
=�B
9�B
9�B
7�B
6�B
6�B
6�B
6�B
4yB
4zB
3tB
3uB
4{B
3vB
4}B
6�B
7�B
7�B
7�B
5�B
5�B
2uB
2uB
2vB
2vB
2wB
0kB
/fB
._B
,TB
-[B
-[B
.bB
-\B
-]B
/jB
/kB
/lB
.eB
,ZB
,ZB
,[B
,\B
+VB
*QB
*QB
)LB
)LB
(GB
%4B
%5B
$0B
 B
 B
B
	B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
B

B
,B
(B
*B
,B
.B
;B
=B
KB
SB
OB
QB
eB
aB
cB
jB
xB
zB
�B
�B
!�B
 �B
!�B
"�B
"�B
#�B
%�B
%�B
&�B
'�B
(�B
(�B
*�B	iSB	iSB	hMB	hMB	iSB	iSB	jYB	hMB	hMB	iSB	hMB	hMB	gGB	iSB	hMB	iSB	hMB	iSB	hMB	hMB	gGB	gGB	hMB	hMB	hMB	hMB	gGB	gGB	gGB	gGB	gGB	gGB	gGB	f@B	f@B	f@B	f@B	f@B	f@B	f@B	f@B	fAB	gHB	gHB	gHB	fAB	gHB	hNB	gHB	gHB	gHB	gHB	gIB	gIB	hOB	hOB	gIB	hOB	gIB	hOG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202552021061413571420210614135714202107221611132021072216111320210722161113201807242202552021061413571420210614135714202107221611132021072216111320210722161113PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025520180724220255  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025520180724220255QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025520180724220255QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141720210722161417IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                