CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   +   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2004-04-16T02:49:14Z creation;2011-12-05T02:55:48Z update;2015-06-25T03:18:52Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          74   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7D   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7H   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    8    DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    80   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8@   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8`   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8d   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8h   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8�   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9    PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z         �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure        �  :�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  ;�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  ;�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature         �  <h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  =   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature         �  =@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  =�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  >   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity        �  >�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  ?p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity        �  ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  @H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  @t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  A    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   A�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   J�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   S�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  \�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ]0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ]4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ]8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ]<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ]@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ]�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ]�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ]�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ]�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ]�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ]�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ]�Argo profile    3.1 1.2 19500101000000  29025   SAGE                                                            Nobuyuki SHIKAMA                                                PRES            TEMP            PSAL               �A   JA  20040416024914  20150625072518  A1_25335_201                    2C  D   PALACE                          74                              090999                          846 @�]/r(1   @�]/ĥ[g@C�&�x���c���`A�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��AfffA�ffA�33B��B133BX��B�  B���B���B�33B�33B�33B���CL�CffCL�C$L�C.ffC8ffCBL�CL�CV33C`  Cj33Ct33C~�C�  C�33C�&fC��C�&fC�@ C��C��C��C��C�&fC�@ C�L�C�&fC��1111111111111111111111111111111111111111111 @���A33AX  A�33A�  B33B-��BU33B~ffB���B�  B�ffB�ffB�ffB�  CffC� CffC#ffC-� C7� CAffCK33CUL�C_�CiL�CsL�C}33C���C�� C��3C��fC��3C���C��fC���C���C��fC��3C���C�ٚCĳ3Cɦf1111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A+7LA+/A+;dA+�A)��A(�HA)�FA)�A)��A)S�A)�A)�-A*(�A*�uA,A+��A)�A&�A"��A�A5?A��AȴA��AoAXAQ�Ar�A�-A�;A
(�A�HAȴA5?A��A Q�@��m@�w@�n�@�u@���@�V@�+1111111111111111111111111111111111111111111 A+7LA+/A+;dA+�A)��A(�HA)�FA)�A)��A)S�A)�A)�-A*(�A*�uA,A+��A)�A&�A"��A�A5?A��AȴA��AoAXAQ�Ar�A�-A�;A
(�A�HAȴA5?A��A Q�@��m@�w@�n�@�u@���@�V@�+1111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�EB�B��B�B�GB�
B�9B�9B�B�IB��B�BBU�B��B�4B�_B~]B^BIB,=B.}B\B%B+�B*eBA;BD�BO�BO�BG_B?.B9$B0�B'RB�B�BYB B�^B�O1111111111111111111111111111111111111111111 B�1B�1B�1B�7B�hB��BÖB�
B�
B�B�HB�/B�fB�BDBW
B�B��B�B� B_;BJ�B-B0!B\B%�B-B+BB�BE�BP�BP�BH�B@�B:^B2-B(�B�B\B+BB��B�1111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP(NextCycle) where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                        TEMP_ADJ = TEMP                                                                                                                                                                                                                                                 PSAL_ADJ = RecalS = psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.9(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201110311012012011103110120120111031101201201111020144052011110201440520111102014405201111150132332011111501323320111115013233  JA  ARFMfmtp2.0                                                                 20040416024914  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.2                                                                 20040416024914  QCP$                G�O�G�O�G�O�            FB7CJA  ARFMfmtp2.2                                                                 20060602004536  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060602004536  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060602004537  QCP$                G�O�G�O�G�O�           1F6BCJA  ARUP                                                                        20060602035720                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060623003541  QCP$                G�O�G�O�G�O�            FB7CJA  ARGQaqcp2.5                                                                 20060623003541  QCP$                G�O�G�O�G�O�            FB40JA  ARUP                                                                        20060623012450                      G�O�G�O�G�O�                JA  ARFMdecpA1_a                                                                20090325025955  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090325042114  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090325042115  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090325042115  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090325042116  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090325042116  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090325042116  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090325042116  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090325042116  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20090325043318                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20111031101201  CV  JULD            G�O�G�O�F��|                JM  ARSQJMQC1.0                                                                 20111031101201  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20111031101201  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20111102014405  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V2                                                       20111115013233  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20111205025324  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20111205025548                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150625031849                      G�O�G�O�G�O�                JA  ARDU                                                                        20150625072518                      G�O�G�O�G�O�                