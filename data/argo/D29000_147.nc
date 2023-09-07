CDF   #   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   ,   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2004-05-21T00:45:52Z creation;2015-04-27T01:59:40Z conversion to V3.1;2019-04-22T05:15:33Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z         �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  :L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  :x   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  ;(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  ;T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  <   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  <0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  <�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  =   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  =�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  =�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  ,  >�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  >�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  ?t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  @$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  @�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ad   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Jd   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   Sd   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  \d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    \�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    \�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    \�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    \�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  \�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ]4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ]D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ]H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ]X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ]\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ]`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ]dArgo profile    3.1 1.2 19500101000000  20040521004552  20190422101515  29000   SAGE                                                            Nobuyuki SHIKAMA                                                PRES            TEMP            PSAL               �A   JA  M1_001971_147                   2C  D   PALACE                          134                             030298                          846 @�e�\Y��1   @�eթ&N @C���Q��d`A�7L1   ARGOS   A   B   D   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   @&ff@`  A��A���A陚B33BDffBm33B���B�  B�33B�33B���B���CL�C� C�fCffC)ffC3L�C=33CGL�CQffCZ��Ce  Cn��Cy  C���C���C�� C�s3C�s3C��3C�� C���C�s3C�Y�C���C���C��3C�ffCǌ�C�fC�ٚ11111111111111111111111111111111111111111144@���@ə�A>ffA�33B   B(ffBO��BxffB�ffB���B���B���B�ffB�ffC�CL�C�3C"33C,33C6�C@  CJ�CT33C]��Cg��Cq��C{��C�  C��3C��fC�ٚC�ٚC��C��fC��3C�ٚC�� C�  C��3C��C���C��3C��C�@ 11111111111111111111111111111111111111111111A<bNA<bNA<ffA3�FA.A�A+"�A++A*��A%33A!�hA#t�A#`BA"�A"ĜA#S�A$�A"��A�Ar�A�HA��A�^A�A��A�A�#A7LA�FA
VA	dZAG�A$�A �@�9X@���@��@�bN@�@�V@�A�@���@�&�@���A7�h11111111111111111111111111111111111111111144A<bNA<bNA<ffA3�FA.A�A+"�A++A*��A%33A!�hA#t�A#`BA"�A"ĜA#S�A$�A"��A�Ar�A�HA��A�^A�A��A�A�#A7LA�FA
VA	dZAG�A$�A �@�9X@���@��@�bN@�@�V@�A�@���@�&�@���G�O�11111111111111111111111111111111111111111114BɆB��B��B0�BCB�B�BJB�B�_B�B2GB^�Bs�B�B��B��B��B$�B)�B�BB
#BuB�6B�>B'BDB�B�B�B B�B��B�5B�B�B�}B��B�rB��B��A��B.}11111111111111111111111111111111111111111144B�KB��B��B0�B�B0BbB~B[B�B
�B1'B]/BrB�RB�8B�;B�B$�B)DB�B4G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111113334444444444444444444444@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611102228272016111022282720161110222827201611120200572016111202005720161112020057201804060457042018040604570420180406045704  JA  ARFMfmtp2.0                                                                 20040521004552  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.2                                                                 20040521004555  QCP$                G�O�G�O�G�O�            FB7CJA  ARGQrqcp2.2                                                                 20040521004555  QCF$                G�O�G�O�G�O�            4800JA  ARFMfmtp2.2                                                                 20060612021103  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060612021104  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060612021104  QCP$                G�O�G�O�G�O�           1F6BCJA  ARGQrqcp2.4                                                                 20060612021104  QCF$                G�O�G�O�G�O�            9000JA  ARUP                                                                        20060612050420                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060619051831  QCP$                G�O�G�O�G�O�            FB7CJA  ARGQrqcp2.5                                                                 20060619051831  QCF$                G�O�G�O�G�O�            4800JA  ARGQaqcp2.5                                                                 20060619051831  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.5                                                                 20060619051831  QCF$                G�O�G�O�G�O�            4800JA  ARUP                                                                        20060619070454                      G�O�G�O�G�O�                JA  ARFMdecpM1_a                                                                20090310060639  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090310063128  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090310063129  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090310063129  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090310063130  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcpt19b                                                                20090310063130  QCF$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090310063130  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090310063130  QCF$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090310063130  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090310063130  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090310063130  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20090310070213                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150427015940                      G�O�G�O�G�O�                JA  ARUP                                                                        20150427060514                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20161110000000  CF  PRES_ADJUSTED_QCC�fC�ٚG�O�                JM  ARSQJMQC2.0                                                                 20161110000000  CF  TEMP_ADJUSTED_QCC�fC�fG�O�                JM  ARCAJMQC2.0                                                                 20161110132827  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110132827  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20161111170057  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180403000000  CF  PSAL_ADJUSTED_QCC3L�Cǌ�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405195704  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190422101515                      G�O�G�O�G�O�                