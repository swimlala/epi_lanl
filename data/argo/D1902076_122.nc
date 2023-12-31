CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   `   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-06-07T03:52:24Z creation;2019-06-10T18:53:13Z conversion to V3.1;2019-09-10T08:31:08Z update;2022-11-10T04:11:49Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  `  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  `  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  `  ?`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  `  A@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  `  C    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  `  E    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  I�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Jp   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Sp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   \p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ep   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    e�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    e�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    e�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  f    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    fP   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    fT   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         fd   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         fh   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        fl   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    fpArgo profile    3.1 1.2 19500101000000  20190607035224  20221119051506  1902076                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA  V4_131540_122                   2C  DDS�ARVOR                           OIN-13JAP-ARL-68                5607A05                         844 @�ûU�l�1   @�þrX� �C���l�D@DS�E���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A#33Ap  A���Ař�A�ffBffB$  B6��BI33B^��Bn��B�  B�33B�33B�ffB���B�ffB���B���B�33B�ffB�33B���B���C�fC  C�3C  CffC��CffC$� C)�3C/ffC3�C8� C=��CC33CG33CQ�3C[ffCe�3Cp33Cy�fC�Y�C�&fC��fC��C�Y�C�33C�s3C��C�@ C�&fC�33C�ffC�L�C�33C�  C�  C��3C��C�&fC�&fC�ٚC�33C�33C��C�33D&fD�D�D� D�D&fD �D$�3D*fD/33D4�D8�fD>&fDB��DG��DM&fDQ��DW�D\fD`��De��Dj�3Do��Dt�fDz  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A#33Ap  A���Ař�A�ffBffB$  B6��BI33B^��Bn��B�  B�33B�33B�ffB���B�ffB���B���B�33B�ffB�33B���B���C�fC  C�3C  CffC��CffC$� C)�3C/ffC3�C8� C=��CC33CG33CQ�3C[ffCe�3Cp33Cy�fC�Y�C�&fC��fC��C�Y�C�33C�s3C��C�@ C�&fC�33C�ffC�L�C�33C�  C�  C��3C��C�&fC�&fC�ٚC�33C�33C��C�33D&fD�D�D� D�D&fD �D$�3D*fD/33D4�D8�fD>&fDB��DG��DM&fDQ��DW�D\fD`��De��Dj�3Do��Dt�fDz  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Ae/AeG�AeO�AeK�AeO�Ae\)AedZAedZAeG�Ae\)Ae;dAe%Ae�Ad�AdĜAdv�AbbNAY`BAR$�ANM�AS�AP�HAPVAK�AJn�AF�AD��ABA�AA\)A<�`A9��A:bA7��A8�`A6v�A5�A5�TA5XA3�A1XA0n�A-|�A)�PA'��A&�A#p�A#�A!t�A =qAp�AZA=qAp�A��A��A�^A"�A�FAA-A
�yA
bA�-A&�A�mA`B@�+@��@���@��m@�Z@���@�O�@�1@�X@�V@��@�@�E�@���@�7L@�ƨ@��m@��j@���@���@��!@��@��@�?}@�v�@�\)@�V@�l�@��@{t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Ae/AeG�AeO�AeK�AeO�Ae\)AedZAedZAeG�Ae\)Ae;dAe%Ae�Ad�AdĜAdv�AbbNAY`BAR$�ANM�AS�AP�HAPVAK�AJn�AF�AD��ABA�AA\)A<�`A9��A:bA7��A8�`A6v�A5�A5�TA5XA3�A1XA0n�A-|�A)�PA'��A&�A#p�A#�A!t�A =qAp�AZA=qAp�A��A��A�^A"�A�FAA-A
�yA
bA�-A&�A�mA`B@�+@��@���@��m@�Z@���@�O�@�1@�X@�V@��@�@�E�@���@�7L@�ƨ@��m@��j@���@���@��!@��@��@�?}@�v�@�\)@�V@�l�@��@{t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�hB�bB�bB�bB�bB�bB�\B�\B�PB�\B�VB�JB�JB�1B�+B�Bw�BZB6FBM�B��B��B��B�}B�RB�B�}B�}BB�JBw�B� BjB�Bs�Bt�Bq�Bl�B`BBI�B?}B!�B
��B
�B
�;B
ȴB
��B
��B
��B
��B
ĜB
�?B
�B
��B
��B
��B
�uB
�PB
�B
y�B
y�B
s�B
iyB
W
B
N�B
>wB
33B
"�B
�B
�B
DB	��B	�HB	�
B	��B	�XB	��B	�\B	o�B	gmB	cTB	aHB	hsB	ffB	jB	m�B	x�B	x�B	�B	�PB	��B	��B	�B	�'B	��B	ɺ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�NB�bB�bB�bB�bB�bB�\B�\B�PB�vB�pB�JB�dB�KB�_B�-B|�B]�B8BK^B�)B�oB�<B� B�B�B��B�4B�B�<Bw�B�UBj0B�?BtBt�Bq�BmCBa|BJrB@4B"�B
�jB
��B
��B
��B
�JB
�BB
��B
ΥB
�9B
��B
�QB
�RB
�@B
��B
��B
��B
��B
zDB
zB
tnB
j0B
WsB
O�B
?B
4B
#:B
B
�B
�B	�zB	�B	�?B	�<B	�*B	�/B	�HB	p!B	g�B	c�B	a�B	h�B	f�B	j�B	m�B	y	B	x�B	�MB	��B	��B	��B	�IB	�[B	��B	ɺ311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.1(dbar); PO2=-0.1(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906210015332019062100153320190621001533202210251259462022102512594620221025125946201906220011362019062200113620190622001136  JA  ARFMdecpV4_b                                                                20190607035223  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190607035224  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190607035224  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190607035225  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190607035225  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190607035226  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20190607035623                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190610185218  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190610185310  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190610185311  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190610185311  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190610185311  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190610185312  QCP$                G�O�G�O�G�O�               0JA      jafc1.0                                                                 20190610185313                      G�O�G�O�G�O�                JA  ARUP                                                                        20190610185636                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190611000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20190620151533  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190620151533  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190621151136  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190912041517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025035946  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221119051506                      G�O�G�O�G�O�                