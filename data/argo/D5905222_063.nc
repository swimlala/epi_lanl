CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-18T06:52:19Z creation;2020-02-21T15:52:48Z conversion to V3.1;2022-11-10T04:20:39Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  EX   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Il   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    il   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200218065219  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ?A   JA  V4_131533_063                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��wF� 1   @��q5y�@0�dZ��d����S�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�  A)��A{33A���A�33A�33B��B��B3��BHffB]33Bk��B�33B�33B�33B�ffB�33B�33B�ffB���B���Bܙ�B癚B���B���C� C�fC�3C  C�3C�C �3C%�C*  C/�C5�C9ffC>�3CC��CH�CRffC[ffCe��Cp33CzL�C�� C�33C�L�C�  C�33C�ffC��C��3C��fC�ffC�33C�@ C�ٚC³3CǙ�C�33C��fC�L�C��C�L�C�@ C�s3C�@ C�33C�33D�3D33DfD3D,�DٚD�fD%&fD)��D/3D43D9fD=� DC  DHfDM  DR,�DW9�D[��Da  Df�Dk&fDp�Dt�fDz  D�L�D�vfD�� D�fD�S3D�|�D���D��D�VfD��3D���D�� D�<�DԆfD��fD�  D�S3D�3D�ٚD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�  A)��A{33A���A�33A�33B��B��B3��BHffB]33Bk��B�33B�33B�33B�ffB�33B�33B�ffB���B���Bܙ�B癚B���B���C� C�fC�3C  C�3C�C �3C%�C*  C/�C5�C9ffC>�3CC��CH�CRffC[ffCe��Cp33CzL�C�� C�33C�L�C�  C�33C�ffC��C��3C��fC�ffC�33C�@ C�ٚC³3CǙ�C�33C��fC�L�C��C�L�C�@ C�s3C�@ C�33C�33D�3D33DfD3D,�DٚD�fD%&fD)��D/3D43D9fD=� DC  DHfDM  DR,�DW9�D[��Da  Df�Dk&fDp�Dt�fDz  D�L�D�vfD�� D�fD�S3D�|�D���D��D�VfD��3D���D�� D�<�DԆfD��fD�  D�S3D�3D�ٚD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA˼jA˲-A˝�A˗�A˙�A˝�A˝�Aˡ�Aˡ�A˝�A˝�A˟�Aˡ�Aˣ�Aˣ�A˥�Aˡ�A�n�A�AȮAƇ+A��A�%A�-A�"�A���A�\)A�v�A��;A��+A�7LA�l�A���A��yA���A��A��mA��mA�p�A|�yAg�A]l�AR��AE�A=&�A2��A+�#A%��A$1A =qA�A�`AbNA��A=qAhsAhsA��AoA�+AjA�wAhsA�+@��h@�t�@��@�t�@��@�z�@�M�@�ȴ@�hs@Լj@Ͼw@˅@�K�@�n�@���@�1'@��w@�j@���@� �@��@��#@�\)@�z�@���@�33@�%@��@��/@��R@��@���@�p�@+@tI�@k��@co@YX@N�R@GK�@?�@8r�@0��@*��@&�@"=q@��@�^@�h@G�@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA˼jA˲-A˝�A˗�A˙�A˝�A˝�Aˡ�Aˡ�A˝�A˝�A˟�Aˡ�Aˣ�Aˣ�A˥�Aˡ�A�n�A�AȮAƇ+A��A�%A�-A�"�A���A�\)A�v�A��;A��+A�7LA�l�A���A��yA���A��A��mA��mA�p�A|�yAg�A]l�AR��AE�A=&�A2��A+�#A%��A$1A =qA�A�`AbNA��A=qAhsAhsA��AoA�+AjA�wAhsA�+@��h@�t�@��@�t�@��@�z�@�M�@�ȴ@�hs@Լj@Ͼw@˅@�K�@�n�@���@�1'@��w@�j@���@� �@��@��#@�\)@�z�@���@�33@�%@��@��/@��R@��@���@�p�@+@tI�@k��@co@YX@N�R@GK�@?�@8r�@0��@*��@&�@"=q@��@�^@�h@G�@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	DB	�B	�?B	�qB
@�B
ÖB.B��B��B�B^5BJ�B
�yB
��B
��B
�-B�BffB?}BB
��B
&�B	�B	jB	@�B��B�B��B�^B�XBƨB��B�B�B�B��B	\B	!�B	K�B	`BB	l�B	�B	�B	�B	~�B	� B	�1B	� B	�B	�=B	�\B	��B	��B	�B	�LB	�XB	ŢB	ȴB	�B	�;B	�`B	�mB	�B	�B	��B	��B
  B
B
%B
	7B
PB
\B
oB
�B
�B
�B
�B
�B
'�B
,B
49B
=qB
B�B
F�B
N�B
S�B
YB
_;B
dZB
hsB
m�B
p�B
t�B
v�B
z�B
~�B
�B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	 OB	�B	jB	��B	�mB
D3B
ÖB0UB��B�'B�9Bb4BO�B
��B
�[B
�fB
��B�Bh�BCaB�B
�fB
,�B	ߤB	m]B	CGB	UB�+B�vB�jB��B�EB��B�B�)B�cB��B	�B	"B	LJB	a-B	lqB	��B	�B	��B	�B	�OB	�7B	�OB	��B	�B	��B	�B	�TB	��B	��B	��B	�B	�B	�SB	ߊB	�B	�B	��B	��B	�+B	�6B
 4B
aB
YB
	lB
�B
�B
�B
�B
�B
�B
�B
�B
(
B
,=B
4nB
=�B
B�B
F�B
OB
S�B
Y1B
_VB
dtB
hsB
m�B
p�B
t�B
v�B
z�B
B
�'B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003030016002020030300160020200303001600202210251312062022102513120620221025131206202210251807142022102518071420221025180714  JA  ARFMdecpV4_b                                                                20200218065218  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200218065219  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200218065219  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200218065220  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200218065220  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200218065220  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200218065430                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200221155218  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200221155246  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200221155246  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200221155247  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200221155247  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200221155247  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200221155248                      G�O�G�O�G�O�                JA  ARUP                                                                        20200221155334                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200224151600  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20200224151600  QCF$                G�O�G�O�G�O�300000          JM  ARCAJMQC2.0                                                                 20200302151600  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200302151600  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041206  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090714  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                