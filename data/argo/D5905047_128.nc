CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-05T06:52:39Z creation;2019-11-08T21:53:46Z conversion to V3.1;2022-07-26T02:44:48Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20191105065239  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_128                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @��{�~� 1   @�鉦� @6;"��`B�c���"��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�ff@�ffA0  Ak33A�33A�  A�33B33B ffB933BI33B[33Br��B�  B�  B���B�ffB�  B���B���Bə�B�ffB�ffB�  B���B�ffC�C  C��C� C� C  C��C$L�C*� C/�C3�3C833C>ffCCffCI�CRffC\ffCf��Cp33Cz�C�ffC�� C�ffC�@ C�� C�@ C���C�  C��fC�Y�C���C�L�C�Y�C��fC��C�Y�C�s3C�s3C�ٚC�L�C�L�C�fC��fC��C�Y�D  D  D@ D&fDfD� D�3D%  D)��D.� D4  D9,�D>fDC33DHfDL��DQ� DWfD\�D`ٚDf3DkfDp�Du&fDz  D�C3D��3D��3D���D�FfD�� D�ɚD���D�P D�y�D���D�fD�@ DԠ D�ɚD��D�S3D�fD�ɚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�ff@�ffA0  Ak33A�33A�  A�33B33B ffB933BI33B[33Br��B�  B�  B���B�ffB�  B���B���Bə�B�ffB�ffB�  B���B�ffC�C  C��C� C� C  C��C$L�C*� C/�C3�3C833C>ffCCffCI�CRffC\ffCf��Cp33Cz�C�ffC�� C�ffC�@ C�� C�@ C���C�  C��fC�Y�C���C�L�C�Y�C��fC��C�Y�C�s3C�s3C�ٚC�L�C�L�C�fC��fC��C�Y�D  D  D@ D&fDfD� D�3D%  D)��D.� D4  D9,�D>fDC33DHfDL��DQ� DWfD\�D`ٚDf3DkfDp�Du&fDz  D�C3D��3D��3D���D�FfD�� D�ɚD���D�P D�y�D���D�fD�@ DԠ D�ɚD��D�S3D�fD�ɚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��/A�ĜAܰ!A��mAպ^A�ffA���A��
AɑhA��yA���A�`BA�A�A���A��#A���A���A��/A�JA�C�A�A�oA���A��A���A�"�A�C�A��-A��mA��A��A�jA���A��A���A��!A�33A���A���Ax1Aot�AhZA_|�AS33AO�AH9XAD1A>ȴA<�A0�\A,^5A&(�A!;dA�AO�A7LA��AO�A\)Az�A|�A��@���@�1'@�j@�@�ȴ@�b@ۅ@�b@�X@�ȴ@�C�@�\)@� �@���@�M�@�v�@��;@�|�@�  @�%@��@���@��@��^@�b@���@�;d@�(�@��w@�O�@��@�t�@�X@{C�@st�@g�;@`�@W;d@O��@J�@A�#@;�
@6ȴ@0�9@,1@'�@#�@K�@�\@�-@bN@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��/A�ĜAܰ!A��mAպ^A�ffA���A��
AɑhA��yA���A�`BA�A�A���A��#A���A���A��/A�JA�C�A�A�oA���A��A���A�"�A�C�A��-A��mA��A��A�jA���A��A���A��!A�33A���A���Ax1Aot�AhZA_|�AS33AO�AH9XAD1A>ȴA<�A0�\A,^5A&(�A!;dA�AO�A7LA��AO�A\)Az�A|�A��@���@�1'@�j@�@�ȴ@�b@ۅ@�b@�X@�ȴ@�C�@�\)@� �@���@�M�@�v�@��;@�|�@�  @�%@��@���@��@��^@�b@���@�;d@�(�@��w@�O�@��@�t�@�X@{C�@st�@g�;@`�@W;d@O��@J�@A�#@;�
@6ȴ@0�9@,1@'�@#�@K�@�\@�-@bN@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB�B��B�dB�^B�LB��B�9B�B��Bt�BiyBVBu�B�?BC�BQ�BT�Bk�Bv�Bm�B^5BH�B@�BC�BL�BM�BD�B>wB-B"�B{BbB��B�HBɺB�-B�7B.B
ƨB
��B
w�B
�B	�B	ƨB	�uB	T�B	9XB	�B��B�sB�B��B�hB�B�B|�Bs�BffBcTB`BBT�BJ�B<jBE�BG�BL�BK�BT�B]/B|�B��B�fB�B	hB	\B	%�B	B�B	R�B	bNB	v�B	y�B	�VB	��B	�FB	�}B	ɺB	ŢB	��B	��B	�B	�NB	�yB	�B	��B
B
	7B
oB
�B
'�B
1'B
5?B
?}B
C�B
H�B
P�B
VB
ZB
^5B
cTB
gmB
k�B
o�B
s�B
x�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1�B��B��B��B��B��B��B��B��Be�B^�BG�BdZB��B2�BB�BH1B[=Bf�B]�BP�B9$B/iB3B;�B>BB49B.cB�B&B{BB��B��B�rB�NBz�B �B
�8B
�mB
i�B

rB	��B	��B	�9B	D�B	*B		RB�B��B�)B�B��BshBq[Bl�BdZBU�BRoBP.BD�B;�B,"B5�B8B<�B;�BD�BL�Bl"B��B�gB��B	 �B��B	�B	1�B	BB	QhB	e�B	h�B	}qB	��B	�FB	�cB	��B	��B	��B	��B	��B	�4B	ؓB	�B	�B	��B	�B
;B
~B
�B
�B
$&B
.IB
2aB
7�B
?�B
D�B
H�B
MB
R B
V9B
ZQB
^OB
b�B
g�B
m�B
r�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.017(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201911190015262019111900152620191119001526202207232057162022072320571620220723205716202207261127062022072611270620220726112706  JA  ARFMdecpV4_b                                                                20191105065238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191105065239  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191105065240  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191105065240  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191105065240  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191105065241  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20191105065506                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20191108215306  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191108215344  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191108215344  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191108215345  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191108215345  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191108215345  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191108215346                      G�O�G�O�G�O�                JA  ARUP                                                                        20191108215433                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191109000000  CF  PSAL_ADJUSTED_QC?�ff?�ffG�O�                JM  ARCAJMQC2.0                                                                 20191118151526  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191118151526  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115716  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                