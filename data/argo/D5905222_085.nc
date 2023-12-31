CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-09-26T06:52:04Z creation;2020-09-29T21:53:09Z conversion to V3.1;2022-11-10T04:19:43Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200926065204  20221117231507  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               UA   JA  V4_131533_085                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�:�0[�1   @�;N� @/Õ�$��dK"��`B1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A$��Aq��A���A�33A���B
��B ffB6��BI33BZ��Bq��B�ffB���B�  B���B�  B�ffB�  B�33B���B�33B���B�B�33CffC� C�3C�3C�C�fC �C%33C*��C.��C4� C9L�C>L�CCL�CHffCQ33C\��Cf��Cq  Cy� C���C���C���C�� C�&fC�Y�C�33C�33C�Y�C�  C�&fC��3C���C¦fC��3C�ffC�ٚC�&fC�@ C��C�L�C��C�� C�ffC�s3DfDfDfD�D  D�3D 3D%  D*9�D/  D4  D9,�D>  DC&fDH,�DM�DR�DW3D\&fDa,�Df  Dk33Dp&fDu�Dz�D�C3D���D�ٚD���D�I�D��3D���D�3D�33D���D�ɚD��D�<�DԆfD���D� D�L�D��D��D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A$��Aq��A���A�33A���B
��B ffB6��BI33BZ��Bq��B�ffB���B�  B���B�  B�ffB�  B�33B���B�33B���B�B�33CffC� C�3C�3C�C�fC �C%33C*��C.��C4� C9L�C>L�CCL�CHffCQ33C\��Cf��Cq  Cy� C���C���C���C�� C�&fC�Y�C�33C�33C�Y�C�  C�&fC��3C���C¦fC��3C�ffC�ٚC�&fC�@ C��C�L�C��C�� C�ffC�s3DfDfDfD�D  D�3D 3D%  D*9�D/  D4  D9,�D>  DC&fDH,�DM�DR�DW3D\&fDa,�Df  Dk33Dp&fDu�Dz�D�C3D���D�ٚD���D�I�D��3D���D�3D�33D���D�ɚD��D�<�DԆfD���D� D�L�D��D��D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��
A�%A��A��`A��;A��
A���A�ȴA�A�ĜA�ĜA�ĜA�ƨA�Aڗ�A�G�A���A���A�VAȮAŴ9A¸RA�1A�-A�\)A��yA��A��#A�K�A���A�l�A�dZA��9A�  A�n�A�+A��A���A�VA�;dA�
=A�\)A%Ak��AZ9XAI��AA�A;��A2�\A)"�A#�AVA�Ax�A/A�;AK�A	�-A��A�AbNA�@�"�@�\)@�bN@�$�@��
@��@�V@��-@��@�1@߅@ڗ�@�?}@�b@̋D@Ɂ@ũ�@�&�@��!@�b@��@���@�33@���@�x�@��@���@���@��@��9@��@�n�@���@��w@�V@��@�l�@{o@q7L@g;d@]��@R�\@J^5@A�#@:�!@4I�@-@(��@"�@O�@��@��@�^@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��
A�%A��A��`A��;A��
A���A�ȴA�A�ĜA�ĜA�ĜA�ƨA�Aڗ�A�G�A���A���A�VAȮAŴ9A¸RA�1A�-A�\)A��yA��A��#A�K�A���A�l�A�dZA��9A�  A�n�A�+A��A���A�VA�;dA�
=A�\)A%Ak��AZ9XAI��AA�A;��A2�\A)"�A#�AVA�Ax�A/A�;AK�A	�-A��A�AbNA�@�"�@�\)@�bN@�$�@��
@��@�V@��-@��@�1@߅@ڗ�@�?}@�b@̋D@Ɂ@ũ�@�&�@��!@�b@��@���@�33@���@�x�@��@���@���@��@��9@��@�n�@���@��w@�V@��@�l�@{o@q7L@g;d@]��@R�\@J^5@A�#@:�!@4I�@-@(��@"�@O�@��@��@�^@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�B	��B
{B
"�B
>wB
�B
8RB
v�B
�/B
��B�B`BB}�B��B�qB��B��BǮB�B�BB��B�wB�JBv�BaHBC�B�B
��B
B	�bB	=qB	B	B��B�B�HB�B	
=B	oB	#�B	33B	C�B	VB	P�B	o�B	x�B	v�B	x�B	~�B	|�B	�B	��B	�B	�3B	�RB	��B	ÖB	ɺB	��B	�B	�;B	�ZB	�sB	�B	�B	�B	��B	��B	��B	��B
B
B
%B
B
DB
	7B
JB
\B
�B
�B
�B
�B
&�B
-B
5?B
<jB
C�B
I�B
P�B
W
B
\)B
bNB
ffB
l�B
r�B
v�B
z�B
|�B
�B
�B
�7B
�D31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�B
fB
�B
5B
�B
,�B
k�B
ϑB
�B�BRTBp�B�9B�!B�B�B��B�B�{B��B��B�Bi�BU2B6�B2B
��B	�dB	�?B	4B�>B�B��BޞB�FB�B�B	�B	B	%,B	6`B	H�B	C-B	a-B	k6B	i*B	kB	qB	o B	uB	�~B	��B	�`B	��B	��B	��B	��B	��B	�B	�4B	�B	�7B	�]B	�vB	�nB	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	�B
;B
KB

rB

rB
vB
�B
�B
&�B
./B
5?B
;B
B�B
H�B
M�B
S�B
XB
^5B
dZB
hsB
l�B
n�B
r�B
v�B
z�B
|�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.014(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202010100016102020101000161020201010001610202210251312282022102513122820221025131228202210251808402022102518084020221025180840  JA  ARFMdecpV4_b                                                                20200926065203  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200926065204  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200926065204  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200926065206  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200926065206  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200926065206  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200926065326                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200929215226  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200929215307  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200929215307  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200929215308  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200929215308  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200929215308  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200929215309                      G�O�G�O�G�O�                JA  ARUP                                                                        20200929215354                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200929151615  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20200929151615  QCF$                G�O�G�O�G�O�200000          JM  ARCAJMQC2.0                                                                 20201009151610  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201009151610  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041228  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090840  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231507                      G�O�G�O�G�O�                