CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-07-11T19:05:40Z creation;2017-07-11T19:05:42Z conversion to V3.1;2019-09-10T08:59:38Z update;2022-07-26T02:48:15Z update;     
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
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Et   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  GD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  KX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    iD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  iH   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20170711190540  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  V4_131545_043                   2C  Dd�lARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @��O���1   @��Ϳ��@3�n��P�d�l�C��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�ffA+33A|��A�33A���A�ffBffB#��B3��BJffB_33Bu��B�  B�ffB�ffB���B�33B���B�33B���Bҙ�B���B癚B�  B���C  CL�CL�C� CL�C  C�3C%�fC*�C/�fC4�C9� C=��CC� CH� CRL�C[�3Cf��Cp�Cy�3C��fC�� C�ffC�@ C�L�C��C��C�&fC�� C�Y�C��3C�ٚC��3C�&fC�@ C�&fC�  C�  C���C���C��fC��C�Y�C��3C�&fD��D,�D&fD&fD�D�3D &fD$��D)ٚD/,�D49�D9fD>&fDC,�DH  DL�3DQ��DW  D\,�Da  Df  Dj��Dp&fDt�3Dz�D�Y�D��3D�� D�fD�L�D�� D�ٚD�	�D�` D���D��fD�3D�\�D�y�D��fD���D�P D��D��fD�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�ffA+33A|��A�33A���A�ffBffB#��B3��BJffB_33Bu��B�  B�ffB�ffB���B�33B���B�33B���Bҙ�B���B癚B�  B���C  CL�CL�C� CL�C  C�3C%�fC*�C/�fC4�C9� C=��CC� CH� CRL�C[�3Cf��Cp�Cy�3C��fC�� C�ffC�@ C�L�C��C��C�&fC�� C�Y�C��3C�ٚC��3C�&fC�@ C�&fC�  C�  C���C���C��fC��C�Y�C��3C�&fD��D,�D&fD&fD�D�3D &fD$��D)ٚD/,�D49�D9fD>&fDC,�DH  DL�3DQ��DW  D\,�Da  Df  Dj��Dp&fDt�3Dz�D�Y�D��3D�� D�fD�L�D�� D�ٚD�	�D�` D���D��fD�3D�\�D�y�D��fD���D�P D��D��fD�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���Aܲ-Aܩ�Aܝ�Aܛ�AܑhA�|�A�|�A�x�A�v�A�v�A�n�AځA��#A֣�Aԕ�Aө�A���A�Q�A��A�n�AȾwA��A�G�A�$�A�=qA�(�A�x�A�7LA���A�ĜA��FA���A�33A�hsA�$�A�I�A�?}A�VA�bNA��A�jA���A���A~v�Av-AnZAgAZ �AS�mAJI�A>bA8-A3��A-|�A(r�A#�Ar�A�Av�A�A33AoA�j@���@���@�S�@��
@�!@��
@�ƨ@�J@Η�@�V@�o@�`B@��F@�dZ@��D@���@�|�@��@��@���@�hs@�;d@�A�@�-@�Ĝ@�@���@�z�@��@��@�-@�/@���@z��@o�P@fE�@]�@TZ@P �@H�9@A&�@:M�@4(�@0 �@*=q@%�@|�@7L@��@�;@��@
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���Aܲ-Aܩ�Aܝ�Aܛ�AܑhA�|�A�|�A�x�A�v�A�v�A�n�AځA��#A֣�Aԕ�Aө�A���A�Q�A��A�n�AȾwA��A�G�A�$�A�=qA�(�A�x�A�7LA���A�ĜA��FA���A�33A�hsA�$�A�I�A�?}A�VA�bNA��A�jA���A���A~v�Av-AnZAgAZ �AS�mAJI�A>bA8-A3��A-|�A(r�A#�Ar�A�Av�A�A33AoA�j@���@���@�S�@��
@�!@��
@�ƨ@�J@Η�@�V@�o@�`B@��F@�dZ@��D@���@�|�@��@��@���@�hs@�;d@�A�@�-@�Ĝ@�@���@�z�@��@��@�-@�/@���@z��@o�P@fE�@]�@TZ@P �@H�9@A&�@:M�@4(�@0 �@*=q@%�@|�@7L@��@�;@��@
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
S�B
`BB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
e`B
hsB�B�B��B��B��BhB�B6FBo�B}�BXB�=B��Bl�B��B�RB��B��B�oB�VB�JB�%B�Bu�B�BS�BoB�^B��BVB
�NB
�VB
>wB
�B	�5B	�qB	q�B	K�B	�B�B�BB��B�wB�-B��B��B�hB�hB�{B�=B�7B�7B�B�B�B��B��B�B�B��B�B��B�B��B	�B	7LB	F�B	VB	k�B	z�B	�7B	��B	��B	�FB	�^B	ƨB	��B	�NB	�B	�B	��B
B
JB
\B
�B
�B
'�B
/B
5?B
<jB
?}B
E�B
K�B
Q�B
W
B
[#B
_;B
cTB
hsB
n�B
r�B
u�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
TFB
`BB
bNB
bNB
bhB
bhB
bNB
bNB
cTB
dZB
ezB
kB�B�CBևB��B;B&B�B8�Bp�B� BX�B��B��Bn}B��B�B��B��B��B�B��B�B��Bv�B��BW?B$B�"B��BZ�B
�B
�NB
@�B
yB	��B	��B	s�B	N<B	�B�TB�|BуB��B��B�DB��B�oB�oB�9B��B��B��B��B��B��B��B�
B��B�B�nB��B�^B�CB�B	�B	7�B	F�B	VSB	k�B	{dB	�lB	��B	�B	��B	��B	��B	�B	�B	��B	��B	�.B
SB
dB
vB
�B
�B
(
B
/5B
5ZB
<�B
?�B
E�B
K�B
RB
W$B
[=B
_;B
cnB
h�B
n�B
r�B
u�B
w�B
z�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707220016082017072200160820170722001608202207232055592022072320555920220723205559202207261121282022072611212820220726112128  JA  ARFMdecpV4_b                                                                20170711185245  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170711190540  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170711190541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170711190542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170711190542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170711190542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170711190542                      G�O�G�O�G�O�                JA  ARUP                                                                        20170711191424                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170712000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20170721151608  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170721151608  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050608  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115559  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                