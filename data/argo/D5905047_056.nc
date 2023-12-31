CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-11-18T18:54:03Z creation;2017-11-18T18:54:04Z conversion to V3.1;2019-09-10T08:57:50Z update;2022-07-26T02:47:44Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20171118185403  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               8A   JA  V4_131545_056                   2C  Dd�MARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�5z�D��1   @�5|GL��@50bM���d�M���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @���A,��Al��A�  A���A���B	33B33B0��BD��BX��BlffB�33B�ffB�33B�ffB�ffB�ffB�ffBș�B���B�ffB癚B�  B���C�fC  CffC33C��C��C �3C%L�C*  C.��C4ffC8�fC=ffCB33CHffCRffC]  Cf�Cp�3Cz�3C�Y�C�33C�  C�33C��3C��C��3C��3C�  C�  C�ffC��fC���C��3C�&fC�L�C�L�C׌�C�@ C�ffC��C�� C�fC��fC�� D�fD��D  D@ D�3D��D��D%  D*�D.�fD4�D933D>  DB��DG�3DL��DR3DV��D\�Da  Df9�Dk3Dp�Du�Dy��D�)�D���D���D�� D�FfD��fD���D�	�D�P D�vfD��3D��D�@ DԌ�D�ٚD��fD�FfD�fD���D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @���A,��Al��A�  A���A���B	33B33B0��BD��BX��BlffB�33B�ffB�33B�ffB�ffB�ffB�ffBș�B���B�ffB癚B�  B���C�fC  CffC33C��C��C �3C%L�C*  C.��C4ffC8�fC=ffCB33CHffCRffC]  Cf�Cp�3Cz�3C�Y�C�33C�  C�33C��3C��C��3C��3C�  C�  C�ffC��fC���C��3C�&fC�L�C�L�C׌�C�@ C�ffC��C�� C�fC��fC�� D�fD��D  D@ D�3D��D��D%  D*�D.�fD4�D933D>  DB��DG�3DL��DR3DV��D\�Da  Df9�Dk3Dp�Du�Dy��D�)�D���D���D�� D�FfD��fD���D�	�D�P D�vfD��3D��D�@ DԌ�D�ٚD��fD�FfD�fD���D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA���A���AոRA���A��;A�A�(�A�ffA�jA�jA�hsA�jA�l�A�^5A��AԍPA�n�A�+AɅA���A��
A�1A�A��A��A�A�Q�A�hsA�t�A�A�bNA���A�%A��mA�%A�~�A�oA�+A��-A�`BA���A�$�A�VA~��Aw�TAm"�A\AW�hAM�AGC�AA/A:��A9/A5p�A.M�A+�hA)�A#K�A"�AVA9XA33A�A	/A�mA�9@�I�@���@���@�S�@���@�~�@���@��@��H@�dZ@�J@���@�C�@���@�r�@�dZ@�o@��D@�n�@���@��
@�x�@�Z@���@�hs@�Z@���@�
=@��@��@��@w+@nȴ@g|�@^�y@Tj@N�+@Fv�@>�@8  @17L@*J@%?}@!x�@(�@A�@�H@;d@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA���A���AոRA���A��;A�A�(�A�ffA�jA�jA�hsA�jA�l�A�^5A��AԍPA�n�A�+AɅA���A��
A�1A�A��A��A�A�Q�A�hsA�t�A�A�bNA���A�%A��mA�%A�~�A�oA�+A��-A�`BA���A�$�A�VA~��Aw�TAm"�A\AW�hAM�AGC�AA/A:��A9/A5p�A.M�A+�hA)�A#K�A"�AVA9XA33A�A	/A�mA�9@�I�@���@���@�S�@���@�~�@���@��@��H@�dZ@�J@���@�C�@���@�r�@�dZ@�o@��D@�n�@���@��
@�x�@�Z@���@�hs@�Z@���@�
=@��@��@��@w+@nȴ@g|�@^�y@Tj@N�+@Fv�@>�@8  @17L@*J@%?}@!x�@(�@A�@�H@;d@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�1B�+B�+B�bB��B�B��B�NB�ZB�`B�fB�fB�mB�sB�B��B%B��B�B7LB=qBK�BN�Bq�By�BhsBn�B� B}�B�1B�%Bv�BiyBZBJ�B!�BJB��B�!Bx�B
��B
ŢB
��B
m�B
=qB	�B	�7B	ffB	/B	\B��B�TB�)B��B�RB�!B��B��B��B�B|�Bu�Bu�Bn�Bk�BgmBl�Br�Bs�Bv�B� B�=B�oB��B�FB�wB�B��B	\B	33B	W
B	bNB	z�B	��B	�3B	�^B	�qB	ȴB	��B	�
B	�BB	�fB	��B	��B
B
\B
�B
$�B
(�B
.B
49B
<jB
>wB
F�B
K�B
O�B
T�B
[#B
_;B
bNB
gmB
k�B
l�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�gB�1B�+B�+B�HB�yB��B�OB�4B�ZB�`B�fB�fB�B��B�B�"B�B OB#�B8RB>BMjBR:Br�B{�BkBr|B�B�B��B�KBxlBk�B[#BM�B&fBB͟B��B}�B
��B
ǮB
��B
o5B
@B	�B	��B	iB	0�B	�B��B��B�/BοB�>B��B��B�B�
B�GB~wBv�BwfBo�Bl�Bh�Bl�BsBtnBw�B��B�)B�[B�mB��B��B��B��B	�B	3hB	W?B	b�B	{B	��B	�MB	�xB	��B	��B	� B	�$B	�\B	�B	��B	�B
GB
�B
�B
%B
)B
./B
4nB
<�B
>�B
F�B
K�B
O�B
UB
[#B
_VB
bNB
g�B
k�B
l�B
s�B
t�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711290016132017112900161320171129001613202207232056112022072320561120220723205611202207261122202022072611222020220726112220  JA  ARFMdecpV4_b                                                                20171118185153  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20171118185403  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20171118185403  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20171118185404  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20171118185404  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20171118185404  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171118185404                      G�O�G�O�G�O�                JA  ARUP                                                                        20171118185626                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20171119000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20171128151613  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171128151613  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050643  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115611  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                