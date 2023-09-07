CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-04-04T06:52:11Z creation;2019-04-07T15:53:23Z conversion to V3.1;2019-09-10T08:04:22Z update;2022-11-10T04:22:01Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190404065211  20221117231505  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131533_031                   2C  Dd7+ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @س�}�u 1   @س�r��@.�t�j~��d7+I�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?L��@���A)��At��A�ffAə�A噚BffB#33B4ffBH��B]33Bp��B�  B�33B���B�ffB�33B�  B���B�  B�  B���B�ffB���B���C33C��C  C�CffC� C ffC%  C)��C.ffC4ffC8�fC=�fCB�3CG��CQ�fC\�Ce�fCpffC{  C�@ C��3C�ffC�Y�C�@ C�@ C��3C��3C�@ C���C��3C�s3C��C�&fC�L�C��C�&fC�ffC��3C��3C�L�C��C�ٚC��3C��3D��D� D�3D3DٚD��D 3D%&fD*9�D.��D3��D8��D>3DB�fDH�DM  DQٚDW�D\fDafDf3Dj� Do� Du�Dz�D�I�D�|�D��fD�3D�` D���D�ٚD���D�33D��3D���D���D�FfD�y�D��3D��fD�6fD�|�D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?L��@���A)��At��A�ffAə�A噚BffB#33B4ffBH��B]33Bp��B�  B�33B���B�ffB�33B�  B���B�  B�  B���B�ffB���B���C33C��C  C�CffC� C ffC%  C)��C.ffC4ffC8�fC=�fCB�3CG��CQ�fC\�Ce�fCpffC{  C�@ C��3C�ffC�Y�C�@ C�@ C��3C��3C�@ C���C��3C�s3C��C�&fC�L�C��C�&fC�ffC��3C��3C�L�C��C�ٚC��3C��3D��D� D�3D3DٚD��D 3D%&fD*9�D.��D3��D8��D>3DB�fDH�DM  DQٚDW�D\fDafDf3Dj� Do� Du�Dz�D�I�D�|�D��fD�3D�` D���D�ٚD���D�33D��3D���D���D�FfD�y�D��3D��fD�6fD�|�D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϺ^A�ȴAϗ�AϑhAϛ�A�z�A�x�A�ZA�5?A͗�A��HA��A��
A�ȴAȏ\A�JA�JA�/A���A���A�p�A��A��wA�=qA�bNA��7A�A�$�A���A�9XA���A��PA�jA��;A���A��A�A�$�A�;Ax�+Ar=qAe��A]�-AMp�A9
=A/C�A(�`A${A�`A�mAbNA&�A�A33AA	A|�A9XA�@�O�@�@�n�@�P@��@�1'@��@���@���@�5?@�Q�@��@��@�^5@��@��@Ȭ@��@��;@���@�S�@�;d@�dZ@��u@�V@�ȴ@�b@�@�dZ@�bN@��@�ƨ@��9@�ȴ@��`@�5?@��@���@���@u@l��@eV@[�F@T�/@M�T@E�@>��@6�+@0�9@*��@&5?@!�@z�@  @ƨ@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϺ^A�ȴAϗ�AϑhAϛ�A�z�A�x�A�ZA�5?A͗�A��HA��A��
A�ȴAȏ\A�JA�JA�/A���A���A�p�A��A��wA�=qA�bNA��7A�A�$�A���A�9XA���A��PA�jA��;A���A��A�A�$�A�;Ax�+Ar=qAe��A]�-AMp�A9
=A/C�A(�`A${A�`A�mAbNA&�A�A33AA	A|�A9XA�@�O�@�@�n�@�P@��@�1'@��@���@���@�5?@�Q�@��@��@�^5@��@��@Ȭ@��@��;@���@�S�@�;d@�dZ@��u@�V@�ȴ@�b@�@�dZ@�bN@��@�ƨ@��9@�ȴ@��`@�5?@��@���@���@u@l��@eV@[�F@T�/@M�T@E�@>��@6�+@0�9@*��@&5?@!�@z�@  @ƨ@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBBBB=qB��B�#B		7B
aHB
�5B)�BS�B\)B�1B�RB�/BJB�B�B	7B  B�`B��B�'B|�BS�B49BB
�^B
u�B
`BB
=qB
0!B
+B	��B	��B	l�B	5?B�B�B�5B	B	VB	B	{B	1'B	I�B	=qB	u�B	t�B	z�B	|�B	�B	�B	�B	�PB	�bB	�7B	q�B	� B	�DB	�VB	�{B	��B	��B	��B	�wB	��B	��B	�)B	�TB	�mB	�B	�B	�B	��B	��B	��B
B
+B

=B
VB
JB
DB
VB
uB
oB
�B
�B
�B
�B
-B
2-B
8RB
<jB
B�B
H�B
M�B
S�B
YB
^5B
cTB
gmB
k�B
n�B
r�B
u�B
y�B
~�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB9BBB9BB9BSB�B>�B�dB�=B	
=B
bNB
ߊB,qBT�B_�B��B�B�hB�BKB	B
rB-B�
B�:B�BHBU�B7fB�B
�}B
xlB
c B
?�B
2GB

�B	�B	��B	n�B	9�B��B�yB�'B	�B	�B	�B	�B	1�B	KxB	>(B	v`B	utB	{B	}�B	��B	��B	��B	��B	�NB	�XB	rGB	�4B	��B	��B	��B	��B	��B	��B	��B	�B	�TB	ܒB	�B	��B	��B	��B	��B	�B	�B	�BB
GB
zB

rB
�B
~B
xB
pB
�B
�B
�B
�B
�B
�B
-CB
2|B
8lB
<�B
B�B
H�B
M�B
TB
Y1B
^OB
cnB
g�B
k�B
n�B
r�B
u�B
y�B
B
�AB
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904180015592019041800155920190418001559202210251311362022102513113620221025131136202210251805122022102518051220221025180512  JA  ARFMdecpV4_b                                                                20190404065210  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190404065211  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190404065212  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190404065212  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190404065212  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190404065212  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190404065848                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190407155207  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190407155321  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190407155322  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190407155322  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190407155322  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190407155323  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190407155323                      G�O�G�O�G�O�                JA  ARUP                                                                        20190407155600                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190408000000  CF  PSAL_ADJUSTED_QC?L��?L��G�O�                JM  ARCAJMQC2.0                                                                 20190417151559  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190417151559  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190418151455  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920071516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041136  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231505                      G�O�G�O�G�O�                