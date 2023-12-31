CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-03-13T15:58:38Z creation;2017-03-13T15:58:40Z conversion to V3.1;2019-09-10T09:01:18Z update;2022-07-26T02:48:44Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20170313155838  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131545_031                   2C  Dd��ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @����ޠ 1   @���[� @2������d���+1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A(  A{33A�33A�33A�  B��B��B733BH  B\  Bp  B���B���B�  B���B�  B�ffB�ffB�33Bҙ�B�ffB���B�33B�  C�fC�C��C�C  C�C �3C$��C*33C/��C4��C8�fC>�3CC33CG��CQ��C\�3Cf� Cp�Cy��C��3C�L�C�Y�C�Y�C�s3C�L�C��3C�33C�Y�C�ffC��fC�s3C�@ C�&fC�&fC���Cѳ3Cֳ3CۦfC���C�  C�ffC�  C�s3C�  D�D  D�D�3D&fD�fD   D%&fD*&fD/�D4&fD8�fD>�DCfDH,�DL�fDQ��DW&fD\�Da,�Df�Dk3Do� Du&fDz�D�33D�y�D���D���D�@ D���D���D�	�D�<�D���D���D�  D�<�Dԃ3D��3D�fD�<�D�fD�� D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @���A&ffAy��A�ffA�ffA�33BffB33B6��BG��B[��Bo��B���B���B���B�ffB���B�33B�33B�  B�ffB�33B噚B�  B���C��C  C�3C  C�fC  C ��C$�3C*�C/� C4�3C8��C>��CC�CG� CQ� C\��CfffCp  Cy� C��fC�@ C�L�C�L�C�ffC�@ C��fC�&fC�L�C�Y�C�ٚC�ffC�33C��C��C�� CѦfC֦fCۙ�C�� C��3C�Y�C��3C�ffC��3D3D�D3D��D  D� D��D%  D*  D/fD4  D8� D>fDC  DH&fDL� DQ�fDW  D\fDa&fDf3Dk�DoٚDu  Dz3D�0 D�vfD�ɚD���D�<�D���D���D�fD�9�D���D�ɚD��D�9�DԀ D�� D�3D�9�D�3D��D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A��mA���AȼjAȶFAȲ-AȬAȧ�Aȣ�AȲ-Aȥ�AȅA��A�ƨAĶFA�t�A���A�`BA�-A���A�^5A�\)A�t�A�A�1'A�E�A�M�A�v�A�7LA�;dA�z�A�G�A��uA�^5A��!A�t�A��A�`BA��#A���A�n�A��A��TA�K�A� �Az5?Ap��AfI�AV��AM��AGt�A@��A9�hA2  A+�wA%l�A33A�A��A�PA(�A�!A�wA �\@��^@�n�@�^@�@߶F@� �@�~�@��@˥�@���@�M�@���@��@��9@�ff@�J@� �@�&�@��/@�33@��R@�K�@���@�~�@�
=@�%@���@��!@���@�K�@�bN@���@��@�`B@x�9@m��@d�@Yx�@SS�@J�@D9X@>�+@7;d@2M�@)%@#C�@E�@�9@9X@r�@E�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A��mA���AȼjAȶFAȲ-AȬAȧ�Aȣ�AȲ-Aȥ�AȅA��A�ƨAĶFA�t�A���A�`BA�-A���A�^5A�\)A�t�A�A�1'A�E�A�M�A�v�A�7LA�;dA�z�A�G�A��uA�^5A��!A�t�A��A�`BA��#A���A�n�A��A��TA�K�A� �Az5?Ap��AfI�AV��AM��AGt�A@��A9�hA2  A+�wA%l�A33A�A��A�PA(�A�!A�wA �\@��^@�n�@�^@�@߶F@� �@�~�@��@˥�@���@�M�@���@��@��9@�ff@�J@� �@�&�@��/@�33@��R@�K�@���@�~�@�
=@�%@���@��!@���@�K�@�bN@���@��@�`B@x�9@m��@d�@Yx�@SS�@J�@D9X@>�+@7;d@2M�@)%@#C�@E�@�9@9X@r�@E�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�ZB�TB�TB�NB�NB�HB�NB�NB�yB�B��B/B49BYBW
B�B�B�B�%B�1B�%B}�BaHBp�BW
B+B[#B]/BB�B�BbB�HB�B�ZB��B�-B�VBo�Bm�B\)B'�B
�B
��B
��B
p�B
(�B	�#B	k�B	6FB	bB�B�B��BȴB�XB�)B�mBȴB�uB�oB�7Bt�BiyBn�Bk�Br�By�B�=B�oBȴBĜB��B	5?B	[#B	iyB	x�B	�B	��B	��B	�!B	��B	��B	��B	�TB	�sB	�B	�B	��B	��B
B
B

=B
\B
oB
�B
�B
!�B
(�B
/B
49B
9XB
=qB
C�B
H�B
M�B
T�B
ZB
^5B
dZB
hsB
m�B
q�B
v�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�QB�tB�nB�nB�NB�NB�bB�NB�NB�B�B�jB/�B5?B[WBYeB�;B�SB��B�tB�B��B��Bd@Br�BZQB-CB`'B`\BF%B!�B�B��B��B�zB�2B�%B�}Bo�Bn�B]�B)�B
��B
��B
�B
r�B
+kB	�B	m�B	8B	:B��B�)B̘BʌB�dB�B��B�=B�2B��B��Bu�Bj�Bo5BmCBs�Bz�B��B��B�RB�9B�B	5tB	[WB	i�B	y>B	�MB	�B	��B	�oB	�B	� B	�gB	�B	�B	��B	��B	�B	�(B
aB
mB

rB
�B
�B
�B
�B
"B
)DB
/OB
4�B
9�B
=�B
C�B
H�B
NB
UB
ZkB
^jB
dtB
h�B
m�B
q�B
v�B
y	B
|31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.3(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201703240015522017032400155220170324001552202207232055482022072320554820220723205548202207261120402022072611204020220726112040  JA  ARFMdecpV4_b                                                                20170313155242  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170313155838  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170313155838  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170313155839  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170313155839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170313155839  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170313155840                      G�O�G�O�G�O�                JA  ARUP                                                                        20170313160409                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170314000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20170323151552  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170323151552  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050531  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115548  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                