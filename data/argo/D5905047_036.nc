CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-05-02T19:05:41Z creation;2017-05-02T19:05:43Z conversion to V3.1;2019-09-10T09:00:37Z update;2022-07-26T02:48:32Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20170502190541  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               $A   JA  V4_131545_036                   2C  Dd�ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�zɓ� 1   @��� @4+I��d�+J1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�33A+33At��A���A�ffA�ffB
��B"ffB533BG��BY33Bq33B�33B�ffB�ffB���B���B�  B�ffBș�B�33Bޙ�B�ffB���B�ffC�CffC33C�fC  C�fC�fC%� C*�fC.��C4L�C9  C>  CC33CH33CR33C\ffCe��Cp�3Cy�fC�  C�@ C��fC��3C���C�L�C�&fC��C�&fC��3C���C�33C��fC��C��fC�&fC�ٚC�L�C۳3C��C�s3C��3C�� C���C�ffD  D,�D�DfD,�D�3D 33D%33D*�D.�3D3�fD9  D>�DC  DG� DM�DR&fDW&fD\3Da  Df,�Dj��Do� Dt�3DzfD�L�D��3D�ٚD��3D�<�D��3D��3D��fD�I�D���D��3D��3D�P Dԙ�Dڰ D���D�P D�|�D���D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>L��@�33A+33At��A���A�ffA�ffB
��B"ffB533BG��BY33Bq33B�33B�ffB�ffB���B���B�  B�ffBș�B�33Bޙ�B�ffB���B�ffC�CffC33C�fC  C�fC�fC%� C*�fC.��C4L�C9  C>  CC33CH33CR33C\ffCe��Cp�3Cy�fC�  C�@ C��fC��3C���C�L�C�&fC��C�&fC��3C���C�33C��fC��C��fC�&fC�ٚC�L�C۳3C��C�s3C��3C�� C���C�ffD  D,�D�DfD,�D�3D 33D%33D*�D.�3D3�fD9  D>�DC  DG� DM�DR&fDW&fD\3Da  Df,�Dj��Do� Dt�3DzfD�L�D��3D�ٚD��3D�<�D��3D��3D��fD�I�D���D��3D��3D�P Dԙ�Dڰ D���D�P D�|�D���D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�M�A�1'A�1'A�1'A�/A�/A�-A�1'A�1'A�/A�(�A�"�A���A��HA��A��A�^5A˩�A�;dA���A�ffA�;dA�  A�A�A��TA���A�{A�x�A���A���A�?}A���A��yA��RA���A��!A�%A���A���A�7LA�/A���A���A~�Aq�#Adr�A[��AZ{AP�RAMO�AD1'A<M�A1�PA*bA%�A�^AG�A~�A�#A1'A��A��Ax�A�9@�\)@�9X@�p�@�^5@�^@�E�@߾w@�E�@ϝ�@Ƨ�@��j@�@��@�\)@�hs@��@��\@� �@��\@��D@�x�@��j@���@�v�@�t�@�=q@�~�@��D@�S�@�V@��R@��D@�  @x�`@p �@f@\9X@UO�@O\)@J�H@C�
@>ff@9��@3�m@,�@%�@�@j@bN@1@��@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�M�A�1'A�1'A�1'A�/A�/A�-A�1'A�1'A�/A�(�A�"�A���A��HA��A��A�^5A˩�A�;dA���A�ffA�;dA�  A�A�A��TA���A�{A�x�A���A���A�?}A���A��yA��RA���A��!A�%A���A���A�7LA�/A���A���A~�Aq�#Adr�A[��AZ{AP�RAMO�AD1'A<M�A1�PA*bA%�A�^AG�A~�A�#A1'A��A��Ax�A�9@�\)@�9X@�p�@�^5@�^@�E�@߾w@�E�@ϝ�@Ƨ�@��j@�@��@�\)@�hs@��@��\@� �@��\@��D@�x�@��j@���@�v�@�t�@�=q@�~�@��D@�S�@�V@��R@��D@�  @x�`@p �@f@\9X@UO�@O\)@J�H@C�
@>ff@9��@3�m@,�@%�@�@j@bN@1@��@31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
-B
5?B
7LB
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
�oB
�XB
��B
=B�DB�bB�qB�mBBA�B6FB>wBp�BffB+B%�Be`Bm�B\)BW
B1'B"�B�B�TB�B�\Bo�B=qB
��B
=qB
#�B	��B	J�B	�B	\B	#�B	�B	+B�NB�LB�!B��B��B�\B�B{�Bo�Bo�Bu�Bp�BhsBn�BiyBhsBk�B�\B��B��B�9B�qB�;B��B	�B	+B	:^B	G�B	gmB	s�B	�B	��B	��B	��B	�LB	B	��B	�mB	�B	��B	��B
  B
B

=B
VB
{B
!�B
'�B
2-B
9XB
>wB
B�B
E�B
J�B
N�B
Q�B
VB
\)B
cTB
hsB
l�B
o�B
s�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
-CB
5?B
7LB
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6`B
7�B
9>B
�B
��B
�cB
rB�xB� B�wB�XB�BBAB8RB?Br�Bi�B,B($BgBqB^jB[=B3�B%�B$ZB�RB�'B��Bq�BA�B
�B
?HB
&�B	��B	MB	1B	B	$�B	B		B�FB�rB�[B��B��B��B��B|�Bp�Bp�Bv�BqvBjBp!Bi�BiBlWB�\B��B��B��B�BBߤB�LB	�B	+6B	:�B	HB	g�B	s�B	�9B	��B	�B	�B	��B	��B	�.B	�B	��B	��B	�B
 4B
3B

rB
�B
�B
!�B
($B
2aB
9rB
>�B
B�B
E�B
J�B
N�B
RB
VB
\]B
cTB
h�B
l�B
o�B
s�B
y�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705130015512017051300155120170513001551202207232055532022072320555320220723205553202207261121002022072611210020220726112100  JA  ARFMdecpV4_b                                                                20170502185233  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170502190541  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170502190541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170502190542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170502190542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170502190542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170502190543                      G�O�G�O�G�O�                JA  ARUP                                                                        20170502191249                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170503000000  CF  PSAL_ADJUSTED_QC>L��>L��G�O�                JM  ARSQJMQC2.0                                                                 20170503000000  CF  TEMP_ADJUSTED_QC>L��>L��G�O�                JM  ARCAJMQC2.0                                                                 20170512151551  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170512151551  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050547  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115553  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                