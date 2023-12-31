CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-09-19T18:55:02Z creation;2017-09-19T18:55:04Z conversion to V3.1;2019-09-10T08:58:40Z update;2022-07-26T02:47:58Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20170919185502  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA  V4_131545_050                   2C  Dd�+ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�&z�+�1   @�&{�ff�@4�����d�+I�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�  AffAa��A���A�  A�  BffB!��B933BK33B`ffBrffB���B���B�ffB���B�ffB���B�33B�33B���B�  B���B�  B�  C  CL�C�3C��CffC�3C ��C$��C*33C/��C4L�C9L�C>L�CC� CH� CR� C[�Ce�Co�Cy  C���C�33C�@ C�&fC�L�C�@ C��fC��3C�� C�ٚC�@ C�� C�&fC�&fC�@ C��3C�33C�  C�@ C��fC�Y�C�ffC�� C��fC�  D&fD&fD3D�3DٚD�fD�3D$�3D*3D.� D4�D8ٚD>fDC  DH9�DM  DRfDW33D\&fDafDf&fDj��Dp�Dt��Dz3D�C3D��3D��3D�3D�Y�D��3D���D�fD�S3D��3D��fD���D�)�Dԉ�DڶfD�	�D�FfD�3D�ٚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>���@�33A  Ac33A���A���A���B��B"  B9��BK��B`��Br��B�  B���B���B�  B���B�  B�ffB�ffB�  B�33B�  B�33B�33C�CffC��C�fC� C��C �fC$�fC*L�C/�3C4ffC9ffC>ffCC��CH��CR��C[33Ce33Co33Cy�C���C�@ C�L�C�33C�Y�C�L�C��3C�  C���C��fC�L�C���C�33C�33C�L�C�  C�@ C��C�L�C��3C�ffC�s3C���C��3C��D,�D,�D�D��D� D��DٚD$ٚD*�D.�fD4  D8� D>�DC&fDH@ DM&fDR�DW9�D\,�Da�Df,�Dj�3Dp3Dt�3Dz�D�FfD��fD��fD�fD�\�D��fD�� D�	�D�VfD��fD�ɚD���D�,�DԌ�Dڹ�D��D�I�D�fD���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�!A�A��A��#A�9A�jA��A��;A�~�AݶFA۩�A��TA���A�+A�E�A��A�+Aƣ�A�A���A�M�A�{A��RA�1A�ȴA�O�A�33A��A�1'A��A�S�A��A�XA�Q�A���A��A�?}A��;A��wA�x�A�=qA��wA��HAz�+At�uAn�+Ah�9A^VAR�RAJVA@ffA:ĜA3�A0�A-ƨA#��A%A%A��A
=A{A$�A
�\AVA��A�T@��F@��@���@��@���@�I�@�Ĝ@ǥ�@+@�Z@�Ĝ@��@�V@��@�v�@���@��w@�{@��;@�z�@��@�7L@�Z@�G�@�o@���@�-@�b@��7@��;@��@z~�@s��@i�#@ct�@Y��@S33@L�@IX@B�@;@4Z@/�;@)G�@"�@
=@�m@v�@�@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�!A�A��A��#A�9A�jA��A��;A�~�AݶFA۩�A��TA���A�+A�E�A��A�+Aƣ�A�A���A�M�A�{A��RA�1A�ȴA�O�A�33A��A�1'A��A�S�A��A�XA�Q�A���A��A�?}A��;A��wA�x�A�=qA��wA��HAz�+At�uAn�+Ah�9A^VAR�RAJVA@ffA:ĜA3�A0�A-ƨA#��A%A%A��A
=A{A$�A
�\AVA��A�T@��F@��@���@��@���@�I�@�Ĝ@ǥ�@+@�Z@�Ĝ@��@�V@��@�v�@���@��w@�{@��;@�z�@��@�7L@�Z@�G�@�o@���@�-@�b@��7@��;@��@z~�@s��@i�#@ct�@Y��@S33@L�@IX@B�@;@4Z@/�;@)G�@"�@
=@�m@v�@�@�R31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BhsBŢBǮBƨBŢB��B�^B��B��B2-BE�Bq�B�%B�B�'B�LB�!B�B�3B�RB�!B�B�?B�FB�dB��B��BƨB�?B�Bu�B^5B6FB,BPB�ZB��B��B�DBz�B[#B
�HB
gmB
F�B
-B
PB	�NB	��B	XB	 �B��B�TBȴB�}B�B��B��B��B��B�{B�bB� Bl�BiyBgmBe`BcTBe`Be`BbNBiyB}�B{�B��BȴB�)B��B	DB	$�B	:^B	R�B	ffB	�B	�DB	��B	�B	�LB	ÖB	��B	��B	�/B	�sB	�B	��B
B
%B
\B
�B
#�B
.B
1'B
6FB
>wB
A�B
E�B
L�B
O�B
Q�B
YB
_;B
cTB
ffB
hsB
n�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bk�BżB��B��B��B�UB��B͟B �B4BI�Bt�B�RB�OB�hB��B��B��B��B�^B��B�)B��B�B��BϑBбB�B��B��Bv�Bb�B7�B./B�B�2B��B��B��B}B^5B
�LB
i�B
H1B
.�B
�B	�B	��B	ZkB	#TB�JB�`BɠB�OB��B�,B�OB�qB�eB��B�:B�oBmCBi�Bh>BffBc�Be�BfLBb�Bj�B~�B|�B�;B�7B�]B�B	xB	%,B	:�B	S&B	f�B	�B	�xB	��B	�B	�fB	ÖB	�B	�&B	�dB	�B	��B	�	B
B
?B
�B
�B
#�B
.B
1[B
6`B
>wB
A�B
E�B
L�B
O�B
RB
YB
_VB
cTB
ffB
hsB
n�B
u�B
y�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709300016162017093000161620170930001616202207232056062022072320560620220723205606202207261121562022072611215620220726112156  JA  ARFMdecpV4_b                                                                20170919185234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170919185502  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170919185503  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170919185503  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170919185503  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170919185504  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170919185504                      G�O�G�O�G�O�                JA  ARUP                                                                        20170919185742                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170920000000  CF  PSAL_ADJUSTED_QC>L��@�  G�O�                JM  ARSQJMQC2.0                                                                 20170920000000  CF  TEMP_ADJUSTED_QC>L��>L��G�O�                JM  ARCAJMQC2.0                                                                 20170929151616  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170929151616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050627  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                