CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-04-27T15:54:34Z creation;2018-04-27T15:54:36Z conversion to V3.1;2019-09-10T08:55:37Z update;2022-07-26T02:47:05Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20180427155434  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA  V4_131545_072                   2C  Dd\�ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�]{W:� 1   @�]{���@3��E����d\�1&�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       @ff@�33A0  A{33A�33A�ffA�33BffB!��B5��BI33B^  Bs33B���B���B�ffB���B�ffB�  B�33B�33B�  B�33B�  B�33B�ffC��CffC33C�C� CffC ffC%�3C*  C/��C3�3C9�3C>33CB��CG� CR�C\L�Cf�Cp�fCz�3C�33C�  C�Y�C�@ C�Y�C�L�C�  C�� C��3C�&fC�L�C�� C��C��C��C��C�@ C�33C�Y�C���C��fC��3C�  C�&fC�L�D3D�D�D�D&fD��D�3D%  D*3D.�3D4fD9�D=�3DC3DG�3DL� DRfDV��D[�3Da  Df3Dj�3DoٚDu  Dy��D�VfD�p D���D��D�FfD���D��fD�	�D�L�D��fD���D�� D�I�D�|�D��fD��D�@ D� D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@ff@�33A0  A{33A�33A�ffA�33BffB!��B5��BI33B^  Bs33B���B���B�ffB���B�ffB�  B�33B�33B�  B�33B�  B�33B�ffC��CffC33C�C� CffC ffC%�3C*  C/��C3�3C9�3C>33CB��CG� CR�C\L�Cf�Cp�fCz�3C�33C�  C�Y�C�@ C�Y�C�L�C�  C�� C��3C�&fC�L�C�� C��C��C��C��C�@ C�33C�Y�C���C��fC��3C�  C�&fC�L�D3D�D�D�D&fD��D�3D%  D*3D.�3D4fD9�D=�3DC3DG�3DL� DRfDV��D[�3Da  Df3Dj�3DoٚDu  Dy��D�VfD�p D���D��D�FfD���D��fD�	�D�L�D��fD���D�� D�I�D�|�D��fD��D�@ D� D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�"�A�oA͸RA�l�A���A�=qA�jA�bNA��A�E�A�VA���A���Aǡ�A�^5A�%A�M�A�ȴA�bAĕ�A���A°!A���A��`A�1A���A��A�z�A��+A���A���A��wA�z�A�A���A�7LA��jA�z�A���A���A���A��RA�l�A�O�A}��AnĜAd�A\~�AO��AGx�AA7LA>�A5�A.�A'��A"�DA�hA�;A��A�#AVA-A��@��m@��;@��@� �@�u@�l�@݉7@� �@��@�|�@�o@�@��@��@�`B@���@�bN@�@�?}@�|�@��^@�bN@�@�-@�|�@�x�@���@��9@�v�@�b@�@�r�@���@�P@v5?@k@_�w@W�w@N�R@F5?@>5?@8b@0��@-/@(�u@&ff@!��@��@bN@z�@r�@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�"�A�oA͸RA�l�A���A�=qA�jA�bNA��A�E�A�VA���A���Aǡ�A�^5A�%A�M�A�ȴA�bAĕ�A���A°!A���A��`A�1A���A��A�z�A��+A���A���A��wA�z�A�A���A�7LA��jA�z�A���A���A���A��RA�l�A�O�A}��AnĜAd�A\~�AO��AGx�AA7LA>�A5�A.�A'��A"�DA�hA�;A��A�#AVA-A��@��m@��;@��@� �@�u@�l�@݉7@� �@��@�|�@�o@�@��@��@�`B@���@�bN@�@�?}@�|�@��^@�bN@�@�-@�|�@�x�@���@��9@�v�@�b@�@�r�@���@�P@v5?@k@_�w@W�w@N�R@F5?@>5?@8b@0��@-/@(�u@&ff@!��@��@bN@z�@r�@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	1'B	8RB	6FB	;dB	E�B	ZB	�+B	�B	�?B
`BB
��BB�B%�B�B=qBjB�=B��B��B�TB��B+B\B>wBH�B?}B@�BbNBu�B�7B�B�Bo�BT�BD�B�B��B�B�7Bm�B49B�B
�fB
ffB
J�B	�B	��B	o�B	&�B	B�B�BB�jB�B��B��B�PB�Bt�Bv�Bk�BffBdZBcTBffBjBn�Bn�Bs�Bw�B�B�B�fB	B	0!B	:^B	=qB	D�B	jB	�B	��B	�B	��B	��B	�HB	�ZB	�sB	�B	�B	��B
B
	7B
PB
bB
{B
�B
�B
)�B
1'B
:^B
?}B
F�B
I�B
O�B
W
B
]/B
aHB
ffB
hsB
m�B
q�B
u�B
x�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	1vB	8lB	6�B	;�B	F%B	Z�B	��B	�B	�+B
`�B
�fBaBqB&�B�B=�BkB��B�bB�B��B��BfBB?.BIlB@�BB�Bc:BwfB��B�B�BsBVSBHfB�B�'B��B�BqAB6FB�B
��B
gmB
NVB	��B	��B	r�B	)*B	�B��B�hB�]B��B�4B��B��B��Bu�Bw�Bl�BgmBe�Bd�BgRBkQBo5BoiBt�Bx�B�MB�_B��B	uB	0oB	:�B	=�B	EB	j�B	�MB	��B	�IB	��B	�B	�|B	�B	�B	��B	�B	��B
MB
	lB
�B
}B
�B
�B
�B
*0B
1[B
:^B
?�B
F�B
I�B
O�B
W$B
]IB
abB
ffB
h�B
m�B
q�B
u�B
x�B
}"B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805080015552018050800155520180508001555202207232056252022072320562520220723205625202207261123242022072611232420220726112324  JA  ARFMdecpV4_b                                                                20180427155313  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180427155434  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180427155435  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180427155435  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180427155435  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180427155436  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180427155436                      G�O�G�O�G�O�                JA  ARUP                                                                        20180427155736                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180428000000  CF  PSAL_ADJUSTED_QC@ff@ffG�O�                JM  ARCAJMQC2.0                                                                 20180507151555  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180507151555  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604092931  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115625  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                