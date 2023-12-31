CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-07-08T06:52:10Z creation;2019-07-11T21:53:44Z conversion to V3.1;2019-09-10T08:49:47Z update;2022-07-26T02:45:17Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190708065210  20220818051506  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  V4_131545_116                   2C  Dc��ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @��{��&�1   @�ˆo���@4%`A�7L�c��S���1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@���AffAs33A���A͙�A���B
��B ffB8��BJffB\��Bt  B���B���B���B�ffB�  B���B�  B�33BЙ�B�  B�33B�B�ffC�C33CffC�3C33C��C�3C%L�C)� C/ffC3�fC8��C=ffCBL�CGL�CQL�C[L�Cf��CpffCy��C�� C���C��C�L�C��fC���C�  C��fC�33C�s3C�L�C�Y�C�ffC��fC��C�&fC�ffC�s3C��C�L�C�ٚC�@ C�&fC�Y�C�  D  D,�D  D33D  D9�D �D%3D*3D.��D4fD8� D=�fDC  DH9�DM  DRfDV�3D[�fD`��Df,�DkfDp33Du3Dy� D�S3D���D��3D��D�#3D���D���D� D�C3D���D��3D�3D�6fDԐ D��fD�3D�Y�D�vfD�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111=���@���A��Aq��A�  A���A�  B
ffB   B8ffBJ  B\ffBs��B�ffB���B�ffB�33B���B���B���B�  B�ffB���B�  B�ffB�33C  C�CL�C��C�C� C��C%33C)ffC/L�C3��C8� C=L�CB33CG33CQ33C[33Cf�3CpL�Cy�3C��3C�� C�  C�@ C�ٚC���C��3C���C�&fC�ffC�@ C�L�C�Y�C�ٚC�  C��C�Y�C�ffC�  C�@ C���C�33C��C�L�C��3D�D&fD�D,�D�D33D fD%�D*�D.�3D4  D8��D=� DB��DH33DM�DR  DV��D[� D`�3Df&fDk  Dp,�Du�DyٚD�P D���D�� D��fD�  D���D�ɚD��D�@ D���D�� D�  D�33DԌ�D��3D� D�VfD�s3D�fD�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�dZA�M�A�5?A��mA��A�-A�;dA�+A�t�AΝ�A��;A��Aȟ�A�VA�=qA�A�&�A��hA���A�5?A�A��jA�dZA�Q�A�x�A���A�&�A��A��`A��-A���A��TA��+A��A�M�A���A�"�A��+A���A��9A�ffA��hAzAp�!Aa+AY��AQ33AI��A@=qA8ffA0�A*��A&ffAVA��A�-A��A�!A��A	�A(�A�+A�w@�Q�@�p�@�@�b@�-@֧�@�z�@ʧ�@�ȴ@�Q�@��9@�Ĝ@�v�@��@�|�@��F@���@�{@��@���@�E�@�+@�z�@�^5@��m@��j@��@�G�@��T@�+@���@��H@�Ĝ@��@x  @p �@iX@b��@["�@R�H@M�-@E�h@=�-@7��@0�`@)�@"�\@$�@hs@z�@l�@�m@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�dZA�M�A�5?A��mA��A�-A�;dA�+A�t�AΝ�A��;A��Aȟ�A�VA�=qA�A�&�A��hA���A�5?A�A��jA�dZA�Q�A�x�A���A�&�A��A��`A��-A���A��TA��+A��A�M�A���A�"�A��+A���A��9A�ffA��hAzAp�!Aa+AY��AQ33AI��A@=qA8ffA0�A*��A&ffAVA��A�-A��A�!A��A	�A(�A�+A�w@�Q�@�p�@�@�b@�-@֧�@�z�@ʧ�@�ȴ@�Q�@��9@�Ĝ@�v�@��@�|�@��F@���@�{@��@���@�E�@�+@�z�@�^5@��m@��j@��@�G�@��T@�+@���@��H@�Ĝ@��@x  @p �@iX@b��@["�@R�H@M�-@E�h@=�-@7��@0�`@)�@"�\@$�@hs@z�@l�@�m@1'31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	�!B	�!B	�9B
�=B>wB��B�B�dB��B�B��BDB{B�B�`BPB,B)�B1'B9XBJ�BW
B^5BgmBVB8RBq�B�B�B&�B,BB�ZBBbNB$�B
��B
�B
�LB
�B
YB
�B	��B	u�B	M�B	!�B��B��B�B��B�%B}�Bn�Bm�Bo�Bl�Bo�B�B�hB�hB�PB�B^5B^5BiyB~�B�B�bB��B��B��B�/B	JB	 �B	9XB	L�B	k�B	�B	�hB	��B	�RB	ɺB	��B	�5B	�fB	�B	�B	��B	��B
B
	7B
PB
oB
�B
�B
!�B
)�B
0!B
6FB
;dB
@�B
G�B
K�B
R�B
ZB
_;B
dZB
jB
o�B
s�B
w�B
{�B
�B
�%B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	�B	��B
|jB33B�gB�!B�'B��B�dB�zB�"B�B��B��B OBjB�B#�B+B<�BHfBPbBYBI�B*�Be�B�B�B+B�B��BרB�BV�BB
�RB
�BB
�B
s3B
LB
	RB	��B	h�B	AB	�B�=B��B�BB�JBxlBq�B`�B_VBa�B^�Ba�Bu�B��B�aB.BvFBP}BO�B\)Bp�Bs�B�[B��B��B�B�pB��B	 B	*B	=�B	\�B	sMB	��B	��B	�_B	��B	�B	�\B	�sB	ߤB	��B	��B	�B	�TB	�^B	�]B
�B
�B
�B
�B
	B
!-B
'8B
,WB
1�B
8�B
<�B
C�B
KB
P.B
U2B
[WB
`vB
d�B
h�B
l�B
r�B
wB
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.015(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201907220015272019072200152720190722001527202207232057052022072320570520220723205705202207261126202022072611262020220726112620  JA  ARFMdecpV4_b                                                                20190708065209  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190708065210  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190708065210  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190708065211  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190708065211  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190708065211  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190708065644                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190711215301  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190711215342  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190711215343  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190711215343  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190711215343  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190711215344  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190711215344                      G�O�G�O�G�O�                JA  ARUP                                                                        20190711215709                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190712000000  CF  PSAL_ADJUSTED_QC>L��@���G�O�                JM  ARSQJMQC2.0                                                                 20190712000000  CF  TEMP_ADJUSTED_QC>L��>L��G�O�                JM  ARCAJMQC2.0                                                                 20190721151527  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190721151527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190722151506  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115705  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022620  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051506                      G�O�G�O�G�O�                