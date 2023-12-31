CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   c   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2009-09-29T06:54:37Z creation;2015-04-24T02:28:17Z conversion to V3.1;2019-04-22T03:38:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  ;(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  =   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  ?   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  CL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  HT   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  I�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Jp   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Sp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   \p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ep   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    e�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    e�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    e�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  f    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    fP   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    fT   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         fd   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         fh   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        fl   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    fpArgo profile    3.1 1.2 19500101000000  20090929065437  20190423051518  2900548 Argo eq. HNFRI                                                  Akira Kusaka                                                    PRES            TEMP            PSAL               �A   JA  A3_030280_154                   2C  D   APEX                            1602                            070204                          846 @�O:��,�1   @�O:� @C��"��`�d�����1   ARGOS   A   A   A   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   @�ffA��AfffA�33A�33A���B33B��B2  BF  B[33BnffB���B�ffB�  B�  B���B���B���Bř�B�33B�  B�33BB���C� C� C33CL�C  C� C  C$�C)ffC.33C3�C8�C=L�CA�fCG33CL  CP��CVL�C[33C`33Ce�Cj33Co33CtL�CyffC~L�C��3C�@ C��fC�L�C�� C�L�C���C�@ C��fC���C�� C���C�� Cǌ�Cљ�C۳3C�� C�� C���D��DٚD�fD� D� D��D�fD$� D)� D.��D3�fD8�3D=��DB�fDG��DL�3DQ��DV� D[�3D`�3De�3Dj�fDo� Dt�3Dy��D�&fD�c3D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA	��A^ffA�33A�33A���B33B��B0  BD  BY33BlffB33B�ffB�  B�  B���B���B���Bę�B�33B�  B�33B홚B���C  C  C
�3C��C� C  C� C#��C(�fC-�3C2��C7��C<��CAffCF�3CK� CPL�CU��CZ�3C_�3Cd��Ci�3Cn�3Cs��Cx�fC}��C�s3C�  C�ffC��C�� C��C�Y�C�  C�ffC�Y�C�@ C�L�C�@ C�L�C�Y�C�s3C� C� C�Y�D��D��D�fD� D� D��D�fD$� D)� D.��D3�fD8�3D=��DB�fDG��DL�3DQ��DV� D[�3D`�3De�3Dj�fDo� Dt�3Dy��D�fD�S3D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�JA�
=A�%A�A�A�1A�  A��A��A��hAt�AYdZAS��AH��ACS�AB��AD�yA@�A@�A?�^A=A<1'A9�FA7VA7+A6�A4�A2�9A1��A0�\A/�PA.bNA,=qA,��A*��A*��A)�PA(�A(I�A'�hA&�/A%?}A#��A#VA#oA"A!7LA�wA��A�AhsAC�A��AC�AE�A��Az�A�AffA"�A
�A{@�l�@�@�r�@�1'@���@�~�@�1@���@��@�t�@�^5@��@���@���@��\@�5?@��w@�C�@���@�n�@��F@�Ĝ@{S�@wl�@s@p �@l1@h �@d�j@aG�@]`B@Y�7@QX@I��@Ct�@<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�JA�
=A�%A�A�A�1A�  A��A��A��hAt�AYdZAS��AH��ACS�AB��AD�yA@�A@�A?�^A=A<1'A9�FA7VA7+A6�A4�A2�9A1��A0�\A/�PA.bNA,=qA,��A*��A*��A)�PA(�A(I�A'�hA&�/A%?}A#��A#VA#oA"A!7LA�wA��A�AhsAC�A��AC�AE�A��Az�A�AffA"�A
�A{@�l�@�@�r�@�1'@���@�~�@�1@���@��@�t�@�^5@��@���@���@��\@�5?@��w@�C�@���@�n�@��F@�Ĝ@{S�@wl�@s@p �@l1@h �@d�j@aG�@]`B@Y�7@QX@I��@Ct�@<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBdZBdZBcTBcTBcTBcTBdZBbNBaHBVB�B33BjB5?B\B�BR�B2-BF�BR�BYBP�B;dB/BR�BhsBS�B^5BhsBr�Bx�Bz�Bs�B�1B�B�7B�+B�DB�\B�PB�VB�=B�%B�B�=B�B�Bz�Br�Be`B^5BhsBVBK�BO�BO�BJ�BN�BR�BJ�B=qB.B�B+B��B�yB�HB�5B�#B�#B�yB�B��BVB�B-BB�BR�Be`B~�B�JB��B��B�jB��B�#B�yB�B	  B	JB	�B	 �B	-B	9XB	Q�B	ffB	y�B	�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BY�BYBYBX_BXyBX_BXyBYBW�BW�B_!B�tB+�Bd�B,�BBBJ	B'�B<PBIBOBGEB1�B$ZBH1B^�BI�BS�B^BhsBn�BqBh�B~ByrB~�B|�B��B��B��B�3B�B{�Bx8B�Bz�Bx�Bp�Bh�B[qBS�B^�BLBA�BEmBE�B@�BD�BH�B@�B3hB$@B�B�B��BߊB�
B��B��B��B�!B�,B�oB�B:B"�B88BH�B[	BtTB��B�:B��B��B�3BЗB��B�$B�tB	�B	�B	B	"hB	.�B	GEB	[�B	oB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<\Yf<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.5(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0000), deepest deltaS=0.011(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-800(dbar) is excluded in mapping; Use P>(dbar) Use P<(dbar) Use THETA<(deg.C) Use THETA>(deg.C)                                                                           Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201611102118172016111021181720161110211817201611110209322016111102093220161111020932201904221200572019042212005720190422120057  JA  ARFMdecpA3_b                                                                20090929065436  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090929065437  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090929065438  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090929065438  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090929065439  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090929065439  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090929065439  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090929065439  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090929065440  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20090929070151                      G�O�G�O�G�O�                JA  ARFMdecpA3_b                                                                20091002215424  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091002215751  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091002215752  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091002215752  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091002215753  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091002215753  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091002215753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091002215753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091002215753  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091002220121                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424022817                      G�O�G�O�G�O�                JA  ARUP                                                                        20150427114513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121817  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121817  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20161110170932  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190422030057  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190423051518                      G�O�G�O�G�O�                