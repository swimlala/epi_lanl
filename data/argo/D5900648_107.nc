CDF   -   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-08-17T01:08:13Z creation;2013-09-24T05:26:07Z update;2015-06-09T19:23:54Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               kA   JA  20070817010813  20150614052515  A5_28347_107                    2C  D   APEX                            1316                            013004                          846 @ԍ��1   @ԍ��Q�o@6��
=p��c��C��1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��A`  A�33A���A�33B	33B  B1��BF  BZffBm33B�33B�  B�33B�  B�ffB�33B���B�33B���B���B���B�  B�  CffCffC33CffC33CffC� C$ffC)� C.� C333C8L�C=  CA��CG�CQffC[33Ce33Co�Cy� C��fC�� C���C�ffC�� C���C��fC�� C�� C���C���C�� C���C¦fCǙ�C�s3C�Y�C�s3Cۀ C�� C�fC�3C�� C��C�s3D�3D�3D� D�fDٚD�3DٚD$ٚD)ٚD.�3D3� D8�fD=��DBٚDGٚDLٚDQٚDV�fD[�3D`ٚDe��Dj�fDo� Dt� Dy� D�&fD�\�D���D�ٚD�fD�i�D��fD��3D�,�D�i�D�� D�� D�&fD�l�Dک�D��3D�,�D�ffD�3D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��A`  A�33A���A�33B	33B  B1��BF  BZffBm33B�33B�  B�33B�  B�ffB�33B���B�33B���B���B���B�  B�  CffCffC33CffC33CffC� C$ffC)� C.� C333C8L�C=  CA��CG�CQffC[33Ce33Co�Cy� C��fC�� C���C�ffC�� C���C��fC�� C�� C���C���C�� C���C¦fCǙ�C�s3C�Y�C�s3Cۀ C�� C�fC�3C�� C��C�s3D�3D�3D� D�fDٚD�3DٚD$ٚD)ٚD.�3D3� D8�fD=��DBٚDGٚDLٚDQٚDV�fD[�3D`ٚDe��Dj�fDo� Dt� Dy� D�&fD�\�D���D�ٚD�fD�i�D��fD��3D�,�D�i�D�� D�� D�&fD�l�Dک�D��3D�,�D�ffD�3D�#33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��mAѸRAѥ�Aѧ�Aѥ�Aѥ�Aѧ�Aџ�A�VAУ�A�n�A��A�bA�5?A��A��
A�-A��/A��FA�~�A��A�E�A��A�?}A��FA��A���A�dZA��A���A�bNA��;A�`BA��;A��A�bA��A���A���A�l�A��A�A��^Au��Aml�Aa�A^��AX�AR �AJ��AG�A?�mA;p�A6�A5�;A1�A)A#
=AĜAAx�A�A�yA�A
�jA�HAI�A 1'@�j@�v�@�v�@�  @�@���@�bN@�Q�@��@��^@�ƨ@���@��@�;d@��`@��@��@��#@�o@�p�@�;d@���@�"�@���@�  @|�D@v@pb@i��@`b@Y�@N��@E�@?;d@7�;@3@,�@(�u@#"�@E�@&�@I�@G�@V@	��@
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��mA��mAѸRAѥ�Aѧ�Aѥ�Aѥ�Aѧ�Aџ�A�VAУ�A�n�A��A�bA�5?A��A��
A�-A��/A��FA�~�A��A�E�A��A�?}A��FA��A���A�dZA��A���A�bNA��;A�`BA��;A��A�bA��A���A���A�l�A��A�A��^Au��Aml�Aa�A^��AX�AR �AJ��AG�A?�mA;p�A6�A5�;A1�A)A#
=AĜAAx�A�A�yA�A
�jA�HAI�A 1'@�j@�v�@�v�@�  @�@���@�bN@�Q�@��@��^@�ƨ@���@��@�;d@��`@��@��@��#@�o@�p�@�;d@���@�"�@���@�  @|�D@v@pb@i��@`b@Y�@N��@E�@?;d@7�;@3@,�@(�u@#"�@E�@&�@I�@G�@V@	��@
=3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�7B�7B�7B�7B�7B�=B�DB�JB��B�BT�Bq�B.B=qB%�B"�B�BoBB��B�B�`B�;B�BÖB�B��B�VBr�BbNBW
BF�B6FB �B�B��B��B�{B�BgmB-B
�B
�LB
z�B
VB	ǮB	}�B	u�B	�B	ZB	�B��B��B�3B��B�jB��B�uB�\B�DB�%B�B�B}�Bz�Bp�Be`B^5B^5BXBP�BR�BL�BQ�BcTBu�B��B�B�^B��B�;B�B	PB	/B	L�B	q�B	�%B	��B	�FB	��B	�B	�fB	�B	��B
B
bB
�B
�B
"�B
(�B
49B
@�B
I�B
Q�B
W
B
^5B
aHB
gmB
m�B
q�B
v�B
z�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�7B�7B�7B�7B�7B�=B�DB�JB��B�BVBt�B1'B@�B'�B#�B�B�BB��B�B�mB�HB�BŢB�B��B�hBs�BcTBXBG�B7LB#�B��B��B��B��B�BiyB/B
�B
�XB
}�B
bB	��B	~�B	w�B	�B	\)B	�B��B��B�9B��B�qB��B��B�bB�JB�+B�B�B~�B{�Br�BffB_;B_;BYBQ�BS�BM�BR�BdZBv�B��B�B�^B��B�;B�B	PB	/B	L�B	q�B	�%B	��B	�FB	��B	�B	�fB	�B	��B
B
bB
�B
�B
"�B
(�B
49B
@�B
I�B
Q�B
W
B
^5B
aHB
gmB
m�B
q�B
v�B
z�B
� B
�B
�13333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200708300808202007083008082020070830080820200708300818012007083008180120070830081801201309120000002013091200000020130912000000  JA  ARFMdecpA5_a                                                                20070817010754  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070817010813  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070817010815  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070817010818  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070817010824  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070817010824  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070817010825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070817010825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070817010826  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070817010826  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070817013753                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20070821034232  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070821034236  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070821034237  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070821034237  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070821034241  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20070821034241  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070821034241  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20070821034241  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070821034241  QCP$                G�O�G�O�G�O�           10000JA  ARGQaqcpt16a                                                                20070821034241  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070821042444                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065053  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070105  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070106  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070106  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070106  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070107  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070107  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070107  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070107  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070107  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070451                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20070820172813  CV  DAT$            G�O�G�O�F�m�                JM  ARSQJMQC1.0                                                                 20070820172813  CF  PRES            @�  D�#3G�O�                JM  ARSQJMQC1.0                                                                 20070820172813  CF  TEMP            @�  D�#3G�O�                JM  ARSQJMQC1.0                                                                 20070820172813  CF  PSAL            @�  D�#3G�O�                JM  ARCAJMQC1.0                                                                 20070830080820  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070830080820  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070830081801  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052352  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052607                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192343                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052515                      G�O�G�O�G�O�                