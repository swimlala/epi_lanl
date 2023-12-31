CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-27T06:52:21Z creation;2020-05-30T21:53:29Z conversion to V3.1;2020-12-25T04:14:01Z update;     
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
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i4Argo profile    3.1 1.2 19500101000000  20200527065221  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_167                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�}h�} 1   @��{��@<[��S���c�bM��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�ff@�  A��As33A�  A�  A�  B33B   B333BE��B^ffBr  B�  B�ffB�ffB�  B���B���B�33B�ffB�  B�ffB�33B�  B�  CffC�fCffC� C� C�fC ffC%L�C*  C/33C4��C9��C>��CC�3CH� CRL�C\  Cf��Cp��Cz� C�&fC�@ C�&fC�  C�� C�ٚC�&fC��C�ffC�L�C�L�C�  C�L�C�@ C�@ C�&fC��fC�ffC�  C�&fC��3C� C�&fC��C�L�D��D��D  D��D��D9�D 9�D%fD*3D/  D4  D8��D=��DC�DH3DM  DR3DW�D\&fDafDf3Dj�fDo�3Du�Dy��D�I�D��fD��3D��D�FfD�� D�ٚD�  D�9�D��3D���D�fD�VfDԆfDڬ�D�fD�P D��D�ٚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�ff@�  A��As33A�  A�  A�  B33B   B333BE��B^ffBr  B�  B�ffB�ffB�  B���B���B�33B�ffB�  B�ffB�33B�  B�  CffC�fCffC� C� C�fC ffC%L�C*  C/33C4��C9��C>��CC�3CH� CRL�C\  Cf��Cp��Cz� C�&fC�@ C�&fC�  C�� C�ٚC�&fC��C�ffC�L�C�L�C�  C�L�C�@ C�@ C�&fC��fC�ffC�  C�&fC��3C� C�&fC��C�L�D��D��D  D��D��D9�D 9�D%fD*3D/  D4  D8��D=��DC�DH3DM  DR3DW�D\&fDafDf3Dj�fDo�3Du�Dy��D�I�D��fD��3D��D�FfD�� D�ٚD�  D�9�D��3D���D�fD�VfDԆfDڬ�D�fD�P D��D�ٚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�
=A��A�9XA�1A�oA�S�A�jA�9XA��A��
A���A�ĜA�"�A��A��DA�~�A�ĜA��A�7LA���A�C�A�jA�x�A�t�A�?}A��-A��A��wA���A�bNA�t�A�K�A�O�A��A��
A��uA��!A�Q�A�"�A�A���A|ȴAv��Amp�Aa|�A[�AX(�AQ&�ALVAG��ADE�A=ƨA:��A6�A2�\A-�A*jA(��A&�\A$bA"-A%A�A�PA��AJAoA	/Av�AV@�{@�5?@�@���@ҧ�@�$�@��@���@�
=@�33@��@���@�&�@�&�@���@���@���@���@�
=@�O�@�+@��@}�h@z^5@wK�@u�T@nV@e�@[�@T�/@N�+@I�#@D�j@=�h@7��@3"�@-O�@(A�@#S�@p�@�^@�@M�@�-@
��@A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�
=A��A�9XA�1A�oA�S�A�jA�9XA��A��
A���A�ĜA�"�A��A��DA�~�A�ĜA��A�7LA���A�C�A�jA�x�A�t�A�?}A��-A��A��wA���A�bNA�t�A�K�A�O�A��A��
A��uA��!A�Q�A�"�A�A���A|ȴAv��Amp�Aa|�A[�AX(�AQ&�ALVAG��ADE�A=ƨA:��A6�A2�\A-�A*jA(��A&�\A$bA"-A%A�A�PA��AJAoA	/Av�AV@�{@�5?@�@���@ҧ�@�$�@��@���@�
=@�33@��@���@�&�@�&�@���@���@���@���@�
=@�O�@�+@��@}�h@z^5@wK�@u�T@nV@e�@[�@T�/@N�+@I�#@D�j@=�h@7��@3"�@-O�@(A�@#S�@p�@�^@�@M�@�-@
��@A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B+B/B33B:^B0!B2-B/B.B1'B&�B6FB1'B9XBH�BQ�BVBXBjBw�B~�Bu�Bk�BW
BN�BL�B=qB$�B&�B��B��B��Bs�BdZB+B+B
�#B
ÖB
�'B
�hB
� B
O�B
/B	�mB	��B	s�B	^5B	;dB	$�B	�B	bB��B�BȴB�B��B��B�VB�+B{�Bw�Bm�BhsB[#BR�BL�BE�B8RB2-B(�B!�B�BVBJB
=B\B�B-B>wBVBk�B�B��B�XBĜB��B�B��B	1B	�B	'�B	;dB	N�B	]/B	iyB	o�B	�oB	�FB	�B	�B
B
\B
�B
&�B
0!B
9XB
B�B
I�B
O�B
W
B
\)B
aHB
gmB
m�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BVB+QB/5B33B;B0�B2aB/5B.cB2GB'�B6�B3�B:�BI�BR�BV�BZBk�ByrB�Bw�Bm]BX+BP}BOB?�B&2B(�B��B͟B��Bt�Bh
B-CB	�B
ܒB
��B
�B
��B
��B
QhB
1AB	�eB	�?B	t�B	_�B	<�B	&2B	�B	 B��B��BɺB�oB��B�B��B��B|�Bx�BnIBi_B[�BTaBM�BG+B9XB33B*B"�B?BBB�B)BBkB-�B?BV�Bk�B��B�'B��B��B�,B�B�	B	fB	�B	($B	;�B	OB	]dB	i�B	o�B	��B	�zB	�1B	�B
'B
vB
�B
'B
0;B
9rB
B�B
I�B
O�B
W$B
\CB
aHB
g�B
m�B
r�B
v�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006100015302020061000153020200610001530202006100200082020061002000820200610020008202006110012532020061100125320200611001253  JA  ARFMdecpV4_b                                                                20200527065220  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200527065221  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200527065222  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200527065222  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200527065222  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200527065223  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200527065324                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200530215253  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200530215327  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200530215328  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200530215329  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200530215329  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200530215329  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200530215329                      G�O�G�O�G�O�                JA  ARUP                                                                        20200530215416                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200531000000  CF  PSAL_ADJUSTED_QC?�ff?�ffG�O�                JM  ARCAJMQC2.0                                                                 20200609151530  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200609151530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200609170008  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200610151253  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                