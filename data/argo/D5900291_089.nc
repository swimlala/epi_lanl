CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   I   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-07-08T04:46:17Z creation;2009-03-18T05:16:14Z update;2015-06-08T13:57:10Z conversion to V3.1;     
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
resolution        =���   standard_name         sea_water_pressure     axis      Z        $  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  :�   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       $  ;   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <4   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  <�   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  =�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  >�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  ?   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @8   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  @�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  A�   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  B�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  C   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  D<   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  E�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   F<   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O<   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X<   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a<   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    b   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b    HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b0   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b4   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b8   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b<Argo profile    3.1 1.2 19500101000000  5900291 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               YA   JA  20050708044617  20150617054511  A4_13305_089                    2C  D   APEX                            692                             072602                          846 @��<_���1   @��>��Y�@+�ě��T�c�M���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A33AffA�  A���B��BDffBm33B�ffB�ffB�ffB�  B���B���C�3CffC� CL�C)��C3� C=  CG�C[��Co33C�� C���C�� C��fC���C��fC��3CǙ�Cр C�s3C�3C�fC��fD�3DٚD��D��D�fD�fD��D$ٚD)� D.ٚD3�3D8�3D=� DBٚDGٚDN  DT33DZ��D`��Dg&fDmS3Ds�fDy� D�)�D�l�D���D�� D�)�D�i�D��fD�s3D��fD�i�D��3D�c3D���D��31111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@���Ap  A���B
��B2ffB[33B�ffB�ffB�ffB�  B���B���B�ffC�fC  C��C%�C/  C8� CB��CW�Cj�3C  C�Y�C�@ C�ffC�L�C�ffC�s3C�Y�C�@ C�33C�s3C�ffC�ffD �3D��D
��D��D�fD�fD��D#��D(� D-��D2�3D7�3D<� DA��DF��DL� DS3DYl�D_��DffDl33DrffDx� D���D���D��D�P D���D�ٚD�VfD��3D�VfD�ٚD�S3D��3D�\�D��31111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A҉7A�t�A�"�A���A���A���A��A��mA�bA�JA�p�A���A�x�A��A���A�-A{��Ai��AV��AO��AI�A1x�A(�A"��A��AȴA��A5?AA	�hAhsAȴAƨ@��@��D@�G�@�E�@��@�J@�~�@�x�@϶F@˅@��@�{@���@�@�9X@���@���@�;d@�  @��m@��m@�/@��j@�V@���@�(�@�I�@�@z~�@r��@j�@d��@Pb@?;d@0Q�@%@^5@G�@o@
�1111111111111111111111111111111111111111111111111111111111111111111111111   A҉7A�t�A�"�A���A���A���A��A��mA�bA�JA�p�A���A�x�A��A���A�-A{��Ai��AV��AO��AI�A1x�A(�A"��A��AȴA��A5?AA	�hAhsAȴAƨ@��@��D@�G�@�E�@��@�J@�~�@�x�@϶F@˅@��@�{@���@�@�9X@���@���@�;d@�  @��m@��m@�/@��j@�V@���@�(�@�I�@�@z~�@r��@j�@d��@Pb@?;d@0Q�@%@^5@G�@o@
�1111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	_;B	^5B	^5B	^5B	^5B	^5B	_;B	_;B	YB
ŢB
E�B
|�B
t�B
^5B	�5B	�B	`BB	&�B�B	/B	�VB	Q�B	dZB	�7B	��B	�'B	�dB	�jB	�;B
	7B
.B
.B
)�B
�B
�B
�B
VB
hB
PB
1B
+B	��B	��B	��B	��B	��B
  B
B
+B
B
	7B
PB
hB
�B
�B
�B
�B
�B
#�B
+B
2-B
7LB
=qB
B�B
G�B
T�B
`BB
hsB
q�B
y�B
�B
�+B
�+1111111111111111111111111111111111111111111111111111111111111111111111111   B	`BB	_;B	_;B	_;B	_;B	_;B	`BB	aHB	aHB
��B
L�B
�B
y�B
gmB	�ZB	�9B	ffB	.B�B	33B	�uB	T�B	gmB	�JB	��B	�9B	�wB	�wB	�HB
DB
0!B
0!B
,B
!�B
�B
!�B
bB
{B
bB

=B
	7B	��B	��B	��B	��B	��B
B
B
	7B
+B
DB
\B
uB
�B
�B
�B
�B
!�B
%�B
-B
49B
9XB
?}B
D�B
I�B
W
B
bNB
jB
s�B
{�B
�B
�7B
�71111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 4.5 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200507211049492005072110494920050721104949200701260455192007012604551920070126045519200710110000002007101100000020071011000000  JA  ARFMfmtp2.2                                                                 20050708044617  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050708044618  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050708045929                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050712004737  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050712004738  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050712005945                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20050721104949  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20050721104949  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126045519  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20071011000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071015075206  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071015090822                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312113702  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318050759  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318051614                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608135701                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617054511                      G�O�G�O�G�O�                