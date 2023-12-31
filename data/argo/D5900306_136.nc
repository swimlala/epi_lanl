CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   H   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-10-26T10:50:34Z creation;2009-03-18T05:34:10Z update;2015-06-08T19:03:52Z conversion to V3.1;     
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
resolution        =���   standard_name         sea_water_pressure     axis      Z           9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure          ;   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <(   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <p   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           =�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           >�   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        @`   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          A�   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          B�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        DP   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  Ep   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   F    SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O    SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X    SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a    HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    a�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    a�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    a�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         a�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         a�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        a�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b Argo profile    3.1 1.2 19500101000000  5900306 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20061026105034  20150617140521  A4_14747_136                    2C  D   APEX                            702                             072602                          846 @�D
΁��1   @�D�vT@;���F�d��x���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A��A��A�  A�ffB  BF  Bn��B�33B���B�  B�ffB�33B���C�3CffCffC� C)33C3L�C=33CG�C[�Cn��C��fC�s3C�ffC��3C���C��3C���CǙ�CѦfC���C�fC�� C�s3D�3DٚD��D� DٚD�fD��D$�3D)��D.� D3�fD8ٚD=��DB�fDG�3DN  DTFfDZ�fD`��Dg�DmS3Ds�fDy�fD�#3D�ffD���D��D�,�D�p D�ٚD�p D���D�l�D���D�c3D�� 111111111111111111111111111111111111111111111111111111111111111111111111@�  @�  AvffAə�B��B5��B^ffB�  B���B���B�33B�  B晚B�33CL�CL�CffC%�C/33C9�CC  CW  Cj�3C33C�ffC�Y�C��fC���C��fC�� CŌ�Cϙ�C�� C㙚C��3C�ffD ��D�3D
�fDٚD�3D� D�fD#��D(�fD-��D2� D7�3D<�fDA� DF��DM�DS@ DY� D_�fDf3DlL�Dr� Dx� D�� D��3D�&fD�ffD���D���D�VfD���D�i�D��D�i�D�� D��111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�n�A�t�A�v�A�9XA�oA�XA�v�A�ĜA�1A�%A�C�A�A�A��A�JA�  A��A�
=A���AwdZAo�TAe��A[�AV�jAM+AI7LAA;dA:ffA1/A*�yA#��A�AQ�A�FA
  A�A �@�!@�E�@ޟ�@�S�@���@�I�@�{@��`@�Z@���@�n�@���@���@�Z@�@���@� �@�1'@�Ĝ@{��@uO�@p�u@hb@^��@V$�@M?}@F�@AX@3ƨ@&ȴ@K�@�@�T@/?�I�111111111111111111111111111111111111111111111111111111111111111111111111A�l�A�n�A�t�A�v�A�9XA�oA�XA�v�A�ĜA�1A�%A�C�A�A�A��A�JA�  A��A�
=A���AwdZAo�TAe��A[�AV�jAM+AI7LAA;dA:ffA1/A*�yA#��A�AQ�A�FA
  A�A �@�!@�E�@ޟ�@�S�@���@�I�@�{@��`@�Z@���@�n�@���@���@�Z@�@���@� �@�1'@�Ĝ@{��@uO�@p�u@hb@^��@V$�@M?}@F�@AX@3ƨ@&ȴ@K�@�@�T@/?�I�111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhsBiyBiyBhsBiyBhsBR�B�B�jB��BN�BJB��By�BF�B
�B
�wB
��B
�JB
W
B
1'B
B	��B	�'B	�B	k�B	D�B	 �B��B�B�FB��B� Br�B`BBS�BG�B?}B;dB9XB8RB>wBE�BG�BN�BcTBiyB~�B�+B��B�?B��B�B��B	�B	/B	D�B	XB	gmB	�B	��B	�dB	��B	�mB	��B
oB
)�B
;dB
H�B
W
B
e`B
q�111111111111111111111111111111111111111111111111111111111111111111111111BiyBjBjBiyBjBjBYB��B�wB��BR�BbB��B}�BK�B
�B
��B
��B
�hB
[#B
49B
B	��B	�9B	�B	n�B	G�B	#�B��B�)B�XB��B�Bu�BcTBW
BJ�BB�B>wB<jB;dBA�BG�BI�BQ�BffBk�B�B�7B��B�LB��B�B��B	�B	1'B	F�B	ZB	iyB	�+B	��B	�qB	�
B	�yB	��B
{B
,B
=qB
J�B
YB
gmB
s�111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 4.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200611081751272006110817512720061108175127200701260542352007012605423520070126054235200809300000002008093000000020080930000000  JA  ARFMfmtp2.3                                                                 20061026105034  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20061026105035  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061026105035  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20061026105035  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20061026110521                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20061030153657  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20061030153657  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061030153658  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20061030153658  QCP$                G�O�G�O�G�O�           1FB40JA  ARGQrelo2.1                                                                 20061030153658  CV  TIME            G�O�G�O�F� �                JA  ARGQrelo2.1                                                                 20061030153658  CV  LAT$            G�O�G�O�A�%                JA  ARGQrelo2.1                                                                 20061030153658  CV  LON$            G�O�G�O��%`�                JA  ARUP                                                                        20061030154701                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20061029174446  CV  DAT$            G�O�G�O�F� t                JM  ARGQJMQC1.0                                                                 20061029174446  CV  LAT$            G�O�G�O�A���                JM  ARGQJMQC1.0                                                                 20061029174446  CV  LON$            G�O�G�O��%W�                JM  ARCAJMQC1.0                                                                 20061108175127  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20061108175127  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126054235  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008083919  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008100643                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114257  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318053013  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318053410                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608190343                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617140521                      G�O�G�O�G�O�                