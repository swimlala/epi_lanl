CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-08-02T04:51:44Z creation;2013-09-24T05:26:29Z update;2015-06-09T19:16:34Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  20060802045144  20150614050517  A5_28347_069                    2C  D   APEX                            1316                            013004                          846 @�.�=�/�1   @�.�Z��@7��7Kƨ�c:�Q�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffAffAc33A���A�ffA�  B  B  B2ffBD��BZ  Bm��B���B�33B���B���B�ffB�33B���B���B�ffBڙ�B䙚B�  B���CL�CffC33CL�C� CL�CffC#�fC)33C-�fC3ffC833C<�fCB33CGffCQ  C[33CeL�CoL�Cx�fC���C��3C�ٚC�� C���C���C��fC�� C�� C�s3C��fC���C���C¦fCǌ�C�� CѦfC�s3Cی�C�� C�fC�� C�fC� C���D��D� D��D�3DٚD��DٚD$��D)� D.ٚD3��D8ٚD=� DB��DG�3DL�3DQ� DV� D[ٚD`ٚDe�3Dj� Do�3Dt��Dy�fD�0 D�i�D�� D��fD�,�D�i�D���D�� D�#3D�` D��fD��D�&fD�ffDڦfD���D�fD�c3D��D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffAffAc33A���A�ffA�  B  B  B2ffBD��BZ  Bm��B���B�33B���B���B�ffB�33B���B���B�ffBڙ�B䙚B�  B���CL�CffC33CL�C� CL�CffC#�fC)33C-�fC3ffC833C<�fCB33CGffCQ  C[33CeL�CoL�Cx�fC���C��3C�ٚC�� C���C���C��fC�� C�� C�s3C��fC���C���C¦fCǌ�C�� CѦfC�s3Cی�C�� C�fC�� C�fC� C���D��D� D��D�3DٚD��DٚD$��D)� D.ٚD3��D8ٚD=� DB��DG�3DL�3DQ� DV� D[ٚD`ٚDe�3Dj� Do�3Dt��Dy�fD�0 D�i�D�� D��fD�,�D�i�D���D�� D�#3D�` D��fD��D�&fD�ffDڦfD���D�fD�c3D��D��f2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��`A̬A̛�ÁA�I�AɓuA�1A²-A��TA��mA���A�oA���A�\)A��A���A���A�?}A�ȴA��A�"�A�ZA��A�jA�oA���A�XA�/A���A��;A��FA���A��7A�t�A�S�A�&�A�
=A��
A��-A�oA���A��!A���A���A�;dA��A��
A�/A��A�bNApv�Ak7LAcK�AUt�AI��AAS�A=O�A7�FA29XA.$�A(�DA'XA#S�A|�AE�Ar�A�A5?A	�#@�^5@�l�@��@Ѳ-@���@��@�V@�G�@�X@��
@�
=@��;@���@��@��+@���@�z�@���@�%@��@�S�@���@�%@�1@��T@��h@y�@p��@hA�@_�@VE�@N�+@I��@D��@?\)@9��@3t�@.�@*��@%p�@��@M�@�h@�;@`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��`A̬A̛�ÁA�I�AɓuA�1A²-A��TA��mA���A�oA���A�\)A��A���A���A�?}A�ȴA��A�"�A�ZA��A�jA�oA���A�XA�/A���A��;A��FA���A��7A�t�A�S�A�&�A�
=A��
A��-A�oA���A��!A���A���A�;dA��A��
A�/A��A�bNApv�Ak7LAcK�AUt�AI��AAS�A=O�A7�FA29XA.$�A(�DA'XA#S�A|�AE�Ar�A�A5?A	�#@�^5@�l�@��@Ѳ-@���@��@�V@�G�@�X@��
@�
=@��;@���@��@��+@���@�z�@���@�%@��@�S�@���@�%@�1@��T@��h@y�@p��@hA�@_�@VE�@N�+@I��@D��@?\)@9��@3t�@.�@*��@%p�@��@M�@�h@�;@`B2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�B9XB=qB;dB;dB49BG�BR�BO�BcTBv�B�bB�wB�BPB�B!�B(�B33B:^B>wB@�BE�BK�BM�BN�BO�BR�BT�BVBVBXBYBYBZB[#B[#B[#B\)B[#BW
BH�BA�B:^B)�BoB��BJ�BiyB
��B
7LB	�XB	��B	w�B	oB�B��BĜB�9B�3B��B�%B� Bw�Bo�Bk�Bl�BffB_;BW
BF�B1'B<jB8RB8RB?}BE�BS�Bu�B�uB�'B��B��B	B	�B	B�B	B�B	W
B	o�B	�1B	��B	�}B	��B	�B	�BB	��B
1B
oB
�B
!�B
+B
1'B
49B
?}B
D�B
L�B
P�B
W
B
\)B
_;B
ffB
jB
o�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BE�B9XB=qB;dB;dB7LBJ�BVBT�Be`Bx�B�oB��B�BVB�B"�B(�B33B:^B>wBA�BE�BK�BM�BN�BO�BR�BT�BVBVBXBYBYBZB[#B[#B[#B\)B[#BXBI�BB�B:^B+BuB��BO�Bm�B
��B
;dB	�dB	��B	z�B	�B�B��BŢB�FB�9B��B�%B�Bx�Bp�Bl�Bm�BgmB`BBYBG�B33B=qB9XB9XB@�BF�BT�Bu�B�uB�'B��B��B	B	�B	B�B	B�B	W
B	o�B	�1B	��B	�}B	��B	�B	�BB	��B
1B
oB
�B
!�B
+B
1'B
49B
?}B
D�B
L�B
P�B
W
B
\)B
_;B
ffB
jB
o�B
r�B
u�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200608251713522006082517135220060825171352200701270206162007012702061620070127020616201309120000002013091200000020130912000000  JA  ARFMfmtp2.3                                                                 20060802045144  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060802045144  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060802045144  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060802045144  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060802045721                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20060806034619  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060806034619  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060806034620  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060806034620  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060806035550                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065003  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401065933  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401065933  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401065933  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401065933  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401065934  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401065934  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401065934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401065934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401065935  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070446                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20060805165611  CV  DAT$            G�O�G�O�F�v
                JM  ARSQJMQC1.0                                                                 20060805165611  CF  PRES            @�ffD��fG�O�                JM  ARSQJMQC1.0                                                                 20060805165611  CF  TEMP            @�ffD��fG�O�                JM  ARSQJMQC1.0                                                                 20060805165611  CF  PSAL            @�ffD��fG�O�                JM  ARCAJMQC1.0                                                                 20060825171352  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060825171352  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070127020616  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052441  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052629                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609191629                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614050517                      G�O�G�O�G�O�                