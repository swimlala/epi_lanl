CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-05-19T04:51:57Z creation;2013-09-24T05:26:20Z update;2015-06-09T19:22:07Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA  20070519045157  20150614052517  A5_28347_098                    2C  D   APEX                            1316                            013004                          846 @�w;���1   @�w>L�Xe@7j~��#�cxA�7K�1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��AfffA�ffA�ffA�ffB��BffB/33BD��BW33Bm33B���B�  B���B�33B���B���B���B�  B�  Bڙ�B�33B�ffB���CffCL�CL�C33CL�CL�CffC$L�C)�C.33C333C833C=� CB33CG�CQffC[�Ce�CoL�Cy33C�� C���C��fC���C���C��3C�� C���C���C�� C��fC���C�s3C�s3Cǳ3C̦fC�Y�C֙�CۦfC���C�fC���C���C��3C��3D�fD�3D�fD��D� D��DٚD$� D)� D.��D3�3D8��D=�3DB��DG� DLٚDQٚDV� D[�3D`� De�fDj��DoٚDt� Dy�3D�,�D�l�D�� D�� D�&fD�i�D���D��3D��D�ffD���D���D�#3D�i�Dک�D���D��D�Y�D�fD�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A��AfffA�ffA�ffA�ffB��BffB/33BD��BW33Bm33B���B�  B���B�33B���B���B���B�  B�  Bڙ�B�33B�ffB���CffCL�CL�C33CL�CL�CffC$L�C)�C.33C333C833C=� CB33CG�CQffC[�Ce�CoL�Cy33C�� C���C��fC���C���C��3C�� C���C���C�� C��fC���C�s3C�s3Cǳ3C̦fC�Y�C֙�CۦfC���C�fC���C���C��3C��3D�fD�3D�fD��D� D��DٚD$� D)� D.��D3�3D8��D=�3DB��DG� DLٚDQٚDV� D[�3D`� De�fDj��DoٚDt� Dy�3D�,�D�l�D�� D�� D�&fD�i�D���D��3D��D�ffD���D���D�#3D�i�Dک�D���D��D�Y�D�fD�	�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�~�A��A��`A���A��wA��yA�G�A�$�A�bA��A��9A���A�{A�33A�ĜA���A�hsA�JA�K�A��A��uA�-A�$�A��jA���A��A�|�A���A�XA��FA��A��+A���A���A��+A�=qA��A�A�A�K�A��uA�bNA���A�JA���A��`Aw�mAn��AdA�AZZAP�AHĜA=��A9\)A3G�A/��A,9XA(n�A&1A��A�yA�AoA�A��A
��A�TA �\A �@��T@�33@�5?@�Ĝ@�ƨ@�l�@���@���@�bN@�/@���@��u@��/@��#@�t�@��#@�Z@��y@�-@�ff@�(�@��+@�z�@��H@���@�%@�w@y%@n�y@f�@_K�@V��@O�;@H  @B^5@<�/@4��@.��@+t�@'�@"=q@��@V@�7@z�@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�~�A��A��`A���A��wA��yA�G�A�$�A�bA��A��9A���A�{A�33A�ĜA���A�hsA�JA�K�A��A��uA�-A�$�A��jA���A��A�|�A���A�XA��FA��A��+A���A���A��+A�=qA��A�A�A�K�A��uA�bNA���A�JA���A��`Aw�mAn��AdA�AZZAP�AHĜA=��A9\)A3G�A/��A,9XA(n�A&1A��A�yA�AoA�A��A
��A�TA �\A �@��T@�33@�5?@�Ĝ@�ƨ@�l�@���@���@�bN@�/@���@��u@��/@��#@�t�@��#@�Z@��y@�-@�ff@�(�@��+@�z�@��H@���@�%@�w@y%@n�y@f�@_K�@V��@O�;@H  @B^5@<�/@4��@.��@+t�@'�@"=q@��@V@�7@z�@l�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBiyBiyBjBk�Bl�Bl�BiyBe`BgmBe`BaHB_;Br�Bu�B�B�=B�oB�{B��B�dB��B�B�NB�TB�ZB�TB�/B�NB�TB�HB�;B�;B��BŢB�^B��B�=Bl�BXB;dB�B�^Bl�B�B
ŢB
`BB	��B	��B	l�B	'�B�B�sB�}B�B��B��B�B��B��B��B�bB�Bz�Bq�Bq�BiyB^5BcTBu�BW
BYBN�BB�BT�B[#B|�B�uB��BɺB��B�fB	B	�B	33B	B�B	^5B	t�B	�B	��B	�RB	ǮB	�B	�NB	�sB	�B	��B
B
\B
�B
#�B
.B
8RB
?}B
F�B
J�B
P�B
XB
[#B
`BB
dZB
k�B
q�B
t�B
x�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BiyBiyBjBk�Bl�Bm�BjBe`BgmBe`BaHB`BBs�Bv�B�B�DB�oB��B��B�jB��B�B�TB�TB�`B�ZB�5B�TB�TB�NB�HB�BB�
BǮB�dB�B�JBm�BZB=qB��B�jBn�B�B
ȴB
cTB	��B	ÖB	o�B	+B�B�B��B�B��B��B�B��B��B��B�oB�B{�Br�Br�Bk�B_;BcTBw�BXBZBO�BC�BVB[#B}�B�{B��B��B��B�fB	B	�B	33B	B�B	^5B	t�B	�B	��B	�RB	ǮB	�B	�NB	�sB	�B	��B
B
\B
�B
#�B
.B
8RB
?}B
F�B
J�B
P�B
XB
[#B
`BB
dZB
k�B
q�B
t�B
x�B
}�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200706011720592007060117205920070601172059200706011729212007060117292120070601172921201309120000002013091200000020130912000000  JA  ARFMfmtp2.3                                                                 20070519045157  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070519045158  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcp2.6                                                                 20070519045158  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.6                                                                 20070519045158  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20070519050307                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20070523034132  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20070523034133  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcp2.6                                                                 20070523034133  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.6                                                                 20070523034133  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20070523040738                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065042  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070043  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070044  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070044  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070044  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070045  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070045  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070045  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070045  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090401070045  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070449                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20070522151924  CV  DAT$            G�O�G�O�F���                JM  ARSQJMQC1.0                                                                 20070522151924  CF  PRES            @���D�	�G�O�                JM  ARSQJMQC1.0                                                                 20070522151924  CF  TEMP            @���D�	�G�O�                JM  ARSQJMQC1.0                                                                 20070522151924  CF  PSAL            @���D�	�G�O�                JM  ARCAJMQC1.0                                                                 20070601172059  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070601172059  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070601172921  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052424  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052620                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192202                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052517                      G�O�G�O�G�O�                