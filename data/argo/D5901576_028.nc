CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-03-05T09:59:04Z creation;2009-09-01T09:17:46Z update;2015-06-11T11:27:00Z conversion to V3.1;     
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
_FillValue                  t  ;p   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >(   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B<   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  FP   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H    PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  H�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Jd   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M8   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V8   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _8   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h8   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i,   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i0   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i4   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20090305095904  20150614170515  A9_76264_028                    2C  D   APEX                            3512                            070407                          846 @�L��u�1   @�O:Ӫ@+M�����d}���o1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��Ai��A�33A�33A�33BffB��B0  BD��BY��Bn  B�33B�ffB�33B�33B���B�  B���B�33B�33B�  B�33B�ffB���CL�CffC�C  C33C� C��C$��C)33C.ffC3��C8L�C=ffCB33CG� CQ�C[  CeL�Cn�fCy� C���C���C��3C��fC���C�� C��3C���C�� C���C�s3C���C���C¦fCǙ�C̳3Cѳ3C֦fCی�C���C� C��C�s3C�Y�C��fD� DٚD��D��D�3D�3D�fD$�fD)ٚD.��D3��D8�3D=�fDBٚDG��DL��DQ�fDV�3D[� D`�3DeٚDj��Do�fDt��DyٚD�#3D�\�D��3D���D�)�D�\�D��3D��D�  D�l�D��3D���D�  D�` Dڬ�D��3D��D�Y�D��D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���A��Ad��A���A���A���B33B��B.��BC��BXffBl��B���B���B���B���B�  B�ffB�33Bƙ�BЙ�B�ffB㙚B���B�33C  C�C
��C�3C�fC33CL�C$L�C(�fC.�C3L�C8  C=�CA�fCG33CP��CZ�3Ce  Cn��Cy33C��fC��fC���C�� C�s3C�Y�C���C�s3C�Y�C�s3C�L�C�s3C�s3C C�s3Č�Cь�Cր C�ffC�ffC�Y�C�ffC�L�C�33C�� D��D�fD��D��D� D� D�3D$�3D)�fD.�fD3��D8� D=�3DB�fDG�fDL��DQ�3DV� D[��D`� De�fDj��Do�3Dt��Dy�fD��D�S3D���D��3D�  D�S3D���D�� D�fD�c3D���D��3D�fD�VfDڣ3D�ٚD�3D�P D�3D�ɚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�9XA�-A�1'A�5?A�9XA�33A�5?A�33A�/A�-A�-A�+A�$�AʶFA�1A�  A��A�jA�
=A�9XA�ĜA��!A�1A���A�z�A�M�A�E�A�bNA�v�A�A��9A�dZA}|�At~�AgAdbNAT1AK��AE��A@��A;�TA3+A+��A!�^AƨAhsAAA��AƨA�uA�mAbNAdZA9XAK�A�uAC�A
�A	�A��A�#AXA 1'@��@�?}@���@���@��@�  @�ȴ@އ+@�Z@�@��
@ũ�@��;@�O�@�Ĝ@��/@�`B@�C�@�r�@�{@���@�X@��y@�  @���@�I�@�$�@�l�@�?}@��R@� �@���@x �@o��@g�@]`B@U`B@KC�@B�@;�F@5�-@.E�@&��@"M�@��@Ĝ@�D@�P@S�@	hs@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�9XA�-A�1'A�5?A�9XA�33A�5?A�33A�/A�-A�-A�+A�$�AʶFA�1A�  A��A�jA�
=A�9XA�ĜA��!A�1A���A�z�A�M�A�E�A�bNA�v�A�A��9A�dZA}|�At~�AgAdbNAT1AK��AE��A@��A;�TA3+A+��A!�^AƨAhsAAA��AƨA�uA�mAbNAdZA9XAK�A�uAC�A
�A	�A��A�#AXA 1'@��@�?}@���@���@��@�  @�ȴ@އ+@�Z@�@��
@ũ�@��;@�O�@�Ĝ@��/@�`B@�C�@�r�@�{@���@�X@��y@�  @���@�I�@�$�@�l�@�?}@��R@� �@���@x �@o��@g�@]`B@U`B@KC�@B�@;�F@5�-@.E�@&��@"M�@��@Ĝ@�D@�P@S�@	hs@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B�B�BPB	7BB
=B��B�BŢBC�B%B
|�B
�B
}�B
ffB
R�B
2-B	��B	B	�PB	M�B	2-B�B�TB�)B�B��B��BĜBƨB��B	1B	�B	A�B	y�B	�{B	��B	��B	�HB	�B
  B
B
+B
	7B

=B
1B
%B
	7B
	7B
	7B
+B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B

=B
	7B
DB
hB
uB
hB
�B
�B
�B
�B
�B
 �B
#�B
)�B
1'B
7LB
<jB
A�B
F�B
K�B
Q�B
W
B
\)B
`BB
e`B
jB
m�B
p�B
u�B
y�B
}�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
	7B�B�BbB\B1BJBB�B��BG�BPB
� B
�B
�B
k�B
W
B
8RB
B	ƨB	�uB	O�B	:^B��B�fB�/B�#B��B�
BƨBǮB��B		7B	�B	A�B	z�B	�{B	��B	��B	�HB	�B
  B
B
+B

=B

=B
	7B
+B

=B
	7B
	7B
1B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B

=B
	7B
DB
hB
uB
hB
�B
�B
�B
�B
�B
 �B
#�B
)�B
1'B
7LB
<jB
A�B
F�B
K�B
Q�B
W
B
\)B
`BB
e`B
jB
m�B
p�B
u�B
y�B
}�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.3(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200903181422262009031814222620090318142226200903181440032009031814400320090318144003200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20090305095903  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090305095904  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090305095904  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090305095905  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090305095906  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090305095906  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090305095906  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090305095906  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090305095906  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090305100550                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090309065617  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090309065719  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090309065720  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090309065720  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090309065721  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090309065721  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090309065721  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090309065721  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090309065721  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20090309065721  CV  TIME            G�O�G�O�                    JA  ARGQrelo2.1                                                                 20090309065721  CV  LAT$            G�O�G�O�AZbN                JA  ARGQrelo2.1                                                                 20090309065721  CV  LON$            G�O�G�O��#��                JA  ARUP                                                                        20090309070057                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090308223840  CV  DAT$            G�O�G�O�F��v                JM  ARCAJMQC1.0                                                                 20090318142226  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090318142226  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090318144003  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091710  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091746                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112650                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170515                      G�O�G�O�G�O�                