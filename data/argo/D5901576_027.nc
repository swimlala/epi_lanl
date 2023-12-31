CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-02-23T09:59:03Z creation;2009-09-01T09:17:41Z update;2015-06-11T11:26:47Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20090223095903  20150614170510  A9_76264_027                    2C  D   APEX                            3512                            070407                          846 @���r�1   @��+�]@+&�x����dw+I�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Ai��A���A�  A�33B	��B��B/33BE33BY��Bk��B�ffB���B���B�33B���B���B�ffB�ffB�33B�  B�33B�ffB���C�CffC� C33C��C�3C33C$L�C)ffC.L�C3ffC833C=ffCB�CG33CQ�C[�CeffCo� Cy��C��3C��fC���C�� C�� C�s3C���C��3C��3C���C���C��fC���C³3CǦfC̳3CѦfC֦fC�� C�fC�fC�3CC��3C�� DٚD�fD��D��DٚDٚD�3D$�3D)� D.ٚD3� D8ٚD=ٚDB�3DG� DL��DQ�3DV�fD[� D`� De�fDj�3Do� Dt� DyٚD�)�D�i�D���D���D�)�D�i�D�� D��D�#3D�` D���D��3D��D�Y�Dک�D��fD��D�i�D�fD��3D�Y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���A33Ah  A���A�33A�ffB	33B33B.��BD��BY33Bk33B�33B���B�ffB�  B���B���B�33B�33B�  B���B�  B�33B�ffC  CL�CffC�C� C��C�C$33C)L�C.33C3L�C8�C=L�CB  CG�CQ  C[  CeL�CoffCy� C��fC���C���C�s3C�s3C�ffC���C��fC��fC�� C���C���C���C¦fCǙ�C̦fCљ�C֙�C۳3C���C噚C�fC��C��fC��3D�3D� D�fD�3D�3D�3D��D$��D)ٚD.�3D3��D8�3D=�3DB��DG��DL�3DQ��DV� D[��D`��De� Dj��DoٚDtٚDy�3D�&fD�ffD���D�ٚD�&fD�ffD���D��fD�  D�\�D��fD�� D�fD�VfDڦfD��3D��D�ffD�3D�� D�Vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��#A��#A��;A��;A��/A��
A���A��/A�ȴAͶFA͸RA͡�A͉7A�oA���A�A̸RA̴9Ȁ\A��HA�`BA�z�A��#A���A� �A���A�?}A�^5A���A�C�A�A�A|{Af�HA\�AT�!AP-AK�AFn�AD1A;�TA1?}A(=qA!�-A=qA�A��A1AE�A�TAv�A9XAO�A33A&�A
bA	S�A+A�A%At�A`B@���@��\@��\@�dZ@�"�@�9@�@�v�@�V@��y@�~�@Լj@��H@Ͳ-@�V@�~�@���@�
=@�1@���@�{@��T@�l�@�b@���@�Ĝ@��+@�^5@��m@���@�$�@�b@�hs@�ȴ@���@}�@sdZ@i7L@_��@V{@O�;@J�@A��@7�P@0r�@)&�@$9X@�y@J@��@�@�@
��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��#A��#A��;A��;A��/A��
A���A��/A�ȴAͶFA͸RA͡�A͉7A�oA���A�A̸RA̴9Ȁ\A��HA�`BA�z�A��#A���A� �A���A�?}A�^5A���A�C�A�A�A|{Af�HA\�AT�!AP-AK�AFn�AD1A;�TA1?}A(=qA!�-A=qA�A��A1AE�A�TAv�A9XAO�A33A&�A
bA	S�A+A�A%At�A`B@���@��\@��\@�dZ@�"�@�9@�@�v�@�V@��y@�~�@Լj@��H@Ͳ-@�V@�~�@���@�
=@�1@���@�{@��T@�l�@�b@���@�Ĝ@��+@�^5@��m@���@�$�@�b@�hs@�ȴ@���@}�@sdZ@i7L@_��@V{@O�;@J�@A��@7�P@0r�@)&�@$9X@�y@J@��@�@�@
��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�mB	�yB	�yB	�B
,B
��B1'BVBZBp�Bz�BiyBS�B
�fB
��B
0!B	�XB	I�B	%�B	B�B�mB�mB�#B�#B��B�dBɺB��B��B	B�B	�\B	��B	��B	�B	��B	��B	��B
B
%B
1B
B
VB
JB
PB
JB
1B
+B
%B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
+B
+B
	7B

=B
DB
�B
�B
�B
�B
�B
�B
�B
!�B
'�B
1'B
7LB
;dB
A�B
E�B
M�B
Q�B
VB
[#B
_;B
cTB
hsB
l�B
p�B
t�B
w�B
{�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�mB	�yB	�yB	�B
2-B
��B7LB]/B_;Bs�B|�Bk�B\)B
�B
��B
9XB	ÖB	N�B	)�B	%B�B�B�sB�/B�5B��B�qB��B��B��B	B�B	�\B	��B	��B	�#B	��B	��B	��B
B
%B
	7B
B
\B
JB
VB
PB
	7B
1B
%B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
+B
+B
	7B

=B
DB
�B
�B
�B
�B
�B
�B
�B
!�B
'�B
1'B
7LB
;dB
A�B
E�B
M�B
Q�B
VB
[#B
_;B
cTB
hsB
l�B
p�B
t�B
w�B
{�B
� B
�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200903081338432009030813384320090308133843200903081348312009030813483120090308134831200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20090223095902  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090223095903  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090223095903  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090223095903  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090223095905  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090223095905  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090223095905  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090223095905  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090223095905  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090223100440                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090227035647  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090227035804  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090227035805  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090227035805  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090227035806  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090227035806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090227035806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090227035806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090227035806  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090227040148                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090226231937  CV  DAT$            G�O�G�O�F��8                JM  ARCAJMQC1.0                                                                 20090308133843  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090308133843  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090308134831  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091719  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091741                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112638                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170510                      G�O�G�O�G�O�                