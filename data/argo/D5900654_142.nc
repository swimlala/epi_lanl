CDF   :   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-08-04T06:49:57Z creation;2009-09-01T08:43:31Z update;2015-06-09T21:24:35Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080804064957  20150621190523  A5_23712_142                    2C  D   APEX                            1566                            013004                          846 @����Sx1   @��4%�@2�
=p���c���+1   ARGOS   A   A   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���AffAc33A���A���A���B
  B��B2  BF��B[33BnffB�33B���B�  B�ffB�33B���B�  B�ffB���Bٙ�B�  B�33B�33CffCL�C33CffC�C  C� C$  C)  C.  C3  C833C<��CB� CG33CQL�C[L�CeL�CoffCyL�C��fC�ffC�� C��3C��fC���C���C��fC�� C��fC�� C�� C�� C�ٚCǦfC̳3Cь�C֦fC�ffC�3C�fC��C��C� C�s3D�fD�3D� D��D��D� D��D$�3D)�3D.��D3�fD8�3D=��DB��DG�fDL�3DQ�3DV�3D[� D`�fDe�fDj�3Do��Dt��Dy� D�&fD�c3D��3D���D��D�i�D���D��D�)�D�p D���D���D�&fD�l�Dڣ3D�ٚD�#3D�P D�3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA��AY��A�  A�  A�  B��B33B/��BDffBX��Bl  B�  B�ffB���B�33B�  B���B���B�33Bϙ�B�ffB���B�  B�  C ��C�3C
��C��C� CffC�fC#ffC(ffC-ffC2ffC7��C<33CA�fCF��CP�3CZ�3Cd�3Cn��Cx�3C�Y�C��C�33C�ffC�Y�C�L�C�@ C�Y�C�33C�Y�C�33C�33C�33C�C�Y�C�ffC�@ C�Y�C��C�ffC�Y�C�@ C�@ C�33C�&fD� D��D��D�3D�3D��D�fD$��D)��D.�fD3� D8��D=�3DB�fDG� DL��DQ��DV��D[��D`� De� Dj��Do�fDt�fDy��D�3D�P D�� D�ٚD�	�D�VfD���D��fD�fD�\�D���D�ɚD�3D�Y�Dڐ D��fD� D�<�D� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�%A���A�t�A��HAҮAџ�A�x�AЗ�A�^5A��A�?}A�  A�1A��A���A�jA�jA��/A��DA��7A�^5A�;dA���A�A�r�A���A��A�G�A�ȴA���A�E�A�1'A�7LA�C�A��A��TA�;dA�VA�O�A���A��A�E�A}x�An�HA^��AR�AN �AE�-AA�;A< �A4��A-K�A&�yA%�7A
=A(�A�^A�At�A �A%AĜ@��u@�@�;d@�P@�V@ܬ@�C�@�J@�I�@�l�@�?}@�/@�{@�{@���@���@�|�@���@�Z@�v�@�9X@�33@��@���@��h@�J@�ȴ@�Z@�ȴ@�ƨ@�p�@�|�@���@��@w;d@pb@hQ�@^ȴ@X  @NV@E�-@;dZ@2�!@*�\@%�h@"-@ff@�!@�@��@�;@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113 A���A�%A���A�t�A��HAҮAџ�A�x�AЗ�A�^5A��A�?}A�  A�1A��A���A�jA�jA��/A��DA��7A�^5A�;dA���A�A�r�A���A��A�G�A�ȴA���A�E�A�1'A�7LA�C�A��A��TA�;dA�VA�O�A���A��A�E�A}x�An�HA^��AR�AN �AE�-AA�;A< �A4��A-K�A&�yA%�7A
=A(�A�^A�At�A �A%AĜ@��u@�@�;d@�P@�V@ܬ@�C�@�J@�I�@�l�@�?}@�/@�{@�{@���@���@�|�@���@�Z@�v�@�9X@�33@��@���@��h@�J@�ȴ@�Z@�ȴ@�ƨ@�p�@�|�@���@��@w;d@pb@hQ�@^ȴ@X  @NV@E�-@;dZ@2�!@*�\@%�h@"-@ff@�!@�@��@�;@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B+B(�B%�B!�B�B{B�BPB
��B
��B
�B7LB��BVBs�B�hB�?BƨBɺB�B�NBɺB�B��B�-B��Bk�BG�B%�B�B  B�BB�9B��B�=BdZBZBG�B�B
��B
��B
N�B
uB	�9B	R�B	�B	B�NB��B�^B�FB��B��B��B�hB�DB�B� B~�B�=B��B��B��B��B�B��B�wB��B�/B�B��B	+B	@�B	W
B	hsB	x�B	�\B	��B	�!B	�RB	�qB	ĜB	��B	��B	�
B	�)B	�NB	�B	�B	��B	��B	��B
B
+B
hB
�B
!�B
&�B
/B
6FB
;dB
C�B
I�B
S�B
[#B
aHB
k�B
jB
m�B
q�B
t�B
x�    B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111143 B)�B+B(�B&�B!�B �B{B�BbB
�B
��B
�B7LB��BW
Bt�B�oB�?BƨB��B�B�TB��B�!B��B�9B��Bm�BI�B&�B�BB�ZB�FB��B�PBe`B[#BK�B �B
��B
��B
P�B
�B	�RB	VB	�B	B�TB��B�jB�RB��B��B��B�oB�PB�B�B� B�DB��B��B��B��B�!B��B�}B��B�5B�B��B	+B	@�B	W
B	hsB	x�B	�\B	��B	�!B	�RB	�qB	ĜB	��B	��B	�
B	�)B	�NB	�B	�B	��B	��B	��B
B
+B
hB
�B
!�B
&�B
/B
6FB
;dB
C�B
I�B
S�B
[#B
aHB
k�B
jB
m�B
q�B
t�B
x�G�O�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.6(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200808171414262008081714142620080817141426200808171428002008081714280020080817142800200908260000002009082600000020090826000000  JA  ARFMdecpA5_a                                                                20080804064954  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080804064957  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080804064957  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080804064958  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080804065002  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080804065002  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080804065002  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080804065002  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8a                                                                20080804065002  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080804065002  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20080804065002  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080804070523                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080808034850  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080808034855  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080808034856  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080808034856  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080808034900  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080808034900  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080808034901  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080808034901  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8a                                                                20080808034901  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080808034901  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20080808034901  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080808052904                      G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080912002146  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080912002146  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080912002150  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080912002150  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080912002151  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080912002151  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8a                                                                20080912002151  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080912002151  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16b                                                                20080912002151  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080912020045                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080808034850  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090414042858  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090414042859  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090414042859  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090414042859  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090414042900  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090414042900  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090414042900  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8b                                                                20090414042900  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8b                                                                20090414042900  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090414042900  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20090414042900  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090414042945                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080807233742  CV  DAT$            G�O�G�O�F�0.                JM  ARSQJMQC1.0                                                                 20080807233742  CF  TEMP            D��fD��fG�O�                JM  ARSQJMQC1.0                                                                 20080807233742  CF  PSAL            D��fD��fG�O�                JM  ARCAJMQC1.0                                                                 20080817141426  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080817141426  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080817142800  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090826000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901084253  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901084331                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609212424                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621190523                      G�O�G�O�G�O�                