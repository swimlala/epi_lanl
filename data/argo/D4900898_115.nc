CDF   "   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-08-30T15:57:02Z creation;2015-03-10T02:10:41Z update;2015-06-07T15:15:11Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA  20090830155702  20150613142512  A9_60144_115                    2C  D   APEX                            2414                            061305                          846 @�GԎX1   @�G��5��@C˥�S���d	V�1   ARGOS   A   A   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A33Ak33A���Ař�A�ffB33B33B0��BC��BX  Bk33B33B���B�ffB�ffB���B���B���Bƙ�B���B�  B�  BB�ffCL�CL�C
��C  C�CL�C�fC$�C)� C.�C3��C8ffC=��CB��CGL�CQ33C[�Ce� Co�Cy� C�� C���C���C���C���C�s3C�s3C���C���C��fC�s3C���C��3C³3C�� C�� Cљ�Cր Cی�C���C�s3C�s3CC� C�s3D�3DٚD��D�3D�3D� D��D$�fD)�fD.��D3�3D8�fD=ٚDB� DG� DL�3DQ�3DV��D[��D`��DeٚDj� Do��Dt��Dy�3D�  D�i�D���D��fD�)�D�c3D���D���D�#3D�` D���D��fD�  D�p Dڬ�D���D�&fD�S3D� D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A	��AY��A���A���AᙚB��B��B,ffB?33BS��Bf��Bz��B�ffB�33B�33B�ffB���B���B�ffBΙ�B���B���B�ffB�33C 33C33C	�3C�fC  C33C��C#  C(ffC-  C2� C7L�C<� CA� CF33CP�CZ  CdffCn  CxffC��3C��C��C�  C�  C��fC��fC�  C��C��C��fC��C�&fC�&fC�33C�33C��C��3C�  C�  C��fC��fC��C��3C��fD��D�3Ds3D��D��Dy�D�fD$� D)� D.s3D3l�D8� D=�3DBy�DG��DL��DQl�DV�fD[s3D`�fDe�3Dj��Do�fDts3Dy��D���D�FfD�y�D��3D�fD�@ D���D�ɚD�  D�<�D�y�D��3D���D�L�Dډ�D๚D�3D�0 D�|�D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�?}A�A�VA�(�A�  A��AhE�AY"�AT��AOS�AO`BAK�#AJJAF�AD1'AB  A=��A< �A9l�A7
=A4�uA3��A2��A2��A3��A37LA2��A0�RA-�A-t�A,��A,M�A+��A+�PA)�A'�-A& �A$�A#��A!ƨA   A�;A�A�A�AQ�A�yAhsAG�AƨA��A��A	A1A�wAS�@�J@��D@�@��
@�7L@�S�@ܓu@أ�@���@с@��@ȣ�@�hs@�@�ff@�9X@�+@���@�z�@���@�j@��-@�n�@�~�@�{@��R@�K�@��/@|j@x �@sdZ@oK�@l(�@g��@cdZ@_�@\z�@X��@O��@I%@CC�@<j@6�@2~�@.E�@*^5@&v�@"��@ȴ@o@ȴ@��@�;@�@	7L@$�@�H@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�?}A�A�VA�(�A�  A��AhE�AY"�AT��AOS�AO`BAK�#AJJAF�AD1'AB  A=��A< �A9l�A7
=A4�uA3��A2��A2��A3��A37LA2��A0�RA-�A-t�A,��A,M�A+��A+�PA)�A'�-A& �A$�A#��A!ƨA   A�;A�A�A�AQ�A�yAhsAG�AƨA��A��A	A1A�wAS�@�J@��D@�@��
@�7L@�S�@ܓu@أ�@���@с@��@ȣ�@�hs@�@�ff@�9X@�+@���@�z�@���@�j@��-@�n�@�~�@�{@��R@�K�@��/@|j@x �@sdZ@oK�@l(�@g��@cdZ@_�@\z�@X��@O��@I%@CC�@<j@6�@2~�@.E�@*^5@&v�@"��@ȴ@o@ȴ@��@�;@�@	7L@$�@�H@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B~�B|�B|�BN�B(�BL�Bx�B�B�
B�fBB��B��B��B��B�B�B�B�B��B�B��BbB �B<jBP�BcTBcTBaHBr�B�B�1B��B��B�{B�7B�+B~�Bx�Bn�Be`B[#BW
BP�BK�BH�BG�BI�BG�BC�BE�B@�B0!B(�B �B�BhB	7BB��B�B�mB�BB�)B�B�;B�BB�;B�5B�`B�B��B%BuB �B2-B=qBK�B[#Bm�Bz�B�DB��B�B�jBɺB�B�mB�B	B	VB	�B	$�B	0!B	J�B	_;B	q�B	�1B	��B	�B	�dB	��B	�B	�mB	��B
B
oB
�B
)�B
33B
=qB
G�B
O�B
S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B~�B~�B}�B}�BdZB.BYB�B�!B�B�fBB��B  B��B��B�B�B��B�B��B��B��BbB �B=qBQ�Be`Be`BbNBs�B�B�7B��B��B��B�DB�7B�Bz�Bp�BgmB]/BYBR�BM�BI�BI�BK�BI�BD�BG�BB�B2-B+B"�B�BoB
=BB��B�B�yB�HB�5B�B�HB�NB�BB�BB�mB�B��B+B{B!�B33B>wBL�B\)Bn�B{�B�JB��B�!B�qB��B�#B�sB�B	B	\B	�B	%�B	1'B	K�B	`BB	r�B	�7B	��B	�B	�jB	��B	�#B	�sB	��B
B
uB
 �B
+B
49B
>wB
H�B
P�B
T�1111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<��|<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=1.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200909121357302009091213573020090912135730200909121628322009091216283220090912162832201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090830155700  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090830155702  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090830155702  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090830155702  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090830155703  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090830155704  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090830155704  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090830155704  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090830155704  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090830155704  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090830160441                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090903095426  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090903095720  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090903095721  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090903095721  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090903095721  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090903095722  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090903095722  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090903095722  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090903095722  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090903095723  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090903100103                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090902230909  CV  DAT$            G�O�G�O�F�>�                JM  ARSQJMQC1.0                                                                 20090902230909  CF  PSAL            Ař�Ař�G�O�                JM  ARCAJMQC1.0                                                                 20090912135730  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090912135730  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090912162832  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014529  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004922                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092145  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021041                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151503                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142512                      G�O�G�O�G�O�                