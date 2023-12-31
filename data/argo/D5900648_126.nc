CDF   1   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-02-23T06:50:25Z creation;2013-09-24T05:26:22Z update;2015-06-09T19:27:33Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA  20080223065025  20150614052518  A5_28347_126                    2C  D   APEX                            1316                            013004                          846 @Խ@?���1   @ԽB�%�@5ۥ�S���cm�hr�!1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Aa��A�  A�ffA�  B	��BffB1��BE��BY��Bl��B���B�  B�ffB���B�33B�  B�ffB�33B�  B�  B䙚B�  B�33CffCffC� CL�CffC33C�C$33C)33C.33C3L�C8� C=�CA�fCGffCQffC[ffCe�Co� CyffC�� C��fC�� C���C���C��3C���C��fC��3C��3C���C��3C�� C CǙ�C̀ CѦfC֙�Cۙ�C�fC�� CꙚCC� C��fD�3D�fD��D��D�fD� D�fD$�fD)�fD.�3D3��D8ٚD=ٚDB� DG�3DL��DQ� DVٚD[� D`��De��Dj� Do��Dt� DyٚD�,�D�` D���D��fD�&fD�i�D��fD��D�  D�l�D���D��3D�)�D�i�Dڠ D��3D��D�c3D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A33Aa��A�  A�ffA�  B	��BffB1��BE��BY��Bl��B���B�  B�ffB���B�33B�  B�ffB�33B�  B�  B䙚B�  B�33CffCffC� CL�CffC33C�C$33C)33C.33C3L�C8� C=�CA�fCGffCQffC[ffCe�Co� CyffC�� C��fC�� C���C���C��3C���C��fC��3C��3C���C��3C�� C CǙ�C̀ CѦfC֙�Cۙ�C�fC�� CꙚCC� C��fD�3D�fD��D��D�fD� D�fD$�fD)�fD.�3D3��D8ٚD=ٚDB� DG�3DL��DQ� DVٚD[� D`��De��Dj� Do��Dt� DyٚD�,�D�` D���D��fD�&fD�i�D��fD��D�  D�l�D���D��3D�)�D�i�Dڠ D��3D��D�c3D��D���3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��hA�E�A�S�A��A���A��A�&�A���A�O�A�"�A���A�C�A�oA�oA��A��A���A��TA�ĜA�v�A�bA�G�A�JA�1A�dZA�A��HA��A��A���A�A���A�bNA��hA�r�A��A��#A�G�A�XA��#A���A{dZAn=qAhM�AZE�AO�PAFbA=+A6-A1hsA,Q�A&�yA%��A#�7A��Ap�AffAA�A��A��A
�DA5?A�w@�x�@�ff@�33@�V@�&�@�@�"�@�J@��-@��-@�  @�G�@��H@��y@���@�S�@���@�9X@�=q@�9X@��@�O�@��@�;d@�-@��@�`B@��w@���@�;@{�F@r-@i��@c@ZM�@R�\@J^5@CS�@=�-@7�P@2J@)�@&�y@#C�@+@t�@��@��@�D@Ĝ@�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113 A��yA��hA�E�A�S�A��A���A��A�&�A���A�O�A�"�A���A�C�A�oA�oA��A��A���A��TA�ĜA�v�A�bA�G�A�JA�1A�dZA�A��HA��A��A���A�A���A�bNA��hA�r�A��A��#A�G�A�XA��#A���A{dZAn=qAhM�AZE�AO�PAFbA=+A6-A1hsA,Q�A&�yA%��A#�7A��Ap�AffAA�A��A��A
�DA5?A�w@�x�@�ff@�33@�V@�&�@�@�"�@�J@��-@��-@�  @�G�@��H@��y@���@�S�@���@�9X@�=q@�9X@��@�O�@��@�;d@�-@��@�`B@��w@���@�;@{�F@r-@i��@c@ZM�@R�\@J^5@CS�@=�-@7�P@2J@)�@&�y@#C�@+@t�@��@��@�D@Ĝ@�w3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBC�BA�B9XB"�B�B�B�B@�BM�Bw�B�B�uB��B��B��B��B��B��BƨB��B��B��B��B�jB��B�PB�B|�Bo�BhsBQ�B9XB'�B%B��B��Bl�B-B
�B
��B
n�B
Q�B
oB	��B	iyB	oB��B��B��B�hB�hB�hB�oB�bB�JB�B~�By�Bu�BjBhsB`BBXBP�BJ�BF�B=qB8RB8RB7LB6FB?}BR�BhsB�B��B��B�}B��B�B	B	�B	C�B	]/B	w�B	�JB	��B	�-B	ÖB	��B	�B	�TB	�B	��B	��B
\B
�B
$�B
.B
8RB
B�B
I�B
P�    B
^5B
gmB
iy    B
p�B
t�B
z�B
~�B
�    B
�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141111143 BC�BA�B:^B#�B�B�B�B@�BM�Bw�B�B�uB��B��B��B��B��B��BƨB��B��B��B��B��B��B�VB�B� Bq�BjBS�B:^B)�B	7B��B��Bo�B0!B
��B
��B
o�B
S�B
�B	��B	m�B	�B��B�B��B�oB�oB�oB�oB�hB�PB�B� By�Bw�Bk�BiyBaHBYBQ�BK�BG�B>wB9XB9XB8RB7LB@�BS�BhsB�B��B��B�}B��B�B	B	�B	C�B	]/B	w�B	�JB	��B	�-B	ÖB	��B	�B	�TB	�B	��B	��B
\B
�B
$�B
.B
8RB
B�B
I�B
P�G�O�B
^5B
gmB
iyG�O�B
p�B
t�B
z�B
~�B
�G�O�B
�73333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433343333343 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed(maybe salty drift)                                                                                                                                                                                                                      200803070757212008030707572120080307075721200803070804142008030708041420080307080414201309120000002013091200000020130912000000  JA  ARFMdecpA5_a                                                                20080223065022  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080223065025  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080223065025  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080223065026  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080223065029  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080223065029  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080223065030  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080223065030  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8a                                                                20080223065030  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080223065030  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20080223065030  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080223070035                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080227034848  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080227034853  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080227034854  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080227034854  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080227034858  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080227034858  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080227034858  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080227034858  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8a                                                                20080227034858  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080227034858  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20080227034858  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080227051222                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20090401065117  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090401070153  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090401070154  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090401070154  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090401070154  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090401070155  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090401070155  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090401070155  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8b                                                                20090401070155  QCF$                G�O�G�O�G�O�            4040JA  ARGQaqcp2.8b                                                                20090401070155  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090401070155  QCF$                G�O�G�O�G�O�            4040JA  ARGQrqcpt16b                                                                20090401070156  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090401070447                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080226170025  CV  DAT$            G�O�G�O�F��                JM  ARSQJMQC1.0                                                                 20080226170025  CF  PRES            @���D���G�O�                JM  ARSQJMQC1.0                                                                 20080226170025  CF  TEMP            @���D��G�O�                JM  ARSQJMQC1.0                                                                 20080226170025  CF  PSAL            @���D�c3G�O�                JM  ARCAJMQC1.0                                                                 20080307075721  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080307075721  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080307080414  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2013V01                                                      20130912000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052451  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052622                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609192728                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052518                      G�O�G�O�G�O�                