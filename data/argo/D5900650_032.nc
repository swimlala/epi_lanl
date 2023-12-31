CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-07-29T04:50:26Z creation;2009-03-18T07:29:22Z update;2015-06-09T19:59:28Z conversion to V3.1;     
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
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL                A   JA  20050729045026  20150614052515  A5_23632_032                    2C  D   APEX                            1557                            013004                          846 @�Ҁ�� �1   @�҃ "�@2Ǯz�H�b�n��O�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA  Ac33A���A�  A�33B
  B  B0��BE��BX  BlffB�33B�  B���B�33B�33B�ffB�ffB�  B�  B���B�  B�33B�ffC  C� C
�fC33C�fCffC33C$  C)� C.ffC333C8L�C=�CBL�CG  CQ33C[�CeL�Co��Cy��C�s3C���C�ffC�s3C���C��3C���C��3C���C���C��fC��fC���C³3Cǳ3C�� C�� C֙�Cی�C�� C�3C�s3C�3C�ffC�� D�3D� D� D�3D�3DٚD� D$ٚD)�fD.��D3��D8�3D=�3DB�fDG� DL�3DQ�3DV�3D[��D`�3De��Dj�fDo�fDt��Dy�3D�&fD�l�D��3D��D��D�ffD��3D�ٚD�  D�ffD��fD��3D��D�Y�Dک�D�� D��D�` D�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33AffAa��A���A�33A�ffB	��B��B0ffBE33BW��Bl  B�  B���B�ffB�  B�  B�33B�33B���B���Bڙ�B���B�  B�33C �fCffC
��C�C��CL�C�C#�fC)ffC.L�C3�C833C=  CB33CF�fCQ�C[  Ce33Co� Cy� C�ffC���C�Y�C�ffC�� C��fC���C��fC�� C���C���C���C���C¦fCǦfC̳3Cѳ3C֌�Cۀ C�3C�fC�ffC�fC�Y�C�s3D��D��DٚD��D��D�3D��D$�3D)� D.�3D3�fD8��D=��DB� DGٚDL��DQ��DV��D[�fD`��De�fDj� Do� Dt�fDy��D�#3D�i�D�� D��fD�fD�c3D�� D��fD��D�c3D��3D�� D��D�VfDڦfD���D�fD�\�D� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AыDA�-A�AЇ+A�t�A�n�A�hsA�dZA�bNA�`BA�`BA�^5A�XA�C�A;wA�9XA�O�A�z�A�VA�ȴA���A��A�^5A�VA��/A�x�A��`A�I�A�K�A��A��7A���A���A�VA�bNA���A���A�jA��A���A�S�A��A}��Ao�A`�AS�TAG�AD{A;�A7x�A.��A)C�A$9XA��AAr�AVA��A�#A��A��AX@�V@�^5@���@�
=@��#@�Ĝ@݁@Ցh@�n�@�~�@�7L@�j@��@���@��@�7L@��!@�1@�p�@��@��@�X@��@�33@��@��#@� �@���@�1'@���@�C�@�?}@~��@u`B@l�j@d9X@\(�@R�@M`B@G�@>V@9��@3��@,�j@'�@#@E�@7L@ƨ@
=@S�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AыDA�-A�AЇ+A�t�A�n�A�hsA�dZA�bNA�`BA�`BA�^5A�XA�C�A;wA�9XA�O�A�z�A�VA�ȴA���A��A�^5A�VA��/A�x�A��`A�I�A�K�A��A��7A���A���A�VA�bNA���A���A�jA��A���A�S�A��A}��Ao�A`�AS�TAG�AD{A;�A7x�A.��A)C�A$9XA��AAr�AVA��A�#A��A��AX@�V@�^5@���@�
=@��#@�Ĝ@݁@Ցh@�n�@�~�@�7L@�j@��@���@��@�7L@��!@�1@�p�@��@��@�X@��@�33@��@��#@� �@���@�1'@���@�C�@�?}@~��@u`B@l�j@d9X@\(�@R�@M`B@G�@>V@9��@3��@,�j@'�@#@E�@7L@ƨ@
=@S�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB<jB;dB<jB<jB<jB<jB<jB<jB<jB=qB=qB=qB=qB=qB\)B�9B�B�B"�B49BT�Bm�Bz�B�=B�B�+B�7B�hB�FB�B�oBl�BVBO�B7LB�`BB~�BD�BVB
��B
O�B	�`B	s�B	PBŢB��B�VB�oB�uB{�B}�B�+B�JB�JB�uB��B��B��B��B�oB�7B�B�B~�B�uB��B��B�
B��B��BÖB�B�mB	  B	%�B	C�B	W
B	o�B	�7B	��B	�9B	�jB	ŢB	��B	�B	�5B	�mB	�B	��B	��B	��B
B
+B
DB
{B
�B
$�B
,B
9XB
A�B
F�B
J�B
Q�B
YB
YB
]/B
bNB
hsB
l�B
r�B
u�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B<jB;dB<jB<jB<jB<jB<jB<jB<jB=qB=qB=qB=qB=qB\)B�9B�B�B"�B49BT�Bm�Bz�B�=B�B�+B�7B�hB�FB�B�oBl�BVBO�B7LB�`BB~�BD�BVB
��B
O�B	�`B	s�B	PBŢB��B�VB�oB�uB{�B}�B�+B�JB�JB�uB��B��B��B��B�oB�7B�B�B~�B�uB��B��B�
B��B��BÖB�B�mB	  B	%�B	C�B	W
B	o�B	�7B	��B	�9B	�jB	ŢB	��B	�B	�5B	�mB	�B	��B	��B	��B
B
+B
DB
{B
�B
$�B
,B
9XB
A�B
F�B
J�B
Q�B
YB
YB
]/B
bNB
hsB
l�B
r�B
u�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200508110000002005081100000020050811000000200604190000002006041900000020060419000000JA  ARFMfmtp2.2                                                                 20050729045026  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050729045027  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050729045811                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050802005059  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050802005100  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050802005844                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050811000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050811000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013434  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013658                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120633  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072400  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072922                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609195923                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052515                      G�O�G�O�G�O�                