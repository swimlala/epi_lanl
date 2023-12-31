CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:17:59Z creation;2022-06-04T19:17:59Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191759  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               *A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���/hL1   @����@/I�^5�cq��l�D1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0��B8  B@  BH  BP  BW��B`  Bg��Bp  Bx  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  Bۙ�B�  B�  B�  B�  B�  B�  B�  B�  C   C��C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(33C*  C,�C-��C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @ ��@g
>@�Q�@�AA9AYAyA��HA��HA��HA�{A�{A��HA��HA��HBp�Bp�Bp�Bp�B&p�B/=qB6p�B>p�BFp�BNp�BV
>B^p�Bf
>Bnp�Bvp�B~p�B�8RB�8RB���B�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RBӞ�B�8RB���B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC5�C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�\C)�)C+��C-h�C/��C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc��Ce��Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D g
D �
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
D	g
D	�
D
g
D
�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
D g
D �pD!g
D!�
D"g
D"�
D#g
D#�
D$g
D$�
D%g
D%�
D&g
D&�
D'g
D'�
D(g
D(�
D)g
D)�
D*g
D*�
D+g
D+�
D,g
D,�
D-g
D-�
D.g
D.�
D/g
D/�
D0g
D0�
D1g
D1�
D2g
D2�
D3g
D3�
D4g
D4�
D5g
D5�
D6g
D6�
D7g
D7�
D8g
D8�
D9g
D9�
D:g
D:�
D;g
D;�
D<g
D<�
D=g
D=�
D>g
D>�
D?g
D?�
D@g
D@�
DAg
DA�
DBg
DB�
DCg
DC�
DDg
DD�
DEg
DE�
DFg
DF�
DGg
DG�
DHg
DH�
DIg
DI�
DJg
DJ�
DKg
DK�
DLg
DL�
DMg
DM�
DNg
DN�
DOg
DO�
DPg
DP�
DQg
DQ�
DRg
DR�
DSg
DS�
DTg
DT�
DUg
DU�
DVg
DV�
DWg
DW�
DXg
DX�
DYg
DY�
DZg
DZ�
D[g
D[�
D\g
D\�
D]g
D]�
D^g
D^�
D_g
D_�
D`g
D`�
Dag
Da�
Dbg
Db�
Dcg
Dc�
Ddg
Dd�
Deg
De�
Dfg
Df�
Dgg
Dg�
Dhg
Dh�
Dig
Di�
Djg
Dj�
Dkg
Dk�
Dlg
Dl�
Dmg
Dm�
Dng
Dn�
Dog
Do�
Dpg
Dp�
Dqg
Dq�
Drg
Dr�
Dsg
Ds�
Dtg
Dt�
Dug
Du�
Dvg
Dv�
Dwg
Dw�
Dxg
Dx�
Dyg
Dy�
Dzg
Dz�
D{g
D{�
D|g
D|�
D}g
D}�
D~g
D~�
Dg
D�
D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�v�D���D��D�3�D�s�D³�D��D�3�D�s�Dó�D��D�3�D�s�Dĳ�D��D�3�D�s�Dų�D��D�3�D�s�DƳ�D��D�3�D�s�Dǳ�D��D�3�D�s�Dȳ�D��D�3�D�s�Dɳ�D��D�3�D�s�Dʳ�D��D�3�D�s�D˳�D��D�3�D�s�D̳�D��D�3�D�s�Dͳ�D��D�3�D�s�Dγ�D��D�3�D�s�Dϳ�D��D�3�D�s�Dг�D��D�3�D�s�Dѳ�D��D�3�D�s�Dҳ�D��D�3�D�s�Dӳ�D��D�3�D�s�DԳ�D��D�3�D�s�Dճ�D��D�3�D�s�Dֳ�D��D�3�D�s�D׳�D��D�3�D�pRDس�D��D�3�D�s�Dٳ�D��D�3�D�s�Dڶ�D��D�3�D�s�D۳�D��D�3�D�s�Dܳ�D��D�3�D�s�Dݳ�D��D�3�D�s�D޳�D��D�3�D�s�D߳�D��D�3�D�s�D೅D��D�3�D�s�D᳅D��D�3�D�s�DⳅD��D�3�D�s�D㳅D��D�3�D�s�D䳅D��D�3�D�s�D峅D��D�3�D�s�D泅D��D�3�D�s�D糅D��D�3�D�s�D賅D��D�3�D�s�D鳅D��D�3�D�s�D곅D��D�3�D�s�D볅D��D�3�D�s�D쳅D��D�3�D�s�D���D��D�3�D�s�DD��D�3�D�s�DﳅD��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���Aɗ�A�uZA�� Aȕ�A�.A�AUA��oA���AƤ�A�uZA�GzA�:�A�6FA�0UA�($A�{A� A��A��A��
A�ʌAſ�Aŗ$A�A�A�~A�ݘA�:�A�бA��A�v+A��NA���A��BA��UA��9A��6A���A�ݘA��TA��[A��dA��qA���A�NA���A��kA�A�e�A�e�A���A��A��A��A���A���A���A��A���A�˒A���A�A�A��-A�@A��_A���A�xA�P�A�B�A��-A�B�A�iDA�($A��dA���A��A��_A��A��%A�w�A�\�A��2A�o�A�ҽA���A�'�A�%A{�AxdZAt��Aq�^Ao��Ak��Ag�Ae5?AcA[Z�AVl�AU�ATIRAS�KAQ�bAN��AJ�AHU�AE��ADjADD�ACo�A@�oA>2�A<�A<qA;��A;ԕA<[�A<��A;J#A8�KA4�A1�A.l�A-2aA+�A)-�A'�hA&1A#�{A l"A�;A�A�_A33A(�AMAh
A��A��AخA�.A!�Az�AW�A6A��A1'A*�A4A�A�jA\)A��Aq�A�A�AQ�A�MA�A��Af�A��Ag�A�BA}VA%�AA�A�]A'�An�A��A�AffA��A&A�A\�A0�A�BA�A�eAC-A�AH�A
��A
A	�A	�A	qA�TAzA8A��At�A+�A�A:�A�0A4�A��A�9A��A��A��A{JAS�A6zA#:A�jA�A:�A��AP�A ��A ;@�u�@�Xy@��z@���@�s�@�@@��@�h
@���@�q@��@��@�V�@���@�;@��1@�	@�P�@��@��A@�p�@�G�@�@�g8@�?�@�O@��#@�T�@�V�@�:*@�1@��@@�ߤ@�<@��@���@�p�@�/@��P@�s�@�w2@�/�@�w2@�;@�'R@��@�Y@䟾@�7�@�S&@���@��)@�4n@���@��@�zx@�W?@�ی@�^5@���@ݹ�@ݎ�@��@�_�@���@�_p@��@�6@�a@ؠ�@�_@�	@ׅ�@�K�@��@�w�@�M@���@�9�@��H@ԟ�@�V�@��@�7L@�Xy@��+@�1�@��@Ъe@�L0@�($@��T@���@�oi@���@�F�@̰�@��@ˡ�@�Q�@�@�V@��f@�ѷ@ʃ�@�m�@�h
@�C-@ɵt@�j@�1�@�ѷ@��@Ǵ�@�?}@ƾ�@�ƨ@�.I@���@Ĳ�@Ċr@�7@�_p@��@�~(@�3�@�!�@���@��
@�l�@�T�@��@�N�@��	@�B�@�(�@�%@��[@��j@�_�@��m@�C�@��5@��_@�)�@���@���@���@���@���@�H@��@�خ@���@�~�@�Q�@��@���@�j@�0U@��@��}@�zx@��@��`@�n�@�($@�y�@��A@��_@���@�d�@��m@���@�Y�@�@���@��@��@�~�@�@��@��@���@�e�@��N@���@�S&@�2a@��@��@���@��o@�s�@���@���@�RT@��@���@�<�@�ݘ@�P�@�%F@���@�͟@�_@��)@���@�n/@�T�@�+@�L0@��N@�N<@���@�oi@��[@�q@��e@���@���@��7@�A�@��@���@�h�@� �@��&@��@���@��7@�L�@�1�@��@��/@���@��A@�@�@� �@���@�k�@� \@��|@�҉@��}@�Q@�c@��]@�e�@�>B@��@�B�@�+@��+@�M�@���@�Vm@�,�@��B@�5?@��o@��@�U�@�'�@��R@�^5@��
@���@�E9@�	l@���@��_@�l"@�L0@�(�@��@��&@�ƨ@���@���@���@�/�@��@��H@��s@���@���@�Ft@���@�m]@� \@��H@���@��4@�v�@�"h@���@��@�m]@�S�@�F@�@�҉@���@�}V@�n�@�Q�@�=q@�-@��@���@�m]@���@��@�N�@�#:@��r@���@�J�@�4@��I@�U2@�{@��@���@�{J@�J#@���@��I@�~�@�bN@�6@��]@��@��@�C@���@�q�@�PH@�E�@��@�X�@��@�Ĝ@���@�h�@�g�@�Y@���@���@�Ov@�	@��@�c�@��@��8@��K@��'@��I@�oi@�[�@�S�@�<�@�@��@o@~�@}��@}|@}7L@|�e@|z�@|Q�@|>B@{��@{W?@{S@z@�@yu�@x��@x��@x�4@x�I@xXy@w�[@wqv@we�@wZ�@w9�@w�@v�@v��@v�<@va|@v#:@v
�@u�@u�"@uA @uV@u;@t�o@t1'@t�@s��@r�@r��@rL0@qx�@q(�@pѷ@p��@pz�@p'R@o��@o��@o��@ob�@oY@nd�@n#:@m�@m�d@m��@mY�@m<6@l��@l~(@k�&@k>�@j�@i�#@iu�@h�_@g�K@f�M@f��@fu%@fh
@fB[@e�.@e�^@ec�@e	l@d�E@d�9@d|�@dM@dFt@d7�@d@c�}@c��@cy�@c�@b��@b�@bQ@b0U@b	@b{@a�#@a��@aG�@`��@`Ft@_��@_�0@_b�@_$t@^ȴ@^}V@^YK@]�T@]��@]\�@\��@\��@\c�@[�@[��@[�$@[��@[s@[33@Z�@Zz@Y�@Y��@YA @X�U@Xoi@X2�@X@W�]@W��@V�8@V�<@V��@V��@Vff@V($@U�o@U��@Uzx@U�@T��@T-�@S�w@S�{@SW?@R�@R��@Rd�@RW�@R�@Q��@Pc�@O�+@O�
@O�a@Oo�@N��@N��@NZ�@N&�@M�N@L:�@Kخ@K]�@J�"@J��@J�@J�r@JGE@I�@I��@IY�@I<6@H��@H<�@H2�@HM@G��@G9�@G�@F��@FE�@F4@E�Z@E��@EJ�@E?}@E5�@D��@D��@DU2@Cƨ@C��@C�@BL0@A�@A:�@@��@@�I@@�@@[�@@�@?�@>�h@>��@>\�@>	@=�Z@=��@=�@<�$@<��@<Z@<"h@;˒@;]�@;�@:�h@:=q@9��@9ϫ@9e,@9&�@8�E@8g8@7�V@7�{@7s@7Z�@7�@6�<@6~�@6�@5�)@5��@5@5��@5f�@5/@4�@4��@4�o@3�A@3|�@3@2�R@2^5@2C�@23�@2�@1�d@1f�@1Dg@1q@0��@0l"@04n@0�@/��@/��@/��@/�[@/�@@/��@.��@.��@.��@.d�@.?@-�@-��@-N<@-:�@-�@,�@,��@+�W@+�@+K�@*��@*�H@*͟@*�b@*^5@*$�@)�h@)(�@)�@(��@(��@(~(@(bN@(N�@(9X@(~@'��@'�q@'RT@&��@&�,@&��@&GE@%��@%��@%X@$�v@$�O@$�4@$�o@$*�@#�@#�Q@#�P@#]�@#�@"��@"�}@"�@"J�@" �@!�@!��@!|@!a�@!L�@!+�@!�@ ��@ ��@ Ɇ@ �@ c�@ <�@�@�@�&@��@��@� @��@o�@X�@�@�]@�R@L0@$�@4@u@�@�'@j@Q�@�@��@��@H@�]@�a@S�@�@ں@��@YK@+k@#:@��@}�@a�@S&@F@=�@-w@@@��@�@�@�;@��@�K@�K@��@�	@=@�@�H@�@i�@Q@8�@$�@4@��@�@A @Ɇ@�@�@l"@2�@�@�@��@l�@,�@��@�@YK@��@��@��@=�@V@��@�@w�@`�@9X@%�@1@˒@�@��@�4@a@'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���Aɗ�A�uZA�� Aȕ�A�.A�AUA��oA���AƤ�A�uZA�GzA�:�A�6FA�0UA�($A�{A� A��A��A��
A�ʌAſ�Aŗ$A�A�A�~A�ݘA�:�A�бA��A�v+A��NA���A��BA��UA��9A��6A���A�ݘA��TA��[A��dA��qA���A�NA���A��kA�A�e�A�e�A���A��A��A��A���A���A���A��A���A�˒A���A�A�A��-A�@A��_A���A�xA�P�A�B�A��-A�B�A�iDA�($A��dA���A��A��_A��A��%A�w�A�\�A��2A�o�A�ҽA���A�'�A�%A{�AxdZAt��Aq�^Ao��Ak��Ag�Ae5?AcA[Z�AVl�AU�ATIRAS�KAQ�bAN��AJ�AHU�AE��ADjADD�ACo�A@�oA>2�A<�A<qA;��A;ԕA<[�A<��A;J#A8�KA4�A1�A.l�A-2aA+�A)-�A'�hA&1A#�{A l"A�;A�A�_A33A(�AMAh
A��A��AخA�.A!�Az�AW�A6A��A1'A*�A4A�A�jA\)A��Aq�A�A�AQ�A�MA�A��Af�A��Ag�A�BA}VA%�AA�A�]A'�An�A��A�AffA��A&A�A\�A0�A�BA�A�eAC-A�AH�A
��A
A	�A	�A	qA�TAzA8A��At�A+�A�A:�A�0A4�A��A�9A��A��A��A{JAS�A6zA#:A�jA�A:�A��AP�A ��A ;@�u�@�Xy@��z@���@�s�@�@@��@�h
@���@�q@��@��@�V�@���@�;@��1@�	@�P�@��@��A@�p�@�G�@�@�g8@�?�@�O@��#@�T�@�V�@�:*@�1@��@@�ߤ@�<@��@���@�p�@�/@��P@�s�@�w2@�/�@�w2@�;@�'R@��@�Y@䟾@�7�@�S&@���@��)@�4n@���@��@�zx@�W?@�ی@�^5@���@ݹ�@ݎ�@��@�_�@���@�_p@��@�6@�a@ؠ�@�_@�	@ׅ�@�K�@��@�w�@�M@���@�9�@��H@ԟ�@�V�@��@�7L@�Xy@��+@�1�@��@Ъe@�L0@�($@��T@���@�oi@���@�F�@̰�@��@ˡ�@�Q�@�@�V@��f@�ѷ@ʃ�@�m�@�h
@�C-@ɵt@�j@�1�@�ѷ@��@Ǵ�@�?}@ƾ�@�ƨ@�.I@���@Ĳ�@Ċr@�7@�_p@��@�~(@�3�@�!�@���@��
@�l�@�T�@��@�N�@��	@�B�@�(�@�%@��[@��j@�_�@��m@�C�@��5@��_@�)�@���@���@���@���@���@�H@��@�خ@���@�~�@�Q�@��@���@�j@�0U@��@��}@�zx@��@��`@�n�@�($@�y�@��A@��_@���@�d�@��m@���@�Y�@�@���@��@��@�~�@�@��@��@���@�e�@��N@���@�S&@�2a@��@��@���@��o@�s�@���@���@�RT@��@���@�<�@�ݘ@�P�@�%F@���@�͟@�_@��)@���@�n/@�T�@�+@�L0@��N@�N<@���@�oi@��[@�q@��e@���@���@��7@�A�@��@���@�h�@� �@��&@��@���@��7@�L�@�1�@��@��/@���@��A@�@�@� �@���@�k�@� \@��|@�҉@��}@�Q@�c@��]@�e�@�>B@��@�B�@�+@��+@�M�@���@�Vm@�,�@��B@�5?@��o@��@�U�@�'�@��R@�^5@��
@���@�E9@�	l@���@��_@�l"@�L0@�(�@��@��&@�ƨ@���@���@���@�/�@��@��H@��s@���@���@�Ft@���@�m]@� \@��H@���@��4@�v�@�"h@���@��@�m]@�S�@�F@�@�҉@���@�}V@�n�@�Q�@�=q@�-@��@���@�m]@���@��@�N�@�#:@��r@���@�J�@�4@��I@�U2@�{@��@���@�{J@�J#@���@��I@�~�@�bN@�6@��]@��@��@�C@���@�q�@�PH@�E�@��@�X�@��@�Ĝ@���@�h�@�g�@�Y@���@���@�Ov@�	@��@�c�@��@��8@��K@��'@��I@�oi@�[�@�S�@�<�@�@��@o@~�@}��@}|@}7L@|�e@|z�@|Q�@|>B@{��@{W?@{S@z@�@yu�@x��@x��@x�4@x�I@xXy@w�[@wqv@we�@wZ�@w9�@w�@v�@v��@v�<@va|@v#:@v
�@u�@u�"@uA @uV@u;@t�o@t1'@t�@s��@r�@r��@rL0@qx�@q(�@pѷ@p��@pz�@p'R@o��@o��@o��@ob�@oY@nd�@n#:@m�@m�d@m��@mY�@m<6@l��@l~(@k�&@k>�@j�@i�#@iu�@h�_@g�K@f�M@f��@fu%@fh
@fB[@e�.@e�^@ec�@e	l@d�E@d�9@d|�@dM@dFt@d7�@d@c�}@c��@cy�@c�@b��@b�@bQ@b0U@b	@b{@a�#@a��@aG�@`��@`Ft@_��@_�0@_b�@_$t@^ȴ@^}V@^YK@]�T@]��@]\�@\��@\��@\c�@[�@[��@[�$@[��@[s@[33@Z�@Zz@Y�@Y��@YA @X�U@Xoi@X2�@X@W�]@W��@V�8@V�<@V��@V��@Vff@V($@U�o@U��@Uzx@U�@T��@T-�@S�w@S�{@SW?@R�@R��@Rd�@RW�@R�@Q��@Pc�@O�+@O�
@O�a@Oo�@N��@N��@NZ�@N&�@M�N@L:�@Kخ@K]�@J�"@J��@J�@J�r@JGE@I�@I��@IY�@I<6@H��@H<�@H2�@HM@G��@G9�@G�@F��@FE�@F4@E�Z@E��@EJ�@E?}@E5�@D��@D��@DU2@Cƨ@C��@C�@BL0@A�@A:�@@��@@�I@@�@@[�@@�@?�@>�h@>��@>\�@>	@=�Z@=��@=�@<�$@<��@<Z@<"h@;˒@;]�@;�@:�h@:=q@9��@9ϫ@9e,@9&�@8�E@8g8@7�V@7�{@7s@7Z�@7�@6�<@6~�@6�@5�)@5��@5@5��@5f�@5/@4�@4��@4�o@3�A@3|�@3@2�R@2^5@2C�@23�@2�@1�d@1f�@1Dg@1q@0��@0l"@04n@0�@/��@/��@/��@/�[@/�@@/��@.��@.��@.��@.d�@.?@-�@-��@-N<@-:�@-�@,�@,��@+�W@+�@+K�@*��@*�H@*͟@*�b@*^5@*$�@)�h@)(�@)�@(��@(��@(~(@(bN@(N�@(9X@(~@'��@'�q@'RT@&��@&�,@&��@&GE@%��@%��@%X@$�v@$�O@$�4@$�o@$*�@#�@#�Q@#�P@#]�@#�@"��@"�}@"�@"J�@" �@!�@!��@!|@!a�@!L�@!+�@!�@ ��@ ��@ Ɇ@ �@ c�@ <�@�@�@�&@��@��@� @��@o�@X�@�@�]@�R@L0@$�@4@u@�@�'@j@Q�@�@��@��@H@�]@�a@S�@�@ں@��@YK@+k@#:@��@}�@a�@S&@F@=�@-w@@@��@�@�@�;@��@�K@�K@��@�	@=@�@�H@�@i�@Q@8�@$�@4@��@�@A @Ɇ@�@�@l"@2�@�@�@��@l�@,�@��@�@YK@��@��@��@=�@V@��@�@w�@`�@9X@%�@1@˒@�@��@�4@a@'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BH1BJ�BJ�BTaBf2B�B��B��B�eB��B��B�]B��B�B�.B�.B�BB�B��B��B��B��B�B�B�kB� B��B��B	�B	NVB	�9B	�'B
�B
B
�B
9XB
E9B
sMB
��B
�)B
�@B
��B
��B
��B
�pB
յB
�B�B�B0;B:�BO\Ba�BgRBp�Bz�B��B��B��B��B�B�"B�CB�B��B�BB�B iB�[B��B�vB��B՛B�?B�qB��B�GB\�B5?B
��B
��B
O(B
�B	�pB
�B	�B	͹B	��B	��B	�B	wfB	d�B	Q�B	G_B	=�B	)*B	�B	FB	 vB	6�B	E�B	)yB	pB	B�RB�B��B	�B	�B	#�B	CB	pB	,WB	G_B	U�B	u�B	shB	dZB	J=B	,�B	"�B	eB		�B��B�AB��BڠB�B��B�/B�7B�B�9B�kB� B�B�:B�SB�,B��B�B��B��B�B	 �B	�B	gB	B	�B	xB	N�B	c�B	~B	��B	�B	��B	�eB	��B	��B	�B	��B	��B	��B	�DB	�%B	�?B	�|B	��B	� B	��B	�\B	�HB	��B	��B	��B	�-B	�vB	��B	��B	�nB	��B	�LB	��B	��B	��B	�fB	�2B	�B	��B	�DB	�B	��B	��B	�0B	��B	�"B	��B	��B	�B	�B	�IB	�cB	��B	��B	��B	��B	�OB	��B	��B	�oB	�B	�B	��B	��B	��B	�9B	�LB	��B	��B	��B	��B	��B	�+B	�B	��B	�lB	�rB	�*B	��B	��B	��B	��B	�jB	�VB	�VB	�B	ÖB	��B	ÖB	�B	��B	�;B	��B	��B	�HB	�.B	��B	��B	�B	āB	�3B	�3B	�MB	�gB	�B	�B	��B	�B	ǮB	�1B	�B	�#B	��B	̈́B	�B	�B	�6B	ΥB	ΥB	οB	�B	��B	�NB	��B	�B	��B	��B	��B	�B	�B	�B	уB	�oB	ӏB	�&B	ӏB	�B	�B	�aB	��B	��B	յB	�B	�B	�B	�9B	ּB	�?B	��B	��B	�B	��B	�KB	ٴB	�KB	�B	��B	��B	�kB	�=B	��B	��B	ܬB	��B	�dB	�IB	ݲB	�B	��B	�B	�B	��B	�TB	�TB	�B	�tB	�FB	�`B	��B	�zB	�B	�B	�@B	�B	�B	�$B	�$B	�yB	�0B	�B	��B	�=B	�=B	�B	��B	�B	�B	��B	�wB	�/B	�cB	�B	�/B	��B	�B	�OB	�}B	�B	�aB	��B	��B	�MB	�B	��B	��B	��B	��B	�B	��B	�LB	��B	�8B	��B	��B	�lB	�	B	�0B	�JB	��B	��B	��B	�B	�"B	��B
�B
�B
�B
B
�B
�B
�B
�B
�B
%B
tB
tB
tB
?B
B
B
�B
EB
B
�B
�B
EB
+B
+B
�B
	�B
	�B

rB

�B
^B
)B
DB
^B
�B
B
�B
�B
�B
�B
B
�B
~B
�B
B
^B
jB
~B
~B
~B
dB
�B
�B
�B
�B
6B
�B
�B
HB
 B
 B
hB
:B
oB
:B
�B
�B
�B
NB
hB
�B
�B
oB
B
@B
[B
�B
�B
B
[B
B
�B
FB
�B
B
�B
B
�B
B
B
B
�B
�B
�B
�B
�B
B
�B
4B
�B
�B
�B
�B
 B
:B
oB
�B
�B
�B
�B
�B
�B
uB
�B
uB
�B
�B
�B
B
�B
B
�B
�B
9B
mB
�B
�B
�B
�B
eB
�B
	B
�B
~B
~B
~B
~B
~B
~B
IB
B
dB
�B
B
dB
�B
�B
�B
�B
dB
dB
�B
B
�B
�B
 B
�B
 'B
 �B
!|B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#TB
$B
$&B
#�B
$�B
%�B
&B
&�B
&�B
&LB
&LB
&2B
&2B
&�B
'B
'8B
)DB
)�B
*B
*KB
*�B
+�B
,"B
,�B
-B
-)B
-]B
-wB
-�B
./B
.cB
.�B
/5B
/iB
/�B
/�B
0B
0B
0!B
0�B
0�B
1'B
1�B
2-B
2GB
2GB
2-B
2GB
2�B
33B
3B
33B
3MB
3hB
3�B
3�B
3�B
3�B
3�B
4B
4TB
4�B
4�B
4�B
5ZB
5�B
6+B
5�B
6�B
6�B
6�B
72B
7�B
8B
8RB
88B
8lB
8lB
8�B
8�B
8�B
8lB
8�B
8�B
8�B
8�B
9	B
9>B
9XB
9>B
9XB
9�B
9�B
:^B
;0B
:�B
;JB
<B
<�B
=<B
=qB
=�B
=�B
=�B
=�B
>B
>wB
>�B
>�B
>�B
?.B
?HB
?HB
?HB
?cB
?�B
?�B
?�B
@OB
@�B
@�B
AB
@�B
@�B
@�B
AB
A B
A�B
BuB
B�B
CGB
CaB
C�B
C�B
D3B
DgB
DgB
EB
EB
EB
E�B
E�B
E�B
F%B
F?B
F?B
F?B
F%B
FtB
FtB
F�B
F�B
F�B
F�B
F�B
FtB
F�B
F�B
FtB
F�B
GEB
GzB
GzB
G�B
G�B
GzB
G�B
G�B
G�B
HfB
H�B
IB
I7B
IRB
I7B
I�B
I�B
I�B
I�B
I�B
J#B
KB
KDB
K)B
KB
K^B
K�B
K�B
K�B
K�B
LB
M�B
M�B
NpB
N�B
N�B
OB
OB
O(B
O�B
O�B
O�B
O�B
P}B
P�B
P�B
P�B
Q4B
QhB
QhB
Q�B
R:B
RTB
RTB
R�B
R�B
R�B
R�B
SB
S&B
S@B
S�B
S�B
S�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
W$B
W$B
WsB
W�B
XB
X+B
XEB
X_B
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
ZQB
Z7B
ZQB
Z�B
[qB
[qB
[qB
[qB
[�B
[�B
\)B
\xB
\�B
\xB
\�B
\�B
\�B
\�B
]/B
]/B
]IB
]�B
^B
^jB
^�B
_!B
_B
_B
_!B
_pB
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
b�B
c�B
c�B
d&B
d@B
dZB
d@B
dtB
d�B
d�B
ezB
e�B
e�B
e�B
fB
fB
f2B
fLB
fLB
fLB
f�B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
iDB
i_B
i_B
i�B
i�B
i�B
jKB
jKB
jeB
j�B
j�B
kB
k6B
k6B
kQB
kkB
k�B
k�B
k�B
k�B
k�B
lB
l=B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m]B
m�B
nB
nB
nB
nB
n}B
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
pUB
poB
p�B
p�B
q'B
qAB
q'B
q�B
q�B
rB
rB
rB
rB
rB
r-B
r-B
rGB
shB
s3B
s3B
sMB
s3B
sMB
shB
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
vB
vFB
v+B
v`B
v�B
v�B
v�B
v�B
wB
w�B
w�B
xB
x8B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
z*B
zB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BH1BJ�BJ�BTaBf2B�B��B��B�eB��B��B�]B��B�B�.B�.B�BB�B��B��B��B��B�B�B�kB� B��B��B	�B	NVB	�9B	�'B
�B
B
�B
9XB
E9B
sMB
��B
�)B
�@B
��B
��B
��B
�pB
յB
�B�B�B0;B:�BO\Ba�BgRBp�Bz�B��B��B��B��B�B�"B�CB�B��B�BB�B iB�[B��B�vB��B՛B�?B�qB��B�GB\�B5?B
��B
��B
O(B
�B	�pB
�B	�B	͹B	��B	��B	�B	wfB	d�B	Q�B	G_B	=�B	)*B	�B	FB	 vB	6�B	E�B	)yB	pB	B�RB�B��B	�B	�B	#�B	CB	pB	,WB	G_B	U�B	u�B	shB	dZB	J=B	,�B	"�B	eB		�B��B�AB��BڠB�B��B�/B�7B�B�9B�kB� B�B�:B�SB�,B��B�B��B��B�B	 �B	�B	gB	B	�B	xB	N�B	c�B	~B	��B	�B	��B	�eB	��B	��B	�B	��B	��B	��B	�DB	�%B	�?B	�|B	��B	� B	��B	�\B	�HB	��B	��B	��B	�-B	�vB	��B	��B	�nB	��B	�LB	��B	��B	��B	�fB	�2B	�B	��B	�DB	�B	��B	��B	�0B	��B	�"B	��B	��B	�B	�B	�IB	�cB	��B	��B	��B	��B	�OB	��B	��B	�oB	�B	�B	��B	��B	��B	�9B	�LB	��B	��B	��B	��B	��B	�+B	�B	��B	�lB	�rB	�*B	��B	��B	��B	��B	�jB	�VB	�VB	�B	ÖB	��B	ÖB	�B	��B	�;B	��B	��B	�HB	�.B	��B	��B	�B	āB	�3B	�3B	�MB	�gB	�B	�B	��B	�B	ǮB	�1B	�B	�#B	��B	̈́B	�B	�B	�6B	ΥB	ΥB	οB	�B	��B	�NB	��B	�B	��B	��B	��B	�B	�B	�B	уB	�oB	ӏB	�&B	ӏB	�B	�B	�aB	��B	��B	յB	�B	�B	�B	�9B	ּB	�?B	��B	��B	�B	��B	�KB	ٴB	�KB	�B	��B	��B	�kB	�=B	��B	��B	ܬB	��B	�dB	�IB	ݲB	�B	��B	�B	�B	��B	�TB	�TB	�B	�tB	�FB	�`B	��B	�zB	�B	�B	�@B	�B	�B	�$B	�$B	�yB	�0B	�B	��B	�=B	�=B	�B	��B	�B	�B	��B	�wB	�/B	�cB	�B	�/B	��B	�B	�OB	�}B	�B	�aB	��B	��B	�MB	�B	��B	��B	��B	��B	�B	��B	�LB	��B	�8B	��B	��B	�lB	�	B	�0B	�JB	��B	��B	��B	�B	�"B	��B
�B
�B
�B
B
�B
�B
�B
�B
�B
%B
tB
tB
tB
?B
B
B
�B
EB
B
�B
�B
EB
+B
+B
�B
	�B
	�B

rB

�B
^B
)B
DB
^B
�B
B
�B
�B
�B
�B
B
�B
~B
�B
B
^B
jB
~B
~B
~B
dB
�B
�B
�B
�B
6B
�B
�B
HB
 B
 B
hB
:B
oB
:B
�B
�B
�B
NB
hB
�B
�B
oB
B
@B
[B
�B
�B
B
[B
B
�B
FB
�B
B
�B
B
�B
B
B
B
�B
�B
�B
�B
�B
B
�B
4B
�B
�B
�B
�B
 B
:B
oB
�B
�B
�B
�B
�B
�B
uB
�B
uB
�B
�B
�B
B
�B
B
�B
�B
9B
mB
�B
�B
�B
�B
eB
�B
	B
�B
~B
~B
~B
~B
~B
~B
IB
B
dB
�B
B
dB
�B
�B
�B
�B
dB
dB
�B
B
�B
�B
 B
�B
 'B
 �B
!|B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#TB
$B
$&B
#�B
$�B
%�B
&B
&�B
&�B
&LB
&LB
&2B
&2B
&�B
'B
'8B
)DB
)�B
*B
*KB
*�B
+�B
,"B
,�B
-B
-)B
-]B
-wB
-�B
./B
.cB
.�B
/5B
/iB
/�B
/�B
0B
0B
0!B
0�B
0�B
1'B
1�B
2-B
2GB
2GB
2-B
2GB
2�B
33B
3B
33B
3MB
3hB
3�B
3�B
3�B
3�B
3�B
4B
4TB
4�B
4�B
4�B
5ZB
5�B
6+B
5�B
6�B
6�B
6�B
72B
7�B
8B
8RB
88B
8lB
8lB
8�B
8�B
8�B
8lB
8�B
8�B
8�B
8�B
9	B
9>B
9XB
9>B
9XB
9�B
9�B
:^B
;0B
:�B
;JB
<B
<�B
=<B
=qB
=�B
=�B
=�B
=�B
>B
>wB
>�B
>�B
>�B
?.B
?HB
?HB
?HB
?cB
?�B
?�B
?�B
@OB
@�B
@�B
AB
@�B
@�B
@�B
AB
A B
A�B
BuB
B�B
CGB
CaB
C�B
C�B
D3B
DgB
DgB
EB
EB
EB
E�B
E�B
E�B
F%B
F?B
F?B
F?B
F%B
FtB
FtB
F�B
F�B
F�B
F�B
F�B
FtB
F�B
F�B
FtB
F�B
GEB
GzB
GzB
G�B
G�B
GzB
G�B
G�B
G�B
HfB
H�B
IB
I7B
IRB
I7B
I�B
I�B
I�B
I�B
I�B
J#B
KB
KDB
K)B
KB
K^B
K�B
K�B
K�B
K�B
LB
M�B
M�B
NpB
N�B
N�B
OB
OB
O(B
O�B
O�B
O�B
O�B
P}B
P�B
P�B
P�B
Q4B
QhB
QhB
Q�B
R:B
RTB
RTB
R�B
R�B
R�B
R�B
SB
S&B
S@B
S�B
S�B
S�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
W$B
W$B
WsB
W�B
XB
X+B
XEB
X_B
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
ZQB
Z7B
ZQB
Z�B
[qB
[qB
[qB
[qB
[�B
[�B
\)B
\xB
\�B
\xB
\�B
\�B
\�B
\�B
]/B
]/B
]IB
]�B
^B
^jB
^�B
_!B
_B
_B
_!B
_pB
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
b�B
c�B
c�B
d&B
d@B
dZB
d@B
dtB
d�B
d�B
ezB
e�B
e�B
e�B
fB
fB
f2B
fLB
fLB
fLB
f�B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
iDB
i_B
i_B
i�B
i�B
i�B
jKB
jKB
jeB
j�B
j�B
kB
k6B
k6B
kQB
kkB
k�B
k�B
k�B
k�B
k�B
lB
l=B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m]B
m�B
nB
nB
nB
nB
n}B
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
pUB
poB
p�B
p�B
q'B
qAB
q'B
q�B
q�B
rB
rB
rB
rB
rB
r-B
r-B
rGB
shB
s3B
s3B
sMB
s3B
sMB
shB
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
vB
vFB
v+B
v`B
v�B
v�B
v�B
v�B
wB
w�B
w�B
xB
x8B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
z*B
zB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105235  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191759  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191759  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191759                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041807  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041807  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                