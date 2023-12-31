CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-08T03:53:11Z creation;2022-07-08T03:53:12Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220708035311  20220708040318  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��:�vT1   @������@0�7KƧ��c�
=p��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@���A   A   AA��A`  A�  A�  A�  A�33A�33A�  A���A���B   B  B  B  B   B'��B/��B8  B@  BH  BP  BX  B`ffBg��Bp  BxffB~��B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C633C8  C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Ck�fCn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @}p�@�Q�@��AA?\)A]A}A��HA��HA�{A�{A��HA߮A�A��HBp�Bp�Bp�Bp�B'
>B/
>B7p�B?p�BGp�BOp�BWp�B_�
Bg
>Bop�Bw�
B~=qB��RB��B��RB��RB��RB��RB��RB��RB��B��RB��RB��RB��RB��B��B��RBøRBǸRB˸RBϸRB��BׅBۅB߸RB�RB�RB�RB�RB�RB��RB��RB��C�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C6\C7�)C9�)C;C=C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg��Ci��CkCm�)CoCq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��GC��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
D �
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D	w
D	�
D
w
D
�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D}pD�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D w
D �
D!w
D!�
D"w
D"�
D#w
D#�
D$w
D$�
D%w
D%�
D&w
D&�
D'w
D'�
D(w
D(�
D)w
D)�
D*w
D*�
D+w
D+�
D,w
D,�
D-w
D-�
D.w
D.�
D/w
D/�
D0w
D0�
D1w
D1�
D2w
D2�
D3w
D3�
D4w
D4�
D5w
D5�
D6w
D6�
D7w
D7�
D8w
D8�
D9w
D9�
D:w
D:�
D;w
D;�
D<w
D<�
D=w
D=�
D>w
D>�
D?w
D?�
D@w
D@�
DAw
DA�
DBp�DB�
DCw
DC�
DDw
DD�
DEw
DE�
DFw
DF�
DGw
DG�
DHw
DH�
DIw
DI�
DJw
DJ�
DKw
DK�
DLw
DL�
DMw
DM�
DNw
DN�
DOw
DO�
DPw
DP�
DQ}pDQ�pDRw
DR�
DSw
DS�
DTw
DT�
DUw
DU�
DVw
DV�
DWw
DW�
DXw
DX�
DYw
DY�
DZw
DZ�
D[w
D[�
D\w
D\�
D]w
D]�
D^w
D^�
D_w
D_�
D`w
D`�
Daw
Da�
Dbw
Db�
Dcw
Dc�
Ddw
Dd�
Dew
De�
Dfw
Df�
Dgw
Dg�
Dhw
Dh�
Diw
Di�
Djw
Dj�
Dkw
Dk�
Dlw
Dl�
Dmw
Dm�
Dnw
Dn�
Dow
Do�
Dpw
Dp�
Dqw
Dq�
Drw
Dr�
Dsw
Ds�
Dtw
Dt�
Duw
Du�
Dvw
Dv�
Dww
Dw�
Dxw
Dx�
Dyw
Dy�
Dzw
Dz�
D{w
D{�
D|w
D|�
D}w
D}�
D~w
D~�
Dw
D�
D�;�D�{�D���D���D�;�D�{�D���D��RD�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�NpA�P�A�[#A�YA�Y�A�YA�Z�A�^�A�_�A�c�A�d&A�cTA�e,A�jA�j�A�iDA�jA�j�A�k�A�n/A�m]A�sMA�u�A�r�A�p�A�^5A�N<A�F?Aˎ"A�w�A��A��VA���A�d�AƗ�A�xA��A�
	A�w�A���A�z�A�ҽA�
rA��VA��2A�+�A��sA�{A��IA�XyA���A�3�A�rA���A���A���A�`vA��bA�"�A��	A�hsA�5�A�.IA�k�A��DA��A��OA�f�A�K�A��$A���A���A�]/A�e�A���A�^jA��VA���A�hsA�EmA�A��A��A��XA�HKA���A���A���A�m)A��A��oA��oAbNA{[�Au�XAs�Ap	AkU�Ai��AiVAi�Ahh
Ae��Aa�.A^�A];�AY(�AT�AQ҉AO��AN�!AK�~AKAH֡AF�fAC�A@O�A>GA;�KA8m�A6�XA4�'A2�jA2	A1�\A0�KA/�
A/C�A/�sA/�A.GEA-�uA,�9A,IRA+��A+R�A*�[A*��A)�A'��A'}�A&�A&zA%�_A%�A$�2A$��A#�A"�A"_pA"@�A!�aA!?�A!�A �vA FA�A��AL0A��AoiA�;A�rA=qA�A�BAMA�+Av�Al"AjAMA�Au�A�AVA�8A�*A�A��AC�A�aA@OA� A��AѷA�AN<A��A��AJ�A�KAK�A1A�Aa|A iAcA��Ah
A�A.IA~(A��AȴA�PAA��A
��A
��A
�'A
�A
��A	҉A	JA��Ax�A��A �A��A��A��AqvA�;AXyA��Au�AK^A*0A!-A�pAFAGA��Ax�AS&A��A��A\�A@�AA �zA qvA S&A =�A @@���@��@�c@�|�@�s@�C@���@��@�q�@��6@���@���@��o@�]�@��v@�6�@��8@�$@�A�@�˒@���@�/@�V�@�0@��@�(@� �@�ƨ@�P@�;d@�'�@�;@��@�-�@뙚@�D�@��@��@�_p@橓@�1@�Y�@��@�҉@��5@�y>@�@�@���@�j@�&@��@��@���@�GE@�?�@ߣn@�}V@�[�@�7�@�>�@ܵ�@�c�@ڊr@�R�@ٮ@��c@�M�@��z@�c @�)�@��@�|@Ԛ�@�@�x�@�a@�+@Ҳ�@�a|@���@��@Сb@�_�@���@�c�@ε@�V@�.�@�@��)@��}@͟V@�r�@�	�@ˑh@�N<@�E9@�5�@�҉@�bN@�!�@��@�O@�
=@��X@�q�@��@�e,@��@�֡@�w�@�U2@Ž�@Ľ<@�N�@� �@���@�b�@�@«6@��@���@�w2@�y�@�`B@�S&@�G�@��"@��}@�.�@��@�~�@�A�@��y@���@�)�@��@��*@��X@���@���@�K�@�&@�S@��!@� �@�s@��@���@�H@���@�L�@�*0@��p@�  @�	@�s�@��@��@���@�p�@�A @�1�@��4@�.�@�~@���@�C�@�V@���@��\@�oi@�YK@�J�@�.�@��@�k�@�4@�ȴ@���@�$�@��@@��f@���@��M@�2a@�֡@���@���@�tT@�O@��K@���@�;@��X@��+@�6�@�خ@��C@���@�!-@�� @���@���@�@O@��2@�_@��@��w@�m]@�@�ی@��1@�Xy@�{@�iD@�J#@�IR@�=@�+@��@�N�@���@�`B@��@���@��!@�`�@�7@���@��d@�x@��@���@�Q�@��@�k�@�+@���@��@�~�@��@��&@��Q@���@�)_@���@���@�i�@��@��@���@���@�8�@�@�֡@��U@���@�h
@�2�@�O@��@���@�X�@���@��@���@���@��@��P@��H@�ȴ@��_@�_@�;�@��@��@��"@�[W@�K�@�o@�͟@��9@���@�s�@��o@�>�@��K@���@���@���@��A@�c @�-�@�{@���@���@�o�@�\)@�4@��@��1@�8�@��}@�g�@��@���@�V@��@���@�X@�/�@��@�z�@�oi@�M�@���@���@���@�j@���@�l�@��@���@��@�7L@�֡@���@��@��6@�j@�B[@�!@��.@�ݘ@��h@�+@��s@��$@��b@���@�6�@�@���@���@��@�W?@�8@��@��?@���@�}V@�PH@��@��@�|�@�rG@�`B@�P�@�1�@���@���@��@�bN@�(�@�ԕ@��q@���@���@�hs@�<6@��@�Ĝ@��@�3�@RT@~R�@}��@}��@|��@|�.@|w�@|h�@|4n@{خ@{��@{6z@{�@zȴ@zJ�@z;�@y��@y�N@y�X@yB�@y \@xK^@w"�@v-@ux�@t��@t��@toi@tS�@t6@t�@sH�@s�@r�@r�\@rOv@q��@q��@q�@pu�@p4n@p�@o��@o��@os@o{J@oo�@o&@o�@o i@n��@n��@nE�@m�t@m��@m|@m`B@m�@ly>@ka@jC�@i�@i�@iq@h�@g��@gb�@f�@fR�@e��@e=�@d��@dh�@dD�@d�@c��@bȴ@b@�@b=q@b:*@b)�@a�Z@a�^@a8�@`Ɇ@_�:@_�@^ȴ@^i�@]�@]!�@\Ĝ@\��@\�@[�@[˒@[��@[v`@[9�@[;d@[+@Z�8@Z�'@Zq�@Y��@YQ�@X�9@W��@WW?@WS@V�<@V��@Vd�@V@Uc@U(�@U;@T�Y@Tr�@T%�@S�w@S�	@SMj@Rȴ@R�@RV@R$�@Q�Z@Q��@QG�@P��@P�_@Pc�@PN�@PA�@P"h@O��@Og�@N��@NOv@N0U@N�@MJ�@L�[@LĜ@L��@Loi@L(�@Lx@K6z@J��@J��@Jl�@I�D@I��@I�@I�@Hی@H��@H��@H/�@G�@G��@Gqv@G4�@F��@F�A@F+k@Eϫ@E�@EL�@E7L@E8�@E�@D��@Dj@D  @C�F@CS�@B�h@B$�@Aϫ@A��@A�@A��@A�=@ArG@Ae,@A?}@@��@@�.@@H@?�}@?RT@?;d@?6z@?�@>��@>��@>�x@>�@=��@=%F@<��@<q@<tT@<q@<N�@<A�@<2�@<�@;��@;��@;��@;A�@:�"@:�X@:�@9�o@9��@9��@9G�@8�E@8$@7a@6��@6-@5�t@5IR@5%@4�)@4��@4�u@4*�@3��@3��@3a@3)_@3!-@3 i@2��@1��@1ϫ@1j@0�.@/�A@/�k@/j�@/1�@.͟@.a|@.+k@.�@.4@-��@-c@-&�@,��@,�I@,Xy@+�]@+��@+H�@+!-@+�@*�h@*d�@*;�@)�>@)��@)N<@(�p@(��@(bN@(]d@(/�@'�r@'خ@'e�@'�@&�@&�<@&p;@&O@%�.@%�@%��@%G�@%A @$�`@$�@$�@#��@#��@#��@#�0@#s@#Y@#@"�L@"��@"4@!o @!G�@!8�@!(�@!�@ �f@ ѷ@ �j@ C-@�g@��@�@@|�@X�@33@�@
=@��@�@ȴ@{�@�@�@�@�h@T�@:�@ \@�|@�E@�@ѷ@��@�I@��@~(@PH@1@��@��@��@��@l�@33@�@C@�@�@
=@҉@�+@d�@�T@��@w2@�@��@�5@��@��@�u@2�@l�@O@E9@$t@��@�r@d�@�@��@��@�@Dg@#�@@�@��@U2@9X@��@�@�@�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�NpA�P�A�[#A�YA�Y�A�YA�Z�A�^�A�_�A�c�A�d&A�cTA�e,A�jA�j�A�iDA�jA�j�A�k�A�n/A�m]A�sMA�u�A�r�A�p�A�^5A�N<A�F?Aˎ"A�w�A��A��VA���A�d�AƗ�A�xA��A�
	A�w�A���A�z�A�ҽA�
rA��VA��2A�+�A��sA�{A��IA�XyA���A�3�A�rA���A���A���A�`vA��bA�"�A��	A�hsA�5�A�.IA�k�A��DA��A��OA�f�A�K�A��$A���A���A�]/A�e�A���A�^jA��VA���A�hsA�EmA�A��A��A��XA�HKA���A���A���A�m)A��A��oA��oAbNA{[�Au�XAs�Ap	AkU�Ai��AiVAi�Ahh
Ae��Aa�.A^�A];�AY(�AT�AQ҉AO��AN�!AK�~AKAH֡AF�fAC�A@O�A>GA;�KA8m�A6�XA4�'A2�jA2	A1�\A0�KA/�
A/C�A/�sA/�A.GEA-�uA,�9A,IRA+��A+R�A*�[A*��A)�A'��A'}�A&�A&zA%�_A%�A$�2A$��A#�A"�A"_pA"@�A!�aA!?�A!�A �vA FA�A��AL0A��AoiA�;A�rA=qA�A�BAMA�+Av�Al"AjAMA�Au�A�AVA�8A�*A�A��AC�A�aA@OA� A��AѷA�AN<A��A��AJ�A�KAK�A1A�Aa|A iAcA��Ah
A�A.IA~(A��AȴA�PAA��A
��A
��A
�'A
�A
��A	҉A	JA��Ax�A��A �A��A��A��AqvA�;AXyA��Au�AK^A*0A!-A�pAFAGA��Ax�AS&A��A��A\�A@�AA �zA qvA S&A =�A @@���@��@�c@�|�@�s@�C@���@��@�q�@��6@���@���@��o@�]�@��v@�6�@��8@�$@�A�@�˒@���@�/@�V�@�0@��@�(@� �@�ƨ@�P@�;d@�'�@�;@��@�-�@뙚@�D�@��@��@�_p@橓@�1@�Y�@��@�҉@��5@�y>@�@�@���@�j@�&@��@��@���@�GE@�?�@ߣn@�}V@�[�@�7�@�>�@ܵ�@�c�@ڊr@�R�@ٮ@��c@�M�@��z@�c @�)�@��@�|@Ԛ�@�@�x�@�a@�+@Ҳ�@�a|@���@��@Сb@�_�@���@�c�@ε@�V@�.�@�@��)@��}@͟V@�r�@�	�@ˑh@�N<@�E9@�5�@�҉@�bN@�!�@��@�O@�
=@��X@�q�@��@�e,@��@�֡@�w�@�U2@Ž�@Ľ<@�N�@� �@���@�b�@�@«6@��@���@�w2@�y�@�`B@�S&@�G�@��"@��}@�.�@��@�~�@�A�@��y@���@�)�@��@��*@��X@���@���@�K�@�&@�S@��!@� �@�s@��@���@�H@���@�L�@�*0@��p@�  @�	@�s�@��@��@���@�p�@�A @�1�@��4@�.�@�~@���@�C�@�V@���@��\@�oi@�YK@�J�@�.�@��@�k�@�4@�ȴ@���@�$�@��@@��f@���@��M@�2a@�֡@���@���@�tT@�O@��K@���@�;@��X@��+@�6�@�خ@��C@���@�!-@�� @���@���@�@O@��2@�_@��@��w@�m]@�@�ی@��1@�Xy@�{@�iD@�J#@�IR@�=@�+@��@�N�@���@�`B@��@���@��!@�`�@�7@���@��d@�x@��@���@�Q�@��@�k�@�+@���@��@�~�@��@��&@��Q@���@�)_@���@���@�i�@��@��@���@���@�8�@�@�֡@��U@���@�h
@�2�@�O@��@���@�X�@���@��@���@���@��@��P@��H@�ȴ@��_@�_@�;�@��@��@��"@�[W@�K�@�o@�͟@��9@���@�s�@��o@�>�@��K@���@���@���@��A@�c @�-�@�{@���@���@�o�@�\)@�4@��@��1@�8�@��}@�g�@��@���@�V@��@���@�X@�/�@��@�z�@�oi@�M�@���@���@���@�j@���@�l�@��@���@��@�7L@�֡@���@��@��6@�j@�B[@�!@��.@�ݘ@��h@�+@��s@��$@��b@���@�6�@�@���@���@��@�W?@�8@��@��?@���@�}V@�PH@��@��@�|�@�rG@�`B@�P�@�1�@���@���@��@�bN@�(�@�ԕ@��q@���@���@�hs@�<6@��@�Ĝ@��@�3�@RT@~R�@}��@}��@|��@|�.@|w�@|h�@|4n@{خ@{��@{6z@{�@zȴ@zJ�@z;�@y��@y�N@y�X@yB�@y \@xK^@w"�@v-@ux�@t��@t��@toi@tS�@t6@t�@sH�@s�@r�@r�\@rOv@q��@q��@q�@pu�@p4n@p�@o��@o��@os@o{J@oo�@o&@o�@o i@n��@n��@nE�@m�t@m��@m|@m`B@m�@ly>@ka@jC�@i�@i�@iq@h�@g��@gb�@f�@fR�@e��@e=�@d��@dh�@dD�@d�@c��@bȴ@b@�@b=q@b:*@b)�@a�Z@a�^@a8�@`Ɇ@_�:@_�@^ȴ@^i�@]�@]!�@\Ĝ@\��@\�@[�@[˒@[��@[v`@[9�@[;d@[+@Z�8@Z�'@Zq�@Y��@YQ�@X�9@W��@WW?@WS@V�<@V��@Vd�@V@Uc@U(�@U;@T�Y@Tr�@T%�@S�w@S�	@SMj@Rȴ@R�@RV@R$�@Q�Z@Q��@QG�@P��@P�_@Pc�@PN�@PA�@P"h@O��@Og�@N��@NOv@N0U@N�@MJ�@L�[@LĜ@L��@Loi@L(�@Lx@K6z@J��@J��@Jl�@I�D@I��@I�@I�@Hی@H��@H��@H/�@G�@G��@Gqv@G4�@F��@F�A@F+k@Eϫ@E�@EL�@E7L@E8�@E�@D��@Dj@D  @C�F@CS�@B�h@B$�@Aϫ@A��@A�@A��@A�=@ArG@Ae,@A?}@@��@@�.@@H@?�}@?RT@?;d@?6z@?�@>��@>��@>�x@>�@=��@=%F@<��@<q@<tT@<q@<N�@<A�@<2�@<�@;��@;��@;��@;A�@:�"@:�X@:�@9�o@9��@9��@9G�@8�E@8$@7a@6��@6-@5�t@5IR@5%@4�)@4��@4�u@4*�@3��@3��@3a@3)_@3!-@3 i@2��@1��@1ϫ@1j@0�.@/�A@/�k@/j�@/1�@.͟@.a|@.+k@.�@.4@-��@-c@-&�@,��@,�I@,Xy@+�]@+��@+H�@+!-@+�@*�h@*d�@*;�@)�>@)��@)N<@(�p@(��@(bN@(]d@(/�@'�r@'خ@'e�@'�@&�@&�<@&p;@&O@%�.@%�@%��@%G�@%A @$�`@$�@$�@#��@#��@#��@#�0@#s@#Y@#@"�L@"��@"4@!o @!G�@!8�@!(�@!�@ �f@ ѷ@ �j@ C-@�g@��@�@@|�@X�@33@�@
=@��@�@ȴ@{�@�@�@�@�h@T�@:�@ \@�|@�E@�@ѷ@��@�I@��@~(@PH@1@��@��@��@��@l�@33@�@C@�@�@
=@҉@�+@d�@�T@��@w2@�@��@�5@��@��@�u@2�@l�@O@E9@$t@��@�r@d�@�@��@��@�@Dg@#�@@�@��@U2@9X@��@�@�@�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�SB
�SB
�B
�B
�B
�9B
�9B
�B
�B
�B
�B
�9B
�9B
�B
�B
�B
�9B
�9B
�SB
�9B
�SB
�9B
�SB
��B
��B
�B
��B
��B
�?B+B8�B>BD3BJ�B[#Bv�B}"B��B�hB��B�B+�BC-BX�BjBs�By$B.B�B�xB�"B�EBY�B��B��B��B�	B��BյB׍B�0B�Bc�Bj0BaHBK�B%`B.IB B�B�!B�pB�@B��B��B�zB3�B
��B
�DB
�B
�rB
��B
��B
��B
tB
Z�B
G�B
>�B
4�B
+QB
�B
dB
  B	�B	�DB	�B	��B	B	w�B	r|B	pB	i�B	Z�B	F�B	/�B	*B	�B��B�/B��B��B	aB	 OB��B��B�zB�MB	"�B	(XB	�B	B	�B		B		B	�B	�B	~B	B	3�B	E�B	DB	O(B	T�B	Q�B	TaB	]�B	f2B	u�B	z�B	i�B	p;B	t�B	�iB	�}B	�_B	�cB	�$B	��B	�gB	�#B	��B	�?B	��B	�BB	�hB	��B	�>B	�B	��B	�B	�B	��B	��B	��B	�+B	��B	��B	�.B
UB
oB
�B
B
B
�B
 B
 �B
;B
�B

XB

�B
�B
�B
�B
aB
�B
oB
B
�B
�B
B
;B
 iB
 4B
 4B	��B
 �B
 iB
uB
GB
�B
�B
�B
�B
aB
B
B
�B
AB
'B
�B
�B
oB
�B
�B
�B
B
 �B
�B
'B
�B
�B
�B
B
�B
�B
�B
�B
UB
 B
 �B
 �B
B
 �B
 �B
 4B	��B
 iB
 4B
 B
 OB
 �B
 �B
 iB
  B	��B	�]B	�6B	��B	��B	��B	��B	�B	�`B	�FB	�`B	��B	�`B	��B	�2B	��B	��B	�zB	��B	�zB	��B	�B	��B	��B	�ZB	�tB	�%B	��B	�B	�+B	�FB	��B	�FB	�FB	�B	��B	��B	�hB	�B	�B	�B	�B	�B	�B	�B	�AB	�GB	�B	�9B	�%B	�hB	�B	�hB	��B	�B	�B	�MB	��B	�-B	�3B	�B	��B	�-B	��B	�B	�%B	��B	�FB	�B	��B	�B	�MB	�B	�%B	��B	�FB	��B	��B	�zB	��B	�+B	��B	��B	�B	�?B	�B	��B	�B	��B	�B	��B	��B	��B	�3B	��B	��B	��B	�RB	�8B	�RB	�XB	�rB	�XB	��B	��B	�B	��B	�	B	��B	�RB	�B	��B	��B	��B	��B	�rB	�XB	�>B	��B	��B	�^B	��B	��B	�dB	�B	��B	��B	��B	��B	�B	�6B	�B	�B	�6B	��B	�B	�6B	��B	�B
�B
B
mB
SB
mB
B
B
mB
YB
�B
�B
tB
�B
9B
?B
%B
�B
KB
�B
	�B
	�B
	B
	7B
	7B
	RB
	RB

�B
B

�B
)B
�B
B
dB
�B
~B
~B
~B
~B
~B
�B
�B
B
B
�B
<B
VB
pB
VB
(B
\B
BB
(B
BB
BB
BB
�B
\B
(B
\B
�B
.B
bB
bB
�B
4B
�B
�B
:B
�B
oB
�B
&B
�B
FB
{B
�B
{B
�B
�B
gB
MB
MB
2B
�B
B
9B
SB
�B
�B
$B
sB
�B
�B
+B
yB
1B
1B
eB
B
�B
B
7B
7B
QB
�B
�B
�B
	B
#B
qB
�B
�B
B
]B
]B
�B
�B
�B
dB
IB
~B
dB
dB
�B
�B
jB
�B
pB
�B
!-B
!bB
!�B
!�B
!�B
!�B
!�B
"B
!�B
"4B
"hB
"�B
"�B
"�B
#B
#:B
#TB
#:B
#:B
#�B
$tB
%B
%`B
%`B
%`B
%FB
%`B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(
B
(�B
)*B
)�B
)�B
*0B
*�B
+B
+B
+kB
+�B
+�B
+�B
,�B
,qB
,�B
,=B
,�B
-wB
-�B
.B
.cB
.cB
.�B
.}B
.}B
.cB
.}B
.IB
.�B
.�B
.�B
.�B
/5B
/iB
/�B
/�B
/�B
0;B
0!B
/�B
0!B
0UB
0;B
0oB
0�B
0UB
0!B
/�B
1[B
1'B
0UB
0�B
0�B
0�B
0UB
0�B
2aB
2�B
3�B
3�B
49B
4nB
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5B
4�B
5?B
5�B
6zB
6�B
6�B
6�B
6�B
6zB
6�B
7LB
7B
7�B
7�B
7�B
8lB
9XB
9�B
9�B
9�B
:xB
:B
:�B
;�B
<B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=qB
=<B
=<B
=�B
=�B
=�B
=�B
>B
>�B
>]B
>BB
>BB
>�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
@ B
?�B
?}B
?HB
>�B
>wB
>B
=�B
=�B
=�B
=�B
>BB
?HB
?}B
?�B
@iB
@�B
@�B
AB
A�B
AoB
AoB
AoB
A�B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
CB
B�B
C{B
CB
CaB
D�B
D�B
E�B
E�B
E�B
F%B
F%B
F%B
F?B
FtB
F�B
FtB
FtB
F�B
F�B
F�B
G+B
GB
G_B
G�B
G�B
HKB
H�B
H�B
H�B
H�B
IRB
I�B
I�B
J	B
I�B
J=B
J�B
J�B
K�B
L~B
L�B
L�B
L�B
MB
MB
MPB
M�B
M�B
N"B
N"B
NB
NB
N"B
N"B
N�B
N�B
NpB
NVB
N�B
N�B
N�B
OBB
OBB
O\B
OBB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QhB
Q�B
R B
R�B
S&B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
T�B
UB
UMB
UB
UB
UMB
U2B
U2B
T�B
T�B
T�B
T�B
T�B
UgB
V�B
WsB
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
X�B
X�B
YB
Y1B
YKB
YKB
YeB
YeB
Y�B
YB
YeB
Y�B
YeB
YB
ZkB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[�B
[�B
\]B
\�B
]/B
]IB
]~B
]~B
]�B
^B
^�B
^jB
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
abB
aHB
abB
abB
a�B
a�B
bNB
b�B
b�B
c B
c�B
dB
d&B
dB
dtB
d�B
d�B
eFB
e�B
e�B
f2B
fLB
ffB
fLB
f�B
f�B
f�B
gB
gmB
gmB
g�B
g�B
h$B
h$B
h
B
hsB
h�B
h�B
iB
iB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jB
jKB
jB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
poB
poB
p�B
p�B
p�B
p�B
q'B
qAB
q'B
q'B
qB
qB
q[B
qvB
q�B
q�B
r-B
rB
r�B
r�B
r�B
r�B
r�B
r�B
shB
tnB
tnB
tnB
t�B
uB
uB
u?B
utB
u�B
u�B
u�B
vB
v+B
v+B
v`B
vzB
v�B
v�B
wB
wB
v�B
w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�SB
�SB
�B
�B
�B
�9B
�9B
�B
�B
�B
�B
�9B
�9B
�B
�B
�B
�9B
�9B
�SB
�9B
�SB
�9B
�SB
��B
��B
�B
��B
��B
�?B+B8�B>BD3BJ�B[#Bv�B}"B��B�hB��B�B+�BC-BX�BjBs�By$B.B�B�xB�"B�EBY�B��B��B��B�	B��BյB׍B�0B�Bc�Bj0BaHBK�B%`B.IB B�B�!B�pB�@B��B��B�zB3�B
��B
�DB
�B
�rB
��B
��B
��B
tB
Z�B
G�B
>�B
4�B
+QB
�B
dB
  B	�B	�DB	�B	��B	B	w�B	r|B	pB	i�B	Z�B	F�B	/�B	*B	�B��B�/B��B��B	aB	 OB��B��B�zB�MB	"�B	(XB	�B	B	�B		B		B	�B	�B	~B	B	3�B	E�B	DB	O(B	T�B	Q�B	TaB	]�B	f2B	u�B	z�B	i�B	p;B	t�B	�iB	�}B	�_B	�cB	�$B	��B	�gB	�#B	��B	�?B	��B	�BB	�hB	��B	�>B	�B	��B	�B	�B	��B	��B	��B	�+B	��B	��B	�.B
UB
oB
�B
B
B
�B
 B
 �B
;B
�B

XB

�B
�B
�B
�B
aB
�B
oB
B
�B
�B
B
;B
 iB
 4B
 4B	��B
 �B
 iB
uB
GB
�B
�B
�B
�B
aB
B
B
�B
AB
'B
�B
�B
oB
�B
�B
�B
B
 �B
�B
'B
�B
�B
�B
B
�B
�B
�B
�B
UB
 B
 �B
 �B
B
 �B
 �B
 4B	��B
 iB
 4B
 B
 OB
 �B
 �B
 iB
  B	��B	�]B	�6B	��B	��B	��B	��B	�B	�`B	�FB	�`B	��B	�`B	��B	�2B	��B	��B	�zB	��B	�zB	��B	�B	��B	��B	�ZB	�tB	�%B	��B	�B	�+B	�FB	��B	�FB	�FB	�B	��B	��B	�hB	�B	�B	�B	�B	�B	�B	�B	�AB	�GB	�B	�9B	�%B	�hB	�B	�hB	��B	�B	�B	�MB	��B	�-B	�3B	�B	��B	�-B	��B	�B	�%B	��B	�FB	�B	��B	�B	�MB	�B	�%B	��B	�FB	��B	��B	�zB	��B	�+B	��B	��B	�B	�?B	�B	��B	�B	��B	�B	��B	��B	��B	�3B	��B	��B	��B	�RB	�8B	�RB	�XB	�rB	�XB	��B	��B	�B	��B	�	B	��B	�RB	�B	��B	��B	��B	��B	�rB	�XB	�>B	��B	��B	�^B	��B	��B	�dB	�B	��B	��B	��B	��B	�B	�6B	�B	�B	�6B	��B	�B	�6B	��B	�B
�B
B
mB
SB
mB
B
B
mB
YB
�B
�B
tB
�B
9B
?B
%B
�B
KB
�B
	�B
	�B
	B
	7B
	7B
	RB
	RB

�B
B

�B
)B
�B
B
dB
�B
~B
~B
~B
~B
~B
�B
�B
B
B
�B
<B
VB
pB
VB
(B
\B
BB
(B
BB
BB
BB
�B
\B
(B
\B
�B
.B
bB
bB
�B
4B
�B
�B
:B
�B
oB
�B
&B
�B
FB
{B
�B
{B
�B
�B
gB
MB
MB
2B
�B
B
9B
SB
�B
�B
$B
sB
�B
�B
+B
yB
1B
1B
eB
B
�B
B
7B
7B
QB
�B
�B
�B
	B
#B
qB
�B
�B
B
]B
]B
�B
�B
�B
dB
IB
~B
dB
dB
�B
�B
jB
�B
pB
�B
!-B
!bB
!�B
!�B
!�B
!�B
!�B
"B
!�B
"4B
"hB
"�B
"�B
"�B
#B
#:B
#TB
#:B
#:B
#�B
$tB
%B
%`B
%`B
%`B
%FB
%`B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(
B
(�B
)*B
)�B
)�B
*0B
*�B
+B
+B
+kB
+�B
+�B
+�B
,�B
,qB
,�B
,=B
,�B
-wB
-�B
.B
.cB
.cB
.�B
.}B
.}B
.cB
.}B
.IB
.�B
.�B
.�B
.�B
/5B
/iB
/�B
/�B
/�B
0;B
0!B
/�B
0!B
0UB
0;B
0oB
0�B
0UB
0!B
/�B
1[B
1'B
0UB
0�B
0�B
0�B
0UB
0�B
2aB
2�B
3�B
3�B
49B
4nB
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5B
4�B
5?B
5�B
6zB
6�B
6�B
6�B
6�B
6zB
6�B
7LB
7B
7�B
7�B
7�B
8lB
9XB
9�B
9�B
9�B
:xB
:B
:�B
;�B
<B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=qB
=<B
=<B
=�B
=�B
=�B
=�B
>B
>�B
>]B
>BB
>BB
>�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
@ B
?�B
?}B
?HB
>�B
>wB
>B
=�B
=�B
=�B
=�B
>BB
?HB
?}B
?�B
@iB
@�B
@�B
AB
A�B
AoB
AoB
AoB
A�B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
CB
B�B
C{B
CB
CaB
D�B
D�B
E�B
E�B
E�B
F%B
F%B
F%B
F?B
FtB
F�B
FtB
FtB
F�B
F�B
F�B
G+B
GB
G_B
G�B
G�B
HKB
H�B
H�B
H�B
H�B
IRB
I�B
I�B
J	B
I�B
J=B
J�B
J�B
K�B
L~B
L�B
L�B
L�B
MB
MB
MPB
M�B
M�B
N"B
N"B
NB
NB
N"B
N"B
N�B
N�B
NpB
NVB
N�B
N�B
N�B
OBB
OBB
O\B
OBB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QhB
Q�B
R B
R�B
S&B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
T�B
UB
UMB
UB
UB
UMB
U2B
U2B
T�B
T�B
T�B
T�B
T�B
UgB
V�B
WsB
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
X�B
X�B
YB
Y1B
YKB
YKB
YeB
YeB
Y�B
YB
YeB
Y�B
YeB
YB
ZkB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[�B
[�B
\]B
\�B
]/B
]IB
]~B
]~B
]�B
^B
^�B
^jB
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
abB
aHB
abB
abB
a�B
a�B
bNB
b�B
b�B
c B
c�B
dB
d&B
dB
dtB
d�B
d�B
eFB
e�B
e�B
f2B
fLB
ffB
fLB
f�B
f�B
f�B
gB
gmB
gmB
g�B
g�B
h$B
h$B
h
B
hsB
h�B
h�B
iB
iB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jB
jKB
jB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
poB
poB
p�B
p�B
p�B
p�B
q'B
qAB
q'B
q'B
qB
qB
q[B
qvB
q�B
q�B
r-B
rB
r�B
r�B
r�B
r�B
r�B
r�B
shB
tnB
tnB
tnB
t�B
uB
uB
u?B
utB
u�B
u�B
u�B
vB
v+B
v+B
v`B
vzB
v�B
v�B
wB
wB
v�B
w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220708035145  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220708035311  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220708035311  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220708035312                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220708125316  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220708125316  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220708040318                      G�O�G�O�G�O�                