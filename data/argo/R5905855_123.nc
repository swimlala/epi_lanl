CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-28T09:42:00Z creation;2022-06-28T09:42:01Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220628094200  20220628095823  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ڙ"���1   @�ڙ�F)�@1G�z��c�ȴ9X1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@y��@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,33C-�fC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch�Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @*=p@p��@��@��AA=A]A}A�{A��HA��HA��HA��HA��HA�A��Bp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��B��RB��RB��RB��RB��RB��B��B��RB��RB��RB��RB��RB��RBøRB��B˸RBυBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	��C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C,\C-C/C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc��Ce��Cg��Ci�)CkCm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��GC��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��D w
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
Dw
D�
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
D%}pD%�pD&w
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
DBw
DB�
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
DQw
DQ�
DRw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�	lA��>A��A��?Aϱ�Aϟ�AϋDA�jKA�N<A�;�A��A��RA���AιXAήIAΣ�AΜAΈ�A�w2A�f2A�U�A�=�A�6�A�0�A�"�A��
A�*�A��A���AʼA�-�AȰ�A�k�A�>wA��&AǿAǡ�A�xlAƛ�A��tA���A�+kAJA�`vA�r�A���A�A��PA�҉A�b�A�AA�;�A�v`A�X�A�1A���A�aA�|PA��A�_;A��A��
A�v`A��A���A��qA�� A�=�A���A���A��IA�_�A��:A���A��hA�l�A��A��A��A�یA�cTA�+A�Q�A�A|=qAv1�AqC�ApQ�Ao��AlMAi�hAgGAd(�Ac"hAa�~A`�.A_�4A^��A\,=AX"hAVFtAU��AT=�AO�~AL�AIAH;�AG��AF@ADo�AA�YA@bA>�3A<ĜA:J�A6��A6�A5?A4��A4#�A3��A3h
A3F�A3�A2�9A2oA1��A1��A1W?A0S&A/��A/0�A.C�A-�A,2�A*�A)+A(w�A(�HA)�MA)eA(��A'�A'A%m�A$��A#�~A"�4A!�VA!!�A �A =qA��AbA�A��A�A<�A�rA�mA͟A��A��A��A�A~�AuAb�AW�A�A�rAoiAh
A\�AS�A/AFAe�AS&A�A��AA�TA�sA��A��A-�AbNA{JA!A�A�Au%AIRAC�A��A� A4A>�AqA��AQ�A��AsAMA��A  Ac�A�AA�A�7A`BA
�,A
$tA	ĜA	M�AM�A�A��A!�A��As�A0�A�AƨAMjA��ATaA+kA�A҉AU2A�A�A��A|�ArGAVmA�oA��Al�ARTA;�A)_A �
A Ov@��@�l"@��@�E9@�I�@�n/@�]d@��h@�z�@��{@���@���@�w�@�	l@��T@��H@�5?@�G@�-w@��@���@�%F@��c@��@쀝@�e�@��@�@�A�@�^5@�l�@�>�@��@��@�\�@��@�[@�a|@��@࿱@ව@�L@�_@ߗ�@�.I@��@��@� i@��@�V@�<�@�!�@�6z@�xl@��T@�(�@���@��@��5@ں�@�O@ع$@�s�@׋�@��	@�O@�G�@Ԗ�@�N�@��@��Q@ӿH@Ӆ@Ҳ�@�@эP@��@ї$@�A @��Q@��@Μx@Κ@�-�@�X�@�%�@�A�@�dZ@�T�@�(�@�;d@Ƚ<@ȔF@��@Ǽ@�(�@ƾ�@�~@��Q@��@��;@�n/@�@�ی@_@��T@��@��r@��@�9�@���@�u%@�I�@���@�@�s�@��@�;@�tT@���@�0�@��@���@�]d@��@���@�p�@��"@���@��W@�;d@��[@� i@�Y@�$t@�@��H@���@��m@��h@��@���@��@�9X@�G@�ݘ@�~�@�V@�Z@���@�s@�N<@��@���@�S�@��]@��;@��@�j@�^�@�O�@��f@���@�YK@���@��X@�S&@�@���@��w@���@�L0@��@�خ@��H@��V@�>�@��!@�)�@��a@��=@�c@�o�@�g�@�X@�6z@�S@��@�U2@�#:@�ϫ@�f�@�G�@��|@�Xy@���@���@��;@���@�1�@���@���@�l�@�6�@�)�@��@��@�ݘ@���@�1�@��@���@�K^@�$@��@���@��@��@��@�b@�K�@��@��@�\�@�:�@�,=@��@��3@���@�,�@��@���@�2�@��@�خ@���@�J�@��@��`@���@��@�r�@�e�@�?@�G@���@��7@�]�@�$t@�o@�;@��@��e@���@�C�@�	�@��N@���@�X�@�8@�;@���@���@�c�@�b@���@�RT@��K@��}@�}V@�l"@�"h@��@��@�~�@�_p@�9�@��5@��+@�m�@�&�@�7@��@��@�֡@��@��.@���@�Q@��@���@�hs@�X@�P�@�>�@�-w@��K@��!@��6@��.@�I�@���@���@��@���@���@�U�@�8@��@�~(@��Z@��=@�`B@��@�֡@���@�$@���@�U�@�=@���@�u%@��@���@���@��d@��"@�l�@�dZ@�O@��@�ی@��R@�~�@�=q@�($@��Q@��q@���@�}�@�a�@�:�@�
=@��@��h@��o@�M@��r@��@���@�qv@�?}@��@��@��@��9@�Q@�&�@�u@���@���@��t@��{@�&�@�ں@���@�g8@�(�@�4@�+@�;@�0@_p@~�H@~s�@~#:@}��@}��@}��@}4@|��@|D�@|4n@{��@z�6@y�@yu�@x�?@x!@w(@v�\@v��@v��@v3�@u��@u��@uQ�@u4@s�]@s|�@sS@rz@q��@q��@q�=@q��@q�M@qVm@q@p�U@p:�@p�@o��@o˒@o��@o��@o8@n�"@n��@n�@mԕ@m��@mc�@mL�@m7L@m/@m0�@m(�@m(�@m&�@m%F@l��@l��@lg8@l$@k��@kt�@k�@j��@j�1@jp;@j
�@i�'@i7L@h�5@h֡@h��@h֡@h�)@hu�@g��@g=@f�@f�x@eԕ@e`B@d�@d?�@c��@c@O@c4�@c1�@c"�@b�]@b�}@b{�@b;�@a�d@a\�@a�@`�@`�@`Ft@_��@_�@^��@^Ta@^O@]o @\�	@\1'@[��@[��@[��@[��@[g�@[!-@Z�s@Y�@YS&@X�_@X9X@X�@W�}@W�@V�]@V��@V��@V-@V@U��@U@Um]@UIR@U�@T��@T:�@T@S�*@S@Rl�@R+k@R	@Q�@Q��@Q��@Q�X@Q�7@QB�@P��@P�@Pm�@P	�@O@O@N�@NE�@M��@M�@L�@L�/@L�?@L�@L��@LM@K{J@K;d@K)_@K�@J��@J��@J��@J;�@I��@I4@Hѷ@H��@G�@G��@GJ#@F�!@FGE@E�@E�S@EVm@EA @E@D��@DI�@C�W@CC�@B��@B�,@B��@B��@B��@B	@A��@A�X@A8�@@�@@�u@@Xy@?o�@>��@>��@>�b@>�F@>��@>;�@>u@=X@<��@<��@<��@<tT@<oi@<V�@</�@;�Q@;U�@;1�@;�@:�M@:?@9�@9��@9o @9;@8��@8�@7��@7>�@7,�@7!-@7Y@6��@6L0@6&�@6O@5��@4j@3�@3��@3J#@2V@1��@1X@1Q�@1Dg@1�@0��@0Ft@/�@/!-@.��@.{@-��@-��@-u�@-:�@,�e@,�.@,�@,_@,7@+�&@+�K@+��@+��@+1�@+�@*�'@*a|@*3�@*�@*	@)��@)��@)��@)T�@)8�@)7L@)7L@)/@)+�@)%F@(��@(z�@(~(@(oi@('R@(!@(%�@'�g@'e�@';d@&ں@&�6@&4@%�"@%0�@%@%%F@%0�@%�@$�[@$��@$w�@$H@$~@#��@#��@#��@#�@#�f@#�@"ȴ@"�!@"�!@"�@"�\@"u%@"V@"?@"B[@"@�@":*@"6�@"�@!�.@!�)@!�>@!�j@!��@ ��@ l"@��@�*@t�@O@/�@}V@�@�@��@��@O�@�@�)@�@tT@A�@@��@�:@$t@�H@҉@��@�1@��@��@��@��@{�@.�@�@hs@^�@S&@=�@ی@r�@�@��@��@o�@a@O@;d@�@�@~�@c @-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�	lA��>A��A��?Aϱ�Aϟ�AϋDA�jKA�N<A�;�A��A��RA���AιXAήIAΣ�AΜAΈ�A�w2A�f2A�U�A�=�A�6�A�0�A�"�A��
A�*�A��A���AʼA�-�AȰ�A�k�A�>wA��&AǿAǡ�A�xlAƛ�A��tA���A�+kAJA�`vA�r�A���A�A��PA�҉A�b�A�AA�;�A�v`A�X�A�1A���A�aA�|PA��A�_;A��A��
A�v`A��A���A��qA�� A�=�A���A���A��IA�_�A��:A���A��hA�l�A��A��A��A�یA�cTA�+A�Q�A�A|=qAv1�AqC�ApQ�Ao��AlMAi�hAgGAd(�Ac"hAa�~A`�.A_�4A^��A\,=AX"hAVFtAU��AT=�AO�~AL�AIAH;�AG��AF@ADo�AA�YA@bA>�3A<ĜA:J�A6��A6�A5?A4��A4#�A3��A3h
A3F�A3�A2�9A2oA1��A1��A1W?A0S&A/��A/0�A.C�A-�A,2�A*�A)+A(w�A(�HA)�MA)eA(��A'�A'A%m�A$��A#�~A"�4A!�VA!!�A �A =qA��AbA�A��A�A<�A�rA�mA͟A��A��A��A�A~�AuAb�AW�A�A�rAoiAh
A\�AS�A/AFAe�AS&A�A��AA�TA�sA��A��A-�AbNA{JA!A�A�Au%AIRAC�A��A� A4A>�AqA��AQ�A��AsAMA��A  Ac�A�AA�A�7A`BA
�,A
$tA	ĜA	M�AM�A�A��A!�A��As�A0�A�AƨAMjA��ATaA+kA�A҉AU2A�A�A��A|�ArGAVmA�oA��Al�ARTA;�A)_A �
A Ov@��@�l"@��@�E9@�I�@�n/@�]d@��h@�z�@��{@���@���@�w�@�	l@��T@��H@�5?@�G@�-w@��@���@�%F@��c@��@쀝@�e�@��@�@�A�@�^5@�l�@�>�@��@��@�\�@��@�[@�a|@��@࿱@ව@�L@�_@ߗ�@�.I@��@��@� i@��@�V@�<�@�!�@�6z@�xl@��T@�(�@���@��@��5@ں�@�O@ع$@�s�@׋�@��	@�O@�G�@Ԗ�@�N�@��@��Q@ӿH@Ӆ@Ҳ�@�@эP@��@ї$@�A @��Q@��@Μx@Κ@�-�@�X�@�%�@�A�@�dZ@�T�@�(�@�;d@Ƚ<@ȔF@��@Ǽ@�(�@ƾ�@�~@��Q@��@��;@�n/@�@�ی@_@��T@��@��r@��@�9�@���@�u%@�I�@���@�@�s�@��@�;@�tT@���@�0�@��@���@�]d@��@���@�p�@��"@���@��W@�;d@��[@� i@�Y@�$t@�@��H@���@��m@��h@��@���@��@�9X@�G@�ݘ@�~�@�V@�Z@���@�s@�N<@��@���@�S�@��]@��;@��@�j@�^�@�O�@��f@���@�YK@���@��X@�S&@�@���@��w@���@�L0@��@�خ@��H@��V@�>�@��!@�)�@��a@��=@�c@�o�@�g�@�X@�6z@�S@��@�U2@�#:@�ϫ@�f�@�G�@��|@�Xy@���@���@��;@���@�1�@���@���@�l�@�6�@�)�@��@��@�ݘ@���@�1�@��@���@�K^@�$@��@���@��@��@��@�b@�K�@��@��@�\�@�:�@�,=@��@��3@���@�,�@��@���@�2�@��@�خ@���@�J�@��@��`@���@��@�r�@�e�@�?@�G@���@��7@�]�@�$t@�o@�;@��@��e@���@�C�@�	�@��N@���@�X�@�8@�;@���@���@�c�@�b@���@�RT@��K@��}@�}V@�l"@�"h@��@��@�~�@�_p@�9�@��5@��+@�m�@�&�@�7@��@��@�֡@��@��.@���@�Q@��@���@�hs@�X@�P�@�>�@�-w@��K@��!@��6@��.@�I�@���@���@��@���@���@�U�@�8@��@�~(@��Z@��=@�`B@��@�֡@���@�$@���@�U�@�=@���@�u%@��@���@���@��d@��"@�l�@�dZ@�O@��@�ی@��R@�~�@�=q@�($@��Q@��q@���@�}�@�a�@�:�@�
=@��@��h@��o@�M@��r@��@���@�qv@�?}@��@��@��@��9@�Q@�&�@�u@���@���@��t@��{@�&�@�ں@���@�g8@�(�@�4@�+@�;@�0@_p@~�H@~s�@~#:@}��@}��@}��@}4@|��@|D�@|4n@{��@z�6@y�@yu�@x�?@x!@w(@v�\@v��@v��@v3�@u��@u��@uQ�@u4@s�]@s|�@sS@rz@q��@q��@q�=@q��@q�M@qVm@q@p�U@p:�@p�@o��@o˒@o��@o��@o8@n�"@n��@n�@mԕ@m��@mc�@mL�@m7L@m/@m0�@m(�@m(�@m&�@m%F@l��@l��@lg8@l$@k��@kt�@k�@j��@j�1@jp;@j
�@i�'@i7L@h�5@h֡@h��@h֡@h�)@hu�@g��@g=@f�@f�x@eԕ@e`B@d�@d?�@c��@c@O@c4�@c1�@c"�@b�]@b�}@b{�@b;�@a�d@a\�@a�@`�@`�@`Ft@_��@_�@^��@^Ta@^O@]o @\�	@\1'@[��@[��@[��@[��@[g�@[!-@Z�s@Y�@YS&@X�_@X9X@X�@W�}@W�@V�]@V��@V��@V-@V@U��@U@Um]@UIR@U�@T��@T:�@T@S�*@S@Rl�@R+k@R	@Q�@Q��@Q��@Q�X@Q�7@QB�@P��@P�@Pm�@P	�@O@O@N�@NE�@M��@M�@L�@L�/@L�?@L�@L��@LM@K{J@K;d@K)_@K�@J��@J��@J��@J;�@I��@I4@Hѷ@H��@G�@G��@GJ#@F�!@FGE@E�@E�S@EVm@EA @E@D��@DI�@C�W@CC�@B��@B�,@B��@B��@B��@B	@A��@A�X@A8�@@�@@�u@@Xy@?o�@>��@>��@>�b@>�F@>��@>;�@>u@=X@<��@<��@<��@<tT@<oi@<V�@</�@;�Q@;U�@;1�@;�@:�M@:?@9�@9��@9o @9;@8��@8�@7��@7>�@7,�@7!-@7Y@6��@6L0@6&�@6O@5��@4j@3�@3��@3J#@2V@1��@1X@1Q�@1Dg@1�@0��@0Ft@/�@/!-@.��@.{@-��@-��@-u�@-:�@,�e@,�.@,�@,_@,7@+�&@+�K@+��@+��@+1�@+�@*�'@*a|@*3�@*�@*	@)��@)��@)��@)T�@)8�@)7L@)7L@)/@)+�@)%F@(��@(z�@(~(@(oi@('R@(!@(%�@'�g@'e�@';d@&ں@&�6@&4@%�"@%0�@%@%%F@%0�@%�@$�[@$��@$w�@$H@$~@#��@#��@#��@#�@#�f@#�@"ȴ@"�!@"�!@"�@"�\@"u%@"V@"?@"B[@"@�@":*@"6�@"�@!�.@!�)@!�>@!�j@!��@ ��@ l"@��@�*@t�@O@/�@}V@�@�@��@��@O�@�@�)@�@tT@A�@@��@�:@$t@�H@҉@��@�1@��@��@��@��@{�@.�@�@hs@^�@S&@=�@ی@r�@�@��@��@o�@a@O@;d@�@�@~�@c @-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
7�B
8RB
8B
8B
88B
9$B
9>B
>wB
G�B
T,B
_�B
^�B
]IB
[�B
Z�B
Z�B
Z�B
Z�B
ZQB
Y�B
YB
[	B
[�B
[#B
ZB
\�B
`'B
cB
k�B
�JB
�FB
ƨB<B	B<�BA�BFYBOvBS�BW�B[�Bb4B��B��B��BGB6B�BBBKB 'B�BFB_BB�B�B�B	RBB9B�B��B��B�B��BۦB�6B�B�wB�B��Bw2BjKBZkBK�B3�B%�BpB
�,B
�B
��B
��B
h�B
S�B
2�B
EB	�4B	�SB	��B	�B	��B	�jB	��B	v�B	gB	b�B	[�B	XB	S&B	K�B	BuB	2�B	*KB	'B	!�B	B		�B	B�B��B�B�B�nB�|B�MB��B�B��B��B	 OB	�B	uB	[B	�B	�B	�B	�B	�B	vB	�B	�B	)�B	6�B	;B	?cB	DB	LdB	S�B	gB	wfB	�vB	��B	��B	�cB	��B	��B	�MB	��B	āB	�XB	�~B	�xB	͹B	�gB	��B	�B	�mB	�B	��B	�wB	��B	� B	�B	�B	��B	��B	�B	�GB	�?B	�LB	�xB	�*B	��B	�rB	�>B	�	B	��B	��B
GB
	B

�B
�B

�B

�B

�B

�B
	RB
�B
B	�VB	�8B	�B	��B	��B	��B	�$B	��B	��B
UB
�B
B
B
B
1B
EB
�B
SB
�B
�B
�B
�B
uB
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
B
�B
B
�B
[B
AB
'B
oB
 OB
 4B
 iB
 �B
B
 �B
B
B
 iB
 iB
 OB
 B
 iB
 iB
 iB	��B	�}B	��B	�HB	�wB	�(B	��B	��B	��B	��B	��B	��B	�RB	�fB	�B	��B	��B	�B	�%B	��B	�9B	��B	�B	�3B	�B	��B	�aB	��B	�|B	�-B	�B	�B	�vB	�B	�B	��B	�B	��B	��B	�3B	�B	�3B	�9B	��B	�B	�B	�hB	�B	�B	��B	�AB	��B	�B	�aB	��B	��B	�'B	�B	��B	��B	��B	�oB	�}B	�)B	��B	�KB	�B	�kB	�B	��B	�B	�B	�B	��B	�B	�qB	�|B	�?B	�B	�B	��B	��B	��B	�MB	�GB	��B	�?B	�8B	�*B	��B	��B	��B	�XB	�lB	�B	�XB	�B	��B	�DB	�*B	��B	��B	��B	�0B	��B	��B	�B	�B	��B	�PB	��B	��B	�6B	�jB	��B	��B	�qB	��B	�qB	��B	��B	��B	�jB	��B	�(B	��B	��B	�BB	��B	�BB	��B
B
�B
B
�B
 B
B
�B
�B
B

�B
DB
�B
~B
0B
�B
0B
~B
dB
B
�B
B
�B
�B
�B
�B
�B
"B
"B
B
pB
�B
�B
BB
\B
�B
bB
bB
 B
�B
4B
�B
�B
�B
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
[B
&B
uB
FB
aB
aB
FB
B
2B
gB
�B
�B
�B
�B
�B
�B
B
mB
�B
YB
�B
�B
�B
sB
YB
sB
B
+B
�B
�B
�B
QB
QB
kB
kB
QB
�B
�B
#B
�B
�B
�B
CB
)B
CB
�B
�B
�B
CB
]B
xB
�B
�B
dB
~B
/B
�B
�B
�B
B
OB
OB
�B
!B
!B
;B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
# B
#nB
#�B
#�B
$&B
$@B
$�B
$�B
$�B
$�B
%FB
%zB
%zB
%�B
%�B
&LB
'mB
'�B
'�B
'�B
'�B
'�B
'�B
($B
(>B
($B
($B
($B
(
B
(�B
)B
(�B
(�B
)yB
)�B
)�B
)�B
)�B
*B
)�B
)�B
*eB
*�B
+�B
+�B
,WB
,�B
,�B
,�B
,�B
-B
-�B
-�B
-�B
-B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
./B
.IB
.�B
.}B
.�B
.�B
/B
/�B
/�B
/�B
/�B
/iB
/B
/OB
/�B
/�B
/�B
0�B
1'B
1'B
0�B
1[B
2GB
2�B
2�B
2�B
2�B
2�B
2aB
2GB
2aB
2-B
2GB
2aB
2-B
1�B
1�B
2-B
2GB
2|B
2�B
2�B
2�B
2|B
2GB
1�B
1�B
1AB
1vB
1AB
1[B
2B
3�B
3�B
4B
3�B
3�B
2�B
2GB
3�B
4B
4B
4B
4B
4B
3�B
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
5tB
5�B
5�B
5�B
6zB
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8B
9�B
9�B
:�B
:�B
;B
;JB
;dB
;�B
<B
<6B
<B
<B
<6B
<jB
<�B
<PB
<�B
<�B
<�B
<�B
<�B
="B
=qB
=�B
>BB
>wB
>wB
>]B
>]B
>�B
?HB
?HB
?}B
?�B
@4B
@iB
@�B
AB
AoB
A�B
A�B
A�B
A�B
BB
BAB
BAB
B[B
B�B
B�B
C-B
C-B
CGB
CGB
C�B
DMB
D�B
D�B
D�B
EB
ESB
F?B
FtB
FtB
FtB
FYB
FtB
F�B
FtB
F�B
FtB
GEB
G_B
G+B
G_B
G�B
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
H�B
IB
IlB
I�B
I�B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KB
K)B
K^B
KDB
KB
KDB
LB
K�B
K�B
K�B
K�B
K�B
L0B
L�B
MB
M6B
MPB
M6B
MPB
M�B
N<B
NB
M�B
NB
M�B
N"B
N"B
NB
NpB
N�B
OB
OBB
O\B
OvB
OvB
O�B
P}B
P�B
QhB
Q�B
Q�B
RB
RB
RB
R�B
RoB
RoB
R�B
SB
S@B
S@B
S�B
TaB
T{B
T{B
T{B
TaB
T�B
T�B
UgB
U�B
U�B
U�B
VB
U�B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
XyB
YKB
YKB
Y�B
ZQB
ZQB
Z�B
Z�B
[#B
[=B
[WB
[=B
[�B
[�B
\)B
[�B
\CB
]�B
]�B
]~B
]�B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_VB
_�B
`B
`�B
aB
aB
a-B
abB
bNB
bB
b4B
b4B
b�B
b�B
cB
cB
cB
c�B
c�B
d&B
d�B
d�B
d�B
eB
e,B
e,B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fLB
fLB
fLB
f�B
f�B
f�B
f�B
f�B
gB
g8B
g�B
gmB
h$B
hsB
h�B
h�B
h�B
h�B
h�B
iB
i*B
iyB
iyB
i�B
i�B
jB
i�B
jB
i�B
j�B
j�B
j�B
j�B
kB
kB
k6B
k6B
kkB
kQB
kkB
kQB
kQB
k�B
k�B
k�B
k�B
kkB
kkB
l"B
lqB
l�B
mB
mCB
mwB
mwB
m�B
ncB
nIB
ncB
n}B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
poB
p�B
p�B
q[B
rB
rGB
rGB
r|B
r|B
r|B
r�B
r�B
r�B
sMB
shB
sh111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
7�B
8RB
8B
8B
88B
9$B
9>B
>wB
G�B
T,B
_�B
^�B
]IB
[�B
Z�B
Z�B
Z�B
Z�B
ZQB
Y�B
YB
[	B
[�B
[#B
ZB
\�B
`'B
cB
k�B
�JB
�FB
ƨB<B	B<�BA�BFYBOvBS�BW�B[�Bb4B��B��B��BGB6B�BBBKB 'B�BFB_BB�B�B�B	RBB9B�B��B��B�B��BۦB�6B�B�wB�B��Bw2BjKBZkBK�B3�B%�BpB
�,B
�B
��B
��B
h�B
S�B
2�B
EB	�4B	�SB	��B	�B	��B	�jB	��B	v�B	gB	b�B	[�B	XB	S&B	K�B	BuB	2�B	*KB	'B	!�B	B		�B	B�B��B�B�B�nB�|B�MB��B�B��B��B	 OB	�B	uB	[B	�B	�B	�B	�B	�B	vB	�B	�B	)�B	6�B	;B	?cB	DB	LdB	S�B	gB	wfB	�vB	��B	��B	�cB	��B	��B	�MB	��B	āB	�XB	�~B	�xB	͹B	�gB	��B	�B	�mB	�B	��B	�wB	��B	� B	�B	�B	��B	��B	�B	�GB	�?B	�LB	�xB	�*B	��B	�rB	�>B	�	B	��B	��B
GB
	B

�B
�B

�B

�B

�B

�B
	RB
�B
B	�VB	�8B	�B	��B	��B	��B	�$B	��B	��B
UB
�B
B
B
B
1B
EB
�B
SB
�B
�B
�B
�B
uB
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
B
�B
B
�B
[B
AB
'B
oB
 OB
 4B
 iB
 �B
B
 �B
B
B
 iB
 iB
 OB
 B
 iB
 iB
 iB	��B	�}B	��B	�HB	�wB	�(B	��B	��B	��B	��B	��B	��B	�RB	�fB	�B	��B	��B	�B	�%B	��B	�9B	��B	�B	�3B	�B	��B	�aB	��B	�|B	�-B	�B	�B	�vB	�B	�B	��B	�B	��B	��B	�3B	�B	�3B	�9B	��B	�B	�B	�hB	�B	�B	��B	�AB	��B	�B	�aB	��B	��B	�'B	�B	��B	��B	��B	�oB	�}B	�)B	��B	�KB	�B	�kB	�B	��B	�B	�B	�B	��B	�B	�qB	�|B	�?B	�B	�B	��B	��B	��B	�MB	�GB	��B	�?B	�8B	�*B	��B	��B	��B	�XB	�lB	�B	�XB	�B	��B	�DB	�*B	��B	��B	��B	�0B	��B	��B	�B	�B	��B	�PB	��B	��B	�6B	�jB	��B	��B	�qB	��B	�qB	��B	��B	��B	�jB	��B	�(B	��B	��B	�BB	��B	�BB	��B
B
�B
B
�B
 B
B
�B
�B
B

�B
DB
�B
~B
0B
�B
0B
~B
dB
B
�B
B
�B
�B
�B
�B
�B
"B
"B
B
pB
�B
�B
BB
\B
�B
bB
bB
 B
�B
4B
�B
�B
�B
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
[B
&B
uB
FB
aB
aB
FB
B
2B
gB
�B
�B
�B
�B
�B
�B
B
mB
�B
YB
�B
�B
�B
sB
YB
sB
B
+B
�B
�B
�B
QB
QB
kB
kB
QB
�B
�B
#B
�B
�B
�B
CB
)B
CB
�B
�B
�B
CB
]B
xB
�B
�B
dB
~B
/B
�B
�B
�B
B
OB
OB
�B
!B
!B
;B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
# B
#nB
#�B
#�B
$&B
$@B
$�B
$�B
$�B
$�B
%FB
%zB
%zB
%�B
%�B
&LB
'mB
'�B
'�B
'�B
'�B
'�B
'�B
($B
(>B
($B
($B
($B
(
B
(�B
)B
(�B
(�B
)yB
)�B
)�B
)�B
)�B
*B
)�B
)�B
*eB
*�B
+�B
+�B
,WB
,�B
,�B
,�B
,�B
-B
-�B
-�B
-�B
-B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
./B
.IB
.�B
.}B
.�B
.�B
/B
/�B
/�B
/�B
/�B
/iB
/B
/OB
/�B
/�B
/�B
0�B
1'B
1'B
0�B
1[B
2GB
2�B
2�B
2�B
2�B
2�B
2aB
2GB
2aB
2-B
2GB
2aB
2-B
1�B
1�B
2-B
2GB
2|B
2�B
2�B
2�B
2|B
2GB
1�B
1�B
1AB
1vB
1AB
1[B
2B
3�B
3�B
4B
3�B
3�B
2�B
2GB
3�B
4B
4B
4B
4B
4B
3�B
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
5tB
5�B
5�B
5�B
6zB
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8B
9�B
9�B
:�B
:�B
;B
;JB
;dB
;�B
<B
<6B
<B
<B
<6B
<jB
<�B
<PB
<�B
<�B
<�B
<�B
<�B
="B
=qB
=�B
>BB
>wB
>wB
>]B
>]B
>�B
?HB
?HB
?}B
?�B
@4B
@iB
@�B
AB
AoB
A�B
A�B
A�B
A�B
BB
BAB
BAB
B[B
B�B
B�B
C-B
C-B
CGB
CGB
C�B
DMB
D�B
D�B
D�B
EB
ESB
F?B
FtB
FtB
FtB
FYB
FtB
F�B
FtB
F�B
FtB
GEB
G_B
G+B
G_B
G�B
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
H�B
IB
IlB
I�B
I�B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KB
K)B
K^B
KDB
KB
KDB
LB
K�B
K�B
K�B
K�B
K�B
L0B
L�B
MB
M6B
MPB
M6B
MPB
M�B
N<B
NB
M�B
NB
M�B
N"B
N"B
NB
NpB
N�B
OB
OBB
O\B
OvB
OvB
O�B
P}B
P�B
QhB
Q�B
Q�B
RB
RB
RB
R�B
RoB
RoB
R�B
SB
S@B
S@B
S�B
TaB
T{B
T{B
T{B
TaB
T�B
T�B
UgB
U�B
U�B
U�B
VB
U�B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
XyB
YKB
YKB
Y�B
ZQB
ZQB
Z�B
Z�B
[#B
[=B
[WB
[=B
[�B
[�B
\)B
[�B
\CB
]�B
]�B
]~B
]�B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_VB
_�B
`B
`�B
aB
aB
a-B
abB
bNB
bB
b4B
b4B
b�B
b�B
cB
cB
cB
c�B
c�B
d&B
d�B
d�B
d�B
eB
e,B
e,B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fLB
fLB
fLB
f�B
f�B
f�B
f�B
f�B
gB
g8B
g�B
gmB
h$B
hsB
h�B
h�B
h�B
h�B
h�B
iB
i*B
iyB
iyB
i�B
i�B
jB
i�B
jB
i�B
j�B
j�B
j�B
j�B
kB
kB
k6B
k6B
kkB
kQB
kkB
kQB
kQB
k�B
k�B
k�B
k�B
kkB
kkB
l"B
lqB
l�B
mB
mCB
mwB
mwB
m�B
ncB
nIB
ncB
n}B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
poB
p�B
p�B
q[B
rB
rGB
rGB
r|B
r|B
r|B
r�B
r�B
r�B
sMB
shB
sh111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220628094153  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220628094200  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220628094201  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220628094201                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220628184205  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220628184205  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220628095823                      G�O�G�O�G�O�                