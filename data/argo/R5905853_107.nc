CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:41:55Z creation;2022-06-04T17:41:56Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174155  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               kA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٱ/J%*�1   @ٱ/�&N!@.�`A�7L�cA/��w1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�A�33B  B��B  B   B(  B0  B7��B@  BH  BP  BX  B`��Bh  Bp  Bw��B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�ffB���B���B�  B���B�  B�  B���B˙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C33C  C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ Dϼ�D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@}p�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA�z�A�{Bp�B
>Bp�Bp�B'p�B/p�B7
>B?p�BGp�BOp�BWp�B`=qBgp�Bop�Bw
>Bp�B��RB��RB��RB�Q�B��RB��RB�Q�B��RB��RB��RB��B��B��B��RB��B��RBøRBȅB�Q�BϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C\C�)CCC�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE��CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CUCW�)CY�)C[�)C]�)C_�)Ca�)CcCe�)Cg�)Ci�)Ck�)Cm�)Co�)CqCs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D!�D"w
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
DX�pDYw
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
Dx}pDx�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��RD�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�DϸRD���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�8RD�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�~�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�8RD�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A�@A�@�A�@�A�A A�A�A�B�A�C�A�DgA�C�A�CaA�8�A��	AѪeAј�Aя\A�49A��KA�/�A�	lA��gA�C�A���A��rA�5�A�ܒA�\A�jKA�dZA��;AǨ$A�:�A�qA�)�A�&LA�-�A���A��A�[�A���A�$A���A�Q�A�|�A��A��A���A���A�IA�h�A��!A��A�B[A� 4A��:A��4A�	�A�fA���A�4A��qA���A�K^A�5�A~�A}a|Ay{JAv�[Au�Aq�An��Aj�AcN<A^��A\qAY�qAX��AW�#AU�AS!�AR �AQ2�AO5?AM~(AJ=AH�AG��AEm]A?�A=�wA=��A>�A@OvA@:�A?$tA=�]A:��A8�@A5rGA3�A5��A7	�A5��A1��A.qvA,�5A) \A$A iAߤA��A�MA�jAVmA$�A7LA+A	A	lA�AĜA	AS�A1�A�hA�OA�+A�A��AS�A��A�RA�A�RA��Am]AZA@A�/Ag8A6�A��A@OAQ�A?�A�KA��A�AȴA��A��AN�A��A�.A($A�!A�A%FA��ADgA�mAqA�hAXyAZAZ�ATaA�AJ�A��AuAhsAA A%�A!-A
�A	-�A�oA�vA��A�sA��AD�A��A6�A�A�A��A��Aw�AiDA&A��AH�A �}A @���@��@��O@�!�@���@��d@�s@��@��@��P@���@�u%@�S�@�F�@�'R@���@�-@� �@��@�@�U�@���@���@�x@��@�e@�}V@�-@�@�_�@��H@�s@��@��B@��@띲@�8�@�T�@�z@�m�@�)�@�ϫ@��g@��@��;@睲@�l�@�Y@�~�@��g@��y@�#:@�b�@�m�@ዬ@�8@�R@�L0@߫�@��)@޵�@޽<@޾�@�{@��d@ݎ"@�G�@��2@��@�\)@ڵ�@�~(@�M@��+@�u�@��f@�{�@�@׺^@��@��Z@Տ�@�t�@�B�@��E@�}V@�4n@�n/@�o@ң@ѡ�@�C�@�>B@ϗ�@ρ@Α @�]d@͵t@�@̱�@�*�@�_@�&@���@�X�@��s@ǅ@�8�@���@�N�@ŋ�@���@ķ�@ċD@�C�@��@� �@×$@�7L@»�@�9X@�@��@�(@�͟@�q�@�'R@��@��M@�@@��@�H�@�x@���@���@���@�N�@��@���@�ϫ@���@��=@�Y�@���@�V@��&@���@��6@���@���@�E9@��@�y>@�@���@��@�<6@��@�z�@�
�@��6@��'@�v`@�@O@��@�c @��@��V@�Vm@�*0@��h@���@�R�@�3�@��@�J�@���@�m�@��@�ƨ@��@���@�:�@���@�H�@�,�@�@���@�C-@��@��=@�\�@�(�@�֡@���@�?@��z@�zx@�4@��@���@�_@�@���@�`B@���@��.@���@�=�@��8@�@���@��@�ߤ@���@�YK@�:*@��@��#@��@�ߤ@�;�@��@��V@�_p@�҉@�q@��@��}@�}�@�A @��@��b@�;�@��@��;@��w@��@���@���@��1@��@���@���@�ԕ@�n/@�{�@�{@�ԕ@�l�@�;d@��@���@�D�@��@���@��"@�v`@�T�@�:�@�͟@�w�@�?@�@��Q@���@�9�@���@��@�^5@���@�F@��@��X@���@�?@�ϫ@���@�|@�c�@��@��e@�I�@�-�@�$@�@���@��w@���@�33@��B@��<@���@�W�@�u@���@�S�@��@��2@��m@���@��@��.@�[�@��+@��a@��	@�rG@�O@�=�@��v@��I@�H@��@�� @���@���@�s�@�Z�@�/@�C@�Y@�V@��m@�Z�@�"h@��@��d@�rG@�@@��v@��'@��b@���@���@�X�@���@���@���@��@��Y@�l�@�O@�@���@�9�@��f@���@��F@�C-@�@�k@~��@~u@}�C@}��@}a�@}q@|��@|�@{�*@{\)@{�@zv�@ys�@x�P@x��@w�@@wiD@w$t@v�h@v=q@u�@up�@uq@t�@t�u@t7@s�m@s\)@s@r+k@qp�@qL�@q5�@q/@q@@p�5@p$@o�F@n�2@nxl@nTa@m�@m	l@l]d@l�@k�w@k�:@kg�@k�@j�+@i�@h�`@hg8@h7�@h"h@g��@g�{@go@f��@fL0@f@e�-@eS&@e�@dj@c�q@cK�@b��@bO@a�j@a��@a��@a2a@`��@`��@`e�@`G@_�@_U�@^��@^^5@]�)@]@@\�@\Ft@[�P@[
=@Z�}@Z#:@Ze@Z�@Y�D@Y�T@Y�@Y�@Y�7@YS&@Y�@X��@Xw�@W�:@W"�@V�@V�6@Vi�@V3�@V�@U�@U��@U@Tr�@S��@S�@R��@R�@Q��@Q��@QX@P��@O�&@O;d@OC@N��@N~�@M�j@M|@M0�@M;@L��@LZ@K��@K��@KX�@Ko@J�@Jff@J	@I�N@I��@I��@IF@H��@Hl"@HK^@G�A@G�@G(@Fl�@F0U@E�@E��@E��@DɆ@Dy>@D �@Cخ@C��@B�"@Bc @B6�@Bu@A��@Am]@A*0@A�@@�E@@Q�@?ƨ@?�@?��@?��@?F�@?
=@>��@>�1@>h
@>L0@=��@=@=L�@<�_@<y>@<Q�@;�;@;��@;P�@:�8@:��@:��@:M�@9�@9�"@9e,@95�@9	l@8�v@8��@8I�@7�
@7�@7x@7iD@7\)@71�@6�X@6�@6�A@60U@5�@5��@5��@5G�@5�@4��@4z�@3�[@3v`@3b�@3,�@3�@2��@2�@2�L@2��@2M�@2)�@1�@1�@1T�@0�@0��@0�9@0��@0q@0H@0@/��@/��@/~�@/RT@/9�@.�"@.�@.�@.�@.}V@.Z�@.M�@.B[@-�@-rG@-0�@-�@,�9@,��@,]d@,-�@+J#@*��@*Ta@*V@*@�@*�@)��@)}�@(��@(�[@(y>@(M@(�@'�W@'ݘ@'˒@'�*@'x@'~�@'dZ@'/�@&�M@&��@&-@&�@%�.@%ԕ@%*0@%%@$�v@$�D@$4n@#�@#�@#�F@#�@@#a@"�}@"1�@"�@!�)@!�z@!�t@!�"@!m]@!Q�@!IR@!<6@!�@ ��@ ��@ 7�@ ~@ �@��@{J@e�@Mj@!-@�y@ߤ@�s@��@{�@^5@&�@��@�o@��@�@zx@B�@&�@#�@�@�@�P@�`@�E@��@r�@ �@�m@��@��@W?@$t@�@�,@u%@Q@B[@0U@4@�Z@��@�'@�~@k�@Vm@0�@��@�	@�5@�@Ĝ@��@��@oi@A�@��@��@��@s@Mj@8@"�@��@n�@;�@0U@@��@x�@B�@#�@�@�@�@Ĝ@�@Q�@  @~�@dZ@b�@RT@F�@F�@;d@&@�@ں@q�@YK@�@_@�@��@��@��@o @<6@=�@=�@2a@(�@	l@�@I�@(�@�@� @��@�@҉@��@v�@c @W�@Q@8�@�@�@��@p�@f�@%F@�@��@ѷ@�$@�@~(@]d@<�@�@�@�F@n/@;d@�@
�2@
��@
u%@
&�@
�@
@	�@	�T@	��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A�@A�@�A�@�A�A A�A�A�B�A�C�A�DgA�C�A�CaA�8�A��	AѪeAј�Aя\A�49A��KA�/�A�	lA��gA�C�A���A��rA�5�A�ܒA�\A�jKA�dZA��;AǨ$A�:�A�qA�)�A�&LA�-�A���A��A�[�A���A�$A���A�Q�A�|�A��A��A���A���A�IA�h�A��!A��A�B[A� 4A��:A��4A�	�A�fA���A�4A��qA���A�K^A�5�A~�A}a|Ay{JAv�[Au�Aq�An��Aj�AcN<A^��A\qAY�qAX��AW�#AU�AS!�AR �AQ2�AO5?AM~(AJ=AH�AG��AEm]A?�A=�wA=��A>�A@OvA@:�A?$tA=�]A:��A8�@A5rGA3�A5��A7	�A5��A1��A.qvA,�5A) \A$A iAߤA��A�MA�jAVmA$�A7LA+A	A	lA�AĜA	AS�A1�A�hA�OA�+A�A��AS�A��A�RA�A�RA��Am]AZA@A�/Ag8A6�A��A@OAQ�A?�A�KA��A�AȴA��A��AN�A��A�.A($A�!A�A%FA��ADgA�mAqA�hAXyAZAZ�ATaA�AJ�A��AuAhsAA A%�A!-A
�A	-�A�oA�vA��A�sA��AD�A��A6�A�A�A��A��Aw�AiDA&A��AH�A �}A @���@��@��O@�!�@���@��d@�s@��@��@��P@���@�u%@�S�@�F�@�'R@���@�-@� �@��@�@�U�@���@���@�x@��@�e@�}V@�-@�@�_�@��H@�s@��@��B@��@띲@�8�@�T�@�z@�m�@�)�@�ϫ@��g@��@��;@睲@�l�@�Y@�~�@��g@��y@�#:@�b�@�m�@ዬ@�8@�R@�L0@߫�@��)@޵�@޽<@޾�@�{@��d@ݎ"@�G�@��2@��@�\)@ڵ�@�~(@�M@��+@�u�@��f@�{�@�@׺^@��@��Z@Տ�@�t�@�B�@��E@�}V@�4n@�n/@�o@ң@ѡ�@�C�@�>B@ϗ�@ρ@Α @�]d@͵t@�@̱�@�*�@�_@�&@���@�X�@��s@ǅ@�8�@���@�N�@ŋ�@���@ķ�@ċD@�C�@��@� �@×$@�7L@»�@�9X@�@��@�(@�͟@�q�@�'R@��@��M@�@@��@�H�@�x@���@���@���@�N�@��@���@�ϫ@���@��=@�Y�@���@�V@��&@���@��6@���@���@�E9@��@�y>@�@���@��@�<6@��@�z�@�
�@��6@��'@�v`@�@O@��@�c @��@��V@�Vm@�*0@��h@���@�R�@�3�@��@�J�@���@�m�@��@�ƨ@��@���@�:�@���@�H�@�,�@�@���@�C-@��@��=@�\�@�(�@�֡@���@�?@��z@�zx@�4@��@���@�_@�@���@�`B@���@��.@���@�=�@��8@�@���@��@�ߤ@���@�YK@�:*@��@��#@��@�ߤ@�;�@��@��V@�_p@�҉@�q@��@��}@�}�@�A @��@��b@�;�@��@��;@��w@��@���@���@��1@��@���@���@�ԕ@�n/@�{�@�{@�ԕ@�l�@�;d@��@���@�D�@��@���@��"@�v`@�T�@�:�@�͟@�w�@�?@�@��Q@���@�9�@���@��@�^5@���@�F@��@��X@���@�?@�ϫ@���@�|@�c�@��@��e@�I�@�-�@�$@�@���@��w@���@�33@��B@��<@���@�W�@�u@���@�S�@��@��2@��m@���@��@��.@�[�@��+@��a@��	@�rG@�O@�=�@��v@��I@�H@��@�� @���@���@�s�@�Z�@�/@�C@�Y@�V@��m@�Z�@�"h@��@��d@�rG@�@@��v@��'@��b@���@���@�X�@���@���@���@��@��Y@�l�@�O@�@���@�9�@��f@���@��F@�C-@�@�k@~��@~u@}�C@}��@}a�@}q@|��@|�@{�*@{\)@{�@zv�@ys�@x�P@x��@w�@@wiD@w$t@v�h@v=q@u�@up�@uq@t�@t�u@t7@s�m@s\)@s@r+k@qp�@qL�@q5�@q/@q@@p�5@p$@o�F@n�2@nxl@nTa@m�@m	l@l]d@l�@k�w@k�:@kg�@k�@j�+@i�@h�`@hg8@h7�@h"h@g��@g�{@go@f��@fL0@f@e�-@eS&@e�@dj@c�q@cK�@b��@bO@a�j@a��@a��@a2a@`��@`��@`e�@`G@_�@_U�@^��@^^5@]�)@]@@\�@\Ft@[�P@[
=@Z�}@Z#:@Ze@Z�@Y�D@Y�T@Y�@Y�@Y�7@YS&@Y�@X��@Xw�@W�:@W"�@V�@V�6@Vi�@V3�@V�@U�@U��@U@Tr�@S��@S�@R��@R�@Q��@Q��@QX@P��@O�&@O;d@OC@N��@N~�@M�j@M|@M0�@M;@L��@LZ@K��@K��@KX�@Ko@J�@Jff@J	@I�N@I��@I��@IF@H��@Hl"@HK^@G�A@G�@G(@Fl�@F0U@E�@E��@E��@DɆ@Dy>@D �@Cخ@C��@B�"@Bc @B6�@Bu@A��@Am]@A*0@A�@@�E@@Q�@?ƨ@?�@?��@?��@?F�@?
=@>��@>�1@>h
@>L0@=��@=@=L�@<�_@<y>@<Q�@;�;@;��@;P�@:�8@:��@:��@:M�@9�@9�"@9e,@95�@9	l@8�v@8��@8I�@7�
@7�@7x@7iD@7\)@71�@6�X@6�@6�A@60U@5�@5��@5��@5G�@5�@4��@4z�@3�[@3v`@3b�@3,�@3�@2��@2�@2�L@2��@2M�@2)�@1�@1�@1T�@0�@0��@0�9@0��@0q@0H@0@/��@/��@/~�@/RT@/9�@.�"@.�@.�@.�@.}V@.Z�@.M�@.B[@-�@-rG@-0�@-�@,�9@,��@,]d@,-�@+J#@*��@*Ta@*V@*@�@*�@)��@)}�@(��@(�[@(y>@(M@(�@'�W@'ݘ@'˒@'�*@'x@'~�@'dZ@'/�@&�M@&��@&-@&�@%�.@%ԕ@%*0@%%@$�v@$�D@$4n@#�@#�@#�F@#�@@#a@"�}@"1�@"�@!�)@!�z@!�t@!�"@!m]@!Q�@!IR@!<6@!�@ ��@ ��@ 7�@ ~@ �@��@{J@e�@Mj@!-@�y@ߤ@�s@��@{�@^5@&�@��@�o@��@�@zx@B�@&�@#�@�@�@�P@�`@�E@��@r�@ �@�m@��@��@W?@$t@�@�,@u%@Q@B[@0U@4@�Z@��@�'@�~@k�@Vm@0�@��@�	@�5@�@Ĝ@��@��@oi@A�@��@��@��@s@Mj@8@"�@��@n�@;�@0U@@��@x�@B�@#�@�@�@�@Ĝ@�@Q�@  @~�@dZ@b�@RT@F�@F�@;d@&@�@ں@q�@YK@�@_@�@��@��@��@o @<6@=�@=�@2a@(�@	l@�@I�@(�@�@� @��@�@҉@��@v�@c @W�@Q@8�@�@�@��@p�@f�@%F@�@��@ѷ@�$@�@~(@]d@<�@�@�@�F@n/@;d@�@
�2@
��@
u%@
&�@
�@
@	�@	�T@	��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B72B4�B3hB49B5B6�B4�B)DB$�B BB�B�Bs�B�_B�qB	xB	[	B	�jB	��B	��B
%�B
BB
N�B
��B
�vB
�8B
��B
��B
��B
�6B
�fB
�bB
�4B
��B
�MB
�OB
��B
ٴB
�"B
�B
��B
�sB
�tB
��B
��B
��B
�zB
z*B
k6B
XB
?�B
B
	7B	��B	�*B	׍B	�B	��B	�B	� B	�B	TaB	1vB	!�B	�B	B	�B	DB	�B�cB	�B	�B	#:B	!|B	!B	B�.B��B�B��B�)B	0�B	LB	IB	C{B	2�B	$�B	�B		B	G�B	kB	�GB	x�B	^B	Q�B	9$B	(�B	B�%B�B�mB��B�B�$B�HB��B��B��B��B��B��B�QB�3B�`B�jB��B�_BڠB��B	bB	�B	&B	aB	'�B	V�B	h�B	w�B	�fB	�JB	�VB	��B	�B	�B	��B	��B	�[B	��B	�4B	�B	��B	�B	�YB	�_B	�B	ɆB	��B	�
B	ӏB	�4B	�HB	�:B	ѷB	уB	ѝB	ѷB	�[B	�8B	�FB	��B	�yB	�2B	�tB	�B	�B	�B	�}B	��B	ЗB	ΊB	�B	��B	�?B	�B	�mB	ƨB	�zB	��B	� B	�2B	�{B	�FB	�9B	�oB	��B	�&B	��B	רB	�QB	یB	�WB	�5B	ߊB	��B	ߊB	�jB	��B	ݲB	�B	�pB	��B	ߊB	��B	��B	�pB	�VB	�VB	ߊB	ߊB	�B	�5B	��B	�B	�B	��B	�B	��B	��B	�B	��B	�@B	�TB	�B	�-B	��B	��B	��B	��B	��B	��B	�,B	�B	��B	��B	�B	�_B	�B	��B	�XB	�B	�B	�kB	�B	�B	�_B	��B	�0B	�0B	�KB	�B	�WB	�WB	�=B	�WB	�B	�)B	�B	��B	�B	��B	�B	�B	�}B	��B	�)B	�WB	�6B	��B	�B	�B	�B	�KB	�QB	�B	�CB	��B	�eB	�B	�fB	�fB	��B	��B	�B	�B	�B	�WB	��B	�B	�B	��B	�)B	��B	�B	�>B	�B	�B	�
B	�*B	��B	�B	�qB	�iB	�iB	�OB	�B	�}B	�]B	�B	�IB	�B	�B	�B	�IB	�B	��B	��B	�GB	��B	��B	��B	��B	��B	��B	�^B	��B	��B	��B	�JB	��B	�"B	�<B	�<B	�"B	�B	�"B	�<B	�VB	��B	�wB	�B	�HB	�B	��B	�cB	��B	��B	��B	��B	�}B	��B
 B
 �B	��B
  B
 iB
 B
 �B
 iB
 �B
 OB
 �B
 B
B
�B
�B
�B
�B
�B
{B
�B
�B
-B
B
B
GB
B
�B
�B
-B
{B
GB
-B
'B
�B
UB
oB
 �B
 �B
oB
�B
�B
'B
;B
 �B	��B
 OB	��B
AB
B
SB
tB
tB
+B
�B
zB
1B
�B
�B
�B
�B
KB
�B
1B
fB
	B

XB

XB
B
�B

rB

�B

�B

�B
dB

�B
^B
�B
\B
}B
oB
uB
�B
�B
�B
�B
 B
4B
TB
�B
[B
aB
2B
9B
sB
_B
QB
#B
=B
]B
]B
�B
�B
�B
�B
IB
dB
�B
�B
�B
!B
VB
�B
 �B
 �B
 �B
 \B
 �B
!-B
!�B
!HB
!HB
!�B
!HB
!�B
!�B
"hB
# B
"�B
#nB
"�B
#�B
$�B
$�B
$&B
$�B
%FB
%`B
$�B
%`B
%�B
&2B
&B
'B
&�B
'8B
'B
'�B
'B
($B
'�B
(XB
($B
(�B
)yB
)_B
)�B
*KB
)�B
)�B
+�B
,=B
,qB
,�B
,�B
-�B
-�B
.IB
-�B
,�B
.B
.�B
/iB
0;B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2|B
2-B
2aB
2�B
2�B
3B
3hB
4B
4TB
49B
4TB
4�B
4�B
5�B
5�B
5�B
5tB
6+B
6�B
7B
7B
7�B
7LB
7�B
8RB
8lB
8�B
9�B
9	B
9>B
9>B
9�B
9�B
9$B
9�B
:xB
9�B
:*B
:^B
:B
:*B
:*B
;0B
:�B
;�B
<B
;�B
;�B
<B
<�B
<jB
=qB
=<B
<�B
<�B
<�B
>BB
>�B
?HB
>�B
?.B
?.B
?cB
?}B
?�B
@�B
@�B
@�B
@iB
AB
A;B
A�B
BAB
B�B
B�B
CaB
C{B
CGB
C�B
C�B
C�B
D�B
D�B
D�B
EB
EB
E9B
E�B
FYB
F�B
GB
GB
GzB
G�B
HKB
HB
HKB
H�B
H�B
HKB
H�B
H�B
IB
IRB
I7B
IRB
I�B
J	B
JrB
J�B
J�B
J�B
KB
J�B
J�B
K^B
KDB
L0B
MPB
L�B
N"B
MjB
MjB
MjB
N�B
OB
N�B
N�B
O�B
OvB
PHB
P�B
PHB
P�B
P�B
Q4B
P�B
QNB
Q�B
Q�B
R B
Q�B
RoB
R�B
R�B
RoB
SB
R�B
S[B
S[B
S�B
S�B
TaB
T�B
T�B
U�B
UgB
T�B
VB
VB
VmB
V�B
V�B
WsB
WsB
W�B
W�B
WsB
X_B
X�B
XB
W�B
Y1B
YKB
YKB
YeB
X�B
Y�B
Y�B
Y�B
ZB
ZQB
ZkB
Z�B
Z7B
[#B
[�B
[WB
[=B
[WB
[�B
\xB
\xB
\�B
\�B
\�B
]B
]�B
]~B
]�B
^5B
^OB
^OB
^�B
_�B
_pB
_�B
_�B
_�B
_�B
_VB
_VB
_VB
_�B
`vB
`�B
`B
`BB
`�B
`�B
aB
b�B
bhB
a�B
bhB
b4B
a�B
a�B
a�B
a�B
b4B
bhB
c B
c B
c�B
cTB
c�B
c�B
c�B
dtB
d�B
c�B
dtB
dZB
d�B
dZB
d�B
d�B
dZB
d�B
d�B
d�B
eFB
d�B
d�B
e�B
e�B
f2B
f�B
f�B
f�B
gB
f�B
g8B
g�B
h�B
h>B
h�B
hXB
iB
iB
i�B
iDB
i�B
iyB
i�B
jKB
j0B
jKB
jeB
kB
jB
j�B
j�B
j�B
k�B
lB
k6B
k�B
k6B
lWB
l"B
l�B
lqB
lqB
l�B
mCB
l�B
mB
mwB
n�B
n/B
n�B
n�B
oiB
oiB
n�B
oOB
o5B
o5B
n�B
o�B
oOB
o�B
p�B
poB
pUB
p�B
p�B
p�B
qAB
p�B
qB
q'B
p�B
qAB
qvB
qAB
qvB
q�B
q�B
q�B
rB
r-B
r-B
r�B
r-B
rGB
r�B
r�B
r�B
r|B
sMB
sMB
s�B
s�B
s3B
s�B
t9B
tB
s�B
s�B
t�B
t9B
t�B
t�B
t�B
t�B
t�B
u?B
t�B
u%B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
u�B
vFB
v�B
v�B
wB
w2B
w�B
wLB
w�B
w�B
xlB
xlB
x�B
x�B
x�B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
{�B
{dB
z�B
{�B
{�B
{JB
{JB
{�B
{B
{�B
|6B
{�B
|�B
|jB
|�B
|�B
|�B
|�B
|�B
}qB
}<B
|�B
}<B
}"B
}VB
}�B
}�B
~BB
~(B
~�B
~�B
�B
�B
�4B
�4B
�OB
�OB
�OB
�B
� B
��B
��B
� B
��B
�UB
�oB
�;B
��B
��B
��B
��B
�B
�B
�[B
�AB
�AB
��B
��B
��B
��B
�GB
�{B
��B
��B
��B
��B
��B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B72B4�B3hB49B5B6�B4�B)DB$�B BB�B�Bs�B�_B�qB	xB	[	B	�jB	��B	��B
%�B
BB
N�B
��B
�vB
�8B
��B
��B
��B
�6B
�fB
�bB
�4B
��B
�MB
�OB
��B
ٴB
�"B
�B
��B
�sB
�tB
��B
��B
��B
�zB
z*B
k6B
XB
?�B
B
	7B	��B	�*B	׍B	�B	��B	�B	� B	�B	TaB	1vB	!�B	�B	B	�B	DB	�B�cB	�B	�B	#:B	!|B	!B	B�.B��B�B��B�)B	0�B	LB	IB	C{B	2�B	$�B	�B		B	G�B	kB	�GB	x�B	^B	Q�B	9$B	(�B	B�%B�B�mB��B�B�$B�HB��B��B��B��B��B��B�QB�3B�`B�jB��B�_BڠB��B	bB	�B	&B	aB	'�B	V�B	h�B	w�B	�fB	�JB	�VB	��B	�B	�B	��B	��B	�[B	��B	�4B	�B	��B	�B	�YB	�_B	�B	ɆB	��B	�
B	ӏB	�4B	�HB	�:B	ѷB	уB	ѝB	ѷB	�[B	�8B	�FB	��B	�yB	�2B	�tB	�B	�B	�B	�}B	��B	ЗB	ΊB	�B	��B	�?B	�B	�mB	ƨB	�zB	��B	� B	�2B	�{B	�FB	�9B	�oB	��B	�&B	��B	רB	�QB	یB	�WB	�5B	ߊB	��B	ߊB	�jB	��B	ݲB	�B	�pB	��B	ߊB	��B	��B	�pB	�VB	�VB	ߊB	ߊB	�B	�5B	��B	�B	�B	��B	�B	��B	��B	�B	��B	�@B	�TB	�B	�-B	��B	��B	��B	��B	��B	��B	�,B	�B	��B	��B	�B	�_B	�B	��B	�XB	�B	�B	�kB	�B	�B	�_B	��B	�0B	�0B	�KB	�B	�WB	�WB	�=B	�WB	�B	�)B	�B	��B	�B	��B	�B	�B	�}B	��B	�)B	�WB	�6B	��B	�B	�B	�B	�KB	�QB	�B	�CB	��B	�eB	�B	�fB	�fB	��B	��B	�B	�B	�B	�WB	��B	�B	�B	��B	�)B	��B	�B	�>B	�B	�B	�
B	�*B	��B	�B	�qB	�iB	�iB	�OB	�B	�}B	�]B	�B	�IB	�B	�B	�B	�IB	�B	��B	��B	�GB	��B	��B	��B	��B	��B	��B	�^B	��B	��B	��B	�JB	��B	�"B	�<B	�<B	�"B	�B	�"B	�<B	�VB	��B	�wB	�B	�HB	�B	��B	�cB	��B	��B	��B	��B	�}B	��B
 B
 �B	��B
  B
 iB
 B
 �B
 iB
 �B
 OB
 �B
 B
B
�B
�B
�B
�B
�B
{B
�B
�B
-B
B
B
GB
B
�B
�B
-B
{B
GB
-B
'B
�B
UB
oB
 �B
 �B
oB
�B
�B
'B
;B
 �B	��B
 OB	��B
AB
B
SB
tB
tB
+B
�B
zB
1B
�B
�B
�B
�B
KB
�B
1B
fB
	B

XB

XB
B
�B

rB

�B

�B

�B
dB

�B
^B
�B
\B
}B
oB
uB
�B
�B
�B
�B
 B
4B
TB
�B
[B
aB
2B
9B
sB
_B
QB
#B
=B
]B
]B
�B
�B
�B
�B
IB
dB
�B
�B
�B
!B
VB
�B
 �B
 �B
 �B
 \B
 �B
!-B
!�B
!HB
!HB
!�B
!HB
!�B
!�B
"hB
# B
"�B
#nB
"�B
#�B
$�B
$�B
$&B
$�B
%FB
%`B
$�B
%`B
%�B
&2B
&B
'B
&�B
'8B
'B
'�B
'B
($B
'�B
(XB
($B
(�B
)yB
)_B
)�B
*KB
)�B
)�B
+�B
,=B
,qB
,�B
,�B
-�B
-�B
.IB
-�B
,�B
.B
.�B
/iB
0;B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2|B
2-B
2aB
2�B
2�B
3B
3hB
4B
4TB
49B
4TB
4�B
4�B
5�B
5�B
5�B
5tB
6+B
6�B
7B
7B
7�B
7LB
7�B
8RB
8lB
8�B
9�B
9	B
9>B
9>B
9�B
9�B
9$B
9�B
:xB
9�B
:*B
:^B
:B
:*B
:*B
;0B
:�B
;�B
<B
;�B
;�B
<B
<�B
<jB
=qB
=<B
<�B
<�B
<�B
>BB
>�B
?HB
>�B
?.B
?.B
?cB
?}B
?�B
@�B
@�B
@�B
@iB
AB
A;B
A�B
BAB
B�B
B�B
CaB
C{B
CGB
C�B
C�B
C�B
D�B
D�B
D�B
EB
EB
E9B
E�B
FYB
F�B
GB
GB
GzB
G�B
HKB
HB
HKB
H�B
H�B
HKB
H�B
H�B
IB
IRB
I7B
IRB
I�B
J	B
JrB
J�B
J�B
J�B
KB
J�B
J�B
K^B
KDB
L0B
MPB
L�B
N"B
MjB
MjB
MjB
N�B
OB
N�B
N�B
O�B
OvB
PHB
P�B
PHB
P�B
P�B
Q4B
P�B
QNB
Q�B
Q�B
R B
Q�B
RoB
R�B
R�B
RoB
SB
R�B
S[B
S[B
S�B
S�B
TaB
T�B
T�B
U�B
UgB
T�B
VB
VB
VmB
V�B
V�B
WsB
WsB
W�B
W�B
WsB
X_B
X�B
XB
W�B
Y1B
YKB
YKB
YeB
X�B
Y�B
Y�B
Y�B
ZB
ZQB
ZkB
Z�B
Z7B
[#B
[�B
[WB
[=B
[WB
[�B
\xB
\xB
\�B
\�B
\�B
]B
]�B
]~B
]�B
^5B
^OB
^OB
^�B
_�B
_pB
_�B
_�B
_�B
_�B
_VB
_VB
_VB
_�B
`vB
`�B
`B
`BB
`�B
`�B
aB
b�B
bhB
a�B
bhB
b4B
a�B
a�B
a�B
a�B
b4B
bhB
c B
c B
c�B
cTB
c�B
c�B
c�B
dtB
d�B
c�B
dtB
dZB
d�B
dZB
d�B
d�B
dZB
d�B
d�B
d�B
eFB
d�B
d�B
e�B
e�B
f2B
f�B
f�B
f�B
gB
f�B
g8B
g�B
h�B
h>B
h�B
hXB
iB
iB
i�B
iDB
i�B
iyB
i�B
jKB
j0B
jKB
jeB
kB
jB
j�B
j�B
j�B
k�B
lB
k6B
k�B
k6B
lWB
l"B
l�B
lqB
lqB
l�B
mCB
l�B
mB
mwB
n�B
n/B
n�B
n�B
oiB
oiB
n�B
oOB
o5B
o5B
n�B
o�B
oOB
o�B
p�B
poB
pUB
p�B
p�B
p�B
qAB
p�B
qB
q'B
p�B
qAB
qvB
qAB
qvB
q�B
q�B
q�B
rB
r-B
r-B
r�B
r-B
rGB
r�B
r�B
r�B
r|B
sMB
sMB
s�B
s�B
s3B
s�B
t9B
tB
s�B
s�B
t�B
t9B
t�B
t�B
t�B
t�B
t�B
u?B
t�B
u%B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
u�B
vFB
v�B
v�B
wB
w2B
w�B
wLB
w�B
w�B
xlB
xlB
x�B
x�B
x�B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
{�B
{dB
z�B
{�B
{�B
{JB
{JB
{�B
{B
{�B
|6B
{�B
|�B
|jB
|�B
|�B
|�B
|�B
|�B
}qB
}<B
|�B
}<B
}"B
}VB
}�B
}�B
~BB
~(B
~�B
~�B
�B
�B
�4B
�4B
�OB
�OB
�OB
�B
� B
��B
��B
� B
��B
�UB
�oB
�;B
��B
��B
��B
��B
�B
�B
�[B
�AB
�AB
��B
��B
��B
��B
�GB
�{B
��B
��B
��B
��B
��B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104928  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174155  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174156  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174156                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024203  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024203  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                