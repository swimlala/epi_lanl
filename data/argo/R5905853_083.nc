CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:37:35Z creation;2022-06-04T17:37:36Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173735  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               SA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�tॼ}�1   @�t�-!�@.��;dZ�cҏ\(��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�ff@���A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bę�B���B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���C  C�fC�fC�fC
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CFL�CH  CI�fCL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct33Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@�z�@��@�{A
=A?
=A]p�A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB�{B�G�B�G�B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�z�BǮB��HB��HBӮB��HB��HB��HB��HB�B��HB��HB��HB��HB��HB�z�C�C�
C�
C�
C	�C�C�C�C�C�C
>C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CF=qCG�CI�
CK�CM�CO�CQ�CS�CU�CW�
CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cr
>Ct#�Cu�Cw�Cy�C{�C}�
C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C�C��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�:�D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�wfA�s�A�p�A�|�A΂A�.A�|A΂uAΌ~AΎ�AΏ�Aΐ�AΎ�AΎ�AΑhAΒ�AΕAΗYAΗ�AΔFAΖ�AΖ�AΘ�AΛ=AΜCAΛ�AΟ!A΍�A΍PA·_AΌ�AΓ@A΀ A�|�A�x�A�oiA�V�A�H�A�уA��A�e`A�4�A�GA���A�\]A�/�A�چAčA�:^A���A���A�t�A�bA�A�zA�qvA�A��3A���A�v�A�:*A�e,A��{A���A�1A�w�A�c�A��iA��A��WA�4�A�4nA�A��A���A�.A��"A�MA�7LA�xA�/�A��lA���A�bA��$A��A�wfA��A���A�^jA���A��?A��{A�bA�خA�uZA��+A���A�uA�l�A���A�}"A	A{zAw�sAt��Ap��Am�$Ae�Aas�A^��A]�A[Y�AWbARg8AQ�6AO�AL�BAH�AF��AD�>ACl�AA�AAH�AA4A@�A=i�A;�A;dZA:�A9��A9�A7��A6�A4A2�A.��A+VmA,�KA*g8A+�A+�A,n/A*-A%��A$X�A#}VA#"hA!ĜA!�PA DgA�A�nA�VAk�A�kA%�A��Ao A`BA<�A\�A&�A�A33A>BA��A�:A�oA�>A&AbNA \A�FAy>A~A��An/A&�A��Aw2A�/A�DA7AZ�A�A�-A�IA0�A�yA��A33A��A��A�YAɆA�ZA�A]dA�A"hA�A?�A
�A
�rA
ffA
:�A	�+A	��A	�RA	�)A	�CA	TaA�At�A<6A�A�A�]A)_A�A�>A��A��Ac�A��A=A��AjA�gA@�A�AU�A ��@��"@�ی@��@�x@���@���@��@�	l@�!@�G@��@�H�@��@�7L@@��>@�_p@쩓@���@�Q�@���@�p;@�f@��@�ѷ@迱@�@腈@�E�@�iD@�.@���@�9�@�@�I�@��D@���@��@�c�@�d�@��N@�	@�iD@�A�@���@�  @�ϫ@߹�@ߨX@ߐ�@��@���@�~�@��@ܯO@�s�@܃�@�Y@ڕ�@�L0@��@��3@�Mj@��@���@ֲ�@�S&@�ی@�w�@�1@�8�@�M@�˒@Ӥ@@Ӏ4@�X�@��@Ң4@���@н<@��W@�o@�V�@���@�@̕@�Xy@��@˨X@�m]@˚k@˪�@˲-@��@�ƨ@ˏ�@�-�@�ƨ@Ƥ�@ƃ@�  @Ķ�@�� @�iD@©�@�N�@��@��	@��@�H�@��@��j@���@�J#@�!�@��@���@��9@��F@�l"@�J�@�~@��@�W?@��@�&�@�X@��h@�5?@��f@�S�@��E@�]d@���@�e�@�C@��@���@��@�v`@�4@��@�@�@���@�5�@���@�J�@�b@��>@��9@�ƨ@�G�@��@���@�V@�.�@��A@�T�@��@�\�@�:�@�#:@�t�@���@���@�RT@�ߤ@�ff@���@���@���@�=@��	@���@�J�@�u@��h@�0�@��@���@��@��X@���@�:�@� �@��[@�9�@��v@�@��@��@�Y@��@�A�@��@�ں@�?@���@���@�J�@�4�@�	l@��B@�v�@�2�@�4@��d@�+@�ߤ@��p@���@�J�@��]@��h@�1�@��"@��@��P@��$@�q�@�($@��W@���@�,�@��y@�;@�c @�@���@�s�@�O�@�&@��	@���@�y>@�O@�u@��@��}@��*@�y�@�W?@�P�@��@���@�!@��m@��a@��@���@���@��7@�qv@��@�Xy@�-�@���@��H@�IR@��y@�Ĝ@��9@�j@�b@��j@���@�T�@��@���@�B[@��@��@�H�@�Ĝ@���@�^5@��@��T@���@�~�@�L�@��m@��6@���@�m�@�-�@�� @��@��w@�|�@�;@�:*@�P�@�%F@�n/@�K�@�)_@���@���@�e�@��o@���@��	@�x�@�a@�E9@�@��8@��E@��O@���@�?@�$�@��@���@�s@�5�@��5@���@�*�@��@���@�B�@��@��}@�Xy@�6@�1�@��@�@g�@~��@~-@}T�@|֡@|��@|e�@{ݘ@{Y@zv�@zOv@z	@y�@yJ�@x��@x-�@x�@w�{@v�c@vW�@vc @vu@u�@uw2@u�@t?�@s�@s��@s1�@r��@rR�@r{@q�D@q�z@qc@p�5@p��@p1@o|�@o�@n�h@nE�@m��@m�@m�=@m�@mY�@l�j@l�@k�@k�	@k�@j�@j��@jYK@i��@i��@is�@i%F@h��@h?�@h7@g��@g�;@g>�@f�x@f$�@e��@d�v@dU2@d4n@c�]@c��@cU�@cO@b�@bV@a��@`�e@`��@`��@`��@`�e@``�@_�@_�{@_@O@_�@^��@^!�@]��@]�n@]-w@\PH@[��@[X�@[S@Z�@Z
�@Yo @Y!�@Xی@X9X@X�@W�@W��@W{J@WC@V�@V�@V�+@V��@V?@U�@UDg@U \@T��@T�@TFt@S��@SJ#@S�@R��@Rq�@R!�@Q�9@Q�"@Q%F@P�e@P2�@O��@N�@N��@N��@Nh
@N+k@M�@M��@Mm]@M�@M;@L��@Ly>@L'R@K��@K{J@K33@J͟@JM�@I�o@I7L@IV@H��@HM@G�@Gs@G8@G@F��@F��@F^5@FO@E�@E�@Ex�@E@DQ�@C�]@C��@C�@B��@B^5@B �@Am]@AA @A�@@ی@@j@@M@?˒@>�@>�@>v�@>^5@>�@=��@=S&@<�[@<��@<[�@;�@;��@;��@;qv@;�@:�\@:	@9�'@9p�@9e,@9T�@9@@8��@8u�@8Q�@7�W@7X�@7�@6xl@5�@5�'@5G�@4��@4��@4Xy@3��@3��@3l�@31�@2�H@2�!@2�A@2{�@2Ov@1�@1x�@1B�@1�@0�9@0Z@/�r@/Z�@.�]@.�+@.kQ@.O@-��@-a�@,��@,�9@,S�@,�@+��@+�$@+RT@+S@*u%@*0U@)ԕ@)��@)Vm@(��@(��@(_@(K^@(1@'�g@'�	@')_@&��@&~�@&\�@&O@%�Z@%��@%��@%j@%B�@%�@%%@$��@$�@$*�@$�@#�@#��@#|�@#e�@#\)@#F�@#9�@#�@"�8@"�H@"~�@"J�@"�@!��@!�@!��@!N<@!+�@ ��@ �@ �j@ ��@ ��@ bN@ 7@   @�K@�V@�@C�@�@�@ں@��@E�@�@�H@��@4@�@��@��@|�@�@m�@bN@1'@@�A@ݘ@˒@��@s@U�@"�@�]@l�@8�@�@��@��@�j@��@��@IR@�@�|@��@��@tT@�@�m@�K@�q@��@��@t�@W?@,�@�@��@u%@-@��@s�@4@@@��@�_@�_@�D@w�@9X@�@�@�A@��@.I@�@�s@��@_�@5?@��@�@B�@@��@�u@Q�@	�@�W@� @��@�f@iD@\)@/�@�@;�@J@�@�"@��@\�@@�)@�@��@l"@/�@�K@�*@��@�@o�@9�@
�y@
��@
��@
��@
�@
R�@
_@	�o@	�@	�j@	��@	<6@	&�@	@@֡@�O@w�@I�@*�@M@��@��@RT@�@��@�1@��@p;@W�@6�@4@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�wfA�s�A�p�A�|�A΂A�.A�|A΂uAΌ~AΎ�AΏ�Aΐ�AΎ�AΎ�AΑhAΒ�AΕAΗYAΗ�AΔFAΖ�AΖ�AΘ�AΛ=AΜCAΛ�AΟ!A΍�A΍PA·_AΌ�AΓ@A΀ A�|�A�x�A�oiA�V�A�H�A�уA��A�e`A�4�A�GA���A�\]A�/�A�چAčA�:^A���A���A�t�A�bA�A�zA�qvA�A��3A���A�v�A�:*A�e,A��{A���A�1A�w�A�c�A��iA��A��WA�4�A�4nA�A��A���A�.A��"A�MA�7LA�xA�/�A��lA���A�bA��$A��A�wfA��A���A�^jA���A��?A��{A�bA�خA�uZA��+A���A�uA�l�A���A�}"A	A{zAw�sAt��Ap��Am�$Ae�Aas�A^��A]�A[Y�AWbARg8AQ�6AO�AL�BAH�AF��AD�>ACl�AA�AAH�AA4A@�A=i�A;�A;dZA:�A9��A9�A7��A6�A4A2�A.��A+VmA,�KA*g8A+�A+�A,n/A*-A%��A$X�A#}VA#"hA!ĜA!�PA DgA�A�nA�VAk�A�kA%�A��Ao A`BA<�A\�A&�A�A33A>BA��A�:A�oA�>A&AbNA \A�FAy>A~A��An/A&�A��Aw2A�/A�DA7AZ�A�A�-A�IA0�A�yA��A33A��A��A�YAɆA�ZA�A]dA�A"hA�A?�A
�A
�rA
ffA
:�A	�+A	��A	�RA	�)A	�CA	TaA�At�A<6A�A�A�]A)_A�A�>A��A��Ac�A��A=A��AjA�gA@�A�AU�A ��@��"@�ی@��@�x@���@���@��@�	l@�!@�G@��@�H�@��@�7L@@��>@�_p@쩓@���@�Q�@���@�p;@�f@��@�ѷ@迱@�@腈@�E�@�iD@�.@���@�9�@�@�I�@��D@���@��@�c�@�d�@��N@�	@�iD@�A�@���@�  @�ϫ@߹�@ߨX@ߐ�@��@���@�~�@��@ܯO@�s�@܃�@�Y@ڕ�@�L0@��@��3@�Mj@��@���@ֲ�@�S&@�ی@�w�@�1@�8�@�M@�˒@Ӥ@@Ӏ4@�X�@��@Ң4@���@н<@��W@�o@�V�@���@�@̕@�Xy@��@˨X@�m]@˚k@˪�@˲-@��@�ƨ@ˏ�@�-�@�ƨ@Ƥ�@ƃ@�  @Ķ�@�� @�iD@©�@�N�@��@��	@��@�H�@��@��j@���@�J#@�!�@��@���@��9@��F@�l"@�J�@�~@��@�W?@��@�&�@�X@��h@�5?@��f@�S�@��E@�]d@���@�e�@�C@��@���@��@�v`@�4@��@�@�@���@�5�@���@�J�@�b@��>@��9@�ƨ@�G�@��@���@�V@�.�@��A@�T�@��@�\�@�:�@�#:@�t�@���@���@�RT@�ߤ@�ff@���@���@���@�=@��	@���@�J�@�u@��h@�0�@��@���@��@��X@���@�:�@� �@��[@�9�@��v@�@��@��@�Y@��@�A�@��@�ں@�?@���@���@�J�@�4�@�	l@��B@�v�@�2�@�4@��d@�+@�ߤ@��p@���@�J�@��]@��h@�1�@��"@��@��P@��$@�q�@�($@��W@���@�,�@��y@�;@�c @�@���@�s�@�O�@�&@��	@���@�y>@�O@�u@��@��}@��*@�y�@�W?@�P�@��@���@�!@��m@��a@��@���@���@��7@�qv@��@�Xy@�-�@���@��H@�IR@��y@�Ĝ@��9@�j@�b@��j@���@�T�@��@���@�B[@��@��@�H�@�Ĝ@���@�^5@��@��T@���@�~�@�L�@��m@��6@���@�m�@�-�@�� @��@��w@�|�@�;@�:*@�P�@�%F@�n/@�K�@�)_@���@���@�e�@��o@���@��	@�x�@�a@�E9@�@��8@��E@��O@���@�?@�$�@��@���@�s@�5�@��5@���@�*�@��@���@�B�@��@��}@�Xy@�6@�1�@��@�@g�@~��@~-@}T�@|֡@|��@|e�@{ݘ@{Y@zv�@zOv@z	@y�@yJ�@x��@x-�@x�@w�{@v�c@vW�@vc @vu@u�@uw2@u�@t?�@s�@s��@s1�@r��@rR�@r{@q�D@q�z@qc@p�5@p��@p1@o|�@o�@n�h@nE�@m��@m�@m�=@m�@mY�@l�j@l�@k�@k�	@k�@j�@j��@jYK@i��@i��@is�@i%F@h��@h?�@h7@g��@g�;@g>�@f�x@f$�@e��@d�v@dU2@d4n@c�]@c��@cU�@cO@b�@bV@a��@`�e@`��@`��@`��@`�e@``�@_�@_�{@_@O@_�@^��@^!�@]��@]�n@]-w@\PH@[��@[X�@[S@Z�@Z
�@Yo @Y!�@Xی@X9X@X�@W�@W��@W{J@WC@V�@V�@V�+@V��@V?@U�@UDg@U \@T��@T�@TFt@S��@SJ#@S�@R��@Rq�@R!�@Q�9@Q�"@Q%F@P�e@P2�@O��@N�@N��@N��@Nh
@N+k@M�@M��@Mm]@M�@M;@L��@Ly>@L'R@K��@K{J@K33@J͟@JM�@I�o@I7L@IV@H��@HM@G�@Gs@G8@G@F��@F��@F^5@FO@E�@E�@Ex�@E@DQ�@C�]@C��@C�@B��@B^5@B �@Am]@AA @A�@@ی@@j@@M@?˒@>�@>�@>v�@>^5@>�@=��@=S&@<�[@<��@<[�@;�@;��@;��@;qv@;�@:�\@:	@9�'@9p�@9e,@9T�@9@@8��@8u�@8Q�@7�W@7X�@7�@6xl@5�@5�'@5G�@4��@4��@4Xy@3��@3��@3l�@31�@2�H@2�!@2�A@2{�@2Ov@1�@1x�@1B�@1�@0�9@0Z@/�r@/Z�@.�]@.�+@.kQ@.O@-��@-a�@,��@,�9@,S�@,�@+��@+�$@+RT@+S@*u%@*0U@)ԕ@)��@)Vm@(��@(��@(_@(K^@(1@'�g@'�	@')_@&��@&~�@&\�@&O@%�Z@%��@%��@%j@%B�@%�@%%@$��@$�@$*�@$�@#�@#��@#|�@#e�@#\)@#F�@#9�@#�@"�8@"�H@"~�@"J�@"�@!��@!�@!��@!N<@!+�@ ��@ �@ �j@ ��@ ��@ bN@ 7@   @�K@�V@�@C�@�@�@ں@��@E�@�@�H@��@4@�@��@��@|�@�@m�@bN@1'@@�A@ݘ@˒@��@s@U�@"�@�]@l�@8�@�@��@��@�j@��@��@IR@�@�|@��@��@tT@�@�m@�K@�q@��@��@t�@W?@,�@�@��@u%@-@��@s�@4@@@��@�_@�_@�D@w�@9X@�@�@�A@��@.I@�@�s@��@_�@5?@��@�@B�@@��@�u@Q�@	�@�W@� @��@�f@iD@\)@/�@�@;�@J@�@�"@��@\�@@�)@�@��@l"@/�@�K@�*@��@�@o�@9�@
�y@
��@
��@
��@
�@
R�@
_@	�o@	�@	�j@	��@	<6@	&�@	@@֡@�O@w�@I�@*�@M@��@��@RT@�@��@�1@��@p;@W�@6�@4@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	бB	� B	�B	бB	�}B	�}B	бB	ЗB	бB	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	�}B	ЗB	ЗB	��B	� B	��B	�4B	��B	ѷB	��B	� B	ѝB	ѝB	�oB	�TB	ңB	ҽB	��B	�uB
>wB
bB
l�B
tTB
~�B
��B
�,B
�]B
��B
�iB
�B
�B
�B
��B
ƨB
��B
�mB
��B
�B
�%B �B:�B_�Bq�Bv�By�B|�B��B��B��B�fB�dB��B��BYB	BVBhB�B�B�B �B �B!-B�B��B�RB�B�BܒB�[B��B�BXyB+kBzB
�`B
՛B
��B
ĶB
�tB
��B
raB
GB
�B	�B	��B	�FB	�pB	��B	h�B	Q B	E�B	:�B	0;B	# B	1B	�B��B��B�|B��B�-B�hB��B��B��B�nB��B��B��B�}B	�B	-�B	O�B	]�B	[�B	L�B	:B	"�B	W�B	Y�B	�7B	�/B	�zB	�B	��B	��B	��B	�qB	��B	��B	��B	�HB	��B	��B	�B	�B	��B	ªB	�aB	�+B	�#B	��B	�BB	��B	��B	�B	�B	��B	��B
uB
UB	�	B	��B	��B	��B
�B
B
�B
�B
�B
?B
�B
YB
�B
�B
uB
�B
oB
uB
UB
B
 iB	�HB	��B	��B	�]B	��B	�$B	��B	��B	��B	��B	�B	�3B	�ZB	�lB	��B	�6B	��B
 �B
B
B
�B
�B
oB	��B	�HB	�}B	�}B	�DB	�fB	��B	�B	�-B	�9B	�B	��B	�UB	�cB	��B	�B	�B	�B	��B	�B	��B	��B	��B	ԯB	�B	ӏB	��B	��B	�=B	�zB	ƎB	�B	ǔB	ǮB	ǔB	ǔB	ȀB	ȚB	��B	�B	ɠB	ʦB	��B	�B	��B	��B	��B	��B	��B	��B	̳B	�jB	�PB	οB	�pB	�pB	�VB	�<B	�}B	бB	бB	ЗB	бB	ѝB	уB	�NB	�4B	�hB	� B	ѝB	�B	��B	�MB	յB	�B	��B	�]B	ݲB	��B	��B	�BB	�HB	�TB	�B	�B	��B	�B	�ZB	�`B	�$B	��B	�OB	��B	�AB	�B	�UB	��B	�5B	�/B	��B	�B	�kB	�yB	�sB	�mB	�B	��B	�DB	�B	�/B	�B	�GB	�B	��B	��B	�^B	��B	�!B	��B	�5B	��B	��B	�=B	�B	�B	��B	��B	�CB	�'B	��B	�MB	�B	��B	��B	��B	�XB	��B	��B	�0B	��B	��B	�^B	��B	�0B	�^B	��B	�2B	�B	��B	��B	� B	�B	��B	��B	�=B	�qB	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	��B	�B	�CB	�B	�[B	�B	�oB	��B	�;B	��B	�B	�B	�B	�B	�[B	��B	�aB	��B	�B	�B	�B	��B	��B	�zB	��B	��B	�B	�	B	��B	�B	��B	�<B	�]B	�wB	�cB	��B
 4B
 OB
 �B
oB
UB
'B
 B
uB
%B
B
?B
YB
�B
EB
zB
_B
�B
EB
�B
1B
	lB
	�B

�B

rB

XB

�B

�B

�B

�B

rB

�B
�B
B
B
jB
vB
�B
�B
�B
.B
�B
�B
�B
2B
B
gB
SB
�B
?B
�B
B
eB
B
kB
�B
�B
�B
�B
B
kB
	B
WB
qB
#B
�B
=B
=B
�B
B
=B
=B
�B
B
/B
dB
dB
IB
B
OB
�B
;B
�B
;B
pB
�B
VB
!bB
!�B
!�B
!bB
!�B
"�B
#B
#�B
$B
#�B
%,B
$�B
$�B
%FB
%�B
%zB
$�B
$�B
$�B
#�B
#�B
"B
"�B
&�B
(XB
)_B
)*B
)�B
*B
*�B
*eB
*KB
*B
*B
*KB
*�B
*�B
*eB
*�B
*B
+B
*�B
)�B
+QB
+�B
+QB
+6B
+�B
+QB
+�B
,"B
,�B
+�B
*�B
)�B
*B
)�B
)�B
)�B
*�B
+B
*�B
+6B
-�B
.B
.�B
.IB
./B
/iB
.�B
/B
/�B
1[B
4B
5?B
5�B
5�B
5?B
5B
5tB
6�B
6�B
6�B
7B
7B
72B
72B
7�B
7�B
8B
88B
8�B
9$B
9�B
:*B
9�B
:xB
:�B
;JB
;�B
<B
<B
<jB
<�B
<B
<6B
=VB
=�B
=�B
=�B
>wB
>]B
>wB
>wB
?B
?}B
?cB
>�B
?cB
@ B
?�B
?}B
>�B
?�B
?�B
?.B
>�B
?�B
@�B
@�B
@�B
A;B
A;B
@�B
@�B
@�B
@�B
@�B
@�B
AoB
BuB
A�B
A�B
B'B
D�B
EmB
E�B
EmB
E�B
FYB
G+B
G�B
G�B
G�B
H�B
G�B
H�B
HfB
H1B
H�B
H�B
H�B
IB
IB
I�B
J�B
J#B
J�B
J=B
J�B
KDB
K�B
K)B
KDB
KxB
LdB
LdB
K�B
L~B
L�B
MB
M�B
MPB
M6B
MB
MjB
M�B
M�B
NpB
N�B
OB
O�B
O�B
P�B
P�B
P}B
P�B
QB
QNB
QNB
RB
Q�B
RB
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
UB
T�B
T�B
U2B
UMB
U�B
U�B
VB
VB
V9B
V�B
V9B
V�B
V�B
W�B
WYB
W�B
W�B
XB
X�B
YB
YeB
X�B
X�B
Y1B
YB
YB
YKB
ZkB
Z�B
Z�B
Z�B
[#B
[#B
[#B
[WB
[�B
[�B
]B
\�B
\xB
\�B
\�B
\�B
]�B
^OB
^jB
^�B
^�B
^B
^B
^�B
^�B
^�B
^�B
_pB
_�B
`'B
`'B
`BB
aB
`BB
`�B
a�B
abB
a�B
a|B
bhB
a�B
b�B
bB
b4B
b�B
cTB
b�B
b�B
c�B
cTB
c�B
d�B
e`B
e`B
eFB
e`B
e�B
e�B
fLB
f�B
gB
gB
gB
gmB
g�B
g�B
h$B
h�B
h�B
h�B
i*B
i_B
i�B
jB
i�B
jeB
jB
jKB
j�B
kB
k6B
kB
k�B
lB
k�B
k�B
k�B
lB
lWB
lB
l"B
m]B
mB
m)B
mCB
m�B
nIB
m�B
m�B
nB
m�B
m�B
n}B
n/B
o B
o5B
oOB
o5B
n}B
o B
o�B
oOB
o�B
o5B
o�B
pB
oOB
pB
p�B
o�B
pUB
p�B
poB
p�B
qvB
p�B
q�B
qvB
q�B
rB
q�B
q�B
r|B
r�B
r�B
r�B
sMB
r�B
sMB
s3B
shB
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tnB
t�B
t�B
u%B
uZB
uZB
u�B
uZB
u�B
u�B
u�B
u�B
u�B
v+B
utB
u�B
u�B
u�B
v+B
u�B
vB
vFB
vFB
v�B
v�B
v�B
wB
w2B
wfB
wfB
xB
xB
xlB
x�B
y	B
y>B
yXB
yXB
zB
zB
y�B
zB
z^B
z�B
z�B
z�B
{B
{B
{�B
{�B
{�B
|jB
|�B
|�B
}"B
}<B
}�B
}�B
}�B
}�B
~B
}�B
~B
~(B
~BB
~�B
.B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
� B
�4B
�iB
��B
��B
��B
��B
� B
�;B
�UB
�;B
�;B
��B
��B
��B
��B
��B
�[B
��B
��B
��B
��B
�-B
�aB
�{B
��B
��B
��B
�B
��B
��B
��B
�9B
�B
�9B
�9B
�mB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	бB	� B	�B	бB	�}B	�}B	бB	ЗB	бB	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	�}B	ЗB	ЗB	��B	� B	��B	�4B	��B	ѷB	��B	� B	ѝB	ѝB	�oB	�TB	ңB	ҽB	��B	�uB
>wB
bB
l�B
tTB
~�B
��B
�,B
�]B
��B
�iB
�B
�B
�B
��B
ƨB
��B
�mB
��B
�B
�%B �B:�B_�Bq�Bv�By�B|�B��B��B��B�fB�dB��B��BYB	BVBhB�B�B�B �B �B!-B�B��B�RB�B�BܒB�[B��B�BXyB+kBzB
�`B
՛B
��B
ĶB
�tB
��B
raB
GB
�B	�B	��B	�FB	�pB	��B	h�B	Q B	E�B	:�B	0;B	# B	1B	�B��B��B�|B��B�-B�hB��B��B��B�nB��B��B��B�}B	�B	-�B	O�B	]�B	[�B	L�B	:B	"�B	W�B	Y�B	�7B	�/B	�zB	�B	��B	��B	��B	�qB	��B	��B	��B	�HB	��B	��B	�B	�B	��B	ªB	�aB	�+B	�#B	��B	�BB	��B	��B	�B	�B	��B	��B
uB
UB	�	B	��B	��B	��B
�B
B
�B
�B
�B
?B
�B
YB
�B
�B
uB
�B
oB
uB
UB
B
 iB	�HB	��B	��B	�]B	��B	�$B	��B	��B	��B	��B	�B	�3B	�ZB	�lB	��B	�6B	��B
 �B
B
B
�B
�B
oB	��B	�HB	�}B	�}B	�DB	�fB	��B	�B	�-B	�9B	�B	��B	�UB	�cB	��B	�B	�B	�B	��B	�B	��B	��B	��B	ԯB	�B	ӏB	��B	��B	�=B	�zB	ƎB	�B	ǔB	ǮB	ǔB	ǔB	ȀB	ȚB	��B	�B	ɠB	ʦB	��B	�B	��B	��B	��B	��B	��B	��B	̳B	�jB	�PB	οB	�pB	�pB	�VB	�<B	�}B	бB	бB	ЗB	бB	ѝB	уB	�NB	�4B	�hB	� B	ѝB	�B	��B	�MB	յB	�B	��B	�]B	ݲB	��B	��B	�BB	�HB	�TB	�B	�B	��B	�B	�ZB	�`B	�$B	��B	�OB	��B	�AB	�B	�UB	��B	�5B	�/B	��B	�B	�kB	�yB	�sB	�mB	�B	��B	�DB	�B	�/B	�B	�GB	�B	��B	��B	�^B	��B	�!B	��B	�5B	��B	��B	�=B	�B	�B	��B	��B	�CB	�'B	��B	�MB	�B	��B	��B	��B	�XB	��B	��B	�0B	��B	��B	�^B	��B	�0B	�^B	��B	�2B	�B	��B	��B	� B	�B	��B	��B	�=B	�qB	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	��B	�B	�CB	�B	�[B	�B	�oB	��B	�;B	��B	�B	�B	�B	�B	�[B	��B	�aB	��B	�B	�B	�B	��B	��B	�zB	��B	��B	�B	�	B	��B	�B	��B	�<B	�]B	�wB	�cB	��B
 4B
 OB
 �B
oB
UB
'B
 B
uB
%B
B
?B
YB
�B
EB
zB
_B
�B
EB
�B
1B
	lB
	�B

�B

rB

XB

�B

�B

�B

�B

rB

�B
�B
B
B
jB
vB
�B
�B
�B
.B
�B
�B
�B
2B
B
gB
SB
�B
?B
�B
B
eB
B
kB
�B
�B
�B
�B
B
kB
	B
WB
qB
#B
�B
=B
=B
�B
B
=B
=B
�B
B
/B
dB
dB
IB
B
OB
�B
;B
�B
;B
pB
�B
VB
!bB
!�B
!�B
!bB
!�B
"�B
#B
#�B
$B
#�B
%,B
$�B
$�B
%FB
%�B
%zB
$�B
$�B
$�B
#�B
#�B
"B
"�B
&�B
(XB
)_B
)*B
)�B
*B
*�B
*eB
*KB
*B
*B
*KB
*�B
*�B
*eB
*�B
*B
+B
*�B
)�B
+QB
+�B
+QB
+6B
+�B
+QB
+�B
,"B
,�B
+�B
*�B
)�B
*B
)�B
)�B
)�B
*�B
+B
*�B
+6B
-�B
.B
.�B
.IB
./B
/iB
.�B
/B
/�B
1[B
4B
5?B
5�B
5�B
5?B
5B
5tB
6�B
6�B
6�B
7B
7B
72B
72B
7�B
7�B
8B
88B
8�B
9$B
9�B
:*B
9�B
:xB
:�B
;JB
;�B
<B
<B
<jB
<�B
<B
<6B
=VB
=�B
=�B
=�B
>wB
>]B
>wB
>wB
?B
?}B
?cB
>�B
?cB
@ B
?�B
?}B
>�B
?�B
?�B
?.B
>�B
?�B
@�B
@�B
@�B
A;B
A;B
@�B
@�B
@�B
@�B
@�B
@�B
AoB
BuB
A�B
A�B
B'B
D�B
EmB
E�B
EmB
E�B
FYB
G+B
G�B
G�B
G�B
H�B
G�B
H�B
HfB
H1B
H�B
H�B
H�B
IB
IB
I�B
J�B
J#B
J�B
J=B
J�B
KDB
K�B
K)B
KDB
KxB
LdB
LdB
K�B
L~B
L�B
MB
M�B
MPB
M6B
MB
MjB
M�B
M�B
NpB
N�B
OB
O�B
O�B
P�B
P�B
P}B
P�B
QB
QNB
QNB
RB
Q�B
RB
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
UB
T�B
T�B
U2B
UMB
U�B
U�B
VB
VB
V9B
V�B
V9B
V�B
V�B
W�B
WYB
W�B
W�B
XB
X�B
YB
YeB
X�B
X�B
Y1B
YB
YB
YKB
ZkB
Z�B
Z�B
Z�B
[#B
[#B
[#B
[WB
[�B
[�B
]B
\�B
\xB
\�B
\�B
\�B
]�B
^OB
^jB
^�B
^�B
^B
^B
^�B
^�B
^�B
^�B
_pB
_�B
`'B
`'B
`BB
aB
`BB
`�B
a�B
abB
a�B
a|B
bhB
a�B
b�B
bB
b4B
b�B
cTB
b�B
b�B
c�B
cTB
c�B
d�B
e`B
e`B
eFB
e`B
e�B
e�B
fLB
f�B
gB
gB
gB
gmB
g�B
g�B
h$B
h�B
h�B
h�B
i*B
i_B
i�B
jB
i�B
jeB
jB
jKB
j�B
kB
k6B
kB
k�B
lB
k�B
k�B
k�B
lB
lWB
lB
l"B
m]B
mB
m)B
mCB
m�B
nIB
m�B
m�B
nB
m�B
m�B
n}B
n/B
o B
o5B
oOB
o5B
n}B
o B
o�B
oOB
o�B
o5B
o�B
pB
oOB
pB
p�B
o�B
pUB
p�B
poB
p�B
qvB
p�B
q�B
qvB
q�B
rB
q�B
q�B
r|B
r�B
r�B
r�B
sMB
r�B
sMB
s3B
shB
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tnB
t�B
t�B
u%B
uZB
uZB
u�B
uZB
u�B
u�B
u�B
u�B
u�B
v+B
utB
u�B
u�B
u�B
v+B
u�B
vB
vFB
vFB
v�B
v�B
v�B
wB
w2B
wfB
wfB
xB
xB
xlB
x�B
y	B
y>B
yXB
yXB
zB
zB
y�B
zB
z^B
z�B
z�B
z�B
{B
{B
{�B
{�B
{�B
|jB
|�B
|�B
}"B
}<B
}�B
}�B
}�B
}�B
~B
}�B
~B
~(B
~BB
~�B
.B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
� B
�4B
�iB
��B
��B
��B
��B
� B
�;B
�UB
�;B
�;B
��B
��B
��B
��B
��B
�[B
��B
��B
��B
��B
�-B
�aB
�{B
��B
��B
��B
�B
��B
��B
��B
�9B
�B
�9B
�9B
�mB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104918  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173735  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173736  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173736                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023744  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023744  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                