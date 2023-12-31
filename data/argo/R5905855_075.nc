CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:50Z creation;2022-06-04T19:23:50Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192350  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               KA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�a�F1   @�a�&~�/@,��
=p��c�p��
=1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  @���A   AC33A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�ffB���B�33B���B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  C   C�C�fC�fC  C
  C  C  C�fC  C  C�C33C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@|(�@�{@��HA
=AB=pA]p�A}p�A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�G�B��B�G�B��B�{BîBǮB��HB��HB��HB��HB��HB߮B��HB��HB��HB��HB��HB��B��HB��HC
>C�
C�
C�C	�C�C�C�
C�C�C
>C#�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C^
>C`
>Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL��DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}��D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D��GD��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��aA���A�� Aʿ�A�� A��aA���A���A��[A���A���A�A��UA��'A�ÖA���A��aA���A��gA��9A��tA��EA��KA�ȴA���A���A���A�˒A��0A��6A��6A��pA��vA���A��HA�ѷA���A�ԕA���A�҉A�ҽA���A��}A��)AʧRA��A���A��;A��MA���A�($A��YA��A�#�A��eA�,=A��4A�A��~A���A��`A� �A���A��dA�̘A�A���A�OBA�ߤA��A�N�A���A��MA�A|��Ay��At�HAn��Al��Ak��AkaAj��Ah�MAdu�A_e,AY~ARw�ANC�AK��AH�AF��AE��AA|�A?�{A<�A;�bA9��A8�"A8:�A6�mA4}�A3��A2oA1eA0U�A07A.�{A,�0A,*�A+`�A*e�A(�RA'�A',�A$4A!w2A��A��A	A�qA��AخA�A��A�ATaAqAbNA�Aa�A�{AA�A��A�mA�{A	A�VA�-A�!A�fA1A?�AXA��A�A��A�A��A��A��A3�A%Ac�A�A*�A�)A��A&�A�}An/Am�A�[AɆA�AiDA�)A��AA�A�A�A��A��Am]A[WA1A��At�A�A�XA^5A#�A�gA��AbNA
��A	��A	<6A�BA��A8�AL0A
=AɆAa|A��A<�AJA��A��A*0A��A�nA/A�*A��AS�A \A �)A R�A   @�o @��@�ں@��@��e@��A@���@���@�"h@��@���@��@�_p@���@��@�X�@���@��@�)_@�~(@�%�@��@��6@��@�@�@�1@�Z�@�S@��@��f@�ȴ@�/�@�e,@�X@�,�@���@���@���@�6@�Q�@��o@��@�h
@�/�@��6@�&@��@�YK@�qv@��P@��s@�7@�@��@���@�:*@��z@�	@�8�@��@��&@�j�@�<6@�}@���@��@�g8@��@�8@��]@���@�ں@�E�@ܱ�@�-�@��9@�b�@���@�=q@���@٠�@��@ظR@�\�@ׂ�@��@�"h@�|�@�/@ԧ�@�@��@�9�@��X@�l"@�7@���@ѨX@�5�@�I�@�W?@Ξ�@���@͎�@�J�@�;@̰�@�v�@˶F@�v`@�/@���@���@Ʌ�@�f�@�S&@�F@�Ɇ@�GE@��@Ǌ�@�'�@Ƴh@�$�@ł�@��	@ĭ�@�8�@�V@��@��@�a�@�@��@���@�e�@�{�@���@�e�@�G�@�1�@��v@�~(@�\�@�H�@�-�@��@�(�@�	l@�r�@��@��'@�J�@�u�@���@�]�@�(�@�!-@�q@�C@��@��D@�/�@��[@��@�u�@�خ@���@�O@���@��@��R@�w�@�1'@�	@���@�1�@�+@�	l@���@��E@���@�p;@�e@�`B@�ѷ@��<@�6@��m@�~�@��B@���@�0U@��@�~�@�=@�V@��y@�_@��@�}�@��@���@��+@�dZ@��@�j@�+k@��-@��5@�_@�	@���@��V@�G�@��@���@�M@��N@�#�@��8@��'@���@�E�@��@��#@���@�zx@�P�@�S@���@�h�@�@���@�[W@�@��`@��D@�O@���@���@�>�@��]@��e@��_@�y>@�M�@��@��@��$@�=�@��@��8@��@�ff@�u@���@�(�@���@���@�I�@��@�
�@���@��6@���@�Z�@��@��@�ߤ@���@�`�@�($@�O@�	@��)@�ƨ@���@��@�S@��j@�Q�@��@���@��@��I@�g8@���@�J�@��@��?@�_�@�1@��@��S@���@�:�@��@�҉@��_@�n�@�Ov@���@�J#@��P@���@���@���@�g8@�%�@��W@���@�hs@�C�@��@��@�Q�@�$@��@���@�^�@�"�@���@�{�@�Q@��@��&@���@�<6@��@�z�@�0U@���@��@@�b�@�6z@�!�@��@�8�@��a@�s�@�g�@��y@��@�Ov@�@��m@�zx@�� @�s�@�c�@�r@@~��@~Ta@}�@}�n@}*0@|��@|?�@|b@|x@{��@{;d@{�@z��@z�@z�'@z}V@y��@yw2@x�j@x-�@w�@w�@v�@v��@v��@v{@u��@u�S@uo @t�	@t��@s��@sU�@r�s@r_�@r�@q��@qf�@q�@p�p@pw�@pPH@px@o��@n�X@n�@nc @n0U@m��@mc�@m�@l�@k�F@kb�@kW?@k9�@k!-@j�M@j�6@i�@i�M@if�@iIR@h��@h�u@h!@g��@gRT@f�2@f{�@fOv@f;�@e��@e#�@d��@d��@d�@d|�@d�@c�0@b��@b	@a��@a�@`��@`Ft@`�@_�@_�6@_��@_'�@^�6@^R�@^�@]�Z@]�S@]-w@\�z@\!@[�@Z��@Z��@ZR�@Z)�@Z�@Y�Z@Y��@Y��@Y\�@X�p@X��@Xl"@X[�@X@Wn/@V��@Vxl@V!�@V�@U��@UQ�@T��@T�@S��@S�f@R�]@Rl�@R�@Q�)@Q��@Q��@P�v@P�@O��@O�f@O6z@N��@N��@N �@M�=@Mq@L��@L��@LbN@L<�@L,=@L@K�q@Ka@J�,@JR�@J0U@I�3@I\�@I8�@H��@H@G�	@F��@F_�@E��@EL�@D�@D�D@Dg8@DQ�@C�@C�q@C�@@C��@C>�@CS@C@B�'@Be@A��@AQ�@@ѷ@@�.@@q@@]d@@K^@@(�@?��@?��@?��@?�	@?�{@?l�@?A�@?4�@?/�@?C@>͟@>xl@>8�@=��@=�N@=��@=�"@=;@<�@<m�@<@;4�@:��@:_�@:@9��@9zx@9 \@8�f@8��@8!@7�w@7~�@79�@7Y@6�"@6�c@6��@65?@5�#@5�@5+�@4��@4�Y@4]d@3�0@3C�@2�M@2��@2l�@28�@2O@1��@1`B@12a@0��@0��@0u�@0b@/�;@/�w@/�P@/Mj@/&@.n�@._@-�T@-��@-�-@-��@,֡@,V�@,N�@,K^@,H@,:�@,1'@,�@+�W@+��@+��@+{J@+o�@+!-@*��@*M�@*#:@)�H@)�"@)��@)m]@)e,@)Dg@(�|@(��@(�O@(�@(w�@(r�@('R@'�@'�}@'�V@'{J@']�@'H�@&�@&kQ@&@�@&1�@&+k@&O@%�z@%x�@%c�@%N<@%5�@% \@$��@$�/@$�U@$��@$�I@$tT@$:�@#�m@#��@#��@#@O@"�<@"=q@!�)@!��@!@!��@!��@!L�@!@ �[@ ��@ �I@ %�@�}@�$@_p@�@�B@z@h
@a|@ �@��@m]@Vm@IR@?}@7L@-w@V@�@�@V�@��@�	@s@a@W?@8@�2@��@��@�r@v�@\�@0U@�@x�@N<@�@ی@��@�4@��@c�@,=@G@��@��@~�@_p@&@��@�\@s�@C�@��@�N@�C@��@`B@Ɇ@�o@H@7�@6@,=@!@�r@� @�@iD@.I@@�m@��@\�@�.@�3@�n@e,@<6@;@��@m�@!@�*@{J@y�@s@J#@C@�@��@ff@GE@�@��@�@��@J�@+�@�K@�4@j@c�@!@��@�K@��@��@O@)_@
�H@
͟@
�<@
�@
��@
�6@
��@
xl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��aA���A�� Aʿ�A�� A��aA���A���A��[A���A���A�A��UA��'A�ÖA���A��aA���A��gA��9A��tA��EA��KA�ȴA���A���A���A�˒A��0A��6A��6A��pA��vA���A��HA�ѷA���A�ԕA���A�҉A�ҽA���A��}A��)AʧRA��A���A��;A��MA���A�($A��YA��A�#�A��eA�,=A��4A�A��~A���A��`A� �A���A��dA�̘A�A���A�OBA�ߤA��A�N�A���A��MA�A|��Ay��At�HAn��Al��Ak��AkaAj��Ah�MAdu�A_e,AY~ARw�ANC�AK��AH�AF��AE��AA|�A?�{A<�A;�bA9��A8�"A8:�A6�mA4}�A3��A2oA1eA0U�A07A.�{A,�0A,*�A+`�A*e�A(�RA'�A',�A$4A!w2A��A��A	A�qA��AخA�A��A�ATaAqAbNA�Aa�A�{AA�A��A�mA�{A	A�VA�-A�!A�fA1A?�AXA��A�A��A�A��A��A��A3�A%Ac�A�A*�A�)A��A&�A�}An/Am�A�[AɆA�AiDA�)A��AA�A�A�A��A��Am]A[WA1A��At�A�A�XA^5A#�A�gA��AbNA
��A	��A	<6A�BA��A8�AL0A
=AɆAa|A��A<�AJA��A��A*0A��A�nA/A�*A��AS�A \A �)A R�A   @�o @��@�ں@��@��e@��A@���@���@�"h@��@���@��@�_p@���@��@�X�@���@��@�)_@�~(@�%�@��@��6@��@�@�@�1@�Z�@�S@��@��f@�ȴ@�/�@�e,@�X@�,�@���@���@���@�6@�Q�@��o@��@�h
@�/�@��6@�&@��@�YK@�qv@��P@��s@�7@�@��@���@�:*@��z@�	@�8�@��@��&@�j�@�<6@�}@���@��@�g8@��@�8@��]@���@�ں@�E�@ܱ�@�-�@��9@�b�@���@�=q@���@٠�@��@ظR@�\�@ׂ�@��@�"h@�|�@�/@ԧ�@�@��@�9�@��X@�l"@�7@���@ѨX@�5�@�I�@�W?@Ξ�@���@͎�@�J�@�;@̰�@�v�@˶F@�v`@�/@���@���@Ʌ�@�f�@�S&@�F@�Ɇ@�GE@��@Ǌ�@�'�@Ƴh@�$�@ł�@��	@ĭ�@�8�@�V@��@��@�a�@�@��@���@�e�@�{�@���@�e�@�G�@�1�@��v@�~(@�\�@�H�@�-�@��@�(�@�	l@�r�@��@��'@�J�@�u�@���@�]�@�(�@�!-@�q@�C@��@��D@�/�@��[@��@�u�@�خ@���@�O@���@��@��R@�w�@�1'@�	@���@�1�@�+@�	l@���@��E@���@�p;@�e@�`B@�ѷ@��<@�6@��m@�~�@��B@���@�0U@��@�~�@�=@�V@��y@�_@��@�}�@��@���@��+@�dZ@��@�j@�+k@��-@��5@�_@�	@���@��V@�G�@��@���@�M@��N@�#�@��8@��'@���@�E�@��@��#@���@�zx@�P�@�S@���@�h�@�@���@�[W@�@��`@��D@�O@���@���@�>�@��]@��e@��_@�y>@�M�@��@��@��$@�=�@��@��8@��@�ff@�u@���@�(�@���@���@�I�@��@�
�@���@��6@���@�Z�@��@��@�ߤ@���@�`�@�($@�O@�	@��)@�ƨ@���@��@�S@��j@�Q�@��@���@��@��I@�g8@���@�J�@��@��?@�_�@�1@��@��S@���@�:�@��@�҉@��_@�n�@�Ov@���@�J#@��P@���@���@���@�g8@�%�@��W@���@�hs@�C�@��@��@�Q�@�$@��@���@�^�@�"�@���@�{�@�Q@��@��&@���@�<6@��@�z�@�0U@���@��@@�b�@�6z@�!�@��@�8�@��a@�s�@�g�@��y@��@�Ov@�@��m@�zx@�� @�s�@�c�@�r@@~��@~Ta@}�@}�n@}*0@|��@|?�@|b@|x@{��@{;d@{�@z��@z�@z�'@z}V@y��@yw2@x�j@x-�@w�@w�@v�@v��@v��@v{@u��@u�S@uo @t�	@t��@s��@sU�@r�s@r_�@r�@q��@qf�@q�@p�p@pw�@pPH@px@o��@n�X@n�@nc @n0U@m��@mc�@m�@l�@k�F@kb�@kW?@k9�@k!-@j�M@j�6@i�@i�M@if�@iIR@h��@h�u@h!@g��@gRT@f�2@f{�@fOv@f;�@e��@e#�@d��@d��@d�@d|�@d�@c�0@b��@b	@a��@a�@`��@`Ft@`�@_�@_�6@_��@_'�@^�6@^R�@^�@]�Z@]�S@]-w@\�z@\!@[�@Z��@Z��@ZR�@Z)�@Z�@Y�Z@Y��@Y��@Y\�@X�p@X��@Xl"@X[�@X@Wn/@V��@Vxl@V!�@V�@U��@UQ�@T��@T�@S��@S�f@R�]@Rl�@R�@Q�)@Q��@Q��@P�v@P�@O��@O�f@O6z@N��@N��@N �@M�=@Mq@L��@L��@LbN@L<�@L,=@L@K�q@Ka@J�,@JR�@J0U@I�3@I\�@I8�@H��@H@G�	@F��@F_�@E��@EL�@D�@D�D@Dg8@DQ�@C�@C�q@C�@@C��@C>�@CS@C@B�'@Be@A��@AQ�@@ѷ@@�.@@q@@]d@@K^@@(�@?��@?��@?��@?�	@?�{@?l�@?A�@?4�@?/�@?C@>͟@>xl@>8�@=��@=�N@=��@=�"@=;@<�@<m�@<@;4�@:��@:_�@:@9��@9zx@9 \@8�f@8��@8!@7�w@7~�@79�@7Y@6�"@6�c@6��@65?@5�#@5�@5+�@4��@4�Y@4]d@3�0@3C�@2�M@2��@2l�@28�@2O@1��@1`B@12a@0��@0��@0u�@0b@/�;@/�w@/�P@/Mj@/&@.n�@._@-�T@-��@-�-@-��@,֡@,V�@,N�@,K^@,H@,:�@,1'@,�@+�W@+��@+��@+{J@+o�@+!-@*��@*M�@*#:@)�H@)�"@)��@)m]@)e,@)Dg@(�|@(��@(�O@(�@(w�@(r�@('R@'�@'�}@'�V@'{J@']�@'H�@&�@&kQ@&@�@&1�@&+k@&O@%�z@%x�@%c�@%N<@%5�@% \@$��@$�/@$�U@$��@$�I@$tT@$:�@#�m@#��@#��@#@O@"�<@"=q@!�)@!��@!@!��@!��@!L�@!@ �[@ ��@ �I@ %�@�}@�$@_p@�@�B@z@h
@a|@ �@��@m]@Vm@IR@?}@7L@-w@V@�@�@V�@��@�	@s@a@W?@8@�2@��@��@�r@v�@\�@0U@�@x�@N<@�@ی@��@�4@��@c�@,=@G@��@��@~�@_p@&@��@�\@s�@C�@��@�N@�C@��@`B@Ɇ@�o@H@7�@6@,=@!@�r@� @�@iD@.I@@�m@��@\�@�.@�3@�n@e,@<6@;@��@m�@!@�*@{J@y�@s@J#@C@�@��@ff@GE@�@��@�@��@J�@+�@�K@�4@j@c�@!@��@�K@��@��@O@)_@
�H@
͟@
�<@
�@
��@
�6@
��@
xl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�dB�dB�dB�dB�dB�dB�B�dB�JB�0B�JB�JB�0B�0B�0B�0B�B�B��B�0B�B�0B�0B�0B�B�B��B�B�B�B�0B��B��B�B�JB�B��B��B��B�JB�JB�B��B��B	�B	L�B	��B
S�B
�B
��B
��B
��B
��B
��B
�IB
�KB
��B
�B
�:B
�?B
��B
�lB
�SB
��B
��B
�#B
�BB
p�B
OB
<B
�B
B	�cB	�BB	�SB	�TB	��B	��B	zDB	p;B	lqB	gB	abB	T�B	EmB	*�B	B	 �B�$B�`B��B�ZB��B�hB̈́B�B��B�UB��B��B��B��B��B��B�|B�VB��B��B�zB��B�6B	<B	�B	�B	�B	9B��B�B�UB�5B�B�B�B		�B	�B		�B	�B��B��B��B�6B�"B��B		RB	*0B	Y1B	n�B	yXB	HB	��B	��B	�CB	�mB	��B	��B	�B	�%B	��B	��B	�9B	�B	�<B	�6B	̘B	�B	�B	�rB	ƎB	�BB	�}B	��B	�PB	�NB	�kB	��B	�@B	ևB	�'B	�bB	�B	��B	�hB	�B	�hB	�hB	�`B	�B	� B	�OB	��B	��B	�B	�B	��B	��B	�B	�XB	�B	�RB	��B	�B	�;B	��B	�B	�BB	��B	��B	�NB	�NB	�nB	�zB	�B	�FB	�B	�mB	�B	�B	�
B	��B	�sB	�sB	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�nB	�nB	�zB	��B	��B
�B
	lB
B
�B
�B
�B
MB	��B	�qB	�(B	��B
B
{B
B
%B
B
�B
YB
	lB

rB

#B
�B
�B
xB
)B
�B

�B

rB

�B
	�B
�B
�B
�B
1B
+B
�B
�B
�B
�B
fB
	�B
	�B
	�B

�B
�B
�B
)B

�B

rB

=B
	�B
	7B
KB
�B
YB
�B
SB
�B
�B
�B
�B
uB
[B
�B
�B
oB
oB
 �B
 �B
 OB
 �B
 �B
 �B
 OB
 �B	��B	��B
 B
 4B
 B
 B	��B	��B	�HB	�}B	�cB
 �B
 �B
 �B
 iB	��B
 OB
 �B
 4B	��B	��B
  B
B
 �B
 iB
 �B
UB
�B
B
 OB	��B	��B	��B	��B	�HB	�cB	��B	��B
�B
zB
�B
{B
�B
�B
UB
�B
 �B
 �B
 B
�B
�B
�B
�B
-B
-B
-B
GB
�B
�B
�B
�B
�B
�B
�B
�B
9B
�B
�B
%B
B
�B
_B
�B
�B
�B
B
EB
EB
�B
fB
KB
KB
�B
�B
	B
	�B
	�B
	�B
	�B
	�B

	B

#B

#B

=B

�B
B

�B
^B

�B
)B
xB
DB
�B
dB
~B
�B
B
B
6B
B
jB
�B
�B
�B
�B
BB
\B
\B
vB
.B
bB
�B
�B
�B
 B
NB
B
�B
:B
�B
�B
�B
B
[B
�B
�B
�B
�B
B
aB
�B
�B
�B
�B
mB
�B
�B
�B
�B
9B
�B
�B
�B
�B
�B
�B
�B
�B

B
YB
sB
�B
�B
�B
�B
�B
�B
sB
�B
�B
B
B
7B
QB
#B
�B
�B
=B
WB
�B
�B
�B
�B
�B
�B
CB
]B
]B
�B
/B
IB
~B
/B
/B
�B
B
5B
;B
!B
B
!B
�B
B
;B
�B
pB
�B
 B
 �B
 �B
 �B
 �B
 �B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
# B
#B
"�B
"�B
#B
# B
#TB
#:B
#TB
#�B
#�B
$&B
$tB
$�B
%FB
%�B
&�B
'�B
(XB
(sB
(�B
)�B
)�B
)yB
)�B
)yB
)�B
)�B
*B
*�B
+B
,"B
,"B
,WB
,qB
,�B
,�B
.IB
-�B
-�B
.cB
.�B
.�B
/5B
/5B
/5B
/OB
/�B
0B
0UB
0�B
1AB
1B
1'B
1[B
1[B
1�B
1�B
2aB
2GB
2aB
2�B
3B
3�B
4B
49B
49B
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
7LB
7fB
7�B
7�B
8B
8RB
8�B
8�B
8�B
9$B
9�B
9�B
9�B
:�B
:�B
:�B
:xB
:xB
:�B
;dB
<B
<�B
<jB
<6B
<PB
<6B
<�B
<�B
<�B
<�B
="B
=<B
=qB
=�B
>(B
>�B
?HB
?.B
?.B
?.B
?�B
@iB
@iB
@�B
@iB
A B
AB
A�B
B[B
B�B
CaB
D3B
D�B
E9B
E�B
E�B
F?B
F�B
G+B
GEB
GEB
G+B
G�B
H1B
H�B
IB
I�B
I�B
I�B
J#B
J#B
J#B
JXB
J=B
J=B
JrB
J�B
J�B
J�B
J�B
J�B
KxB
K�B
LB
LJB
LB
L0B
LJB
L�B
MB
MPB
MB
M�B
M�B
N"B
N"B
N<B
N"B
N�B
N�B
OB
OB
OvB
O�B
O�B
PHB
P}B
P�B
Q B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R:B
R:B
RoB
R�B
R�B
R�B
S@B
S�B
S�B
TaB
T{B
UMB
U�B
U�B
U�B
U�B
VB
V9B
VB
VB
V�B
V�B
VmB
V�B
W?B
WsB
W�B
X+B
X_B
XyB
X�B
X�B
X�B
YKB
YKB
YeB
YeB
YKB
YeB
YB
YB
YB
YeB
Y�B
ZB
ZB
ZB
ZkB
Z�B
Z�B
Z�B
[WB
[	B
[qB
\]B
\�B
\�B
]dB
]IB
]~B
]�B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_B
^�B
_;B
_VB
_�B
_�B
`'B
`BB
`\B
`\B
`�B
a-B
abB
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
b�B
cB
c B
c B
c:B
cnB
cTB
d&B
dZB
dtB
dtB
d�B
dtB
eFB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
f2B
ffB
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
g�B
h>B
hXB
hsB
hXB
h�B
hsB
h�B
iB
h�B
iDB
i_B
iyB
i_B
i�B
jeB
j�B
j�B
j�B
j�B
kB
k�B
kkB
k�B
k�B
k�B
k�B
k�B
lB
lB
lB
l"B
lqB
l�B
l�B
l�B
mB
m�B
m�B
n/B
nIB
nIB
ncB
n}B
n�B
n�B
n�B
n}B
n}B
n�B
n�B
o B
oB
o5B
o�B
pB
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q[B
q[B
qvB
q�B
r-B
rGB
rGB
raB
r|B
r�B
r�B
r�B
s3B
sB
sMB
shB
s�B
tB
t9B
t�B
t�B
t�B
t�B
t�B
uB
u%B
uZB
uZB
u�B
u�B
u�B
u�B
v+B
vzB
v�B
v�B
w2B
w2B
wLB
wfB
wLB
xB
x8B
xlB
xlB
xRB
xlB
xRB
x�B
x�B
x�B
y	B
y$B
yXB
yrB
y�B
y�B
zB
zDB
zDB
z�B
z�B
z�B
z�B
{JB
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
}"B
}<B
}<B
}�B
}�B
}�B
}�B
~BB
~BB
~�B
~�B
B
B
}B
�B
�B
�B
� B
�4B
�OB
��B
��B
��B
��B
��B
��B
��B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�dB�dB�dB�dB�dB�dB�B�dB�JB�0B�JB�JB�0B�0B�0B�0B�B�B��B�0B�B�0B�0B�0B�B�B��B�B�B�B�0B��B��B�B�JB�B��B��B��B�JB�JB�B��B��B	�B	L�B	��B
S�B
�B
��B
��B
��B
��B
��B
�IB
�KB
��B
�B
�:B
�?B
��B
�lB
�SB
��B
��B
�#B
�BB
p�B
OB
<B
�B
B	�cB	�BB	�SB	�TB	��B	��B	zDB	p;B	lqB	gB	abB	T�B	EmB	*�B	B	 �B�$B�`B��B�ZB��B�hB̈́B�B��B�UB��B��B��B��B��B��B�|B�VB��B��B�zB��B�6B	<B	�B	�B	�B	9B��B�B�UB�5B�B�B�B		�B	�B		�B	�B��B��B��B�6B�"B��B		RB	*0B	Y1B	n�B	yXB	HB	��B	��B	�CB	�mB	��B	��B	�B	�%B	��B	��B	�9B	�B	�<B	�6B	̘B	�B	�B	�rB	ƎB	�BB	�}B	��B	�PB	�NB	�kB	��B	�@B	ևB	�'B	�bB	�B	��B	�hB	�B	�hB	�hB	�`B	�B	� B	�OB	��B	��B	�B	�B	��B	��B	�B	�XB	�B	�RB	��B	�B	�;B	��B	�B	�BB	��B	��B	�NB	�NB	�nB	�zB	�B	�FB	�B	�mB	�B	�B	�
B	��B	�sB	�sB	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�nB	�nB	�zB	��B	��B
�B
	lB
B
�B
�B
�B
MB	��B	�qB	�(B	��B
B
{B
B
%B
B
�B
YB
	lB

rB

#B
�B
�B
xB
)B
�B

�B

rB

�B
	�B
�B
�B
�B
1B
+B
�B
�B
�B
�B
fB
	�B
	�B
	�B

�B
�B
�B
)B

�B

rB

=B
	�B
	7B
KB
�B
YB
�B
SB
�B
�B
�B
�B
uB
[B
�B
�B
oB
oB
 �B
 �B
 OB
 �B
 �B
 �B
 OB
 �B	��B	��B
 B
 4B
 B
 B	��B	��B	�HB	�}B	�cB
 �B
 �B
 �B
 iB	��B
 OB
 �B
 4B	��B	��B
  B
B
 �B
 iB
 �B
UB
�B
B
 OB	��B	��B	��B	��B	�HB	�cB	��B	��B
�B
zB
�B
{B
�B
�B
UB
�B
 �B
 �B
 B
�B
�B
�B
�B
-B
-B
-B
GB
�B
�B
�B
�B
�B
�B
�B
�B
9B
�B
�B
%B
B
�B
_B
�B
�B
�B
B
EB
EB
�B
fB
KB
KB
�B
�B
	B
	�B
	�B
	�B
	�B
	�B

	B

#B

#B

=B

�B
B

�B
^B

�B
)B
xB
DB
�B
dB
~B
�B
B
B
6B
B
jB
�B
�B
�B
�B
BB
\B
\B
vB
.B
bB
�B
�B
�B
 B
NB
B
�B
:B
�B
�B
�B
B
[B
�B
�B
�B
�B
B
aB
�B
�B
�B
�B
mB
�B
�B
�B
�B
9B
�B
�B
�B
�B
�B
�B
�B
�B

B
YB
sB
�B
�B
�B
�B
�B
�B
sB
�B
�B
B
B
7B
QB
#B
�B
�B
=B
WB
�B
�B
�B
�B
�B
�B
CB
]B
]B
�B
/B
IB
~B
/B
/B
�B
B
5B
;B
!B
B
!B
�B
B
;B
�B
pB
�B
 B
 �B
 �B
 �B
 �B
 �B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
# B
#B
"�B
"�B
#B
# B
#TB
#:B
#TB
#�B
#�B
$&B
$tB
$�B
%FB
%�B
&�B
'�B
(XB
(sB
(�B
)�B
)�B
)yB
)�B
)yB
)�B
)�B
*B
*�B
+B
,"B
,"B
,WB
,qB
,�B
,�B
.IB
-�B
-�B
.cB
.�B
.�B
/5B
/5B
/5B
/OB
/�B
0B
0UB
0�B
1AB
1B
1'B
1[B
1[B
1�B
1�B
2aB
2GB
2aB
2�B
3B
3�B
4B
49B
49B
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
7LB
7fB
7�B
7�B
8B
8RB
8�B
8�B
8�B
9$B
9�B
9�B
9�B
:�B
:�B
:�B
:xB
:xB
:�B
;dB
<B
<�B
<jB
<6B
<PB
<6B
<�B
<�B
<�B
<�B
="B
=<B
=qB
=�B
>(B
>�B
?HB
?.B
?.B
?.B
?�B
@iB
@iB
@�B
@iB
A B
AB
A�B
B[B
B�B
CaB
D3B
D�B
E9B
E�B
E�B
F?B
F�B
G+B
GEB
GEB
G+B
G�B
H1B
H�B
IB
I�B
I�B
I�B
J#B
J#B
J#B
JXB
J=B
J=B
JrB
J�B
J�B
J�B
J�B
J�B
KxB
K�B
LB
LJB
LB
L0B
LJB
L�B
MB
MPB
MB
M�B
M�B
N"B
N"B
N<B
N"B
N�B
N�B
OB
OB
OvB
O�B
O�B
PHB
P}B
P�B
Q B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R:B
R:B
RoB
R�B
R�B
R�B
S@B
S�B
S�B
TaB
T{B
UMB
U�B
U�B
U�B
U�B
VB
V9B
VB
VB
V�B
V�B
VmB
V�B
W?B
WsB
W�B
X+B
X_B
XyB
X�B
X�B
X�B
YKB
YKB
YeB
YeB
YKB
YeB
YB
YB
YB
YeB
Y�B
ZB
ZB
ZB
ZkB
Z�B
Z�B
Z�B
[WB
[	B
[qB
\]B
\�B
\�B
]dB
]IB
]~B
]�B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_B
^�B
_;B
_VB
_�B
_�B
`'B
`BB
`\B
`\B
`�B
a-B
abB
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
b�B
cB
c B
c B
c:B
cnB
cTB
d&B
dZB
dtB
dtB
d�B
dtB
eFB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
f2B
ffB
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
g�B
h>B
hXB
hsB
hXB
h�B
hsB
h�B
iB
h�B
iDB
i_B
iyB
i_B
i�B
jeB
j�B
j�B
j�B
j�B
kB
k�B
kkB
k�B
k�B
k�B
k�B
k�B
lB
lB
lB
l"B
lqB
l�B
l�B
l�B
mB
m�B
m�B
n/B
nIB
nIB
ncB
n}B
n�B
n�B
n�B
n}B
n}B
n�B
n�B
o B
oB
o5B
o�B
pB
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q[B
q[B
qvB
q�B
r-B
rGB
rGB
raB
r|B
r�B
r�B
r�B
s3B
sB
sMB
shB
s�B
tB
t9B
t�B
t�B
t�B
t�B
t�B
uB
u%B
uZB
uZB
u�B
u�B
u�B
u�B
v+B
vzB
v�B
v�B
w2B
w2B
wLB
wfB
wLB
xB
x8B
xlB
xlB
xRB
xlB
xRB
x�B
x�B
x�B
y	B
y$B
yXB
yrB
y�B
y�B
zB
zDB
zDB
z�B
z�B
z�B
z�B
{JB
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
}"B
}<B
}<B
}�B
}�B
}�B
}�B
~BB
~BB
~�B
~�B
B
B
}B
�B
�B
�B
� B
�4B
�OB
��B
��B
��B
��B
��B
��B
��B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105243  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192350  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192350  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192350                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042357  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042357  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                