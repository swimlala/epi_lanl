CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-22T18:43:10Z creation;2023-02-22T18:43:11Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230222184310  20230222185701  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��"�91   @��"�9E@0V�t��d ���S�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�  A�33A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�ffBÙ�B�  B˙�B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C33C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@|(�@�{@�{A
=A@��A_
=A
=A��A��A��RA��AυA߅A�A��RBBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�z�B��HB�G�B�z�B��HB�z�B��HB��HB�{BڮB߮B��HB��HB��HB��HB��HB��HB��HB��HC�C#�C�
C�
C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C4
>C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Cj
>Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DP�DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҺ�D��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͂�A͍A͏�A͐�A͍�A͌JA͊rA͇�A͌�A͏�A͓uA͔�A͗$A͖SA͓�A͕�A͙eA�tA�f�A�oiA�XA�8�A�(�A�.�A�HA�)�A��A��0A̪eA̷LA̷LA��'A̹�A̱�A̬=A̴A̸�A̵A̬=A̦�A̠\A̖�A̎VA̍�A̋�A�l�A�2-A��A˲-A�b�Aé*A�چA��vA�K�A�A�?}A���A�kA���A��	A��A�YA�W?A�-�A���A��6A���A��%A�`BA��A��A��A�#nA�Q�A��sA�eA�<jA�0�A�d�A���A���A��A���A��|A�B'A��A���A��uA��VA��A~B[A}~�A}	A|aA{�@A{6�At@OAfA�Ab<6A_��A]{�AW:*AS��ARzxAR�AQ1'AOn�ANl"AL��AJ�$AH�$AE�AAs�A>�DA<�vA;`BA8g8A6��A5qA3s�A2=qA0�BA0!-A/�A/�9A.�DA+2aA)$�A(I�A&ƨA%��A%��A$>�A"SA!��A!��A!$tA��A&�A��ASA�A33A�YA�A�vA.�Aw2AL0A�rAc AiDA\)A	�A֡AںA��A�TAAںA_�A��A�A��A6�A�A^5A@OA�A�PA:�A��A�A
�oA	��A	��A��A\�AF�A��A�A�_A�[A �A ��A��A�5A�ZA4A �@��@�kQ@�ff@�7�@�{J@�O@���@��@� \@�1'@�|@��"@�f�@�g8@��@�a@��@�E�@�خ@��@�!�@�n@�ff@��@�.�@핁@�s@���@�!@�q@�Y�@��@淀@�9@�@�?@�D�@�ff@�J�@�I�@�1'@�	�@��D@��j@姇@�;d@�@�*�@��Z@�� @��@��/@��+@�Vm@��B@�l"@�
�@߮�@� \@��o@�k�@�L�@�S@ܮ}@܃@�M@ۇ�@ڡb@�L0@���@ِ�@�&�@��'@���@�!-@��p@�bN@��@�	�@�x@�q@�X@���@�Xy@ѥ�@�/@�GE@Ϫ�@�j@�%@δ9@�z@�a|@�YK@�4@͕�@�V�@ː�@��@��/@���@�(�@Ț�@�ff@�]d@�C-@���@��T@Ǹ�@ǥ@ǈf@�K�@�&@�+�@��@�v�@�V@�?�@��@ź^@��@�Ta@�Ft@�0U@�.�@��r@��m@â�@�{J@�?}@�@�(�@��@���@�E9@�c @��@��@���@�E9@���@�-�@��@��0@���@���@���@�o�@���@�r�@�W�@�?�@�  @��n@��~@�t�@�
=@���@�m�@�"h@�_@���@��@���@�u%@�c�@�Ft@�
�@��H@�s�@��v@�7�@��}@���@��M@�c�@�,�@�
=@�͟@�W�@��@��6@���@��@�N�@��@��-@��@�Q�@�o@��j@�7�@�ϫ@�k�@��@���@��@��@�w�@�h
@�:*@��6@���@�hs@�7L@���@��u@���@���@��}@�-@�l�@�G�@��@��o@��@���@�W�@�+k@���@��q@���@�\�@�@�u%@�e@��6@���@�=�@�
=@��@�M�@�}�@���@���@�7�@��Z@���@�/@��@���@���@�ی@�i�@��F@��@��\@�C�@��+@���@�a@�C@��6@�u@��@@���@�X�@��[@�h
@�>B@�:*@�.�@��@�  @���@���@�&�@��@��K@�y>@�l�@�YK@�#:@��&@���@�_p@�]�@�_p@�hs@�l�@�X�@�V@���@��X@�bN@�.�@�$�@��@��@�s�@�\�@�=@��@��@���@��m@��6@���@�J�@��A@���@��@�x�@�w2@�u�@�a@�2a@��@�͟@�~(@�4n@���@�J#@��@�@��@��r@�"h@��@���@��@�V@���@�H�@�/�@�1@��@���@��@��K@���@��-@���@�|�@�J#@�@@��@���@�_�@�H�@�0U@��@�� @�n/@�/@��5@��p@���@�c @�?@�!�@�G@��X@�-w@��5@��@��p@��O@�^5@��@�o�@�5�@���@�d�@��@���@���@�:�@��@��@�"h@��C@�|@�\)@�o�@���@�[W@�ߤ@���@�[�@�_p@�0U@~�\@~c @}�@}��@}V@|K^@{�f@{�@z��@z��@z�A@y�C@y@xی@xɆ@x�e@xr�@x9X@xx@w��@v��@u�3@u<6@u+@u�@t�5@t�p@ty>@t"h@s��@sS@r�+@r)�@q��@qc�@qG�@p�E@p��@p��@pC-@o�Q@o�*@o|�@o33@n�@n�X@n��@n�r@n�F@n��@n�r@m��@k�r@jȴ@j͟@j��@j��@i�X@h�v@htT@hh�@g�a@gY@f\�@fu%@e��@e��@eu�@e*0@d��@dbN@d �@c�w@c&@b�y@bM�@a�^@`��@`��@`g8@`4n@`'R@_4�@^0U@]�.@]��@]�N@]rG@]�@\�@\7�@[��@[��@[��@[o@Z� @ZB[@Y�@Y��@Y|@Y[W@Y�@X��@X��@W��@V��@VZ�@U�>@U%@S�;@R�@R8�@Q�T@Q|@Qu�@Pw�@P�@OU�@N�@N��@N�6@N�@M�S@L9X@K��@KZ�@J�@JV@Izx@I+@I�@I�@IV@H�@Hw�@H�@G�@G��@G�@F{�@F#:@E�>@E \@D��@De�@DS�@DA�@D>B@D(�@C��@Bl�@A�3@A�@A��@A��@A��@A��@Aj@AL�@A-w@A@@�@@��@@��@@��@?�@?��@?S�@>J@=e,@=!�@<��@<�K@<�/@<�I@<V�@;� @;y�@;6z@;C@;�@;S@:��@:d�@:H�@:;�@:)�@9��@9�>@9�j@9c@9S&@9 \@8�	@8�?@8�O@8r�@7��@6��@6~�@5��@5�'@5c@5S&@5*0@5�@5%@4�?@42�@3�;@3��@3�4@3t�@3iD@3\)@3,�@2�8@2��@2�H@2�X@2�h@2��@2}V@2!�@1��@1�M@18�@1@@0ѷ@0��@0!@/�@/t�@/O@/6z@.��@.��@.�B@.�X@.�x@.l�@.H�@.($@.@-��@-:�@,��@,�@,Z@,6@,,=@+�@+��@+��@+X�@+C@+�@+o@+�@*�8@*�@)��@)�@)��@)�~@)�@(��@(��@(��@(l"@(S�@(Ft@(C-@(H@(D�@(:�@(  @'|�@'U�@'C@&ں@&Z�@&+k@&_@%�7@%=�@%	l@%�@$�P@$�	@$ѷ@$|�@$Xy@#�W@#RT@#9�@#�@"�M@"�@"�'@"�b@"�\@"kQ@"V@"?@"@!�@!��@!��@!��@!5�@!�@ ��@ ��@ �@ g8@ 7�@ݘ@��@qv@n/@o�@_p@�"@n�@�@��@��@B�@�@I�@7@��@�F@4�@ i@�s@��@��@�!@�L@�b@� @}V@W�@?@8�@-@��@�7@4@�@��@7@ƨ@��@�q@o�@4�@��@�@��@~�@u%@J�@O@u@��@�M@hs@Q�@�@��@��@�D@[�@"h@�@��@��@1�@�@ں@�L@�x@��@�r@q�@H�@{@_@@�^@N<@?}@0�@�@�P@��@��@��@�u@r�@<�@�@�6@��@v`@>�@��@}V@L0@5?@{@�d@��@�@p�@/@�@�@�@��@w�@m�@c�@M@$@�@�@�6@��@��@X�@)_11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͂�A͍A͏�A͐�A͍�A͌JA͊rA͇�A͌�A͏�A͓uA͔�A͗$A͖SA͓�A͕�A͙eA�tA�f�A�oiA�XA�8�A�(�A�.�A�HA�)�A��A��0A̪eA̷LA̷LA��'A̹�A̱�A̬=A̴A̸�A̵A̬=A̦�A̠\A̖�A̎VA̍�A̋�A�l�A�2-A��A˲-A�b�Aé*A�چA��vA�K�A�A�?}A���A�kA���A��	A��A�YA�W?A�-�A���A��6A���A��%A�`BA��A��A��A�#nA�Q�A��sA�eA�<jA�0�A�d�A���A���A��A���A��|A�B'A��A���A��uA��VA��A~B[A}~�A}	A|aA{�@A{6�At@OAfA�Ab<6A_��A]{�AW:*AS��ARzxAR�AQ1'AOn�ANl"AL��AJ�$AH�$AE�AAs�A>�DA<�vA;`BA8g8A6��A5qA3s�A2=qA0�BA0!-A/�A/�9A.�DA+2aA)$�A(I�A&ƨA%��A%��A$>�A"SA!��A!��A!$tA��A&�A��ASA�A33A�YA�A�vA.�Aw2AL0A�rAc AiDA\)A	�A֡AںA��A�TAAںA_�A��A�A��A6�A�A^5A@OA�A�PA:�A��A�A
�oA	��A	��A��A\�AF�A��A�A�_A�[A �A ��A��A�5A�ZA4A �@��@�kQ@�ff@�7�@�{J@�O@���@��@� \@�1'@�|@��"@�f�@�g8@��@�a@��@�E�@�خ@��@�!�@�n@�ff@��@�.�@핁@�s@���@�!@�q@�Y�@��@淀@�9@�@�?@�D�@�ff@�J�@�I�@�1'@�	�@��D@��j@姇@�;d@�@�*�@��Z@�� @��@��/@��+@�Vm@��B@�l"@�
�@߮�@� \@��o@�k�@�L�@�S@ܮ}@܃@�M@ۇ�@ڡb@�L0@���@ِ�@�&�@��'@���@�!-@��p@�bN@��@�	�@�x@�q@�X@���@�Xy@ѥ�@�/@�GE@Ϫ�@�j@�%@δ9@�z@�a|@�YK@�4@͕�@�V�@ː�@��@��/@���@�(�@Ț�@�ff@�]d@�C-@���@��T@Ǹ�@ǥ@ǈf@�K�@�&@�+�@��@�v�@�V@�?�@��@ź^@��@�Ta@�Ft@�0U@�.�@��r@��m@â�@�{J@�?}@�@�(�@��@���@�E9@�c @��@��@���@�E9@���@�-�@��@��0@���@���@���@�o�@���@�r�@�W�@�?�@�  @��n@��~@�t�@�
=@���@�m�@�"h@�_@���@��@���@�u%@�c�@�Ft@�
�@��H@�s�@��v@�7�@��}@���@��M@�c�@�,�@�
=@�͟@�W�@��@��6@���@��@�N�@��@��-@��@�Q�@�o@��j@�7�@�ϫ@�k�@��@���@��@��@�w�@�h
@�:*@��6@���@�hs@�7L@���@��u@���@���@��}@�-@�l�@�G�@��@��o@��@���@�W�@�+k@���@��q@���@�\�@�@�u%@�e@��6@���@�=�@�
=@��@�M�@�}�@���@���@�7�@��Z@���@�/@��@���@���@�ی@�i�@��F@��@��\@�C�@��+@���@�a@�C@��6@�u@��@@���@�X�@��[@�h
@�>B@�:*@�.�@��@�  @���@���@�&�@��@��K@�y>@�l�@�YK@�#:@��&@���@�_p@�]�@�_p@�hs@�l�@�X�@�V@���@��X@�bN@�.�@�$�@��@��@�s�@�\�@�=@��@��@���@��m@��6@���@�J�@��A@���@��@�x�@�w2@�u�@�a@�2a@��@�͟@�~(@�4n@���@�J#@��@�@��@��r@�"h@��@���@��@�V@���@�H�@�/�@�1@��@���@��@��K@���@��-@���@�|�@�J#@�@@��@���@�_�@�H�@�0U@��@�� @�n/@�/@��5@��p@���@�c @�?@�!�@�G@��X@�-w@��5@��@��p@��O@�^5@��@�o�@�5�@���@�d�@��@���@���@�:�@��@��@�"h@��C@�|@�\)@�o�@���@�[W@�ߤ@���@�[�@�_p@�0U@~�\@~c @}�@}��@}V@|K^@{�f@{�@z��@z��@z�A@y�C@y@xی@xɆ@x�e@xr�@x9X@xx@w��@v��@u�3@u<6@u+@u�@t�5@t�p@ty>@t"h@s��@sS@r�+@r)�@q��@qc�@qG�@p�E@p��@p��@pC-@o�Q@o�*@o|�@o33@n�@n�X@n��@n�r@n�F@n��@n�r@m��@k�r@jȴ@j͟@j��@j��@i�X@h�v@htT@hh�@g�a@gY@f\�@fu%@e��@e��@eu�@e*0@d��@dbN@d �@c�w@c&@b�y@bM�@a�^@`��@`��@`g8@`4n@`'R@_4�@^0U@]�.@]��@]�N@]rG@]�@\�@\7�@[��@[��@[��@[o@Z� @ZB[@Y�@Y��@Y|@Y[W@Y�@X��@X��@W��@V��@VZ�@U�>@U%@S�;@R�@R8�@Q�T@Q|@Qu�@Pw�@P�@OU�@N�@N��@N�6@N�@M�S@L9X@K��@KZ�@J�@JV@Izx@I+@I�@I�@IV@H�@Hw�@H�@G�@G��@G�@F{�@F#:@E�>@E \@D��@De�@DS�@DA�@D>B@D(�@C��@Bl�@A�3@A�@A��@A��@A��@A��@Aj@AL�@A-w@A@@�@@��@@��@@��@?�@?��@?S�@>J@=e,@=!�@<��@<�K@<�/@<�I@<V�@;� @;y�@;6z@;C@;�@;S@:��@:d�@:H�@:;�@:)�@9��@9�>@9�j@9c@9S&@9 \@8�	@8�?@8�O@8r�@7��@6��@6~�@5��@5�'@5c@5S&@5*0@5�@5%@4�?@42�@3�;@3��@3�4@3t�@3iD@3\)@3,�@2�8@2��@2�H@2�X@2�h@2��@2}V@2!�@1��@1�M@18�@1@@0ѷ@0��@0!@/�@/t�@/O@/6z@.��@.��@.�B@.�X@.�x@.l�@.H�@.($@.@-��@-:�@,��@,�@,Z@,6@,,=@+�@+��@+��@+X�@+C@+�@+o@+�@*�8@*�@)��@)�@)��@)�~@)�@(��@(��@(��@(l"@(S�@(Ft@(C-@(H@(D�@(:�@(  @'|�@'U�@'C@&ں@&Z�@&+k@&_@%�7@%=�@%	l@%�@$�P@$�	@$ѷ@$|�@$Xy@#�W@#RT@#9�@#�@"�M@"�@"�'@"�b@"�\@"kQ@"V@"?@"@!�@!��@!��@!��@!5�@!�@ ��@ ��@ �@ g8@ 7�@ݘ@��@qv@n/@o�@_p@�"@n�@�@��@��@B�@�@I�@7@��@�F@4�@ i@�s@��@��@�!@�L@�b@� @}V@W�@?@8�@-@��@�7@4@�@��@7@ƨ@��@�q@o�@4�@��@�@��@~�@u%@J�@O@u@��@�M@hs@Q�@�@��@��@�D@[�@"h@�@��@��@1�@�@ں@�L@�x@��@�r@q�@H�@{@_@@�^@N<@?}@0�@�@�P@��@��@��@�u@r�@<�@�@�6@��@v`@>�@��@}V@L0@5?@{@�d@��@�@p�@/@�@�@�@��@w�@m�@c�@M@$@�@�@�6@��@��@X�@)_11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	NB	NB	M�B	M�B	NB	NB	M�B	M�B	MPB	MPB	M�B	NB	N�B	P}B	RoB	Q�B	Q B	c�B	v+B	�B	��B	��B	�B	��B	�B	��B	��B	� B	|�B	��B	�[B	�SB	�YB	�?B	�?B	��B	�^B	��B	��B	�=B	��B	�fB	��B	��B	�1B	��B	�B	��B	��B	��B
�B
�B
�B
�:B
��B%BV�BuB�	B�B�B��B��Bh�B��B�B��B�uB�EB�B�GB��Ba-BB'B/ B(�B�B�B
�B
ۦB
�=B
�9B
��B
��B
�}B
�B
i�B
Y�B
IB
8�B
/�B
*�B
'RB
"�B
IB
SB	��B	x�B	bB	[�B	QNB	7fB	/�B	/�B	1AB	3�B	/�B	,B	&fB	$�B	$�B	%FB	%FB	+�B	1�B	:^B	C�B	=B	6�B	1AB	/�B	4nB	6+B	4�B	2aB	-)B	"�B	�B	"hB	;�B	=VB	?�B	F�B	DB	DgB	O(B	\�B	DgB	5�B	-�B	)�B	)�B	$ZB	)�B	E9B	C�B	@OB	]~B	r�B	xRB	��B	�B	�MB	��B	q�B	s�B	y>B	�B	��B	��B	�uB	�mB	�1B	�1B	��B	�B	�uB	�B	��B	��B	�{B	�oB	�hB	��B	�bB	�vB	�\B	�TB	��B	��B	�~B	z�B	g�B	f2B	c�B	sB	��B	��B	�B	�SB	}�B	u�B	x�B	|�B	HB	��B	�=B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	��B	� B	��B	��B	~(B	{0B	|jB	~]B	��B	��B	�tB	��B	�&B	�SB	��B	�=B	��B	��B	��B	�ZB	�RB	��B	��B	��B	�B	��B	�8B	�JB	��B	��B	�<B	�wB	��B	� B	�iB	�[B	��B	�B	ÖB	�gB	āB	�gB	�mB	�+B	ȚB	��B	ʌB	��B	˒B	�B	�PB	�B	̈́B	͟B	�PB	�B	�B	��B	��B	ЗB	��B	�4B	҉B	��B	ңB	ңB	�@B	�B	�FB	�B	�,B	ԯB	յB	�2B	՛B	�gB	�?B	خB	��B	�KB	�1B	�eB	ڠB	�	B	�]B	�NB	�`B	�zB	�`B	�B	�B	��B	�B	�B	�B	��B	�KB	�QB	�}B	�'B	��B	�?B	�FB	�fB	�2B	�fB	��B	��B	�LB	�`B	��B	��B	��B	��B	��B	�	B	��B	�LB	��B	��B	��B	��B	�B	��B	�JB	�JB	�0B	�JB	�dB	�B	�B	�0B	��B	��B	�(B	��B
�B
B
�B
MB
3B
�B
�B
�B
�B
9B
�B
-B
�B
�B
AB
aB
gB
MB
�B
+B
B
B
KB
	7B
	�B
	�B

=B

	B

=B

	B

=B

�B
B
�B
JB
�B
�B
B
B
B
6B
6B
�B
�B

�B

=B
	B
	RB
�B
�B
�B
�B
_B
	�B

�B
6B
�B
\B
�B
:B
�B
&B
uB
@B
B
:B
�B
B
�B
�B
[B
�B
B
FB
FB
�B
{B
�B
�B
�B
mB
�B
�B
SB
gB
�B
�B
oB
uB
�B
�B
�B
B
�B
FB
�B
mB
B
�B
+B
_B
�B
�B
yB
�B
�B
�B
�B
B
B
�B
~B
�B
�B
IB
IB
�B
jB
�B
!B
;B
 \B
!B
!-B
!|B
!B
 �B
!B
!HB
!bB
!-B
!|B
!�B
!�B
!�B
"B
"4B
"hB
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%,B
$�B
&fB
&2B
&2B
&LB
&�B
&�B
&�B
&fB
%�B
&fB
&�B
&fB
&�B
&�B
&�B
&�B
&�B
'B
'�B
(>B
(XB
(�B
)�B
*0B
)�B
*B
*B
*�B
*�B
*�B
*�B
,WB
,�B
-B
-CB
-]B
.cB
.}B
.�B
.}B
.}B
.�B
/B
/5B
/B
.�B
.�B
-�B
,�B
,�B
,B
+kB
,=B
,WB
,"B
,"B
,"B
,qB
+�B
+B
*�B
*KB
+�B
,�B
,�B
-CB
,�B
,B
*�B
+B
(�B
(�B
(sB
(�B
(�B
(�B
($B
'�B
(>B
(>B
(>B
(XB
(sB
)B
*eB
*�B
+QB
+6B
+B
+kB
+6B
*B
*�B
*�B
+B
+QB
+kB
+�B
+�B
+�B
+6B
+B
+QB
+�B
+�B
,B
,qB
,�B
,�B
,�B
-CB
-)B
-]B
-�B
-�B
.B
.IB
.�B
0B
2�B
3hB
4B
3�B
3hB
3�B
4B
4�B
6+B
6�B
7B
6�B
7�B
8B
9�B
<�B
=VB
>B
>BB
>�B
>�B
?B
?HB
?�B
?�B
@B
@iB
@�B
AB
A�B
A�B
A�B
A�B
B�B
CB
B�B
B�B
CB
C-B
C{B
C�B
D3B
D3B
D3B
D�B
D�B
ESB
ESB
E�B
E�B
F%B
F?B
FYB
FB
E�B
F�B
GB
F�B
GB
G�B
GB
FB
E�B
F?B
F�B
G�B
H1B
H�B
HfB
H�B
I�B
J#B
J	B
IlB
G�B
G�B
HB
G�B
HfB
IB
I�B
I�B
I�B
IlB
I�B
J�B
KxB
KxB
LB
LJB
L~B
L~B
L�B
M�B
M�B
N�B
O\B
OvB
OvB
O�B
O�B
P.B
Q4B
QhB
QhB
Q�B
Q�B
R:B
R�B
SB
S&B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
VSB
W�B
Y1B
Y�B
Y�B
Y�B
Z7B
Z7B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[�B
[�B
\�B
\�B
\�B
]B
]/B
]/B
]/B
]dB
]�B
]�B
^5B
^5B
^5B
^5B
^5B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
`B
`vB
`�B
a-B
a-B
aB
a�B
a|B
a|B
abB
a�B
a�B
a�B
a�B
a�B
bNB
b�B
c B
c:B
cTB
cTB
cTB
c�B
dB
dB
dZB
d�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
fB
f�B
f�B
gB
gB
gRB
gRB
gmB
gmB
gRB
gRB
g8B
g�B
h$B
h
B
hXB
h�B
iB
iB
iDB
i�B
i�B
jKB
j0B
jKB
jKB
jeB
j�B
j�B
k6B
k�B
k�B
k�B
l"B
l"B
l"B
lWB
lWB
lqB
l�B
l�B
l�B
mB
m)B
m)B
m)B
mwB
m�B
m�B
m�B
n/B
n/B
nIB
n�B
n�B
oB
oB
o B
o B
oOB
pB
pUB
p�B
p�B
qB
qAB
rB
rB
rGB
raB
sB
s3B
shB
shB
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
u%B
u�B
vB
u�B
u�B
vFB
vzB
v�B
v�B
v�B
w2B
v�B
wLB
wLB
wfB
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
x�B
y>B
yrB
y�B
z*B
y�B
zDB
zxB
z^B
zxB
zxB
z�B
z�B
{JB
{B
{dB
{�B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}"B
}VB
}VB
}qB
}�B
~(B
~�B
~�B
~�B
~�B
.B
}B
}B
}B
�B
�B
�B
��B
��B
�B
�B
�B
� B
�UB
�UB
�oB
��B
��B
��B
�B
�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	NB	NB	M�B	M�B	NB	NB	M�B	M�B	MPB	MPB	M�B	NB	N�B	P}B	RoB	Q�B	Q B	c�B	v+B	�B	��B	��B	�B	��B	�B	��B	��B	� B	|�B	��B	�[B	�SB	�YB	�?B	�?B	��B	�^B	��B	��B	�=B	��B	�fB	��B	��B	�1B	��B	�B	��B	��B	��B
�B
�B
�B
�:B
��B%BV�BuB�	B�B�B��B��Bh�B��B�B��B�uB�EB�B�GB��Ba-BB'B/ B(�B�B�B
�B
ۦB
�=B
�9B
��B
��B
�}B
�B
i�B
Y�B
IB
8�B
/�B
*�B
'RB
"�B
IB
SB	��B	x�B	bB	[�B	QNB	7fB	/�B	/�B	1AB	3�B	/�B	,B	&fB	$�B	$�B	%FB	%FB	+�B	1�B	:^B	C�B	=B	6�B	1AB	/�B	4nB	6+B	4�B	2aB	-)B	"�B	�B	"hB	;�B	=VB	?�B	F�B	DB	DgB	O(B	\�B	DgB	5�B	-�B	)�B	)�B	$ZB	)�B	E9B	C�B	@OB	]~B	r�B	xRB	��B	�B	�MB	��B	q�B	s�B	y>B	�B	��B	��B	�uB	�mB	�1B	�1B	��B	�B	�uB	�B	��B	��B	�{B	�oB	�hB	��B	�bB	�vB	�\B	�TB	��B	��B	�~B	z�B	g�B	f2B	c�B	sB	��B	��B	�B	�SB	}�B	u�B	x�B	|�B	HB	��B	�=B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	��B	� B	��B	��B	~(B	{0B	|jB	~]B	��B	��B	�tB	��B	�&B	�SB	��B	�=B	��B	��B	��B	�ZB	�RB	��B	��B	��B	�B	��B	�8B	�JB	��B	��B	�<B	�wB	��B	� B	�iB	�[B	��B	�B	ÖB	�gB	āB	�gB	�mB	�+B	ȚB	��B	ʌB	��B	˒B	�B	�PB	�B	̈́B	͟B	�PB	�B	�B	��B	��B	ЗB	��B	�4B	҉B	��B	ңB	ңB	�@B	�B	�FB	�B	�,B	ԯB	յB	�2B	՛B	�gB	�?B	خB	��B	�KB	�1B	�eB	ڠB	�	B	�]B	�NB	�`B	�zB	�`B	�B	�B	��B	�B	�B	�B	��B	�KB	�QB	�}B	�'B	��B	�?B	�FB	�fB	�2B	�fB	��B	��B	�LB	�`B	��B	��B	��B	��B	��B	�	B	��B	�LB	��B	��B	��B	��B	�B	��B	�JB	�JB	�0B	�JB	�dB	�B	�B	�0B	��B	��B	�(B	��B
�B
B
�B
MB
3B
�B
�B
�B
�B
9B
�B
-B
�B
�B
AB
aB
gB
MB
�B
+B
B
B
KB
	7B
	�B
	�B

=B

	B

=B

	B

=B

�B
B
�B
JB
�B
�B
B
B
B
6B
6B
�B
�B

�B

=B
	B
	RB
�B
�B
�B
�B
_B
	�B

�B
6B
�B
\B
�B
:B
�B
&B
uB
@B
B
:B
�B
B
�B
�B
[B
�B
B
FB
FB
�B
{B
�B
�B
�B
mB
�B
�B
SB
gB
�B
�B
oB
uB
�B
�B
�B
B
�B
FB
�B
mB
B
�B
+B
_B
�B
�B
yB
�B
�B
�B
�B
B
B
�B
~B
�B
�B
IB
IB
�B
jB
�B
!B
;B
 \B
!B
!-B
!|B
!B
 �B
!B
!HB
!bB
!-B
!|B
!�B
!�B
!�B
"B
"4B
"hB
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%,B
$�B
&fB
&2B
&2B
&LB
&�B
&�B
&�B
&fB
%�B
&fB
&�B
&fB
&�B
&�B
&�B
&�B
&�B
'B
'�B
(>B
(XB
(�B
)�B
*0B
)�B
*B
*B
*�B
*�B
*�B
*�B
,WB
,�B
-B
-CB
-]B
.cB
.}B
.�B
.}B
.}B
.�B
/B
/5B
/B
.�B
.�B
-�B
,�B
,�B
,B
+kB
,=B
,WB
,"B
,"B
,"B
,qB
+�B
+B
*�B
*KB
+�B
,�B
,�B
-CB
,�B
,B
*�B
+B
(�B
(�B
(sB
(�B
(�B
(�B
($B
'�B
(>B
(>B
(>B
(XB
(sB
)B
*eB
*�B
+QB
+6B
+B
+kB
+6B
*B
*�B
*�B
+B
+QB
+kB
+�B
+�B
+�B
+6B
+B
+QB
+�B
+�B
,B
,qB
,�B
,�B
,�B
-CB
-)B
-]B
-�B
-�B
.B
.IB
.�B
0B
2�B
3hB
4B
3�B
3hB
3�B
4B
4�B
6+B
6�B
7B
6�B
7�B
8B
9�B
<�B
=VB
>B
>BB
>�B
>�B
?B
?HB
?�B
?�B
@B
@iB
@�B
AB
A�B
A�B
A�B
A�B
B�B
CB
B�B
B�B
CB
C-B
C{B
C�B
D3B
D3B
D3B
D�B
D�B
ESB
ESB
E�B
E�B
F%B
F?B
FYB
FB
E�B
F�B
GB
F�B
GB
G�B
GB
FB
E�B
F?B
F�B
G�B
H1B
H�B
HfB
H�B
I�B
J#B
J	B
IlB
G�B
G�B
HB
G�B
HfB
IB
I�B
I�B
I�B
IlB
I�B
J�B
KxB
KxB
LB
LJB
L~B
L~B
L�B
M�B
M�B
N�B
O\B
OvB
OvB
O�B
O�B
P.B
Q4B
QhB
QhB
Q�B
Q�B
R:B
R�B
SB
S&B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
VSB
W�B
Y1B
Y�B
Y�B
Y�B
Z7B
Z7B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[�B
[�B
\�B
\�B
\�B
]B
]/B
]/B
]/B
]dB
]�B
]�B
^5B
^5B
^5B
^5B
^5B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
`B
`vB
`�B
a-B
a-B
aB
a�B
a|B
a|B
abB
a�B
a�B
a�B
a�B
a�B
bNB
b�B
c B
c:B
cTB
cTB
cTB
c�B
dB
dB
dZB
d�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
fB
f�B
f�B
gB
gB
gRB
gRB
gmB
gmB
gRB
gRB
g8B
g�B
h$B
h
B
hXB
h�B
iB
iB
iDB
i�B
i�B
jKB
j0B
jKB
jKB
jeB
j�B
j�B
k6B
k�B
k�B
k�B
l"B
l"B
l"B
lWB
lWB
lqB
l�B
l�B
l�B
mB
m)B
m)B
m)B
mwB
m�B
m�B
m�B
n/B
n/B
nIB
n�B
n�B
oB
oB
o B
o B
oOB
pB
pUB
p�B
p�B
qB
qAB
rB
rB
rGB
raB
sB
s3B
shB
shB
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
u%B
u�B
vB
u�B
u�B
vFB
vzB
v�B
v�B
v�B
w2B
v�B
wLB
wLB
wfB
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
x�B
y>B
yrB
y�B
z*B
y�B
zDB
zxB
z^B
zxB
zxB
z�B
z�B
{JB
{B
{dB
{�B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}"B
}VB
}VB
}qB
}�B
~(B
~�B
~�B
~�B
~�B
.B
}B
}B
}B
�B
�B
�B
��B
��B
�B
�B
�B
� B
�UB
�UB
�oB
��B
��B
��B
�B
�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230222184309  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230222184310  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230222184310  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230222184311                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230222184311  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230222184311  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230222185701                      G�O�G�O�G�O�                