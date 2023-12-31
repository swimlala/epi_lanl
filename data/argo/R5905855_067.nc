CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:22:26Z creation;2022-06-04T19:22:27Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192226  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�M�2@y]1   @�M٘Eȡ@/G+I��c��t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A33AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B���B�  B�  B�  B�  B���B�33B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�ffB���B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6�C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\33C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5@|(�@�{A=pAp�A=p�A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7\)B?BGBOBWB_BgBoBwBB��HB�{B��B��B��HB��HB��HB��HB�z�B�{B��B��HB��HB��HB��HB��HBîB��HB��HB��HB��HB�G�BۮB߮B�B��HB��HB��HB��HB��HB��HB��HC�C�C�C�C

>C�C�C�C�C�C�C�C�
C�C�C�C!�C#�C%�C'�C)�C+�C-�C0
>C2
>C3�C6
>C7�
C9�
C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C\#�C]�
C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD�GD�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�>D�~D��D��D�>D�tz1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A���A���A��A� iA�	�A��A�.A�:A�hA�bA�A��A�xA�	�A�bA�MA��A�kA�A�IA��A��A�(�A�$A�8�A�GEA�T,A�J�A�A�A�C�A�J�A�J�A�i�A��A�h�A��	A��AĬ=A�y�A�A��A�:^A���A���A��cA��A�ȴA���A��A�OBA��2A���A�5tA���A�:�A�A�E�A�LdA��4A�ܒA�)_A�<6A�=�A��'A��%A�4nA��nA���A�U2A�49A�W�A��zA�MjA�^�A�M�A��1A��JA�ȀA�8�A��&A���A�k�A��xA�cTA�p;A�qA�S�A�  A��,A��A���A|�Av�AAr�Ap��Ak�hAh,=Ac�A^�A\h�A[,�AY
=AVa�AT�AS�fAQ��AP�AO)�AJ�dAE�DAA[�A@;A?��AA1�AA�A@iDA=��A=7�A<_pA;�aA;��A:�qA9A�A8�A8�A9�A8A7"hA5ϫA4bA--�A)ȴA)5�A(��A'GEA%�DA$��A$��A$�PA$FA#��A"��A"FtA!�}A!e�A!GEA ��A -�AJ�AK^A�yA��A�A�AzA�A�A#:A��AL�A(�AԕA�A �A��A$tA�jA:*AbA�1AJ#A�A~A��A{�A/A�6A&A~A��A�A��Ao�A�ArGAQA9�A��Ab�ACA�ARTA�ARTAHA�mA��A��A�HA��AE�A�AA�KAS�AA
��A
@�A	��A	��A	�YA�Ai�A(�A� A@OA�sA��Ah�A;dA�A�A��A3�A��A�4Ae�AɆA{JA|�A��A�1A�4AA�XAq�AiDA?�AuA��AL0A �A ��A "h@�:�@��v@���@���@��@��m@��+@�'R@��h@���@�^5@�{@��9@�/@�u%@��.@��#@���@��C@�V@�҉@�GE@�6�@�a�@�"�@�S@��.@�"h@��@�ݘ@��@�	@�@���@�@�h@�!-@��m@�=@���@�R@�5?@�]�@�R�@뙚@꿱@�<�@���@��@��E@���@�}@�k@�4@��@�oi@�9X@��@�@��}@�@@��@�Ĝ@ߴ�@�p�@�&�@ݜ@�-w@��@ܦL@ܑ�@܇�@�a|@��@�\)@��B@�ƨ@�C�@��s@�!@׷@�{J@֯O@�e�@��@��@�q�@��g@�qv@���@�J�@��@�͟@Р�@Ϻ^@�33@�;@�4@�J#@̣@�h
@��@˙�@�"�@�`�@�W?@ȥz@Ȗ�@�|�@Ǡ�@ƹ�@��T@��@�h�@��@��#@öF@Á�@�p�@�IR@�}V@�3�@�7@���@�^�@��	@��{@�:�@���@��I@�:�@�˒@� \@�y>@���@��@�:*@�!�@��@�1�@���@��	@�~(@���@���@��U@���@���@���@��4@�V�@��@�a|@��@�� @���@���@���@�_@��D@���@�x@�Y@� i@�͟@��u@�e�@�N�@���@��C@�e�@���@��@��.@�V@���@���@�X@�@��@�֡@���@�z�@�oi@�d�@�GE@��@��@���@�f�@�C�@��@��H@���@���@�b@��@�x@� \@���@���@�a|@�=q@��@�J�@��@���@�`�@��@�}�@��@���@�e�@�<�@��@�]�@�P�@���@�l�@�-@�u@��@�O�@��@��@�҉@�q@�!�@��3@�/�@���@��H@��?@�{�@�?@���@���@�A�@�S@��,@�s�@�9X@���@���@�7L@��@�͟@���@�?@���@�{J@�H�@�@@��@���@�tT@�_@�>B@��@��Z@�o�@�-w@�S@��)@��O@�"h@�@��Z@���@���@�b�@�.I@��@�l"@�8�@�G@��)@��Q@���@�O@��m@�^5@�L0@�$�@��K@���@�c�@�S&@�RT@�;d@���@�l�@�W�@�)�@���@�A�@�V@��2@���@���@��)@���@�j@�S�@�4�@� i@��@�:�@��$@�@���@�"h@���@�!-@��6@�~(@�g8@�4n@��g@���@��h@�33@��@�PH@��@���@�ϫ@���@��h@�\)@�;d@�Y@��@��4@��D@�I�@��9@��7@� i@�ں@��<@��_@�L0@�  @���@�ԕ@���@��V@�m]@�1�@��@�ѷ@�"h@���@�c@��@��E@��D@�Ft@��@��@�r@��@~�@}�@}�@}��@}N<@}7L@|�`@|�@|��@|��@|��@|��@|��@|��@|c�@|b@{�+@{��@{�
@{�w@{��@{8@zE�@yf�@x��@x�@w�m@w�;@wl�@v�]@v�@u��@u�3@u�@t�K@t/�@s|�@s�@r�@r��@q��@q*0@p�j@pN�@px@o=@n��@n��@np;@nv�@n-@m��@m�@lѷ@l��@l�e@l��@k�&@k4�@j��@jTa@jJ@i�X@h�v@hl"@h�@g�
@g��@g��@g�$@g�P@gt�@f�!@fe@e��@eԕ@eA @dی@d�@c��@cx@b�@b��@a��@aY�@a<6@a�@a;@`��@`��@_��@^ȴ@^�@]��@]�@\�u@\bN@\Z@\U2@\6@[��@[�$@[C�@[S@Z�@ZE�@Y��@Yj@X��@Xl"@W�@WO@V��@V��@V��@VW�@V{@U��@TXy@SF�@S!-@R�s@R{�@Q��@Q�@Q�#@Q��@Q�@QN<@Q2a@P��@P�.@PZ@PD�@P/�@O�Q@Ot�@O�@N�6@M�.@M�S@MS&@M=�@M:�@M	l@L�$@Lb@K�@Kg�@KS�@KO@J�h@I��@I��@I=�@G�A@G�@F�@FR�@E`B@E(�@E�@D��@D�$@Dg8@C��@C��@C�@B��@BR�@B�@A��@A�@@��@@2�@@�@?��@?t�@?�@?
=@>�@>kQ@>+k@=��@=\�@=L�@=F@=8�@=�@=@<�	@<�@<D�@;˒@;~�@;$t@:L0@9��@9��@9o @9=�@9�@8�I@8-�@7�r@7�Q@7��@7W?@7(@6��@6�B@6�r@5�X@5f�@5Dg@5-w@4�	@4��@4j@3��@3 i@2��@2{�@2$�@1�@1��@1��@1c@1*0@0�@0r�@0_@0M@0  @/��@/�q@/�*@/]�@/'�@/
=@.��@.l�@-�o@-�@-@-a�@,�5@,��@,��@,��@+�
@+l�@+�@*�@*l�@)��@)��@)a�@)S&@)�@(��@(tT@'�@'Mj@&��@&h
@&.�@%�@%�9@%��@% \@$|�@$1@#��@#��@#�P@#>�@"J�@!��@!|@!a�@!c�@!L�@!�@ ��@ bN@ I�@ :�@ -�@ x@ �@ �@   @�@��@�@�@�@Z�@Y@d�@;�@)�@�@
�@�@�h@c@[W@%F@�@��@��@c�@�@��@ݘ@� @1�@�H@��@u%@&�@�@��@@�H@�@�X@��@e,@O�@Dg@*0@�@z�@�@�Q@ƨ@v`@\)@A�@C@��@1�@��@�@�N@�@p�@j@O�@/@��@�z@��@��@PH@!@�&@�@��@��@e�@U�@,�@�@�8@�s@��@� @p;@1�@J@�Z@�>@�@��@��@�9@��@�=@[W@5�@;@�	@��@��@��@��@�@|�@tT@l"@:�@�@�f@Mj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A���A���A��A� iA�	�A��A�.A�:A�hA�bA�A��A�xA�	�A�bA�MA��A�kA�A�IA��A��A�(�A�$A�8�A�GEA�T,A�J�A�A�A�C�A�J�A�J�A�i�A��A�h�A��	A��AĬ=A�y�A�A��A�:^A���A���A��cA��A�ȴA���A��A�OBA��2A���A�5tA���A�:�A�A�E�A�LdA��4A�ܒA�)_A�<6A�=�A��'A��%A�4nA��nA���A�U2A�49A�W�A��zA�MjA�^�A�M�A��1A��JA�ȀA�8�A��&A���A�k�A��xA�cTA�p;A�qA�S�A�  A��,A��A���A|�Av�AAr�Ap��Ak�hAh,=Ac�A^�A\h�A[,�AY
=AVa�AT�AS�fAQ��AP�AO)�AJ�dAE�DAA[�A@;A?��AA1�AA�A@iDA=��A=7�A<_pA;�aA;��A:�qA9A�A8�A8�A9�A8A7"hA5ϫA4bA--�A)ȴA)5�A(��A'GEA%�DA$��A$��A$�PA$FA#��A"��A"FtA!�}A!e�A!GEA ��A -�AJ�AK^A�yA��A�A�AzA�A�A#:A��AL�A(�AԕA�A �A��A$tA�jA:*AbA�1AJ#A�A~A��A{�A/A�6A&A~A��A�A��Ao�A�ArGAQA9�A��Ab�ACA�ARTA�ARTAHA�mA��A��A�HA��AE�A�AA�KAS�AA
��A
@�A	��A	��A	�YA�Ai�A(�A� A@OA�sA��Ah�A;dA�A�A��A3�A��A�4Ae�AɆA{JA|�A��A�1A�4AA�XAq�AiDA?�AuA��AL0A �A ��A "h@�:�@��v@���@���@��@��m@��+@�'R@��h@���@�^5@�{@��9@�/@�u%@��.@��#@���@��C@�V@�҉@�GE@�6�@�a�@�"�@�S@��.@�"h@��@�ݘ@��@�	@�@���@�@�h@�!-@��m@�=@���@�R@�5?@�]�@�R�@뙚@꿱@�<�@���@��@��E@���@�}@�k@�4@��@�oi@�9X@��@�@��}@�@@��@�Ĝ@ߴ�@�p�@�&�@ݜ@�-w@��@ܦL@ܑ�@܇�@�a|@��@�\)@��B@�ƨ@�C�@��s@�!@׷@�{J@֯O@�e�@��@��@�q�@��g@�qv@���@�J�@��@�͟@Р�@Ϻ^@�33@�;@�4@�J#@̣@�h
@��@˙�@�"�@�`�@�W?@ȥz@Ȗ�@�|�@Ǡ�@ƹ�@��T@��@�h�@��@��#@öF@Á�@�p�@�IR@�}V@�3�@�7@���@�^�@��	@��{@�:�@���@��I@�:�@�˒@� \@�y>@���@��@�:*@�!�@��@�1�@���@��	@�~(@���@���@��U@���@���@���@��4@�V�@��@�a|@��@�� @���@���@���@�_@��D@���@�x@�Y@� i@�͟@��u@�e�@�N�@���@��C@�e�@���@��@��.@�V@���@���@�X@�@��@�֡@���@�z�@�oi@�d�@�GE@��@��@���@�f�@�C�@��@��H@���@���@�b@��@�x@� \@���@���@�a|@�=q@��@�J�@��@���@�`�@��@�}�@��@���@�e�@�<�@��@�]�@�P�@���@�l�@�-@�u@��@�O�@��@��@�҉@�q@�!�@��3@�/�@���@��H@��?@�{�@�?@���@���@�A�@�S@��,@�s�@�9X@���@���@�7L@��@�͟@���@�?@���@�{J@�H�@�@@��@���@�tT@�_@�>B@��@��Z@�o�@�-w@�S@��)@��O@�"h@�@��Z@���@���@�b�@�.I@��@�l"@�8�@�G@��)@��Q@���@�O@��m@�^5@�L0@�$�@��K@���@�c�@�S&@�RT@�;d@���@�l�@�W�@�)�@���@�A�@�V@��2@���@���@��)@���@�j@�S�@�4�@� i@��@�:�@��$@�@���@�"h@���@�!-@��6@�~(@�g8@�4n@��g@���@��h@�33@��@�PH@��@���@�ϫ@���@��h@�\)@�;d@�Y@��@��4@��D@�I�@��9@��7@� i@�ں@��<@��_@�L0@�  @���@�ԕ@���@��V@�m]@�1�@��@�ѷ@�"h@���@�c@��@��E@��D@�Ft@��@��@�r@��@~�@}�@}�@}��@}N<@}7L@|�`@|�@|��@|��@|��@|��@|��@|��@|c�@|b@{�+@{��@{�
@{�w@{��@{8@zE�@yf�@x��@x�@w�m@w�;@wl�@v�]@v�@u��@u�3@u�@t�K@t/�@s|�@s�@r�@r��@q��@q*0@p�j@pN�@px@o=@n��@n��@np;@nv�@n-@m��@m�@lѷ@l��@l�e@l��@k�&@k4�@j��@jTa@jJ@i�X@h�v@hl"@h�@g�
@g��@g��@g�$@g�P@gt�@f�!@fe@e��@eԕ@eA @dی@d�@c��@cx@b�@b��@a��@aY�@a<6@a�@a;@`��@`��@_��@^ȴ@^�@]��@]�@\�u@\bN@\Z@\U2@\6@[��@[�$@[C�@[S@Z�@ZE�@Y��@Yj@X��@Xl"@W�@WO@V��@V��@V��@VW�@V{@U��@TXy@SF�@S!-@R�s@R{�@Q��@Q�@Q�#@Q��@Q�@QN<@Q2a@P��@P�.@PZ@PD�@P/�@O�Q@Ot�@O�@N�6@M�.@M�S@MS&@M=�@M:�@M	l@L�$@Lb@K�@Kg�@KS�@KO@J�h@I��@I��@I=�@G�A@G�@F�@FR�@E`B@E(�@E�@D��@D�$@Dg8@C��@C��@C�@B��@BR�@B�@A��@A�@@��@@2�@@�@?��@?t�@?�@?
=@>�@>kQ@>+k@=��@=\�@=L�@=F@=8�@=�@=@<�	@<�@<D�@;˒@;~�@;$t@:L0@9��@9��@9o @9=�@9�@8�I@8-�@7�r@7�Q@7��@7W?@7(@6��@6�B@6�r@5�X@5f�@5Dg@5-w@4�	@4��@4j@3��@3 i@2��@2{�@2$�@1�@1��@1��@1c@1*0@0�@0r�@0_@0M@0  @/��@/�q@/�*@/]�@/'�@/
=@.��@.l�@-�o@-�@-@-a�@,�5@,��@,��@,��@+�
@+l�@+�@*�@*l�@)��@)��@)a�@)S&@)�@(��@(tT@'�@'Mj@&��@&h
@&.�@%�@%�9@%��@% \@$|�@$1@#��@#��@#�P@#>�@"J�@!��@!|@!a�@!c�@!L�@!�@ ��@ bN@ I�@ :�@ -�@ x@ �@ �@   @�@��@�@�@�@Z�@Y@d�@;�@)�@�@
�@�@�h@c@[W@%F@�@��@��@c�@�@��@ݘ@� @1�@�H@��@u%@&�@�@��@@�H@�@�X@��@e,@O�@Dg@*0@�@z�@�@�Q@ƨ@v`@\)@A�@C@��@1�@��@�@�N@�@p�@j@O�@/@��@�z@��@��@PH@!@�&@�@��@��@e�@U�@,�@�@�8@�s@��@� @p;@1�@J@�Z@�>@�@��@��@�9@��@�=@[W@5�@;@�	@��@��@��@��@�@|�@tT@l"@:�@�@�f@Mj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�sB	�sB	�YB	�sB	�YB	��B	�
B	��B	��B	�yB	�EB	�sB	�
B	��B	�$B	�_B	��B	�B	�1B	�B	�QB	��B	�BB	��B	�$B	��B	�gB	�"B	�uB	�B	یB	��B	ݘB	��B	��B
#:B
0UB
2GB
@ B
E�B
F�B
S�B
o B
|�B
��B
�B
��B
�B
�{B
�]B�BgBQ�B`B]IBf�B�B�eB��B�lB�B�IB��B��B�#B��B�gBr�Bn�Bf�B[qBU�BO\B7�BKB�B
��B
�B
��B
��B
��B
uZB
kkB
c�B
^�B
sB
i�B
RTB
="B
.B	�B	�/B	��B	�B	�DB	q�B	YKB	FtB	+B	�B	B	jB	B	�B		B	]B	.cB	0�B	C{B	=�B	?�B	:*B	=�B	p;B	�=B	��B	�,B	��B	�[B	��B	��B	��B	��B	ּB	�KB
MB
bB
�B
B	�(B	��B	�]B	�5B	�[B	��B	��B	��B	�;B	��B	�B	�oB	āB	�B	�B	ɠB	�#B	��B	бB	��B	�+B	�gB	��B	ЗB	͟B	�B	�	B	��B	�XB	ʌB	ΥB	�uB	ڠB	�KB	��B
�B
�B
B
�B
(B
B
TB
�B
sB
�B
�B
B
 \B
 �B
"�B
&LB
)B
)DB
)yB
(�B
'B
'RB
)*B
*0B
)�B
(�B
(�B
&�B
$@B
"B
'�B
'�B
*B
1'B
2�B
33B
3�B
5ZB
7�B
7�B
6�B
5tB
3�B
0!B
+B
-�B
-wB
(�B
$�B
"NB
 vB
�B
�B
�B
/B
]B
�B
=B
�B
7B
�B
�B
�B
B
KB
�B
�B
�B
�B
jB
 'B
pB
�B
 �B
 B
VB
dB
�B
�B
�B
B
SB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
WB
=B
�B
WB
�B
kB
�B
B
�B
�B
�B
MB
�B
MB
�B
�B
mB
�B
�B
SB
$B
�B
�B
�B
SB
MB
�B
�B
�B
�B
�B
hB
�B
�B
B
�B
(B
VB
jB
�B
�B

�B

�B
	�B
	B
�B
�B
�B
�B
B
3B
�B
?B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
uB
 �B
 �B
 �B
UB
 B
 �B	��B
B
B
oB
 �B
 �B
 �B
 �B
 B
 B	�HB	��B	��B	�HB
  B
 �B
 �B	��B
 B
UB
9B
�B
B
'B
�B
MB
�B
�B
 �B
 B
 �B
 OB
 �B
oB
oB
�B
�B
�B
AB
'B
uB
�B
�B
�B
�B
B
KB
fB
�B
1B
B
�B
+B
?B
B
�B
B
EB
)B
�B
�B
B
�B
�B
�B
�B
�B
~B
�B
�B
PB
6B
6B
6B
B
�B
�B
JB
~B
�B
�B
~B
jB
B
�B
�B
�B
�B
�B
VB
�B
~B
dB
JB
0B
�B
�B
�B
xB
�B
�B
�B
�B

�B

rB

XB

�B
^B
0B
"B
�B
�B
�B
�B
BB
�B
�B
�B
<B
�B
�B
�B
B
<B
VB
�B
BB
�B
.B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
 B
 B
oB
&B
uB
uB
�B
�B
�B
B
�B
�B
�B
�B
SB
�B
�B
�B
�B

B
sB
�B
�B
�B
�B
+B
_B
�B
�B
�B
B
B
�B
�B
kB
�B
�B
�B
�B
B
B
B
)B
B
]B
)B
]B
CB
)B
)B
)B
B
�B
�B
B
B
�B
�B
~B
�B
~B
dB
/B
/B
B
�B
�B
�B
�B
�B
�B
�B
�B
xB
�B
�B
xB
xB
�B
�B
CB
/B
~B
5B
�B
;B
 BB
!-B
"hB
"�B
"hB
"�B
#�B
#�B
#�B
$�B
%`B
%�B
%�B
&2B
&fB
&fB
&�B
'B
'8B
'RB
'8B
'�B
'�B
(XB
)B
)�B
*�B
*�B
*�B
+B
+�B
,=B
,"B
,�B
,�B
,�B
-B
-wB
-wB
-�B
.}B
.�B
/ B
/OB
/iB
/�B
0!B
0oB
0UB
0;B
0�B
1B
1B
0�B
1'B
1'B
1AB
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2-B
2B
2-B
2-B
2B
2aB
3B
3�B
3�B
4B
4B
4B
49B
4B
3�B
4B
4B
4B
4nB
4�B
4�B
5B
5�B
5�B
6�B
7LB
7�B
8B
8B
8�B
9XB
9XB
9�B
:*B
:�B
:�B
;�B
;�B
;�B
;�B
;B
<B
<jB
<�B
<�B
<�B
=B
=�B
>B
>]B
>wB
>wB
>�B
>wB
>wB
>BB
?HB
?�B
?}B
?cB
@ B
@4B
@OB
@�B
AB
AUB
AUB
A�B
B'B
B'B
B[B
B[B
B'B
BAB
B�B
C�B
C�B
D3B
D�B
D�B
EB
EB
EB
D�B
E9B
EmB
E�B
E�B
FB
F?B
FYB
F�B
F�B
GB
G_B
G�B
G�B
HB
HB
G�B
HKB
H�B
J#B
J�B
J�B
J�B
K)B
K�B
KxB
KxB
KxB
K�B
K�B
K�B
LB
LJB
L~B
LdB
LdB
L�B
L�B
L�B
M6B
M�B
N<B
NpB
NVB
N<B
NpB
N�B
OB
OBB
OBB
O\B
OB
O�B
PHB
O�B
P}B
Q�B
R:B
R B
R�B
S�B
S�B
S�B
S�B
TB
TFB
T�B
T�B
UMB
U�B
U�B
VSB
VmB
WYB
WsB
W�B
W�B
W�B
W�B
XB
W�B
XyB
X�B
X�B
YeB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
[	B
[	B
\]B
\�B
\�B
\�B
\�B
\xB
\�B
]dB
]dB
]~B
]�B
]�B
^B
^5B
^B
^OB
_;B
_!B
_!B
_;B
_VB
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a|B
aHB
a|B
a�B
a�B
bB
bhB
bhB
b�B
b�B
b�B
b�B
b�B
cB
cB
cB
cnB
c�B
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gRB
g8B
g�B
h$B
h�B
h�B
h�B
i*B
i*B
i*B
i�B
i�B
jeB
jeB
j�B
j�B
j�B
kkB
k�B
lWB
lWB
lWB
lWB
l�B
l�B
mCB
mCB
m]B
m]B
m�B
m�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
o B
o B
o B
o5B
oB
oiB
o�B
o�B
o�B
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
rGB
r|B
r|B
r|B
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
tB
tB
tB
t�B
tnB
t�B
t�B
u%B
utB
u�B
u�B
u�B
vB
vB
v+B
v+B
vFB
v�B
v�B
v�B
v�B
v�B
wB
w2B
wLB
wLB
w�B
w�B
w�B
w�B
xB
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y	B
x�B
y	B
y	B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
zxB
zxB
zxB
z^B
z�B
z�B
{B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�sB	�sB	�YB	�sB	�YB	��B	�
B	��B	��B	�yB	�EB	�sB	�
B	��B	�$B	�_B	��B	�B	�1B	�B	�QB	��B	�BB	��B	�$B	��B	�gB	�"B	�uB	�B	یB	��B	ݘB	��B	��B
#:B
0UB
2GB
@ B
E�B
F�B
S�B
o B
|�B
��B
�B
��B
�B
�{B
�]B�BgBQ�B`B]IBf�B�B�eB��B�lB�B�IB��B��B�#B��B�gBr�Bn�Bf�B[qBU�BO\B7�BKB�B
��B
�B
��B
��B
��B
uZB
kkB
c�B
^�B
sB
i�B
RTB
="B
.B	�B	�/B	��B	�B	�DB	q�B	YKB	FtB	+B	�B	B	jB	B	�B		B	]B	.cB	0�B	C{B	=�B	?�B	:*B	=�B	p;B	�=B	��B	�,B	��B	�[B	��B	��B	��B	��B	ּB	�KB
MB
bB
�B
B	�(B	��B	�]B	�5B	�[B	��B	��B	��B	�;B	��B	�B	�oB	āB	�B	�B	ɠB	�#B	��B	бB	��B	�+B	�gB	��B	ЗB	͟B	�B	�	B	��B	�XB	ʌB	ΥB	�uB	ڠB	�KB	��B
�B
�B
B
�B
(B
B
TB
�B
sB
�B
�B
B
 \B
 �B
"�B
&LB
)B
)DB
)yB
(�B
'B
'RB
)*B
*0B
)�B
(�B
(�B
&�B
$@B
"B
'�B
'�B
*B
1'B
2�B
33B
3�B
5ZB
7�B
7�B
6�B
5tB
3�B
0!B
+B
-�B
-wB
(�B
$�B
"NB
 vB
�B
�B
�B
/B
]B
�B
=B
�B
7B
�B
�B
�B
B
KB
�B
�B
�B
�B
jB
 'B
pB
�B
 �B
 B
VB
dB
�B
�B
�B
B
SB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
WB
=B
�B
WB
�B
kB
�B
B
�B
�B
�B
MB
�B
MB
�B
�B
mB
�B
�B
SB
$B
�B
�B
�B
SB
MB
�B
�B
�B
�B
�B
hB
�B
�B
B
�B
(B
VB
jB
�B
�B

�B

�B
	�B
	B
�B
�B
�B
�B
B
3B
�B
?B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
uB
 �B
 �B
 �B
UB
 B
 �B	��B
B
B
oB
 �B
 �B
 �B
 �B
 B
 B	�HB	��B	��B	�HB
  B
 �B
 �B	��B
 B
UB
9B
�B
B
'B
�B
MB
�B
�B
 �B
 B
 �B
 OB
 �B
oB
oB
�B
�B
�B
AB
'B
uB
�B
�B
�B
�B
B
KB
fB
�B
1B
B
�B
+B
?B
B
�B
B
EB
)B
�B
�B
B
�B
�B
�B
�B
�B
~B
�B
�B
PB
6B
6B
6B
B
�B
�B
JB
~B
�B
�B
~B
jB
B
�B
�B
�B
�B
�B
VB
�B
~B
dB
JB
0B
�B
�B
�B
xB
�B
�B
�B
�B

�B

rB

XB

�B
^B
0B
"B
�B
�B
�B
�B
BB
�B
�B
�B
<B
�B
�B
�B
B
<B
VB
�B
BB
�B
.B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
 B
 B
oB
&B
uB
uB
�B
�B
�B
B
�B
�B
�B
�B
SB
�B
�B
�B
�B

B
sB
�B
�B
�B
�B
+B
_B
�B
�B
�B
B
B
�B
�B
kB
�B
�B
�B
�B
B
B
B
)B
B
]B
)B
]B
CB
)B
)B
)B
B
�B
�B
B
B
�B
�B
~B
�B
~B
dB
/B
/B
B
�B
�B
�B
�B
�B
�B
�B
�B
xB
�B
�B
xB
xB
�B
�B
CB
/B
~B
5B
�B
;B
 BB
!-B
"hB
"�B
"hB
"�B
#�B
#�B
#�B
$�B
%`B
%�B
%�B
&2B
&fB
&fB
&�B
'B
'8B
'RB
'8B
'�B
'�B
(XB
)B
)�B
*�B
*�B
*�B
+B
+�B
,=B
,"B
,�B
,�B
,�B
-B
-wB
-wB
-�B
.}B
.�B
/ B
/OB
/iB
/�B
0!B
0oB
0UB
0;B
0�B
1B
1B
0�B
1'B
1'B
1AB
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2-B
2B
2-B
2-B
2B
2aB
3B
3�B
3�B
4B
4B
4B
49B
4B
3�B
4B
4B
4B
4nB
4�B
4�B
5B
5�B
5�B
6�B
7LB
7�B
8B
8B
8�B
9XB
9XB
9�B
:*B
:�B
:�B
;�B
;�B
;�B
;�B
;B
<B
<jB
<�B
<�B
<�B
=B
=�B
>B
>]B
>wB
>wB
>�B
>wB
>wB
>BB
?HB
?�B
?}B
?cB
@ B
@4B
@OB
@�B
AB
AUB
AUB
A�B
B'B
B'B
B[B
B[B
B'B
BAB
B�B
C�B
C�B
D3B
D�B
D�B
EB
EB
EB
D�B
E9B
EmB
E�B
E�B
FB
F?B
FYB
F�B
F�B
GB
G_B
G�B
G�B
HB
HB
G�B
HKB
H�B
J#B
J�B
J�B
J�B
K)B
K�B
KxB
KxB
KxB
K�B
K�B
K�B
LB
LJB
L~B
LdB
LdB
L�B
L�B
L�B
M6B
M�B
N<B
NpB
NVB
N<B
NpB
N�B
OB
OBB
OBB
O\B
OB
O�B
PHB
O�B
P}B
Q�B
R:B
R B
R�B
S�B
S�B
S�B
S�B
TB
TFB
T�B
T�B
UMB
U�B
U�B
VSB
VmB
WYB
WsB
W�B
W�B
W�B
W�B
XB
W�B
XyB
X�B
X�B
YeB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
[	B
[	B
\]B
\�B
\�B
\�B
\�B
\xB
\�B
]dB
]dB
]~B
]�B
]�B
^B
^5B
^B
^OB
_;B
_!B
_!B
_;B
_VB
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a|B
aHB
a|B
a�B
a�B
bB
bhB
bhB
b�B
b�B
b�B
b�B
b�B
cB
cB
cB
cnB
c�B
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gRB
g8B
g�B
h$B
h�B
h�B
h�B
i*B
i*B
i*B
i�B
i�B
jeB
jeB
j�B
j�B
j�B
kkB
k�B
lWB
lWB
lWB
lWB
l�B
l�B
mCB
mCB
m]B
m]B
m�B
m�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
o B
o B
o B
o5B
oB
oiB
o�B
o�B
o�B
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
rGB
r|B
r|B
r|B
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
tB
tB
tB
t�B
tnB
t�B
t�B
u%B
utB
u�B
u�B
u�B
vB
vB
v+B
v+B
vFB
v�B
v�B
v�B
v�B
v�B
wB
w2B
wLB
wLB
w�B
w�B
w�B
w�B
xB
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y	B
x�B
y	B
y	B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
zxB
zxB
zxB
z^B
z�B
z�B
{B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105241  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192226  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192227  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192227                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042235  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042235  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                