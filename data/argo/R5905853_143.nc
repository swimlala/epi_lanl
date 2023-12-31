CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-08T21:41:38Z creation;2023-01-08T21:41:39Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230108214138  20230108222616  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�����~1   @��2��@.��vȴ�c�hr�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBpffBxffB�  B�ffB���B�  B�  B�  B�33B���B�  B���B�  B���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C L�C  C�fC�C�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"�C$�C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV33CW��CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB`(�Bh(�Bp(�Bx(�BB�G�B��B��HB��HB��HB�{B��B��HB��B��HB��B��B��HB��HB�z�B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC =qC�C�
C
>C�
C	�
C�C�C�C�C�C�C�C�C�C�C�C"
>C$
>C%�
C'�
C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CV#�CW�qCY�C[�C]�C_�Ca�
Cc�
Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN��DO�DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Diu�Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}��D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD�GD�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�n1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��A��A��vA��vA��A���A��A���A���A���A��2A��	A���A���A��"A��A���A��A��A���A���A��DA��%A���A��`A��DA��.A�~A��Aͷ�A�f�A�>BA��pA�Y�A�#�A�A��A���A�{A��A�	A��BA�=qA�YA�уA��A�R�A��,A��tA���A�l�A���A��iA�kQA��XA��bA��oA��xA���A��]A��0A�=�A��A�_A���A�ȀA�y�A���A��A�+A��A��VA�k�A���A�jKA��AyOAo�Ah�1AeVAa(�A_:�A^��A^jA]�A\c�AXw2ASzxAOȴAL��AK��AI�$AG�AC�FABSA>��A<�!A:YKA8��A7A55�A3�A1�A1�A/A.��A,E�A*_pA*��A+  A&�RA$��A$C-A"��A"%A ��A!��A#�A$dZA$U2A#��A"�0A"�<A!�;A"xA!�+A!��A!7�A!xA �A ��A �:A ��A _�A��Al�A�A��AF�A��A�oA�A��AGA�FA�wAƨAqA�3A��AG�A�A{�A� AU�A5�A�,Au�A��A(�A��AѷA��Ad�AA��A�FA�=A�fA-�A��A,�A��A@OA�XA+A�}A��A�.A�HAVA
�uA
)_A	��A�A�_A]�A%A;A�AAc A֡A)_Ac A�A�FA?}A�A%�A��A�AGEA �1@�ϫ@���@�!�@�2�@��@���@��@��@�?}@�u%@��@�+@��o@�rG@��@���@�U2@��@��Q@�n/@���@�y>@�!@��N@�9�@�"�@�Y@�E9@��@�^5@�P@� @�?�@�Dg@��@�{@��@��d@�h
@�e@�*�@���@��@��@�,�@�p;@�%�@�}�@���@㧇@�x@�e@�&�@��K@ߎ�@�_@�o @�P�@܉�@�4@�z�@�|�@��@ئL@�4@׎"@��@�e�@շ�@Ծ�@�oi@�]d@��@�ѷ@�?@��@��@���@��D@�S&@�(@΀�@ͷ�@�#�@��@�u%@�8�@��)@��@ˤ@@ʸR@ɸ�@ȯO@�+@�m�@Ÿ�@�iD@��@��@ģ�@�0U@ô�@�6z@¤�@�[�@�6�@�7@��r@���@�G�@���@�YK@��H@�K�@��@���@� �@��h@�j�@���@�V@���@��@��"@�s@�(�@�d�@�!�@��z@���@���@��h@���@�_@�~@��q@��B@�&�@�	�@��F@��k@�C�@��E@�GE@���@���@�!�@�@��@���@��u@�a|@��.@�w2@��|@��U@�e�@��m@�hs@���@�L0@�;�@�7@��}@�\)@��p@�tT@�\�@�[�@�I�@�$@���@�M@��@�O@�-@�8�@�M@�V@�e�@�d�@�1'@��-@���@�U�@��E@�	�@���@�B�@���@���@�g�@���@��H@��b@���@�g8@�Ft@���@��'@�n/@�%F@���@��Y@��>@�+@��@�V�@�$�@�_@��N@��@�:�@���@�L0@�s�@�9�@��@��F@���@���@��@���@��-@��V@���@�IR@��8@���@�Xy@��@��>@��6@�b�@��c@��<@�bN@��@��@��@��@@�e�@�*0@���@��9@�p;@��@��#@�+@��@�,�@�#�@���@��I@�7@���@��:@�K�@���@�PH@�,=@�:*@�-@��@�-@�2�@�2�@�~@�{@��@���@���@���@�l�@�!-@��M@��E@��@�h
@�U2@�<�@�$�@�1@��&@�x�@�P�@�+@���@�_@��@��@�{J@�!-@��@���@�_�@��@��n@���@��<@���@�,=@��@���@�_p@��@��@�Z@�R�@�I�@� �@��A@��W@��*@��@��9@�{�@�/�@��@�ԕ@��{@�_p@�V@��[@��@���@�oi@�W�@�@��q@��~@�?}@��m@�\�@�-@��T@��X@�O�@� i@��v@���@��}@���@��m@���@�Ov@��]@��@��K@��@�,�@���@�h�@�R�@�3�@�&@~�@~$�@}��@}4@|��@|(�@{��@{|�@{S�@zߤ@y�X@y	l@xm�@x@w��@v��@u�H@t�@t<�@s��@sC@r�h@r�b@rW�@qԕ@qj@qq@p�e@p�@pl"@pM@p�@o�{@o�@n��@n+k@m��@mT�@l�@l�$@k�$@k i@ju%@j_@is�@i�@i�@h$@g��@hx@h(�@hM@hH@g�]@g��@gE9@f��@f��@f�@e0�@dr�@d  @c�F@c�k@cP�@b��@b�@b�@a�n@as�@a2a@`�@`�u@`Z@_�0@_|�@_W?@_@^��@^&�@]�^@]0�@\��@\ �@[��@[U�@Z��@Zxl@Z;�@Y��@Y:�@X��@XM@X�@W�	@Wn/@Wb�@W+@V��@V�m@V{�@V�@U��@U�@U��@U�S@U|@UY�@UQ�@UO�@U�@T��@Tg8@S�@Sy�@S$t@R�<@Rd�@R@Q��@Qzx@Q?}@P�	@P�Y@P~@O��@Oe�@O.I@O i@N�@N�!@N$�@M��@Mj@L�P@LtT@L@Ks@K�@J��@J��@I��@Iq@H��@H��@HM@H �@G��@G�@G.I@F�@F��@F~�@F;�@F&�@E�@E=�@D��@D��@Dc�@D/�@C�]@C�Q@C��@C��@C��@C i@Bh
@B+k@A��@A�@A��@A&�@@�	@@֡@@r�@@1@?dZ@>��@>�H@>͟@>�@>c @>&�@>J@=�>@=��@=e,@=%F@<�/@<�I@<PH@<-�@<x@;�m@;��@;S�@:�@:6�@9�D@9�d@9��@9Vm@9�@8j@8�@7�V@7@6�2@6��@6�L@6Q@5�j@5x�@5hs@50�@5+@5;@4��@4l"@3�Q@3b�@36z@2��@2z@2 �@1�h@1u�@1X@1!�@1	l@0h�@0!@0�@/��@/��@/_p@/9�@/Y@.��@.0U@-�N@-��@-�@-L�@-�@,��@,�?@,�@,S�@,G@+��@+~�@+/�@*��@*ff@*�@)�@)�h@)Dg@)�@(��@(֡@(�p@(�?@(��@(�@(y>@(Xy@(�@'�Q@'�*@'e�@'/�@'�@'�@&�]@&��@&C�@&@%�@%��@%e,@%L�@$��@$w�@$:�@#��@#�6@#�K@#�a@#��@#��@#'�@"�@"��@"�F@"��@"s�@"h
@"B[@"@!�z@!�@!w2@!4@!�@ �@ �@ A�@ 2�@ !@��@��@�$@'�@��@\�@B[@�@ԕ@��@s�@G�@�@�?@u�@H@2�@�@˒@��@+@��@�L@� @~�@6�@@�@�H@�X@[W@�@�j@~(@bN@x@� @�@S�@)_@��@�s@��@��@Ta@�@�@j@Dg@*0@�f@��@��@�e@�I@�@Xy@%�@G@�Q@�@@o�@@O@�@�H@�!@^5@($@��@�@�N@rG@%F@�@�p@��@�@�.@oi@:�@'R@�@�@��@��@Z�@�@�8@�M@�@�\@$�@�3@��@m]@2a@�5@�O@r�@Z@K^@1'@~@�@��@��@a@O@A�@(@
�@
��@
��@
�R@
�6@
�\@
_�@
#:@
J@	��@	�3@	�t@	��@	8�@	!�@�@�)@��@�@H@<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��A��A��vA��vA��A���A��A���A���A���A��2A��	A���A���A��"A��A���A��A��A���A���A��DA��%A���A��`A��DA��.A�~A��Aͷ�A�f�A�>BA��pA�Y�A�#�A�A��A���A�{A��A�	A��BA�=qA�YA�уA��A�R�A��,A��tA���A�l�A���A��iA�kQA��XA��bA��oA��xA���A��]A��0A�=�A��A�_A���A�ȀA�y�A���A��A�+A��A��VA�k�A���A�jKA��AyOAo�Ah�1AeVAa(�A_:�A^��A^jA]�A\c�AXw2ASzxAOȴAL��AK��AI�$AG�AC�FABSA>��A<�!A:YKA8��A7A55�A3�A1�A1�A/A.��A,E�A*_pA*��A+  A&�RA$��A$C-A"��A"%A ��A!��A#�A$dZA$U2A#��A"�0A"�<A!�;A"xA!�+A!��A!7�A!xA �A ��A �:A ��A _�A��Al�A�A��AF�A��A�oA�A��AGA�FA�wAƨAqA�3A��AG�A�A{�A� AU�A5�A�,Au�A��A(�A��AѷA��Ad�AA��A�FA�=A�fA-�A��A,�A��A@OA�XA+A�}A��A�.A�HAVA
�uA
)_A	��A�A�_A]�A%A;A�AAc A֡A)_Ac A�A�FA?}A�A%�A��A�AGEA �1@�ϫ@���@�!�@�2�@��@���@��@��@�?}@�u%@��@�+@��o@�rG@��@���@�U2@��@��Q@�n/@���@�y>@�!@��N@�9�@�"�@�Y@�E9@��@�^5@�P@� @�?�@�Dg@��@�{@��@��d@�h
@�e@�*�@���@��@��@�,�@�p;@�%�@�}�@���@㧇@�x@�e@�&�@��K@ߎ�@�_@�o @�P�@܉�@�4@�z�@�|�@��@ئL@�4@׎"@��@�e�@շ�@Ծ�@�oi@�]d@��@�ѷ@�?@��@��@���@��D@�S&@�(@΀�@ͷ�@�#�@��@�u%@�8�@��)@��@ˤ@@ʸR@ɸ�@ȯO@�+@�m�@Ÿ�@�iD@��@��@ģ�@�0U@ô�@�6z@¤�@�[�@�6�@�7@��r@���@�G�@���@�YK@��H@�K�@��@���@� �@��h@�j�@���@�V@���@��@��"@�s@�(�@�d�@�!�@��z@���@���@��h@���@�_@�~@��q@��B@�&�@�	�@��F@��k@�C�@��E@�GE@���@���@�!�@�@��@���@��u@�a|@��.@�w2@��|@��U@�e�@��m@�hs@���@�L0@�;�@�7@��}@�\)@��p@�tT@�\�@�[�@�I�@�$@���@�M@��@�O@�-@�8�@�M@�V@�e�@�d�@�1'@��-@���@�U�@��E@�	�@���@�B�@���@���@�g�@���@��H@��b@���@�g8@�Ft@���@��'@�n/@�%F@���@��Y@��>@�+@��@�V�@�$�@�_@��N@��@�:�@���@�L0@�s�@�9�@��@��F@���@���@��@���@��-@��V@���@�IR@��8@���@�Xy@��@��>@��6@�b�@��c@��<@�bN@��@��@��@��@@�e�@�*0@���@��9@�p;@��@��#@�+@��@�,�@�#�@���@��I@�7@���@��:@�K�@���@�PH@�,=@�:*@�-@��@�-@�2�@�2�@�~@�{@��@���@���@���@�l�@�!-@��M@��E@��@�h
@�U2@�<�@�$�@�1@��&@�x�@�P�@�+@���@�_@��@��@�{J@�!-@��@���@�_�@��@��n@���@��<@���@�,=@��@���@�_p@��@��@�Z@�R�@�I�@� �@��A@��W@��*@��@��9@�{�@�/�@��@�ԕ@��{@�_p@�V@��[@��@���@�oi@�W�@�@��q@��~@�?}@��m@�\�@�-@��T@��X@�O�@� i@��v@���@��}@���@��m@���@�Ov@��]@��@��K@��@�,�@���@�h�@�R�@�3�@�&@~�@~$�@}��@}4@|��@|(�@{��@{|�@{S�@zߤ@y�X@y	l@xm�@x@w��@v��@u�H@t�@t<�@s��@sC@r�h@r�b@rW�@qԕ@qj@qq@p�e@p�@pl"@pM@p�@o�{@o�@n��@n+k@m��@mT�@l�@l�$@k�$@k i@ju%@j_@is�@i�@i�@h$@g��@hx@h(�@hM@hH@g�]@g��@gE9@f��@f��@f�@e0�@dr�@d  @c�F@c�k@cP�@b��@b�@b�@a�n@as�@a2a@`�@`�u@`Z@_�0@_|�@_W?@_@^��@^&�@]�^@]0�@\��@\ �@[��@[U�@Z��@Zxl@Z;�@Y��@Y:�@X��@XM@X�@W�	@Wn/@Wb�@W+@V��@V�m@V{�@V�@U��@U�@U��@U�S@U|@UY�@UQ�@UO�@U�@T��@Tg8@S�@Sy�@S$t@R�<@Rd�@R@Q��@Qzx@Q?}@P�	@P�Y@P~@O��@Oe�@O.I@O i@N�@N�!@N$�@M��@Mj@L�P@LtT@L@Ks@K�@J��@J��@I��@Iq@H��@H��@HM@H �@G��@G�@G.I@F�@F��@F~�@F;�@F&�@E�@E=�@D��@D��@Dc�@D/�@C�]@C�Q@C��@C��@C��@C i@Bh
@B+k@A��@A�@A��@A&�@@�	@@֡@@r�@@1@?dZ@>��@>�H@>͟@>�@>c @>&�@>J@=�>@=��@=e,@=%F@<�/@<�I@<PH@<-�@<x@;�m@;��@;S�@:�@:6�@9�D@9�d@9��@9Vm@9�@8j@8�@7�V@7@6�2@6��@6�L@6Q@5�j@5x�@5hs@50�@5+@5;@4��@4l"@3�Q@3b�@36z@2��@2z@2 �@1�h@1u�@1X@1!�@1	l@0h�@0!@0�@/��@/��@/_p@/9�@/Y@.��@.0U@-�N@-��@-�@-L�@-�@,��@,�?@,�@,S�@,G@+��@+~�@+/�@*��@*ff@*�@)�@)�h@)Dg@)�@(��@(֡@(�p@(�?@(��@(�@(y>@(Xy@(�@'�Q@'�*@'e�@'/�@'�@'�@&�]@&��@&C�@&@%�@%��@%e,@%L�@$��@$w�@$:�@#��@#�6@#�K@#�a@#��@#��@#'�@"�@"��@"�F@"��@"s�@"h
@"B[@"@!�z@!�@!w2@!4@!�@ �@ �@ A�@ 2�@ !@��@��@�$@'�@��@\�@B[@�@ԕ@��@s�@G�@�@�?@u�@H@2�@�@˒@��@+@��@�L@� @~�@6�@@�@�H@�X@[W@�@�j@~(@bN@x@� @�@S�@)_@��@�s@��@��@Ta@�@�@j@Dg@*0@�f@��@��@�e@�I@�@Xy@%�@G@�Q@�@@o�@@O@�@�H@�!@^5@($@��@�@�N@rG@%F@�@�p@��@�@�.@oi@:�@'R@�@�@��@��@Z�@�@�8@�M@�@�\@$�@�3@��@m]@2a@�5@�O@r�@Z@K^@1'@~@�@��@��@a@O@A�@(@
�@
��@
��@
�R@
�6@
�\@
_�@
#:@
J@	��@	�3@	�t@	��@	8�@	!�@�@�)@��@�@H@<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	ݲB	��B	��B	��B	��B	��B	�B	��B	ܬB	��B	��B	��B	��B	�/B	��B	��B	�B	��B	��B	�pB	�VB	�!B	�!B	��B	�;B	�5B	�~B	�dB	��B	�B	��B	��B

�B
a�B
�,B
��B
�B
��B
� B
�FBoB{B
��B
�B�B>�BMBCaBA�B@�B@�BC�BJ�BR�B]�BJ	B/iB(
B �B1BBJB
��B
��B
�B
��B
�6B
ϑB
�	B
.B
q[B
^�B
6+B
�B
	�B
{B
�B	�lB	�B	��B	�IB	f�B	QNB	B�B	1�B	&�B	$�B	#:B	�B	�B	+B��B��B��B�rBÖB�B��BāB�(B��B��B��B��B�]B�zB��B�fB�B��B��B�;B�~B	zB�&B�B�;B��B��B�B	NB	33B	Y1B	n�B	w�B	r�B	�AB	�3B	��B	�B	�hB	��B	��B	��B	��B	�B	�6B	��B	��B	ɠB	ʌB	�DB	��B	̈́B	οB	͹B	�~B	ʦB	ɺB	ʦB	ǮB	��B	��B	�?B	ɆB	˒B	ʌB	�lB	�B	�B	�B	�-B	��B	�;B	��B	��B	�B	��B	��B	��B	�BB	��B	��B	��B	��B	�B	��B	�XB	�`B	��B	�nB	��B	��B	�GB	�%B	�hB	�B	�OB	�WB	��B	�QB	�"B	�B	��B	�zB	��B	�HB	��B	�B	�YB	��B	�$B	�$B	�?B	�YB	�sB	�EB	��B	�KB	��B	��B	��B	��B	�mB	�
B	�`B	��B	�TB	��B	�CB	�}B	��B	�B	��B	�B	�B	�"B	��B	�5B	�!B	�vB	�B	��B	��B	��B	�}B	ƎB	��B	�(B	��B	�VB	�B	��B	��B	�B	��B	�B	�'B	�3B	ÖB	��B	��B	ªB	��B	�SB	��B	�VB	�B	ҽB	�.B	��B	�SB	�B	�aB	��B	�)B	ǔB	żB	�gB	�gB	�B	�9B	�B	��B	�3B	�B	�gB	ĶB	ĜB	��B	żB	��B	�B	��B	��B	ǮB	�1B	�1B	�lB	�#B	�DB	��B	�~B	�6B	��B	�YB	ԯB	ѷB	�HB	�\B	�}B	�HB	ѝB	�B	ӏB	��B	՛B	�$B	��B	��B	ٚB	�B	�7B	��B	��B	یB	�WB	�#B	�qB	�CB	�=B	ۦB	��B	�IB	ݲB	ݲB	��B	ߊB	��B	��B	�B	�B	��B	��B	�TB	�ZB	�B	��B	�fB	�B	�
B	�
B	�RB	��B	��B	�0B	�B	�B	�QB	�"B	�wB	��B	�B	�B	�5B	�5B	�B	��B	�!B	�B	�B	�B	�-B	�|B	�3B	�B	�9B	�B	�B	�TB	��B	��B	��B	��B	��B	�B	�zB	��B	�8B	�*B	��B	�B	��B	�wB	�.B	��B
 �B
 �B
AB
{B
�B
�B
MB
B
3B
B
�B
B
YB
mB
SB
SB
�B
�B
EB
�B
	RB
	lB

=B

�B

�B

rB
�B
�B

�B
^B
^B

�B

�B

XB
	�B
	�B
	lB
	B
�B
	RB
)B
~B
~B
�B
6B
�B
�B
�B
�B
HB
bB
�B
�B
�B
�B
bB
HB
�B
 B
4B
4B
�B
NB
NB
hB
NB
hB
4B
�B
 B
[B
aB
�B
�B
�B
�B
B
B
�B
�B
�B
,B
�B
_B
KB
�B
B
kB
WB
�B
]B
IB
�B
�B
B
�B
�B
B
5B
B
�B
�B
 �B
 �B
!B
!|B
!bB
"�B
#�B
#nB
#nB
#B
#nB
$&B
$B
#�B
#�B
$&B
$@B
%,B
%`B
$�B
%�B
%�B
&�B
'8B
'�B
'RB
'�B
'mB
'�B
'mB
'mB
'B
'mB
(�B
)_B
)_B
)�B
)�B
*�B
+�B
+�B
,"B
,WB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,=B
+�B
,�B
,�B
-�B
.cB
.cB
.IB
.cB
.�B
0;B
1B
1�B
2|B
2�B
2aB
2aB
33B
4B
4B
4B
4B
4nB
4�B
5?B
5%B
5?B
5ZB
5tB
5�B
5ZB
4�B
5%B
4�B
4�B
4�B
5?B
5ZB
5B
3�B
3hB
3B
2�B
3�B
3MB
33B
3�B
49B
4�B
4�B
5B
5%B
5?B
5?B
5tB
6`B
6�B
7B
7�B
7�B
88B
8RB
8lB
9	B
9$B
9�B
9�B
9�B
:�B
;�B
<PB
<B
<�B
<�B
=�B
?�B
@ B
@4B
@�B
@�B
@�B
AB
AB
A�B
AoB
A�B
BAB
BB
BuB
B�B
D3B
DMB
DgB
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
FtB
F�B
F�B
GEB
GzB
G+B
GEB
GEB
H1B
H�B
H�B
H�B
IB
H�B
H�B
IB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J=B
J�B
J�B
KB
KB
J�B
K^B
KDB
KDB
KDB
K�B
K�B
K�B
MB
MB
M�B
M�B
NB
N<B
N�B
OBB
O�B
P.B
PHB
PbB
P}B
P}B
Q B
P�B
QNB
RTB
RoB
RB
RTB
RTB
RTB
R�B
S�B
S@B
S�B
TB
T�B
U2B
U�B
VB
V9B
VB
V9B
VSB
V�B
VmB
V�B
WYB
W�B
W�B
W�B
W�B
XB
X+B
X+B
XB
XB
XyB
XyB
XyB
X�B
X�B
YB
Y�B
Z7B
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\xB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]IB
^5B
^�B
^�B
_!B
_pB
_�B
_�B
_�B
_pB
_pB
`B
_�B
`�B
`�B
`�B
`�B
abB
a�B
bNB
cTB
cnB
c�B
c�B
c�B
d&B
dB
dZB
dZB
d@B
d�B
eB
e�B
f2B
f2B
fLB
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h$B
h>B
h>B
h�B
h�B
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
jB
kB
kQB
kkB
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mCB
m�B
m�B
m�B
nB
nB
nIB
ncB
n�B
oB
oB
o5B
o�B
oiB
o�B
pUB
poB
qB
qB
p�B
p�B
p�B
q[B
q�B
q�B
rB
r-B
r-B
rGB
rGB
r|B
r�B
r�B
r�B
sB
sMB
s�B
s�B
s�B
tTB
t9B
tTB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
wLB
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
y	B
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y�B
y�B
zDB
zxB
z�B
{B
z�B
{JB
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
}"B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
B
cB
�B
�B
�B
�OB
�4B
�OB
��B
��B
�B
�UB
��B
��B
��B
��B
�B
�B
�B
�'B
�AB
��B
��B
�-B
�B
��B
��B
�{B
��B
�MB
�MB
��B
��B
��B
�B
�SB
�SB
�mB
��B
��B
��B
��B
�YB
�tB
��B
��B
��B
��B
�+B
�_B
�EB
�EB
�_B
��B
��B
��B
�KB
�KB
�fB
��B
�7B
�7B
��B
��B
��B
��B
�	B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	ݲB	��B	��B	��B	��B	��B	�B	��B	ܬB	��B	��B	��B	��B	�/B	��B	��B	�B	��B	��B	�pB	�VB	�!B	�!B	��B	�;B	�5B	�~B	�dB	��B	�B	��B	��B

�B
a�B
�,B
��B
�B
��B
� B
�FBoB{B
��B
�B�B>�BMBCaBA�B@�B@�BC�BJ�BR�B]�BJ	B/iB(
B �B1BBJB
��B
��B
�B
��B
�6B
ϑB
�	B
.B
q[B
^�B
6+B
�B
	�B
{B
�B	�lB	�B	��B	�IB	f�B	QNB	B�B	1�B	&�B	$�B	#:B	�B	�B	+B��B��B��B�rBÖB�B��BāB�(B��B��B��B��B�]B�zB��B�fB�B��B��B�;B�~B	zB�&B�B�;B��B��B�B	NB	33B	Y1B	n�B	w�B	r�B	�AB	�3B	��B	�B	�hB	��B	��B	��B	��B	�B	�6B	��B	��B	ɠB	ʌB	�DB	��B	̈́B	οB	͹B	�~B	ʦB	ɺB	ʦB	ǮB	��B	��B	�?B	ɆB	˒B	ʌB	�lB	�B	�B	�B	�-B	��B	�;B	��B	��B	�B	��B	��B	��B	�BB	��B	��B	��B	��B	�B	��B	�XB	�`B	��B	�nB	��B	��B	�GB	�%B	�hB	�B	�OB	�WB	��B	�QB	�"B	�B	��B	�zB	��B	�HB	��B	�B	�YB	��B	�$B	�$B	�?B	�YB	�sB	�EB	��B	�KB	��B	��B	��B	��B	�mB	�
B	�`B	��B	�TB	��B	�CB	�}B	��B	�B	��B	�B	�B	�"B	��B	�5B	�!B	�vB	�B	��B	��B	��B	�}B	ƎB	��B	�(B	��B	�VB	�B	��B	��B	�B	��B	�B	�'B	�3B	ÖB	��B	��B	ªB	��B	�SB	��B	�VB	�B	ҽB	�.B	��B	�SB	�B	�aB	��B	�)B	ǔB	żB	�gB	�gB	�B	�9B	�B	��B	�3B	�B	�gB	ĶB	ĜB	��B	żB	��B	�B	��B	��B	ǮB	�1B	�1B	�lB	�#B	�DB	��B	�~B	�6B	��B	�YB	ԯB	ѷB	�HB	�\B	�}B	�HB	ѝB	�B	ӏB	��B	՛B	�$B	��B	��B	ٚB	�B	�7B	��B	��B	یB	�WB	�#B	�qB	�CB	�=B	ۦB	��B	�IB	ݲB	ݲB	��B	ߊB	��B	��B	�B	�B	��B	��B	�TB	�ZB	�B	��B	�fB	�B	�
B	�
B	�RB	��B	��B	�0B	�B	�B	�QB	�"B	�wB	��B	�B	�B	�5B	�5B	�B	��B	�!B	�B	�B	�B	�-B	�|B	�3B	�B	�9B	�B	�B	�TB	��B	��B	��B	��B	��B	�B	�zB	��B	�8B	�*B	��B	�B	��B	�wB	�.B	��B
 �B
 �B
AB
{B
�B
�B
MB
B
3B
B
�B
B
YB
mB
SB
SB
�B
�B
EB
�B
	RB
	lB

=B

�B

�B

rB
�B
�B

�B
^B
^B

�B

�B

XB
	�B
	�B
	lB
	B
�B
	RB
)B
~B
~B
�B
6B
�B
�B
�B
�B
HB
bB
�B
�B
�B
�B
bB
HB
�B
 B
4B
4B
�B
NB
NB
hB
NB
hB
4B
�B
 B
[B
aB
�B
�B
�B
�B
B
B
�B
�B
�B
,B
�B
_B
KB
�B
B
kB
WB
�B
]B
IB
�B
�B
B
�B
�B
B
5B
B
�B
�B
 �B
 �B
!B
!|B
!bB
"�B
#�B
#nB
#nB
#B
#nB
$&B
$B
#�B
#�B
$&B
$@B
%,B
%`B
$�B
%�B
%�B
&�B
'8B
'�B
'RB
'�B
'mB
'�B
'mB
'mB
'B
'mB
(�B
)_B
)_B
)�B
)�B
*�B
+�B
+�B
,"B
,WB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,=B
+�B
,�B
,�B
-�B
.cB
.cB
.IB
.cB
.�B
0;B
1B
1�B
2|B
2�B
2aB
2aB
33B
4B
4B
4B
4B
4nB
4�B
5?B
5%B
5?B
5ZB
5tB
5�B
5ZB
4�B
5%B
4�B
4�B
4�B
5?B
5ZB
5B
3�B
3hB
3B
2�B
3�B
3MB
33B
3�B
49B
4�B
4�B
5B
5%B
5?B
5?B
5tB
6`B
6�B
7B
7�B
7�B
88B
8RB
8lB
9	B
9$B
9�B
9�B
9�B
:�B
;�B
<PB
<B
<�B
<�B
=�B
?�B
@ B
@4B
@�B
@�B
@�B
AB
AB
A�B
AoB
A�B
BAB
BB
BuB
B�B
D3B
DMB
DgB
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
FtB
F�B
F�B
GEB
GzB
G+B
GEB
GEB
H1B
H�B
H�B
H�B
IB
H�B
H�B
IB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J=B
J�B
J�B
KB
KB
J�B
K^B
KDB
KDB
KDB
K�B
K�B
K�B
MB
MB
M�B
M�B
NB
N<B
N�B
OBB
O�B
P.B
PHB
PbB
P}B
P}B
Q B
P�B
QNB
RTB
RoB
RB
RTB
RTB
RTB
R�B
S�B
S@B
S�B
TB
T�B
U2B
U�B
VB
V9B
VB
V9B
VSB
V�B
VmB
V�B
WYB
W�B
W�B
W�B
W�B
XB
X+B
X+B
XB
XB
XyB
XyB
XyB
X�B
X�B
YB
Y�B
Z7B
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\xB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]IB
^5B
^�B
^�B
_!B
_pB
_�B
_�B
_�B
_pB
_pB
`B
_�B
`�B
`�B
`�B
`�B
abB
a�B
bNB
cTB
cnB
c�B
c�B
c�B
d&B
dB
dZB
dZB
d@B
d�B
eB
e�B
f2B
f2B
fLB
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h$B
h>B
h>B
h�B
h�B
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
jB
kB
kQB
kkB
k�B
k�B
l"B
l=B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mCB
m�B
m�B
m�B
nB
nB
nIB
ncB
n�B
oB
oB
o5B
o�B
oiB
o�B
pUB
poB
qB
qB
p�B
p�B
p�B
q[B
q�B
q�B
rB
r-B
r-B
rGB
rGB
r|B
r�B
r�B
r�B
sB
sMB
s�B
s�B
s�B
tTB
t9B
tTB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
wLB
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
y	B
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y�B
y�B
zDB
zxB
z�B
{B
z�B
{JB
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
}"B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
B
cB
�B
�B
�B
�OB
�4B
�OB
��B
��B
�B
�UB
��B
��B
��B
��B
�B
�B
�B
�'B
�AB
��B
��B
�-B
�B
��B
��B
�{B
��B
�MB
�MB
��B
��B
��B
�B
�SB
�SB
�mB
��B
��B
��B
��B
�YB
�tB
��B
��B
��B
��B
�+B
�_B
�EB
�EB
�_B
��B
��B
��B
�KB
�KB
�fB
��B
�7B
�7B
��B
��B
��B
��B
�	B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230108214118  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230108214138  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230108214139  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230108214139                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230108214140  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230108214140  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230108222616                      G�O�G�O�G�O�                