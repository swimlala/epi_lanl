CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:18:57Z creation;2022-06-04T19:18:58Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191857  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���c�1   @��DDD@0��v��cp��
=q1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�33B˙�B���B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�33B�ffC�fC  C  C  C
  C  C  C  C  C�C33C�fC�fC  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2�C3��C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^L�C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@u@�{@�{Ap�A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HBîB�{B�z�BϮB��HB��HBܮB�G�B��HB��HB��HB��HB��HB��HB�{B�G�C�
C�C�C�C	�C�C�C�C�C
>C#�C�
C�
C�C�C�C!�C#�C%�C'�C)�C+�
C-�C/�C2
>C3�>C5�C7�
C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C\
>C^=qC_�
Ca�
Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD�GD�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�JA�	�A��A�A��A�GA�A�A���A��A���A��"A��vA�AμjAΕ�AΆ�A΄�A΃GA΂A�~�A�~(A�{�A�zDA�v�A�pA�k�A�ZA�IA�LdA�/OA�#nA�uÁoA�R�A�U2A�@�A�"4A��+A�˒Aˮ�Aˊ=A�v`A�#�A���A�YA�3�A�� AǜxA�dZAù�A��&A��A�$�A�p�A��1A�cA�o�A��JA��A�MA�{JA�)*A��LA�t�A�+�A�3�A�ȀA���A��sA�Z�A��A�>�A���A�  A�N�A�&LA�"�A���A�NpA���A��	A�˒A��"A��A�+kA��A�dZAw�sAv@Au4Ap^�Amy�Ak� Ai�KAe�HAaZ�A^�A\��A[qAX!�AQ'RAO��AOl�AOe�AN��AK��AGs�ADFtA@I�A<��A:֡A9�EA8͟A7-wA4,�A3(A07A.H�A,��A+d�A*,�A)��A(�AA'�A%c A$�HA$p�A#�'A"h
A!�rA!=qA!�0A"�A#4�A#L�A#7�A#>BA#�A!�&A _�A C�A 'RA �oA!_pA"DgA!�MA"/�A!p�A ɆA �~A�MA�]Aj�A�A�A�A+�A�jA4nA($A�pA8A7LA[WA1�A!A�AA�A��A��A��A�AzxA��A�A@OA��Al"A��A�	Av`A�bA��A�xA�DAS�A��A�A�wA
�PA
h�A
�A	�-A	N�A�AffA�FA�A�Au�A�mA��A��Al�A��A�A��A;A�A��AA�AAJA ��A 6�A 
�@��@�hs@���@�b@�S&@���@�J@� �@�m]@�M@�0�@�{�@��M@��@���@��z@��A@���@�[�@��+@�O�@�Ta@�c�@�@�O@�@�s�@�%@� @�i�@�>B@�,=@�f�@�e@���@�s�@�@��@��@�~�@��K@�W?@���@�j@�U2@�\�@��@睲@�]�@�A�@��@�b@��8@�ѷ@�[�@�@�@�7@�%@���@���@�~�@��@ާ�@��@ݞ�@�[W@�o@���@܈�@���@۩*@ۀ4@�0�@ڵ@�g8@��&@ٰ�@�k�@��8@�Ĝ@ع�@؛�@�@�@���@��@�t�@��5@�|�@��g@�˒@�c@Ի�@�K^@�@�#�@��H@�bN@���@ў�@�-w@��/@Ї+@�1'@϶F@ρ�@�W?@��c@ΐ.@�_@�-�@��@͓@�"�@��/@̜x@��@��@˽�@�o @�E9@�	l@ʜx@�H@�
�@��D@���@ɍP@�P�@�5�@��@�
=@��/@�e�@�+k@�5?@���@Ƿ@�[W@��@��[@�oi@�I�@���@��;@���@ų�@�b�@�L�@�>�@��@�ߤ@Īe@���@�q@�]d@�Q�@�@�
=@��@�	l@�҉@���@���@�C-@��@�4@�u%@�#:@��@��"@��@�*0@�v�@��r@���@�8@���@�L0@�@�G@���@�v`@�\)@��@���@�L0@��^@�A @��@���@�3�@��@��@�"�@���@�*�@��@�g�@�F@�+@���@�|�@���@���@�x@�<6@���@���@���@�4n@���@�m]@�$t@���@���@�:�@� �@���@�s@�T�@�&@��e@�8�@��@��o@���@���@���@���@�\�@��p@��@�c @�$@��A@���@���@��{@�`B@�?}@�
=@��@�J�@�_@�/@���@�i�@�	�@���@��P@�e,@��@��@��R@��r@��@���@�g�@�Dg@�*0@���@�֡@�҉@��?@��6@��A@�(�@�G@��@�rG@���@���@�N�@�J�@�*�@��N@��@��p@���@�Ft@��Q@�~�@�-w@��@��'@��}@��+@�'R@�s@��@��1@�Z�@�:�@��C@�_p@�N<@��@���@��X@���@�oi@�K^@�u@��@���@�	l@��@��F@��$@�|�@�?}@���@�H�@��Q@��@���@�C�@���@���@�M@�@��K@�@O@��U@�`�@��@�@@��@��8@��2@���@�z@� �@���@���@�{J@�F@���@���@��r@�Z�@���@��@�y�@�dZ@�=�@�@��M@���@�l�@�K^@�;�@�$�@�4@��@���@�,�@�ߤ@���@��@�h�@��@���@�\�@�,�@�%@���@��K@��@���@���@��I@�7@���@���@���@���@�;d@���@��@��<@���@�xl@�R�@�7�@��@��.@��Z@��9@��H@��4@�f�@�/�@��H@�oi@�Z�@�Ft@��j@��@�x@�+@���@��B@�{�@�?@�#:@�w@8@~��@~Q@~�@}�X@}p�@}+�@|�@|�/@|�)@|�@|(�@{�@{s@z�c@zd�@y��@y@@x�@xr�@w�@wݘ@w��@w_p@v��@v��@vv�@v0U@u�o@u��@uw2@u��@u@@te�@s��@s�@s�:@s>�@r~�@r.�@q�t@q2a@q�@p��@pM@o�@@o6z@o�@n�@nJ@m�@m@mrG@m+@l�`@l�.@lC-@l�@k�&@j��@j�x@j6�@j4@i��@i�t@i`B@i�@h�j@h��@hFt@g��@g�g@gMj@f�@f�1@fs�@f^5@fJ�@f!�@f4@e��@e7L@e�@d�@dh�@dK^@dD�@d<�@d*�@d�@c��@c|�@cMj@bߤ@b�+@b&�@a@a��@`�@`q@`7�@`,=@`  @_�A@_خ@_�F@_�	@_"�@^W�@]�Z@]��@]�M@\��@[�+@[�@[>�@Z�}@ZQ@Z@Y�9@Y��@Ys�@X��@X~(@X9X@Xx@W�6@W�@V}V@VJ@U�@T�4@S��@SZ�@S>�@S.I@S�@R�@R4@Q�S@QDg@P�@PbN@Oخ@N��@N��@N#:@M�@M�@M��@Mu�@MIR@M@L��@L��@L��@L�Y@L`�@K��@Ky�@J�R@I��@I��@I�3@I�t@Is�@I2a@H�$@H,=@Gƨ@G�@F�2@F�<@F�@F@�@E�@E�-@E�"@E:�@D�v@D_@C�
@CS�@C.I@B�@Bh
@B)�@A�@A��@A��@A�7@AS&@@�@@��@?��@?K�@>��@>�\@>($@=��@=��@=}�@<�@<U2@;�*@;j�@;'�@:�2@:i�@:B[@:8�@:-@:@9�z@9@8l"@7��@7S�@7�@6��@6c @6?@5ϫ@5:�@4��@4m�@4�@3�@3�@3��@3�q@3�{@3Z�@39�@2�M@2�]@2�R@2p;@2($@1��@0��@0��@0q@/��@/]�@/8@//�@/4�@/33@/)_@/Y@/�@/�@/�@/
=@/�@.�'@.q�@-}�@-8�@,�e@,c�@,:�@,%�@,!@+�m@+�4@*�@*��@*�@*��@*~�@*-@)�9@)s�@)!�@)�@(�	@(��@(ѷ@(g8@'�+@'�6@'�@'�V@'��@'@O@'"�@'o@'(@' i@&��@&Ta@&.�@%�@%�M@$�5@$N�@#��@#(@"��@"�8@"�c@"��@"�L@"Q@"e@!�.@!�@!�S@!Q�@!&�@ Ɇ@ ��@ l"@ ]d@ $@�@�g@��@��@iD@_p@>�@�@�}@��@n�@0U@{@�@�@ԕ@��@j@<6@��@�@Z@�@�W@�4@E9@ߤ@��@v�@ff@YK@C�@#:@��@��@|@5�@��@z�@:�@x@�0@�@�@��@�]@�m@�m@�L@xl@ff@V@$�@�S@u�@s�@j@j@j@c�@Dg@ \@��@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�JA�	�A��A�A��A�GA�A�A���A��A���A��"A��vA�AμjAΕ�AΆ�A΄�A΃GA΂A�~�A�~(A�{�A�zDA�v�A�pA�k�A�ZA�IA�LdA�/OA�#nA�uÁoA�R�A�U2A�@�A�"4A��+A�˒Aˮ�Aˊ=A�v`A�#�A���A�YA�3�A�� AǜxA�dZAù�A��&A��A�$�A�p�A��1A�cA�o�A��JA��A�MA�{JA�)*A��LA�t�A�+�A�3�A�ȀA���A��sA�Z�A��A�>�A���A�  A�N�A�&LA�"�A���A�NpA���A��	A�˒A��"A��A�+kA��A�dZAw�sAv@Au4Ap^�Amy�Ak� Ai�KAe�HAaZ�A^�A\��A[qAX!�AQ'RAO��AOl�AOe�AN��AK��AGs�ADFtA@I�A<��A:֡A9�EA8͟A7-wA4,�A3(A07A.H�A,��A+d�A*,�A)��A(�AA'�A%c A$�HA$p�A#�'A"h
A!�rA!=qA!�0A"�A#4�A#L�A#7�A#>BA#�A!�&A _�A C�A 'RA �oA!_pA"DgA!�MA"/�A!p�A ɆA �~A�MA�]Aj�A�A�A�A+�A�jA4nA($A�pA8A7LA[WA1�A!A�AA�A��A��A��A�AzxA��A�A@OA��Al"A��A�	Av`A�bA��A�xA�DAS�A��A�A�wA
�PA
h�A
�A	�-A	N�A�AffA�FA�A�Au�A�mA��A��Al�A��A�A��A;A�A��AA�AAJA ��A 6�A 
�@��@�hs@���@�b@�S&@���@�J@� �@�m]@�M@�0�@�{�@��M@��@���@��z@��A@���@�[�@��+@�O�@�Ta@�c�@�@�O@�@�s�@�%@� @�i�@�>B@�,=@�f�@�e@���@�s�@�@��@��@�~�@��K@�W?@���@�j@�U2@�\�@��@睲@�]�@�A�@��@�b@��8@�ѷ@�[�@�@�@�7@�%@���@���@�~�@��@ާ�@��@ݞ�@�[W@�o@���@܈�@���@۩*@ۀ4@�0�@ڵ@�g8@��&@ٰ�@�k�@��8@�Ĝ@ع�@؛�@�@�@���@��@�t�@��5@�|�@��g@�˒@�c@Ի�@�K^@�@�#�@��H@�bN@���@ў�@�-w@��/@Ї+@�1'@϶F@ρ�@�W?@��c@ΐ.@�_@�-�@��@͓@�"�@��/@̜x@��@��@˽�@�o @�E9@�	l@ʜx@�H@�
�@��D@���@ɍP@�P�@�5�@��@�
=@��/@�e�@�+k@�5?@���@Ƿ@�[W@��@��[@�oi@�I�@���@��;@���@ų�@�b�@�L�@�>�@��@�ߤ@Īe@���@�q@�]d@�Q�@�@�
=@��@�	l@�҉@���@���@�C-@��@�4@�u%@�#:@��@��"@��@�*0@�v�@��r@���@�8@���@�L0@�@�G@���@�v`@�\)@��@���@�L0@��^@�A @��@���@�3�@��@��@�"�@���@�*�@��@�g�@�F@�+@���@�|�@���@���@�x@�<6@���@���@���@�4n@���@�m]@�$t@���@���@�:�@� �@���@�s@�T�@�&@��e@�8�@��@��o@���@���@���@���@�\�@��p@��@�c @�$@��A@���@���@��{@�`B@�?}@�
=@��@�J�@�_@�/@���@�i�@�	�@���@��P@�e,@��@��@��R@��r@��@���@�g�@�Dg@�*0@���@�֡@�҉@��?@��6@��A@�(�@�G@��@�rG@���@���@�N�@�J�@�*�@��N@��@��p@���@�Ft@��Q@�~�@�-w@��@��'@��}@��+@�'R@�s@��@��1@�Z�@�:�@��C@�_p@�N<@��@���@��X@���@�oi@�K^@�u@��@���@�	l@��@��F@��$@�|�@�?}@���@�H�@��Q@��@���@�C�@���@���@�M@�@��K@�@O@��U@�`�@��@�@@��@��8@��2@���@�z@� �@���@���@�{J@�F@���@���@��r@�Z�@���@��@�y�@�dZ@�=�@�@��M@���@�l�@�K^@�;�@�$�@�4@��@���@�,�@�ߤ@���@��@�h�@��@���@�\�@�,�@�%@���@��K@��@���@���@��I@�7@���@���@���@���@�;d@���@��@��<@���@�xl@�R�@�7�@��@��.@��Z@��9@��H@��4@�f�@�/�@��H@�oi@�Z�@�Ft@��j@��@�x@�+@���@��B@�{�@�?@�#:@�w@8@~��@~Q@~�@}�X@}p�@}+�@|�@|�/@|�)@|�@|(�@{�@{s@z�c@zd�@y��@y@@x�@xr�@w�@wݘ@w��@w_p@v��@v��@vv�@v0U@u�o@u��@uw2@u��@u@@te�@s��@s�@s�:@s>�@r~�@r.�@q�t@q2a@q�@p��@pM@o�@@o6z@o�@n�@nJ@m�@m@mrG@m+@l�`@l�.@lC-@l�@k�&@j��@j�x@j6�@j4@i��@i�t@i`B@i�@h�j@h��@hFt@g��@g�g@gMj@f�@f�1@fs�@f^5@fJ�@f!�@f4@e��@e7L@e�@d�@dh�@dK^@dD�@d<�@d*�@d�@c��@c|�@cMj@bߤ@b�+@b&�@a@a��@`�@`q@`7�@`,=@`  @_�A@_خ@_�F@_�	@_"�@^W�@]�Z@]��@]�M@\��@[�+@[�@[>�@Z�}@ZQ@Z@Y�9@Y��@Ys�@X��@X~(@X9X@Xx@W�6@W�@V}V@VJ@U�@T�4@S��@SZ�@S>�@S.I@S�@R�@R4@Q�S@QDg@P�@PbN@Oخ@N��@N��@N#:@M�@M�@M��@Mu�@MIR@M@L��@L��@L��@L�Y@L`�@K��@Ky�@J�R@I��@I��@I�3@I�t@Is�@I2a@H�$@H,=@Gƨ@G�@F�2@F�<@F�@F@�@E�@E�-@E�"@E:�@D�v@D_@C�
@CS�@C.I@B�@Bh
@B)�@A�@A��@A��@A�7@AS&@@�@@��@?��@?K�@>��@>�\@>($@=��@=��@=}�@<�@<U2@;�*@;j�@;'�@:�2@:i�@:B[@:8�@:-@:@9�z@9@8l"@7��@7S�@7�@6��@6c @6?@5ϫ@5:�@4��@4m�@4�@3�@3�@3��@3�q@3�{@3Z�@39�@2�M@2�]@2�R@2p;@2($@1��@0��@0��@0q@/��@/]�@/8@//�@/4�@/33@/)_@/Y@/�@/�@/�@/
=@/�@.�'@.q�@-}�@-8�@,�e@,c�@,:�@,%�@,!@+�m@+�4@*�@*��@*�@*��@*~�@*-@)�9@)s�@)!�@)�@(�	@(��@(ѷ@(g8@'�+@'�6@'�@'�V@'��@'@O@'"�@'o@'(@' i@&��@&Ta@&.�@%�@%�M@$�5@$N�@#��@#(@"��@"�8@"�c@"��@"�L@"Q@"e@!�.@!�@!�S@!Q�@!&�@ Ɇ@ ��@ l"@ ]d@ $@�@�g@��@��@iD@_p@>�@�@�}@��@n�@0U@{@�@�@ԕ@��@j@<6@��@�@Z@�@�W@�4@E9@ߤ@��@v�@ff@YK@C�@#:@��@��@|@5�@��@z�@:�@x@�0@�@�@��@�]@�m@�m@�L@xl@ff@V@$�@�S@u�@s�@j@j@j@c�@Dg@ \@��@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�LB�2B�B��B��B��B�`B�`B�FB�`B��B�tB��B�@B�tB��B��B�:B�B�B�B� B� B� B�:B� B�TB��B�tB�B�;B�B�bB��B��B��B՛B�BܒB�IB�5BޞB�jBݘB�BںBڠB�:B֡B�dB�pB�;B�UB�OB	��B	��B
S&B
�B
�B
�~B
��B
�B
�`B
�B7BE�B_pBT,BjB
��BMB6B5�BG�B9�B�B
�B
�pB
��B
��B
��B
��B
�oB
bB
;�B
	B
�B	�B	�DB	��B	�xB	�;B	y�B	Z�B	S�B	O\B	E�B	4�B	�B	:B		�B	�B��B��B	+B	)�B	5B	>�B	M�B	V�B	^�B	Q�B	>B	;�B	A�B	<�B	1�B	/B	�B	VB	 OB��B�B�fB	 iB	�B	B	�B	]B	_B	WB	 'B	B	�B	2�B	F�B	_VB	iB	pUB	z�B	�9B	��B	~wB	��B	�B	�TB	�RB	��B	��B	�6B	��B	�BB	��B	�oB	҉B	͹B	�B	��B	��B	�4B	�dB	�B	�mB	��B	�lB	�]B	�`B	��B	ȚB	�JB	�4B	��B	�sB	҉B	�JB	āB	��B	�dB	�^B	��B	�B	��B	�B	ƨB	�"B	��B	�@B	��B	�jB	�DB	��B	ЗB	�B	�DB	ӏB	��B	��B	��B	��B	��B	�B	��B	�:B	��B	��B	�FB	�B	�!B	�6B	��B	�_B	�=B	�yB	یB	�qB	�yB	�KB	�qB	ޞB	��B	��B	�qB	�}B	�B	�}B	��B	�B	�TB	��B	�B	�TB	�zB	��B	�rB	�B	��B
  B	��B	�}B	��B	�qB	�B	�DB	�B	�rB	�6B	��B	��B	��B	�}B	�HB	�HB	�qB	��B	�6B	�PB	�	B	��B	��B	��B	�xB	�JB	�dB	��B	�}B
B
oB
B
 �B
  B	��B	��B	��B	��B	�6B	��B	�dB	�DB	��B	��B	�RB	�B	�RB	�lB	�XB	��B	��B	��B	�^B	��B	��B	��B	�xB	�B	��B	�B	��B	��B	��B	��B	��B	�B	�dB	��B	��B	�B	��B	��B	��B	��B	�VB	��B	��B	��B	�B	��B	��B	�dB	�B	�0B	��B	��B	��B	��B	�xB	�^B	�xB	�^B	�^B	�DB	�xB	�xB	�B	�B	�B	�dB	��B	�PB	�PB	�PB	�"B	�(B	��B	��B	�B	�BB	�wB	��B	��B	��B	��B	��B	��B	��B
 B
 �B
 �B
UB
�B
UB
 �B
 B
�B
�B
gB
YB
B
�B
�B

�B

�B

rB
�B
fB
+B
�B
9B
�B
)B
B
0B
�B
xB

XB
	lB
�B
B
�B
�B
�B
�B
�B
�B
�B
-B
�B
{B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
GB
GB
�B
MB
�B
B
SB
�B
mB
SB
�B
%B
%B
�B
�B
�B
�B
tB
tB
tB
�B
tB
tB
�B
�B
YB
YB
?B
�B
�B
B
?B
tB
�B
B
zB
B
zB
�B
�B
�B
zB
�B
fB
�B
�B
	B
	7B
	lB
	lB
	�B
	�B
	�B
	�B

#B

	B

	B
DB
DB
xB
�B
dB
JB
dB
�B
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
4B
NB
4B
�B
�B
\B
�B
�B
�B
hB
B
�B
B
6B
B
�B
�B
<B
�B
\B
�B
�B
.B
}B
hB
�B
:B
�B
&B
�B
�B
�B
�B
�B
�B
�B
{B
�B
[B
�B
�B
,B
�B
�B
SB
B
B
�B
�B
�B
�B
�B
�B

B
sB
�B
EB
+B
_B
1B
eB
KB
KB
B
�B
�B
1B
B
#B
B
CB
�B
�B
�B
�B
�B
�B
 �B
!-B
!bB
!�B
!�B
"NB
"�B
#B
#:B
#:B
#:B
#TB
$B
$�B
%,B
%,B
%,B
%zB
&B
&�B
'B
'mB
'�B
'�B
'�B
'�B
'�B
'mB
'�B
(sB
(�B
(sB
)*B
)�B
*0B
*eB
*�B
+6B
+6B
+�B
+�B
+�B
+�B
+�B
,B
,=B
,=B
,�B
-B
-]B
-�B
.�B
.}B
.�B
/B
/B
/iB
/�B
0B
/�B
/�B
/�B
0UB
0�B
0oB
0�B
1'B
0�B
1B
1AB
1�B
2B
2�B
2�B
3MB
3�B
3�B
3�B
49B
4�B
4�B
5ZB
5�B
5�B
5�B
5�B
5�B
6+B
6�B
6�B
6�B
6�B
72B
7�B
7�B
7LB
7�B
8B
8RB
8lB
8B
8lB
9$B
9$B
9XB
9�B
9�B
9�B
9�B
:*B
:DB
:*B
:^B
:�B
:�B
:�B
;B
;B
;�B
;�B
<B
<B
;�B
<6B
<B
<�B
<�B
<�B
="B
=�B
=�B
>BB
>�B
?}B
?cB
?.B
?cB
?HB
?HB
?cB
?cB
?}B
?�B
?}B
?�B
?�B
?�B
@ B
@iB
@�B
@iB
@OB
@OB
@4B
@�B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
?�B
?�B
@ B
@B
@4B
@4B
@4B
@OB
@ B
@�B
A�B
A�B
A�B
A�B
B'B
A�B
A�B
BB
B�B
B�B
C-B
CaB
C{B
C�B
D�B
EB
EmB
E�B
FB
E�B
F�B
F�B
G�B
HB
IB
IRB
I7B
I7B
IRB
I�B
J	B
JrB
J�B
K)B
K�B
K�B
LdB
L�B
MB
M6B
MjB
M�B
NB
N"B
NVB
N�B
N�B
NpB
NpB
NVB
N�B
N�B
O�B
O�B
P.B
P.B
P.B
PbB
P}B
Q B
Q4B
Q�B
RB
R B
R:B
RTB
R�B
S&B
S@B
S@B
SuB
TB
TaB
T�B
U2B
UgB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
W$B
W$B
X+B
XB
XEB
X_B
X�B
YB
X�B
X�B
YeB
Y�B
ZQB
ZkB
Z�B
Z�B
[#B
[#B
[#B
[	B
[	B
[#B
[�B
[�B
\�B
\�B
\�B
]/B
]/B
]B
]~B
]�B
^OB
^jB
^�B
^�B
^�B
_B
_!B
_;B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
abB
a|B
aHB
a�B
b4B
bhB
bNB
bhB
bNB
bhB
b�B
bhB
b�B
bhB
bNB
bhB
bhB
b�B
cnB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
d�B
d�B
eB
ezB
e�B
fB
e�B
e�B
fB
e�B
f�B
f�B
f�B
f�B
gB
g8B
gmB
gmB
gmB
gmB
g�B
g�B
h
B
h$B
h$B
h�B
h�B
iDB
j0B
j�B
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
lWB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
nB
n/B
nIB
n}B
n}B
n}B
o B
o B
oB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
pB
pB
p�B
p�B
p�B
qB
qB
q�B
q�B
rB
r-B
raB
raB
raB
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tB
t�B
t�B
uB
uB
u%B
u?B
u%B
utB
utB
u�B
u�B
u�B
vFB
v`B
vFB
v`B
v`B
vFB
vFB
v`B
v+B
w2B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�LB�2B�B��B��B��B�`B�`B�FB�`B��B�tB��B�@B�tB��B��B�:B�B�B�B� B� B� B�:B� B�TB��B�tB�B�;B�B�bB��B��B��B՛B�BܒB�IB�5BޞB�jBݘB�BںBڠB�:B֡B�dB�pB�;B�UB�OB	��B	��B
S&B
�B
�B
�~B
��B
�B
�`B
�B7BE�B_pBT,BjB
��BMB6B5�BG�B9�B�B
�B
�pB
��B
��B
��B
��B
�oB
bB
;�B
	B
�B	�B	�DB	��B	�xB	�;B	y�B	Z�B	S�B	O\B	E�B	4�B	�B	:B		�B	�B��B��B	+B	)�B	5B	>�B	M�B	V�B	^�B	Q�B	>B	;�B	A�B	<�B	1�B	/B	�B	VB	 OB��B�B�fB	 iB	�B	B	�B	]B	_B	WB	 'B	B	�B	2�B	F�B	_VB	iB	pUB	z�B	�9B	��B	~wB	��B	�B	�TB	�RB	��B	��B	�6B	��B	�BB	��B	�oB	҉B	͹B	�B	��B	��B	�4B	�dB	�B	�mB	��B	�lB	�]B	�`B	��B	ȚB	�JB	�4B	��B	�sB	҉B	�JB	āB	��B	�dB	�^B	��B	�B	��B	�B	ƨB	�"B	��B	�@B	��B	�jB	�DB	��B	ЗB	�B	�DB	ӏB	��B	��B	��B	��B	��B	�B	��B	�:B	��B	��B	�FB	�B	�!B	�6B	��B	�_B	�=B	�yB	یB	�qB	�yB	�KB	�qB	ޞB	��B	��B	�qB	�}B	�B	�}B	��B	�B	�TB	��B	�B	�TB	�zB	��B	�rB	�B	��B
  B	��B	�}B	��B	�qB	�B	�DB	�B	�rB	�6B	��B	��B	��B	�}B	�HB	�HB	�qB	��B	�6B	�PB	�	B	��B	��B	��B	�xB	�JB	�dB	��B	�}B
B
oB
B
 �B
  B	��B	��B	��B	��B	�6B	��B	�dB	�DB	��B	��B	�RB	�B	�RB	�lB	�XB	��B	��B	��B	�^B	��B	��B	��B	�xB	�B	��B	�B	��B	��B	��B	��B	��B	�B	�dB	��B	��B	�B	��B	��B	��B	��B	�VB	��B	��B	��B	�B	��B	��B	�dB	�B	�0B	��B	��B	��B	��B	�xB	�^B	�xB	�^B	�^B	�DB	�xB	�xB	�B	�B	�B	�dB	��B	�PB	�PB	�PB	�"B	�(B	��B	��B	�B	�BB	�wB	��B	��B	��B	��B	��B	��B	��B
 B
 �B
 �B
UB
�B
UB
 �B
 B
�B
�B
gB
YB
B
�B
�B

�B

�B

rB
�B
fB
+B
�B
9B
�B
)B
B
0B
�B
xB

XB
	lB
�B
B
�B
�B
�B
�B
�B
�B
�B
-B
�B
{B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
GB
GB
�B
MB
�B
B
SB
�B
mB
SB
�B
%B
%B
�B
�B
�B
�B
tB
tB
tB
�B
tB
tB
�B
�B
YB
YB
?B
�B
�B
B
?B
tB
�B
B
zB
B
zB
�B
�B
�B
zB
�B
fB
�B
�B
	B
	7B
	lB
	lB
	�B
	�B
	�B
	�B

#B

	B

	B
DB
DB
xB
�B
dB
JB
dB
�B
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
4B
NB
4B
�B
�B
\B
�B
�B
�B
hB
B
�B
B
6B
B
�B
�B
<B
�B
\B
�B
�B
.B
}B
hB
�B
:B
�B
&B
�B
�B
�B
�B
�B
�B
�B
{B
�B
[B
�B
�B
,B
�B
�B
SB
B
B
�B
�B
�B
�B
�B
�B

B
sB
�B
EB
+B
_B
1B
eB
KB
KB
B
�B
�B
1B
B
#B
B
CB
�B
�B
�B
�B
�B
�B
 �B
!-B
!bB
!�B
!�B
"NB
"�B
#B
#:B
#:B
#:B
#TB
$B
$�B
%,B
%,B
%,B
%zB
&B
&�B
'B
'mB
'�B
'�B
'�B
'�B
'�B
'mB
'�B
(sB
(�B
(sB
)*B
)�B
*0B
*eB
*�B
+6B
+6B
+�B
+�B
+�B
+�B
+�B
,B
,=B
,=B
,�B
-B
-]B
-�B
.�B
.}B
.�B
/B
/B
/iB
/�B
0B
/�B
/�B
/�B
0UB
0�B
0oB
0�B
1'B
0�B
1B
1AB
1�B
2B
2�B
2�B
3MB
3�B
3�B
3�B
49B
4�B
4�B
5ZB
5�B
5�B
5�B
5�B
5�B
6+B
6�B
6�B
6�B
6�B
72B
7�B
7�B
7LB
7�B
8B
8RB
8lB
8B
8lB
9$B
9$B
9XB
9�B
9�B
9�B
9�B
:*B
:DB
:*B
:^B
:�B
:�B
:�B
;B
;B
;�B
;�B
<B
<B
;�B
<6B
<B
<�B
<�B
<�B
="B
=�B
=�B
>BB
>�B
?}B
?cB
?.B
?cB
?HB
?HB
?cB
?cB
?}B
?�B
?}B
?�B
?�B
?�B
@ B
@iB
@�B
@iB
@OB
@OB
@4B
@�B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
?�B
?�B
@ B
@B
@4B
@4B
@4B
@OB
@ B
@�B
A�B
A�B
A�B
A�B
B'B
A�B
A�B
BB
B�B
B�B
C-B
CaB
C{B
C�B
D�B
EB
EmB
E�B
FB
E�B
F�B
F�B
G�B
HB
IB
IRB
I7B
I7B
IRB
I�B
J	B
JrB
J�B
K)B
K�B
K�B
LdB
L�B
MB
M6B
MjB
M�B
NB
N"B
NVB
N�B
N�B
NpB
NpB
NVB
N�B
N�B
O�B
O�B
P.B
P.B
P.B
PbB
P}B
Q B
Q4B
Q�B
RB
R B
R:B
RTB
R�B
S&B
S@B
S@B
SuB
TB
TaB
T�B
U2B
UgB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
W$B
W$B
X+B
XB
XEB
X_B
X�B
YB
X�B
X�B
YeB
Y�B
ZQB
ZkB
Z�B
Z�B
[#B
[#B
[#B
[	B
[	B
[#B
[�B
[�B
\�B
\�B
\�B
]/B
]/B
]B
]~B
]�B
^OB
^jB
^�B
^�B
^�B
_B
_!B
_;B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
abB
a|B
aHB
a�B
b4B
bhB
bNB
bhB
bNB
bhB
b�B
bhB
b�B
bhB
bNB
bhB
bhB
b�B
cnB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
d�B
d�B
eB
ezB
e�B
fB
e�B
e�B
fB
e�B
f�B
f�B
f�B
f�B
gB
g8B
gmB
gmB
gmB
gmB
g�B
g�B
h
B
h$B
h$B
h�B
h�B
iDB
j0B
j�B
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
lWB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
nB
n/B
nIB
n}B
n}B
n}B
o B
o B
oB
oOB
o�B
o�B
o�B
o�B
o�B
o�B
pB
pB
p�B
p�B
p�B
qB
qB
q�B
q�B
rB
r-B
raB
raB
raB
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tB
t�B
t�B
uB
uB
u%B
u?B
u%B
utB
utB
u�B
u�B
u�B
vFB
v`B
vFB
v`B
v`B
vFB
vFB
v`B
v+B
w2B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105237  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191857  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191858  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191858                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041905  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041905  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                