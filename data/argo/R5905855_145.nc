CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-02T18:41:43Z creation;2023-02-02T18:41:44Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230202184143  20230202185911  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���L;*1   @��9u0�@/Z�1'�d,�/��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�CL�C  C  C��C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�3D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@|(�@�{@�{A
=A?
=A_
=A�Q�A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB�{B�z�B�z�B��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB�{B�B��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C
>C=qC�C�C�qC!�
C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(��D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU��DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�AGD�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D�GD�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD�GD�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S&A�V9A�W
A�V9A�V9A�WsA�ZQA�Y�A�[WA�\�A�]/A�`A�a�A�b�A�c A�a�A�a|A�T,A�@�A�=�A�<�A�<�A�?HA�GA�B'A�,qA�A��A��A�A�
rA��A�oA��.A��DA���A��fA��PA�%A�A�YA�4nA�A A���Aͻ0A̕Aì�A��WA��yA��A���A��A�\�A�<jA���A�{A�7A��A�ÖA���A�jA��YA��A��A���A�}"A���A�*�A�!bA�(A��1A���A��A���A��A��A}o AyFApMAa�A]��A[SAW�bAUHASAQp;AP6�AN�RAKn�AI�@AG�'AD��A@�/A>s�A=��A<�cA9�A6TaA5�mA5��A4�A3�\A2�
A2%FA1�aA1|A0�A/�xA.A-�A+i�A(��A&j�A#��A"��A"(�A!}�A ��A�A�"A;AA!�A!1�A ��A e�A��AMjA�A�hA�A�hA(�A-A9�Aq�A�fA�A{�A��A��AL�A�A%FA��A��AA�A�tA��A��A��AݘA*�AMjA�cAxAیA�+A�A�zAXyA6zA3�A|�A�A��A{A��Az�A4nA��A'RA��A
�.A	��A��A	�A	�A�YA33Av�Av`A;dA�A͟A�1AS�A�A��Al�A�yAu%AA�A3�A��A��A��AkQA�AƨA��A[WA�^A,�A �gA ɆA �]A �YA �@�@���@�m�@���@�a�@��U@�B[@�Xy@�Z@��W@�C�@��?@���@��@��@���@�c�@��@�`B@�r�@�}V@�~�@�Ov@��@�h�@�D�@�<�@�:*@���@�g8@�3�@�5?@��@�w2@�ff@��N@�7@�A�@��@�i�@�K^@�M@���@�� @�@�9X@ߩ�@�8�@� i@���@�?�@ݡ�@�F@���@�bN@�خ@�@ڹ�@ڃ@�E�@�#:@��@���@�e,@��)@���@�a�@�2a@��2@ֱ�@�l�@��@�_�@���@Ӄ{@�S&@��f@���@��@Ь�@�H@��@ϴ�@ϫ�@Ϣ�@�Y�@΅�@�+k@��@�iD@��	@��@́o@��@�Vm@ʟ�@ɯ�@�B�@�ߤ@�y>@���@�s@ƚ�@�!�@���@Ű�@�\�@�r�@��@î@�^�@���@�w�@�h
@�K^@��@��z@�u@��@�+@���@��$@�q@�+k@���@�S&@��@�l�@��@�S�@��h@�~�@�_�@�H�@�-�@���@��F@��M@�F�@��@�1'@��@��o@��@�Dg@��1@���@��@�s�@��@�W?@�@���@�L0@�`B@�ߤ@��h@�L�@�1�@�͟@�4n@��Z@��d@���@�Vm@�=�@���@�3�@�	@���@��H@��V@�W?@��@�z�@�(�@��^@��@�_p@��@��f@��H@��'@��\@�	@���@�S�@�-w@���@�z�@�2�@��>@�s@��@���@���@�u�@�V@�'R@��[@�[W@�(@�Ɇ@�oi@��@���@��@��@�o @�B�@��@��j@���@�]d@�e@���@���@�%F@���@�ѷ@��e@�q@�S�@�?�@��@���@�+�@�ߤ@���@�R�@�u@���@��@�z@�*�@��@�
�@�1@��D@��n@�Mj@�&�@��K@���@�~�@�kQ@�N�@���@��H@�k�@���@��u@���@�W�@��a@�j@�RT@���@��@���@�v�@�i�@�>B@���@���@��h@�Dg@��)@���@�Xy@�A�@�"h@���@��[@�a�@�!�@��@��U@���@�tT@��j@�~�@�j�@�]�@�G�@�"�@���@���@�V�@�4n@�7@�u@��@��@�u�@��@��@��@��@��,@���@���@���@��U@��$@��6@��\@�h
@��@��@�?�@�O@��]@���@��Q@��@���@���@��f@�S&@��@�	l@���@�͟@�~�@���@�4�@��K@���@�_@�*�@�x@��@���@��-@���@�T�@�IR@�B�@��@��h@�V�@���@�|�@�F�@�!�@���@��.@���@���@�z�@�g8@�O@~��@~kQ@~#:@}�n@}\�@|�@{��@{x@z��@yc�@x��@w��@w_p@v��@v	@u*0@t�`@ty>@t �@s��@s(@q�N@pu�@pM@o�m@o�;@o��@o�;@o�@o��@o�a@o~�@o'�@n��@m�@m�S@mF@m�@l2�@k+@j�@i��@is�@iS&@i�@hی@h��@hc�@h6@h'R@h~@h�@g��@ge�@g�@f�!@f1�@e�@e�@d��@c�*@b�@a��@aIR@a2a@a*0@a+@`�@`�@`|�@_{J@]�'@\��@\��@\bN@\�@[�6@[�w@[;d@Z��@Z��@Z� @Z^5@Y�n@Y=�@X�[@Xl"@W$t@V�}@Vd�@U�@U��@U��@U}�@UJ�@U@TĜ@T �@S�[@S�	@SMj@R��@R��@Rȴ@Ru%@R$�@Q�o@Q��@Q2a@Q�@P�P@P�@P,=@O�;@O��@O(@N҉@N��@N�@N{�@Np;@M�@MV@M�@Mq@L�@L|�@Ly>@Lh�@L,=@Kݘ@K��@K��@K|�@KMj@K'�@J�b@I�z@Hl"@H�@G��@G�@F��@F �@E�7@E8�@Doi@D6@D@C�;@C�*@CiD@CS@B҉@B�L@BL0@A��@@��@?�@?��@?��@?t�@?"�@?�@>��@>�@>� @=ϫ@<��@<9X@;�q@;J#@;�@;S@:��@:~�@:H�@:($@:�@9��@9�@9�N@9�C@9	l@8oi@7�}@7�$@7x@7]�@7O@7!-@7�@6�c@6�s@6�h@6��@65?@5ԕ@5�-@5��@5-w@5@5	l@5V@5@@5�@5�@4��@4q@3��@3j�@2��@2a|@2W�@2E�@1�9@0�)@0��@01'@/E9@.�]@.Ov@-��@-�@-hs@-@,�@,4n@+��@+�@@+{J@+l�@+j�@+\)@+(@*��@*�x@*Z�@*Q@*E�@*#:@*
�@)��@)�@)2a@(�@(�U@(��@(u�@(e�@(A�@(@'�@'˒@'�	@'e�@'Mj@'$t@&�]@&��@&{�@&R�@&{@&	@&@&u@&
�@%�@%�^@%rG@%�@$�`@$��@$7�@$x@#˒@#��@#�@#s@#b�@#C@"�B@!�D@!��@!hs@!8�@ �`@ �@ N�@ 1'@ !@ �@�
@j�@�c@��@0U@��@c�@A @8�@�@�K@|�@� @�@]�@�@��@ߤ@��@M�@+k@4@�@@�@��@�H@�@hs@Vm@%F@�|@��@�u@~(@j@[�@<�@��@iD@=@ں@�x@��@p;@B[@8�@3�@!�@ �@�@�@��@��@IR@7L@��@��@tT@C-@4n@�@�@ƨ@�{@j�@A�@!-@@�@v�@p;@d�@YK@{@��@�X@�'@��@a�@L�@<6@(�@�v@��@M@	�@��@�k@�$@��@��@�@dZ@'�@�@��@��@��@u%@H�@1�@��@w2@=�@V@�@�@c�@Ft@ �@�+@�w@�@|�@j�@RT@U�@P�@K�@=@
��@
�m@
�!@
��@
�@
��@
��@
}V@
u%@
kQ@
V@
V@
H�@	�Z@	��@	��@	�~@	o @	Q�@	8�@	*0@��@��@�@w�@,=@�@	�@@	�@�*@�{@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S&A�V9A�W
A�V9A�V9A�WsA�ZQA�Y�A�[WA�\�A�]/A�`A�a�A�b�A�c A�a�A�a|A�T,A�@�A�=�A�<�A�<�A�?HA�GA�B'A�,qA�A��A��A�A�
rA��A�oA��.A��DA���A��fA��PA�%A�A�YA�4nA�A A���Aͻ0A̕Aì�A��WA��yA��A���A��A�\�A�<jA���A�{A�7A��A�ÖA���A�jA��YA��A��A���A�}"A���A�*�A�!bA�(A��1A���A��A���A��A��A}o AyFApMAa�A]��A[SAW�bAUHASAQp;AP6�AN�RAKn�AI�@AG�'AD��A@�/A>s�A=��A<�cA9�A6TaA5�mA5��A4�A3�\A2�
A2%FA1�aA1|A0�A/�xA.A-�A+i�A(��A&j�A#��A"��A"(�A!}�A ��A�A�"A;AA!�A!1�A ��A e�A��AMjA�A�hA�A�hA(�A-A9�Aq�A�fA�A{�A��A��AL�A�A%FA��A��AA�A�tA��A��A��AݘA*�AMjA�cAxAیA�+A�A�zAXyA6zA3�A|�A�A��A{A��Az�A4nA��A'RA��A
�.A	��A��A	�A	�A�YA33Av�Av`A;dA�A͟A�1AS�A�A��Al�A�yAu%AA�A3�A��A��A��AkQA�AƨA��A[WA�^A,�A �gA ɆA �]A �YA �@�@���@�m�@���@�a�@��U@�B[@�Xy@�Z@��W@�C�@��?@���@��@��@���@�c�@��@�`B@�r�@�}V@�~�@�Ov@��@�h�@�D�@�<�@�:*@���@�g8@�3�@�5?@��@�w2@�ff@��N@�7@�A�@��@�i�@�K^@�M@���@�� @�@�9X@ߩ�@�8�@� i@���@�?�@ݡ�@�F@���@�bN@�خ@�@ڹ�@ڃ@�E�@�#:@��@���@�e,@��)@���@�a�@�2a@��2@ֱ�@�l�@��@�_�@���@Ӄ{@�S&@��f@���@��@Ь�@�H@��@ϴ�@ϫ�@Ϣ�@�Y�@΅�@�+k@��@�iD@��	@��@́o@��@�Vm@ʟ�@ɯ�@�B�@�ߤ@�y>@���@�s@ƚ�@�!�@���@Ű�@�\�@�r�@��@î@�^�@���@�w�@�h
@�K^@��@��z@�u@��@�+@���@��$@�q@�+k@���@�S&@��@�l�@��@�S�@��h@�~�@�_�@�H�@�-�@���@��F@��M@�F�@��@�1'@��@��o@��@�Dg@��1@���@��@�s�@��@�W?@�@���@�L0@�`B@�ߤ@��h@�L�@�1�@�͟@�4n@��Z@��d@���@�Vm@�=�@���@�3�@�	@���@��H@��V@�W?@��@�z�@�(�@��^@��@�_p@��@��f@��H@��'@��\@�	@���@�S�@�-w@���@�z�@�2�@��>@�s@��@���@���@�u�@�V@�'R@��[@�[W@�(@�Ɇ@�oi@��@���@��@��@�o @�B�@��@��j@���@�]d@�e@���@���@�%F@���@�ѷ@��e@�q@�S�@�?�@��@���@�+�@�ߤ@���@�R�@�u@���@��@�z@�*�@��@�
�@�1@��D@��n@�Mj@�&�@��K@���@�~�@�kQ@�N�@���@��H@�k�@���@��u@���@�W�@��a@�j@�RT@���@��@���@�v�@�i�@�>B@���@���@��h@�Dg@��)@���@�Xy@�A�@�"h@���@��[@�a�@�!�@��@��U@���@�tT@��j@�~�@�j�@�]�@�G�@�"�@���@���@�V�@�4n@�7@�u@��@��@�u�@��@��@��@��@��,@���@���@���@��U@��$@��6@��\@�h
@��@��@�?�@�O@��]@���@��Q@��@���@���@��f@�S&@��@�	l@���@�͟@�~�@���@�4�@��K@���@�_@�*�@�x@��@���@��-@���@�T�@�IR@�B�@��@��h@�V�@���@�|�@�F�@�!�@���@��.@���@���@�z�@�g8@�O@~��@~kQ@~#:@}�n@}\�@|�@{��@{x@z��@yc�@x��@w��@w_p@v��@v	@u*0@t�`@ty>@t �@s��@s(@q�N@pu�@pM@o�m@o�;@o��@o�;@o�@o��@o�a@o~�@o'�@n��@m�@m�S@mF@m�@l2�@k+@j�@i��@is�@iS&@i�@hی@h��@hc�@h6@h'R@h~@h�@g��@ge�@g�@f�!@f1�@e�@e�@d��@c�*@b�@a��@aIR@a2a@a*0@a+@`�@`�@`|�@_{J@]�'@\��@\��@\bN@\�@[�6@[�w@[;d@Z��@Z��@Z� @Z^5@Y�n@Y=�@X�[@Xl"@W$t@V�}@Vd�@U�@U��@U��@U}�@UJ�@U@TĜ@T �@S�[@S�	@SMj@R��@R��@Rȴ@Ru%@R$�@Q�o@Q��@Q2a@Q�@P�P@P�@P,=@O�;@O��@O(@N҉@N��@N�@N{�@Np;@M�@MV@M�@Mq@L�@L|�@Ly>@Lh�@L,=@Kݘ@K��@K��@K|�@KMj@K'�@J�b@I�z@Hl"@H�@G��@G�@F��@F �@E�7@E8�@Doi@D6@D@C�;@C�*@CiD@CS@B҉@B�L@BL0@A��@@��@?�@?��@?��@?t�@?"�@?�@>��@>�@>� @=ϫ@<��@<9X@;�q@;J#@;�@;S@:��@:~�@:H�@:($@:�@9��@9�@9�N@9�C@9	l@8oi@7�}@7�$@7x@7]�@7O@7!-@7�@6�c@6�s@6�h@6��@65?@5ԕ@5�-@5��@5-w@5@5	l@5V@5@@5�@5�@4��@4q@3��@3j�@2��@2a|@2W�@2E�@1�9@0�)@0��@01'@/E9@.�]@.Ov@-��@-�@-hs@-@,�@,4n@+��@+�@@+{J@+l�@+j�@+\)@+(@*��@*�x@*Z�@*Q@*E�@*#:@*
�@)��@)�@)2a@(�@(�U@(��@(u�@(e�@(A�@(@'�@'˒@'�	@'e�@'Mj@'$t@&�]@&��@&{�@&R�@&{@&	@&@&u@&
�@%�@%�^@%rG@%�@$�`@$��@$7�@$x@#˒@#��@#�@#s@#b�@#C@"�B@!�D@!��@!hs@!8�@ �`@ �@ N�@ 1'@ !@ �@�
@j�@�c@��@0U@��@c�@A @8�@�@�K@|�@� @�@]�@�@��@ߤ@��@M�@+k@4@�@@�@��@�H@�@hs@Vm@%F@�|@��@�u@~(@j@[�@<�@��@iD@=@ں@�x@��@p;@B[@8�@3�@!�@ �@�@�@��@��@IR@7L@��@��@tT@C-@4n@�@�@ƨ@�{@j�@A�@!-@@�@v�@p;@d�@YK@{@��@�X@�'@��@a�@L�@<6@(�@�v@��@M@	�@��@�k@�$@��@��@�@dZ@'�@�@��@��@��@u%@H�@1�@��@w2@=�@V@�@�@c�@Ft@ �@�+@�w@�@|�@j�@RT@U�@P�@K�@=@
��@
�m@
�!@
��@
�@
��@
��@
}V@
u%@
kQ@
V@
V@
H�@	�Z@	��@	��@	�~@	o @	Q�@	8�@	*0@��@��@�@w�@,=@�@	�@@	�@�*@�{@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	D�B	EB	D�B	D�B	D�B	D�B	EB	E9B	EB	EB	E9B	EB	EB	E9B	E9B	E9B	EB	D�B	IB	L�B	O�B	S�B	[=B	d�B	hXB	jB	lWB	n/B	n�B	o�B	qAB	q�B	q�B	s3B	t�B	uB	vB	xlB	~B	�B	�8B	�XB	��B	�<B
dB
+�B
��B
�~B
�QB
r�B
tB
��B
�MB
�jB
�~B
̳B
��B
��B
�B
��B
�5B:xBO�B2�BB
��B
��B
�B
��B
��B
l�B
B	�B	��B	��B	��B	�6B	�zB	��B	jKB	]�B	a-B	]�B	W�B	R�B	P}B	L�B	I�B	B�B	>�B	@OB	AB	+B	�B	%�B	/ B	5�B	6�B	J#B	\�B	t�B	�XB	�
B	��B	��B	��B	�LB	��B	��B	�B	�B	vFB	l"B	bNB	j�B	l"B	j�B	lqB	j�B	c B	e�B	�iB	��B	��B	��B	�B	��B	�oB	�?B	�'B	��B	��B	�oB	�mB	�HB	�OB	��B	��B	�-B	�vB	�B	��B	��B	��B	��B	��B	�B	��B	�\B	�yB	�-B	�"B
�B
�B
�B
�B
B
�B
#�B
!�B
pB
�B
�B
B
�B
]B
�B
�B
�B
WB
�B
B
KB
MB
&B
 B
�B

B
2B
�B
�B
dB
�B
�B
�B
dB
/B
xB
xB
qB
B
�B
�B
�B
&B
�B
�B
�B
�B
�B
mB
�B
�B
&B
�B
2B
)B
�B
1B
yB

B
�B
9B
�B
FB
�B

B
B
1B
�B
�B
mB
�B
B
,B
�B
B
VB
0B
	�B
	�B

	B
�B
B
VB
<B
�B
B
�B
�B
�B
�B
}B
bB
B
.B
.B
.B
�B
�B
�B
�B
BB
BB
�B
�B
�B
<B
"B
jB
B
dB
0B
B
�B

=B
	�B
	7B
�B
�B
1B
�B
zB
�B
	B
�B
�B
�B
fB
�B
�B
�B
�B
�B
{B
�B
B
-B
MB
B
�B
�B
mB
B
B
9B
MB
3B
�B
�B
mB
SB
B
B
�B
�B
{B
-B
�B
[B
AB
'B
�B
B
�B
{B
gB
�B
gB
�B
B
B
�B
uB
B
{B
-B
�B
�B
oB
uB
[B
B
�B
�B
�B
�B
�B
�B
KB
KB
�B
�B
B
+B
�B
�B
�B
�B
EB
_B
zB
B
?B
�B
YB
�B
B
zB
�B
�B
	B
	7B
	�B
	B
	�B
	�B
	�B
	�B

=B

#B

	B
	�B

rB
	�B
DB

�B
B
B

�B

�B

rB

�B
	lB
	�B

rB
B
^B
�B
xB
)B

�B

�B

�B
B

�B
JB
VB
pB
B
6B
xB

rB

�B
�B
�B
VB
�B
�B
�B
vB
BB
�B
B
�B
VB
\B
�B
vB
�B
bB
�B
�B
:B
:B
[B
�B
B
gB
�B
9B
�B
�B
�B
$B
yB
B
�B
�B
B
kB
=B
WB
B
xB
�B
�B
�B
B
~B
~B
jB
�B
VB
VB
pB
 'B
 BB
!bB
#:B
# B
# B
#TB
$B
$�B
$�B
%B
%,B
%,B
%B
$�B
%FB
%B
$�B
$�B
$@B
$ZB
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%zB
%zB
%`B
%�B
%�B
&�B
'B
'�B
'�B
($B
($B
(sB
(�B
)�B
+�B
+�B
,B
,B
,B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
.}B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.cB
.}B
/iB
/5B
/�B
/�B
0!B
0�B
0oB
0�B
0�B
0�B
1[B
1�B
1vB
1AB
1�B
2-B
2�B
4B
4B
5?B
5�B
7�B
7�B
8B
7�B
8RB
8�B
9XB
:xB
:�B
:�B
;B
;B
:�B
;dB
;JB
;JB
;B
:�B
:xB
:DB
:^B
;0B
;0B
;B
:�B
:�B
:�B
:�B
:�B
;JB
;0B
;JB
;JB
;JB
;JB
;JB
;0B
;JB
;JB
;0B
;dB
<B
<B
<PB
<PB
<�B
=�B
>�B
>�B
?cB
?}B
?}B
?�B
@ B
@4B
@iB
@iB
@iB
@�B
@�B
@�B
AB
A;B
A�B
A�B
BB
B'B
B�B
C-B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C{B
D3B
E�B
E�B
FB
F?B
F�B
F�B
F�B
G_B
GzB
GzB
G�B
GzB
HfB
H�B
HKB
G�B
H�B
IB
H�B
IB
IB
IB
IB
H�B
IB
IRB
I�B
J=B
J#B
JrB
JrB
JrB
JrB
JrB
J�B
J�B
J�B
KB
K)B
J�B
KB
K)B
KB
KDB
KB
J�B
J�B
J�B
J=B
I�B
IlB
IRB
IB
I7B
IRB
I7B
I7B
I7B
I�B
J	B
J	B
J#B
I�B
J#B
I�B
IRB
IB
J#B
JXB
J�B
J�B
J�B
KxB
K�B
K�B
L�B
MB
MB
MB
M6B
MjB
M�B
M�B
NB
NVB
N�B
O�B
PHB
PHB
PHB
PbB
P�B
P�B
P�B
P�B
P�B
QNB
R:B
R�B
SuB
TB
T,B
T,B
T�B
T�B
U2B
UB
U2B
UgB
U�B
U�B
VB
VSB
U�B
V�B
XyB
Y�B
ZB
ZQB
Z�B
[	B
[WB
[qB
[�B
[�B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^B
]�B
]~B
]IB
]IB
]~B
]IB
]�B
]�B
]~B
]�B
]�B
^B
^jB
^�B
_pB
_�B
`B
`BB
`�B
`�B
`�B
`�B
aHB
a�B
bNB
cB
b�B
cnB
e`B
e�B
e�B
fB
f�B
f�B
gB
gB
gRB
gmB
g�B
g�B
g�B
g�B
h$B
h>B
h>B
hsB
h�B
h�B
i*B
iB
iB
i*B
iB
iB
iB
h�B
h�B
h�B
h�B
h�B
i*B
i�B
i�B
i�B
i�B
j0B
jKB
j�B
kB
kB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
n}B
n�B
o B
oOB
o�B
o�B
pB
pB
p!B
p!B
p�B
qvB
q�B
raB
r�B
raB
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s3B
shB
s�B
s�B
s�B
tB
tTB
t�B
uB
uB
u?B
u%B
u?B
u�B
v`B
v�B
wB
wB
wB
wB
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
xlB
xRB
x�B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zxB
zxB
z�B
z�B
z�B
{dB
{B
{dB
{B
{B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}B
}VB
}qB
}qB
}�B
~B
~BB
~BB
~BB
~(B
~(B
~B
~(B
~BB
~wB
~wB
~�B
~�B
~�B
~wB
~�B
~�B
~�B
.B
cB
�B
�B
� B
�B
��B
��B
��B
�B
�UB
��B
��B
��B
��B
��B
�[B
�[B
�[B
�[B
�uB
��B
��B
��B
��B
��B
��B
��B
�GB
�aB
�GB
�B
�B
�B
�B
�-B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	D�B	EB	D�B	D�B	D�B	D�B	EB	E9B	EB	EB	E9B	EB	EB	E9B	E9B	E9B	EB	D�B	IB	L�B	O�B	S�B	[=B	d�B	hXB	jB	lWB	n/B	n�B	o�B	qAB	q�B	q�B	s3B	t�B	uB	vB	xlB	~B	�B	�8B	�XB	��B	�<B
dB
+�B
��B
�~B
�QB
r�B
tB
��B
�MB
�jB
�~B
̳B
��B
��B
�B
��B
�5B:xBO�B2�BB
��B
��B
�B
��B
��B
l�B
B	�B	��B	��B	��B	�6B	�zB	��B	jKB	]�B	a-B	]�B	W�B	R�B	P}B	L�B	I�B	B�B	>�B	@OB	AB	+B	�B	%�B	/ B	5�B	6�B	J#B	\�B	t�B	�XB	�
B	��B	��B	��B	�LB	��B	��B	�B	�B	vFB	l"B	bNB	j�B	l"B	j�B	lqB	j�B	c B	e�B	�iB	��B	��B	��B	�B	��B	�oB	�?B	�'B	��B	��B	�oB	�mB	�HB	�OB	��B	��B	�-B	�vB	�B	��B	��B	��B	��B	��B	�B	��B	�\B	�yB	�-B	�"B
�B
�B
�B
�B
B
�B
#�B
!�B
pB
�B
�B
B
�B
]B
�B
�B
�B
WB
�B
B
KB
MB
&B
 B
�B

B
2B
�B
�B
dB
�B
�B
�B
dB
/B
xB
xB
qB
B
�B
�B
�B
&B
�B
�B
�B
�B
�B
mB
�B
�B
&B
�B
2B
)B
�B
1B
yB

B
�B
9B
�B
FB
�B

B
B
1B
�B
�B
mB
�B
B
,B
�B
B
VB
0B
	�B
	�B

	B
�B
B
VB
<B
�B
B
�B
�B
�B
�B
}B
bB
B
.B
.B
.B
�B
�B
�B
�B
BB
BB
�B
�B
�B
<B
"B
jB
B
dB
0B
B
�B

=B
	�B
	7B
�B
�B
1B
�B
zB
�B
	B
�B
�B
�B
fB
�B
�B
�B
�B
�B
{B
�B
B
-B
MB
B
�B
�B
mB
B
B
9B
MB
3B
�B
�B
mB
SB
B
B
�B
�B
{B
-B
�B
[B
AB
'B
�B
B
�B
{B
gB
�B
gB
�B
B
B
�B
uB
B
{B
-B
�B
�B
oB
uB
[B
B
�B
�B
�B
�B
�B
�B
KB
KB
�B
�B
B
+B
�B
�B
�B
�B
EB
_B
zB
B
?B
�B
YB
�B
B
zB
�B
�B
	B
	7B
	�B
	B
	�B
	�B
	�B
	�B

=B

#B

	B
	�B

rB
	�B
DB

�B
B
B

�B

�B

rB

�B
	lB
	�B

rB
B
^B
�B
xB
)B

�B

�B

�B
B

�B
JB
VB
pB
B
6B
xB

rB

�B
�B
�B
VB
�B
�B
�B
vB
BB
�B
B
�B
VB
\B
�B
vB
�B
bB
�B
�B
:B
:B
[B
�B
B
gB
�B
9B
�B
�B
�B
$B
yB
B
�B
�B
B
kB
=B
WB
B
xB
�B
�B
�B
B
~B
~B
jB
�B
VB
VB
pB
 'B
 BB
!bB
#:B
# B
# B
#TB
$B
$�B
$�B
%B
%,B
%,B
%B
$�B
%FB
%B
$�B
$�B
$@B
$ZB
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%zB
%zB
%`B
%�B
%�B
&�B
'B
'�B
'�B
($B
($B
(sB
(�B
)�B
+�B
+�B
,B
,B
,B
,qB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
.}B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.cB
.}B
/iB
/5B
/�B
/�B
0!B
0�B
0oB
0�B
0�B
0�B
1[B
1�B
1vB
1AB
1�B
2-B
2�B
4B
4B
5?B
5�B
7�B
7�B
8B
7�B
8RB
8�B
9XB
:xB
:�B
:�B
;B
;B
:�B
;dB
;JB
;JB
;B
:�B
:xB
:DB
:^B
;0B
;0B
;B
:�B
:�B
:�B
:�B
:�B
;JB
;0B
;JB
;JB
;JB
;JB
;JB
;0B
;JB
;JB
;0B
;dB
<B
<B
<PB
<PB
<�B
=�B
>�B
>�B
?cB
?}B
?}B
?�B
@ B
@4B
@iB
@iB
@iB
@�B
@�B
@�B
AB
A;B
A�B
A�B
BB
B'B
B�B
C-B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C{B
D3B
E�B
E�B
FB
F?B
F�B
F�B
F�B
G_B
GzB
GzB
G�B
GzB
HfB
H�B
HKB
G�B
H�B
IB
H�B
IB
IB
IB
IB
H�B
IB
IRB
I�B
J=B
J#B
JrB
JrB
JrB
JrB
JrB
J�B
J�B
J�B
KB
K)B
J�B
KB
K)B
KB
KDB
KB
J�B
J�B
J�B
J=B
I�B
IlB
IRB
IB
I7B
IRB
I7B
I7B
I7B
I�B
J	B
J	B
J#B
I�B
J#B
I�B
IRB
IB
J#B
JXB
J�B
J�B
J�B
KxB
K�B
K�B
L�B
MB
MB
MB
M6B
MjB
M�B
M�B
NB
NVB
N�B
O�B
PHB
PHB
PHB
PbB
P�B
P�B
P�B
P�B
P�B
QNB
R:B
R�B
SuB
TB
T,B
T,B
T�B
T�B
U2B
UB
U2B
UgB
U�B
U�B
VB
VSB
U�B
V�B
XyB
Y�B
ZB
ZQB
Z�B
[	B
[WB
[qB
[�B
[�B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^B
]�B
]~B
]IB
]IB
]~B
]IB
]�B
]�B
]~B
]�B
]�B
^B
^jB
^�B
_pB
_�B
`B
`BB
`�B
`�B
`�B
`�B
aHB
a�B
bNB
cB
b�B
cnB
e`B
e�B
e�B
fB
f�B
f�B
gB
gB
gRB
gmB
g�B
g�B
g�B
g�B
h$B
h>B
h>B
hsB
h�B
h�B
i*B
iB
iB
i*B
iB
iB
iB
h�B
h�B
h�B
h�B
h�B
i*B
i�B
i�B
i�B
i�B
j0B
jKB
j�B
kB
kB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
n}B
n�B
o B
oOB
o�B
o�B
pB
pB
p!B
p!B
p�B
qvB
q�B
raB
r�B
raB
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s3B
shB
s�B
s�B
s�B
tB
tTB
t�B
uB
uB
u?B
u%B
u?B
u�B
v`B
v�B
wB
wB
wB
wB
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
xlB
xRB
x�B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zxB
zxB
z�B
z�B
z�B
{dB
{B
{dB
{B
{B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}B
}VB
}qB
}qB
}�B
~B
~BB
~BB
~BB
~(B
~(B
~B
~(B
~BB
~wB
~wB
~�B
~�B
~�B
~wB
~�B
~�B
~�B
.B
cB
�B
�B
� B
�B
��B
��B
��B
�B
�UB
��B
��B
��B
��B
��B
�[B
�[B
�[B
�[B
�uB
��B
��B
��B
��B
��B
��B
��B
�GB
�aB
�GB
�B
�B
�B
�B
�-B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230202184141  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230202184143  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230202184144  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230202184144                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230202184145  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230202184145  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230202185911                      G�O�G�O�G�O�                