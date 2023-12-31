CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:44:33Z creation;2022-06-04T17:44:33Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174433  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ئ��fff1   @ئ�7�H�@-�
=p���dA$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B���C  C  C  C�fC	�fC  C�C33C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��3D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@|(�@�{@�{A
=A?
=A]p�A
=A��A��A��A��AυA߅A�Q�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB�z�B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B�B��HB��HB��HB��HB��HB��C�C�C�C�
C	�
C�C
>C#�C�C�C�
C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CD
>CE�CG�CI�CK�CM�CO�
CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#��D#�)D$|)D%�D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{��D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D��GD��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A˦LA˙eA˘_A˖�A˓uA˓@Aˑ4AˊrA˃�A�{�A�]�A�;�A�,qA�"A���AʥzA�f2A�4A��|A��A�ÖA���A��A�R�A�i�A�u�Aʇ_A�XEA�m)A�pA�d&A�^�A�;A��A��sA���A��Aɩ�A�c�A�xAȈ�A���A�s�A� iA�m�A�~A��^Ać�A� A�R A��OA��A��3A��A��A��@A��A�C-A��AA��A�MA��A�~(A���A�;dA��bA���A��A��A�wfA���A�kA�x8A���A�h
A�1'A�͟A���A�49A��5A��A��WA�`�A��eA���A�P�A��=A���A�jKA��tA�-CA�.IA�4�A���A�W�A���A�O�A�(A��CA��"A���A~%A{B�Ay�Av	lAs�HAr~An��Ak6zAg��Ae��Aa+A_��A[��AV��AT$tAO\�AL��AJ�yAFAA�A=/�A:�3A8��A4�A1� A/��A/f�A.cA,��A)��A(��A'#:A&.IA%��A%~(A$�A#��A#��A"YKA �rA p�A xA�$A�QA��AAԕA�AA!-Ag�AuA�A��A\�A~A�KAFA�jA33A�A�]A�xA*0AیA��At�AH�A�;A�9AFA�A�@A�fAJ�A;A�@A�A5�A�YAbNAM�A�|A��A��Au�A�pA1'A
v`A	�MA	d�A��A��A�A�Aw2AA�OA�]A�Aq�A?}A�|A��A:�A �oA VA o@���@� �@��'@�4n@��@�B[@��f@��.@� �@�zx@��@��b@�Z�@�=@�p;@�1�@�@�#:@�@�s�@�+�@�y>@��N@��@��@�\�@�Z�@��o@�9�@�Y@�5?@��@��@�J�@��U@��@�?}@�I@��+@�Dg@���@�h@⎊@�;�@��@�k�@� \@��@�PH@���@�g�@�/�@�6@�e,@��2@�C�@ۨX@�a@ڧ�@��d@ٞ�@�
=@��'@�Q@�K^@�3�@��>@�j�@�Ft@���@լq@�/@�{�@Ӂ@�͟@�oi@�c�@�7@с@�+@п�@�q@Ёo@��@�˒@ϥ@�y�@�B�@��"@��@̙1@˾w@��P@�z�@�?�@��K@�0�@�q@�e,@�+@�Ov@�^5@�	@� \@�"�@ǂ�@�*0@���@ƅ�@�GE@�J�@�B[@���@ìq@��@�*�@�#:@Ê�@�A�@���@�o�@���@��[@���@��@��@���@�\)@��E@���@��@��F@���@���@�1'@��S@���@�J�@��F@�qv@�"�@���@��)@��O@�?@�A @���@���@�=q@�@��@�C-@��.@���@���@�A @��]@��@�
=@��B@���@�Ta@��o@���@���@�N<@��@��e@� �@��$@���@�T�@��@��I@�O@��=@�+@��@���@�3�@�˒@�e,@� \@���@��@�s�@�V�@�	�@�ݘ@���@�?}@�ߤ@���@��Y@�Q@���@��^@��*@�x@��@���@�U2@�@�@�=q@���@��@�5�@��|@�͟@��e@�p;@�GE@�9X@��@���@��
@��"@�@�҉@��m@���@��@�y>@�($@��@��*@���@��{@�o�@�.I@��y@���@���@��h@��u@�l"@�W�@�@�@���@�Z�@��B@��4@���@�g�@�;d@���@�xl@�]d@�V@�=q@�$@��@���@���@�Vm@��@�?@�-@��@��T@��V@�p�@�_p@�9�@��`@��e@�S�@�#:@�ԕ@�5�@��!@��.@�S@��B@��@�oi@�_@�3�@��D@��4@��@��@�@���@���@���@�K^@���@�#�@�l"@�1�@�u@���@��@�Ĝ@���@���@��'@�g�@��@��2@��}@�n�@���@���@��X@���@���@��	@��@�{J@�O@���@��}@�}V@�tT@�H�@�~@��@�خ@���@��@���@�J#@��8@���@��u@�L0@��@���@���@�b�@��@�͟@��@��F@�-@���@�|�@�.I@�*0@�(�@��@��@��@���@���@���@��9@�u�@�L0@�:�@�#:@�Q@�f@S�@o@~��@~�s@~a|@}��@|�$@{��@{"�@z�@z�2@z{�@z($@yQ�@y%F@y�@x��@x�v@x�?@x��@x��@x�@wt�@v�!@v_@u�X@u�@t��@t�o@tl"@t:�@s��@s>�@r��@rc @q&�@p�[@p�$@p�@poi@pQ�@p*�@p�@o��@o>�@nߤ@nM�@m�H@m�@mIR@l��@lی@l��@lbN@kJ#@j��@j�L@j{�@j@�@j	@iA @h�o@g��@f�R@fv�@f?@e�Z@eA @d�$@dPH@c��@c9�@c�@b�]@b)�@aԕ@a��@a�@`�?@_�@_�P@^�@^~�@^V@^B[@^O@]�^@]!�@\�@\�/@\Ɇ@\�_@\ �@[��@[�{@[�$@[��@[�@[=@Z�<@Zs�@Z	@Y�z@Y�@Yzx@YN<@X�	@X-�@W��@WK�@V��@VM�@U�3@U��@U=�@Tm�@Sƨ@Rߤ@ROv@R-@R�@Q�@Q�@P��@P"h@O�W@O�K@N�y@M��@L�@L!@K��@KH�@K�@J��@JO@I�'@IL�@H��@Hr�@HU2@H�@G�W@G�
@G��@F�M@F��@F�m@F�A@F�@E�7@E0�@D�P@D�`@D֡@D�)@D�j@D9X@C�g@C�0@C��@C9�@C33@C!-@B�2@B��@Bv�@B	@AN<@@�`@@��@@�@?��@?�P@?U�@?$t@>�<@=��@=�'@=L�@<�	@<��@<֡@<ѷ@<Ĝ@<`�@;�+@;ݘ@;��@;e�@:�R@9��@9F@9+�@9#�@9@@9�@8֡@8��@8`�@7��@7C�@6��@5�@5�^@5�-@5�t@5��@5��@5��@5��@5w2@5F@4�P@4�p@4|�@4N�@4%�@3��@3|�@3a@3U�@3(@2�@2��@2��@2R�@1�@1��@1w2@15�@0�e@0-�@/��@/��@/��@/>�@/�@.�B@.��@.C�@-��@-c@-O�@,��@+�]@+Y@*��@*�@*��@*+k@)�@)�o@)�@)�@)��@)O�@(�/@(�?@(~(@(M@(G@'�K@'v`@'1�@'@&�@&��@&z@&W�@%��@%��@%�'@%��@%��@%^�@%#�@$ی@$�@$Z@$H@#��@#ݘ@#�@#~�@#y�@#\)@#33@#�@"��@"�<@"{�@"R�@"+k@"u@!�t@!rG@!A @ �P@ ��@�@�[@��@o�@]�@P�@$t@��@�}@0U@�>@�@��@��@-w@�@ی@�_@I�@1@��@�F@��@�V@x@Z�@o@��@��@҉@��@��@��@��@}V@u%@v�@v�@c @R�@L0@!�@��@�z@�@��@��@zx@k�@L�@@@�@�?@��@tT@M@b@�
@˒@�[@��@�	@Z�@=@�@��@��@+k@ �@�)@�>@�@�@��@m]@Y�@&�@ѷ@`�@9X@�@��@��@�q@�4@��@�c@�H@�}@��@YK@;�@$�@�@��@��@rG@�@�p@�o@oi@D�@$@~@�@	�@�A@�@�a@��@��@s@H�@S@�s@��@��@6�@�@�#@ԕ@��@��@w2@c�@T�@N<@&�@�@z�@r�@tT@oi@l"@`�@>B@~@@�
@y�@>�@
�s@
z@
W�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A˦LA˙eA˘_A˖�A˓uA˓@Aˑ4AˊrA˃�A�{�A�]�A�;�A�,qA�"A���AʥzA�f2A�4A��|A��A�ÖA���A��A�R�A�i�A�u�Aʇ_A�XEA�m)A�pA�d&A�^�A�;A��A��sA���A��Aɩ�A�c�A�xAȈ�A���A�s�A� iA�m�A�~A��^Ać�A� A�R A��OA��A��3A��A��A��@A��A�C-A��AA��A�MA��A�~(A���A�;dA��bA���A��A��A�wfA���A�kA�x8A���A�h
A�1'A�͟A���A�49A��5A��A��WA�`�A��eA���A�P�A��=A���A�jKA��tA�-CA�.IA�4�A���A�W�A���A�O�A�(A��CA��"A���A~%A{B�Ay�Av	lAs�HAr~An��Ak6zAg��Ae��Aa+A_��A[��AV��AT$tAO\�AL��AJ�yAFAA�A=/�A:�3A8��A4�A1� A/��A/f�A.cA,��A)��A(��A'#:A&.IA%��A%~(A$�A#��A#��A"YKA �rA p�A xA�$A�QA��AAԕA�AA!-Ag�AuA�A��A\�A~A�KAFA�jA33A�A�]A�xA*0AیA��At�AH�A�;A�9AFA�A�@A�fAJ�A;A�@A�A5�A�YAbNAM�A�|A��A��Au�A�pA1'A
v`A	�MA	d�A��A��A�A�Aw2AA�OA�]A�Aq�A?}A�|A��A:�A �oA VA o@���@� �@��'@�4n@��@�B[@��f@��.@� �@�zx@��@��b@�Z�@�=@�p;@�1�@�@�#:@�@�s�@�+�@�y>@��N@��@��@�\�@�Z�@��o@�9�@�Y@�5?@��@��@�J�@��U@��@�?}@�I@��+@�Dg@���@�h@⎊@�;�@��@�k�@� \@��@�PH@���@�g�@�/�@�6@�e,@��2@�C�@ۨX@�a@ڧ�@��d@ٞ�@�
=@��'@�Q@�K^@�3�@��>@�j�@�Ft@���@լq@�/@�{�@Ӂ@�͟@�oi@�c�@�7@с@�+@п�@�q@Ёo@��@�˒@ϥ@�y�@�B�@��"@��@̙1@˾w@��P@�z�@�?�@��K@�0�@�q@�e,@�+@�Ov@�^5@�	@� \@�"�@ǂ�@�*0@���@ƅ�@�GE@�J�@�B[@���@ìq@��@�*�@�#:@Ê�@�A�@���@�o�@���@��[@���@��@��@���@�\)@��E@���@��@��F@���@���@�1'@��S@���@�J�@��F@�qv@�"�@���@��)@��O@�?@�A @���@���@�=q@�@��@�C-@��.@���@���@�A @��]@��@�
=@��B@���@�Ta@��o@���@���@�N<@��@��e@� �@��$@���@�T�@��@��I@�O@��=@�+@��@���@�3�@�˒@�e,@� \@���@��@�s�@�V�@�	�@�ݘ@���@�?}@�ߤ@���@��Y@�Q@���@��^@��*@�x@��@���@�U2@�@�@�=q@���@��@�5�@��|@�͟@��e@�p;@�GE@�9X@��@���@��
@��"@�@�҉@��m@���@��@�y>@�($@��@��*@���@��{@�o�@�.I@��y@���@���@��h@��u@�l"@�W�@�@�@���@�Z�@��B@��4@���@�g�@�;d@���@�xl@�]d@�V@�=q@�$@��@���@���@�Vm@��@�?@�-@��@��T@��V@�p�@�_p@�9�@��`@��e@�S�@�#:@�ԕ@�5�@��!@��.@�S@��B@��@�oi@�_@�3�@��D@��4@��@��@�@���@���@���@�K^@���@�#�@�l"@�1�@�u@���@��@�Ĝ@���@���@��'@�g�@��@��2@��}@�n�@���@���@��X@���@���@��	@��@�{J@�O@���@��}@�}V@�tT@�H�@�~@��@�خ@���@��@���@�J#@��8@���@��u@�L0@��@���@���@�b�@��@�͟@��@��F@�-@���@�|�@�.I@�*0@�(�@��@��@��@���@���@���@��9@�u�@�L0@�:�@�#:@�Q@�f@S�@o@~��@~�s@~a|@}��@|�$@{��@{"�@z�@z�2@z{�@z($@yQ�@y%F@y�@x��@x�v@x�?@x��@x��@x�@wt�@v�!@v_@u�X@u�@t��@t�o@tl"@t:�@s��@s>�@r��@rc @q&�@p�[@p�$@p�@poi@pQ�@p*�@p�@o��@o>�@nߤ@nM�@m�H@m�@mIR@l��@lی@l��@lbN@kJ#@j��@j�L@j{�@j@�@j	@iA @h�o@g��@f�R@fv�@f?@e�Z@eA @d�$@dPH@c��@c9�@c�@b�]@b)�@aԕ@a��@a�@`�?@_�@_�P@^�@^~�@^V@^B[@^O@]�^@]!�@\�@\�/@\Ɇ@\�_@\ �@[��@[�{@[�$@[��@[�@[=@Z�<@Zs�@Z	@Y�z@Y�@Yzx@YN<@X�	@X-�@W��@WK�@V��@VM�@U�3@U��@U=�@Tm�@Sƨ@Rߤ@ROv@R-@R�@Q�@Q�@P��@P"h@O�W@O�K@N�y@M��@L�@L!@K��@KH�@K�@J��@JO@I�'@IL�@H��@Hr�@HU2@H�@G�W@G�
@G��@F�M@F��@F�m@F�A@F�@E�7@E0�@D�P@D�`@D֡@D�)@D�j@D9X@C�g@C�0@C��@C9�@C33@C!-@B�2@B��@Bv�@B	@AN<@@�`@@��@@�@?��@?�P@?U�@?$t@>�<@=��@=�'@=L�@<�	@<��@<֡@<ѷ@<Ĝ@<`�@;�+@;ݘ@;��@;e�@:�R@9��@9F@9+�@9#�@9@@9�@8֡@8��@8`�@7��@7C�@6��@5�@5�^@5�-@5�t@5��@5��@5��@5��@5w2@5F@4�P@4�p@4|�@4N�@4%�@3��@3|�@3a@3U�@3(@2�@2��@2��@2R�@1�@1��@1w2@15�@0�e@0-�@/��@/��@/��@/>�@/�@.�B@.��@.C�@-��@-c@-O�@,��@+�]@+Y@*��@*�@*��@*+k@)�@)�o@)�@)�@)��@)O�@(�/@(�?@(~(@(M@(G@'�K@'v`@'1�@'@&�@&��@&z@&W�@%��@%��@%�'@%��@%��@%^�@%#�@$ی@$�@$Z@$H@#��@#ݘ@#�@#~�@#y�@#\)@#33@#�@"��@"�<@"{�@"R�@"+k@"u@!�t@!rG@!A @ �P@ ��@�@�[@��@o�@]�@P�@$t@��@�}@0U@�>@�@��@��@-w@�@ی@�_@I�@1@��@�F@��@�V@x@Z�@o@��@��@҉@��@��@��@��@}V@u%@v�@v�@c @R�@L0@!�@��@�z@�@��@��@zx@k�@L�@@@�@�?@��@tT@M@b@�
@˒@�[@��@�	@Z�@=@�@��@��@+k@ �@�)@�>@�@�@��@m]@Y�@&�@ѷ@`�@9X@�@��@��@�q@�4@��@�c@�H@�}@��@YK@;�@$�@�@��@��@rG@�@�p@�o@oi@D�@$@~@�@	�@�A@�@�a@��@��@s@H�@S@�s@��@��@6�@�@�#@ԕ@��@��@w2@c�@T�@N<@&�@�@z�@r�@tT@oi@l"@`�@>B@~@@�
@y�@>�@
�s@
z@
W�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BzByXBy�Bz^Bz�Bz�B{B|�B�'B��B�B�'B�B�B	A B	ʦB
�B
;0B
W
B
ZkB
Z�B
`\B
tnB
�'B
��B
��B
�{B
�
B
��B
�5B
�hB
��B
�)B
�'B
��B
��B
�+B
�jBpB#�B>�Bd�Br-B��B�B�{B��B�:B�WB�vB��B��B��B�B�B�B<�BKxBP�BWYBX�BX�BW�BR�B]�Bb�B]IBJ�B&LB)B�B?BB�B�cB��B�[B��B�|B�pB��B��B�\B��BuB2|BuB&B'B!|B%B
�B
��B
k�B
n�B
a�B
u�B
�iB
v�B
l�B
e�B
WsB
G+B
>]B
%FB
TB
 �B	�]B	�B	��B	��B	�B	�+B	p�B	<�B	*B	sB	�B�B��B�B��B��B��B�TB��B��BĶB�7B�BɺB��B�XB��B�BҽBޞB�mB�B�B��B��B��B��B�B	
�B	#nB	0!B	3�B	5�B	=qB	B�B	O�B	S�B	V�B	ZB	\]B	c�B	jeB	nIB	tTB	B	}B	�'B	�KB	��B	��B	��B	�{B	�eB	��B	��B	��B	�bB	� B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�nB	�B	��B	��B	��B	��B	��B	�rB	��B	żB	�tB	�tB	��B	�B	�gB	ŢB	ƨB	ǮB	��B	�	B	�B	��B	ȚB	�fB	��B	��B	ʌB	ˬB	�B	҉B	уB	��B	�B	�B	� B	�TB	� B	� B	�&B	ҽB	�oB	�TB	��B	�B	�}B	��B	ΥB	οB	ΊB	�\B	��B	�}B	��B	�bB	ЗB	бB	�VB	�~B	�jB	��B	ЗB	��B	�B	҉B	�&B	ӏB	өB	өB	ҽB	�@B	ңB	��B	�[B	�oB	�:B	ѝB	҉B	�TB	҉B	��B	�FB	ԯB	�gB	ՁB	��B	��B	�B	ևB	֡B	��B	��B	��B	�_B	�B	ٴB	�kB	�kB	�B	�B	�xB	ݲB	�B	�5B	߾B	�'B	��B	�\B	�'B	��B	��B	��B	ޞB	ݘB	ݲB	ݘB	ݘB	޸B	ޞB	��B	�B	�B	�nB	�B	�B	�nB	�B	�B	�)B	�"B	�B	�"B	�WB	�B	�_B	��B	�>B	�B	�wB	�/B	��B	�B	�}B	��B	�B	��B	�B	��B	��B	��B	�AB	�|B	�aB	�aB	�hB	��B	�B	�B	�B	�B	�nB	��B	�`B	��B	��B	�8B	�	B	�DB	�DB	�B	�DB	�*B	��B	��B	��B	�*B	��B	��B	�JB	��B	��B	��B	��B	��B	��B	�wB	��B	��B
  B	��B
 B
 �B
B
�B
�B
�B
3B
�B
9B
B
mB
�B
B
?B
�B
�B
EB
_B
+B
�B
�B
YB
�B
mB
9B
�B
�B
�B
EB
zB
�B
�B
�B
	7B
	B
	B

�B
xB
B
dB
dB
�B
�B
�B
�B
B
B
�B
B
�B
�B
B
B
B
pB
(B
vB
�B
�B
�B
B
B
HB
HB
HB
HB
�B
�B
}B
.B
B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
&B
�B
FB
2B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
FB
�B
�B
:B
 B
:B
:B
�B
[B
[B
�B
FB
B
[B
{B
?B
�B
�B
�B
sB
�B
�B
�B
+B
B
EB
EB
�B
�B
�B
�B
�B
�B
�B
/B
IB
�B
B
jB
�B
�B
�B
�B
B
�B
 �B
!bB
!�B
!�B
"4B
"�B
"�B
"�B
#:B
#TB
#nB
#�B
$�B
$�B
%B
%�B
&B
&�B
&�B
'8B
'�B
($B
(>B
(>B
)B
)DB
*B
*eB
*KB
*eB
*eB
*eB
*�B
*�B
*�B
*�B
*�B
+QB
+QB
+kB
+kB
+�B
+�B
,"B
,=B
,=B
,"B
,qB
-B
-wB
.�B
.�B
.�B
.�B
/B
/iB
0!B
0;B
0;B
0;B
0oB
0oB
0oB
0;B
0�B
0�B
1�B
1�B
2GB
2|B
4B
4nB
4�B
4�B
5%B
5tB
5�B
5�B
7fB
7�B
7�B
7�B
7�B
7�B
8B
8B
8lB
8�B
8�B
9	B
9rB
9�B
9�B
:^B
:^B
:^B
:�B
;�B
;�B
;�B
<B
<B
;�B
<�B
<�B
=VB
>]B
>BB
>]B
>�B
?.B
?cB
?}B
@B
@OB
@4B
@4B
@�B
@OB
@OB
@ B
@�B
A B
A�B
C�B
C�B
C�B
C�B
C�B
DMB
C�B
C�B
C�B
C�B
DMB
DgB
DB
C�B
D�B
E9B
E�B
E�B
FYB
F�B
F�B
F�B
G+B
G+B
G_B
G�B
G�B
G�B
H�B
IB
I7B
IlB
IlB
IRB
I�B
JrB
KB
K)B
J�B
K)B
K)B
K�B
K�B
L�B
L0B
L0B
L�B
NpB
N�B
N�B
O(B
O\B
O\B
O�B
PHB
P}B
P�B
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
RB
RoB
RoB
RoB
R�B
R�B
S@B
S[B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
T,B
TaB
T{B
TaB
TaB
T�B
T�B
T�B
T�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
W
B
W�B
W�B
W�B
XB
X+B
X+B
XB
XB
XEB
X�B
XyB
XyB
XyB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
Y�B
Z7B
Z7B
Z7B
ZQB
ZQB
Z7B
Z7B
ZkB
ZQB
ZQB
ZkB
Z�B
Z�B
[WB
[�B
[�B
[�B
\xB
\xB
\xB
\�B
]B
\�B
\�B
]/B
]�B
]�B
^B
^5B
^�B
_;B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_pB
_!B
_VB
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
abB
a�B
a�B
a�B
a�B
b�B
c�B
c�B
c�B
dtB
dZB
d�B
d�B
ezB
e�B
e�B
ffB
gB
g�B
g�B
h
B
h>B
h>B
h$B
h>B
h�B
h�B
h�B
h�B
iB
iDB
i*B
iyB
i�B
i�B
i�B
i�B
j0B
jKB
jeB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n}B
ncB
n}B
n�B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p;B
p;B
p�B
p�B
p�B
q'B
qB
qAB
qAB
q[B
qvB
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
s3B
shB
s�B
tB
t9B
tTB
t�B
t�B
t�B
t�B
uZB
u?B
u?B
utB
u�B
u�B
u�B
u�B
vB
vFB
v`B
vzB
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xRB
xRB
x�B
x�B
x�B
y	B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
zxB
z�B
{B
{JB
{JB
{JB
{JB
{JB
{dB
{dB
{�B
{�B
{�B
|PB
|�B
|�B
}VB
}q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BzByXBy�Bz^Bz�Bz�B{B|�B�'B��B�B�'B�B�B	A B	ʦB
�B
;0B
W
B
ZkB
Z�B
`\B
tnB
�'B
��B
��B
�{B
�
B
��B
�5B
�hB
��B
�)B
�'B
��B
��B
�+B
�jBpB#�B>�Bd�Br-B��B�B�{B��B�:B�WB�vB��B��B��B�B�B�B<�BKxBP�BWYBX�BX�BW�BR�B]�Bb�B]IBJ�B&LB)B�B?BB�B�cB��B�[B��B�|B�pB��B��B�\B��BuB2|BuB&B'B!|B%B
�B
��B
k�B
n�B
a�B
u�B
�iB
v�B
l�B
e�B
WsB
G+B
>]B
%FB
TB
 �B	�]B	�B	��B	��B	�B	�+B	p�B	<�B	*B	sB	�B�B��B�B��B��B��B�TB��B��BĶB�7B�BɺB��B�XB��B�BҽBޞB�mB�B�B��B��B��B��B�B	
�B	#nB	0!B	3�B	5�B	=qB	B�B	O�B	S�B	V�B	ZB	\]B	c�B	jeB	nIB	tTB	B	}B	�'B	�KB	��B	��B	��B	�{B	�eB	��B	��B	��B	�bB	� B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�nB	�B	��B	��B	��B	��B	��B	�rB	��B	żB	�tB	�tB	��B	�B	�gB	ŢB	ƨB	ǮB	��B	�	B	�B	��B	ȚB	�fB	��B	��B	ʌB	ˬB	�B	҉B	уB	��B	�B	�B	� B	�TB	� B	� B	�&B	ҽB	�oB	�TB	��B	�B	�}B	��B	ΥB	οB	ΊB	�\B	��B	�}B	��B	�bB	ЗB	бB	�VB	�~B	�jB	��B	ЗB	��B	�B	҉B	�&B	ӏB	өB	өB	ҽB	�@B	ңB	��B	�[B	�oB	�:B	ѝB	҉B	�TB	҉B	��B	�FB	ԯB	�gB	ՁB	��B	��B	�B	ևB	֡B	��B	��B	��B	�_B	�B	ٴB	�kB	�kB	�B	�B	�xB	ݲB	�B	�5B	߾B	�'B	��B	�\B	�'B	��B	��B	��B	ޞB	ݘB	ݲB	ݘB	ݘB	޸B	ޞB	��B	�B	�B	�nB	�B	�B	�nB	�B	�B	�)B	�"B	�B	�"B	�WB	�B	�_B	��B	�>B	�B	�wB	�/B	��B	�B	�}B	��B	�B	��B	�B	��B	��B	��B	�AB	�|B	�aB	�aB	�hB	��B	�B	�B	�B	�B	�nB	��B	�`B	��B	��B	�8B	�	B	�DB	�DB	�B	�DB	�*B	��B	��B	��B	�*B	��B	��B	�JB	��B	��B	��B	��B	��B	��B	�wB	��B	��B
  B	��B
 B
 �B
B
�B
�B
�B
3B
�B
9B
B
mB
�B
B
?B
�B
�B
EB
_B
+B
�B
�B
YB
�B
mB
9B
�B
�B
�B
EB
zB
�B
�B
�B
	7B
	B
	B

�B
xB
B
dB
dB
�B
�B
�B
�B
B
B
�B
B
�B
�B
B
B
B
pB
(B
vB
�B
�B
�B
B
B
HB
HB
HB
HB
�B
�B
}B
.B
B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
&B
�B
FB
2B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
FB
�B
�B
:B
 B
:B
:B
�B
[B
[B
�B
FB
B
[B
{B
?B
�B
�B
�B
sB
�B
�B
�B
+B
B
EB
EB
�B
�B
�B
�B
�B
�B
�B
/B
IB
�B
B
jB
�B
�B
�B
�B
B
�B
 �B
!bB
!�B
!�B
"4B
"�B
"�B
"�B
#:B
#TB
#nB
#�B
$�B
$�B
%B
%�B
&B
&�B
&�B
'8B
'�B
($B
(>B
(>B
)B
)DB
*B
*eB
*KB
*eB
*eB
*eB
*�B
*�B
*�B
*�B
*�B
+QB
+QB
+kB
+kB
+�B
+�B
,"B
,=B
,=B
,"B
,qB
-B
-wB
.�B
.�B
.�B
.�B
/B
/iB
0!B
0;B
0;B
0;B
0oB
0oB
0oB
0;B
0�B
0�B
1�B
1�B
2GB
2|B
4B
4nB
4�B
4�B
5%B
5tB
5�B
5�B
7fB
7�B
7�B
7�B
7�B
7�B
8B
8B
8lB
8�B
8�B
9	B
9rB
9�B
9�B
:^B
:^B
:^B
:�B
;�B
;�B
;�B
<B
<B
;�B
<�B
<�B
=VB
>]B
>BB
>]B
>�B
?.B
?cB
?}B
@B
@OB
@4B
@4B
@�B
@OB
@OB
@ B
@�B
A B
A�B
C�B
C�B
C�B
C�B
C�B
DMB
C�B
C�B
C�B
C�B
DMB
DgB
DB
C�B
D�B
E9B
E�B
E�B
FYB
F�B
F�B
F�B
G+B
G+B
G_B
G�B
G�B
G�B
H�B
IB
I7B
IlB
IlB
IRB
I�B
JrB
KB
K)B
J�B
K)B
K)B
K�B
K�B
L�B
L0B
L0B
L�B
NpB
N�B
N�B
O(B
O\B
O\B
O�B
PHB
P}B
P�B
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
RB
RoB
RoB
RoB
R�B
R�B
S@B
S[B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
T,B
TaB
T{B
TaB
TaB
T�B
T�B
T�B
T�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
W
B
W�B
W�B
W�B
XB
X+B
X+B
XB
XB
XEB
X�B
XyB
XyB
XyB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
Y�B
Z7B
Z7B
Z7B
ZQB
ZQB
Z7B
Z7B
ZkB
ZQB
ZQB
ZkB
Z�B
Z�B
[WB
[�B
[�B
[�B
\xB
\xB
\xB
\�B
]B
\�B
\�B
]/B
]�B
]�B
^B
^5B
^�B
_;B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_pB
_!B
_VB
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
abB
a�B
a�B
a�B
a�B
b�B
c�B
c�B
c�B
dtB
dZB
d�B
d�B
ezB
e�B
e�B
ffB
gB
g�B
g�B
h
B
h>B
h>B
h$B
h>B
h�B
h�B
h�B
h�B
iB
iDB
i*B
iyB
i�B
i�B
i�B
i�B
j0B
jKB
jeB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n}B
ncB
n}B
n�B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p;B
p;B
p�B
p�B
p�B
q'B
qB
qAB
qAB
q[B
qvB
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
s3B
shB
s�B
tB
t9B
tTB
t�B
t�B
t�B
t�B
uZB
u?B
u?B
utB
u�B
u�B
u�B
u�B
vB
vFB
v`B
vzB
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xRB
xRB
x�B
x�B
x�B
y	B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
zxB
z�B
{B
{JB
{JB
{JB
{JB
{JB
{dB
{dB
{�B
{�B
{�B
|PB
|�B
|�B
}VB
}q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104934  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174433  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174433  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174433                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024440  QCP$                G�O�G�O�G�O�         208F35EJA  ARGQrqcpc3.6                                                                20220605024440  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                