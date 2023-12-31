CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:45:05Z creation;2022-06-04T17:45:05Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174505  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @خ}�~K1   @خ~[6;�@-�C���d=?|�h1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@���A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BA33BG33BO��BX  B`  Bh  Bp  Bx  B���B���B�33B�ffB���B���B�  B�  B�  B�  B�33B�ffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@u@��H@�{A ��A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B@��BF��BO\)BWB_BgBoBwB�z�B�z�B�{B�G�B��B��B��HB��HB��HB��HB�{B�G�B��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C
>C�C�C	�C�C�C�C�C�C�C�C�C�C�C�
C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C:
>C<
>C=�C?�CA�
CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cn
>Cp
>Cq�Cs�Cu�
Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM��DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW��DX�DX��DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D��GD��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��GD��D��D�>D�~D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aɪ�Aɥ�AɜxAɞ�Aɛ�Aɛ	Aɘ�AɈfA�n/A�d&A�a�A�_�A�\]A�[�A�\�A�WsA�W�A�XEA�YA�\�A�d�A�o�AɅ�A���A�یA���A�e`A�2�A�	7A� 4A��A���A�~�A�ѷA�y>A��1A�
�A�?�A��A��%A��A�Q�A���A���A��\A��=A�	A���A�l�A�Y�A��A��A���A���A�B�A��HA���A�]/A�%A��XA�  A��A�i�A��A��	A�'�A�5�A�A�A�;�A��A���A���A���A�m]A���A��*A�:^A�B'A�5?A���A���A�=�A�?A��8A���A�JXA�A�l�A��qA�\)A{��Ax�
Av��Au!-As�{ApGEAl�Ac��A^�$A\�AY�)AW�AVQAT�AQ�AK�9AJ�xAG�KAG-AE�ADj�AC�A@�AA>cA<A;	A:VmA9��A8��A7xlA4��A2%FA0��A-o A*��A(�A&��A%��A%�A#��A"dZA!>BA!"hA!�A!
=A!7A �hA�AK�A�ASA��A-A͟Ao A2�A��A��A�A��A��A�3A��A%�A��A�IA��A�zA�A�AZ�A��A�A�|A*0A��A�IA�A��A!�A*0Aw2A�QA�FA��A��A[�A͟A�A5?A��A	A��A��A�A�!A��A{A֡AjAC�A
o�A	�=A	_A�hA^5A�tAAk�A-A�sAԕAsA/AA��A�jA�'A�A�[AA��A خA �rA 7�@�ߤ@��@�O�@���@�=q@��3@��k@�w2@��@��6@��@���@��@�9�@��@��@��B@�oi@�^5@�Q@�:*@�7�@��@�U�@�($@��@��N@�N<@��@��c@��'@���@��@�4@���@�z@��@�h�@癚@沖@思@��T@�xl@�  @�@@���@�H@�W?@�A @��@�	�@ߗ$@��@ޤ�@ޓu@�>B@��@�B[@�#:@��@�ݘ@�(�@ڹ�@��@�>�@��E@�7�@�"h@��j@ס�@�b�@��5@֑ @�	�@ՠ'@ԅ�@��@�b�@��c@�_@�B�@�=q@Ϝ�@��@�V�@͎"@�C@��@�l�@˓�@ʵ�@�~�@�z@�;�@��.@ɚk@�6z@���@ȷ�@ș1@ǶF@�Y�@�o@���@�Q�@��@�c�@�oi@��#@�q@§@��@��V@�O@��@���@�C�@�!�@��.@���@���@�X�@���@�;�@��z@��@�x�@�V@�:�@��.@���@���@�G@� i@���@�&�@�� @���@�֡@�y>@�l"@���@���@���@�33@�_�@��0@�qv@�	l@���@��\@�h�@��@��t@��@���@�f�@�/@���@��@��@�7�@�.I@�PH@��@��@�T�@���@�U2@�M@��D@�}�@�+@�@��@�1�@��W@��d@��F@���@���@�|@�E9@�o@���@���@���@�6�@���@�4�@���@��@��@��6@���@�j@��@��Q@��4@�m]@�RT@��@� i@��,@�~@���@�L�@�V@��@��X@�Xy@��&@��K@���@��?@�~@�p;@�I�@��@��@�j@�J#@�.I@��@�'R@��D@���@���@��@�F@��@��/@��@�w�@�n/@���@�Q@���@���@���@�p�@�C@�kQ@�G@���@�\�@��	@���@��@�n/@�L�@��@���@���@�l�@�1�@��;@�Vm@�@O@�>�@�A @�*0@��@��,@��@���@�j@�8�@��}@�J�@��@��W@��H@�A�@���@�h�@��@�b�@�F@�2a@��@���@���@��L@���@�u%@�$�@��Q@���@���@��M@�|�@�y�@�F@���@���@��1@���@�q@�_@�_@���@��@�\�@�%F@� i@��@���@�\�@�x@�� @���@�g�@�@O@��y@��x@�s�@�5?@�#:@��#@��^@���@�|@�RT@��c@��O@���@��Y@���@�j@�M�@�:�@��@��@o�@S�@!-@~�@}�"@}%F@|�)@|7�@{{J@{�@z�L@z��@y��@y<6@y@@x�O@x4n@w��@wx@wA�@v��@v.�@u��@u�'@t�K@t�D@t��@t�@t�@toi@s�;@sC�@r��@rC�@q�@q�@p�U@pV�@p"h@o��@o�@oK�@n�2@n��@nxl@nO@me,@l`�@lb@k�g@k|�@kW?@j��@jZ�@i�>@izx@hm�@g�
@g"�@f�b@fc @eԕ@eG�@d�@d��@d�.@dZ@d~@c�$@c&@bv�@b!�@a�z@a�S@arG@aIR@a@`�E@`y>@`*�@`�@_�@_�F@_y�@^��@^҉@^͟@^R�@]�@]��@]��@]s�@]!�@\Ɇ@\Z@[��@[��@[�k@[�k@[�4@[F�@Z�y@Z6�@Y�.@Y@Y�S@YA @X�@Xu�@W�]@W��@Wy�@V�s@V��@V8�@Uf�@T�u@T?�@S�w@S/�@R��@R�A@Ru%@R@Q��@Q�z@Q�@P��@PD�@P�@O�@O�6@O�$@O9�@N�F@Np;@N-@M�j@Mu�@M<6@M�@L�@L��@LK^@L?�@Lx@K��@K��@K\)@K9�@J�b@J�\@Ji�@J:*@I��@I�M@I^�@I:�@I�@H��@H�.@H?�@G�&@Gs@G+@G�@G�@F�@F�@E��@E:�@D��@Dq@D[�@D9X@C� @Ca@C�@B�s@B��@BE�@B8�@A��@A�~@A%F@@�E@@��@@��@@|�@?��@?|�@?Mj@?8@>�@>�1@>?@>�@=��@=��@=c@=?}@<��@<��@<ѷ@<��@<�D@<N�@;��@;{J@:��@: �@9ԕ@9m]@9�@8�f@8Ĝ@8bN@8@7��@7b�@6�@6�F@68�@6!�@5�j@5��@5a�@4��@4��@4I�@4�@3��@3�@2Ov@2-@1��@1�@1�@1J�@15�@1-w@1�@0�`@0��@0�j@0u�@/��@/��@/{J@/RT@/1�@/
=@.�R@.�x@.�+@.-@-�N@-�t@-u�@-4@,�v@,��@, �@+ƨ@+�$@+|�@+s@+iD@+F�@*q�@*.�@)�D@)�3@)�7@)G�@(�P@(tT@(@'�}@'��@'�@'l�@'$t@'�@'�@&�B@&V@&!�@&�@& �@%�^@%�@$��@$�@$z�@$u�@$m�@$@#�Q@#خ@#�}@#P�@"�M@"��@"�x@"_@!Dg@!�@ �j@ ��@ �.@ ~(@ q@ e�@ U2@ N�@ "h@ 1@��@�@��@{@�@e,@-w@%@֡@z�@(�@��@��@��@�@��@��@�@	@�@c�@�`@�u@PH@�@��@�@@��@�	@|�@iD@P�@>�@+@��@i�@#:@u@�z@J�@�|@�I@Ft@��@a@4�@o@��@��@q�@$�@�.@��@��@�"@Y�@B�@&�@�P@�?@�Y@>B@�@�A@�g@ƨ@�*@�P@x@n/@a@]�@9�@�@��@R�@8�@3�@!�@�~@Dg@7L@�@�@�P@�E@g8@%�@�@�@�;@خ@��@��@��@��@qv@=@!-@
��@
�M@
�c@
��@
�]@
�@
ȴ@
�<@
��@
�\@
��@
xl@
5?@	�Z@	�>@	��@	�@	�X@	s�@	B�@	�@	q@	+@��@��@��@��@��@�.@g8@M@H@7�@~@�@�A@�@�:@|�@l�@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aɪ�Aɥ�AɜxAɞ�Aɛ�Aɛ	Aɘ�AɈfA�n/A�d&A�a�A�_�A�\]A�[�A�\�A�WsA�W�A�XEA�YA�\�A�d�A�o�AɅ�A���A�یA���A�e`A�2�A�	7A� 4A��A���A�~�A�ѷA�y>A��1A�
�A�?�A��A��%A��A�Q�A���A���A��\A��=A�	A���A�l�A�Y�A��A��A���A���A�B�A��HA���A�]/A�%A��XA�  A��A�i�A��A��	A�'�A�5�A�A�A�;�A��A���A���A���A�m]A���A��*A�:^A�B'A�5?A���A���A�=�A�?A��8A���A�JXA�A�l�A��qA�\)A{��Ax�
Av��Au!-As�{ApGEAl�Ac��A^�$A\�AY�)AW�AVQAT�AQ�AK�9AJ�xAG�KAG-AE�ADj�AC�A@�AA>cA<A;	A:VmA9��A8��A7xlA4��A2%FA0��A-o A*��A(�A&��A%��A%�A#��A"dZA!>BA!"hA!�A!
=A!7A �hA�AK�A�ASA��A-A͟Ao A2�A��A��A�A��A��A�3A��A%�A��A�IA��A�zA�A�AZ�A��A�A�|A*0A��A�IA�A��A!�A*0Aw2A�QA�FA��A��A[�A͟A�A5?A��A	A��A��A�A�!A��A{A֡AjAC�A
o�A	�=A	_A�hA^5A�tAAk�A-A�sAԕAsA/AA��A�jA�'A�A�[AA��A خA �rA 7�@�ߤ@��@�O�@���@�=q@��3@��k@�w2@��@��6@��@���@��@�9�@��@��@��B@�oi@�^5@�Q@�:*@�7�@��@�U�@�($@��@��N@�N<@��@��c@��'@���@��@�4@���@�z@��@�h�@癚@沖@思@��T@�xl@�  @�@@���@�H@�W?@�A @��@�	�@ߗ$@��@ޤ�@ޓu@�>B@��@�B[@�#:@��@�ݘ@�(�@ڹ�@��@�>�@��E@�7�@�"h@��j@ס�@�b�@��5@֑ @�	�@ՠ'@ԅ�@��@�b�@��c@�_@�B�@�=q@Ϝ�@��@�V�@͎"@�C@��@�l�@˓�@ʵ�@�~�@�z@�;�@��.@ɚk@�6z@���@ȷ�@ș1@ǶF@�Y�@�o@���@�Q�@��@�c�@�oi@��#@�q@§@��@��V@�O@��@���@�C�@�!�@��.@���@���@�X�@���@�;�@��z@��@�x�@�V@�:�@��.@���@���@�G@� i@���@�&�@�� @���@�֡@�y>@�l"@���@���@���@�33@�_�@��0@�qv@�	l@���@��\@�h�@��@��t@��@���@�f�@�/@���@��@��@�7�@�.I@�PH@��@��@�T�@���@�U2@�M@��D@�}�@�+@�@��@�1�@��W@��d@��F@���@���@�|@�E9@�o@���@���@���@�6�@���@�4�@���@��@��@��6@���@�j@��@��Q@��4@�m]@�RT@��@� i@��,@�~@���@�L�@�V@��@��X@�Xy@��&@��K@���@��?@�~@�p;@�I�@��@��@�j@�J#@�.I@��@�'R@��D@���@���@��@�F@��@��/@��@�w�@�n/@���@�Q@���@���@���@�p�@�C@�kQ@�G@���@�\�@��	@���@��@�n/@�L�@��@���@���@�l�@�1�@��;@�Vm@�@O@�>�@�A @�*0@��@��,@��@���@�j@�8�@��}@�J�@��@��W@��H@�A�@���@�h�@��@�b�@�F@�2a@��@���@���@��L@���@�u%@�$�@��Q@���@���@��M@�|�@�y�@�F@���@���@��1@���@�q@�_@�_@���@��@�\�@�%F@� i@��@���@�\�@�x@�� @���@�g�@�@O@��y@��x@�s�@�5?@�#:@��#@��^@���@�|@�RT@��c@��O@���@��Y@���@�j@�M�@�:�@��@��@o�@S�@!-@~�@}�"@}%F@|�)@|7�@{{J@{�@z�L@z��@y��@y<6@y@@x�O@x4n@w��@wx@wA�@v��@v.�@u��@u�'@t�K@t�D@t��@t�@t�@toi@s�;@sC�@r��@rC�@q�@q�@p�U@pV�@p"h@o��@o�@oK�@n�2@n��@nxl@nO@me,@l`�@lb@k�g@k|�@kW?@j��@jZ�@i�>@izx@hm�@g�
@g"�@f�b@fc @eԕ@eG�@d�@d��@d�.@dZ@d~@c�$@c&@bv�@b!�@a�z@a�S@arG@aIR@a@`�E@`y>@`*�@`�@_�@_�F@_y�@^��@^҉@^͟@^R�@]�@]��@]��@]s�@]!�@\Ɇ@\Z@[��@[��@[�k@[�k@[�4@[F�@Z�y@Z6�@Y�.@Y@Y�S@YA @X�@Xu�@W�]@W��@Wy�@V�s@V��@V8�@Uf�@T�u@T?�@S�w@S/�@R��@R�A@Ru%@R@Q��@Q�z@Q�@P��@PD�@P�@O�@O�6@O�$@O9�@N�F@Np;@N-@M�j@Mu�@M<6@M�@L�@L��@LK^@L?�@Lx@K��@K��@K\)@K9�@J�b@J�\@Ji�@J:*@I��@I�M@I^�@I:�@I�@H��@H�.@H?�@G�&@Gs@G+@G�@G�@F�@F�@E��@E:�@D��@Dq@D[�@D9X@C� @Ca@C�@B�s@B��@BE�@B8�@A��@A�~@A%F@@�E@@��@@��@@|�@?��@?|�@?Mj@?8@>�@>�1@>?@>�@=��@=��@=c@=?}@<��@<��@<ѷ@<��@<�D@<N�@;��@;{J@:��@: �@9ԕ@9m]@9�@8�f@8Ĝ@8bN@8@7��@7b�@6�@6�F@68�@6!�@5�j@5��@5a�@4��@4��@4I�@4�@3��@3�@2Ov@2-@1��@1�@1�@1J�@15�@1-w@1�@0�`@0��@0�j@0u�@/��@/��@/{J@/RT@/1�@/
=@.�R@.�x@.�+@.-@-�N@-�t@-u�@-4@,�v@,��@, �@+ƨ@+�$@+|�@+s@+iD@+F�@*q�@*.�@)�D@)�3@)�7@)G�@(�P@(tT@(@'�}@'��@'�@'l�@'$t@'�@'�@&�B@&V@&!�@&�@& �@%�^@%�@$��@$�@$z�@$u�@$m�@$@#�Q@#خ@#�}@#P�@"�M@"��@"�x@"_@!Dg@!�@ �j@ ��@ �.@ ~(@ q@ e�@ U2@ N�@ "h@ 1@��@�@��@{@�@e,@-w@%@֡@z�@(�@��@��@��@�@��@��@�@	@�@c�@�`@�u@PH@�@��@�@@��@�	@|�@iD@P�@>�@+@��@i�@#:@u@�z@J�@�|@�I@Ft@��@a@4�@o@��@��@q�@$�@�.@��@��@�"@Y�@B�@&�@�P@�?@�Y@>B@�@�A@�g@ƨ@�*@�P@x@n/@a@]�@9�@�@��@R�@8�@3�@!�@�~@Dg@7L@�@�@�P@�E@g8@%�@�@�@�;@خ@��@��@��@��@qv@=@!-@
��@
�M@
�c@
��@
�]@
�@
ȴ@
�<@
��@
�\@
��@
xl@
5?@	�Z@	�>@	��@	�@	�X@	s�@	B�@	�@	q@	+@��@��@��@��@��@�.@g8@M@H@7�@~@�@�A@�@�:@|�@l�@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�"B�B�B��B��B��B�wB��B�qB�WB��B��B��B��B�AB��B�B�aB��B��B	YB	uB	6�B	HfB
tB
�1B
�hB
�B�B=�Bm�B�'B� B��B��B�TB��B��B�B��B�B��B  BoB�B�B�B B�B�B	�BB��B�B��B�IB�B��B�B�B��BѷB��B��B��B�^Bl�B]BL�B�BoB
��B
�B
ʦB
�&B
�
B
��B
u�B
X�B
=�B
0�B
&�B
9B
�B
�B
�B
 OB	�%B	��B	ЗB	��B	�aB	�B	�B	��B	v�B	OBB	9�B	.�B	$tB	kB	FB	B	1B�B�cB�B�bB�WBөBϫB�B�EB��B�HB�}B�wB��B�B��B��B�iBĜB��B�pBרB�'B�B��B��B�EB�tB��B�$B�]B	 B��B�B�.B	[B	�B	$@B	E�B	:�B	?�B	9$B	0�B	-�B	E�B	k�B	}<B	��B	��B	��B	�B	�"B	��B	�`B	��B	��B	��B	�yB	�#B	��B	��B	�B	�CB	�B	�kB	��B	��B	�B	��B	�.B	��B	�B	ΥB	�JB	�B	�lB	�uB	��B	�B	�&B	��B	��B	��B	�BB	ªB	�dB	�B	�B	��B	��B	�0B	��B	�HB	��B	��B	�}B	�GB	�MB	��B	�B	�SB	��B	��B	�=B	ɆB	�B	ǔB	�+B	��B	��B	ʦB	̳B	��B	�B	уB	уB	уB	�hB	ѝB	�B	�TB	�@B	�[B	�uB	��B	�FB	՛B	�MB	�MB	՛B	�gB	՛B	�gB	�aB	ңB	�uB	�[B	��B	өB	ӏB	��B	�B	�TB	� B	��B	��B	�
B	ՁB	ՁB	��B	�aB	�B	��B	�SB	�B	��B	��B	ԯB	ԯB	�B	ҽB	�TB	҉B	�uB	ӏB	��B	ԯB	��B	�{B	�aB	�{B	ּB	��B	�~B	یB	�B	�CB	�dB	�-B	�B	�ZB	�B	��B	�fB	��B	��B	�2B	�B	��B	�&B	��B	�B	�pB	ݘB	��B	��B	�	B	ںB	�WB	�B	�B	޸B	�!B	��B	�jB	��B	�;B	��B	��B	�:B	�TB	�hB	�NB	�B	�B	�bB	��B	�hB	�:B	�B	�ZB	�B	��B	�B	�B	�B	�B	��B	�B	�B	�fB	�8B	��B	�yB	�0B	��B	�KB	�B	�B	�B	��B	�qB	�CB	��B	�OB	�B	�B	�-B	��B	��B	�AB	��B	�GB	��B	��B	�3B	�B	��B	��B	��B	��B	��B	�tB	��B	��B	��B	�tB	�tB	��B	��B	�tB	��B	�9B	�?B	�B	�B	�FB	��B	��B	�B	�RB	�RB	��B	��B	�>B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B
  B
 �B
�B
AB
[B
AB
�B
GB
B
�B
�B
�B
�B
�B
B
�B
zB
1B
�B
�B
�B
�B
zB
EB
�B
zB
�B
B
^B

#B
B
�B
�B
B
�B
�B
�B
�B
"B
PB
�B
dB
~B
�B
6B
VB
�B
\B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
B
�B
HB
NB
:B
oB
�B
@B
�B
�B
�B
[B
�B
�B
�B
�B
sB
+B
B
B
MB
2B
gB
9B
�B
YB
�B
eB
QB
B
�B
WB
�B
�B
B
CB
xB
/B
B
B
�B
dB
�B
�B
;B
pB
�B
pB
�B
 BB
 'B
 �B
 �B
!�B
#B
"�B
"�B
"�B
#:B
#�B
$tB
%B
&�B
&�B
'�B
'�B
(
B
($B
($B
(�B
)yB
)�B
*B
)�B
*B
*B
*�B
*�B
+B
+�B
,WB
,qB
,�B
,�B
,�B
-CB
-�B
.�B
/B
/ B
/ B
/ B
/B
/ B
/ B
/OB
/�B
0�B
0�B
0�B
0�B
1�B
2aB
2|B
3B
33B
2�B
33B
33B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4nB
5B
5�B
6�B
6�B
7�B
88B
8B
8B
8B
8RB
88B
8�B
9rB
9�B
9�B
9�B
:^B
;B
;dB
;JB
;JB
;dB
;B
;dB
;dB
;�B
;�B
;�B
<jB
=B
=VB
=�B
>B
>wB
?}B
?�B
@4B
@�B
@�B
AB
A�B
A�B
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CB
C-B
CaB
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
ESB
EmB
E�B
F%B
F%B
F%B
F?B
GB
G+B
G�B
G+B
F�B
G+B
G_B
G�B
G�B
HKB
H�B
H�B
H�B
IRB
IlB
I�B
J	B
J�B
JXB
J�B
J�B
K�B
LdB
L~B
L�B
L�B
L�B
MPB
M6B
M�B
M�B
M�B
M�B
N"B
N�B
OB
N�B
O(B
O\B
O�B
O�B
O�B
PB
P.B
PHB
PHB
PbB
P�B
P�B
P�B
P�B
QhB
QNB
QNB
QhB
Q�B
Q�B
Q�B
RB
RB
RTB
R�B
R�B
SB
SuB
S�B
SuB
S@B
S�B
S�B
TB
T,B
TaB
T�B
T�B
T�B
UB
U�B
U�B
U�B
U�B
VB
VB
V�B
V�B
V�B
W
B
W?B
W$B
W$B
W$B
W
B
W?B
W$B
WsB
W�B
W�B
W�B
W�B
W�B
X+B
XEB
XyB
X_B
XyB
XyB
XyB
X�B
X�B
Y1B
Y�B
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[#B
[qB
[�B
\B
\xB
\�B
\�B
\�B
\�B
\�B
]B
]~B
]~B
]dB
]dB
]B
]~B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_B
_!B
_;B
_pB
_�B
_�B
_�B
_�B
_�B
`B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
cB
c�B
c�B
c�B
c�B
cnB
c B
cnB
c�B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
e`B
e�B
fB
f2B
f2B
f�B
g�B
h
B
h
B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
j�B
kkB
kQB
k�B
k�B
k�B
k�B
lB
lB
lB
k�B
lB
l"B
l�B
mB
m]B
m�B
ncB
n}B
n�B
n�B
o B
o5B
o�B
o�B
p!B
p;B
p;B
p;B
p;B
poB
p�B
q[B
q�B
q�B
r-B
r�B
r�B
sB
sB
s3B
s3B
s3B
sMB
sMB
shB
shB
s�B
tB
tnB
t�B
t�B
u?B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
y$B
y>B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|B
|B
|B
|B
|�B
|�B
}<B
}<B
}<B
}<B
}<B
}VB
}VB
}<B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~BB
~]B
~�B
~�B
~�B
~�B
.B
B
.B
HB
HB
�B
�B
�B
�B
�B
�4B
�4B
�OB
�iB
��B
��B
��B
�B
�B
�B
�B
� B
�;B
��B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�"B�B�B��B��B��B�wB��B�qB�WB��B��B��B��B�AB��B�B�aB��B��B	YB	uB	6�B	HfB
tB
�1B
�hB
�B�B=�Bm�B�'B� B��B��B�TB��B��B�B��B�B��B  BoB�B�B�B B�B�B	�BB��B�B��B�IB�B��B�B�B��BѷB��B��B��B�^Bl�B]BL�B�BoB
��B
�B
ʦB
�&B
�
B
��B
u�B
X�B
=�B
0�B
&�B
9B
�B
�B
�B
 OB	�%B	��B	ЗB	��B	�aB	�B	�B	��B	v�B	OBB	9�B	.�B	$tB	kB	FB	B	1B�B�cB�B�bB�WBөBϫB�B�EB��B�HB�}B�wB��B�B��B��B�iBĜB��B�pBרB�'B�B��B��B�EB�tB��B�$B�]B	 B��B�B�.B	[B	�B	$@B	E�B	:�B	?�B	9$B	0�B	-�B	E�B	k�B	}<B	��B	��B	��B	�B	�"B	��B	�`B	��B	��B	��B	�yB	�#B	��B	��B	�B	�CB	�B	�kB	��B	��B	�B	��B	�.B	��B	�B	ΥB	�JB	�B	�lB	�uB	��B	�B	�&B	��B	��B	��B	�BB	ªB	�dB	�B	�B	��B	��B	�0B	��B	�HB	��B	��B	�}B	�GB	�MB	��B	�B	�SB	��B	��B	�=B	ɆB	�B	ǔB	�+B	��B	��B	ʦB	̳B	��B	�B	уB	уB	уB	�hB	ѝB	�B	�TB	�@B	�[B	�uB	��B	�FB	՛B	�MB	�MB	՛B	�gB	՛B	�gB	�aB	ңB	�uB	�[B	��B	өB	ӏB	��B	�B	�TB	� B	��B	��B	�
B	ՁB	ՁB	��B	�aB	�B	��B	�SB	�B	��B	��B	ԯB	ԯB	�B	ҽB	�TB	҉B	�uB	ӏB	��B	ԯB	��B	�{B	�aB	�{B	ּB	��B	�~B	یB	�B	�CB	�dB	�-B	�B	�ZB	�B	��B	�fB	��B	��B	�2B	�B	��B	�&B	��B	�B	�pB	ݘB	��B	��B	�	B	ںB	�WB	�B	�B	޸B	�!B	��B	�jB	��B	�;B	��B	��B	�:B	�TB	�hB	�NB	�B	�B	�bB	��B	�hB	�:B	�B	�ZB	�B	��B	�B	�B	�B	�B	��B	�B	�B	�fB	�8B	��B	�yB	�0B	��B	�KB	�B	�B	�B	��B	�qB	�CB	��B	�OB	�B	�B	�-B	��B	��B	�AB	��B	�GB	��B	��B	�3B	�B	��B	��B	��B	��B	��B	�tB	��B	��B	��B	�tB	�tB	��B	��B	�tB	��B	�9B	�?B	�B	�B	�FB	��B	��B	�B	�RB	�RB	��B	��B	�>B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B
  B
 �B
�B
AB
[B
AB
�B
GB
B
�B
�B
�B
�B
�B
B
�B
zB
1B
�B
�B
�B
�B
zB
EB
�B
zB
�B
B
^B

#B
B
�B
�B
B
�B
�B
�B
�B
"B
PB
�B
dB
~B
�B
6B
VB
�B
\B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
B
�B
HB
NB
:B
oB
�B
@B
�B
�B
�B
[B
�B
�B
�B
�B
sB
+B
B
B
MB
2B
gB
9B
�B
YB
�B
eB
QB
B
�B
WB
�B
�B
B
CB
xB
/B
B
B
�B
dB
�B
�B
;B
pB
�B
pB
�B
 BB
 'B
 �B
 �B
!�B
#B
"�B
"�B
"�B
#:B
#�B
$tB
%B
&�B
&�B
'�B
'�B
(
B
($B
($B
(�B
)yB
)�B
*B
)�B
*B
*B
*�B
*�B
+B
+�B
,WB
,qB
,�B
,�B
,�B
-CB
-�B
.�B
/B
/ B
/ B
/ B
/B
/ B
/ B
/OB
/�B
0�B
0�B
0�B
0�B
1�B
2aB
2|B
3B
33B
2�B
33B
33B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4nB
5B
5�B
6�B
6�B
7�B
88B
8B
8B
8B
8RB
88B
8�B
9rB
9�B
9�B
9�B
:^B
;B
;dB
;JB
;JB
;dB
;B
;dB
;dB
;�B
;�B
;�B
<jB
=B
=VB
=�B
>B
>wB
?}B
?�B
@4B
@�B
@�B
AB
A�B
A�B
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CB
C-B
CaB
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
ESB
EmB
E�B
F%B
F%B
F%B
F?B
GB
G+B
G�B
G+B
F�B
G+B
G_B
G�B
G�B
HKB
H�B
H�B
H�B
IRB
IlB
I�B
J	B
J�B
JXB
J�B
J�B
K�B
LdB
L~B
L�B
L�B
L�B
MPB
M6B
M�B
M�B
M�B
M�B
N"B
N�B
OB
N�B
O(B
O\B
O�B
O�B
O�B
PB
P.B
PHB
PHB
PbB
P�B
P�B
P�B
P�B
QhB
QNB
QNB
QhB
Q�B
Q�B
Q�B
RB
RB
RTB
R�B
R�B
SB
SuB
S�B
SuB
S@B
S�B
S�B
TB
T,B
TaB
T�B
T�B
T�B
UB
U�B
U�B
U�B
U�B
VB
VB
V�B
V�B
V�B
W
B
W?B
W$B
W$B
W$B
W
B
W?B
W$B
WsB
W�B
W�B
W�B
W�B
W�B
X+B
XEB
XyB
X_B
XyB
XyB
XyB
X�B
X�B
Y1B
Y�B
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[#B
[qB
[�B
\B
\xB
\�B
\�B
\�B
\�B
\�B
]B
]~B
]~B
]dB
]dB
]B
]~B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_B
_!B
_;B
_pB
_�B
_�B
_�B
_�B
_�B
`B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
cB
c�B
c�B
c�B
c�B
cnB
c B
cnB
c�B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
e`B
e�B
fB
f2B
f2B
f�B
g�B
h
B
h
B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
j�B
kkB
kQB
k�B
k�B
k�B
k�B
lB
lB
lB
k�B
lB
l"B
l�B
mB
m]B
m�B
ncB
n}B
n�B
n�B
o B
o5B
o�B
o�B
p!B
p;B
p;B
p;B
p;B
poB
p�B
q[B
q�B
q�B
r-B
r�B
r�B
sB
sB
s3B
s3B
s3B
sMB
sMB
shB
shB
s�B
tB
tnB
t�B
t�B
u?B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
y$B
y>B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|B
|B
|B
|B
|�B
|�B
}<B
}<B
}<B
}<B
}<B
}VB
}VB
}<B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~BB
~]B
~�B
~�B
~�B
~�B
.B
B
.B
HB
HB
�B
�B
�B
�B
�B
�4B
�4B
�OB
�iB
��B
��B
��B
�B
�B
�B
�B
� B
�;B
��B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104936  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174505  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174505  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174505                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024512  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024512  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                