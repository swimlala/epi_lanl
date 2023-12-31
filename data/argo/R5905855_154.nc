CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-04T03:42:32Z creation;2023-05-04T03:42:33Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230504034232  20230504035817  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�(|��l�1   @�(}���@/���S���c��z�H1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B晚B���B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB�{B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB�z�B�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B�z�B�B��HB��HB��HB��HB��HC�C�C�C�C	�C
>C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C<
>C>
>C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC�C�C�C��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D|�D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�
�A��A���A���A���A��A�ѷA�҉A��,A��2A��2A���A��TA�ѷA�ӏA�՛A��KA��/A��A��NA���A��A��A���A��A��NA��BA��A�ܒA���A���Aο�A�ZA͒:A�FA�8RAɍA�B[A�kA��A�ZQA��WA�	7A��A�b�A�MjA���A�:�A��A��A���A�@�A��jA�[�A��vA���A��OA�R�A�yrA�'A���A��A�V9A��]A���A��A�QNA�6zA��FA�	�A��9A�o5A�5A�	A�VA��OA�u%A���A���A��A���A�P�A���A�?}A�ȴA��PA�|�A�=<A�c�A��sA�/�A�t�A���A��6A�M�A�A��A��A|�mAw>BAth�Ao��Al,�Ai�AiHAh��Af�Aa��A[�;A[*0AX�CAS8�AO}�AM��AK�6AH��AExADm�AD	lAB�A@n/A>��A=R�A<�_A;�A;aA:i�A7��A5��A5G�A4��A4��A1�A0?}A/oA-?�A+�A*qA)`�A'�3A&�A$e�A#��A!��A �A�A��A^5A�@A�A1'Ah�A�Ax�A+A�A#�A�)A��Af�A�A�jA�AzA�A�A�A�TA�HAMjAe,A	A��A��A|�A$tA	��A��A!�A�'A��A�A�@��@��D@�~�@��@��@�c @�� @���@�s�@��@�B�@��h@���@���@��@���@���@�+@���@�<�@���@���@�@�@�Ft@�u�@��}@�@��@�t@�F�@�@�(�@��@�4@�P�@�@��@���@�X�@���@��U@�h�@�ԕ@駇@��
@�O@�`�@�W�@�@�Mj@�J#@�Dg@�{J@�v`@��K@�s�@�w@�S@�Vm@��5@栐@�E�@���@�Q�@�@�/�@�l"@�u�@�d�@�!�@�5?@�_@ߊ�@�,�@��@���@�&@��@��a@ݼ@�\�@�	@��@�-�@�c�@�]d@�[�@�&�@�)�@ټ@ٴ�@٘�@�o @��M@�)�@���@�O@��@�PH@�M@��@Ք�@��@�~�@ӵt@��@Һ�@�~@��@�u�@�,=@��@��d@�]�@���@��`@�l"@�`B@��]@�u�@�)�@�ݘ@ˌ~@�9�@ʢ4@�M@ɭC@�c�@��@ȼj@Ȉ�@���@�-w@��@ƹ�@�-�@��>@��6@ř�@Ĝx@��@ü@���@�q@���@�g8@���@���@�@�{�@��@��$@�m]@�S�@���@��_@�{�@�M@���@��@���@�W�@�1@��w@�7L@��@�L0@���@��@��\@�ff@�@�@�"h@��j@���@�e�@���@��I@�2�@��7@�a@�*0@���@�ȴ@���@�8�@��+@�ƨ@�x�@�;d@��E@��z@�*�@��g@���@��C@��P@��h@�t�@�=@���@�r�@��@���@�IR@��K@��D@�~�@�1�@��^@�s�@�/@��@���@�b@���@���@�s�@�1�@���@�l"@�~@��w@���@�dZ@� i@���@�.�@��o@��@���@�J�@��@���@�-�@�@��g@���@���@�/@���@�ѷ@��1@�@��$@�P�@��@���@�֡@��@�S@��N@���@��r@�1�@��6@�!-@���@�l�@�c @��~@��@��F@�Q�@��@���@�L�@��|@���@���@�{�@�N�@�$@���@��;@���@�C�@��@���@���@��@�E9@�%F@� \@��X@���@���@���@�Q�@��@��.@�a|@�K^@�Ft@�2�@�@��r@��j@��-@��@�T�@�!-@��|@��<@���@���@�s�@�"h@��j@�o @�`B@�N<@�F�@�+@�͟@��@�G�@�͟@��@���@�@���@�t�@��<@��1@�Ĝ@�Q@��@��#@���@�s�@��=@�n/@��@��@�W�@�?@�b@��@��Z@���@��k@�<6@��@��@��}@�K^@��@���@�q@���@���@��X@���@�L�@�8�@��@��$@�~(@�H@��@��7@�0�@�C@��@��X@���@�1�@��@��@��$@�RT@��@�u%@�1�@�{@}�.@|�5@|��@|��@}�@}=�@}�@|��@|�O@|~(@|l"@|h�@{�@{)_@zq�@z$�@y�9@x�P@xy>@w�V@wdZ@wE9@v�c@v��@v�@vl�@u�N@u}�@u?}@tɆ@tu�@s�P@r��@r�R@q�@qԕ@q�@p�@pK^@p(�@p(�@p(�@p%�@o�K@oqv@n�@n{@m��@n:*@n+k@n�@n	@m�)@mm]@l[�@k�$@k�@jȴ@jH�@i�-@i0�@i�h@h�K@g�@g.I@f&�@d��@d��@d<�@c��@b�!@a��@a\�@a(�@`�	@`�u@`"h@^ȴ@]@]&�@\w�@\!@[�@[��@[@O@Zl�@Yϫ@Y-w@X��@W�@V�M@V)�@U�M@UL�@U�@Th�@Sa@R��@Q�Z@QB�@Q�@P�v@P��@P�@O�	@O8@N��@N4@M�S@M@L��@L@Ke�@J�@J�@J{�@J8�@J#:@JJ�@J��@J�@J��@JTa@I�j@I4@H�@H��@H��@H��@H�j@H��@H�@H�@HXy@G�m@G�[@Gb�@G�@F�A@FTa@FL0@FL0@FOv@FOv@FOv@FOv@FJ�@F+k@F_@E�@E-w@E�@E�@E�@E@@EV@D��@Dr�@Cb�@C33@B��@B��@B}V@B&�@A��@Au�@A\�@A#�@@��@@�@@�O@@H@@�@?�A@?��@?�4@>�<@>�@=��@=?}@<�	@<�f@<��@<�@<��@<Xy@<N�@<(�@<@;��@;�6@;��@:�@:Z�@9��@9��@90�@8�P@8�@8�$@7� @7��@7�@7�@7_p@7E9@6��@6s�@6�A@6B[@5�9@5�-@5`B@4��@4�p@4��@4M@3��@3�f@3K�@3$t@2҉@2��@1�^@1�"@1L�@1;@0�?@0��@0tT@0?�@/�0@.��@.�@.�@.�F@.kQ@.@-@-��@-e,@- \@,��@,�Y@,g8@,C-@,6@,x@+�@+O@+>�@+$t@*�y@*��@*GE@*u@)��@)ϫ@)@)�h@) \@(�/@(�@(�.@(tT@(I�@(~@'��@'��@'\)@'33@'�@&�X@%��@%�z@%o @$��@$�@$/�@#��@#�Q@#��@#�V@#��@#n/@#$t@"�H@"�}@"h
@!�o@!��@!��@!u�@!<6@ �5@ �?@ r�@ �@�@��@�0@�P@K�@@�@�H@�B@��@i�@�@��@�3@�h@B�@�@��@�z@��@m�@/�@  @��@��@�@1�@��@��@�+@p;@.�@�@��@e,@8�@�@��@Xy@x@ݘ@�w@�F@�:@x@U�@+@�@��@��@��@s�@1�@ �@��@�@��@��@j@8�@@@��@e�@K^@�@��@�K@��@��@U�@��@�H@��@��@�R@��@�F@��@n�@8�@@
�@ �@��@�#@�@Y�@<6@�@��@��@y>@��@��@��@_p@Y@�@� @z@c @-@
�@�.@�>@��@��@��@x�@T�@J�@7L@�@�@�@�@M@�@�g@�q@��@�	@|�@n/@E9@E9@@O@
�@
��@
n�@
Ta@
&�@	��@	�@	��@	�@	�t@	��@	f�@	F@	(�@	!�@	�@��@�/@��@��@4n@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�
�A��A���A���A���A��A�ѷA�҉A��,A��2A��2A���A��TA�ѷA�ӏA�՛A��KA��/A��A��NA���A��A��A���A��A��NA��BA��A�ܒA���A���Aο�A�ZA͒:A�FA�8RAɍA�B[A�kA��A�ZQA��WA�	7A��A�b�A�MjA���A�:�A��A��A���A�@�A��jA�[�A��vA���A��OA�R�A�yrA�'A���A��A�V9A��]A���A��A�QNA�6zA��FA�	�A��9A�o5A�5A�	A�VA��OA�u%A���A���A��A���A�P�A���A�?}A�ȴA��PA�|�A�=<A�c�A��sA�/�A�t�A���A��6A�M�A�A��A��A|�mAw>BAth�Ao��Al,�Ai�AiHAh��Af�Aa��A[�;A[*0AX�CAS8�AO}�AM��AK�6AH��AExADm�AD	lAB�A@n/A>��A=R�A<�_A;�A;aA:i�A7��A5��A5G�A4��A4��A1�A0?}A/oA-?�A+�A*qA)`�A'�3A&�A$e�A#��A!��A �A�A��A^5A�@A�A1'Ah�A�Ax�A+A�A#�A�)A��Af�A�A�jA�AzA�A�A�A�TA�HAMjAe,A	A��A��A|�A$tA	��A��A!�A�'A��A�A�@��@��D@�~�@��@��@�c @�� @���@�s�@��@�B�@��h@���@���@��@���@���@�+@���@�<�@���@���@�@�@�Ft@�u�@��}@�@��@�t@�F�@�@�(�@��@�4@�P�@�@��@���@�X�@���@��U@�h�@�ԕ@駇@��
@�O@�`�@�W�@�@�Mj@�J#@�Dg@�{J@�v`@��K@�s�@�w@�S@�Vm@��5@栐@�E�@���@�Q�@�@�/�@�l"@�u�@�d�@�!�@�5?@�_@ߊ�@�,�@��@���@�&@��@��a@ݼ@�\�@�	@��@�-�@�c�@�]d@�[�@�&�@�)�@ټ@ٴ�@٘�@�o @��M@�)�@���@�O@��@�PH@�M@��@Ք�@��@�~�@ӵt@��@Һ�@�~@��@�u�@�,=@��@��d@�]�@���@��`@�l"@�`B@��]@�u�@�)�@�ݘ@ˌ~@�9�@ʢ4@�M@ɭC@�c�@��@ȼj@Ȉ�@���@�-w@��@ƹ�@�-�@��>@��6@ř�@Ĝx@��@ü@���@�q@���@�g8@���@���@�@�{�@��@��$@�m]@�S�@���@��_@�{�@�M@���@��@���@�W�@�1@��w@�7L@��@�L0@���@��@��\@�ff@�@�@�"h@��j@���@�e�@���@��I@�2�@��7@�a@�*0@���@�ȴ@���@�8�@��+@�ƨ@�x�@�;d@��E@��z@�*�@��g@���@��C@��P@��h@�t�@�=@���@�r�@��@���@�IR@��K@��D@�~�@�1�@��^@�s�@�/@��@���@�b@���@���@�s�@�1�@���@�l"@�~@��w@���@�dZ@� i@���@�.�@��o@��@���@�J�@��@���@�-�@�@��g@���@���@�/@���@�ѷ@��1@�@��$@�P�@��@���@�֡@��@�S@��N@���@��r@�1�@��6@�!-@���@�l�@�c @��~@��@��F@�Q�@��@���@�L�@��|@���@���@�{�@�N�@�$@���@��;@���@�C�@��@���@���@��@�E9@�%F@� \@��X@���@���@���@�Q�@��@��.@�a|@�K^@�Ft@�2�@�@��r@��j@��-@��@�T�@�!-@��|@��<@���@���@�s�@�"h@��j@�o @�`B@�N<@�F�@�+@�͟@��@�G�@�͟@��@���@�@���@�t�@��<@��1@�Ĝ@�Q@��@��#@���@�s�@��=@�n/@��@��@�W�@�?@�b@��@��Z@���@��k@�<6@��@��@��}@�K^@��@���@�q@���@���@��X@���@�L�@�8�@��@��$@�~(@�H@��@��7@�0�@�C@��@��X@���@�1�@��@��@��$@�RT@��@�u%@�1�@�{@}�.@|�5@|��@|��@}�@}=�@}�@|��@|�O@|~(@|l"@|h�@{�@{)_@zq�@z$�@y�9@x�P@xy>@w�V@wdZ@wE9@v�c@v��@v�@vl�@u�N@u}�@u?}@tɆ@tu�@s�P@r��@r�R@q�@qԕ@q�@p�@pK^@p(�@p(�@p(�@p%�@o�K@oqv@n�@n{@m��@n:*@n+k@n�@n	@m�)@mm]@l[�@k�$@k�@jȴ@jH�@i�-@i0�@i�h@h�K@g�@g.I@f&�@d��@d��@d<�@c��@b�!@a��@a\�@a(�@`�	@`�u@`"h@^ȴ@]@]&�@\w�@\!@[�@[��@[@O@Zl�@Yϫ@Y-w@X��@W�@V�M@V)�@U�M@UL�@U�@Th�@Sa@R��@Q�Z@QB�@Q�@P�v@P��@P�@O�	@O8@N��@N4@M�S@M@L��@L@Ke�@J�@J�@J{�@J8�@J#:@JJ�@J��@J�@J��@JTa@I�j@I4@H�@H��@H��@H��@H�j@H��@H�@H�@HXy@G�m@G�[@Gb�@G�@F�A@FTa@FL0@FL0@FOv@FOv@FOv@FOv@FJ�@F+k@F_@E�@E-w@E�@E�@E�@E@@EV@D��@Dr�@Cb�@C33@B��@B��@B}V@B&�@A��@Au�@A\�@A#�@@��@@�@@�O@@H@@�@?�A@?��@?�4@>�<@>�@=��@=?}@<�	@<�f@<��@<�@<��@<Xy@<N�@<(�@<@;��@;�6@;��@:�@:Z�@9��@9��@90�@8�P@8�@8�$@7� @7��@7�@7�@7_p@7E9@6��@6s�@6�A@6B[@5�9@5�-@5`B@4��@4�p@4��@4M@3��@3�f@3K�@3$t@2҉@2��@1�^@1�"@1L�@1;@0�?@0��@0tT@0?�@/�0@.��@.�@.�@.�F@.kQ@.@-@-��@-e,@- \@,��@,�Y@,g8@,C-@,6@,x@+�@+O@+>�@+$t@*�y@*��@*GE@*u@)��@)ϫ@)@)�h@) \@(�/@(�@(�.@(tT@(I�@(~@'��@'��@'\)@'33@'�@&�X@%��@%�z@%o @$��@$�@$/�@#��@#�Q@#��@#�V@#��@#n/@#$t@"�H@"�}@"h
@!�o@!��@!��@!u�@!<6@ �5@ �?@ r�@ �@�@��@�0@�P@K�@@�@�H@�B@��@i�@�@��@�3@�h@B�@�@��@�z@��@m�@/�@  @��@��@�@1�@��@��@�+@p;@.�@�@��@e,@8�@�@��@Xy@x@ݘ@�w@�F@�:@x@U�@+@�@��@��@��@s�@1�@ �@��@�@��@��@j@8�@@@��@e�@K^@�@��@�K@��@��@U�@��@�H@��@��@�R@��@�F@��@n�@8�@@
�@ �@��@�#@�@Y�@<6@�@��@��@y>@��@��@��@_p@Y@�@� @z@c @-@
�@�.@�>@��@��@��@x�@T�@J�@7L@�@�@�@�@M@�@�g@�q@��@�	@|�@n/@E9@E9@@O@
�@
��@
n�@
Ta@
&�@	��@	�@	��@	�@	�t@	��@	f�@	F@	(�@	!�@	�@��@�/@��@��@4n@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	y�B	y	B	y�B	y�B	z*B	{0B	}"B	��B	��B	��B	��B	�1B	�WB	�OB	��B	�fB	��B	�wB	�AB	��B	�?B	�RB	��B	��B	�B	�]B	�]B	��B	��B	��B	��B	�B	�6B	��B	�/B	��B	�=B	�nB	��B	�fB	ڠB	�wB
7LB
dB
�B
�"B
�2B
�5B
�B	lB�BTB�B&�B-]B(
B%�B#B
�B
��B
��B
��B8�BZ�BVBTBVSBY1BVBU�BY�B]Bc�Br�B�uB�WB�7B��B�OB�SB��B�{BBuBo�Bh>B\)BI�BC{B#TB
�B
�/B
��B
�BB
v+B
`'B
@�B
0;B
$�B
�B	��B	�qB	��B	��B	� B	��B	��B	��B	xB	^�B	YB	P.B	;�B	+�B	# B	CB	�B	tB	B	 �B�<B�FB��B�)B�B�B�`B� B�B��BڠB�KB��B�+B�2B��B��B�MB��B�gB��BרB�_B֡B�BߊB�B�B�B�?B�8B	�B	B	 OB	�B	B	 �B	!�B	!bB	"�B	/�B	3B	0UB	+�B	.}B	9>B	?�B	@iB	@OB	?�B	AUB	@4B	?HB	8lB	1vB	0�B	6B	.�B	�B	B	 �B	�B		�B��B��B�9B��B	�B�cB	�B	�B��B	xB	B	)�B	,qB	.cB	.�B	+6B	=�B	B�B	GzB	J#B	J�B	K�B	KDB	KxB	L0B	L�B	L�B	J	B	F?B	CaB	FB	I�B	KxB	LdB	M�B	N�B	O�B	TB	UgB	WsB	X�B	YB	ZB	Z�B	[�B	_�B	bNB	f�B	i�B	n�B	shB	{�B	�B	�{B	�?B	��B	�RB	��B	��B	�&B	�[B	�MB	��B	��B	�QB	�B	�
B	��B	�rB	��B	��B	��B	��B	��B	�B	��B	��B	�vB	�GB	�!B	�zB	��B	��B	�+B	��B	� B	��B	��B	��B	��B	�B	�_B	�lB	��B	��B	�B	��B	�YB	��B	�B	�jB	͟B	�<B	ΥB	οB	�vB	��B	� B	ЗB	� B	� B	ҽB	ңB	��B	�B	ՁB	ؓB	�EB	�B	ٚB	�1B	�B	ٚB	چB	ۦB	ݲB	�B	�!B	�BB	�B	��B	��B	��B	�B	�hB	�B	��B	��B	��B	�B	�8B	�B	�>B	��B	�B	��B	�B	�B	�WB	��B	�/B	��B	�B	�B	�B	�OB	�B	�B	��B	�UB	�'B	��B	��B	�B	��B	�MB	�B	�B	��B	�tB	�zB	��B	�B	�LB	�8B	��B	��B	�^B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�BB	�]B	��B	�}B	�}B
 �B
�B
oB
�B
�B
�B
B
�B
{B
B
�B
�B
gB
B
SB
B
�B
�B
�B
	7B
	B
	�B
	�B
�B
DB
�B
�B
�B
PB
�B
�B
jB
jB
�B
<B
�B
�B
�B
�B
�B
B
B
�B
\B
HB
}B
4B
oB
B
uB
[B
B
�B
�B
gB
�B
B
�B
 \B
�B
�B
�B
kB
�B
B
�B
�B
B
dB
]B
CB
xB
�B
B
�B
�B
B
5B
jB
OB
�B
B
�B
�B
�B
 'B
!�B
"�B
'�B
*�B
+6B
,B
.�B
.�B
.�B
/�B
0B
0�B
2|B
2�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
6`B
6B
6B
6+B
6+B
5�B
5�B
4�B
3�B
2�B
2�B
1�B
0�B
0oB
/5B
.}B
1vB
0�B
1AB
1�B
1AB
2GB
5?B
5�B
5ZB
5�B
7B
88B
8�B
9rB
9�B
:^B
<jB
=�B
?HB
?�B
?.B
?cB
?HB
?cB
>�B
>BB
>BB
>B
>BB
?B
@B
@4B
@�B
A;B
BB
B�B
B�B
B�B
C�B
C�B
D�B
DB
DMB
EB
GB
GB
GEB
FtB
F�B
G�B
G�B
FB
DB
DB
D�B
F�B
IB
J=B
J�B
JrB
JrB
JXB
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M6B
MB
M6B
MB
MB
L�B
M�B
M�B
MPB
L�B
L�B
L0B
K�B
MB
MB
MB
OBB
N�B
NVB
N<B
N<B
N<B
N�B
O�B
O�B
O\B
NVB
MPB
O�B
QNB
Q�B
R B
R:B
R:B
R�B
RB
QhB
QNB
QhB
Q4B
QB
RTB
RoB
Q B
P�B
P�B
PHB
PB
O�B
OvB
OBB
O(B
OB
OBB
O(B
N�B
NVB
NpB
N�B
O�B
QB
R�B
S�B
S�B
T�B
S�B
RoB
RB
RTB
R�B
R�B
S[B
TFB
T{B
T�B
U2B
U�B
V9B
VB
VmB
VmB
VmB
VmB
XEB
YeB
YB
Y�B
ZB
ZB
ZQB
Z�B
Z�B
[=B
[qB
[=B
[�B
\�B
]~B
^�B
`vB
a�B
a|B
abB
a�B
a�B
a�B
bhB
cTB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
ezB
gRB
gRB
g�B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i_B
i_B
i_B
iDB
iDB
h�B
h�B
g�B
g8B
g8B
gRB
g�B
g�B
g�B
h
B
h$B
g�B
hsB
i*B
iB
h�B
h�B
iB
i_B
i*B
iyB
jB
j0B
j�B
j�B
j�B
j�B
j�B
j�B
k6B
kB
k6B
k6B
k6B
j�B
j�B
kB
j�B
kkB
lB
lB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
mB
m�B
m�B
n�B
o�B
pUB
pUB
p�B
p�B
p�B
q'B
q�B
q�B
q�B
rB
rGB
raB
raB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t�B
u�B
u�B
u�B
u�B
vB
v�B
v�B
wB
wLB
w�B
w�B
xB
xB
xB
xB
x8B
x�B
x�B
xlB
xlB
x�B
x�B
x�B
y	B
y$B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
zB
y�B
zDB
z^B
z�B
z�B
z�B
z�B
{dB
{dB
{�B
|B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}VB
}�B
}�B
}�B
~(B
~�B
~�B
~�B
B
HB
�B
�B
�B
�iB
�OB
�iB
��B
��B
� B
�UB
�UB
��B
��B
��B
��B
�'B
�AB
�'B
�'B
�uB
��B
��B
�-B
�-B
�aB
��B
��B
�B
�B
�B
�gB
��B
�B
�B
�B
�B
�9B
��B
��B
��B
��B
�tB
��B
��B
��B
��B
��B
��B
�tB
��B
��B
��B
��B
��B
�B
��B
�EB
�_B
�zB
��B
��B
��B
��B
��B
�B
��B
�RB
�lB
�	B
�	B
�#B
�XB
�=B
�rB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
�^B
�^B
�xB
��B
��B
��B
��B
�dB
�dB
�dB
�dB
�B
��B
�B
�jB
�jB
��B
��B
�B
�B
�<B
�pB
�pB
��B
�pB
��B
��B
��B
�B
��B
�B
�B
��B
�BB
�vB
��B
��B
�B
�HB
�bB
��B
��B
��B
�4B
�B
�B
�B
�TB
�TB
��B
��B
��B
�B
�&B
�@B
�[B
��B
�,B
�aB
�{B
�{B
��B
��B
��B
��B
��B
�MB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	y�B	y	B	y�B	y�B	z*B	{0B	}"B	��B	��B	��B	��B	�1B	�WB	�OB	��B	�fB	��B	�wB	�AB	��B	�?B	�RB	��B	��B	�B	�]B	�]B	��B	��B	��B	��B	�B	�6B	��B	�/B	��B	�=B	�nB	��B	�fB	ڠB	�wB
7LB
dB
�B
�"B
�2B
�5B
�B	lB�BTB�B&�B-]B(
B%�B#B
�B
��B
��B
��B8�BZ�BVBTBVSBY1BVBU�BY�B]Bc�Br�B�uB�WB�7B��B�OB�SB��B�{BBuBo�Bh>B\)BI�BC{B#TB
�B
�/B
��B
�BB
v+B
`'B
@�B
0;B
$�B
�B	��B	�qB	��B	��B	� B	��B	��B	��B	xB	^�B	YB	P.B	;�B	+�B	# B	CB	�B	tB	B	 �B�<B�FB��B�)B�B�B�`B� B�B��BڠB�KB��B�+B�2B��B��B�MB��B�gB��BרB�_B֡B�BߊB�B�B�B�?B�8B	�B	B	 OB	�B	B	 �B	!�B	!bB	"�B	/�B	3B	0UB	+�B	.}B	9>B	?�B	@iB	@OB	?�B	AUB	@4B	?HB	8lB	1vB	0�B	6B	.�B	�B	B	 �B	�B		�B��B��B�9B��B	�B�cB	�B	�B��B	xB	B	)�B	,qB	.cB	.�B	+6B	=�B	B�B	GzB	J#B	J�B	K�B	KDB	KxB	L0B	L�B	L�B	J	B	F?B	CaB	FB	I�B	KxB	LdB	M�B	N�B	O�B	TB	UgB	WsB	X�B	YB	ZB	Z�B	[�B	_�B	bNB	f�B	i�B	n�B	shB	{�B	�B	�{B	�?B	��B	�RB	��B	��B	�&B	�[B	�MB	��B	��B	�QB	�B	�
B	��B	�rB	��B	��B	��B	��B	��B	�B	��B	��B	�vB	�GB	�!B	�zB	��B	��B	�+B	��B	� B	��B	��B	��B	��B	�B	�_B	�lB	��B	��B	�B	��B	�YB	��B	�B	�jB	͟B	�<B	ΥB	οB	�vB	��B	� B	ЗB	� B	� B	ҽB	ңB	��B	�B	ՁB	ؓB	�EB	�B	ٚB	�1B	�B	ٚB	چB	ۦB	ݲB	�B	�!B	�BB	�B	��B	��B	��B	�B	�hB	�B	��B	��B	��B	�B	�8B	�B	�>B	��B	�B	��B	�B	�B	�WB	��B	�/B	��B	�B	�B	�B	�OB	�B	�B	��B	�UB	�'B	��B	��B	�B	��B	�MB	�B	�B	��B	�tB	�zB	��B	�B	�LB	�8B	��B	��B	�^B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�BB	�]B	��B	�}B	�}B
 �B
�B
oB
�B
�B
�B
B
�B
{B
B
�B
�B
gB
B
SB
B
�B
�B
�B
	7B
	B
	�B
	�B
�B
DB
�B
�B
�B
PB
�B
�B
jB
jB
�B
<B
�B
�B
�B
�B
�B
B
B
�B
\B
HB
}B
4B
oB
B
uB
[B
B
�B
�B
gB
�B
B
�B
 \B
�B
�B
�B
kB
�B
B
�B
�B
B
dB
]B
CB
xB
�B
B
�B
�B
B
5B
jB
OB
�B
B
�B
�B
�B
 'B
!�B
"�B
'�B
*�B
+6B
,B
.�B
.�B
.�B
/�B
0B
0�B
2|B
2�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
6`B
6B
6B
6+B
6+B
5�B
5�B
4�B
3�B
2�B
2�B
1�B
0�B
0oB
/5B
.}B
1vB
0�B
1AB
1�B
1AB
2GB
5?B
5�B
5ZB
5�B
7B
88B
8�B
9rB
9�B
:^B
<jB
=�B
?HB
?�B
?.B
?cB
?HB
?cB
>�B
>BB
>BB
>B
>BB
?B
@B
@4B
@�B
A;B
BB
B�B
B�B
B�B
C�B
C�B
D�B
DB
DMB
EB
GB
GB
GEB
FtB
F�B
G�B
G�B
FB
DB
DB
D�B
F�B
IB
J=B
J�B
JrB
JrB
JXB
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M6B
MB
M6B
MB
MB
L�B
M�B
M�B
MPB
L�B
L�B
L0B
K�B
MB
MB
MB
OBB
N�B
NVB
N<B
N<B
N<B
N�B
O�B
O�B
O\B
NVB
MPB
O�B
QNB
Q�B
R B
R:B
R:B
R�B
RB
QhB
QNB
QhB
Q4B
QB
RTB
RoB
Q B
P�B
P�B
PHB
PB
O�B
OvB
OBB
O(B
OB
OBB
O(B
N�B
NVB
NpB
N�B
O�B
QB
R�B
S�B
S�B
T�B
S�B
RoB
RB
RTB
R�B
R�B
S[B
TFB
T{B
T�B
U2B
U�B
V9B
VB
VmB
VmB
VmB
VmB
XEB
YeB
YB
Y�B
ZB
ZB
ZQB
Z�B
Z�B
[=B
[qB
[=B
[�B
\�B
]~B
^�B
`vB
a�B
a|B
abB
a�B
a�B
a�B
bhB
cTB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
ezB
gRB
gRB
g�B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i_B
i_B
i_B
iDB
iDB
h�B
h�B
g�B
g8B
g8B
gRB
g�B
g�B
g�B
h
B
h$B
g�B
hsB
i*B
iB
h�B
h�B
iB
i_B
i*B
iyB
jB
j0B
j�B
j�B
j�B
j�B
j�B
j�B
k6B
kB
k6B
k6B
k6B
j�B
j�B
kB
j�B
kkB
lB
lB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
mB
m�B
m�B
n�B
o�B
pUB
pUB
p�B
p�B
p�B
q'B
q�B
q�B
q�B
rB
rGB
raB
raB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
t�B
u�B
u�B
u�B
u�B
vB
v�B
v�B
wB
wLB
w�B
w�B
xB
xB
xB
xB
x8B
x�B
x�B
xlB
xlB
x�B
x�B
x�B
y	B
y$B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
zB
y�B
zDB
z^B
z�B
z�B
z�B
z�B
{dB
{dB
{�B
|B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}VB
}�B
}�B
}�B
~(B
~�B
~�B
~�B
B
HB
�B
�B
�B
�iB
�OB
�iB
��B
��B
� B
�UB
�UB
��B
��B
��B
��B
�'B
�AB
�'B
�'B
�uB
��B
��B
�-B
�-B
�aB
��B
��B
�B
�B
�B
�gB
��B
�B
�B
�B
�B
�9B
��B
��B
��B
��B
�tB
��B
��B
��B
��B
��B
��B
�tB
��B
��B
��B
��B
��B
�B
��B
�EB
�_B
�zB
��B
��B
��B
��B
��B
�B
��B
�RB
�lB
�	B
�	B
�#B
�XB
�=B
�rB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
�^B
�^B
�xB
��B
��B
��B
��B
�dB
�dB
�dB
�dB
�B
��B
�B
�jB
�jB
��B
��B
�B
�B
�<B
�pB
�pB
��B
�pB
��B
��B
��B
�B
��B
�B
�B
��B
�BB
�vB
��B
��B
�B
�HB
�bB
��B
��B
��B
�4B
�B
�B
�B
�TB
�TB
��B
��B
��B
�B
�&B
�@B
�[B
��B
�,B
�aB
�{B
�{B
��B
��B
��B
��B
��B
�MB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230504034216  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230504034232  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230504034233  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230504034233                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230504034234  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230504034234  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230504035817                      G�O�G�O�G�O�                