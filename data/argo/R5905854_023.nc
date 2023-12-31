CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:48:33Z creation;2022-06-04T17:48:33Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174833  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��@�m:1   @��A ���@0C��$��ca�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B���B���B�  B�  B���B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���C   C� C� C  C  C	�fC�fC  C  C  C  C�C  C�C33C�C   C!�fC#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\)@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB�{B�z�B��B��HB��HB��B��HB��HB��HB��HB�z�B��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�G�B��B��B��HCp�Cp�C�C�C	�
C�
C�C�C�C�C
>C�C
>C#�C
>C�C!�
C#�
C%�
C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG��DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dw�Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�AGD�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~Dֺ�D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~Dߺ�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߸Aߦ�Aߨ�Aߘ�A߆YA߀�A�t�A�uZA�G�A�*�A�#�A��A�A��A��A�YA�oA��A�VA��A�PA��A�~A�
�A�
�A�	lA��A��A�;A���A��.A��PA��Aޣ�A�iA���A�k�A��fAܧRA��A�jA�2-Aѿ�AЩ�A�یA�6A�8�A�cTA���A��GA�>�A�=�A��A�$�A�AA��A���A��{A���A�y>A��A�6�A�SA� �A�J�A�F?A��A���A��0A��A�x�A��UA�U�A��A��A�W�A�\]A}OA|��Az��As�Ap�Aj:�Ae��Ac��A_�zA]z�AZ�AS��AQ��APϫAO��AN��AMl"AKȴAK�AIL�AF%�AD|AAm]A>�A<"�A;ԕA:xA8�	A7ݘA6��A4�BA3��A32aA3�A2#�A1Z�A0�mA0RTA/[�A-��A+�A)�;A)1'A'$tA%��A%��A$�&A$W�A#w2A"�KA!�"A!_A �KA 	A 6A 9XA�A��A��A 4nA �CA ��A O�A��AxAM�A	A�CAo A:�A�A��A��A7A�ARTA_A�4A"hA��A�A��AԕA{�A'�A��A]dA+A��A�A}�A8A�gA�A�gA��Ae,A�vA�4AW?A�mAC-A�DA�NA�'A��An/A?}AsA{�A�@A
�6A
A�A	H�Aj�A��A��Au%A@OA��A�A�)A)�A�A�^Ac A��A?A��A�A:�A�A�8A��A��A	A ��A bN@��-@��[@�@�@�-@�7@���@���@�"h@���@�(�@��d@��@�Q�@��6@��@���@�g�@���@�8@��O@�@�L0@���@�s@��@�g8@��@�G�@�Ov@�kQ@�?�@�@���@�@�j@�iD@�<�@�}�@��@��@��@�w�@�e�@�,=@瞄@�j�@�6@���@�0�@��B@�-�@�ƨ@�:�@�YK@�	@�ƨ@�e,@�<@�|�@�q�@�s�@�n�@��+@޽<@ݎ�@��8@��p@���@���@�tT@�PH@��@�q@ٯ�@�ԕ@٩�@�.I@�u@���@״�@�{J@��/@�o @��@�G@�v`@�e�@�m]@��@�_@�-@�_@ч�@�W?@��K@�c @��@�ԕ@Ϗ�@�C�@ΝI@�1@͓@�33@�͟@�L0@�6�@��@�a@���@��@��s@ʞ�@�_@��;@Ɉf@�X�@�+@��@Ȩ�@ȥz@Ȑ.@�c�@�.�@�u@Ǧ�@�
=@�Ĝ@Ƙ_@�@œ�@��@�`�@�!@��&@�J�@³h@��r@���@��@�ں@���@�GE@���@��f@�+�@�Y@��I@��D@�y>@��d@���@�Q@���@�B�@�;@�� @�C�@��@�ȴ@�U2@�S�@��@��@�.I@�Ɇ@�8�@��@�Mj@��@�PH@���@��V@�(@��@���@�H@�/�@�~@���@�4@�%@��@���@� �@���@���@��@�n/@�'�@�҉@���@�d�@�H@��@�;@�q@�^5@�9X@��.@���@��g@�L�@���@�ff@�H�@��@��S@�ں@���@��@�xl@�K^@�4@��K@�c@�T�@�4�@���@��@�|�@���@��@��@�	@���@���@�X@��B@��}@�GE@��Z@���@�-w@�Z�@���@�e�@�O�@�4�@�	l@��?@��F@�L0@��t@��@�c�@�@@���@���@��.@���@�r�@�`�@��j@�J�@���@�[�@�%�@��@���@��:@�N<@��@�ѷ@�xl@�H@�'R@��=@�ȴ@�a|@�O@���@��{@�>�@���@���@���@�h
@�1'@�!@� �@��{@�1�@���@��2@��O@�c�@�9X@��K@�u�@�-w@�Ɇ@���@�m�@�=q@�9X@���@��H@�i�@�K^@�0U@��]@��Q@�@��@@��P@�y�@�m]@�]�@�N<@�4@�!�@��@��K@���@���@�S�@��@���@���@��\@�v�@�M@�	@���@��@���@��=@�U�@�)_@���@�֡@��B@���@���@���@�R�@���@�l�@�;d@�+@�S@��?@�l"@���@�~�@��@���@���@��8@�oi@���@���@��@�e,@�=�@���@���@�-@���@��k@�|@�]�@�	l@��X@��4@�}V@�I�@�4@�a@�	@A�@~�R@~�A@~��@~?@}ԕ@}4@|j@|@{��@{�0@{��@z��@z��@z{@y��@yX@y*0@x�/@x�j@x��@xh�@x4n@x1@w�w@wW?@vxl@u�-@u�S@uL�@t�/@t_@s��@s�	@r҉@ru%@r=q@q�.@q�t@q�@p�@p:�@o�F@on/@oE9@oC@n��@m��@mhs@m�@l��@l�U@l�O@l��@k�V@j�@j� @jp;@jV@j_@ik�@i%F@h��@h�@h�@g��@gA�@f�6@fff@fM�@f8�@f@eo @d�|@d`�@c��@b6�@a�>@a��@a \@`�@`�p@`��@`V�@`�@_�F@_s@^��@]�d@]a�@] \@\H@[��@[�$@[1�@Z��@Z-@Y^�@X�U@X4n@W�K@Wv`@V�@U�T@U;@T�e@T�@S�@SA�@R�M@R�2@R�!@R��@R+k@Q�t@Q�n@Q�=@Qw2@Q5�@P�5@O��@O�@O�@N�@N�'@Nc @N�@M��@MVm@L�	@L��@L��@L$@Kt�@J��@J�1@Jp;@JC�@J�@I�H@H��@H�@H��@He�@H1'@H�@G��@G�	@G,�@F�@F��@F��@F\�@F;�@F&�@F	@E�^@ET�@E�@D�@Doi@Dm�@Doi@D`�@D2�@D�@C�Q@C��@CO@C$t@B�@BE�@B4@A�>@A�@A��@Ap�@A5�@A	l@@��@@[�@@Xy@@Q�@@6@@%�@@	�@?��@?
=@>C�@=��@=�-@=��@=rG@=[W@<��@<|�@;��@;� @;�F@;$t@:҉@:��@:��@:�@:\�@:&�@9�>@9c�@8��@8�@8V�@7�A@7��@7RT@7@6�@6c @65?@6O@6e@6{@5��@5��@5��@5��@5��@58�@4�K@4�4@4e�@47�@4 �@4�@3��@3�}@3�@3qv@3W?@39�@2҉@2��@2+k@1ϫ@1��@1+�@0��@0��@0�@0�e@0��@0��@0��@0�D@0oi@0'R@/��@/�
@/��@.�@.��@.s�@.C�@-w2@-@,��@,�I@,~(@,~@+�@+t�@+1�@+@*�@*��@*h
@)��@)@)p�@)Vm@)Q�@):�@) \@)%@(�)@(��@(tT@(!@'�F@'�f@'t�@'s@'iD@&�"@&��@&l�@&GE@&0U@%��@%�@%�@$��@$�@$]d@#�r@#�@@#�{@#l�@#_p@#8@#)_@#@"�b@"��@"��@"i�@"Z�@"@�@"�@"&�@"($@"&�@!�o@!�'@!*0@ Ĝ@ oi@ Q�@ ?�@ @�0@RT@�@��@��@E�@&�@_@�@��@�'@X@�@�)@��@M@�@�@�]@�@�@�A@�@J#@o@ں@��@��@E�@_@�=@:�@�@V@�@�@V@�@�@��@!@�@�
@�K@��@�:@RT@8@)_@
=@@�8@��@�@�B@��@��@d�@��@�3@�C@�7@|@p�@7L@�f@�@��@��@e�@%�@�@��@|�@O@@O@6z@+@�@�}@W�@u@��@ϫ@�C@F@�@�@ѷ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߸Aߦ�Aߨ�Aߘ�A߆YA߀�A�t�A�uZA�G�A�*�A�#�A��A�A��A��A�YA�oA��A�VA��A�PA��A�~A�
�A�
�A�	lA��A��A�;A���A��.A��PA��Aޣ�A�iA���A�k�A��fAܧRA��A�jA�2-Aѿ�AЩ�A�یA�6A�8�A�cTA���A��GA�>�A�=�A��A�$�A�AA��A���A��{A���A�y>A��A�6�A�SA� �A�J�A�F?A��A���A��0A��A�x�A��UA�U�A��A��A�W�A�\]A}OA|��Az��As�Ap�Aj:�Ae��Ac��A_�zA]z�AZ�AS��AQ��APϫAO��AN��AMl"AKȴAK�AIL�AF%�AD|AAm]A>�A<"�A;ԕA:xA8�	A7ݘA6��A4�BA3��A32aA3�A2#�A1Z�A0�mA0RTA/[�A-��A+�A)�;A)1'A'$tA%��A%��A$�&A$W�A#w2A"�KA!�"A!_A �KA 	A 6A 9XA�A��A��A 4nA �CA ��A O�A��AxAM�A	A�CAo A:�A�A��A��A7A�ARTA_A�4A"hA��A�A��AԕA{�A'�A��A]dA+A��A�A}�A8A�gA�A�gA��Ae,A�vA�4AW?A�mAC-A�DA�NA�'A��An/A?}AsA{�A�@A
�6A
A�A	H�Aj�A��A��Au%A@OA��A�A�)A)�A�A�^Ac A��A?A��A�A:�A�A�8A��A��A	A ��A bN@��-@��[@�@�@�-@�7@���@���@�"h@���@�(�@��d@��@�Q�@��6@��@���@�g�@���@�8@��O@�@�L0@���@�s@��@�g8@��@�G�@�Ov@�kQ@�?�@�@���@�@�j@�iD@�<�@�}�@��@��@��@�w�@�e�@�,=@瞄@�j�@�6@���@�0�@��B@�-�@�ƨ@�:�@�YK@�	@�ƨ@�e,@�<@�|�@�q�@�s�@�n�@��+@޽<@ݎ�@��8@��p@���@���@�tT@�PH@��@�q@ٯ�@�ԕ@٩�@�.I@�u@���@״�@�{J@��/@�o @��@�G@�v`@�e�@�m]@��@�_@�-@�_@ч�@�W?@��K@�c @��@�ԕ@Ϗ�@�C�@ΝI@�1@͓@�33@�͟@�L0@�6�@��@�a@���@��@��s@ʞ�@�_@��;@Ɉf@�X�@�+@��@Ȩ�@ȥz@Ȑ.@�c�@�.�@�u@Ǧ�@�
=@�Ĝ@Ƙ_@�@œ�@��@�`�@�!@��&@�J�@³h@��r@���@��@�ں@���@�GE@���@��f@�+�@�Y@��I@��D@�y>@��d@���@�Q@���@�B�@�;@�� @�C�@��@�ȴ@�U2@�S�@��@��@�.I@�Ɇ@�8�@��@�Mj@��@�PH@���@��V@�(@��@���@�H@�/�@�~@���@�4@�%@��@���@� �@���@���@��@�n/@�'�@�҉@���@�d�@�H@��@�;@�q@�^5@�9X@��.@���@��g@�L�@���@�ff@�H�@��@��S@�ں@���@��@�xl@�K^@�4@��K@�c@�T�@�4�@���@��@�|�@���@��@��@�	@���@���@�X@��B@��}@�GE@��Z@���@�-w@�Z�@���@�e�@�O�@�4�@�	l@��?@��F@�L0@��t@��@�c�@�@@���@���@��.@���@�r�@�`�@��j@�J�@���@�[�@�%�@��@���@��:@�N<@��@�ѷ@�xl@�H@�'R@��=@�ȴ@�a|@�O@���@��{@�>�@���@���@���@�h
@�1'@�!@� �@��{@�1�@���@��2@��O@�c�@�9X@��K@�u�@�-w@�Ɇ@���@�m�@�=q@�9X@���@��H@�i�@�K^@�0U@��]@��Q@�@��@@��P@�y�@�m]@�]�@�N<@�4@�!�@��@��K@���@���@�S�@��@���@���@��\@�v�@�M@�	@���@��@���@��=@�U�@�)_@���@�֡@��B@���@���@���@�R�@���@�l�@�;d@�+@�S@��?@�l"@���@�~�@��@���@���@��8@�oi@���@���@��@�e,@�=�@���@���@�-@���@��k@�|@�]�@�	l@��X@��4@�}V@�I�@�4@�a@�	@A�@~�R@~�A@~��@~?@}ԕ@}4@|j@|@{��@{�0@{��@z��@z��@z{@y��@yX@y*0@x�/@x�j@x��@xh�@x4n@x1@w�w@wW?@vxl@u�-@u�S@uL�@t�/@t_@s��@s�	@r҉@ru%@r=q@q�.@q�t@q�@p�@p:�@o�F@on/@oE9@oC@n��@m��@mhs@m�@l��@l�U@l�O@l��@k�V@j�@j� @jp;@jV@j_@ik�@i%F@h��@h�@h�@g��@gA�@f�6@fff@fM�@f8�@f@eo @d�|@d`�@c��@b6�@a�>@a��@a \@`�@`�p@`��@`V�@`�@_�F@_s@^��@]�d@]a�@] \@\H@[��@[�$@[1�@Z��@Z-@Y^�@X�U@X4n@W�K@Wv`@V�@U�T@U;@T�e@T�@S�@SA�@R�M@R�2@R�!@R��@R+k@Q�t@Q�n@Q�=@Qw2@Q5�@P�5@O��@O�@O�@N�@N�'@Nc @N�@M��@MVm@L�	@L��@L��@L$@Kt�@J��@J�1@Jp;@JC�@J�@I�H@H��@H�@H��@He�@H1'@H�@G��@G�	@G,�@F�@F��@F��@F\�@F;�@F&�@F	@E�^@ET�@E�@D�@Doi@Dm�@Doi@D`�@D2�@D�@C�Q@C��@CO@C$t@B�@BE�@B4@A�>@A�@A��@Ap�@A5�@A	l@@��@@[�@@Xy@@Q�@@6@@%�@@	�@?��@?
=@>C�@=��@=�-@=��@=rG@=[W@<��@<|�@;��@;� @;�F@;$t@:҉@:��@:��@:�@:\�@:&�@9�>@9c�@8��@8�@8V�@7�A@7��@7RT@7@6�@6c @65?@6O@6e@6{@5��@5��@5��@5��@5��@58�@4�K@4�4@4e�@47�@4 �@4�@3��@3�}@3�@3qv@3W?@39�@2҉@2��@2+k@1ϫ@1��@1+�@0��@0��@0�@0�e@0��@0��@0��@0�D@0oi@0'R@/��@/�
@/��@.�@.��@.s�@.C�@-w2@-@,��@,�I@,~(@,~@+�@+t�@+1�@+@*�@*��@*h
@)��@)@)p�@)Vm@)Q�@):�@) \@)%@(�)@(��@(tT@(!@'�F@'�f@'t�@'s@'iD@&�"@&��@&l�@&GE@&0U@%��@%�@%�@$��@$�@$]d@#�r@#�@@#�{@#l�@#_p@#8@#)_@#@"�b@"��@"��@"i�@"Z�@"@�@"�@"&�@"($@"&�@!�o@!�'@!*0@ Ĝ@ oi@ Q�@ ?�@ @�0@RT@�@��@��@E�@&�@_@�@��@�'@X@�@�)@��@M@�@�@�]@�@�@�A@�@J#@o@ں@��@��@E�@_@�=@:�@�@V@�@�@V@�@�@��@!@�@�
@�K@��@�:@RT@8@)_@
=@@�8@��@�@�B@��@��@d�@��@�3@�C@�7@|@p�@7L@�f@�@��@��@e�@%�@�@��@|�@O@@O@6z@+@�@�}@W�@u@��@ϫ@�C@F@�@�@ѷ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	~�B	~�B	cB	}�B	~�B	~]B	}�B	}�B	}qB	}<B	}B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|jB	|jB	|jB	|6B	|6B	|B	z�B	w�B	sB	lqB	oB	q[B	q�B	pB	m�B	poB	i�B	h�B	iyB	k�B	p!B	��B	�B	�B	��B	��B
�B
4�B
Q�B
��B
�B
�B
�tB
�B
��B
y�B	��B
GB	��B	��B	ǔB	�B	�_B	��B	�.B	�tB	�kB	�|B	�?B	� B	��B	�=B	�QB	�WB	�=B	s�B	P�B	=�B	5ZB	&�B	kB	�B�6B�B�B��B�B�zB�-B�]B�9BȚB�.B�+B��B�_B�
B�FB��B�;B�VB�!B��B�BB�vB��B��B��B�2B�DB�B�[BƨB�zB�B�dB� B��B�pB��B�hB�B�vB��B	4B	/ B	C�B	K�B	XB	`'B	s�B	��B	��B	��B	��B	��B	�\B	�pB	�B	��B	�B	�\B	��B	�&B	�:B	�8B	��B	��B	��B	��B	�0B	�eB	�B	��B	�3B	��B	��B	�B	�XB	�$B	�XB	�$B	��B	��B	��B	�jB	�B	�]B	��B	��B	�iB	�[B	�{B	��B	�3B	�MB	�MB	�3B	�YB	żB	�B	ǮB	��B	��B	�#B	�B	�JB	�B	�B	͹B	οB	οB	οB	�NB	��B	�4B	ѷB	ԯB	ؓB	��B	��B	��B	�4B	�B	�B	�:B	��B	�B	�2B	��B	�&B	�nB	�B	�hB	�B	�B	�@B	� B	�:B	�TB	�nB	��B	��B	�,B	��B	��B	�B	�`B	��B	��B	��B	�B	��B	�B	�B	�&B	�B	�B	�8B	�
B	�FB	�tB	�FB	�B	��B	��B	�B	�B	�`B	�`B	�B	�B	��B	�B	�0B	��B	�yB	�sB	�KB	�6B	�qB	�B	�B	�cB	�B	�B	�vB	�AB	�B	��B	�aB	�vB	�B	�cB	�IB	��B	��B	�vB	�[B	�vB	��B	�hB	�B	�hB	��B	��B	��B	�B	��B	�-B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�0B	�B	�B	��B	�IB	�B	��B	�B	��B	�'B	��B	�B	�AB	��B	��B	��B	�zB	��B	��B	��B	�B	�lB	��B	��B	��B	�8B	��B	�B	��B	�B	�"B	�qB	��B	��B	��B	�dB	��B	�rB	�B	�xB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�8B	��B	�XB	��B	�*B	��B	�8B	�>B	��B	��B	��B	��B	�LB	�B	�lB	�RB	�lB	��B	�RB	��B	�XB	�XB	��B	��B	��B	��B	��B	�JB	�JB	�PB	�B	�B	��B	��B	�6B	��B	�qB	�VB	�qB	��B	��B	��B	��B
  B
 B
 iB
 �B
 �B
 OB
B
;B
 B
oB
�B
�B
oB
B
uB
-B
B
aB
MB
B
B
9B
SB
�B
�B
?B
tB
tB
?B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
EB
�B
B
B
1B
fB
�B
	B
	RB

�B

rB

�B
DB
�B
�B
�B
�B
�B
�B
JB
�B
B
�B
"B
�B
�B
�B
(B
BB
�B
�B
B
�B
�B
B
:B
�B
�B
uB
�B
B
,B
{B
{B
�B
aB
,B
�B
MB
�B
�B
2B
�B
gB
�B
�B
�B
SB
mB
�B
�B
SB
�B
�B
�B
�B
B
�B
WB
WB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
xB
]B
�B
B
dB
�B
�B
�B
!B
�B
�B
�B
�B
 BB
 �B
!B
!|B
!�B
!�B
!�B
!�B
"B
"�B
#:B
#�B
$B
$&B
$@B
$�B
%FB
%�B
&�B
'�B
(XB
(�B
(�B
+6B
)�B
)�B
)�B
*eB
*�B
+6B
+B
+�B
,WB
,qB
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/B
/�B
/�B
/�B
/�B
0!B
0!B
1AB
1�B
1�B
2B
2B
2�B
2�B
3hB
3�B
3�B
4B
4nB
4TB
4TB
4TB
4�B
4�B
4�B
5?B
5�B
6`B
6+B
6FB
6zB
6�B
7B
7fB
7�B
8B
8B
8B
88B
8�B
8RB
8RB
8�B
8�B
8�B
8�B
9XB
9�B
9rB
9�B
9�B
9�B
9�B
9XB
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
<B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>(B
>wB
>�B
?�B
@�B
@�B
AoB
BAB
B'B
BAB
B[B
B�B
CaB
C�B
C�B
C�B
E9B
E�B
E�B
F�B
GB
G+B
GzB
G�B
H1B
H�B
IB
I7B
I�B
I�B
I�B
JXB
J�B
JrB
J=B
J�B
K^B
K�B
K�B
K�B
K�B
LJB
MB
MB
MB
M6B
MjB
MB
M�B
NB
M�B
M�B
NB
N"B
NVB
N�B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PB
O�B
PB
PHB
P}B
P}B
P�B
P�B
P�B
Q B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
Q�B
R:B
RoB
R�B
SB
SB
SB
SB
SB
S&B
S&B
S[B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
UMB
VSB
VmB
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
YeB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
\B
\xB
\]B
\)B
\CB
\xB
\�B
\�B
]/B
]/B
]dB
]�B
]�B
]�B
^OB
^�B
_pB
_�B
_�B
`B
`B
`'B
`B
`B
`'B
`BB
`\B
`\B
`�B
`�B
a-B
aHB
abB
a�B
a�B
bB
bB
a�B
bB
bB
bB
bB
b4B
b�B
b�B
b�B
b�B
cB
b�B
b�B
b�B
cTB
cB
cB
b�B
b�B
cB
c B
cnB
c�B
c�B
c�B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
hXB
h�B
iDB
iDB
iyB
i�B
jKB
jeB
jeB
jeB
j�B
j�B
j�B
k6B
k6B
kQB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
l�B
mB
m]B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
n�B
n�B
o5B
oOB
o�B
o�B
pB
p!B
p;B
p;B
p;B
p;B
p;B
pUB
p�B
p�B
q'B
q'B
qAB
q�B
q�B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tTB
tnB
tnB
tnB
tnB
tnB
t�B
t�B
t�B
t�B
uZB
utB
utB
u�B
u�B
u�B
u�B
vB
v+B
vFB
vFB
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	~�B	~�B	cB	}�B	~�B	~]B	}�B	}�B	}qB	}<B	}B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|jB	|jB	|jB	|6B	|6B	|B	z�B	w�B	sB	lqB	oB	q[B	q�B	pB	m�B	poB	i�B	h�B	iyB	k�B	p!B	��B	�B	�B	��B	��B
�B
4�B
Q�B
��B
�B
�B
�tB
�B
��B
y�B	��B
GB	��B	��B	ǔB	�B	�_B	��B	�.B	�tB	�kB	�|B	�?B	� B	��B	�=B	�QB	�WB	�=B	s�B	P�B	=�B	5ZB	&�B	kB	�B�6B�B�B��B�B�zB�-B�]B�9BȚB�.B�+B��B�_B�
B�FB��B�;B�VB�!B��B�BB�vB��B��B��B�2B�DB�B�[BƨB�zB�B�dB� B��B�pB��B�hB�B�vB��B	4B	/ B	C�B	K�B	XB	`'B	s�B	��B	��B	��B	��B	��B	�\B	�pB	�B	��B	�B	�\B	��B	�&B	�:B	�8B	��B	��B	��B	��B	�0B	�eB	�B	��B	�3B	��B	��B	�B	�XB	�$B	�XB	�$B	��B	��B	��B	�jB	�B	�]B	��B	��B	�iB	�[B	�{B	��B	�3B	�MB	�MB	�3B	�YB	żB	�B	ǮB	��B	��B	�#B	�B	�JB	�B	�B	͹B	οB	οB	οB	�NB	��B	�4B	ѷB	ԯB	ؓB	��B	��B	��B	�4B	�B	�B	�:B	��B	�B	�2B	��B	�&B	�nB	�B	�hB	�B	�B	�@B	� B	�:B	�TB	�nB	��B	��B	�,B	��B	��B	�B	�`B	��B	��B	��B	�B	��B	�B	�B	�&B	�B	�B	�8B	�
B	�FB	�tB	�FB	�B	��B	��B	�B	�B	�`B	�`B	�B	�B	��B	�B	�0B	��B	�yB	�sB	�KB	�6B	�qB	�B	�B	�cB	�B	�B	�vB	�AB	�B	��B	�aB	�vB	�B	�cB	�IB	��B	��B	�vB	�[B	�vB	��B	�hB	�B	�hB	��B	��B	��B	�B	��B	�-B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�0B	�B	�B	��B	�IB	�B	��B	�B	��B	�'B	��B	�B	�AB	��B	��B	��B	�zB	��B	��B	��B	�B	�lB	��B	��B	��B	�8B	��B	�B	��B	�B	�"B	�qB	��B	��B	��B	�dB	��B	�rB	�B	�xB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�8B	��B	�XB	��B	�*B	��B	�8B	�>B	��B	��B	��B	��B	�LB	�B	�lB	�RB	�lB	��B	�RB	��B	�XB	�XB	��B	��B	��B	��B	��B	�JB	�JB	�PB	�B	�B	��B	��B	�6B	��B	�qB	�VB	�qB	��B	��B	��B	��B
  B
 B
 iB
 �B
 �B
 OB
B
;B
 B
oB
�B
�B
oB
B
uB
-B
B
aB
MB
B
B
9B
SB
�B
�B
?B
tB
tB
?B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
EB
�B
B
B
1B
fB
�B
	B
	RB

�B

rB

�B
DB
�B
�B
�B
�B
�B
�B
JB
�B
B
�B
"B
�B
�B
�B
(B
BB
�B
�B
B
�B
�B
B
:B
�B
�B
uB
�B
B
,B
{B
{B
�B
aB
,B
�B
MB
�B
�B
2B
�B
gB
�B
�B
�B
SB
mB
�B
�B
SB
�B
�B
�B
�B
B
�B
WB
WB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
xB
]B
�B
B
dB
�B
�B
�B
!B
�B
�B
�B
�B
 BB
 �B
!B
!|B
!�B
!�B
!�B
!�B
"B
"�B
#:B
#�B
$B
$&B
$@B
$�B
%FB
%�B
&�B
'�B
(XB
(�B
(�B
+6B
)�B
)�B
)�B
*eB
*�B
+6B
+B
+�B
,WB
,qB
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/B
/�B
/�B
/�B
/�B
0!B
0!B
1AB
1�B
1�B
2B
2B
2�B
2�B
3hB
3�B
3�B
4B
4nB
4TB
4TB
4TB
4�B
4�B
4�B
5?B
5�B
6`B
6+B
6FB
6zB
6�B
7B
7fB
7�B
8B
8B
8B
88B
8�B
8RB
8RB
8�B
8�B
8�B
8�B
9XB
9�B
9rB
9�B
9�B
9�B
9�B
9XB
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
<B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>(B
>wB
>�B
?�B
@�B
@�B
AoB
BAB
B'B
BAB
B[B
B�B
CaB
C�B
C�B
C�B
E9B
E�B
E�B
F�B
GB
G+B
GzB
G�B
H1B
H�B
IB
I7B
I�B
I�B
I�B
JXB
J�B
JrB
J=B
J�B
K^B
K�B
K�B
K�B
K�B
LJB
MB
MB
MB
M6B
MjB
MB
M�B
NB
M�B
M�B
NB
N"B
NVB
N�B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PB
O�B
PB
PHB
P}B
P}B
P�B
P�B
P�B
Q B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
Q�B
R:B
RoB
R�B
SB
SB
SB
SB
SB
S&B
S&B
S[B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
UMB
VSB
VmB
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
YeB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
\B
\xB
\]B
\)B
\CB
\xB
\�B
\�B
]/B
]/B
]dB
]�B
]�B
]�B
^OB
^�B
_pB
_�B
_�B
`B
`B
`'B
`B
`B
`'B
`BB
`\B
`\B
`�B
`�B
a-B
aHB
abB
a�B
a�B
bB
bB
a�B
bB
bB
bB
bB
b4B
b�B
b�B
b�B
b�B
cB
b�B
b�B
b�B
cTB
cB
cB
b�B
b�B
cB
c B
cnB
c�B
c�B
c�B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
hXB
h�B
iDB
iDB
iyB
i�B
jKB
jeB
jeB
jeB
j�B
j�B
j�B
k6B
k6B
kQB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
l�B
mB
m]B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
n�B
n�B
o5B
oOB
o�B
o�B
pB
p!B
p;B
p;B
p;B
p;B
p;B
pUB
p�B
p�B
q'B
q'B
qAB
q�B
q�B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tTB
tnB
tnB
tnB
tnB
tnB
t�B
t�B
t�B
t�B
uZB
utB
utB
u�B
u�B
u�B
u�B
vB
v+B
vFB
vFB
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104944  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174833  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174833  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174833                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024840  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024840  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                