CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-29T00:42:10Z creation;2023-01-29T00:42:11Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230129004210  20230129005944  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���~1   @���+<@-��t��c$�/��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A^ffA�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�33B�  B���B�  B�  B�  B�33B�33B���B���B�  B�33B�33B�  B���B�  B�  B̙�B�  Bә�B���B���B���B�  B�  B�  B�  B�  B�  B�  C   CL�C  C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@|(�@�{A ��A
=A?
=A]p�A
=A�Q�A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB`(�BgBoBwBB��HB�{B��HB��B��HB��HB��HB�{B�{B�z�B��B��HB�{B�{B��HB��B��HB��HB�z�B��HB�z�B׮BۮB߮B��HB��HB��HB��HB��HB��HB��HB��HC=qC�C�
C�
C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C8
>C:
>C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cr
>Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!u�D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�AGDǁGDǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D���D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�qG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʢhAʣ�Aʢ�Aʞ�Aʠ'AʥzAʦ�Aʨ$Aʱ�Aʭ�Aʯ�Aʴ9Aʸ�Aʼ�A��tA��,A��A��2A��cA�oA�AUA�m�A˘�A˙1A˚7A˝�A˟�A˞�A˞A˝A˟�A˭CAˮ�A˩�A˥�A˦LA˥�A˥�A˦LAˡ�A˛qA�cAʔAĔ�AįOA�-wA��VA�^5A�$@A�y�A���A��"A��'A��A��A�,=A���A�<�A��aA���A�
�A��A��A�f2A��YA�f�A���A��A�@A��QA���A��A��(A���A��+A��A�;�A��A�qvA��ZA�G�AU�A}�Az=AwK^As�Ap�7Ao��An��An�Ai��Ad�Acm�Aa֡A`�A^�cAZ+AYAWl"AUȴAP�AAI�!AE>BAA>BA>�zA:!A7A6w2A6iDA5��A5(A4C-A3W?A2&�A1�A0��A/��A,ɆA+�A)��A(�5A'�BA'/�A&��A$��A#�7A"�UA"�MA!�A!CA �kA �AY�A;Az�A��A4A�A�PA��A�A?A�HAںA�EA҉A�hA�eA}VA�A;AdZAu�A��AA��A�A�BAL0A��A��AiDA��A'RA�A�A �A
?A	~(AѷA�A�AN<A iA;dA�A&�A(�A�$A:�A�<A{�A�A�MA	lA{JA��A �>A \�A 8�A �@�]�@�|�@�(�@�M�@�u%@��o@� \@��~@���@���@�(�@�6@���@��@���@�m�@�J@�Z@���@�ی@�6�@��@�p�@���@�@���@��@�ƨ@��@�h@�G@�W?@�Ɇ@���@��a@�!-@�U2@�<6@�4@�1'@��@��@�o @�%@�~(@�ں@�.�@��@�s@�o@��@��F@��@�A @ޒ�@��@ݙ�@��@�l�@ۆ�@��@���@�p�@��@�i�@��m@��@���@֟�@��@ճ�@��K@ջ0@Ւ:@�k�@�)_@Զ�@�
�@�IR@Ү}@�:*@ѐ�@��@�D�@�7L@��2@η�@�M�@�S�@�z@�?}@�V@�B[@���@ˑh@˄M@��8@ȅ�@�A @�F@ǥ�@Ǟ�@ƽ<@�RT@�2�@��.@ŗ�@�IR@�)_@�$t@��N@�u�@Ɵ�@�n�@�w�@ŀ4@�x�@Śk@�c�@�33@�&�@�=@�F�@�)_@���@�m�@��@���@õt@��@�@�{�@��T@��@���@���@��=@�P�@��D@�S@��@��n@��j@�Q�@��@�9�@�y>@���@�v`@�@��@�(�@���@���@���@�x@��}@�ff@�{�@�<6@��O@�dZ@�:�@��C@��h@�a�@��@���@� �@�rG@��5@�y>@��@�2a@���@���@���@�Xy@�	�@�{J@��@���@�[�@��D@�\)@�.I@�@@��y@�/�@���@�t�@�9�@��2@���@��]@��h@�O�@� \@��@��@���@�h�@�qv@�@��`@��x@�!@��a@��@��9@�`�@�c@�Y@�ں@�m�@�;�@�;�@�+k@��@���@�n/@��]@���@���@�bN@�e�@�-@�~@��@���@��k@�|�@�5�@�(@��,@�d�@��@���@���@�L0@� �@���@�w2@�S&@���@��O@��@��@���@�S�@��f@�l�@�+k@�˒@�v`@�@�Ĝ@��o@�u%@�V@�	@���@���@�\�@�e�@��@���@��e@��\@�>B@��@��^@�J�@�o@��@���@���@�<�@��.@�ݘ@���@�K�@�
=@��f@��@��)@�j@�b@��@��=@�f�@�8�@��M@��R@�ff@�7�@��@���@�=@�ں@���@�C-@�4@��Z@���@���@��M@�S&@�!-@��/@��h@��@�d�@�6�@�	@��#@���@���@��@�/@��U@���@�`�@�V�@�U2@�6@��o@��@�Vm@�>�@��@���@���@��@��a@��f@�O@��@��]@�}V@��@��6@�k�@�B�@��|@��$@��D@�~(@�e�@�@���@�|@�hs@�`B@�Y�@�F�@�,�@��@���@���@�Z�@�	@�G@��@��w@���@�j�@�=�@���@��5@��y@���@�|�@�2�@��@~�@}��@}Q�@|�[@|!@{�@{��@{.I@{(@zJ�@zu@yq@w�
@v�B@v�6@v}V@v�@t��@t9X@s��@rd�@q��@qhs@qJ�@q@@pl"@pb@o�K@oo�@oo@n?@m�t@m�7@m4@l��@l�4@l �@k�@j�m@jq�@jV@j�@h�)@g��@g]�@gE9@g>�@g=@gs@f�"@f_@e��@e&�@d�?@d�z@d�D@d-�@c��@c�V@c{J@c�@bxl@a��@ap�@a&�@`��@`�@`j@`~@_�F@_��@^�!@^e@]�d@]��@]�@]8�@\��@\K^@[�q@Z��@Z	@Y�@Y�'@Y�7@Yk�@Y�@X�/@X�@XtT@Wy�@V��@V��@Vȴ@V?@U��@U0�@T�p@Tq@T?�@S��@S�4@S�@Rxl@RYK@RE�@Q��@Qe,@Q@P��@PQ�@O�r@O��@O�{@O6z@O�@Nz@N.�@N	@M�@M��@M�=@M�~@Mp�@M^�@MDg@L��@L��@K�}@K��@Kl�@K_p@KC�@J��@J��@J_�@I�@I�@I�h@IV@H�@H��@H:�@G�*@G�4@Gj�@F��@F~�@F3�@E�9@E[W@E�@D�@D�@Dl"@D!@Co�@C(@B��@BC�@A��@ADg@@�@@��@@�o@@6@@1@?�A@?��@>�y@>�1@>�A@>YK@>
�@=�@<�K@<7@;�@;�@;;d@:��@:_@9��@9��@9�@8�@8ی@8�[@8�)@8��@8K^@7��@7v`@7�@6�]@6�r@6�@5��@5��@5 \@4�@4��@4�@3��@2�@2�R@2~�@2_�@2R�@1��@1k�@1?}@1�@0�@0��@0N�@0�@/�@/�6@/��@/J#@/
=@.��@.�R@.�b@.J�@.�@-��@-�X@-w2@--w@,��@,��@,_@,K^@,%�@+��@+�@+�$@+��@+��@+$t@*� @*L0@*�@)�@)��@)��@)S&@)�@(�u@(4n@(�@'��@'��@'dZ@')_@&�B@&��@&Q@&&�@&u@%�=@%G�@$��@$ی@$�Y@$[�@$G@#)_@"�@"�R@"��@"��@"�b@"&�@!�>@!�@!/@ �z@ ��@ Xy@ C-@ @��@)_@�@��@}V@J@�3@��@-w@�P@�?@�u@h�@Z@Q�@	�@�:@dZ@J#@33@�@�@��@�1@i�@{@��@�@a�@A @��@��@�@��@u�@Xy@:�@7�@,=@��@�{@b�@H�@!-@
=@�"@��@�B@�6@}V@ff@GE@	@�9@@��@Vm@ی@��@oi@Z@H@6@(�@�@خ@�P@"�@�B@\�@$�@�@�@��@�-@�=@�S@c@s�@-w@�)@K^@�@�A@خ@��@o�@X�@A�@!-@@�B@�+@z@l�@YK@GE@+k@�D@�@�'@��@m]@F@*0@��@�?@�.@[�@*�@�@��@�&@�}@��@��@H�@
�y@
��@
��@
_�@
L0@
.�@
e@	��@	IR@	@@��@ی@Ɇ@��@~(@`�@'R@1@�A@��@�g@��@t�@E9@"�@��@�B@��@{�@=q@O@	@�@�z@��@hs@Dg@*0@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʢhAʣ�Aʢ�Aʞ�Aʠ'AʥzAʦ�Aʨ$Aʱ�Aʭ�Aʯ�Aʴ9Aʸ�Aʼ�A��tA��,A��A��2A��cA�oA�AUA�m�A˘�A˙1A˚7A˝�A˟�A˞�A˞A˝A˟�A˭CAˮ�A˩�A˥�A˦LA˥�A˥�A˦LAˡ�A˛qA�cAʔAĔ�AįOA�-wA��VA�^5A�$@A�y�A���A��"A��'A��A��A�,=A���A�<�A��aA���A�
�A��A��A�f2A��YA�f�A���A��A�@A��QA���A��A��(A���A��+A��A�;�A��A�qvA��ZA�G�AU�A}�Az=AwK^As�Ap�7Ao��An��An�Ai��Ad�Acm�Aa֡A`�A^�cAZ+AYAWl"AUȴAP�AAI�!AE>BAA>BA>�zA:!A7A6w2A6iDA5��A5(A4C-A3W?A2&�A1�A0��A/��A,ɆA+�A)��A(�5A'�BA'/�A&��A$��A#�7A"�UA"�MA!�A!CA �kA �AY�A;Az�A��A4A�A�PA��A�A?A�HAںA�EA҉A�hA�eA}VA�A;AdZAu�A��AA��A�A�BAL0A��A��AiDA��A'RA�A�A �A
?A	~(AѷA�A�AN<A iA;dA�A&�A(�A�$A:�A�<A{�A�A�MA	lA{JA��A �>A \�A 8�A �@�]�@�|�@�(�@�M�@�u%@��o@� \@��~@���@���@�(�@�6@���@��@���@�m�@�J@�Z@���@�ی@�6�@��@�p�@���@�@���@��@�ƨ@��@�h@�G@�W?@�Ɇ@���@��a@�!-@�U2@�<6@�4@�1'@��@��@�o @�%@�~(@�ں@�.�@��@�s@�o@��@��F@��@�A @ޒ�@��@ݙ�@��@�l�@ۆ�@��@���@�p�@��@�i�@��m@��@���@֟�@��@ճ�@��K@ջ0@Ւ:@�k�@�)_@Զ�@�
�@�IR@Ү}@�:*@ѐ�@��@�D�@�7L@��2@η�@�M�@�S�@�z@�?}@�V@�B[@���@ˑh@˄M@��8@ȅ�@�A @�F@ǥ�@Ǟ�@ƽ<@�RT@�2�@��.@ŗ�@�IR@�)_@�$t@��N@�u�@Ɵ�@�n�@�w�@ŀ4@�x�@Śk@�c�@�33@�&�@�=@�F�@�)_@���@�m�@��@���@õt@��@�@�{�@��T@��@���@���@��=@�P�@��D@�S@��@��n@��j@�Q�@��@�9�@�y>@���@�v`@�@��@�(�@���@���@���@�x@��}@�ff@�{�@�<6@��O@�dZ@�:�@��C@��h@�a�@��@���@� �@�rG@��5@�y>@��@�2a@���@���@���@�Xy@�	�@�{J@��@���@�[�@��D@�\)@�.I@�@@��y@�/�@���@�t�@�9�@��2@���@��]@��h@�O�@� \@��@��@���@�h�@�qv@�@��`@��x@�!@��a@��@��9@�`�@�c@�Y@�ں@�m�@�;�@�;�@�+k@��@���@�n/@��]@���@���@�bN@�e�@�-@�~@��@���@��k@�|�@�5�@�(@��,@�d�@��@���@���@�L0@� �@���@�w2@�S&@���@��O@��@��@���@�S�@��f@�l�@�+k@�˒@�v`@�@�Ĝ@��o@�u%@�V@�	@���@���@�\�@�e�@��@���@��e@��\@�>B@��@��^@�J�@�o@��@���@���@�<�@��.@�ݘ@���@�K�@�
=@��f@��@��)@�j@�b@��@��=@�f�@�8�@��M@��R@�ff@�7�@��@���@�=@�ں@���@�C-@�4@��Z@���@���@��M@�S&@�!-@��/@��h@��@�d�@�6�@�	@��#@���@���@��@�/@��U@���@�`�@�V�@�U2@�6@��o@��@�Vm@�>�@��@���@���@��@��a@��f@�O@��@��]@�}V@��@��6@�k�@�B�@��|@��$@��D@�~(@�e�@�@���@�|@�hs@�`B@�Y�@�F�@�,�@��@���@���@�Z�@�	@�G@��@��w@���@�j�@�=�@���@��5@��y@���@�|�@�2�@��@~�@}��@}Q�@|�[@|!@{�@{��@{.I@{(@zJ�@zu@yq@w�
@v�B@v�6@v}V@v�@t��@t9X@s��@rd�@q��@qhs@qJ�@q@@pl"@pb@o�K@oo�@oo@n?@m�t@m�7@m4@l��@l�4@l �@k�@j�m@jq�@jV@j�@h�)@g��@g]�@gE9@g>�@g=@gs@f�"@f_@e��@e&�@d�?@d�z@d�D@d-�@c��@c�V@c{J@c�@bxl@a��@ap�@a&�@`��@`�@`j@`~@_�F@_��@^�!@^e@]�d@]��@]�@]8�@\��@\K^@[�q@Z��@Z	@Y�@Y�'@Y�7@Yk�@Y�@X�/@X�@XtT@Wy�@V��@V��@Vȴ@V?@U��@U0�@T�p@Tq@T?�@S��@S�4@S�@Rxl@RYK@RE�@Q��@Qe,@Q@P��@PQ�@O�r@O��@O�{@O6z@O�@Nz@N.�@N	@M�@M��@M�=@M�~@Mp�@M^�@MDg@L��@L��@K�}@K��@Kl�@K_p@KC�@J��@J��@J_�@I�@I�@I�h@IV@H�@H��@H:�@G�*@G�4@Gj�@F��@F~�@F3�@E�9@E[W@E�@D�@D�@Dl"@D!@Co�@C(@B��@BC�@A��@ADg@@�@@��@@�o@@6@@1@?�A@?��@>�y@>�1@>�A@>YK@>
�@=�@<�K@<7@;�@;�@;;d@:��@:_@9��@9��@9�@8�@8ی@8�[@8�)@8��@8K^@7��@7v`@7�@6�]@6�r@6�@5��@5��@5 \@4�@4��@4�@3��@2�@2�R@2~�@2_�@2R�@1��@1k�@1?}@1�@0�@0��@0N�@0�@/�@/�6@/��@/J#@/
=@.��@.�R@.�b@.J�@.�@-��@-�X@-w2@--w@,��@,��@,_@,K^@,%�@+��@+�@+�$@+��@+��@+$t@*� @*L0@*�@)�@)��@)��@)S&@)�@(�u@(4n@(�@'��@'��@'dZ@')_@&�B@&��@&Q@&&�@&u@%�=@%G�@$��@$ی@$�Y@$[�@$G@#)_@"�@"�R@"��@"��@"�b@"&�@!�>@!�@!/@ �z@ ��@ Xy@ C-@ @��@)_@�@��@}V@J@�3@��@-w@�P@�?@�u@h�@Z@Q�@	�@�:@dZ@J#@33@�@�@��@�1@i�@{@��@�@a�@A @��@��@�@��@u�@Xy@:�@7�@,=@��@�{@b�@H�@!-@
=@�"@��@�B@�6@}V@ff@GE@	@�9@@��@Vm@ی@��@oi@Z@H@6@(�@�@خ@�P@"�@�B@\�@$�@�@�@��@�-@�=@�S@c@s�@-w@�)@K^@�@�A@خ@��@o�@X�@A�@!-@@�B@�+@z@l�@YK@GE@+k@�D@�@�'@��@m]@F@*0@��@�?@�.@[�@*�@�@��@�&@�}@��@��@H�@
�y@
��@
��@
_�@
L0@
.�@
e@	��@	IR@	@@��@ی@Ɇ@��@~(@`�@'R@1@�A@��@�g@��@t�@E9@"�@��@�B@��@{�@=q@O@	@�@�z@��@hs@Dg@*0@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	�VB	�VB	��B	�6B	�jB	�BB	�pB	��B	��B	��B	�oB	�B	�B	��B	��B	��B	�^B	�B	��B
�B
�B
1B
CB
�B
pB
pB
pB
 B
#�B
$�B
$B
$B
$�B
%�B
%�B
%�B
%`B
%�B
&2B
B�B
�3B
��B
ŢB
�KB
�B
�B
�vB�B�B�B$�B;BC�BC�BA B4B2�B1vB)_B,qB:�B�B0B"NB0B
�0B
�0B
��B
��B
vB
m�B
I�B
>B
�B
 B	�eB	ٴB	бB	˒B	ƨB	��B	��B	�\B	{B	r�B	qB	m)B	c B	DgB	9	B	1[B	*B	 �B	B	
=B	�B�BԯB�tB��BvzBl�B\�B^5BgmBi*BkBl�BoiBu%Bz*B��B��B��B�B��B��B�B�0B��BΥB�1B�oB�$B��B	�B	OB	)B	*�B	(�B	&�B	-�B	33B	5tB	8�B	H�B	QNB	O\B	F�B	E�B	F�B	N<B	MB	E�B	@�B	IB	J�B	S�B	SuB	T�B	V�B	]~B	^B	bNB	e`B	e,B	e�B	e�B	d�B	d�B	d�B	i�B	j�B	f�B	d�B	e,B	ezB	c B	`BB	_�B	_B	f�B	o�B	y$B	z*B	|�B	}"B	{dB	z�B	w�B	w�B	u�B	u�B	v�B	vFB	u?B	s�B	s�B	t9B	w2B	wLB	r�B	vB	|�B	�XB	��B	��B	�2B	�_B	�/B	��B	��B	��B	��B	��B	��B	�4B	�'B	��B	�xB	�#B	��B	��B	��B	��B	�B	�B	�sB	�9B	��B	��B	� B	�vB	�vB	�:B	�aB	��B	�kB	��B	�XB	�DB	��B	��B	�TB	��B	��B	��B	��B	��B	�dB	��B	��B	�B	�B	�B	�AB	��B	�-B	ÖB	��B	�9B	��B	�?B	��B	�gB	��B	�gB	��B	�SB	ǮB	��B	��B	��B	�KB	��B	�_B	��B	ǔB	ȚB	ʦB	�#B	�lB	�mB	ĶB	��B	�B	��B	��B	��B	��B	��B	āB	�)B	�<B	��B	�B	�}B	��B	�B	��B	ĶB	�[B	�B	��B	��B	�B	��B	��B	�B	ٚB	��B	�;B	�B	�B	��B	�DB	�=B	�B	��B	�IB	�B	�vB	�vB	�AB	�B	�B	�B	�B	��B	��B	�B	�GB	�B	��B	�B	��B	�B	�B	��B	�B	�*B	��B	�B	�B	�B	�B	�B	�B	�`B	��B	��B	�B	�
B	�sB	�sB	��B	��B	��B	��B	��B	��B	��B	�HB
 4B
 OB
 iB
�B
�B
'B
�B
 �B	�HB	�.B	�B
 �B
AB
B
9B
mB
�B
�B
B
B
B
�B
tB
EB
+B
_B
�B
KB
KB
zB
�B
�B
�B
EB
�B
�B
�B
�B
�B
1B
�B
	RB
	B
	�B
	�B
�B
B
3B
B
�B
�B
B
�B
�B
�B
�B
tB
�B
�B
	7B

�B

�B
xB
^B
DB
B

�B
xB
�B
JB
�B
B
�B
xB
^B
xB
�B
�B
JB
JB
�B
�B
<B
�B
VB
�B
�B
:B
:B
�B
:B
�B
oB
:B
�B
&B
�B
�B
oB
�B
MB
�B
$B
�B
�B
�B
�B
B
�B
WB
�B
OB
jB
5B
�B
pB
�B
�B
�B
�B
 BB
!B
!B
 �B
!bB
!�B
!�B
"�B
"�B
"�B
#:B
#�B
#�B
#�B
$&B
$&B
$ZB
$�B
$�B
$tB
$@B
%,B
$�B
%`B
%`B
%�B
%�B
%�B
%�B
&B
&2B
&2B
&�B
&fB
&�B
&LB
'B
'mB
'�B
'�B
'�B
(>B
(�B
(�B
(�B
)DB
)DB
)�B
*B
+B
+kB
+�B
,B
,qB
,"B
+QB
+kB
+QB
+B
,WB
+�B
+�B
+�B
+�B
,"B
-�B
-�B
/�B
/�B
0oB
0oB
0�B
0�B
1'B
1�B
2-B
2aB
2�B
2�B
2�B
3B
3B
3hB
3�B
3�B
3�B
3�B
4B
49B
4�B
4�B
4�B
4�B
4�B
4nB
49B
49B
3�B
4TB
4�B
5?B
5�B
5�B
5�B
6`B
6zB
6B
5�B
4�B
4�B
4�B
4�B
5tB
5�B
6+B
72B
7B
7�B
7�B
8�B
9	B
8�B
8�B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
;0B
;B
<6B
<PB
>(B
?B
?HB
>�B
?cB
@ B
@�B
@�B
A;B
A B
AUB
A�B
BAB
CGB
C�B
C�B
DB
DgB
D�B
D3B
D�B
D�B
D�B
E�B
E�B
FB
FB
F%B
F?B
F�B
GEB
G_B
G_B
GzB
G�B
G�B
HB
HKB
IB
IlB
I�B
I�B
J�B
J�B
J�B
J�B
KB
K)B
K�B
LdB
L�B
MPB
M�B
M�B
M6B
N<B
N"B
N"B
N�B
N�B
N�B
OvB
O�B
PbB
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
Q�B
R B
R B
R B
R�B
R�B
S�B
S�B
S�B
S�B
TB
TFB
TFB
T{B
T�B
T�B
UMB
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
XyB
X�B
Y1B
YKB
Y�B
ZB
ZkB
ZQB
Z�B
Z�B
[	B
[	B
[	B
[�B
\B
\B
\]B
\CB
\B
]/B
^�B
^OB
^jB
^�B
^�B
_B
_VB
_�B
`BB
`vB
`�B
`�B
`�B
`�B
aHB
a�B
bB
bNB
bhB
b�B
b�B
b�B
c:B
cnB
c�B
c�B
d&B
d�B
e`B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gmB
h
B
h$B
h
B
h$B
h�B
h�B
h�B
h�B
h�B
i*B
iDB
i*B
iB
iB
iDB
iyB
j0B
jeB
jeB
j�B
j�B
j�B
kQB
k�B
k�B
k�B
lB
l=B
l"B
lWB
lWB
lqB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
nB
nIB
n}B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
p�B
qB
qvB
q�B
q�B
q�B
q�B
raB
r|B
raB
r�B
sB
sB
shB
shB
s�B
t�B
tnB
tnB
t�B
t�B
uZB
u?B
utB
u�B
u�B
v�B
v�B
wB
v�B
wB
w2B
w�B
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
y	B
yXB
y>B
y�B
y�B
y�B
y�B
zB
zB
z*B
z*B
zB
z�B
z�B
z�B
z�B
{0B
{JB
{dB
{JB
{B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
}"B
}�B
}�B
~B
~B
~(B
~(B
~B
~(B
~]B
~�B
~�B
HB
�B
�B
�B
�B
�4B
�OB
�OB
�iB
�iB
�iB
��B
��B
��B
��B
��B
��B
��B
�AB
�'B
�[B
�uB
�uB
��B
�B
�-B
�-B
�GB
�GB
�{B
��B
��B
��B
��B
��B
�MB
�MB
��B
�B
�B
�B
�SB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�_B
�+B
�EB
�EB
��B
�fB
��B
��B
��B
�B
�B
�7B
�RB
��B
��B
��B
��B
��B
�#B
�#B
�rB
�XB
��B
��B
��B
�DB
�xB
��B
��B
��B
��B
�0B
�dB
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	�VB	�VB	��B	�6B	�jB	�BB	�pB	��B	��B	��B	�oB	�B	�B	��B	��B	��B	�^B	�B	��B
�B
�B
1B
CB
�B
pB
pB
pB
 B
#�B
$�B
$B
$B
$�B
%�B
%�B
%�B
%`B
%�B
&2B
B�B
�3B
��B
ŢB
�KB
�B
�B
�vB�B�B�B$�B;BC�BC�BA B4B2�B1vB)_B,qB:�B�B0B"NB0B
�0B
�0B
��B
��B
vB
m�B
I�B
>B
�B
 B	�eB	ٴB	бB	˒B	ƨB	��B	��B	�\B	{B	r�B	qB	m)B	c B	DgB	9	B	1[B	*B	 �B	B	
=B	�B�BԯB�tB��BvzBl�B\�B^5BgmBi*BkBl�BoiBu%Bz*B��B��B��B�B��B��B�B�0B��BΥB�1B�oB�$B��B	�B	OB	)B	*�B	(�B	&�B	-�B	33B	5tB	8�B	H�B	QNB	O\B	F�B	E�B	F�B	N<B	MB	E�B	@�B	IB	J�B	S�B	SuB	T�B	V�B	]~B	^B	bNB	e`B	e,B	e�B	e�B	d�B	d�B	d�B	i�B	j�B	f�B	d�B	e,B	ezB	c B	`BB	_�B	_B	f�B	o�B	y$B	z*B	|�B	}"B	{dB	z�B	w�B	w�B	u�B	u�B	v�B	vFB	u?B	s�B	s�B	t9B	w2B	wLB	r�B	vB	|�B	�XB	��B	��B	�2B	�_B	�/B	��B	��B	��B	��B	��B	��B	�4B	�'B	��B	�xB	�#B	��B	��B	��B	��B	�B	�B	�sB	�9B	��B	��B	� B	�vB	�vB	�:B	�aB	��B	�kB	��B	�XB	�DB	��B	��B	�TB	��B	��B	��B	��B	��B	�dB	��B	��B	�B	�B	�B	�AB	��B	�-B	ÖB	��B	�9B	��B	�?B	��B	�gB	��B	�gB	��B	�SB	ǮB	��B	��B	��B	�KB	��B	�_B	��B	ǔB	ȚB	ʦB	�#B	�lB	�mB	ĶB	��B	�B	��B	��B	��B	��B	��B	āB	�)B	�<B	��B	�B	�}B	��B	�B	��B	ĶB	�[B	�B	��B	��B	�B	��B	��B	�B	ٚB	��B	�;B	�B	�B	��B	�DB	�=B	�B	��B	�IB	�B	�vB	�vB	�AB	�B	�B	�B	�B	��B	��B	�B	�GB	�B	��B	�B	��B	�B	�B	��B	�B	�*B	��B	�B	�B	�B	�B	�B	�B	�`B	��B	��B	�B	�
B	�sB	�sB	��B	��B	��B	��B	��B	��B	��B	�HB
 4B
 OB
 iB
�B
�B
'B
�B
 �B	�HB	�.B	�B
 �B
AB
B
9B
mB
�B
�B
B
B
B
�B
tB
EB
+B
_B
�B
KB
KB
zB
�B
�B
�B
EB
�B
�B
�B
�B
�B
1B
�B
	RB
	B
	�B
	�B
�B
B
3B
B
�B
�B
B
�B
�B
�B
�B
tB
�B
�B
	7B

�B

�B
xB
^B
DB
B

�B
xB
�B
JB
�B
B
�B
xB
^B
xB
�B
�B
JB
JB
�B
�B
<B
�B
VB
�B
�B
:B
:B
�B
:B
�B
oB
:B
�B
&B
�B
�B
oB
�B
MB
�B
$B
�B
�B
�B
�B
B
�B
WB
�B
OB
jB
5B
�B
pB
�B
�B
�B
�B
 BB
!B
!B
 �B
!bB
!�B
!�B
"�B
"�B
"�B
#:B
#�B
#�B
#�B
$&B
$&B
$ZB
$�B
$�B
$tB
$@B
%,B
$�B
%`B
%`B
%�B
%�B
%�B
%�B
&B
&2B
&2B
&�B
&fB
&�B
&LB
'B
'mB
'�B
'�B
'�B
(>B
(�B
(�B
(�B
)DB
)DB
)�B
*B
+B
+kB
+�B
,B
,qB
,"B
+QB
+kB
+QB
+B
,WB
+�B
+�B
+�B
+�B
,"B
-�B
-�B
/�B
/�B
0oB
0oB
0�B
0�B
1'B
1�B
2-B
2aB
2�B
2�B
2�B
3B
3B
3hB
3�B
3�B
3�B
3�B
4B
49B
4�B
4�B
4�B
4�B
4�B
4nB
49B
49B
3�B
4TB
4�B
5?B
5�B
5�B
5�B
6`B
6zB
6B
5�B
4�B
4�B
4�B
4�B
5tB
5�B
6+B
72B
7B
7�B
7�B
8�B
9	B
8�B
8�B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
;0B
;B
<6B
<PB
>(B
?B
?HB
>�B
?cB
@ B
@�B
@�B
A;B
A B
AUB
A�B
BAB
CGB
C�B
C�B
DB
DgB
D�B
D3B
D�B
D�B
D�B
E�B
E�B
FB
FB
F%B
F?B
F�B
GEB
G_B
G_B
GzB
G�B
G�B
HB
HKB
IB
IlB
I�B
I�B
J�B
J�B
J�B
J�B
KB
K)B
K�B
LdB
L�B
MPB
M�B
M�B
M6B
N<B
N"B
N"B
N�B
N�B
N�B
OvB
O�B
PbB
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
Q�B
R B
R B
R B
R�B
R�B
S�B
S�B
S�B
S�B
TB
TFB
TFB
T{B
T�B
T�B
UMB
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
XyB
X�B
Y1B
YKB
Y�B
ZB
ZkB
ZQB
Z�B
Z�B
[	B
[	B
[	B
[�B
\B
\B
\]B
\CB
\B
]/B
^�B
^OB
^jB
^�B
^�B
_B
_VB
_�B
`BB
`vB
`�B
`�B
`�B
`�B
aHB
a�B
bB
bNB
bhB
b�B
b�B
b�B
c:B
cnB
c�B
c�B
d&B
d�B
e`B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gmB
h
B
h$B
h
B
h$B
h�B
h�B
h�B
h�B
h�B
i*B
iDB
i*B
iB
iB
iDB
iyB
j0B
jeB
jeB
j�B
j�B
j�B
kQB
k�B
k�B
k�B
lB
l=B
l"B
lWB
lWB
lqB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
nB
nIB
n}B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
p�B
qB
qvB
q�B
q�B
q�B
q�B
raB
r|B
raB
r�B
sB
sB
shB
shB
s�B
t�B
tnB
tnB
t�B
t�B
uZB
u?B
utB
u�B
u�B
v�B
v�B
wB
v�B
wB
w2B
w�B
wfB
w�B
w�B
w�B
xB
xB
x8B
xlB
x�B
x�B
y	B
yXB
y>B
y�B
y�B
y�B
y�B
zB
zB
z*B
z*B
zB
z�B
z�B
z�B
z�B
{0B
{JB
{dB
{JB
{B
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
}"B
}�B
}�B
~B
~B
~(B
~(B
~B
~(B
~]B
~�B
~�B
HB
�B
�B
�B
�B
�4B
�OB
�OB
�iB
�iB
�iB
��B
��B
��B
��B
��B
��B
��B
�AB
�'B
�[B
�uB
�uB
��B
�B
�-B
�-B
�GB
�GB
�{B
��B
��B
��B
��B
��B
�MB
�MB
��B
�B
�B
�B
�SB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�_B
�+B
�EB
�EB
��B
�fB
��B
��B
��B
�B
�B
�7B
�RB
��B
��B
��B
��B
��B
�#B
�#B
�rB
�XB
��B
��B
��B
�DB
�xB
��B
��B
��B
��B
�0B
�dB
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230129004157  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230129004210  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230129004210  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230129004211                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230129004211  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230129004211  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230129005944                      G�O�G�O�G�O�                