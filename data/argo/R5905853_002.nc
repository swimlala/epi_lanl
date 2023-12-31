CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:22:51Z creation;2022-06-04T17:22:51Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604172251  20220610121506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ةQ��sK1   @ةR+��@,$�/��d�C��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB�B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C  CffC��C�fC   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@"�\@�G�@�{@�{Ap�A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB�{B��HB��HB��HB��HB��HB��HB�{B�{B��B��HB��HB��B��B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�G�B�G�B�z�B��B��B��HC�C�C�C�C	�C�C�C�C�C�C�C�CW
C�qC�
C�C!�C#�C%�
C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CL
>CN
>CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC�C�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]��D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A� 'A��A���Aˣ�A˺�A�qA�+AʭA�o�A�V�A�E9A�8�A�4�A�.�A�,�A�(XA��A��%A��A��A��jA���Aɸ�Aɹ�AɶzAɵ�Aɷ�AɸRAɵ�Aɲ-Aɮ�AɬqAɩ�AɧRAɣ�AɞAɄ�AɃ{A�}"A�z�A�~(A�xA�k�A��A�"A�(�A�RTA�,=A�VmA��@A���A�tA��A��%A�e�A��vA�ߤA�)�A�;A�49A��kA�($A��MA���A�JA�|A�{A�@OA���A�B�A��PA��A���A�sMA���A�cTA�%zA��;A�^A���A��A��A�چA���A��"A��cA�jKA�;�A��}A�$A}8Az �AxAv��Au+�Aq�An�Am �Ah��Abk�A_	AY4�AT�[AN�#AM�qAM>�AL�AL.�AK��AK�0AH�DAF��AE��AEA�AD�A@�A>�A<��A<��A<�A<��A;iDA9C�A7�jA2aA/MA.��A.��A.<6A+�fA)XyA'-�A&�\A&L0A&E�A&c�A&Q�A"�aA!3�A!�nA!v`A~(Af�AoiA��AیA�4AU2A�A<�AGA�AخA�5A4�A�A:*AN<A��AL0A�DA��AT�A�A�[A��A��A�~A�5A_A=A��A!�A~�A>�A0�A��A�AzAl�Ag�A��A
��A
�A
�xA
��A
�A!�A^�A
�,A	��A	H�A	_Ab�A8�A�1A��A��AݘA~�A��A�!A[WA�BA�zA�SA[�A�A��Ah�ARTASAh
A6�A l"@���@�?}@�xl@�v�@��.@��@���@�*0@��@���@�ں@��@�X�@���@��2@���@�� @�A @�=@�)�@���@�ی@�1@�V@�1@��@��@��@@�ϫ@�/�@���@�7L@�}V@븻@�c@�X@��@��@�Mj@�@�7�@�u@�~@�N<@��@���@��;@�'@�g�@�8@��m@�H@��@��z@�G�@��@���@�L@�&�@ߊ	@��@�B[@�(�@��@ݚk@�e,@��@ܓu@�Q�@�D�@���@ِ�@�S@�2�@�J#@�8�@ԕ�@��@���@Ӭq@�m]@ҧ@�҉@�;�@ϲ-@�,�@�Q�@�_p@��"@�S�@��@��d@ˍP@�m]@�8@ʋD@�e@ɺ^@�O�@���@��]@�d�@ǈf@�J#@�V@��@�YK@��@�1@��@�x�@��@�?�@��@��@��@�7@���@�/@º�@�@�[�@�+k@��@��q@�IR@���@�B[@�x@���@�hs@�w�@�8�@�$�@�	@��@��0@��@�R�@��f@���@��}@�9X@��@��4@�X�@��@���@�}V@�?�@��@��t@�!-@�xl@��@��P@���@�1�@��Q@�*0@���@�1@���@�Y�@�8@���@��R@���@�e@�qv@�S&@�0�@��@�;@���@���@�ƨ@�@@�PH@�\)@�(@���@�?�@���@�w2@�Dg@��K@�y>@�M@�1@��H@�{J@�]�@�/�@��@��x@�&�@�˒@���@�i�@��.@�� @���@�F@��@�ȴ@�q�@�GE@���@�q@��|@�֡@���@��F@�Q�@�_@��$@�S@��1@�M�@� �@�4@��Z@�ϫ@��@��m@���@�H�@�.�@��M@�F@�6z@�.I@��@��_@�:*@��@���@��~@�l�@���@�X�@�N<@��@���@�h�@�E�@�8�@�7@��@�g�@�H�@�*0@��H@��Y@�!�@��w@��{@�RT@�+@��9@�M�@��@�  @���@�}�@�+@��@��@�K^@�5?@�,=@��@��@��>@��@��S@�O@� i@�ی@��@�h�@��@�خ@�ϫ@��@��^@���@��@�\)@�9�@�C@�%@�ߤ@���@��+@�Z@�4n@��@��q@�o�@�hs@�9�@���@�[�@���@�{J@�(�@��P@��U@�oi@��@���@�l�@��K@�kQ@�7�@��Q@�t�@�Mj@�4@���@�L0@��"@�+�@��E@��@�-@�s�@��9@�M@���@���@��V@�e,@�P�@�0�@���@�}V@�E�@��@��@33@~��@~�b@}��@|��@|S�@|	�@|  @{�@{/�@ze@y��@yN<@x�@x�U@x��@x�z@xh�@w{J@v�b@vB[@v
�@uc@u&�@u�@uV@t�/@t:�@s�P@r��@r�1@r&�@q`B@p�`@p�j@p��@p�@o�@og�@n�8@n�@mԕ@m��@m8�@l�)@l�@l*�@k��@k��@k33@j�R@jTa@j�@i|@i%F@h�f@h�`@h��@h�Y@h�@g�a@g��@gx@g+@f�@f@e�~@e<6@d�z@c_p@b�,@b��@bGE@b#:@a��@a5�@`�|@`�@`�@_��@_Mj@_�@^��@]O�@\�@[ݘ@[,�@Z��@Y�@Y�M@Y-w@Y�@X�[@Xoi@X �@X7@X7@X�@X�@W�+@WC@V�@Vu@U�C@U5�@T�e@TN�@T�@S�@S]�@S33@S�@R�@Rں@R��@R	@Q�z@QB�@PXy@O�F@O�P@O;d@O�@N��@Ne@Mc@MDg@L��@L_@K�6@K/�@J��@Jz@J@�@J{@I+�@Hoi@H-�@H�@Gݘ@GRT@G�@F�c@F��@F��@FM�@F;�@F�@F_@E��@E�S@Ea�@E7L@D֡@D��@D�o@D-�@C�6@CS�@C�@B��@B��@A�o@A��@Aj@AY�@A%@@�@@?�@?�+@?�[@?��@?iD@?&@>�6@>5?@>�@=�>@=�N@=��@=��@=��@=�t@=�@=�C@=�7@=A @<�5@<�@<Ɇ@<��@<@;�@;o�@;&@:Ov@:3�@9�T@9O�@8��@8-�@7�{@7RT@6͟@6�}@6V@5�#@58�@4�	@4��@4r�@4G@3��@3E9@2�@2��@1�)@1\�@0��@04n@0�@/��@/�@/��@/��@/�f@/>�@/�@.�6@.��@.Q@.
�@-��@-�@-(�@,�@,��@,~(@,	�@+�A@+|�@+A�@+�@*��@*}V@*&�@)�@)�#@)�@)�-@)��@)Dg@(�	@(��@(�D@([�@(%�@'��@'j�@'�@&�H@&�h@&�1@&�\@&}V@&J�@%�)@%�-@%A @$�@$�E@$�p@$��@$�_@$*�@#��@#��@#O@#&@"�8@"��@"��@"��@"��@"��@"xl@"�@!ԕ@!�C@!c@!<6@ �4@ u�@ A�@ �@��@�k@@O@�@�M@�2@�,@�X@�!@��@#:@�)@��@��@��@hs@X@G�@@��@H@D�@-�@�@��@�V@��@=@,�@'�@�@�@ i@��@ߤ@�@��@��@E�@�.@��@|@^�@O�@B�@0�@�f@��@�I@D�@@�@��@��@S�@8@ i@�h@�@l�@Ov@3�@!�@��@�@�@�@��@��@s�@/@�@��@�9@�@6@�@�m@�Q@خ@�
@� @�@��@��@A�@�c@҉@��@�h@{�@1�@�Z@��@[W@?}@�@�@%@�e@w�@oi@G@��@n/@�@��@ں@�@xl@^5@�@�)@�H@f�@F@�@�P@�@��@��@|�@U2@�@��@�$@�$@��@iD@.I@
�]@
�'@
�@
�+@
V@
J�@
=q@
6�@
	@	�Z@	�@	ϫ@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A� 'A��A���Aˣ�A˺�A�qA�+AʭA�o�A�V�A�E9A�8�A�4�A�.�A�,�A�(XA��A��%A��A��A��jA���Aɸ�Aɹ�AɶzAɵ�Aɷ�AɸRAɵ�Aɲ-Aɮ�AɬqAɩ�AɧRAɣ�AɞAɄ�AɃ{A�}"A�z�A�~(A�xA�k�A��A�"A�(�A�RTA�,=A�VmA��@A���A�tA��A��%A�e�A��vA�ߤA�)�A�;A�49A��kA�($A��MA���A�JA�|A�{A�@OA���A�B�A��PA��A���A�sMA���A�cTA�%zA��;A�^A���A��A��A�چA���A��"A��cA�jKA�;�A��}A�$A}8Az �AxAv��Au+�Aq�An�Am �Ah��Abk�A_	AY4�AT�[AN�#AM�qAM>�AL�AL.�AK��AK�0AH�DAF��AE��AEA�AD�A@�A>�A<��A<��A<�A<��A;iDA9C�A7�jA2aA/MA.��A.��A.<6A+�fA)XyA'-�A&�\A&L0A&E�A&c�A&Q�A"�aA!3�A!�nA!v`A~(Af�AoiA��AیA�4AU2A�A<�AGA�AخA�5A4�A�A:*AN<A��AL0A�DA��AT�A�A�[A��A��A�~A�5A_A=A��A!�A~�A>�A0�A��A�AzAl�Ag�A��A
��A
�A
�xA
��A
�A!�A^�A
�,A	��A	H�A	_Ab�A8�A�1A��A��AݘA~�A��A�!A[WA�BA�zA�SA[�A�A��Ah�ARTASAh
A6�A l"@���@�?}@�xl@�v�@��.@��@���@�*0@��@���@�ں@��@�X�@���@��2@���@�� @�A @�=@�)�@���@�ی@�1@�V@�1@��@��@��@@�ϫ@�/�@���@�7L@�}V@븻@�c@�X@��@��@�Mj@�@�7�@�u@�~@�N<@��@���@��;@�'@�g�@�8@��m@�H@��@��z@�G�@��@���@�L@�&�@ߊ	@��@�B[@�(�@��@ݚk@�e,@��@ܓu@�Q�@�D�@���@ِ�@�S@�2�@�J#@�8�@ԕ�@��@���@Ӭq@�m]@ҧ@�҉@�;�@ϲ-@�,�@�Q�@�_p@��"@�S�@��@��d@ˍP@�m]@�8@ʋD@�e@ɺ^@�O�@���@��]@�d�@ǈf@�J#@�V@��@�YK@��@�1@��@�x�@��@�?�@��@��@��@�7@���@�/@º�@�@�[�@�+k@��@��q@�IR@���@�B[@�x@���@�hs@�w�@�8�@�$�@�	@��@��0@��@�R�@��f@���@��}@�9X@��@��4@�X�@��@���@�}V@�?�@��@��t@�!-@�xl@��@��P@���@�1�@��Q@�*0@���@�1@���@�Y�@�8@���@��R@���@�e@�qv@�S&@�0�@��@�;@���@���@�ƨ@�@@�PH@�\)@�(@���@�?�@���@�w2@�Dg@��K@�y>@�M@�1@��H@�{J@�]�@�/�@��@��x@�&�@�˒@���@�i�@��.@�� @���@�F@��@�ȴ@�q�@�GE@���@�q@��|@�֡@���@��F@�Q�@�_@��$@�S@��1@�M�@� �@�4@��Z@�ϫ@��@��m@���@�H�@�.�@��M@�F@�6z@�.I@��@��_@�:*@��@���@��~@�l�@���@�X�@�N<@��@���@�h�@�E�@�8�@�7@��@�g�@�H�@�*0@��H@��Y@�!�@��w@��{@�RT@�+@��9@�M�@��@�  @���@�}�@�+@��@��@�K^@�5?@�,=@��@��@��>@��@��S@�O@� i@�ی@��@�h�@��@�خ@�ϫ@��@��^@���@��@�\)@�9�@�C@�%@�ߤ@���@��+@�Z@�4n@��@��q@�o�@�hs@�9�@���@�[�@���@�{J@�(�@��P@��U@�oi@��@���@�l�@��K@�kQ@�7�@��Q@�t�@�Mj@�4@���@�L0@��"@�+�@��E@��@�-@�s�@��9@�M@���@���@��V@�e,@�P�@�0�@���@�}V@�E�@��@��@33@~��@~�b@}��@|��@|S�@|	�@|  @{�@{/�@ze@y��@yN<@x�@x�U@x��@x�z@xh�@w{J@v�b@vB[@v
�@uc@u&�@u�@uV@t�/@t:�@s�P@r��@r�1@r&�@q`B@p�`@p�j@p��@p�@o�@og�@n�8@n�@mԕ@m��@m8�@l�)@l�@l*�@k��@k��@k33@j�R@jTa@j�@i|@i%F@h�f@h�`@h��@h�Y@h�@g�a@g��@gx@g+@f�@f@e�~@e<6@d�z@c_p@b�,@b��@bGE@b#:@a��@a5�@`�|@`�@`�@_��@_Mj@_�@^��@]O�@\�@[ݘ@[,�@Z��@Y�@Y�M@Y-w@Y�@X�[@Xoi@X �@X7@X7@X�@X�@W�+@WC@V�@Vu@U�C@U5�@T�e@TN�@T�@S�@S]�@S33@S�@R�@Rں@R��@R	@Q�z@QB�@PXy@O�F@O�P@O;d@O�@N��@Ne@Mc@MDg@L��@L_@K�6@K/�@J��@Jz@J@�@J{@I+�@Hoi@H-�@H�@Gݘ@GRT@G�@F�c@F��@F��@FM�@F;�@F�@F_@E��@E�S@Ea�@E7L@D֡@D��@D�o@D-�@C�6@CS�@C�@B��@B��@A�o@A��@Aj@AY�@A%@@�@@?�@?�+@?�[@?��@?iD@?&@>�6@>5?@>�@=�>@=�N@=��@=��@=��@=�t@=�@=�C@=�7@=A @<�5@<�@<Ɇ@<��@<@;�@;o�@;&@:Ov@:3�@9�T@9O�@8��@8-�@7�{@7RT@6͟@6�}@6V@5�#@58�@4�	@4��@4r�@4G@3��@3E9@2�@2��@1�)@1\�@0��@04n@0�@/��@/�@/��@/��@/�f@/>�@/�@.�6@.��@.Q@.
�@-��@-�@-(�@,�@,��@,~(@,	�@+�A@+|�@+A�@+�@*��@*}V@*&�@)�@)�#@)�@)�-@)��@)Dg@(�	@(��@(�D@([�@(%�@'��@'j�@'�@&�H@&�h@&�1@&�\@&}V@&J�@%�)@%�-@%A @$�@$�E@$�p@$��@$�_@$*�@#��@#��@#O@#&@"�8@"��@"��@"��@"��@"��@"xl@"�@!ԕ@!�C@!c@!<6@ �4@ u�@ A�@ �@��@�k@@O@�@�M@�2@�,@�X@�!@��@#:@�)@��@��@��@hs@X@G�@@��@H@D�@-�@�@��@�V@��@=@,�@'�@�@�@ i@��@ߤ@�@��@��@E�@�.@��@|@^�@O�@B�@0�@�f@��@�I@D�@@�@��@��@S�@8@ i@�h@�@l�@Ov@3�@!�@��@�@�@�@��@��@s�@/@�@��@�9@�@6@�@�m@�Q@خ@�
@� @�@��@��@A�@�c@҉@��@�h@{�@1�@�Z@��@[W@?}@�@�@%@�e@w�@oi@G@��@n/@�@��@ں@�@xl@^5@�@�)@�H@f�@F@�@�P@�@��@��@|�@U2@�@��@�$@�$@��@iD@.I@
�]@
�'@
�@
�+@
V@
J�@
=q@
6�@
	@	�Z@	�@	ϫ@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	՛B
�B	�6B
&�B
r�B
��B
�iB
�4B
�4B
� B
��B
��B
�}B
�}B
��B
�"B
��B
�<B
�PB
�rB
�LB
�fB
��B
�FB
�zB
��B
��B
�`B
�`B
��B
��B
��B
�2B
�LB
�dB
��B
�B
�6B
��B
��B
��B
�*B4B��B��B�CB�aB�>B'B�B�B�B#�B#�B*�B-�B1B1�B5�B2-B(�BIB?B�B1B��B�B��B��B�BB��BxB{BuB[�B;�BUB
�"B
� B
�B
�B
��B
cnB
]/B
S&B
D3B
1'B
�B
BB	��B	�B	�VB	�@B	��B	��B	�gB	eFB	:�B	2�B	�B	�B�;B�B�B�=B�B��B��B��B�6B	3B	�B	!|B	�B	�B	�B	�B	"�B	+6B	%�B	$�B	(�B	 B	�B	�B	.}B	/�B	(XB	!�B	-�B	8RB	DMB	N"B	YeB	t9B	m�B	c B	r�B	u�B	p�B	i�B	[�B	N�B	D�B	B�B	GzB	WYB	�fB	��B	��B	��B	��B	�OB	��B	�$B	�B	�NB	��B	��B	�6B	�5B	�;B	�B	��B	��B	��B	��B	�B	�MB	��B	�ZB	��B	�5B	�B	�2B	��B	��B	��B	�B	�B	�B	ΊB	ϑB	уB	��B	ܬB	�2B	��B	�tB	��B	�B	�B	�B	��B	�7B	��B	�&B	�zB	�:B	�B	�B	�:B	�B	�nB	�B	�TB	�B	�B	��B	� B	��B	��B	ѝB	�HB	��B	�}B	��B	ޞB	�\B	�B	��B	�B	�B	�B	�B	�iB	�B	��B	�B	��B	��B	�MB	��B	��B	�B	�B	�B	�B	�fB	�B	��B	��B	�eB	�=B	�B	�B	��B	��B	�B	�tB	��B	�XB	�B	�B	�B	�$B	�B	�B	��B	�B	�>B	�$B	�B	�RB	��B	�0B	��B	�WB	�B	�/B	�B	�IB	�B	� B	�B	�UB	�UB	��B	�B	��B	�UB	�B	�B	�CB	�/B	�B	�OB	�[B	�[B	�B	�B	�B	�}B	��B	�]B	�wB	�B	�B	�5B	�B	��B	��B	��B	�B	�aB	�aB	�B	�GB	�B	��B	�B	��B	�TB	�B	�nB	��B	��B	�`B	��B	��B	�B	��B	��B	�lB	��B	�rB	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	�cB	��B	��B	��B
 4B
 B
 �B
 �B
B
�B
;B
 B
 �B	��B	�}B	��B	��B	�B	�B	��B	�<B	�(B	��B	�B	�cB
 4B
UB
B
�B
�B
B
�B
�B
B
[B
�B
uB
�B
-B
�B
B
3B
gB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
?B
?B
EB
�B
�B
fB
�B
	�B

�B

�B

�B

�B

�B
B
JB
�B
�B
JB
B
�B
B
�B
�B
pB
�B
�B
BB
�B
HB
}B
 B
�B
:B
oB
�B
�B
�B
�B
B
NB
�B
�B
�B
�B
�B
�B
�B
�B
&B
B
B
@B
&B
�B
gB
MB
2B
�B
�B
MB
�B
�B
�B
2B
9B
mB
�B
�B
�B
QB
=B
qB
�B
�B
�B
/B
/B
dB
�B
B
�B
�B
�B
B
�B
�B
�B
�B
 vB
 \B
 �B
 �B
!HB
!|B
!bB
!|B
!|B
!|B
!�B
!�B
!�B
!�B
"B
"B
"4B
"�B
# B
#B
# B
#:B
#:B
#TB
#nB
#�B
$&B
$@B
$&B
$@B
$@B
$tB
$�B
$�B
%,B
%�B
%�B
%�B
&2B
&fB
'8B
'�B
'�B
(�B
(XB
(�B
(�B
)�B
)�B
)�B
*�B
*�B
+6B
+�B
+�B
,=B
,B
,�B
-�B
/iB
0oB
1'B
1�B
1�B
2�B
3hB
3�B
3�B
3�B
3�B
4B
4B
49B
4TB
4�B
5B
5?B
5tB
5�B
6B
5�B
6�B
7LB
8B
8RB
8RB
8lB
8�B
9XB
9XB
9�B
:B
:B
:B
9�B
9�B
:�B
:�B
;0B
;B
;B
;B
;dB
;JB
;dB
;�B
<PB
<�B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>B
>(B
>]B
>�B
?B
?.B
?cB
?}B
?�B
?�B
?�B
@ B
@iB
@�B
@�B
@�B
AUB
AUB
AoB
AoB
AoB
AoB
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
CB
D3B
DgB
DgB
D�B
D�B
D�B
E�B
EmB
E9B
ESB
F?B
F%B
F%B
F%B
G�B
G�B
G�B
HKB
H�B
IB
IRB
I�B
I�B
I�B
I�B
J#B
J#B
J	B
J	B
I�B
I�B
J�B
KDB
K)B
KxB
K�B
LB
LJB
LJB
L~B
L�B
L�B
L�B
L�B
L�B
MB
M6B
M6B
M�B
N"B
NpB
NpB
N�B
N�B
OB
OBB
O�B
O�B
P.B
PbB
P�B
QB
Q4B
QhB
Q�B
QhB
R:B
R�B
R�B
R�B
R�B
S@B
S[B
S[B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
TB
T,B
TaB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
VB
V�B
V�B
W
B
W$B
WsB
W�B
XB
XEB
X_B
XyB
XyB
X�B
X�B
Y1B
YeB
YB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZB
ZkB
Z�B
Z�B
[=B
[�B
]IB
\�B
\�B
]IB
]dB
^jB
^�B
^�B
_;B
_;B
_�B
`B
`�B
`�B
`�B
aHB
a�B
a�B
bB
b4B
bhB
b�B
cnB
c�B
dZB
dZB
dtB
dtB
dtB
d�B
d�B
d�B
d�B
eFB
eFB
ezB
e`B
ezB
ezB
fB
f2B
f2B
ffB
f�B
f�B
gRB
gRB
g�B
gRB
h
B
h>B
hXB
hXB
hsB
hsB
h�B
h�B
i*B
i*B
i_B
iyB
i�B
i�B
jeB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
l=B
l=B
l=B
l=B
lWB
l�B
m)B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
nB
ncB
n}B
n�B
n�B
n�B
o B
o�B
o�B
pB
p!B
p;B
poB
qB
qB
qAB
qAB
q[B
q[B
qvB
q�B
q�B
rB
rGB
rGB
raB
r�B
r�B
r�B
r�B
sB
s3B
sMB
sMB
s�B
s�B
s�B
s�B
t9B
tB
t9B
tTB
tTB
tTB
tTB
tnB
t�B
t�B
t�B
t�B
u?B
uZB
u�B
u�B
u�B
u�B
u�B
vB
u�B
v`B
vzB
v�B
v�B
v�B
wB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
yXB
yrB
y�B
zB
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{dB
{B
{�B
{�B
{�B
|B
|6B
|�B
}B
}B
}VB
}<B
}<B
}�B
}�B
}�B
~BB
~wB
~�B
cB
HB
HB
cB
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
�UB
��B
��B
��B
��B
��B
�AB
�AB
�uB
��B
��B
�aB
�aB
��B
�{B
��B
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	՛B
�B	�6B
&�B
r�B
��B
�iB
�4B
�4B
� B
��B
��B
�}B
�}B
��B
�"B
��B
�<B
�PB
�rB
�LB
�fB
��B
�FB
�zB
��B
��B
�`B
�`B
��B
��B
��B
�2B
�LB
�dB
��B
�B
�6B
��B
��B
��B
�*B4B��B��B�CB�aB�>B'B�B�B�B#�B#�B*�B-�B1B1�B5�B2-B(�BIB?B�B1B��B�B��B��B�BB��BxB{BuB[�B;�BUB
�"B
� B
�B
�B
��B
cnB
]/B
S&B
D3B
1'B
�B
BB	��B	�B	�VB	�@B	��B	��B	�gB	eFB	:�B	2�B	�B	�B�;B�B�B�=B�B��B��B��B�6B	3B	�B	!|B	�B	�B	�B	�B	"�B	+6B	%�B	$�B	(�B	 B	�B	�B	.}B	/�B	(XB	!�B	-�B	8RB	DMB	N"B	YeB	t9B	m�B	c B	r�B	u�B	p�B	i�B	[�B	N�B	D�B	B�B	GzB	WYB	�fB	��B	��B	��B	��B	�OB	��B	�$B	�B	�NB	��B	��B	�6B	�5B	�;B	�B	��B	��B	��B	��B	�B	�MB	��B	�ZB	��B	�5B	�B	�2B	��B	��B	��B	�B	�B	�B	ΊB	ϑB	уB	��B	ܬB	�2B	��B	�tB	��B	�B	�B	�B	��B	�7B	��B	�&B	�zB	�:B	�B	�B	�:B	�B	�nB	�B	�TB	�B	�B	��B	� B	��B	��B	ѝB	�HB	��B	�}B	��B	ޞB	�\B	�B	��B	�B	�B	�B	�B	�iB	�B	��B	�B	��B	��B	�MB	��B	��B	�B	�B	�B	�B	�fB	�B	��B	��B	�eB	�=B	�B	�B	��B	��B	�B	�tB	��B	�XB	�B	�B	�B	�$B	�B	�B	��B	�B	�>B	�$B	�B	�RB	��B	�0B	��B	�WB	�B	�/B	�B	�IB	�B	� B	�B	�UB	�UB	��B	�B	��B	�UB	�B	�B	�CB	�/B	�B	�OB	�[B	�[B	�B	�B	�B	�}B	��B	�]B	�wB	�B	�B	�5B	�B	��B	��B	��B	�B	�aB	�aB	�B	�GB	�B	��B	�B	��B	�TB	�B	�nB	��B	��B	�`B	��B	��B	�B	��B	��B	�lB	��B	�rB	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	�cB	��B	��B	��B
 4B
 B
 �B
 �B
B
�B
;B
 B
 �B	��B	�}B	��B	��B	�B	�B	��B	�<B	�(B	��B	�B	�cB
 4B
UB
B
�B
�B
B
�B
�B
B
[B
�B
uB
�B
-B
�B
B
3B
gB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
?B
?B
EB
�B
�B
fB
�B
	�B

�B

�B

�B

�B

�B
B
JB
�B
�B
JB
B
�B
B
�B
�B
pB
�B
�B
BB
�B
HB
}B
 B
�B
:B
oB
�B
�B
�B
�B
B
NB
�B
�B
�B
�B
�B
�B
�B
�B
&B
B
B
@B
&B
�B
gB
MB
2B
�B
�B
MB
�B
�B
�B
2B
9B
mB
�B
�B
�B
QB
=B
qB
�B
�B
�B
/B
/B
dB
�B
B
�B
�B
�B
B
�B
�B
�B
�B
 vB
 \B
 �B
 �B
!HB
!|B
!bB
!|B
!|B
!|B
!�B
!�B
!�B
!�B
"B
"B
"4B
"�B
# B
#B
# B
#:B
#:B
#TB
#nB
#�B
$&B
$@B
$&B
$@B
$@B
$tB
$�B
$�B
%,B
%�B
%�B
%�B
&2B
&fB
'8B
'�B
'�B
(�B
(XB
(�B
(�B
)�B
)�B
)�B
*�B
*�B
+6B
+�B
+�B
,=B
,B
,�B
-�B
/iB
0oB
1'B
1�B
1�B
2�B
3hB
3�B
3�B
3�B
3�B
4B
4B
49B
4TB
4�B
5B
5?B
5tB
5�B
6B
5�B
6�B
7LB
8B
8RB
8RB
8lB
8�B
9XB
9XB
9�B
:B
:B
:B
9�B
9�B
:�B
:�B
;0B
;B
;B
;B
;dB
;JB
;dB
;�B
<PB
<�B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>B
>(B
>]B
>�B
?B
?.B
?cB
?}B
?�B
?�B
?�B
@ B
@iB
@�B
@�B
@�B
AUB
AUB
AoB
AoB
AoB
AoB
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
CB
D3B
DgB
DgB
D�B
D�B
D�B
E�B
EmB
E9B
ESB
F?B
F%B
F%B
F%B
G�B
G�B
G�B
HKB
H�B
IB
IRB
I�B
I�B
I�B
I�B
J#B
J#B
J	B
J	B
I�B
I�B
J�B
KDB
K)B
KxB
K�B
LB
LJB
LJB
L~B
L�B
L�B
L�B
L�B
L�B
MB
M6B
M6B
M�B
N"B
NpB
NpB
N�B
N�B
OB
OBB
O�B
O�B
P.B
PbB
P�B
QB
Q4B
QhB
Q�B
QhB
R:B
R�B
R�B
R�B
R�B
S@B
S[B
S[B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
TB
T,B
TaB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
VB
V�B
V�B
W
B
W$B
WsB
W�B
XB
XEB
X_B
XyB
XyB
X�B
X�B
Y1B
YeB
YB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZB
ZkB
Z�B
Z�B
[=B
[�B
]IB
\�B
\�B
]IB
]dB
^jB
^�B
^�B
_;B
_;B
_�B
`B
`�B
`�B
`�B
aHB
a�B
a�B
bB
b4B
bhB
b�B
cnB
c�B
dZB
dZB
dtB
dtB
dtB
d�B
d�B
d�B
d�B
eFB
eFB
ezB
e`B
ezB
ezB
fB
f2B
f2B
ffB
f�B
f�B
gRB
gRB
g�B
gRB
h
B
h>B
hXB
hXB
hsB
hsB
h�B
h�B
i*B
i*B
i_B
iyB
i�B
i�B
jeB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
l=B
l=B
l=B
l=B
lWB
l�B
m)B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
nB
ncB
n}B
n�B
n�B
n�B
o B
o�B
o�B
pB
p!B
p;B
poB
qB
qB
qAB
qAB
q[B
q[B
qvB
q�B
q�B
rB
rGB
rGB
raB
r�B
r�B
r�B
r�B
sB
s3B
sMB
sMB
s�B
s�B
s�B
s�B
t9B
tB
t9B
tTB
tTB
tTB
tTB
tnB
t�B
t�B
t�B
t�B
u?B
uZB
u�B
u�B
u�B
u�B
u�B
vB
u�B
v`B
vzB
v�B
v�B
v�B
wB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
yXB
yrB
y�B
zB
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{dB
{B
{�B
{�B
{�B
|B
|6B
|�B
}B
}B
}VB
}<B
}<B
}�B
}�B
}�B
~BB
~wB
~�B
cB
HB
HB
cB
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
�UB
��B
��B
��B
��B
��B
�AB
�AB
�uB
��B
��B
�aB
�aB
��B
�{B
��B
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104842  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172251  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172251  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172251                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022258  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022258  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610121506                      G�O�G�O�G�O�                