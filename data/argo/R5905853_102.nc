CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:40:58Z creation;2022-06-04T17:40:59Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174058  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               fA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @٤����1   @٤��i@/Ƨ�c^��vȴ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�33@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BBffBG��BP��BX  B`  Bg33Bp  BxffB��B�  B�  B�  B�33B���B�  B���B�  B�  B�  B�33B���B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�ffB���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CBffCC��CE�fCH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�@��HA
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7BB(�BG\)BP�\BWB_Bf��BoBx(�B\)B��HB��HB��HB�{B��B��HB��B��HB��HB��HB�{B��B�{B��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB�{B�G�B�B��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C
>C
>C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C@
>CBW
CC�qCE�
CG�CI�CK�CM�CO�CQ�CS�CU�
CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC�C��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#��D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D)�D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS��DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds��Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�AGD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��GD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�g�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�=A��A�+�A�-CA�.A�/�A�0UA�1'A�1�A�2�A�3�A�4A�5?A�6zA�4�A�4nA�6�A�8�A�9�A�:�A�;�A��RA�OAѯ�A�a�A��A��|A��tA��A��4A��A���A�zA���A��XA�dZA�y>A��$A�=A��iA��A�:^A�.}A�%FA�-A�ںA�&�A�)�A�HKA��A���A���A�MjA���A��A���A�d�A��A���A��A�(XA��ZA�I�A���A�|�A} iAt��Aqf�AkAi�Ai(Ah��AgW?Ac�Ab��Ab�Aau�A`��A`}VA^��A\��AYL0ASVAO�AMrGAL��AKAJ�[AG��AD��AC�AB�A@	lA?�A<w�A:ϫA8�<A6�XA5`�A4��A43�A1�A/�3A/�A.�@A.�A.�A.یA.��A.�/A.��A.t�A-��A-QA,�$A,.�A+�rA+�A+PHA*A*��A*A)��A)6A)�A(�|A(��A(��A(U�A(�A'�\A&�eA%�A$�DA$<6A#��A#A"�A"��A!�NA ��A�AN�A(AYAxA@A��A�KAA�*AMjA�@AO�A;�Ag8A��AxlAy>AXAGA�sA�
ARTA
=Aj�A��A��A��A��A�A�A�*APHAf�AOA�AS�AA��A�]A&�A�8A��A��A��A��Ac�A8�Aa|A
��A	��A�A�VAS&A��A�.A.IA;�Aq�A�A=A,=A��Au�A ��A <�@��@��e@��D@��{@��@�a�@��/@���@��@��D@��@�Y@�s�@�C-@�+k@�#:@��@�˒@�^�@�o@�>B@�S@��@�@�g8@�;�@�:*@���@��)@���@�X@��f@�1@�@�ی@�v`@�!�@���@�@�@���@���@���@��@�6@�}�@�;@�6@�F�@ޤ�@�
�@�u�@ܹ�@�w�@��@ڭ�@�?}@،@��6@֚@�YK@Չ7@Գh@�V@�'R@ӕ�@�?}@�$t@�(@��@�6�@���@�ƨ@�>�@з�@�
�@�u�@�_p@�;d@� i@΍�@�h�@�J�@�;�@��Z@��@�v�@�PH@�_@�Y�@�֡@�͟@ʹ�@ʝI@�YK@���@�x@�E9@��P@ȯO@ȏ\@ȝI@�5?@��o@ǐ�@�5�@�S@���@�M�@Ů�@��@ľ@���@�p�@�Ta@���@���@�X�@��p@�bN@�ԕ@��@���@��)@�^5@�1'@��'@��"@���@��@��@��h@�v`@�s�@��U@���@��K@�:*@���@�O@��r@��g@��@�"h@��@�Mj@��@��p@��9@��e@��z@�$�@��@���@�]�@���@�7�@��A@�*0@��@�C@���@��[@���@���@�S�@��I@�oi@�.�@��+@���@�^�@�"�@��@��L@�|�@�!@��*@�_p@���@��I@�PH@��@�|@���@�d�@�N�@�$�@�	@��@��F@�O@��M@��@�bN@�2�@��@���@�f�@�!-@��@��_@�xl@�'R@���@�rG@�\)@�0�@��c@��I@�I�@�u@���@�"�@���@�H@��@���@�o @�@��B@���@�p;@�1@��@�\�@�@��+@���@��^@���@�9�@��@���@�R�@��@���@�t�@�5�@��"@��X@���@�V�@��@��m@��j@���@��:@�^�@�-w@���@��2@��B@��9@��I@�h
@��@�s�@���@��@�t�@�c@���@�+@���@���@�s�@�j@�g8@��6@�/@�%@��@��_@�;�@�@��3@��V@���@���@�iD@�4�@�S&@�\)@�+@�ȴ@�[�@�L0@�-�@��&@�\�@��5@��@�7@���@���@���@�u�@�T�@� i@���@�#:@��D@��N@���@�/�@� i@���@�bN@�$�@�	�@�˒@��P@��@���@���@�_�@�2�@� �@��&@���@�E9@��9@�<�@��]@��@���@�x�@��@��@��u@���@�c�@�6�@���@��@�L�@��@��m@���@�bN@�&�@��@~��@~	@}+@|�9@|z�@|U2@|�@{��@{W?@{!-@z�H@z�<@z�@z�@y��@y��@y��@y��@y�~@y0�@x�@xoi@x:�@w�a@wY@vߤ@v�'@v��@v�'@v�@u��@u�X@u��@uB�@t�j@t��@ty>@t7�@s�Q@sH�@s i@r�]@r�1@r_�@q��@q��@q0�@q;@p��@pe�@p�@o|�@o�@n�@n�H@n͟@n�R@n@�@m�@m��@m[W@l�I@l@k�q@kW?@j�@jTa@i�@i�7@i	l@h��@h4n@g@fO@e�D@eA @d֡@d��@dc�@c��@c�k@cP�@c4�@c�@bGE@b�@a�@a��@a�S@ak�@a5�@`�@`��@`D�@_�+@_��@_y�@_&@^�m@^u%@^{@^�@]ԕ@]m]@]+�@\�@\m�@\H@\�@[�
@[��@Z�2@Zl�@Z �@Y�^@Y%F@Xm�@W�@Wo@W@V�@V�@VYK@U�9@U��@UrG@U<6@U&�@T��@S�;@S� @S˒@S��@S��@S��@S�@R�6@R{�@RM�@RH�@R$�@Q��@P�P@Poi@P@O�@OC@N҉@N��@Ni�@N.�@M��@M�t@M��@M|@M�@L��@L!@K��@K��@J��@J8�@I�=@I5�@H�`@H��@G�@G��@G)_@G
=@F�8@F��@F@Eԕ@Ec@Dی@D��@Dy>@D_@D�@Cƨ@C�{@B�<@B:*@A��@AY�@@�9@@Ft@@,=@@7@?�@?��@?_p@?,�@>�@>�@=�@=u�@=`B@=:�@=�@<Z@;�Q@;qv@;�@:$�@9�z@9��@9�7@9T�@9�@8��@8�[@8�@8c�@8�@7��@6�s@6��@6�<@6��@5�o@5��@5�"@5=�@5+�@5@4��@4Z@4,=@3�@3��@3��@3y�@3qv@3_p@3;d@2�y@2��@2�@2{�@2-@1��@1Dg@1	l@0�[@0��@0��@0:�@0'R@0!@07@0�@0@/�+@/~�@/33@/C@/ i@.��@.��@.c @.C�@-�@-k�@-Q�@-V@,��@,y>@,b@,@,�@+��@+��@+�@+�@+RT@*�B@*��@*n�@*a|@*?@*!�@*	@)��@)��@)�@)a�@)0�@(�@(��@(7�@(�@'��@'�0@'��@'F�@'�@&�@&�r@&E�@%�d@%�7@%��@%|@%/@$��@$�@$:�@#خ@#��@#j�@#=@#�@"��@"�@"ff@"e@!�3@!s�@!2a@ �E@ `�@ A�@ (�@��@|�@S�@�c@�m@�1@O@�@�@s�@[W@#�@�E@�$@��@w�@Xy@_@~@��@~�@U�@�@�c@��@��@Ov@��@rG@^�@(�@�/@e�@*�@��@A�@�2@� @ff@3�@�Z@�@��@��@#�@�@��@�j@�4@c�@A�@�@��@o�@\)@J#@$t@��@R�@-@@��@m]@F@ \@�@��@��@�p@Ĝ@�@��@<�@G@�m@��@��@�P@j�@+@�@��@��@8�@�@u@�@��@�H@�-@��@��@p�@X@�@��@�/@�E@�U@�o@C-@M@�@�Q@�@@�@Z�@�@@
��@
ں@
�'@
z@
H�@
O@	��@	�Z@	��@	ϫ@	��@	�=@	�S@	��@	rG@	o @	^�@	8�@��@�?@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�=A��A�+�A�-CA�.A�/�A�0UA�1'A�1�A�2�A�3�A�4A�5?A�6zA�4�A�4nA�6�A�8�A�9�A�:�A�;�A��RA�OAѯ�A�a�A��A��|A��tA��A��4A��A���A�zA���A��XA�dZA�y>A��$A�=A��iA��A�:^A�.}A�%FA�-A�ںA�&�A�)�A�HKA��A���A���A�MjA���A��A���A�d�A��A���A��A�(XA��ZA�I�A���A�|�A} iAt��Aqf�AkAi�Ai(Ah��AgW?Ac�Ab��Ab�Aau�A`��A`}VA^��A\��AYL0ASVAO�AMrGAL��AKAJ�[AG��AD��AC�AB�A@	lA?�A<w�A:ϫA8�<A6�XA5`�A4��A43�A1�A/�3A/�A.�@A.�A.�A.یA.��A.�/A.��A.t�A-��A-QA,�$A,.�A+�rA+�A+PHA*A*��A*A)��A)6A)�A(�|A(��A(��A(U�A(�A'�\A&�eA%�A$�DA$<6A#��A#A"�A"��A!�NA ��A�AN�A(AYAxA@A��A�KAA�*AMjA�@AO�A;�Ag8A��AxlAy>AXAGA�sA�
ARTA
=Aj�A��A��A��A��A�A�A�*APHAf�AOA�AS�AA��A�]A&�A�8A��A��A��A��Ac�A8�Aa|A
��A	��A�A�VAS&A��A�.A.IA;�Aq�A�A=A,=A��Au�A ��A <�@��@��e@��D@��{@��@�a�@��/@���@��@��D@��@�Y@�s�@�C-@�+k@�#:@��@�˒@�^�@�o@�>B@�S@��@�@�g8@�;�@�:*@���@��)@���@�X@��f@�1@�@�ی@�v`@�!�@���@�@�@���@���@���@��@�6@�}�@�;@�6@�F�@ޤ�@�
�@�u�@ܹ�@�w�@��@ڭ�@�?}@،@��6@֚@�YK@Չ7@Գh@�V@�'R@ӕ�@�?}@�$t@�(@��@�6�@���@�ƨ@�>�@з�@�
�@�u�@�_p@�;d@� i@΍�@�h�@�J�@�;�@��Z@��@�v�@�PH@�_@�Y�@�֡@�͟@ʹ�@ʝI@�YK@���@�x@�E9@��P@ȯO@ȏ\@ȝI@�5?@��o@ǐ�@�5�@�S@���@�M�@Ů�@��@ľ@���@�p�@�Ta@���@���@�X�@��p@�bN@�ԕ@��@���@��)@�^5@�1'@��'@��"@���@��@��@��h@�v`@�s�@��U@���@��K@�:*@���@�O@��r@��g@��@�"h@��@�Mj@��@��p@��9@��e@��z@�$�@��@���@�]�@���@�7�@��A@�*0@��@�C@���@��[@���@���@�S�@��I@�oi@�.�@��+@���@�^�@�"�@��@��L@�|�@�!@��*@�_p@���@��I@�PH@��@�|@���@�d�@�N�@�$�@�	@��@��F@�O@��M@��@�bN@�2�@��@���@�f�@�!-@��@��_@�xl@�'R@���@�rG@�\)@�0�@��c@��I@�I�@�u@���@�"�@���@�H@��@���@�o @�@��B@���@�p;@�1@��@�\�@�@��+@���@��^@���@�9�@��@���@�R�@��@���@�t�@�5�@��"@��X@���@�V�@��@��m@��j@���@��:@�^�@�-w@���@��2@��B@��9@��I@�h
@��@�s�@���@��@�t�@�c@���@�+@���@���@�s�@�j@�g8@��6@�/@�%@��@��_@�;�@�@��3@��V@���@���@�iD@�4�@�S&@�\)@�+@�ȴ@�[�@�L0@�-�@��&@�\�@��5@��@�7@���@���@���@�u�@�T�@� i@���@�#:@��D@��N@���@�/�@� i@���@�bN@�$�@�	�@�˒@��P@��@���@���@�_�@�2�@� �@��&@���@�E9@��9@�<�@��]@��@���@�x�@��@��@��u@���@�c�@�6�@���@��@�L�@��@��m@���@�bN@�&�@��@~��@~	@}+@|�9@|z�@|U2@|�@{��@{W?@{!-@z�H@z�<@z�@z�@y��@y��@y��@y��@y�~@y0�@x�@xoi@x:�@w�a@wY@vߤ@v�'@v��@v�'@v�@u��@u�X@u��@uB�@t�j@t��@ty>@t7�@s�Q@sH�@s i@r�]@r�1@r_�@q��@q��@q0�@q;@p��@pe�@p�@o|�@o�@n�@n�H@n͟@n�R@n@�@m�@m��@m[W@l�I@l@k�q@kW?@j�@jTa@i�@i�7@i	l@h��@h4n@g@fO@e�D@eA @d֡@d��@dc�@c��@c�k@cP�@c4�@c�@bGE@b�@a�@a��@a�S@ak�@a5�@`�@`��@`D�@_�+@_��@_y�@_&@^�m@^u%@^{@^�@]ԕ@]m]@]+�@\�@\m�@\H@\�@[�
@[��@Z�2@Zl�@Z �@Y�^@Y%F@Xm�@W�@Wo@W@V�@V�@VYK@U�9@U��@UrG@U<6@U&�@T��@S�;@S� @S˒@S��@S��@S��@S�@R�6@R{�@RM�@RH�@R$�@Q��@P�P@Poi@P@O�@OC@N҉@N��@Ni�@N.�@M��@M�t@M��@M|@M�@L��@L!@K��@K��@J��@J8�@I�=@I5�@H�`@H��@G�@G��@G)_@G
=@F�8@F��@F@Eԕ@Ec@Dی@D��@Dy>@D_@D�@Cƨ@C�{@B�<@B:*@A��@AY�@@�9@@Ft@@,=@@7@?�@?��@?_p@?,�@>�@>�@=�@=u�@=`B@=:�@=�@<Z@;�Q@;qv@;�@:$�@9�z@9��@9�7@9T�@9�@8��@8�[@8�@8c�@8�@7��@6�s@6��@6�<@6��@5�o@5��@5�"@5=�@5+�@5@4��@4Z@4,=@3�@3��@3��@3y�@3qv@3_p@3;d@2�y@2��@2�@2{�@2-@1��@1Dg@1	l@0�[@0��@0��@0:�@0'R@0!@07@0�@0@/�+@/~�@/33@/C@/ i@.��@.��@.c @.C�@-�@-k�@-Q�@-V@,��@,y>@,b@,@,�@+��@+��@+�@+�@+RT@*�B@*��@*n�@*a|@*?@*!�@*	@)��@)��@)�@)a�@)0�@(�@(��@(7�@(�@'��@'�0@'��@'F�@'�@&�@&�r@&E�@%�d@%�7@%��@%|@%/@$��@$�@$:�@#خ@#��@#j�@#=@#�@"��@"�@"ff@"e@!�3@!s�@!2a@ �E@ `�@ A�@ (�@��@|�@S�@�c@�m@�1@O@�@�@s�@[W@#�@�E@�$@��@w�@Xy@_@~@��@~�@U�@�@�c@��@��@Ov@��@rG@^�@(�@�/@e�@*�@��@A�@�2@� @ff@3�@�Z@�@��@��@#�@�@��@�j@�4@c�@A�@�@��@o�@\)@J#@$t@��@R�@-@@��@m]@F@ \@�@��@��@�p@Ĝ@�@��@<�@G@�m@��@��@�P@j�@+@�@��@��@8�@�@u@�@��@�H@�-@��@��@p�@X@�@��@�/@�E@�U@�o@C-@M@�@�Q@�@@�@Z�@�@@
��@
ں@
�'@
z@
H�@
O@	��@	�Z@	��@	ϫ@	��@	�=@	�S@	��@	rG@	o @	^�@	8�@��@�?@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B>]B>�B>�B>]B>BB>]B>�B>wB>�B>�B>�B>�B>�B>�B?B>�B>�B>�B>�B>�B>�B?�Bd�B�=B	~]B	�B
 BB
�IBO�BQ�BYeBU�BR�BO�BO�BZ�B�kB��B>�B�B
�<B
��B BAB&fB'B'�B)DB�B�B	�B
�fB
�mB
��B
��B
�5B
��B
|�B
l�B
c�B
Z�B
\�B
>�B
3�B
$B
B	�BB	�9B	��B	�BB	�B	�\B	�~B	�KB	y�B	t�B	q�B	oOB	lWB	i�B	`�B	W$B	J�B	@�B	8RB	9�B	>wB	?}B	<�B	;�B	4�B	1�B	3�B	>wB	cnB	`�B	dtB	� B	��B	��B	�B	��B	�jB	��B	ðB	�lB	�nB	�B	�B	�B	�B	�B	�hB	�0B	�(B	��B
 �B
 �B
�B
�B
	lB
	7B
)B
6B
VB
(B
�B
.B
B
}B
bB
�B
B
�B
�B
�B
?B
{B
[B
oB	�HB	��B	�dB	�B	�B	՛B	��B	��B	ȚB	�B	ԕB	�B
"�B
!HB
%`B
pB
�B
(�B
.�B
0;B
1�B
0�B
-�B
)*B
'RB
#nB
B
�B	��B
�B
!B
#�B
*�B
'�B
'8B
$�B
$B
%FB
&�B
&2B
&fB
'�B
4�B
4�B
5�B
4B
.�B
($B
%`B
# B
�B
oB
	�B	�JB	��B	��B
�B
[B	�;B	��B	�WB	�B	��B	�zB	�ZB	��B	خB	��B	��B	ՁB	�aB	�B	�B	�(B	уB	��B	��B	ӏB	ӏB	�FB	��B	�mB	ּB	ּB	��B	�$B	�sB	ּB	�$B	�$B	�
B	��B	��B	�
B	��B	یB	ܒB	ڠB	�kB	��B	�KB	�sB	�B	�B	ԯB	��B	��B	յB	�B	�B	�SB	�B	�mB	�
B	ּB	��B	�EB	�eB	��B	ٚB	�B	�1B	��B	�eB	�1B	ٴB	��B	ٴB	��B	�QB	�B	�B	�kB	�kB	چB	�#B	یB	�=B	ںB	ڠB	ڠB	�#B	��B	��B	�B	ܒB	ݘB	�jB	�OB	ބB	�OB	��B	�VB	ޞB	�OB	�jB	ߤB	�pB	�VB	��B	�'B	�|B	��B	�B	��B	�|B	��B	��B	�&B	�tB	�@B	��B	�B	��B	�zB	�2B	�LB	�B	�B	�B	�B	��B	�B	�sB	��B	��B	�B	�B	�=B	��B	�IB	��B	�!B	�'B	�B	�B	�B	��B	��B	�lB	�dB	��B	��B	�B	��B	��B	��B	�]B	��B	��B	��B	�"B	�jB	��B	�B	��B	�B	�HB
 4B	��B	�.B	�]B	��B	�B	�qB	��B	�(B
 4B
�B
[B
�B
�B
UB
 B	��B	��B
 iB
 �B
 �B
�B
uB
-B
-B
�B
�B
�B
�B
GB
�B
�B
 B
 OB	�B	��B	��B	�.B	��B	�HB	�HB	��B	�cB	��B	��B	��B
  B	��B
 4B
 OB
 B
 �B
 B
oB
B
;B
oB
�B
[B
B
aB
B
�B
B
�B
B
%B
YB
�B
�B
�B
�B
B
zB
_B
zB
�B
fB
�B
	7B
	RB
	�B

XB

XB
	�B
	�B
	�B
	�B
	�B
	�B
	�B

=B

#B

�B

�B
)B
DB
�B
B
6B
�B
�B
�B
"B
�B
}B
�B
B
�B
TB
[B
�B
�B
sB
?B
�B
eB
	B
kB
B
B
7B
B
eB
KB
�B
B
�B
�B
qB
�B
pB
 vB
 �B
!�B
"4B
!�B
!�B
!�B
"NB
"B
!�B
!�B
!bB
!�B
"hB
"�B
"�B
#TB
#�B
$ZB
$�B
$�B
%,B
&2B
&fB
&�B
'mB
'�B
'�B
(>B
)DB
)yB
*0B
*eB
*�B
*�B
+QB
+kB
+�B
,�B
-�B
-�B
.B
.IB
.}B
.�B
/�B
0!B
0�B
0oB
0�B
0�B
0�B
0UB
0oB
0�B
1'B
1'B
2B
2aB
3MB
3�B
3�B
3�B
3�B
33B
3hB
4�B
5�B
6�B
6�B
6�B
6�B
5�B
6�B
6B
5�B
6zB
5�B
6B
6`B
6+B
5�B
6zB
6FB
7�B
7�B
7�B
6`B
6�B
7fB
6�B
6�B
6zB
7fB
7fB
6�B
6�B
6�B
6�B
7B
6�B
6�B
6`B
6FB
7LB
7�B
7�B
7LB
72B
7�B
7�B
8�B
88B
8�B
8RB
8B
8RB
9>B
9rB
9�B
;�B
;0B
<PB
<�B
=�B
=VB
=�B
<�B
<jB
<B
;�B
<�B
<�B
=<B
=<B
=�B
>]B
>(B
>�B
>�B
>�B
?�B
?�B
?cB
@�B
?�B
@�B
A�B
AoB
A;B
A�B
B'B
B�B
B�B
C�B
C�B
DB
DgB
D�B
D�B
D�B
DMB
D�B
D�B
D�B
ESB
E�B
F�B
F�B
F�B
F�B
GB
GB
G�B
G�B
H�B
H�B
J	B
I�B
I�B
JXB
I�B
I�B
K)B
K^B
J�B
J�B
K^B
LdB
K�B
K�B
LB
K�B
KxB
LJB
L�B
LdB
MB
M�B
L�B
L~B
M6B
M�B
N�B
O(B
O(B
O�B
PB
PHB
O�B
O�B
PbB
PHB
P}B
P.B
P�B
P�B
Q�B
Q�B
R B
RTB
RB
S@B
S�B
S�B
SuB
T�B
T�B
U2B
T�B
U2B
T�B
U�B
T�B
UMB
VmB
V�B
VSB
V�B
V�B
W
B
WsB
X+B
W�B
W�B
X_B
YB
YeB
YB
YB
Y�B
Y�B
ZB
ZB
ZkB
[	B
[	B
[�B
Z�B
[qB
Z�B
[�B
[#B
[WB
Z�B
\)B
[�B
\)B
\)B
[�B
[�B
\�B
\)B
\]B
\xB
\)B
\�B
\CB
\�B
\�B
\xB
\CB
\]B
\]B
\�B
]/B
]�B
]�B
]�B
]/B
]/B
]dB
]�B
]�B
]�B
^B
]�B
^jB
^B
^jB
^�B
^�B
_;B
_B
_VB
_�B
`B
_VB
_�B
`B
`BB
`\B
`'B
`'B
`B
`�B
aHB
`\B
`\B
aHB
aB
a|B
abB
a�B
bNB
a�B
b�B
b�B
b�B
c�B
cTB
cTB
c B
cB
cnB
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
ezB
ezB
e�B
f2B
f�B
fB
ffB
gRB
f�B
g�B
gRB
h
B
g�B
h
B
iB
iB
hsB
h�B
h�B
i�B
i�B
jKB
jB
j�B
j�B
k�B
j�B
k�B
k�B
lWB
l�B
mCB
l�B
mCB
m�B
n/B
nIB
m�B
n�B
n}B
n�B
o B
o�B
p!B
p�B
p�B
pB
o�B
pUB
p�B
p�B
p�B
q[B
q�B
q�B
q[B
q�B
q�B
q�B
rB
rB
raB
r�B
s3B
sMB
s�B
tB
tB
t�B
t�B
u?B
uZB
t�B
t�B
uZB
u%B
uZB
u�B
u%B
uZB
vB
v+B
v`B
wB
v�B
v�B
v�B
w�B
w�B
w�B
xlB
y	B
x�B
yXB
yXB
z^B
z*B
y�B
y�B
zDB
y�B
z*B
zB
z�B
{B
{�B
{�B
|6B
|�B
|�B
}�B
}qB
}�B
}�B
~B
~]B
~BB
~B
}�B
~�B
~�B
HB
~�B
~�B
B
�B
�B
HB
�B
� B
�4B
�B
�B
�iB
�iB
� B
��B
�B
��B
� B
��B
�oB
��B
�;B
��B
�AB
�GB
�{B
�{B
�B
��B
�aB
�GB
�MB
��B
�{B
�3B
�MB
��B
��B
��B
�B
��B
�B
�9B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B>]B>�B>�B>]B>BB>]B>�B>wB>�B>�B>�B>�B>�B>�B?B>�B>�B>�B>�B>�B>�B?�Bd�B�=B	~]B	�B
 BB
�IBO�BQ�BYeBU�BR�BO�BO�BZ�B�kB��B>�B�B
�<B
��B BAB&fB'B'�B)DB�B�B	�B
�fB
�mB
��B
��B
�5B
��B
|�B
l�B
c�B
Z�B
\�B
>�B
3�B
$B
B	�BB	�9B	��B	�BB	�B	�\B	�~B	�KB	y�B	t�B	q�B	oOB	lWB	i�B	`�B	W$B	J�B	@�B	8RB	9�B	>wB	?}B	<�B	;�B	4�B	1�B	3�B	>wB	cnB	`�B	dtB	� B	��B	��B	�B	��B	�jB	��B	ðB	�lB	�nB	�B	�B	�B	�B	�B	�hB	�0B	�(B	��B
 �B
 �B
�B
�B
	lB
	7B
)B
6B
VB
(B
�B
.B
B
}B
bB
�B
B
�B
�B
�B
?B
{B
[B
oB	�HB	��B	�dB	�B	�B	՛B	��B	��B	ȚB	�B	ԕB	�B
"�B
!HB
%`B
pB
�B
(�B
.�B
0;B
1�B
0�B
-�B
)*B
'RB
#nB
B
�B	��B
�B
!B
#�B
*�B
'�B
'8B
$�B
$B
%FB
&�B
&2B
&fB
'�B
4�B
4�B
5�B
4B
.�B
($B
%`B
# B
�B
oB
	�B	�JB	��B	��B
�B
[B	�;B	��B	�WB	�B	��B	�zB	�ZB	��B	خB	��B	��B	ՁB	�aB	�B	�B	�(B	уB	��B	��B	ӏB	ӏB	�FB	��B	�mB	ּB	ּB	��B	�$B	�sB	ּB	�$B	�$B	�
B	��B	��B	�
B	��B	یB	ܒB	ڠB	�kB	��B	�KB	�sB	�B	�B	ԯB	��B	��B	յB	�B	�B	�SB	�B	�mB	�
B	ּB	��B	�EB	�eB	��B	ٚB	�B	�1B	��B	�eB	�1B	ٴB	��B	ٴB	��B	�QB	�B	�B	�kB	�kB	چB	�#B	یB	�=B	ںB	ڠB	ڠB	�#B	��B	��B	�B	ܒB	ݘB	�jB	�OB	ބB	�OB	��B	�VB	ޞB	�OB	�jB	ߤB	�pB	�VB	��B	�'B	�|B	��B	�B	��B	�|B	��B	��B	�&B	�tB	�@B	��B	�B	��B	�zB	�2B	�LB	�B	�B	�B	�B	��B	�B	�sB	��B	��B	�B	�B	�=B	��B	�IB	��B	�!B	�'B	�B	�B	�B	��B	��B	�lB	�dB	��B	��B	�B	��B	��B	��B	�]B	��B	��B	��B	�"B	�jB	��B	�B	��B	�B	�HB
 4B	��B	�.B	�]B	��B	�B	�qB	��B	�(B
 4B
�B
[B
�B
�B
UB
 B	��B	��B
 iB
 �B
 �B
�B
uB
-B
-B
�B
�B
�B
�B
GB
�B
�B
 B
 OB	�B	��B	��B	�.B	��B	�HB	�HB	��B	�cB	��B	��B	��B
  B	��B
 4B
 OB
 B
 �B
 B
oB
B
;B
oB
�B
[B
B
aB
B
�B
B
�B
B
%B
YB
�B
�B
�B
�B
B
zB
_B
zB
�B
fB
�B
	7B
	RB
	�B

XB

XB
	�B
	�B
	�B
	�B
	�B
	�B
	�B

=B

#B

�B

�B
)B
DB
�B
B
6B
�B
�B
�B
"B
�B
}B
�B
B
�B
TB
[B
�B
�B
sB
?B
�B
eB
	B
kB
B
B
7B
B
eB
KB
�B
B
�B
�B
qB
�B
pB
 vB
 �B
!�B
"4B
!�B
!�B
!�B
"NB
"B
!�B
!�B
!bB
!�B
"hB
"�B
"�B
#TB
#�B
$ZB
$�B
$�B
%,B
&2B
&fB
&�B
'mB
'�B
'�B
(>B
)DB
)yB
*0B
*eB
*�B
*�B
+QB
+kB
+�B
,�B
-�B
-�B
.B
.IB
.}B
.�B
/�B
0!B
0�B
0oB
0�B
0�B
0�B
0UB
0oB
0�B
1'B
1'B
2B
2aB
3MB
3�B
3�B
3�B
3�B
33B
3hB
4�B
5�B
6�B
6�B
6�B
6�B
5�B
6�B
6B
5�B
6zB
5�B
6B
6`B
6+B
5�B
6zB
6FB
7�B
7�B
7�B
6`B
6�B
7fB
6�B
6�B
6zB
7fB
7fB
6�B
6�B
6�B
6�B
7B
6�B
6�B
6`B
6FB
7LB
7�B
7�B
7LB
72B
7�B
7�B
8�B
88B
8�B
8RB
8B
8RB
9>B
9rB
9�B
;�B
;0B
<PB
<�B
=�B
=VB
=�B
<�B
<jB
<B
;�B
<�B
<�B
=<B
=<B
=�B
>]B
>(B
>�B
>�B
>�B
?�B
?�B
?cB
@�B
?�B
@�B
A�B
AoB
A;B
A�B
B'B
B�B
B�B
C�B
C�B
DB
DgB
D�B
D�B
D�B
DMB
D�B
D�B
D�B
ESB
E�B
F�B
F�B
F�B
F�B
GB
GB
G�B
G�B
H�B
H�B
J	B
I�B
I�B
JXB
I�B
I�B
K)B
K^B
J�B
J�B
K^B
LdB
K�B
K�B
LB
K�B
KxB
LJB
L�B
LdB
MB
M�B
L�B
L~B
M6B
M�B
N�B
O(B
O(B
O�B
PB
PHB
O�B
O�B
PbB
PHB
P}B
P.B
P�B
P�B
Q�B
Q�B
R B
RTB
RB
S@B
S�B
S�B
SuB
T�B
T�B
U2B
T�B
U2B
T�B
U�B
T�B
UMB
VmB
V�B
VSB
V�B
V�B
W
B
WsB
X+B
W�B
W�B
X_B
YB
YeB
YB
YB
Y�B
Y�B
ZB
ZB
ZkB
[	B
[	B
[�B
Z�B
[qB
Z�B
[�B
[#B
[WB
Z�B
\)B
[�B
\)B
\)B
[�B
[�B
\�B
\)B
\]B
\xB
\)B
\�B
\CB
\�B
\�B
\xB
\CB
\]B
\]B
\�B
]/B
]�B
]�B
]�B
]/B
]/B
]dB
]�B
]�B
]�B
^B
]�B
^jB
^B
^jB
^�B
^�B
_;B
_B
_VB
_�B
`B
_VB
_�B
`B
`BB
`\B
`'B
`'B
`B
`�B
aHB
`\B
`\B
aHB
aB
a|B
abB
a�B
bNB
a�B
b�B
b�B
b�B
c�B
cTB
cTB
c B
cB
cnB
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
ezB
ezB
e�B
f2B
f�B
fB
ffB
gRB
f�B
g�B
gRB
h
B
g�B
h
B
iB
iB
hsB
h�B
h�B
i�B
i�B
jKB
jB
j�B
j�B
k�B
j�B
k�B
k�B
lWB
l�B
mCB
l�B
mCB
m�B
n/B
nIB
m�B
n�B
n}B
n�B
o B
o�B
p!B
p�B
p�B
pB
o�B
pUB
p�B
p�B
p�B
q[B
q�B
q�B
q[B
q�B
q�B
q�B
rB
rB
raB
r�B
s3B
sMB
s�B
tB
tB
t�B
t�B
u?B
uZB
t�B
t�B
uZB
u%B
uZB
u�B
u%B
uZB
vB
v+B
v`B
wB
v�B
v�B
v�B
w�B
w�B
w�B
xlB
y	B
x�B
yXB
yXB
z^B
z*B
y�B
y�B
zDB
y�B
z*B
zB
z�B
{B
{�B
{�B
|6B
|�B
|�B
}�B
}qB
}�B
}�B
~B
~]B
~BB
~B
}�B
~�B
~�B
HB
~�B
~�B
B
�B
�B
HB
�B
� B
�4B
�B
�B
�iB
�iB
� B
��B
�B
��B
� B
��B
�oB
��B
�;B
��B
�AB
�GB
�{B
�{B
�B
��B
�aB
�GB
�MB
��B
�{B
�3B
�MB
��B
��B
��B
�B
��B
�B
�9B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104926  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174058  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174059  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174059                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024107  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024107  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                