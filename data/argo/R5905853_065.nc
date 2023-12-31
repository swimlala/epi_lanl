CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:34:14Z creation;2022-06-04T17:34:14Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173414  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               AA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�G����1   @�G��˩�@-�n��O��c
�t�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A��A   A@  A`  A�  A�  A���A�  A�  A�  A�  A���B��BffB  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB��B���B���B�  B�  B�  B�  B�  B�  B�ffB�ffB�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$�C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP33CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @(��@|(�@�{A ��A
=A?
=A_
=A
=A��A�Q�A��A��AυA߅A�Q�B\)B(�BBBB'\)B/B7B?BGBOBWB_BgBoBx(�B\)B��B��B��HB��HB��HB��HB��HB��HB�G�B�G�B��HB��HB��HB��HB��HB��HBîB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C"
>C$
>C%�
C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CP#�CQ�
CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��C��C��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��C��RC�C��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/��D0u�D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DO�DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx��Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�GD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D�GD�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D��D��D�>D�~D�D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A׊�Aכ�AלAמAן�Aמ�Aמ�Aס-Aנ�Aכ�AלCAל�Aם�Aי�A׃A�=A�VA�9�A�уA�&A��0A�5tA���Aυ�A���A�gmA�tTA�;0A�w�A�O�AįA��VA�oA��VA���A��A���A�M�A���A�h>A�AUA��XA�[�A���A�2�A��A�hA�QA�l�A���A�
�A�SA�HA�aA���A��A�	�A���A���A��"A��A�	7A�zDA�?A��A��A��A��A��A�~A�I�A�D3A�&LA���A�9�A�)�A��A��gA���A��A��:A���A��AyϫAw.�Av1As��AqqAm� Ak)_Af��Aa��A]�kAZAV�FAR��AO2�AM�4AL1'AJ	AHxAF@�AC��A?h
A>6�A=��A=�A<�0A=L0A>�SA@t�AA�DAA�SAA!-A>�
A<YA<1'A<}�A:҉A7VmA3�0A3�mA/�zA,VmA)O�A)J�A)��A)��A(�A&+A%*0A$J�A#ĜA#!�A"��A!��A!%�A �mA 	A�A��AIRA֡A?A��A��A�A�A�}A�APHA��A�A[�Am]A{JA��A�A�}A��AoiA�5AA_A_AVA�A�[A��AJ#A�AAZ�A�cA�bA7�A�AoiA
��A
33A	��A� A�BAĜA<�A�cA�uAv�A�AsA�$Af�AD�A<6Ak�A7A��A4nA��A خA g�A H�@��X@�@�?�@���@��|@�+k@�[W@��B@���@���@��&@�:�@�C@�L0@��@�P�@��K@��I@��@��@�Ov@��D@�W?@�S@�GE@�~@��q@�H@�e,@�@�P�@�Ɇ@�=q@�7�@駇@���@�:�@��]@碜@��@��@�w2@�H@��@�$@��|@�+k@�N<@⻙@�u@�~(@�Ft@���@�(@���@�!�@�z�@�$�@ݲ-@��@��@�e,@��@ڳh@�B[@�˒@�e,@��@ر�@�  @׊	@��|@�֡@־�@ռ@��@�A�@ӥ@���@Қ@�ff@�5?@���@ъ	@ќ@�?}@о@�R�@�@ϝ�@�4�@ά�@�	�@�<6@�Y@��@���@��B@̡b@��@˚k@�K�@�9�@��@���@���@ʧ�@�^5@Ɏ�@��@Ȓ�@�%�@�x�@�%@�ѷ@Ƥ�@Ƅ�@ſH@�O@��@��@��@Ĭ�@�(�@��&@�{J@�@�}V@�&�@�ƨ@�W?@���@���@�c�@�5?@���@��C@�|�@�rG@�6z@��@��5@��h@�H@��@��@���@�B�@��/@�C-@��@���@�\�@�(�@��?@���@�o @���@��@�Z�@��K@��@���@��@�\�@�#:@��@�s@�:�@��v@�~(@�>B@�ݘ@��0@���@�P�@�'�@��@�y>@�'R@��^@�x�@�6z@��$@�a|@�@���@�Mj@��M@�Z�@�/�@��9@�U�@��@�a|@��@���@���@�_p@�@���@�h
@��K@��@��p@�~�@�Q�@��@��@�v`@�O�@���@���@�-@���@���@�:*@��@��&@���@�P�@���@�5?@���@���@�V@���@��\@�,=@���@��@�&�@��B@�V@�/�@���@��$@��f@�u�@��@��@�$t@�R�@�5?@�x@��@@���@��@�|�@�qv@�-w@���@��4@�l"@��@���@�F@�@@�S@��@�h�@�)�@��@�u@���@�A @�'�@��"@�͟@�ff@���@��j@��t@�qv@�;d@��E@���@�D�@��@��@���@�@���@�S�@�M�@�5?@�G@��A@��T@���@�o @�$t@���@�kQ@�:�@��T@���@�O�@��@��'@�~�@�@��@��#@��P@�=�@�V@��H@���@��x@�1'@���@��)@�$@��Q@���@�t�@�Mj@�>�@�5�@�)_@��@���@�}V@�$�@��]@��@�S�@�0�@� \@��@��/@��I@�M@�*�@�  @���@�a�@�!�@�Ĝ@�a|@��6@���@��@���@���@�L�@��@��@���@�[�@�7�@���@�@���@�@O@�!-@��@��c@���@�p;@�=q@�$�@��@��@�K@Z�@�@~z@~�@}��@}��@}V@|ی@|�v@|�[@|c�@|2�@|@{�
@{n/@z��@z_�@zO@y�N@yL�@x�@x��@x]d@x7@w��@w�$@wj�@w=@v�X@vB[@u��@u��@us�@u^�@u+�@u�@u%@t�e@t`�@t*�@s�@s��@r�8@r��@r^5@r#:@q�^@q`B@q�@p��@p�@oS�@o@O@o�@n5?@m�~@mo @l��@l-�@k��@kE9@k�@k�@j�c@j=q@i�-@i�@h��@h!@g�@gU�@g@f��@f��@fOv@e��@e*0@d��@d  @cb�@b��@b��@b8�@a�@a2a@`�@`�e@`tT@`Xy@`�@_��@_�6@_�q@_X�@^��@^L0@]�#@]��@]?}@\��@\��@\M@[��@[iD@[�@Z�r@Yԕ@Y�@X�4@XS�@W�W@W6z@V�2@V�F@VJ�@V?@U��@Uzx@U0�@UV@T��@T�o@T$@S�*@SU�@R�H@R�+@R
�@Q��@QrG@P�P@Ph�@O��@O=@O i@Nߤ@N��@N�x@N-@M��@M�7@Mj@L��@LM@K��@K��@K+@J�}@JOv@JJ@I��@Ihs@H��@H1'@G��@Gqv@G,�@F��@F��@F_�@F&�@E�o@E�N@E��@EIR@D�`@D�U@D��@DA�@C�m@C�@Ce�@C@O@B��@B�]@B��@BQ@B�@A��@A�~@AO�@A \@@�K@@��@@�@?�@?� @?��@?,�@?�@>�y@>�1@>^5@>?@>�@=�@=m]@=#�@<�@<(�@;x@;@:�@:��@:8�@:	@9��@9�@9��@9��@9��@9c@9Y�@9%F@8�@8�@8r�@8A�@8�@7��@7l�@7)_@6�@65?@5�@5 \@4�_@4l"@3��@3��@3~�@3g�@2�M@2�}@2�1@26�@1f�@1�@1�@0��@0�p@0l"@0%�@/�q@/�{@/g�@/E9@.�@.ߤ@.��@.c @.	@-�@-�9@-��@-J�@-�@,��@,��@,:�@+�k@+@O@+'�@*�c@*�b@*��@*{�@*n�@*�@)��@)�S@)!�@(�/@(�z@(c�@(<�@(!@'�@'�@'_p@'�@&��@&�@&^5@&�@%��@%�@%X@$�	@$��@$�_@$z�@$H@$@#��@#|�@#o�@#)_@"�@"��@"Z�@!�@!��@!��@!�'@!N<@!%@ ��@ M@�@�[@�k@C�@��@�\@Q@e@�#@�-@��@e,@%F@��@�p@�@u�@C-@@�w@�k@e�@+@ i@ȴ@� @n�@i�@^5@_@��@�t@�X@��@rG@Y�@*0@@%@�	@Ɇ@]d@%�@b@˒@�@v`@33@@��@�@}V@ff@\�@\�@Ta@3�@ �@��@�@�=@f�@�@�v@��@�D@bN@V�@@�q@x@K�@�"@��@�@�@kQ@C�@J@��@m]@?}@/@�@�@�@��@U2@1'@�@�@ݘ@�g@��@�w@��@��@��@x@_p@�"@�@;�@.�@�@ �@�j@@��@�M@G�@-w@-w@�@�@��@��@g8@�@�W@��@��@o�@O@1�@
�M@
�@
�A@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A׊�Aכ�AלAמAן�Aמ�Aמ�Aס-Aנ�Aכ�AלCAל�Aם�Aי�A׃A�=A�VA�9�A�уA�&A��0A�5tA���Aυ�A���A�gmA�tTA�;0A�w�A�O�AįA��VA�oA��VA���A��A���A�M�A���A�h>A�AUA��XA�[�A���A�2�A��A�hA�QA�l�A���A�
�A�SA�HA�aA���A��A�	�A���A���A��"A��A�	7A�zDA�?A��A��A��A��A��A�~A�I�A�D3A�&LA���A�9�A�)�A��A��gA���A��A��:A���A��AyϫAw.�Av1As��AqqAm� Ak)_Af��Aa��A]�kAZAV�FAR��AO2�AM�4AL1'AJ	AHxAF@�AC��A?h
A>6�A=��A=�A<�0A=L0A>�SA@t�AA�DAA�SAA!-A>�
A<YA<1'A<}�A:҉A7VmA3�0A3�mA/�zA,VmA)O�A)J�A)��A)��A(�A&+A%*0A$J�A#ĜA#!�A"��A!��A!%�A �mA 	A�A��AIRA֡A?A��A��A�A�A�}A�APHA��A�A[�Am]A{JA��A�A�}A��AoiA�5AA_A_AVA�A�[A��AJ#A�AAZ�A�cA�bA7�A�AoiA
��A
33A	��A� A�BAĜA<�A�cA�uAv�A�AsA�$Af�AD�A<6Ak�A7A��A4nA��A خA g�A H�@��X@�@�?�@���@��|@�+k@�[W@��B@���@���@��&@�:�@�C@�L0@��@�P�@��K@��I@��@��@�Ov@��D@�W?@�S@�GE@�~@��q@�H@�e,@�@�P�@�Ɇ@�=q@�7�@駇@���@�:�@��]@碜@��@��@�w2@�H@��@�$@��|@�+k@�N<@⻙@�u@�~(@�Ft@���@�(@���@�!�@�z�@�$�@ݲ-@��@��@�e,@��@ڳh@�B[@�˒@�e,@��@ر�@�  @׊	@��|@�֡@־�@ռ@��@�A�@ӥ@���@Қ@�ff@�5?@���@ъ	@ќ@�?}@о@�R�@�@ϝ�@�4�@ά�@�	�@�<6@�Y@��@���@��B@̡b@��@˚k@�K�@�9�@��@���@���@ʧ�@�^5@Ɏ�@��@Ȓ�@�%�@�x�@�%@�ѷ@Ƥ�@Ƅ�@ſH@�O@��@��@��@Ĭ�@�(�@��&@�{J@�@�}V@�&�@�ƨ@�W?@���@���@�c�@�5?@���@��C@�|�@�rG@�6z@��@��5@��h@�H@��@��@���@�B�@��/@�C-@��@���@�\�@�(�@��?@���@�o @���@��@�Z�@��K@��@���@��@�\�@�#:@��@�s@�:�@��v@�~(@�>B@�ݘ@��0@���@�P�@�'�@��@�y>@�'R@��^@�x�@�6z@��$@�a|@�@���@�Mj@��M@�Z�@�/�@��9@�U�@��@�a|@��@���@���@�_p@�@���@�h
@��K@��@��p@�~�@�Q�@��@��@�v`@�O�@���@���@�-@���@���@�:*@��@��&@���@�P�@���@�5?@���@���@�V@���@��\@�,=@���@��@�&�@��B@�V@�/�@���@��$@��f@�u�@��@��@�$t@�R�@�5?@�x@��@@���@��@�|�@�qv@�-w@���@��4@�l"@��@���@�F@�@@�S@��@�h�@�)�@��@�u@���@�A @�'�@��"@�͟@�ff@���@��j@��t@�qv@�;d@��E@���@�D�@��@��@���@�@���@�S�@�M�@�5?@�G@��A@��T@���@�o @�$t@���@�kQ@�:�@��T@���@�O�@��@��'@�~�@�@��@��#@��P@�=�@�V@��H@���@��x@�1'@���@��)@�$@��Q@���@�t�@�Mj@�>�@�5�@�)_@��@���@�}V@�$�@��]@��@�S�@�0�@� \@��@��/@��I@�M@�*�@�  @���@�a�@�!�@�Ĝ@�a|@��6@���@��@���@���@�L�@��@��@���@�[�@�7�@���@�@���@�@O@�!-@��@��c@���@�p;@�=q@�$�@��@��@�K@Z�@�@~z@~�@}��@}��@}V@|ی@|�v@|�[@|c�@|2�@|@{�
@{n/@z��@z_�@zO@y�N@yL�@x�@x��@x]d@x7@w��@w�$@wj�@w=@v�X@vB[@u��@u��@us�@u^�@u+�@u�@u%@t�e@t`�@t*�@s�@s��@r�8@r��@r^5@r#:@q�^@q`B@q�@p��@p�@oS�@o@O@o�@n5?@m�~@mo @l��@l-�@k��@kE9@k�@k�@j�c@j=q@i�-@i�@h��@h!@g�@gU�@g@f��@f��@fOv@e��@e*0@d��@d  @cb�@b��@b��@b8�@a�@a2a@`�@`�e@`tT@`Xy@`�@_��@_�6@_�q@_X�@^��@^L0@]�#@]��@]?}@\��@\��@\M@[��@[iD@[�@Z�r@Yԕ@Y�@X�4@XS�@W�W@W6z@V�2@V�F@VJ�@V?@U��@Uzx@U0�@UV@T��@T�o@T$@S�*@SU�@R�H@R�+@R
�@Q��@QrG@P�P@Ph�@O��@O=@O i@Nߤ@N��@N�x@N-@M��@M�7@Mj@L��@LM@K��@K��@K+@J�}@JOv@JJ@I��@Ihs@H��@H1'@G��@Gqv@G,�@F��@F��@F_�@F&�@E�o@E�N@E��@EIR@D�`@D�U@D��@DA�@C�m@C�@Ce�@C@O@B��@B�]@B��@BQ@B�@A��@A�~@AO�@A \@@�K@@��@@�@?�@?� @?��@?,�@?�@>�y@>�1@>^5@>?@>�@=�@=m]@=#�@<�@<(�@;x@;@:�@:��@:8�@:	@9��@9�@9��@9��@9��@9c@9Y�@9%F@8�@8�@8r�@8A�@8�@7��@7l�@7)_@6�@65?@5�@5 \@4�_@4l"@3��@3��@3~�@3g�@2�M@2�}@2�1@26�@1f�@1�@1�@0��@0�p@0l"@0%�@/�q@/�{@/g�@/E9@.�@.ߤ@.��@.c @.	@-�@-�9@-��@-J�@-�@,��@,��@,:�@+�k@+@O@+'�@*�c@*�b@*��@*{�@*n�@*�@)��@)�S@)!�@(�/@(�z@(c�@(<�@(!@'�@'�@'_p@'�@&��@&�@&^5@&�@%��@%�@%X@$�	@$��@$�_@$z�@$H@$@#��@#|�@#o�@#)_@"�@"��@"Z�@!�@!��@!��@!�'@!N<@!%@ ��@ M@�@�[@�k@C�@��@�\@Q@e@�#@�-@��@e,@%F@��@�p@�@u�@C-@@�w@�k@e�@+@ i@ȴ@� @n�@i�@^5@_@��@�t@�X@��@rG@Y�@*0@@%@�	@Ɇ@]d@%�@b@˒@�@v`@33@@��@�@}V@ff@\�@\�@Ta@3�@ �@��@�@�=@f�@�@�v@��@�D@bN@V�@@�q@x@K�@�"@��@�@�@kQ@C�@J@��@m]@?}@/@�@�@�@��@U2@1'@�@�@ݘ@�g@��@�w@��@��@��@x@_p@�"@�@;�@.�@�@ �@�j@@��@�M@G�@-w@-w@�@�@��@��@g8@�@�W@��@��@o�@O@1�@
�M@
�@
�A@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B B�B�BoB�B B B B�BhB�BhBNB�BS�B�;B
2�B�B�B iB�BgB�B;B
�B
�.B�B�B9BYB �B-)B%�B]B�B B�B-BE9Bb�B�B�MB��B�4B��B��B��B��B��B�B�
B�}B�BzB�B�B"B&2B4�B>BB;JB3hB,qB�B�BPBYB �B�vB�B�B�MB� B�qB�BwBO�B&�B
��B
B
x8B
AB
�B	��B	�0B	�rB	��B	��B	�1B	q[B	U2B	1�B	QB	�B�`B�BҽB�)BŢB��B�aB�NB��BɆBՁB�$B��B�
B	
�B	E9B	�)B	�	B	�B	�B	�;B	�?B
YB
�B
!�B
1B	�,B	�B	ٴB	��B	��B	��B	�B	�B	��B	�HB	��B	��B	��B	�
B	��B	��B	�vB	��B	��B	�fB	�;B	��B	��B	��B	��B	�'B	�?B	��B	��B	�dB	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	�B	żB	��B	уB	ѷB	ҽB	ЗB	�vB	ΊB	��B	��B	ˬB	�	B	ɠB	�B	��B	ƨB	��B	�?B	�B	�EB	�EB	�?B	āB	żB	��B	��B	��B	��B	˒B	̘B	�,B	�B	ݘB	�B	��B	�_B	յB	��B	�[B	ӏB	ևB	��B	ؓB	�$B	��B	�$B	յB	�B	ңB	бB	ϑB	�vB	�:B	�NB	��B	�BB	��B	�}B	�TB	�TB	��B	҉B	��B	�dB	ȀB	��B	ɺB	��B	ňB	�KB	�=B	�XB	�B	ѝB	ҽB	ҽB	ҽB	ӏB	�gB	�FB	�B	�2B	ںB	ۦB	�)B	ۦB	�B	�B	�kB	��B	��B	�IB	��B	�jB	ߊB	��B	�B	�BB	�BB	�\B	��B	�bB	�HB	�B	�NB	�4B	�NB	�hB	�B	�B	�:B	��B	��B	�B	�B	�B	��B	�`B	��B	�B	�B	�LB	�B	�B	�$B	��B	�B	��B	�B	�B	�B	�KB	�KB	�DB	��B	�kB	��B	�B	�B	��B	�qB	�)B	�B	�]B	�CB	�CB	�B	�)B	�CB	��B	�"B	�B	��B	�wB	��B	�wB	��B	�qB	�=B	�qB	�B	�B	�B	��B	�)B	��B	��B	�IB	�5B	�iB	�OB	��B	��B	�B	��B	�B	�[B	�aB	��B	�nB	��B	�ZB	�`B	��B	�8B	��B	�XB	�XB	��B	�^B	�xB	�xB	�0B	��B	�^B	�JB	��B	��B	��B	�B	��B	��B	�B	�jB	�6B	�B	�PB	��B	�"B	�qB	��B	�BB	��B	�]B	�]B	��B	��B	�cB
 iB
 �B
 4B
  B	��B	��B
 B
  B
 4B
 4B
  B
 �B
 �B
 B
 �B
 iB
 �B
 �B
�B
[B
[B
�B
B
B
�B
�B
�B
B
�B
�B
�B
EB
�B
�B
YB
YB
?B
�B
_B
_B
_B
�B
�B
KB
	B
	RB

�B
 B
�B
2B
9B
9B
�B
�B
�B
�B
SB
�B
9B
SB
B
B
�B
+B
�B
�B
�B
�B
_B
yB
�B
�B
KB
eB
B
7B
#B
�B
B
B
B
CB
�B
�B
�B
dB
�B
�B
xB
�B
]B
CB
�B
�B
�B
/B
/B
�B
B
B
B
5B
�B
!B
�B
pB
;B
�B
�B
�B
 \B
 vB
 �B
 \B
!�B
!�B
"NB
"�B
"�B
"�B
#B
# B
#:B
#:B
#�B
#�B
#�B
$@B
$ZB
$tB
$@B
$@B
$B
$B
#�B
$B
$B
#�B
$�B
&�B
'RB
'�B
(XB
(�B
(�B
(�B
(sB
($B
(
B
(�B
)*B
*KB
,�B
-]B
-�B
.cB
.cB
.}B
.�B
/iB
/OB
0!B
0UB
/�B
/�B
/�B
/�B
0UB
1�B
1�B
1[B
2B
2aB
2|B
33B
3B
3�B
3�B
4B
4B
4B
4�B
4�B
5�B
5�B
5ZB
5tB
5�B
5�B
5�B
6zB
6`B
6`B
6�B
6�B
6�B
72B
72B
7�B
7�B
7�B
8B
8RB
9$B
9>B
9rB
9XB
:B
:xB
:xB
:^B
:�B
:�B
;0B
;JB
;JB
;�B
<B
<6B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=VB
=qB
=�B
=�B
>BB
>]B
>]B
>wB
>BB
>BB
>(B
>]B
>(B
=�B
=�B
>(B
>�B
>wB
?}B
?�B
?�B
?}B
@ B
@�B
A B
@�B
@�B
@�B
@�B
@�B
@�B
AB
A;B
A;B
AUB
A;B
AoB
A�B
A�B
BAB
B�B
B�B
C-B
C-B
C�B
DMB
DMB
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
EB
ESB
E�B
FB
FtB
F�B
F�B
F�B
FtB
F�B
G�B
H1B
H1B
HfB
H�B
H�B
IlB
I�B
J=B
J#B
JrB
J�B
K^B
KxB
K^B
K^B
K�B
K�B
K�B
LB
LdB
L�B
MB
MPB
MjB
M�B
M�B
M�B
MjB
M�B
N<B
O\B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
Q4B
QNB
Q�B
Q�B
RoB
R�B
R�B
S&B
R�B
S@B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
UMB
UgB
U�B
U�B
U�B
VB
VmB
VB
VmB
V�B
V�B
W?B
W?B
WsB
W�B
W�B
WYB
W�B
W�B
XEB
X+B
XyB
XyB
X�B
Y1B
YB
YB
Y�B
Y�B
ZQB
ZQB
Z7B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
[�B
[�B
\)B
\CB
\CB
\�B
\�B
]B
]dB
]�B
]�B
]�B
]�B
^B
]�B
^5B
^B
^�B
^5B
^�B
^�B
^�B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
`'B
`B
_�B
`vB
`�B
`�B
`�B
a-B
abB
aHB
abB
abB
a�B
a�B
bhB
bhB
b�B
b�B
c B
c:B
c B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
d�B
eB
e,B
e�B
e�B
e�B
e�B
ffB
f�B
ffB
ffB
f�B
f�B
gB
g�B
g�B
g�B
h$B
h>B
h$B
h>B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
jB
jKB
j0B
j�B
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
l"B
l=B
lWB
l�B
l�B
mCB
m)B
mCB
m�B
nB
m�B
n�B
o B
oB
o5B
o�B
pB
pUB
p�B
p�B
q'B
q'B
qAB
q[B
q�B
q�B
q�B
r-B
r-B
rGB
raB
r�B
sB
sB
s�B
s�B
s�B
tB
tTB
tTB
tTB
t�B
t�B
t�B
uB
u%B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
wB
wB
wB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
xRB
x�B
x�B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
zB
zDB
z^B
zxB
zDB
zxB
zxB
z�B
z�B
{B
{dB
{dB
{B
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
~BB
~BB
~BB
~]B
~]B
~�B
~�B
~�B
B
B
~�B
HB
HB
cB
�B
�B
�B
�4B
�iB
��B
��B
��B
� B
�oB
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B B�B�BoB�B B B B�BhB�BhBNB�BS�B�;B
2�B�B�B iB�BgB�B;B
�B
�.B�B�B9BYB �B-)B%�B]B�B B�B-BE9Bb�B�B�MB��B�4B��B��B��B��B��B�B�
B�}B�BzB�B�B"B&2B4�B>BB;JB3hB,qB�B�BPBYB �B�vB�B�B�MB� B�qB�BwBO�B&�B
��B
B
x8B
AB
�B	��B	�0B	�rB	��B	��B	�1B	q[B	U2B	1�B	QB	�B�`B�BҽB�)BŢB��B�aB�NB��BɆBՁB�$B��B�
B	
�B	E9B	�)B	�	B	�B	�B	�;B	�?B
YB
�B
!�B
1B	�,B	�B	ٴB	��B	��B	��B	�B	�B	��B	�HB	��B	��B	��B	�
B	��B	��B	�vB	��B	��B	�fB	�;B	��B	��B	��B	��B	�'B	�?B	��B	��B	�dB	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	�B	żB	��B	уB	ѷB	ҽB	ЗB	�vB	ΊB	��B	��B	ˬB	�	B	ɠB	�B	��B	ƨB	��B	�?B	�B	�EB	�EB	�?B	āB	żB	��B	��B	��B	��B	˒B	̘B	�,B	�B	ݘB	�B	��B	�_B	յB	��B	�[B	ӏB	ևB	��B	ؓB	�$B	��B	�$B	յB	�B	ңB	бB	ϑB	�vB	�:B	�NB	��B	�BB	��B	�}B	�TB	�TB	��B	҉B	��B	�dB	ȀB	��B	ɺB	��B	ňB	�KB	�=B	�XB	�B	ѝB	ҽB	ҽB	ҽB	ӏB	�gB	�FB	�B	�2B	ںB	ۦB	�)B	ۦB	�B	�B	�kB	��B	��B	�IB	��B	�jB	ߊB	��B	�B	�BB	�BB	�\B	��B	�bB	�HB	�B	�NB	�4B	�NB	�hB	�B	�B	�:B	��B	��B	�B	�B	�B	��B	�`B	��B	�B	�B	�LB	�B	�B	�$B	��B	�B	��B	�B	�B	�B	�KB	�KB	�DB	��B	�kB	��B	�B	�B	��B	�qB	�)B	�B	�]B	�CB	�CB	�B	�)B	�CB	��B	�"B	�B	��B	�wB	��B	�wB	��B	�qB	�=B	�qB	�B	�B	�B	��B	�)B	��B	��B	�IB	�5B	�iB	�OB	��B	��B	�B	��B	�B	�[B	�aB	��B	�nB	��B	�ZB	�`B	��B	�8B	��B	�XB	�XB	��B	�^B	�xB	�xB	�0B	��B	�^B	�JB	��B	��B	��B	�B	��B	��B	�B	�jB	�6B	�B	�PB	��B	�"B	�qB	��B	�BB	��B	�]B	�]B	��B	��B	�cB
 iB
 �B
 4B
  B	��B	��B
 B
  B
 4B
 4B
  B
 �B
 �B
 B
 �B
 iB
 �B
 �B
�B
[B
[B
�B
B
B
�B
�B
�B
B
�B
�B
�B
EB
�B
�B
YB
YB
?B
�B
_B
_B
_B
�B
�B
KB
	B
	RB

�B
 B
�B
2B
9B
9B
�B
�B
�B
�B
SB
�B
9B
SB
B
B
�B
+B
�B
�B
�B
�B
_B
yB
�B
�B
KB
eB
B
7B
#B
�B
B
B
B
CB
�B
�B
�B
dB
�B
�B
xB
�B
]B
CB
�B
�B
�B
/B
/B
�B
B
B
B
5B
�B
!B
�B
pB
;B
�B
�B
�B
 \B
 vB
 �B
 \B
!�B
!�B
"NB
"�B
"�B
"�B
#B
# B
#:B
#:B
#�B
#�B
#�B
$@B
$ZB
$tB
$@B
$@B
$B
$B
#�B
$B
$B
#�B
$�B
&�B
'RB
'�B
(XB
(�B
(�B
(�B
(sB
($B
(
B
(�B
)*B
*KB
,�B
-]B
-�B
.cB
.cB
.}B
.�B
/iB
/OB
0!B
0UB
/�B
/�B
/�B
/�B
0UB
1�B
1�B
1[B
2B
2aB
2|B
33B
3B
3�B
3�B
4B
4B
4B
4�B
4�B
5�B
5�B
5ZB
5tB
5�B
5�B
5�B
6zB
6`B
6`B
6�B
6�B
6�B
72B
72B
7�B
7�B
7�B
8B
8RB
9$B
9>B
9rB
9XB
:B
:xB
:xB
:^B
:�B
:�B
;0B
;JB
;JB
;�B
<B
<6B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=VB
=qB
=�B
=�B
>BB
>]B
>]B
>wB
>BB
>BB
>(B
>]B
>(B
=�B
=�B
>(B
>�B
>wB
?}B
?�B
?�B
?}B
@ B
@�B
A B
@�B
@�B
@�B
@�B
@�B
@�B
AB
A;B
A;B
AUB
A;B
AoB
A�B
A�B
BAB
B�B
B�B
C-B
C-B
C�B
DMB
DMB
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
EB
ESB
E�B
FB
FtB
F�B
F�B
F�B
FtB
F�B
G�B
H1B
H1B
HfB
H�B
H�B
IlB
I�B
J=B
J#B
JrB
J�B
K^B
KxB
K^B
K^B
K�B
K�B
K�B
LB
LdB
L�B
MB
MPB
MjB
M�B
M�B
M�B
MjB
M�B
N<B
O\B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
Q4B
QNB
Q�B
Q�B
RoB
R�B
R�B
S&B
R�B
S@B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
UMB
UgB
U�B
U�B
U�B
VB
VmB
VB
VmB
V�B
V�B
W?B
W?B
WsB
W�B
W�B
WYB
W�B
W�B
XEB
X+B
XyB
XyB
X�B
Y1B
YB
YB
Y�B
Y�B
ZQB
ZQB
Z7B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
[�B
[�B
\)B
\CB
\CB
\�B
\�B
]B
]dB
]�B
]�B
]�B
]�B
^B
]�B
^5B
^B
^�B
^5B
^�B
^�B
^�B
^�B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
_�B
`'B
`B
_�B
`vB
`�B
`�B
`�B
a-B
abB
aHB
abB
abB
a�B
a�B
bhB
bhB
b�B
b�B
c B
c:B
c B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
d�B
eB
e,B
e�B
e�B
e�B
e�B
ffB
f�B
ffB
ffB
f�B
f�B
gB
g�B
g�B
g�B
h$B
h>B
h$B
h>B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
jB
jKB
j0B
j�B
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
l"B
l=B
lWB
l�B
l�B
mCB
m)B
mCB
m�B
nB
m�B
n�B
o B
oB
o5B
o�B
pB
pUB
p�B
p�B
q'B
q'B
qAB
q[B
q�B
q�B
q�B
r-B
r-B
rGB
raB
r�B
sB
sB
s�B
s�B
s�B
tB
tTB
tTB
tTB
t�B
t�B
t�B
uB
u%B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
wB
wB
wB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
xRB
x�B
x�B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
zB
zDB
z^B
zxB
zDB
zxB
zxB
z�B
z�B
{B
{dB
{dB
{B
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
~BB
~BB
~BB
~]B
~]B
~�B
~�B
~�B
B
B
~�B
HB
HB
cB
�B
�B
�B
�4B
�iB
��B
��B
��B
� B
�oB
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104910  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173414  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173414  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173414                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023422  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023422  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                