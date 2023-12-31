CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-02-07T18:03:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230207180328  20230207180328  5906096 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7902                            2B  A   NAVIS_A                         1010                            170425                          863 @�P�IM1   @��>��@*l�C���c^n��O�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B'��B0  B8  BA��BG33BO��BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBB�\B\)B'\)B/B7BA\)BF��BO\)BWB_BgBp(�BwBB��HB��HB��HB��HB��HB��HB��HB�{B��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CT
>CU�CW�CY�C[�C]�C`
>Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�AGD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~Dߺ�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D�GD�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D���D��z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�A�A�?}A��A�AռjA�p�A�Q�A�5?A�(�A�&�A�(�A�&�A�"�A��A��A���A�E�A���AӺ^AӰ!A�S�A��/A�v�A�1A�%A��+A��A���A��A�$�A�5?A���A�oA���A�p�A���A���A��\A�O�A��7A��FA�/A��A���A��A��wA�hsA�oA���A�`BA�jA���A�=qA��7A��TA�\)A�Q�A�-A�;A|�Ay��Aw+Aq�Ah  Ab�!Aa��A]O�AW�AV�`AT��AN��AK\)AJ��AH$�AB�HAA�7A>��A;�-A:�RA9�A9
=A8VA7��A7%A6��A6VA5ƨA4��A3��A2A1�A0�jA0(�A/&�A.v�A.�A.9XA.$�A. �A.bA-��A,ȴA,�9A,jA,9XA+��A+�;A+��A+A+��A+p�A+�^A+��A+��A,I�A+x�A*�A(��A'|�A%�
A#`BA!�FA!;dA �HA =qA|�A�yA��A�AM�A1A��A(�A��AG�A��A�`A�!Az�AA�A �A�A�wAK�A%A��A=qAA/A��A�+AE�A�wA`BA��A��AjA��AO�A��A�jA�A{A�mA�7A\)A
=A��AffA �A��A��A�A$�AZA�mA%A-A�FA�7A|�A��A��A��A��A��A�jA�\A-A�
A��A�mA�mAl�AoA�A
��A
�A	\)A	|�A	��A	\)A��A��A�RA��AjA�;A�A��A$�A�wA\)AA�HAr�A(�A  A�hA?}A"�A��A�RA�\AVA �A�#A�A+A �A �9A A�A �@���@�;d@�"�@��@��@���@��R@�{@��@�bN@�t�@��H@�V@��@��@�z�@��m@�|�@�\)@���@�M�@�@���@�&�@�j@�(�@���@�C�@��@��@��@�1'@�@@�@���@��@�bN@�  @�F@��y@��T@�7@�`B@��@�@���@�M�@��`@�bN@��
@�@�t�@�+@�5?@��@�@�l�@޸R@�5?@݁@��@�9X@��@�1@���@ۥ�@��y@�v�@��@���@�7L@� �@�o@�{@�p�@ԛ�@ӍP@�+@���@���@���@�V@�&�@�1'@ύP@�K�@�@Ο�@�ff@���@���@�b@ˮ@�"�@�ȴ@�n�@�$�@�p�@�%@�A�@��@ǝ�@�;d@�^5@�@ź^@�X@�V@ċD@�I�@�1'@î@�l�@�\)@�33@��@�$�@�@�hs@�/@�%@��@�Ĝ@� �@��P@���@�-@���@��-@�7L@��u@�I�@��@���@�l�@�+@�ȴ@�ff@�-@���@�hs@�&�@��9@�(�@���@�o@�E�@�O�@��u@�9X@��m@���@�"�@��!@��@��@��/@� �@�ƨ@��@�;d@�@��y@���@��+@��@�hs@�%@�bN@��@��@��@�"�@���@�$�@��#@��^@��@��@��`@��@�Ĝ@��@�z�@�9X@�b@��m@���@�"�@���@��R@���@�n�@�@��#@���@���@�`B@��/@��u@�(�@��w@��@���@��R@���@���@��+@��+@�~�@�E�@�hs@��@��9@�bN@�9X@��@�l�@�o@��!@�ff@�=q@���@�hs@�O�@�?}@�O�@�/@���@��@��@��@�z�@�1'@�b@��;@�|�@�;d@�v�@���@�p�@��j@�Z@�9X@�(�@�1@��F@��@�ȴ@�v�@�^5@�V@�=q@��@���@��T@���@�@���@��7@�/@��9@�bN@� �@���@��@�;d@�@��y@��R@�v�@�-@���@���@�hs@�?}@�%@���@���@�bN@�1'@��m@���@���@�"�@��y@��@��!@���@�v�@�-@��@��@�@��h@�O�@�&�@�%@��`@��9@���@�1'@���@���@�\)@��H@��R@���@��+@�M�@��-@�&�@���@��`@��9@��u@�z�@�bN@�I�@�1'@��@���@�t�@�S�@���@���@�n�@�$�@��@���@�hs@�O�@�V@���@�Z@�b@��@��
@��w@�K�@�33@��y@���@��\@���@��-@��h@�hs@�O�@�O�@�?}@�V@���@��/@�z�@�9X@�b@\)@~ȴ@~�+@~E�@}�h@|�@|1@{33@z�\@z=q@y��@y�#@yhs@x�@x  @w��@w|�@vȴ@u@u�h@t��@tZ@t�@t1@sS�@so@r�\@r^5@q�@qx�@p��@p�@p1'@p  @o�w@o;d@nv�@m�@m`B@mV@l��@l�j@l1@k��@ko@jM�@i�^@i��@ihs@h��@h��@hA�@hb@g�w@g��@gl�@f��@f@eO�@e?}@eV@d��@dj@c�
@c�@b��@bn�@bM�@a�@a�^@a7L@`��@`r�@`A�@_��@_+@^��@^@]?}@\�@\(�@[��@[33@Z��@Z��@Z~�@Z�@Y�#@Y�#@Y��@Y%@X�9@X�@Xr�@W��@WK�@W
=@V��@V5?@U�@T�@TI�@T1@S�F@S��@S�@SdZ@SS�@S"�@So@R��@R��@R~�@RM�@Q��@Q��@Qhs@QX@P��@P��@Pr�@P  @O�@O��@O\)@Nȴ@N�+@NV@M�T@M�h@Mp�@MO�@L�j@L�D@L(�@Kƨ@Ko@J��@J�!@J=q@I�#@Ix�@I7L@I�@H��@H��@H�@HA�@G��@G|�@F�@F��@F�+@F{@E�h@E?}@D�/@D�j@Dz�@D9X@D1@C��@Ct�@C@B�\@B=q@A�#@Ax�@A7L@A�@@��@@�u@@Q�@?�;@?�@?l�@?�@>��@>V@>5?@>5?@>{@=�T@=@=�h@=V@<I�@;�
@;C�@;@:��@:M�@9��@9�#@9�^@9��@9G�@9�@9%@8��@8��@8Ĝ@8�u@8r�@8 �@7�@6�y@6�R@6�+@6v�@6V@6{@5@5�h@5O�@5V@4�/@4�j@4�D@4j@4Z@4Z@49X@41@3�
@3ƨ@3��@3t�@3o@2�@2�!@2^5@2�@2�@1�@1��@1hs@1%@0��@0��@0Ĝ@0�u@0�@0Q�@0b@0  @0  @/�w@/\)@/;d@.�@.v�@.ff@.E�@.$�@.@-�@-��@-�-@-�h@-O�@,�/@,�D@,I�@,�@+��@+ƨ@+�@+t�@+C�@*�H@*�!@*n�@*�@)��@)�7@)G�@)%@(r�@(1'@(b@'��@'��@'\)@'�@&ȴ@&�+@%�@%@%�h@%O�@$�/@$��@$9X@#�m@#t�@#C�@#o@"��@"J@!��@!�#@!��@!�^@!�7@!�@ ��@ ��@ �@ A�@ b@�@�@l�@�y@�R@��@�+@v�@ff@ff@5?@{@�T@�h@�@�@��@�@�/@�@��@I�@1@��@�
@��@dZ@�H@��@��@~�@=q@��@x�@7L@��@�@Q�@1'@b@1'@�w@+@
=@�y@��@v�@V@E�@$�@�T@@��@��@�@��@�/@��@��@�j@z�@��@��@o@��@��@�!@��@M�@J@��@�#@x�@G�@7L@�@��@�`@�9@bN@A�@ �@�@��@��@|�@K�@��@�@��@V@{@@@�h@�@`B@/@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�A�A�?}A��A�AռjA�p�A�Q�A�5?A�(�A�&�A�(�A�&�A�"�A��A��A���A�E�A���AӺ^AӰ!A�S�A��/A�v�A�1A�%A��+A��A���A��A�$�A�5?A���A�oA���A�p�A���A���A��\A�O�A��7A��FA�/A��A���A��A��wA�hsA�oA���A�`BA�jA���A�=qA��7A��TA�\)A�Q�A�-A�;A|�Ay��Aw+Aq�Ah  Ab�!Aa��A]O�AW�AV�`AT��AN��AK\)AJ��AH$�AB�HAA�7A>��A;�-A:�RA9�A9
=A8VA7��A7%A6��A6VA5ƨA4��A3��A2A1�A0�jA0(�A/&�A.v�A.�A.9XA.$�A. �A.bA-��A,ȴA,�9A,jA,9XA+��A+�;A+��A+A+��A+p�A+�^A+��A+��A,I�A+x�A*�A(��A'|�A%�
A#`BA!�FA!;dA �HA =qA|�A�yA��A�AM�A1A��A(�A��AG�A��A�`A�!Az�AA�A �A�A�wAK�A%A��A=qAA/A��A�+AE�A�wA`BA��A��AjA��AO�A��A�jA�A{A�mA�7A\)A
=A��AffA �A��A��A�A$�AZA�mA%A-A�FA�7A|�A��A��A��A��A��A�jA�\A-A�
A��A�mA�mAl�AoA�A
��A
�A	\)A	|�A	��A	\)A��A��A�RA��AjA�;A�A��A$�A�wA\)AA�HAr�A(�A  A�hA?}A"�A��A�RA�\AVA �A�#A�A+A �A �9A A�A �@���@�;d@�"�@��@��@���@��R@�{@��@�bN@�t�@��H@�V@��@��@�z�@��m@�|�@�\)@���@�M�@�@���@�&�@�j@�(�@���@�C�@��@��@��@�1'@�@@�@���@��@�bN@�  @�F@��y@��T@�7@�`B@��@�@���@�M�@��`@�bN@��
@�@�t�@�+@�5?@��@�@�l�@޸R@�5?@݁@��@�9X@��@�1@���@ۥ�@��y@�v�@��@���@�7L@� �@�o@�{@�p�@ԛ�@ӍP@�+@���@���@���@�V@�&�@�1'@ύP@�K�@�@Ο�@�ff@���@���@�b@ˮ@�"�@�ȴ@�n�@�$�@�p�@�%@�A�@��@ǝ�@�;d@�^5@�@ź^@�X@�V@ċD@�I�@�1'@î@�l�@�\)@�33@��@�$�@�@�hs@�/@�%@��@�Ĝ@� �@��P@���@�-@���@��-@�7L@��u@�I�@��@���@�l�@�+@�ȴ@�ff@�-@���@�hs@�&�@��9@�(�@���@�o@�E�@�O�@��u@�9X@��m@���@�"�@��!@��@��@��/@� �@�ƨ@��@�;d@�@��y@���@��+@��@�hs@�%@�bN@��@��@��@�"�@���@�$�@��#@��^@��@��@��`@��@�Ĝ@��@�z�@�9X@�b@��m@���@�"�@���@��R@���@�n�@�@��#@���@���@�`B@��/@��u@�(�@��w@��@���@��R@���@���@��+@��+@�~�@�E�@�hs@��@��9@�bN@�9X@��@�l�@�o@��!@�ff@�=q@���@�hs@�O�@�?}@�O�@�/@���@��@��@��@�z�@�1'@�b@��;@�|�@�;d@�v�@���@�p�@��j@�Z@�9X@�(�@�1@��F@��@�ȴ@�v�@�^5@�V@�=q@��@���@��T@���@�@���@��7@�/@��9@�bN@� �@���@��@�;d@�@��y@��R@�v�@�-@���@���@�hs@�?}@�%@���@���@�bN@�1'@��m@���@���@�"�@��y@��@��!@���@�v�@�-@��@��@�@��h@�O�@�&�@�%@��`@��9@���@�1'@���@���@�\)@��H@��R@���@��+@�M�@��-@�&�@���@��`@��9@��u@�z�@�bN@�I�@�1'@��@���@�t�@�S�@���@���@�n�@�$�@��@���@�hs@�O�@�V@���@�Z@�b@��@��
@��w@�K�@�33@��y@���@��\@���@��-@��h@�hs@�O�@�O�@�?}@�V@���@��/@�z�@�9X@�b@\)@~ȴ@~�+@~E�@}�h@|�@|1@{33@z�\@z=q@y��@y�#@yhs@x�@x  @w��@w|�@vȴ@u@u�h@t��@tZ@t�@t1@sS�@so@r�\@r^5@q�@qx�@p��@p�@p1'@p  @o�w@o;d@nv�@m�@m`B@mV@l��@l�j@l1@k��@ko@jM�@i�^@i��@ihs@h��@h��@hA�@hb@g�w@g��@gl�@f��@f@eO�@e?}@eV@d��@dj@c�
@c�@b��@bn�@bM�@a�@a�^@a7L@`��@`r�@`A�@_��@_+@^��@^@]?}@\�@\(�@[��@[33@Z��@Z��@Z~�@Z�@Y�#@Y�#@Y��@Y%@X�9@X�@Xr�@W��@WK�@W
=@V��@V5?@U�@T�@TI�@T1@S�F@S��@S�@SdZ@SS�@S"�@So@R��@R��@R~�@RM�@Q��@Q��@Qhs@QX@P��@P��@Pr�@P  @O�@O��@O\)@Nȴ@N�+@NV@M�T@M�h@Mp�@MO�@L�j@L�D@L(�@Kƨ@Ko@J��@J�!@J=q@I�#@Ix�@I7L@I�@H��@H��@H�@HA�@G��@G|�@F�@F��@F�+@F{@E�h@E?}@D�/@D�j@Dz�@D9X@D1@C��@Ct�@C@B�\@B=q@A�#@Ax�@A7L@A�@@��@@�u@@Q�@?�;@?�@?l�@?�@>��@>V@>5?@>5?@>{@=�T@=@=�h@=V@<I�@;�
@;C�@;@:��@:M�@9��@9�#@9�^@9��@9G�@9�@9%@8��@8��@8Ĝ@8�u@8r�@8 �@7�@6�y@6�R@6�+@6v�@6V@6{@5@5�h@5O�@5V@4�/@4�j@4�D@4j@4Z@4Z@49X@41@3�
@3ƨ@3��@3t�@3o@2�@2�!@2^5@2�@2�@1�@1��@1hs@1%@0��@0��@0Ĝ@0�u@0�@0Q�@0b@0  @0  @/�w@/\)@/;d@.�@.v�@.ff@.E�@.$�@.@-�@-��@-�-@-�h@-O�@,�/@,�D@,I�@,�@+��@+ƨ@+�@+t�@+C�@*�H@*�!@*n�@*�@)��@)�7@)G�@)%@(r�@(1'@(b@'��@'��@'\)@'�@&ȴ@&�+@%�@%@%�h@%O�@$�/@$��@$9X@#�m@#t�@#C�@#o@"��@"J@!��@!�#@!��@!�^@!�7@!�@ ��@ ��@ �@ A�@ b@�@�@l�@�y@�R@��@�+@v�@ff@ff@5?@{@�T@�h@�@�@��@�@�/@�@��@I�@1@��@�
@��@dZ@�H@��@��@~�@=q@��@x�@7L@��@�@Q�@1'@b@1'@�w@+@
=@�y@��@v�@V@E�@$�@�T@@��@��@�@��@�/@��@��@�j@z�@��@��@o@��@��@�!@��@M�@J@��@�#@x�@G�@7L@�@��@�`@�9@bN@A�@ �@�@��@��@|�@K�@��@�@��@V@{@@@�h@�@`B@/@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B}�B}�B}�B|�B|�B}�B~�B}�B|�B|�B}�B~�B� B� B�B�B�VB	\B	$�B	 �B	C�B	�B
uB
K�B
�qB
��B
B
��BoB6FB]/BW
Be`BjBffB� BɺB��B�oB\)BE�B9XB#�B�BB
�B
��B
�B
�bB
�B
dZB
A�B
-B
.B
33B
.B
)�B
%�B
+B
7LB
6FB
&�B
�B
  B	�/B	�FB	�3B	��B	��B	�FB	��B	ŢB	��B	��B	�?B	�#B	�B	�BB	��B
oB
33B
XB
o�B
�B
�DB
�VB
�hB
�uB
��B
��B
�bB
�1B
�JB
��B
��B
��B
��B
�'B
�?B
�RB
�dB
�}B
ĜB
ƨB
ǮB
ǮB
ɺB
��B
��B
��B
��B
�B
�B
��B
��BVB\B1B
��B
�B
�B
ƨB
�XB
�^B
�jB
�qB
B
ƨB
ƨB
ǮB
ŢB
��B
�qB
�-B
�B
�'B
�'B
�'B
�3B
�?B
�FB
�LB
�LB
�RB
�^B
�dB
�^B
�LB
�FB
�FB
�'B
�'B
�!B
�B
�!B
�!B
�B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�'B
�-B
�9B
�?B
�FB
�FB
�LB
�LB
�LB
�dB
�wB
ƨB
B
�dB
�9B
�'B
�!B
�!B
�RB
�wB
B
�}B
�dB
�dB
�XB
�XB
�LB
�RB
�qB
��B
��B
�^B
�qB
�^B
�XB
�?B
�^B
�jB
�qB
�jB
�dB
�dB
�^B
�XB
�LB
�9B
�'B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
�uB
�oB
�uB
�uB
��B
�uB
�uB
�uB
�oB
�{B
�uB
�uB
�oB
�bB
�\B
�\B
�VB
�VB
�VB
�PB
�JB
�JB
�DB
�DB
�DB
�7B
�=B
�7B
�7B
�7B
�7B
�1B
�+B
�%B
�%B
�B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
}�B
~�B
|�B
|�B
|�B
{�B
z�B
z�B
x�B
y�B
y�B
y�B
y�B
w�B
w�B
w�B
v�B
v�B
w�B
u�B
v�B
v�B
u�B
t�B
t�B
u�B
t�B
s�B
s�B
t�B
s�B
r�B
q�B
r�B
q�B
r�B
q�B
o�B
p�B
s�B
s�B
u�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
x�B
y�B
x�B
y�B
y�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
{�B
z�B
z�B
{�B
|�B
|�B
{�B
{�B
{�B
|�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
� B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
� B
� B
� B
� B
� B
� B
�B
�B
� B
�B
� B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
� B
~�B
� B
� B
� B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�B
�%B
�+B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�\B
�VB
�VB
�VB
�\B
�VB
�PB
�VB
�bB
�\B
�bB
�bB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�{B
�{B
�uB
�uB
�uB
��B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�-B
�-B
�-B
�3B
�-B
�3B
�3B
�3B
�?B
�9B
�?B
�9B
�?B
�FB
�?B
�?B
�FB
�?B
�FB
�FB
�FB
�FB
�LB
�RB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�dB
�^B
�dB
�^B
�dB
�dB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�qB
�qB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�qB
�wB
�wB
�}B
�wB
�}B
�wB
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
B
��B
B
B
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ǮB
ǮB
ȴB
ǮB
ȴB
ȴB
ɺB
ȴB
ȴB
ȴB
ȴB
ɺB
ȴB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�
B
�
B
�B
�
B
�
B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�)B
�#B
�)B
�)B
�)B
�/B
�)B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�;B
�;B
�;B
�;B
�BB
�;B
�BB
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�yB
�B
�B
�B
�333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  B}�B}�B}�B|�B|�B}�B~�B}�B|�B|�B}�B~�B� B� B�B�B�VB	\B	$�B	 �B	C�B	�B
uB
K�B
�qB
��B
B
��BoB6FB]/BW
Be`BjBffB� BɺB��B�oB\)BE�B9XB#�B�BB
�B
��B
�B
�bB
�B
dZB
A�B
-B
.B
33B
.B
)�B
%�B
+B
7LB
6FB
&�B
�B
  B	�/B	�FB	�3B	��B	��B	�FB	��B	ŢB	��B	��B	�?B	�#B	�B	�BB	��B
oB
33B
XB
o�B
�B
�DB
�VB
�hB
�uB
��B
��B
�bB
�1B
�JB
��B
��B
��B
��B
�'B
�?B
�RB
�dB
�}B
ĜB
ƨB
ǮB
ǮB
ɺB
��B
��B
��B
��B
�B
�B
��B
��BVB\B1B
��B
�B
�B
ƨB
�XB
�^B
�jB
�qB
B
ƨB
ƨB
ǮB
ŢB
��B
�qB
�-B
�B
�'B
�'B
�'B
�3B
�?B
�FB
�LB
�LB
�RB
�^B
�dB
�^B
�LB
�FB
�FB
�'B
�'B
�!B
�B
�!B
�!B
�B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�'B
�-B
�9B
�?B
�FB
�FB
�LB
�LB
�LB
�dB
�wB
ƨB
B
�dB
�9B
�'B
�!B
�!B
�RB
�wB
B
�}B
�dB
�dB
�XB
�XB
�LB
�RB
�qB
��B
��B
�^B
�qB
�^B
�XB
�?B
�^B
�jB
�qB
�jB
�dB
�dB
�^B
�XB
�LB
�9B
�'B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
�uB
�oB
�uB
�uB
��B
�uB
�uB
�uB
�oB
�{B
�uB
�uB
�oB
�bB
�\B
�\B
�VB
�VB
�VB
�PB
�JB
�JB
�DB
�DB
�DB
�7B
�=B
�7B
�7B
�7B
�7B
�1B
�+B
�%B
�%B
�B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
}�B
~�B
|�B
|�B
|�B
{�B
z�B
z�B
x�B
y�B
y�B
y�B
y�B
w�B
w�B
w�B
v�B
v�B
w�B
u�B
v�B
v�B
u�B
t�B
t�B
u�B
t�B
s�B
s�B
t�B
s�B
r�B
q�B
r�B
q�B
r�B
q�B
o�B
p�B
s�B
s�B
u�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
x�B
y�B
x�B
y�B
y�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
{�B
z�B
z�B
{�B
|�B
|�B
{�B
{�B
{�B
|�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
� B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
� B
� B
� B
� B
� B
� B
�B
�B
� B
�B
� B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
� B
~�B
� B
� B
� B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�B
�%B
�+B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�\B
�VB
�VB
�VB
�\B
�VB
�PB
�VB
�bB
�\B
�bB
�bB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
�{B
�{B
�uB
�uB
�uB
��B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�-B
�-B
�-B
�3B
�-B
�3B
�3B
�3B
�?B
�9B
�?B
�9B
�?B
�FB
�?B
�?B
�FB
�?B
�FB
�FB
�FB
�FB
�LB
�RB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�dB
�^B
�dB
�^B
�dB
�dB
�dB
�jB
�dB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�qB
�qB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�qB
�wB
�wB
�}B
�wB
�}B
�wB
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
B
��B
B
B
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ǮB
ǮB
ȴB
ǮB
ȴB
ȴB
ɺB
ȴB
ȴB
ȴB
ȴB
ɺB
ȴB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�
B
�
B
�B
�
B
�
B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�)B
�#B
�)B
�)B
�)B
�/B
�)B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�;B
�;B
�;B
�;B
�BB
�;B
�BB
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�yB
�B
�B
�B
�333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230207180328                              AO  ARCAADJP                                                                    20230207180328    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230207180328  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230207180328  QCF$                G�O�G�O�G�O�8000            