CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-08-01T15:37:58Z creation;2017-08-01T15:38:03Z conversion to V3.1;2019-12-18T07:29:06Z update;2022-11-21T05:32:13Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170801153758  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA  I1_0397_108                     2C  Dd9�NAVIS_A                         0397                            ARGO 011514                     863 @� 8Q�1   @� �O� @;��4֡b�d9�*0U21   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��H@��@�{@�{A
=A?
=A_
=A
=A��A��A��A��AθRA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C&
=C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"u�D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Diu�Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�z�D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D���D�>D�~D��D��D�>D�HD�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�A؇+A�\)A��A���Aպ^Aգ�A�E�A�  A��;A԰!A�hsA��A�\)A��#A�n�A�O�A�^5A��^A��A�&�A��A��A��uA��
A�33A��A���A���A��\A��FA��A��^A�A�A���A���A�dZA�`BA�VA��A���A�K�A�XA�JA���A���A���A��A�9XA��;A��wA���A�^5A�%A�v�A��A�ȴA��uA�ƨA�33A���A���A��\A�hsA��PA�G�A�\)A��PA��A��RA��7A�=qA��yA��A~bA|�A{�^A{&�Az��Ay��AxAv��Au�#AtjAt(�As�
Asl�Ar$�Ao��AmVAj��Ahv�Af��Af-AeƨAe��Ae��Ae�hAe\)Ac��AaA_��A_/A]XA[�hAZ�/AZv�AZ^5AX�AV^5AUK�AT�uAS�^AQ��APffAO��AO"�AMO�AK�mAJ��AI�AI7LAHVAG�TAFZAD��ADZAD=qADJAC�hAC&�AB�HAB �AA|�AAO�AA%A@��A@��A@=qA?�hA>��A=��A=?}A<M�A;O�A:~�A9�;A9?}A8z�A7��A7%A6�jA6�!A6��A6z�A65?A5�hA4�A2��A1ƨA1�A1VA0��A0(�A/`BA.��A.�uA-��A-t�A-oA,~�A+�A+��A+��A+�A+33A)�^A(^5A'dZA'&�A&��A%��A$�`A$�A#&�A"M�A!G�A ĜA 1'A�PAffA{A��A��AK�AoA�A��AjA9XA�wA�A1'AVA��A"�AoA��A�RA�A�A�PA?}A��AffA�AoA%A�RAVA�-A;dA$�AȴA~�A
��A
I�A	�;A	��A	�^A	��AI�A��A5?A?}A�DA{AXAbA\)A Z@�z�@�+@�ff@��h@�V@��/@�I�@��@�;d@��H@�n�@� �@��@��@�F@���@띲@�?}@�u@�l�@�?}@��@��H@�@�r�@�ȴ@�E�@ٙ�@��@أ�@�Q�@���@ם�@׍P@�C�@���@ְ!@�$�@ա�@�G�@Լj@��H@�j@�l�@Ώ\@�5?@ƸR@�J@���@�`B@ċD@�I�@� �@��;@Ý�@��y@��@�ƨ@�33@���@�;d@���@���@�"�@��R@�E�@�hs@�1@�|�@��@�ff@�-@��@�@��#@���@�&�@���@��m@�{@���@�1@��;@��@��^@�G�@��/@�\)@���@��7@�Ĝ@�M�@��h@�&�@�%@���@��@�1@���@��P@�;d@�-@�7L@���@�
=@�ff@��#@���@�@��@���@��@��
@�dZ@��@��y@���@��!@���@���@�ff@�M�@�=q@��@��@�p�@���@�I�@�S�@���@���@���@�E�@�$�@��@�@��@��@���@�O�@��`@��@�(�@��m@�ƨ@��@�+@���@�=q@�X@��@��#@�hs@��`@�Ĝ@��9@��u@���@��@���@���@��@�|�@�t�@�t�@�S�@�o@���@��@��7@�r�@�(�@�1@��
@���@��P@�K�@��@��!@�5?@��@��D@�A�@��@��@;d@~��@~�y@~�@~ȴ@~�R@~�R@~�+@~$�@}�@}��@}V@|�@|I�@|I�@|I�@|9X@|�@|1@{�
@{�
@{ƨ@{t�@{o@zM�@y�#@y��@yhs@y�@x�`@x�9@x  @wl�@u��@t��@s"�@rM�@r�@q��@q��@q�@pQ�@p  @o\)@n�@nff@mO�@l�j@l�D@l9X@l9X@l�@l1@k�m@kƨ@k�F@k��@k�@kt�@kdZ@k33@j�@j�@j��@j�\@jn�@j-@i�^@i%@g�@f�y@f��@f�+@fff@fE�@f$�@f{@e�@e�-@e�@d�@d�j@d�D@d(�@c�
@ct�@aX@_;d@^�@^�R@^��@^�+@^v�@^ff@^ff@^ff@^ff@^E�@^@]��@]`B@]?}@]/@]O�@]O�@]?}@]/@]?}@]?}@]?}@]/@]/@]�@]V@\�/@\��@\z�@\Z@\I�@\9X@[��@[ƨ@[��@[t�@[C�@["�@[@Z�@Z��@Z�!@Z~�@ZM�@Z=q@Z-@Y�@Y�^@Y�7@Yhs@YX@Y7L@Y�@Y%@X��@X��@X�`@XĜ@X�9@X�u@XbN@X1'@X  @W�w@W\)@V��@U`B@R��@Q7L@P�`@P��@P��@P�9@P��@P�u@P�@P�@Pr�@PbN@PA�@Pb@O�w@O�P@O\)@O+@O�@O�@O
=@O
=@N��@N��@Nv�@N@M�T@M��@M��@M�-@M�h@Mp�@MO�@L�j@LI�@L9X@L(�@L1@K�m@K�
@K�
@K�F@K��@KdZ@KS�@K33@K33@K"�@Ko@Ko@Ko@K@J�@J�@J�H@J��@I�@H��@G�;@E�h@C33@BJ@@  @>�y@>�y@>ȴ@>��@>$�@=��@=�-@=�@=?}@=/@=V@<��@<�/@<j@;ƨ@:��@:�@9��@9��@9��@9��@9��@9�^@9�^@9��@9��@9��@9�7@9hs@97L@8��@8��@8bN@7�@7l�@7K�@7;d@6�R@6v�@65?@6@5�@5�@6@6@6@6@6@6{@6@5��@5`B@5V@4z�@3ƨ@3�@3dZ@333@3@2��@2~�@2�@1�^@1X@0�9@0  @/�@/|�@.v�@-O�@-/@-�@,�@,z�@,j@,j@,Z@,9X@,1@+��@+�m@+�m@+�
@+��@+dZ@+C�@+@*�H@*�H@*�@*�H@*�@*�@*�@*�@*�H@*�H@*�H@*�H@*�H@*��@*�!@*�!@*~�@*�@*J@*J@*�@*J@)�@(��@(  @'�;@'�@'l�@'l�@'+@&�R@&v�@%��@$�@$�D@#dZ@"��@"n�@"M�@"=q@"�@!�@!��@!�^@!��@!�7@!x�@!x�@!hs@!X@ �`@�w@K�@+@
=@��@�@ȴ@��@��@�+@V@V@ff@V@E�@E�@5?@@��@�h@�@O�@/@V@�@1@�
@�@�H@n�@-@J@�#@��@�^@��@��@�7@�7@�7@x�@hs@hs@X@X@�@��@��@�@1'@��@�@��@�P@;d@�y@�@�R@{@O�@?}@/@��@��@�/@��@�m@dZ@C�@33@�@��@�!@��@~�@^5@=q@�@hs@%@�`@Ĝ@Ĝ@Ĝ@��@�9@��@bN@bN@Q�@b@  @�;@�w@�P@|�@\)@;d@+@
=@�y@��@v�@V@5?@�@�-@�h@`B@`B@O�@?}@?}@?}@/@�@V@V@V@��@�/@�j@�@�D@j@9X@(�@��@��@S�@33@
�@
~�@
^5@
^5@
^5@
^5@
=q@	��@��@Ĝ@r�@r�@�@r�@r�@r�@�@1'@�;@�;@��@��@|�@;d@
=@�@ȴ@�R@��@��@�+@V@{@@@{@@`B@V@V@��@�D@z�@9X@9X@�@�F@�@t�@t�@t�@t�@C�@"�@o@@@@�H@��@�\@��@�!@��@~�@n�@^5@^5@^5@^5@^5@^5@M�@-@�@�@��@��@�@��@�#@��@��@�^@x�@�7@hs@ ��@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�A؇+A�\)A��A���Aպ^Aգ�A�E�A�  A��;A԰!A�hsA��A�\)A��#A�n�A�O�A�^5A��^A��A�&�A��A��A��uA��
A�33A��A���A���A��\A��FA��A��^A�A�A���A���A�dZA�`BA�VA��A���A�K�A�XA�JA���A���A���A��A�9XA��;A��wA���A�^5A�%A�v�A��A�ȴA��uA�ƨA�33A���A���A��\A�hsA��PA�G�A�\)A��PA��A��RA��7A�=qA��yA��A~bA|�A{�^A{&�Az��Ay��AxAv��Au�#AtjAt(�As�
Asl�Ar$�Ao��AmVAj��Ahv�Af��Af-AeƨAe��Ae��Ae�hAe\)Ac��AaA_��A_/A]XA[�hAZ�/AZv�AZ^5AX�AV^5AUK�AT�uAS�^AQ��APffAO��AO"�AMO�AK�mAJ��AI�AI7LAHVAG�TAFZAD��ADZAD=qADJAC�hAC&�AB�HAB �AA|�AAO�AA%A@��A@��A@=qA?�hA>��A=��A=?}A<M�A;O�A:~�A9�;A9?}A8z�A7��A7%A6�jA6�!A6��A6z�A65?A5�hA4�A2��A1ƨA1�A1VA0��A0(�A/`BA.��A.�uA-��A-t�A-oA,~�A+�A+��A+��A+�A+33A)�^A(^5A'dZA'&�A&��A%��A$�`A$�A#&�A"M�A!G�A ĜA 1'A�PAffA{A��A��AK�AoA�A��AjA9XA�wA�A1'AVA��A"�AoA��A�RA�A�A�PA?}A��AffA�AoA%A�RAVA�-A;dA$�AȴA~�A
��A
I�A	�;A	��A	�^A	��AI�A��A5?A?}A�DA{AXAbA\)A Z@�z�@�+@�ff@��h@�V@��/@�I�@��@�;d@��H@�n�@� �@��@��@�F@���@띲@�?}@�u@�l�@�?}@��@��H@�@�r�@�ȴ@�E�@ٙ�@��@أ�@�Q�@���@ם�@׍P@�C�@���@ְ!@�$�@ա�@�G�@Լj@��H@�j@�l�@Ώ\@�5?@ƸR@�J@���@�`B@ċD@�I�@� �@��;@Ý�@��y@��@�ƨ@�33@���@�;d@���@���@�"�@��R@�E�@�hs@�1@�|�@��@�ff@�-@��@�@��#@���@�&�@���@��m@�{@���@�1@��;@��@��^@�G�@��/@�\)@���@��7@�Ĝ@�M�@��h@�&�@�%@���@��@�1@���@��P@�;d@�-@�7L@���@�
=@�ff@��#@���@�@��@���@��@��
@�dZ@��@��y@���@��!@���@���@�ff@�M�@�=q@��@��@�p�@���@�I�@�S�@���@���@���@�E�@�$�@��@�@��@��@���@�O�@��`@��@�(�@��m@�ƨ@��@�+@���@�=q@�X@��@��#@�hs@��`@�Ĝ@��9@��u@���@��@���@���@��@�|�@�t�@�t�@�S�@�o@���@��@��7@�r�@�(�@�1@��
@���@��P@�K�@��@��!@�5?@��@��D@�A�@��@��@;d@~��@~�y@~�@~ȴ@~�R@~�R@~�+@~$�@}�@}��@}V@|�@|I�@|I�@|I�@|9X@|�@|1@{�
@{�
@{ƨ@{t�@{o@zM�@y�#@y��@yhs@y�@x�`@x�9@x  @wl�@u��@t��@s"�@rM�@r�@q��@q��@q�@pQ�@p  @o\)@n�@nff@mO�@l�j@l�D@l9X@l9X@l�@l1@k�m@kƨ@k�F@k��@k�@kt�@kdZ@k33@j�@j�@j��@j�\@jn�@j-@i�^@i%@g�@f�y@f��@f�+@fff@fE�@f$�@f{@e�@e�-@e�@d�@d�j@d�D@d(�@c�
@ct�@aX@_;d@^�@^�R@^��@^�+@^v�@^ff@^ff@^ff@^ff@^E�@^@]��@]`B@]?}@]/@]O�@]O�@]?}@]/@]?}@]?}@]?}@]/@]/@]�@]V@\�/@\��@\z�@\Z@\I�@\9X@[��@[ƨ@[��@[t�@[C�@["�@[@Z�@Z��@Z�!@Z~�@ZM�@Z=q@Z-@Y�@Y�^@Y�7@Yhs@YX@Y7L@Y�@Y%@X��@X��@X�`@XĜ@X�9@X�u@XbN@X1'@X  @W�w@W\)@V��@U`B@R��@Q7L@P�`@P��@P��@P�9@P��@P�u@P�@P�@Pr�@PbN@PA�@Pb@O�w@O�P@O\)@O+@O�@O�@O
=@O
=@N��@N��@Nv�@N@M�T@M��@M��@M�-@M�h@Mp�@MO�@L�j@LI�@L9X@L(�@L1@K�m@K�
@K�
@K�F@K��@KdZ@KS�@K33@K33@K"�@Ko@Ko@Ko@K@J�@J�@J�H@J��@I�@H��@G�;@E�h@C33@BJ@@  @>�y@>�y@>ȴ@>��@>$�@=��@=�-@=�@=?}@=/@=V@<��@<�/@<j@;ƨ@:��@:�@9��@9��@9��@9��@9��@9�^@9�^@9��@9��@9��@9�7@9hs@97L@8��@8��@8bN@7�@7l�@7K�@7;d@6�R@6v�@65?@6@5�@5�@6@6@6@6@6@6{@6@5��@5`B@5V@4z�@3ƨ@3�@3dZ@333@3@2��@2~�@2�@1�^@1X@0�9@0  @/�@/|�@.v�@-O�@-/@-�@,�@,z�@,j@,j@,Z@,9X@,1@+��@+�m@+�m@+�
@+��@+dZ@+C�@+@*�H@*�H@*�@*�H@*�@*�@*�@*�@*�H@*�H@*�H@*�H@*�H@*��@*�!@*�!@*~�@*�@*J@*J@*�@*J@)�@(��@(  @'�;@'�@'l�@'l�@'+@&�R@&v�@%��@$�@$�D@#dZ@"��@"n�@"M�@"=q@"�@!�@!��@!�^@!��@!�7@!x�@!x�@!hs@!X@ �`@�w@K�@+@
=@��@�@ȴ@��@��@�+@V@V@ff@V@E�@E�@5?@@��@�h@�@O�@/@V@�@1@�
@�@�H@n�@-@J@�#@��@�^@��@��@�7@�7@�7@x�@hs@hs@X@X@�@��@��@�@1'@��@�@��@�P@;d@�y@�@�R@{@O�@?}@/@��@��@�/@��@�m@dZ@C�@33@�@��@�!@��@~�@^5@=q@�@hs@%@�`@Ĝ@Ĝ@Ĝ@��@�9@��@bN@bN@Q�@b@  @�;@�w@�P@|�@\)@;d@+@
=@�y@��@v�@V@5?@�@�-@�h@`B@`B@O�@?}@?}@?}@/@�@V@V@V@��@�/@�j@�@�D@j@9X@(�@��@��@S�@33@
�@
~�@
^5@
^5@
^5@
^5@
=q@	��@��@Ĝ@r�@r�@�@r�@r�@r�@�@1'@�;@�;@��@��@|�@;d@
=@�@ȴ@�R@��@��@�+@V@{@@@{@@`B@V@V@��@�D@z�@9X@9X@�@�F@�@t�@t�@t�@t�@C�@"�@o@@@@�H@��@�\@��@�!@��@~�@n�@^5@^5@^5@^5@^5@^5@M�@-@�@�@��@��@�@��@�#@��@��@�^@x�@�7@hs@ ��@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111PBBB  B  B  B��B��B��B��B��B�B��B��Bp�B@�B,B+B,B5?B5?BPB��B�/B��B�^B�!B�RB�B �B �B�B�BPB��B�;B�B��B�B�ZB�qB��B��B�7B|�Bq�BbNBS�BL�BI�BG�BF�BB�B=qB7LB33B.B)�B �B�B�B\BB
��B
�sB
�B
��B
��B
�dB
�LB
�3B
�B
��B
��B
�\B
�B
� B
{�B
v�B
o�B
gmB
aHB
\)B
R�B
P�B
N�B
J�B
A�B
2-B
�B
oB
B	��B	��B	��B	��B	��B	�B	�B	�sB	�B	��B	��B	B	�XB	�?B	�-B	�!B	��B	��B	�uB	�VB	�7B	~�B	u�B	r�B	k�B	`BB	W
B	O�B	I�B	D�B	@�B	<jB	2-B	.B	.B	.B	.B	-B	+B	(�B	&�B	$�B	#�B	"�B	!�B	�B	�B	�B	�B	uB	oB	VB		7B	%B	B	  B��B��B��B��B��B��B�B�B�B�yB�NB�5B�)B�B�B�B��B��B��B��B��BɺBǮBŢBŢBĜBÖB��B�^B�?B�-B�!B�B��B��B��B��B��B��B��B�{B�oB�\B�VB�PB�JB�DB�=B�7B�1B�1B�%B�B�B}�Bx�Bv�Bu�Bt�Bt�Bs�Br�Bq�Bo�Bn�Bl�Bk�BhsBgmBgmBffBdZBbNB_;B[#BW
BQ�BO�BN�BM�BM�BL�BJ�BG�BD�BA�B?}B=qB<jB9XB7LB49B0!B-B+B)�B(�B(�B'�B'�B&�B%�B%�B#�B!�B�B�B�B�B�B�B�B�B�B�BuBoBhBhBhBhBhBbBbBbBbBbBbBbBbBbBbB\BVB\BoBuBhB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B%�B'�B)�B+B,B.B2-B49B6FB8RB9XB9XB9XB9XB:^B;dB;dB<jB@�BB�BE�BD�BG�BJ�BK�BK�BP�BQ�BT�BVB^5B`BBbNBcTBffBgmBhsBhsBiyBiyBk�Bn�Bu�Bw�Bz�B}�B}�B~�B� B�B�B�=B�DB�JB�JB�PB�PB�VB�VB�\B�\B�bB�bB�hB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�?B�RB�^B�dB�qB��BBǮB��B�B�#B�NB�mB�sB�sB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B	B	%B	+B		7B	
=B	JB	\B	hB	oB	�B	�B	�B	 �B	!�B	#�B	$�B	%�B	&�B	&�B	&�B	&�B	&�B	'�B	'�B	(�B	(�B	)�B	+B	,B	-B	-B	/B	1'B	2-B	33B	49B	49B	5?B	6FB	7LB	9XB	:^B	<jB	=qB	=qB	>wB	@�B	B�B	K�B	Q�B	W
B	ZB	ZB	ZB	[#B	\)B	_;B	_;B	aHB	bNB	bNB	cTB	dZB	dZB	e`B	dZB	e`B	e`B	e`B	ffB	ffB	ffB	ffB	gmB	gmB	gmB	hsB	gmB	gmB	hsB	hsB	iyB	k�B	n�B	s�B	v�B	w�B	w�B	x�B	x�B	x�B	y�B	y�B	z�B	z�B	|�B	}�B	}�B	~�B	� B	�B	�7B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�3B	�?B	�?B	�LB	�RB	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
1B

=B
VB
hB
hB
hB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
-B
-B
-B
/B
1'B
1'B
1'B
2-B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
?}B
?}B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111PB�B�B OB 4B iB��B�B��B��B��B��B�B�`B��BI�B.B,�B.cB8�B<PBB�0B�-B�&B�jB�3B�XB�6B"�B"�B vB�B�B��B��B��B�B�`B��B��B��B��B��BcBs�BdZBU�BM�BJ=BHBG_BC{B>�B88B4B/ B+�B!�BkB�BB�B
�2B
�QB
�CB
ΊB
ªB
�6B
��B
�B
�UB
�6B
��B
� B
�B
��B
|�B
x8B
qvB
h�B
b�B
]�B
SuB
Q�B
O�B
L�B
D�B
5tB
"�B
2B
�B	�B	�RB	��B	��B	�B	��B	��B	�B	��B	�B	�B	�gB	�DB	��B	�B	�|B	�LB	��B	��B	��B	�^B	��B	v�B	tB	m�B	a�B	XyB	Q B	J�B	E�B	A�B	>BB	3�B	.�B	.cB	.}B	.�B	-�B	+�B	)�B	'�B	%,B	$ZB	#:B	"NB	 \B	�B	�B	�B	�B	�B	�B	
XB	B	�B	B��B��B�+B��B��B�B�nB��B�;B�B�TB޸B��B��B��B��BӏBуBбBΊB�^BʌB�KB��B��B�9BĜB�[B�B�`B��B�B�cB�B�$B�,B��B�B�B�yB��B��B��B��B��B��B��B��B��B��B��B�B�SB��B� By�BwfBu�BuBu%Bt9BshBraBp!BoOBmwBl�BiBg�Bg�Bg8BezBcnB`�B]~BZBS�BP}BO\BNBN"BM�BL�BIlBE�BB�B@�B>wB=�B:�B8�B6B2|B-�B+�B*�B)yB)DB(sB(XB'�B&�B&�B%�B#�B \BpB!BB�B7BB�BYB?B�B�BoB�B�B�B�B�B�B�B}B�B�B�B�B�B�B.B�B�B[B�BFB�BB�BB7B�B�B�B�BEB�B�B�B=BB"4B&�B(sB*eB+�B,�B/B2�B4�B6�B8�B9rB9�B9�B9�B:�B;�B<PB=�BAUBCGBFBE�BH�BK)BLdBL�BQ�BR�BVBW�B^�B`�Bb�Bc�Bf�Bg�Bh�Bh�Bi�BjeBlqBo�BvFBxRB{JB~(B~BBHB��B�aB��B��B��B��B��B��B��B�pB��B�vB��B��B��B��B�9B�	B�]B� B��B�B�>B�*B�B�B�B�=B�WB�cB��B��B��B��B��B��B��B�B�B�BΥB�7BیB�B�B�B��B��B�B�B�B�B��B��B��B��B��B�%B�fB�JB��B	GB	YB	_B		RB	
�B	�B	�B	�B	B	SB	)B	�B	!B	!�B	$B	%B	%�B	'B	'B	'B	'B	'B	($B	(
B	)*B	)*B	*0B	+6B	,"B	-)B	-CB	/5B	1AB	2GB	3MB	4TB	4nB	5tB	6�B	7�B	9�B	:�B	<�B	=�B	=�B	>�B	@�B	CaB	L0B	R�B	WYB	ZQB	ZQB	ZkB	[qB	\xB	_pB	_�B	a|B	b�B	b�B	c�B	dtB	dtB	ezB	d�B	ezB	e�B	e�B	f�B	f�B	f�B	f�B	g�B	g�B	g�B	h�B	g�B	g�B	h�B	h�B	i�B	k�B	o B	tB	v�B	xB	xB	x�B	x�B	x�B	y�B	y�B	z�B	{B	}B	~B	~(B	.B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�GB	�3B	�hB	�ZB	�tB	��B	�lB	��B	��B	�rB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ðB	��B	ĶB	żB	��B	��B	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�BB	ЗB	��B	ܬB	�|B	�hB	�NB	�hB	�TB	�nB	�TB	�TB	�B	�B	�tB	�tB	�B	�B	�B	�B	�B	�sB	�B	�yB	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�RB	�^B	��B
�B
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
B
�B
�B
 B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
$B
%B
&B
'B
(
B
($B
(
B
($B
)B
)*B
)DB
*0B
*eB
+QB
-]B
-CB
-wB
/iB
1[B
1AB
1AB
2GB
3MB
4TB
4TB
4TB
4TB
5ZB
5?B
5ZB
5ZB
5tB
6`B
6`B
6`B
7fB
7fB
7LB
7LB
7fB
7fB
7LB
7LB
7fB
7LB
7LB
7fB
7LB
7LB
7fB
8RB
7fB
8lB
9rB
9XB
9XB
9rB
9�B
9�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
?�B
@ B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
F%B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
NB
NB
OB
PB
Q B
Q B
P�B
P�B
P�B
Q B
Q�B
RB
Q�B
Q�B
RB
Q�B
Q�B
Q�B
RB
RB
SB
SB
S&B
T,B
TB
T�B
UB
UB
UB
V9B
W$B
W?B
W?B
XB
X+B
X+B
XB
X+B
XEB
X_B
YeB
Z7B
Z7B
[WB
[=B
\)B
]/B
]IB
]IB
^OB
^OB
^�B
_VB
`\B
`BB
`\B
`BB
`BB
`BB
`vB
`\B
`\B
`vB
abB
abB
abB
a|B
a|B
a|B
bhB
bhB
bhB
bhB
bhB
b�B
bhB
bhB
b�B
bhB
b�B
bhB
b�B
cTB
cTB
cnB
cTB
cTB
cTB
cnB
cnB
dtB
dZB
dZB
dtB
dtB
dZB
dtB
dtB
d�B
ezB
ezB
e�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
gmB
g�B
g�B
h�B
i�B
i�B
i�B
iyB
i�B
iyB
iyB
iyB
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<r{�<��U<V�b<�}<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708120033222017081200332220170812003322202211182131192022111821311920221118213119201804031936442018040319364420180403193644  JA  ARFMdecpA19c                                                                20170802003519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170801153758  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170801153800  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170801153802  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170801153803  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170801153803  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170801153803  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170801153803  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170801153803  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170801153803                      G�O�G�O�G�O�                JA  ARUP                                                                        20170801160029                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170801153414  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170811153322  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170811153322  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103644  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123119  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                