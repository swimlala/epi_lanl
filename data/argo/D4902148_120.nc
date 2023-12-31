CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-11-29T15:35:49Z creation;2017-11-29T15:35:52Z conversion to V3.1;2019-12-18T07:26:32Z update;2022-11-21T05:31:37Z update;     
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
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20171129153549  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA  I1_0397_120                     2C  DdNAVIS_A                         0397                            ARGO 011514                     863 @�9g�7�1   @�9 '�} @:��)^��d4�K1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�H@��@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�AHD��HD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D�Ǯ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�9XA�9XA�7LA�7LA�A��yA�ƨA�A�A�ĜA��wA��jA��jA��jA��jA��RA��^A��^A��^A��RA��^A��FA��9A��A���A��hA�ffA�S�A�?}A�+A�+A�"�A��A��A��A�bA��#A���A��RA���A��#A�;dA�K�A��A��A�1A�t�A� �A��#A��A��!A�VA���A�^5A�7LA�7LA�XA�I�A�ZA���A�^5A�O�A�1A���A�VA�dZA�7LA�1A��yA��FA�=qA��TA�v�A�%A�\)A��#A�bA��
A��A�~�A�-A�ƨA�-A�^5A��!A��A�S�A��yA�C�A�~�A��A��jA��hA}S�Az��AzJAyXAw�PAv�DAu&�As�;Ar�yAq��Aq33Ao?}AmC�Ak��Ai�Ag�mAgO�Af�`AfffAfAe"�Abv�A`��A^�HA\��A\JA[��A[G�AZ��AY�#AX9XAW�AV~�AVJAU��AU�^AU�AU7LAT�9AT�ATE�AS��AS&�AR��AR5?AP��AP^5AO��AM��AKXAJ�!AIt�AHI�AG�FAF�AF�AE�hAD�uAC�AB�+AA�A?A?%A>�!A>Q�A=�FA<�A:��A9G�A6��A5/A4ffA3��A3��A3|�A3+A2�HA2Q�A1�A0-A/C�A.jA.I�A-��A-�
A-?}A,A�A+l�A*ffA)�;A)dZA(r�A'�^A&�A%`BA#�A#C�A#�A"�A!��A ĜA ��A 1A�^A&�At�A�A(�A�wA`BA%A�/A�+AA�A  AdZA��AhsA�A$�A��A%AZA��A/A��A�+Av�A�^A33A�uA�PA�9AQ�A�A1'A��A�A"�A
=A
ĜA
bNA	XA�A�A�A?}A��A��A�AƨA�yA�+AC�@�ƨ@�o@��!@��9@�+@�ƨ@�n�@��`@��y@�&�@�F@�  @�-@��
@�o@�n�@�7@��@��H@�b@��H@���@ؼj@ץ�@�E�@�p�@��@�bN@�b@�ƨ@ӝ�@�S�@ҏ\@�{@���@��@���@ѩ�@��/@�b@�;d@́@�1'@���@�\)@�o@�hs@�~�@�5?@��@Ų-@�Ĝ@�9X@�|�@�ȴ@�@���@�z�@�1'@��m@���@��#@�t�@��!@�{@��7@�bN@���@��-@���@�b@��@��y@��7@���@�o@�M�@�@�X@��;@�n�@���@�j@��
@���@�+@���@��\@�M�@�$�@�@�bN@�  @�ƨ@��@�ȴ@���@�`B@�7L@�?}@�?}@�7L@�%@��u@���@���@�S�@���@��+@�$�@���@��/@�v�@��D@�(�@���@���@��;@�5?@�O�@�%@��@���@��/@��`@��`@��`@��/@��/@���@�bN@�(�@���@�o@��7@�7L@��@�Q�@�C�@��R@���@���@�n�@�@���@��@��j@��9@��@�Q�@��m@�C�@��y@�=q@��@��@��#@�V@�5?@�J@�{@�J@�@��@���@�@���@��9@�1'@� �@� �@�(�@��w@�t�@�dZ@�o@��H@���@�^5@��@��#@���@��@�r�@�;d@��@�M�@�@��@���@��@���@�O�@��`@�bN@� �@�  @~��@}�@~5?@}�h@|��@|j@|9X@z��@z^5@y��@yx�@x��@xĜ@x �@w��@wl�@v�@v�R@vv�@v{@uO�@t�j@s��@s��@so@r�!@r�\@rM�@q�^@q&�@p��@pA�@p  @o�@oK�@o�@o
=@nȴ@n$�@m?}@lZ@k�m@k��@kC�@kS�@kdZ@kC�@ko@j^5@i��@i��@i�#@i�^@i�7@ix�@iG�@i&�@h��@h��@h�9@hr�@g�;@g��@g�P@g\)@g+@f��@f�@f��@f��@f��@f��@fE�@f@e��@e�@e/@dz�@c�
@b��@b�\@b�\@b~�@bn�@b^5@a�@a�^@a7L@`�`@`Q�@_��@_|�@_�@^�@^��@^v�@^E�@]�@]@]O�@\��@\��@\9X@[�m@[33@Z�!@Z=q@ZJ@Y��@Y�#@Y��@Y�^@Y��@YG�@X��@X�u@X �@X  @W�@W�@W��@W|�@W+@V��@V$�@U��@U�h@UO�@U�@T�j@Tj@S�
@S��@S��@S�@S�@SS�@S33@So@R��@R�@Q�^@Q7L@P�`@Pr�@Pb@O�@O|�@O�@N�R@N��@Nff@N{@M?}@LZ@KS�@KC�@K33@Ko@K@J�@J��@Jn�@J�@I��@I�@I�#@I�^@Ihs@H�`@HQ�@G�@G��@G+@F��@Fv�@E`B@D�@D�@C�m@C�
@C�F@C��@C��@C�@Ct�@C33@C@B�@B��@BJ@A�^@Ax�@AG�@@��@@Q�@?�;@?��@?|�@?;d@>�y@>�+@>{@=��@=@=�-@=�h@=`B@=`B@=?}@=V@<z�@<1@;ƨ@;t�@;S�@;S�@;C�@;33@;"�@;@:�@:~�@9�@9�@9�^@9�7@9hs@9G�@9&�@9%@8��@8Ĝ@8�u@8r�@8  @7|�@7;d@7+@6�+@6E�@6$�@5�T@5��@5`B@5/@4�@4(�@41@41@3��@3�m@3ƨ@3��@3�@3dZ@3"�@3@2��@2�\@2n�@2-@1�7@1%@0bN@01'@/�@/;d@.�y@.ȴ@.��@.�+@.ff@.{@-��@-�@-O�@-V@,��@,��@-V@-V@-�@-V@,z�@,9X@,(�@+��@+�
@+��@+��@+dZ@+C�@+C�@+33@+33@+o@+o@*�@*�H@*��@*��@*�!@*~�@*=q@)�@)��@)G�@)%@(��@(r�@(A�@(b@'�w@'+@&5?@$�/@$Z@#ƨ@#S�@#33@"�H@"��@"��@"��@"�!@"n�@"^5@"M�@"M�@"=q@"-@"-@"J@!��@!��@!��@!��@!��@!�7@!x�@!X@!X@!7L@!7L@!&�@!�@ ��@ �`@ ��@ Ĝ@ �9@ ��@ 1'@�;@�w@|�@�@��@v�@5?@�h@p�@?}@��@��@(�@ƨ@��@S�@o@��@M�@�@�@hs@�`@Q�@A�@  @��@|�@l�@l�@;d@��@��@v�@E�@�T@�@O�@/@�@V@��@�@��@�j@�j@�@��@z�@I�@(�@1@1@��@�m@�F@��@�@S�@@�@�H@��@��@��@��@�\@�\@~�@~�@n�@n�@�@�@�7@&�@��@��@  @��@��@l�@�@�y@��@�+@v�@ff@V@@��@�-@�@p�@`B@?}@?}@/@�@��@z�@j@I�@(�@��@��@ƨ@t�@S�@
��@
~�@
-@
�@
J@	��@	�@	�@	�@	�#@	��@	��@	�^@	�^@	��@	hs@Ĝ@A�@1'@�@�;@�;@�;@��@�w@�@�w@�w@�@��@��@�P@|�@�@�y@ȴ@��@v�@V@V@5?@@?}@V@V@�/@�j@�j@��@z�@I�@(�@�@�@�
@dZ@@��@��@n�@=q@J@�@��@�7@�7@�7@�7@7L@&�@ �`@ Ĝ@ Ĝ@ ��@ ��@ Ĝ@ ��@ �@ r�@ bN@ A�@ 1'?��;?���?�|�?�\)?�;d?���?��?��R?�{?���?�O�?��?�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�9XA�9XA�7LA�7LA�A��yA�ƨA�A�A�ĜA��wA��jA��jA��jA��jA��RA��^A��^A��^A��RA��^A��FA��9A��A���A��hA�ffA�S�A�?}A�+A�+A�"�A��A��A��A�bA��#A���A��RA���A��#A�;dA�K�A��A��A�1A�t�A� �A��#A��A��!A�VA���A�^5A�7LA�7LA�XA�I�A�ZA���A�^5A�O�A�1A���A�VA�dZA�7LA�1A��yA��FA�=qA��TA�v�A�%A�\)A��#A�bA��
A��A�~�A�-A�ƨA�-A�^5A��!A��A�S�A��yA�C�A�~�A��A��jA��hA}S�Az��AzJAyXAw�PAv�DAu&�As�;Ar�yAq��Aq33Ao?}AmC�Ak��Ai�Ag�mAgO�Af�`AfffAfAe"�Abv�A`��A^�HA\��A\JA[��A[G�AZ��AY�#AX9XAW�AV~�AVJAU��AU�^AU�AU7LAT�9AT�ATE�AS��AS&�AR��AR5?AP��AP^5AO��AM��AKXAJ�!AIt�AHI�AG�FAF�AF�AE�hAD�uAC�AB�+AA�A?A?%A>�!A>Q�A=�FA<�A:��A9G�A6��A5/A4ffA3��A3��A3|�A3+A2�HA2Q�A1�A0-A/C�A.jA.I�A-��A-�
A-?}A,A�A+l�A*ffA)�;A)dZA(r�A'�^A&�A%`BA#�A#C�A#�A"�A!��A ĜA ��A 1A�^A&�At�A�A(�A�wA`BA%A�/A�+AA�A  AdZA��AhsA�A$�A��A%AZA��A/A��A�+Av�A�^A33A�uA�PA�9AQ�A�A1'A��A�A"�A
=A
ĜA
bNA	XA�A�A�A?}A��A��A�AƨA�yA�+AC�@�ƨ@�o@��!@��9@�+@�ƨ@�n�@��`@��y@�&�@�F@�  @�-@��
@�o@�n�@�7@��@��H@�b@��H@���@ؼj@ץ�@�E�@�p�@��@�bN@�b@�ƨ@ӝ�@�S�@ҏ\@�{@���@��@���@ѩ�@��/@�b@�;d@́@�1'@���@�\)@�o@�hs@�~�@�5?@��@Ų-@�Ĝ@�9X@�|�@�ȴ@�@���@�z�@�1'@��m@���@��#@�t�@��!@�{@��7@�bN@���@��-@���@�b@��@��y@��7@���@�o@�M�@�@�X@��;@�n�@���@�j@��
@���@�+@���@��\@�M�@�$�@�@�bN@�  @�ƨ@��@�ȴ@���@�`B@�7L@�?}@�?}@�7L@�%@��u@���@���@�S�@���@��+@�$�@���@��/@�v�@��D@�(�@���@���@��;@�5?@�O�@�%@��@���@��/@��`@��`@��`@��/@��/@���@�bN@�(�@���@�o@��7@�7L@��@�Q�@�C�@��R@���@���@�n�@�@���@��@��j@��9@��@�Q�@��m@�C�@��y@�=q@��@��@��#@�V@�5?@�J@�{@�J@�@��@���@�@���@��9@�1'@� �@� �@�(�@��w@�t�@�dZ@�o@��H@���@�^5@��@��#@���@��@�r�@�;d@��@�M�@�@��@���@��@���@�O�@��`@�bN@� �@�  @~��@}�@~5?@}�h@|��@|j@|9X@z��@z^5@y��@yx�@x��@xĜ@x �@w��@wl�@v�@v�R@vv�@v{@uO�@t�j@s��@s��@so@r�!@r�\@rM�@q�^@q&�@p��@pA�@p  @o�@oK�@o�@o
=@nȴ@n$�@m?}@lZ@k�m@k��@kC�@kS�@kdZ@kC�@ko@j^5@i��@i��@i�#@i�^@i�7@ix�@iG�@i&�@h��@h��@h�9@hr�@g�;@g��@g�P@g\)@g+@f��@f�@f��@f��@f��@f��@fE�@f@e��@e�@e/@dz�@c�
@b��@b�\@b�\@b~�@bn�@b^5@a�@a�^@a7L@`�`@`Q�@_��@_|�@_�@^�@^��@^v�@^E�@]�@]@]O�@\��@\��@\9X@[�m@[33@Z�!@Z=q@ZJ@Y��@Y�#@Y��@Y�^@Y��@YG�@X��@X�u@X �@X  @W�@W�@W��@W|�@W+@V��@V$�@U��@U�h@UO�@U�@T�j@Tj@S�
@S��@S��@S�@S�@SS�@S33@So@R��@R�@Q�^@Q7L@P�`@Pr�@Pb@O�@O|�@O�@N�R@N��@Nff@N{@M?}@LZ@KS�@KC�@K33@Ko@K@J�@J��@Jn�@J�@I��@I�@I�#@I�^@Ihs@H�`@HQ�@G�@G��@G+@F��@Fv�@E`B@D�@D�@C�m@C�
@C�F@C��@C��@C�@Ct�@C33@C@B�@B��@BJ@A�^@Ax�@AG�@@��@@Q�@?�;@?��@?|�@?;d@>�y@>�+@>{@=��@=@=�-@=�h@=`B@=`B@=?}@=V@<z�@<1@;ƨ@;t�@;S�@;S�@;C�@;33@;"�@;@:�@:~�@9�@9�@9�^@9�7@9hs@9G�@9&�@9%@8��@8Ĝ@8�u@8r�@8  @7|�@7;d@7+@6�+@6E�@6$�@5�T@5��@5`B@5/@4�@4(�@41@41@3��@3�m@3ƨ@3��@3�@3dZ@3"�@3@2��@2�\@2n�@2-@1�7@1%@0bN@01'@/�@/;d@.�y@.ȴ@.��@.�+@.ff@.{@-��@-�@-O�@-V@,��@,��@-V@-V@-�@-V@,z�@,9X@,(�@+��@+�
@+��@+��@+dZ@+C�@+C�@+33@+33@+o@+o@*�@*�H@*��@*��@*�!@*~�@*=q@)�@)��@)G�@)%@(��@(r�@(A�@(b@'�w@'+@&5?@$�/@$Z@#ƨ@#S�@#33@"�H@"��@"��@"��@"�!@"n�@"^5@"M�@"M�@"=q@"-@"-@"J@!��@!��@!��@!��@!��@!�7@!x�@!X@!X@!7L@!7L@!&�@!�@ ��@ �`@ ��@ Ĝ@ �9@ ��@ 1'@�;@�w@|�@�@��@v�@5?@�h@p�@?}@��@��@(�@ƨ@��@S�@o@��@M�@�@�@hs@�`@Q�@A�@  @��@|�@l�@l�@;d@��@��@v�@E�@�T@�@O�@/@�@V@��@�@��@�j@�j@�@��@z�@I�@(�@1@1@��@�m@�F@��@�@S�@@�@�H@��@��@��@��@�\@�\@~�@~�@n�@n�@�@�@�7@&�@��@��@  @��@��@l�@�@�y@��@�+@v�@ff@V@@��@�-@�@p�@`B@?}@?}@/@�@��@z�@j@I�@(�@��@��@ƨ@t�@S�@
��@
~�@
-@
�@
J@	��@	�@	�@	�@	�#@	��@	��@	�^@	�^@	��@	hs@Ĝ@A�@1'@�@�;@�;@�;@��@�w@�@�w@�w@�@��@��@�P@|�@�@�y@ȴ@��@v�@V@V@5?@@?}@V@V@�/@�j@�j@��@z�@I�@(�@�@�@�
@dZ@@��@��@n�@=q@J@�@��@�7@�7@�7@�7@7L@&�@ �`@ Ĝ@ Ĝ@ ��@ ��@ Ĝ@ ��@ �@ r�@ bN@ A�@ 1'?��;?���?�|�?�\)?�;d?���?��?��R?�{?���?�O�?��?�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111#�B&�B&�B&�B%�B$�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B$�B#�B$�B$�B$�B#�B#�B#�B$�B#�B �B�B�B�BDB��B�qB��By�BW
BN�BI�B;dB)�BbB�B�HB�B�B��BB�?B��B��B�oB�1B�B�Bz�Bt�BjBffBcTBT�BF�BB�B<jB5?B$�B�B\BPB
=B1BB
��B
��B
�B
�NB
�B
��B
��B
��B
�LB
�!B
�B
��B
�\B
}�B
x�B
r�B
gmB
aHB
XB
O�B
H�B
A�B
:^B
.B
 �B
{B
+B	��B	��B	��B	��B	�B	�B	�B	��B	ÖB	�XB	�9B	�'B	�B	�B	��B	��B	��B	�hB	�\B	�VB	�PB	�DB	�7B	�%B	�B	�B	� B	{�B	y�B	t�B	l�B	iyB	ffB	YB	M�B	H�B	B�B	<jB	9XB	5?B	1'B	-B	(�B	#�B	�B	�B	bB	JB	DB		7B	%B	B��B��B�B�ZB�HB�BB�BB�;B�5B�)B�B��B��B��BɺBȴBǮBƨBĜBB�}B�dB�RB�FB�3B�B�B��B��B��B��B��B��B�{B�uB�hB�bB�PB�1B�1B�1B�+B�%B�B�B�B�B�B� B}�Bz�Bx�Bv�Bt�Br�Bq�Bo�Bn�Bm�Bl�Bk�BiyBgmBe`BcTB`BB_;B\)BZBYBYBXBW
BVBT�BR�BQ�BP�BO�BL�BJ�BI�BG�BE�BD�BB�B@�B?}B>wB<jB9XB7LB6FB5?B33B2-B1'B/B/B.B.B.B-B-B-B,B.B.B.B.B-B.B/B0!B0!B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B49B5?B6FB7LB8RB8RB9XB8RB6FB:^B:^B;dB:^B;dB;dB<jB=qB=qB?}B?}B?}B?}B@�BA�BE�BG�BG�BH�BH�BM�BQ�BR�BT�BVBW
BYB[#BaHBcTBe`Be`BjBn�Bq�Bu�Bw�Bx�B{�B}�B~�B� B�B�B�+B�1B�1B�1B�=B�JB�JB�PB�PB�JB�JB�JB�PB�PB�VB�VB�VB�VB�\B�VB�PB�\B�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�FB�^B�^B�dB�dB�wB��BÖBƨBƨBǮBɺB��B��B��B�B�5B�BB�TB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	B	B	+B		7B	DB	PB	\B	hB	\B	bB	�B	�B	 �B	 �B	"�B	'�B	-B	49B	5?B	7LB	7LB	8RB	<jB	@�B	C�B	H�B	M�B	M�B	O�B	S�B	W
B	XB	YB	ZB	[#B	\)B	]/B	^5B	_;B	`BB	cTB	dZB	hsB	iyB	jB	jB	l�B	n�B	n�B	p�B	s�B	u�B	v�B	w�B	x�B	z�B	{�B	|�B	|�B	|�B	|�B	}�B	� B	�B	�B	�%B	�%B	�1B	�7B	�DB	�JB	�VB	�bB	�hB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�9B	�?B	�RB	�^B	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
JB
PB
VB
VB
\B
bB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
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
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
@�B
A�B
A�B
B�B
B�B
C�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
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
iyB
iyB
iyB
iyB
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
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
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111#�B&�B'B'B&LB%B#B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B$B$&B%B$B$�B$�B$�B#�B#�B#�B%B$@B!B BBBIB[B�hB�[B��B~�BX�BP}BMB@4B0oByB�|B��B��BڠB�[B�+B�$B��B��B�{B�B�B��B|�Bv�Bk6Bg�BfBW�BG�BC�B>BB8�B'�BB�B�B
�B	7BMB
��B
��B
�IB
�B
چB
� B
�dB
�aB
��B
��B
��B
��B
�B
~�B
z*B
t�B
h�B
cB
Y�B
QB
I�B
B�B
<�B
0�B
# B

B
�B	��B	�B	��B	��B	�|B	�wB	�xB	�B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	|�B	z�B	vzB	mCB	j�B	i*B	[�B	N�B	J=B	C�B	=VB	:^B	6`B	2-B	.cB	*KB	%`B	;B	�B	NB	�B	�B	
XB	B	GB�<B��B�B�`B��B��B�B߾B��B�/BںB�2B�B̳B�	B�7B�1BǮB��B��B��B�6B�>B��B�nB��B��B��B��B�OB�jB�B��B��B�FB� B��B�BB�7B�B��B��B��B��B��B��B��B�B� B�B|By�Bw�Bu�Bs�Br�BpUBoBn/BmBl�BjeBhsBf�BdtBaB`vB]�BZ�BY�BY�BX_BW�BV�BVSBS�BRTBQ�BQ�BNpBK�BJ�BI7BF�BE�BDMBB'B@4B?HB>wB<�B9XB7fB6zB4�B3�B2�B1vB0�B/OB.�B.�B-�B.IB./B-�B/B/ B/B/ B-�B.�B/�B0�B0�B0oB0oB0�B0�B1�B1vB1vB1[B1vB1�B5B6B7fB88B8�B8�B9�B9�B7�B:�B:�B;�B;0B;�B<B=B>B>(B?�B?�B@B@OBA�BB�BFYBH1BHKBI�BI�BN�BR�BS�BU�BV�BXBY�B\CBa�Bc�Be�Bf�Bk�BoOBr|BvFBxBy>B|6B~(BHB�iB��B��B��B��B��B��B��B��B�~B�jB��B�~B��B��B��B��B��B��B��B��B��B�\B��B��B��B��B��B��B��B�#B��B��B��B��B��B��B��B��B��B��B�B��B�B��B��B�QB��B��B��B��B�xB�xB��B��B��B�B��B��B��B��B�#B�PB�:B�gB�1B�OB�\B�:B�B�B�B�B�B��B��B��B�$B��B�]B�.B�.B�B�cB	 4B	 B	UB	GB	SB	zB		�B	�B	�B	�B	B	.B	�B	B	�B	 �B	 �B	#B	($B	-wB	4�B	5�B	7�B	7�B	8�B	<�B	@�B	C�B	IB	NB	N"B	PbB	TFB	W?B	XEB	YKB	ZQB	[WB	\]B	]dB	^jB	_VB	`�B	c�B	d�B	h�B	i�B	j�B	j�B	l�B	n�B	n�B	p�B	s�B	u�B	wB	w�B	y$B	z�B	|B	}B	}<B	}<B	}qB	~]B	�OB	�-B	�3B	�?B	�?B	�KB	�lB	�xB	��B	�pB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�:B	�LB	�>B	�)B	�B	�/B	�/B	�IB	�iB	�[B	�vB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	��B	�B	� B	�&B	�FB	�B	�B	�
B	�?B	�YB	�_B	�eB	�WB	�IB	�IB	�jB	�VB	�pB	��B	�B	�tB	�tB	�B	�`B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�2B	��B	��B	�	B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�<B	�"B	�(B
 4B
;B
UB
AB
aB
�B
�B
_B
KB
	RB
	RB
	RB

=B

rB

XB

XB

XB

rB
^B
�B
dB
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
!�B
"�B
# B
#�B
#�B
$�B
%B
%�B
&B
&2B
'B
'�B
'�B
'�B
($B
)*B
)*B
)B
)B
)B
*0B
*0B
*0B
+B
+6B
+6B
,WB
-CB
./B
.IB
/OB
0;B
1AB
1AB
1AB
1[B
1vB
2aB
3hB
3hB
3hB
49B
49B
49B
49B
4TB
4nB
4nB
5ZB
5ZB
5ZB
5ZB
6`B
6`B
6`B
6`B
7LB
7LB
7LB
7fB
7LB
7fB
7LB
7�B
7fB
7fB
7fB
7fB
8lB
8�B
9rB
9rB
:�B
:�B
;�B
;B
;�B
;�B
<�B
=�B
@�B
A�B
A�B
B�B
B�B
C�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
MB
N"B
NB
N�B
N�B
O�B
PB
PB
QB
QB
R B
R B
S&B
TB
T,B
UB
UB
UB
T�B
UB
V9B
VB
W$B
W$B
W?B
W?B
X+B
X+B
YB
YB
YB
YB
YKB
Y1B
YB
YB
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
ZB
Z7B
Z7B
[#B
[WB
[=B
[=B
\)B
\CB
\)B
\CB
\CB
\CB
\)B
\)B
]IB
]/B
]/B
]IB
]IB
]dB
]~B
^�B
^jB
_VB
_pB
`vB
`vB
a|B
a|B
abB
b�B
bhB
bhB
bNB
b�B
bhB
c�B
c�B
d�B
dtB
dZB
dtB
dZB
dZB
dtB
dtB
ezB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
iyB
i�B
iyB
i�B
iyB
iyB
iyB
i�B
iyB
iyB
i�B
i�B
i�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
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
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
w�B
xB
w�B
x�B
x�B
x�B
y	B
x�B
y	B
x�B
zB
y�B
zB
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712100032132017121000321320171210003213202211182132442022111821324420221118213244201804031938202018040319382020180403193820  JA  ARFMdecpA19c                                                                20171130003513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171129153549  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171129153551  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171129153551  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171129153552  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171129153552  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171129153552  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171129153552  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171129153552  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171129153552                      G�O�G�O�G�O�                JA  ARUP                                                                        20171129155513                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171129153224  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171209153213  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171209153213  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103820  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123244  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                